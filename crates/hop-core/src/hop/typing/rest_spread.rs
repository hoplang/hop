//! Where each function's rest parameter lands.
//!
//! A function may declare a rest parameter and must forward it with exactly
//! one `...name` spread. Following that spread to wherever it lands decides
//! the parameters the rest adds to the function: those of a function it is
//! spread into, and an optional parameter for each attribute of the element
//! it reaches. This runs before any body is checked, because a call site
//! needs the parameters its callee ends up with.

use std::collections::{BTreeSet, HashMap};

use super::type_env::{FunctionSignature, ParamEntry, Row, Tail};
use crate::dependency_graph::DependencyGraph;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::{
    ParsedArguments, ParsedAttribute, ParsedExpr, ParsedMarkup, ParsedNamedArgument,
};
use crate::hop::typing::type_error::{TypeError, TypeErrorKind};
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;

/// Where a function's rest lands, and enough of the site it lands on to
/// decide the parameters the rest adds.
#[derive(Debug, Clone)]
pub enum RestSpreadTarget {
    Element {
        element: HtmlElementKind,
        supplied_attrs: Vec<AttributeName>,
        spread_range: DocumentRange,
    },
    Function {
        callee: FunctionName,
        supplied_attrs: Vec<AttributeName>,
        spread_range: DocumentRange,
    },
}

impl RestSpreadTarget {
    fn spread_range(&self) -> &DocumentRange {
        match self {
            RestSpreadTarget::Element { spread_range, .. } => spread_range,
            RestSpreadTarget::Function { spread_range, .. } => spread_range,
        }
    }
}

/// A `...name` spread attribute found in a body, with the target it lands on.
pub struct SpreadOccurrence {
    spread_name: VarName,
    target: RestSpreadTarget,
}

/// The named attributes written at a spread's site, which the rest cannot
/// supply a second time.
fn named_attrs(attributes: &[ParsedAttribute]) -> Vec<AttributeName> {
    attributes
        .iter()
        .filter_map(|a| a.name().cloned())
        .collect()
}

/// Collect every `...name` spread in an expression, in source order.
pub fn collect_spreads(expr: &ParsedExpr, out: &mut Vec<SpreadOccurrence>) {
    match expr {
        ParsedExpr::Markup { markup } => collect_spreads_in_markup(markup, out),
        ParsedExpr::Call {
            name,
            args: ParsedArguments::Named(arguments),
            ..
        } => {
            for argument in arguments {
                if let ParsedNamedArgument::Spread {
                    name: spread,
                    range,
                } = argument
                {
                    out.push(SpreadOccurrence {
                        spread_name: spread.clone(),
                        target: RestSpreadTarget::Function {
                            callee: name.clone(),
                            // A `children` argument is named like any other,
                            // so it is among the supplied attributes.
                            supplied_attrs: arguments
                                .iter()
                                .filter_map(|argument| match argument {
                                    ParsedNamedArgument::Value { name, .. } => Some(name.clone()),
                                    ParsedNamedArgument::Spread { .. } => None,
                                })
                                .collect(),
                            spread_range: range.clone(),
                        },
                    });
                }
            }
        }
        _ => {}
    }
    expr.for_each_child(&mut |child| collect_spreads(child, out));
}

fn collect_spreads_in_markup(markup: &ParsedMarkup, out: &mut Vec<SpreadOccurrence>) {
    match markup {
        ParsedMarkup::Element {
            kind: element,
            attributes,
            ..
        } => {
            for attr in attributes {
                if let ParsedAttribute::Spread { name, range } = attr {
                    out.push(SpreadOccurrence {
                        spread_name: name.clone(),
                        target: RestSpreadTarget::Element {
                            element: element.clone(),
                            supplied_attrs: named_attrs(attributes),
                            spread_range: range.clone(),
                        },
                    });
                }
            }
        }
        ParsedMarkup::Call {
            function_name,
            attributes,
            children,
            ..
        } => {
            for attr in attributes {
                if let ParsedAttribute::Spread { name, range } = attr {
                    let mut supplied_attrs = named_attrs(attributes);
                    // A tag pair passes its content as the argument
                    // `children`, written like any other.
                    if children.is_some() {
                        supplied_attrs.push(
                            AttributeName::new(CheapString::new("children".to_string()))
                                .expect("children is an attribute name"),
                        );
                    }
                    out.push(SpreadOccurrence {
                        spread_name: name.clone(),
                        target: RestSpreadTarget::Function {
                            callee: function_name.clone(),
                            supplied_attrs,
                            spread_range: range.clone(),
                        },
                    });
                }
            }
        }
        _ => {}
    }

    for expr in markup.expressions() {
        collect_spreads(expr, out);
    }

    for child in markup.children() {
        collect_spreads_in_markup(child, out);
    }
}

/// Pair a functions's rest parameter with the single spread that forwards it.
///
/// Every spread must name the declared rest, and a declared rest must be spread
/// exactly once. The rest comes with the function that declares it, for the
/// diagnostic when it is never spread. Pages cannot declare one, so
/// they pass `None` and every spread they contain is rejected.
pub fn pair_rest_spread(
    rest_param: Option<(&FunctionName, &(VarName, DocumentRange))>,
    spreads: Vec<SpreadOccurrence>,
    errors: &mut Vec<TypeError>,
) -> Option<RestSpreadTarget> {
    let rest_name = rest_param.map(|(_, (name, _))| name);
    let mut valid: Vec<SpreadOccurrence> = Vec::new();
    for occ in spreads {
        match rest_name {
            Some(rn) if occ.spread_name == *rn => valid.push(occ),
            _ => errors.push(TypeError::new(
                TypeErrorKind::SpreadNotDeclaredRest {
                    name: occ.spread_name.clone(),
                },
                occ.target.spread_range().clone(),
            )),
        }
    }
    for occ in valid.iter().skip(1) {
        errors.push(TypeError::new(
            TypeErrorKind::RestSpreadMoreThanOnce {
                name: occ.spread_name.clone(),
            },
            occ.target.spread_range().clone(),
        ));
    }
    if let Some((owner, (name, range))) = rest_param {
        if valid.is_empty() {
            errors.push(TypeError::new(
                TypeErrorKind::RestNeverSpread {
                    function: owner.clone(),
                    name: name.clone(),
                },
                range.clone(),
            ));
        }
    }
    valid.into_iter().next().map(|occ| occ.target)
}

/// Follow every function's rest to wherever it lands, and settle the
/// parameters each function has.
///
/// A function spreads its rest exactly once, the typechecker rejects a second
/// spread, so the spread relation is one-to-one, and following it either
/// reaches an HTML element, leaves the module for an import, or comes back to a
/// function already on the path. Only that last case has no tail to assign.
///
/// This is deliberately not the call graph. Two functions can call each other
/// while their rests run down a perfectly straight line to an element, and that
/// line is what decides the tail.
///
/// Returns the settled signature per function, whose row holds the declared
/// parameters followed by those the rest adds.
pub fn resolve_rest_targets(
    rest_targets: &HashMap<CheapString, Option<RestSpreadTarget>>,
    declared: &HashMap<CheapString, FunctionSignature>,
    errors: &mut Vec<TypeError>,
) -> HashMap<CheapString, FunctionSignature> {
    let mut spread_graph: DependencyGraph<CheapString> = DependencyGraph::new();
    for (name, rest_target) in rest_targets {
        let mut target = BTreeSet::new();
        if let Some(RestSpreadTarget::Function { callee, .. }) = rest_target {
            // A spread into an import is already settled: modules are checked
            // in import order, and imports cannot form a cycle.
            if rest_targets.contains_key(callee.as_str()) {
                target.insert(callee.to_cheap_string());
            }
        }
        spread_graph.set_dependencies(name.clone(), target);
    }

    let mut settled = declared.clone();
    for scc in spread_graph.sorted_sccs() {
        // With one spread per function an SCC is a cycle outright, whether it
        // runs through several functions or a function straight back to
        // itself. Every member spreads into another member, so every member is
        // where the rest fails to land.
        let is_cycle = scc.len() > 1 || scc.iter().any(|name| spread_graph.depends_on(name, name));

        for name in &scc {
            let Some(rest_target) = rest_targets.get(name) else {
                continue;
            };
            let Some(provisional) = declared.get(name) else {
                continue;
            };
            let params = &provisional.row.fields[..provisional.declared];

            let rest = if is_cycle {
                if let Some(target) = rest_target {
                    errors.push(TypeError::new(
                        TypeErrorKind::RestSpreadCycle {
                            name: FunctionName::new(name.clone())
                                .expect("function names are validated by the parser"),
                        },
                        target.spread_range().clone(),
                    ));
                }
                Row {
                    fields: Vec::new(),
                    tail: Tail::Closed,
                }
            } else {
                rest_row(rest_target.as_ref(), params, &settled)
            };

            let mut fields = params.to_vec();
            fields.extend(rest.fields);
            settled.insert(
                name.clone(),
                FunctionSignature {
                    declared: provisional.declared,
                    row: Row {
                        fields,
                        tail: rest.tail,
                    },
                    return_type: provisional.return_type.clone(),
                    rest_param: provisional.rest_param.clone(),
                },
            );
        }
    }
    settled
}

/// The parameters this function's rest adds, one for each name that the
/// site it is spread into accepts, except the names written at the spread
/// and the parameters the function declares itself.
///
/// Only reads the declaration and the target's settled signature, so it runs
/// before any body is checked.
fn rest_row(
    rest_target: Option<&RestSpreadTarget>,
    declared: &[ParamEntry],
    settled: &HashMap<CheapString, FunctionSignature>,
) -> Row {
    let (mut row, supplied_attrs) = match rest_target {
        Some(RestSpreadTarget::Element {
            element,
            supplied_attrs,
            ..
        }) => (
            Row {
                fields: Vec::new(),
                tail: Tail::Element {
                    element: element.clone(),
                    lacks: Vec::new(),
                },
            },
            supplied_attrs,
        ),
        Some(RestSpreadTarget::Function {
            callee,
            supplied_attrs,
            ..
        }) => match settled.get(callee.as_str()) {
            Some(callee_sig) => (callee_sig.row.clone(), supplied_attrs),
            None => {
                return Row {
                    fields: Vec::new(),
                    tail: Tail::Closed,
                };
            }
        },
        None => {
            return Row {
                fields: Vec::new(),
                tail: Tail::Closed,
            };
        }
    };
    // The rest adds neither a name written at the spread nor a parameter the
    // function declares itself. Such a name leaves the fields, and the tail
    // lacks it, whether the rest is spread into an element or a function.
    let declared_names = declared
        .iter()
        .map(|param| AttributeName::from(param.name.clone()));
    for name in supplied_attrs.iter().cloned().chain(declared_names) {
        row.fields
            .retain(|field| !field.name.as_str().eq_ignore_ascii_case(name.as_str()));
        if let Tail::Element { lacks, .. } = &mut row.tail
            && !lacks.contains(&name)
        {
            lacks.push(name);
        }
    }
    row
}
