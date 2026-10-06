//! Where each function's rest parameter lands.
//!
//! A function may declare a rest parameter and must forward it with exactly
//! one `...name` spread. Following that spread to wherever it lands decides
//! the function's tail, and which of the target's parameters the rest
//! carries. This runs before any body is checked, because a call site needs
//! the parameters its callee ends up forwarding.

use std::collections::{BTreeSet, HashMap};

use super::type_env::{FunctionSignature, ParamEntry, Tail};
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
/// decide the tail.
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
        has_children: bool,
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
                            has_children: false,
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
                    out.push(SpreadOccurrence {
                        spread_name: name.clone(),
                        target: RestSpreadTarget::Function {
                            callee: function_name.clone(),
                            supplied_attrs: named_attrs(attributes),
                            has_children: children.is_some(),
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

/// Follow every function's rest to wherever it lands, and record which of the
/// target's parameters it carries.
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
/// Returns the settled signature per function, with the parameters its rest
/// carries.
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

            let (forwarded, tail) = if is_cycle {
                if let Some(target) = rest_target {
                    errors.push(TypeError::new(
                        TypeErrorKind::RestSpreadCycle {
                            name: FunctionName::new(name.clone())
                                .expect("function names are validated by the parser"),
                        },
                        target.spread_range().clone(),
                    ));
                }
                (Vec::new(), Tail::Closed)
            } else {
                rest_target_signature(rest_target.as_ref(), &provisional.params, &settled)
            };

            settled.insert(
                name.clone(),
                FunctionSignature {
                    params: provisional.params.clone(),
                    forwarded,
                    return_type: provisional.return_type.clone(),
                    tail,
                    rest_param: provisional.rest_param.clone(),
                },
            );
        }
    }
    settled
}

/// Where this function's rest lands, and the callee parameters it carries.
///
/// Only reads the declaration and the target's settled signature, so it runs
/// before any body is checked.
fn rest_target_signature(
    rest_target: Option<&RestSpreadTarget>,
    declared: &[ParamEntry],
    settled: &HashMap<CheapString, FunctionSignature>,
) -> (Vec<ParamEntry>, Tail) {
    let declared_names: Vec<&VarName> = declared.iter().map(|p| &p.name).collect();
    match rest_target {
        Some(RestSpreadTarget::Element {
            element,
            supplied_attrs,
            ..
        }) => (
            Vec::new(),
            Tail::Html {
                element: element.clone(),
                reserved: supplied_attrs.clone(),
            },
        ),
        Some(RestSpreadTarget::Function {
            callee,
            supplied_attrs,
            has_children,
            ..
        }) => match settled.get(callee.as_str()) {
            Some(callee_sig) => {
                let tail = match callee_sig.tail.clone() {
                    Tail::Html {
                        element,
                        mut reserved,
                    } => {
                        for attr in supplied_attrs {
                            let names_callee_param = callee_sig
                                .params
                                .iter()
                                .chain(&callee_sig.forwarded)
                                .any(|p| p.name.as_str().eq_ignore_ascii_case(attr.as_str()));
                            if !names_callee_param && !reserved.contains(attr) {
                                reserved.push(attr.clone());
                            }
                        }
                        Tail::Html { element, reserved }
                    }
                    Tail::Closed => Tail::Closed,
                };
                let covered_by_rest = |p: &ParamEntry| {
                    !(supplied_attrs
                        .iter()
                        .any(|a| a.as_str().eq_ignore_ascii_case(p.name.as_str()))
                        || (*has_children && p.name.as_str() == "children")
                        || declared_names.contains(&&p.name))
                };
                let forwarded = callee_sig
                    .params
                    .iter()
                    .chain(&callee_sig.forwarded)
                    .filter(|p| covered_by_rest(p))
                    .cloned()
                    .collect::<Vec<_>>();
                (forwarded, tail)
            }
            _ => (Vec::new(), Tail::Closed),
        },
        None => (Vec::new(), Tail::Closed),
    }
}
