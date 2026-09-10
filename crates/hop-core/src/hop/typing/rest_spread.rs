//! Where each function's rest parameter lands.
//!
//! A function may declare a rest parameter and must forward it with exactly
//! one `...name` spread. Following that spread to wherever it lands decides
//! the function's tail, and which of the target's parameters the rest
//! carries. This runs before any body is checked, because a call site needs
//! the parameters its callee ends up forwarding.

use std::collections::{BTreeSet, HashMap, HashSet};

use super::type_env::{FunctionSignature, ParamEntry, Tail};
use crate::dependency_graph::DependencyGraph;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::parsed_node::{ParsedAttribute, ParsedNode};
use crate::html::HtmlElementKind;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};

/// Where a function's rest lands, and enough of the site it lands on to
/// decide the tail.
#[derive(Debug, Clone)]
pub enum RestSpreadTarget {
    Element {
        element: HtmlElementKind,
        supplied_attrs: Vec<CheapString>,
        spread_range: DocumentRange,
    },
    Function {
        callee: FunctionName,
        supplied_attrs: Vec<CheapString>,
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
fn named_attrs(attributes: &[ParsedAttribute]) -> Vec<CheapString> {
    attributes
        .iter()
        .filter_map(|a| a.name_range().map(|name| name.to_cheap_string()))
        .collect()
}

/// Collect every `...name` spread in a body, in source order.
pub fn collect_spreads(body: &ParsedExpr, out: &mut Vec<SpreadOccurrence>) {
    for node in body.nodes() {
        collect_spreads_in_node(node, out);
    }
}

fn collect_spreads_in_node(node: &ParsedNode, out: &mut Vec<SpreadOccurrence>) {
    match node {
        ParsedNode::HtmlElement {
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
        ParsedNode::FunctionInvocation {
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

    for expr in node.expressions() {
        collect_spreads(expr, out);
    }

    for child in node.children() {
        collect_spreads_in_node(child, out);
    }
}

/// Pair a functions's rest parameter with the single spread that forwards it.
///
/// Every spread must name the declared rest, and a declared rest must be spread
/// exactly once. The rest comes with the function that declares it, for the
/// diagnostic when it is never spread. Pages and views cannot declare one, so
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
/// Returns the settled signature per function: the declared parameters
/// followed by the forwarded ones.
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
                            name: FunctionName::from_cheap_string(name.clone())
                                .expect("function names are validated by the parser"),
                        },
                        target.spread_range().clone(),
                    ));
                }
                (Vec::new(), Tail::Closed)
            } else {
                rest_target_signature(rest_target.as_ref(), &provisional.params, &settled)
            };

            let mut params = provisional.params.clone();
            params.extend(forwarded);
            settled.insert(
                name.clone(),
                FunctionSignature {
                    params,
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
                        let callee_param_names: HashSet<&str> =
                            callee_sig.params.iter().map(|p| p.name.as_str()).collect();
                        for attr in supplied_attrs {
                            let a = attr.as_str();
                            if !callee_param_names.contains(a)
                                && !reserved.iter().any(|r| r.as_str() == a)
                            {
                                reserved.push(attr.clone());
                            }
                        }
                        Tail::Html { element, reserved }
                    }
                    Tail::Closed => Tail::Closed,
                };
                let covered_by_rest = |p: &ParamEntry| {
                    !(supplied_attrs.iter().any(|a| a.as_str() == p.name.as_str())
                        || (*has_children && p.name.as_str() == "children")
                        || declared_names.contains(&&p.name))
                };
                let forwarded = callee_sig
                    .params
                    .iter()
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
