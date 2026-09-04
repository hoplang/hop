//! Where each component's rest parameter lands.
//!
//! A component may declare a rest parameter and must forward it with exactly
//! one `...name` spread. Following that spread to wherever it lands decides
//! the component's tail, and which of the target's parameters the rest
//! carries. This runs before any body is checked, because a call site needs
//! the parameters its callee ends up forwarding.

use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::Arc;

use super::r#type::{FunctionSignature, ParamEntry, Tail, Type};
use super::type_env::{TypeBinding, TypeEnv};
use crate::dependency_graph::DependencyGraph;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::parsed_ast::ParsedAttribute;
use crate::hop::parsing::parsed_node::ParsedNode;
use crate::html::HtmlElement;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};

/// Where a component's rest lands, and enough of the site it lands on to
/// decide the tail.
#[derive(Debug, Clone)]
pub enum RestSpreadTarget {
    Element {
        element: HtmlElement,
        supplied_attrs: Vec<CheapString>,
        spread_range: DocumentRange,
    },
    Component {
        callee: TypeName,
        supplied_attrs: Vec<CheapString>,
        has_children: bool,
        spread_range: DocumentRange,
    },
}

impl RestSpreadTarget {
    fn spread_range(&self) -> &DocumentRange {
        match self {
            RestSpreadTarget::Element { spread_range, .. } => spread_range,
            RestSpreadTarget::Component { spread_range, .. } => spread_range,
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
        .filter_map(|a| match a {
            ParsedAttribute::Named { name, .. } => Some(name.to_cheap_string()),
            ParsedAttribute::Spread { .. } => None,
        })
        .collect()
}

/// Collect every `...name` spread in a body, in source order.
///
/// A node's own attributes are visited before its children, so the first
/// occurrence found is the first one written. Match children live inside the
/// cases rather than in `children()`, so they are descended into explicitly.
pub fn collect_spreads(nodes: &[ParsedNode], out: &mut Vec<SpreadOccurrence>) {
    for node in nodes {
        match node {
            ParsedNode::Html {
                element,
                attributes,
                children,
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
                collect_spreads(children, out);
            }
            ParsedNode::ComponentInvocation {
                component_name,
                args,
                children,
                ..
            } => {
                for attr in args {
                    if let ParsedAttribute::Spread { name, range } = attr {
                        out.push(SpreadOccurrence {
                            spread_name: name.clone(),
                            target: RestSpreadTarget::Component {
                                callee: component_name.clone(),
                                supplied_attrs: named_attrs(args),
                                has_children: children.is_some(),
                                spread_range: range.clone(),
                            },
                        });
                    }
                }
                collect_spreads(children.as_deref().unwrap_or(&[]), out);
            }
            ParsedNode::Match { cases, .. } => {
                for case in cases {
                    collect_spreads(&case.children, out);
                }
            }
            other => collect_spreads(other.children(), out),
        }
    }
}

/// Pair a declaration's rest parameter with the single spread that forwards it.
///
/// Every spread must name the declared rest, and a declared rest must be spread
/// exactly once. Pages and views cannot declare one, so they pass `None` and
/// every spread they contain is rejected.
pub fn pair_rest_spread(
    owner: &TypeName,
    rest_param: Option<&(VarName, DocumentRange)>,
    spreads: Vec<SpreadOccurrence>,
    errors: &mut Vec<TypeError>,
) -> Option<RestSpreadTarget> {
    let rest_name = rest_param.map(|(name, _)| name);
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
    if let Some((name, range)) = rest_param {
        if valid.is_empty() {
            errors.push(TypeError::new(
                TypeErrorKind::RestNeverSpread {
                    component: owner.clone(),
                    name: name.clone(),
                },
                range.clone(),
            ));
        }
    }
    valid.into_iter().next().map(|occ| occ.target)
}

/// Follow every component's rest to wherever it lands, and record which of the
/// target's parameters it carries.
///
/// A component spreads its rest exactly once, the parser rejects a second
/// spread, so the spread relation is a function, and following it either
/// reaches an HTML element, leaves the module for an import, or comes back to a
/// component already on the path. Only that last case has no tail to assign.
///
/// This is deliberately not the call graph. Two components can call each other
/// while their rests run down a perfectly straight line to an element, and that
/// line is what decides the tail.
///
/// Returns the forwarded parameters per component, which the declarations need
/// and which the settled signatures no longer distinguish from declared ones.
pub fn resolve_rest_targets(
    rest_targets: &HashMap<TypeName, Option<RestSpreadTarget>>,
    type_env: &mut TypeEnv,
    errors: &mut Vec<TypeError>,
) -> HashMap<TypeName, Vec<ParamEntry>> {
    let mut spread_graph: DependencyGraph<TypeName> = DependencyGraph::new();
    for (name, rest_target) in rest_targets {
        let mut target = BTreeSet::new();
        if let Some(RestSpreadTarget::Component { callee, .. }) = rest_target {
            // A spread into an import is already settled: modules are checked
            // in import order, and imports cannot form a cycle.
            if rest_targets.contains_key(callee) {
                target.insert(callee.clone());
            }
        }
        spread_graph.set_dependencies(name.clone(), target);
    }

    let mut forwarded_params = HashMap::new();
    for scc in spread_graph.sorted_sccs() {
        // With one spread per component an SCC is a cycle outright, whether it
        // runs through several components or a component straight back to
        // itself. Every member spreads into another member, so every member is
        // where the rest fails to land.
        let is_cycle = scc.len() > 1 || scc.iter().any(|name| spread_graph.depends_on(name, name));

        for name in &scc {
            let Some(rest_target) = rest_targets.get(name) else {
                continue;
            };
            let Some((TypeBinding::Component(provisional), _)) = type_env.lookup(name) else {
                continue;
            };
            // Nothing has extended this signature yet, so its parameters are
            // exactly the declared ones.
            let declared: Vec<ParamEntry> = provisional.params.clone();
            let rest_param = provisional.rest_param.clone();

            let (forwarded, tail) = if is_cycle {
                if let Some(target) = rest_target {
                    errors.push(TypeError::new(
                        TypeErrorKind::RestSpreadCycle {
                            component: name.clone(),
                        },
                        target.spread_range().clone(),
                    ));
                }
                (Vec::new(), Tail::Closed)
            } else {
                rest_target_signature(rest_target.as_ref(), &declared, type_env)
            };

            let mut params = declared;
            params.extend(forwarded.iter().cloned());
            type_env.replace_binding(
                name,
                TypeBinding::Component(FunctionSignature {
                    params,
                    return_type: Arc::new(Type::Fragment),
                    tail,
                    rest_param,
                }),
            );
            forwarded_params.insert(name.clone(), forwarded);
        }
    }
    forwarded_params
}

/// Where this component's rest lands, and the callee parameters it carries.
///
/// Only reads the declaration and the target's settled signature, so it runs
/// before any body is checked.
fn rest_target_signature(
    rest_target: Option<&RestSpreadTarget>,
    declared: &[ParamEntry],
    type_env: &mut TypeEnv,
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
        Some(RestSpreadTarget::Component {
            callee,
            supplied_attrs,
            has_children,
            ..
        }) => match type_env.lookup(callee) {
            Some((TypeBinding::Component(callee_sig), _)) => {
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
