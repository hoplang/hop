use super::r#type::Type;
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typecheck_node::typecheck_node;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::DocumentRange;
use crate::hop::parsing::parsed_expr::{
    Constructor, ParsedExpr, ParsedMatchArm, ParsedMatchPattern,
};
use crate::hop::parsing::parsed_node::ParsedMatchCase;
use crate::hop::patterns::compiler::{Decision, compile_match};
use crate::hop::patterns::typed::{TypedMatchPattern, typecheck_pattern};
use crate::hop::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::hop::typing::TypedExpr;
use crate::hop::typing::type_env::TypeEnv;
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};
use crate::variable_scope::VariableScope;

#[derive(Clone, Copy)]
pub enum MatchArms<'a> {
    Exprs(&'a [ParsedMatchArm]),
    Cases(&'a [ParsedMatchCase]),
}

impl<'a> MatchArms<'a> {
    fn len(&self) -> usize {
        match self {
            MatchArms::Exprs(arms) => arms.len(),
            MatchArms::Cases(cases) => cases.len(),
        }
    }

    fn pattern(&self, index: usize) -> &'a ParsedMatchPattern {
        match self {
            MatchArms::Exprs(arms) => &arms[index].pattern,
            MatchArms::Cases(cases) => &cases[index].pattern,
        }
    }

    /// The range to blame for a mismatch in an arm's body type.
    fn body_range(&self, index: usize) -> &'a DocumentRange {
        match self {
            MatchArms::Exprs(arms) => arms[index].body.range(),
            MatchArms::Cases(cases) => cases[index].pattern.range(),
        }
    }
}

pub fn typecheck_match(
    subject: &ParsedExpr,
    arms: MatchArms<'_>,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope<VarName, (Type, DocumentRange)>,
    type_env: &mut TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let typed_subject = typecheck_expr(
        subject,
        None,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    )?;

    let subject_type = typed_subject.typ();
    if !subject_type.is_matchable() {
        errors.push(TypeError::new(
            TypeErrorKind::MatchNotImplementedForType {
                found: subject_type,
            },
            subject.range().clone(),
        ));
        return None;
    }
    let typed_patterns = (0..arms.len())
        .map(|index| typecheck_pattern(arms.pattern(index), subject_type.clone(), registry, errors))
        .collect::<Option<Vec<_>>>()?;

    // A subject that is already a variable is matched on directly. Any other
    // expression is bound to a fresh variable around the whole decision tree,
    // so every switch and binding in the tree can refer to it by name.
    let (subject_name, subject_to_bind) = match typed_subject {
        TypedExpr::Var { value, .. } => (value, None),
        subject => (var_env.fresh_var_counter().fresh_var(), Some(subject)),
    };

    let tree = compile_match(
        var_env.fresh_var_counter(),
        registry,
        &typed_patterns,
        subject_name.clone(),
        subject_type,
        subject.range(),
        errors,
    );
    let arm_bodies = typecheck_arm_bodies(
        arms,
        &typed_patterns,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    );
    let (tree, (typed_bodies, result_type)) = (tree?, arm_bodies?);

    let body = decision_to_typed_expr(&tree, &typed_bodies, result_type.clone());
    Some(match subject_to_bind {
        Some(subject) => TypedExpr::Let {
            var: subject_name,
            value: Box::new(subject),
            body: Box::new(body),
            typ: result_type,
        },
        None => body,
    })
}

fn typecheck_arm_bodies(
    arms: MatchArms<'_>,
    typed_patterns: &[TypedMatchPattern],
    forwarded_params: &[VarName],
    var_env: &mut VariableScope<VarName, (Type, DocumentRange)>,
    type_env: &mut TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<(Vec<TypedExpr>, Type)> {
    let mut typed_bodies = Vec::new();
    let mut result_type: Option<Type> = None;

    for (index, typed_pattern) in typed_patterns.iter().enumerate() {
        collect_pattern_definition_links(arms.pattern(index), type_env, definition_links);

        let bindings = typed_pattern.bindings();
        let mut arm_ok = true;
        let mut pushed = Vec::new();
        for (name, typ, range) in &bindings {
            match var_env.push(name.clone(), (typ.clone(), range.clone())) {
                Ok(_) => {
                    annotations.push(HoverAnnotation::TypeForVarName {
                        range: range.clone(),
                        typ: typ.clone(),
                        var_name: name.clone(),
                    });
                    pushed.push(range);
                }
                Err(_) => {
                    errors.push(TypeError::new(
                        TypeErrorKind::VariableAlreadyDefined { name: name.clone() },
                        range.clone(),
                    ));
                    arm_ok = false;
                }
            }
        }

        let typed_body = match arms {
            // Use the first arm's type as context for subsequent arms
            MatchArms::Exprs(arms) => typecheck_expr(
                &arms[index].body,
                result_type.as_ref(),
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            ),
            MatchArms::Cases(cases) => Some(TypedExpr::FragmentConcat {
                nodes: cases[index]
                    .children
                    .iter()
                    .filter_map(|child| {
                        typecheck_node(
                            child,
                            forwarded_params,
                            registry,
                            errors,
                            var_env,
                            type_env,
                            annotations,
                            definition_links,
                            asset_references,
                        )
                    })
                    .collect(),
            }),
        };

        for range in pushed.iter().rev() {
            let (name, _, accessed) = var_env.pop();
            if !accessed {
                errors.push(TypeError::new(
                    TypeErrorKind::MatchUnusedBinding { name },
                    (*range).clone(),
                ));
            }
        }

        let Some(typed_body) = typed_body else {
            continue;
        };
        let body_type = typed_body.typ();

        match &result_type {
            None => {
                result_type = Some(body_type.clone());
            }
            Some(expected) => {
                if body_type != *expected {
                    errors.push(TypeError::new(
                        TypeErrorKind::MatchArmTypeMismatch {
                            expected: expected.clone(),
                            found: body_type,
                        },
                        arms.body_range(index).clone(),
                    ));
                    arm_ok = false;
                }
            }
        }

        if arm_ok {
            typed_bodies.push(typed_body);
        }
    }

    if typed_bodies.len() != arms.len() {
        return None;
    }

    Some((typed_bodies, result_type?))
}

/// Collect definition links for enum variant references in match patterns.
fn collect_pattern_definition_links(
    pattern: &ParsedMatchPattern,
    type_env: &mut TypeEnv,
    definition_links: &mut Vec<DefinitionLink>,
) {
    match pattern {
        ParsedMatchPattern::Constructor {
            constructor: Constructor::EnumVariant { enum_name, .. },
            enum_name_range: Some(enum_name_range),
            fields,
            args,
            ..
        } => {
            if let Some((_, def_range)) = type_env.lookup(enum_name) {
                definition_links.push(DefinitionLink {
                    use_range: enum_name_range.clone(),
                    definition_range: def_range.clone(),
                });
            }
            for (_, _, field_pattern) in fields {
                collect_pattern_definition_links(field_pattern, type_env, definition_links);
            }
            for arg in args {
                collect_pattern_definition_links(arg, type_env, definition_links);
            }
        }
        ParsedMatchPattern::Constructor { fields, args, .. } => {
            for (_, _, field_pattern) in fields {
                collect_pattern_definition_links(field_pattern, type_env, definition_links);
            }
            for arg in args {
                collect_pattern_definition_links(arg, type_env, definition_links);
            }
        }
        ParsedMatchPattern::Wildcard { .. } | ParsedMatchPattern::Binding { .. } => {}
    }
}

/// Convert a compiled Decision tree into a TypedExpr.
///
/// Every variable the tree refers to is in scope: the subject variable is
/// bound by the caller, and nested variables are bound by the enclosing switch.
fn decision_to_typed_expr(
    decision: &Decision,
    typed_bodies: &[TypedExpr],
    result_type: Type,
) -> TypedExpr {
    match decision {
        Decision::Success(body) => {
            let mut result = typed_bodies[body.value].clone();
            // Wrap with Let expressions for each binding (in reverse order so first binding is outermost)
            for binding in body.bindings.iter().rev() {
                let typ = result.typ();
                result = TypedExpr::Let {
                    var: binding.name.clone(),
                    value: Box::new(TypedExpr::Var {
                        value: binding.source_name.clone(),
                        typ: binding.typ.clone(),
                    }),
                    body: Box::new(result),
                    typ,
                };
            }
            result
        }

        Decision::SwitchBool {
            variable,
            true_case,
            false_case,
        } => TypedExpr::Match {
            match_: Match::Bool {
                subject: Box::new(TypedExpr::Var {
                    value: variable.name.clone(),
                    typ: variable.typ.clone(),
                }),
                true_body: Box::new(decision_to_typed_expr(
                    &true_case.body,
                    typed_bodies,
                    result_type.clone(),
                )),
                false_body: Box::new(decision_to_typed_expr(
                    &false_case.body,
                    typed_bodies,
                    result_type.clone(),
                )),
            },
            typ: result_type,
        },

        Decision::SwitchOption {
            variable,
            some_case,
            none_case,
        } => TypedExpr::Match {
            match_: Match::Option {
                subject: Box::new(TypedExpr::Var {
                    value: variable.name.clone(),
                    typ: variable.typ.clone(),
                }),
                some_arm_binding: some_case.bound_name.clone(),
                some_arm_body: Box::new(decision_to_typed_expr(
                    &some_case.body,
                    typed_bodies,
                    result_type.clone(),
                )),
                none_arm_body: Box::new(decision_to_typed_expr(
                    &none_case.body,
                    typed_bodies,
                    result_type.clone(),
                )),
            },
            typ: result_type,
        },

        Decision::SwitchEnum { variable, cases } => {
            let arms = cases
                .iter()
                .map(|case| {
                    let pattern = EnumPattern::Variant {
                        enum_name: case.enum_name.clone(),
                        variant_name: case.variant_name.clone(),
                    };

                    // Filter out wildcard bindings (bound_name is None)
                    let bindings: Vec<_> = case
                        .bindings
                        .iter()
                        .filter_map(|b| {
                            b.bound_name
                                .as_ref()
                                .map(|name| (b.field_name.clone(), name.clone()))
                        })
                        .collect();

                    let body =
                        decision_to_typed_expr(&case.body, typed_bodies, result_type.clone());

                    EnumMatchArm {
                        pattern,
                        bindings,
                        body,
                    }
                })
                .collect();

            TypedExpr::Match {
                match_: Match::Enum {
                    subject: Box::new(TypedExpr::Var {
                        value: variable.name.clone(),
                        typ: variable.typ.clone(),
                    }),
                    arms,
                },
                typ: result_type,
            }
        }

        Decision::SwitchRecord { variable, case } => {
            let mut body = decision_to_typed_expr(&case.body, typed_bodies, result_type);

            // Wrap with Let expressions for each field (using FieldAccess)
            // Iterate in reverse so bindings are in the correct order
            // Skip wildcard bindings (bound_name is None)
            for binding in case.bindings.iter().rev() {
                let Some(bound_name) = &binding.bound_name else {
                    continue;
                };

                // Create field access: subject.field_name
                let field_access = TypedExpr::FieldAccess {
                    record: Box::new(TypedExpr::Var {
                        value: variable.name.clone(),
                        typ: variable.typ.clone(),
                    }),
                    field: binding.field_name.clone(),
                    typ: binding.typ.clone(),
                };

                let typ = body.typ();
                body = TypedExpr::Let {
                    var: bound_name.clone(),
                    value: Box::new(field_access),
                    body: Box::new(body),
                    typ,
                };
            }

            body
        }
    }
}
