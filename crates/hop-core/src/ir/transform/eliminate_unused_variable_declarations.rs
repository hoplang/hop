use std::collections::HashSet;

use crate::expr::patterns::{EnumMatchArm, Match};
use crate::ir::pure_module::{PureExpr, PureForSource};
use crate::ir::var_id::VarId;

/// A pass that eliminates unused variable declarations.
///
/// - Unused let expressions are replaced with their body
/// - Unused Option match bindings are set to `_` (wildcard)
/// - Unused Enum match bindings are removed from the bindings list
/// - Unused for loop variables are set to `_` (wildcard)
pub fn eliminate_unused_variable_declarations(expr: PureExpr) -> PureExpr {
    let mut used = HashSet::new();
    transform(expr, &mut used)
}

/// Transform expr bottom-up, adding every variable referenced by the
/// returned subtree to the used set.
fn transform(expr: PureExpr, used: &mut HashSet<VarId>) -> PureExpr {
    match expr {
        PureExpr::Let {
            var,
            value,
            body,
            kind,
            id,
        } => {
            let mut body_used = HashSet::new();
            let body = transform(*body, &mut body_used);
            let result = if body_used.contains(&var.id) {
                PureExpr::Let {
                    var,
                    value: Box::new(transform(*value, used)),
                    body: Box::new(body),
                    kind,
                    id,
                }
            } else {
                // The value is dropped with the binding, so its variable
                // uses are never recorded and enclosing binders may become
                // unused in turn.
                body
            };
            used.extend(body_used);
            result
        }

        PureExpr::Match { match_, kind, id } => {
            let match_ = match match_ {
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => Match::Bool {
                    subject: Box::new(transform(*subject, used)),
                    true_body: Box::new(transform(*true_body, used)),
                    false_body: Box::new(transform(*false_body, used)),
                },
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => {
                    let mut some_used = HashSet::new();
                    let some_arm_body = transform(*some_arm_body, &mut some_used);
                    let some_arm_binding =
                        some_arm_binding.filter(|binding| some_used.contains(&binding.id));
                    used.extend(some_used);
                    Match::Option {
                        subject: Box::new(transform(*subject, used)),
                        some_arm_binding,
                        some_arm_body: Box::new(some_arm_body),
                        none_arm_body: Box::new(transform(*none_arm_body, used)),
                    }
                }
                Match::Enum { subject, arms } => Match::Enum {
                    subject: Box::new(transform(*subject, used)),
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            let mut arm_used = HashSet::new();
                            let body = transform(arm.body, &mut arm_used);
                            let bindings = arm
                                .bindings
                                .into_iter()
                                .filter(|(_, var)| arm_used.contains(&var.id))
                                .collect();
                            used.extend(arm_used);
                            EnumMatchArm {
                                pattern: arm.pattern,
                                bindings,
                                body,
                            }
                        })
                        .collect(),
                },
            };
            PureExpr::Match { match_, kind, id }
        }

        PureExpr::FragmentFor {
            var,
            source,
            body,
            id,
        } => {
            let mut body_used = HashSet::new();
            let body = transform(*body, &mut body_used);
            let var = var.filter(|v| body_used.contains(&v.id));
            used.extend(body_used);
            let source = match *source {
                PureForSource::Array(array) => PureForSource::Array(transform(array, used)),
                PureForSource::RangeInclusive { start, end } => PureForSource::RangeInclusive {
                    start: transform(start, used),
                    end: transform(end, used),
                },
            };
            PureExpr::FragmentFor {
                var,
                source: Box::new(source),
                body: Box::new(body),
                id,
            }
        }

        PureExpr::VariableReference { value, kind, id } => {
            used.insert(value.id);
            PureExpr::VariableReference { value, kind, id }
        }

        // Everything else binds nothing and references nothing itself:
        // recurse into children uniformly.
        expr => expr.map_children(&mut |child| transform(child, used)),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::pure_module::{PureFunctionDeclaration, PureModule, PureViewDeclaration};
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::runtime::evaluator::evaluate_view;
    use crate::ir::runtime::random::random_value;
    use crate::ir::runtime::value::Value;
    use crate::symbols::type_name::TypeName;
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};

    #[test]
    fn fuzz_random_pure_modules_evaluate_identically_after_elimination() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = StdRng::seed_from_u64(u.arbitrary()?);

            let view_args: Vec<(TypeName, HashMap<VarName, Value>)> = module
                .views
                .iter()
                .map(|view| {
                    let args = view
                        .parameters
                        .iter()
                        .map(|p| {
                            (
                                p.name().clone(),
                                random_value(&mut rng, &p.typ, None, &registry),
                            )
                        })
                        .collect();
                    (view.name.clone(), args)
                })
                .collect();

            let before: Vec<String> = view_args
                .iter()
                .map(|(view_name, args)| evaluate_view(&module, view_name, args.clone()).unwrap())
                .collect();

            let module = run(module);

            let after: Vec<String> = view_args
                .iter()
                .map(|(view_name, args)| evaluate_view(&module, view_name, args.clone()).unwrap())
                .collect();

            assert_eq!(before, after);
            Ok(())
        });
    }

    fn run(module: PureModule) -> PureModule {
        PureModule {
            views: module
                .views
                .into_iter()
                .map(|view| PureViewDeclaration {
                    name: view.name,
                    parameters: view.parameters,
                    body: eliminate_unused_variable_declarations(view.body),
                })
                .collect(),
            functions: module
                .functions
                .into_iter()
                .map(|function| PureFunctionDeclaration {
                    name: function.name,
                    parameters: function.parameters,
                    return_type: function.return_type,
                    body: eliminate_unused_variable_declarations(function.body),
                })
                .collect(),
            records: module.records,
            enums: module.enums,
            expr_ids: module.expr_ids,
            var_ids: module.var_ids,
        }
    }

    fn check(module: PureModule, expected: Expect) {
        let before = module.to_string();
        let module = run(module);
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_discard_unused_for_loop_variable() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.fragment_for(Some("unused"), t.array(vec![t.str("a"), t.str("b")]), |t| {
                        t.raw("Hello")
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  raw("Hello") for v0 in ["a", "b"]
                }

                -- after --
                view Test() {
                  raw("Hello") for _ in ["a", "b"]
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_used_for_loop_variable() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.fragment_for(Some("item"), t.array(vec![t.str("a"), t.str("b")]), |t| {
                        t.escape(t.var("item"))
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  escape(v0) for v0 in ["a", "b"]
                }

                -- after --
                view Test() {
                  escape(v0) for v0 in ["a", "b"]
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_unused_let() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("unused", t.str("value"), |t| t.raw("Hello"))
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "value" in raw("Hello")
                }

                -- after --
                view Test() {
                  raw("Hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_used_let() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("message", t.str("Hello"), |t| t.escape(t.var("message")))
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "Hello" in escape(v0)
                }

                -- after --
                view Test() {
                  let v0 = "Hello" in escape(v0)
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_chain_in_a_single_pass() {
        // y uses x, but y itself is unused: dropping y drops its value and
        // with it the only use of x, so x must be eliminated too.
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("x", t.str("a"), |t| {
                        t.let_expr("y", t.var("x"), |t| t.raw("Hello"))
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "a" in let v1 = v0 in raw("Hello")
                }

                -- after --
                view Test() {
                  raw("Hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_inside_for_loop_body() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.fragment_for(Some("item"), t.array(vec![t.str("a"), t.str("b")]), |t| {
                        t.let_expr("unused", t.str("value"), |t| t.escape(t.var("item")))
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v1 = "value" in escape(v0) for v0 in ["a", "b"]
                }

                -- after --
                view Test() {
                  escape(v0) for v0 in ["a", "b"]
                }
            "#]],
        );
    }

    #[test]
    fn should_discard_unused_option_match_binding() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.option_match_expr_with_binding(
                        t.some(t.str("x")),
                        "v",
                        |t| t.raw("some"),
                        t.raw("none"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(v0) => raw("some"),
                    None => raw("none"),
                  }
                }

                -- after --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(_) => raw("some"),
                    None => raw("none"),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_used_option_match_binding() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.option_match_expr_with_binding(
                        t.some(t.str("x")),
                        "v",
                        |t| t.escape(t.var("v")),
                        t.raw("none"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(v0) => escape(v0),
                    None => raw("none"),
                  }
                }

                -- after --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(v0) => escape(v0),
                    None => raw("none"),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_discard_unused_enum_match_bindings() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Status",
                    [("Active", vec![("since", "String")]), ("Inactive", vec![])],
                )
                .view_no_params("Test", |t| {
                    t.enum_match_expr(
                        t.enum_variant_with_fields(
                            "Status",
                            "Active",
                            vec![("since", t.str("now"))],
                        ),
                        |arms| {
                            arms.arm_bound("Active", [("since", "s")], |t| t.raw("active"));
                            arms.arm("Inactive", |t| t.raw("inactive"));
                        },
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                enum Status {
                  Active {since: String},
                  Inactive,
                }
                view Test() {
                  match Status::Active {since: "now"} {
                    Status::Active {since: v0} => raw("active"),
                    Status::Inactive => raw("inactive"),
                  }
                }

                -- after --
                enum Status {
                  Active {since: String},
                  Inactive,
                }
                view Test() {
                  match Status::Active {since: "now"} {
                    Status::Active => raw("active"),
                    Status::Inactive => raw("inactive"),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_used_enum_match_bindings() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Status",
                    [("Active", vec![("since", "String")]), ("Inactive", vec![])],
                )
                .view_no_params("Test", |t| {
                    t.enum_match_expr(
                        t.enum_variant_with_fields(
                            "Status",
                            "Active",
                            vec![("since", t.str("now"))],
                        ),
                        |arms| {
                            arms.arm_bound("Active", [("since", "s")], |t| t.escape(t.var("s")));
                            arms.arm("Inactive", |t| t.raw("inactive"));
                        },
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                enum Status {
                  Active {since: String},
                  Inactive,
                }
                view Test() {
                  match Status::Active {since: "now"} {
                    Status::Active {since: v0} => escape(v0),
                    Status::Inactive => raw("inactive"),
                  }
                }

                -- after --
                enum Status {
                  Active {since: String},
                  Inactive,
                }
                view Test() {
                  match Status::Active {since: "now"} {
                    Status::Active {since: v0} => escape(v0),
                    Status::Inactive => raw("inactive"),
                  }
                }
            "#]],
        );
    }
}
