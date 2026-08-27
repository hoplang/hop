use super::pure_module::{PureExpr, PureFunctionDeclaration, PureModule, PureViewDeclaration};
use crate::ir::{expr_id::ExprIdCounter, transform};

fn optimize_body(body: PureExpr, expr_ids: &mut ExprIdCounter) -> PureExpr {
    let body = transform::perform_partial_evaluation(body, expr_ids);
    let body = transform::eliminate_unused_variable_declarations(body);
    transform::normalize_fragments(body, expr_ids, 60)
}

pub fn optimize(module: PureModule) -> PureModule {
    let mut expr_ids = module.expr_ids;
    let views = module
        .views
        .into_iter()
        .map(|view| PureViewDeclaration {
            name: view.name,
            parameters: view.parameters,
            body: optimize_body(view.body, &mut expr_ids),
        })
        .collect();
    let functions = module
        .functions
        .into_iter()
        .map(|function| PureFunctionDeclaration {
            name: function.name,
            parameters: function.parameters,
            return_type: function.return_type,
            body: optimize_body(function.body, &mut expr_ids),
        })
        .collect();
    PureModule {
        views,
        functions,
        records: module.records,
        enums: module.enums,
        expr_ids,
        var_ids: module.var_ids,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::runtime::evaluator::evaluate_view;
    use crate::ir::runtime::{random::random_value, value::Value};
    use crate::symbols::type_name::TypeName;
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};

    #[test]
    fn fuzz_random_pure_modules_evaluate_identically_after_optimization() {
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

            let before_module = module.to_string();
            let before_outputs: Vec<String> = view_args
                .iter()
                .map(|(view_name, args)| evaluate_view(&module, view_name, args.clone()).unwrap())
                .collect();

            let module = optimize(module);

            for ((view_name, args), before_output) in view_args.iter().zip(&before_outputs) {
                let after_output = evaluate_view(&module, view_name, args.clone()).unwrap();
                assert_eq!(
                    before_output, &after_output,
                    "view {view_name}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    fn check(module: PureModule, expected: Expect) {
        let before = module.to_string();
        let result = optimize(module);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_optimize_single_view() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("unused", t.str("value"), |t| {
                        t.concat(vec![t.raw("Hello"), t.raw(" "), t.raw("World")])
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "value" in concat(
                    raw("Hello"),
                    raw(" "),
                    raw("World"),
                  )
                }

                -- after --
                view Test() {
                  concat(raw("Hello World"))
                }
            "#]],
        );
    }

    #[test]
    fn should_optimize_multiple_views() {
        check(
            PureModuleBuilder::new()
                .view_no_params("First", |t| {
                    t.let_expr("unused", t.str("x"), |t| {
                        t.concat(vec![t.raw("A"), t.raw("B")])
                    })
                })
                .view_no_params("Second", |t| {
                    t.concat(vec![t.bool_match_expr(
                        t.bool(true),
                        t.concat(vec![t.raw("C"), t.raw("D")]),
                        t.concat(vec![]),
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                view First() {
                  let v0 = "x" in concat(raw("A"), raw("B"))
                }
                view Second() {
                  concat(
                    match true {
                      true => concat(raw("C"), raw("D")),
                      false => concat(),
                    },
                  )
                }

                -- after --
                view First() {
                  concat(raw("AB"))
                }
                view Second() {
                  concat(raw("CD"))
                }
            "#]],
        );
    }

    #[test]
    fn should_apply_constant_propagation_before_unused_let_elimination() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("flag", t.bool(true), |t| {
                        t.concat(vec![t.bool_match_expr(
                            t.var("flag"),
                            t.concat(vec![t.raw("yes")]),
                            t.concat(vec![]),
                        )])
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in concat(
                    match v0 {
                      true => concat(raw("yes")),
                      false => concat(),
                    },
                  )
                }

                -- after --
                view Test() {
                  concat(raw("yes"))
                }
            "#]],
        );
    }

    #[test]
    fn should_chain_multiple_optimizations() {
        // let x = "hello"
        // let unused = x  -- unused, should be eliminated
        // match true { .. }  -- selected, arms coalesced
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("x", t.str("hello"), |t| {
                        t.let_expr("unused", t.var("x"), |t| {
                            t.concat(vec![t.bool_match_expr(
                                t.bool(true),
                                t.concat(vec![t.raw("A"), t.raw("B")]),
                                t.concat(vec![]),
                            )])
                        })
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "hello" in let v1 = v0 in concat(
                    match true {
                      true => concat(raw("A"), raw("B")),
                      false => concat(),
                    },
                  )
                }

                -- after --
                view Test() {
                  concat(raw("AB"))
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_records_and_enums() {
        check(
            PureModuleBuilder::new()
                .record("User", [("name", "String"), ("age", "Int")])
                .enum_unit("Status", ["Active", "Inactive"])
                .view_no_params("Test", |t| t.concat(vec![t.raw("Hello")]))
                .build(),
            expect![[r#"
                -- before --
                enum Status {
                  Active,
                  Inactive,
                }
                record User {
                  name: String,
                  age: Int,
                }
                view Test() {
                  concat(raw("Hello"))
                }

                -- after --
                enum Status {
                  Active,
                  Inactive,
                }
                record User {
                  name: String,
                  age: Int,
                }
                view Test() {
                  concat(raw("Hello"))
                }
            "#]],
        );
    }

    #[test]
    fn should_escape_propagated_constants_at_compile_time() {
        // Partial evaluation inlines the constant, elimination drops the
        // let, and normalization escapes and merges the result.
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("name", t.str("<Ada>"), |t| {
                        t.concat(vec![t.raw("<p>"), t.escape(t.var("name")), t.raw("</p>")])
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "<Ada>" in concat(
                    raw("<p>"),
                    escape(v0),
                    raw("</p>"),
                  )
                }

                -- after --
                view Test() {
                  concat(raw("<p>&lt;Ada&gt;</p>"))
                }
            "#]],
        );
    }
}
