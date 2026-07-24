use super::syntax::ast::{IrModule, IrStatement};
use crate::expr::typing::type_registry::TypeRegistry;
use crate::ir::syntax::transform::{
    coalesce_write_statements, eliminate_if_statements, eliminate_match_statements,
    eliminate_unused_variable_declarations, perform_partial_evaluation, simplify_write_exprs,
};

fn optimize_statements(body: &mut Vec<IrStatement>, registry: &TypeRegistry) {
    perform_partial_evaluation(body, registry);
    eliminate_unused_variable_declarations(body);
    eliminate_if_statements(body);
    eliminate_match_statements(body);
    simplify_write_exprs(body);
    coalesce_write_statements(body, 60);
}

pub fn optimize(mut module: IrModule, registry: &TypeRegistry) -> IrModule {
    for view in &mut module.views {
        optimize_statements(&mut view.body, registry);
    }
    for component in &mut module.components {
        optimize_statements(&mut component.body, registry);
    }
    module
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use super::*;
    use crate::ir::runtime::{evaluator::evaluate_view, random::random_value, value::Value};
    use crate::ir::syntax::builder::IrModuleBuilder;
    use crate::ir::syntax::random::random_ir_module;
    use crate::symbols::type_name::TypeName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};

    fn check(built: (IrModule, TypeRegistry), expected: Expect) {
        let (module, registry) = built;
        let before = module.to_string();
        let result = optimize(module, &registry);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_optimization() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_ir_module(u);
            let mut rng = StdRng::seed_from_u64(u.arbitrary()?);

            // Generate args up-front so the exact same values are used to
            // evaluate before and after the pipeline.
            let view_args: Vec<(TypeName, HashMap<String, Value>)> = module
                .views
                .iter()
                .map(|view| {
                    let args = view
                        .parameters
                        .iter()
                        .map(|p| {
                            (
                                p.name.as_str().to_string(),
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

            let module = optimize(module, &registry);

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

    #[test]
    fn should_optimize_single_component() {
        let module = IrModuleBuilder::new()
            .view_no_params("Test", |t| {
                t.let_stmt("unused", t.str("value"), |t| {
                    t.write("Hello");
                    t.write(" ");
                    t.write("World");
                });
            })
            .build_with_registry();

        check(
            module,
            expect![[r#"
                -- before --
                view Test() {
                  let unused = "value" in {
                    write("Hello")
                    write(" ")
                    write("World")
                  }
                }

                -- after --
                view Test() {
                  write("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn should_optimize_multiple_components() {
        let module = IrModuleBuilder::new()
            .view_no_params("First", |t| {
                t.let_stmt("unused", t.str("x"), |t| {
                    t.write("A");
                    t.write("B");
                });
            })
            .view_no_params("Second", |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("C");
                    t.write("D");
                });
            })
            .build_with_registry();

        check(
            module,
            expect![[r#"
                -- before --
                view First() {
                  let unused = "x" in {
                    write("A")
                    write("B")
                  }
                }
                view Second() {
                  if true {
                    write("C")
                    write("D")
                  }
                }

                -- after --
                view First() {
                  write("AB")
                }
                view Second() {
                  write("CD")
                }
            "#]],
        );
    }

    #[test]
    fn should_apply_constant_propagation_before_unused_let_elimination() {
        let module = IrModuleBuilder::new()
            .view_no_params("Test", |t| {
                t.let_stmt("flag", t.bool(true), |t| {
                    t.if_stmt(t.var("flag"), |t| {
                        t.write("yes");
                    });
                });
            })
            .build_with_registry();

        check(
            module,
            expect![[r#"
                -- before --
                view Test() {
                  let flag = true in {
                    if flag {
                      write("yes")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("yes")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_bool_match_with_constant_subject() {
        let module = IrModuleBuilder::new()
            .view_no_params("Test", |t| {
                t.let_stmt("flag", t.bool(true), |t| {
                    t.bool_match_stmt(
                        t.var("flag"),
                        |t| {
                            t.write("yes");
                        },
                        |t| {
                            t.write("no");
                        },
                    );
                });
            })
            .build_with_registry();

        check(
            module,
            expect![[r#"
                -- before --
                view Test() {
                  let flag = true in {
                    match flag {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  write("yes")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_records_and_enums() {
        let module = IrModuleBuilder::new()
            .record("User", [("name", "String"), ("age", "Int")])
            .enum_unit("Status", ["Active", "Inactive"])
            .view_no_params("Test", |t| {
                t.write("Hello");
            })
            .build_with_registry();

        check(
            module,
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
                  write("Hello")
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
                  write("Hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_chain_multiple_optimizations() {
        let module = IrModuleBuilder::new()
            .view_no_params("Test", |t| {
                // let x = "hello"
                // let unused = x  -- unused, should be eliminated
                // if true { write("A"); write("B") }  -- if should be eliminated, writes coalesced
                t.let_stmt("x", t.str("hello"), |t| {
                    t.let_stmt("unused", t.var("x"), |t| {
                        t.if_stmt(t.bool(true), |t| {
                            t.write("A");
                            t.write("B");
                        });
                    });
                });
            })
            .build_with_registry();

        check(
            module,
            expect![[r#"
                -- before --
                view Test() {
                  let x = "hello" in {
                    let unused = x in {
                      if true {
                        write("A")
                        write("B")
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  write("AB")
                }
            "#]],
        );
    }
}
