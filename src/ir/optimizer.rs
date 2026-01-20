use super::syntax::ast::IrModule;
use super::syntax::transform::{
    ConstantPropagationPass, Pass, UnusedIfEliminationPass, UnusedLetEliminationPass,
    WriteCoalescingPass, WriteExprSimplificationPass,
};

pub fn optimize(mut module: IrModule) -> IrModule {
    module.components = module
        .components
        .into_iter()
        .map(|component| {
            let component = ConstantPropagationPass::run(component);
            let component = UnusedLetEliminationPass::run(component);
            let component = UnusedIfEliminationPass::run(component);
            let component = WriteExprSimplificationPass::run(component);
            let component = WriteCoalescingPass::with_limit(60).run_with_limit(component);
            component
        })
        .collect();
    module
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::syntax::builder::IrModuleBuilder;
    use expect_test::{Expect, expect};

    fn check(module: IrModule, expected: Expect) {
        let before = module.to_string();
        let result = optimize(module);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_optimize_single_component() {
        let module = IrModuleBuilder::new()
            .component("Test", [], |t| {
                t.let_stmt("unused", t.str("value"), |t| {
                    t.write("Hello");
                    t.write(" ");
                    t.write("World");
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                Test() {
                  let unused = "value" in {
                    write("Hello")
                    write(" ")
                    write("World")
                  }
                }

                -- after --
                Test() {
                  write("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn should_optimize_multiple_components() {
        let module = IrModuleBuilder::new()
            .component("First", [], |t| {
                t.let_stmt("unused", t.str("x"), |t| {
                    t.write("A");
                    t.write("B");
                });
            })
            .component("Second", [], |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("C");
                    t.write("D");
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                First() {
                  let unused = "x" in {
                    write("A")
                    write("B")
                  }
                }
                Second() {
                  if true {
                    write("C")
                    write("D")
                  }
                }

                -- after --
                First() {
                  write("AB")
                }
                Second() {
                  write("CD")
                }
            "#]],
        );
    }

    #[test]
    fn should_apply_constant_propagation_before_unused_let_elimination() {
        let module = IrModuleBuilder::new()
            .component("Test", [], |t| {
                t.let_stmt("flag", t.bool(true), |t| {
                    t.if_stmt(t.var("flag"), |t| {
                        t.write("yes");
                    });
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                Test() {
                  let flag = true in {
                    if flag {
                      write("yes")
                    }
                  }
                }

                -- after --
                Test() {
                  write("yes")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_records_and_enums() {
        let module = IrModuleBuilder::new()
            .record("User", |r| {
                r.field("name", Type::String);
                r.field("age", Type::Int);
            })
            .enum_decl("Status", ["Active", "Inactive"])
            .component("Test", [], |t| {
                t.write("Hello");
            })
            .build();

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
                Test() {
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
                Test() {
                  write("Hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_chain_multiple_optimizations() {
        let module = IrModuleBuilder::new()
            .component("Test", [], |t| {
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
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                Test() {
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
                Test() {
                  write("AB")
                }
            "#]],
        );
    }
}
