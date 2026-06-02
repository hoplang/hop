use crate::ir::{
    IrExpr,
    ast::{IrStatement, traverse_statements_mut},
};

/// A pass that eliminates if statements with constant conditions
pub struct IfStatementEliminationPass;

impl IfStatementEliminationPass {
    pub fn run(body: &mut Vec<IrStatement>) {
        traverse_statements_mut(body, &mut |stmts| {
            let mut transformed = Vec::new();
            for stmt in std::mem::take(stmts) {
                match stmt {
                    IrStatement::If {
                        condition: IrExpr::BooleanLiteral { value: true, .. },
                        body,
                        ..
                    } => transformed.extend(body),
                    IrStatement::If {
                        condition: IrExpr::BooleanLiteral { value: false, .. },
                        ..
                    } => {}
                    other => transformed.push(other),
                }
            }
            *stmts = transformed;
        });
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::dop::Type;
    use crate::ir::ast::IrViewDeclaration;
    use crate::ir::syntax::builder::{build_ir, build_ir_no_params};
    use expect_test::{Expect, expect};

    fn check(mut view: IrViewDeclaration, expected: Expect) {
        let before = view.to_string();
        IfStatementEliminationPass::run(&mut view.body);
        let after = view.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_eliminate_if_statement_that_is_always_true() {
        check(
            build_ir_no_params("Test", |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("Always shown");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  if true {
                    write("Always shown")
                  }
                }

                -- after --
                view Test() {
                  write("Always shown")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_if_statement_that_is_always_false() {
        check(
            build_ir_no_params("Test", |t| {
                t.if_stmt(t.bool(false), |t| {
                    t.write("Never shown");
                });
                t.write("After if");
            }),
            expect![[r#"
                -- before --
                view Test() {
                  if false {
                    write("Never shown")
                  }
                  write("After if")
                }

                -- after --
                view Test() {
                  write("After if")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_if_statement_with_dynamic_conditions() {
        check(
            build_ir("Test", [("show", Type::Bool)], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("Dynamic");
                });
                t.if_stmt(t.bool(true), |t| {
                    t.write("Static true");
                });
                t.if_stmt(t.bool(false), |t| {
                    t.write("Static false");
                });
            }),
            expect![[r#"
                -- before --
                view Test(show: Bool) {
                  if show {
                    write("Dynamic")
                  }
                  if true {
                    write("Static true")
                  }
                  if false {
                    write("Static false")
                  }
                }

                -- after --
                view Test(show: Bool) {
                  if show {
                    write("Dynamic")
                  }
                  write("Static true")
                }
            "#]],
        );
    }

    #[test]
    fn should_handle_elimination_of_nested_if_statements() {
        check(
            build_ir("Test", [("condition", Type::Bool)], |t| {
                t.if_stmt(t.var("condition"), |t| {
                    t.write("Before nested");
                    t.if_stmt(t.bool(true), |t| {
                        t.write("Nested always true");
                    });
                    t.if_stmt(t.bool(false), |t| {
                        t.write("Nested never shown");
                    });
                    t.write("After nested");
                });
                t.if_stmt(t.bool(true), |t| {
                    t.write("Outer true - before nested");
                    t.if_stmt(t.bool(false), |t| {
                        t.write("Inner false - never shown");
                    });
                    t.if_stmt(t.var("condition"), |t| {
                        t.write("Inner dynamic");
                    });
                    t.write("Outer true - after nested");
                });
            }),
            expect![[r#"
                -- before --
                view Test(condition: Bool) {
                  if condition {
                    write("Before nested")
                    if true {
                      write("Nested always true")
                    }
                    if false {
                      write("Nested never shown")
                    }
                    write("After nested")
                  }
                  if true {
                    write("Outer true - before nested")
                    if false {
                      write("Inner false - never shown")
                    }
                    if condition {
                      write("Inner dynamic")
                    }
                    write("Outer true - after nested")
                  }
                }

                -- after --
                view Test(condition: Bool) {
                  if condition {
                    write("Before nested")
                    write("Nested always true")
                    write("After nested")
                  }
                  write("Outer true - before nested")
                  if condition {
                    write("Inner dynamic")
                  }
                  write("Outer true - after nested")
                }
            "#]],
        );
    }
}
