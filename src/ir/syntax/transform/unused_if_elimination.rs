use crate::ir::{
    IrExpr,
    ast::{IrEntrypointDeclaration, IrStatement},
};

/// A pass that eliminates unused if branches with constant conditions
pub struct UnusedIfEliminationPass;

impl UnusedIfEliminationPass {
    fn transform_statements(statements: Vec<IrStatement>) -> Vec<IrStatement> {
        statements
            .into_iter()
            .flat_map(|stmt| match stmt {
                // If with constant true condition - replace with body
                IrStatement::If {
                    condition: IrExpr::BooleanLiteral { value: true, .. },
                    body,
                    ..
                } => body,

                // If with constant false condition - replace with else branch if exists
                IrStatement::If {
                    condition: IrExpr::BooleanLiteral { value: false, .. },
                    else_body,
                    ..
                } => else_body.unwrap_or_else(Vec::new),

                // All other statements (including dynamic If) pass through unchanged
                other => vec![other],
            })
            .collect()
    }

    pub fn run(comp_decl: &mut IrEntrypointDeclaration) {
        // First, recursively process all nested bodies using visit_mut
        for stmt in &mut comp_decl.body {
            stmt.traverse_mut(&mut |s| match s {
                IrStatement::If {
                    body, else_body, ..
                } => {
                    *body = Self::transform_statements(std::mem::take(body));
                    *else_body = else_body.take().map(Self::transform_statements)
                }
                IrStatement::For { body, .. } | IrStatement::Let { body, .. } => {
                    *body = Self::transform_statements(std::mem::take(body));
                }
                _ => {}
            });
        }

        // Then transform top-level statements
        comp_decl.body = Self::transform_statements(std::mem::take(&mut comp_decl.body));
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::dop::Type;
    use crate::ir::syntax::builder::{build_ir, build_ir_no_params};
    use expect_test::{Expect, expect};

    fn check(mut entrypoint: IrEntrypointDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        UnusedIfEliminationPass::run(&mut entrypoint);
        let after = entrypoint.to_string();
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
                Test() {
                  if true {
                    write("Always shown")
                  }
                }

                -- after --
                Test() {
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
                Test() {
                  if false {
                    write("Never shown")
                  }
                  write("After if")
                }

                -- after --
                Test() {
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
                Test(show: Bool) {
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
                Test(show: Bool) {
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
                Test(condition: Bool) {
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
                Test(condition: Bool) {
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
