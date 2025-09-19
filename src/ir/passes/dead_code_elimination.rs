use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypoint, IrStatement},
};

/// A pass that eliminates dead code, particularly unreachable If branches
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
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

                // If with constant false condition - remove entirely
                IrStatement::If {
                    condition: IrExpr::BooleanLiteral { value: false, .. },
                    ..
                } => vec![],

                // All other statements (including dynamic If) pass through unchanged
                other => vec![other],
            })
            .collect()
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        // First, recursively process all nested bodies using visit_mut
        for stmt in &mut entrypoint.body {
            stmt.traverse_mut(&mut |s| match s {
                IrStatement::If { body, .. }
                | IrStatement::For { body, .. }
                | IrStatement::Let { body, .. } => {
                    *body = Self::transform_statements(std::mem::take(body));
                }
                _ => {}
            });
        }

        // Then transform top-level statements
        entrypoint.body = Self::transform_statements(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let before = entrypoint.to_string();
        let result = DeadCodeEliminationPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_removes_always_true_if() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("Always shown");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if true {
                    write("Always shown")
                  }
                }

                -- after --
                test() {
                  write("Always shown")
                }
            "#]],
        );
    }

    #[test]
    fn test_removes_always_false_if() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.bool(false), |t| {
                    t.write("Never shown");
                });
                t.write("After if");
            }),
            expect![[r#"
                -- before --
                test() {
                  if false {
                    write("Never shown")
                  }
                  write("After if")
                }

                -- after --
                test() {
                  write("After if")
                }
            "#]],
        );
    }

    #[test]
    fn test_preserves_dynamic_conditions() {
        check(
            build_ir_auto("test", vec![("show", Type::Bool)], |t| {
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
                test(show: boolean) {
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
                test(show: boolean) {
                  if show {
                    write("Dynamic")
                  }
                  write("Static true")
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_if_elimination() {
        check(
            build_ir_auto("test", vec![("condition", Type::Bool)], |t| {
                t.if_stmt(
                    t.var("condition"),
                    |t| {
                        t.write("Before nested");
                        t.if_stmt(t.bool(true), |t| {
                            t.write("Nested always true");
                        });
                        t.if_stmt(t.bool(false), |t| {
                            t.write("Nested never shown");
                        });
                        t.write("After nested");
                    },
                );
                t.if_stmt(
                    t.bool(true),
                    |t| {
                        t.write("Outer true - before nested");
                        t.if_stmt(t.bool(false), |t| {
                            t.write("Inner false - never shown");
                        });
                        t.if_stmt(t.var("condition"), |t| {
                            t.write("Inner dynamic");
                        });
                        t.write("Outer true - after nested");
                    },
                );
            }),
            expect![[r#"
                -- before --
                test(condition: boolean) {
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
                test(condition: boolean) {
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
