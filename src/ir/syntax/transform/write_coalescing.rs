use crate::ir::ast::{IrComponentDeclaration, IrStatement, StatementId};

use super::Pass;

/// A pass that concatenates consecutive Write statements into a single Write statement
#[allow(dead_code)]
pub struct WriteCoalescingPass;

#[allow(dead_code)]
impl WriteCoalescingPass {
    /// Transform a list of statements, coalescing consecutive Write statements
    fn transform_statements(statements: Vec<IrStatement>) -> Vec<IrStatement> {
        let mut result = Vec::new();
        let mut pending_write: Option<(StatementId, String)> = None;

        for statement in statements {
            match statement {
                IrStatement::Write { id, content: text } => {
                    // Accumulate consecutive writes
                    match pending_write {
                        Some((_, ref mut accumulated)) => {
                            accumulated.push_str(&text);
                        }
                        None => {
                            pending_write = Some((id, text));
                        }
                    }
                }
                IrStatement::If {
                    id,
                    condition,
                    body,
                    else_body,
                } => {
                    // Flush any pending write before a control flow statement
                    if let Some((write_id, text)) = pending_write.take() {
                        result.push(IrStatement::Write {
                            id: write_id,
                            content: text,
                        });
                    }
                    // Recursively transform the body
                    result.push(IrStatement::If {
                        id,
                        condition,
                        body: Self::transform_statements(body),
                        else_body: else_body.map(Self::transform_statements),
                    });
                }
                IrStatement::For {
                    id,
                    var,
                    array,
                    body,
                } => {
                    // Flush any pending write before a control flow statement
                    if let Some((write_id, text)) = pending_write.take() {
                        result.push(IrStatement::Write {
                            id: write_id,
                            content: text,
                        });
                    }
                    // Recursively transform the body
                    result.push(IrStatement::For {
                        id,
                        var,
                        array,
                        body: Self::transform_statements(body),
                    });
                }
                IrStatement::Let {
                    id,
                    var,
                    value,
                    body,
                } => {
                    // Flush any pending write before a control flow statement
                    if let Some((write_id, text)) = pending_write.take() {
                        result.push(IrStatement::Write {
                            id: write_id,
                            content: text,
                        });
                    }
                    // Recursively transform the body
                    result.push(IrStatement::Let {
                        id,
                        var,
                        value,
                        body: Self::transform_statements(body),
                    });
                }
                IrStatement::WriteExpr { .. } => {
                    // WriteExpr can't be coalesced with Write statements
                    // Flush any pending write
                    if let Some((write_id, text)) = pending_write.take() {
                        result.push(IrStatement::Write {
                            id: write_id,
                            content: text,
                        });
                    }
                    result.push(statement);
                }
            }
        }

        // Flush any remaining pending write
        if let Some((write_id, text)) = pending_write {
            result.push(IrStatement::Write {
                id: write_id,
                content: text,
            });
        }

        result
    }
}

impl Pass for WriteCoalescingPass {
    fn run(mut entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        entrypoint.body = Self::transform_statements(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{dop::Type, ir::syntax::ir_builder::build_ir};
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrComponentDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        let result = WriteCoalescingPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn coalesce_consecutive_writes() {
        check(
            build_ir("Test", vec![], |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
                t.write("!");
            }),
            expect![[r#"
                -- before --
                Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                  write("!")
                }

                -- after --
                Test() {
                  write("Hello World!")
                }
            "#]],
        );
    }

    #[test]
    fn coalesce_with_interruption() {
        check(
            build_ir("Test", vec![], |t| {
                t.write("Before");
                t.write(" if");
                t.if_stmt(t.bool(true), |t| {
                    t.write("Inside if");
                });
                t.write("After");
                t.write(" if");
            }),
            expect![[r#"
                -- before --
                Test() {
                  write("Before")
                  write(" if")
                  if true {
                    write("Inside if")
                  }
                  write("After")
                  write(" if")
                }

                -- after --
                Test() {
                  write("Before if")
                  if true {
                    write("Inside if")
                  }
                  write("After if")
                }
            "#]],
        );
    }

    #[test]
    fn coalesce_inside_if() {
        check(
            build_ir("Test", vec![], |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("Line");
                    t.write(" ");
                    t.write("one");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if true {
                    write("Line")
                    write(" ")
                    write("one")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Line one")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn coalesce_inside_for() {
        check(
            build_ir("Test", vec![], |t| {
                t.for_loop("item", t.array(vec![t.str("x")]), |t| {
                    t.write("Item");
                    t.write(": ");
                    t.write_expr_escaped(t.var("item"));
                    t.write(" - ");
                    t.write("Done");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  for item in ["x"] {
                    write("Item")
                    write(": ")
                    write_escaped(item)
                    write(" - ")
                    write("Done")
                  }
                }

                -- after --
                Test() {
                  for item in ["x"] {
                    write("Item: ")
                    write_escaped(item)
                    write(" - Done")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn coalesce_inside_let() {
        check(
            build_ir("Test", vec![], |t| {
                t.let_stmt("x", t.str("value"), |t| {
                    t.write("The");
                    t.write(" value");
                    t.write(" is");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = "value" in {
                    write("The")
                    write(" value")
                    write(" is")
                  }
                }

                -- after --
                Test() {
                  let x = "value" in {
                    write("The value is")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn nested_coalescing() {
        check(
            build_ir("Test", vec![], |t| {
                t.write("Start");
                t.write(": ");
                t.if_stmt(t.bool(true), |t| {
                    t.write("In");
                    t.write(" if");
                    t.for_loop("i", t.array(vec![t.str("foo")]), |t| {
                        t.write("Loop");
                        t.write(" body");
                    });
                    t.write("After");
                    t.write(" loop");
                });
                t.write("End");
                t.write(".");
            }),
            expect![[r#"
                -- before --
                Test() {
                  write("Start")
                  write(": ")
                  if true {
                    write("In")
                    write(" if")
                    for i in ["foo"] {
                      write("Loop")
                      write(" body")
                    }
                    write("After")
                    write(" loop")
                  }
                  write("End")
                  write(".")
                }

                -- after --
                Test() {
                  write("Start: ")
                  if true {
                    write("In if")
                    for i in ["foo"] {
                      write("Loop body")
                    }
                    write("After loop")
                  }
                  write("End.")
                }
            "#]],
        );
    }

    #[test]
    fn no_coalescing_with_write_expr() {
        check(
            build_ir("Test", vec![("x", Type::String)], |t| {
                t.write("Value");
                t.write(": ");
                t.write_expr_escaped(t.var("x"));
                t.write(" - ");
                t.write("done");
            }),
            expect![[r#"
                -- before --
                Test(x: String) {
                  write("Value")
                  write(": ")
                  write_escaped(x)
                  write(" - ")
                  write("done")
                }

                -- after --
                Test(x: String) {
                  write("Value: ")
                  write_escaped(x)
                  write(" - done")
                }
            "#]],
        );
    }

    #[test]
    fn empty_input() {
        check(
            build_ir("Test", vec![], |_| {}),
            expect![[r#"
                -- before --
                Test() {}

                -- after --
                Test() {}
            "#]],
        );
    }

    #[test]
    fn single_write() {
        check(
            build_ir("Test", vec![], |t| {
                t.write("Single");
            }),
            expect![[r#"
                -- before --
                Test() {
                  write("Single")
                }

                -- after --
                Test() {
                  write("Single")
                }
            "#]],
        );
    }
}
