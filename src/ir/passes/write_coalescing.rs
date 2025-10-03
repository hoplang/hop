use crate::ir::ast::{IrEntrypoint, IrStatement, StatementId};

use super::Pass;

/// A pass that concatenates consecutive Write statements into a single Write statement
pub struct WriteCoalescingPass;

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
    fn run(mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        entrypoint.body = Self::transform_statements(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{dop::Type, ir::test_utils::build_ir_auto};
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let before = entrypoint.to_string();
        let result = WriteCoalescingPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_coalesce_consecutive_writes() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
                t.write("!");
            }),
            expect![[r#"
                -- before --
                test() {
                  write("Hello")
                  write(" ")
                  write("World")
                  write("!")
                }

                -- after --
                test() {
                  write("Hello World!")
                }
            "#]],
        );
    }

    #[test]
    fn test_coalesce_with_interruption() {
        check(
            build_ir_auto("test", vec![], |t| {
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
                test() {
                  write("Before")
                  write(" if")
                  if true {
                    write("Inside if")
                  }
                  write("After")
                  write(" if")
                }

                -- after --
                test() {
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
    fn test_coalesce_inside_if() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("Line");
                    t.write(" ");
                    t.write("one");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if true {
                    write("Line")
                    write(" ")
                    write("one")
                  }
                }

                -- after --
                test() {
                  if true {
                    write("Line one")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_coalesce_inside_for() {
        check(
            build_ir_auto("test", vec![], |t| {
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
                test() {
                  for item in ["x"] {
                    write("Item")
                    write(": ")
                    write_escaped(item)
                    write(" - ")
                    write("Done")
                  }
                }

                -- after --
                test() {
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
    fn test_coalesce_inside_let() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("x", t.str("value"), |t| {
                    t.write("The");
                    t.write(" value");
                    t.write(" is");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let x = "value" in {
                    write("The")
                    write(" value")
                    write(" is")
                  }
                }

                -- after --
                test() {
                  let x = "value" in {
                    write("The value is")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_coalescing() {
        check(
            build_ir_auto("test", vec![], |t| {
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
                test() {
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
                test() {
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
    fn test_no_coalescing_with_write_expr() {
        check(
            build_ir_auto("test", vec![("x", Type::String)], |t| {
                t.write("Value");
                t.write(": ");
                t.write_expr_escaped(t.var("x"));
                t.write(" - ");
                t.write("done");
            }),
            expect![[r#"
                -- before --
                test(x: string) {
                  write("Value")
                  write(": ")
                  write_escaped(x)
                  write(" - ")
                  write("done")
                }

                -- after --
                test(x: string) {
                  write("Value: ")
                  write_escaped(x)
                  write(" - done")
                }
            "#]],
        );
    }

    #[test]
    fn test_empty_input() {
        check(
            build_ir_auto("test", vec![], |_| {}),
            expect![[r#"
                -- before --
                test() {}

                -- after --
                test() {}
            "#]],
        );
    }

    #[test]
    fn test_single_write() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.write("Single");
            }),
            expect![[r#"
                -- before --
                test() {
                  write("Single")
                }

                -- after --
                test() {
                  write("Single")
                }
            "#]],
        );
    }
}
