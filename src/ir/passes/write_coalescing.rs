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
    use crate::{dop::Type, ir::test_utils::IrTestBuilder};
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let result = WriteCoalescingPass::run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_coalesce_consecutive_writes() {
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![
            t.write("Hello"),
            t.write(" "),
            t.write("World"),
            t.write("!"),
        ]);

        check(
            entrypoint,
            expect![[r#"
                test() {
                  write("Hello World!")
                }
            "#]],
        );
    }

    #[test]
    fn test_coalesce_with_interruption() {
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![
            t.write("Before"),
            t.write(" if"),
            t.if_stmt(t.bool(true), vec![t.write("Inside if")]),
            t.write("After"),
            t.write(" if"),
        ]);

        check(
            entrypoint,
            expect![[r#"
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
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![t.if_stmt(
            t.bool(true),
            vec![t.write("Line"), t.write(" "), t.write("one")],
        )]);

        check(
            entrypoint,
            expect![[r#"
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
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![t.for_loop("item", t.array(vec![t.str("x")]), |t| {
            vec![
                t.write("Item"),
                t.write(": "),
                t.write_expr(t.var("item"), true),
                t.write(" - "),
                t.write("Done"),
            ]
        })]);

        check(
            entrypoint,
            expect![[r#"
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
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![t.let_stmt("x", t.str("value"), |t| {
            vec![t.write("The"), t.write(" value"), t.write(" is")]
        })]);

        check(
            entrypoint,
            expect![[r#"
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
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![
            t.write("Start"),
            t.write(": "),
            t.if_stmt(
                t.bool(true),
                vec![
                    t.write("In"),
                    t.write(" if"),
                    t.for_loop("i", t.array(vec![t.str("foo")]), |t| {
                        vec![t.write("Loop"), t.write(" body")]
                    }),
                    t.write("After"),
                    t.write(" loop"),
                ],
            ),
            t.write("End"),
            t.write("."),
        ]);

        check(
            entrypoint,
            expect![[r#"
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
        let t = IrTestBuilder::new(vec![("x".to_string(), Type::String)]);

        let entrypoint = t.build(vec![
            t.write("Value"),
            t.write(": "),
            t.write_expr(t.var("x"), true),
            t.write(" - "),
            t.write("done"),
        ]);

        check(
            entrypoint,
            expect![[r#"
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
        let entrypoint = IrEntrypoint {
            name: "test".to_string(),
            parameters: vec![],
            body: vec![],
        };

        check(
            entrypoint,
            expect![[r#"
                test() {}
            "#]],
        );
    }

    #[test]
    fn test_single_write() {
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![t.write("Single")]);

        check(
            entrypoint,
            expect![[r#"
                test() {
                  write("Single")
                }
            "#]],
        );
    }
}
