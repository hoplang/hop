use crate::ir::ast::{IrStatement, StatementId, traverse_statements_mut};

/// A pass that concatenates consecutive Write statements into a single Write statement.
///
/// The `limit` parameter controls the maximum combined length of merged writes.
/// Two writes A and B will only be merged if `len(A) + len(B) < limit`.
pub struct WriteCoalescingPass {
    limit: usize,
}

impl WriteCoalescingPass {
    /// Create a new WriteCoalescingPass with the specified limit.
    /// Two writes will only be merged if their combined length is less than the limit.
    pub fn with_limit(limit: usize) -> Self {
        Self { limit }
    }

    pub fn run(&self, body: &mut Vec<IrStatement>) {
        traverse_statements_mut(body, &mut |stmts| {
            let statements = std::mem::take(stmts);
            let mut result = Vec::new();
            let mut pending_write: Option<(StatementId, String)> = None;

            for statement in statements {
                match statement {
                    IrStatement::Write { id, content: text } => match pending_write {
                        Some((pending_id, ref mut accumulated)) => {
                            if accumulated.len() + text.len() < self.limit {
                                accumulated.push_str(&text);
                            } else {
                                result.push(IrStatement::Write {
                                    id: pending_id,
                                    content: std::mem::take(accumulated),
                                });
                                pending_write = Some((id, text));
                            }
                        }
                        None => {
                            pending_write = Some((id, text));
                        }
                    },
                    other => {
                        if let Some((write_id, text)) = pending_write.take() {
                            result.push(IrStatement::Write {
                                id: write_id,
                                content: text,
                            });
                        }
                        result.push(other);
                    }
                }
            }

            if let Some((write_id, text)) = pending_write {
                result.push(IrStatement::Write {
                    id: write_id,
                    content: text,
                });
            }

            *stmts = result;
        });
    }
}

impl Default for WriteCoalescingPass {
    fn default() -> Self {
        Self { limit: usize::MAX }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{
        ast::IrViewDeclaration,
        syntax::builder::{build_ir, build_ir_no_params},
    };
    use expect_test::{Expect, expect};

    fn check(mut view: IrViewDeclaration, expected: Expect) {
        let before = view.to_string();
        WriteCoalescingPass::default().run(&mut view.body);
        let after = view.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn coalesce_consecutive_writes() {
        check(
            build_ir_no_params("Test", |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
                t.write("!");
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                  write("!")
                }

                -- after --
                view Test() {
                  write("Hello World!")
                }
            "#]],
        );
    }

    #[test]
    fn coalesce_with_interruption() {
        check(
            build_ir_no_params("Test", |t| {
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
                view Test() {
                  write("Before")
                  write(" if")
                  if true {
                    write("Inside if")
                  }
                  write("After")
                  write(" if")
                }

                -- after --
                view Test() {
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
            build_ir_no_params("Test", |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("Line");
                    t.write(" ");
                    t.write("one");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  if true {
                    write("Line")
                    write(" ")
                    write("one")
                  }
                }

                -- after --
                view Test() {
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
            build_ir_no_params("Test", |t| {
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
                view Test() {
                  for item in ["x"] {
                    write("Item")
                    write(": ")
                    write_escaped(item)
                    write(" - ")
                    write("Done")
                  }
                }

                -- after --
                view Test() {
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
            build_ir_no_params("Test", |t| {
                t.let_stmt("x", t.str("value"), |t| {
                    t.write("The");
                    t.write(" value");
                    t.write(" is");
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let x = "value" in {
                    write("The")
                    write(" value")
                    write(" is")
                  }
                }

                -- after --
                view Test() {
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
            build_ir_no_params("Test", |t| {
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
                view Test() {
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
                view Test() {
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
            build_ir("Test", [("x", "String")], |t| {
                t.write("Value");
                t.write(": ");
                t.write_expr_escaped(t.var("x"));
                t.write(" - ");
                t.write("done");
            }),
            expect![[r#"
                -- before --
                view Test(x: String) {
                  write("Value")
                  write(": ")
                  write_escaped(x)
                  write(" - ")
                  write("done")
                }

                -- after --
                view Test(x: String) {
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
            build_ir_no_params("Test", |_| {}),
            expect![[r#"
                -- before --
                view Test() {}

                -- after --
                view Test() {}
            "#]],
        );
    }

    #[test]
    fn single_write() {
        check(
            build_ir_no_params("Test", |t| {
                t.write("Single");
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write("Single")
                }

                -- after --
                view Test() {
                  write("Single")
                }
            "#]],
        );
    }

    fn check_with_limit(mut view: IrViewDeclaration, limit: usize, expected: Expect) {
        let before = view.to_string();
        let pass = WriteCoalescingPass::with_limit(limit);
        pass.run(&mut view.body);
        let after = view.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn limit_prevents_merge_when_exceeded() {
        // "Hello" (5) + " " (1) = 6, which is >= limit of 6, so no merge
        // " " (1) + "World" (5) = 6, which is >= limit of 6, so no merge
        check_with_limit(
            build_ir_no_params("Test", |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
            }),
            6,
            expect![[r#"
                -- before --
                view Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                }

                -- after --
                view Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                }
            "#]],
        );
    }

    #[test]
    fn limit_allows_merge_when_under() {
        // "Hello" (5) + " " (1) = 6, which is < limit of 7, so merge to "Hello " (6)
        // "Hello " (6) + "World" (5) = 11, which is >= limit of 7, so no merge
        check_with_limit(
            build_ir_no_params("Test", |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
            }),
            7,
            expect![[r#"
                -- before --
                view Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                }

                -- after --
                view Test() {
                  write("Hello ")
                  write("World")
                }
            "#]],
        );
    }

    #[test]
    fn limit_creates_multiple_chunks() {
        // With limit 5, we can't merge anything since each write is 3 chars
        // "AAA" + "BBB" = 6 >= 5, so no merge
        check_with_limit(
            build_ir_no_params("Test", |t| {
                t.write("AAA");
                t.write("BBB");
                t.write("CCC");
                t.write("DDD");
            }),
            5,
            expect![[r#"
                -- before --
                view Test() {
                  write("AAA")
                  write("BBB")
                  write("CCC")
                  write("DDD")
                }

                -- after --
                view Test() {
                  write("AAA")
                  write("BBB")
                  write("CCC")
                  write("DDD")
                }
            "#]],
        );
    }

    #[test]
    fn limit_allows_partial_merges() {
        // With limit 7: "AAA" (3) + "BBB" (3) = 6 < 7, merge to "AAABBB"
        // Then "AAABBB" (6) + "CCC" (3) = 9 >= 7, flush and start new
        // "CCC" (3) + "DDD" (3) = 6 < 7, merge to "CCCDDD"
        check_with_limit(
            build_ir_no_params("Test", |t| {
                t.write("AAA");
                t.write("BBB");
                t.write("CCC");
                t.write("DDD");
            }),
            7,
            expect![[r#"
                -- before --
                view Test() {
                  write("AAA")
                  write("BBB")
                  write("CCC")
                  write("DDD")
                }

                -- after --
                view Test() {
                  write("AAABBB")
                  write("CCCDDD")
                }
            "#]],
        );
    }

    #[test]
    fn limit_zero_prevents_all_merges() {
        check_with_limit(
            build_ir_no_params("Test", |t| {
                t.write("A");
                t.write("B");
            }),
            0,
            expect![[r#"
                -- before --
                view Test() {
                  write("A")
                  write("B")
                }

                -- after --
                view Test() {
                  write("A")
                  write("B")
                }
            "#]],
        );
    }

    #[test]
    fn limit_applies_inside_nested_structures() {
        check_with_limit(
            build_ir_no_params("Test", |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("AAA");
                    t.write("BBB");
                    t.write("CCC");
                });
            }),
            7,
            expect![[r#"
                -- before --
                view Test() {
                  if true {
                    write("AAA")
                    write("BBB")
                    write("CCC")
                  }
                }

                -- after --
                view Test() {
                  if true {
                    write("AAABBB")
                    write("CCC")
                  }
                }
            "#]],
        );
    }
}
