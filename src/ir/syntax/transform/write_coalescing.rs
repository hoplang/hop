use crate::ir::ast::{IrComponentDeclaration, IrStatement, StatementId};

use super::Pass;

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

    /// Transform a list of statements, coalescing consecutive Write statements
    fn transform_statements(&self, statements: Vec<IrStatement>) -> Vec<IrStatement> {
        let mut result = Vec::new();
        let mut pending_write: Option<(StatementId, String)> = None;

        for statement in statements {
            match statement {
                IrStatement::Write { id, content: text } => {
                    // Accumulate consecutive writes if within limit
                    match pending_write {
                        Some((pending_id, ref accumulated)) => {
                            if accumulated.len() + text.len() < self.limit {
                                // Safe to merge
                                pending_write.as_mut().unwrap().1.push_str(&text);
                            } else {
                                // Would exceed limit, flush pending and start new
                                result.push(IrStatement::Write {
                                    id: pending_id,
                                    content: accumulated.clone(),
                                });
                                pending_write = Some((id, text));
                            }
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
                        body: self.transform_statements(body),
                        else_body: else_body.map(|b| self.transform_statements(b)),
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
                        body: self.transform_statements(body),
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
                        body: self.transform_statements(body),
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
                IrStatement::Match { id, match_ } => {
                    // Flush any pending write before a control flow statement
                    if let Some((write_id, text)) = pending_write.take() {
                        result.push(IrStatement::Write {
                            id: write_id,
                            content: text,
                        });
                    }
                    // Recursively transform all match bodies
                    let transformed_match = match match_ {
                        crate::dop::patterns::Match::Bool {
                            subject,
                            true_body,
                            false_body,
                        } => crate::dop::patterns::Match::Bool {
                            subject,
                            true_body: Box::new(self.transform_statements(*true_body)),
                            false_body: Box::new(self.transform_statements(*false_body)),
                        },
                        crate::dop::patterns::Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body,
                            none_arm_body,
                        } => crate::dop::patterns::Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body: Box::new(self.transform_statements(*some_arm_body)),
                            none_arm_body: Box::new(self.transform_statements(*none_arm_body)),
                        },
                        crate::dop::patterns::Match::Enum { subject, arms } => {
                            crate::dop::patterns::Match::Enum {
                                subject,
                                arms: arms
                                    .into_iter()
                                    .map(|arm| crate::dop::patterns::EnumMatchArm {
                                        pattern: arm.pattern,
                                        bindings: arm.bindings,
                                        body: self.transform_statements(arm.body),
                                    })
                                    .collect(),
                            }
                        }
                    };
                    result.push(IrStatement::Match {
                        id,
                        match_: transformed_match,
                    });
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

    /// Run the pass on a component with the configured limit
    pub fn run_with_limit(&self, mut entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        entrypoint.body = self.transform_statements(entrypoint.body);
        entrypoint
    }
}

impl Default for WriteCoalescingPass {
    fn default() -> Self {
        Self { limit: usize::MAX }
    }
}

impl Pass for WriteCoalescingPass {
    fn run(entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        Self::default().run_with_limit(entrypoint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{dop::Type, ir::syntax::builder::build_ir};
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [("x", Type::String)], |t| {
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
            build_ir("Test", [], |_| {}),
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
            build_ir("Test", [], |t| {
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

    fn check_with_limit(entrypoint: IrComponentDeclaration, limit: usize, expected: Expect) {
        let before = entrypoint.to_string();
        let pass = WriteCoalescingPass::with_limit(limit);
        let result = pass.run_with_limit(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn limit_prevents_merge_when_exceeded() {
        // "Hello" (5) + " " (1) = 6, which is >= limit of 6, so no merge
        // " " (1) + "World" (5) = 6, which is >= limit of 6, so no merge
        check_with_limit(
            build_ir("Test", [], |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
            }),
            6,
            expect![[r#"
                -- before --
                Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                }

                -- after --
                Test() {
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
            build_ir("Test", [], |t| {
                t.write("Hello");
                t.write(" ");
                t.write("World");
            }),
            7,
            expect![[r#"
                -- before --
                Test() {
                  write("Hello")
                  write(" ")
                  write("World")
                }

                -- after --
                Test() {
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
            build_ir("Test", [], |t| {
                t.write("AAA");
                t.write("BBB");
                t.write("CCC");
                t.write("DDD");
            }),
            5,
            expect![[r#"
                -- before --
                Test() {
                  write("AAA")
                  write("BBB")
                  write("CCC")
                  write("DDD")
                }

                -- after --
                Test() {
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
            build_ir("Test", [], |t| {
                t.write("AAA");
                t.write("BBB");
                t.write("CCC");
                t.write("DDD");
            }),
            7,
            expect![[r#"
                -- before --
                Test() {
                  write("AAA")
                  write("BBB")
                  write("CCC")
                  write("DDD")
                }

                -- after --
                Test() {
                  write("AAABBB")
                  write("CCCDDD")
                }
            "#]],
        );
    }

    #[test]
    fn limit_zero_prevents_all_merges() {
        check_with_limit(
            build_ir("Test", [], |t| {
                t.write("A");
                t.write("B");
            }),
            0,
            expect![[r#"
                -- before --
                Test() {
                  write("A")
                  write("B")
                }

                -- after --
                Test() {
                  write("A")
                  write("B")
                }
            "#]],
        );
    }

    #[test]
    fn limit_applies_inside_nested_structures() {
        check_with_limit(
            build_ir("Test", [], |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write("AAA");
                    t.write("BBB");
                    t.write("CCC");
                });
            }),
            7,
            expect![[r#"
                -- before --
                Test() {
                  if true {
                    write("AAA")
                    write("BBB")
                    write("CCC")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("AAABBB")
                    write("CCC")
                  }
                }
            "#]],
        );
    }
}
