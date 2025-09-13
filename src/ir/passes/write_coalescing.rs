use crate::ir::ast::{IrEntrypoint, IrNode, NodeId};

use super::Pass;

/// A pass that concatenates consecutive Write nodes into a single Write node
pub struct WriteCoalescingPass;

impl WriteCoalescingPass {
    pub fn new() -> Self {
        Self
    }

    fn next_id(next_id: &mut NodeId) -> NodeId {
        let id = *next_id;
        *next_id += 1;
        id
    }

    /// Transform a list of IR nodes, coalescing consecutive Write nodes
    fn transform_nodes(nodes: Vec<IrNode>, next_id: &mut NodeId) -> Vec<IrNode> {
        let mut result = Vec::new();
        let mut pending_write: Option<String> = None;

        for node in nodes {
            match node {
                IrNode::Write { id: _, content: text } => {
                    // Accumulate consecutive writes
                    match pending_write {
                        Some(ref mut accumulated) => {
                            accumulated.push_str(&text);
                        }
                        None => {
                            pending_write = Some(text);
                        }
                    }
                }
                IrNode::If { id, condition, body } => {
                    // Flush any pending write before a control flow node
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { id: Self::next_id(next_id), content: text });
                    }
                    // Recursively transform the body
                    result.push(IrNode::If {
                        id,
                        condition,
                        body: Self::transform_nodes(body, next_id),
                    });
                }
                IrNode::For { id, var, array, body } => {
                    // Flush any pending write before a control flow node
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { id: Self::next_id(next_id), content: text });
                    }
                    // Recursively transform the body
                    result.push(IrNode::For {
                        id,
                        var,
                        array,
                        body: Self::transform_nodes(body, next_id),
                    });
                }
                IrNode::Let { id, var, value, body } => {
                    // Flush any pending write before a control flow node
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { id: Self::next_id(next_id), content: text });
                    }
                    // Recursively transform the body
                    result.push(IrNode::Let {
                        id,
                        var,
                        value,
                        body: Self::transform_nodes(body, next_id),
                    });
                }
                IrNode::WriteExpr { .. } => {
                    // WriteExpr can't be coalesced with Write nodes
                    // Flush any pending write
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { id: Self::next_id(next_id), content: text });
                    }
                    result.push(node);
                }
            }
        }

        // Flush any remaining pending write
        if let Some(text) = pending_write {
            result.push(IrNode::Write { id: Self::next_id(next_id), content: text });
        }

        result
    }
}

impl Pass for WriteCoalescingPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        let mut next_id = 1;
        entrypoint.body = Self::transform_nodes(entrypoint.body, &mut next_id);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_coalesce_consecutive_writes() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.write("Hello"),
                t.write(" "),
                t.write("World"),
                t.write("!"),
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("Hello World!")
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_coalesce_with_interruption() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.write("Before"),
                t.write(" if"),
                t.if_stmt(
                    t.boolean(true),
                    vec![t.write("Inside if")]
                ),
                t.write("After"),
                t.write(" if"),
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("Before if")
                If(condition: true) {
                  Write("Inside if")
                }
                Write("After if")
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_coalesce_inside_if() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.if_stmt(
                    t.boolean(true),
                    vec![
                        t.write("Line"),
                        t.write(" "),
                        t.write("one"),
                    ]
                )
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("Line one")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_coalesce_inside_for() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.for_loop(
                    "item",
                    t.array(vec![t.str("x")]),
                    vec![
                        t.write("Item"),
                        t.write(": "),
                        t.write_expr(t.var("item"), true),
                        t.write(" - "),
                        t.write("Done"),
                    ]
                )
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                For(var: item, array: ["x"]) {
                  Write("Item: ")
                  WriteExpr(expr: item, escape: true)
                  Write(" - Done")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_coalesce_inside_let() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.let_stmt(
                    "x",
                    t.str("value"),
                    vec![
                        t.write("The"),
                        t.write(" value"),
                        t.write(" is"),
                    ]
                )
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Let(var: x, value: "value") {
                  Write("The value is")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_nested_coalescing() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.write("Start"),
                t.write(": "),
                t.if_stmt(
                    t.boolean(true),
                    vec![
                        t.write("In"),
                        t.write(" if"),
                        t.for_loop(
                            "i",
                            t.array(vec![]),
                            vec![
                                t.write("Loop"),
                                t.write(" body"),
                            ]
                        ),
                        t.write("After"),
                        t.write(" loop"),
                    ]
                ),
                t.write("End"),
                t.write("."),
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("Start: ")
                If(condition: true) {
                  Write("In if")
                  For(var: i, array: []) {
                    Write("Loop body")
                  }
                  Write("After loop")
                }
                Write("End.")
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_no_coalescing_with_write_expr() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                t.write("Value"),
                t.write(": "),
                t.write_expr(t.var("x"), true),
                t.write(" - "),
                t.write("done"),
            ],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("Value: ")
                WriteExpr(expr: x, escape: true)
                Write(" - done")
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_empty_input() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_single_write() {
        let t = IrTestBuilder::new();

        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![t.write("Single")],
        };

        check(
            entrypoint,
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("Single")
              }
            }
        "#]],
        );
    }
}
