use crate::ir::ast::{IrEntrypoint, IrNode};

use super::Pass;

/// A pass that concatenates consecutive Write nodes into a single Write node
pub struct WriteCoalescingPass;

impl WriteCoalescingPass {
    pub fn new() -> Self {
        Self
    }

    /// Transform a list of IR nodes, coalescing consecutive Write nodes
    fn transform_nodes(nodes: Vec<IrNode>) -> Vec<IrNode> {
        let mut result = Vec::new();
        let mut pending_write: Option<String> = None;

        for node in nodes {
            match node {
                IrNode::Write { content: text } => {
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
                IrNode::If { condition, body } => {
                    // Flush any pending write before a control flow node
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { content: text });
                    }
                    // Recursively transform the body
                    result.push(IrNode::If {
                        condition,
                        body: Self::transform_nodes(body),
                    });
                }
                IrNode::For { var, array, body } => {
                    // Flush any pending write before a control flow node
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { content: text });
                    }
                    // Recursively transform the body
                    result.push(IrNode::For {
                        var,
                        array,
                        body: Self::transform_nodes(body),
                    });
                }
                IrNode::Let { var, value, body } => {
                    // Flush any pending write before a control flow node
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { content: text });
                    }
                    // Recursively transform the body
                    result.push(IrNode::Let {
                        var,
                        value,
                        body: Self::transform_nodes(body),
                    });
                }
                IrNode::WriteExpr { .. } => {
                    // WriteExpr can't be coalesced with Write nodes
                    // Flush any pending write
                    if let Some(text) = pending_write.take() {
                        result.push(IrNode::Write { content: text });
                    }
                    result.push(node);
                }
            }
        }

        // Flush any remaining pending write
        if let Some(text) = pending_write {
            result.push(IrNode::Write { content: text });
        }

        result
    }
}

impl Pass for WriteCoalescingPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        entrypoint.body = Self::transform_nodes(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{IrExpr, ast::IrExprValue};
    use expect_test::{Expect, expect};

    // Helper functions to create IrExpr for tests
    fn make_string(s: &str) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::String(s.to_string()),
        }
    }

    fn make_boolean(b: bool) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Boolean(b),
        }
    }

    fn make_var(name: &str) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Var(name.to_string()),
        }
    }

    fn make_array(elements: Vec<IrExpr>) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Array(elements),
        }
    }

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_coalesce_consecutive_writes() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write {
                    content: "Hello".to_string(),
                },
                IrNode::Write {
                    content: " ".to_string(),
                },
                IrNode::Write {
                    content: "World".to_string(),
                },
                IrNode::Write {
                    content: "!".to_string(),
                },
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write {
                    content: "Before".to_string(),
                },
                IrNode::Write {
                    content: " if".to_string(),
                },
                IrNode::If {
                    condition: make_boolean(true),
                    body: vec![IrNode::Write {
                        content: "Inside if".to_string(),
                    }],
                },
                IrNode::Write {
                    content: "After".to_string(),
                },
                IrNode::Write {
                    content: " if".to_string(),
                },
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::If {
                condition: make_boolean(true),
                body: vec![
                    IrNode::Write {
                        content: "Line".to_string(),
                    },
                    IrNode::Write {
                        content: " ".to_string(),
                    },
                    IrNode::Write {
                        content: "one".to_string(),
                    },
                ],
            }],
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::For {
                var: "item".to_string(),
                array: make_array(vec![make_string("x")]),
                body: vec![
                    IrNode::Write {
                        content: "Item".to_string(),
                    },
                    IrNode::Write {
                        content: ": ".to_string(),
                    },
                    IrNode::WriteExpr {
                        expr: make_var("item"),
                        escape: true,
                    },
                    IrNode::Write {
                        content: " - ".to_string(),
                    },
                    IrNode::Write {
                        content: "Done".to_string(),
                    },
                ],
            }],
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Let {
                var: "x".to_string(),
                value: make_string("value"),
                body: vec![
                    IrNode::Write {
                        content: "The".to_string(),
                    },
                    IrNode::Write {
                        content: " value".to_string(),
                    },
                    IrNode::Write {
                        content: " is".to_string(),
                    },
                ],
            }],
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write {
                    content: "Start".to_string(),
                },
                IrNode::Write {
                    content: ": ".to_string(),
                },
                IrNode::If {
                    condition: make_boolean(true),
                    body: vec![
                        IrNode::Write {
                            content: "In".to_string(),
                        },
                        IrNode::Write {
                            content: " if".to_string(),
                        },
                        IrNode::For {
                            var: "i".to_string(),
                            array: make_array(vec![]),
                            body: vec![
                                IrNode::Write {
                                    content: "Loop".to_string(),
                                },
                                IrNode::Write {
                                    content: " body".to_string(),
                                },
                            ],
                        },
                        IrNode::Write {
                            content: "After".to_string(),
                        },
                        IrNode::Write {
                            content: " loop".to_string(),
                        },
                    ],
                },
                IrNode::Write {
                    content: "End".to_string(),
                },
                IrNode::Write {
                    content: ".".to_string(),
                },
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write {
                    content: "Value".to_string(),
                },
                IrNode::Write {
                    content: ": ".to_string(),
                },
                IrNode::WriteExpr {
                    expr: make_var("x"),
                    escape: true,
                },
                IrNode::Write {
                    content: " - ".to_string(),
                },
                IrNode::Write {
                    content: "done".to_string(),
                },
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
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Write {
                content: "Single".to_string(),
            }],
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
