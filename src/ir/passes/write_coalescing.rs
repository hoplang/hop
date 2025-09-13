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
                IrNode::Write(text) => {
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
                        result.push(IrNode::Write(text));
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
                        result.push(IrNode::Write(text));
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
                        result.push(IrNode::Write(text));
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
                        result.push(IrNode::Write(text));
                    }
                    result.push(node);
                }
            }
        }

        // Flush any remaining pending write
        if let Some(text) = pending_write {
            result.push(IrNode::Write(text));
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
    use crate::ir::{IrExpr, expr::IrExprValue};

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

    #[test]
    fn test_coalesce_consecutive_writes() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write("Hello".to_string()),
                IrNode::Write(" ".to_string()),
                IrNode::Write("World".to_string()),
                IrNode::Write("!".to_string()),
            ],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Write("Hello World!".to_string())];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_coalesce_with_interruption() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write("Before".to_string()),
                IrNode::Write(" if".to_string()),
                IrNode::If {
                    condition: make_boolean(true),
                    body: vec![IrNode::Write("Inside if".to_string())],
                },
                IrNode::Write("After".to_string()),
                IrNode::Write(" if".to_string()),
            ],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![
            IrNode::Write("Before if".to_string()),
            IrNode::If {
                condition: make_boolean(true),
                body: vec![IrNode::Write("Inside if".to_string())],
            },
            IrNode::Write("After if".to_string()),
        ];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_coalesce_inside_if() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::If {
                condition: make_boolean(true),
                body: vec![
                    IrNode::Write("Line".to_string()),
                    IrNode::Write(" ".to_string()),
                    IrNode::Write("one".to_string()),
                ],
            }],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::If {
            condition: make_boolean(true),
            body: vec![IrNode::Write("Line one".to_string())],
        }];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_coalesce_inside_for() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::For {
                var: "item".to_string(),
                array: make_array(vec![make_string("x")]),
                body: vec![
                    IrNode::Write("Item".to_string()),
                    IrNode::Write(": ".to_string()),
                    IrNode::WriteExpr {
                        expr: make_var("item"),
                        escape: true,
                    },
                    IrNode::Write(" - ".to_string()),
                    IrNode::Write("Done".to_string()),
                ],
            }],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::For {
            var: "item".to_string(),
            array: make_array(vec![make_string("x")]),
            body: vec![
                IrNode::Write("Item: ".to_string()),
                IrNode::WriteExpr {
                    expr: make_var("item"),
                    escape: true,
                },
                IrNode::Write(" - Done".to_string()),
            ],
        }];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_coalesce_inside_let() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Let {
                var: "x".to_string(),
                value: make_string("value"),
                body: vec![
                    IrNode::Write("The".to_string()),
                    IrNode::Write(" value".to_string()),
                    IrNode::Write(" is".to_string()),
                ],
            }],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Let {
            var: "x".to_string(),
            value: make_string("value"),
            body: vec![IrNode::Write("The value is".to_string())],
        }];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_nested_coalescing() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write("Start".to_string()),
                IrNode::Write(": ".to_string()),
                IrNode::If {
                    condition: make_boolean(true),
                    body: vec![
                        IrNode::Write("In".to_string()),
                        IrNode::Write(" if".to_string()),
                        IrNode::For {
                            var: "i".to_string(),
                            array: make_array(vec![]),
                            body: vec![
                                IrNode::Write("Loop".to_string()),
                                IrNode::Write(" body".to_string()),
                            ],
                        },
                        IrNode::Write("After".to_string()),
                        IrNode::Write(" loop".to_string()),
                    ],
                },
                IrNode::Write("End".to_string()),
                IrNode::Write(".".to_string()),
            ],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![
            IrNode::Write("Start: ".to_string()),
            IrNode::If {
                condition: make_boolean(true),
                body: vec![
                    IrNode::Write("In if".to_string()),
                    IrNode::For {
                        var: "i".to_string(),
                        array: make_array(vec![]),
                        body: vec![IrNode::Write("Loop body".to_string())],
                    },
                    IrNode::Write("After loop".to_string()),
                ],
            },
            IrNode::Write("End.".to_string()),
        ];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_no_coalescing_with_write_expr() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::Write("Value".to_string()),
                IrNode::Write(": ".to_string()),
                IrNode::WriteExpr {
                    expr: make_var("x"),
                    escape: true,
                },
                IrNode::Write(" - ".to_string()),
                IrNode::Write("done".to_string()),
            ],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![
            IrNode::Write("Value: ".to_string()),
            IrNode::WriteExpr {
                expr: make_var("x"),
                escape: true,
            },
            IrNode::Write(" - done".to_string()),
        ];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_empty_input() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        assert_eq!(result.body, vec![]);
    }

    #[test]
    fn test_single_write() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Write("Single".to_string())],
        };

        let mut pass = WriteCoalescingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Write("Single".to_string())];
        assert_eq!(result.body, expected);
    }
}
