use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypoint, IrNode},
    expr::IrExprValue,
};

/// A pass that eliminates dead code, particularly unreachable If branches
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Check if an expression is a constant boolean value
    fn is_constant_boolean(expr: &IrExpr) -> Option<bool> {
        match &expr.value {
            IrExprValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    /// Transform a list of IR nodes, eliminating dead code
    fn transform_nodes(nodes: Vec<IrNode>) -> Vec<IrNode> {
        let mut result = Vec::new();
        for node in nodes {
            match node {
                IrNode::If { condition, body } => {
                    // Check if the condition is a constant boolean
                    if let Some(const_bool) = Self::is_constant_boolean(&condition) {
                        if const_bool {
                            // Condition is always true, replace with body
                            result.extend(Self::transform_nodes(body));
                        }
                        // If false, do nothing - effectively removes this node
                    } else {
                        // Can't evaluate at compile time, keep the if but transform body
                        result.push(IrNode::If {
                            condition,
                            body: Self::transform_nodes(body),
                        });
                    }
                }
                IrNode::For { var, array, body } => {
                    // Transform the body of the for loop
                    result.push(IrNode::For {
                        var,
                        array,
                        body: Self::transform_nodes(body),
                    });
                }
                IrNode::Let { var, value, body } => {
                    // Transform the body of the let
                    result.push(IrNode::Let {
                        var,
                        value,
                        body: Self::transform_nodes(body),
                    });
                }
                _ => {
                    // All other nodes are preserved as-is
                    result.push(node);
                }
            }
        }
        result
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        entrypoint.body = Self::transform_nodes(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use crate::dop::Type;
    use super::*;

    // Helper functions to create IrExpr for tests
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

    #[test]
    fn test_removes_always_true_if() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::If {
                condition: make_boolean(true),
                body: vec![IrNode::Write("Always shown".to_string())],
            }],
        };

        let mut pass = DeadCodeEliminationPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Write("Always shown".to_string())];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_removes_always_false_if() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::If {
                    condition: make_boolean(false),
                    body: vec![IrNode::Write("Never shown".to_string())],
                },
                IrNode::Write("After if".to_string()),
            ],
        };

        let mut pass = DeadCodeEliminationPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Write("After if".to_string())];
        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_preserves_dynamic_conditions() {
        let entrypoint = IrEntrypoint {
            parameters: vec![("show".to_string(), Type::Bool)],
            body: vec![
                IrNode::If {
                    condition: make_var("show"),
                    body: vec![IrNode::Write("Dynamic".to_string())],
                },
                IrNode::If {
                    condition: make_boolean(true),
                    body: vec![IrNode::Write("Static true".to_string())],
                },
                IrNode::If {
                    condition: make_boolean(false),
                    body: vec![IrNode::Write("Static false".to_string())],
                },
            ],
        };

        let mut pass = DeadCodeEliminationPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![
            IrNode::If {
                condition: make_var("show"),
                body: vec![IrNode::Write("Dynamic".to_string())],
            },
            IrNode::Write("Static true".to_string()),
        ];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_nested_dead_code_elimination() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Let {
                var: "x".to_string(),
                value: make_var("value"),
                body: vec![
                    IrNode::If {
                        condition: make_boolean(true),
                        body: vec![IrNode::Write("Inside let and true if".to_string())],
                    },
                    IrNode::If {
                        condition: make_boolean(false),
                        body: vec![IrNode::Write("Never shown".to_string())],
                    },
                ],
            }],
        };

        let mut pass = DeadCodeEliminationPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Let {
            var: "x".to_string(),
            value: make_var("value"),
            body: vec![IrNode::Write("Inside let and true if".to_string())],
        }];

        assert_eq!(result.body, expected);
    }
}