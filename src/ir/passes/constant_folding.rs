use super::Pass;
use crate::ir::{BinaryOp, IrEntrypoint, IrExpr, IrNode, UnaryOp};

/// Represents a compile-time constant value
#[derive(Debug, Clone, PartialEq)]
enum ConstantValue {
    String(String),
    Boolean(bool),
    Number(f64),
    Array(Vec<ConstantValue>),
    Object(Vec<(String, ConstantValue)>),
}

/// A pass that evaluates constant expressions at compile time
pub struct ConstantFoldingPass;

impl ConstantFoldingPass {
    pub fn new() -> Self {
        Self
    }

    /// Try to evaluate a constant expression at compile time
    fn try_eval_constant(expr: &IrExpr) -> Option<ConstantValue> {
        match expr {
            IrExpr::StringLiteral(s) => Some(ConstantValue::String(s.clone())),
            IrExpr::BooleanLiteral(b) => Some(ConstantValue::Boolean(*b)),
            IrExpr::NumberLiteral(n) => Some(ConstantValue::Number(*n)),
            IrExpr::BinaryOp { left, op, right } => {
                let left_val = Self::try_eval_constant(left)?;
                let right_val = Self::try_eval_constant(right)?;
                match op {
                    BinaryOp::Equal => Some(ConstantValue::Boolean(left_val == right_val)),
                }
            }
            IrExpr::UnaryOp { op, operand } => {
                let val = Self::try_eval_constant(operand)?;
                match op {
                    UnaryOp::Not => match val {
                        ConstantValue::Boolean(b) => Some(ConstantValue::Boolean(!b)),
                        _ => None,
                    },
                }
            }
            IrExpr::ArrayLiteral(elements) => {
                let mut const_elements = Vec::new();
                for elem in elements {
                    const_elements.push(Self::try_eval_constant(elem)?);
                }
                Some(ConstantValue::Array(const_elements))
            }
            IrExpr::ObjectLiteral(properties) => {
                let mut const_props = Vec::new();
                for (key, value) in properties {
                    const_props.push((key.clone(), Self::try_eval_constant(value)?));
                }
                Some(ConstantValue::Object(const_props))
            }
            _ => None, // Variables and property access can't be evaluated at compile time
        }
    }

    /// Transform a list of IR nodes, applying constant folding optimizations
    fn transform_nodes(nodes: Vec<IrNode>) -> Vec<IrNode> {
        let mut result = Vec::new();
        for node in nodes {
            match node {
                IrNode::If { condition, body } => {
                    // Try to evaluate the condition at compile time
                    if let Some(const_val) = Self::try_eval_constant(&condition) {
                        match const_val {
                            ConstantValue::Boolean(true) => {
                                // Condition is always true, replace with body
                                result.extend(Self::transform_nodes(body));
                            }
                            ConstantValue::Boolean(false) => {
                                // Condition is always false, remove the entire if
                                // Do nothing - effectively removes this node
                            }
                            _ => {
                                // Non-boolean constant (shouldn't happen with proper typing)
                                result.push(IrNode::If {
                                    condition,
                                    body: Self::transform_nodes(body),
                                });
                            }
                        }
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
                // Write nodes are already constant
                IrNode::Write(_) | IrNode::WriteExpr { .. } => {
                    result.push(node);
                }
            }
        }
        result
    }
}

impl Pass for ConstantFoldingPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        entrypoint.body = Self::transform_nodes(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::IrEntrypoint;

    #[test]
    fn test_constant_folding_removes_always_true_if() {
        // Create an if with condition "x" == "x" (always true)
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::If {
                condition: IrExpr::BinaryOp {
                    left: Box::new(IrExpr::StringLiteral("x".to_string())),
                    op: BinaryOp::Equal,
                    right: Box::new(IrExpr::StringLiteral("x".to_string())),
                },
                body: vec![IrNode::Write("Always shown".to_string())],
            }],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: The If node should be removed and replaced with its body
        let expected = vec![IrNode::Write("Always shown".to_string())];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_removes_always_false_if() {
        // Create an if with condition "x" == "y" (always false)
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::If {
                    condition: IrExpr::BinaryOp {
                        left: Box::new(IrExpr::StringLiteral("x".to_string())),
                        op: BinaryOp::Equal,
                        right: Box::new(IrExpr::StringLiteral("y".to_string())),
                    },
                    body: vec![IrNode::Write("Never shown".to_string())],
                },
                IrNode::Write("After if".to_string()),
            ],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: The If node should be completely removed
        let expected = vec![IrNode::Write("After if".to_string())];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_with_negation() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::If {
                    condition: IrExpr::UnaryOp {
                        op: UnaryOp::Not,
                        operand: Box::new(IrExpr::BooleanLiteral(false)),
                    },
                    body: vec![IrNode::Write("Shown".to_string())],
                },
                IrNode::If {
                    condition: IrExpr::UnaryOp {
                        op: UnaryOp::Not,
                        operand: Box::new(IrExpr::BooleanLiteral(true)),
                    },
                    body: vec![IrNode::Write("Not shown".to_string())],
                },
            ],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: !false -> true (keep body), !true -> false (remove)
        let expected = vec![IrNode::Write("Shown".to_string())];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_preserves_dynamic_conditions() {
        let entrypoint = IrEntrypoint {
            parameters: vec!["show".to_string()],
            body: vec![
                // Dynamic condition - should be preserved
                IrNode::If {
                    condition: IrExpr::Variable("show".to_string()),
                    body: vec![IrNode::Write("Dynamic".to_string())],
                },
                // Static true - should be replaced with body
                IrNode::If {
                    condition: IrExpr::BooleanLiteral(true),
                    body: vec![IrNode::Write("Static true".to_string())],
                },
                // Static false - should be removed
                IrNode::If {
                    condition: IrExpr::BooleanLiteral(false),
                    body: vec![IrNode::Write("Static false".to_string())],
                },
            ],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: Dynamic if preserved, static true replaced, static false removed
        let expected = vec![
            IrNode::If {
                condition: IrExpr::Variable("show".to_string()),
                body: vec![IrNode::Write("Dynamic".to_string())],
            },
            IrNode::Write("Static true".to_string()),
        ];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_in_nested_structures() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Let {
                var: "x".to_string(),
                value: IrExpr::StringLiteral("value".to_string()),
                body: vec![
                    IrNode::If {
                        condition: IrExpr::BooleanLiteral(true),
                        body: vec![IrNode::Write("Inside let and true if".to_string())],
                    },
                    IrNode::If {
                        condition: IrExpr::BooleanLiteral(false),
                        body: vec![IrNode::Write("Never shown".to_string())],
                    },
                ],
            }],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: Let preserved but ifs inside optimized
        let expected = vec![IrNode::Let {
            var: "x".to_string(),
            value: IrExpr::StringLiteral("value".to_string()),
            body: vec![IrNode::Write("Inside let and true if".to_string())],
        }];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_number_equality() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![
                IrNode::If {
                    condition: IrExpr::BinaryOp {
                        left: Box::new(IrExpr::NumberLiteral(42.0)),
                        op: BinaryOp::Equal,
                        right: Box::new(IrExpr::NumberLiteral(42.0)),
                    },
                    body: vec![IrNode::Write("Numbers equal".to_string())],
                },
                IrNode::If {
                    condition: IrExpr::BinaryOp {
                        left: Box::new(IrExpr::NumberLiteral(1.0)),
                        op: BinaryOp::Equal,
                        right: Box::new(IrExpr::NumberLiteral(2.0)),
                    },
                    body: vec![IrNode::Write("Numbers not equal".to_string())],
                },
            ],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        let expected = vec![IrNode::Write("Numbers equal".to_string())];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_in_for_loop() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::For {
                var: "item".to_string(),
                array: IrExpr::ArrayLiteral(vec![
                    IrExpr::StringLiteral("a".to_string()),
                    IrExpr::StringLiteral("b".to_string()),
                ]),
                body: vec![
                    IrNode::If {
                        condition: IrExpr::BooleanLiteral(true),
                        body: vec![IrNode::Write("Always in loop".to_string())],
                    },
                    IrNode::If {
                        condition: IrExpr::BooleanLiteral(false),
                        body: vec![IrNode::Write("Never in loop".to_string())],
                    },
                ],
            }],
        };

        // Run the optimization pass
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: For loop preserved but ifs inside optimized
        let expected = vec![IrNode::For {
            var: "item".to_string(),
            array: IrExpr::ArrayLiteral(vec![
                IrExpr::StringLiteral("a".to_string()),
                IrExpr::StringLiteral("b".to_string()),
            ]),
            body: vec![IrNode::Write("Always in loop".to_string())],
        }];

        assert_eq!(result.body, expected);
    }
}
