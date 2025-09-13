use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypoint, IrNode},
    ast::{BinaryOp, IrExprValue, UnaryOp},
};

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
        match &expr.value {
            IrExprValue::String(s) => Some(ConstantValue::String(s.clone())),
            IrExprValue::Boolean(b) => Some(ConstantValue::Boolean(*b)),
            IrExprValue::Number(n) => Some(ConstantValue::Number(*n)),
            IrExprValue::BinaryOp { left, op, right } => {
                let left_val = Self::try_eval_constant(left)?;
                let right_val = Self::try_eval_constant(right)?;
                match op {
                    BinaryOp::Equal => Some(ConstantValue::Boolean(left_val == right_val)),
                }
            }
            IrExprValue::UnaryOp { op, operand } => {
                let val = Self::try_eval_constant(operand)?;
                match op {
                    UnaryOp::Not => match val {
                        ConstantValue::Boolean(b) => Some(ConstantValue::Boolean(!b)),
                        _ => None,
                    },
                }
            }
            IrExprValue::Array(elements) => {
                let mut const_elements = Vec::new();
                for elem in elements {
                    const_elements.push(Self::try_eval_constant(elem)?);
                }
                Some(ConstantValue::Array(const_elements))
            }
            IrExprValue::Object(properties) => {
                let mut const_props = Vec::new();
                for (key, value) in properties {
                    const_props.push((key.clone(), Self::try_eval_constant(value)?));
                }
                Some(ConstantValue::Object(const_props))
            }
            _ => None, // Variables and property access can't be evaluated at compile time
        }
    }

    /// Transform an expression, applying constant folding optimizations
    fn transform_expr(expr: IrExpr) -> IrExpr {
        match expr.value {
            IrExprValue::BinaryOp { left, op, right } => {
                let left = Self::transform_expr(*left);
                let right = Self::transform_expr(*right);

                // Try to evaluate if both operands are constants
                let new_expr = IrExpr {
                    id: expr.id,
                    value: IrExprValue::BinaryOp {
                        left: Box::new(left.clone()),
                        op,
                        right: Box::new(right.clone()),
                    },
                };

                if let Some(const_val) = Self::try_eval_constant(&new_expr) {
                    Self::constant_to_expr(const_val, expr.id)
                } else {
                    new_expr
                }
            }
            IrExprValue::UnaryOp { op, operand } => {
                let operand = Self::transform_expr(*operand);

                let new_expr = IrExpr {
                    id: expr.id,
                    value: IrExprValue::UnaryOp {
                        op,
                        operand: Box::new(operand.clone()),
                    },
                };

                if let Some(const_val) = Self::try_eval_constant(&new_expr) {
                    Self::constant_to_expr(const_val, expr.id)
                } else {
                    new_expr
                }
            }
            IrExprValue::Array(elements) => {
                let transformed_elements: Vec<IrExpr> =
                    elements.into_iter().map(Self::transform_expr).collect();

                let new_expr = IrExpr {
                    id: expr.id,
                    value: IrExprValue::Array(transformed_elements.clone()),
                };

                if let Some(const_val) = Self::try_eval_constant(&new_expr) {
                    Self::constant_to_expr(const_val, expr.id)
                } else {
                    new_expr
                }
            }
            IrExprValue::Object(properties) => {
                let transformed_props: Vec<(String, IrExpr)> = properties
                    .into_iter()
                    .map(|(key, value)| (key, Self::transform_expr(value)))
                    .collect();

                let new_expr = IrExpr {
                    id: expr.id,
                    value: IrExprValue::Object(transformed_props.clone()),
                };

                if let Some(const_val) = Self::try_eval_constant(&new_expr) {
                    Self::constant_to_expr(const_val, expr.id)
                } else {
                    new_expr
                }
            }
            _ => expr, // Other expressions (literals, variables, etc.) are returned as-is
        }
    }

    /// Convert a ConstantValue back to an IrExpr
    fn constant_to_expr(const_val: ConstantValue, id: u32) -> IrExpr {
        let value = match const_val {
            ConstantValue::String(s) => IrExprValue::String(s),
            ConstantValue::Boolean(b) => IrExprValue::Boolean(b),
            ConstantValue::Number(n) => IrExprValue::Number(n),
            ConstantValue::Array(elements) => {
                let expr_elements: Vec<IrExpr> = elements
                    .into_iter()
                    .map(|elem| Self::constant_to_expr(elem, id))
                    .collect();
                IrExprValue::Array(expr_elements)
            }
            ConstantValue::Object(properties) => {
                let expr_props: Vec<(String, IrExpr)> = properties
                    .into_iter()
                    .map(|(key, value)| (key, Self::constant_to_expr(value, id)))
                    .collect();
                IrExprValue::Object(expr_props)
            }
        };

        IrExpr { id, value }
    }

    /// Transform a list of IR nodes, applying constant folding to expressions
    fn transform_nodes(nodes: Vec<IrNode>) -> Vec<IrNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                IrNode::If { condition, body } => IrNode::If {
                    condition: Self::transform_expr(condition),
                    body: Self::transform_nodes(body),
                },
                IrNode::For { var, array, body } => IrNode::For {
                    var,
                    array: Self::transform_expr(array),
                    body: Self::transform_nodes(body),
                },
                IrNode::Let { var, value, body } => IrNode::Let {
                    var,
                    value: Self::transform_expr(value),
                    body: Self::transform_nodes(body),
                },
                IrNode::WriteExpr { expr, escape } => IrNode::WriteExpr {
                    expr: Self::transform_expr(expr),
                    escape,
                },
                _ => node, // Write nodes and others are preserved as-is
            })
            .collect()
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
    use crate::dop::Type;

    use super::*;

    // Helper functions to create IrExpr for tests
    fn make_string(s: &str) -> IrExpr {
        IrExpr {
            id: 0, // Use dummy ID for tests
            value: IrExprValue::String(s.to_string()),
        }
    }

    fn make_boolean(b: bool) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Boolean(b),
        }
    }

    fn make_binary_op(left: IrExpr, op: BinaryOp, right: IrExpr) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
        }
    }

    fn make_unary_op(op: UnaryOp, operand: IrExpr) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::UnaryOp {
                op,
                operand: Box::new(operand),
            },
        }
    }

    fn make_var(name: &str) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Var(name.to_string()),
        }
    }

    fn make_number(n: f64) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Number(n),
        }
    }

    fn make_array(elements: Vec<IrExpr>) -> IrExpr {
        IrExpr {
            id: 0,
            value: IrExprValue::Array(elements),
        }
    }

    #[test]
    fn test_constant_folding_folds_binary_operations() {
        // Create an if with condition "x" == "x" (should be folded to true)
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::If {
                condition: make_binary_op(make_string("x"), BinaryOp::Equal, make_string("x")),
                body: vec![IrNode::Write("Condition folded to true".to_string())],
            }],
        };

        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: The condition should be folded to a boolean true
        let expected = vec![IrNode::If {
            condition: make_boolean(true),
            body: vec![IrNode::Write("Condition folded to true".to_string())],
        }];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_folds_unary_operations() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::If {
                condition: make_unary_op(UnaryOp::Not, make_boolean(false)),
                body: vec![IrNode::Write("Negation folded".to_string())],
            }],
        };

        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: !false should be folded to true
        let expected = vec![IrNode::If {
            condition: make_boolean(true),
            body: vec![IrNode::Write("Negation folded".to_string())],
        }];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_preserves_dynamic_expressions() {
        let entrypoint = IrEntrypoint {
            parameters: vec![("show".to_string(), Type::Bool)],
            body: vec![IrNode::If {
                condition: make_var("show"),
                body: vec![IrNode::Write("Dynamic condition preserved".to_string())],
            }],
        };

        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: Dynamic condition should be preserved as-is
        let expected = vec![IrNode::If {
            condition: make_var("show"),
            body: vec![IrNode::Write("Dynamic condition preserved".to_string())],
        }];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_in_nested_expressions() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::Let {
                var: "x".to_string(),
                value: make_binary_op(make_number(2.0), BinaryOp::Equal, make_number(2.0)),
                body: vec![IrNode::Write("Let with folded expression".to_string())],
            }],
        };

        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: Binary operation in let value should be folded
        let expected = vec![IrNode::Let {
            var: "x".to_string(),
            value: make_boolean(true),
            body: vec![IrNode::Write("Let with folded expression".to_string())],
        }];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_in_write_expr() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::WriteExpr {
                expr: make_binary_op(make_string("hello"), BinaryOp::Equal, make_string("hello")),
                escape: true,
            }],
        };

        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: Expression in WriteExpr should be folded
        let expected = vec![IrNode::WriteExpr {
            expr: make_boolean(true),
            escape: true,
        }];

        assert_eq!(result.body, expected);
    }

    #[test]
    fn test_constant_folding_in_for_array() {
        let entrypoint = IrEntrypoint {
            parameters: vec![],
            body: vec![IrNode::For {
                var: "item".to_string(),
                array: make_array(vec![
                    make_binary_op(make_number(1.0), BinaryOp::Equal, make_number(1.0)),
                    make_binary_op(make_number(2.0), BinaryOp::Equal, make_number(3.0)),
                ]),
                body: vec![IrNode::Write("For loop body".to_string())],
            }],
        };

        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);

        // Expected: Array elements should be folded
        let expected = vec![IrNode::For {
            var: "item".to_string(),
            array: make_array(vec![make_boolean(true), make_boolean(false)]),
            body: vec![IrNode::Write("For loop body".to_string())],
        }];

        assert_eq!(result.body, expected);
    }
}
