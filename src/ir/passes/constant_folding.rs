use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{BinaryOp, IrExprValue, UnaryOp},
    ast::{IrEntrypoint, IrNode},
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
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = ConstantFoldingPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_constant_folding_folds_binary_operations() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // "x" == "x" => true
                    t.if_stmt(
                        t.eq(t.str("x"), t.str("x")),
                        vec![t.write("Condition folded to true")],
                    ),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("Condition folded to true")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_constant_folding_folds_unary_operations() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // !false => true
                    t.if_stmt(t.not(t.boolean(false)), vec![t.write("Negation folded")]),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("Negation folded")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_constant_folding_preserves_dynamic_expressions() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![("show".to_string(), Type::Bool)],
                body: vec![
                    // show is preserved
                    t.if_stmt(t.var("show"), vec![t.write("Dynamic condition preserved")]),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: [show: boolean]
              body: {
                If(condition: show) {
                  Write("Dynamic condition preserved")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_constant_folding_in_nested_expressions() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // 2.0 == 2.0 => true
                    t.let_stmt(
                        "x",
                        t.eq(t.num(2.0), t.num(2.0)),
                        vec![t.write("Let with folded expression")],
                    ),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Let(var: x, value: true) {
                  Write("Let with folded expression")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_constant_folding_in_for_array() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![t.for_loop(
                    "item",
                    t.array(vec![
                        // 1.0 == 1.0 => true
                        t.eq(t.num(1.0), t.num(1.0)),
                        // 2.0 == 2.0 => true
                        t.eq(t.num(2.0), t.num(3.0)),
                    ]),
                    vec![t.write("For loop body")],
                )],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                For(var: item, array: [true, false]) {
                  Write("For loop body")
                }
              }
            }
        "#]],
        );
    }
}
