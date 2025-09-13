use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{BinaryOp, ExprId, IrExprValue, UnaryOp},
    ast::{IrEntrypoint, IrNode},
};
use datafrog::{Iteration, Relation};
use std::collections::HashMap;

/// A datafrog-based constant folding pass for unary expressions
pub struct DatafrogConstantFoldingPass;

impl DatafrogConstantFoldingPass {
    pub fn new() -> Self {
        Self
    }

    /// Run datafrog to compute which expressions are constants
    fn compute_constants(expr: &IrExpr) -> HashMap<ExprId, IrExprValue> {
        let mut iteration = Iteration::new();

        // Boolean values of expressions: (expr_id => boolean_value)
        let bool_value = iteration.variable::<(ExprId, bool)>("bool_value");
        bool_value.extend(expr.dfs_iter().filter_map(|n| match n.value {
            IrExprValue::Boolean(b) => Some((n.id, b)),
            _ => None,
        }));

        // Values of left operands in equality expressions: (eq_expr_id => left_value)
        let eq_left_value = iteration.variable::<(ExprId, bool)>("eq_left_value");

        // Values of right operands in equality expressions: (eq_expr_id => right_value)
        let eq_right_value = iteration.variable::<(ExprId, bool)>("eq_right_value");

        // Not operations keyed by operand: (operand_id => expr_id)
        let not_rel = Relation::from_iter(expr.dfs_iter().filter_map(|n| match &n.value {
            IrExprValue::UnaryOp {
                op: UnaryOp::Not,
                operand,
            } => Some((operand.id, n.id)),
            _ => None,
        }));

        // Equality operations - left operand: (left_operand_id => expr_id)
        let eq_left = Relation::from_iter(expr.dfs_iter().filter_map(|n| match &n.value {
            IrExprValue::BinaryOp {
                op: BinaryOp::Eq,
                left,
                right: _,
            } => Some((left.id, n.id)),
            _ => None,
        }));

        // Equality operations - right operand: (right_operand_id => expr_id)
        let eq_right = Relation::from_iter(expr.dfs_iter().filter_map(|n| match &n.value {
            IrExprValue::BinaryOp {
                op: BinaryOp::Eq,
                left: _,
                right,
            } => Some((right.id, n.id)),
            _ => None,
        }));

        while iteration.changed() {
            // bool_value(exp, !b) :- not_rel(op, exp), bool_value(op, b).
            bool_value.from_join(
                &bool_value,
                &not_rel,
                |_: &ExprId, bool_val: &bool, expr_id: &ExprId| (*expr_id, !bool_val),
            );

            // eq_left_value(eq_expr, left_val) :- eq_left(left_op, eq_expr), bool_value(left_op, left_val).
            eq_left_value.from_join(
                &bool_value,
                &eq_left,
                |_: &ExprId, left_val: &bool, eq_expr: &ExprId| (*eq_expr, *left_val),
            );

            // eq_right_value(eq_expr, right_val) :- eq_right(right_op, eq_expr), bool_value(right_op, right_val).
            eq_right_value.from_join(
                &bool_value,
                &eq_right,
                |_: &ExprId, right_val: &bool, eq_expr: &ExprId| (*eq_expr, *right_val),
            );

            // bool_value(eq, lv == rv) :- eq_left_value(eq, lv), eq_right_value(eq, rv).
            bool_value.from_join(
                &eq_left_value,
                &eq_right_value,
                |eq_expr: &ExprId, left_val: &bool, right_val: &bool| {
                    (*eq_expr, left_val == right_val)
                },
            );
        }

        // Convert results to IrExprValue
        let mut results = HashMap::new();
        for (id, bool_val) in bool_value.complete().iter() {
            results.insert(*id, IrExprValue::Boolean(*bool_val));
        }
        results
    }

    /// Transform an expression using computed constants
    fn transform_expr(expr: IrExpr) -> IrExpr {
        let constants = Self::compute_constants(&expr);
        Self::apply_constants(expr, &constants)
    }

    fn apply_constants(expr: IrExpr, constants: &HashMap<ExprId, IrExprValue>) -> IrExpr {
        if let Some(const_val) = constants.get(&expr.id) {
            IrExpr {
                id: expr.id,
                value: const_val.clone(),
            }
        } else {
            // Recursively transform children
            let value = match expr.value {
                IrExprValue::UnaryOp { op, operand } => IrExprValue::UnaryOp {
                    op,
                    operand: Box::new(Self::apply_constants(*operand, constants)),
                },
                IrExprValue::BinaryOp { op, left, right } => IrExprValue::BinaryOp {
                    op,
                    left: Box::new(Self::apply_constants(*left, constants)),
                    right: Box::new(Self::apply_constants(*right, constants)),
                },
                other => other,
            };
            IrExpr { id: expr.id, value }
        }
    }

    /// Transform nodes, applying constant folding to expressions
    fn transform_nodes(nodes: Vec<IrNode>) -> Vec<IrNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                IrNode::If {
                    id,
                    condition,
                    body,
                } => IrNode::If {
                    id,
                    condition: Self::transform_expr(condition),
                    body: Self::transform_nodes(body),
                },
                IrNode::WriteExpr { id, expr, escape } => IrNode::WriteExpr {
                    id,
                    expr: Self::transform_expr(expr),
                    escape,
                },
                IrNode::For {
                    id,
                    var,
                    array,
                    body,
                } => IrNode::For {
                    id,
                    var,
                    array: Self::transform_expr(array),
                    body: Self::transform_nodes(body),
                },
                IrNode::Let {
                    id,
                    var,
                    value,
                    body,
                } => IrNode::Let {
                    id,
                    var,
                    value: Self::transform_expr(value),
                    body: Self::transform_nodes(body),
                },
                other => other,
            })
            .collect()
    }
}

impl Pass for DatafrogConstantFoldingPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        entrypoint.body = Self::transform_nodes(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = DatafrogConstantFoldingPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_simple_not_folding() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // !false => true
                    t.if_stmt(t.not(t.boolean(false)), vec![t.write("Should be true")]),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("Should be true")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_double_not_folding() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // !!true => true
                    t.if_stmt(
                        t.not(t.not(t.boolean(true))),
                        vec![t.write("Double negation")],
                    ),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("Double negation")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_triple_not_folding() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // !!!false => true
                    t.if_stmt(
                        t.not(t.not(t.not(t.boolean(false)))),
                        vec![t.write("Triple negation")],
                    ),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("Triple negation")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_equality_folding() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // true == true => true
                    t.if_stmt(
                        t.eq(t.boolean(true), t.boolean(true)),
                        vec![t.write("true == true")],
                    ),
                    // false == false => true
                    t.if_stmt(
                        t.eq(t.boolean(false), t.boolean(false)),
                        vec![t.write("false == false")],
                    ),
                    // true == false => false
                    t.if_stmt(
                        t.eq(t.boolean(true), t.boolean(false)),
                        vec![t.write("Should not appear")],
                    ),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: true) {
                  Write("true == true")
                }
                If(condition: true) {
                  Write("false == false")
                }
                If(condition: false) {
                  Write("Should not appear")
                }
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_complex_equality_with_negations() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // (!!false == !false) => (false == true) => false
                    t.if_stmt(
                        t.eq(t.not(t.not(t.boolean(false))), t.not(t.boolean(false))),
                        vec![t.write("Should not appear")],
                    ),
                ],
            },
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                If(condition: false) {
                  Write("Should not appear")
                }
              }
            }
        "#]],
        );
    }
}
