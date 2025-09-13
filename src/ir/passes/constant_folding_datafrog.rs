use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{ExprId, IrExprValue, UnaryOp},
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

        // Variables - (expr_id, boolean_value)
        let bool_value = iteration.variable::<(ExprId, bool)>("bool_value");

        // Variable for not operations - (operand_id, expr_id)
        let not_rel = iteration.variable::<(ExprId, ExprId)>("not_rel");

        // Extract facts from expression tree using DFS iterator
        for node in expr.dfs_iter() {
            match &node.value {
                IrExprValue::Boolean(b) => {
                    // Insert literals directly into the bool_value variable
                    bool_value.insert(Relation::from_vec(vec![(node.id, *b)]));
                }
                IrExprValue::UnaryOp {
                    op: UnaryOp::Not,
                    operand,
                } => {
                    // Insert NOT facts directly in the correct order for join: (operand_id, expr_id)
                    not_rel.insert(Relation::from_vec(vec![(operand.id, node.id)]));
                }
                _ => {} // Other expression types don't contribute facts yet
            }
        }

        while iteration.changed() {
            // bool_value(Y, !V) :- not_rel(X, Y), bool_value(X, V).
            bool_value.from_join(
                &bool_value,
                &not_rel,
                |_operand_id: &ExprId, bool_val: &bool, expr_id: &ExprId| {
                    // _operand_id is the join key (matches between both relations)
                    // bool_val is the value from bool_value
                    // result_id is the value from not_rel
                    (*expr_id, !bool_val)
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
}
