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

        // Extract facts from expression tree using DFS iterator
        let mut literal_facts = Vec::new();
        let mut not_facts = Vec::new();

        for node in expr.dfs_iter() {
            match &node.value {
                IrExprValue::Boolean(b) => {
                    literal_facts.push((node.id, *b));
                }
                IrExprValue::UnaryOp { op: UnaryOp::Not, operand } => {
                    not_facts.push((node.id, operand.id));
                }
                _ => {} // Other expression types don't contribute facts yet
            }
        }

        // Variables - (expr_id, boolean_value)
        let constant = iteration.variable::<(ExprId, bool)>("constant");

        // Initialize with literals
        constant.insert(literal_facts.into());

        // Reformat not_facts to be (operand_id, result_id) for join
        let not_op_inverted: Vec<(ExprId, ExprId)> = not_facts
            .into_iter()
            .map(|(result, operand)| (operand, result))
            .collect();

        // Create relation for not operations (operand_id, result_id)
        let not_rel = Relation::from_vec(not_op_inverted);

        // Rule: !constant => constant
        // from_join signature: fn(&V1, &K, &V2) -> (K_new, V_new)
        // constant is Variable<(ExprId, bool)> where K=ExprId, V=bool
        // not_rel is Relation<(ExprId, ExprId)> where K=ExprId, V=ExprId
        // The callback receives (&bool, &ExprId, &ExprId)
        while iteration.changed() {
            constant.from_join(
                &constant,
                &not_rel,
                |_operand_id: &ExprId, bool_val: &bool, result_id: &ExprId| {
                    // _operand_id is the join key (matches between both relations)
                    // bool_val is the value from constant
                    // result_id is the value from not_rel
                    (*result_id, !bool_val)
                },
            );
        }

        // Convert results to IrExprValue
        let mut results = HashMap::new();
        for (id, bool_val) in constant.complete().iter() {
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
