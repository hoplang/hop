use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{BinaryOp, ExprId, IrExprValue, NodeEvent, UnaryOp},
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

    /// Collect all expressions from the entrypoint
    fn collect_all_expressions(entrypoint: &IrEntrypoint) -> Vec<&IrExpr> {
        let mut expressions = Vec::new();

        for event in entrypoint.visit_nodes() {
            if let NodeEvent::Enter(node) = event {
                match node {
                    IrNode::If { condition, .. } => {
                        expressions.extend(condition.dfs_iter());
                    }
                    IrNode::WriteExpr { expr, .. } => {
                        expressions.extend(expr.dfs_iter());
                    }
                    IrNode::For { array, .. } => {
                        expressions.extend(array.dfs_iter());
                    }
                    IrNode::Let { value, .. } => {
                        expressions.extend(value.dfs_iter());
                    }
                    IrNode::Write { .. } => {}
                }
            }
        }

        expressions
    }

    /// Collect variable bindings with proper scoping
    /// Returns (def_expr_id, var_expr_id) pairs
    fn collect_variable_bindings(entrypoint: &IrEntrypoint) -> Vec<(ExprId, ExprId)> {
        let mut var_bindings = Vec::new();
        let mut scope_stack: HashMap<String, ExprId> = HashMap::new();

        for event in entrypoint.visit_nodes() {
            match event {
                NodeEvent::Enter(node) => {
                    match node {
                        IrNode::If { condition, .. } => {
                            Self::collect_var_uses(condition, &mut var_bindings, &scope_stack);
                        }
                        IrNode::WriteExpr { expr, .. } => {
                            Self::collect_var_uses(expr, &mut var_bindings, &scope_stack);
                        }
                        IrNode::For { array, .. } => {
                            Self::collect_var_uses(array, &mut var_bindings, &scope_stack);
                        }
                        IrNode::Let { var, value, .. } => {
                            // Collect variable uses from the value expression (before the variable is in scope)
                            Self::collect_var_uses(value, &mut var_bindings, &scope_stack);
                            // Add the variable binding to the scope
                            scope_stack.insert(var.clone(), value.id);
                        }
                        IrNode::Write { .. } => {}
                    }
                }
                NodeEvent::Exit(node) => {
                    if let IrNode::Let { var, .. } = node {
                        // Remove the variable binding when leaving the scope
                        scope_stack.remove(var);
                    }
                }
            }
        }

        var_bindings
    }

    fn collect_var_uses(
        expr: &IrExpr,
        var_bindings: &mut Vec<(ExprId, ExprId)>,
        scope_stack: &HashMap<String, ExprId>,
    ) {
        for e in expr.dfs_iter() {
            // If this is a variable reference, record its binding
            if let IrExprValue::Var(name) = &e.value {
                if let Some(&def_expr_id) = scope_stack.get(name) {
                    // Record: (defining_expr_id, var_expr_id) - already in the right order for joining
                    var_bindings.push((def_expr_id, e.id));
                }
            }
        }
    }

    /// Run datafrog to compute which expressions are constants across the entire entrypoint
    fn compute_constants(entrypoint: &IrEntrypoint) -> HashMap<ExprId, IrExprValue> {
        let all_expressions = Self::collect_all_expressions(entrypoint);
        let var_bindings = Self::collect_variable_bindings(entrypoint);
        let mut iteration = Iteration::new();

        // Boolean values of expressions: (expr_id => boolean_value)
        let bool_value = iteration.variable::<(ExprId, bool)>("bool_value");
        bool_value.extend(all_expressions.iter().filter_map(|n| match n.value {
            IrExprValue::Boolean(b) => Some((n.id, b)),
            _ => None,
        }));

        // String values of expressions: (expr_id => string_value)
        let string_value = iteration.variable::<(ExprId, String)>("string_value");
        string_value.extend(all_expressions.iter().filter_map(|n| match &n.value {
            IrExprValue::String(s) => Some((n.id, s.clone())),
            _ => None,
        }));

        // Values of left operands in equality expressions: (eq_expr_id => left_value)
        let eq_left_value = iteration.variable::<(ExprId, bool)>("eq_left_value");

        // Values of right operands in equality expressions: (eq_expr_id => right_value)
        let eq_right_value = iteration.variable::<(ExprId, bool)>("eq_right_value");

        // Not operations keyed by operand: (operand_id => expr_id)
        let not_rel = Relation::from_iter(all_expressions.iter().filter_map(|n| match &n.value {
            IrExprValue::UnaryOp {
                op: UnaryOp::Not,
                operand,
            } => Some((operand.id, n.id)),
            _ => None,
        }));

        // Equality operations - left operand: (left_operand_id => expr_id)
        let eq_left = Relation::from_iter(all_expressions.iter().filter_map(|n| match &n.value {
            IrExprValue::BinaryOp {
                op: BinaryOp::Eq,
                left,
                right: _,
            } => Some((left.id, n.id)),
            _ => None,
        }));

        // Equality operations - right operand: (right_operand_id => expr_id)
        let eq_right = Relation::from_iter(all_expressions.iter().filter_map(|n| match &n.value {
            IrExprValue::BinaryOp {
                op: BinaryOp::Eq,
                left: _,
                right,
            } => Some((right.id, n.id)),
            _ => None,
        }));

        // Variable bindings: (defining_expr_id => var_expr_id)
        let var_def = Relation::from_iter(var_bindings.iter().cloned());

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

            // Propagate constants through variable bindings
            // bool_value(var_expr, val) :- bool_value(def_expr, val), var_def(def_expr, var_expr).
            bool_value.from_join(
                &bool_value,
                &var_def,
                |_def_expr: &ExprId, val: &bool, var_expr: &ExprId| (*var_expr, *val),
            );

            // string_value(var_expr, val) :- string_value(def_expr, val), var_def(def_expr, var_expr).
            string_value.from_join(
                &string_value,
                &var_def,
                |_def_expr: &ExprId, val: &String, var_expr: &ExprId| (*var_expr, val.clone()),
            );
        }

        // Convert results to IrExprValue
        let mut results = HashMap::new();
        for (id, bool_val) in bool_value.complete().iter() {
            results.insert(*id, IrExprValue::Boolean(*bool_val));
        }
        for (id, string_val) in string_value.complete().iter() {
            results.insert(*id, IrExprValue::String(string_val.clone()));
        }
        results
    }

    fn transform_expr(expr: IrExpr, constants: &HashMap<ExprId, IrExprValue>) -> IrExpr {
        if let Some(const_val) = constants.get(&expr.id) {
            IrExpr {
                id: expr.id,
                value: const_val.clone(),
            }
        } else {
            expr
        }
    }
}

impl Pass for DatafrogConstantFoldingPass {
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint {
        // Compute constants once for the entire entrypoint
        let constants = Self::compute_constants(&entrypoint);

        // Apply transformations using the computed constants
        entrypoint.map_expressions(|expr| Self::transform_expr(expr, &constants))
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

    #[test]
    fn test_variable_constant_propagation() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // let x = !!true
                    t.let_stmt(
                        "x",
                        t.not(t.not(t.boolean(true))),
                        vec![
                            // if x => if true
                            t.if_stmt(t.var("x"), vec![t.write("x is true")]),
                            // if !x => if false
                            t.if_stmt(t.not(t.var("x")), vec![t.write("x is false")]),
                        ],
                    ),
                ],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: x, value: true) {
                      If(condition: true) {
                        Write("x is true")
                      }
                      If(condition: false) {
                        Write("x is false")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_variable_in_equality() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![t.let_stmt(
                    "x",
                    t.boolean(true),
                    vec![t.let_stmt(
                        "y",
                        t.not(t.boolean(true)),
                        vec![
                            // x == y => true == false => false
                            t.if_stmt(t.eq(t.var("x"), t.var("y")), vec![t.write("x equals y")]),
                            // x == !y => true == true => true
                            t.if_stmt(
                                t.eq(t.var("x"), t.not(t.var("y"))),
                                vec![t.write("x equals not y")],
                            ),
                        ],
                    )],
                )],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: x, value: true) {
                      Let(var: y, value: false) {
                        If(condition: false) {
                          Write("x equals y")
                        }
                        If(condition: true) {
                          Write("x equals not y")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_string_constant_propagation() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![t.let_stmt(
                    "message",
                    t.str("Hello, World!"),
                    vec![
                        // Variable containing string should be replaced with the string constant
                        t.write_expr(t.var("message"), true),
                    ],
                )],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: message, value: "Hello, World!") {
                      WriteExpr(expr: "Hello, World!", escape: true)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_string_variable_propagation() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![t.let_stmt(
                    "greeting",
                    t.str("Hello"),
                    vec![t.let_stmt(
                        "name",
                        t.str("World"),
                        vec![
                            // Both variables should be replaced with their string constants
                            t.write_expr(t.var("greeting"), true),
                            t.write_expr(t.var("name"), true),
                        ],
                    )],
                )],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: greeting, value: "Hello") {
                      Let(var: name, value: "World") {
                        WriteExpr(expr: "Hello", escape: true)
                        WriteExpr(expr: "World", escape: true)
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_string_variable_multiple_uses() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![t.let_stmt(
                    "title",
                    t.str("Welcome"),
                    vec![
                        // Multiple uses of the same string variable
                        t.write_expr(t.var("title"), true),
                        t.write_expr(t.var("title"), true),
                        t.let_stmt(
                            "subtitle",
                            t.var("title"),
                            vec![
                                // Transitive propagation through another variable
                                t.write_expr(t.var("subtitle"), true),
                            ],
                        ),
                    ],
                )],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: title, value: "Welcome") {
                      WriteExpr(expr: "Welcome", escape: true)
                      WriteExpr(expr: "Welcome", escape: true)
                      Let(var: subtitle, value: "Welcome") {
                        WriteExpr(expr: "Welcome", escape: true)
                      }
                    }
                  }
                }
            "#]],
        );
    }
}
