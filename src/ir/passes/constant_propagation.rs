use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{BinaryOp, ExprId, StatementEvent, UnaryOp},
    ast::{IrEntrypoint, IrStatement},
};
use datafrog::{Iteration, Relation};
use std::collections::HashMap;

/// Constant values that can be tracked during constant folding
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Const {
    Bool(bool),
    String(String),
}

/// A datafrog-based constant propagation pass that tracks and propagates constant values
pub struct ConstantPropagationPass;

impl ConstantPropagationPass {
    pub fn new() -> Self {
        Self
    }

    /// Collect all expressions from the entrypoint
    fn collect_all_expressions(entrypoint: &IrEntrypoint) -> Vec<&IrExpr> {
        let mut expressions = Vec::new();

        for event in entrypoint.visit_statements() {
            if let StatementEvent::Enter(statment) = event {
                match statment {
                    IrStatement::If { condition, .. } => {
                        expressions.extend(condition.dfs_iter());
                    }
                    IrStatement::WriteExpr { expr, .. } => {
                        expressions.extend(expr.dfs_iter());
                    }
                    IrStatement::For { array, .. } => {
                        expressions.extend(array.dfs_iter());
                    }
                    IrStatement::Let { value, .. } => {
                        expressions.extend(value.dfs_iter());
                    }
                    IrStatement::Write { .. } => {}
                }
            }
        }

        expressions
    }

    /// Collect variable bindings with proper scoping
    /// Returns (defining_expr_id, referencing_expr_id) pairs
    fn collect_variable_references(entrypoint: &IrEntrypoint) -> Vec<(ExprId, ExprId)> {
        let mut var_bindings = Vec::new();
        let mut scope: HashMap<String, ExprId> = HashMap::new();

        for event in entrypoint.visit_statements() {
            match event {
                StatementEvent::Enter(statement) => match statement {
                    IrStatement::If { condition, .. } => {
                        Self::collect_var_refs(condition, &mut var_bindings, &scope);
                    }
                    IrStatement::WriteExpr { expr, .. } => {
                        Self::collect_var_refs(expr, &mut var_bindings, &scope);
                    }
                    IrStatement::For { array, .. } => {
                        Self::collect_var_refs(array, &mut var_bindings, &scope);
                    }
                    IrStatement::Let { var, value, .. } => {
                        Self::collect_var_refs(value, &mut var_bindings, &scope);
                        scope.insert(var.to_string(), value.id());
                    }
                    IrStatement::Write { .. } => {}
                },
                StatementEvent::Exit(statement) => {
                    if let IrStatement::Let { var, .. } = statement {
                        scope.remove(var.as_str());
                    }
                }
            }
        }

        var_bindings
    }

    fn collect_var_refs(
        expr: &IrExpr,
        var_bindings: &mut Vec<(ExprId, ExprId)>,
        scope_stack: &HashMap<String, ExprId>,
    ) {
        for ref_expr in expr.dfs_iter() {
            // If this is a variable reference, record its binding
            if let IrExpr::Var {
                value: name,
                annotation,
                ..
            } = ref_expr
            {
                if let Some(&def_expr_id) = scope_stack.get(name.as_str()) {
                    // Record: (defining_expr_id, referencing_expr_id)
                    var_bindings.push((def_expr_id, annotation.0));
                }
            }
        }
    }
}

impl Pass for ConstantPropagationPass {
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint {
        let all_expressions = Self::collect_all_expressions(&entrypoint);
        let references = Self::collect_variable_references(&entrypoint);
        let mut iteration = Iteration::new();

        // Constant values of expressions: (expr_id => const_value)
        let const_value = iteration.variable::<(ExprId, Const)>("const_value");
        const_value.extend(all_expressions.iter().filter_map(|n| match n {
            IrExpr::BooleanLiteral {
                value,
                annotation: (id, _),
                ..
            } => Some((*id, Const::Bool(*value))),
            IrExpr::StringLiteral {
                value,
                annotation: (id, _),
                ..
            } => Some((*id, Const::String(value.clone()))),
            _ => None,
        }));

        // Values of left operands in equality expressions: (eq_expr_id => left_value)
        let eq_left_value = iteration.variable::<(ExprId, Const)>("eq_left_value");

        // Values of right operands in equality expressions: (eq_expr_id => right_value)
        let eq_right_value = iteration.variable::<(ExprId, Const)>("eq_right_value");

        // Not operations keyed by operand: (operand_id => expr_id)
        let not_rel = Relation::from_iter(all_expressions.iter().filter_map(|n| match n {
            IrExpr::UnaryOp {
                operator: UnaryOp::Not,
                operand,
                annotation,
                ..
            } => Some((operand.id(), annotation.0)),
            _ => None,
        }));

        // Equality operations - left operand: (left_operand_id => expr_id)
        let eq_left = Relation::from_iter(all_expressions.iter().filter_map(|n| match n {
            IrExpr::BinaryOp {
                operator: BinaryOp::Eq,
                left,
                right: _,
                annotation,
                ..
            } => Some((left.id(), annotation.0)),
            _ => None,
        }));

        // Equality operations - right operand: (right_operand_id => expr_id)
        let eq_right = Relation::from_iter(all_expressions.iter().filter_map(|n| match n {
            IrExpr::BinaryOp {
                operator: BinaryOp::Eq,
                left: _,
                right,
                annotation,
                ..
            } => Some((right.id(), annotation.0)),
            _ => None,
        }));

        // Variable bindings: (defining_expr_id => referencing_expr_id)
        let def_to_ref = Relation::from_iter(references.iter().cloned());

        while iteration.changed() {
            // const_value(exp, !b) :- not_rel(op, exp), const_value(op, Bool(b)).
            const_value.from_join(
                &const_value,
                &not_rel,
                |_: &ExprId, const_val: &Const, expr_id: &ExprId| match const_val {
                    Const::Bool(b) => (*expr_id, Const::Bool(!b)),
                    _ => unreachable!(),
                },
            );

            // eq_left_value(eq_expr, left_val) :- eq_left(left_op, eq_expr), const_value(left_op, left_val).
            eq_left_value.from_join(
                &const_value,
                &eq_left,
                |_: &ExprId, const_val: &Const, eq_expr: &ExprId| (*eq_expr, const_val.clone()),
            );

            // eq_right_value(eq_expr, right_val) :- eq_right(right_op, eq_expr), const_value(right_op, right_val).
            eq_right_value.from_join(
                &const_value,
                &eq_right,
                |_: &ExprId, const_val: &Const, eq_expr: &ExprId| (*eq_expr, const_val.clone()),
            );

            // const_value(eq, Bool(lv == rv)) :- eq_left_value(eq, lv), eq_right_value(eq, rv).
            const_value.from_join(
                &eq_left_value,
                &eq_right_value,
                |eq_expr: &ExprId, left_val: &Const, right_val: &Const| {
                    (*eq_expr, Const::Bool(left_val == right_val))
                },
            );

            // Propagate constants through variable bindings
            // const_value(referencing_expr, val) :- const_value(defining_expr, val), def_to_ref(defining_expr, referencing_expr).
            const_value.from_join(
                &const_value,
                &def_to_ref,
                |_def_expr: &ExprId, val: &Const, var_expr: &ExprId| (*var_expr, val.clone()),
            );
        }

        let const_map = const_value
            .complete()
            .iter()
            .cloned()
            .collect::<HashMap<_, _>>();

        entrypoint.map_expressions(|expr| {
            if let Some(const_val) = const_map.get(&expr.id()) {
                match const_val {
                    Const::Bool(b) => IrExpr::BooleanLiteral {
                        value: *b,
                        annotation: (expr.id(), expr.typ().clone()),
                    },
                    Const::String(s) => IrExpr::StringLiteral {
                        value: s.clone(),
                        annotation: (expr.id(), expr.typ().clone()),
                    },
                }
            } else {
                expr
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = ConstantPropagationPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_simple_not_folding() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // !false => true
                t.if_stmt(t.not(t.bool(false)), vec![t.write("Should be true")]),
            ]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // !!true => true
                t.if_stmt(t.not(t.not(t.bool(true))), vec![t.write("Double negation")]),
            ]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // !!!false => true
                t.if_stmt(
                    t.not(t.not(t.not(t.bool(false)))),
                    vec![t.write("Triple negation")],
                ),
            ]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // true == true => true
                t.if_stmt(
                    t.eq(t.bool(true), t.bool(true)),
                    vec![t.write("true == true")],
                ),
                // false == false => true
                t.if_stmt(
                    t.eq(t.bool(false), t.bool(false)),
                    vec![t.write("false == false")],
                ),
                // true == false => false
                t.if_stmt(
                    t.eq(t.bool(true), t.bool(false)),
                    vec![t.write("Should not appear")],
                ),
            ]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // (!!false == !false) => (false == true) => false
                t.if_stmt(
                    t.eq(t.not(t.not(t.bool(false))), t.not(t.bool(false))),
                    vec![t.write("Should not appear")],
                ),
            ]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // let x = !!true
                t.let_stmt("x", t.not(t.not(t.bool(true))), |t| {
                    vec![
                        // if x => if true
                        t.if_stmt(t.var("x"), vec![t.write("x is true")]),
                        // if !x => if false
                        t.if_stmt(t.not(t.var("x")), vec![t.write("x is false")]),
                    ]
                }),
            ]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("x", t.bool(true), |t| {
                vec![t.let_stmt("y", t.not(t.bool(true)), |t| {
                    vec![
                        // x == y => true == false => false
                        t.if_stmt(t.eq(t.var("x"), t.var("y")), vec![t.write("x equals y")]),
                        // x == !y => true == true => true
                        t.if_stmt(
                            t.eq(t.var("x"), t.not(t.var("y"))),
                            vec![t.write("x equals not y")],
                        ),
                    ]
                })]
            })]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("message", t.str("Hello, World!"), |t| {
                vec![
                    // Variable containing string should be replaced with the string constant
                    t.write_expr(t.var("message"), true),
                ]
            })]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("greeting", t.str("Hello"), |t| {
                vec![t.let_stmt("name", t.str("World"), |t| {
                    vec![
                        // Both variables should be replaced with their string constants
                        t.write_expr(t.var("greeting"), true),
                        t.write_expr(t.var("name"), true),
                    ]
                })]
            })]),
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
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("title", t.str("Welcome"), |t| {
                vec![
                    // Multiple uses of the same string variable
                    t.write_expr(t.var("title"), true),
                    t.write_expr(t.var("title"), true),
                    t.let_stmt("subtitle", t.var("title"), |t| {
                        vec![
                            // Transitive propagation through another variable
                            t.write_expr(t.var("subtitle"), true),
                        ]
                    }),
                ]
            })]),
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

    #[test]
    fn test_string_equality_folding() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // "hello" == "hello" => true
                t.if_stmt(
                    t.eq(t.str("hello"), t.str("hello")),
                    vec![t.write("Strings are equal")],
                ),
                // "hello" == "world" => false
                t.if_stmt(
                    t.eq(t.str("hello"), t.str("world")),
                    vec![t.write("Should not appear")],
                ),
                // Test with variables containing strings
                t.let_stmt("greeting", t.str("hello"), |t| {
                    vec![t.let_stmt("message", t.str("hello"), |t| {
                        vec![
                            // greeting == message => "hello" == "hello" => true
                            t.if_stmt(
                                t.eq(t.var("greeting"), t.var("message")),
                                vec![t.write("Variables are equal")],
                            ),
                        ]
                    })]
                }),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    If(condition: true) {
                      Write("Strings are equal")
                    }
                    If(condition: false) {
                      Write("Should not appear")
                    }
                    Let(var: greeting, value: "hello") {
                      Let(var: message, value: "hello") {
                        If(condition: true) {
                          Write("Variables are equal")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }
}
