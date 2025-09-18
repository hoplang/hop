use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{BinaryOp, ExprId, UnaryOp},
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

impl Pass for ConstantPropagationPass {
    fn run(entrypoint: IrEntrypoint) -> IrEntrypoint {
        let mut iteration = Iteration::new();

        let mut initial_constants = Vec::new();
        let mut not_relations = Vec::new();
        let mut eq_left_relations = Vec::new();
        let mut eq_right_relations = Vec::new();
        let mut var_references = Vec::new();

        for stmt in &entrypoint.body {
            stmt.traverse_with_scope(&mut |s, scope| {
                let Some(primary_expr) = s.expr() else {
                    return;
                };
                primary_expr.traverse(&mut |expr| {
                    match expr {
                        IrExpr::BooleanLiteral { value, .. } => {
                            initial_constants.push((expr.id(), Const::Bool(*value)));
                        }
                        IrExpr::StringLiteral { value, .. } => {
                            initial_constants.push((expr.id(), Const::String(value.clone())));
                        }
                        IrExpr::UnaryOp {
                            operator: UnaryOp::Not,
                            operand,
                            ..
                        } => {
                            not_relations.push((operand.id(), expr.id()));
                        }
                        IrExpr::BinaryOp {
                            operator: BinaryOp::Eq,
                            left,
                            right,
                            ..
                        } => {
                            eq_left_relations.push((left.id(), expr.id()));
                            eq_right_relations.push((right.id(), expr.id()));
                        }
                        IrExpr::Var { value: name, .. } => {
                            // Check if this variable is defined by a Let or For statement
                            if let Some(defining_stmt) = scope.get(&name.to_string()) {
                                let def_expr_id = match defining_stmt {
                                    IrStatement::Let { value, .. } => value.id(),
                                    IrStatement::For { array, .. } => array.id(),
                                    _ => return,
                                };
                                var_references.push((def_expr_id, expr.id()));
                            }
                        }
                        _ => {}
                    }
                });
            });
        }

        // Constant values of expressions: (expr_id => const_value)
        let const_value = iteration.variable::<(ExprId, Const)>("const_value");
        const_value.extend(initial_constants);

        // Values of left operands in equality expressions: (eq_expr_id => left_value)
        let eq_left_value = iteration.variable::<(ExprId, Const)>("eq_left_value");

        // Values of right operands in equality expressions: (eq_expr_id => right_value)
        let eq_right_value = iteration.variable::<(ExprId, Const)>("eq_right_value");

        // Not operations keyed by operand: (operand_id => expr_id)
        let not_rel = Relation::from_iter(not_relations);

        // Equality operations - left operand: (left_operand_id => expr_id)
        let eq_left = Relation::from_iter(eq_left_relations);

        // Equality operations - right operand: (right_operand_id => expr_id)
        let eq_right = Relation::from_iter(eq_right_relations);

        // Variable bindings: (defining_expr_id => referencing_expr_id)
        let def_to_ref = Relation::from_iter(var_references);

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

        let mut result = entrypoint;
        for stmt in &mut result.body {
            stmt.traverse_mut(&mut |s| {
                if let Some(expr) = s.expr_mut() {
                    expr.traverse_mut(&mut |e| {
                        if let Some(const_val) = const_map.get(&e.id()) {
                            *e = match const_val {
                                Const::Bool(b) => IrExpr::BooleanLiteral {
                                    value: *b,
                                    annotation: (e.id(), e.typ().clone()),
                                },
                                Const::String(s) => IrExpr::StringLiteral {
                                    value: s.clone(),
                                    annotation: (e.id(), e.typ().clone()),
                                },
                            };
                        }
                    });
                }
            });
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let result = ConstantPropagationPass::run(entrypoint);
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
                test() {
                  if true {
                    write("Should be true")
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
                test() {
                  if true {
                    write("Double negation")
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
                test() {
                  if true {
                    write("Triple negation")
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
                test() {
                  if true {
                    write("true == true")
                  }
                  if true {
                    write("false == false")
                  }
                  if false {
                    write("Should not appear")
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
                test() {
                  if false {
                    write("Should not appear")
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
                test() {
                  let x = true in {
                    if true {
                      write("x is true")
                    }
                    if false {
                      write("x is false")
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
                test() {
                  let x = true in {
                    let y = false in {
                      if false {
                        write("x equals y")
                      }
                      if true {
                        write("x equals not y")
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
                test() {
                  let message = "Hello, World!" in {
                    write_escaped("Hello, World!")
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
                test() {
                  let greeting = "Hello" in {
                    let name = "World" in {
                      write_escaped("Hello")
                      write_escaped("World")
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
                test() {
                  let title = "Welcome" in {
                    write_escaped("Welcome")
                    write_escaped("Welcome")
                    let subtitle = "Welcome" in {
                      write_escaped("Welcome")
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
                test() {
                  if true {
                    write("Strings are equal")
                  }
                  if false {
                    write("Should not appear")
                  }
                  let greeting = "hello" in {
                    let message = "hello" in {
                      if true {
                        write("Variables are equal")
                      }
                    }
                  }
                }
            "#]],
        );
    }
}
