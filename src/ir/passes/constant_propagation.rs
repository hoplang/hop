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
                                    annotation: e.id(),
                                },
                                Const::String(s) => IrExpr::StringLiteral {
                                    value: s.clone(),
                                    annotation: e.id(),
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
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let before = entrypoint.to_string();
        let result = ConstantPropagationPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_not_folding() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.not(t.bool(false)), |t| {
                    t.write("Should be true");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if (!false) {
                    write("Should be true")
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.not(t.not(t.bool(true))), |t| {
                    t.write("Double negation");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if (!(!true)) {
                    write("Double negation")
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.not(t.not(t.not(t.bool(false)))), |t| {
                    t.write("Triple negation");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if (!(!(!false))) {
                    write("Triple negation")
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.eq(t.bool(true), t.bool(true)), |t| {
                    t.write("true == true");
                });
                t.if_stmt(t.eq(t.bool(false), t.bool(false)), |t| {
                    t.write("false == false");
                });
                t.if_stmt(t.eq(t.bool(true), t.bool(false)), |t| {
                    t.write("Should not appear");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if (true == true) {
                    write("true == true")
                  }
                  if (false == false) {
                    write("false == false")
                  }
                  if (true == false) {
                    write("Should not appear")
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.eq(t.not(t.not(t.bool(false))), t.not(t.bool(false))), |t| {
                    t.write("Should not appear");
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if ((!(!false)) == (!false)) {
                    write("Should not appear")
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("x", t.not(t.not(t.bool(true))), |t| {
                    t.if_stmt(t.var("x"), |t| {
                        t.write("x is true");
                    });
                    t.if_stmt(t.not(t.var("x")), |t| {
                        t.write("x is false");
                    });
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let x = (!(!true)) in {
                    if x {
                      write("x is true")
                    }
                    if (!x) {
                      write("x is false")
                    }
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("x", t.bool(true), |t| {
                    t.let_stmt("y", t.not(t.bool(true)), |t| {
                        t.if_stmt(t.eq(t.var("x"), t.var("y")), |t| {
                            t.write("x equals y");
                        });
                        t.if_stmt(t.eq(t.var("x"), t.not(t.var("y"))), |t| {
                            t.write("x equals not y");
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let x = true in {
                    let y = (!true) in {
                      if (x == y) {
                        write("x equals y")
                      }
                      if (x == (!y)) {
                        write("x equals not y")
                      }
                    }
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("message", t.str("Hello, World!"), |t| {
                    t.write_expr_escaped(t.var("message"));
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let message = "Hello, World!" in {
                    write_escaped(message)
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("greeting", t.str("Hello"), |t| {
                    t.let_stmt("name", t.str("World"), |t| {
                        t.write_expr_escaped(t.var("greeting"));
                        t.write_expr_escaped(t.var("name"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let greeting = "Hello" in {
                    let name = "World" in {
                      write_escaped(greeting)
                      write_escaped(name)
                    }
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("title", t.str("Welcome"), |t| {
                    t.write_expr_escaped(t.var("title"));
                    t.write_expr_escaped(t.var("title"));
                    t.let_stmt("subtitle", t.var("title"), |t| {
                        t.write_expr_escaped(t.var("subtitle"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let title = "Welcome" in {
                    write_escaped(title)
                    write_escaped(title)
                    let subtitle = title in {
                      write_escaped(subtitle)
                    }
                  }
                }

                -- after --
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
        check(
            build_ir_auto("test", vec![], |t| {
                t.if_stmt(t.eq(t.str("hello"), t.str("hello")), |t| {
                    t.write("Strings are equal");
                });
                t.if_stmt(t.eq(t.str("hello"), t.str("world")), |t| {
                    t.write("Should not appear");
                });
                t.let_stmt("greeting", t.str("hello"), |t| {
                    t.let_stmt("message", t.str("hello"), |t| {
                        t.if_stmt(t.eq(t.var("greeting"), t.var("message")), |t| {
                            t.write("Variables are equal");
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  if ("hello" == "hello") {
                    write("Strings are equal")
                  }
                  if ("hello" == "world") {
                    write("Should not appear")
                  }
                  let greeting = "hello" in {
                    let message = "hello" in {
                      if (greeting == message) {
                        write("Variables are equal")
                      }
                    }
                  }
                }

                -- after --
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
