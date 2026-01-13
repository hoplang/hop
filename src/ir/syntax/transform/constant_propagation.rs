use super::Pass;
use crate::document::document::CheapString;
use crate::dop::patterns::{EnumPattern, Match};
use crate::ir::{
    IrExpr,
    ast::ExprId,
    ast::{IrComponentDeclaration, IrStatement},
};
use datafrog::{Iteration, Relation};
use std::collections::HashMap;
use tailwind_merge::tw_merge;

/// Constant values that can be tracked during constant folding
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum Const {
    Bool(bool),
    String(String),
    Enum {
        enum_name: String,
        variant_name: String,
    },
}
/// A datafrog-based constant propagation pass that tracks and propagates constant values
pub struct ConstantPropagationPass;

impl Pass for ConstantPropagationPass {
    fn run(entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        let mut iteration = Iteration::new();

        let mut initial_constants = Vec::new();
        let mut initial_enum_constants = Vec::new();
        let mut not_relations = Vec::new();
        let mut eq_left_relations = Vec::new();
        let mut eq_right_relations = Vec::new();
        let mut concat_left_relations = Vec::new();
        let mut concat_right_relations = Vec::new();
        let mut merge_left_relations = Vec::new();
        let mut merge_right_relations = Vec::new();
        let mut var_references = Vec::new();
        let mut match_subjects = Vec::new();
        let mut match_arms_relations = Vec::new();

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
                            initial_constants.push((expr.id(), Const::String(value.to_string())));
                        }
                        IrExpr::BooleanNegation { operand, .. } => {
                            not_relations.push((operand.id(), expr.id()));
                        }
                        IrExpr::Equals { left, right, .. } => {
                            eq_left_relations.push((left.id(), expr.id()));
                            eq_right_relations.push((right.id(), expr.id()));
                        }
                        IrExpr::StringConcat { left, right, .. } => {
                            concat_left_relations.push((left.id(), expr.id()));
                            concat_right_relations.push((right.id(), expr.id()));
                        }
                        IrExpr::EnumLiteral {
                            enum_name,
                            variant_name,
                            fields,
                            ..
                        } if fields.is_empty() => {
                            // Only unit variants (no fields) can be treated as constants
                            initial_constants.push((
                                expr.id(),
                                Const::Enum {
                                    enum_name: enum_name.clone(),
                                    variant_name: variant_name.clone(),
                                },
                            ));
                            initial_enum_constants
                                .push((expr.id(), (enum_name.clone(), variant_name.clone())));
                        }
                        IrExpr::Match { match_, .. } => match match_ {
                            Match::Enum { subject, arms } => {
                                // Look up the defining statement for the subject variable
                                if let Some(defining_stmt) = scope.get(subject.0.as_str()) {
                                    let def_expr_id = match defining_stmt {
                                        IrStatement::Let { value, .. } => value.id(),
                                        IrStatement::For { array, .. } => array.id(),
                                        _ => 0,
                                    };
                                    match_subjects.push((def_expr_id, expr.id()));
                                }
                                for arm in arms {
                                    match &arm.pattern {
                                        EnumPattern::Variant {
                                            enum_name,
                                            variant_name,
                                        } => {
                                            match_arms_relations.push((
                                                (
                                                    expr.id(),
                                                    enum_name.clone(),
                                                    variant_name.clone(),
                                                ),
                                                arm.body.id(),
                                            ));
                                        }
                                    }
                                }
                            }
                            Match::Bool { .. } => {
                                // Boolean patterns are not currently constant-folded
                            }
                            Match::Option { .. } => {
                                // Option patterns are not currently constant-folded
                            }
                        },
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
                        IrExpr::MergeClasses { left, right, .. } => {
                            merge_left_relations.push((left.id(), expr.id()));
                            merge_right_relations.push((right.id(), expr.id()));
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

        // Values of left operands in string concat expressions: (concat_expr_id => left_value)
        let concat_left_value = iteration.variable::<(ExprId, Const)>("concat_left_value");

        // Values of right operands in string concat expressions: (concat_expr_id => right_value)
        let concat_right_value = iteration.variable::<(ExprId, Const)>("concat_right_value");

        // Not operations keyed by operand: (operand_id => expr_id)
        let not_rel = Relation::from_iter(not_relations);

        // Equals operations - left operand: (left_operand_id => expr_id)
        let eq_left = Relation::from_iter(eq_left_relations);

        // Equals operations - right operand: (right_operand_id => expr_id)
        let eq_right = Relation::from_iter(eq_right_relations);

        // String concat operations - left operand: (left_operand_id => expr_id)
        let concat_left = Relation::from_iter(concat_left_relations);

        // String concat operations - right operand: (right_operand_id => expr_id)
        let concat_right = Relation::from_iter(concat_right_relations);

        // MergeClasses operations - left operand: (left_operand_id => expr_id)
        let merge_left = Relation::from_iter(merge_left_relations);

        // MergeClasses operations - right operand: (right_operand_id => expr_id)
        let merge_right = Relation::from_iter(merge_right_relations);

        // Values of left operands in merge expressions: (merge_expr_id => left_value)
        let merge_left_value = iteration.variable::<(ExprId, Const)>("merge_left_value");

        // Values of right operands in merge expressions: (merge_expr_id => right_value)
        let merge_right_value = iteration.variable::<(ExprId, Const)>("merge_right_value");

        // Variable bindings: (defining_expr_id => referencing_expr_id)
        let def_to_ref = Relation::from_iter(var_references);

        // Match subject expressions: (subject_id => match_id)
        let match_subject = Relation::from_iter(match_subjects);

        // Match arms: ((match_id, enum_name, variant_name) => arm_body_id)
        let match_arm_rel = Relation::from_iter(match_arms_relations);

        // Enum constant values tracked separately for match folding: (expr_id => (enum_name, variant_name))
        let enum_const = iteration.variable::<(ExprId, (String, String))>("enum_const");
        enum_const.extend(initial_enum_constants);

        // Match expressions with known enum subjects: ((match_id, enum_name, variant_name) => match_id)
        let match_with_enum =
            iteration.variable::<((ExprId, String, String), ExprId)>("match_with_enum");

        // Selected arm bodies for matches with constant subjects: (arm_body_id => match_id)
        let selected_arm = iteration.variable::<(ExprId, ExprId)>("selected_arm");

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

            // concat_left_value(concat_expr, left_val) :- concat_left(left_op, concat_expr), const_value(left_op, left_val).
            concat_left_value.from_join(
                &const_value,
                &concat_left,
                |_: &ExprId, const_val: &Const, concat_expr: &ExprId| {
                    (*concat_expr, const_val.clone())
                },
            );

            // concat_right_value(concat_expr, right_val) :- concat_right(right_op, concat_expr), const_value(right_op, right_val).
            concat_right_value.from_join(
                &const_value,
                &concat_right,
                |_: &ExprId, const_val: &Const, concat_expr: &ExprId| {
                    (*concat_expr, const_val.clone())
                },
            );

            // const_value(concat, String(lv + rv)) :- concat_left_value(concat, String(lv)), concat_right_value(concat, String(rv)).
            const_value.from_join(
                &concat_left_value,
                &concat_right_value,
                |concat_expr: &ExprId, left_val: &Const, right_val: &Const| match (
                    left_val, right_val,
                ) {
                    (Const::String(l), Const::String(r)) => {
                        let mut result = l.clone();
                        result.push_str(r);
                        (*concat_expr, Const::String(result))
                    }
                    _ => unreachable!("StringConcat can only have string operands"),
                },
            );

            // merge_left_value(merge_expr, left_val) :- merge_left(left_op, merge_expr), const_value(left_op, left_val).
            merge_left_value.from_join(
                &const_value,
                &merge_left,
                |_: &ExprId, const_val: &Const, merge_expr: &ExprId| {
                    (*merge_expr, const_val.clone())
                },
            );

            // merge_right_value(merge_expr, right_val) :- merge_right(right_op, merge_expr), const_value(right_op, right_val).
            merge_right_value.from_join(
                &const_value,
                &merge_right,
                |_: &ExprId, const_val: &Const, merge_expr: &ExprId| {
                    (*merge_expr, const_val.clone())
                },
            );

            // const_value(merge, String(tw_merge(lv + " " + rv))) :- merge_left_value(merge, String(lv)), merge_right_value(merge, String(rv)).
            const_value.from_join(
                &merge_left_value,
                &merge_right_value,
                |merge_expr: &ExprId, left_val: &Const, right_val: &Const| match (
                    left_val, right_val,
                ) {
                    (Const::String(l), Const::String(r)) => {
                        let combined = format!("{} {}", l, r);
                        let merged = tw_merge(&combined);
                        (*merge_expr, Const::String(merged))
                    }
                    _ => unreachable!("MergeClasses can only have string operands"),
                },
            );

            // Propagate constants through variable bindings
            // const_value(referencing_expr, val) :- const_value(defining_expr, val), def_to_ref(defining_expr, referencing_expr).
            const_value.from_join(
                &const_value,
                &def_to_ref,
                |_def_expr: &ExprId, val: &Const, var_expr: &ExprId| (*var_expr, val.clone()),
            );

            // Propagate enum constants through variable bindings
            // enum_const(var, (e, v)) :- enum_const(def, (e, v)), def_to_ref(def, var).
            enum_const.from_join(
                &enum_const,
                &def_to_ref,
                |_def_expr: &ExprId, ev: &(String, String), var_expr: &ExprId| {
                    (*var_expr, ev.clone())
                },
            );

            // Find match expressions whose subject is a constant enum
            // match_with_enum((m, e, v), m) :- match_subject(s, m), enum_const(s, (e, v)).
            match_with_enum.from_join(
                &enum_const,
                &match_subject,
                |_subject: &ExprId, (e, v): &(String, String), match_id: &ExprId| {
                    ((*match_id, e.clone(), v.clone()), *match_id)
                },
            );

            // Select the matching arm body for each match with a constant subject
            // selected_arm(arm_body, m) :- match_with_enum((m, e, v), m), match_arm_rel((m, e, v), arm_body).
            selected_arm.from_join(
                &match_with_enum,
                &match_arm_rel,
                |_key: &(ExprId, String, String), match_id: &ExprId, arm_body: &ExprId| {
                    (*arm_body, *match_id)
                },
            );

            // Propagate arm body's constant value to the match expression
            // const_value(m, val) :- selected_arm(arm_body, m), const_value(arm_body, val).
            const_value.from_join(
                &const_value,
                &selected_arm,
                |_arm_body: &ExprId, val: &Const, match_id: &ExprId| (*match_id, val.clone()),
            );
        }

        let const_map: HashMap<ExprId, Const> = const_value
            .complete()
            .iter()
            .cloned()
            .collect();

        let mut result = entrypoint;
        for stmt in &mut result.body {
            stmt.traverse_mut(&mut |s| {
                if let Some(expr) = s.expr_mut() {
                    expr.traverse_mut(&mut |e| {
                        if let Some(const_val) = const_map.get(&e.id()) {
                            *e = match const_val {
                                Const::Bool(b) => IrExpr::BooleanLiteral {
                                    value: *b,
                                    id: e.id(),
                                },
                                Const::String(s) => IrExpr::StringLiteral {
                                    value: CheapString::new(s.clone()),
                                    id: e.id(),
                                },
                                Const::Enum {
                                    enum_name,
                                    variant_name,
                                } => IrExpr::EnumLiteral {
                                    enum_name: enum_name.clone(),
                                    variant_name: variant_name.clone(),
                                    // Constants are only for unit variants (no fields)
                                    fields: vec![],
                                    kind: e.as_type().clone(),
                                    id: e.id(),
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
    use crate::ir::syntax::builder::{build_ir, build_ir_with_enums};
    use expect_test::{Expect, expect};

    use super::*;

    fn check(entrypoint: IrComponentDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        let result = ConstantPropagationPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_fold_simple_boolean_negation() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.not(t.bool(false)), |t| {
                    t.write("Should be true");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (!false) {
                    write("Should be true")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Should be true")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_double_boolean_negation() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.not(t.not(t.bool(true))), |t| {
                    t.write("Double negation");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (!(!true)) {
                    write("Double negation")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Double negation")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_triple_boolean_negation() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.not(t.not(t.not(t.bool(false)))), |t| {
                    t.write("Triple negation");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (!(!(!false))) {
                    write("Triple negation")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Triple negation")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_boolean_equality_comparisons() {
        let ep = build_ir("Test", [], |t| {
            t.if_stmt(t.eq(t.bool(true), t.bool(true)), |t| {
                t.write("true == true");
            });
            t.if_stmt(t.eq(t.bool(false), t.bool(false)), |t| {
                t.write("false == false");
            });
            t.if_stmt(t.eq(t.bool(true), t.bool(false)), |t| {
                t.write("Should not appear");
            });
        });
        check(
            ep,
            expect![[r#"
                -- before --
                Test() {
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
                Test() {
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
    fn should_fold_equality_with_nested_negations() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(
                    t.eq(t.not(t.not(t.bool(false))), t.not(t.bool(false))),
                    |t| {
                        t.write("Should not appear");
                    },
                );
            }),
            expect![[r#"
                -- before --
                Test() {
                  if ((!(!false)) == (!false)) {
                    write("Should not appear")
                  }
                }

                -- after --
                Test() {
                  if false {
                    write("Should not appear")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_constants_through_variables() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
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
                Test() {
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
    fn should_fold_equality_with_variable_operands() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
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
                Test() {
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
    fn should_propagate_string_constants_through_variables() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("message", t.str("Hello, World!"), |t| {
                    t.write_expr_escaped(t.var("message"));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let message = "Hello, World!" in {
                    write_escaped(message)
                  }
                }

                -- after --
                Test() {
                  let message = "Hello, World!" in {
                    write_escaped("Hello, World!")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_nested_string_variables() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("greeting", t.str("Hello"), |t| {
                    t.let_stmt("name", t.str("World"), |t| {
                        t.write_expr_escaped(t.var("greeting"));
                        t.write_expr_escaped(t.var("name"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let greeting = "Hello" in {
                    let name = "World" in {
                      write_escaped(greeting)
                      write_escaped(name)
                    }
                  }
                }

                -- after --
                Test() {
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
    fn should_propagate_string_variable_to_multiple_uses() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
                  let title = "Welcome" in {
                    write_escaped(title)
                    write_escaped(title)
                    let subtitle = title in {
                      write_escaped(subtitle)
                    }
                  }
                }

                -- after --
                Test() {
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
    fn should_fold_string_equality_comparisons() {
        check(
            build_ir("Test", [], |t| {
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
                Test() {
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
                Test() {
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

    #[test]
    fn should_fold_nested_string_concatenation() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(
                    t.string_concat(t.string_concat(t.str("Hello"), t.str(" ")), t.str("World")),
                );
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped((("Hello" + " ") + "World"))
                }

                -- after --
                Test() {
                  write_escaped("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_string_concatenation_in_equality() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(
                    t.eq(t.string_concat(t.str("foo"), t.str("bar")), t.str("foobar")),
                    |t| {
                        t.write("Concatenation matches");
                    },
                );
                t.if_stmt(
                    t.eq(
                        t.string_concat(t.str("hello"), t.str(" world")),
                        t.str("hello"),
                    ),
                    |t| {
                        t.write("Should not appear");
                    },
                );
            }),
            expect![[r#"
                -- before --
                Test() {
                  if (("foo" + "bar") == "foobar") {
                    write("Concatenation matches")
                  }
                  if (("hello" + " world") == "hello") {
                    write("Should not appear")
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Concatenation matches")
                  }
                  if false {
                    write("Should not appear")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_string_concatenation_with_propagated_variables() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("prefix", t.str("Hello"), |t| {
                    t.let_stmt("suffix", t.string_concat(t.str(" "), t.str("World")), |t| {
                        t.let_stmt(
                            "full",
                            t.string_concat(t.var("prefix"), t.var("suffix")),
                            |t| {
                                t.write_expr_escaped(t.var("full"));
                            },
                        );
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let prefix = "Hello" in {
                    let suffix = (" " + "World") in {
                      let full = (prefix + suffix) in {
                        write_escaped(full)
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let prefix = "Hello" in {
                    let suffix = " World" in {
                      let full = "Hello World" in {
                        write_escaped("Hello World")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_simple_match_expression() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                    t.write_expr_escaped(t.match_expr(
                        t.var("color"),
                        vec![("Red", t.str("red")), ("Blue", t.str("blue"))],
                    ));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let color = Color::Red in {
                    write_escaped(match color {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                Test() {
                  let color = Color::Red in {
                    write_escaped("red")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_match_with_variable_subject() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("color", t.enum_variant("Color", "Blue"), |t| {
                    t.write_expr_escaped(t.match_expr(
                        t.var("color"),
                        vec![("Red", t.str("red")), ("Blue", t.str("blue"))],
                    ));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let color = Color::Blue in {
                    write_escaped(match color {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                Test() {
                  let color = Color::Blue in {
                    write_escaped("blue")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_match_with_constant_arm_body() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                    t.if_stmt(
                        t.match_expr(
                            t.var("color"),
                            vec![("Red", t.not(t.bool(false))), ("Blue", t.bool(false))],
                        ),
                        |t| {
                            t.write("Match evaluated to true");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let color = Color::Red in {
                    if match color {
                      Color::Red => (!false),
                      Color::Blue => false,
                    } {
                      write("Match evaluated to true")
                    }
                  }
                }

                -- after --
                Test() {
                  let color = Color::Red in {
                    if true {
                      write("Match evaluated to true")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_nested_match_in_equality() {
        check(
            build_ir_with_enums(
                "Test",
                [],
                vec![("Status", vec!["Active", "Inactive"])],
                |t| {
                    t.let_stmt("status", t.enum_variant("Status", "Active"), |t| {
                        t.if_stmt(
                            t.eq(
                                t.match_expr(
                                    t.var("status"),
                                    vec![("Active", t.str("on")), ("Inactive", t.str("off"))],
                                ),
                                t.str("on"),
                            ),
                            |t| {
                                t.write("Status is active");
                            },
                        );
                    });
                },
            ),
            expect![[r#"
                -- before --
                Test() {
                  let status = Status::Active in {
                    if (match status {
                      Status::Active => "on",
                      Status::Inactive => "off",
                    } == "on") {
                      write("Status is active")
                    }
                  }
                }

                -- after --
                Test() {
                  let status = Status::Active in {
                    if true {
                      write("Status is active")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_constant_through_variables() {
        check(
            build_ir_with_enums("Test", [], vec![("Color", vec!["Red", "Blue"])], |t| {
                t.let_stmt("x", t.enum_variant("Color", "Red"), |t| {
                    t.let_stmt("y", t.var("x"), |t| {
                        t.write_expr_escaped(t.match_expr(
                            t.var("y"),
                            vec![("Red", t.str("red")), ("Blue", t.str("blue"))],
                        ));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = Color::Red in {
                    let y = x in {
                      write_escaped(match y {
                        Color::Red => "red",
                        Color::Blue => "blue",
                      })
                    }
                  }
                }

                -- after --
                Test() {
                  let x = Color::Red in {
                    let y = Color::Red in {
                      write_escaped("red")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn sibling_let_constant_propagation() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("x", t.str("first"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.let_stmt("y", t.str("second"), |t| {
                    t.write_expr_escaped(t.var("y"));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = "first" in {
                    write_escaped(x)
                  }
                  let y = "second" in {
                    write_escaped(y)
                  }
                }

                -- after --
                Test() {
                  let x = "first" in {
                    write_escaped("first")
                  }
                  let y = "second" in {
                    write_escaped("second")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_enum_variant_fields() {
        use crate::dop::Type;
        use crate::ir::syntax::builder::IrModuleBuilder;

        let module = IrModuleBuilder::new()
            .enum_with_fields("Msg", |e| {
                e.variant_with_fields("Say", vec![("text", Type::String)]);
            })
            .component("Test", [], |t| {
                t.let_stmt(
                    "x",
                    t.enum_variant_with_fields("Msg", "Say", vec![("text", t.str("hi"))]),
                    |_| {},
                );
            })
            .build();

        check(
            module.components.into_iter().next().unwrap(),
            expect![[r#"
                -- before --
                Test() {
                  let x = Msg::Say(text: "hi") in {}
                }

                -- after --
                Test() {
                  let x = Msg::Say(text: "hi") in {}
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_constant_strings() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![
                    t.str("flex"),
                    t.str("items-center"),
                    t.str("gap-4"),
                ]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped(tw_merge("flex", tw_merge("items-center", "gap-4")))
                }

                -- after --
                Test() {
                  write_escaped("flex items-center gap-4")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_tailwind_conflicts() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![
                    t.str("px-4"),
                    t.str("py-2"),
                    t.str("p-6"),
                ]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped(tw_merge("px-4", tw_merge("py-2", "p-6")))
                }

                -- after --
                Test() {
                  write_escaped("p-6")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_propagated_variables() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("base", t.str("flex items-center"), |t| {
                    t.let_stmt("extra", t.str("gap-4 text-red-500"), |t| {
                        t.write_expr_escaped(t.merge_classes(vec![t.var("base"), t.var("extra")]));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let base = "flex items-center" in {
                    let extra = "gap-4 text-red-500" in {
                      write_escaped(tw_merge(base, extra))
                    }
                  }
                }

                -- after --
                Test() {
                  let base = "flex items-center" in {
                    let extra = "gap-4 text-red-500" in {
                      write_escaped("flex items-center gap-4 text-red-500")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_empty_merge_classes() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped("")
                }

                -- after --
                Test() {
                  write_escaped("")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_nested_merge_classes() {
        check(
            build_ir("Test", [], |t| {
                t.write_expr_escaped(t.merge_classes(vec![
                    t.str("px-4"),
                    t.merge_classes(vec![t.str("px-2"), t.str("p-3")]),
                ]));
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped(tw_merge("px-4", tw_merge("px-2", "p-3")))
                }

                -- after --
                Test() {
                  write_escaped("p-3")
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_enum_match() {
        check(
            build_ir_with_enums("Test", [], vec![("Size", vec!["Small", "Large"])], |t| {
                t.let_stmt("size", t.enum_variant("Size", "Large"), |t| {
                    t.write_expr_escaped(t.merge_classes(vec![
                        t.str("px-4"),
                        t.match_expr(
                            t.var("size"),
                            vec![("Small", t.str("text-sm")), ("Large", t.str("text-lg"))],
                        ),
                    ]));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let size = Size::Large in {
                    write_escaped(tw_merge("px-4", match size {
                      Size::Small => "text-sm",
                      Size::Large => "text-lg",
                    }))
                  }
                }

                -- after --
                Test() {
                  let size = Size::Large in {
                    write_escaped("px-4 text-lg")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_merge_classes_with_enum_match_containing_merge_classes() {
        check(
            build_ir_with_enums("Test", [], vec![("Size", vec!["Small", "Large"])], |t| {
                t.let_stmt("size", t.enum_variant("Size", "Large"), |t| {
                    t.write_expr_escaped(t.merge_classes(vec![
                        t.str("flex"),
                        t.match_expr(
                            t.var("size"),
                            vec![
                                ("Small", t.merge_classes(vec![t.str("p-2"), t.str("text-sm")])),
                                ("Large", t.merge_classes(vec![t.str("p-4"), t.str("text-lg")])),
                            ],
                        ),
                    ]));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let size = Size::Large in {
                    write_escaped(tw_merge("flex", match size {
                      Size::Small => tw_merge("p-2", "text-sm"),
                      Size::Large => tw_merge("p-4", "text-lg"),
                    }))
                  }
                }

                -- after --
                Test() {
                  let size = Size::Large in {
                    write_escaped("flex p-4 text-lg")
                  }
                }
            "#]],
        );
    }
}
