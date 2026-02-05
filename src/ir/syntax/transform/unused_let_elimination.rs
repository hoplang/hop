use crate::dop::patterns::Match;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypointDeclaration, IrForSource, IrStatement, StatementId},
};
use std::collections::HashSet;

/// A pass that eliminates unused let statements and unused match bindings.
/// - Unused let statements are replaced with their body
/// - Unused Option match bindings are set to None (wildcard)
/// - Unused Enum match bindings are removed from the bindings list
pub struct UnusedLetEliminationPass;

/// Collected information about unused variables
struct UnusedVars {
    /// Let statements with unused variables
    unused_lets: HashSet<StatementId>,
    /// Match statements with unused Option bindings
    unused_option_bindings: HashSet<StatementId>,
    /// Match statements with unused Enum bindings: (stmt_id, field_name)
    unused_enum_bindings: HashSet<(StatementId, String)>,
}

impl UnusedLetEliminationPass {
    /// Collect which let statements and match bindings have unused variables
    fn collect_unused_vars(entrypoint: &IrEntrypointDeclaration) -> UnusedVars {
        let mut all_lets = HashSet::new();
        let mut used_lets = HashSet::new();
        // Track Option match bindings: stmt_id -> binding_name
        let mut option_bindings: Vec<(StatementId, String)> = Vec::new();
        let mut used_option_bindings: HashSet<StatementId> = HashSet::new();
        // Track Enum match bindings: (stmt_id, field_name) -> binding_name
        let mut enum_bindings: Vec<((StatementId, String), String)> = Vec::new();
        let mut used_enum_bindings: HashSet<(StatementId, String)> = HashSet::new();

        // First collect all let statement IDs and match bindings
        for stmt in &entrypoint.body {
            stmt.traverse(&mut |s| match s {
                IrStatement::Let { id, .. } => {
                    all_lets.insert(*id);
                }
                IrStatement::Match { id, match_ } => match match_ {
                    Match::Option {
                        some_arm_binding: Some(binding),
                        ..
                    } => {
                        option_bindings.push((*id, binding.to_string()));
                    }
                    Match::Enum { arms, .. } => {
                        for arm in arms {
                            for (field_name, var_name) in &arm.bindings {
                                enum_bindings
                                    .push(((*id, field_name.to_string()), var_name.to_string()));
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
            });
        }

        // Helper to check if a variable name is an Option or Enum binding use
        let check_binding_use =
            |name_str: &str,
             option_bindings: &[(StatementId, String)],
             enum_bindings: &[((StatementId, String), String)],
             used_option_bindings: &mut HashSet<StatementId>,
             used_enum_bindings: &mut HashSet<(StatementId, String)>| {
                for (stmt_id, binding_name) in option_bindings {
                    if binding_name == name_str {
                        used_option_bindings.insert(*stmt_id);
                    }
                }
                for ((stmt_id, field_name), binding_name) in enum_bindings {
                    if binding_name == name_str {
                        used_enum_bindings.insert((*stmt_id, field_name.clone()));
                    }
                }
            };

        // Now traverse with scope tracking to find used variables
        for stmt in &entrypoint.body {
            stmt.traverse_with_scope(&mut |s, scope| {
                // Check for match statement subjects (which are variable references)
                if let IrStatement::Match { match_, .. } = s {
                    let subject_name = match match_ {
                        Match::Bool { subject, .. } => &subject.0,
                        Match::Enum { subject, .. } => &subject.0,
                        Match::Option { subject, .. } => &subject.0,
                    };
                    let name_str = subject_name.to_string();
                    if let Some(IrStatement::Let { id, .. }) = scope.get(&name_str) {
                        used_lets.insert(*id);
                    }
                    // Also check if the subject is an Option or Enum binding
                    check_binding_use(
                        &name_str,
                        &option_bindings,
                        &enum_bindings,
                        &mut used_option_bindings,
                        &mut used_enum_bindings,
                    );
                }

                // Process only the primary expression of this statement (not nested ones)
                // This avoids O(n²) behavior from traverse_exprs
                let mut check_expr = |primary_expr: &IrExpr| {
                    primary_expr.traverse(&mut |expr| {
                        // Check for direct variable references
                        if let IrExpr::Var { value: name, .. } = expr {
                            let name_str = name.to_string();
                            if let Some(IrStatement::Let { id, .. }) = scope.get(&name_str) {
                                used_lets.insert(*id);
                            }
                            // Check if this is an Option or Enum binding use
                            check_binding_use(
                                &name_str,
                                &option_bindings,
                                &enum_bindings,
                                &mut used_option_bindings,
                                &mut used_enum_bindings,
                            );
                        }
                        // Check for match expression subjects (which are variable references)
                        if let IrExpr::Match { match_, .. } = expr {
                            let subject_name = match match_ {
                                Match::Bool { subject, .. } => &subject.0,
                                Match::Enum { subject, .. } => &subject.0,
                                Match::Option { subject, .. } => &subject.0,
                            };
                            let name_str = subject_name.to_string();
                            if let Some(IrStatement::Let { id, .. }) = scope.get(&name_str) {
                                used_lets.insert(*id);
                            }
                            // Also check if the subject is an Option or Enum binding
                            check_binding_use(
                                &name_str,
                                &option_bindings,
                                &enum_bindings,
                                &mut used_option_bindings,
                                &mut used_enum_bindings,
                            );
                        }
                    });
                };

                if let Some(primary_expr) = s.expr() {
                    check_expr(primary_expr);
                }

                // Also check for loop range expressions (not covered by s.expr())
                if let IrStatement::For {
                    source: IrForSource::RangeInclusive { start, end },
                    ..
                } = s
                {
                    check_expr(start);
                    check_expr(end);
                }
            });
        }

        // Compute unused sets
        let unused_lets = all_lets.difference(&used_lets).cloned().collect();
        let unused_option_bindings = option_bindings
            .iter()
            .filter(|(id, _)| !used_option_bindings.contains(id))
            .map(|(id, _)| *id)
            .collect();
        let all_enum_binding_keys: HashSet<_> = enum_bindings
            .iter()
            .map(|((id, field), _)| (*id, field.clone()))
            .collect();
        let unused_enum_bindings = all_enum_binding_keys
            .difference(&used_enum_bindings)
            .cloned()
            .collect();

        UnusedVars {
            unused_lets,
            unused_option_bindings,
            unused_enum_bindings,
        }
    }

    /// Transform a list of statements, eliminating unused lets.
    /// Returns (transformed_statements, made_changes).
    fn transform_statements(
        statements: Vec<IrStatement>,
        unused_vars: &UnusedVars,
    ) -> (Vec<IrStatement>, bool) {
        let mut made_changes = false;
        let result = statements
            .into_iter()
            .flat_map(|stmt| match stmt {
                // Unused let - replace with its body, recursively transforming it
                IrStatement::Let { body, id, .. } if unused_vars.unused_lets.contains(&id) => {
                    made_changes = true;
                    Self::transform_statements(body, unused_vars).0
                }

                // All other statements pass through
                other => vec![other],
            })
            .collect();
        (result, made_changes)
    }

    /// Remove unused bindings from a Match. Returns true if any changes were made.
    fn transform_match_bindings(
        match_: &mut Match<Vec<IrStatement>>,
        id: StatementId,
        unused_vars: &UnusedVars,
    ) -> bool {
        match match_ {
            Match::Option {
                some_arm_binding, ..
            } => {
                if some_arm_binding.is_some() && unused_vars.unused_option_bindings.contains(&id) {
                    *some_arm_binding = None;
                    true
                } else {
                    false
                }
            }
            Match::Enum { arms, .. } => {
                let mut changed = false;
                for arm in arms {
                    let original_len = arm.bindings.len();
                    arm.bindings.retain(|(field_name, _)| {
                        !unused_vars
                            .unused_enum_bindings
                            .contains(&(id, field_name.to_string()))
                    });
                    if arm.bindings.len() != original_len {
                        changed = true;
                    }
                }
                changed
            }
            Match::Bool { .. } => false,
        }
    }

    pub fn run(entrypoint: &mut IrEntrypointDeclaration) {
        // Iterate until no more changes are made.
        // This is necessary because removing a let statement may make another
        // variable unused (e.g., removing `let val = v0` makes `v0` unused).
        loop {
            let unused_vars = Self::collect_unused_vars(entrypoint);
            let mut made_changes = false;

            // Use visit_mut to recursively process nested bodies and transform match bindings
            for stmt in &mut entrypoint.body {
                stmt.traverse_mut(&mut |s| match s {
                    IrStatement::If {
                        body, else_body, ..
                    } => {
                        let (new_body, changed) =
                            Self::transform_statements(std::mem::take(body), &unused_vars);
                        *body = new_body;
                        if changed {
                            made_changes = true;
                        }
                        *else_body = else_body.take().map(|else_body| {
                            let (new_body, changed) =
                                Self::transform_statements(else_body, &unused_vars);
                            if changed {
                                made_changes = true;
                            }
                            new_body
                        })
                    }
                    IrStatement::For { body, .. } | IrStatement::Let { body, .. } => {
                        let (new_body, changed) =
                            Self::transform_statements(std::mem::take(body), &unused_vars);
                        *body = new_body;
                        if changed {
                            made_changes = true;
                        }
                    }
                    IrStatement::Match { id, match_ } => {
                        if Self::transform_match_bindings(match_, *id, &unused_vars) {
                            made_changes = true;
                        }
                        // Also transform statements inside Match arm bodies
                        match match_ {
                            Match::Option {
                                some_arm_body,
                                none_arm_body,
                                ..
                            } => {
                                let (new_some_body, some_changed) = Self::transform_statements(
                                    std::mem::take(some_arm_body.as_mut()),
                                    &unused_vars,
                                );
                                **some_arm_body = new_some_body;
                                let (new_none_body, none_changed) = Self::transform_statements(
                                    std::mem::take(none_arm_body.as_mut()),
                                    &unused_vars,
                                );
                                **none_arm_body = new_none_body;
                                if some_changed || none_changed {
                                    made_changes = true;
                                }
                            }
                            Match::Bool {
                                true_body,
                                false_body,
                                ..
                            } => {
                                let (new_true_body, true_changed) = Self::transform_statements(
                                    std::mem::take(true_body.as_mut()),
                                    &unused_vars,
                                );
                                **true_body = new_true_body;
                                let (new_false_body, false_changed) = Self::transform_statements(
                                    std::mem::take(false_body.as_mut()),
                                    &unused_vars,
                                );
                                **false_body = new_false_body;
                                if true_changed || false_changed {
                                    made_changes = true;
                                }
                            }
                            Match::Enum { arms, .. } => {
                                for arm in arms {
                                    let (new_body, changed) = Self::transform_statements(
                                        std::mem::take(&mut arm.body),
                                        &unused_vars,
                                    );
                                    arm.body = new_body;
                                    if changed {
                                        made_changes = true;
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                });
            }

            // Then transform top-level statements
            let (new_body, changed) =
                Self::transform_statements(std::mem::take(&mut entrypoint.body), &unused_vars);
            entrypoint.body = new_body;
            if changed {
                made_changes = true;
            }

            if !made_changes {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dop::Type,
        ir::syntax::builder::{IrBuilder, IrModuleBuilder, build_ir_no_params},
    };
    use expect_test::{Expect, expect};

    fn check(mut entrypoint: IrEntrypointDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        UnusedLetEliminationPass::run(&mut entrypoint);
        let after = entrypoint.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_eliminate_unused_let_in_outermost_scope() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("unused", t.str("value"), |t| {
                    t.write("Hello");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let unused = "value" in {
                    write("Hello")
                  }
                }

                -- after --
                Test() {
                  write("Hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_text_expression() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("message", t.str("Hello"), |t| {
                    t.write_expr(t.var("message"), false);
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let message = "Hello" in {
                    write_expr(message)
                  }
                }

                -- after --
                Test() {
                  let message = "Hello" in {
                    write_expr(message)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_if_statement() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("cond", t.bool(true), |t| {
                    t.if_stmt(t.var("cond"), |t| {
                        t.write("Condition is true");
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let cond = true in {
                    if cond {
                      write("Condition is true")
                    }
                  }
                }

                -- after --
                Test() {
                  let cond = true in {
                    if cond {
                      write("Condition is true")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statement_inside_if_body() {
        check(
            build_ir_no_params("Test", |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.let_stmt("unused", t.str("value"), |t| {
                        t.write("Inside if");
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  if true {
                    let unused = "value" in {
                      write("Inside if")
                    }
                  }
                }

                -- after --
                Test() {
                  if true {
                    write("Inside if")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statement_inside_for_loop_body() {
        check(
            build_ir_no_params("Test", |t| {
                let items = t.array(vec![t.str("a"), t.str("b")]);
                t.for_loop("item", items, |t| {
                    t.let_stmt("unused", t.str("value"), |t| {
                        t.write_expr(t.var("item"), false);
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  for item in ["a", "b"] {
                    let unused = "value" in {
                      write_expr(item)
                    }
                  }
                }

                -- after --
                Test() {
                  for item in ["a", "b"] {
                    write_expr(item)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_binary_op() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("x", t.bool(true), |t| {
                    t.let_stmt("y", t.bool(false), |t| {
                        t.if_stmt(t.eq(t.var("x"), t.var("y")), |t| {
                            t.write("Equal");
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = true in {
                    let y = false in {
                      if (x == y) {
                        write("Equal")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let x = true in {
                    let y = false in {
                      if (x == y) {
                        write("Equal")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statements_declared_in_sequence() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("a", t.str("a_value"), |t| {
                    t.write("First");
                });
                t.let_stmt("b", t.str("b_value"), |t| {
                    t.write("Second");
                });
                t.write("Third");
            }),
            expect![[r#"
                -- before --
                Test() {
                  let a = "a_value" in {
                    write("First")
                  }
                  let b = "b_value" in {
                    write("Second")
                  }
                  write("Third")
                }

                -- after --
                Test() {
                  write("First")
                  write("Second")
                  write("Third")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_inside_array() {
        check(
            build_ir_no_params("Test", |t| {
                let items = t.array(vec![t.str("a"), t.str("b")]);
                t.let_stmt("items", items, |t| {
                    t.for_loop("item", t.var("items"), |t| {
                        t.write_expr(t.var("item"), false);
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let items = ["a", "b"] in {
                    for item in items {
                      write_expr(item)
                    }
                  }
                }

                -- after --
                Test() {
                  let items = ["a", "b"] in {
                    for item in items {
                      write_expr(item)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_for_range_end() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("count", t.int(3), |t| {
                    t.for_range("i", t.int(1), t.var("count"), |t| {
                        t.write_expr(t.int_to_string(t.var("i")), false);
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let count = 3 in {
                    for i in 1..=count {
                      write_expr(i.to_string())
                    }
                  }
                }

                -- after --
                Test() {
                  let count = 3 in {
                    for i in 1..=count {
                      write_expr(i.to_string())
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_for_range_start() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("start", t.int(1), |t| {
                    t.for_range("i", t.var("start"), t.int(5), |t| {
                        t.write_expr(t.int_to_string(t.var("i")), false);
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let start = 1 in {
                    for i in start..=5 {
                      write_expr(i.to_string())
                    }
                  }
                }

                -- after --
                Test() {
                  let start = 1 in {
                    for i in start..=5 {
                      write_expr(i.to_string())
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_discarded_for_range() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("count", t.int(3), |t| {
                    t.for_range_discarded(t.int(1), t.var("count"), |t| {
                        t.write("x");
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let count = 3 in {
                    for _ in 1..=count {
                      write("x")
                    }
                  }
                }

                -- after --
                Test() {
                  let count = 3 in {
                    for _ in 1..=count {
                      write("x")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statement_when_sibling_statement_uses_same_variable() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("x", t.str("first x"), |t| {
                    t.write_expr(t.var("x"), false);
                });
                t.let_stmt("x", t.str("second x"), |t| {
                    t.write("No reference to x here");
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = "first x" in {
                    write_expr(x)
                  }
                  let x = "second x" in {
                    write("No reference to x here")
                  }
                }

                -- after --
                Test() {
                  let x = "first x" in {
                    write_expr(x)
                  }
                  write("No reference to x here")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_nested_unused_let_statements() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("outer", t.str("outer_value"), |t| {
                    t.let_stmt("inner", t.str("inner_value"), |t| {
                        t.write("No variables used");
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let outer = "outer_value" in {
                    let inner = "inner_value" in {
                      write("No variables used")
                    }
                  }
                }

                -- after --
                Test() {
                  write("No variables used")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_deeply_nested_unused_let_statements() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("level1", t.str("value1"), |t| {
                    t.let_stmt("level2", t.str("value2"), |t| {
                        t.let_stmt("level3", t.str("value3"), |t| {
                            t.let_stmt("level4", t.str("value4"), |t| {
                                t.write("Deeply nested, no variables used");
                            });
                        });
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let level1 = "value1" in {
                    let level2 = "value2" in {
                      let level3 = "value3" in {
                        let level4 = "value4" in {
                          write("Deeply nested, no variables used")
                        }
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  write("Deeply nested, no variables used")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_used_in_bool_match_expr() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("flag", t.bool(true), |t| {
                    t.write_expr(
                        t.bool_match_expr(t.var("flag"), t.str("yes"), t.str("no")),
                        true,
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let flag = true in {
                    write_escaped(match flag {true => "yes", false => "no"})
                  }
                }

                -- after --
                Test() {
                  let flag = true in {
                    write_escaped(match flag {true => "yes", false => "no"})
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_used_as_enum_match_subject() {
        let module = IrModuleBuilder::new()
            .enum_with_fields("BadgeElement", |e| {
                e.variant("Span");
                e.variant_with_fields("Link", vec![("href", Type::String)]);
            })
            .component_no_params("Test", |t| {
                t.let_stmt("element", t.enum_variant("BadgeElement", "Span"), |t| {
                    t.let_stmt("match_subject", t.var("element"), |t| {
                        t.enum_match_stmt_with_bindings(
                            t.var("match_subject"),
                            vec![
                                (
                                    "Span",
                                    vec![],
                                    Box::new(|t: &mut IrBuilder| {
                                        t.write("<span>badge</span>");
                                    }),
                                ),
                                (
                                    "Link",
                                    vec![("href", "h")],
                                    Box::new(|t: &mut IrBuilder| {
                                        t.write("<a>badge</a>");
                                    }),
                                ),
                            ],
                        );
                    });
                });
            })
            .build();

        let entrypoint = module.entrypoints.into_iter().next().unwrap();
        check(
            entrypoint,
            expect![[r#"
                -- before --
                Test() {
                  let element = BadgeElement::Span in {
                    let match_subject = element in {
                      match match_subject {
                        BadgeElement::Span => {
                          write("<span>badge</span>")
                        }
                        BadgeElement::Link(href: h) => {
                          write("<a>badge</a>")
                        }
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let element = BadgeElement::Span in {
                    let match_subject = element in {
                      match match_subject {
                        BadgeElement::Span => {
                          write("<span>badge</span>")
                        }
                        BadgeElement::Link => {
                          write("<a>badge</a>")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_used_in_enum_literal() {
        let module = IrModuleBuilder::new()
            .enum_with_fields("MyEnum", |e| {
                e.variant_with_fields("Foo", vec![("value", Type::String)]);
            })
            .component_no_params("Test", |t| {
                // let x = "hello"
                // let foo = MyEnum::Foo(value: x)
                // match foo { Foo(v) => write(v) }  -- uses foo, which uses x
                t.let_stmt("x", t.str("hello"), |t| {
                    t.let_stmt(
                        "foo",
                        t.enum_variant_with_fields("MyEnum", "Foo", vec![("value", t.var("x"))]),
                        |t| {
                            t.enum_match_stmt_with_bindings(
                                t.var("foo"),
                                vec![(
                                    "Foo",
                                    vec![("value", "v")],
                                    Box::new(|t: &mut IrBuilder| {
                                        t.write_expr(t.var("v"), true);
                                    }),
                                )],
                            );
                        },
                    );
                });
            })
            .build();

        let entrypoint = module.entrypoints.into_iter().next().unwrap();
        check(
            entrypoint,
            expect![[r#"
                -- before --
                Test() {
                  let x = "hello" in {
                    let foo = MyEnum::Foo {value: x} in {
                      match foo {
                        MyEnum::Foo(value: v) => {
                          write_escaped(v)
                        }
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let x = "hello" in {
                    let foo = MyEnum::Foo {value: x} in {
                      match foo {
                        MyEnum::Foo(value: v) => {
                          write_escaped(v)
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_used_in_enum_variant_field() {
        let module = IrModuleBuilder::new()
            .enum_with_fields("BadgeElement", |e| {
                e.variant("Span");
                e.variant_with_fields("Link", vec![("href", Type::String)]);
            })
            .component_no_params("Test", |t| {
                // let href = "/home"
                t.let_stmt("href", t.str("/home"), |t| {
                    // let element = BadgeElement::Link(href: href)
                    t.let_stmt(
                        "element",
                        t.enum_variant_with_fields(
                            "BadgeElement",
                            "Link",
                            vec![("href", t.var("href"))],
                        ),
                        |t| {
                            // let match_subject = element
                            t.let_stmt("match_subject", t.var("element"), |t| {
                                // match match_subject { Span => ..., Link(h) => ... }
                                t.enum_match_stmt_with_bindings(
                                    t.var("match_subject"),
                                    vec![
                                        (
                                            "Span",
                                            vec![],
                                            Box::new(|t: &mut IrBuilder| {
                                                t.write("<span>badge</span>");
                                            }),
                                        ),
                                        (
                                            "Link",
                                            vec![("href", "h")],
                                            Box::new(|t: &mut IrBuilder| {
                                                t.write_expr(t.var("h"), true);
                                            }),
                                        ),
                                    ],
                                );
                            });
                        },
                    );
                });
            })
            .build();

        let entrypoint = module.entrypoints.into_iter().next().unwrap();
        check(
            entrypoint,
            expect![[r#"
                -- before --
                Test() {
                  let href = "/home" in {
                    let element = BadgeElement::Link {href: href} in {
                      let match_subject = element in {
                        match match_subject {
                          BadgeElement::Span => {
                            write("<span>badge</span>")
                          }
                          BadgeElement::Link(href: h) => {
                            write_escaped(h)
                          }
                        }
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let href = "/home" in {
                    let element = BadgeElement::Link {href: href} in {
                      let match_subject = element in {
                        match match_subject {
                          BadgeElement::Span => {
                            write("<span>badge</span>")
                          }
                          BadgeElement::Link(href: h) => {
                            write_escaped(h)
                          }
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_unused_let_inside_option_match_arm_body() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.option_match_stmt(
                        t.var("opt"),
                        Some("v0"),
                        |t| {
                            t.let_stmt("val", t.var("v0"), |t| {
                                t.write("constant");
                            });
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(v0) => {
                        let val = v0 in {
                          write("constant")
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(_) => {
                        write("constant")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_unused_option_match_binding() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.option_match_stmt(
                        t.var("opt"),
                        Some("unused_binding"),
                        |t| {
                            t.write("some");
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(unused_binding) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_option_match_binding_when_used_as_nested_match_subject() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("outer_opt", t.some(t.some(t.str("deep"))), |t| {
                    t.option_match_stmt(
                        t.var("outer_opt"),
                        Some("inner"),
                        |t| {
                            t.option_match_stmt(
                                t.var("inner"),
                                Some("value"),
                                |t| {
                                    t.write_expr(t.var("value"), false);
                                },
                                |t| {
                                    t.write("inner-none");
                                },
                            );
                        },
                        |t| {
                            t.write("outer-none");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let outer_opt = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match outer_opt {
                      Some(inner) => {
                        match inner {
                          Some(value) => {
                            write_expr(value)
                          }
                          None => {
                            write("inner-none")
                          }
                        }
                      }
                      None => {
                        write("outer-none")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let outer_opt = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match outer_opt {
                      Some(inner) => {
                        match inner {
                          Some(value) => {
                            write_expr(value)
                          }
                          None => {
                            write("inner-none")
                          }
                        }
                      }
                      None => {
                        write("outer-none")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_cascading_unused_variables() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("opt", t.some(t.str("hello")), |t| {
                    t.option_match_stmt(
                        t.var("opt"),
                        Some("v0"),
                        |t| {
                            t.let_stmt("val", t.var("v0"), |t| {
                                t.write("constant");
                            });
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(v0) => {
                        let val = v0 in {
                          write("constant")
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(_) => {
                        write("constant")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_unused_let_inside_bool_match_arm_body() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_stmt("flag", t.bool(true), |t| {
                    t.bool_match_stmt(
                        t.var("flag"),
                        |t| {
                            t.let_stmt("unused", t.str("not used"), |t| {
                                t.write("true branch");
                            });
                        },
                        |t| {
                            t.write("false branch");
                        },
                    );
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let flag = true in {
                    match flag {
                      true => {
                        let unused = "not used" in {
                          write("true branch")
                        }
                      }
                      false => {
                        write("false branch")
                      }
                    }
                  }
                }

                -- after --
                Test() {
                  let flag = true in {
                    match flag {
                      true => {
                        write("true branch")
                      }
                      false => {
                        write("false branch")
                      }
                    }
                  }
                }
            "#]],
        );
    }
}
