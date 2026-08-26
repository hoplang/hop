use crate::expr::patterns::Match;
use crate::ir::{
    IrExpr,
    ir_module::{IrStatement, StatementId, VarId, traverse_statements_mut},
};
use std::collections::{HashMap, HashSet};

/// Collected information about unused variables
struct UnusedVars {
    /// Let statements with unused variables
    unused_lets: HashSet<StatementId>,
    /// Match statements with unused Option bindings
    unused_option_bindings: HashSet<StatementId>,
    /// Enum match arm bindings that are unused
    unused_enum_bindings: HashSet<VarId>,
    /// For statements whose loop variable is unused
    unused_for_bindings: HashSet<StatementId>,
}

/// A pass that eliminates unused variable declarations.
/// - Unused let statements are replaced with their body
/// - Unused Option match bindings are set to `_` (wildcard)
/// - Unused Enum match bindings are removed from the bindings list
/// - Unused for loop variables are set to `_` (wildcard)
pub fn eliminate_unused_variable_declarations(body: &mut Vec<IrStatement>) {
    loop {
        let unused_vars = collect_unused_vars(body);
        let mut made_changes = false;

        traverse_statements_mut(body, &mut |stmts| {
            let mut changed = false;
            let mut transformed = Vec::new();
            for stmt in std::mem::take(stmts) {
                match stmt {
                    IrStatement::Let { body, id, .. } if unused_vars.unused_lets.contains(&id) => {
                        changed = true;
                        transformed.extend(body);
                    }
                    IrStatement::Match { id, mut match_ } => {
                        match &mut match_ {
                            Match::Option {
                                some_arm_binding, ..
                            } => {
                                if some_arm_binding.is_some()
                                    && unused_vars.unused_option_bindings.contains(&id)
                                {
                                    *some_arm_binding = None;
                                    changed = true;
                                }
                            }
                            Match::Enum { arms, .. } => {
                                for arm in arms {
                                    let before = arm.bindings.len();
                                    arm.bindings.retain(|(_, var_name)| {
                                        !unused_vars.unused_enum_bindings.contains(&var_name.id)
                                    });
                                    if arm.bindings.len() != before {
                                        changed = true;
                                    }
                                }
                            }
                            Match::Bool { .. } => {}
                        }
                        transformed.push(IrStatement::Match { id, match_ });
                    }
                    IrStatement::For {
                        id,
                        mut var,
                        source,
                        body,
                    } => {
                        if var.is_some() && unused_vars.unused_for_bindings.contains(&id) {
                            var = None;
                            changed = true;
                        }
                        transformed.push(IrStatement::For {
                            id,
                            var,
                            source,
                            body,
                        });
                    }
                    other => transformed.push(other),
                }
            }
            *stmts = transformed;
            if changed {
                made_changes = true;
            }
        });

        if !made_changes {
            break;
        }
    }
}

/// Collect which let statements and match bindings have unused variables.
/// Each binder within a declaration has a distinct `VarId`, so a flat table
/// suffices here and no scope bookkeeping is needed.
fn collect_unused_vars(body: &[IrStatement]) -> UnusedVars {
    // All variables that are referenced anywhere
    let mut used_vars: HashSet<VarId> = HashSet::new();

    let mut let_bindings: HashMap<VarId, StatementId> = HashMap::new();
    let mut option_bindings: HashMap<VarId, StatementId> = HashMap::new();
    let mut enum_bindings: HashSet<VarId> = HashSet::new();
    let mut for_bindings: HashMap<VarId, StatementId> = HashMap::new();

    for stmt in body {
        stmt.traverse(&mut |s| {
            match s {
                IrStatement::Let { id, var, .. } => {
                    // Ids come from a counter, so a duplicate means some pass
                    // has cloned a binder without refreshing its identity.
                    let prev = let_bindings.insert(var.id, *id);
                    assert!(prev.is_none(), "duplicate binding of variable `{var}`");
                }
                IrStatement::Match { id, match_ } => {
                    match match_ {
                        Match::Option {
                            some_arm_binding: some_arm,
                            ..
                        } => {
                            if let Some(binding) = some_arm {
                                let prev = option_bindings.insert(binding.id, *id);
                                assert!(
                                    prev.is_none(),
                                    "duplicate binding of option variable `{binding}`"
                                );
                            }
                        }
                        Match::Enum { arms, .. } => {
                            for arm in arms {
                                for (_, var_name) in &arm.bindings {
                                    let inserted = enum_bindings.insert(var_name.id);
                                    assert!(
                                        inserted,
                                        "duplicate binding of enum variable `{var_name}`"
                                    );
                                }
                            }
                        }
                        Match::Bool { .. } => {
                            // No bindings
                        }
                    }
                }
                IrStatement::For { id, var, .. } => {
                    if let Some(var) = var {
                        let prev = for_bindings.insert(var.id, *id);
                        assert!(prev.is_none(), "duplicate binding of loop variable `{var}`");
                    }
                }
                // These bind nothing.
                IrStatement::Write { .. }
                | IrStatement::WriteString { .. }
                | IrStatement::WriteFragment { .. }
                | IrStatement::ComponentInvocation { .. } => {}
            }

            // Collect variable references from all expressions
            s.traverse_exprs(&mut |e| {
                if let IrExpr::Var { value: var, .. } = e {
                    used_vars.insert(var.id);
                }
            });
        });
    }

    let unused_lets: HashSet<StatementId> = let_bindings
        .iter()
        .filter(|(var, _)| !used_vars.contains(*var))
        .map(|(_, id)| *id)
        .collect();

    let unused_option_bindings: HashSet<StatementId> = option_bindings
        .iter()
        .filter(|(var, _)| !used_vars.contains(*var))
        .map(|(_, id)| *id)
        .collect();

    let unused_enum_bindings: HashSet<VarId> = enum_bindings
        .iter()
        .filter(|var| !used_vars.contains(*var))
        .copied()
        .collect();

    let unused_for_bindings: HashSet<StatementId> = for_bindings
        .iter()
        .filter(|(var, _)| !used_vars.contains(*var))
        .map(|(_, id)| *id)
        .collect();

    UnusedVars {
        unused_lets,
        unused_option_bindings,
        unused_enum_bindings,
        unused_for_bindings,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ir_module::IrModule;
    use crate::ir::ir_module_builder::IrModuleBuilder;
    use expect_test::{Expect, expect};

    fn check(mut module: IrModule, expected: Expect) {
        let before = module.to_string();
        for view in &mut module.views {
            eliminate_unused_variable_declarations(&mut view.body);
        }
        for component in &mut module.components {
            eliminate_unused_variable_declarations(&mut component.body);
        }
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_discard_unused_for_loop_variable() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.for_loop("unused", t.array(vec![t.str("a"), t.str("b")]), |t| {
                        t.write("Hello");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  for v0 in ["a", "b"] {
                    write("Hello")
                  }
                }

                -- after --
                view Test() {
                  for _ in ["a", "b"] {
                    write("Hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_used_for_loop_variable() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.for_loop("item", t.array(vec![t.str("a"), t.str("b")]), |t| {
                        t.write_string(t.var("item"));
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  for v0 in ["a", "b"] {
                    write_string(v0)
                  }
                }

                -- after --
                view Test() {
                  for v0 in ["a", "b"] {
                    write_string(v0)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_unused_let_in_outermost_scope() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("unused", t.str("value"), |t| {
                        t.write("Hello");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "value" in {
                    write("Hello")
                  }
                }

                -- after --
                view Test() {
                  write("Hello")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_text_expression() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("message", t.str("Hello"), |t| {
                        t.write_string(t.var("message"));
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "Hello" in {
                    write_string(v0)
                  }
                }

                -- after --
                view Test() {
                  let v0 = "Hello" in {
                    write_string(v0)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_if_statement() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("cond", t.bool(true), |t| {
                        t.if_stmt(t.var("cond"), |t| {
                            t.write("Condition is true");
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    match v0 {
                      true => {
                        write("Condition is true")
                      }
                      false => {
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    match v0 {
                      true => {
                        write("Condition is true")
                      }
                      false => {
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statement_inside_if_body() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.if_stmt(t.bool(true), |t| {
                        t.let_stmt("unused", t.str("value"), |t| {
                            t.write("Inside if");
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match true {
                    true => {
                      let v0 = "value" in {
                        write("Inside if")
                      }
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Inside if")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statement_inside_for_loop_body() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    let items = t.array(vec![t.str("a"), t.str("b")]);
                    t.for_loop("item", items, |t| {
                        t.let_stmt("unused", t.str("value"), |t| {
                            t.write_string(t.var("item"));
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  for v0 in ["a", "b"] {
                    let v1 = "value" in {
                      write_string(v0)
                    }
                  }
                }

                -- after --
                view Test() {
                  for v0 in ["a", "b"] {
                    write_string(v0)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_binary_op() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.bool(true), |t| {
                        t.let_stmt("y", t.bool(false), |t| {
                            t.if_stmt(t.eq(t.var("x"), t.var("y")), |t| {
                                t.write("Equal");
                            });
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    let v1 = false in {
                      match (v0 == v1) {
                        true => {
                          write("Equal")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    let v1 = false in {
                      match (v0 == v1) {
                        true => {
                          write("Equal")
                        }
                        false => {
                        }
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("a", t.str("a_value"), |t| {
                        t.write("First");
                    });
                    t.let_stmt("b", t.str("b_value"), |t| {
                        t.write("Second");
                    });
                    t.write("Third");
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "a_value" in {
                    write("First")
                  }
                  let v1 = "b_value" in {
                    write("Second")
                  }
                  write("Third")
                }

                -- after --
                view Test() {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    let items = t.array(vec![t.str("a"), t.str("b")]);
                    t.let_stmt("items", items, |t| {
                        t.for_loop("item", t.var("items"), |t| {
                            t.write_string(t.var("item"));
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = ["a", "b"] in {
                    for v1 in v0 {
                      write_string(v1)
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = ["a", "b"] in {
                    for v1 in v0 {
                      write_string(v1)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_for_range_end() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("count", t.int(3), |t| {
                        t.for_range(Some("i"), t.int(1), t.var("count"), |t| {
                            t.write_string(t.int_to_string(t.var("i")));
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = 3 in {
                    for v1 in 1..=v0 {
                      write_string(v1.to_string())
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = 3 in {
                    for v1 in 1..=v0 {
                      write_string(v1.to_string())
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_for_range_start() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("start", t.int(1), |t| {
                        t.for_range(Some("i"), t.var("start"), t.int(5), |t| {
                            t.write_string(t.int_to_string(t.var("i")));
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = 1 in {
                    for v1 in v0..=5 {
                      write_string(v1.to_string())
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = 1 in {
                    for v1 in v0..=5 {
                      write_string(v1.to_string())
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_statement_when_variable_is_used_in_discarded_for_range() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("count", t.int(3), |t| {
                        t.for_range(None, t.int(1), t.var("count"), |t| {
                            t.write("x");
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = 3 in {
                    for _ in 1..=v0 {
                      write("x")
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = 3 in {
                    for _ in 1..=v0 {
                      write("x")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_let_statement_when_sibling_statement_variable_is_not_referenced() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.str("first x"), |t| {
                        t.write_string(t.var("x"));
                    });
                    t.let_stmt("x_1", t.str("second x"), |t| {
                        t.write("No reference to x_1 here");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "first x" in {
                    write_string(v0)
                  }
                  let v1 = "second x" in {
                    write("No reference to x_1 here")
                  }
                }

                -- after --
                view Test() {
                  let v0 = "first x" in {
                    write_string(v0)
                  }
                  write("No reference to x_1 here")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_nested_unused_let_statements() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("outer", t.str("outer_value"), |t| {
                        t.let_stmt("inner", t.str("inner_value"), |t| {
                            t.write("No variables used");
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "outer_value" in {
                    let v1 = "inner_value" in {
                      write("No variables used")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("No variables used")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_deeply_nested_unused_let_statements() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("level1", t.str("value1"), |t| {
                        t.let_stmt("level2", t.str("value2"), |t| {
                            t.let_stmt("level3", t.str("value3"), |t| {
                                t.let_stmt("level4", t.str("value4"), |t| {
                                    t.write("Deeply nested, no variables used");
                                });
                            });
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "value1" in {
                    let v1 = "value2" in {
                      let v2 = "value3" in {
                        let v3 = "value4" in {
                          write("Deeply nested, no variables used")
                        }
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  write("Deeply nested, no variables used")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_deeply_nested_cascading_unused_let_statements() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.str("str"), |t| {
                        t.let_stmt("y", t.var("x"), |t| {
                            t.let_stmt("z", t.var("y"), |t| {
                                t.write("Deeply nested, no variables used");
                            });
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "str" in {
                    let v1 = v0 in {
                      let v2 = v1 in {
                        write("Deeply nested, no variables used")
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  write("Deeply nested, no variables used")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_deeply_nested_cascading_unused_let_statements_but_keep_used() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.str("str"), |t| {
                        t.let_stmt("y", t.var("x"), |t| {
                            t.let_stmt("z", t.var("y"), |t| {
                                t.write_string(t.var("x"));
                            });
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = "str" in {
                    let v1 = v0 in {
                      let v2 = v1 in {
                        write_string(v0)
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = "str" in {
                    write_string(v0)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_used_in_bool_match_expr() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("flag", t.bool(true), |t| {
                        t.write_string(t.bool_match_expr(t.var("flag"), t.str("yes"), t.str("no")));
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    write_string(match v0 {true => "yes", false => "no"})
                  }
                }

                -- after --
                view Test() {
                  let v0 = true in {
                    write_string(match v0 {true => "yes", false => "no"})
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_let_used_as_enum_match_subject() {
        let module = IrModuleBuilder::new()
            .enum_(
                "BadgeElement",
                [("Span", vec![]), ("Link", vec![("href", "String")])],
            )
            .view_no_params("Test", |t| {
                t.let_stmt("element", t.enum_variant("BadgeElement", "Span"), |t| {
                    t.let_stmt("match_subject", t.var("element"), |t| {
                        t.enum_match_stmt(t.var("match_subject"), |m| {
                            m.arm("Span", |t| {
                                t.write("<span>badge</span>");
                            });
                            m.arm_bound("Link", [("href", "h")], |t| {
                                t.write("<a>badge</a>");
                            });
                        });
                    });
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                enum BadgeElement {
                  Span,
                  Link {href: String},
                }
                view Test() {
                  let v0 = BadgeElement::Span in {
                    let v1 = v0 in {
                      match v1 {
                        BadgeElement::Span => {
                          write("<span>badge</span>")
                        }
                        BadgeElement::Link(href: v2) => {
                          write("<a>badge</a>")
                        }
                      }
                    }
                  }
                }

                -- after --
                enum BadgeElement {
                  Span,
                  Link {href: String},
                }
                view Test() {
                  let v0 = BadgeElement::Span in {
                    let v1 = v0 in {
                      match v1 {
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
            .enum_("MyEnum", [("Foo", vec![("value", "String")])])
            .view_no_params("Test", |t| {
                // let x = "hello"
                // let foo = MyEnum::Foo(value: x)
                // match foo { Foo(v) => write(v) }  -- uses foo, which uses x
                t.let_stmt("x", t.str("hello"), |t| {
                    t.let_stmt(
                        "foo",
                        t.enum_variant_with_fields("MyEnum", "Foo", vec![("value", t.var("x"))]),
                        |t| {
                            t.enum_match_stmt(t.var("foo"), |m| {
                                m.arm_bound("Foo", [("value", "v")], |t| {
                                    t.write_string(t.var("v"));
                                });
                            });
                        },
                    );
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                enum MyEnum {
                  Foo {value: String},
                }
                view Test() {
                  let v0 = "hello" in {
                    let v1 = MyEnum::Foo {value: v0} in {
                      match v1 {
                        MyEnum::Foo(value: v2) => {
                          write_string(v2)
                        }
                      }
                    }
                  }
                }

                -- after --
                enum MyEnum {
                  Foo {value: String},
                }
                view Test() {
                  let v0 = "hello" in {
                    let v1 = MyEnum::Foo {value: v0} in {
                      match v1 {
                        MyEnum::Foo(value: v2) => {
                          write_string(v2)
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
            .enum_(
                "BadgeElement",
                [("Span", vec![]), ("Link", vec![("href", "String")])],
            )
            .view_no_params("Test", |t| {
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
                                t.enum_match_stmt(t.var("match_subject"), |m| {
                                    m.arm("Span", |t| {
                                        t.write("<span>badge</span>");
                                    });
                                    m.arm_bound("Link", [("href", "h")], |t| {
                                        t.write_string(t.var("h"));
                                    });
                                });
                            });
                        },
                    );
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                enum BadgeElement {
                  Span,
                  Link {href: String},
                }
                view Test() {
                  let v0 = "/home" in {
                    let v1 = BadgeElement::Link {href: v0} in {
                      let v2 = v1 in {
                        match v2 {
                          BadgeElement::Span => {
                            write("<span>badge</span>")
                          }
                          BadgeElement::Link(href: v3) => {
                            write_string(v3)
                          }
                        }
                      }
                    }
                  }
                }

                -- after --
                enum BadgeElement {
                  Span,
                  Link {href: String},
                }
                view Test() {
                  let v0 = "/home" in {
                    let v1 = BadgeElement::Link {href: v0} in {
                      let v2 = v1 in {
                        match v2 {
                          BadgeElement::Span => {
                            write("<span>badge</span>")
                          }
                          BadgeElement::Link(href: v3) => {
                            write_string(v3)
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
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
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
                      Some(v1) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("outer_opt", t.some(t.some(t.str("deep"))), |t| {
                        t.option_match_stmt(
                            t.var("outer_opt"),
                            Some("inner"),
                            |t| {
                                t.option_match_stmt(
                                    t.var("inner"),
                                    Some("value"),
                                    |t| {
                                        t.write_string(t.var("value"));
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match v0 {
                      Some(v1) => {
                        match v1 {
                          Some(v2) => {
                            write_string(v2)
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
                view Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match v0 {
                      Some(v1) => {
                        match v1 {
                          Some(v2) => {
                            write_string(v2)
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
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
                view Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = true in {
                    match v0 {
                      true => {
                        let v1 = "not used" in {
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
                view Test() {
                  let v0 = true in {
                    match v0 {
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

    #[test]
    fn should_preserve_used_binding_when_sibling_arm_has_unused_binding_with_same_field_name() {
        // Two variants share the field name `f0`. The binding in arm A is
        // used, the binding in arm B is unused. Only the unused one should
        // be removed, the used one must survive.
        let module = IrModuleBuilder::new()
            .enum_(
                "E",
                [("A", vec![("f0", "String")]), ("B", vec![("f0", "String")])],
            )
            .view_no_params("Test", |t| {
                let e = t.enum_variant_with_fields("E", "A", vec![("f0", t.str("hi"))]);
                t.let_stmt("e", e, |t| {
                    t.enum_match_stmt(t.var("e"), |m| {
                        m.arm_bound("A", [("f0", "used")], |t| {
                            t.write_string(t.var("used"));
                        });
                        m.arm_bound("B", [("f0", "unused")], |t| {
                            t.write("no reference to unused here");
                        });
                    });
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                enum E {
                  A {f0: String},
                  B {f0: String},
                }
                view Test() {
                  let v0 = E::A {f0: "hi"} in {
                    match v0 {
                      E::A(f0: v1) => {
                        write_string(v1)
                      }
                      E::B(f0: v2) => {
                        write("no reference to unused here")
                      }
                    }
                  }
                }

                -- after --
                enum E {
                  A {f0: String},
                  B {f0: String},
                }
                view Test() {
                  let v0 = E::A {f0: "hi"} in {
                    match v0 {
                      E::A(f0: v1) => {
                        write_string(v1)
                      }
                      E::B => {
                        write("no reference to unused here")
                      }
                    }
                  }
                }
            "#]],
        );
    }
}
