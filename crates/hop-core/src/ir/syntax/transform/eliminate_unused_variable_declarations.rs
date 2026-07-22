use crate::expr::patterns::Match;
use crate::ir::{
    IrExpr,
    ast::{IrStatement, StatementId, traverse_statements_mut},
};
use crate::symbols::field_name::FieldName;
use crate::symbols::var_name::VarName;
use std::collections::{HashMap, HashSet};

/// Collected information about unused variables
struct UnusedVars {
    /// Let statements with unused variables
    unused_lets: HashSet<StatementId>,
    /// Match statements with unused Option bindings
    unused_option_bindings: HashSet<StatementId>,
    /// Enum match arm bindings that are unused, identified by variable name
    /// (variable names are globally unique after VariableRenamingPass, so this
    /// distinguishes bindings in different arms even when they share a field
    /// name)
    unused_enum_bindings: HashSet<VarName>,
    /// Record destructures with unused field bindings
    unused_record_bindings: HashSet<(StatementId, FieldName)>,
}

/// A pass that eliminates unused variable declarations.
/// - Unused let statements are replaced with their body
/// - Unused Option match bindings are set to `_` (wildcard)
/// - Unused Enum match bindings are removed from the bindings list
/// - Unused record destructure bindings are removed; an empty destructure is
///   replaced with its body
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
                                        !unused_vars.unused_enum_bindings.contains(var_name)
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
                    IrStatement::LetRecordDestructure {
                        id,
                        subject,
                        mut bindings,
                        body,
                    } => {
                        let before = bindings.len();
                        bindings.retain(|(field_name, _)| {
                            !unused_vars
                                .unused_record_bindings
                                .contains(&(id, field_name.clone()))
                        });
                        if bindings.len() != before {
                            changed = true;
                        }
                        if bindings.is_empty() {
                            // An empty destructure is dropped, keeping its body.
                            changed = true;
                            transformed.extend(body);
                        } else {
                            transformed.push(IrStatement::LetRecordDestructure {
                                id,
                                subject,
                                bindings,
                                body,
                            });
                        }
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
/// Variables are globally unique after VariableRenamingPass, so we only
/// need name-based tracking (no scope bookkeeping).
fn collect_unused_vars(body: &[IrStatement]) -> UnusedVars {
    // All variable names that are referenced anywhere
    let mut used_var_names: HashSet<VarName> = HashSet::new();

    let mut let_bindings: HashMap<VarName, StatementId> = HashMap::new();
    let mut option_bindings: HashMap<VarName, StatementId> = HashMap::new();
    let mut enum_bindings: HashSet<VarName> = HashSet::new();
    let mut record_bindings: HashMap<VarName, (StatementId, FieldName)> = HashMap::new();

    for stmt in body {
        stmt.traverse(&mut |s| {
            match s {
                IrStatement::Let { id, var, .. } => {
                    // Variable names are globally unique after VariableRenamingPass.
                    // If this panics, the renaming pass may not have run before this pass.
                    let prev = let_bindings.insert(var.clone(), *id);
                    assert!(
                        prev.is_none(),
                        "duplicate variable name `{var}` in let_vars"
                    );
                }
                IrStatement::LetRecordDestructure { id, bindings, .. } => {
                    for (field_name, var_name) in bindings {
                        let prev =
                            record_bindings.insert(var_name.clone(), (*id, field_name.clone()));
                        assert!(prev.is_none(), "duplicate record binding name `{var_name}`");
                    }
                }
                IrStatement::Match { id, match_ } => {
                    match match_ {
                        Match::Option {
                            some_arm_binding: some_arm,
                            ..
                        } => {
                            if let Some(binding) = some_arm {
                                let prev = option_bindings.insert(binding.clone(), *id);
                                assert!(
                                    prev.is_none(),
                                    "duplicate option binding name `{binding}`"
                                );
                            }
                        }
                        Match::Enum { arms, .. } => {
                            for arm in arms {
                                for (_, var_name) in &arm.bindings {
                                    let inserted = enum_bindings.insert(var_name.clone());
                                    assert!(inserted, "duplicate enum binding name `{var_name}`");
                                }
                            }
                        }
                        Match::Bool { .. } => {
                            // No bindings
                        }
                    }
                }
                _ => {}
            }

            // Collect variable references from all expressions
            s.traverse_exprs(&mut |e| {
                if let IrExpr::Var { value: name, .. } = e {
                    used_var_names.insert(name.clone());
                }
            });
        });
    }

    let unused_lets: HashSet<StatementId> = let_bindings
        .iter()
        .filter(|(name, _)| !used_var_names.contains(*name))
        .map(|(_, id)| *id)
        .collect();

    let unused_option_bindings: HashSet<StatementId> = option_bindings
        .iter()
        .filter(|(binding_name, _)| !used_var_names.contains(*binding_name))
        .map(|(_, id)| *id)
        .collect();

    let unused_enum_bindings: HashSet<VarName> = enum_bindings
        .iter()
        .filter(|binding_name| !used_var_names.contains(*binding_name))
        .cloned()
        .collect();

    let unused_record_bindings: HashSet<(StatementId, FieldName)> = record_bindings
        .iter()
        .filter(|(binding_name, _)| !used_var_names.contains(*binding_name))
        .map(|(_, (id, field_name))| (*id, field_name.clone()))
        .collect();

    UnusedVars {
        unused_lets,
        unused_option_bindings,
        unused_enum_bindings,
        unused_record_bindings,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::IrModule;
    use crate::ir::syntax::builder::IrModuleBuilder;
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
                  let unused = "value" in {
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
                        t.write_expr(t.var("message"), false);
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let message = "Hello" in {
                    write_expr(message)
                  }
                }

                -- after --
                view Test() {
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
                  let cond = true in {
                    if cond {
                      write("Condition is true")
                    }
                  }
                }

                -- after --
                view Test() {
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
                  if true {
                    let unused = "value" in {
                      write("Inside if")
                    }
                  }
                }

                -- after --
                view Test() {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    let items = t.array(vec![t.str("a"), t.str("b")]);
                    t.for_loop("item", items, |t| {
                        t.let_stmt("unused", t.str("value"), |t| {
                            t.write_expr(t.var("item"), false);
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  for item in ["a", "b"] {
                    let unused = "value" in {
                      write_expr(item)
                    }
                  }
                }

                -- after --
                view Test() {
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
                  let x = true in {
                    let y = false in {
                      if (x == y) {
                        write("Equal")
                      }
                    }
                  }
                }

                -- after --
                view Test() {
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
                  let a = "a_value" in {
                    write("First")
                  }
                  let b = "b_value" in {
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
                            t.write_expr(t.var("item"), false);
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let items = ["a", "b"] in {
                    for item in items {
                      write_expr(item)
                    }
                  }
                }

                -- after --
                view Test() {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("count", t.int(3), |t| {
                        t.for_range("i", t.int(1), t.var("count"), |t| {
                            t.write_expr(t.int_to_string(t.var("i")), false);
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let count = 3 in {
                    for i in 1..=count {
                      write_expr(i.to_string())
                    }
                  }
                }

                -- after --
                view Test() {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("start", t.int(1), |t| {
                        t.for_range("i", t.var("start"), t.int(5), |t| {
                            t.write_expr(t.int_to_string(t.var("i")), false);
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let start = 1 in {
                    for i in start..=5 {
                      write_expr(i.to_string())
                    }
                  }
                }

                -- after --
                view Test() {
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
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("count", t.int(3), |t| {
                        t.for_range_discarded(t.int(1), t.var("count"), |t| {
                            t.write("x");
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let count = 3 in {
                    for _ in 1..=count {
                      write("x")
                    }
                  }
                }

                -- after --
                view Test() {
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
    fn should_eliminate_let_statement_when_sibling_statement_variable_is_not_referenced() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_stmt("x", t.str("first x"), |t| {
                        t.write_expr(t.var("x"), false);
                    });
                    t.let_stmt("x_1", t.str("second x"), |t| {
                        t.write("No reference to x_1 here");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let x = "first x" in {
                    write_expr(x)
                  }
                  let x_1 = "second x" in {
                    write("No reference to x_1 here")
                  }
                }

                -- after --
                view Test() {
                  let x = "first x" in {
                    write_expr(x)
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
                  let outer = "outer_value" in {
                    let inner = "inner_value" in {
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
                  let x = "str" in {
                    let y = x in {
                      let z = y in {
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
                                t.write_expr(t.var("x"), false);
                            });
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let x = "str" in {
                    let y = x in {
                      let z = y in {
                        write_expr(x)
                      }
                    }
                  }
                }

                -- after --
                view Test() {
                  let x = "str" in {
                    write_expr(x)
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
                        t.write_expr(
                            t.bool_match_expr(t.var("flag"), t.str("yes"), t.str("no")),
                            true,
                        );
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  let flag = true in {
                    write_escaped(match flag {true => "yes", false => "no"})
                  }
                }

                -- after --
                view Test() {
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
                enum BadgeElement {
                  Span,
                  Link {href: String},
                }
                view Test() {
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
                                    t.write_expr(t.var("v"), true);
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
                enum MyEnum {
                  Foo {value: String},
                }
                view Test() {
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
                                        t.write_expr(t.var("h"), true);
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
                enum BadgeElement {
                  Span,
                  Link {href: String},
                }
                view Test() {
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
                view Test() {
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
                view Test() {
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
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
                view Test() {
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
                view Test() {
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
                view Test() {
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

    #[test]
    fn should_eliminate_unused_record_destructure_binding() {
        let module = IrModuleBuilder::new()
            .record("Point", [("x", "String"), ("y", "String")])
            .view_no_params("Test", |t| {
                let point = t.record("Point", vec![("x", t.str("hi")), ("y", t.str("bye"))]);
                t.record_destructure_stmt(point, vec![("x", "a"), ("y", "b")], |t| {
                    t.write_expr(t.var("a"), false);
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  let {x: a, y: b} = Point {x: "hi", y: "bye"} in {
                    write_expr(a)
                  }
                }

                -- after --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  let {x: a} = Point {x: "hi", y: "bye"} in {
                    write_expr(a)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_empty_record_destructure() {
        let module = IrModuleBuilder::new()
            .record("Point", [("x", "String"), ("y", "String")])
            .view_no_params("Test", |t| {
                let point = t.record("Point", vec![("x", t.str("hi")), ("y", t.str("bye"))]);
                t.record_destructure_stmt(point, vec![("x", "a")], |t| {
                    t.write("no bindings used");
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  let {x: a} = Point {x: "hi", y: "bye"} in {
                    write("no bindings used")
                  }
                }

                -- after --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  write("no bindings used")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_used_record_destructure_binding() {
        let module = IrModuleBuilder::new()
            .record("Point", [("x", "String"), ("y", "String")])
            .view_no_params("Test", |t| {
                let point = t.record("Point", vec![("x", t.str("hi")), ("y", t.str("bye"))]);
                t.record_destructure_stmt(point, vec![("x", "a")], |t| {
                    t.write_expr(t.var("a"), false);
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  let {x: a} = Point {x: "hi", y: "bye"} in {
                    write_expr(a)
                  }
                }

                -- after --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  let {x: a} = Point {x: "hi", y: "bye"} in {
                    write_expr(a)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_cascading_unused_record_destructure_binding() {
        let module = IrModuleBuilder::new()
            .record("Point", [("x", "String"), ("y", "String")])
            .view_no_params("Test", |t| {
                let point = t.record("Point", vec![("x", t.str("hi")), ("y", t.str("bye"))]);
                t.record_destructure_stmt(point, vec![("x", "v")], |t| {
                    t.let_stmt("a", t.var("v"), |t| {
                        t.write("constant");
                    });
                });
            })
            .build();

        check(
            module,
            expect![[r#"
                -- before --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  let {x: v} = Point {x: "hi", y: "bye"} in {
                    let a = v in {
                      write("constant")
                    }
                  }
                }

                -- after --
                record Point {
                  x: String,
                  y: String,
                }
                view Test() {
                  write("constant")
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
                            t.write_expr(t.var("used"), false);
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
                  let e = E::A {f0: "hi"} in {
                    match e {
                      E::A(f0: used) => {
                        write_expr(used)
                      }
                      E::B(f0: unused) => {
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
                  let e = E::A {f0: "hi"} in {
                    match e {
                      E::A(f0: used) => {
                        write_expr(used)
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
