use super::Pass;
use crate::dop::patterns::Match;
use crate::ir::{
    IrExpr,
    ast::{IrComponentDeclaration, IrStatement, StatementId},
};
use std::collections::HashSet;

/// A pass that eliminates unused let statements
/// If a variable is never referenced in the body of the let, the let statement is replaced with its body
pub struct UnusedLetEliminationPass;

impl UnusedLetEliminationPass {
    /// Collect which let statements have unused variables
    fn collect_unused_lets(entrypoint: &IrComponentDeclaration) -> HashSet<StatementId> {
        let mut all_lets = HashSet::new();
        let mut used_lets = HashSet::new();

        // First collect all let statement IDs
        for stmt in &entrypoint.body {
            stmt.traverse(&mut |s| {
                if let IrStatement::Let { id, .. } = s {
                    all_lets.insert(*id);
                }
            });
        }

        // Now traverse with scope tracking to find used lets
        for stmt in &entrypoint.body {
            stmt.traverse_with_scope(&mut |s, scope| {
                // Check for match statement subjects (which are variable references)
                if let IrStatement::Match { match_, .. } = s {
                    let subject_name = match match_ {
                        Match::Bool { subject, .. } => &subject.0,
                        Match::Enum { subject, .. } => &subject.0,
                        Match::Option { subject, .. } => &subject.0,
                    };
                    if let Some(IrStatement::Let { id, .. }) = scope.get(&subject_name.to_string())
                    {
                        used_lets.insert(*id);
                    }
                }

                // Process only the primary expression of this statement (not nested ones)
                // This avoids O(n²) behavior from traverse_exprs
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| {
                        // Check for direct variable references
                        if let IrExpr::Var { value: name, .. } = expr {
                            if let Some(IrStatement::Let { id, .. }) = scope.get(&name.to_string())
                            {
                                used_lets.insert(*id);
                            }
                        }
                        // Check for match expression subjects (which are variable references)
                        if let IrExpr::Match { match_, .. } = expr {
                            let subject_name = match match_ {
                                Match::Bool { subject, .. } => &subject.0,
                                Match::Enum { subject, .. } => &subject.0,
                                Match::Option { subject, .. } => &subject.0,
                            };
                            if let Some(IrStatement::Let { id, .. }) =
                                scope.get(&subject_name.to_string())
                            {
                                used_lets.insert(*id);
                            }
                        }
                    });
                }
            });
        }

        // Return the set of unused lets (all - used)
        all_lets.difference(&used_lets).cloned().collect()
    }

    /// Transform a list of statements, eliminating unused lets
    fn transform_statements(
        statements: Vec<IrStatement>,
        unused_lets: &HashSet<StatementId>,
    ) -> Vec<IrStatement> {
        statements
            .into_iter()
            .flat_map(|stmt| match stmt {
                // Unused let - replace with its body, recursively transforming it
                IrStatement::Let { body, id, .. } if unused_lets.contains(&id) => {
                    Self::transform_statements(body, unused_lets)
                }

                // All other statements pass through
                other => vec![other],
            })
            .collect()
    }
}

impl Pass for UnusedLetEliminationPass {
    fn run(mut entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        // First collect which let statements have unused variables
        let unused_lets = Self::collect_unused_lets(&entrypoint);

        // Use visit_mut to recursively process nested bodies
        for stmt in &mut entrypoint.body {
            stmt.traverse_mut(&mut |s| match s {
                IrStatement::If {
                    body, else_body, ..
                } => {
                    *body = Self::transform_statements(std::mem::take(body), &unused_lets);
                    *else_body = else_body
                        .take()
                        .map(|else_body| Self::transform_statements(else_body, &unused_lets))
                }
                IrStatement::For { body, .. } | IrStatement::Let { body, .. } => {
                    *body = Self::transform_statements(std::mem::take(body), &unused_lets);
                }
                _ => {}
            });
        }

        // Then transform top-level statements
        entrypoint.body = Self::transform_statements(entrypoint.body, &unused_lets);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dop::Type,
        ir::syntax::builder::{IrBuilder, IrModuleBuilder, build_ir},
    };
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrComponentDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        let result = UnusedLetEliminationPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_eliminate_unused_let_in_outermost_scope() {
        check(
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
    fn should_eliminate_let_statement_when_sibling_statement_uses_same_variable() {
        check(
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            build_ir("Test", [], |t| {
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
            .component("Test", [], |t| {
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

        let entrypoint = module.components.into_iter().next().unwrap();
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
                        BadgeElement::Link(href: h) => {
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
            .component("Test", [], |t| {
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

        let entrypoint = module.components.into_iter().next().unwrap();
        check(
            entrypoint,
            expect![[r#"
                -- before --
                Test() {
                  let x = "hello" in {
                    let foo = MyEnum::Foo(value: x) in {
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
                    let foo = MyEnum::Foo(value: x) in {
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
            .component("Test", [], |t| {
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

        let entrypoint = module.components.into_iter().next().unwrap();
        check(
            entrypoint,
            expect![[r#"
                -- before --
                Test() {
                  let href = "/home" in {
                    let element = BadgeElement::Link(href: href) in {
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
                    let element = BadgeElement::Link(href: href) in {
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
}
