use crate::expr::patterns::Match;
use crate::ir::{
    IrExpr,
    ir_module::{IrStatement, traverse_statements_mut},
};

pub fn eliminate_match_statements(body: &mut Vec<IrStatement>) {
    traverse_statements_mut(body, &mut |stmts| {
        let mut transformed = Vec::new();
        for stmt in std::mem::take(stmts) {
            match stmt {
                IrStatement::Match {
                    id,
                    match_:
                        Match::Bool {
                            subject,
                            true_body,
                            false_body,
                        },
                } => match *subject {
                    IrExpr::BooleanLiteral { value: true, .. } => {
                        transformed.extend(*true_body);
                    }
                    IrExpr::BooleanLiteral { value: false, .. } => {
                        transformed.extend(*false_body);
                    }
                    other_subject => {
                        transformed.push(IrStatement::Match {
                            id,
                            match_: Match::Bool {
                                subject: Box::new(other_subject),
                                true_body,
                                false_body,
                            },
                        });
                    }
                },
                IrStatement::Match {
                    id,
                    match_:
                        Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body,
                            none_arm_body,
                        },
                } => match *subject {
                    IrExpr::OptionLiteral { value: None, .. } => {
                        transformed.extend(*none_arm_body);
                    }
                    IrExpr::OptionLiteral {
                        value: Some(inner), ..
                    } => match some_arm_binding {
                        Some(var) => {
                            transformed.push(IrStatement::Let {
                                id,
                                var,
                                value: *inner,
                                body: *some_arm_body,
                            });
                        }
                        None => {
                            transformed.extend(*some_arm_body);
                        }
                    },
                    other_subject => {
                        transformed.push(IrStatement::Match {
                            id,
                            match_: Match::Option {
                                subject: Box::new(other_subject),
                                some_arm_binding,
                                some_arm_body,
                                none_arm_body,
                            },
                        });
                    }
                },
                other => transformed.push(other),
            }
        }
        *stmts = transformed;
    });
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
            eliminate_match_statements(&mut view.body);
        }
        for component in &mut module.components {
            eliminate_match_statements(&mut component.body);
        }
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_eliminate_bool_match_that_is_always_true() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.bool_match_stmt(
                        t.bool(true),
                        |t| {
                            t.write("true branch");
                        },
                        |t| {
                            t.write("false branch");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match true {
                    true => {
                      write("true branch")
                    }
                    false => {
                      write("false branch")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("true branch")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_bool_match_that_is_always_false() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.bool_match_stmt(
                        t.bool(false),
                        |t| {
                            t.write("true branch");
                        },
                        |t| {
                            t.write("false branch");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match false {
                    true => {
                      write("true branch")
                    }
                    false => {
                      write("false branch")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("false branch")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_if_sugar_with_constant_subject() {
        check(
            IrModuleBuilder::new()
                .view("Test", [("show", "Bool")], |t| {
                    t.if_stmt(t.var("show"), |t| {
                        t.write("Dynamic");
                    });
                    t.if_stmt(t.bool(true), |t| {
                        t.write("Static true");
                    });
                    t.if_stmt(t.bool(false), |t| {
                        t.write("Static false");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      write("Dynamic")
                    }
                    false => {
                    }
                  }
                  match true {
                    true => {
                      write("Static true")
                    }
                    false => {
                    }
                  }
                  match false {
                    true => {
                      write("Static false")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                view Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      write("Dynamic")
                    }
                    false => {
                    }
                  }
                  write("Static true")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_bool_match_with_dynamic_subject() {
        check(
            IrModuleBuilder::new()
                .view("Test", [("show", "Bool")], |t| {
                    t.bool_match_stmt(
                        t.var("show"),
                        |t| {
                            t.write("dynamic true");
                        },
                        |t| {
                            t.write("dynamic false");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      write("dynamic true")
                    }
                    false => {
                      write("dynamic false")
                    }
                  }
                }

                -- after --
                view Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      write("dynamic true")
                    }
                    false => {
                      write("dynamic false")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_handle_nested_bool_match_elimination() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.bool_match_stmt(
                        t.bool(true),
                        |t| {
                            t.write("outer true");
                            t.bool_match_stmt(
                                t.bool(false),
                                |t| {
                                    t.write("inner true - gone");
                                },
                                |t| {
                                    t.write("inner false - kept");
                                },
                            );
                        },
                        |t| {
                            t.write("outer false - gone");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match true {
                    true => {
                      write("outer true")
                      match false {
                        true => {
                          write("inner true - gone")
                        }
                        false => {
                          write("inner false - kept")
                        }
                      }
                    }
                    false => {
                      write("outer false - gone")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("outer true")
                  write("inner false - kept")
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_option_match_on_none() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.option_match_stmt(
                        t.none("String"),
                        Some("x"),
                        |t| {
                            t.write("some branch");
                        },
                        |t| {
                            t.write("none branch");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::None {
                    Some(v0) => {
                      write("some branch")
                    }
                    None => {
                      write("none branch")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("none branch")
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_option_match_with_dynamic_subject() {
        check(
            IrModuleBuilder::new()
                .view("Test", [("maybe", "Option[String]")], |t| {
                    t.option_match_stmt(
                        t.var("maybe"),
                        Some("x"),
                        |t| {
                            t.write_string(t.var("x"));
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test(maybe@v0: Option[String]) {
                  match v0 {
                    Some(v1) => {
                      write_string(v1)
                    }
                    None => {
                      write("none")
                    }
                  }
                }

                -- after --
                view Test(maybe@v0: Option[String]) {
                  match v0 {
                    Some(v1) => {
                      write_string(v1)
                    }
                    None => {
                      write("none")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_option_match_on_some_with_binding() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.option_match_stmt(
                        t.some(t.str("hi")),
                        Some("x"),
                        |t| {
                            t.write_string(t.var("x"));
                        },
                        |t| {
                            t.write("none branch");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("hi") {
                    Some(v0) => {
                      write_string(v0)
                    }
                    None => {
                      write("none branch")
                    }
                  }
                }

                -- after --
                view Test() {
                  let v0 = "hi" in {
                    write_string(v0)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_option_match_on_some_without_binding() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.option_match_stmt(
                        t.some(t.str("hi")),
                        None,
                        |t| {
                            t.write("some branch");
                        },
                        |t| {
                            t.write("none branch");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("hi") {
                    Some(_) => {
                      write("some branch")
                    }
                    None => {
                      write("none branch")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("some branch")
                }
            "#]],
        );
    }

    #[test]
    fn should_handle_nested_option_match_elimination() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.option_match_stmt(
                        t.some(t.str("outer")),
                        None,
                        |t| {
                            t.write("outer some");
                            t.option_match_stmt(
                                t.none("String"),
                                None,
                                |t| {
                                    t.write("inner some - gone");
                                },
                                |t| {
                                    t.write("inner none - kept");
                                },
                            );
                        },
                        |t| {
                            t.write("outer none - gone");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("outer") {
                    Some(_) => {
                      write("outer some")
                      match Option[String]::None {
                        Some(_) => {
                          write("inner some - gone")
                        }
                        None => {
                          write("inner none - kept")
                        }
                      }
                    }
                    None => {
                      write("outer none - gone")
                    }
                  }
                }

                -- after --
                view Test() {
                  write("outer some")
                  write("inner none - kept")
                }
            "#]],
        );
    }
}
