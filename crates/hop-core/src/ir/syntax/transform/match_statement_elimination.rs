use crate::expr::patterns::Match;
use crate::ir::{
    IrExpr,
    ast::{IrStatement, traverse_statements_mut},
};

pub struct MatchStatementEliminationPass;

impl MatchStatementEliminationPass {
    pub fn run(body: &mut Vec<IrStatement>) {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Type;
    use crate::ir::ast::IrViewDeclaration;
    use crate::ir::syntax::builder::{build_ir, build_ir_no_params};
    use expect_test::{Expect, expect};
    use std::sync::Arc;

    fn check(mut view: IrViewDeclaration, expected: Expect) {
        let before = view.to_string();
        MatchStatementEliminationPass::run(&mut view.body);
        let after = view.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_eliminate_bool_match_that_is_always_true() {
        check(
            build_ir_no_params("Test", |t| {
                t.bool_match_stmt(
                    t.bool(true),
                    |t| {
                        t.write("true branch");
                    },
                    |t| {
                        t.write("false branch");
                    },
                );
            }),
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
            build_ir_no_params("Test", |t| {
                t.bool_match_stmt(
                    t.bool(false),
                    |t| {
                        t.write("true branch");
                    },
                    |t| {
                        t.write("false branch");
                    },
                );
            }),
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
    fn should_preserve_bool_match_with_dynamic_subject() {
        check(
            build_ir("Test", [("show", Type::Bool)], |t| {
                t.bool_match_stmt(
                    t.var("show"),
                    |t| {
                        t.write("dynamic true");
                    },
                    |t| {
                        t.write("dynamic false");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test(show: Bool) {
                  match show {
                    true => {
                      write("dynamic true")
                    }
                    false => {
                      write("dynamic false")
                    }
                  }
                }

                -- after --
                view Test(show: Bool) {
                  match show {
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
            build_ir_no_params("Test", |t| {
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
            }),
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
            build_ir_no_params("Test", |t| {
                t.option_match_stmt(
                    t.none(Type::String),
                    Some("x"),
                    |t| {
                        t.write("some branch");
                    },
                    |t| {
                        t.write("none branch");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::None {
                    Some(x) => {
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
            build_ir(
                "Test",
                [("maybe", Type::Option(Arc::new(Type::String)))],
                |t| {
                    t.option_match_stmt(
                        t.var("maybe"),
                        Some("x"),
                        |t| {
                            t.write_expr(t.var("x"), false);
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                },
            ),
            expect![[r#"
                -- before --
                view Test(maybe: Option[String]) {
                  match maybe {
                    Some(x) => {
                      write_expr(x)
                    }
                    None => {
                      write("none")
                    }
                  }
                }

                -- after --
                view Test(maybe: Option[String]) {
                  match maybe {
                    Some(x) => {
                      write_expr(x)
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
            build_ir_no_params("Test", |t| {
                t.option_match_stmt(
                    t.some(t.str("hi")),
                    Some("x"),
                    |t| {
                        t.write_expr(t.var("x"), false);
                    },
                    |t| {
                        t.write("none branch");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("hi") {
                    Some(x) => {
                      write_expr(x)
                    }
                    None => {
                      write("none branch")
                    }
                  }
                }

                -- after --
                view Test() {
                  let x = "hi" in {
                    write_expr(x)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_eliminate_option_match_on_some_without_binding() {
        check(
            build_ir_no_params("Test", |t| {
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
            }),
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
            build_ir_no_params("Test", |t| {
                t.option_match_stmt(
                    t.some(t.str("outer")),
                    None,
                    |t| {
                        t.write("outer some");
                        t.option_match_stmt(
                            t.none(Type::String),
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
            }),
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
