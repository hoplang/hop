use crate::html::write_escaped_html;
use crate::ir::IrExpr;
use crate::ir::ast::{IrStatement, traverse_statements_mut};

/// A pass that simplifies WriteExpr statements with constant string expressions into a Write
/// statement
pub fn simplify_write_exprs(body: &mut Vec<IrStatement>) {
    traverse_statements_mut(body, &mut |stmts| {
        for statement in stmts.iter_mut() {
            if let IrStatement::WriteExpr {
                id,
                expr: IrExpr::StringLiteral { value: s, .. },
                escape,
            } = statement
            {
                let content = if *escape {
                    let mut buf = String::new();
                    write_escaped_html(s, &mut buf);
                    buf
                } else {
                    s.to_string()
                };
                *statement = IrStatement::Write { id: *id, content };
            }
        }
    });
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
            simplify_write_exprs(&mut view.body);
        }
        for component in &mut module.components {
            simplify_write_exprs(&mut component.body);
        }
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simplify_constant_string() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    // WriteExpr with constant string should become Write
                    t.write_expr(t.str("Hello, World!"), false);
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  write_expr("Hello, World!")
                }

                -- after --
                view Test() {
                  write("Hello, World!")
                }
            "#]],
        );
    }

    #[test]
    fn simplify_with_escaping() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    // WriteExpr with escaping enabled
                    t.write_expr(t.str("<div>Hello & Goodbye</div>"), true);
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  write_escaped("<div>Hello & Goodbye</div>")
                }

                -- after --
                view Test() {
                  write("&lt;div&gt;Hello &amp; Goodbye&lt;/div&gt;")
                }
            "#]],
        );
    }

    #[test]
    fn nested_transformations() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.if_stmt(t.bool(true), |t| {
                        t.write_expr(t.str("Inside if"), false);
                        t.for_loop("item", t.array(vec![t.str("foo")]), |t| {
                            t.write_expr(t.str("Inside for"), false);
                        });
                    });
                    t.let_stmt("x", t.str("value"), |t| {
                        t.write_expr(t.str("Inside let"), false);
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  if true {
                    write_expr("Inside if")
                    for item in ["foo"] {
                      write_expr("Inside for")
                    }
                  }
                  let x = "value" in {
                    write_expr("Inside let")
                  }
                }

                -- after --
                view Test() {
                  if true {
                    write("Inside if")
                    for item in ["foo"] {
                      write("Inside for")
                    }
                  }
                  let x = "value" in {
                    write("Inside let")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn mixed_write_and_write_expr() {
        check(
            IrModuleBuilder::new()
                .view("Test", [("x", "String")], |t| {
                    t.write("Already a Write statement");
                    t.write_expr(t.str("Will become Write"), false);
                    t.write_expr(t.var("x"), false);
                })
                .build(),
            expect![[r#"
                -- before --
                view Test(x: String) {
                  write("Already a Write statement")
                  write_expr("Will become Write")
                  write_expr(x)
                }

                -- after --
                view Test(x: String) {
                  write("Already a Write statement")
                  write("Will become Write")
                  write_expr(x)
                }
            "#]],
        );
    }
}
