use crate::html::write_escaped_html;
use crate::ir::IrExpr;
use crate::ir::ir_module::{IrStatement, StatementIdCounter, traverse_statements_mut};

/// A pass that simplifies WriteExpr statements with constant string expressions into a Write
/// statement
pub fn simplify_write_exprs(body: &mut Vec<IrStatement>, stmt_ids: &mut StatementIdCounter) {
    traverse_statements_mut(body, &mut |stmts| {
        let mut transformed = Vec::new();
        for stmt in std::mem::take(stmts) {
            match stmt {
                // TODO:
                // * Handle nested string concatenation
                // * Handle WriteString(a + b) => WriteString(a); WriteString(b); => Write(a); Write(b);
                IrStatement::WriteString {
                    id: _,
                    expr: IrExpr::StringConcat { left, right, id: _ },
                } => {
                    transformed.push(IrStatement::WriteString {
                        id: stmt_ids.next(),
                        expr: *left,
                    });
                    transformed.push(IrStatement::WriteString {
                        id: stmt_ids.next(),
                        expr: *right,
                    });
                }
                IrStatement::WriteString {
                    id,
                    expr: IrExpr::StringLiteral { value: s, id: _ },
                } => {
                    let mut buf = String::new();
                    write_escaped_html(&s, &mut buf);
                    transformed.push(IrStatement::Write { id, content: buf });
                }
                _ => {
                    transformed.push(stmt);
                }
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
            simplify_write_exprs(&mut view.body, &mut module.stmt_ids);
        }
        for component in &mut module.components {
            simplify_write_exprs(&mut component.body, &mut module.stmt_ids);
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
                    t.write_string(t.str("Hello, World!"));
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  write_string("Hello, World!")
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
                    t.write_string(t.str("<div>Hello & Goodbye</div>"));
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  write_string("<div>Hello & Goodbye</div>")
                }

                -- after --
                view Test() {
                  write("&lt;div&gt;Hello &amp; Goodbye&lt;/div&gt;")
                }
            "#]],
        );
    }

    #[test]
    fn simplify_string_concat() {
        check(
            IrModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.write_string(t.string_concat(t.str("Hello"), t.str(" World")));
                })
                .build(),
            // TODO: This can be optimized further
            expect![[r#"
                -- before --
                view Test() {
                  write_string(("Hello" + " World"))
                }

                -- after --
                view Test() {
                  write_string("Hello")
                  write_string(" World")
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
                        t.write_string(t.str("Inside if"));
                        t.for_loop("item", t.array(vec![t.str("foo")]), |t| {
                            t.write_string(t.str("Inside for"));
                        });
                    });
                    t.let_stmt("x", t.str("value"), |t| {
                        t.write_string(t.str("Inside let"));
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match true {
                    true => {
                      write_string("Inside if")
                      for v0 in ["foo"] {
                        write_string("Inside for")
                      }
                    }
                    false => {
                    }
                  }
                  let v1 = "value" in {
                    write_string("Inside let")
                  }
                }

                -- after --
                view Test() {
                  match true {
                    true => {
                      write("Inside if")
                      for v0 in ["foo"] {
                        write("Inside for")
                      }
                    }
                    false => {
                    }
                  }
                  let v1 = "value" in {
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
                    t.write_string(t.str("Will become Write"));
                    t.write_string(t.var("x"));
                })
                .build(),
            expect![[r#"
                -- before --
                view Test(x@v0: String) {
                  write("Already a Write statement")
                  write_string("Will become Write")
                  write_string(v0)
                }

                -- after --
                view Test(x@v0: String) {
                  write("Already a Write statement")
                  write("Will become Write")
                  write_string(v0)
                }
            "#]],
        );
    }
}
