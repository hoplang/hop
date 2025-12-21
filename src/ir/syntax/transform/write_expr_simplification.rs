use super::Pass;
use crate::common::escape_html;
use crate::ir::IrExpr;
use crate::ir::ast::{IrComponentDeclaration, IrStatement};

/// A pass that simplifies WriteExpr statements with constant string expressions into a Write
/// statement
pub struct WriteExprSimplificationPass;

impl Pass for WriteExprSimplificationPass {
    fn run(mut entrypoint: IrComponentDeclaration) -> IrComponentDeclaration {
        // Use visit_mut to transform all statements in the tree
        for stmt in &mut entrypoint.body {
            stmt.traverse_mut(&mut |statement| {
                // Transform WriteExpr with constant strings into Write
                if let IrStatement::WriteExpr {
                    id,
                    expr: IrExpr::StringLiteral { value: s, .. },
                    escape,
                } = statement
                {
                    // Apply HTML escaping if needed
                    let content = if *escape { escape_html(s) } else { s.clone() };
                    *statement = IrStatement::Write { id: *id, content };
                }
            });
        }
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{dop::Type, ir::syntax::builder::build_ir};
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrComponentDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        let result = WriteExprSimplificationPass::run(entrypoint);
        let after = result.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simplify_constant_string() {
        check(
            build_ir("Test", [], |t| {
                // WriteExpr with constant string should become Write
                t.write_expr(t.str("Hello, World!"), false);
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_expr("Hello, World!")
                }

                -- after --
                Test() {
                  write("Hello, World!")
                }
            "#]],
        );
    }

    #[test]
    fn simplify_with_escaping() {
        check(
            build_ir("Test", [], |t| {
                // WriteExpr with escaping enabled
                t.write_expr(t.str("<div>Hello & Goodbye</div>"), true);
            }),
            expect![[r#"
                -- before --
                Test() {
                  write_escaped("<div>Hello & Goodbye</div>")
                }

                -- after --
                Test() {
                  write("&lt;div&gt;Hello &amp; Goodbye&lt;/div&gt;")
                }
            "#]],
        );
    }

    #[test]
    fn nested_transformations() {
        check(
            build_ir("Test", [], |t| {
                t.if_stmt(t.bool(true), |t| {
                    t.write_expr(t.str("Inside if"), false);
                    t.for_loop("item", t.array(vec![t.str("foo")]), |t| {
                        t.write_expr(t.str("Inside for"), false);
                    });
                });
                t.let_stmt("x", t.str("value"), |t| {
                    t.write_expr(t.str("Inside let"), false);
                });
            }),
            expect![[r#"
                -- before --
                Test() {
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
                Test() {
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
            build_ir("Test", [("x", Type::String)], |t| {
                t.write("Already a Write statement");
                t.write_expr(t.str("Will become Write"), false);
                t.write_expr(t.var("x"), false);
            }),
            expect![[r#"
                -- before --
                Test(x: String) {
                  write("Already a Write statement")
                  write_expr("Will become Write")
                  write_expr(x)
                }

                -- after --
                Test(x: String) {
                  write("Already a Write statement")
                  write("Will become Write")
                  write_expr(x)
                }
            "#]],
        );
    }
}
