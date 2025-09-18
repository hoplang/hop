use super::Pass;
use crate::common::escape_html;
use crate::ir::IrExpr;
use crate::ir::ast::{IrEntrypoint, IrStatement};

/// A pass that simplifies WriteExpr statements with constant string expressions into a Write
/// statement
pub struct WriteExprSimplificationPass;

impl WriteExprSimplificationPass {
    pub fn new() -> Self {
        Self
    }
}

impl Pass for WriteExprSimplificationPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
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
    use crate::{dop::Type, ir::test_utils::IrTestBuilder};
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = WriteExprSimplificationPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_simplify_constant_string() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // WriteExpr with constant string should become Write
                t.write_expr(t.str("Hello, World!"), false),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("Hello, World!")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_simplify_with_escaping() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // WriteExpr with escaping enabled
                t.write_expr(t.str("<div>Hello & Goodbye</div>"), true),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("&lt;div&gt;Hello &amp; Goodbye&lt;/div&gt;")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_transformations() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                t.if_stmt(
                    t.bool(true),
                    vec![
                        t.write_expr(t.str("Inside if"), false),
                        t.for_loop("item", t.array(vec![t.str("foo")]), |t| {
                            vec![t.write_expr(t.str("Inside for"), false)]
                        }),
                    ],
                ),
                t.let_stmt("x", t.str("value"), |t| {
                    vec![t.write_expr(t.str("Inside let"), false)]
                }),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    If(condition: true) {
                      Write("Inside if")
                      For(var: item, array: ["foo"]) {
                        Write("Inside for")
                      }
                    }
                    Let(var: x, value: "value") {
                      Write("Inside let")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_mixed_write_and_write_expr() {
        let t = IrTestBuilder::new(vec![("x".to_string(), Type::String)]);
        check(
            t.build(vec![
                t.write("Already a Write statement"),
                t.write_expr(t.str("Will become Write"), false),
                t.write_expr(t.var("x"), false),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: [x: string]
                  body: {
                    Write("Already a Write statement")
                    Write("Will become Write")
                    WriteExpr(expr: x, escape: false)
                  }
                }
            "#]],
        );
    }
}
