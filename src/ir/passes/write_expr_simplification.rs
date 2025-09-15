use super::Pass;
use crate::common::escape_html;
use crate::ir::ast::{IrEntrypoint, IrExprValue, IrStatement};

/// A pass that simplifies WriteExpr statements with constant string expressions into a Write
/// statement
pub struct WriteExprSimplificationPass;

impl WriteExprSimplificationPass {
    pub fn new() -> Self {
        Self
    }

    fn transform_statement(statement: IrStatement) -> IrStatement {
        match statement {
            IrStatement::WriteExpr { id, expr, escape } => {
                // If the expression is a constant string, convert to Write statement
                if let IrExprValue::String(s) = &expr.value {
                    // Apply HTML escaping if needed
                    let content = if escape { escape_html(s) } else { s.clone() };
                    IrStatement::Write { id, content }
                } else {
                    IrStatement::WriteExpr { id, expr, escape }
                }
            }
            // Recursively transform child statements
            IrStatement::If {
                id,
                condition,
                body,
            } => IrStatement::If {
                id,
                condition,
                body: body.into_iter().map(Self::transform_statement).collect(),
            },
            IrStatement::For {
                id,
                var,
                array,
                body,
            } => IrStatement::For {
                id,
                var,
                array,
                body: body.into_iter().map(Self::transform_statement).collect(),
            },
            IrStatement::Let {
                id,
                var,
                value,
                body,
            } => IrStatement::Let {
                id,
                var,
                value,
                body: body.into_iter().map(Self::transform_statement).collect(),
            },
            // Write statements remain unchanged
            statement @ IrStatement::Write { .. } => statement,
        }
    }
}

impl Pass for WriteExprSimplificationPass {
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint {
        IrEntrypoint {
            parameters: entrypoint.parameters,
            body: entrypoint
                .body
                .into_iter()
                .map(Self::transform_statement)
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = WriteExprSimplificationPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_simplify_constant_string() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // WriteExpr with constant string should become Write
                    t.write_expr(t.str("Hello, World!"), false),
                ],
            },
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
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // WriteExpr with escaping enabled
                    t.write_expr(t.str("<div>Hello & Goodbye</div>"), true),
                ],
            },
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
    fn test_preserve_non_constant_expressions() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    // Variable reference should remain as WriteExpr
                    t.write_expr(t.var("message"), false),
                    // Boolean should remain as WriteExpr
                    t.write_expr(t.boolean(true), false),
                ],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    WriteExpr(expr: message, escape: false)
                    WriteExpr(expr: true, escape: false)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_transformations() {
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    t.if_stmt(
                        t.boolean(true),
                        vec![
                            t.write_expr(t.str("Inside if"), false),
                            t.for_loop(
                                "item",
                                t.array(vec![]),
                                vec![t.write_expr(t.str("Inside for"), false)],
                            ),
                        ],
                    ),
                    t.let_stmt(
                        "x",
                        t.str("value"),
                        vec![t.write_expr(t.str("Inside let"), false)],
                    ),
                ],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    If(condition: true) {
                      Write("Inside if")
                      For(var: item, array: []) {
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
        let t = IrTestBuilder::new();
        check(
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    t.write("Already a Write statement"),
                    t.write_expr(t.str("Will become Write"), false),
                    t.write_expr(t.var("stays_as_WriteExpr"), false),
                ],
            },
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("Already a Write statement")
                    Write("Will become Write")
                    WriteExpr(expr: stays_as_WriteExpr, escape: false)
                  }
                }
            "#]],
        );
    }
}
