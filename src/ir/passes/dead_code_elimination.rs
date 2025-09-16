use super::Pass;
use crate::ir::{
    IrExpr,
    ast::IrExprValue,
    ast::{IrEntrypoint, IrStatement},
};

/// A pass that eliminates dead code, particularly unreachable If branches
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Check if an expression is a constant boolean value
    fn is_constant_boolean(expr: &IrExpr) -> Option<bool> {
        match &expr.value {
            IrExprValue::BooleanLiteral(b) => Some(*b),
            _ => None,
        }
    }

    /// Transform a list of statements, eliminating dead code
    fn transform_statements(statements: Vec<IrStatement>) -> Vec<IrStatement> {
        let mut result = Vec::new();
        for statement in statements {
            match statement {
                IrStatement::If {
                    id,
                    condition,
                    body,
                } => {
                    // Check if the condition is a constant boolean
                    if let Some(const_bool) = Self::is_constant_boolean(&condition) {
                        if const_bool {
                            // Condition is always true, replace with body
                            result.extend(Self::transform_statements(body));
                        }
                        // If false, do nothing - effectively removes this statement
                    } else {
                        // Can't evaluate at compile time, keep the if but transform body
                        result.push(IrStatement::If {
                            id,
                            condition,
                            body: Self::transform_statements(body),
                        });
                    }
                }
                IrStatement::For {
                    id,
                    var,
                    array,
                    body,
                } => {
                    // Transform the body of the for loop
                    result.push(IrStatement::For {
                        id,
                        var,
                        array,
                        body: Self::transform_statements(body),
                    });
                }
                IrStatement::Let {
                    id,
                    var,
                    value,
                    body,
                } => {
                    // Transform the body of the let
                    result.push(IrStatement::Let {
                        id,
                        var,
                        value,
                        body: Self::transform_statements(body),
                    });
                }
                _ => {
                    // All other statements are preserved as-is
                    result.push(statement);
                }
            }
        }
        result
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        entrypoint.body = Self::transform_statements(entrypoint.body);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = DeadCodeEliminationPass::new();
        let result = pass.run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_removes_always_true_if() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.if_stmt(t.bool(true), vec![t.write("Always shown")])]),
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("Always shown")
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_removes_always_false_if() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                t.if_stmt(t.bool(false), vec![t.write("Never shown")]),
                t.write("After if"),
            ]),
            expect![[r#"
            IrEntrypoint {
              parameters: []
              body: {
                Write("After if")
              }
            }
        "#]],
        );
    }

    #[test]
    fn test_preserves_dynamic_conditions() {
        let t = IrTestBuilder::new(vec![("show".to_string(), Type::Bool)]);
        check(
            t.build(vec![
                t.if_stmt(t.var("show"), vec![t.write("Dynamic")]),
                t.if_stmt(t.bool(true), vec![t.write("Static true")]),
                t.if_stmt(t.bool(false), vec![t.write("Static false")]),
            ]),
            expect![[r#"
            IrEntrypoint {
              parameters: [show: boolean]
              body: {
                If(condition: show) {
                  Write("Dynamic")
                }
                Write("Static true")
              }
            }
        "#]],
        );
    }
}
