use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypoint, IrStatement},
};

/// A pass that eliminates dead code, particularly unreachable If branches
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    pub fn new() -> Self {
        Self
    }

    fn transform_statements(statements: Vec<IrStatement>) -> Vec<IrStatement> {
        statements
            .into_iter()
            .flat_map(Self::transform_statement)
            .collect()
    }

    fn transform_statement(statement: IrStatement) -> Vec<IrStatement> {
        match statement {
            IrStatement::If {
                id,
                condition,
                body,
            } => {
                // Check if condition is a constant boolean
                match &condition {
                    IrExpr::BooleanLiteral { value: true, .. } => {
                        // If always true, replace with body
                        Self::transform_statements(body)
                    }
                    IrExpr::BooleanLiteral { value: false, .. } => {
                        // If always false, remove entirely
                        vec![]
                    }
                    _ => {
                        // Dynamic condition, keep if but transform body
                        vec![IrStatement::If {
                            id,
                            condition,
                            body: Self::transform_statements(body),
                        }]
                    }
                }
            }
            IrStatement::For {
                id,
                var,
                array,
                body,
            } => vec![IrStatement::For {
                id,
                var,
                array,
                body: Self::transform_statements(body),
            }],
            IrStatement::Let {
                id,
                var,
                value,
                body,
            } => vec![IrStatement::Let {
                id,
                var,
                value,
                body: Self::transform_statements(body),
            }],
            // Leaf statements (Write, WriteExpr) pass through unchanged
            statement => vec![statement],
        }
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint {
        IrEntrypoint {
            parameters: entrypoint.parameters,
            body: Self::transform_statements(entrypoint.body),
        }
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
