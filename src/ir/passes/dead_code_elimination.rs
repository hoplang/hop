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

    /// Check if an expression is a constant boolean value
    fn is_constant_boolean(expr: &IrExpr) -> Option<bool> {
        match expr {
            IrExpr::BooleanLiteral { value, .. } => Some(*value),
            _ => None,
        }
    }

    fn transform_statement(statement: IrStatement) -> Vec<IrStatement> {
        match statement {
            IrStatement::If {
                id,
                condition,
                body,
            } => {
                if let Some(const_bool) = Self::is_constant_boolean(&condition) {
                    if const_bool {
                        body.into_iter()
                            .flat_map(Self::transform_statement)
                            .collect()
                    } else {
                        vec![]
                    }
                } else {
                    vec![IrStatement::If {
                        id,
                        condition,
                        body: body
                            .into_iter()
                            .flat_map(Self::transform_statement)
                            .collect(),
                    }]
                }
            }
            IrStatement::For {
                id,
                var,
                array,
                body,
            } => {
                vec![IrStatement::For {
                    id,
                    var,
                    array,
                    body: body
                        .into_iter()
                        .flat_map(Self::transform_statement)
                        .collect(),
                }]
            }
            IrStatement::Let {
                id,
                var,
                value,
                body,
            } => {
                vec![IrStatement::Let {
                    id,
                    var,
                    value,
                    body: body
                        .into_iter()
                        .flat_map(Self::transform_statement)
                        .collect(),
                }]
            }
            _ => {
                vec![statement]
            }
        }
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint {
        IrEntrypoint {
            parameters: entrypoint.parameters,
            body: entrypoint
                .body
                .into_iter()
                .flat_map(Self::transform_statement)
                .collect(),
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
