use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypoint, IrStatement},
};

/// A pass that eliminates dead code, particularly unreachable If branches
pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    fn transform_statements(statements: Vec<IrStatement>) -> Vec<IrStatement> {
        statements
            .into_iter()
            .flat_map(|stmt| match stmt {
                // If with constant true condition - replace with body
                IrStatement::If {
                    condition: IrExpr::BooleanLiteral { value: true, .. },
                    body,
                    ..
                } => body,

                // If with constant false condition - remove entirely
                IrStatement::If {
                    condition: IrExpr::BooleanLiteral { value: false, .. },
                    ..
                } => vec![],

                // All other statements (including dynamic If) pass through unchanged
                other => vec![other],
            })
            .collect()
    }
}

impl Pass for DeadCodeEliminationPass {
    fn run(mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        // First, recursively process all nested bodies using visit_mut
        for stmt in &mut entrypoint.body {
            stmt.traverse_mut(&mut |s| match s {
                IrStatement::If { body, .. }
                | IrStatement::For { body, .. }
                | IrStatement::Let { body, .. } => {
                    *body = Self::transform_statements(std::mem::take(body));
                }
                _ => {}
            });
        }

        // Then transform top-level statements
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
        let result = DeadCodeEliminationPass::run(entrypoint);
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
