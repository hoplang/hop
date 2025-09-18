use super::Pass;
use crate::ir::{
    IrExpr,
    ast::{IrEntrypoint, IrStatement, StatementId},
};
use std::collections::HashSet;

/// A pass that eliminates unused let statements
/// If a variable is never referenced in the body of the let, the let statement is replaced with its body
pub struct UnusedLetEliminationPass;

impl UnusedLetEliminationPass {
    /// Collect which let statements have unused variables
    fn collect_unused_lets(entrypoint: &IrEntrypoint) -> HashSet<StatementId> {
        let mut all_lets = HashSet::new();
        let mut used_lets = HashSet::new();

        // First collect all let statement IDs
        for stmt in &entrypoint.body {
            stmt.traverse(&mut |s| {
                if let IrStatement::Let { id, .. } = s {
                    all_lets.insert(*id);
                }
            });
        }

        // Now traverse with scope tracking to find used lets
        for stmt in &entrypoint.body {
            stmt.traverse_with_scope(&mut |s, scope| {
                // Process only the primary expression of this statement (not nested ones)
                // This avoids O(n²) behavior from traverse_exprs
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| {
                        if let IrExpr::Var { value: name, .. } = expr {
                            // If this variable is in scope and was defined by a Let, mark it as used
                            if let Some(IrStatement::Let { id, .. }) = scope.get(&name.to_string())
                            {
                                used_lets.insert(*id);
                            }
                        }
                    });
                }
            });
        }

        // Return the set of unused lets (all - used)
        all_lets.difference(&used_lets).cloned().collect()
    }

    /// Transform a list of statements, eliminating unused lets
    fn transform_statements(
        statements: Vec<IrStatement>,
        unused_lets: &HashSet<StatementId>,
    ) -> Vec<IrStatement> {
        statements
            .into_iter()
            .flat_map(|stmt| match stmt {
                // Unused let - replace with its body
                IrStatement::Let { body, id, .. } if unused_lets.contains(&id) => body,

                // All other statements pass through
                other => vec![other],
            })
            .collect()
    }
}

impl Pass for UnusedLetEliminationPass {
    fn run(mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        // First collect which let statements have unused variables
        let unused_lets = Self::collect_unused_lets(&entrypoint);

        // Use visit_mut to recursively process nested bodies
        for stmt in &mut entrypoint.body {
            stmt.traverse_mut(&mut |s| match s {
                IrStatement::If { body, .. }
                | IrStatement::For { body, .. }
                | IrStatement::Let { body, .. } => {
                    *body = Self::transform_statements(std::mem::take(body), &unused_lets);
                }
                _ => {}
            });
        }

        // Then transform top-level statements
        entrypoint.body = Self::transform_statements(entrypoint.body, &unused_lets);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let result = UnusedLetEliminationPass::run(entrypoint);
        expected.assert_eq(&result.to_string());
    }

    #[test]
    fn test_eliminate_unused_let() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // Unused let should be eliminated
                t.let_stmt("unused", t.str("value"), |t| vec![t.write("Hello")]),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("Hello")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_preserve_used_let() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                // Used let should be preserved
                t.let_stmt("message", t.str("Hello"), |t| {
                    vec![t.write_expr(t.var("message"), false)]
                }),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: message, value: "Hello") {
                      WriteExpr(expr: message, escape: false)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_unused_lets() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("outer", t.str("outer_value"), |t| {
                vec![t.let_stmt("inner", t.str("inner_value"), |t| {
                    vec![t.write("No variables used")]
                })]
            })]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("No variables used")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_used_in_nested_structure() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("cond", t.bool(true), |t| {
                vec![t.if_stmt(t.var("cond"), vec![t.write("Condition is true")])]
            })]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: cond, value: true) {
                      If(condition: cond) {
                        Write("Condition is true")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_eliminate_in_if_body() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.if_stmt(
                t.bool(true),
                vec![t.let_stmt("unused", t.str("value"), |t| vec![t.write("Inside if")])],
            )]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    If(condition: true) {
                      Write("Inside if")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_eliminate_in_for_body() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.for_loop(
                "item",
                t.array(vec![t.str("a"), t.str("b")]),
                |t| {
                    vec![t.let_stmt("unused", t.str("value"), |t| {
                        vec![t.write_expr(t.var("item"), false)]
                    })]
                },
            )]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    For(var: item, array: ["a", "b"]) {
                      WriteExpr(expr: item, escape: false)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_used_in_binary_op() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt("x", t.bool(true), |t| {
                vec![t.let_stmt("y", t.bool(false), |t| {
                    vec![t.if_stmt(t.eq(t.var("x"), t.var("y")), vec![t.write("Equal")])]
                })]
            })]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: x, value: true) {
                      Let(var: y, value: false) {
                        If(condition: (x == y)) {
                          Write("Equal")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_multiple_unused_lets_in_sequence() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                t.let_stmt("a", t.str("a_value"), |t| vec![t.write("First")]),
                t.let_stmt("b", t.str("b_value"), |t| vec![t.write("Second")]),
                t.write("Third"),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("First")
                    Write("Second")
                    Write("Third")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_variable_used_in_array() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt(
                "items",
                t.array(vec![t.str("a"), t.str("b")]),
                |t| {
                    vec![t.for_loop("item", t.var("items"), |t| {
                        vec![t.write_expr(t.var("item"), false)]
                    })]
                },
            )]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: items, value: ["a", "b"]) {
                      For(var: item, array: items) {
                        WriteExpr(expr: item, escape: false)
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_variable_used_in_property_access() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![t.let_stmt(
                "obj",
                t.object(vec![("name", t.str("value"))]),
                |t| vec![t.write_expr(t.prop_access(t.var("obj"), "name"), false)],
            )]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: obj, value: {name: "value"}) {
                      WriteExpr(expr: obj.name, escape: false)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_sibling_lets_same_name_first_used() {
        let t = IrTestBuilder::new(vec![]);
        check(
            t.build(vec![
                t.let_stmt("x", t.str("first x"), |t| {
                    vec![t.write_expr(t.var("x"), false)]
                }),
                t.let_stmt("x", t.str("second x"), |t| {
                    vec![t.write("No reference to x here")]
                }),
            ]),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: x, value: "first x") {
                      WriteExpr(expr: x, escape: false)
                    }
                    Write("No reference to x here")
                  }
                }
            "#]],
        );
    }
}
