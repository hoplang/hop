use super::Pass;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement, StatementEvent, StatementId};
use std::collections::{HashMap, HashSet};

/// A pass that eliminates unused let statements
/// If a variable is never referenced in the body of the let, the let statement is replaced with its body
pub struct UnusedLetEliminationPass;

impl UnusedLetEliminationPass {
    pub fn new() -> Self {
        Self
    }

    /// Collect which let statements have unused variables
    fn collect_unused_lets(entrypoint: &IrEntrypoint) -> HashSet<StatementId> {
        let mut unused_lets = HashSet::new();
        let mut scope_stack: HashMap<String, bool> = HashMap::new();

        for event in entrypoint.visit_statements() {
            match event {
                StatementEvent::Enter(statement) => {
                    match statement {
                        IrStatement::If { condition, .. } => {
                            Self::mark_vars_used_in_expr(condition, &mut scope_stack);
                        }
                        IrStatement::WriteExpr { expr, .. } => {
                            Self::mark_vars_used_in_expr(expr, &mut scope_stack);
                        }
                        IrStatement::For { array, .. } => {
                            Self::mark_vars_used_in_expr(array, &mut scope_stack);
                        }
                        IrStatement::Let { var, value, .. } => {
                            // Mark variables used in the value expression (before the new variable is in scope)
                            Self::mark_vars_used_in_expr(value, &mut scope_stack);
                            // Add this variable to scope, initially unused
                            scope_stack.insert(var.clone(), false);
                        }
                        IrStatement::Write { .. } => {}
                    }
                }
                StatementEvent::Exit(statement) => {
                    if let IrStatement::Let { id, var, .. } = statement {
                        // Check if this variable was used
                        if let Some(&used) = scope_stack.get(var) {
                            if !used {
                                // Variable was never used, mark this let for elimination
                                unused_lets.insert(*id);
                            }
                        }
                        // Remove from scope
                        scope_stack.remove(var);
                    }
                }
            }
        }

        unused_lets
    }

    /// Mark variables as used in an expression
    fn mark_vars_used_in_expr(expr: &IrExpr, scope_stack: &mut HashMap<String, bool>) {
        // Use dfs_iter to traverse all sub-expressions
        for e in expr.dfs_iter() {
            if let IrExpr::Var { value: name, .. } = e {
                // Mark this variable as used if it's in scope
                if let Some(used) = scope_stack.get_mut(name) {
                    *used = true;
                }
            }
        }
    }

    /// Transform a list of statements, eliminating unused lets
    fn transform_statements(
        statements: Vec<IrStatement>,
        unused_lets: &HashSet<StatementId>,
    ) -> Vec<IrStatement> {
        statements
            .into_iter()
            .flat_map(|statement| Self::transform_statement(statement, unused_lets))
            .collect()
    }

    /// Transform a single statement, returning a list of statements (to allow flattening)
    fn transform_statement(
        statement: IrStatement,
        unused_lets: &HashSet<StatementId>,
    ) -> Vec<IrStatement> {
        match statement {
            IrStatement::Let {
                var,
                value,
                body,
                id,
            } => {
                // Check if this let is marked as unused
                if unused_lets.contains(&id) {
                    // Variable is unused, eliminate the let and just return the transformed body
                    Self::transform_statements(body, unused_lets)
                } else {
                    // Variable is used, keep the let but transform the body
                    vec![IrStatement::Let {
                        id,
                        var,
                        value,
                        body: Self::transform_statements(body, unused_lets),
                    }]
                }
            }
            IrStatement::If {
                id,
                condition,
                body,
            } => {
                vec![IrStatement::If {
                    id,
                    condition,
                    body: Self::transform_statements(body, unused_lets),
                }]
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
                    body: Self::transform_statements(body, unused_lets),
                }]
            }
            // Write and WriteExpr statements remain unchanged
            statement @ (IrStatement::Write { .. } | IrStatement::WriteExpr { .. }) => {
                vec![statement]
            }
        }
    }
}

impl Pass for UnusedLetEliminationPass {
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint {
        // First collect which let statements have unused variables
        let unused_lets = Self::collect_unused_lets(&entrypoint);

        // Then transform the entrypoint, eliminating unused lets
        IrEntrypoint {
            parameters: entrypoint.parameters,
            body: Self::transform_statements(entrypoint.body, &unused_lets),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn check(entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = UnusedLetEliminationPass::new();
        let result = pass.run(entrypoint);
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
