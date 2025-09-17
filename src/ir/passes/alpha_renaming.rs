use std::collections::{HashMap, HashSet};

use crate::dop::VarName;

use crate::dop::expr::Expr;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};

use super::Pass;

/// Alpha renaming pass for the IR AST.
/// This ensures all variable names are unique to avoid conflicts and shadowing.
pub struct AlphaRenamingPass {
    /// Counter for generating unique variable names
    var_counter: usize,
    /// Stack of scopes mapping original names to renamed names
    scope_stack: Vec<HashMap<String, VarName>>,
    /// Track all variable names ever used to ensure uniqueness
    all_used_names: HashSet<String>,
}

impl AlphaRenamingPass {
    pub fn new() -> Self {
        Self {
            var_counter: 0,
            scope_stack: vec![HashMap::new()],
            all_used_names: HashSet::new(),
        }
    }

    /// Rename variables in a list of statements
    fn rename_statements(&mut self, statements: Vec<IrStatement>) -> Vec<IrStatement> {
        statements
            .into_iter()
            .map(|stmt| self.rename_statement(stmt))
            .collect()
    }

    /// Rename variables in a single statement
    fn rename_statement(&mut self, statement: IrStatement) -> IrStatement {
        match statement {
            IrStatement::Write { id, content } => IrStatement::Write { id, content },

            IrStatement::WriteExpr { id, expr, escape } => IrStatement::WriteExpr {
                id,
                expr: self.rename_expr(expr),
                escape,
            },

            IrStatement::If {
                id,
                condition,
                body,
            } => {
                self.push_scope();
                let renamed_body = self.rename_statements(body);
                self.pop_scope();

                IrStatement::If {
                    id,
                    condition: self.rename_expr(condition),
                    body: renamed_body,
                }
            }

            IrStatement::For {
                id,
                var,
                array,
                body,
            } => {
                self.push_scope();
                let renamed_var = self.bind_var(&var);
                let renamed_body = self.rename_statements(body);
                self.pop_scope();

                IrStatement::For {
                    id,
                    var: renamed_var,
                    array: self.rename_expr(array),
                    body: renamed_body,
                }
            }

            IrStatement::Let {
                id,
                var,
                value,
                body,
            } => {
                let renamed_value = self.rename_expr(value);
                self.push_scope();
                let renamed_var = self.bind_var(&var);
                let renamed_body = self.rename_statements(body);
                self.pop_scope();

                IrStatement::Let {
                    id,
                    var: renamed_var,
                    value: renamed_value,
                    body: renamed_body,
                }
            }
        }
    }

    /// Rename variables in an expression
    fn rename_expr(&mut self, expr: IrExpr) -> IrExpr {
        match expr {
            Expr::Var { value, annotation } => {
                let renamed = self.lookup_var(&value);
                Expr::Var {
                    value: renamed,
                    annotation,
                }
            }
            Expr::PropertyAccess {
                object,
                property,
                annotation,
            } => Expr::PropertyAccess {
                object: Box::new(self.rename_expr(*object)),
                property,
                annotation,
            },
            Expr::BinaryOp {
                left,
                operator,
                right,
                annotation,
            } => Expr::BinaryOp {
                left: Box::new(self.rename_expr(*left)),
                operator,
                right: Box::new(self.rename_expr(*right)),
                annotation,
            },
            Expr::UnaryOp {
                operator,
                operand,
                annotation,
            } => Expr::UnaryOp {
                operator,
                operand: Box::new(self.rename_expr(*operand)),
                annotation,
            },
            Expr::ArrayLiteral {
                elements,
                annotation,
            } => Expr::ArrayLiteral {
                elements: elements.into_iter().map(|e| self.rename_expr(e)).collect(),
                annotation,
            },
            Expr::ObjectLiteral {
                properties,
                annotation,
            } => Expr::ObjectLiteral {
                properties: properties
                    .into_iter()
                    .map(|(k, v)| (k, self.rename_expr(v)))
                    .collect(),
                annotation,
            },
            Expr::JsonEncode { value, annotation } => Expr::JsonEncode {
                value: Box::new(self.rename_expr(*value)),
                annotation,
            },
            // Literals don't contain variables
            Expr::StringLiteral { .. } => expr,
            Expr::BooleanLiteral { .. } => expr,
            Expr::NumberLiteral { .. } => expr,
        }
    }

    /// Push a new scope onto the stack
    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    /// Pop a scope from the stack
    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    /// Generate a fresh variable name
    fn fresh_var(&mut self, name: &VarName) -> VarName {
        self.var_counter += 1;
        VarName::try_from(format!("{}_{}", name, self.var_counter)).unwrap()
    }

    /// Bind a variable in the current scope, renaming if necessary
    fn bind_var(&mut self, name: &VarName) -> VarName {
        // Check if this name would shadow an existing binding OR has been used before
        let needs_renaming =
            self.is_name_in_scope(name) || self.all_used_names.contains(name.as_str());

        let renamed = if needs_renaming {
            self.fresh_var(name)
        } else {
            name.clone()
        };

        // Track this name as used
        self.all_used_names.insert(renamed.to_string());

        self.scope_stack
            .last_mut()
            .expect("Scope stack should not be empty")
            .insert(name.to_string(), renamed.clone());

        renamed
    }

    /// Check if a name exists in any parent scope (not the current one)
    fn is_name_in_scope(&self, name: &VarName) -> bool {
        // Check if name exists in any parent scope (not the current one)
        for scope in self.scope_stack.iter().rev().skip(1) {
            if scope.contains_key(name.as_str()) {
                return true;
            }
        }
        false
    }

    /// Look up the renamed version of a variable
    fn lookup_var(&self, name: &VarName) -> VarName {
        for scope in self.scope_stack.iter().rev() {
            if let Some(renamed) = scope.get(name.as_str()) {
                return renamed.clone();
            }
        }
        // If we can't find the variable, it must be a parameter or external reference
        // Return it unchanged
        name.clone()
    }

    /// Reset the pass state for a new entrypoint
    fn reset(&mut self) {
        self.var_counter = 0;
        self.scope_stack.clear();
        self.scope_stack.push(HashMap::new());
        self.all_used_names.clear();
    }
}

impl Pass for AlphaRenamingPass {
    fn run(&mut self, mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        // Reset state for each entrypoint
        self.reset();

        self.push_scope();

        // Create renamed parameter mappings
        let mut renamed_params = Vec::new();
        for (param_name, _) in &entrypoint.parameters {
            let renamed = self.bind_var(param_name);
            renamed_params.push(renamed);
        }

        // Rename the body
        let body = self.rename_statements(entrypoint.body);

        self.pop_scope();

        // Create Let bindings for parameters if they were renamed
        let mut result_body = body;
        for (renamed, (original, typ)) in
            renamed_params.into_iter().zip(&entrypoint.parameters).rev()
        {
            if renamed != *original {
                result_body = vec![IrStatement::Let {
                    id: 0, // ID will be assigned later if needed
                    var: renamed,
                    value: Expr::Var {
                        value: original.clone(),
                        annotation: (0, typ.clone()),
                    },
                    body: result_body,
                }];
            }
        }

        entrypoint.body = result_body;
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn check_renaming(input_entrypoint: IrEntrypoint, expected: Expect) {
        let mut pass = AlphaRenamingPass::new();
        let renamed = pass.run(input_entrypoint);
        expected.assert_eq(&renamed.to_string());
    }

    #[test]
    fn test_simple_no_renaming() {
        let builder = IrTestBuilder::new(vec![("x".to_string(), Type::String)]);

        let entrypoint = builder.build(vec![builder.write_expr(builder.var("x"), true)]);

        check_renaming(
            entrypoint,
            expect![[r#"
                IrEntrypoint {
                  parameters: [x: string]
                  body: {
                    WriteExpr(expr: x, escape: true)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_shadowing_in_for_loop() {
        let builder = IrTestBuilder::new(vec![("x".to_string(), Type::String)]);

        let entrypoint = builder.build(vec![builder.for_loop(
            "x",
            builder.array(vec![builder.str("a")]),
            |b| vec![b.write_expr(b.var("x"), true)],
        )]);

        check_renaming(
            entrypoint,
            expect![[r#"
                IrEntrypoint {
                  parameters: [x: string]
                  body: {
                    For(var: x_1, array: ["a"]) {
                      WriteExpr(expr: x_1, escape: true)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_sibling_scopes() {
        let builder = IrTestBuilder::new(vec![]);

        let entrypoint = builder.build(vec![
            builder.for_loop("x", builder.array(vec![builder.str("a")]), |b| {
                vec![b.write_expr(b.var("x"), true)]
            }),
            builder.for_loop("x", builder.array(vec![builder.str("b")]), |b| {
                vec![b.write_expr(b.var("x"), true)]
            }),
        ]);

        check_renaming(
            entrypoint,
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    For(var: x, array: ["a"]) {
                      WriteExpr(expr: x, escape: true)
                    }
                    For(var: x_1, array: ["b"]) {
                      WriteExpr(expr: x_1, escape: true)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_let_bindings() {
        let builder = IrTestBuilder::new(vec![]);

        let entrypoint = builder.build(vec![builder.let_stmt("x", builder.str("hello"), |b| {
            vec![
                b.write_expr(b.var("x"), true),
                b.let_stmt("x", builder.str("world"), |b2| {
                    vec![b2.write_expr(b2.var("x"), true)]
                }),
            ]
        })]);

        check_renaming(
            entrypoint,
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Let(var: x, value: "hello") {
                      WriteExpr(expr: x, escape: true)
                      Let(var: x_1, value: "world") {
                        WriteExpr(expr: x_1, escape: true)
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_multiple_parameters() {
        let builder = IrTestBuilder::new(vec![
            ("x".to_string(), Type::String),
            ("y".to_string(), Type::String),
        ]);

        let entrypoint = builder.build(vec![
            builder.write_expr(builder.var("x"), true),
            builder.for_loop("y", builder.array(vec![builder.str("a")]), |b| {
                vec![
                    b.write_expr(b.var("x"), true),
                    b.write_expr(b.var("y"), true),
                ]
            }),
        ]);

        check_renaming(
            entrypoint,
            expect![[r#"
                IrEntrypoint {
                  parameters: [x: string, y: string]
                  body: {
                    WriteExpr(expr: x, escape: true)
                    For(var: y_1, array: ["a"]) {
                      WriteExpr(expr: x, escape: true)
                      WriteExpr(expr: y_1, escape: true)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_complex_nesting() {
        let builder = IrTestBuilder::new(vec![(
            "items".to_string(),
            Type::Array(Some(Box::new(Type::String))),
        )]);

        let entrypoint = builder.build(vec![builder.for_loop("item", builder.var("items"), |b| {
            vec![
                b.write("<div>"),
                b.for_loop("item", builder.array(vec![b.str("nested")]), |b2| {
                    vec![
                        b2.write_expr(b2.var("item"), true),
                        b2.let_stmt("item", b2.str("let-value"), |b3| {
                            vec![b3.write_expr(b3.var("item"), true)]
                        }),
                    ]
                }),
                b.write_expr(b.var("item"), true),
                b.write("</div>"),
            ]
        })]);

        check_renaming(
            entrypoint,
            expect![[r#"
                IrEntrypoint {
                  parameters: [items: array[string]]
                  body: {
                    For(var: item, array: items) {
                      Write("<div>")
                      For(var: item_1, array: ["nested"]) {
                        WriteExpr(expr: item_1, escape: true)
                        Let(var: item_2, value: "let-value") {
                          WriteExpr(expr: item_2, escape: true)
                        }
                      }
                      WriteExpr(expr: item, escape: true)
                      Write("</div>")
                    }
                  }
                }
            "#]],
        );
    }
}
