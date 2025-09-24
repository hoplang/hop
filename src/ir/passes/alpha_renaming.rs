use std::collections::{HashMap, HashSet};

use crate::dop::VarName;

use crate::dop::typed_expr::TypedExpr;
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
            TypedExpr::Var {
                value,
                kind,
                annotation,
            } => {
                let renamed = self.lookup_var(&value);
                TypedExpr::Var {
                    value: renamed,
                    kind,
                    annotation,
                }
            }
            TypedExpr::PropertyAccess {
                object,
                property,
                kind,
                annotation,
            } => TypedExpr::PropertyAccess {
                object: Box::new(self.rename_expr(*object)),
                property,
                kind,
                annotation,
            },
            TypedExpr::Negation {
                operand,
                annotation,
            } => TypedExpr::Negation {
                operand: Box::new(self.rename_expr(*operand)),
                annotation,
            },
            TypedExpr::ArrayLiteral {
                elements,
                kind,
                annotation,
            } => TypedExpr::ArrayLiteral {
                elements: elements.into_iter().map(|e| self.rename_expr(e)).collect(),
                kind,
                annotation,
            },
            TypedExpr::ObjectLiteral {
                properties,
                kind,
                annotation,
            } => TypedExpr::ObjectLiteral {
                properties: properties
                    .into_iter()
                    .map(|(k, v)| (k, self.rename_expr(v)))
                    .collect(),
                kind,
                annotation,
            },
            TypedExpr::JsonEncode { value, annotation } => TypedExpr::JsonEncode {
                value: Box::new(self.rename_expr(*value)),
                annotation,
            },
            TypedExpr::StringConcat {
                left,
                right,
                annotation,
            } => TypedExpr::StringConcat {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                annotation,
            },
            TypedExpr::Equals {
                left,
                right,
                operand_types,
                annotation,
            } => TypedExpr::Equals {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                annotation,
            },
            TypedExpr::NotEquals {
                left,
                right,
                operand_types,
                annotation,
            } => TypedExpr::NotEquals {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                annotation,
            },
            TypedExpr::LessThan {
                left,
                right,
                operand_types,
                annotation,
            } => TypedExpr::LessThan {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                annotation,
            },
            // Literals don't contain variables
            TypedExpr::StringLiteral { .. } => expr,
            TypedExpr::BooleanLiteral { .. } => expr,
            TypedExpr::FloatLiteral { .. } => expr,
            TypedExpr::IntLiteral { .. } => expr,
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
}

impl Pass for AlphaRenamingPass {
    fn run(mut entrypoint: IrEntrypoint) -> IrEntrypoint {
        let mut pass = AlphaRenamingPass::new();

        pass.push_scope();

        // Create renamed parameter mappings
        let mut renamed_params = Vec::new();
        for (param_name, _) in &entrypoint.parameters {
            let renamed = pass.bind_var(param_name);
            renamed_params.push(renamed);
        }

        // Rename the body
        let body = pass.rename_statements(entrypoint.body);

        pass.pop_scope();

        // Create Let bindings for parameters if they were renamed
        let mut result_body = body;
        for (renamed, (original, typ)) in
            renamed_params.into_iter().zip(&entrypoint.parameters).rev()
        {
            if renamed != *original {
                result_body = vec![IrStatement::Let {
                    id: 0, // ID will be assigned later if needed
                    var: renamed,
                    value: TypedExpr::Var {
                        value: original.clone(),
                        kind: typ.clone(),
                        annotation: 0,
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
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    fn check(input_entrypoint: IrEntrypoint, expected: Expect) {
        let before = input_entrypoint.to_string();
        let renamed = AlphaRenamingPass::run(input_entrypoint);
        let after = renamed.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_no_renaming() {
        check(
            build_ir_auto("test", vec![("x", Type::String)], |t| {
                t.write_expr_escaped(t.var("x"));
            }),
            expect![[r#"
                -- before --
                test(x: string) {
                  write_escaped(x)
                }

                -- after --
                test(x: string) {
                  write_escaped(x)
                }
            "#]],
        );
    }

    #[test]
    fn test_shadowing_in_for_loop() {
        check(
            build_ir_auto("test", vec![("x", Type::String)], |t| {
                t.for_loop("x", t.array(vec![t.str("a")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                test(x: string) {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                }

                -- after --
                test(x: string) {
                  for x_1 in ["a"] {
                    write_escaped(x_1)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_sibling_scopes() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.for_loop("x", t.array(vec![t.str("a")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.for_loop("x", t.array(vec![t.str("b")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                  for x in ["b"] {
                    write_escaped(x)
                  }
                }

                -- after --
                test() {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                  for x_1 in ["b"] {
                    write_escaped(x_1)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_let_bindings() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("x", t.str("hello"), |t| {
                    t.write_expr_escaped(t.var("x"));
                    t.let_stmt("x", t.str("world"), |t| {
                        t.write_expr_escaped(t.var("x"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let x = "hello" in {
                    write_escaped(x)
                    let x = "world" in {
                      write_escaped(x)
                    }
                  }
                }

                -- after --
                test() {
                  let x = "hello" in {
                    write_escaped(x)
                    let x_1 = "world" in {
                      write_escaped(x_1)
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_multiple_parameters() {
        check(
            build_ir_auto(
                "test",
                vec![("x", Type::String), ("y", Type::String)],
                |t| {
                    t.write_expr_escaped(t.var("x"));
                    t.for_loop("y", t.array(vec![t.str("a")]), |t| {
                        t.write_expr_escaped(t.var("x"));
                        t.write_expr_escaped(t.var("y"));
                    });
                },
            ),
            expect![[r#"
                -- before --
                test(x: string, y: string) {
                  write_escaped(x)
                  for y in ["a"] {
                    write_escaped(x)
                    write_escaped(y)
                  }
                }

                -- after --
                test(x: string, y: string) {
                  write_escaped(x)
                  for y_1 in ["a"] {
                    write_escaped(x)
                    write_escaped(y_1)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_sibling_let_bindings() {
        check(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("x", t.str("first"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.let_stmt("x", t.str("second"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                test() {
                  let x = "first" in {
                    write_escaped(x)
                  }
                  let x = "second" in {
                    write_escaped(x)
                  }
                }

                -- after --
                test() {
                  let x = "first" in {
                    write_escaped(x)
                  }
                  let x_1 = "second" in {
                    write_escaped(x_1)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_complex_nesting() {
        check(
            build_ir_auto(
                "test",
                vec![("items", Type::Array(Some(Box::new(Type::String))))],
                |t| {
                    t.for_loop("item", t.var("items"), |t| {
                        t.write("<div>");
                        t.for_loop("item", t.array(vec![t.str("nested")]), |t| {
                            t.write_expr_escaped(t.var("item"));
                            t.let_stmt("item", t.str("let-value"), |t| {
                                t.write_expr_escaped(t.var("item"));
                            });
                        });
                        t.write_expr_escaped(t.var("item"));
                        t.write("</div>");
                    });
                },
            ),
            expect![[r#"
                -- before --
                test(items: array[string]) {
                  for item in items {
                    write("<div>")
                    for item in ["nested"] {
                      write_escaped(item)
                      let item = "let-value" in {
                        write_escaped(item)
                      }
                    }
                    write_escaped(item)
                    write("</div>")
                  }
                }

                -- after --
                test(items: array[string]) {
                  for item in items {
                    write("<div>")
                    for item_1 in ["nested"] {
                      write_escaped(item_1)
                      let item_2 = "let-value" in {
                        write_escaped(item_2)
                      }
                    }
                    write_escaped(item)
                    write("</div>")
                  }
                }
            "#]],
        );
    }
}
