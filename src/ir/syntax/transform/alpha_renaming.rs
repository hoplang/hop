use std::collections::{HashMap, HashSet};

use crate::dop::VarName;

use crate::ir::ast::{IrComponentDeclaration, IrEnumPattern, IrExpr, IrMatchArm, IrStatement};

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
                else_body,
            } => {
                self.push_scope();
                let renamed_body = self.rename_statements(body);
                self.pop_scope();

                let renamed_else_body = if let Some(else_stmts) = else_body {
                    self.push_scope();
                    let renamed = self.rename_statements(else_stmts);
                    self.pop_scope();
                    Some(renamed)
                } else {
                    None
                };

                IrStatement::If {
                    id,
                    condition: self.rename_expr(condition),
                    body: renamed_body,
                    else_body: renamed_else_body,
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
            IrExpr::Var { value, kind, id } => {
                let renamed = self.lookup_var(&value);
                IrExpr::Var {
                    value: renamed,
                    kind,
                    id,
                }
            }
            IrExpr::FieldAccess {
                record: object,
                field,
                kind,
                id,
            } => IrExpr::FieldAccess {
                record: Box::new(self.rename_expr(*object)),
                field,
                kind,
                id,
            },
            IrExpr::BooleanNegation { operand, id } => IrExpr::BooleanNegation {
                operand: Box::new(self.rename_expr(*operand)),
                id,
            },
            IrExpr::ArrayLiteral { elements, kind, id } => IrExpr::ArrayLiteral {
                elements: elements.into_iter().map(|e| self.rename_expr(e)).collect(),
                kind,
                id,
            },
            IrExpr::RecordLiteral {
                record_name,
                fields,
                kind,
                id,
            } => IrExpr::RecordLiteral {
                record_name,
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, self.rename_expr(v)))
                    .collect(),
                kind,
                id,
            },
            IrExpr::JsonEncode { value, id } => IrExpr::JsonEncode {
                value: Box::new(self.rename_expr(*value)),
                id,
            },
            IrExpr::EnvLookup { key, id } => IrExpr::EnvLookup {
                key: Box::new(self.rename_expr(*key)),
                id,
            },
            IrExpr::StringConcat { left, right, id } => IrExpr::StringConcat {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                id,
            },
            IrExpr::Equals {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::Equals {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                id,
            },
            IrExpr::LessThan {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::LessThan {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                id,
            },
            IrExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::LessThanOrEqual {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                id,
            },
            // Literals and enum variants don't contain variables
            IrExpr::StringLiteral { .. } => expr,
            IrExpr::BooleanLiteral { .. } => expr,
            IrExpr::FloatLiteral { .. } => expr,
            IrExpr::IntLiteral { .. } => expr,
            IrExpr::EnumLiteral { .. } => expr,
            IrExpr::Match {
                subject,
                arms,
                kind,
                id,
            } => IrExpr::Match {
                subject: Box::new(self.rename_expr(*subject)),
                arms: arms
                    .into_iter()
                    .map(|arm| IrMatchArm {
                        pattern: IrEnumPattern {
                            enum_name: arm.pattern.enum_name,
                            variant_name: arm.pattern.variant_name,
                        },
                        body: self.rename_expr(arm.body),
                    })
                    .collect(),
                kind,
                id,
            },
            IrExpr::BooleanLogicalAnd { left, right, id } => IrExpr::BooleanLogicalAnd {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                id,
            },
            IrExpr::BooleanLogicalOr { left, right, id } => IrExpr::BooleanLogicalOr {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                id,
            },
            IrExpr::NumericAdd {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::NumericAdd {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                id,
            },
            IrExpr::NumericSubtract {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::NumericSubtract {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                id,
            },
            IrExpr::NumericMultiply {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::NumericMultiply {
                left: Box::new(self.rename_expr(*left)),
                right: Box::new(self.rename_expr(*right)),
                operand_types,
                id,
            },
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
    fn run(mut comp_decl: IrComponentDeclaration) -> IrComponentDeclaration {
        let mut pass = AlphaRenamingPass::new();

        pass.push_scope();

        // Create renamed parameter mappings
        let mut renamed_params = Vec::new();
        for (param_name, _) in &comp_decl.parameters {
            let renamed = pass.bind_var(param_name);
            renamed_params.push(renamed);
        }

        // Rename the body
        let body = pass.rename_statements(comp_decl.body);

        pass.pop_scope();

        // Create Let bindings for parameters if they were renamed
        let mut result_body = body;
        for (renamed, (original, typ)) in
            renamed_params.into_iter().zip(&comp_decl.parameters).rev()
        {
            if renamed != *original {
                result_body = vec![IrStatement::Let {
                    id: 0, // ID will be assigned later if needed
                    var: renamed,
                    value: IrExpr::Var {
                        value: original.clone(),
                        kind: typ.clone(),
                        id: 0,
                    },
                    body: result_body,
                }];
            }
        }

        comp_decl.body = result_body;
        comp_decl
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::syntax::builder::build_ir;
    use expect_test::{Expect, expect};

    fn check(input_entrypoint: IrComponentDeclaration, expected: Expect) {
        let before = input_entrypoint.to_string();
        let renamed = AlphaRenamingPass::run(input_entrypoint);
        let after = renamed.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_no_renaming() {
        check(
            build_ir("Test", [("x", Type::String)], |t| {
                t.write_expr_escaped(t.var("x"));
            }),
            expect![[r#"
                -- before --
                Test(x: String) {
                  write_escaped(x)
                }

                -- after --
                Test(x: String) {
                  write_escaped(x)
                }
            "#]],
        );
    }

    #[test]
    fn shadowing_in_for_loop() {
        check(
            build_ir("Test", [("x", Type::String)], |t| {
                t.for_loop("x", t.array(vec![t.str("a")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                Test(x: String) {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                }

                -- after --
                Test(x: String) {
                  for x_1 in ["a"] {
                    write_escaped(x_1)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn sibling_scopes() {
        check(
            build_ir("Test", [], |t| {
                t.for_loop("x", t.array(vec![t.str("a")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.for_loop("x", t.array(vec![t.str("b")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                  for x in ["b"] {
                    write_escaped(x)
                  }
                }

                -- after --
                Test() {
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
    fn nested_let_bindings() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("x", t.str("hello"), |t| {
                    t.write_expr_escaped(t.var("x"));
                    t.let_stmt("x", t.str("world"), |t| {
                        t.write_expr_escaped(t.var("x"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = "hello" in {
                    write_escaped(x)
                    let x = "world" in {
                      write_escaped(x)
                    }
                  }
                }

                -- after --
                Test() {
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
    fn multiple_parameters() {
        check(
            build_ir(
                "Test",
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
                Test(x: String, y: String) {
                  write_escaped(x)
                  for y in ["a"] {
                    write_escaped(x)
                    write_escaped(y)
                  }
                }

                -- after --
                Test(x: String, y: String) {
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
    fn sibling_let_bindings() {
        check(
            build_ir("Test", [], |t| {
                t.let_stmt("x", t.str("first"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.let_stmt("x", t.str("second"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                Test() {
                  let x = "first" in {
                    write_escaped(x)
                  }
                  let x = "second" in {
                    write_escaped(x)
                  }
                }

                -- after --
                Test() {
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
    fn complex_nesting() {
        check(
            build_ir(
                "Test",
                vec![("items", Type::Array(Box::new(Type::String)))],
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
                Test(items: Array[String]) {
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
                Test(items: Array[String]) {
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
