use std::collections::{HashMap, HashSet};

use crate::dop::VarName;

use crate::dop::patterns::{EnumMatchArm, Match};
use crate::ir::ast::{IrComponentDeclaration, IrExpr, IrStatement};

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
                expr: self.rename_expr(&expr),
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
                    condition: self.rename_expr(&condition),
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
                    array: self.rename_expr(&array),
                    body: renamed_body,
                }
            }

            IrStatement::Let {
                id,
                var,
                value,
                body,
            } => {
                let renamed_value = self.rename_expr(&value);
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

            IrStatement::Match { id, match_ } => {
                let renamed_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => {
                        let renamed_subject = (self.lookup_var(&subject.0), subject.1.clone());
                        self.push_scope();
                        let renamed_true_body = self.rename_statements(*true_body);
                        self.pop_scope();
                        self.push_scope();
                        let renamed_false_body = self.rename_statements(*false_body);
                        self.pop_scope();
                        Match::Bool {
                            subject: renamed_subject,
                            true_body: Box::new(renamed_true_body),
                            false_body: Box::new(renamed_false_body),
                        }
                    }
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let renamed_subject = (self.lookup_var(&subject.0), subject.1.clone());
                        self.push_scope();
                        let renamed_binding = some_arm_binding.map(|var| self.bind_var(&var));
                        let renamed_some_body = self.rename_statements(*some_arm_body);
                        self.pop_scope();
                        self.push_scope();
                        let renamed_none_body = self.rename_statements(*none_arm_body);
                        self.pop_scope();
                        Match::Option {
                            subject: renamed_subject,
                            some_arm_binding: renamed_binding,
                            some_arm_body: Box::new(renamed_some_body),
                            none_arm_body: Box::new(renamed_none_body),
                        }
                    }
                    Match::Enum { subject, arms } => {
                        let renamed_subject = (self.lookup_var(&subject.0), subject.1.clone());
                        let renamed_arms = arms
                            .into_iter()
                            .map(|arm| {
                                self.push_scope();
                                // Rename bindings and add them to scope
                                let renamed_bindings: Vec<_> = arm
                                    .bindings
                                    .into_iter()
                                    .map(|(field, var)| {
                                        let renamed_var = self.bind_var(&var);
                                        (field, renamed_var)
                                    })
                                    .collect();
                                let renamed_body = self.rename_statements(arm.body);
                                self.pop_scope();
                                EnumMatchArm {
                                    pattern: arm.pattern,
                                    bindings: renamed_bindings,
                                    body: renamed_body,
                                }
                            })
                            .collect();
                        Match::Enum {
                            subject: renamed_subject,
                            arms: renamed_arms,
                        }
                    }
                };
                IrStatement::Match {
                    id,
                    match_: renamed_match,
                }
            }
        }
    }

    /// Rename variables in an expression
    fn rename_expr(&mut self, expr: &IrExpr) -> IrExpr {
        match expr {
            IrExpr::Var { value, kind, id } => {
                let renamed = self.lookup_var(value);
                IrExpr::Var {
                    value: renamed,
                    kind: kind.clone(),
                    id: *id,
                }
            }
            IrExpr::FieldAccess {
                record: object,
                field,
                kind,
                id,
            } => IrExpr::FieldAccess {
                record: Box::new(self.rename_expr(object)),
                field: field.clone(),
                kind: kind.clone(),
                id: *id,
            },
            IrExpr::BooleanNegation { operand, id } => IrExpr::BooleanNegation {
                operand: Box::new(self.rename_expr(operand)),
                id: *id,
            },
            IrExpr::ArrayLiteral { elements, kind, id } => IrExpr::ArrayLiteral {
                elements: elements.iter().map(|e| self.rename_expr(e)).collect(),
                kind: kind.clone(),
                id: *id,
            },
            IrExpr::RecordLiteral {
                record_name,
                fields,
                kind,
                id,
            } => IrExpr::RecordLiteral {
                record_name: record_name.clone(),
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.rename_expr(v)))
                    .collect(),
                kind: kind.clone(),
                id: *id,
            },
            IrExpr::JsonEncode { value, id } => IrExpr::JsonEncode {
                value: Box::new(self.rename_expr(value)),
                id: *id,
            },
            IrExpr::EnvLookup { key, id } => IrExpr::EnvLookup {
                key: Box::new(self.rename_expr(key)),
                id: *id,
            },
            IrExpr::StringConcat { left, right, id } => IrExpr::StringConcat {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                id: *id,
            },
            IrExpr::Equals {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::Equals {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                operand_types: operand_types.clone(),
                id: *id,
            },
            IrExpr::LessThan {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::LessThan {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                operand_types: operand_types.clone(),
                id: *id,
            },
            IrExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::LessThanOrEqual {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                operand_types: operand_types.clone(),
                id: *id,
            },
            // Literals and enum variants don't contain variables
            IrExpr::StringLiteral { value, id } => IrExpr::StringLiteral {
                value: value.clone(),
                id: *id,
            },
            IrExpr::BooleanLiteral { value, id } => IrExpr::BooleanLiteral {
                value: *value,
                id: *id,
            },
            IrExpr::FloatLiteral { value, id } => IrExpr::FloatLiteral {
                value: *value,
                id: *id,
            },
            IrExpr::IntLiteral { value, id } => IrExpr::IntLiteral {
                value: *value,
                id: *id,
            },
            IrExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                kind,
                id,
            } => IrExpr::EnumLiteral {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), self.rename_expr(field_expr))
                    })
                    .collect(),
                kind: kind.clone(),
                id: *id,
            },
            IrExpr::Match { match_, kind, id } => {
                let renamed_match = match match_ {
                    Match::Enum { subject, arms } => {
                        let renamed_arms = arms
                            .iter()
                            .map(|arm| {
                                self.push_scope();
                                // Rename bindings and add them to scope
                                let renamed_bindings: Vec<_> = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, var)| {
                                        let renamed_var = self.bind_var(var);
                                        (field.clone(), renamed_var)
                                    })
                                    .collect();
                                let renamed_body = self.rename_expr(&arm.body);
                                self.pop_scope();
                                EnumMatchArm {
                                    pattern: arm.pattern.clone(),
                                    bindings: renamed_bindings,
                                    body: renamed_body,
                                }
                            })
                            .collect();
                        Match::Enum {
                            subject: (self.lookup_var(&subject.0), subject.1.clone()),
                            arms: renamed_arms,
                        }
                    }
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: (self.lookup_var(&subject.0), subject.1.clone()),
                        true_body: Box::new(self.rename_expr(true_body)),
                        false_body: Box::new(self.rename_expr(false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => Match::Option {
                        subject: (self.lookup_var(&subject.0), subject.1.clone()),
                        some_arm_binding: some_arm_binding.clone(),
                        some_arm_body: Box::new(self.rename_expr(some_arm_body)),
                        none_arm_body: Box::new(self.rename_expr(none_arm_body)),
                    },
                };
                IrExpr::Match {
                    match_: renamed_match,
                    kind: kind.clone(),
                    id: *id,
                }
            }
            IrExpr::BooleanLogicalAnd { left, right, id } => IrExpr::BooleanLogicalAnd {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                id: *id,
            },
            IrExpr::BooleanLogicalOr { left, right, id } => IrExpr::BooleanLogicalOr {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                id: *id,
            },
            IrExpr::NumericAdd {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::NumericAdd {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                operand_types: operand_types.clone(),
                id: *id,
            },
            IrExpr::NumericSubtract {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::NumericSubtract {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                operand_types: operand_types.clone(),
                id: *id,
            },
            IrExpr::NumericMultiply {
                left,
                right,
                operand_types,
                id,
            } => IrExpr::NumericMultiply {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                operand_types: operand_types.clone(),
                id: *id,
            },
            IrExpr::Let {
                var,
                value,
                body,
                kind,
                id,
            } => {
                let renamed_value = self.rename_expr(value);
                self.push_scope();
                let new_var = self.bind_var(var);
                let renamed_body = self.rename_expr(body);
                self.pop_scope();
                IrExpr::Let {
                    var: new_var,
                    value: Box::new(renamed_value),
                    body: Box::new(renamed_body),
                    kind: kind.clone(),
                    id: *id,
                }
            }
            IrExpr::OptionLiteral { value, kind, id } => IrExpr::OptionLiteral {
                value: value.as_ref().map(|v| Box::new(self.rename_expr(v))),
                kind: kind.clone(),
                id: *id,
            },
            IrExpr::MergeClasses { left, right, id } => IrExpr::MergeClasses {
                left: Box::new(self.rename_expr(left)),
                right: Box::new(self.rename_expr(right)),
                id: *id,
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
    use crate::ir::syntax::builder::build_ir;
    use crate::{document::CheapString, dop::Type};
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

    #[test]
    fn should_rename_deeply_nested_string_concat() {
        // Create a deeply nested left-leaning StringConcat tree
        let depth = 100;
        let mut expr = IrExpr::StringLiteral {
            value: CheapString::new("start".to_string()),
            id: 0,
        };
        for i in 0..depth {
            expr = IrExpr::StringConcat {
                left: Box::new(expr),
                right: Box::new(IrExpr::StringLiteral {
                    value: CheapString::new(format!("{}", i)),
                    id: (i + 1) as u32,
                }),
                id: (i + 1000) as u32,
            };
        }

        let mut pass = AlphaRenamingPass::new();
        let _result = pass.rename_expr(&expr);
        // If we get here without stack overflow, the test passes
    }
}
