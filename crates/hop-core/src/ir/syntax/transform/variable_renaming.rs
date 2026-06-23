use std::collections::HashSet;

use crate::expr::patterns::Match;
use crate::ir::ast::{IrComponentDeclaration, IrExpr, IrForSource, IrStatement, IrViewDeclaration};
use crate::symbols::var_name::VarName;

// Reserved keywords across all target languages (sorted for binary search)
const RESERVED_KEYWORDS: &[&str] = &[
    "and",
    "as",
    "assert",
    "await",
    "break",
    "case",
    "catch",
    "chan",
    "class",
    "const",
    "continue",
    "debugger",
    "def",
    "default",
    "defer",
    "del",
    "delete",
    "do",
    "elif",
    "else",
    "except",
    "export",
    "extends",
    "fallthrough",
    "false",
    "finally",
    "for",
    "from",
    "func",
    "function",
    "global",
    "go",
    "goto",
    "if",
    "import",
    "in",
    "instanceof",
    "interface",
    "is",
    "lambda",
    "let",
    "map",
    "new",
    "nonlocal",
    "not",
    "null",
    "or",
    "package",
    "pass",
    "raise",
    "range",
    "return",
    "select",
    "static",
    "struct",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "type",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "yield",
];

/// Variable renaming pass for the IR AST.
///
/// This pass ensures:
/// 1. All variable names are unique to avoid conflicts and shadowing
/// 2. Variables with names that are reserved keywords in target languages
///    (TypeScript, Rust) are renamed to avoid invalid generated code
pub struct VariableRenamingPass {
    /// Counter for generating unique variable names
    var_counter: usize,
    /// Stack of scopes mapping original names to renamed names
    scope_stack: Vec<Vec<(VarName, VarName)>>,
    /// Track all variable names ever used to ensure uniqueness
    all_used_names: HashSet<VarName>,
}

impl Default for VariableRenamingPass {
    fn default() -> Self {
        Self::new()
    }
}

impl VariableRenamingPass {
    pub fn new() -> Self {
        debug_assert!(
            RESERVED_KEYWORDS.windows(2).all(|w| w[0] < w[1]),
            "RESERVED_KEYWORDS must be sorted and have no duplicates"
        );
        Self {
            var_counter: 0,
            scope_stack: vec![Vec::new()],
            all_used_names: HashSet::new(),
        }
    }

    /// Rename variables in a list of statements (in place)
    fn rename_statements(&mut self, statements: &mut Vec<IrStatement>) {
        for stmt in statements {
            self.rename_statement(stmt);
        }
    }

    /// Rename variables in a single statement (in place)
    fn rename_statement(&mut self, statement: &mut IrStatement) {
        match statement {
            IrStatement::Write { .. } => {}

            IrStatement::ComponentInvocation { args, .. } => {
                for arg in args {
                    self.rename_expr(&mut arg.expr);
                }
            }

            IrStatement::WriteExpr { expr, .. } => {
                self.rename_expr(expr);
            }

            IrStatement::If {
                condition, body, ..
            } => {
                self.rename_expr(condition);
                self.push_scope();
                self.rename_statements(body);
                self.pop_scope();
            }

            IrStatement::For {
                var, source, body, ..
            } => {
                match source {
                    IrForSource::Array(array) => self.rename_expr(array),
                    IrForSource::RangeInclusive { start, end } => {
                        self.rename_expr(start);
                        self.rename_expr(end);
                    }
                }
                self.push_scope();
                if let Some(v) = var {
                    *v = self.bind_var(v);
                }
                self.rename_statements(body);
                self.pop_scope();
            }

            IrStatement::Let {
                var, value, body, ..
            } => {
                self.rename_expr(value);
                self.push_scope();
                *var = self.bind_var(var);
                self.rename_statements(body);
                self.pop_scope();
            }

            IrStatement::LetFragment {
                var,
                fragment_body,
                body,
                ..
            } => {
                // fragment_body is evaluated in the outer scope (does NOT see var)
                self.rename_statements(fragment_body);
                self.push_scope();
                *var = self.bind_var(var);
                self.rename_statements(body);
                self.pop_scope();
            }

            IrStatement::LetRecordDestructure {
                subject,
                bindings,
                body,
                ..
            } => {
                self.rename_expr(subject);
                self.push_scope();
                for (_, var) in bindings.iter_mut() {
                    *var = self.bind_var(var);
                }
                self.rename_statements(body);
                self.pop_scope();
            }

            IrStatement::Match { match_, .. } => {
                self.rename_expr(match_.subject_mut());
                match match_ {
                    Match::Bool {
                        true_body,
                        false_body,
                        ..
                    } => {
                        self.push_scope();
                        self.rename_statements(true_body);
                        self.pop_scope();
                        self.push_scope();
                        self.rename_statements(false_body);
                        self.pop_scope();
                    }
                    Match::Option {
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                        ..
                    } => {
                        self.push_scope();
                        if let Some(var) = some_arm_binding {
                            *var = self.bind_var(var);
                        }
                        self.rename_statements(some_arm_body);
                        self.pop_scope();
                        self.push_scope();
                        self.rename_statements(none_arm_body);
                        self.pop_scope();
                    }
                    Match::Enum { arms, .. } => {
                        for arm in arms {
                            self.push_scope();
                            for (_, var) in &mut arm.bindings {
                                *var = self.bind_var(var);
                            }
                            self.rename_statements(&mut arm.body);
                            self.pop_scope();
                        }
                    }
                }
            }
        }
    }

    /// Rename variables in an expression (in place)
    fn rename_expr(&mut self, expr: &mut IrExpr) {
        match expr {
            IrExpr::Var { value, .. } => {
                *value = self.lookup_var(value);
            }
            IrExpr::FieldAccess { record, .. } => {
                self.rename_expr(record);
            }
            IrExpr::BooleanNegation { operand, .. } | IrExpr::NumericNegation { operand, .. } => {
                self.rename_expr(operand);
            }
            IrExpr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.rename_expr(elem);
                }
            }
            IrExpr::RecordLiteral { fields, .. } => {
                for (_, field_expr) in fields {
                    self.rename_expr(field_expr);
                }
            }
            IrExpr::StringConcat { left, right, .. }
            | IrExpr::Equals { left, right, .. }
            | IrExpr::LessThan { left, right, .. }
            | IrExpr::LessThanOrEqual { left, right, .. }
            | IrExpr::BooleanLogicalAnd { left, right, .. }
            | IrExpr::BooleanLogicalOr { left, right, .. }
            | IrExpr::NumericAdd { left, right, .. }
            | IrExpr::NumericSubtract { left, right, .. }
            | IrExpr::NumericMultiply { left, right, .. } => {
                self.rename_expr(left);
                self.rename_expr(right);
            }
            // Literals don't contain variables - nothing to do
            IrExpr::StringLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. } => {}
            IrExpr::EnumLiteral { fields, .. } => {
                for (_, field_expr) in fields {
                    self.rename_expr(field_expr);
                }
            }
            IrExpr::Match { match_, .. } => {
                self.rename_expr(match_.subject_mut());
                match match_ {
                    Match::Enum { arms, .. } => {
                        for arm in arms {
                            self.push_scope();
                            for (_, var) in &mut arm.bindings {
                                *var = self.bind_var(var);
                            }
                            self.rename_expr(&mut arm.body);
                            self.pop_scope();
                        }
                    }
                    Match::Bool {
                        true_body,
                        false_body,
                        ..
                    } => {
                        self.rename_expr(true_body);
                        self.rename_expr(false_body);
                    }
                    Match::Option {
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                        ..
                    } => {
                        self.push_scope();
                        if let Some(var) = some_arm_binding {
                            *var = self.bind_var(var);
                        }
                        self.rename_expr(some_arm_body);
                        self.pop_scope();
                        self.rename_expr(none_arm_body);
                    }
                }
            }
            IrExpr::Let {
                var_name,
                value,
                body,
                ..
            } => {
                self.rename_expr(value);
                self.push_scope();
                *var_name = self.bind_var(var_name);
                self.rename_expr(body);
                self.pop_scope();
            }
            IrExpr::LetRecordDestructure {
                subject,
                bindings,
                body,
                ..
            } => {
                self.rename_expr(subject);
                self.push_scope();
                for (_, var) in bindings.iter_mut() {
                    *var = self.bind_var(var);
                }
                self.rename_expr(body);
                self.pop_scope();
            }
            IrExpr::OptionLiteral { value, .. } => {
                if let Some(inner) = value {
                    self.rename_expr(inner);
                }
            }
            IrExpr::ArrayLength { array, .. } => {
                self.rename_expr(array);
            }
            IrExpr::ArrayIsEmpty { array, .. } => {
                self.rename_expr(array);
            }
            IrExpr::StringIsEmpty { string, .. } => {
                self.rename_expr(string);
            }
            IrExpr::OptionIsSome { option, .. } => {
                self.rename_expr(option);
            }
            IrExpr::OptionIsNone { option, .. } => {
                self.rename_expr(option);
            }
            IrExpr::TwMerge { operand, .. } => {
                self.rename_expr(operand);
            }
            IrExpr::IntToString { value, .. }
            | IrExpr::FloatToInt { value, .. }
            | IrExpr::IntToFloat { value, .. } => {
                self.rename_expr(value);
            }
            IrExpr::FragmentEmpty { .. } => {
                // Leaf constant with no variables to rename
            }
        }
    }

    /// Push a new scope onto the stack
    fn push_scope(&mut self) {
        self.scope_stack.push(Vec::new());
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

    /// Check if a name is a reserved keyword in any target language
    fn is_reserved_keyword(name: &str) -> bool {
        RESERVED_KEYWORDS.binary_search(&name).is_ok()
    }

    /// Bind a variable in the current scope, renaming if necessary
    fn bind_var(&mut self, name: &VarName) -> VarName {
        // Check if this name would shadow an existing binding, has been used before,
        // or is a reserved keyword in a target language
        let needs_renaming = self.is_name_in_scope(name)
            || self.all_used_names.contains(name)
            || Self::is_reserved_keyword(name.as_str());

        let renamed = if needs_renaming {
            self.fresh_var(name)
        } else {
            name.clone()
        };

        // Track this name as used
        self.all_used_names.insert(renamed.clone());

        self.scope_stack
            .last_mut()
            .expect("Scope stack should not be empty")
            .push((name.clone(), renamed.clone()));

        renamed
    }

    /// Check if a name exists in any parent scope (not the current one)
    fn is_name_in_scope(&self, name: &VarName) -> bool {
        // Check if name exists in any parent scope (not the current one)
        for scope in self.scope_stack.iter().rev().skip(1) {
            if scope.iter().any(|(k, _)| k == name) {
                return true;
            }
        }
        false
    }

    /// Look up the renamed version of a variable
    fn lookup_var(&self, name: &VarName) -> VarName {
        for scope in self.scope_stack.iter().rev() {
            if let Some((_, renamed)) = scope.iter().find(|(k, _)| k == name) {
                return renamed.clone();
            }
        }
        // If we can't find the variable, it must be a parameter or external reference
        // Return it unchanged
        name.clone()
    }

    pub fn run(comp_decl: &mut IrViewDeclaration) {
        let mut pass = VariableRenamingPass::new();

        pass.push_scope();

        // Create renamed parameter mappings
        let mut renamed_params = Vec::new();
        for param in &comp_decl.parameters {
            let renamed = pass.bind_var(&param.name);
            renamed_params.push(renamed);
        }

        // Rename the body in place
        pass.rename_statements(&mut comp_decl.body);

        pass.pop_scope();

        // Create Let bindings for parameters if they were renamed
        for (renamed, param) in renamed_params.into_iter().zip(&comp_decl.parameters).rev() {
            if renamed != param.name {
                comp_decl.body = vec![IrStatement::Let {
                    id: 0, // ID will be assigned later if needed
                    var: renamed,
                    value: IrExpr::Var {
                        value: param.name.clone(),
                        kind: param.typ.clone(),
                        id: 0,
                    },
                    body: std::mem::take(&mut comp_decl.body),
                }];
            }
        }
    }

    pub fn run_component(comp_decl: &mut IrComponentDeclaration) {
        let mut pass = VariableRenamingPass::new();

        pass.push_scope();

        let mut renamed_params = Vec::new();
        for param in &comp_decl.parameters {
            let renamed = pass.bind_var(&param.name);
            renamed_params.push(renamed);
        }

        pass.rename_statements(&mut comp_decl.body);

        pass.pop_scope();

        for (renamed, param) in renamed_params.into_iter().zip(&comp_decl.parameters).rev() {
            if renamed != param.name {
                comp_decl.body = vec![IrStatement::Let {
                    id: 0,
                    var: renamed,
                    value: IrExpr::Var {
                        value: param.name.clone(),
                        kind: param.typ.clone(),
                        id: 0,
                    },
                    body: std::mem::take(&mut comp_decl.body),
                }];
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::ir::syntax::builder::{build_ir, build_ir_no_params};
    use crate::{document::CheapString, expr::Type};
    use expect_test::{Expect, expect};

    fn check(mut input_view: IrViewDeclaration, expected: Expect) {
        let before = input_view.to_string();
        VariableRenamingPass::run(&mut input_view);
        let after = input_view.to_string();
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
                view Test(x: String) {
                  write_escaped(x)
                }

                -- after --
                view Test(x: String) {
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
                view Test(x: String) {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                }

                -- after --
                view Test(x: String) {
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
            build_ir_no_params("Test", |t| {
                t.for_loop("x", t.array(vec![t.str("a")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.for_loop("x", t.array(vec![t.str("b")]), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  for x in ["a"] {
                    write_escaped(x)
                  }
                  for x in ["b"] {
                    write_escaped(x)
                  }
                }

                -- after --
                view Test() {
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
            build_ir_no_params("Test", |t| {
                t.let_stmt("x", t.str("hello"), |t| {
                    t.write_expr_escaped(t.var("x"));
                    t.let_stmt("x", t.str("world"), |t| {
                        t.write_expr_escaped(t.var("x"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let x = "hello" in {
                    write_escaped(x)
                    let x = "world" in {
                      write_escaped(x)
                    }
                  }
                }

                -- after --
                view Test() {
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
                view Test(x: String, y: String) {
                  write_escaped(x)
                  for y in ["a"] {
                    write_escaped(x)
                    write_escaped(y)
                  }
                }

                -- after --
                view Test(x: String, y: String) {
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
            build_ir_no_params("Test", |t| {
                t.let_stmt("x", t.str("first"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
                t.let_stmt("x", t.str("second"), |t| {
                    t.write_expr_escaped(t.var("x"));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let x = "first" in {
                    write_escaped(x)
                  }
                  let x = "second" in {
                    write_escaped(x)
                  }
                }

                -- after --
                view Test() {
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
                vec![("items", Type::Array(Arc::new(Type::String)))],
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
                view Test(items: Array[String]) {
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
                view Test(items: Array[String]) {
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
                    id: i + 1,
                }),
                id: i + 1000,
            };
        }

        let mut pass = VariableRenamingPass::new();
        pass.rename_expr(&mut expr);
        // If we get here without stack overflow, the test passes
    }

    #[test]
    fn should_rename_reserved_keywords() {
        // Test that reserved keywords from target languages are renamed
        check(
            build_ir_no_params("Test", |t| {
                // `delete` is reserved in TypeScript
                t.let_stmt("delete", t.str("value"), |t| {
                    t.write_expr_escaped(t.var("delete"));
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let delete = "value" in {
                    write_escaped(delete)
                  }
                }

                -- after --
                view Test() {
                  let delete_1 = "value" in {
                    write_escaped(delete_1)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_rename_multiple_reserved_keywords() {
        // Test multiple reserved keywords from different target languages
        check(
            build_ir_no_params("Test", |t| {
                // `class` is reserved in TypeScript
                t.let_stmt("class", t.str("ts"), |t| {
                    t.write_expr_escaped(t.var("class"));
                    // `const` is reserved in TypeScript and Rust
                    t.let_stmt("const", t.str("rust"), |t| {
                        t.write_expr_escaped(t.var("const"));
                    });
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let class = "ts" in {
                    write_escaped(class)
                    let const = "rust" in {
                      write_escaped(const)
                    }
                  }
                }

                -- after --
                view Test() {
                  let class_1 = "ts" in {
                    write_escaped(class_1)
                    let const_2 = "rust" in {
                      write_escaped(const_2)
                    }
                  }
                }
            "#]],
        );
    }
}
