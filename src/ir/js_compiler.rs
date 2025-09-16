use super::ast::{BinaryOp, IrEntrypoint, IrExpr, IrExprValue, IrModule, IrStatement, UnaryOp};
use crate::dop::r#type::Type;

#[derive(Debug, Clone, Copy)]
pub enum LanguageMode {
    JavaScript,
    TypeScript,
}

/// Compiles an IR module to JavaScript or TypeScript code
pub struct JsCompiler {
    output: String,
    indent_level: usize,
    mode: LanguageMode,
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
}

impl JsCompiler {
    pub fn new(mode: LanguageMode) -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            mode,
            use_template_literals: false,
        }
    }

    /// Convert kebab-case to camelCase
    /// e.g., "my-component-name" -> "myComponentName"
    fn kebab_to_camel_case(name: &str) -> String {
        let mut result = String::new();
        let mut capitalize_next = false;

        for ch in name.chars() {
            if ch == '-' {
                capitalize_next = true;
            } else if capitalize_next {
                result.push(ch.to_ascii_uppercase());
                capitalize_next = false;
            } else {
                result.push(ch);
            }
        }

        result
    }

    pub fn compile_module(&mut self, ir_module: &IrModule, needs_escape_html: bool) -> String {
        if needs_escape_html {
            // Add the escape HTML helper function
            match self.mode {
                LanguageMode::JavaScript => {
                    self.write_line("function escapeHtml(str) {");
                    self.indent();
                    self.write_line("if (typeof str !== 'string') return str;");
                }
                LanguageMode::TypeScript => {
                    self.write_line("function escapeHtml(str: string): string {");
                    self.indent();
                }
            }
            self.write_line("return str");
            self.indent();
            self.write_line(".replace(/&/g, '&amp;')");
            self.write_line(".replace(/</g, '&lt;')");
            self.write_line(".replace(/>/g, '&gt;')");
            self.write_line(".replace(/\"/g, '&quot;')");
            self.write_line(".replace(/'/g, '&#39;');");
            self.dedent();
            self.dedent();
            self.write_line("}");
            self.write_line("");
        }

        // Start export default object
        self.write_line("export default {");
        self.indent();

        // Compile each entrypoint as a function property
        let mut first = true;
        for (name, entrypoint) in &ir_module.entry_points {
            if !first {
                self.output.push_str(",\n");
            }
            first = false;

            self.compile_entrypoint(name, entrypoint);
        }

        // Close the export default object
        self.write_line("");
        self.dedent();
        self.write_line("}");

        self.output.clone()
    }

    fn compile_entrypoint(&mut self, name: &str, entrypoint: &IrEntrypoint) {
        // Convert kebab-case to camelCase for JavaScript property name
        let camel_case_name = Self::kebab_to_camel_case(name);

        // Write the function property with proper indentation
        for _ in 0..self.indent_level {
            self.output.push_str("    ");
        }
        self.output.push_str(&camel_case_name);
        self.output.push_str(": ");

        if entrypoint.parameters.is_empty() {
            match self.mode {
                LanguageMode::JavaScript => {
                    self.output.push_str("() => {");
                }
                LanguageMode::TypeScript => {
                    self.output.push_str("(): string => {");
                }
            }
        } else {
            // Build parameter list
            let params: Vec<String> = entrypoint
                .parameters
                .iter()
                .map(|(name, _)| name.clone())
                .collect();

            match self.mode {
                LanguageMode::JavaScript => {
                    // Destructure parameters from input object
                    let params_str = params.join(", ");
                    self.output
                        .push_str(&format!("({{ {} }}) => {{", params_str));
                }
                LanguageMode::TypeScript => {
                    // Generate TypeScript interface for parameters
                    let type_params: Vec<String> = entrypoint
                        .parameters
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name, Self::type_to_typescript(ty)))
                        .collect();
                    let params_str = params.join(", ");
                    let type_params_str = type_params.join(", ");

                    self.output.push_str(&format!(
                        "({{ {} }}: {{ {} }}): string => {{",
                        params_str, type_params_str
                    ));
                }
            }
        }
        self.output.push('\n');

        self.indent();
        match self.mode {
            LanguageMode::JavaScript => self.write_line("let output = \"\";"),
            LanguageMode::TypeScript => self.write_line("let output: string = \"\";"),
        }

        // Compile the body
        self.compile_statements(&entrypoint.body);

        self.write_line("return output;");
        self.dedent();

        // Write closing brace with proper indentation
        for _ in 0..self.indent_level {
            self.output.push_str("    ");
        }
        self.output.push('}');
    }

    fn type_to_typescript(ty: &Type) -> String {
        match ty {
            Type::Bool => "boolean".to_string(),
            Type::String => "string".to_string(),
            Type::Number => "number".to_string(),
            Type::Array(elem) => match elem {
                Some(elem_type) => format!("{}[]", Self::type_to_typescript(elem_type)),
                None => "unknown[]".to_string(),
            },
            Type::Object(fields) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, Self::type_to_typescript(ty)))
                    .collect();
                format!("{{ {} }}", field_strs.join(", "))
            }
        }
    }

    fn compile_statements(&mut self, statements: &[IrStatement]) {
        for node in statements {
            self.compile_statement(node);
        }
    }

    fn compile_statement(&mut self, statement: &IrStatement) {
        match statement {
            IrStatement::Write { id: _, content } => {
                let quoted = self.quote_string(content);
                self.write_line(&format!("output += {};", quoted));
            }

            IrStatement::WriteExpr {
                id: _,
                expr,
                escape,
            } => {
                let js_expr = self.compile_expr(expr);
                if *escape {
                    self.write_line(&format!("output += escapeHtml({});", js_expr));
                } else {
                    self.write_line(&format!("output += {};", js_expr));
                }
            }

            IrStatement::If {
                id: _,
                condition,
                body,
            } => {
                let js_cond = self.compile_expr(condition);
                self.write_line(&format!("if ({}) {{", js_cond));
                self.indent();
                self.compile_statements(body);
                self.dedent();
                self.write_line("}");
            }

            IrStatement::For {
                id: _,
                var,
                array,
                body,
            } => {
                let js_array = self.compile_expr(array);
                self.write_line(&format!("for (const {} of {}) {{", var, js_array));
                self.indent();
                self.compile_statements(body);
                self.dedent();
                self.write_line("}");
            }

            IrStatement::Let {
                id: _,
                var,
                value,
                body,
            } => {
                let js_value = self.compile_expr(value);
                self.write_line(&format!("const {} = {};", var, js_value));
                self.compile_statements(body);
            }
        }
    }

    fn compile_expr(&self, expr: &IrExpr) -> String {
        match &expr.value {
            IrExprValue::Var(name) => name.clone(),

            IrExprValue::PropertyAccess { object, property } => {
                let obj = self.compile_expr(object);
                format!("{}.{}", obj, property)
            }

            IrExprValue::String(value) => self.quote_string(value),

            IrExprValue::Boolean(value) => value.to_string(),

            IrExprValue::Number(value) => value.to_string(),

            IrExprValue::Array(elements) => {
                let items: Vec<String> = elements.iter().map(|e| self.compile_expr(e)).collect();
                format!("[{}]", items.join(", "))
            }

            IrExprValue::Object(properties) => {
                let props: Vec<String> = properties
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, self.compile_expr(value)))
                    .collect();
                format!("{{{}}}", props.join(", "))
            }

            IrExprValue::BinaryOp { left, op, right } => {
                let l = self.compile_expr(left);
                let r = self.compile_expr(right);
                match op {
                    BinaryOp::Eq => format!("({} === {})", l, r),
                }
            }

            IrExprValue::UnaryOp { op, operand } => {
                let compiled_op = self.compile_expr(operand);
                match op {
                    UnaryOp::Not => format!("!({})", compiled_op),
                }
            }

            IrExprValue::JsonEncode { value } => {
                let compiled_value = self.compile_expr(value);
                format!("JSON.stringify({})", compiled_value)
            }
        }
    }

    // Helper method to escape strings for JavaScript literals
    fn escape_string(&self, s: &str) -> String {
        if self.use_template_literals {
            // For template literals, only escape backticks and ${
            s.replace('\\', "\\\\")
                .replace('`', "\\`")
                .replace("${", "\\${")
        } else {
            // For regular strings, escape double quotes and common escape sequences
            s.replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n")
                .replace('\r', "\\r")
                .replace('\t', "\\t")
        }
    }

    // Helper method to wrap a string in the appropriate quotes
    fn quote_string(&self, s: &str) -> String {
        if self.use_template_literals {
            format!("`{}`", self.escape_string(s))
        } else {
            format!("\"{}\"", self.escape_string(s))
        }
    }

    // Helper methods for indentation
    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    fn write_line(&mut self, line: &str) {
        if !line.is_empty() {
            for _ in 0..self.indent_level {
                self.output.push_str("    ");
            }
        }
        self.output.push_str(line);
        self.output.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};
    use std::collections::BTreeMap;

    fn compile_ir_to_js(ir_module: &IrModule) -> String {
        let mut compiler = JsCompiler::new(LanguageMode::JavaScript);
        compiler.compile_module(ir_module, true) // needs_escape_html = true for tests
    }

    fn compile_ir_to_ts(ir_module: &IrModule) -> String {
        let mut compiler = JsCompiler::new(LanguageMode::TypeScript);
        compiler.compile_module(ir_module, true) // needs_escape_html = true for tests
    }

    fn check(ir_module: &IrModule, expected: Expect) {
        let js = compile_ir_to_js(ir_module);
        expected.assert_eq(&js);
    }

    fn check_ts_output(ir_module: &IrModule, expected: Expect) {
        let ts = compile_ir_to_ts(ir_module);
        expected.assert_eq(&ts);
    }

    #[test]
    fn test_simple_component() {
        let t = IrTestBuilder::new();

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            IrEntrypoint {
                parameters: vec![],
                body: vec![t.write("<div>Hello World</div>\n")],
            },
        );

        check(
            &ir_module,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: () => {
                        let output = "";
                        output += "<div>Hello World</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_component_with_parameters() {
        let t = IrTestBuilder::new();

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-greeting-comp".to_string(),
            IrEntrypoint {
                parameters: vec![
                    ("name".to_string(), Type::String),
                    ("message".to_string(), Type::String),
                ],
                body: vec![
                    t.write("<h1>Hello "),
                    t.write_expr(t.var("name"), true),
                    t.write(", "),
                    t.write_expr(t.var("message"), true),
                    t.write("</h1>\n"),
                ],
            },
        );

        check(
            &ir_module,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testGreetingComp: ({ name, message }) => {
                        let output = "";
                        output += "<h1>Hello ";
                        output += escapeHtml(name);
                        output += ", ";
                        output += escapeHtml(message);
                        output += "</h1>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_if_condition() {
        let t = IrTestBuilder::new();

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            IrEntrypoint {
                parameters: vec![("show".to_string(), Type::Bool)],
                body: vec![t.if_stmt(t.var("show"), vec![t.write("<div>Visible</div>\n")])],
            },
        );

        check(
            &ir_module,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: ({ show }) => {
                        let output = "";
                        if (show) {
                            output += "<div>Visible</div>\n";
                        }
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_for_loop() {
        let t = IrTestBuilder::new();

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            IrEntrypoint {
                parameters: vec![(
                    "items".to_string(),
                    Type::Array(Some(Box::new(Type::String))),
                )],
                body: vec![t.for_loop(
                    "item",
                    t.var("items"),
                    vec![
                        t.write("<li>"),
                        t.write_expr(t.var("item"), true),
                        t.write("</li>\n"),
                    ],
                )],
            },
        );

        check(
            &ir_module,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: ({ items }) => {
                        let output = "";
                        for (const item of items) {
                            output += "<li>";
                            output += escapeHtml(item);
                            output += "</li>\n";
                        }
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_components_with_let_bindings() {
        let t = IrTestBuilder::new();

        let mut ir_module = IrModule::new();
        // Note: In the IR, nested components are already inlined, so we simulate the result
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            IrEntrypoint {
                parameters: vec![],
                body: vec![
                    t.write("<div data-hop-id=\"test/card-comp\">"),
                    t.let_stmt(
                        "title",
                        t.str("Hello World"),
                        vec![
                            t.write("<h2>"),
                            t.write_expr(t.var("title"), true),
                            t.write("</h2>"),
                        ],
                    ),
                    t.write("</div>"),
                ],
            },
        );

        check(
            &ir_module,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: () => {
                        let output = "";
                        output += "<div data-hop-id=\"test/card-comp\">";
                        const title = "Hello World";
                        output += "<h2>";
                        output += escapeHtml(title);
                        output += "</h2>";
                        output += "</div>";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_typescript_with_parameters() {
        let t = IrTestBuilder::new();

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-user-list".to_string(),
            IrEntrypoint {
                parameters: vec![
                    (
                        "users".to_string(),
                        Type::Array(Some(Box::new(Type::Object({
                            let mut map = BTreeMap::new();
                            map.insert("name".to_string(), Type::String);
                            map.insert("id".to_string(), Type::String);
                            map.insert("active".to_string(), Type::Bool);
                            map
                        })))),
                    ),
                    ("title".to_string(), Type::String),
                ],
                body: vec![
                    t.write("<div>\n"),
                    t.write("<h1>\n"),
                    t.write_expr(t.var("title"), true),
                    t.write("</h1>\n"),
                    t.write("<ul>\n"),
                    t.for_loop(
                        "user",
                        t.var("users"),
                        vec![
                            t.write("\n"),
                            t.if_stmt(
                                t.prop_access(t.var("user"), "active"),
                                vec![
                                    t.write("\n<li>User "),
                                    t.write_expr(t.prop_access(t.var("user"), "id"), true),
                                    t.write(": "),
                                    t.write_expr(t.prop_access(t.var("user"), "name"), true),
                                    t.write("</li>\n"),
                                ],
                            ),
                            t.write("\n"),
                        ],
                    ),
                    t.write("</ul>\n"),
                    t.write("</div>\n"),
                ],
            },
        );

        check_ts_output(
            &ir_module,
            expect![[r#"
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testUserList: ({ users, title }: { users: { active: boolean, id: string, name: string }[], title: string }): string => {
                        let output: string = "";
                        output += "<div>\n";
                        output += "<h1>\n";
                        output += escapeHtml(title);
                        output += "</h1>\n";
                        output += "<ul>\n";
                        for (const user of users) {
                            output += "\n";
                            if (user.active) {
                                output += "\n<li>User ";
                                output += escapeHtml(user.id);
                                output += ": ";
                                output += escapeHtml(user.name);
                                output += "</li>\n";
                            }
                            output += "\n";
                        }
                        output += "</ul>\n";
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }
}
