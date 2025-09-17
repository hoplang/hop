use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::cased_string::CasedString;
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrModule, IrStatement};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum LanguageMode {
    JavaScript,
    TypeScript,
}

/// Transpiles an IR module to JavaScript or TypeScript code
pub struct JsTranspiler {
    indent_level: usize,
    mode: LanguageMode,
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
}

impl JsTranspiler {
    pub fn new(mode: LanguageMode) -> Self {
        Self {
            indent_level: 0,
            mode,
            use_template_literals: false,
        }
    }

    pub fn transpile_module_with_escape(
        &mut self,
        ir_module: &IrModule,
        needs_escape_html: bool,
    ) -> String {
        let mut output = String::new();

        if needs_escape_html {
            // Add the escape HTML helper function
            match self.mode {
                LanguageMode::JavaScript => {
                    self.write_line(&mut output, "function escapeHtml(str) {");
                    self.indent();
                    self.write_line(&mut output, "if (typeof str !== 'string') return str;");
                }
                LanguageMode::TypeScript => {
                    self.write_line(&mut output, "function escapeHtml(str: string): string {");
                    self.indent();
                }
            }
            self.write_line(&mut output, "return str");
            self.indent();
            self.write_line(&mut output, ".replace(/&/g, '&amp;')");
            self.write_line(&mut output, ".replace(/</g, '&lt;')");
            self.write_line(&mut output, ".replace(/>/g, '&gt;')");
            self.write_line(&mut output, ".replace(/\"/g, '&quot;')");
            self.write_line(&mut output, ".replace(/'/g, '&#39;');");
            self.dedent();
            self.dedent();
            self.write_line(&mut output, "}");
            self.write_line(&mut output, "");
        }

        // Start export default object
        self.write_line(&mut output, "export default {");
        self.indent();

        // Transpile each entrypoint as a function property
        let mut first = true;
        for (name, entrypoint) in &ir_module.entry_points {
            if !first {
                output.push_str(",\n");
            }
            first = false;

            self.transpile_entrypoint(&mut output, name, entrypoint);
        }

        // Close the export default object
        self.write_line(&mut output, "");
        self.dedent();
        self.write_line(&mut output, "}");

        output
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

    fn write_line(&mut self, output: &mut String, line: &str) {
        if !line.is_empty() {
            for _ in 0..self.indent_level {
                output.push_str("    ");
            }
        }
        output.push_str(line);
        output.push('\n');
    }
}

impl Transpiler for JsTranspiler {
    fn transpile_entrypoint(&mut self, output: &mut String, name: &str, entrypoint: &IrEntrypoint) {
        let camel_case_name = CasedString::from_kebab_case(name).to_camel_case();

        for _ in 0..self.indent_level {
            output.push_str("    ");
        }
        output.push_str(&camel_case_name);
        output.push_str(": (");

        if !entrypoint.parameters.is_empty() {
            output.push_str("{ ");
            // Build parameter list
            let params: Vec<String> = entrypoint
                .parameters
                .iter()
                .map(|(name, _)| name.to_string())
                .collect();

            output.push_str(&params.join(", "));
            output.push_str(" }");

            // Generate TypeScript interface for parameters
            if matches!(self.mode, LanguageMode::TypeScript) {
                output.push_str(": { ");
                let type_params: Vec<String> = entrypoint
                    .parameters
                    .iter()
                    .map(|(name, ty)| {
                        let mut type_str = String::new();
                        self.transpile_type(&mut type_str, ty);
                        format!("{}: {}", name, type_str)
                    })
                    .collect();
                output.push_str(&type_params.join(", "));
                output.push_str(" }");
            }
        }
        output.push(')');
        if matches!(self.mode, LanguageMode::TypeScript) {
            output.push_str(": string");
        }
        output.push_str(" => {\n");

        self.indent();
        match self.mode {
            LanguageMode::JavaScript => self.write_line(output, "let output = \"\";"),
            LanguageMode::TypeScript => self.write_line(output, "let output: string = \"\";"),
        }

        // Transpile the body
        self.transpile_statements(output, &entrypoint.body);

        self.write_line(output, "return output;");
        self.dedent();

        // Write closing brace with proper indentation
        for _ in 0..self.indent_level {
            output.push_str("    ");
        }
        output.push('}');
    }

    fn transpile_module(&mut self, ir_module: &IrModule) -> String {
        // Default to enabling escape_html for backward compatibility
        self.transpile_module_with_escape(ir_module, true)
    }
}

impl StatementTranspiler for JsTranspiler {
    fn transpile_write(&mut self, output: &mut String, content: &str) {
        let quoted = self.quote_string(content);
        self.write_line(output, &format!("output += {};", quoted));
    }

    fn transpile_write_expr(&mut self, output: &mut String, expr: &IrExpr, escape: bool) {
        let mut js_expr = String::new();
        self.transpile_expr(&mut js_expr, expr);
        if escape {
            self.write_line(output, &format!("output += escapeHtml({});", js_expr));
        } else {
            self.write_line(output, &format!("output += {};", js_expr));
        }
    }

    fn transpile_if(&mut self, output: &mut String, condition: &IrExpr, body: &[IrStatement]) {
        let mut js_cond = String::new();
        self.transpile_expr(&mut js_cond, condition);
        self.write_line(output, &format!("if ({}) {{", js_cond));
        self.indent();
        self.transpile_statements(output, body);
        self.dedent();
        self.write_line(output, "}");
    }

    fn transpile_for(
        &mut self,
        output: &mut String,
        var: &str,
        array: &IrExpr,
        body: &[IrStatement],
    ) {
        let mut js_array = String::new();
        self.transpile_expr(&mut js_array, array);
        self.write_line(output, &format!("for (const {} of {}) {{", var, js_array));
        self.indent();
        self.transpile_statements(output, body);
        self.dedent();
        self.write_line(output, "}");
    }

    fn transpile_let(
        &mut self,
        output: &mut String,
        var: &str,
        value: &IrExpr,
        body: &[IrStatement],
    ) {
        let mut js_value = String::new();
        self.transpile_expr(&mut js_value, value);
        self.write_line(output, &format!("const {} = {};", var, js_value));
        self.transpile_statements(output, body);
    }
}

impl ExpressionTranspiler for JsTranspiler {
    fn transpile_var(&self, output: &mut String, name: &str) {
        output.push_str(name);
    }

    fn transpile_property_access(&self, output: &mut String, object: &IrExpr, property: &str) {
        self.transpile_expr(output, object);
        output.push('.');
        output.push_str(property);
    }

    fn transpile_string_literal(&self, output: &mut String, value: &str) {
        let quoted = self.quote_string(value);
        output.push_str(&quoted);
    }

    fn transpile_boolean_literal(&self, output: &mut String, value: bool) {
        output.push_str(&value.to_string());
    }

    fn transpile_number_literal(&self, output: &mut String, value: &serde_json::Number) {
        output.push_str(&value.to_string());
    }

    fn transpile_array_literal(&self, output: &mut String, elements: &[IrExpr], _elem_type: &Type) {
        output.push('[');
        let mut first = true;
        for elem in elements {
            if !first {
                output.push_str(", ");
            }
            first = false;
            self.transpile_expr(output, elem);
        }
        output.push(']');
    }

    fn transpile_object_literal(
        &self,
        output: &mut String,
        properties: &[(String, IrExpr)],
        _field_types: &BTreeMap<String, Type>,
    ) {
        output.push('{');
        let mut first = true;
        for (key, value) in properties {
            if !first {
                output.push_str(", ");
            }
            first = false;
            output.push_str(key);
            output.push_str(": ");
            self.transpile_expr(output, value);
        }
        output.push('}');
    }

    fn transpile_string_equality(&self, output: &mut String, left: &IrExpr, right: &IrExpr) {
        output.push('(');
        self.transpile_expr(output, left);
        output.push_str(" === ");
        self.transpile_expr(output, right);
        output.push(')');
    }

    fn transpile_bool_equality(&self, output: &mut String, left: &IrExpr, right: &IrExpr) {
        output.push('(');
        self.transpile_expr(output, left);
        output.push_str(" === ");
        self.transpile_expr(output, right);
        output.push(')');
    }

    fn transpile_not(&self, output: &mut String, operand: &IrExpr) {
        output.push_str("!(");
        self.transpile_expr(output, operand);
        output.push(')');
    }

    fn transpile_json_encode(&self, output: &mut String, value: &IrExpr) {
        output.push_str("JSON.stringify(");
        self.transpile_expr(output, value);
        output.push(')');
    }
}

impl TypeTranspiler for JsTranspiler {
    fn transpile_bool_type(&self, output: &mut String) {
        output.push_str("boolean");
    }

    fn transpile_string_type(&self, output: &mut String) {
        output.push_str("string");
    }

    fn transpile_number_type(&self, output: &mut String) {
        output.push_str("number");
    }

    fn transpile_array_type(&self, output: &mut String, element_type: Option<&Type>) {
        match element_type {
            Some(elem) => {
                self.transpile_type(output, elem);
                output.push_str("[]");
            }
            None => output.push_str("unknown[]"),
        }
    }

    fn transpile_object_type(&self, output: &mut String, fields: &BTreeMap<String, Type>) {
        output.push_str("{ ");
        let mut first = true;
        for (name, ty) in fields {
            if !first {
                output.push_str(", ");
            }
            first = false;
            output.push_str(name);
            output.push_str(": ");
            self.transpile_type(output, ty);
        }
        output.push_str(" }");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};
    use std::collections::BTreeMap;

    fn transpile_ir_to_js(ir_module: &IrModule) -> String {
        let mut transpiler = JsTranspiler::new(LanguageMode::JavaScript);
        transpiler.transpile_module_with_escape(ir_module, true) // needs_escape_html = true for tests
    }

    fn transpile_ir_to_ts(ir_module: &IrModule) -> String {
        let mut transpiler = JsTranspiler::new(LanguageMode::TypeScript);
        transpiler.transpile_module_with_escape(ir_module, true) // needs_escape_html = true for tests
    }

    fn check(ir_module: &IrModule, expected: Expect) {
        let js = transpile_ir_to_js(ir_module);
        expected.assert_eq(&js);
    }

    fn check_ts_output(ir_module: &IrModule, expected: Expect) {
        let ts = transpile_ir_to_ts(ir_module);
        expected.assert_eq(&ts);
    }

    #[test]
    fn test_simple_component() {
        let t = IrTestBuilder::new(vec![]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            t.build(vec![t.write("<div>Hello World</div>\n")]),
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
        let t = IrTestBuilder::new(vec![
            ("name".to_string(), Type::String),
            ("message".to_string(), Type::String),
        ]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-greeting-comp".to_string(),
            t.build(vec![
                t.write("<h1>Hello "),
                t.write_expr(t.var("name"), true),
                t.write(", "),
                t.write_expr(t.var("message"), true),
                t.write("</h1>\n"),
            ]),
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
        let t = IrTestBuilder::new(vec![("show".to_string(), Type::Bool)]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            t.build(vec![
                t.if_stmt(t.var("show"), vec![t.write("<div>Visible</div>\n")]),
            ]),
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
        let t = IrTestBuilder::new(vec![(
            "items".to_string(),
            Type::Array(Some(Box::new(Type::String))),
        )]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            t.build(vec![t.for_loop("item", t.var("items"), |t| {
                vec![
                    t.write("<li>"),
                    t.write_expr(t.var("item"), true),
                    t.write("</li>\n"),
                ]
            })]),
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
        let t = IrTestBuilder::new(vec![]);

        let mut ir_module = IrModule::new();
        // Note: In the IR, nested components are already inlined, so we simulate the result
        ir_module.entry_points.insert(
            "test-main-comp".to_string(),
            t.build(vec![
                t.write("<div data-hop-id=\"test/card-comp\">"),
                t.let_stmt("title", t.str("Hello World"), |t| {
                    vec![
                        t.write("<h2>"),
                        t.write_expr(t.var("title"), true),
                        t.write("</h2>"),
                    ]
                }),
                t.write("</div>"),
            ]),
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
        let t = IrTestBuilder::new(vec![
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
        ]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-user-list".to_string(),
            t.build(vec![
                t.write("<div>\n"),
                t.write("<h1>\n"),
                t.write_expr(t.var("title"), true),
                t.write("</h1>\n"),
                t.write("<ul>\n"),
                t.for_loop("user", t.var("users"), |t| {
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
                    ]
                }),
                t.write("</ul>\n"),
                t.write("</div>\n"),
            ]),
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

    #[test]
    fn test_complex_literals_and_property_access() {
        let t = IrTestBuilder::new(vec![]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-product-list".to_string(),
            t.build(vec![
                    t.write("<div class=\"products\">\n"),
                    t.for_loop(
                        "product",
                        t.array(vec![
                            t.object(vec![
                                ("name", t.str("Laptop")),
                                ("inStock", t.bool(true)),
                                ("category", t.str("electronics")),
                            ]),
                            t.object(vec![
                                ("name", t.str("Book")),
                                ("inStock", t.bool(false)),
                                ("category", t.str("books")),
                            ]),
                            t.object(vec![
                                ("name", t.str("T-Shirt")),
                                ("inStock", t.bool(true)),
                                ("category", t.str("clothing")),
                            ]),
                        ]),
                        |t| vec![t.let_stmt(
                            "displayInfo",
                            t.object(vec![
                                ("currency", t.str("$")),
                                ("showStock", t.bool(true)),
                                ("prefix", t.str("PROD-")),
                            ]),
                            |t| vec![
                                t.write("<article class=\"product\">\n"),
                                t.write("<h3>"),
                                t.write_expr(t.prop_access(t.var("displayInfo"), "prefix"), false),
                                t.write_expr(t.prop_access(t.var("product"), "name"), true),
                                t.write("</h3>\n"),
                                t.write("</p>\n"),
                                t.write("<p>Category: "),
                                t.write_expr(t.prop_access(t.var("product"), "category"), true),
                                t.write("</p>\n"),
                                t.if_stmt(
                                    t.prop_access(t.var("displayInfo"), "showStock"),
                                    vec![
                                        t.if_stmt(
                                            t.prop_access(t.var("product"), "inStock"),
                                            vec![t.write("<span class=\"in-stock\">✓ In Stock</span>\n")],
                                        ),
                                        t.if_stmt(
                                            t.not(t.prop_access(t.var("product"), "inStock")),
                                            vec![t.write("<span class=\"out-of-stock\">✗ Out of Stock</span>\n")],
                                        ),
                                    ],
                                ),
                                t.write("</article>\n"),
                            ],
                        )],
                    ),
                    t.write("</div>\n"),
                ],
            ),
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
                    testProductList: () => {
                        let output = "";
                        output += "<div class=\"products\">\n";
                        for (const product of [{name: "Laptop", inStock: true, category: "electronics"}, {name: "Book", inStock: false, category: "books"}, {name: "T-Shirt", inStock: true, category: "clothing"}]) {
                            const displayInfo = {currency: "$", showStock: true, prefix: "PROD-"};
                            output += "<article class=\"product\">\n";
                            output += "<h3>";
                            output += displayInfo.prefix;
                            output += escapeHtml(product.name);
                            output += "</h3>\n";
                            output += "</p>\n";
                            output += "<p>Category: ";
                            output += escapeHtml(product.category);
                            output += "</p>\n";
                            if (displayInfo.showStock) {
                                if (product.inStock) {
                                    output += "<span class=\"in-stock\">✓ In Stock</span>\n";
                                }
                                if (!(product.inStock)) {
                                    output += "<span class=\"out-of-stock\">✗ Out of Stock</span>\n";
                                }
                            }
                            output += "</article>\n";
                        }
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }
}
