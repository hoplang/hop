use super::{Doc, ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
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
    mode: LanguageMode,
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
}

impl JsTranspiler {
    pub fn new(mode: LanguageMode) -> Self {
        Self {
            mode,
            use_template_literals: false,
        }
    }

    fn scan_for_escape_html(&self, entrypoint: &IrEntrypoint) -> bool {
        // Use visitor pattern to scan for HTML escaping
        let mut needs_escape = false;
        for stmt in &entrypoint.body {
            stmt.visit(&mut |s| {
                if let IrStatement::WriteExpr { escape: true, .. } = s {
                    needs_escape = true;
                }
            });
            if needs_escape {
                break;
            }
        }
        needs_escape
    }

    fn emit_escape_html_helper(&mut self, doc: &mut Doc) {
        // Add the escape HTML helper function
        match self.mode {
            LanguageMode::JavaScript => {
                doc.write_line("function escapeHtml(str) {");
                doc.indent();
                doc.write_line("if (typeof str !== 'string') return str;");
            }
            LanguageMode::TypeScript => {
                doc.write_line("function escapeHtml(str: string): string {");
                doc.indent();
            }
        }
        doc.write_line("return str");
        doc.indent();
        doc.write_line(".replace(/&/g, '&amp;')");
        doc.write_line(".replace(/</g, '&lt;')");
        doc.write_line(".replace(/>/g, '&gt;')");
        doc.write_line(".replace(/\"/g, '&quot;')");
        doc.write_line(".replace(/'/g, '&#39;');");
        doc.dedent();
        doc.dedent();
        doc.write_line("}");
        doc.write_line("");
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
}

impl Transpiler for JsTranspiler {
    fn transpile_entrypoint(&mut self, doc: &mut Doc, name: &str, entrypoint: &IrEntrypoint) {
        let camel_case_name = CasedString::from_kebab_case(name).to_camel_case();

        doc.write_indent();
        doc.write(&camel_case_name);
        doc.write(": (");

        if !entrypoint.parameters.is_empty() {
            doc.write("{ ");
            // Build parameter list
            let params: Vec<String> = entrypoint
                .parameters
                .iter()
                .map(|(name, _)| name.to_string())
                .collect();

            doc.write(&params.join(", "));
            doc.write(" }");

            // Generate TypeScript interface for parameters
            if matches!(self.mode, LanguageMode::TypeScript) {
                doc.write(": { ");
                let type_params: Vec<String> = entrypoint
                    .parameters
                    .iter()
                    .map(|(name, ty)| {
                        let mut type_doc = Doc::new_with_spaces();
                        self.transpile_type(&mut type_doc, ty);
                        format!("{}: {}", name, type_doc.as_str())
                    })
                    .collect();
                doc.write(&type_params.join(", "));
                doc.write(" }");
            }
        }
        doc.write(")");
        if matches!(self.mode, LanguageMode::TypeScript) {
            doc.write(": string");
        }
        doc.write(" => {\n");

        doc.indent();
        match self.mode {
            LanguageMode::JavaScript => doc.write_line("let output = \"\";"),
            LanguageMode::TypeScript => doc.write_line("let output: string = \"\";"),
        }

        // Transpile the body
        self.transpile_statements(doc, &entrypoint.body);

        doc.write_line("return output;");
        doc.dedent();

        // Write closing brace with proper indentation
        doc.write_indent();
        doc.write("}");
    }

    fn transpile_module(&mut self, ir_module: &IrModule) -> String {
        let mut doc = Doc::new_with_spaces();

        // First pass: scan to determine if we need HTML escaping
        let mut needs_escape_html = false;
        for entrypoint in ir_module.entry_points.values() {
            if self.scan_for_escape_html(entrypoint) {
                needs_escape_html = true;
                break;
            }
        }

        // Add escape HTML helper if needed
        if needs_escape_html {
            self.emit_escape_html_helper(&mut doc);
        }

        // Start export default object
        doc.write_line("export default {");
        doc.indent();

        // Transpile each entrypoint as a function property
        let mut first = true;
        for (name, entrypoint) in &ir_module.entry_points {
            if !first {
                doc.write(",\n");
            }
            first = false;

            self.transpile_entrypoint(&mut doc, name, entrypoint);
        }

        // Close the export default object
        doc.write_line("");
        doc.dedent();
        doc.write_line("}");

        doc.into_string()
    }
}

impl StatementTranspiler for JsTranspiler {
    fn transpile_write(&mut self, doc: &mut Doc, content: &str) {
        let quoted = self.quote_string(content);
        doc.write_line(&format!("output += {};", quoted));
    }

    fn transpile_write_expr(&mut self, doc: &mut Doc, expr: &IrExpr, escape: bool) {
        let mut expr_doc = Doc::new_with_spaces();
        self.transpile_expr(&mut expr_doc, expr);
        if escape {
            doc.write_line(&format!("output += escapeHtml({});", expr_doc.as_str()));
        } else {
            doc.write_line(&format!("output += {};", expr_doc.as_str()));
        }
    }

    fn transpile_if(&mut self, doc: &mut Doc, condition: &IrExpr, body: &[IrStatement]) {
        let mut cond_doc = Doc::new_with_spaces();
        self.transpile_expr(&mut cond_doc, condition);
        doc.write_line(&format!("if ({}) {{", cond_doc.as_str()));
        doc.indent();
        self.transpile_statements(doc, body);
        doc.dedent();
        doc.write_line("}");
    }

    fn transpile_for(&mut self, doc: &mut Doc, var: &str, array: &IrExpr, body: &[IrStatement]) {
        let mut array_doc = Doc::new_with_spaces();
        self.transpile_expr(&mut array_doc, array);
        doc.write_line(&format!("for (const {} of {}) {{", var, array_doc.as_str()));
        doc.indent();
        self.transpile_statements(doc, body);
        doc.dedent();
        doc.write_line("}");
    }

    fn transpile_let(&mut self, doc: &mut Doc, var: &str, value: &IrExpr, body: &[IrStatement]) {
        let mut value_doc = Doc::new_with_spaces();
        self.transpile_expr(&mut value_doc, value);
        doc.write_line(&format!("const {} = {};", var, value_doc.as_str()));
        self.transpile_statements(doc, body);
    }
}

impl ExpressionTranspiler for JsTranspiler {
    fn transpile_var(&self, doc: &mut Doc, name: &str) {
        doc.write(name);
    }

    fn transpile_property_access(&self, doc: &mut Doc, object: &IrExpr, property: &str) {
        self.transpile_expr(doc, object);
        doc.write(".");
        doc.write(property);
    }

    fn transpile_string_literal(&self, doc: &mut Doc, value: &str) {
        let quoted = self.quote_string(value);
        doc.write(&quoted);
    }

    fn transpile_boolean_literal(&self, doc: &mut Doc, value: bool) {
        doc.write(&value.to_string());
    }

    fn transpile_number_literal(&self, doc: &mut Doc, value: &serde_json::Number) {
        doc.write(&value.to_string());
    }

    fn transpile_array_literal(&self, doc: &mut Doc, elements: &[IrExpr], _elem_type: &Type) {
        doc.write("[");
        let mut first = true;
        for elem in elements {
            if !first {
                doc.write(", ");
            }
            first = false;
            self.transpile_expr(doc, elem);
        }
        doc.write("]");
    }

    fn transpile_object_literal(
        &self,
        doc: &mut Doc,
        properties: &[(String, IrExpr)],
        _field_types: &BTreeMap<String, Type>,
    ) {
        doc.write("{");
        let mut first = true;
        for (key, value) in properties {
            if !first {
                doc.write(", ");
            }
            first = false;
            doc.write(key);
            doc.write(": ");
            self.transpile_expr(doc, value);
        }
        doc.write("}");
    }

    fn transpile_string_equality(&self, doc: &mut Doc, left: &IrExpr, right: &IrExpr) {
        doc.write("(");
        self.transpile_expr(doc, left);
        doc.write(" === ");
        self.transpile_expr(doc, right);
        doc.write(")");
    }

    fn transpile_bool_equality(&self, doc: &mut Doc, left: &IrExpr, right: &IrExpr) {
        doc.write("(");
        self.transpile_expr(doc, left);
        doc.write(" === ");
        self.transpile_expr(doc, right);
        doc.write(")");
    }

    fn transpile_not(&self, doc: &mut Doc, operand: &IrExpr) {
        doc.write("!(");
        self.transpile_expr(doc, operand);
        doc.write(")");
    }

    fn transpile_json_encode(&self, doc: &mut Doc, value: &IrExpr) {
        doc.write("JSON.stringify(");
        self.transpile_expr(doc, value);
        doc.write(")");
    }
}

impl TypeTranspiler for JsTranspiler {
    fn transpile_bool_type(&self, doc: &mut Doc) {
        doc.write("boolean");
    }

    fn transpile_string_type(&self, doc: &mut Doc) {
        doc.write("string");
    }

    fn transpile_number_type(&self, doc: &mut Doc) {
        doc.write("number");
    }

    fn transpile_array_type(&self, doc: &mut Doc, element_type: Option<&Type>) {
        match element_type {
            Some(elem) => {
                self.transpile_type(doc, elem);
                doc.write("[]");
            }
            None => doc.write("unknown[]"),
        }
    }

    fn transpile_object_type(&self, doc: &mut Doc, fields: &BTreeMap<String, Type>) {
        doc.write("{ ");
        let mut first = true;
        for (name, ty) in fields {
            if !first {
                doc.write(", ");
            }
            first = false;
            doc.write(name);
            doc.write(": ");
            self.transpile_type(doc, ty);
        }
        doc.write(" }");
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
        transpiler.transpile_module(ir_module)
    }

    fn transpile_ir_to_ts(ir_module: &IrModule) -> String {
        let mut transpiler = JsTranspiler::new(LanguageMode::TypeScript);
        transpiler.transpile_module(ir_module)
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
