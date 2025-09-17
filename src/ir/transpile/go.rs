use super::{Doc, ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::cased_string::CasedString;
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrModule, IrStatement};
use std::collections::{BTreeMap, BTreeSet};

/// Transpiles an IR module to Go code
pub struct GoTranspiler {
    /// Track packages we need to import
    imports: BTreeSet<String>,
}

impl GoTranspiler {
    pub fn new() -> Self {
        Self {
            imports: BTreeSet::new(),
        }
    }

    fn scan_for_imports(&mut self, entrypoint: &IrEntrypoint) {
        // Use visitor pattern to scan statements for imports
        for stmt in &entrypoint.body {
            // Check for HTML escaping in WriteExpr statements
            stmt.visit(&mut |s| {
                if let IrStatement::WriteExpr { expr, escape: true, .. } = s {
                    self.imports.insert("html".to_string());
                    // Check if we need fmt for type conversion
                    match expr.typ() {
                        Type::String => {}
                        _ => {
                            self.imports.insert("fmt".to_string());
                        }
                    }
                } else if let IrStatement::WriteExpr { expr, escape: false, .. } = s {
                    // Check if we need fmt for type conversion
                    match expr.typ() {
                        Type::String => {}
                        _ => {
                            self.imports.insert("fmt".to_string());
                        }
                    }
                }
            });

            // Check expressions for other imports (like json)
            stmt.visit_exprs(&mut |expr| {
                if let IrExpr::JsonEncode { .. } = expr {
                    self.imports.insert("json".to_string());
                }
            });
        }
    }

    // Helper method to escape strings for Go string literals
    fn escape_string(&self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }


}

impl Transpiler for GoTranspiler {
    fn transpile_entrypoint(&mut self, doc: &mut Doc, name: &str, entrypoint: &IrEntrypoint) {
        // Convert kebab-case to PascalCase for Go function name
        let func_name = CasedString::from_kebab_case(name).to_pascal_case();

        if entrypoint.parameters.is_empty() {
            doc.write_line(&format!("func {}() string {{", func_name));
        } else {
            let struct_name = format!("{}Params", func_name);
            doc.write_line(
                &format!("func {}(params {}) string {{", func_name, struct_name),
            );

            // Extract parameters into local variables
            doc.indent();
            for (param_name, _) in &entrypoint.parameters {
                let field_name =
                    CasedString::from_snake_case(param_name.as_str()).to_pascal_case();
                doc.write_line(&format!("{} := params.{}", param_name, field_name));
            }
            doc.dedent();
        }

        doc.indent();

        doc.write_line("var output strings.Builder");

        self.transpile_statements(doc, &entrypoint.body);

        doc.write_line("return output.String()");
        doc.dedent();
        doc.write_line("}");
    }

    fn transpile_module(&mut self, ir_module: &IrModule) -> String {
        let mut doc = Doc::new_with_tabs();

        // Reset state
        self.imports.clear();

        // First pass: scan to determine what imports we need
        for entrypoint in ir_module.entry_points.values() {
            self.scan_for_imports(entrypoint);
        }

        // We always need strings.Builder for output
        self.imports.insert("strings".to_string());

        // Write package declaration
        doc.write_line("package components");
        doc.write_line("");

        // Write imports if needed
        if !self.imports.is_empty() {
            doc.write_line("import (");
            doc.indent();
            let imports: Vec<_> = self.imports.iter().cloned().collect();
            for import in imports {
                if import == "json" {
                    doc.write_line("\"encoding/json\"");
                } else {
                    doc.write_line(&format!("\"{}\"", import));
                }
            }
            doc.dedent();
            doc.write_line(")");
            doc.write_line("");
        }

        // Add JSON helper if needed
        if self.imports.contains("json") {
            doc.write_line("func mustJSONMarshal(v any) string {");
            doc.indent();
            doc.write_line("data, _ := json.Marshal(v)");
            doc.write_line("return string(data)");
            doc.dedent();
            doc.write_line("}");
            doc.write_line("");
        }

        // Generate parameter structs for entrypoints that have parameters
        for (name, entrypoint) in &ir_module.entry_points {
            if !entrypoint.parameters.is_empty() {
                let struct_name = format!(
                    "{}Params",
                    CasedString::from_kebab_case(name).to_pascal_case()
                );
                doc.write_line(&format!("type {} struct {{", struct_name));
                doc.indent();
                for (param_name, param_type) in &entrypoint.parameters {
                    let mut type_doc = Doc::new_with_tabs();
                    self.transpile_type(&mut type_doc, param_type);
                    let field_name =
                        CasedString::from_snake_case(param_name.as_str()).to_pascal_case();
                    doc.write_line(
                        &format!("{} {} `json:\"{}\"`", field_name, type_doc.as_str(), param_name),
                    );
                }
                doc.dedent();
                doc.write_line("}");
                doc.write_line("");
            }
        }

        // Transpile each entrypoint as a function
        for (name, entrypoint) in &ir_module.entry_points {
            self.transpile_entrypoint(&mut doc, name, entrypoint);
            doc.write_line("");
        }

        doc.into_string()
    }
}

impl StatementTranspiler for GoTranspiler {
    fn transpile_write(&mut self, doc: &mut Doc, content: &str) {
        doc.write_indent();
        let escaped = self.escape_string(content);
        doc.write(&format!("output.WriteString(\"{}\")\n", escaped));
    }

    fn transpile_write_expr(&mut self, doc: &mut Doc, expr: &IrExpr, escape: bool) {
        doc.write_indent();
        let mut expr_doc = Doc::new_with_tabs();
        self.transpile_expr(&mut expr_doc, expr);

        // Handle type conversion to string based on expression type
        let string_expr = match expr.typ() {
            Type::String => expr_doc.as_str().to_string(),
            Type::Number => {
                format!("fmt.Sprintf(\"%v\", {})", expr_doc.as_str())
            }
            Type::Bool => {
                format!("fmt.Sprintf(\"%v\", {})", expr_doc.as_str())
            }
            _ => {
                format!("fmt.Sprintf(\"%v\", {})", expr_doc.as_str())
            }
        };

        if escape {
            doc.write(&format!(
                "output.WriteString(html.EscapeString({}))\n",
                string_expr
            ));
        } else {
            doc.write(&format!("output.WriteString({})\n", string_expr));
        }
    }

    fn transpile_if(&mut self, doc: &mut Doc, condition: &IrExpr, body: &[IrStatement]) {
        doc.write_indent();
        doc.write("if ");
        self.transpile_expr(doc, condition);
        doc.write(" {\n");
        doc.indent();
        self.transpile_statements(doc, body);
        doc.dedent();
        doc.write_indent();
        doc.write("}\n");
    }

    fn transpile_for(
        &mut self,
        doc: &mut Doc,
        var: &str,
        array: &IrExpr,
        body: &[IrStatement],
    ) {
        doc.write_indent();
        doc.write(&format!("for _, {} := range ", var));
        self.transpile_expr(doc, array);
        doc.write(" {\n");
        doc.indent();
        self.transpile_statements(doc, body);
        doc.dedent();
        doc.write_indent();
        doc.write("}\n");
    }

    fn transpile_let(
        &mut self,
        doc: &mut Doc,
        var: &str,
        value: &IrExpr,
        body: &[IrStatement],
    ) {
        doc.write_indent();
        doc.write(&format!("{} := ", var));
        self.transpile_expr(doc, value);
        doc.write("\n");
        self.transpile_statements(doc, body);
    }
}

impl ExpressionTranspiler for GoTranspiler {
    fn transpile_var(&self, doc: &mut Doc, name: &str) {
        doc.write(name);
    }

    fn transpile_property_access(&self, doc: &mut Doc, object: &IrExpr, property: &str) {
        self.transpile_expr(doc, object);
        doc.write(".");
        // Go struct field access with PascalCase field names
        let prop_name = CasedString::from_snake_case(property).to_pascal_case();
        doc.write(&prop_name);
    }

    fn transpile_string_literal(&self, doc: &mut Doc, value: &str) {
        doc.write("\"");
        doc.write(&self.escape_string(value));
        doc.write("\"");
    }

    fn transpile_boolean_literal(&self, doc: &mut Doc, value: bool) {
        doc.write(&value.to_string());
    }

    fn transpile_number_literal(&self, doc: &mut Doc, value: &serde_json::Number) {
        // Go requires explicit float notation for decimals
        if let Some(f) = value.as_f64() {
            if f.fract() == 0.0 {
                doc.write(&format!("{}", f as i64));
            } else {
                doc.write(&format!("{}", f));
            }
        } else {
            doc.write(&value.to_string());
        }
    }

    fn transpile_array_literal(&self, doc: &mut Doc, elements: &[IrExpr], elem_type: &Type) {
        match elem_type {
            Type::Array(Some(inner_type)) => {
                doc.write("[]");
                self.transpile_type(doc, inner_type);
            }
            _ => doc.write("[]any"),
        }
        doc.write("{");
        let mut first = true;
        for elem in elements {
            if !first {
                doc.write(", ");
            }
            first = false;
            self.transpile_expr(doc, elem);
        }
        doc.write("}");
    }

    fn transpile_object_literal(
        &self,
        doc: &mut Doc,
        properties: &[(String, IrExpr)],
        field_types: &BTreeMap<String, Type>,
    ) {
        // Build the struct type definition from the field types
        doc.write("struct{");
        for (field_name, field_type) in field_types {
            let go_field = CasedString::from_snake_case(field_name.as_str()).to_pascal_case();
            doc.write(&go_field);
            doc.write(" ");
            self.transpile_type(doc, field_type);
            doc.write(&format!(" `json:\"{}\"`; ", field_name));
        }
        doc.write("}{");

        // Build the struct literal values
        let mut first = true;
        for (key, value) in properties {
            if !first {
                doc.write(", ");
            }
            first = false;
            let field_name = CasedString::from_snake_case(key).to_pascal_case();
            doc.write(&field_name);
            doc.write(": ");
            self.transpile_expr(doc, value);
        }
        doc.write("}");
    }

    fn transpile_string_equality(&self, doc: &mut Doc, left: &IrExpr, right: &IrExpr) {
        doc.write("(");
        self.transpile_expr(doc, left);
        doc.write(" == ");
        self.transpile_expr(doc, right);
        doc.write(")");
    }

    fn transpile_bool_equality(&self, doc: &mut Doc, left: &IrExpr, right: &IrExpr) {
        doc.write("(");
        self.transpile_expr(doc, left);
        doc.write(" == ");
        self.transpile_expr(doc, right);
        doc.write(")");
    }

    fn transpile_not(&self, doc: &mut Doc, operand: &IrExpr) {
        doc.write("!(");
        self.transpile_expr(doc, operand);
        doc.write(")");
    }

    fn transpile_json_encode(&self, doc: &mut Doc, value: &IrExpr) {
        doc.write("mustJSONMarshal(");
        self.transpile_expr(doc, value);
        doc.write(")");
    }
}

impl TypeTranspiler for GoTranspiler {
    fn transpile_bool_type(&self, doc: &mut Doc) {
        doc.write("bool");
    }

    fn transpile_string_type(&self, doc: &mut Doc) {
        doc.write("string");
    }

    fn transpile_number_type(&self, doc: &mut Doc) {
        doc.write("float64");
    }

    fn transpile_array_type(&self, doc: &mut Doc, element_type: Option<&Type>) {
        match element_type {
            Some(elem) => {
                doc.write("[]");
                self.transpile_type(doc, elem);
            }
            None => doc.write("[]any"),
        }
    }

    fn transpile_object_type(&self, doc: &mut Doc, fields: &BTreeMap<String, Type>) {
        // Generate anonymous struct type
        doc.write("struct{");
        for (field_name, field_type) in fields {
            let go_field = CasedString::from_snake_case(field_name).to_pascal_case();
            doc.write(&go_field);
            doc.write(" ");
            self.transpile_type(doc, field_type);
            doc.write(&format!(" `json:\"{}\"`; ", field_name));
        }
        doc.write("}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};
    use std::collections::BTreeMap;

    fn transpile_ir_to_go(ir_module: &IrModule) -> String {
        let mut transpiler = GoTranspiler::new();
        transpiler.transpile_module(ir_module)
    }

    fn check(ir_module: &IrModule, expected: Expect) {
        let go_code = transpile_ir_to_go(ir_module);
        expected.assert_eq(&go_code);
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
                package components

                import (
                	"strings"
                )

                func TestMainComp() string {
                	var output strings.Builder
                	output.WriteString("<div>Hello World</div>\n")
                	return output.String()
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
                package components

                import (
                	"html"
                	"strings"
                )

                type TestGreetingCompParams struct {
                	Name string `json:"name"`
                	Message string `json:"message"`
                }

                func TestGreetingComp(params TestGreetingCompParams) string {
                	name := params.Name
                	message := params.Message
                	var output strings.Builder
                	output.WriteString("<h1>Hello ")
                	output.WriteString(html.EscapeString(name))
                	output.WriteString(", ")
                	output.WriteString(html.EscapeString(message))
                	output.WriteString("</h1>\n")
                	return output.String()
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
                package components

                import (
                	"strings"
                )

                type TestMainCompParams struct {
                	Show bool `json:"show"`
                }

                func TestMainComp(params TestMainCompParams) string {
                	show := params.Show
                	var output strings.Builder
                	if show {
                		output.WriteString("<div>Visible</div>\n")
                	}
                	return output.String()
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
                package components

                import (
                	"html"
                	"strings"
                )

                type TestMainCompParams struct {
                	Items []string `json:"items"`
                }

                func TestMainComp(params TestMainCompParams) string {
                	items := params.Items
                	var output strings.Builder
                	for _, item := range items {
                		output.WriteString("<li>")
                		output.WriteString(html.EscapeString(item))
                		output.WriteString("</li>\n")
                	}
                	return output.String()
                }

            "#]],
        );
    }

    #[test]
    fn test_object_literals() {
        let t = IrTestBuilder::new(vec![]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-objects".to_string(),
            t.build(vec![t.let_stmt(
                "person",
                t.object(vec![
                    ("name", t.str("Alice")),
                    ("age", t.str("30.0")),
                    ("active", t.bool(true)),
                ]),
                |t| {
                    vec![
                        t.write_expr(t.prop_access(t.var("person"), "name"), true),
                        t.write(" is "),
                        t.write_expr(t.prop_access(t.var("person"), "age"), false),
                        t.write(" years old"),
                    ]
                },
            )]),
        );

        check(
            &ir_module,
            expect![[r#"
                package components

                import (
                	"html"
                	"strings"
                )

                func TestObjects() string {
                	var output strings.Builder
                	person := struct{Active bool `json:"active"`; Age string `json:"age"`; Name string `json:"name"`; }{Name: "Alice", Age: "30.0", Active: true}
                	output.WriteString(html.EscapeString(person.Name))
                	output.WriteString(" is ")
                	output.WriteString(person.Age)
                	output.WriteString(" years old")
                	return output.String()
                }

            "#]],
        );
    }

    #[test]
    fn test_nested_arrays_and_objects() {
        let t = IrTestBuilder::new(vec![(
            "users".to_string(),
            Type::Array(Some(Box::new(Type::Object({
                let mut map = BTreeMap::new();
                map.insert("name".to_string(), Type::String);
                map.insert("id".to_string(), Type::String);
                map
            })))),
        )]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-nested".to_string(),
            t.build(vec![t.for_loop("user", t.var("users"), |t| {
                vec![
                    t.write("<div>"),
                    t.write_expr(t.prop_access(t.var("user"), "name"), true),
                    t.write(" (ID: "),
                    t.write_expr(t.prop_access(t.var("user"), "id"), false),
                    t.write(")</div>\n"),
                ]
            })]),
        );

        check(
            &ir_module,
            expect![[r#"
                package components

                import (
                	"html"
                	"strings"
                )

                type TestNestedParams struct {
                	Users []struct{Id string `json:"id"`; Name string `json:"name"`; } `json:"users"`
                }

                func TestNested(params TestNestedParams) string {
                	users := params.Users
                	var output strings.Builder
                	for _, user := range users {
                		output.WriteString("<div>")
                		output.WriteString(html.EscapeString(user.Name))
                		output.WriteString(" (ID: ")
                		output.WriteString(user.Id)
                		output.WriteString(")</div>\n")
                	}
                	return output.String()
                }

            "#]],
        );
    }

    #[test]
    fn test_loop_over_array_literal() {
        let t = IrTestBuilder::new(vec![]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-array-literal-loop".to_string(),
            t.build(vec![
                t.write("<ul>\n"),
                t.for_loop(
                    "color",
                    t.array(vec![t.str("red"), t.str("green"), t.str("blue")]),
                    |t| {
                        vec![
                            t.write("<li>"),
                            t.write_expr(t.var("color"), true),
                            t.write("</li>\n"),
                        ]
                    },
                ),
                t.write("</ul>\n"),
            ]),
        );

        check(
            &ir_module,
            expect![[r#"
                package components

                import (
                	"html"
                	"strings"
                )

                func TestArrayLiteralLoop() string {
                	var output strings.Builder
                	output.WriteString("<ul>\n")
                	for _, color := range []string{"red", "green", "blue"} {
                		output.WriteString("<li>")
                		output.WriteString(html.EscapeString(color))
                		output.WriteString("</li>\n")
                	}
                	output.WriteString("</ul>\n")
                	return output.String()
                }

            "#]],
        );
    }

    #[test]
    fn test_loop_over_object_array_literal() {
        let t = IrTestBuilder::new(vec![]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-products".to_string(),
            t.build(vec![t.for_loop(
                "product",
                t.array(vec![
                    t.object(vec![
                        ("name", t.str("Laptop")),
                        ("price", t.str("999.99")),
                        ("in_stock", t.bool(true)),
                    ]),
                    t.object(vec![
                        ("name", t.str("Mouse")),
                        ("price", t.str("29.99")),
                        ("in_stock", t.bool(false)),
                    ]),
                ]),
                |t| {
                    vec![
                        t.write("<div class=\"product\">\n"),
                        t.write("<h3>"),
                        t.write_expr(t.prop_access(t.var("product"), "name"), true),
                        t.write("</h3>\n"),
                        t.write("<p>$"),
                        t.write_expr(t.prop_access(t.var("product"), "price"), false),
                        t.write("</p>\n"),
                        t.if_stmt(
                            t.prop_access(t.var("product"), "in_stock"),
                            vec![t.write("<span class=\"available\">In Stock</span>\n")],
                        ),
                        t.if_stmt(
                            t.not(t.prop_access(t.var("product"), "in_stock")),
                            vec![t.write("<span class=\"sold-out\">Sold Out</span>\n")],
                        ),
                        t.write("</div>\n"),
                    ]
                },
            )]),
        );

        check(
            &ir_module,
            expect![[r#"
                package components

                import (
                	"html"
                	"strings"
                )

                func TestProducts() string {
                	var output strings.Builder
                	for _, product := range []struct{InStock bool `json:"in_stock"`; Name string `json:"name"`; Price string `json:"price"`; }{struct{InStock bool `json:"in_stock"`; Name string `json:"name"`; Price string `json:"price"`; }{Name: "Laptop", Price: "999.99", InStock: true}, struct{InStock bool `json:"in_stock"`; Name string `json:"name"`; Price string `json:"price"`; }{Name: "Mouse", Price: "29.99", InStock: false}} {
                		output.WriteString("<div class=\"product\">\n")
                		output.WriteString("<h3>")
                		output.WriteString(html.EscapeString(product.Name))
                		output.WriteString("</h3>\n")
                		output.WriteString("<p>$")
                		output.WriteString(product.Price)
                		output.WriteString("</p>\n")
                		if product.InStock {
                			output.WriteString("<span class=\"available\">In Stock</span>\n")
                		}
                		if !(product.InStock) {
                			output.WriteString("<span class=\"sold-out\">Sold Out</span>\n")
                		}
                		output.WriteString("</div>\n")
                	}
                	return output.String()
                }

            "#]],
        );
    }

    #[test]
    fn test_string_comparison() {
        let t = IrTestBuilder::new(vec![
            ("user_role".to_string(), Type::String),
            ("expected_role".to_string(), Type::String),
        ]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-auth-check".to_string(),
            t.build(vec![
                t.if_stmt(
                    t.eq(t.var("user_role"), t.var("expected_role")),
                    vec![t.write("<div>Access granted</div>\n")],
                ),
                t.if_stmt(
                    t.eq(t.var("user_role"), t.str("admin")),
                    vec![t.write("<div>Admin panel available</div>\n")],
                ),
            ]),
        );

        check(
            &ir_module,
            expect![[r#"
                package components

                import (
                	"strings"
                )

                type TestAuthCheckParams struct {
                	UserRole string `json:"user_role"`
                	ExpectedRole string `json:"expected_role"`
                }

                func TestAuthCheck(params TestAuthCheckParams) string {
                	user_role := params.UserRole
                	expected_role := params.ExpectedRole
                	var output strings.Builder
                	if (user_role == expected_role) {
                		output.WriteString("<div>Access granted</div>\n")
                	}
                	if (user_role == "admin") {
                		output.WriteString("<div>Admin panel available</div>\n")
                	}
                	return output.String()
                }

            "#]],
        );
    }

    #[test]
    fn test_json_encode() {
        let t = IrTestBuilder::new(vec![
            (
                "data".to_string(),
                Type::Object({
                    let mut map = BTreeMap::new();
                    map.insert("title".to_string(), Type::String);
                    map.insert("count".to_string(), Type::Number);
                    map.insert("active".to_string(), Type::Bool);
                    map
                }),
            ),
            (
                "items".to_string(),
                Type::Array(Some(Box::new(Type::String))),
            ),
        ]);

        let mut ir_module = IrModule::new();
        ir_module.entry_points.insert(
            "test-json".to_string(),
            t.build(vec![
                t.write("<script>\n"),
                t.write("const data = "),
                t.write_expr(t.json_encode(t.var("data")), false),
                t.write(";\n"),
                t.write("const items = "),
                t.write_expr(t.json_encode(t.var("items")), false),
                t.write(";\n"),
                t.write("const config = "),
                t.write_expr(
                    t.json_encode(t.object(vec![("debug", t.bool(true)), ("version", t.num(1.5))])),
                    false,
                ),
                t.write(";\n"),
                t.write("</script>\n"),
            ]),
        );

        check(
            &ir_module,
            expect![[r#"
                package components

                import (
                	"encoding/json"
                	"strings"
                )

                func mustJSONMarshal(v any) string {
                	data, _ := json.Marshal(v)
                	return string(data)
                }

                type TestJsonParams struct {
                	Data struct{Active bool `json:"active"`; Count float64 `json:"count"`; Title string `json:"title"`; } `json:"data"`
                	Items []string `json:"items"`
                }

                func TestJson(params TestJsonParams) string {
                	data := params.Data
                	items := params.Items
                	var output strings.Builder
                	output.WriteString("<script>\n")
                	output.WriteString("const data = ")
                	output.WriteString(mustJSONMarshal(data))
                	output.WriteString(";\n")
                	output.WriteString("const items = ")
                	output.WriteString(mustJSONMarshal(items))
                	output.WriteString(";\n")
                	output.WriteString("const config = ")
                	output.WriteString(mustJSONMarshal(struct{Debug bool `json:"debug"`; Version float64 `json:"version"`; }{Debug: true, Version: 1.5}))
                	output.WriteString(";\n")
                	output.WriteString("</script>\n")
                	return output.String()
                }

            "#]],
        );
    }
}
