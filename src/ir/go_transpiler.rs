use super::ast::{BinaryOp, IrEntrypoint, IrExpr, IrModule, IrStatement, UnaryOp};
use crate::cased_string::CasedString;
use crate::dop::r#type::Type;
use std::collections::BTreeSet;

/// Transpiles an IR module to Go code
pub struct GoTranspiler {
    output: String,
    indent_level: usize,
    /// Track packages we need to import
    imports: BTreeSet<String>,
}

impl GoTranspiler {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            imports: BTreeSet::new(),
        }
    }

    pub fn transpile_module(&mut self, ir_module: &IrModule) -> String {
        // Reset state
        self.imports.clear();

        // First pass: scan to determine what imports we need
        for entrypoint in ir_module.entry_points.values() {
            self.scan_for_imports(entrypoint);
        }

        // We always need strings.Builder for output
        self.imports.insert("strings".to_string());

        // Write package declaration
        self.write_line("package components");
        self.write_line("");

        // Write imports if needed
        if !self.imports.is_empty() {
            self.write_line("import (");
            self.indent();
            let imports: Vec<_> = self.imports.iter().cloned().collect();
            for import in imports {
                if import == "json" {
                    self.write_line("\"encoding/json\"");
                } else {
                    self.write_line(&format!("\"{}\"", import));
                }
            }
            self.dedent();
            self.write_line(")");
            self.write_line("");
        }

        // Add JSON helper if needed
        if self.imports.contains("json") {
            self.write_line("func mustJSONMarshal(v any) string {");
            self.indent();
            self.write_line("data, _ := json.Marshal(v)");
            self.write_line("return string(data)");
            self.dedent();
            self.write_line("}");
            self.write_line("");
        }

        // Generate parameter structs for entrypoints that have parameters
        for (name, entrypoint) in &ir_module.entry_points {
            if !entrypoint.parameters.is_empty() {
                let struct_name = format!(
                    "{}Params",
                    CasedString::from_kebab_case(name).to_pascal_case()
                );
                self.write_line(&format!("type {} struct {{", struct_name));
                self.indent();
                for (param_name, param_type) in &entrypoint.parameters {
                    let go_type = Self::type_to_go(param_type);
                    let field_name =
                        CasedString::from_snake_case(param_name.as_str()).to_pascal_case();
                    self.write_line(&format!(
                        "{} {} `json:\"{}\"`",
                        field_name, go_type, param_name
                    ));
                }
                self.dedent();
                self.write_line("}");
                self.write_line("");
            }
        }

        // Transpile each entrypoint as a function
        for (name, entrypoint) in &ir_module.entry_points {
            self.transpile_entrypoint(name, entrypoint);
            self.write_line("");
        }

        self.output.clone()
    }

    fn scan_for_imports(&mut self, entrypoint: &IrEntrypoint) {
        // Scan statements for what we need to import
        for stmt in &entrypoint.body {
            self.scan_statement_for_imports(stmt);
        }
    }

    fn scan_statement_for_imports(&mut self, stmt: &IrStatement) {
        match stmt {
            IrStatement::WriteExpr { expr, escape, .. } => {
                if *escape {
                    self.imports.insert("html".to_string());
                }
                // Check if we need fmt for type conversion
                match expr.typ() {
                    Type::String => {}
                    _ => {
                        self.imports.insert("fmt".to_string());
                    }
                }
                self.scan_expr_for_imports(expr);
            }
            IrStatement::If {
                condition, body, ..
            } => {
                self.scan_expr_for_imports(condition);
                for s in body {
                    self.scan_statement_for_imports(s);
                }
            }
            IrStatement::For { array, body, .. } => {
                self.scan_expr_for_imports(array);
                for s in body {
                    self.scan_statement_for_imports(s);
                }
            }
            IrStatement::Let { value, body, .. } => {
                self.scan_expr_for_imports(value);
                for s in body {
                    self.scan_statement_for_imports(s);
                }
            }
            _ => {}
        }
    }

    fn scan_expr_for_imports(&mut self, expr: &IrExpr) {
        match expr {
            IrExpr::JsonEncode { .. } => {
                self.imports.insert("json".to_string());
            }
            IrExpr::PropertyAccess { object, .. } => {
                self.scan_expr_for_imports(object);
            }
            IrExpr::BinaryOp { left, right, .. } => {
                self.scan_expr_for_imports(left);
                self.scan_expr_for_imports(right);
            }
            IrExpr::UnaryOp { operand, .. } => {
                self.scan_expr_for_imports(operand);
            }
            IrExpr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.scan_expr_for_imports(elem);
                }
            }
            IrExpr::ObjectLiteral { properties, .. } => {
                for (_, value) in properties {
                    self.scan_expr_for_imports(value);
                }
            }
            _ => {}
        }
    }

    fn transpile_entrypoint(&mut self, name: &str, entrypoint: &IrEntrypoint) {
        // Convert kebab-case to PascalCase for Go function name
        let func_name = CasedString::from_kebab_case(name).to_pascal_case();

        if entrypoint.parameters.is_empty() {
            self.write_line(&format!("func {}() string {{", func_name));
        } else {
            let struct_name = format!("{}Params", func_name);
            self.write_line(&format!(
                "func {}(params {}) string {{",
                func_name, struct_name
            ));

            // Extract parameters into local variables
            self.indent();
            for (param_name, _) in &entrypoint.parameters {
                let field_name = CasedString::from_snake_case(param_name.as_str()).to_pascal_case();
                self.write_line(&format!("{} := params.{}", param_name, field_name));
            }
            self.dedent();
        }

        self.indent();

        self.write_line("var output strings.Builder");

        self.transpile_statements(&entrypoint.body);

        self.write_line("return output.String()");
        self.dedent();
        self.write_line("}");
    }

    fn type_to_go(ty: &Type) -> String {
        match ty {
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Number => "float64".to_string(),
            Type::Array(elem) => match elem {
                Some(elem_type) => format!("[]{}", Self::type_to_go(elem_type)),
                None => "[]any".to_string(),
            },
            Type::Object(fields) => {
                // Generate anonymous struct type
                let mut struct_def = "struct{".to_string();
                for (field_name, field_type) in fields {
                    let go_field = CasedString::from_snake_case(field_name).to_pascal_case();
                    let go_type = Self::type_to_go(field_type);
                    struct_def.push_str(&format!(
                        "{} {} `json:\"{}\"`; ",
                        go_field, go_type, field_name
                    ));
                }
                struct_def.push('}');
                struct_def
            }
        }
    }

    fn transpile_statements(&mut self, statements: &[IrStatement]) {
        for node in statements {
            self.transpile_statement(node);
        }
    }

    fn transpile_statement(&mut self, statement: &IrStatement) {
        match statement {
            IrStatement::Write { id: _, content } => {
                let escaped = self.escape_string(content);
                self.write_line(&format!("output.WriteString(\"{}\")", escaped));
            }

            IrStatement::WriteExpr {
                id: _,
                expr,
                escape,
            } => {
                let go_expr = self.transpile_expr(expr);

                // Handle type conversion to string based on expression type
                let string_expr = match expr.typ() {
                    Type::String => go_expr,
                    Type::Number => {
                        format!("fmt.Sprintf(\"%v\", {})", go_expr)
                    }
                    Type::Bool => {
                        format!("fmt.Sprintf(\"%v\", {})", go_expr)
                    }
                    _ => {
                        format!("fmt.Sprintf(\"%v\", {})", go_expr)
                    }
                };

                if *escape {
                    self.write_line(&format!(
                        "output.WriteString(html.EscapeString({}))",
                        string_expr
                    ));
                } else {
                    self.write_line(&format!("output.WriteString({})", string_expr));
                }
            }

            IrStatement::If {
                id: _,
                condition,
                body,
            } => {
                let go_cond = self.transpile_expr(condition);
                self.write_line(&format!("if {} {{", go_cond));
                self.indent();
                self.transpile_statements(body);
                self.dedent();
                self.write_line("}");
            }

            IrStatement::For {
                id: _,
                var,
                array,
                body,
            } => {
                let go_array = self.transpile_expr(array);
                self.write_line(&format!("for _, {} := range {} {{", var, go_array));
                self.indent();
                self.transpile_statements(body);
                self.dedent();
                self.write_line("}");
            }

            IrStatement::Let {
                id: _,
                var,
                value,
                body,
            } => {
                let go_value = self.transpile_expr(value);
                self.write_line(&format!("{} := {}", var, go_value));
                self.transpile_statements(body);
            }
        }
    }

    fn transpile_expr(&self, expr: &IrExpr) -> String {
        match expr {
            IrExpr::Var { value: name, .. } => name.to_string(),

            IrExpr::PropertyAccess {
                object, property, ..
            } => {
                let obj = self.transpile_expr(object);
                // Go struct field access with PascalCase field names
                let prop_name = CasedString::from_snake_case(property).to_pascal_case();
                format!("{}.{}", obj, prop_name)
            }

            IrExpr::StringLiteral { value, .. } => {
                format!("\"{}\"", self.escape_string(value))
            }

            IrExpr::BooleanLiteral { value, .. } => value.to_string(),

            IrExpr::NumberLiteral { value, .. } => {
                // Go requires explicit float notation for decimals
                if value.fract() == 0.0 {
                    format!("{}", *value as i64)
                } else {
                    format!("{}", value)
                }
            }

            IrExpr::ArrayLiteral { elements, typ, .. } => {
                if elements.is_empty() {
                    // Need type info for empty slices
                    match typ {
                        Type::Array(Some(elem_type)) => {
                            format!("[]{}{{}}", Self::type_to_go(elem_type))
                        }
                        _ => "[]any{}".to_string(),
                    }
                } else {
                    let items: Vec<String> =
                        elements.iter().map(|e| self.transpile_expr(e)).collect();

                    // Infer slice type from expression type
                    let type_prefix = match typ {
                        Type::Array(Some(elem_type)) => {
                            format!("[]{}", Self::type_to_go(elem_type))
                        }
                        _ => "[]any".to_string(),
                    };

                    format!("{}{{{}}}", type_prefix, items.join(", "))
                }
            }

            IrExpr::ObjectLiteral {
                properties, typ, ..
            } => {
                // Generate anonymous struct literal with proper type
                if properties.is_empty() {
                    // Empty struct
                    "struct{}{}".to_string()
                } else {
                    // Build the struct type definition from the expression's type
                    let struct_type = match typ {
                        Type::Object(fields) => {
                            let mut def = "struct{".to_string();
                            for (field_name, field_type) in fields {
                                let go_field =
                                    CasedString::from_snake_case(field_name).to_pascal_case();
                                let go_type = Self::type_to_go(field_type);
                                def.push_str(&format!(
                                    "{} {} `json:\"{}\"`; ",
                                    go_field, go_type, field_name
                                ));
                            }
                            def.push('}');
                            def
                        }
                        _ => unreachable!(), // Shouldn't happen with proper typing
                    };

                    // Build the struct literal values
                    let values: Vec<String> = properties
                        .iter()
                        .map(|(key, value)| {
                            let field_name = CasedString::from_snake_case(key).to_pascal_case();
                            format!("{}: {}", field_name, self.transpile_expr(value))
                        })
                        .collect();

                    format!("{}{{{}}}", struct_type, values.join(", "))
                }
            }

            IrExpr::BinaryOp {
                left,
                operator: op,
                right,
                ..
            } => {
                let l = self.transpile_expr(left);
                let r = self.transpile_expr(right);
                match op {
                    BinaryOp::Eq => format!("({} == {})", l, r),
                }
            }

            IrExpr::UnaryOp {
                operator: op,
                operand,
                ..
            } => {
                let transpiled_op = self.transpile_expr(operand);
                match op {
                    UnaryOp::Not => format!("!({})", transpiled_op),
                }
            }

            IrExpr::JsonEncode { value, .. } => {
                let transpiled_value = self.transpile_expr(value);
                format!("mustJSONMarshal({})", transpiled_value)
            }
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
                self.output.push('\t');
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
