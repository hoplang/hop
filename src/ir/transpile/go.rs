use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::cased_string::CasedString;
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrModule, IrStatement};
use std::collections::{BTreeMap, BTreeSet};

/// Transpiles an IR module to Go code using pretty printing
pub struct GoTranspiler {}

impl GoTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn scan_for_imports(imports: &mut BTreeSet<String>, entrypoint: &IrEntrypoint) {
        // Use visitor pattern to scan statements for imports
        for stmt in &entrypoint.body {
            // Check for HTML escaping in WriteExpr statements
            stmt.visit(&mut |s| {
                if let IrStatement::WriteExpr {
                    expr, escape: true, ..
                } = s
                {
                    imports.insert("html".to_string());
                    // Check if we need fmt for type conversion
                    match expr.typ() {
                        Type::String => {}
                        _ => {
                            imports.insert("fmt".to_string());
                        }
                    }
                } else if let IrStatement::WriteExpr {
                    expr,
                    escape: false,
                    ..
                } = s
                {
                    // Check if we need fmt for type conversion
                    match expr.typ() {
                        Type::String => {}
                        _ => {
                            imports.insert("fmt".to_string());
                        }
                    }
                }
            });

            // Check expressions for other imports (like json)
            stmt.visit_exprs(&mut |expr| {
                if let IrExpr::JsonEncode { .. } = expr {
                    imports.insert("encoding/json".to_string());
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
    fn transpile_module(&self, ir_module: &IrModule) -> String {
        let mut imports = BTreeSet::new();
        // Clone self to get a mutable version for scanning
        let mut scanner = Self::new();

        // First pass: scan to determine what imports we need
        for entrypoint in ir_module.entry_points.values() {
            Self::scan_for_imports(&mut imports, entrypoint);
        }

        // We always need strings.Builder for output
        imports.insert("strings".to_string());

        // Build the module content using BoxDoc
        let mut result = BoxDoc::nil();

        // Write package declaration
        result = result
            .append(BoxDoc::text("package components"))
            .append(BoxDoc::line())
            .append(BoxDoc::line());

        // Write imports if needed
        if !imports.is_empty() {
            result = result
                .append(BoxDoc::text("import ("))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            imports.iter().map(|import| {
                                BoxDoc::nil()
                                    .append(BoxDoc::text("\""))
                                    .append(BoxDoc::text(import.as_str()))
                                    .append(BoxDoc::text("\""))
                            }),
                            BoxDoc::line(),
                        ))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text(")"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add JSON helper if needed
        if imports.contains("encoding/json") {
            result = result
                .append(BoxDoc::text("func mustJSONMarshal(v any) string {"))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::text("data, _ := json.Marshal(v)"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("return string(data)"))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Generate parameter structs for entrypoints that have parameters
        for (name, entrypoint) in &ir_module.entry_points {
            if !entrypoint.parameters.is_empty() {
                let struct_name = format!(
                    "{}Params",
                    CasedString::from_kebab_case(name).to_pascal_case()
                );

                let fields: Vec<_> = entrypoint
                    .parameters
                    .iter()
                    .map(|(param_name, param_type)| {
                        let field_name =
                            CasedString::from_snake_case(param_name.as_str()).to_pascal_case();
                        BoxDoc::text(field_name)
                            .append(BoxDoc::text(" "))
                            .append(self.transpile_type(param_type))
                            .append(BoxDoc::text(" `json:\""))
                            .append(BoxDoc::text(param_name.as_str()))
                            .append(BoxDoc::text("\"`"))
                    })
                    .collect();

                result = result
                    .append(BoxDoc::text("type "))
                    .append(BoxDoc::text(struct_name))
                    .append(BoxDoc::text(" struct {"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(fields, BoxDoc::line()))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }
        }

        // Transpile each entrypoint as a function
        let entrypoints: Vec<_> = ir_module.entry_points.iter().collect();
        for (i, (name, entrypoint)) in entrypoints.iter().enumerate() {
            result = result.append(self.transpile_entrypoint(name, entrypoint));
            if i < entrypoints.len() - 1 {
                result = result.append(BoxDoc::line());
            }
        }

        // Render to string
        let mut buffer = Vec::new();
        result.render(80, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        // Post-process: convert spaces to tabs for Go convention
        let processed = output
            .lines()
            .map(|line| {
                // Count leading spaces
                let spaces = line.chars().take_while(|c| *c == ' ').count();
                let tabs = "\t".repeat(spaces);
                let rest = &line[spaces..];
                format!("{}{}", tabs, rest)
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Ensure file ends with a newline
        if !processed.ends_with('\n') {
            format!("{}\n", processed)
        } else {
            processed
        }
    }

    fn transpile_entrypoint<'a>(&self, name: &'a str, entrypoint: &'a IrEntrypoint) -> BoxDoc<'a> {
        // Convert kebab-case to PascalCase for Go function name
        let func_name = CasedString::from_kebab_case(name).to_pascal_case();

        let mut result = BoxDoc::text("func ").append(BoxDoc::as_string(func_name.clone()));

        if entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::text("() string {"));
        } else {
            let struct_name = format!("{}Params", func_name);
            result = result
                .append(BoxDoc::text("(params "))
                .append(BoxDoc::as_string(struct_name))
                .append(BoxDoc::text(") string {"));
        }

        // Function body
        let mut body = BoxDoc::nil();

        // Extract parameters into local variables
        if !entrypoint.parameters.is_empty() {
            for (param_name, _) in &entrypoint.parameters {
                let field_name = CasedString::from_snake_case(param_name.as_str()).to_pascal_case();
                body = body
                    .append(BoxDoc::text(param_name.as_str()))
                    .append(BoxDoc::text(" := params."))
                    .append(BoxDoc::as_string(field_name))
                    .append(BoxDoc::line());
            }
        }

        body = body
            .append(BoxDoc::text("var output strings.Builder"))
            .append(BoxDoc::line())
            .append(self.transpile_statements(&entrypoint.body))
            .append(BoxDoc::line())
            .append(BoxDoc::text("return output.String()"));

        result
            .append(BoxDoc::line().append(body).nest(1))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
            .append(BoxDoc::line())
    }
}

impl ExpressionTranspiler for GoTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_property_access<'a>(&self, object: &'a IrExpr, property: &'a str) -> BoxDoc<'a> {
        let prop_name = CasedString::from_snake_case(property).to_pascal_case();
        self.transpile_expr(object)
            .append(BoxDoc::text("."))
            .append(BoxDoc::as_string(prop_name))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("\"{}\"", self.escape_string(value)))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        BoxDoc::text(if value { "true" } else { "false" })
    }

    fn transpile_number_literal<'a>(&self, value: &'a serde_json::Number) -> BoxDoc<'a> {
        // Go requires explicit float notation for decimals
        if let Some(f) = value.as_f64() {
            if f.fract() == 0.0 {
                BoxDoc::as_string(format!("{}", f as i64))
            } else {
                BoxDoc::as_string(format!("{}", f))
            }
        } else {
            BoxDoc::as_string(value.to_string())
        }
    }

    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> BoxDoc<'a> {
        let type_part = match elem_type {
            Type::Array(Some(inner_type)) => {
                BoxDoc::text("[]").append(self.transpile_type(inner_type))
            }
            _ => BoxDoc::text("[]any"),
        };

        type_part
            .append(BoxDoc::text("{"))
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
    }

    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(String, IrExpr)],
        field_types: &'a BTreeMap<String, Type>,
    ) -> BoxDoc<'a> {
        // Build the struct type definition
        let mut struct_type = BoxDoc::text("struct{");
        for (field_name, field_type) in field_types {
            let go_field = CasedString::from_snake_case(field_name.as_str()).to_pascal_case();
            struct_type = struct_type
                .append(BoxDoc::as_string(go_field))
                .append(BoxDoc::text(" "))
                .append(self.transpile_type(field_type))
                .append(BoxDoc::text(" `json:\""))
                .append(BoxDoc::text(field_name))
                .append(BoxDoc::text("\"`; "));
        }
        struct_type = struct_type.append(BoxDoc::text("}"));

        // Build the struct literal values
        struct_type
            .append(BoxDoc::text("{"))
            .append(BoxDoc::intersperse(
                properties.iter().map(|(key, value)| {
                    let field_name = CasedString::from_snake_case(key).to_pascal_case();
                    BoxDoc::as_string(field_name)
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_expr(value))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
    }

    fn transpile_string_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("!(")
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("mustJSONMarshal(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }
}

impl TypeTranspiler for GoTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("bool")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_number_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("float64")
    }

    fn transpile_array_type<'a>(&self, element_type: Option<&'a Type>) -> BoxDoc<'a> {
        match element_type {
            Some(elem) => BoxDoc::text("[]").append(self.transpile_type(elem)),
            None => BoxDoc::text("[]any"),
        }
    }

    fn transpile_object_type<'a>(&self, fields: &'a BTreeMap<String, Type>) -> BoxDoc<'a> {
        // Generate anonymous struct type
        let mut result = BoxDoc::text("struct{");
        for (field_name, field_type) in fields {
            let go_field = CasedString::from_snake_case(field_name).to_pascal_case();
            result = result
                .append(BoxDoc::as_string(go_field))
                .append(BoxDoc::text(" "))
                .append(self.transpile_type(field_type))
                .append(BoxDoc::text(" `json:\""))
                .append(BoxDoc::text(field_name))
                .append(BoxDoc::text("\"`; "));
        }
        result.append(BoxDoc::text("}"))
    }
}

impl StatementTranspiler for GoTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::text("output.WriteString(\"")
            .append(BoxDoc::text(self.escape_string(content)))
            .append(BoxDoc::text("\")"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            BoxDoc::text("output.WriteString(html.EscapeString(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text("))"))
        } else {
            BoxDoc::text("output.WriteString(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(")"))
        }
    }

    fn transpile_if<'a>(&self, condition: &'a IrExpr, body: &'a [IrStatement]) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("if "))
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(1),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }

    fn transpile_for<'a>(
        &self,
        var: &'a str,
        array: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text("for _, ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" := range "))
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(1),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }

    fn transpile_let<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text(var)
            .append(BoxDoc::text(" := "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::line())
            .append(self.transpile_statements(body))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(ir_module: &IrModule) -> String {
        let transpiler = GoTranspiler::new();
        transpiler.transpile_module(ir_module)
    }

    fn check(ir_module: &IrModule, expected: Expect) {
        let go_code = transpile_with_pretty(ir_module);
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
