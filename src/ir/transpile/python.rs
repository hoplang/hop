use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::BTreeMap;

pub struct PythonTranspiler {}

impl PythonTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn scan_for_imports(&self, entrypoint: &IrEntrypoint) -> (bool, bool) {
        let mut needs_html_escape = false;
        let mut needs_json = false;

        for stmt in &entrypoint.body {
            // Check for HTML escaping
            stmt.traverse(&mut |s| {
                if let IrStatement::WriteExpr { escape: true, .. } = s {
                    needs_html_escape = true;
                }
            });

            // Check for JSON encoding
            stmt.traverse(&mut |s| {
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| {
                        if let IrExpr::JsonEncode { .. } = expr {
                            needs_json = true;
                        }
                    });
                }
            });
        }

        (needs_html_escape, needs_json)
    }

    // Helper method to escape strings for Python string literals
    fn escape_string(&self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }
}

impl Transpiler for PythonTranspiler {
    fn transpile_module(&self, entrypoints: &[IrEntrypoint]) -> String {
        let mut needs_html_escape = false;
        let mut needs_json = false;

        // First pass: scan all entrypoints to determine imports
        for entrypoint in entrypoints {
            let (has_escape, has_json) = self.scan_for_imports(entrypoint);
            needs_html_escape |= has_escape;
            needs_json |= has_json;
        }

        let mut result = BoxDoc::nil();

        // Add imports if needed
        if needs_json {
            result = result
                .append(BoxDoc::text("import json"))
                .append(BoxDoc::line());
        }

        if needs_html_escape {
            result = result
                .append(BoxDoc::text("from html import escape as html_escape"))
                .append(BoxDoc::line());
        }

        if needs_json || needs_html_escape {
            result = result.append(BoxDoc::line());
        }

        // Transpile each entrypoint as a function
        for (i, entrypoint) in entrypoints.iter().enumerate() {
            result = result.append(self.transpile_entrypoint(&entrypoint.name, entrypoint));
            if i < entrypoints.len() - 1 {
                result = result.append(BoxDoc::line());
            }
        }

        // Render to string
        let mut buffer = Vec::new();
        result.render(80, &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    fn transpile_entrypoint<'a>(&self, name: &'a str, entrypoint: &'a IrEntrypoint) -> BoxDoc<'a> {
        // Convert kebab-case to snake_case for Python function name
        let func_name = name.replace('-', "_");

        let mut result = BoxDoc::text("def ").append(BoxDoc::as_string(func_name));

        // Function parameters
        result = result.append(BoxDoc::text("("));
        if !entrypoint.parameters.is_empty() {
            // Build parameter list
            let params: Vec<_> = entrypoint
                .parameters
                .iter()
                .map(|(param_name, param_type)| {
                    // Add type hints
                    let type_hint = match param_type {
                        Type::String => ": str",
                        Type::Bool => ": bool",
                        Type::Number => ": float",
                        Type::Int => ": int",
                        Type::Array(Some(elem)) => match elem.as_ref() {
                            Type::String => ": list[str]",
                            Type::Bool => ": list[bool]",
                            Type::Number => ": list[float]",
                            Type::Int => ": list[int]",
                            _ => ": list",
                        },
                        Type::Array(None) => ": list",
                        Type::Object(_) => ": dict",
                        _ => "",
                    };
                    BoxDoc::text(param_name.as_str()).append(BoxDoc::text(type_hint))
                })
                .collect();

            result = result.append(BoxDoc::intersperse(params, BoxDoc::text(", ")));
        }
        result = result.append(BoxDoc::text(") -> str:"));

        // Function body
        result
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("output = []"))
                    .append(BoxDoc::line())
                    .append(self.transpile_statements(&entrypoint.body))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("return ''.join(output)"))
                    .nest(4),
            )
            .append(BoxDoc::line())
    }
}

impl StatementTranspiler for PythonTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::text("output.append(\"")
            .append(BoxDoc::text(self.escape_string(content)))
            .append(BoxDoc::text("\")"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            BoxDoc::text("output.append(html_escape(str(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(")))"))
        } else {
            BoxDoc::text("output.append(str(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text("))"))
        }
    }

    fn transpile_if<'a>(&self, condition: &'a IrExpr, body: &'a [IrStatement]) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("if "))
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(":"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(4),
            )
    }

    fn transpile_for<'a>(
        &self,
        var: &'a str,
        array: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text("for ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" in "))
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(":"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(4),
            )
    }

    fn transpile_let<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text(var)
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::line())
            .append(self.transpile_statements(body))
    }
}

impl ExpressionTranspiler for PythonTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_property_access<'a>(&self, object: &'a IrExpr, property: &'a str) -> BoxDoc<'a> {
        // Python uses dictionary access for objects
        self.transpile_expr(object)
            .append(BoxDoc::text("[\""))
            .append(BoxDoc::text(property))
            .append(BoxDoc::text("\"]"))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("\"{}\"", self.escape_string(value)))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        BoxDoc::text(if value { "True" } else { "False" })
    }

    fn transpile_number_literal<'a>(&self, value: &'a serde_json::Number) -> BoxDoc<'a> {
        BoxDoc::as_string(value.to_string())
    }

    fn transpile_int_literal<'a>(&self, value: i64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}", value))
    }

    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        _elem_type: &'a Option<Box<Type>>,
    ) -> BoxDoc<'a> {
        BoxDoc::text("[")
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("]"))
    }

    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(String, IrExpr)],
        _field_types: &'a BTreeMap<String, Type>,
    ) -> BoxDoc<'a> {
        BoxDoc::text("{")
            .append(BoxDoc::intersperse(
                properties.iter().map(|(key, value)| {
                    BoxDoc::text("\"")
                        .append(BoxDoc::text(key))
                        .append(BoxDoc::text("\": "))
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
        BoxDoc::text("not (")
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("json.dumps(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(", separators=(',', ':'))"))
    }

    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }
}

impl TypeTranspiler for PythonTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("bool")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("str")
    }

    fn transpile_number_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("float")
    }

    fn transpile_int_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("int")
    }

    fn transpile_array_type<'a>(&self, element_type: Option<&'a Type>) -> BoxDoc<'a> {
        match element_type {
            Some(elem) => BoxDoc::text("list[")
                .append(self.transpile_type(elem))
                .append(BoxDoc::text("]")),
            None => BoxDoc::text("list"),
        }
    }

    fn transpile_object_type<'a>(&self, fields: &'a BTreeMap<String, Type>) -> BoxDoc<'a> {
        if fields.is_empty() {
            return BoxDoc::text("dict");
        }

        // For Python, we'll use dict with type hints as comments or in docstrings
        // For now, just use dict
        BoxDoc::text("dict")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(entrypoints: &[IrEntrypoint]) -> String {
        let transpiler = PythonTranspiler::new();
        transpiler.transpile_module(entrypoints)
    }

    fn check(entrypoints: &[IrEntrypoint], expected: Expect) {
        // Format before (IR)
        let before = entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        // Format after (Python code)
        let after = transpile_with_pretty(entrypoints);

        // Create output with before/after format
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);

        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_component() {
        let entrypoints = vec![build_ir_auto("test-main-comp", vec![], |t| {
            t.write("<div>Hello World</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp() {
                  write("<div>Hello World</div>\n")
                }

                -- after --
                def test_main_comp() -> str:
                    output = []
                    output.append("<div>Hello World</div>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_component_with_parameters() {
        let entrypoints = vec![build_ir_auto(
            "test-greeting-comp",
            vec![("name", Type::String), ("message", Type::String)],
            |t| {
                t.write("<h1>Hello ");
                t.write_expr_escaped(t.var("name"));
                t.write(", ");
                t.write_expr_escaped(t.var("message"));
                t.write("</h1>\n");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-greeting-comp(name: string, message: string) {
                  write("<h1>Hello ")
                  write_escaped(name)
                  write(", ")
                  write_escaped(message)
                  write("</h1>\n")
                }

                -- after --
                from html import escape as html_escape

                def test_greeting_comp(name: str, message: str) -> str:
                    output = []
                    output.append("<h1>Hello ")
                    output.append(html_escape(str(name)))
                    output.append(", ")
                    output.append(html_escape(str(message)))
                    output.append("</h1>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_if_condition() {
        let entrypoints = vec![build_ir_auto(
            "test-main-comp",
            vec![("show", Type::Bool)],
            |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Visible</div>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp(show: boolean) {
                  if show {
                    write("<div>Visible</div>\n")
                  }
                }

                -- after --
                def test_main_comp(show: bool) -> str:
                    output = []
                    if show:
                        output.append("<div>Visible</div>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_for_loop() {
        let entrypoints = vec![build_ir_auto(
            "test-main-comp",
            vec![("items", Type::Array(Some(Box::new(Type::String))))],
            |t| {
                t.for_loop("item", t.var("items"), |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("item"));
                    t.write("</li>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp(items: array[string]) {
                  for item in items {
                    write("<li>")
                    write_escaped(item)
                    write("</li>\n")
                  }
                }

                -- after --
                from html import escape as html_escape

                def test_main_comp(items: list[str]) -> str:
                    output = []
                    for item in items:
                        output.append("<li>")
                        output.append(html_escape(str(item)))
                        output.append("</li>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_let_binding() {
        let entrypoints = vec![build_ir_auto("test-greeting", vec![], |t| {
            t.let_stmt("greeting", t.str("Hello from Python!"), |t| {
                t.write("<p>");
                t.write_expr_escaped(t.var("greeting"));
                t.write("</p>\n");
            });
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-greeting() {
                  let greeting = "Hello from Python!" in {
                    write("<p>")
                    write_escaped(greeting)
                    write("</p>\n")
                  }
                }

                -- after --
                from html import escape as html_escape

                def test_greeting() -> str:
                    output = []
                    greeting = "Hello from Python!"
                    output.append("<p>")
                    output.append(html_escape(str(greeting)))
                    output.append("</p>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_json_encode() {
        let parameters = vec![(
            "data",
            Type::Object({
                let mut map = BTreeMap::new();
                map.insert("title".to_string(), Type::String);
                map.insert("count".to_string(), Type::Number);
                map
            }),
        )];

        let entrypoints = vec![build_ir_auto("test-json", parameters, |t| {
            t.write("<script>\n");
            t.write("const data = ");
            t.write_expr(t.json_encode(t.var("data")), false);
            t.write(";\n");
            t.write("</script>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-json(data: {count: number, title: string}) {
                  write("<script>\n")
                  write("const data = ")
                  write_expr(JsonEncode(data))
                  write(";\n")
                  write("</script>\n")
                }

                -- after --
                import json

                def test_json(data: dict) -> str:
                    output = []
                    output.append("<script>\n")
                    output.append("const data = ")
                    output.append(str(json.dumps(data, separators=(',', ':'))))
                    output.append(";\n")
                    output.append("</script>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_string_comparison() {
        let entrypoints = vec![build_ir_auto(
            "test-auth-check",
            vec![("user_role", Type::String), ("expected_role", Type::String)],
            |t| {
                t.if_stmt(t.eq(t.var("user_role"), t.var("expected_role")), |t| {
                    t.write("<div>Access granted</div>\n");
                });
                t.if_stmt(t.eq(t.var("user_role"), t.str("admin")), |t| {
                    t.write("<div>Admin panel available</div>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-auth-check(user_role: string, expected_role: string) {
                  if (user_role == expected_role) {
                    write("<div>Access granted</div>\n")
                  }
                  if (user_role == "admin") {
                    write("<div>Admin panel available</div>\n")
                  }
                }

                -- after --
                def test_auth_check(user_role: str, expected_role: str) -> str:
                    output = []
                    if (user_role == expected_role):
                        output.append("<div>Access granted</div>\n")
                    if (user_role == "admin"):
                        output.append("<div>Admin panel available</div>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_not_operator() {
        let entrypoints = vec![build_ir_auto(
            "test-not",
            vec![("active", Type::Bool)],
            |t| {
                t.if_stmt(t.not(t.var("active")), |t| {
                    t.write("<div>Inactive</div>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-not(active: boolean) {
                  if (!active) {
                    write("<div>Inactive</div>\n")
                  }
                }

                -- after --
                def test_not(active: bool) -> str:
                    output = []
                    if not (active):
                        output.append("<div>Inactive</div>\n")
                    return ''.join(output)
            "#]],
        );
    }
}
