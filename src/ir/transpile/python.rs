use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::cased_string::CasedString;
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::BTreeMap;

pub struct PythonTranspiler {}

impl PythonTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn extract_and_generate_nested_type<'a>(
        &self,
        param_type: &'a Type,
        base_name: &str,
        field_name: &str,
        generated_types: &mut Vec<(String, BoxDoc<'a>)>,
    ) -> BoxDoc<'a> {
        match param_type {
            Type::Object(fields) => {
                // Generate the type name for this dataclass
                let type_name = format!(
                    "{}{}",
                    base_name,
                    CasedString::from_snake_case(field_name).to_pascal_case()
                );

                // Process fields depth-first, collecting nested types
                let mut field_docs = Vec::new();
                for (nested_field_name, nested_field_type) in fields {
                    // Recursively process and get the type reference
                    let field_type_doc = self.extract_and_generate_nested_type(
                        nested_field_type,
                        &type_name, // New base name for nested types
                        nested_field_name,
                        generated_types,
                    );

                    // Build field definition with type annotation
                    let field_doc = BoxDoc::text(nested_field_name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(field_type_doc);

                    field_docs.push(field_doc);
                }

                // Generate the dataclass definition
                let class_def = BoxDoc::text("@dataclass")
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("class "))
                    .append(BoxDoc::as_string(type_name.clone()))
                    .append(BoxDoc::text(":"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(field_docs, BoxDoc::line()))
                            .nest(4),
                    );

                // Add to generated types
                generated_types.push((type_name.clone(), class_def));

                // Return reference to this type
                BoxDoc::as_string(type_name)
            }
            Type::Array(Some(elem_type)) => {
                if matches!(elem_type.as_ref(), Type::Object(_)) {
                    // Generate type for array elements
                    let elem_type_doc = self.extract_and_generate_nested_type(
                        elem_type,
                        base_name,
                        &format!("{}_item", field_name),
                        generated_types,
                    );
                    BoxDoc::text("list[")
                        .append(elem_type_doc)
                        .append(BoxDoc::text("]"))
                } else {
                    // Regular array
                    BoxDoc::text("list[")
                        .append(self.get_python_type(elem_type))
                        .append(BoxDoc::text("]"))
                }
            }
            _ => {
                // Primitive types - just use regular type
                self.get_python_type(param_type)
            }
        }
    }

    fn get_python_type<'a>(&self, param_type: &'a Type) -> BoxDoc<'a> {
        match param_type {
            Type::String => BoxDoc::text("str"),
            Type::Bool => BoxDoc::text("bool"),
            Type::Float => BoxDoc::text("float"),
            Type::Int => BoxDoc::text("int"),
            Type::TrustedHtml => BoxDoc::text("str"),
            Type::Array(Some(elem)) => BoxDoc::text("list[")
                .append(self.get_python_type(elem))
                .append(BoxDoc::text("]")),
            Type::Array(None) => BoxDoc::text("list"),
            Type::Object(_) => BoxDoc::text("dict"),
        }
    }

    fn scan_for_imports(&self, entrypoint: &IrEntrypoint) -> (bool, bool, bool) {
        let mut needs_html_escape = false;
        let mut needs_json = false;
        let mut needs_simple_namespace = false;

        for stmt in &entrypoint.body {
            // Check for HTML escaping
            stmt.traverse(&mut |s| {
                if let IrStatement::WriteExpr { escape: true, .. } = s {
                    needs_html_escape = true;
                }
            });

            // Check for JSON encoding and object literals
            stmt.traverse(&mut |s| {
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| match expr {
                        IrExpr::JsonEncode { .. } => {
                            needs_json = true;
                        }
                        IrExpr::ObjectLiteral { .. } => {
                            needs_simple_namespace = true;
                        }
                        _ => {}
                    });
                }
            });
        }

        (needs_html_escape, needs_json, needs_simple_namespace)
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
        let mut needs_simple_namespace = false;
        let mut needs_dataclasses = false;

        // First pass: scan all entrypoints to determine imports
        for entrypoint in entrypoints {
            let (has_escape, has_json, has_simple_namespace) = self.scan_for_imports(entrypoint);
            needs_html_escape |= has_escape;
            needs_json |= has_json;
            needs_simple_namespace |= has_simple_namespace;
            if !entrypoint.parameters.is_empty() {
                needs_dataclasses = true;
            }
        }

        let mut result = BoxDoc::nil();

        // Add imports if needed
        if needs_dataclasses {
            result = result
                .append(BoxDoc::text("from dataclasses import dataclass"))
                .append(BoxDoc::line());
        }

        if needs_simple_namespace {
            result = result
                .append(BoxDoc::text("from types import SimpleNamespace"))
                .append(BoxDoc::line());
        }

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

        if needs_dataclasses || needs_simple_namespace || needs_json || needs_html_escape {
            result = result.append(BoxDoc::line());
        }

        // Generate parameter dataclasses for entrypoints that have parameters
        for entrypoint in entrypoints {
            if !entrypoint.parameters.is_empty() {
                let class_name = format!(
                    "{}Params",
                    CasedString::from_kebab_case(&entrypoint.name).to_pascal_case()
                );

                let mut nested_types = Vec::new();

                // Process each parameter, extracting nested types
                let fields: Vec<_> = entrypoint
                    .parameters
                    .iter()
                    .map(|(param_name, param_type)| {
                        // Extract nested types and get the type reference
                        let field_type_doc = self.extract_and_generate_nested_type(
                            param_type,
                            &class_name,
                            param_name.as_str(),
                            &mut nested_types,
                        );

                        BoxDoc::text(param_name.as_str())
                            .append(BoxDoc::text(": "))
                            .append(field_type_doc)
                    })
                    .collect();

                // First, add all nested types that were extracted
                for (_name, type_def) in &nested_types {
                    result = result
                        .append(type_def.clone())
                        .append(BoxDoc::line())
                        .append(BoxDoc::line());
                }

                // Then add the main parameter dataclass
                result = result
                    .append(BoxDoc::text("@dataclass"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("class "))
                    .append(BoxDoc::text(class_name))
                    .append(BoxDoc::text(":"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(fields, BoxDoc::line()))
                            .nest(4),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }
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
        if entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::text("() -> str:"));
        } else {
            let class_name = format!(
                "{}Params",
                CasedString::from_kebab_case(name).to_pascal_case()
            );
            result = result
                .append(BoxDoc::text("(params: "))
                .append(BoxDoc::as_string(class_name))
                .append(BoxDoc::text(") -> str:"));
        }

        // Function body
        let mut body = Vec::new();

        // Extract parameters from dataclass into local variables
        for (param_name, _) in &entrypoint.parameters {
            body.push(
                BoxDoc::nil()
                    .append(BoxDoc::text(param_name.as_str()))
                    .append(BoxDoc::text(" = params."))
                    .append(BoxDoc::text(param_name.as_str())),
            );
        }

        body.push(BoxDoc::text("output = []"));

        // Add the statements
        let statements_doc = self.transpile_statements(&entrypoint.body);
        if !entrypoint.body.is_empty() {
            body.push(statements_doc);
        }

        body.push(BoxDoc::text("return ''.join(output)"));

        result
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::intersperse(body, BoxDoc::line()))
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
            BoxDoc::text("output.append(html_escape(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text("))"))
        } else {
            BoxDoc::text("output.append(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(")"))
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
        // Python uses dot notation for dataclass attributes
        self.transpile_expr(object)
            .append(BoxDoc::text("."))
            .append(BoxDoc::text(property))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("\"{}\"", self.escape_string(value)))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        BoxDoc::text(if value { "True" } else { "False" })
    }

    fn transpile_float_literal<'a>(&self, value: f64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}", value))
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
        BoxDoc::text("SimpleNamespace(")
            .append(BoxDoc::intersperse(
                properties.iter().map(|(key, value)| {
                    BoxDoc::text(key)
                        .append(BoxDoc::text("="))
                        .append(self.transpile_expr(value))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_greater_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" > "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_greater_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" > "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" <= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_less_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" <= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_greater_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" >= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_greater_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" >= "))
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

    fn transpile_env_lookup<'a>(&self, key: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("os.environ.get(")
            .append(self.transpile_expr(key))
            .append(BoxDoc::text(", \"\")"))
    }

    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_and<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" and "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_or<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" or "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
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

    fn transpile_trusted_html_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("str")
    }

    fn transpile_float_type<'a>(&self) -> BoxDoc<'a> {
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
                from dataclasses import dataclass
                from html import escape as html_escape

                @dataclass
                class TestGreetingCompParams:
                    name: str
                    message: str

                def test_greeting_comp(params: TestGreetingCompParams) -> str:
                    name = params.name
                    message = params.message
                    output = []
                    output.append("<h1>Hello ")
                    output.append(html_escape(name))
                    output.append(", ")
                    output.append(html_escape(message))
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
                from dataclasses import dataclass

                @dataclass
                class TestMainCompParams:
                    show: bool

                def test_main_comp(params: TestMainCompParams) -> str:
                    show = params.show
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
                from dataclasses import dataclass
                from html import escape as html_escape

                @dataclass
                class TestMainCompParams:
                    items: list[str]

                def test_main_comp(params: TestMainCompParams) -> str:
                    items = params.items
                    output = []
                    for item in items:
                        output.append("<li>")
                        output.append(html_escape(item))
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
                    output.append(html_escape(greeting))
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
                map.insert("count".to_string(), Type::Float);
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
                test-json(data: {count: float, title: string}) {
                  write("<script>\n")
                  write("const data = ")
                  write_expr(JsonEncode(data))
                  write(";\n")
                  write("</script>\n")
                }

                -- after --
                from dataclasses import dataclass
                import json

                @dataclass
                class TestJsonParamsData:
                    count: float
                    title: str

                @dataclass
                class TestJsonParams:
                    data: TestJsonParamsData

                def test_json(params: TestJsonParams) -> str:
                    data = params.data
                    output = []
                    output.append("<script>\n")
                    output.append("const data = ")
                    output.append(json.dumps(data, separators=(',', ':')))
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
                from dataclasses import dataclass

                @dataclass
                class TestAuthCheckParams:
                    user_role: str
                    expected_role: str

                def test_auth_check(params: TestAuthCheckParams) -> str:
                    user_role = params.user_role
                    expected_role = params.expected_role
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
                from dataclasses import dataclass

                @dataclass
                class TestNotParams:
                    active: bool

                def test_not(params: TestNotParams) -> str:
                    active = params.active
                    output = []
                    if not (active):
                        output.append("<div>Inactive</div>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_array_of_nested_objects() {
        let parameters = vec![(
            "users",
            Type::Array(Some(Box::new(Type::Object({
                let mut user = BTreeMap::new();
                user.insert("name".to_string(), Type::String);
                user.insert("email".to_string(), Type::String);
                user.insert(
                    "profile".to_string(),
                    Type::Object({
                        let mut profile = BTreeMap::new();
                        profile.insert("bio".to_string(), Type::String);
                        profile.insert("age".to_string(), Type::Float);
                        profile
                    }),
                );
                user
            })))),
        )];

        let entrypoints = vec![build_ir_auto("test-users", parameters, |t| {
            t.for_loop("user", t.var("users"), |t| {
                t.write("<div>");
                t.write_expr_escaped(t.prop_access(t.var("user"), "name"));
                t.write(" - ");
                t.write_expr_escaped(t.prop_access(t.prop_access(t.var("user"), "profile"), "bio"));
                t.write("</div>\n");
            });
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-users(
                  users: array[{
                    email: string,
                    name: string,
                    profile: {age: float, bio: string},
                  }],
                ) {
                  for user in users {
                    write("<div>")
                    write_escaped(user.name)
                    write(" - ")
                    write_escaped(user.profile.bio)
                    write("</div>\n")
                  }
                }

                -- after --
                from dataclasses import dataclass
                from html import escape as html_escape

                @dataclass
                class TestUsersParamsUsersItemProfile:
                    age: float
                    bio: str

                @dataclass
                class TestUsersParamsUsersItem:
                    email: str
                    name: str
                    profile: TestUsersParamsUsersItemProfile

                @dataclass
                class TestUsersParams:
                    users: list[TestUsersParamsUsersItem]

                def test_users(params: TestUsersParams) -> str:
                    users = params.users
                    output = []
                    for user in users:
                        output.append("<div>")
                        output.append(html_escape(user.name))
                        output.append(" - ")
                        output.append(html_escape(user.profile.bio))
                        output.append("</div>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_object_literal_with_simple_namespace() {
        let entrypoints = vec![build_ir_auto("test-objects", vec![], |t| {
            t.let_stmt(
                "person",
                t.object(vec![
                    ("name", t.str("Alice")),
                    ("city", t.str("NYC")),
                    ("active", t.bool(true)),
                ]),
                |t| {
                    t.write("<div>");
                    t.write_expr_escaped(t.prop_access(t.var("person"), "name"));
                    t.write(" from ");
                    t.write_expr_escaped(t.prop_access(t.var("person"), "city"));
                    t.write("</div>\n");
                },
            );
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-objects() {
                  let person = {
                    name: "Alice",
                    city: "NYC",
                    active: true,
                  } in {
                    write("<div>")
                    write_escaped(person.name)
                    write(" from ")
                    write_escaped(person.city)
                    write("</div>\n")
                  }
                }

                -- after --
                from types import SimpleNamespace
                from html import escape as html_escape

                def test_objects() -> str:
                    output = []
                    person = SimpleNamespace(name="Alice", city="NYC", active=True)
                    output.append("<div>")
                    output.append(html_escape(person.name))
                    output.append(" from ")
                    output.append(html_escape(person.city))
                    output.append("</div>\n")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_deeply_nested_structs() {
        let parameters = vec![(
            "config",
            Type::Object({
                let mut config = BTreeMap::new();
                config.insert("api_key".to_string(), Type::String);
                config.insert(
                    "database".to_string(),
                    Type::Object({
                        let mut db = BTreeMap::new();
                        db.insert("host".to_string(), Type::String);
                        db.insert("port".to_string(), Type::Float);
                        db.insert(
                            "credentials".to_string(),
                            Type::Object({
                                let mut creds = BTreeMap::new();
                                creds.insert("username".to_string(), Type::String);
                                creds.insert("password".to_string(), Type::String);
                                creds
                            }),
                        );
                        db
                    }),
                );
                config
            }),
        )];

        let entrypoints = vec![build_ir_auto("test-deep-config", parameters, |t| {
            t.write("<div>API Key: ");
            t.write_expr_escaped(t.prop_access(t.var("config"), "api_key"));
            t.write("</div>\n");
            t.write("<div>DB Host: ");
            t.write_expr_escaped(t.prop_access(t.prop_access(t.var("config"), "database"), "host"));
            t.write("</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-deep-config(
                  config: {
                    api_key: string,
                    database: {
                      credentials: {password: string, username: string},
                      host: string,
                      port: float,
                    },
                  },
                ) {
                  write("<div>API Key: ")
                  write_escaped(config.api_key)
                  write("</div>\n")
                  write("<div>DB Host: ")
                  write_escaped(config.database.host)
                  write("</div>\n")
                }

                -- after --
                from dataclasses import dataclass
                from html import escape as html_escape

                @dataclass
                class TestDeepConfigParamsConfigDatabaseCredentials:
                    password: str
                    username: str

                @dataclass
                class TestDeepConfigParamsConfigDatabase:
                    credentials: TestDeepConfigParamsConfigDatabaseCredentials
                    host: str
                    port: float

                @dataclass
                class TestDeepConfigParamsConfig:
                    api_key: str
                    database: TestDeepConfigParamsConfigDatabase

                @dataclass
                class TestDeepConfigParams:
                    config: TestDeepConfigParamsConfig

                def test_deep_config(params: TestDeepConfigParams) -> str:
                    config = params.config
                    output = []
                    output.append("<div>API Key: ")
                    output.append(html_escape(config.api_key))
                    output.append("</div>\n")
                    output.append("<div>DB Host: ")
                    output.append(html_escape(config.database.host))
                    output.append("</div>\n")
                    return ''.join(output)
            "#]],
        );
    }
}
