use pretty::BoxDoc;

use super::{ExpressionTranspiler, RecordInfo, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::field_name::FieldName;
use crate::dop::r#type::Type;
use crate::hop::component_name::ComponentName;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};

pub struct PythonTranspiler {}

impl PythonTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn get_python_type(param_type: &Type) -> BoxDoc<'_> {
        match param_type {
            Type::String => BoxDoc::text("str"),
            Type::Bool => BoxDoc::text("bool"),
            Type::Float => BoxDoc::text("float"),
            Type::Int => BoxDoc::text("int"),
            Type::TrustedHTML => BoxDoc::text("TrustedHTML"),
            Type::Array(Some(elem)) => BoxDoc::text("list[")
                .append(Self::get_python_type(elem))
                .append(BoxDoc::text("]")),
            Type::Array(None) => BoxDoc::text("list"),
            Type::Named(name) => BoxDoc::text(name.clone()),
        }
    }

    fn scan_for_imports(&self, entrypoint: &IrEntrypoint) -> (bool, bool, bool) {
        let mut needs_html_escape = false;
        let mut needs_json = false;
        let mut needs_os = false;

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
                    primary_expr.traverse(&mut |expr| match expr {
                        IrExpr::JsonEncode { .. } => {
                            needs_json = true;
                        }
                        IrExpr::EnvLookup { .. } => {
                            needs_os = true;
                        }
                        _ => {}
                    });
                }
            });
        }

        (needs_html_escape, needs_json, needs_os)
    }

    fn scan_for_trusted_html(&self, entrypoints: &[IrEntrypoint]) -> bool {
        for entrypoint in entrypoints {
            for (_, param_type) in &entrypoint.parameters {
                if Self::type_contains_trusted_html(param_type) {
                    return true;
                }
            }
        }
        false
    }

    fn type_contains_trusted_html(t: &Type) -> bool {
        match t {
            Type::TrustedHTML => true,
            Type::Array(Some(elem)) => Self::type_contains_trusted_html(elem),
            _ => false,
        }
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
    fn transpile_module(&self, entrypoints: &[IrEntrypoint], records: &[RecordInfo]) -> String {
        let mut needs_html_escape = false;
        let mut needs_json = false;
        let mut needs_dataclasses = false;
        let mut needs_os = false;

        // First pass: scan all entrypoints to determine imports
        for entrypoint in entrypoints {
            let (has_escape, has_json, has_os) = self.scan_for_imports(entrypoint);
            needs_html_escape |= has_escape;
            needs_json |= has_json;
            needs_os |= has_os;
            if !entrypoint.parameters.is_empty() {
                needs_dataclasses = true;
            }
        }

        // Check if we have records to generate
        if !records.is_empty() {
            needs_dataclasses = true;
        }

        let needs_trusted_html = self.scan_for_trusted_html(entrypoints);

        let mut result = BoxDoc::nil();

        // Add imports if needed
        if needs_os {
            result = result
                .append(BoxDoc::text("import os"))
                .append(BoxDoc::line());
        }

        if needs_dataclasses {
            result = result
                .append(BoxDoc::text("from dataclasses import dataclass"))
                .append(BoxDoc::line());
        }

        if needs_trusted_html {
            result = result
                .append(BoxDoc::text("from typing import NewType"))
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

        if needs_dataclasses || needs_json || needs_html_escape || needs_trusted_html {
            result = result.append(BoxDoc::line());
        }

        // Add TrustedHTML type definition if needed
        if needs_trusted_html {
            result = result
                .append(BoxDoc::text("TrustedHTML = NewType('TrustedHTML', str)"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Generate record type dataclasses
        for record in records {
            let fields: Vec<_> = record
                .fields
                .iter()
                .map(|(field_name, field_type)| {
                    BoxDoc::text(field_name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(Self::get_python_type(field_type))
                })
                .collect();

            result = result
                .append(BoxDoc::text("@dataclass"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("class "))
                .append(BoxDoc::text(record.name.as_str()))
                .append(BoxDoc::text(":"))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(fields, BoxDoc::line()))
                        .nest(4),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Generate parameter dataclasses for entrypoints that have parameters
        for entrypoint in entrypoints {
            if !entrypoint.parameters.is_empty() {
                let class_name = format!("{}Params", entrypoint.name.to_pascal_case());

                let fields: Vec<_> = entrypoint
                    .parameters
                    .iter()
                    .map(|(param_name, param_type)| {
                        BoxDoc::text(param_name.as_str())
                            .append(BoxDoc::text(": "))
                            .append(Self::get_python_type(param_type))
                    })
                    .collect();

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

    fn transpile_entrypoint<'a>(
        &self,
        name: &'a ComponentName,
        entrypoint: &'a IrEntrypoint,
    ) -> BoxDoc<'a> {
        // Convert PascalCase to snake_case for Python function name
        let func_name = name.to_snake_case();

        let mut result = BoxDoc::text("def ").append(BoxDoc::as_string(func_name));

        // Function parameters
        if entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::text("() -> str:"));
        } else {
            let class_name = format!("{}Params", name.to_pascal_case());
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
            // Check if the expression is TrustedHTML - needs explicit cast to str for mypy
            let needs_cast = matches!(expr.as_type(), Type::TrustedHTML);

            let mut doc = BoxDoc::text("output.append(");
            if needs_cast {
                doc = doc.append(BoxDoc::text("str("));
            }
            doc = doc.append(self.transpile_expr(expr));
            if needs_cast {
                doc = doc.append(BoxDoc::text(")"));
            }
            doc.append(BoxDoc::text(")"))
        }
    }

    fn transpile_if<'a>(
        &self,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
        else_body: Option<&'a [IrStatement]>,
    ) -> BoxDoc<'a> {
        let mut doc = BoxDoc::nil()
            .append(BoxDoc::text("if "))
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(":"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(4),
            );

        if let Some(else_stmts) = else_body {
            doc = doc
                .append(BoxDoc::line())
                .append(BoxDoc::text("else:"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(else_stmts))
                        .nest(4),
                );
        }

        doc
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

    fn transpile_field_access<'a>(
        &self,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> BoxDoc<'a> {
        // Python uses dot notation for dataclass attributes
        self.transpile_expr(object)
            .append(BoxDoc::text("."))
            .append(BoxDoc::as_string(field.as_str()))
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

    fn transpile_record_instantiation<'a>(
        &self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In Python, record instantiation is a dataclass constructor call
        BoxDoc::text(record_name)
            .append(BoxDoc::text("("))
            .append(BoxDoc::intersperse(
                fields.iter().map(|(key, value)| {
                    BoxDoc::text(key.as_str())
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

    fn transpile_int_subtract<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" - "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_subtract<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" - "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_multiply<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" * "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_multiply<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" * "))
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
        BoxDoc::text("TrustedHTML")
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

    fn transpile_named_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(entrypoints: &[IrEntrypoint]) -> String {
        let transpiler = PythonTranspiler::new();
        transpiler.transpile_module(entrypoints, &[])
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
        let entrypoints = vec![build_ir_auto("TestMainComp", vec![], |t| {
            t.write("<div>Hello World</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                TestMainComp() {
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
            "TestGreetingComp",
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
                TestGreetingComp(name: String, message: String) {
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
            "TestMainComp",
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
                TestMainComp(show: Bool) {
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
            "TestMainComp",
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
                TestMainComp(items: Array[String]) {
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
        let entrypoints = vec![build_ir_auto("TestGreeting", vec![], |t| {
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
                TestGreeting() {
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
    fn test_string_comparison() {
        let entrypoints = vec![build_ir_auto(
            "TestAuthCheck",
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
                TestAuthCheck(user_role: String, expected_role: String) {
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
            "TestNot",
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
                TestNot(active: Bool) {
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
    fn test_trusted_html_type() {
        let entrypoints = vec![build_ir_auto(
            "RenderHtml",
            vec![
                ("safe_content", Type::TrustedHTML),
                ("user_input", Type::String),
            ],
            |t| {
                t.write("<div>");
                t.write_expr(t.var("safe_content"), false);
                t.write("</div><div>");
                t.write_expr_escaped(t.var("user_input"));
                t.write("</div>");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                RenderHtml(safe_content: TrustedHTML, user_input: String) {
                  write("<div>")
                  write_expr(safe_content)
                  write("</div><div>")
                  write_escaped(user_input)
                  write("</div>")
                }

                -- after --
                from dataclasses import dataclass
                from typing import NewType
                from html import escape as html_escape

                TrustedHTML = NewType('TrustedHTML', str)

                @dataclass
                class RenderHtmlParams:
                    safe_content: TrustedHTML
                    user_input: str

                def render_html(params: RenderHtmlParams) -> str:
                    safe_content = params.safe_content
                    user_input = params.user_input
                    output = []
                    output.append("<div>")
                    output.append(str(safe_content))
                    output.append("</div><div>")
                    output.append(html_escape(user_input))
                    output.append("</div>")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn test_record_declarations() {
        use crate::ir::test_utils::build_ir_with_records;

        let records_def = vec![
            (
                "User",
                vec![
                    ("name", Type::String),
                    ("age", Type::Int),
                    ("active", Type::Bool),
                ],
            ),
            (
                "Address",
                vec![("street", Type::String), ("city", Type::String)],
            ),
        ];

        let entrypoints = vec![build_ir_with_records(
            "UserProfile",
            vec![("user", Type::Named("User".to_string()))],
            records_def,
            |t| {
                t.write("<div>");
                t.write_expr_escaped(t.field_access(t.var("user"), "name"));
                t.write("</div>");
            },
        )];

        let records = vec![
            RecordInfo {
                name: "User".to_string(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("age").unwrap(), Type::Int),
                    (FieldName::new("active").unwrap(), Type::Bool),
                ],
            },
            RecordInfo {
                name: "Address".to_string(),
                fields: vec![
                    (FieldName::new("street").unwrap(), Type::String),
                    (FieldName::new("city").unwrap(), Type::String),
                ],
            },
        ];

        let transpiler = PythonTranspiler::new();
        let output = transpiler.transpile_module(&entrypoints, &records);

        expect![[r#"
            from dataclasses import dataclass
            from html import escape as html_escape

            @dataclass
            class User:
                name: str
                age: int
                active: bool

            @dataclass
            class Address:
                street: str
                city: str

            @dataclass
            class UserProfileParams:
                user: User

            def user_profile(params: UserProfileParams) -> str:
                user = params.user
                output = []
                output.append("<div>")
                output.append(html_escape(user.name))
                output.append("</div>")
                return ''.join(output)
        "#]]
        .assert_eq(&output);
    }

    #[test]
    fn test_record_instantiation() {
        use crate::ir::test_utils::build_ir_with_records;

        let records_def = vec![("User", vec![("name", Type::String), ("age", Type::Int)])];

        let entrypoints = vec![build_ir_with_records(
            "CreateUser",
            vec![],
            records_def,
            |t| {
                t.write("<div>");
                let user = t.record("User", vec![("name", t.str("John")), ("age", t.int(30))]);
                t.write_expr_escaped(t.field_access(user, "name"));
                t.write("</div>");
            },
        )];

        let records = vec![RecordInfo {
            name: "User".to_string(),
            fields: vec![
                (FieldName::new("name").unwrap(), Type::String),
                (FieldName::new("age").unwrap(), Type::Int),
            ],
        }];

        let transpiler = PythonTranspiler::new();
        let output = transpiler.transpile_module(&entrypoints, &records);

        expect![[r#"
            from dataclasses import dataclass
            from html import escape as html_escape

            @dataclass
            class User:
                name: str
                age: int

            def create_user() -> str:
                output = []
                output.append("<div>")
                output.append(html_escape(User(name="John", age=30).name))
                output.append("</div>")
                return ''.join(output)
        "#]]
        .assert_eq(&output);
    }
}
