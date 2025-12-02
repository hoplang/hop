use pretty::BoxDoc;

use super::{ExpressionTranspiler, RecordInfo, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::field_name::FieldName;
use crate::dop::r#type::Type;
use crate::hop::component_name::ComponentName;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::BTreeSet;

pub struct GoTranspiler {
    package_name: String,
}

impl GoTranspiler {
    pub fn new(package_name: String) -> Self {
        Self { package_name }
    }

    fn scan_for_imports(imports: &mut BTreeSet<String>, entrypoint: &IrEntrypoint) {
        // Use visitor pattern to scan statements for imports
        for stmt in &entrypoint.body {
            // Check for HTML escaping in WriteExpr statements
            stmt.traverse(&mut |s| {
                if let IrStatement::WriteExpr { escape: true, .. } = s {
                    imports.insert("html".to_string());
                }
            });

            // Check expressions for other imports (like json, os)
            stmt.traverse(&mut |s| {
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| match expr {
                        IrExpr::JsonEncode { .. } => {
                            imports.insert("encoding/json".to_string());
                        }
                        IrExpr::EnvLookup { .. } => {
                            imports.insert("os".to_string());
                        }
                        _ => {}
                    });
                }
            });
        }
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
            Type::Array(elem) => Self::type_contains_trusted_html(elem),
            _ => false,
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
    fn transpile_module(&self, entrypoints: &[IrEntrypoint], records: &[RecordInfo]) -> String {
        let mut imports = BTreeSet::new();

        // First pass: scan to determine what imports we need
        for entrypoint in entrypoints {
            Self::scan_for_imports(&mut imports, entrypoint);
        }

        // We always need strings.Builder for output
        imports.insert("strings".to_string());

        // Check if we need TrustedHTML type
        let needs_trusted_html = self.scan_for_trusted_html(entrypoints);

        // Build the module content using BoxDoc
        let mut result = BoxDoc::nil();

        // Write package declaration
        result = result
            .append(BoxDoc::text("package "))
            .append(BoxDoc::text(&self.package_name))
            .append(BoxDoc::line())
            .append(BoxDoc::line());

        // Write imports if needed
        if !imports.is_empty() {
            result = result
                .append(BoxDoc::text("import ("))
                .append(
                    BoxDoc::nil()
                        .append(BoxDoc::line())
                        .append(BoxDoc::intersperse(
                            imports.iter().map(|import| {
                                BoxDoc::nil()
                                    .append(BoxDoc::text("\""))
                                    .append(BoxDoc::text(import.as_str()))
                                    .append(BoxDoc::text("\""))
                            }),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(1),
                )
                .append(BoxDoc::text(")"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add TrustedHTML type definition if needed
        if needs_trusted_html {
            result = result
                .append(BoxDoc::text("type TrustedHTML string"))
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

        // Generate record type structs
        for record in records {
            let fields: Vec<_> = record
                .fields
                .iter()
                .map(|(field_name, field_type)| {
                    let pascal_name = field_name.to_pascal_case();
                    BoxDoc::as_string(pascal_name)
                        .append(BoxDoc::text(" "))
                        .append(self.transpile_type(field_type))
                        .append(BoxDoc::text(" `json:\""))
                        .append(BoxDoc::text(field_name.as_str()))
                        .append(BoxDoc::text("\"`"))
                })
                .collect();

            result = result
                .append(BoxDoc::text("type "))
                .append(BoxDoc::text(record.name.as_str()))
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

        // Generate parameter structs for entrypoints that have parameters

        for entrypoint in entrypoints {
            if !entrypoint.parameters.is_empty() {
                let struct_name = format!("{}Params", entrypoint.name.to_pascal_case());

                // Process each parameter
                let fields: Vec<_> = entrypoint
                    .parameters
                    .iter()
                    .map(|(param_name, param_type)| {
                        let field_name = param_name.to_pascal_case();
                        let field_type_doc = self.transpile_type(param_type);

                        BoxDoc::as_string(field_name)
                            .append(BoxDoc::text(" "))
                            .append(field_type_doc)
                            .append(BoxDoc::text(" `json:\""))
                            .append(BoxDoc::text(param_name.as_str()))
                            .append(BoxDoc::text("\"`"))
                    })
                    .collect();

                // Add the parameter struct
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
        for (i, entrypoint) in entrypoints.iter().enumerate() {
            result = result.append(self.transpile_entrypoint(&entrypoint.name, entrypoint));
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

    fn transpile_entrypoint<'a>(
        &self,
        name: &'a ComponentName,
        entrypoint: &'a IrEntrypoint,
    ) -> BoxDoc<'a> {
        let func_name = name.to_pascal_case();

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
        let mut body = Vec::new();

        // Extract parameters into local variables
        for (param_name, _) in &entrypoint.parameters {
            let field_name = param_name.to_pascal_case();
            body.push(
                BoxDoc::nil()
                    .append(BoxDoc::text(param_name.as_str()))
                    .append(BoxDoc::text(" := params."))
                    .append(BoxDoc::as_string(field_name)),
            )
        }

        body.push(BoxDoc::text("var output strings.Builder"));
        body.push(self.transpile_statements(&entrypoint.body));
        body.push(BoxDoc::text("return output.String()"));

        result
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::intersperse(body, BoxDoc::line()))
                    .append(BoxDoc::line())
                    .nest(1),
            )
            .append(BoxDoc::text("}"))
            .append(BoxDoc::line())
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
            // Check if the expression is TrustedHTML - needs conversion to string
            let needs_conversion = matches!(expr.as_type(), Type::TrustedHTML);

            let mut doc = BoxDoc::text("output.WriteString(");
            if needs_conversion {
                doc = doc.append(BoxDoc::text("string("));
            }
            doc = doc.append(self.transpile_expr(expr));
            if needs_conversion {
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
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(1),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"));

        if let Some(else_stmts) = else_body {
            doc = doc
                .append(BoxDoc::text(" else {"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(else_stmts))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"));
        }

        doc
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

impl ExpressionTranspiler for GoTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_field_access<'a>(&self, object: &'a IrExpr, field: &'a FieldName) -> BoxDoc<'a> {
        let field_name = field.to_pascal_case();
        self.transpile_expr(object)
            .append(BoxDoc::text("."))
            .append(BoxDoc::as_string(field_name))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("\"{}\"", self.escape_string(value)))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        BoxDoc::text(if value { "true" } else { "false" })
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
        elem_type: &'a Type,
    ) -> BoxDoc<'a> {
        let type_part = BoxDoc::text("[]").append(self.transpile_type(elem_type));

        type_part
            .append(BoxDoc::text("{"))
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
    }

    fn transpile_record_instantiation<'a>(
        &self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In Go, record instantiation is a struct literal with type name
        BoxDoc::text(record_name)
            .append(BoxDoc::text("{"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::intersperse(
                        fields.iter().map(|(key, value)| {
                            let field_name = key.to_pascal_case();
                            BoxDoc::as_string(field_name)
                                .append(BoxDoc::text(": "))
                                .append(self.transpile_expr(value))
                                .append(BoxDoc::text(","))
                        }),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(1),
            )
            .append(BoxDoc::text("}"))
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
        BoxDoc::text("!(")
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("mustJSONMarshal(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_env_lookup<'a>(&self, key: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("os.Getenv(")
            .append(self.transpile_expr(key))
            .append(BoxDoc::text(")"))
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
            .append(BoxDoc::text(" && "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_or<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" || "))
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

impl TypeTranspiler for GoTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("bool")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_trusted_html_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("TrustedHTML")
    }

    fn transpile_float_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("float64")
    }

    fn transpile_int_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("int")
    }

    fn transpile_array_type<'a>(&self, element_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("[]").append(self.transpile_type(element_type))
    }

    fn transpile_named_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{hop::module_name::ModuleName, ir::test_utils::build_ir_auto};
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(entrypoints: &[IrEntrypoint]) -> String {
        let transpiler = GoTranspiler::new("components".to_string());
        transpiler.transpile_module(entrypoints, &[])
    }

    fn check(entrypoints: &[IrEntrypoint], expected: Expect) {
        // Format before (IR)
        let before = entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        // Format after (Go code)
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
    fn test_json_encode_empty_array_literal() {
        let entrypoints = vec![build_ir_auto("TestMainComp", vec![], |t| {
            t.write_expr(t.json_encode(t.typed_array(Type::String, vec![])), false);
        })];

        // In Go, []string{} is the correct way to declare an empty string slice.
        check(
            &entrypoints,
            expect![[r#"
                -- before --
                TestMainComp() {
                  write_expr(JsonEncode([]))
                }

                -- after --
                package components

                import (
                	"encoding/json"
                	"strings"
                )

                func mustJSONMarshal(v any) string {
                	data, _ := json.Marshal(v)
                	return string(data)
                }

                func TestMainComp() string {
                	var output strings.Builder
                	output.WriteString(mustJSONMarshal([]string{}))
                	return output.String()
                }
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
        let entrypoints = vec![build_ir_auto(
            "TestMainComp",
            vec![("items", Type::Array(Box::new(Type::String)))],
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
    fn test_loop_over_array_literal() {
        let entrypoints = vec![build_ir_auto("TestArrayLiteralLoop", vec![], |t| {
            t.write("<ul>\n");
            t.for_loop(
                "color",
                t.array(vec![t.str("red"), t.str("green"), t.str("blue")]),
                |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("color"));
                    t.write("</li>\n");
                },
            );
            t.write("</ul>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                TestArrayLiteralLoop() {
                  write("<ul>\n")
                  for color in ["red", "green", "blue"] {
                    write("<li>")
                    write_escaped(color)
                    write("</li>\n")
                  }
                  write("</ul>\n")
                }

                -- after --
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
                package components

                import (
                	"html"
                	"strings"
                )

                type TrustedHTML string

                type RenderHtmlParams struct {
                	SafeContent TrustedHTML `json:"safe_content"`
                	UserInput string `json:"user_input"`
                }

                func RenderHtml(params RenderHtmlParams) string {
                	safe_content := params.SafeContent
                	user_input := params.UserInput
                	var output strings.Builder
                	output.WriteString("<div>")
                	output.WriteString(string(safe_content))
                	output.WriteString("</div><div>")
                	output.WriteString(html.EscapeString(user_input))
                	output.WriteString("</div>")
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_env_lookup() {
        let entrypoints = vec![build_ir_auto("TestEnv", vec![], |t| {
            t.write("<div>API URL: ");
            t.write_expr(t.env_lookup(t.str("API_URL")), false);
            t.write("</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                TestEnv() {
                  write("<div>API URL: ")
                  write_expr(EnvLookup("API_URL"))
                  write("</div>\n")
                }

                -- after --
                package components

                import (
                	"os"
                	"strings"
                )

                func TestEnv() string {
                	var output strings.Builder
                	output.WriteString("<div>API URL: ")
                	output.WriteString(os.Getenv("API_URL"))
                	output.WriteString("</div>\n")
                	return output.String()
                }
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
            vec![(
                "user",
                Type::Record {
                    module: ModuleName::new("test".to_string()).unwrap(),
                    name: "User".to_string(),
                    fields: vec![
                        (FieldName::new("name").unwrap(), Type::String),
                        (FieldName::new("age").unwrap(), Type::Int),
                        (FieldName::new("active").unwrap(), Type::Bool),
                    ],
                },
            )],
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

        let transpiler = GoTranspiler::new("components".to_string());
        let output = transpiler.transpile_module(&entrypoints, &records);

        expect![[r#"
            package components

            import (
            	"html"
            	"strings"
            )

            type User struct {
            	Name string `json:"name"`
            	Age int `json:"age"`
            	Active bool `json:"active"`
            }

            type Address struct {
            	Street string `json:"street"`
            	City string `json:"city"`
            }

            type UserProfileParams struct {
            	User User `json:"user"`
            }

            func UserProfile(params UserProfileParams) string {
            	user := params.User
            	var output strings.Builder
            	output.WriteString("<div>")
            	output.WriteString(html.EscapeString(user.Name))
            	output.WriteString("</div>")
            	return output.String()
            }
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

        let transpiler = GoTranspiler::new("components".to_string());
        let output = transpiler.transpile_module(&entrypoints, &records);

        expect![[r#"
            package components

            import (
            	"html"
            	"strings"
            )

            type User struct {
            	Name string `json:"name"`
            	Age int `json:"age"`
            }

            func CreateUser() string {
            	var output strings.Builder
            	output.WriteString("<div>")
            	output.WriteString(html.EscapeString(User{
            		Name: "John",
            		Age: 30,
            	}.Name))
            	output.WriteString("</div>")
            	return output.String()
            }
        "#]]
        .assert_eq(&output);
    }
}
