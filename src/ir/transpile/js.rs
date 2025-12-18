use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::symbols::field_name::FieldName;
#[cfg(test)]
use crate::dop::symbols::type_name::TypeName;
use crate::dop::semantics::r#type::Type;
use crate::hop::symbols::component_name::ComponentName;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrModule, IrStatement};

#[derive(Debug, Clone, Copy)]
pub enum LanguageMode {
    JavaScript,
    TypeScript,
}

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
        let mut needs_escape = false;
        for stmt in &entrypoint.body {
            stmt.traverse(&mut |s| {
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
    fn transpile_module(&self, module: &IrModule) -> String {
        let entrypoints = &module.entrypoints;
        let records = &module.records;

        let mut needs_escape_html = false;
        for entrypoint in entrypoints {
            if self.scan_for_escape_html(entrypoint) {
                needs_escape_html = true;
                break;
            }
        }

        let needs_trusted_html = self.scan_for_trusted_html(entrypoints);

        let mut result = BoxDoc::nil();

        // Add TrustedHTML type definition for TypeScript
        if needs_trusted_html && matches!(self.mode, LanguageMode::TypeScript) {
            result = result
                .append(BoxDoc::text(
                    "type TrustedHTML = string & { readonly __brand: unique symbol };",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add record type definitions for TypeScript
        if matches!(self.mode, LanguageMode::TypeScript) && !records.is_empty() {
            for record in records {
                result = result
                    .append(BoxDoc::text("interface "))
                    .append(BoxDoc::text(record.name.as_str()))
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::nil()
                            .append(BoxDoc::line())
                            .append(BoxDoc::intersperse(
                                record.fields.iter().map(|(name, ty)| {
                                    BoxDoc::text(name.as_str())
                                        .append(BoxDoc::text(": "))
                                        .append(self.transpile_type(ty))
                                        .append(BoxDoc::text(";"))
                                }),
                                BoxDoc::line(),
                            ))
                            .append(BoxDoc::line())
                            .nest(4),
                    )
                    .append(BoxDoc::text("}"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }
        }

        if needs_escape_html {
            result = result
                .append(match self.mode {
                    LanguageMode::JavaScript => BoxDoc::text("function escapeHtml(str) {"),
                    LanguageMode::TypeScript => {
                        BoxDoc::text("function escapeHtml(str: string): string {")
                    }
                })
                .append(
                    BoxDoc::nil()
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("return str"))
                        .append(
                            BoxDoc::nil()
                                .append(BoxDoc::line())
                                .append(BoxDoc::intersperse(
                                    [
                                        BoxDoc::text(".replace(/&/g, '&amp;')"),
                                        BoxDoc::text(".replace(/</g, '&lt;')"),
                                        BoxDoc::text(".replace(/>/g, '&gt;')"),
                                        BoxDoc::text(".replace(/\"/g, '&quot;')"),
                                        BoxDoc::text(".replace(/'/g, '&#39;');"),
                                    ],
                                    BoxDoc::line(),
                                ))
                                .nest(4),
                        )
                        .append(BoxDoc::line())
                        .nest(4),
                )
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        result = result
            .append(BoxDoc::text("export default {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::intersperse(
                        entrypoints.iter().map(|entrypoint| {
                            self.transpile_entrypoint(&entrypoint.name, entrypoint)
                        }),
                        BoxDoc::text(",").append(BoxDoc::hardline()),
                    ))
                    .append(BoxDoc::hardline())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
            .append(BoxDoc::hardline());

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
        // Convert PascalCase to camelCase for JavaScript function name
        let camel_case_name = name.to_camel_case();

        let mut result = BoxDoc::as_string(camel_case_name).append(BoxDoc::text(": ("));

        if !entrypoint.parameters.is_empty() {
            result = result
                .append(BoxDoc::text("{ "))
                .append(BoxDoc::intersperse(
                    entrypoint
                        .parameters
                        .iter()
                        .map(|(name, _)| BoxDoc::text(name.as_str())),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(" }"));

            // Generate TypeScript interface for parameters
            if matches!(self.mode, LanguageMode::TypeScript) {
                result = result
                    .append(BoxDoc::text(": { "))
                    .append(BoxDoc::intersperse(
                        entrypoint.parameters.iter().map(|(name, ty)| {
                            BoxDoc::text(name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(self.transpile_type(ty))
                        }),
                        BoxDoc::text(", "),
                    ))
                    .append(BoxDoc::text(" }"));
            }
        }

        // Function body
        result
            .append(BoxDoc::text(")"))
            .append(if matches!(self.mode, LanguageMode::TypeScript) {
                BoxDoc::text(": string")
            } else {
                BoxDoc::nil()
            })
            .append(BoxDoc::text(" => {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(match self.mode {
                        LanguageMode::JavaScript => BoxDoc::text("let output = \"\";"),
                        LanguageMode::TypeScript => BoxDoc::text("let output: string = \"\";"),
                    })
                    .append(BoxDoc::line())
                    .append(self.transpile_statements(&entrypoint.body))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("return output;"))
                    .append(BoxDoc::line())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
    }
}

impl StatementTranspiler for JsTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("output += "))
            .append(BoxDoc::as_string(self.quote_string(content)))
            .append(BoxDoc::text(";"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            BoxDoc::nil()
                .append(BoxDoc::text("output += escapeHtml("))
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(");"))
        } else {
            BoxDoc::nil()
                .append(BoxDoc::text("output += "))
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(";"))
        }
    }

    fn transpile_if<'a>(
        &self,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
        else_body: Option<&'a [IrStatement]>,
    ) -> BoxDoc<'a> {
        let mut doc = BoxDoc::nil()
            .append(BoxDoc::text("if ("))
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(") {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::hardline())
                    .append(self.transpile_statements(body))
                    .append(BoxDoc::hardline())
                    .nest(4),
            )
            .append(BoxDoc::text("}"));

        if let Some(else_stmts) = else_body {
            doc = doc
                .append(BoxDoc::text(" else {"))
                .append(
                    BoxDoc::nil()
                        .append(BoxDoc::hardline())
                        .append(self.transpile_statements(else_stmts))
                        .append(BoxDoc::hardline())
                        .nest(4),
                )
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
        BoxDoc::nil()
            .append(BoxDoc::text("for (const "))
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" of "))
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(") {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::hardline())
                    .append(self.transpile_statements(body))
                    .append(BoxDoc::hardline())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
    }

    fn transpile_let<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text("const ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(";"))
            .append(BoxDoc::hardline())
            .append(self.transpile_statements(body))
    }
}

impl ExpressionTranspiler for JsTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_field_access<'a>(&self, object: &'a IrExpr, field: &'a FieldName) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(self.transpile_expr(object))
            .append(BoxDoc::text("."))
            .append(BoxDoc::as_string(field.as_str()))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(self.quote_string(value))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        match value {
            true => BoxDoc::text("true"),
            false => BoxDoc::text("false"),
        }
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
        _elem_type: &'a Type,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("["))
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("]"))
    }

    fn transpile_record_literal<'a>(
        &self,
        _record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In JavaScript/TypeScript, record literal is the same as object literal
        BoxDoc::nil()
            .append(BoxDoc::text("{"))
            .append(BoxDoc::intersperse(
                fields.iter().map(|(key, value)| {
                    BoxDoc::text(key.as_str())
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_expr(value))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
    }

    fn transpile_enum_literal<'a>(
        &self,
        _enum_name: &'a str,
        _variant_name: &'a str,
    ) -> BoxDoc<'a> {
        panic!("Enum literal transpilation for JavaScript/TypeScript not yet implemented")
    }

    fn transpile_string_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_enum_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_enum_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
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

    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("!("))
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("JSON.stringify("))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_env_lookup<'a>(&self, key: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("(process?.env?.["))
            .append(self.transpile_expr(key))
            .append(BoxDoc::text("] ?? \"\")"))
    }

    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_and<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" && "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_or<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" || "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_subtract<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" - "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_subtract<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" - "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_multiply<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" * "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_multiply<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" * "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }
}

impl TypeTranspiler for JsTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("boolean")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_trusted_html_type<'a>(&self) -> BoxDoc<'a> {
        match self.mode {
            LanguageMode::TypeScript => BoxDoc::text("TrustedHTML"),
            LanguageMode::JavaScript => BoxDoc::text("string"),
        }
    }

    fn transpile_float_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("number")
    }

    fn transpile_int_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("number")
    }

    fn transpile_array_type<'a>(&self, element_type: &'a Type) -> BoxDoc<'a> {
        self.transpile_type(element_type).append(BoxDoc::text("[]"))
    }

    fn transpile_named_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        hop::symbols::module_name::ModuleName,
        ir::{IrRecord, test_utils::build_ir_auto},
    };
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(entrypoints: &[IrEntrypoint], mode: LanguageMode) -> String {
        let module = IrModule {
            entrypoints: entrypoints.to_vec(),
            records: vec![],
            enums: vec![],
        };
        let transpiler = JsTranspiler::new(mode);
        transpiler.transpile_module(&module)
    }

    fn check(entrypoints: &[IrEntrypoint], expected: Expect) {
        // Format before (IR)
        let before = entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        // Format TypeScript output
        let ts_output = transpile_with_pretty(entrypoints, LanguageMode::TypeScript);

        // Format JavaScript output
        let js_output = transpile_with_pretty(entrypoints, LanguageMode::JavaScript);

        // Create output with before/ts/js format
        let output = format!(
            "-- before --\n{}\n-- ts --\n{}\n-- js --\n{}",
            before, ts_output, js_output
        );

        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        let entrypoints = vec![build_ir_auto("HelloWorld", vec![], |t| {
            t.write("<h1>Hello, World!</h1>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                HelloWorld() {
                  write("<h1>Hello, World!</h1>\n")
                }

                -- ts --
                export default {
                    helloWorld: (): string => {
                        let output: string = "";
                        output += "<h1>Hello, World!</h1>\n";
                        return output;
                    }
                }

                -- js --
                export default {
                    helloWorld: () => {
                        let output = "";
                        output += "<h1>Hello, World!</h1>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn component_with_params_and_escaping() {
        let entrypoints = vec![build_ir_auto(
            "UserInfo",
            vec![("name", Type::String), ("age", Type::String)],
            |t| {
                t.write("<div>\n");
                t.write("<h2>Name: ");
                t.write_expr_escaped(t.var("name"));
                t.write("</h2>\n");
                t.write("<p>Age: ");
                t.write_expr(t.var("age"), false);
                t.write("</p>\n");
                t.write("</div>\n");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                UserInfo(name: String, age: String) {
                  write("<div>\n")
                  write("<h2>Name: ")
                  write_escaped(name)
                  write("</h2>\n")
                  write("<p>Age: ")
                  write_expr(age)
                  write("</p>\n")
                  write("</div>\n")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    userInfo: ({ name, age }: { name: string, age: string }): string => {
                        let output: string = "";
                        output += "<div>\n";
                        output += "<h2>Name: ";
                        output += escapeHtml(name);
                        output += "</h2>\n";
                        output += "<p>Age: ";
                        output += age;
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    userInfo: ({ name, age }) => {
                        let output = "";
                        output += "<div>\n";
                        output += "<h2>Name: ";
                        output += escapeHtml(name);
                        output += "</h2>\n";
                        output += "<p>Age: ";
                        output += age;
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn typescript_with_types() {
        let entrypoints = vec![build_ir_auto(
            "ConditionalDisplay",
            vec![("title", Type::String), ("show", Type::Bool)],
            |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<h1>");
                    t.write_expr_escaped(t.var("title"));
                    t.write("</h1>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                ConditionalDisplay(title: String, show: Bool) {
                  if show {
                    write("<h1>")
                    write_escaped(title)
                    write("</h1>\n")
                  }
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    conditionalDisplay: ({ title, show }: { title: string, show: boolean }): string => {
                        let output: string = "";
                        if (show) {
                            output += "<h1>";
                            output += escapeHtml(title);
                            output += "</h1>\n";
                        }
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    conditionalDisplay: ({ title, show }) => {
                        let output = "";
                        if (show) {
                            output += "<h1>";
                            output += escapeHtml(title);
                            output += "</h1>\n";
                        }
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_array() {
        let entrypoints = vec![build_ir_auto(
            "ListItems",
            vec![("items", Type::Array(Box::new(Type::String)))],
            |t| {
                t.write("<ul>\n");
                t.for_loop("item", t.var("items"), |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("item"));
                    t.write("</li>\n");
                });
                t.write("</ul>\n");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                ListItems(items: Array[String]) {
                  write("<ul>\n")
                  for item in items {
                    write("<li>")
                    write_escaped(item)
                    write("</li>\n")
                  }
                  write("</ul>\n")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    listItems: ({ items }: { items: string[] }): string => {
                        let output: string = "";
                        output += "<ul>\n";
                        for (const item of items) {
                            output += "<li>";
                            output += escapeHtml(item);
                            output += "</li>\n";
                        }
                        output += "</ul>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    listItems: ({ items }) => {
                        let output = "";
                        output += "<ul>\n";
                        for (const item of items) {
                            output += "<li>";
                            output += escapeHtml(item);
                            output += "</li>\n";
                        }
                        output += "</ul>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn let_binding() {
        let entrypoints = vec![build_ir_auto("GreetingCard", vec![], |t| {
            t.let_stmt("greeting", t.str("Hello from hop!"), |t| {
                t.write("<div class=\"card\">\n");
                t.write("<p>");
                t.write_expr_escaped(t.var("greeting"));
                t.write("</p>\n");
                t.write("</div>\n");
            });
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                GreetingCard() {
                  let greeting = "Hello from hop!" in {
                    write("<div class=\"card\">\n")
                    write("<p>")
                    write_escaped(greeting)
                    write("</p>\n")
                    write("</div>\n")
                  }
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    greetingCard: (): string => {
                        let output: string = "";
                        const greeting = "Hello from hop!";
                        output += "<div class=\"card\">\n";
                        output += "<p>";
                        output += escapeHtml(greeting);
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    greetingCard: () => {
                        let output = "";
                        const greeting = "Hello from hop!";
                        output += "<div class=\"card\">\n";
                        output += "<p>";
                        output += escapeHtml(greeting);
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn nested_components_with_let_bindings() {
        let entrypoints = vec![build_ir_auto("TestMainComp", vec![], |t| {
            t.write("<div data-hop-id=\"test/card-comp\">");
            t.let_stmt("title", t.str("Hello World"), |t| {
                t.write("<h2>");
                t.write_expr_escaped(t.var("title"));
                t.write("</h2>");
            });
            t.write("</div>");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                TestMainComp() {
                  write("<div data-hop-id=\"test/card-comp\">")
                  let title = "Hello World" in {
                    write("<h2>")
                    write_escaped(title)
                    write("</h2>")
                  }
                  write("</div>")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: (): string => {
                        let output: string = "";
                        output += "<div data-hop-id=\"test/card-comp\">";
                        const title = "Hello World";
                        output += "<h2>";
                        output += escapeHtml(title);
                        output += "</h2>";
                        output += "</div>";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
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
    fn trusted_html_type() {
        let entrypoints = vec![build_ir_auto(
            "RenderHtml",
            vec![
                ("safe_content", Type::TrustedHTML),
                ("user_input", Type::String),
            ],
            |t| {
                t.write("<div>");
                // TrustedHTML should not be escaped
                t.write_expr(t.var("safe_content"), false);
                t.write("</div><div>");
                // Regular strings should be escaped
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

                -- ts --
                type TrustedHTML = string & { readonly __brand: unique symbol };

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    renderHtml: ({ safe_content, user_input }: { safe_content: TrustedHTML, user_input: string }): string => {
                        let output: string = "";
                        output += "<div>";
                        output += safe_content;
                        output += "</div><div>";
                        output += escapeHtml(user_input);
                        output += "</div>";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    renderHtml: ({ safe_content, user_input }) => {
                        let output = "";
                        output += "<div>";
                        output += safe_content;
                        output += "</div><div>";
                        output += escapeHtml(user_input);
                        output += "</div>";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn record_declarations() {
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
                    module: ModuleName::new("test").unwrap(),
                    name: TypeName::new("User").unwrap(),
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
            IrRecord {
                name: "User".to_string(),
                fields: vec![
                    (FieldName::new("name").unwrap(), Type::String),
                    (FieldName::new("age").unwrap(), Type::Int),
                    (FieldName::new("active").unwrap(), Type::Bool),
                ],
            },
            IrRecord {
                name: "Address".to_string(),
                fields: vec![
                    (FieldName::new("street").unwrap(), Type::String),
                    (FieldName::new("city").unwrap(), Type::String),
                ],
            },
        ];

        let module = IrModule {
            entrypoints,
            records,
            enums: vec![],
        };

        let ts_transpiler = JsTranspiler::new(LanguageMode::TypeScript);
        let ts_output = ts_transpiler.transpile_module(&module);

        let js_transpiler = JsTranspiler::new(LanguageMode::JavaScript);
        let js_output = js_transpiler.transpile_module(&module);

        let output = format!("-- ts --\n{}\n-- js --\n{}", ts_output, js_output);

        expect![[r#"
            -- ts --
            interface User {
                name: string;
                age: number;
                active: boolean;
            }

            interface Address {
                street: string;
                city: string;
            }

            function escapeHtml(str: string): string {
                return str
                    .replace(/&/g, '&amp;')
                    .replace(/</g, '&lt;')
                    .replace(/>/g, '&gt;')
                    .replace(/"/g, '&quot;')
                    .replace(/'/g, '&#39;');
            }

            export default {
                userProfile: ({ user }: { user: User }): string => {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml(user.name);
                    output += "</div>";
                    return output;
                }
            }

            -- js --
            function escapeHtml(str) {
                return str
                    .replace(/&/g, '&amp;')
                    .replace(/</g, '&lt;')
                    .replace(/>/g, '&gt;')
                    .replace(/"/g, '&quot;')
                    .replace(/'/g, '&#39;');
            }

            export default {
                userProfile: ({ user }) => {
                    let output = "";
                    output += "<div>";
                    output += escapeHtml(user.name);
                    output += "</div>";
                    return output;
                }
            }
        "#]]
        .assert_eq(&output);
    }

    #[test]
    fn record_literal() {
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

        let records = vec![IrRecord {
            name: "User".to_string(),
            fields: vec![
                (FieldName::new("name").unwrap(), Type::String),
                (FieldName::new("age").unwrap(), Type::Int),
            ],
        }];

        let module = IrModule {
            entrypoints,
            records,
            enums: vec![],
        };

        let ts_transpiler = JsTranspiler::new(LanguageMode::TypeScript);
        let ts_output = ts_transpiler.transpile_module(&module);

        let js_transpiler = JsTranspiler::new(LanguageMode::JavaScript);
        let js_output = js_transpiler.transpile_module(&module);

        let output = format!("-- ts --\n{}\n-- js --\n{}", ts_output, js_output);

        expect![[r#"
            -- ts --
            interface User {
                name: string;
                age: number;
            }

            function escapeHtml(str: string): string {
                return str
                    .replace(/&/g, '&amp;')
                    .replace(/</g, '&lt;')
                    .replace(/>/g, '&gt;')
                    .replace(/"/g, '&quot;')
                    .replace(/'/g, '&#39;');
            }

            export default {
                createUser: (): string => {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml({name: "John", age: 30}.name);
                    output += "</div>";
                    return output;
                }
            }

            -- js --
            function escapeHtml(str) {
                return str
                    .replace(/&/g, '&amp;')
                    .replace(/</g, '&lt;')
                    .replace(/>/g, '&gt;')
                    .replace(/"/g, '&quot;')
                    .replace(/'/g, '&#39;');
            }

            export default {
                createUser: () => {
                    let output = "";
                    output += "<div>";
                    output += escapeHtml({name: "John", age: 30}.name);
                    output += "</div>";
                    return output;
                }
            }
        "#]]
        .assert_eq(&output);
    }
}
