use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::ir::ast::{IrEntrypointDeclaration, IrExpr, IrForSource, IrModule, IrStatement};

pub struct TsTranspiler {
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
    /// Tracks whether Option type is used during transpilation
    needs_option: bool,
    /// Tracks whether escapeHtml function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether TrustedHTML type is used during transpilation
    needs_trusted_html: bool,
}

impl TsTranspiler {
    pub fn new() -> Self {
        Self {
            use_template_literals: false,
            needs_option: false,
            needs_escape_html: false,
            needs_trusted_html: false,
        }
    }

    fn escape_string(&mut self, s: &str) -> String {
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
    fn quote_string(&mut self, s: &str) -> String {
        if self.use_template_literals {
            format!("`{}`", self.escape_string(s))
        } else {
            format!("\"{}\"", self.escape_string(s))
        }
    }
}

impl Default for TsTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Transpiler for TsTranspiler {
    fn transpile_module(&mut self, module: &IrModule) -> String {
        let entrypoints = &module.entrypoints;
        let records = &module.records;

        // Reset tracking flags for this module
        self.needs_option = false;
        self.needs_escape_html = false;
        self.needs_trusted_html = false;

        let mut result = BoxDoc::nil();

        // Add enum type definitions (namespace-based)
        for enum_def in &module.enums {
            // Generate namespace with tagged union type and constructor functions
            // export namespace Color {
            //     export type Color = { readonly tag: "Red" } | { readonly tag: "Green" };
            //     export function Red(): Color { return { tag: "Red" }; }
            //     export function Green(): Color { return { tag: "Green" }; }
            // }
            result = result
                .append(BoxDoc::text("export namespace "))
                .append(BoxDoc::text(enum_def.name.as_str()))
                .append(BoxDoc::text(" {"))
                .append(BoxDoc::line())
                // Type definition
                .append(BoxDoc::text("    export type "))
                .append(BoxDoc::text(enum_def.name.as_str()))
                .append(BoxDoc::text(" = "))
                .append(BoxDoc::intersperse(
                    enum_def.variants.iter().map(|(variant_name, fields)| {
                        let base = BoxDoc::text("{ readonly tag: \"")
                            .append(BoxDoc::text(variant_name.as_str()))
                            .append(BoxDoc::text("\""));
                        if fields.is_empty() {
                            base.append(BoxDoc::text(" }"))
                        } else {
                            base.append(BoxDoc::intersperse(
                                fields.iter().map(|(field_name, field_type)| {
                                    BoxDoc::text(", readonly ")
                                        .append(BoxDoc::text(field_name.as_str()))
                                        .append(BoxDoc::text(": "))
                                        .append(self.transpile_type(field_type))
                                }),
                                BoxDoc::nil(),
                            ))
                            .append(BoxDoc::text(" }"))
                        }
                    }),
                    BoxDoc::text(" | "),
                ))
                .append(BoxDoc::text(";"))
                .append(BoxDoc::line());

            // Generate constructor function for each variant
            for (variant_name, fields) in &enum_def.variants {
                result = result.append(BoxDoc::line());

                if fields.is_empty() {
                    // Unit variant: no parameters
                    result = result
                        .append(BoxDoc::text("    export function "))
                        .append(BoxDoc::text(variant_name.as_str()))
                        .append(BoxDoc::text("(): "))
                        .append(BoxDoc::text(enum_def.name.as_str()))
                        .append(BoxDoc::text(" {"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("        return { tag: \""))
                        .append(BoxDoc::text(variant_name.as_str()))
                        .append(BoxDoc::text("\" };"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("    }"));
                } else {
                    // Variant with fields: add parameters
                    result = result
                        .append(BoxDoc::text("    export function "))
                        .append(BoxDoc::text(variant_name.as_str()))
                        .append(BoxDoc::text("("))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(field_name, field_type)| {
                                BoxDoc::text(field_name.as_str())
                                    .append(BoxDoc::text(": "))
                                    .append(self.transpile_type(field_type))
                            }),
                            BoxDoc::text(", "),
                        ))
                        .append(BoxDoc::text("): "))
                        .append(BoxDoc::text(enum_def.name.as_str()))
                        .append(BoxDoc::text(" {"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("        return { tag: \""))
                        .append(BoxDoc::text(variant_name.as_str()))
                        .append(BoxDoc::text("\""))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(field_name, _)| {
                                BoxDoc::text(", ").append(BoxDoc::text(field_name.as_str()))
                            }),
                            BoxDoc::nil(),
                        ))
                        .append(BoxDoc::text(" };"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("    }"));
                }
            }

            result = result
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add record type definitions
        if !records.is_empty() {
            for record in records {
                result = result
                    .append(BoxDoc::text("export class "))
                    .append(BoxDoc::text(record.name.as_str()))
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::nil()
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("constructor("))
                            .append(
                                BoxDoc::nil()
                                    .append(BoxDoc::line())
                                    .append(BoxDoc::intersperse(
                                        record.fields.iter().map(|(name, ty)| {
                                            BoxDoc::text("public readonly ")
                                                .append(BoxDoc::text(name.as_str()))
                                                .append(BoxDoc::text(": "))
                                                .append(self.transpile_type(ty))
                                                .append(BoxDoc::text(","))
                                        }),
                                        BoxDoc::line(),
                                    ))
                                    .append(BoxDoc::line())
                                    .nest(4),
                            )
                            .append(BoxDoc::text(") {}"))
                            .append(BoxDoc::line())
                            .nest(4),
                    )
                    .append(BoxDoc::text("}"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }
        }

        result = result.append(BoxDoc::intersperse(
            entrypoints
                .iter()
                .map(|entrypoint| self.transpile_entrypoint(&entrypoint.name, entrypoint)),
            BoxDoc::hardline().append(BoxDoc::hardline()),
        ));

        // Prepend escapeHtml function if needed (after transpilation determined it's used)
        if self.needs_escape_html {
            let escape_fn = BoxDoc::nil()
                .append(BoxDoc::text("function escapeHtml(str: string): string {"))
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
            result = escape_fn.append(result);
        }

        // Prepend Option namespace if needed (after transpilation determined it's used)
        if self.needs_option {
            let option_ns = BoxDoc::nil()
                .append(BoxDoc::text("export namespace Option {"))
                .append(BoxDoc::line())
                .append(BoxDoc::text(
                    "    export type Option<T> = { readonly tag: \"None\" } | { readonly tag: \"Some\", value: T };",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::line())
                .append(BoxDoc::text(
                    "    export function some<T>(value: T): Option<T> {",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::text(
                    "        return { tag: \"Some\", value };",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::text("    }"))
                .append(BoxDoc::line())
                .append(BoxDoc::text(
                    "    export function none<T>(): Option<T> {",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::text(
                    "        return { tag: \"None\" };",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::text("    }"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
            result = option_ns.append(result);
        }

        // Prepend TrustedHTML type if needed (after transpilation determined it's used)
        if self.needs_trusted_html {
            let trusted_html = BoxDoc::nil()
                .append(BoxDoc::text(
                    "type TrustedHTML = string & { readonly __brand: unique symbol };",
                ))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
            result = trusted_html.append(result);
        }

        // Prepend warning header (must be last prepend to appear first in output)
        let warning = BoxDoc::text("// Code generated by the hop compiler. DO NOT EDIT.")
            .append(BoxDoc::line())
            .append(BoxDoc::line());
        result = warning.append(result);

        // Render to string
        let mut buffer = Vec::new();
        result.render(80, &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    fn transpile_entrypoint<'a>(
        &mut self,
        name: &'a TypeName,
        entrypoint: &'a IrEntrypointDeclaration,
    ) -> BoxDoc<'a> {
        let mut result = BoxDoc::text("export function ")
            .append(BoxDoc::text(name.as_ref()))
            .append(BoxDoc::text("("));

        if !entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::intersperse(
                entrypoint.parameters.iter().map(|(name, ty, _)| {
                    BoxDoc::text(name.as_str())
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_type(ty))
                }),
                BoxDoc::text(", "),
            ));
        }

        // Function body
        result
            .append(BoxDoc::text("): string {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("let output: string = \"\";"))
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

impl StatementTranspiler for TsTranspiler {
    fn transpile_write<'a>(&mut self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("output += "))
            .append(BoxDoc::as_string(self.quote_string(content)))
            .append(BoxDoc::text(";"))
    }

    fn transpile_write_expr<'a>(&mut self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            self.needs_escape_html = true;
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
        &mut self,
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
        &mut self,
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        let var_name = var.unwrap_or("_");
        match source {
            IrForSource::Array(array) => BoxDoc::nil()
                .append(BoxDoc::text("for (const "))
                .append(BoxDoc::text(var_name))
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
                .append(BoxDoc::text("}")),
            IrForSource::RangeInclusive { start, end } => BoxDoc::nil()
                .append(BoxDoc::text("for (let "))
                .append(BoxDoc::text(var_name))
                .append(BoxDoc::text(" = "))
                .append(self.transpile_expr(start))
                .append(BoxDoc::text("; "))
                .append(BoxDoc::text(var_name))
                .append(BoxDoc::text(" <= "))
                .append(self.transpile_expr(end))
                .append(BoxDoc::text("; "))
                .append(BoxDoc::text(var_name))
                .append(BoxDoc::text("++) {"))
                .append(
                    BoxDoc::nil()
                        .append(BoxDoc::hardline())
                        .append(self.transpile_statements(body))
                        .append(BoxDoc::hardline())
                        .nest(4),
                )
                .append(BoxDoc::text("}")),
        }
    }

    fn transpile_let_statement<'a>(
        &mut self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        // const [[var]] = [[expr]];
        BoxDoc::text("const ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(";"))
            .append(BoxDoc::hardline())
            .append(self.transpile_statements(body))
    }

    fn transpile_match_statement<'a>(&mut self, match_: &'a Match<Vec<IrStatement>>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                // if ([[subject]]) {
                //   [[true_body_statements]]
                // } else {
                //   [[false_body_statements]]
                // }
                BoxDoc::text("if (")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(") {"))
                    .append(
                        BoxDoc::nil()
                            .append(BoxDoc::hardline())
                            .append(self.transpile_statements(true_body))
                            .append(BoxDoc::hardline())
                            .nest(4),
                    )
                    .append(BoxDoc::text("} else {"))
                    .append(
                        BoxDoc::nil()
                            .append(BoxDoc::hardline())
                            .append(self.transpile_statements(false_body))
                            .append(BoxDoc::hardline())
                            .nest(4),
                    )
                    .append(BoxDoc::text("}"))
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                self.needs_option = true;
                // switch (subject.tag) {
                //   case "Some": {
                //     const [[var_name]] = subject.value;
                //     [[statements]]
                //     break;
                //   }
                //   case "None": {
                //     [[...]]
                //     break;
                //   }
                // }
                let subject_name = subject.0.as_str();
                let some_case = {
                    if let Some(var_name) = some_arm_binding {
                        BoxDoc::text("case \"Some\": {")
                            .append(
                                BoxDoc::hardline()
                                    .append(BoxDoc::text("const "))
                                    .append(BoxDoc::text(var_name.as_str()))
                                    .append(BoxDoc::text(" = "))
                                    .append(BoxDoc::text(subject_name))
                                    .append(BoxDoc::text(".value;"))
                                    .append(BoxDoc::hardline())
                                    .append(self.transpile_statements(some_arm_body))
                                    .append(BoxDoc::hardline())
                                    .append(BoxDoc::text("break;"))
                                    .nest(4),
                            )
                            .append(BoxDoc::hardline())
                            .append(BoxDoc::text("}"))
                    } else {
                        BoxDoc::text("case \"Some\": {")
                            .append(
                                BoxDoc::hardline()
                                    .append(self.transpile_statements(some_arm_body))
                                    .append(BoxDoc::hardline())
                                    .append(BoxDoc::text("break;"))
                                    .nest(4),
                            )
                            .append(BoxDoc::hardline())
                            .append(BoxDoc::text("}"))
                    }
                };

                let none_case = BoxDoc::text("case \"None\": {")
                    .append(
                        BoxDoc::hardline()
                            .append(self.transpile_statements(none_arm_body))
                            .append(BoxDoc::hardline())
                            .append(BoxDoc::text("break;"))
                            .nest(4),
                    )
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"));

                BoxDoc::text("switch (")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(".tag) {"))
                    .append(
                        BoxDoc::hardline()
                            .append(some_case)
                            .append(BoxDoc::hardline())
                            .append(none_case)
                            .nest(4),
                    )
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"))
            }
            Match::Enum { subject, arms } => {
                // switch ([[subject]].tag) {
                //   case "[[variant_name]]": {
                //     const { field: binding, ... } = [[subject]];
                //     [[statements]]
                //     break;
                //   }
                //   [[...]]
                // }
                let subject_name = subject.0.as_str();
                let cases = BoxDoc::intersperse(
                    arms.iter().map(|arm| match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name: _,
                            variant_name,
                        } => {
                            // Generate binding destructuring if there are bindings
                            let bindings_doc = if arm.bindings.is_empty() {
                                BoxDoc::nil()
                            } else {
                                let destructure = BoxDoc::intersperse(
                                    arm.bindings.iter().map(|(field, var)| {
                                        BoxDoc::text(field.as_str())
                                            .append(BoxDoc::text(": "))
                                            .append(BoxDoc::text(var.as_str()))
                                    }),
                                    BoxDoc::text(", "),
                                );
                                BoxDoc::text("const { ")
                                    .append(destructure)
                                    .append(BoxDoc::text(" } = "))
                                    .append(BoxDoc::text(subject_name))
                                    .append(BoxDoc::text(";"))
                                    .append(BoxDoc::hardline())
                            };

                            BoxDoc::text("case \"")
                                .append(BoxDoc::text(variant_name.as_str()))
                                .append(BoxDoc::text("\": {"))
                                .append(
                                    BoxDoc::hardline()
                                        .append(bindings_doc)
                                        .append(self.transpile_statements(&arm.body))
                                        .append(BoxDoc::hardline())
                                        .append(BoxDoc::text("break;"))
                                        .nest(4),
                                )
                                .append(BoxDoc::hardline())
                                .append(BoxDoc::text("}"))
                        }
                    }),
                    BoxDoc::hardline(),
                );

                BoxDoc::text("switch (")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(".tag) {"))
                    .append(BoxDoc::hardline().append(cases).nest(4))
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"))
            }
        }
    }

    fn transpile_statements<'a>(&mut self, statements: &'a [IrStatement]) -> BoxDoc<'a> {
        let mut docs: Vec<BoxDoc<'a>> = Vec::new();
        for stmt in statements {
            docs.push(self.transpile_statement(stmt));
        }
        BoxDoc::intersperse(docs, BoxDoc::hardline())
    }
}

impl ExpressionTranspiler for TsTranspiler {
    fn transpile_var<'a>(&mut self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_field_access<'a>(
        &mut self,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(self.transpile_expr(object))
            .append(BoxDoc::text("."))
            .append(BoxDoc::as_string(field.as_str()))
    }

    fn transpile_string_literal<'a>(&mut self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(self.quote_string(value))
    }

    fn transpile_boolean_literal<'a>(&mut self, value: bool) -> BoxDoc<'a> {
        match value {
            true => BoxDoc::text("true"),
            false => BoxDoc::text("false"),
        }
    }

    fn transpile_float_literal<'a>(&mut self, value: f64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}", value))
    }

    fn transpile_int_literal<'a>(&mut self, value: i64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}", value))
    }

    fn transpile_array_literal<'a>(
        &mut self,
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
        &mut self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        BoxDoc::text("new ")
            .append(BoxDoc::text(record_name))
            .append(BoxDoc::text("("))
            .append(BoxDoc::intersperse(
                fields
                    .iter()
                    .map(|(_key, value)| self.transpile_expr(value)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(")"))
    }

    fn transpile_enum_literal<'a>(
        &mut self,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // Call the namespace constructor function: Color.Red() or Result.Ok(value)
        let base = BoxDoc::text(enum_name)
            .append(BoxDoc::text("."))
            .append(BoxDoc::text(variant_name))
            .append(BoxDoc::text("("));
        if fields.is_empty() {
            base.append(BoxDoc::text(")"))
        } else {
            base.append(BoxDoc::intersperse(
                fields
                    .iter()
                    .map(|(_, field_expr)| self.transpile_expr(field_expr)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(")"))
        }
    }

    fn transpile_string_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_enum_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        // Compare tags directly: left.tag === right.tag
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(".tag === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(".tag)"))
    }

    fn transpile_int_less_than<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_less_than<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
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
        &mut self,
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

    fn transpile_not<'a>(&mut self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("!("))
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_numeric_negation<'a>(&mut self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("-("))
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_concat<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_and<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" && "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_or<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" || "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_add<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_add<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_subtract<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" - "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_subtract<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" - "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_multiply<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" * "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_multiply<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" * "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_option_literal<'a>(
        &mut self,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> BoxDoc<'a> {
        self.needs_option = true;
        match value {
            Some(inner) => {
                // Option.some<T>(value)
                BoxDoc::text("Option.some<")
                    .append(self.transpile_type(inner_type))
                    .append(BoxDoc::text(">("))
                    .append(self.transpile_expr(inner))
                    .append(BoxDoc::text(")"))
            }
            None => {
                // Option.none<T>()
                BoxDoc::text("Option.none<")
                    .append(self.transpile_type(inner_type))
                    .append(BoxDoc::text(">()"))
            }
        }
    }

    fn transpile_match_expr<'a>(&mut self, match_: &'a Match<IrExpr>) -> BoxDoc<'a> {
        match match_ {
            Match::Enum { subject, arms } => {
                // (() => {
                //   switch ([[subject]].tag) {
                //     case "[[variant_name]]": {
                //       const { field: binding } = [[subject]];
                //       return [[body]];
                //     }
                //     [[...]]
                //   }
                // })()
                let subject_name = subject.0.as_str();
                let cases = BoxDoc::intersperse(
                    arms.iter().map(|arm| match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name: _,
                            variant_name,
                        } => {
                            if arm.bindings.is_empty() {
                                BoxDoc::text("case \"")
                                    .append(BoxDoc::text(variant_name.as_str()))
                                    .append(BoxDoc::text("\": return "))
                                    .append(self.transpile_expr(&arm.body))
                                    .append(BoxDoc::text(";"))
                            } else {
                                let destructure = BoxDoc::intersperse(
                                    arm.bindings.iter().map(|(field, var)| {
                                        BoxDoc::text(field.as_str())
                                            .append(BoxDoc::text(": "))
                                            .append(BoxDoc::text(var.as_str()))
                                    }),
                                    BoxDoc::text(", "),
                                );
                                BoxDoc::text("case \"")
                                    .append(BoxDoc::text(variant_name.as_str()))
                                    .append(BoxDoc::text("\": {"))
                                    .append(
                                        BoxDoc::line()
                                            .append(BoxDoc::text("const { "))
                                            .append(destructure)
                                            .append(BoxDoc::text(" } = "))
                                            .append(BoxDoc::text(subject_name))
                                            .append(BoxDoc::text(";"))
                                            .append(BoxDoc::line())
                                            .append(BoxDoc::text("return "))
                                            .append(self.transpile_expr(&arm.body))
                                            .append(BoxDoc::text(";"))
                                            .nest(2),
                                    )
                                    .append(BoxDoc::line())
                                    .append(BoxDoc::text("}"))
                            }
                        }
                    }),
                    BoxDoc::line(),
                );

                let switch_body = BoxDoc::text("switch (")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(".tag) {"))
                    .append(BoxDoc::line().append(cases).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"));

                let iife_close = "})()";

                BoxDoc::text("(() => {")
                    .append(BoxDoc::line())
                    .append(switch_body)
                    .nest(2)
                    .append(BoxDoc::line())
                    .append(BoxDoc::text(iife_close))
                    .group()
            }
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                // subject ? [[true_body]] : [[false_body]]
                BoxDoc::text("(")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" ? "))
                    .append(self.transpile_expr(true_body))
                    .append(BoxDoc::text(" : "))
                    .append(self.transpile_expr(false_body))
                    .append(BoxDoc::text(")"))
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                self.needs_option = true;
                // (() => {
                //   switch (subject.tag) {
                //     case "Some": {
                //       const [[var]] = subject.value;
                //       return [[some_arm]];
                //     }
                //     case "None": return [[none_arm]];
                //   }
                // })()
                let subject_name = subject.0.as_str();
                let some_case = {
                    let body_doc = self.transpile_expr(some_arm_body);
                    if let Some(var_name) = some_arm_binding {
                        BoxDoc::text("case \"Some\": {")
                            .append(
                                BoxDoc::line()
                                    .append(BoxDoc::text("const "))
                                    .append(BoxDoc::text(var_name.as_str()))
                                    .append(BoxDoc::text(" = "))
                                    .append(BoxDoc::text(subject_name))
                                    .append(BoxDoc::text(".value;"))
                                    .append(BoxDoc::line())
                                    .append(BoxDoc::text("return "))
                                    .append(body_doc)
                                    .append(BoxDoc::text(";"))
                                    .nest(2),
                            )
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("}"))
                    } else {
                        // case "Some": return body;
                        BoxDoc::text("case \"Some\": return ")
                            .append(body_doc)
                            .append(BoxDoc::text(";"))
                    }
                };

                let none_case = BoxDoc::text("case \"None\": return ")
                    .append(self.transpile_expr(none_arm_body))
                    .append(BoxDoc::text(";"));

                let cases = BoxDoc::intersperse([some_case, none_case], BoxDoc::line());

                let switch_body = BoxDoc::text("switch (")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(".tag) {"))
                    .append(BoxDoc::line().append(cases).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"));

                BoxDoc::text("(() => {")
                    .append(BoxDoc::line().append(switch_body).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("})()"))
                    .group()
            }
        }
    }

    fn transpile_let<'a>(
        &mut self,
        var: &'a crate::dop::symbols::var_name::VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> BoxDoc<'a> {
        // (() => { const [[var]] = [[value]]; return [[body]]; })()
        BoxDoc::text("(() => {")
            .append(
                BoxDoc::line()
                    .append(BoxDoc::text("const "))
                    .append(BoxDoc::text(var.as_str()))
                    .append(BoxDoc::text(" = "))
                    .append(self.transpile_expr(value))
                    .append(BoxDoc::text(";"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("return "))
                    .append(self.transpile_expr(body))
                    .append(BoxDoc::text(";"))
                    .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("})()"))
    }

    fn transpile_merge_classes<'a>(&mut self, args: &'a [IrExpr]) -> BoxDoc<'a> {
        if args.is_empty() {
            BoxDoc::text("\"\"")
        } else {
            BoxDoc::intersperse(
                args.iter().map(|arg| self.transpile_expr(arg)),
                BoxDoc::text(" + \" \" + "),
            )
        }
    }

    fn transpile_array_length<'a>(&mut self, array: &'a IrExpr) -> BoxDoc<'a> {
        self.transpile_expr(array).append(BoxDoc::text(".length"))
    }

    fn transpile_array_is_empty<'a>(&mut self, array: &'a IrExpr) -> BoxDoc<'a> {
        self.transpile_expr(array)
            .append(BoxDoc::text(".length === 0"))
    }

    fn transpile_int_to_string<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(").toString()"))
    }

    fn transpile_float_to_int<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("Math.trunc(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_to_string<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(").toString()"))
    }

    fn transpile_int_to_float<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a> {
        // In JavaScript, all numbers are floats, so no conversion needed
        self.transpile_expr(value)
    }
}

impl TypeTranspiler for TsTranspiler {
    fn transpile_bool_type<'a>(&mut self) -> BoxDoc<'a> {
        BoxDoc::text("boolean")
    }

    fn transpile_string_type<'a>(&mut self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_trusted_html_type<'a>(&mut self) -> BoxDoc<'a> {
        self.needs_trusted_html = true;
        BoxDoc::text("TrustedHTML")
    }

    fn transpile_float_type<'a>(&mut self) -> BoxDoc<'a> {
        BoxDoc::text("number")
    }

    fn transpile_int_type<'a>(&mut self) -> BoxDoc<'a> {
        BoxDoc::text("number")
    }

    fn transpile_array_type<'a>(&mut self, element_type: &'a Type) -> BoxDoc<'a> {
        self.transpile_type(element_type).append(BoxDoc::text("[]"))
    }

    fn transpile_option_type<'a>(&mut self, inner_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("Option.Option<")
            .append(self.transpile_type(inner_type))
            .append(BoxDoc::text(">"))
    }

    fn transpile_named_type<'a>(&mut self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_enum_type<'a>(&mut self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
            .append(BoxDoc::text("."))
            .append(BoxDoc::text(name))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::ir::syntax::builder::IrModuleBuilder;
    use expect_test::{Expect, expect};

    fn check(module: IrModule, expected: Expect) {
        let before = module.to_string();
        let after = TsTranspiler::new().transpile_module(&module);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        check(
            IrModuleBuilder::new()
                .component_no_params("HelloWorld", |t| {
                    t.write("<h1>Hello, World!</h1>\n");
                })
                .build(),
            expect![[r#"
                -- before --
                HelloWorld() {
                  write("<h1>Hello, World!</h1>\n")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function HelloWorld(): string {
                    let output: string = "";
                    output += "<h1>Hello, World!</h1>\n";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn component_with_params_and_escaping() {
        check(
            IrModuleBuilder::new()
                .component(
                    "UserInfo",
                    [("name", Type::String), ("age", Type::String)],
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
                )
                .build(),
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

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function UserInfo(name: string, age: string): string {
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
                }"#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            IrModuleBuilder::new()
                .component(
                    "ConditionalDisplay",
                    [("title", Type::String), ("show", Type::Bool)],
                    |t| {
                        t.if_stmt(t.var("show"), |t| {
                            t.write("<h1>");
                            t.write_expr_escaped(t.var("title"));
                            t.write("</h1>\n");
                        });
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                ConditionalDisplay(title: String, show: Bool) {
                  if show {
                    write("<h1>")
                    write_escaped(title)
                    write("</h1>\n")
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function ConditionalDisplay(title: string, show: boolean): string {
                    let output: string = "";
                    if (show) {
                        output += "<h1>";
                        output += escapeHtml(title);
                        output += "</h1>\n";
                    }
                    return output;
                }"#]],
        );
    }

    #[test]
    fn for_loop_with_array() {
        check(
            IrModuleBuilder::new()
                .component(
                    "ListItems",
                    [("items", Type::Array(Arc::new(Type::String)))],
                    |t| {
                        t.write("<ul>\n");
                        t.for_loop("item", t.var("items"), |t| {
                            t.write("<li>");
                            t.write_expr_escaped(t.var("item"));
                            t.write("</li>\n");
                        });
                        t.write("</ul>\n");
                    },
                )
                .build(),
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

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function ListItems(items: string[]): string {
                    let output: string = "";
                    output += "<ul>\n";
                    for (const item of items) {
                        output += "<li>";
                        output += escapeHtml(item);
                        output += "</li>\n";
                    }
                    output += "</ul>\n";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn for_loop_with_range() {
        check(
            IrModuleBuilder::new()
                .component_no_params("Counter", |t| {
                    t.for_range("i", t.int(1), t.int(3), |t| {
                        t.write_expr(t.int_to_string(t.var("i")), false);
                        t.write(" ");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                Counter() {
                  for i in 1..=3 {
                    write_expr(i.to_string())
                    write(" ")
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function Counter(): string {
                    let output: string = "";
                    for (let i = 1; i <= 3; i++) {
                        output += (i).toString();
                        output += " ";
                    }
                    return output;
                }"#]],
        );
    }

    #[test]
    fn let_binding() {
        check(
            IrModuleBuilder::new()
                .component_no_params("GreetingCard", |t| {
                    t.let_stmt("greeting", t.str("Hello from hop!"), |t| {
                        t.write("<div class=\"card\">\n");
                        t.write("<p>");
                        t.write_expr_escaped(t.var("greeting"));
                        t.write("</p>\n");
                        t.write("</div>\n");
                    });
                })
                .build(),
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

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function GreetingCard(): string {
                    let output: string = "";
                    const greeting = "Hello from hop!";
                    output += "<div class=\"card\">\n";
                    output += "<p>";
                    output += escapeHtml(greeting);
                    output += "</p>\n";
                    output += "</div>\n";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn nested_components_with_let_bindings() {
        check(
            IrModuleBuilder::new()
                .component_no_params("TestMainComp", |t| {
                    t.write("<div data-hop-id=\"test/card-comp\">");
                    t.let_stmt("title", t.str("Hello World"), |t| {
                        t.write("<h2>");
                        t.write_expr_escaped(t.var("title"));
                        t.write("</h2>");
                    });
                    t.write("</div>");
                })
                .build(),
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

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function TestMainComp(): string {
                    let output: string = "";
                    output += "<div data-hop-id=\"test/card-comp\">";
                    const title = "Hello World";
                    output += "<h2>";
                    output += escapeHtml(title);
                    output += "</h2>";
                    output += "</div>";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn trusted_html_type() {
        check(
            IrModuleBuilder::new()
                .component(
                    "RenderHtml",
                    [
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
                )
                .build(),
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
                // Code generated by the hop compiler. DO NOT EDIT.

                type TrustedHTML = string & { readonly __brand: unique symbol };

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function RenderHtml(safe_content: TrustedHTML, user_input: string): string {
                    let output: string = "";
                    output += "<div>";
                    output += safe_content;
                    output += "</div><div>";
                    output += escapeHtml(user_input);
                    output += "</div>";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn record_declarations() {
        use crate::dop::symbols::{field_name::FieldName, type_name::TypeName};
        use crate::hop::symbols::module_id::ModuleId;

        let user_type = Type::Record {
            module: ModuleId::new("test").unwrap(),
            name: TypeName::new("User").unwrap(),
            fields: vec![
                (FieldName::new("name").unwrap(), Arc::new(Type::String)),
                (FieldName::new("age").unwrap(), Arc::new(Type::Int)),
                (FieldName::new("active").unwrap(), Arc::new(Type::Bool)),
            ],
        };

        check(
            IrModuleBuilder::new()
                .record("User", |r| {
                    r.field("name", Type::String)
                        .field("age", Type::Int)
                        .field("active", Type::Bool);
                })
                .record("Address", |r| {
                    r.field("street", Type::String).field("city", Type::String);
                })
                .component("UserProfile", [("user", user_type)], |t| {
                    t.write("<div>");
                    t.write_expr_escaped(t.field_access(t.var("user"), "name"));
                    t.write("</div>");
                })
                .build(),
            expect![[r#"
                -- before --
                record Address {
                  street: String,
                  city: String,
                }
                record User {
                  name: String,
                  age: Int,
                  active: Bool,
                }
                UserProfile(user: test::User) {
                  write("<div>")
                  write_escaped(user.name)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export class Address {
                    constructor(
                        public readonly street: string,
                        public readonly city: string,
                    ) {}
                }

                export class User {
                    constructor(
                        public readonly name: string,
                        public readonly age: number,
                        public readonly active: boolean,
                    ) {}
                }

                export function UserProfile(user: User): string {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml(user.name);
                    output += "</div>";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn record_literal() {
        check(
            IrModuleBuilder::new()
                .record("User", |r| {
                    r.field("name", Type::String).field("age", Type::Int);
                })
                .component_no_params("CreateUser", |t| {
                    t.write("<div>");
                    let user = t.record("User", vec![("name", t.str("John")), ("age", t.int(30))]);
                    t.write_expr_escaped(t.field_access(user, "name"));
                    t.write("</div>");
                })
                .build(),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                  age: Int,
                }
                CreateUser() {
                  write("<div>")
                  write_escaped(User(name: "John", age: 30).name)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export class User {
                    constructor(
                        public readonly name: string,
                        public readonly age: number,
                    ) {}
                }

                export function CreateUser(): string {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml(new User("John", 30).name);
                    output += "</div>";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn enum_type_declarations() {
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_id::ModuleId;

        let color_type = Type::Enum {
            module: ModuleId::new("test").unwrap(),
            name: TypeName::new("Color").unwrap(),
            variants: vec![
                (TypeName::new("Red").unwrap(), vec![]),
                (TypeName::new("Green").unwrap(), vec![]),
                (TypeName::new("Blue").unwrap(), vec![]),
            ],
        };

        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("ColorDisplay", [("color", color_type)], |t| {
                    t.if_stmt(t.eq(t.var("color"), t.enum_variant("Color", "Red")), |t| {
                        t.write("<div>Red!</div>");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                ColorDisplay(color: test::Color) {
                  if (color == Color::Red) {
                    write("<div>Red!</div>")
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Color {
                    export type Color = { readonly tag: "Red" } | { readonly tag: "Green" } | { readonly tag: "Blue" };

                    export function Red(): Color {
                        return { tag: "Red" };
                    }
                    export function Green(): Color {
                        return { tag: "Green" };
                    }
                    export function Blue(): Color {
                        return { tag: "Blue" };
                    }
                }

                export function ColorDisplay(color: Color.Color): string {
                    let output: string = "";
                    if ((color.tag === Color.Red().tag)) {
                        output += "<div>Red!</div>";
                    }
                    return output;
                }"#]],
        );
    }

    #[test]
    fn match_expression() {
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_id::ModuleId;

        let color_type = Type::Enum {
            module: ModuleId::new("test").unwrap(),
            name: TypeName::new("Color").unwrap(),
            variants: vec![
                (TypeName::new("Red").unwrap(), vec![]),
                (TypeName::new("Green").unwrap(), vec![]),
                (TypeName::new("Blue").unwrap(), vec![]),
            ],
        };

        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("ColorName", [("color", color_type)], |t| {
                    // Use match expression to convert color to string
                    let match_result = t.match_expr(
                        t.var("color"),
                        vec![
                            ("Red", t.str("red")),
                            ("Green", t.str("green")),
                            ("Blue", t.str("blue")),
                        ],
                    );
                    t.write_expr_escaped(match_result);
                })
                .build(),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                ColorName(color: test::Color) {
                  write_escaped(match color {
                    Color::Red => "red",
                    Color::Green => "green",
                    Color::Blue => "blue",
                  })
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export namespace Color {
                    export type Color = { readonly tag: "Red" } | { readonly tag: "Green" } | { readonly tag: "Blue" };

                    export function Red(): Color {
                        return { tag: "Red" };
                    }
                    export function Green(): Color {
                        return { tag: "Green" };
                    }
                    export function Blue(): Color {
                        return { tag: "Blue" };
                    }
                }

                export function ColorName(color: Color.Color): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      switch (color.tag) {
                        case "Red": return "red";
                        case "Green": return "green";
                        case "Blue": return "blue";
                      }
                    })());
                    return output;
                }"#]],
        );
    }

    #[test]
    fn bool_match_expression() {
        check(
            IrModuleBuilder::new()
                .component("IsActive", [("active", Type::Bool)], |t| {
                    let match_result =
                        t.bool_match_expr(t.var("active"), t.str("yes"), t.str("no"));
                    t.write_expr_escaped(match_result);
                })
                .build(),
            expect![[r#"
                -- before --
                IsActive(active: Bool) {
                  write_escaped(match active {true => "yes", false => "no"})
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function IsActive(active: boolean): string {
                    let output: string = "";
                    output += escapeHtml((active ? "yes" : "no"));
                    return output;
                }"#]],
        );
    }

    #[test]
    fn option_match_expression() {
        check(
            IrModuleBuilder::new()
                .component(
                    "CheckOption",
                    [("opt", Type::Option(Arc::new(Type::Int)))],
                    |t| {
                        let match_result =
                            t.option_match_expr(t.var("opt"), t.str("has value"), t.str("empty"));
                        t.write_expr_escaped(match_result);
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                CheckOption(opt: Option[Int]) {
                  write_escaped(match opt {
                    Some(_) => "has value",
                    None => "empty",
                  })
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function CheckOption(opt: Option.Option<number>): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      switch (opt.tag) {
                        case "Some": return "has value";
                        case "None": return "empty";
                      }
                    })());
                    return output;
                }"#]],
        );
    }

    #[test]
    fn nested_option_match_expression() {
        let outer_option_type =
            Arc::new(Type::Option(Arc::new(Type::Option(Arc::new(Type::Bool)))));

        check(
            IrModuleBuilder::new()
                .component("CheckNestedOption", [("opt", outer_option_type)], |t| {
                    // Outer match on opt: Some(v0) => middle_match, None => "none"
                    let outer_match = t.option_match_expr_with_binding(
                        t.var("opt"),
                        "v0",
                        Type::Option(Arc::new(Type::Bool)),
                        |t| {
                            // Middle match on v0 (Option[Bool]): Some(v1) => innermost_match, None => "some-none"
                            t.option_match_expr_with_binding(
                                t.var("v0"),
                                "v1",
                                Type::Bool,
                                |t| {
                                    // Inner match on v1 (Bool): true => "true", false => "false"
                                    t.bool_match_expr(
                                        t.var("v1"),
                                        t.str("some-some-true"),
                                        t.str("some-some-false"),
                                    )
                                },
                                t.str("some-none"),
                            )
                        },
                        t.str("none"),
                    );

                    t.write_expr_escaped(outer_match);
                })
                .build(),
            expect![[r#"
                -- before --
                CheckNestedOption(opt: Option[Option[Bool]]) {
                  write_escaped(match opt {
                    Some(v0) => match v0 {
                      Some(v1) => match v1 {
                        true => "some-some-true",
                        false => "some-some-false",
                      },
                      None => "some-none",
                    },
                    None => "none",
                  })
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function CheckNestedOption(opt: Option.Option<Option.Option<boolean>>): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      switch (opt.tag) {
                        case "Some": {
                          const v0 = opt.value;
                          return (() => {
                            switch (v0.tag) {
                              case "Some": {
                                const v1 = v0.value;
                                return (v1 ? "some-some-true" : "some-some-false");
                              }
                              case "None": return "some-none";
                            }
                          })();
                        }
                        case "None": return "none";
                      }
                    })());
                    return output;
                }"#]],
        );
    }

    #[test]
    fn let_expression() {
        check(
            IrModuleBuilder::new()
                .component("LetExpr", [("name", Type::String)], |t| {
                    // let x = name in x
                    let result = t.let_expr("x", t.var("name"), |t| t.var("x"));
                    t.write_expr_escaped(result);
                })
                .build(),
            expect![[r#"
                -- before --
                LetExpr(name: String) {
                  write_escaped(let x = name in x)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function LetExpr(name: string): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      const x = name;
                      return x;
                    })());
                    return output;
                }"#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            IrModuleBuilder::new()
                .component(
                    "DisplayOption",
                    [("opt", Type::Option(Arc::new(Type::String)))],
                    |t| {
                        t.option_match_stmt(
                            t.var("opt"),
                            Some("value"),
                            |t| {
                                t.write("<span>Found: ");
                                t.write_expr_escaped(t.var("value"));
                                t.write("</span>");
                            },
                            |t| {
                                t.write("<span>Nothing</span>");
                            },
                        );
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                DisplayOption(opt: Option[String]) {
                  match opt {
                    Some(value) => {
                      write("<span>Found: ")
                      write_escaped(value)
                      write("</span>")
                    }
                    None => {
                      write("<span>Nothing</span>")
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function DisplayOption(opt: Option.Option<string>): string {
                    let output: string = "";
                    switch (opt.tag) {
                        case "Some": {
                            const value = opt.value;
                            output += "<span>Found: ";
                            output += escapeHtml(value);
                            output += "</span>";
                            break;
                        }
                        case "None": {
                            output += "<span>Nothing</span>";
                            break;
                        }
                    }
                    return output;
                }"#]],
        );
    }

    #[test]
    fn option_literal() {
        check(
            IrModuleBuilder::new()
                .component(
                    "TestOptionLiteral",
                    [
                        ("opt1", Type::Option(Arc::new(Type::String))),
                        ("opt2", Type::Option(Arc::new(Type::String))),
                    ],
                    |t| {
                        // Test Some literal
                        let match_result =
                            t.option_match_expr(t.var("opt1"), t.str("has value"), t.str("empty"));
                        t.write_expr(match_result, false);

                        // Test None literal
                        let match_result2 =
                            t.option_match_expr(t.var("opt2"), t.str("HAS"), t.str("EMPTY"));
                        t.write_expr(match_result2, false);
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                TestOptionLiteral(
                  opt1: Option[String],
                  opt2: Option[String],
                ) {
                  write_expr(match opt1 {
                    Some(_) => "has value",
                    None => "empty",
                  })
                  write_expr(match opt2 {Some(_) => "HAS", None => "EMPTY"})
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                export function TestOptionLiteral(opt1: Option.Option<string>, opt2: Option.Option<string>): string {
                    let output: string = "";
                    output += (() => {
                      switch (opt1.tag) {
                        case "Some": return "has value";
                        case "None": return "empty";
                      }
                    })();
                    output += (() => {
                      switch (opt2.tag) {
                        case "Some": return "HAS";
                        case "None": return "EMPTY";
                      }
                    })();
                    return output;
                }"#]],
        );
    }

    #[test]
    fn option_literal_inline_match_stmt() {
        check(
            IrModuleBuilder::new()
                .component_no_params("TestInlineMatch", |t| {
                    t.let_stmt("opt", t.some(t.str("world")), |t| {
                        t.option_match_stmt(
                            t.var("opt"),
                            Some("val"),
                            |t| {
                                t.write("Got:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write("Empty");
                            },
                        );
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                TestInlineMatch() {
                  let opt = Option[String]::Some("world") in {
                    match opt {
                      Some(val) => {
                        write("Got:")
                        write_expr(val)
                      }
                      None => {
                        write("Empty")
                      }
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                export function TestInlineMatch(): string {
                    let output: string = "";
                    const opt = Option.some<string>("world");
                    switch (opt.tag) {
                        case "Some": {
                            const val = opt.value;
                            output += "Got:";
                            output += val;
                            break;
                        }
                        case "None": {
                            output += "Empty";
                            break;
                        }
                    }
                    return output;
                }"#]],
        );
    }

    #[test]
    fn enum_with_fields() {
        use crate::dop::symbols::field_name::FieldName;
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_id::ModuleId;

        let result_type = Type::Enum {
            module: ModuleId::new("test").unwrap(),
            name: TypeName::new("Result").unwrap(),
            variants: vec![
                (
                    TypeName::new("Ok").unwrap(),
                    vec![(FieldName::new("value").unwrap(), Arc::new(Type::Int))],
                ),
                (
                    TypeName::new("Err").unwrap(),
                    vec![(FieldName::new("message").unwrap(), Arc::new(Type::String))],
                ),
            ],
        };

        check(
            IrModuleBuilder::new()
                .enum_with_fields("Result", |e| {
                    e.variant_with_fields("Ok", vec![("value", Type::Int)]);
                    e.variant_with_fields("Err", vec![("message", Type::String)]);
                })
                .component("ShowResult", [("r", result_type)], |t| {
                    t.write("<div>");
                    // Create an Ok variant with a field value
                    let ok_result =
                        t.enum_variant_with_fields("Result", "Ok", vec![("value", t.int(42))]);
                    t.let_stmt("ok", ok_result, |t| {
                        t.write_expr(t.str("Created Ok!"), false);
                    });
                    t.write("</div>");
                })
                .build(),
            expect![[r#"
                -- before --
                enum Result {
                  Ok(value: Int),
                  Err(message: String),
                }
                ShowResult(r: test::Result) {
                  write("<div>")
                  let ok = Result::Ok(value: 42) in {
                    write_expr("Created Ok!")
                  }
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Result {
                    export type Result = { readonly tag: "Ok", readonly value: number } | { readonly tag: "Err", readonly message: string };

                    export function Ok(value: number): Result {
                        return { tag: "Ok", value };
                    }
                    export function Err(message: string): Result {
                        return { tag: "Err", message };
                    }
                }

                export function ShowResult(r: Result.Result): string {
                    let output: string = "";
                    output += "<div>";
                    const ok = Result.Ok(42);
                    output += "Created Ok!";
                    output += "</div>";
                    return output;
                }"#]],
        );
    }

    #[test]
    fn enum_match_with_field_bindings() {
        use crate::dop::symbols::field_name::FieldName;
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_id::ModuleId;
        use crate::ir::syntax::builder::IrBuilder;

        let result_type = Type::Enum {
            module: ModuleId::new("test").unwrap(),
            name: TypeName::new("Result").unwrap(),
            variants: vec![
                (
                    TypeName::new("Ok").unwrap(),
                    vec![(FieldName::new("value").unwrap(), Arc::new(Type::String))],
                ),
                (
                    TypeName::new("Err").unwrap(),
                    vec![(FieldName::new("message").unwrap(), Arc::new(Type::String))],
                ),
            ],
        };

        check(
            IrModuleBuilder::new()
                .enum_with_fields("Result", |e| {
                    e.variant_with_fields("Ok", vec![("value", Type::String)]);
                    e.variant_with_fields("Err", vec![("message", Type::String)]);
                })
                .component("ShowResult", [("r", result_type)], |t| {
                    t.enum_match_stmt_with_bindings(
                        t.var("r"),
                        vec![
                            (
                                "Ok",
                                vec![("value", "v")],
                                Box::new(|t: &mut IrBuilder| {
                                    t.write("Value: ");
                                    t.write_expr(t.var("v"), false);
                                }),
                            ),
                            (
                                "Err",
                                vec![("message", "m")],
                                Box::new(|t: &mut IrBuilder| {
                                    t.write("Error: ");
                                    t.write_expr(t.var("m"), false);
                                }),
                            ),
                        ],
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                ShowResult(r: test::Result) {
                  match r {
                    Result::Ok(value: v) => {
                      write("Value: ")
                      write_expr(v)
                    }
                    Result::Err(message: m) => {
                      write("Error: ")
                      write_expr(m)
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Result {
                    export type Result = { readonly tag: "Ok", readonly value: string } | { readonly tag: "Err", readonly message: string };

                    export function Ok(value: string): Result {
                        return { tag: "Ok", value };
                    }
                    export function Err(message: string): Result {
                        return { tag: "Err", message };
                    }
                }

                export function ShowResult(r: Result.Result): string {
                    let output: string = "";
                    switch (r.tag) {
                        case "Ok": {
                            const { value: v } = r;
                            output += "Value: ";
                            output += v;
                            break;
                        }
                        case "Err": {
                            const { message: m } = r;
                            output += "Error: ";
                            output += m;
                            break;
                        }
                    }
                    return output;
                }"#]],
        );
    }
}
