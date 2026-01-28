use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::var_name::VarName;
use crate::hop::symbols::component_name::ComponentName;
use crate::ir::ast::{IrEntrypointDeclaration, IrExpr, IrForSource, IrModule, IrStatement};

pub struct RustTranspiler {}

impl RustTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn scan_for_escape_html(&self, entrypoint: &IrEntrypointDeclaration) -> bool {
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

    fn scan_for_trusted_html(&self, entrypoints: &[IrEntrypointDeclaration]) -> bool {
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
            Type::Option(inner) => Self::type_contains_trusted_html(inner),
            Type::Record { fields, .. } => {
                fields.iter().any(|(_, ty)| Self::type_contains_trusted_html(ty))
            }
            _ => false,
        }
    }

    fn escape_string(&self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    /// Transpile a type for use in function parameters (uses references without explicit lifetimes)
    fn transpile_param_type<'a>(&self, t: &'a Type) -> BoxDoc<'a> {
        match t {
            Type::Bool => BoxDoc::text("bool"),
            Type::String => BoxDoc::text("&str"),
            Type::Float => BoxDoc::text("f64"),
            Type::Int => BoxDoc::text("i64"),
            Type::TrustedHTML => BoxDoc::text("&TrustedHTML"),
            Type::Array(elem) => BoxDoc::text("&[")
                .append(self.transpile_type(elem))
                .append(BoxDoc::text("]")),
            Type::Option(inner) => BoxDoc::text("Option<")
                .append(self.transpile_param_type(inner))
                .append(BoxDoc::text(">")),
            Type::Record { name, .. } => BoxDoc::text("&").append(BoxDoc::text(name.as_str())),
            Type::Enum { name, .. } => BoxDoc::text("&").append(BoxDoc::text(name.as_str())),
            Type::Component { name, .. } => BoxDoc::text("&").append(BoxDoc::text(name.as_str())),
        }
    }
}

impl Default for RustTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Transpiler for RustTranspiler {
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

        // Add TrustedHTML type definition if needed
        if needs_trusted_html {
            result = result
                .append(BoxDoc::text("#[derive(Clone, Debug)]"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("pub struct TrustedHTML(pub String);"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add escape_html helper function if needed
        if needs_escape_html {
            result = result
                .append(BoxDoc::text("fn escape_html(s: &str) -> String {"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("    s.replace('&', \"&amp;\")"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("        .replace('<', \"&lt;\")"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("        .replace('>', \"&gt;\")"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("        .replace('\"', \"&quot;\")"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("        .replace('\\'', \"&#39;\")"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add enum type definitions
        for enum_def in &module.enums {
            result = result
                .append(BoxDoc::text("#[derive(Clone, Debug)]"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("pub enum "))
                .append(BoxDoc::text(enum_def.name.as_str()))
                .append(BoxDoc::text(" {"))
                .append(BoxDoc::line());

            for (variant_name, fields) in &enum_def.variants {
                result = result.append(BoxDoc::text("    "));
                if fields.is_empty() {
                    result = result
                        .append(BoxDoc::text(variant_name.as_str()))
                        .append(BoxDoc::text(","));
                } else {
                    result = result
                        .append(BoxDoc::text(variant_name.as_str()))
                        .append(BoxDoc::text(" { "));

                    let field_docs: Vec<BoxDoc> = fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            BoxDoc::text(field_name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(self.transpile_type(field_type))
                        })
                        .collect();

                    result = result
                        .append(BoxDoc::intersperse(field_docs, BoxDoc::text(", ")))
                        .append(BoxDoc::text(" },"));
                }
                result = result.append(BoxDoc::line());
            }

            result = result
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add record struct definitions
        for record in records {
            result = result
                .append(BoxDoc::text("#[derive(Clone, Debug)]"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("pub struct "))
                .append(BoxDoc::text(record.name.as_str()))
                .append(BoxDoc::text(" {"))
                .append(BoxDoc::line());

            for (field_name, field_type) in &record.fields {
                result = result
                    .append(BoxDoc::text("    pub "))
                    .append(BoxDoc::text(field_name.as_str()))
                    .append(BoxDoc::text(": "))
                    .append(self.transpile_type(field_type))
                    .append(BoxDoc::text(","))
                    .append(BoxDoc::line());
            }

            result = result
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
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

        // Ensure file ends with a newline
        if !output.ends_with('\n') {
            format!("{}\n", output)
        } else {
            output
        }
    }

    fn transpile_entrypoint<'a>(
        &self,
        name: &'a ComponentName,
        entrypoint: &'a IrEntrypointDeclaration,
    ) -> BoxDoc<'a> {
        let func_name = name.to_snake_case();

        let mut result = BoxDoc::text("pub fn ").append(BoxDoc::as_string(func_name.clone()));

        if entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::text("() -> String {"));
        } else {
            result = result
                .append(BoxDoc::text("("))
                .append(BoxDoc::intersperse(
                    entrypoint.parameters.iter().map(|(param_name, param_type)| {
                        BoxDoc::text(param_name.as_str())
                            .append(BoxDoc::text(": "))
                            .append(self.transpile_param_type(param_type))
                    }),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(") -> String {"));
        }

        // Build function body
        let mut body = BoxDoc::nil();

        // Initialize output string
        body = body
            .append(BoxDoc::text("let mut output = String::new();"))
            .append(BoxDoc::hardline());

        // Transpile body statements
        body = body
            .append(self.transpile_statements(&entrypoint.body))
            .append(BoxDoc::hardline());

        // Return output
        body = body.append(BoxDoc::text("output"));

        result
            .append(BoxDoc::hardline().append(body).nest(4))
            .append(BoxDoc::hardline())
            .append(BoxDoc::text("}"))
            .append(BoxDoc::hardline())
    }
}

impl StatementTranspiler for RustTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::text("output.push_str(\"")
            .append(BoxDoc::text(self.escape_string(content)))
            .append(BoxDoc::text("\");"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        let expr_type = expr.as_type();

        if escape {
            // HTML escaping needed
            match expr_type {
                Type::String => BoxDoc::text("output.push_str(&escape_html(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text("));")),
                Type::Int => BoxDoc::text("output.push_str(&escape_html(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text(".to_string()));")),
                _ => BoxDoc::text("output.push_str(&escape_html(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text("));")),
            }
        } else {
            // No escaping needed
            match expr_type {
                Type::String => BoxDoc::text("output.push_str(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text(");")),
                Type::TrustedHTML => BoxDoc::text("output.push_str(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text(".0);")),
                Type::Int => BoxDoc::text("output.push_str(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text(".to_string());")),
                _ => BoxDoc::text("output.push_str(&")
                    .append(self.transpile_expr(expr))
                    .append(BoxDoc::text(");")),
            }
        }
    }

    fn transpile_if<'a>(
        &self,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
        else_body: Option<&'a [IrStatement]>,
    ) -> BoxDoc<'a> {
        let mut doc = BoxDoc::text("if ")
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::hardline()
                    .append(self.transpile_statements(body))
                    .nest(4),
            )
            .append(BoxDoc::hardline())
            .append(BoxDoc::text("}"));

        if let Some(else_stmts) = else_body {
            doc = doc
                .append(BoxDoc::text(" else {"))
                .append(
                    BoxDoc::hardline()
                        .append(self.transpile_statements(else_stmts))
                        .nest(4),
                )
                .append(BoxDoc::hardline())
                .append(BoxDoc::text("}"));
        }

        doc
    }

    fn transpile_for<'a>(
        &self,
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        let var_name = var.unwrap_or("_");

        let mut doc = BoxDoc::text("for ").append(BoxDoc::text(var_name));

        match source {
            IrForSource::Array(array) => {
                doc = doc
                    .append(BoxDoc::text(" in "))
                    .append(self.transpile_expr(array))
                    .append(BoxDoc::text(".iter() {"));
            }
            IrForSource::RangeInclusive { start, end } => {
                doc = doc
                    .append(BoxDoc::text(" in "))
                    .append(self.transpile_expr(start))
                    .append(BoxDoc::text("..="))
                    .append(self.transpile_expr(end))
                    .append(BoxDoc::text(" {"));
            }
        }

        doc.append(
            BoxDoc::hardline()
                .append(self.transpile_statements(body))
                .nest(4),
        )
        .append(BoxDoc::hardline())
        .append(BoxDoc::text("}"))
    }

    fn transpile_let_statement<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text("let ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(";"))
            .append(BoxDoc::hardline())
            .append(self.transpile_statements(body))
    }

    fn transpile_match_statement<'a>(&self, match_: &'a Match<Vec<IrStatement>>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => BoxDoc::text("if ")
                .append(BoxDoc::text(subject.0.as_str()))
                .append(BoxDoc::text(" {"))
                .append(
                    BoxDoc::hardline()
                        .append(self.transpile_statements(true_body))
                        .nest(4),
                )
                .append(BoxDoc::hardline())
                .append(BoxDoc::text("} else {"))
                .append(
                    BoxDoc::hardline()
                        .append(self.transpile_statements(false_body))
                        .nest(4),
                )
                .append(BoxDoc::hardline())
                .append(BoxDoc::text("}")),
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(var) => format!("Some({})", var.as_str()),
                    None => "Some(_)".to_string(),
                };

                let some_arm = BoxDoc::as_string(some_pattern)
                    .append(BoxDoc::text(" => {"))
                    .append(
                        BoxDoc::hardline()
                            .append(self.transpile_statements(some_arm_body))
                            .nest(4),
                    )
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"));

                let none_arm = BoxDoc::text("None => {")
                    .append(
                        BoxDoc::hardline()
                            .append(self.transpile_statements(none_arm_body))
                            .nest(4),
                    )
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"));

                BoxDoc::text("match ")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::hardline()
                            .append(some_arm)
                            .append(BoxDoc::hardline())
                            .append(none_arm)
                            .nest(4),
                    )
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"))
            }
            Match::Enum { subject, arms } => {
                // Extract variant information from the subject's type
                let variants = match subject.1.as_ref() {
                    Type::Enum { variants, .. } => variants,
                    _ => unreachable!("Enum match subject must have Enum type"),
                };

                let arms_doc = BoxDoc::intersperse(
                    arms.iter().map(|arm| {
                        let pattern = match &arm.pattern {
                            EnumPattern::Variant {
                                enum_name,
                                variant_name,
                            } => {
                                if arm.bindings.is_empty() {
                                    // Check if this variant has fields by looking at the type
                                    let has_fields = variants
                                        .iter()
                                        .find(|(name, _)| name.as_str() == variant_name)
                                        .map(|(_, fields)| !fields.is_empty())
                                        .unwrap_or(false);

                                    if has_fields {
                                        format!("{}::{} {{ .. }}", enum_name, variant_name)
                                    } else {
                                        format!("{}::{}", enum_name, variant_name)
                                    }
                                } else {
                                    let bindings: Vec<String> = arm
                                        .bindings
                                        .iter()
                                        .map(|(field, var)| format!("{}: {}", field, var))
                                        .collect();
                                    format!(
                                        "{}::{} {{ {} }}",
                                        enum_name,
                                        variant_name,
                                        bindings.join(", ")
                                    )
                                }
                            }
                        };

                        BoxDoc::as_string(pattern)
                            .append(BoxDoc::text(" => {"))
                            .append(
                                BoxDoc::hardline()
                                    .append(self.transpile_statements(&arm.body))
                                    .nest(4),
                            )
                            .append(BoxDoc::hardline())
                            .append(BoxDoc::text("}"))
                    }),
                    BoxDoc::hardline(),
                );

                BoxDoc::text("match ")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" {"))
                    .append(BoxDoc::hardline().append(arms_doc).nest(4))
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::text("}"))
            }
        }
    }
}

impl TypeTranspiler for RustTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("bool")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("String")
    }

    fn transpile_float_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("f64")
    }

    fn transpile_int_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("i64")
    }

    fn transpile_trusted_html_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("TrustedHTML")
    }

    fn transpile_array_type<'a>(&self, element_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("Vec<")
            .append(self.transpile_type(element_type))
            .append(BoxDoc::text(">"))
    }

    fn transpile_option_type<'a>(&self, inner_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("Option<")
            .append(self.transpile_type(inner_type))
            .append(BoxDoc::text(">"))
    }

    fn transpile_named_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_enum_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }
}

impl ExpressionTranspiler for RustTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_field_access<'a>(&self, object: &'a IrExpr, field: &'a FieldName) -> BoxDoc<'a> {
        self.transpile_expr(object)
            .append(BoxDoc::text("."))
            .append(BoxDoc::text(field.as_str()))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::text("\"")
            .append(BoxDoc::text(self.escape_string(value)))
            .append(BoxDoc::text("\".to_string()"))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        if value {
            BoxDoc::text("true")
        } else {
            BoxDoc::text("false")
        }
    }

    fn transpile_float_literal<'a>(&self, value: f64) -> BoxDoc<'a> {
        // Ensure float literals have a decimal point
        let s = value.to_string();
        if s.contains('.') || s.contains('e') || s.contains('E') {
            BoxDoc::as_string(format!("{}_f64", s))
        } else {
            BoxDoc::as_string(format!("{}.0_f64", s))
        }
    }

    fn transpile_int_literal<'a>(&self, value: i64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}_i64", value))
    }

    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> BoxDoc<'a> {
        if elements.is_empty() {
            BoxDoc::text("Vec::<")
                .append(self.transpile_type(elem_type))
                .append(BoxDoc::text(">::new()"))
        } else {
            BoxDoc::text("vec![")
                .append(BoxDoc::intersperse(
                    elements.iter().map(|e| self.transpile_expr(e)),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text("]"))
        }
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

    fn transpile_enum_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(std::mem::discriminant(&")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(") == std::mem::discriminant(&"))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text("))"))
    }

    fn transpile_int_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
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
        BoxDoc::text("(")
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
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" <= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("!")
            .append(self.transpile_expr(operand))
    }

    fn transpile_numeric_negation<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(-")
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("format!(\"{}{}\", ")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(", "))
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

    fn transpile_record_literal<'a>(
        &self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        if fields.is_empty() {
            BoxDoc::text(record_name).append(BoxDoc::text(" {}"))
        } else {
            BoxDoc::text(record_name)
                .append(BoxDoc::text(" { "))
                .append(BoxDoc::intersperse(
                    fields.iter().map(|(name, value)| {
                        BoxDoc::text(name.as_str())
                            .append(BoxDoc::text(": "))
                            .append(self.transpile_expr(value))
                    }),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(" }"))
        }
    }

    fn transpile_enum_literal<'a>(
        &self,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        if fields.is_empty() {
            BoxDoc::text(enum_name)
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name))
        } else {
            BoxDoc::text(enum_name)
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name))
                .append(BoxDoc::text(" { "))
                .append(BoxDoc::intersperse(
                    fields.iter().map(|(name, value)| {
                        BoxDoc::text(name.as_str())
                            .append(BoxDoc::text(": "))
                            .append(self.transpile_expr(value))
                    }),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(" }"))
        }
    }

    fn transpile_option_literal<'a>(
        &self,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> BoxDoc<'a> {
        match value {
            Some(expr) => BoxDoc::text("Some(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(")")),
            None => BoxDoc::text("None::<")
                .append(self.transpile_type(inner_type))
                .append(BoxDoc::text(">")),
        }
    }

    fn transpile_match_expr<'a>(&self, match_: &'a Match<IrExpr>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => BoxDoc::text("if ")
                .append(BoxDoc::text(subject.0.as_str()))
                .append(BoxDoc::text(" { "))
                .append(self.transpile_expr(true_body))
                .append(BoxDoc::text(" } else { "))
                .append(self.transpile_expr(false_body))
                .append(BoxDoc::text(" }")),
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(var) => format!("Some({})", var.as_str()),
                    None => "Some(_)".to_string(),
                };
                BoxDoc::text("match ")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" { "))
                    .append(BoxDoc::as_string(some_pattern))
                    .append(BoxDoc::text(" => "))
                    .append(self.transpile_expr(some_arm_body))
                    .append(BoxDoc::text(", None => "))
                    .append(self.transpile_expr(none_arm_body))
                    .append(BoxDoc::text(" }"))
            }
            Match::Enum { subject, arms } => {
                // Extract variant information from the subject's type
                let variants = match subject.1.as_ref() {
                    Type::Enum { variants, .. } => variants,
                    _ => unreachable!("Enum match subject must have Enum type"),
                };

                let mut doc = BoxDoc::text("match ")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" { "));

                for (i, arm) in arms.iter().enumerate() {
                    let pattern = match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name,
                            variant_name,
                        } => {
                            if arm.bindings.is_empty() {
                                // Check if this variant has fields by looking at the type
                                let has_fields = variants
                                    .iter()
                                    .find(|(name, _)| name.as_str() == variant_name)
                                    .map(|(_, fields)| !fields.is_empty())
                                    .unwrap_or(false);

                                if has_fields {
                                    format!("{}::{} {{ .. }}", enum_name, variant_name)
                                } else {
                                    format!("{}::{}", enum_name, variant_name)
                                }
                            } else {
                                let bindings: Vec<String> = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, var)| format!("{}: {}", field, var))
                                    .collect();
                                format!(
                                    "{}::{} {{ {} }}",
                                    enum_name,
                                    variant_name,
                                    bindings.join(", ")
                                )
                            }
                        }
                    };

                    doc = doc
                        .append(BoxDoc::as_string(pattern))
                        .append(BoxDoc::text(" => "))
                        .append(self.transpile_expr(&arm.body));

                    if i < arms.len() - 1 {
                        doc = doc.append(BoxDoc::text(", "));
                    }
                }

                doc.append(BoxDoc::text(" }"))
            }
        }
    }

    fn transpile_let<'a>(
        &self,
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::text("{ let ")
            .append(BoxDoc::text(var.as_str()))
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text("; "))
            .append(self.transpile_expr(body))
            .append(BoxDoc::text(" }"))
    }

    fn transpile_merge_classes<'a>(&self, args: &'a [IrExpr]) -> BoxDoc<'a> {
        if args.is_empty() {
            BoxDoc::text("String::new()")
        } else if args.len() == 1 {
            self.transpile_expr(&args[0])
        } else {
            // Join with spaces: [a, " ", b, " ", c].concat()
            BoxDoc::text("[")
                .append(BoxDoc::intersperse(
                    args.iter().map(|arg| {
                        BoxDoc::text("&")
                            .append(self.transpile_expr(arg))
                            .append(BoxDoc::text("[..]"))
                    }),
                    BoxDoc::text(", \" \", "),
                ))
                .append(BoxDoc::text("].concat()"))
        }
    }

    fn transpile_array_length<'a>(&self, array: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(".len() as i64)"))
    }

    fn transpile_int_to_string<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        self.transpile_expr(value).append(BoxDoc::text(".to_string()"))
    }

    fn transpile_float_to_int<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(" as i64)"))
    }

    fn transpile_float_to_string<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        self.transpile_expr(value).append(BoxDoc::text(".to_string()"))
    }

    fn transpile_int_to_float<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(" as f64)"))
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
        let after = RustTranspiler::new().transpile_module(&module);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        check(
            IrModuleBuilder::new()
                .component_no_params("Test", |t| {
                    t.write("<h1>Hello, World!</h1>\n");
                })
                .build(),
            expect![[r#"
                -- before --
                Test() {
                  write("<h1>Hello, World!</h1>\n")
                }

                -- after --
                pub fn test() -> String {
                    let mut output = String::new();
                    output.push_str("<h1>Hello, World!</h1>\n");
                    output
                }
            "#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            IrModuleBuilder::new()
                .component("Test", [("show", Type::Bool)], |t| {
                    t.if_stmt(t.var("show"), |t| {
                        t.write("<h1>Visible</h1>");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                Test(show: Bool) {
                  if show {
                    write("<h1>Visible</h1>")
                  }
                }

                -- after --
                pub fn test(show: bool) -> String {
                    let mut output = String::new();
                    if show {
                        output.push_str("<h1>Visible</h1>");
                    }
                    output
                }
            "#]],
        );
    }

    #[test]
    fn if_else() {
        check(
            IrModuleBuilder::new()
                .component("Test", [("show", Type::Bool)], |t| {
                    t.if_else_stmt(
                        t.var("show"),
                        |t| {
                            t.write("yes");
                        },
                        |t| {
                            t.write("no");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                Test(show: Bool) {
                  if show {
                    write("yes")
                  } else {
                    write("no")
                  }
                }

                -- after --
                pub fn test(show: bool) -> String {
                    let mut output = String::new();
                    if show {
                        output.push_str("yes");
                    } else {
                        output.push_str("no");
                    }
                    output
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_range() {
        check(
            IrModuleBuilder::new()
                .component_no_params("Test", |t| {
                    t.for_range("i", t.int(1), t.int(3), |t| {
                        t.write_expr(t.int_to_string(t.var("i")), false);
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                Test() {
                  for i in 1..=3 {
                    write_expr(i.to_string())
                  }
                }

                -- after --
                pub fn test() -> String {
                    let mut output = String::new();
                    for i in 1_i64..=3_i64 {
                        output.push_str(&i.to_string());
                    }
                    output
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            IrModuleBuilder::new()
                .component("Test", [("opt", Type::Option(Arc::new(Type::String)))], |t| {
                    t.option_match_stmt(
                        t.var("opt"),
                        Some("value"),
                        |t| {
                            t.write("some: ");
                            t.write_expr(t.var("value"), false);
                        },
                        |t| {
                            t.write("none");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                Test(opt: Option[String]) {
                  match opt {
                    Some(value) => {
                      write("some: ")
                      write_expr(value)
                    }
                    None => {
                      write("none")
                    }
                  }
                }

                -- after --
                pub fn test(opt: Option<&str>) -> String {
                    let mut output = String::new();
                    match opt {
                        Some(value) => {
                            output.push_str("some: ");
                            output.push_str(&value);
                        }
                        None => {
                            output.push_str("none");
                        }
                    }
                    output
                }
            "#]],
        );
    }
}
