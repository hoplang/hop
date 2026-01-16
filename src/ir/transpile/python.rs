use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::VarName;
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::hop::symbols::component_name::ComponentName;
use crate::ir::ast::{IrComponentDeclaration, IrExpr, IrForSource, IrModule, IrStatement};

pub struct PythonTranspiler {}

impl PythonTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn scan_for_imports(&self, entrypoint: &IrComponentDeclaration) -> (bool, bool, bool) {
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

    fn scan_for_trusted_html(&self, entrypoints: &[IrComponentDeclaration]) -> bool {
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

    fn scan_for_options(&self, entrypoint: &IrComponentDeclaration) -> bool {
        // Check parameters for Option types
        for (_, param_type) in &entrypoint.parameters {
            if Self::type_contains_option(param_type) {
                return true;
            }
        }

        // Check statements for Option literals or Option matches
        for stmt in &entrypoint.body {
            let mut has_option = false;
            stmt.traverse(&mut |s| {
                if let IrStatement::Match { match_, .. } = s {
                    if matches!(match_, Match::Option { .. }) {
                        has_option = true;
                    }
                }
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| {
                        if let IrExpr::OptionLiteral { .. } = expr {
                            has_option = true;
                        }
                        if let IrExpr::Match { match_, .. } = expr {
                            if matches!(match_, Match::Option { .. }) {
                                has_option = true;
                            }
                        }
                    });
                }
            });
            if has_option {
                return true;
            }
        }

        false
    }

    fn type_contains_option(t: &Type) -> bool {
        match t {
            Type::Option(_) => true,
            Type::Array(elem) => Self::type_contains_option(elem),
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
    fn transpile_module(&self, module: &IrModule) -> String {
        let entrypoints = &module.components;
        let records = &module.records;

        let mut needs_html_escape = false;
        let mut needs_json = false;
        let mut needs_dataclasses = false;
        let mut needs_os = false;
        let mut needs_optional = false;

        // First pass: scan all entrypoints to determine imports
        for entrypoint in entrypoints {
            let (has_escape, has_json, has_os) = self.scan_for_imports(entrypoint);
            needs_html_escape |= has_escape;
            needs_json |= has_json;
            needs_os |= has_os;
            needs_optional |= self.scan_for_options(entrypoint);
            if !entrypoint.parameters.is_empty() {
                needs_dataclasses = true;
            }
        }

        // Check if we have records to generate
        if !records.is_empty() {
            needs_dataclasses = true;
        }

        // Check if we have enums to generate
        let has_enums = !module.enums.is_empty();
        if has_enums {
            needs_dataclasses = true;
        }

        // Options need dataclasses for the Some wrapper
        if needs_optional {
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

        // Build typing imports list
        let mut typing_imports = Vec::new();
        if needs_trusted_html {
            typing_imports.push("NewType");
        }
        if needs_optional {
            typing_imports.push("Generic");
            typing_imports.push("TypeVar");
        }
        if !typing_imports.is_empty() {
            result = result
                .append(BoxDoc::text("from typing import "))
                .append(BoxDoc::text(typing_imports.join(", ")))
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

        if needs_dataclasses
            || needs_json
            || needs_html_escape
            || needs_trusted_html
            || needs_optional
        {
            result = result.append(BoxDoc::line());
        }

        // Add TrustedHTML type definition if needed
        if needs_trusted_html {
            result = result
                .append(BoxDoc::text("TrustedHTML = NewType('TrustedHTML', str)"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add Option type definitions (Some/Nothing) if needed
        if needs_optional {
            result = result
                .append(BoxDoc::text("T = TypeVar('T')"))
                .append(BoxDoc::line())
                .append(BoxDoc::line())
                .append(BoxDoc::text("@dataclass"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("class Some(Generic[T]):"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("    value: T"))
                .append(BoxDoc::line())
                .append(BoxDoc::line())
                .append(BoxDoc::text("class Nothing:"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("    pass"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Generate enum type definitions (dataclasses + Union type alias)
        for enum_def in &module.enums {
            // Generate a dataclass for each variant
            for (variant_name, fields) in &enum_def.variants {
                let class_name = format!("{}{}", enum_def.name, variant_name.as_str());

                result = result
                    .append(BoxDoc::text("@dataclass"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("class "))
                    .append(BoxDoc::as_string(class_name.clone()))
                    .append(BoxDoc::text(":"));

                // Add field declarations if variant has fields
                if !fields.is_empty() {
                    let field_docs: Vec<BoxDoc> = fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            BoxDoc::text(field_name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(self.transpile_type(field_type))
                        })
                        .collect();
                    result = result.append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(field_docs, BoxDoc::line()))
                            .nest(4),
                    );
                }

                // Add the equals method
                result = result
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("def equals(self, o: \""))
                            .append(BoxDoc::text(enum_def.name.as_str()))
                            .append(BoxDoc::text("\") -> bool:"))
                            .append(
                                BoxDoc::line()
                                    .append(BoxDoc::text("return isinstance(o, "))
                                    .append(BoxDoc::as_string(class_name.clone()))
                                    .append(BoxDoc::text(")"))
                                    .nest(4),
                            )
                            .nest(4),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }

            // Generate Union type alias: EnumName = Variant1 | Variant2 | ...
            let variants: Vec<String> = enum_def
                .variants
                .iter()
                .map(|(v, _)| format!("{}{}", enum_def.name, v.as_str()))
                .collect();
            let union_rhs = variants.join(" | ");

            result = result
                .append(BoxDoc::text(enum_def.name.as_str()))
                .append(BoxDoc::text(" = "))
                .append(BoxDoc::as_string(union_rhs))
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
                        .append(self.transpile_type(field_type))
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
                            .append(self.transpile_type(param_type))
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
        entrypoint: &'a IrComponentDeclaration,
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
            // Check if the expression needs explicit cast to str
            let expr_type = expr.as_type();
            let needs_cast = matches!(expr_type, Type::TrustedHTML | Type::Int);

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
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        let var_name = var.unwrap_or("_");
        match source {
            IrForSource::Array(array) => BoxDoc::text("for ")
                .append(BoxDoc::text(var_name))
                .append(BoxDoc::text(" in "))
                .append(self.transpile_expr(array))
                .append(BoxDoc::text(":"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(body))
                        .nest(4),
                ),
            IrForSource::RangeInclusive { start, end } => BoxDoc::text("for ")
                .append(BoxDoc::text(var_name))
                .append(BoxDoc::text(" in range("))
                .append(self.transpile_expr(start))
                .append(BoxDoc::text(", "))
                .append(self.transpile_expr(end))
                .append(BoxDoc::text(" + 1):"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(body))
                        .nest(4),
                ),
        }
    }

    fn transpile_let_statement<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        // Check if value is an empty array - Python needs type annotation for empty lists
        let needs_type_annotation = matches!(
            value,
            IrExpr::ArrayLiteral { elements, .. } if elements.is_empty()
        );

        let var_doc = if needs_type_annotation {
            if let IrExpr::ArrayLiteral { kind, .. } = value {
                BoxDoc::text(var)
                    .append(BoxDoc::text(": "))
                    .append(self.transpile_type(kind))
            } else {
                BoxDoc::text(var)
            }
        } else {
            BoxDoc::text(var)
        };

        var_doc
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::line())
            .append(self.transpile_statements(body))
    }

    fn transpile_match_statement<'a>(&self, match_: &'a Match<Vec<IrStatement>>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                // Transpile as: if subject: true_body else: false_body
                BoxDoc::text("if ")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(":"))
                    .append(
                        BoxDoc::line()
                            .append(self.transpile_statements(true_body))
                            .nest(4),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("else:"))
                    .append(
                        BoxDoc::line()
                            .append(self.transpile_statements(false_body))
                            .nest(4),
                    )
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                // match [[subject]]:
                //     case Some(value=[[binding]]):
                //         [[some_body]]
                //     case Nothing():
                //         [[none_body]]
                let subject_name = subject.0.as_str();

                // Some case
                let some_pattern = match some_arm_binding {
                    Some(binding) => BoxDoc::text("case Some(value=")
                        .append(BoxDoc::text(binding.as_str()))
                        .append(BoxDoc::text("):")),
                    None => BoxDoc::text("case Some():"),
                };
                let some_case = some_pattern.append(
                    BoxDoc::line()
                        .append(self.transpile_statements(some_arm_body))
                        .nest(4),
                );
                let nothing_case = BoxDoc::text("case Nothing():").append(
                    BoxDoc::line()
                        .append(self.transpile_statements(none_arm_body))
                        .nest(4),
                );
                let cases = some_case.append(BoxDoc::line()).append(nothing_case);
                BoxDoc::text("match ")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(":"))
                    .append(BoxDoc::line().append(cases).nest(4))
            }
            Match::Enum { subject, arms } => {
                // match [[subject]]:
                //     case [[EnumNameVariantName]](field=binding, ...):
                //         [[body]]
                //     ...
                let subject_name = subject.0.as_str();

                // Build all cases
                let cases: Vec<_> = arms
                    .iter()
                    .map(|arm| match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name,
                            variant_name,
                        } => {
                            let class_name = format!("{}{}", enum_name, variant_name);
                            // Generate pattern with field bindings
                            let pattern_args = if arm.bindings.is_empty() {
                                BoxDoc::nil()
                            } else {
                                BoxDoc::intersperse(
                                    arm.bindings.iter().map(|(field, var)| {
                                        BoxDoc::text(field.as_str())
                                            .append(BoxDoc::text("="))
                                            .append(BoxDoc::text(var.as_str()))
                                    }),
                                    BoxDoc::text(", "),
                                )
                            };
                            BoxDoc::text("case ")
                                .append(BoxDoc::as_string(class_name))
                                .append(BoxDoc::text("("))
                                .append(pattern_args)
                                .append(BoxDoc::text("):"))
                                .append(
                                    BoxDoc::line()
                                        .append(self.transpile_statements(&arm.body))
                                        .nest(4),
                                )
                        }
                    })
                    .collect();

                let cases_doc = BoxDoc::intersperse(cases, BoxDoc::line());

                BoxDoc::text("match ")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(":"))
                    .append(BoxDoc::line().append(cases_doc).nest(4))
            }
        }
    }
}

impl ExpressionTranspiler for PythonTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_field_access<'a>(&self, object: &'a IrExpr, field: &'a FieldName) -> BoxDoc<'a> {
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
        _elem_type: &'a Type,
    ) -> BoxDoc<'a> {
        BoxDoc::text("[")
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("]"))
    }

    fn transpile_record_literal<'a>(
        &self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In Python, record literal is a dataclass constructor call
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

    fn transpile_enum_literal<'a>(
        &self,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In Python, enum variants are dataclasses with the pattern EnumNameVariantName()
        let base = BoxDoc::text(enum_name)
            .append(BoxDoc::text(variant_name))
            .append(BoxDoc::text("("));
        if fields.is_empty() {
            base.append(BoxDoc::text(")"))
        } else {
            base.append(BoxDoc::intersperse(
                fields.iter().map(|(field_name, field_expr)| {
                    BoxDoc::text(field_name.as_str())
                        .append(BoxDoc::text("="))
                        .append(self.transpile_expr(field_expr))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(")"))
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
        // Use the equals method defined on each enum variant dataclass
        self.transpile_expr(left)
            .append(BoxDoc::text(".equals("))
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

    fn transpile_option_literal<'a>(
        &self,
        value: Option<&'a IrExpr>,
        _inner_type: &'a Type,
    ) -> BoxDoc<'a> {
        match value {
            Some(inner) => BoxDoc::text("Some(")
                .append(self.transpile_expr(inner))
                .append(BoxDoc::text(")")),
            None => BoxDoc::text("Nothing()"),
        }
    }

    fn transpile_match_expr<'a>(&self, match_: &'a Match<IrExpr>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                // ([[true_body]] if [[subject]] else [[false_body]])
                BoxDoc::text("(")
                    .append(self.transpile_expr(true_body))
                    .append(BoxDoc::text(" if "))
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" else "))
                    .append(self.transpile_expr(false_body))
                    .append(BoxDoc::text(")"))
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let subject_name = subject.0.as_str();
                match some_arm_binding {
                    // ((lambda [[binding]]: [[some_body]])([[subject]].value) if isinstance([[subject]], Some) else [[none_body]])
                    Some(binding) => BoxDoc::text("((lambda ")
                        .append(BoxDoc::text(binding.as_str()))
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_expr(some_arm_body))
                        .append(BoxDoc::text(")("))
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".value) if isinstance("))
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(", Some) else "))
                        .append(self.transpile_expr(none_arm_body))
                        .append(BoxDoc::text(")")),
                    // ([[some_body]] if isinstance([[subject]], Some) else [[none_body]])
                    None => BoxDoc::text("(")
                        .append(self.transpile_expr(some_arm_body))
                        .append(BoxDoc::text(" if isinstance("))
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(", Some) else "))
                        .append(self.transpile_expr(none_arm_body))
                        .append(BoxDoc::text(")")),
                }
            }
            Match::Enum { subject, arms } => {
                // ([[body1]] if isinstance([[subject]], Variant1) else [[body2]] if isinstance([[subject]], Variant2) else [[body3]])
                let subject_name = subject.0.as_str();

                if arms.is_empty() {
                    return BoxDoc::text("None");
                }

                let mut result = BoxDoc::text("(");
                for (i, arm) in arms.iter().enumerate() {
                    match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name,
                            variant_name,
                        } => {
                            let class_name = format!("{}{}", enum_name, variant_name);

                            if i == arms.len() - 1 {
                                // Last arm: just the body (as the final else)
                                result = result.append(self.transpile_expr(&arm.body));
                            } else {
                                // Not last: body if isinstance(...) else
                                result = result
                                    .append(self.transpile_expr(&arm.body))
                                    .append(BoxDoc::text(" if isinstance("))
                                    .append(BoxDoc::text(subject_name))
                                    .append(BoxDoc::text(", "))
                                    .append(BoxDoc::as_string(class_name))
                                    .append(BoxDoc::text(") else "));
                            }
                        }
                    }
                }
                result.append(BoxDoc::text(")"))
            }
        }
    }

    fn transpile_let<'a>(
        &self,
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> BoxDoc<'a> {
        // (lambda [[var]]: [[body]])([[value]])
        BoxDoc::text("(lambda ")
            .append(BoxDoc::text(var.as_str()))
            .append(BoxDoc::text(": "))
            .append(self.transpile_expr(body))
            .append(BoxDoc::text(")("))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_merge_classes<'a>(&self, _left: &'a IrExpr, _right: &'a IrExpr) -> BoxDoc<'a> {
        panic!("MergeClasses is not yet supported in Python transpiler")
    }

    fn transpile_array_length<'a>(&self, array: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("len(")
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_to_string<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("str(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_to_int<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("int(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_to_string<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("str(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_to_float<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("float(")
            .append(self.transpile_expr(value))
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

    fn transpile_array_type<'a>(&self, element_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("list[")
            .append(self.transpile_type(element_type))
            .append(BoxDoc::text("]"))
    }

    fn transpile_option_type<'a>(&self, inner_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("Some[")
            .append(self.transpile_type(inner_type))
            .append(BoxDoc::text("] | Nothing"))
    }

    fn transpile_named_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_enum_type<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::syntax::builder::IrModuleBuilder;
    use expect_test::{Expect, expect};

    fn check(module: IrModule, expected: Expect) {
        let before = module.to_string();
        let after = PythonTranspiler::new().transpile_module(&module);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        check(
            IrModuleBuilder::new()
                .component("TestMainComp", [], |t| {
                    t.write("<div>Hello World</div>\n");
                })
                .build(),
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
    fn component_with_parameters() {
        check(
            IrModuleBuilder::new()
                .component(
                    "TestGreetingComp",
                    [("name", Type::String), ("message", Type::String)],
                    |t| {
                        t.write("<h1>Hello ");
                        t.write_expr_escaped(t.var("name"));
                        t.write(", ");
                        t.write_expr_escaped(t.var("message"));
                        t.write("</h1>\n");
                    },
                )
                .build(),
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
    fn if_condition() {
        check(
            IrModuleBuilder::new()
                .component("TestMainComp", [("show", Type::Bool)], |t| {
                    t.if_stmt(t.var("show"), |t| {
                        t.write("<div>Visible</div>\n");
                    });
                })
                .build(),
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
    fn for_loop() {
        check(
            IrModuleBuilder::new()
                .component(
                    "TestMainComp",
                    [("items", Type::Array(Box::new(Type::String)))],
                    |t| {
                        t.for_loop("item", t.var("items"), |t| {
                            t.write("<li>");
                            t.write_expr_escaped(t.var("item"));
                            t.write("</li>\n");
                        });
                    },
                )
                .build(),
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
    fn for_loop_with_range() {
        check(
            IrModuleBuilder::new()
                .component("Counter", [], |t| {
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
                def counter() -> str:
                    output = []
                    for i in range(1, 3 + 1):
                        output.append(str(i))
                        output.append(" ")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn let_binding() {
        check(
            IrModuleBuilder::new()
                .component("TestGreeting", [], |t| {
                    t.let_stmt("greeting", t.str("Hello from Python!"), |t| {
                        t.write("<p>");
                        t.write_expr_escaped(t.var("greeting"));
                        t.write("</p>\n");
                    });
                })
                .build(),
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
    fn string_comparison() {
        check(
            IrModuleBuilder::new()
                .component(
                    "TestAuthCheck",
                    [("user_role", Type::String), ("expected_role", Type::String)],
                    |t| {
                        t.if_stmt(t.eq(t.var("user_role"), t.var("expected_role")), |t| {
                            t.write("<div>Access granted</div>\n");
                        });
                        t.if_stmt(t.eq(t.var("user_role"), t.str("admin")), |t| {
                            t.write("<div>Admin panel available</div>\n");
                        });
                    },
                )
                .build(),
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
    fn not_operator() {
        check(
            IrModuleBuilder::new()
                .component("TestNot", [("active", Type::Bool)], |t| {
                    t.if_stmt(t.not(t.var("active")), |t| {
                        t.write("<div>Inactive</div>\n");
                    });
                })
                .build(),
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
                        t.write_expr(t.var("safe_content"), false);
                        t.write("</div><div>");
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
    fn record_declarations() {
        use crate::dop::symbols::{field_name::FieldName, type_name::TypeName};
        use crate::hop::symbols::module_name::ModuleName;

        let user_type = Type::Record {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("User").unwrap(),
            fields: vec![
                (FieldName::new("name").unwrap(), Type::String),
                (FieldName::new("age").unwrap(), Type::Int),
                (FieldName::new("active").unwrap(), Type::Bool),
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
                from dataclasses import dataclass
                from html import escape as html_escape

                @dataclass
                class Address:
                    street: str
                    city: str

                @dataclass
                class User:
                    name: str
                    age: int
                    active: bool

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
            "#]],
        );
    }

    #[test]
    fn record_literal() {
        check(
            IrModuleBuilder::new()
                .record("User", |r| {
                    r.field("name", Type::String).field("age", Type::Int);
                })
                .component("CreateUser", [], |t| {
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
            "#]],
        );
    }

    #[test]
    fn enum_type_declarations() {
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_name::ModuleName;

        let color_type = Type::Enum {
            module: ModuleName::new("test").unwrap(),
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
                from dataclasses import dataclass

                @dataclass
                class ColorRed:
                    def equals(self, o: "Color") -> bool:
                        return isinstance(o, ColorRed)

                @dataclass
                class ColorGreen:
                    def equals(self, o: "Color") -> bool:
                        return isinstance(o, ColorGreen)

                @dataclass
                class ColorBlue:
                    def equals(self, o: "Color") -> bool:
                        return isinstance(o, ColorBlue)

                Color = ColorRed | ColorGreen | ColorBlue

                @dataclass
                class ColorDisplayParams:
                    color: Color

                def color_display(params: ColorDisplayParams) -> str:
                    color = params.color
                    output = []
                    if color.equals(ColorRed()):
                        output.append("<div>Red!</div>")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn bool_match_statement() {
        check(
            IrModuleBuilder::new()
                .component("DisplayStatus", [("active", Type::Bool)], |t| {
                    t.bool_match_stmt(
                        t.var("active"),
                        |t| {
                            t.write("<span class=\"active\">Active</span>");
                        },
                        |t| {
                            t.write("<span class=\"inactive\">Inactive</span>");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                DisplayStatus(active: Bool) {
                  match active {
                    true => {
                      write("<span class=\"active\">Active</span>")
                    }
                    false => {
                      write("<span class=\"inactive\">Inactive</span>")
                    }
                  }
                }

                -- after --
                from dataclasses import dataclass

                @dataclass
                class DisplayStatusParams:
                    active: bool

                def display_status(params: DisplayStatusParams) -> str:
                    active = params.active
                    output = []
                    if active:
                        output.append("<span class=\"active\">Active</span>")
                    else:
                        output.append("<span class=\"inactive\">Inactive</span>")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn nested_bool_match_expr() {
        check(
            IrModuleBuilder::new()
                .component("TestNestedBoolMatch", [], |t| {
                    t.let_stmt("outer", t.bool(true), |t| {
                        t.let_stmt("inner", t.bool(false), |t| {
                            let result = t.bool_match_expr(
                                t.var("outer"),
                                t.bool_match_expr(t.var("inner"), t.str("TT"), t.str("TF")),
                                t.str("F"),
                            );
                            t.write_expr(result, false);
                        });
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                TestNestedBoolMatch() {
                  let outer = true in {
                    let inner = false in {
                      write_expr(match outer {
                        true => match inner {true => "TT", false => "TF"},
                        false => "F",
                      })
                    }
                  }
                }

                -- after --
                def test_nested_bool_match() -> str:
                    output = []
                    outer = True
                    inner = False
                    output.append((("TT" if inner else "TF") if outer else "F"))
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn nested_option_match_expr() {
        check(
            IrModuleBuilder::new()
                .component("TestNestedOptionMatch", [], |t| {
                    t.let_stmt("nested", t.some(t.some(t.str("deep"))), |t| {
                        let result = t.option_match_expr_with_binding(
                            t.var("nested"),
                            "outer",
                            Type::Option(Box::new(Type::String)),
                            |t| {
                                t.option_match_expr_with_binding(
                                    t.var("outer"),
                                    "inner",
                                    Type::String,
                                    |t| t.var("inner"),
                                    t.str("inner-none"),
                                )
                            },
                            t.str("outer-none"),
                        );
                        t.write_expr(result, false);
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                TestNestedOptionMatch() {
                  let nested = Some(Some("deep")) in {
                    write_expr(match nested {
                      Some(outer) => match outer {
                        Some(inner) => inner,
                        None => "inner-none",
                      },
                      None => "outer-none",
                    })
                  }
                }

                -- after --
                from dataclasses import dataclass
                from typing import Generic, TypeVar

                T = TypeVar('T')

                @dataclass
                class Some(Generic[T]):
                    value: T

                class Nothing:
                    pass

                def test_nested_option_match() -> str:
                    output = []
                    nested = Some(Some("deep"))
                    output.append(((lambda outer: ((lambda inner: inner)(outer.value) if isinstance(outer, Some) else "inner-none"))(nested.value) if isinstance(nested, Some) else "outer-none"))
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            IrModuleBuilder::new()
                .component("TestOption", [], |t| {
                    t.let_stmt("some_val", t.some(t.str("hello")), |t| {
                        t.option_match_stmt(
                            t.var("some_val"),
                            Some("val"),
                            |t| {
                                t.write("Some:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write("None");
                            },
                        );
                    });
                    t.let_stmt("none_val", t.none(Type::String), |t| {
                        t.option_match_stmt(
                            t.var("none_val"),
                            Some("val"),
                            |t| {
                                t.write(",Some:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write(",None");
                            },
                        );
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                TestOption() {
                  let some_val = Some("hello") in {
                    match some_val {
                      Some(val) => {
                        write("Some:")
                        write_expr(val)
                      }
                      None => {
                        write("None")
                      }
                    }
                  }
                  let none_val = None in {
                    match none_val {
                      Some(val) => {
                        write(",Some:")
                        write_expr(val)
                      }
                      None => {
                        write(",None")
                      }
                    }
                  }
                }

                -- after --
                from dataclasses import dataclass
                from typing import Generic, TypeVar

                T = TypeVar('T')

                @dataclass
                class Some(Generic[T]):
                    value: T

                class Nothing:
                    pass

                def test_option() -> str:
                    output = []
                    some_val = Some("hello")
                    match some_val:
                        case Some(value=val):
                            output.append("Some:")
                            output.append(val)
                        case Nothing():
                            output.append("None")
                    none_val = Nothing()
                    match none_val:
                        case Some(value=val):
                            output.append(",Some:")
                            output.append(val)
                        case Nothing():
                            output.append(",None")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn enum_with_fields() {
        use crate::dop::symbols::field_name::FieldName;
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_name::ModuleName;

        let result_type = Type::Enum {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Result").unwrap(),
            variants: vec![
                (
                    TypeName::new("Ok").unwrap(),
                    vec![(FieldName::new("value").unwrap(), Type::Int)],
                ),
                (
                    TypeName::new("Err").unwrap(),
                    vec![(FieldName::new("message").unwrap(), Type::String)],
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
                from dataclasses import dataclass

                @dataclass
                class ResultOk:
                    value: int
                    def equals(self, o: "Result") -> bool:
                        return isinstance(o, ResultOk)

                @dataclass
                class ResultErr:
                    message: str
                    def equals(self, o: "Result") -> bool:
                        return isinstance(o, ResultErr)

                Result = ResultOk | ResultErr

                @dataclass
                class ShowResultParams:
                    r: Result

                def show_result(params: ShowResultParams) -> str:
                    r = params.r
                    output = []
                    output.append("<div>")
                    ok = ResultOk(value=42)
                    output.append("Created Ok!")
                    output.append("</div>")
                    return ''.join(output)
            "#]],
        );
    }

    #[test]
    fn enum_match_with_field_bindings() {
        use crate::dop::symbols::field_name::FieldName;
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_name::ModuleName;
        use crate::ir::syntax::builder::IrBuilder;

        let result_type = Type::Enum {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Result").unwrap(),
            variants: vec![
                (
                    TypeName::new("Ok").unwrap(),
                    vec![(FieldName::new("value").unwrap(), Type::String)],
                ),
                (
                    TypeName::new("Err").unwrap(),
                    vec![(FieldName::new("message").unwrap(), Type::String)],
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
                from dataclasses import dataclass

                @dataclass
                class ResultOk:
                    value: str
                    def equals(self, o: "Result") -> bool:
                        return isinstance(o, ResultOk)

                @dataclass
                class ResultErr:
                    message: str
                    def equals(self, o: "Result") -> bool:
                        return isinstance(o, ResultErr)

                Result = ResultOk | ResultErr

                @dataclass
                class ShowResultParams:
                    r: Result

                def show_result(params: ShowResultParams) -> str:
                    r = params.r
                    output = []
                    match r:
                        case ResultOk(value=v):
                            output.append("Value: ")
                            output.append(v)
                        case ResultErr(message=m):
                            output.append("Error: ")
                            output.append(m)
                    return ''.join(output)
            "#]],
        );
    }
}
