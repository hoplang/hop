use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::dop::VarName;
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::hop::symbols::component_name::ComponentName;
use crate::ir::ast::{IrEntrypointDeclaration, IrExpr, IrForSource, IrModule, IrStatement};
use std::collections::BTreeSet;

pub struct GoTranspiler {
    package_name: String,
}

impl GoTranspiler {
    pub fn new(package_name: String) -> Self {
        Self { package_name }
    }

    fn scan_for_imports(imports: &mut BTreeSet<String>, entrypoint: &IrEntrypointDeclaration) {
        // Use visitor pattern to scan statements for imports
        for stmt in &entrypoint.body {
            // Check for HTML escaping and Int types in WriteExpr statements
            stmt.traverse(&mut |s| {
                if let IrStatement::WriteExpr { escape, expr, .. } = s {
                    if *escape {
                        imports.insert("html".to_string());
                    }
                    if matches!(expr.as_type(), Type::Int) {
                        imports.insert("strconv".to_string());
                    }
                }
            });

            // Check expressions for other imports (like json, os, strconv)
            stmt.traverse(&mut |s| {
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| match expr {
                        IrExpr::IntToString { .. } => {
                            imports.insert("strconv".to_string());
                        }
                        IrExpr::FloatToString { .. } => {
                            imports.insert("strconv".to_string());
                        }
                        _ => {}
                    });
                }
            });
        }
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
            _ => false,
        }
    }

    fn scan_for_options(&self, module: &IrModule) -> bool {
        for entrypoint in &module.entrypoints {
            // Check parameters
            for (_, param_type) in &entrypoint.parameters {
                if Self::type_contains_option(param_type) {
                    return true;
                }
            }
            // Check statements for option expressions
            for stmt in &entrypoint.body {
                let mut found = false;
                stmt.traverse(&mut |s| {
                    if let Some(expr) = s.expr() {
                        expr.traverse(&mut |e| {
                            if matches!(e, IrExpr::OptionLiteral { .. }) {
                                found = true;
                            }
                            if let IrExpr::Match { match_, .. } = e {
                                if matches!(match_, Match::Option { .. }) {
                                    found = true;
                                }
                            }
                        });
                    }
                    if let IrStatement::Match { match_, .. } = s {
                        if matches!(match_, Match::Option { .. }) {
                            found = true;
                        }
                    }
                });
                if found {
                    return true;
                }
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
    fn transpile_module(&self, module: &IrModule) -> String {
        let entrypoints = &module.entrypoints;
        let records = &module.records;

        let mut imports = BTreeSet::new();

        // First pass: scan to determine what imports we need
        for entrypoint in entrypoints {
            Self::scan_for_imports(&mut imports, entrypoint);
        }

        // We always need io for writing output
        imports.insert("io".to_string());

        // Check if we need TrustedHTML type
        let needs_trusted_html = self.scan_for_trusted_html(entrypoints);

        // Check if we need Option type
        let needs_option = self.scan_for_options(module);

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

        // Add Option type definition if needed
        if needs_option {
            result = result
                .append(BoxDoc::text("type Option[T any] struct {"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("\tvalue T"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("\tsome  bool"))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
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

        // Generate enum type definitions (interface-based)
        for enum_def in &module.enums {
            // Generate: type EnumName interface { Equals(o EnumName) bool }
            result = result
                .append(BoxDoc::text("type "))
                .append(BoxDoc::text(enum_def.name.as_str()))
                .append(BoxDoc::text(" interface {"))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::text("Equals(o "))
                        .append(BoxDoc::text(enum_def.name.as_str()))
                        .append(BoxDoc::text(") bool"))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());

            // Generate a struct and Equals method for each variant
            for (variant_name, fields) in &enum_def.variants {
                let struct_name = format!("{}{}", enum_def.name, variant_name.as_str());

                // type [[EnumNameVariant]] struct { Field Type; ... } or struct{}
                result = result
                    .append(BoxDoc::text("type "))
                    .append(BoxDoc::as_string(struct_name.clone()));

                if fields.is_empty() {
                    result = result.append(BoxDoc::text(" struct{}"));
                } else {
                    let field_docs: Vec<BoxDoc> = fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            let pascal_name = field_name.to_pascal_case();
                            BoxDoc::as_string(pascal_name)
                                .append(BoxDoc::text(" "))
                                .append(self.transpile_type(field_type))
                        })
                        .collect();
                    result = result
                        .append(BoxDoc::text(" struct {"))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(field_docs, BoxDoc::line()))
                                .nest(1),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}"));
                }

                result = result.append(BoxDoc::line()).append(BoxDoc::line());

                // func (e [[EnumNameVariant]]) Equals(o [[EnumName]]) bool {
                //   _, ok := o.([[EnumNameVariant]])
                //   return ok
                // }
                result = result
                    .append(BoxDoc::text("func (e "))
                    .append(BoxDoc::as_string(struct_name.clone()))
                    .append(BoxDoc::text(") Equals(o "))
                    .append(BoxDoc::text(enum_def.name.as_str()))
                    .append(BoxDoc::text(") bool {"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("_, ok := o.("))
                            .append(BoxDoc::as_string(struct_name.clone()))
                            .append(BoxDoc::text(")"))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("return ok"))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }
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
        entrypoint: &'a IrEntrypointDeclaration,
    ) -> BoxDoc<'a> {
        let func_name = name.to_pascal_case();

        let mut result = BoxDoc::text("func ").append(BoxDoc::as_string(func_name.clone()));

        if entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::text("(w io.Writer) {"));
        } else {
            result = result
                .append(BoxDoc::text("(w io.Writer, "))
                .append(BoxDoc::intersperse(
                    entrypoint.parameters.iter().map(|(param_name, param_type)| {
                        BoxDoc::text(param_name.as_str())
                            .append(BoxDoc::text(" "))
                            .append(self.transpile_type(param_type))
                    }),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(") {"));
        }

        // Function body
        let mut body = Vec::new();

        body.push(self.transpile_statements(&entrypoint.body));

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
        BoxDoc::text("io.WriteString(w, \"")
            .append(BoxDoc::text(self.escape_string(content)))
            .append(BoxDoc::text("\")"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            BoxDoc::text("io.WriteString(w, html.EscapeString(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text("))"))
        } else {
            // Check if the expression needs conversion to string
            let expr_type = expr.as_type();
            let needs_trusted_html_conversion = matches!(expr_type, Type::TrustedHTML);
            let needs_int_conversion = matches!(expr_type, Type::Int);

            let mut doc = BoxDoc::text("io.WriteString(w, ");
            if needs_trusted_html_conversion {
                doc = doc.append(BoxDoc::text("string("));
            } else if needs_int_conversion {
                doc = doc.append(BoxDoc::text("strconv.Itoa("));
            }
            doc = doc.append(self.transpile_expr(expr));
            if needs_trusted_html_conversion || needs_int_conversion {
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
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        match (var, source) {
            // Underscore binding for arrays: use `for range items` (Go 1.4+)
            (None, IrForSource::Array(array)) => BoxDoc::text("for range ")
                .append(self.transpile_expr(array))
                .append(BoxDoc::text(" {"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(body))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            // Underscore binding for ranges: use `for range end - start + 1` (Go 1.22+)
            (None, IrForSource::RangeInclusive { start, end }) => BoxDoc::text("for range ")
                .append(self.transpile_expr(end))
                .append(BoxDoc::text(" - "))
                .append(self.transpile_expr(start))
                .append(BoxDoc::text(" + 1 {"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(body))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            // Named binding for arrays
            (Some(var), IrForSource::Array(array)) => BoxDoc::text("for _, ")
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
                .append(BoxDoc::text("}")),
            // Named binding for ranges
            (Some(var), IrForSource::RangeInclusive { start, end }) => BoxDoc::text("for ")
                .append(BoxDoc::text(var))
                .append(BoxDoc::text(" := "))
                .append(self.transpile_expr(start))
                .append(BoxDoc::text("; "))
                .append(BoxDoc::text(var))
                .append(BoxDoc::text(" <= "))
                .append(self.transpile_expr(end))
                .append(BoxDoc::text("; "))
                .append(BoxDoc::text(var))
                .append(BoxDoc::text("++ {"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(body))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
        }
    }

    fn transpile_let_statement<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        // For enum types, use `var x EnumType = value` to ensure interface type
        // This is needed for type switches to work properly
        let decl = if let Type::Enum { name, .. } = value.as_type() {
            BoxDoc::text("var ")
                .append(BoxDoc::text(var))
                .append(BoxDoc::text(" "))
                .append(BoxDoc::text(name.as_str()))
                .append(BoxDoc::text(" = "))
                .append(self.transpile_expr(value))
        } else {
            BoxDoc::text(var)
                .append(BoxDoc::text(" := "))
                .append(self.transpile_expr(value))
        };

        decl.append(BoxDoc::line())
            .append(self.transpile_statements(body))
    }

    fn transpile_match_statement<'a>(&self, match_: &'a Match<Vec<IrStatement>>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                // Transpile as: if subject { true_body } else { false_body }
                BoxDoc::text("if ")
                    .append(BoxDoc::text(subject.0.as_str()))
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::line()
                            .append(self.transpile_statements(true_body))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("} else {"))
                    .append(
                        BoxDoc::line()
                            .append(self.transpile_statements(false_body))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                // if subject.some {
                //     val := subject.value
                //     ...
                // } else {
                //     ...
                // }
                let subject_name = subject.0.as_str();
                let some_body = if let Some(var_name) = some_arm_binding {
                    BoxDoc::text(var_name.as_str())
                        .append(BoxDoc::text(" := "))
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".value"))
                        .append(BoxDoc::line())
                        .append(self.transpile_statements(some_arm_body))
                } else {
                    self.transpile_statements(some_arm_body)
                };

                BoxDoc::text("if ")
                    .append(BoxDoc::text(subject_name))
                    .append(BoxDoc::text(".some {"))
                    .append(BoxDoc::line().append(some_body).nest(1))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("} else {"))
                    .append(
                        BoxDoc::line()
                            .append(self.transpile_statements(none_arm_body))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            Match::Enum { subject, arms } => {
                // switch _v := subject.(type) {
                // case EnumNameVariantName:
                //     binding := _v.Field
                //     [[statements]]
                // [[...]]
                // }
                let subject_name = subject.0.as_str();
                let has_any_bindings = arms.iter().any(|arm| !arm.bindings.is_empty());

                let cases = BoxDoc::intersperse(
                    arms.iter().map(|arm| match &arm.pattern {
                        crate::dop::patterns::EnumPattern::Variant {
                            enum_name,
                            variant_name,
                        } => {
                            let struct_name = format!("{}{}", enum_name, variant_name);
                            // Generate binding assignments
                            let bindings_doc = if arm.bindings.is_empty() {
                                BoxDoc::nil()
                            } else {
                                let binding_stmts: Vec<_> = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, var)| {
                                        let pascal_field = field.to_pascal_case();
                                        BoxDoc::text(var.as_str())
                                            .append(BoxDoc::text(" := _v."))
                                            .append(BoxDoc::as_string(pascal_field))
                                    })
                                    .collect();
                                BoxDoc::intersperse(binding_stmts, BoxDoc::line())
                                    .append(BoxDoc::line())
                            };
                            BoxDoc::text("case ")
                                .append(BoxDoc::text(struct_name))
                                .append(BoxDoc::text(":"))
                                .append(
                                    BoxDoc::line()
                                        .append(bindings_doc)
                                        .append(self.transpile_statements(&arm.body))
                                        .nest(1),
                                )
                        }
                    }),
                    BoxDoc::line(),
                );

                // Use "_v :=" syntax only if there are bindings
                let switch_start = if has_any_bindings {
                    BoxDoc::text("switch _v := ")
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".(type) {"))
                } else {
                    BoxDoc::text("switch ")
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".(type) {"))
                };

                switch_start
                    .append(BoxDoc::line().append(cases).nest(1))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
        }
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

    fn transpile_record_literal<'a>(
        &self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In Go, record literal is a struct literal with type name
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

    fn transpile_enum_literal<'a>(
        &self,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a> {
        // In Go, enum variants are structs with the pattern EnumNameVariantName{}
        let base = BoxDoc::text(enum_name)
            .append(BoxDoc::text(variant_name))
            .append(BoxDoc::text("{"));
        if fields.is_empty() {
            base.append(BoxDoc::text("}"))
        } else {
            base.append(BoxDoc::intersperse(
                fields.iter().map(|(field_name, field_expr)| {
                    // Capitalize field name for Go export
                    let capitalized = {
                        let s = field_name.as_str();
                        let mut c = s.chars();
                        match c.next() {
                            None => String::new(),
                            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
                        }
                    };
                    BoxDoc::as_string(capitalized)
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_expr(field_expr))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
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
        // Use the Equals method on the interface-based enum
        self.transpile_expr(left)
            .append(BoxDoc::text(".Equals("))
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
        BoxDoc::text("!(")
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_numeric_negation<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("-(")
            .append(self.transpile_expr(operand))
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

    fn transpile_option_literal<'a>(
        &self,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> BoxDoc<'a> {
        match value {
            Some(inner) => {
                // Option[T]{value: x, some: true}
                BoxDoc::text("Option[")
                    .append(self.transpile_type(inner_type))
                    .append(BoxDoc::text("]{value: "))
                    .append(self.transpile_expr(inner))
                    .append(BoxDoc::text(", some: true}"))
            }
            None => {
                // Option[T]{some: false}
                BoxDoc::text("Option[")
                    .append(self.transpile_type(inner_type))
                    .append(BoxDoc::text("]{some: false}"))
            }
        }
    }

    fn transpile_match_expr<'a>(&self, match_: &'a Match<IrExpr>) -> BoxDoc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                // func() returnType { if subject { return true_body } else { return false_body } }()
                let return_type = self.transpile_type(true_body.as_type());
                BoxDoc::text("func() ")
                    .append(return_type)
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("if "))
                            .append(BoxDoc::text(subject.0.as_str()))
                            .append(BoxDoc::text(" {"))
                            .append(
                                BoxDoc::line()
                                    .append(BoxDoc::text("return "))
                                    .append(self.transpile_expr(true_body))
                                    .nest(2),
                            )
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("} else {"))
                            .append(
                                BoxDoc::line()
                                    .append(BoxDoc::text("return "))
                                    .append(self.transpile_expr(false_body))
                                    .nest(2),
                            )
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("}"))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}()"))
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                // func() ReturnType {
                //     if subject.some {
                //         val := subject.value
                //         return some_body
                //     } else {
                //         return none_body
                //     }
                // }()
                let subject_name = subject.0.as_str();
                let return_type = self.transpile_type(some_arm_body.as_type());

                let some_body = if let Some(var_name) = some_arm_binding {
                    BoxDoc::text(var_name.as_str())
                        .append(BoxDoc::text(" := "))
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".value"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("return "))
                        .append(self.transpile_expr(some_arm_body))
                } else {
                    BoxDoc::text("return ").append(self.transpile_expr(some_arm_body))
                };

                BoxDoc::text("func() ")
                    .append(return_type)
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("if "))
                            .append(BoxDoc::text(subject_name))
                            .append(BoxDoc::text(".some {"))
                            .append(BoxDoc::line().append(some_body).nest(2))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("} else {"))
                            .append(
                                BoxDoc::line()
                                    .append(BoxDoc::text("return "))
                                    .append(self.transpile_expr(none_arm_body))
                                    .nest(2),
                            )
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("}"))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}()"))
            }
            Match::Enum { subject, arms } => {
                // func() ReturnType {
                //     switch _v := subject.(type) {
                //     case EnumNameVariantName:
                //         binding := _v.Field
                //         return body
                //     [[...]]
                //     }
                //     panic("unreachable")
                // }()
                let subject_name = subject.0.as_str();
                let has_any_bindings = arms.iter().any(|arm| !arm.bindings.is_empty());
                let return_type = self.transpile_type(arms[0].body.as_type());

                let cases = BoxDoc::intersperse(
                    arms.iter().map(|arm| match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name,
                            variant_name,
                        } => {
                            let struct_name = format!("{}{}", enum_name, variant_name);
                            // Generate binding assignments
                            let bindings_doc = if arm.bindings.is_empty() {
                                BoxDoc::nil()
                            } else {
                                let binding_stmts: Vec<_> = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, var)| {
                                        let pascal_field = field.to_pascal_case();
                                        BoxDoc::text(var.as_str())
                                            .append(BoxDoc::text(" := _v."))
                                            .append(BoxDoc::as_string(pascal_field))
                                    })
                                    .collect();
                                BoxDoc::intersperse(binding_stmts, BoxDoc::line())
                                    .append(BoxDoc::line())
                            };
                            BoxDoc::text("case ")
                                .append(BoxDoc::text(struct_name))
                                .append(BoxDoc::text(":"))
                                .append(
                                    BoxDoc::line()
                                        .append(bindings_doc)
                                        .append(BoxDoc::text("return "))
                                        .append(self.transpile_expr(&arm.body))
                                        .nest(2),
                                )
                        }
                    }),
                    BoxDoc::line(),
                );

                // Use "_v :=" syntax only if there are bindings
                let switch_start = if has_any_bindings {
                    BoxDoc::text("switch _v := ")
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".(type) {"))
                } else {
                    BoxDoc::text("switch ")
                        .append(BoxDoc::text(subject_name))
                        .append(BoxDoc::text(".(type) {"))
                };

                let switch_body = switch_start
                    .append(BoxDoc::line().append(cases).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("panic(\"unreachable\")"));

                BoxDoc::text("func() ")
                    .append(return_type)
                    .append(BoxDoc::text(" {"))
                    .append(BoxDoc::line().append(switch_body).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}()"))
            }
        }
    }

    fn transpile_let<'a>(
        &self,
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> BoxDoc<'a> {
        // func() returnType { var := value; return body }()
        let return_type = self.transpile_type(body.as_type());

        // For enum types, use `var x EnumType = value` to ensure interface type
        // This is needed for type switches to work properly
        let decl = if let Type::Enum { name, .. } = value.as_type() {
            BoxDoc::text("var ")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" "))
                .append(BoxDoc::text(name.as_str()))
                .append(BoxDoc::text(" = "))
                .append(self.transpile_expr(value))
        } else {
            BoxDoc::text(var.as_str())
                .append(BoxDoc::text(" := "))
                .append(self.transpile_expr(value))
        };

        BoxDoc::text("func() ")
            .append(return_type)
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::line()
                    .append(decl)
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("return "))
                    .append(self.transpile_expr(body))
                    .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}()"))
    }

    fn transpile_merge_classes<'a>(&self, args: &'a [IrExpr]) -> BoxDoc<'a> {
        if args.is_empty() {
            BoxDoc::text("\"\"")
        } else {
            BoxDoc::intersperse(
                args.iter().map(|arg| self.transpile_expr(arg)),
                BoxDoc::text(" + \" \" + "),
            )
        }
    }

    fn transpile_array_length<'a>(&self, array: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("len(")
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_to_string<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("strconv.Itoa(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_to_int<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("int(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_to_string<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("strconv.FormatFloat(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(", 'f', -1, 64)"))
    }

    fn transpile_int_to_float<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("float64(")
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

    fn transpile_option_type<'a>(&self, inner_type: &'a Type) -> BoxDoc<'a> {
        BoxDoc::text("Option[")
            .append(self.transpile_type(inner_type))
            .append(BoxDoc::text("]"))
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
    use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};
    use expect_test::{Expect, expect};
    use std::sync::Arc;

    fn check(module: IrModule, expected: Expect) {
        let before = module.to_string();
        let after = GoTranspiler::new("components".to_string()).transpile_module(&module);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        check(
            IrModuleBuilder::new()
                .component_no_params("TestMainComp", |t| {
                    t.write("<div>Hello World</div>\n");
                })
                .build(),
            expect![[r#"
                -- before --
                TestMainComp() {
                  write("<div>Hello World</div>\n")
                }

                -- after --
                package components

                import (
                	"io"
                )

                func TestMainComp(w io.Writer) {
                	io.WriteString(w, "<div>Hello World</div>\n")
                }
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
                package components

                import (
                	"html"
                	"io"
                )

                func TestGreetingComp(w io.Writer, name string, message string) {
                	io.WriteString(w, "<h1>Hello ")
                	io.WriteString(w, html.EscapeString(name))
                	io.WriteString(w, ", ")
                	io.WriteString(w, html.EscapeString(message))
                	io.WriteString(w, "</h1>\n")
                }
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
                package components

                import (
                	"io"
                )

                func TestMainComp(w io.Writer, show bool) {
                	if show {
                		io.WriteString(w, "<div>Visible</div>\n")
                	}
                }
            "#]],
        );
    }

    #[test]
    fn for_loop() {
        check(
            IrModuleBuilder::new()
                .component(
                    "TestMainComp",
                    [("items", Type::Array(Arc::new(Type::String)))],
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
                package components

                import (
                	"html"
                	"io"
                )

                func TestMainComp(w io.Writer, items []string) {
                	for _, item := range items {
                		io.WriteString(w, "<li>")
                		io.WriteString(w, html.EscapeString(item))
                		io.WriteString(w, "</li>\n")
                	}
                }
            "#]],
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
                package components

                import (
                	"io"
                	"strconv"
                )

                func Counter(w io.Writer) {
                	for i := 1; i <= 3; i++ {
                		io.WriteString(w, strconv.Itoa(i))
                		io.WriteString(w, " ")
                	}
                }
            "#]],
        );
    }

    #[test]
    fn loop_over_array_literal() {
        check(
            IrModuleBuilder::new()
                .component_no_params("TestArrayLiteralLoop", |t| {
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
                })
                .build(),
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
                	"io"
                )

                func TestArrayLiteralLoop(w io.Writer) {
                	io.WriteString(w, "<ul>\n")
                	for _, color := range []string{"red", "green", "blue"} {
                		io.WriteString(w, "<li>")
                		io.WriteString(w, html.EscapeString(color))
                		io.WriteString(w, "</li>\n")
                	}
                	io.WriteString(w, "</ul>\n")
                }
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
                package components

                import (
                	"io"
                )

                func TestAuthCheck(w io.Writer, user_role string, expected_role string) {
                	if (user_role == expected_role) {
                		io.WriteString(w, "<div>Access granted</div>\n")
                	}
                	if (user_role == "admin") {
                		io.WriteString(w, "<div>Admin panel available</div>\n")
                	}
                }
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
                package components

                import (
                	"html"
                	"io"
                )

                type TrustedHTML string

                func RenderHtml(w io.Writer, safe_content TrustedHTML, user_input string) {
                	io.WriteString(w, "<div>")
                	io.WriteString(w, string(safe_content))
                	io.WriteString(w, "</div><div>")
                	io.WriteString(w, html.EscapeString(user_input))
                	io.WriteString(w, "</div>")
                }
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
                package components

                import (
                	"html"
                	"io"
                )

                type Address struct {
                	Street string `json:"street"`
                	City string `json:"city"`
                }

                type User struct {
                	Name string `json:"name"`
                	Age int `json:"age"`
                	Active bool `json:"active"`
                }

                func UserProfile(w io.Writer, user User) {
                	io.WriteString(w, "<div>")
                	io.WriteString(w, html.EscapeString(user.Name))
                	io.WriteString(w, "</div>")
                }
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
                package components

                import (
                	"html"
                	"io"
                )

                type User struct {
                	Name string `json:"name"`
                	Age int `json:"age"`
                }

                func CreateUser(w io.Writer) {
                	io.WriteString(w, "<div>")
                	io.WriteString(w, html.EscapeString(User{
                		Name: "John",
                		Age: 30,
                	}.Name))
                	io.WriteString(w, "</div>")
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_in_condition() {
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
                        t.write("<div>It's red!</div>");
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
                    write("<div>It's red!</div>")
                  }
                }

                -- after --
                package components

                import (
                	"io"
                )

                type Color interface {
                	Equals(o Color) bool
                }

                type ColorRed struct{}

                func (e ColorRed) Equals(o Color) bool {
                	_, ok := o.(ColorRed)
                	return ok
                }

                type ColorGreen struct{}

                func (e ColorGreen) Equals(o Color) bool {
                	_, ok := o.(ColorGreen)
                	return ok
                }

                type ColorBlue struct{}

                func (e ColorBlue) Equals(o Color) bool {
                	_, ok := o.(ColorBlue)
                	return ok
                }

                func ColorDisplay(w io.Writer, color Color) {
                	if color.Equals(ColorRed{}) {
                		io.WriteString(w, "<div>It's red!</div>")
                	}
                }
            "#]],
        );
    }

    #[test]
    fn enum_equality_comparison() {
        use crate::dop::symbols::type_name::TypeName;
        use crate::hop::symbols::module_name::ModuleName;

        let status_type = Type::Enum {
            module: ModuleName::new("test").unwrap(),
            name: TypeName::new("Status").unwrap(),
            variants: vec![
                (TypeName::new("Active").unwrap(), vec![]),
                (TypeName::new("Inactive").unwrap(), vec![]),
                (TypeName::new("Pending").unwrap(), vec![]),
            ],
        };

        check(
            IrModuleBuilder::new()
                .enum_decl("Status", ["Active", "Inactive", "Pending"])
                .component("StatusCheck", [("status", status_type)], |t| {
                    t.if_stmt(
                        t.eq(t.var("status"), t.enum_variant("Status", "Active")),
                        |t| {
                            t.write("<span class=\"active\">Active</span>");
                        },
                    );
                    t.if_stmt(
                        t.eq(t.var("status"), t.enum_variant("Status", "Pending")),
                        |t| {
                            t.write("<span class=\"pending\">Pending</span>");
                        },
                    );
                })
                .build(),
            expect![[r#"
                -- before --
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }
                StatusCheck(status: test::Status) {
                  if (status == Status::Active) {
                    write("<span class=\"active\">Active</span>")
                  }
                  if (status == Status::Pending) {
                    write("<span class=\"pending\">Pending</span>")
                  }
                }

                -- after --
                package components

                import (
                	"io"
                )

                type Status interface {
                	Equals(o Status) bool
                }

                type StatusActive struct{}

                func (e StatusActive) Equals(o Status) bool {
                	_, ok := o.(StatusActive)
                	return ok
                }

                type StatusInactive struct{}

                func (e StatusInactive) Equals(o Status) bool {
                	_, ok := o.(StatusInactive)
                	return ok
                }

                type StatusPending struct{}

                func (e StatusPending) Equals(o Status) bool {
                	_, ok := o.(StatusPending)
                	return ok
                }

                func StatusCheck(w io.Writer, status Status) {
                	if status.Equals(StatusActive{}) {
                		io.WriteString(w, "<span class=\"active\">Active</span>")
                	}
                	if status.Equals(StatusPending{}) {
                		io.WriteString(w, "<span class=\"pending\">Pending</span>")
                	}
                }
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
                package components

                import (
                	"io"
                )

                type Color interface {
                	Equals(o Color) bool
                }

                type ColorRed struct{}

                func (e ColorRed) Equals(o Color) bool {
                	_, ok := o.(ColorRed)
                	return ok
                }

                type ColorGreen struct{}

                func (e ColorGreen) Equals(o Color) bool {
                	_, ok := o.(ColorGreen)
                	return ok
                }

                type ColorBlue struct{}

                func (e ColorBlue) Equals(o Color) bool {
                	_, ok := o.(ColorBlue)
                	return ok
                }

                func ColorDisplay(w io.Writer, color Color) {
                	if color.Equals(ColorRed{}) {
                		io.WriteString(w, "<div>Red!</div>")
                	}
                }
            "#]],
        );
    }

    #[test]
    fn option_literal() {
        check(
            IrModuleBuilder::new()
                .component_no_params("TestOption", |t| {
                    t.let_stmt("opt1", t.some(t.str("hello")), |t| {
                        t.option_match_stmt(
                            t.var("opt1"),
                            Some("val"),
                            |t| {
                                t.write("Got:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write("None");
                            },
                        );
                    });
                    t.let_stmt("opt2", t.none(Type::String), |t| {
                        t.option_match_stmt(
                            t.var("opt2"),
                            Some("val"),
                            |t| {
                                t.write("Got:");
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
                  let opt1 = Option[String]::Some("hello") in {
                    match opt1 {
                      Some(val) => {
                        write("Got:")
                        write_expr(val)
                      }
                      None => {
                        write("None")
                      }
                    }
                  }
                  let opt2 = Option[String]::None in {
                    match opt2 {
                      Some(val) => {
                        write("Got:")
                        write_expr(val)
                      }
                      None => {
                        write(",None")
                      }
                    }
                  }
                }

                -- after --
                package components

                import (
                	"io"
                )

                type Option[T any] struct {
                	value T
                	some  bool
                }

                func TestOption(w io.Writer) {
                	opt1 := Option[string]{value: "hello", some: true}
                	if opt1.some {
                		val := opt1.value
                		io.WriteString(w, "Got:")
                		io.WriteString(w, val)
                	} else {
                		io.WriteString(w, "None")
                	}
                	opt2 := Option[string]{some: false}
                	if opt2.some {
                		val := opt2.value
                		io.WriteString(w, "Got:")
                		io.WriteString(w, val)
                	} else {
                		io.WriteString(w, ",None")
                	}
                }
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
                package components

                import (
                	"io"
                )

                func DisplayStatus(w io.Writer, active bool) {
                	if active {
                		io.WriteString(w, "<span class=\"active\">Active</span>")
                	} else {
                		io.WriteString(w, "<span class=\"inactive\">Inactive</span>")
                	}
                }
            "#]],
        );
    }

    #[test]
    fn enum_match_statement() {
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
                .component("ColorName", [("color", color_type)], |t| {
                    t.enum_match_stmt(
                        t.var("color"),
                        vec![
                            ("Red", Box::new(|t: &mut IrBuilder| t.write("red"))),
                            ("Green", Box::new(|t: &mut IrBuilder| t.write("green"))),
                            ("Blue", Box::new(|t: &mut IrBuilder| t.write("blue"))),
                        ],
                    );
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
                  match color {
                    Color::Red => {
                      write("red")
                    }
                    Color::Green => {
                      write("green")
                    }
                    Color::Blue => {
                      write("blue")
                    }
                  }
                }

                -- after --
                package components

                import (
                	"io"
                )

                type Color interface {
                	Equals(o Color) bool
                }

                type ColorRed struct{}

                func (e ColorRed) Equals(o Color) bool {
                	_, ok := o.(ColorRed)
                	return ok
                }

                type ColorGreen struct{}

                func (e ColorGreen) Equals(o Color) bool {
                	_, ok := o.(ColorGreen)
                	return ok
                }

                type ColorBlue struct{}

                func (e ColorBlue) Equals(o Color) bool {
                	_, ok := o.(ColorBlue)
                	return ok
                }

                func ColorName(w io.Writer, color Color) {
                	switch color.(type) {
                		case ColorRed:
                			io.WriteString(w, "red")
                		case ColorGreen:
                			io.WriteString(w, "green")
                		case ColorBlue:
                			io.WriteString(w, "blue")
                	}
                }
            "#]],
        );
    }

    #[test]
    fn enum_match_expression() {
        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component_no_params("ColorName", |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Green"), |t| {
                        let result = t.match_expr(
                            t.var("color"),
                            vec![
                                ("Red", t.str("red")),
                                ("Green", t.str("green")),
                                ("Blue", t.str("blue")),
                            ],
                        );
                        t.write_expr(result, false);
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
                ColorName() {
                  let color = Color::Green in {
                    write_expr(match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    })
                  }
                }

                -- after --
                package components

                import (
                	"io"
                )

                type Color interface {
                	Equals(o Color) bool
                }

                type ColorRed struct{}

                func (e ColorRed) Equals(o Color) bool {
                	_, ok := o.(ColorRed)
                	return ok
                }

                type ColorGreen struct{}

                func (e ColorGreen) Equals(o Color) bool {
                	_, ok := o.(ColorGreen)
                	return ok
                }

                type ColorBlue struct{}

                func (e ColorBlue) Equals(o Color) bool {
                	_, ok := o.(ColorBlue)
                	return ok
                }

                func ColorName(w io.Writer) {
                	var color Color = ColorGreen{}
                	io.WriteString(w, func() string {
                			switch color.(type) {
                					case ColorRed:
                							return "red"
                					case ColorGreen:
                							return "green"
                					case ColorBlue:
                							return "blue"
                			}
                			panic("unreachable")
                	}())
                }
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
                package components

                import (
                	"io"
                )

                type Result interface {
                	Equals(o Result) bool
                }

                type ResultOk struct {
                	Value int
                }

                func (e ResultOk) Equals(o Result) bool {
                	_, ok := o.(ResultOk)
                	return ok
                }

                type ResultErr struct {
                	Message string
                }

                func (e ResultErr) Equals(o Result) bool {
                	_, ok := o.(ResultErr)
                	return ok
                }

                func ShowResult(w io.Writer, r Result) {
                	io.WriteString(w, "<div>")
                	var ok Result = ResultOk{Value: 42}
                	io.WriteString(w, "Created Ok!")
                	io.WriteString(w, "</div>")
                }
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
                package components

                import (
                	"io"
                )

                type Result interface {
                	Equals(o Result) bool
                }

                type ResultOk struct {
                	Value string
                }

                func (e ResultOk) Equals(o Result) bool {
                	_, ok := o.(ResultOk)
                	return ok
                }

                type ResultErr struct {
                	Message string
                }

                func (e ResultErr) Equals(o Result) bool {
                	_, ok := o.(ResultErr)
                	return ok
                }

                func ShowResult(w io.Writer, r Result) {
                	switch _v := r.(type) {
                		case ResultOk:
                			v := _v.Value
                			io.WriteString(w, "Value: ")
                			io.WriteString(w, v)
                		case ResultErr:
                			m := _v.Message
                			io.WriteString(w, "Error: ")
                			io.WriteString(w, m)
                	}
                }
            "#]],
        );
    }
}
