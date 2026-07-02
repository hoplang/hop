use pretty::{Arena, DocAllocator};

use super::{Doc, Transpiler};
use crate::expr::patterns::{EnumPattern, Match};
use crate::expr::typing::r#type::Type;
use crate::expr::typing::type_registry::TypeRegistry;
use crate::ir::ast::{
    IrArgument, IrComponentDeclaration, IrExpr, IrForSource, IrModule, IrStatement,
    IrViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

pub struct TsTranspiler {
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
    /// Tracks whether Option type is used during transpilation
    needs_option: bool,
    /// Tracks whether escapeHtml function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether Fragment type is used during transpilation
    needs_fragment: bool,
    /// Registry of the module currently being transpiled
    registry: TypeRegistry,
}

impl TsTranspiler {
    pub fn new() -> Self {
        Self {
            use_template_literals: false,
            needs_option: false,
            needs_escape_html: false,
            needs_fragment: false,
            registry: TypeRegistry::default(),
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
    /// Render the `{ field: var } = subject` object-destructuring pattern.
    fn transpile_record_destructure_pattern<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
    ) -> Doc<'a> {
        let bindings_doc = arena.intersperse(
            bindings.iter().map(|(field, var)| {
                arena
                    .text(field.as_str())
                    .append(arena.text(": "))
                    .append(arena.text(var.as_str()))
            }),
            arena.text(", "),
        );
        arena
            .text("{ ")
            .append(bindings_doc)
            .append(arena.text(" } = "))
            .append(self.transpile_expr(arena, subject))
    }

    fn quote_string(&mut self, s: &str) -> String {
        if self.use_template_literals {
            format!("`{}`", self.escape_string(s))
        } else {
            format!("\"{}\"", self.escape_string(s))
        }
    }

    /// How to refer to a match subject in option and enum code generation, which
    /// reads it more than once. A variable is referenced directly, anything else
    /// is bound once to a reserved name that a hop identifier can never produce.
    fn subject_ref(subject: &IrExpr) -> &str {
        match subject {
            IrExpr::Var { value, .. } => value.as_str(),
            _ => "$subject",
        }
    }

    /// For an option or enum statement match, a non-variable subject is bound
    /// once inside a block so it runs only once. A variable passes through.
    fn wrap_match_subject_stmt<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        switch_doc: Doc<'a>,
    ) -> Doc<'a> {
        match subject {
            IrExpr::Var { .. } => switch_doc,
            _ => arena
                .text("{")
                .append(
                    arena
                        .hardline()
                        .append(arena.text("const $subject = "))
                        .append(self.transpile_expr(arena, subject))
                        .append(arena.text(";"))
                        .append(arena.hardline())
                        .append(switch_doc)
                        .nest(4),
                )
                .append(arena.hardline())
                .append(arena.text("}")),
        }
    }

    /// For an option or enum expression match, the switch is wrapped in an
    /// immediately invoked function. A variable is used directly inside it,
    /// anything else is passed in as an argument so it runs only once.
    fn wrap_match_subject_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        switch_body: Doc<'a>,
    ) -> Doc<'a> {
        match subject {
            IrExpr::Var { .. } => arena
                .text("(() => {")
                .append(arena.line().append(switch_body).nest(2))
                .append(arena.line())
                .append(arena.text("})()"))
                .group(),
            _ => arena
                .text("(($subject) => {")
                .append(arena.line().append(switch_body).nest(2))
                .append(arena.line())
                .append(arena.text("})("))
                .append(self.transpile_expr(arena, subject))
                .append(arena.text(")"))
                .group(),
        }
    }
}

impl Default for TsTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Transpiler for TsTranspiler {
    fn registry(&self) -> &TypeRegistry {
        &self.registry
    }

    fn transpile_module(&mut self, module: &IrModule, registry: &TypeRegistry) -> String {
        // Reset tracking flags for this module
        self.needs_option = false;
        self.needs_escape_html = false;
        self.needs_fragment = false;
        self.registry = registry.clone();

        let arena = &Arena::new();

        let views = &module.views;
        let records = &module.records;

        let mut result = arena.nil();

        // Add enum type definitions (namespace-based)
        for enum_def in &module.enums {
            // Generate namespace with tagged union type and constructor functions
            let variant_type_docs: Vec<Doc> = enum_def
                .variants
                .iter()
                .map(|variant| {
                    let base = arena
                        .text("{ readonly tag: \"")
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("\""));
                    if variant.fields.is_empty() {
                        base.append(arena.text(" }"))
                    } else {
                        let field_docs: Vec<Doc> = variant
                            .fields
                            .iter()
                            .map(|(field_name, field_type, _)| {
                                arena
                                    .text(", readonly ")
                                    .append(arena.text(field_name.as_str()))
                                    .append(arena.text(": "))
                                    .append(self.transpile_type(arena, field_type))
                            })
                            .collect();
                        base.append(arena.intersperse(field_docs, arena.nil()))
                            .append(arena.text(" }"))
                    }
                })
                .collect();

            result = result
                .append(arena.text("export namespace "))
                .append(arena.text(enum_def.name.as_str()))
                .append(arena.text(" {"))
                .append(arena.line())
                .append(arena.text("    export type "))
                .append(arena.text(enum_def.name.as_str()))
                .append(arena.text(" = "))
                .append(arena.intersperse(variant_type_docs, arena.text(" | ")))
                .append(arena.text(";"))
                .append(arena.line());

            // Generate constructor function for each variant
            for variant in &enum_def.variants {
                result = result.append(arena.line());

                if variant.fields.is_empty() {
                    // Unit variant: no parameters
                    result = result
                        .append(arena.text("    export function "))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("(): "))
                        .append(arena.text(enum_def.name.as_str()))
                        .append(arena.text(" {"))
                        .append(arena.line())
                        .append(arena.text("        return { tag: \""))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("\" };"))
                        .append(arena.line())
                        .append(arena.text("    }"));
                } else {
                    // Variant with fields: add parameters
                    let param_docs: Vec<Doc> = variant
                        .fields
                        .iter()
                        .map(|(field_name, field_type, _)| {
                            arena
                                .text(field_name.as_str())
                                .append(arena.text(": "))
                                .append(self.transpile_type(arena, field_type))
                        })
                        .collect();
                    let field_name_docs: Vec<Doc> = variant
                        .fields
                        .iter()
                        .map(|(field_name, _, _)| {
                            arena.text(", ").append(arena.text(field_name.as_str()))
                        })
                        .collect();
                    result = result
                        .append(arena.text("    export function "))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("("))
                        .append(arena.intersperse(param_docs, arena.text(", ")))
                        .append(arena.text("): "))
                        .append(arena.text(enum_def.name.as_str()))
                        .append(arena.text(" {"))
                        .append(arena.line())
                        .append(arena.text("        return { tag: \""))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("\""))
                        .append(arena.intersperse(field_name_docs, arena.nil()))
                        .append(arena.text(" };"))
                        .append(arena.line())
                        .append(arena.text("    }"));
                }
            }

            result = result
                .append(arena.line())
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
        }

        // Add record type definitions
        if !records.is_empty() {
            for record in records {
                let field_docs: Vec<Doc> = record
                    .fields
                    .iter()
                    .map(|(name, ty, _)| {
                        arena
                            .text("public readonly ")
                            .append(arena.text(name.as_str()))
                            .append(arena.text(": "))
                            .append(self.transpile_type(arena, ty))
                            .append(arena.text(","))
                    })
                    .collect();
                result = result
                    .append(arena.text("export class "))
                    .append(arena.text(record.name.as_str()))
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .nil()
                            .append(arena.line())
                            .append(arena.text("constructor("))
                            .append(
                                arena
                                    .nil()
                                    .append(arena.line())
                                    .append(arena.intersperse(field_docs, arena.line()))
                                    .append(arena.line())
                                    .nest(4),
                            )
                            .append(arena.text(") {}"))
                            .append(arena.line())
                            .nest(4),
                    )
                    .append(arena.text("}"))
                    .append(arena.line())
                    .append(arena.line());
            }
        }

        // Add component definitions
        for component in &module.components {
            result = result
                .append(self.transpile_component_def(arena, component))
                .append(arena.hardline())
                .append(arena.hardline());
        }

        let view_docs: Vec<Doc> = views
            .iter()
            .map(|view| self.transpile_view(arena, &view.name, view))
            .collect();
        result =
            result.append(arena.intersperse(view_docs, arena.hardline().append(arena.hardline())));

        // Prepend escapeHtml function if needed (after transpilation determined it's used)
        if self.needs_escape_html {
            let escape_fn = arena
                .nil()
                .append(arena.text("function escapeHtml(str: string): string {"))
                .append(
                    arena
                        .nil()
                        .append(arena.line())
                        .append(arena.text("return str"))
                        .append(
                            arena
                                .nil()
                                .append(arena.line())
                                .append(arena.intersperse(
                                    [
                                        arena.text(".replace(/&/g, '&amp;')"),
                                        arena.text(".replace(/</g, '&lt;')"),
                                        arena.text(".replace(/>/g, '&gt;')"),
                                        arena.text(".replace(/\"/g, '&quot;')"),
                                        arena.text(".replace(/'/g, '&#39;');"),
                                    ],
                                    arena.line(),
                                ))
                                .nest(4),
                        )
                        .append(arena.line())
                        .nest(4),
                )
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
            result = escape_fn.append(result);
        }

        // Prepend Option namespace if needed (after transpilation determined it's used)
        if self.needs_option {
            let option_ns = arena
                .nil()
                .append(arena.text("export namespace Option {"))
                .append(arena.line())
                .append(arena.text(
                    "    export type Option<T> = { readonly tag: \"None\" } | { readonly tag: \"Some\", value: T };",
                ))
                .append(arena.line())
                .append(arena.line())
                .append(arena.text("    export function some<T>(value: T): Option<T> {"))
                .append(arena.line())
                .append(arena.text("        return { tag: \"Some\", value };"))
                .append(arena.line())
                .append(arena.text("    }"))
                .append(arena.line())
                .append(arena.text("    export function none<T = never>(): Option<T> {"))
                .append(arena.line())
                .append(arena.text("        return { tag: \"None\" };"))
                .append(arena.line())
                .append(arena.text("    }"))
                .append(arena.line())
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
            result = option_ns.append(result);
        }

        // Prepend Fragment type if needed (after transpilation determined it's used)
        if self.needs_fragment {
            let fragment = arena
                .nil()
                .append(arena.text(
                    "type Fragment = string & { readonly __brand: unique symbol };",
                ))
                .append(arena.line())
                .append(arena.line())
                .append(arena.text("/** Marks a string as trusted HTML, bypassing escaping. Only use with sanitized or trusted content. Calling this function with untrusted content causes XSS vulnerabilities. */"))
                .append(arena.line())
                .append(arena.text("export function trustHtml(str: string): Fragment {"))
                .append(
                    arena
                        .nil()
                        .append(arena.line())
                        .append(arena.text("return str as Fragment;"))
                        .append(arena.line())
                        .nest(4),
                )
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
            result = fragment.append(result);
        }

        // Prepend warning header (must be last prepend to appear first in output)
        let warning = arena
            .text("// Code generated by the hop compiler. DO NOT EDIT.")
            .append(arena.line())
            .append(arena.line());
        result = warning.append(result);

        let output = result.pretty(80).to_string();

        // Ensure file ends with a newline
        if !output.ends_with('\n') {
            format!("{}\n", output)
        } else {
            output
        }
    }

    fn transpile_view<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        view: &'a IrViewDeclaration,
    ) -> Doc<'a> {
        let mut result = arena
            .text("export function ")
            .append(arena.text(name.as_ref()))
            .append(arena.text("("));

        if !view.parameters.is_empty() {
            // Destructuring pattern: {a, b, c}
            let name_docs: Vec<Doc> = view
                .parameters
                .iter()
                .map(|param| arena.text(param.name.as_str()))
                .collect();
            let type_docs: Vec<Doc> = view
                .parameters
                .iter()
                .map(|param| {
                    arena
                        .text(param.name.as_str())
                        .append(arena.text(": "))
                        .append(self.transpile_type(arena, &param.typ))
                })
                .collect();
            result = result
                .append(arena.text("{"))
                .append(arena.intersperse(name_docs, arena.text(", ")))
                .append(arena.text("}: {"))
                .append(arena.intersperse(type_docs, arena.text(", ")))
                .append(arena.text("}"));
        }

        // Function body
        result
            .append(arena.text("): string {"))
            .append(
                arena
                    .nil()
                    .append(arena.line())
                    .append(arena.text("let output: string = \"\";"))
                    .append(arena.line())
                    .append(self.transpile_statements(arena, &view.body))
                    .append(arena.line())
                    .append(arena.text("return output;"))
                    .append(arena.line())
                    .nest(4),
            )
            .append(arena.text("}"))
    }

    fn transpile_component_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        component: &'a IrComponentDeclaration,
    ) -> Doc<'a> {
        let mut result = arena
            .text("function render")
            .append(arena.text(component.name.as_ref()))
            .append(arena.text("("));

        // Build parameter list
        let has_params = !component.parameters.is_empty();
        if has_params {
            // Destructuring pattern: {a, b, c}
            let param_names: Vec<Doc> = component
                .parameters
                .iter()
                .map(|param| arena.text(param.name.as_str()))
                .collect();

            result = result
                .append(arena.text("{"))
                .append(arena.intersperse(param_names, arena.text(", ")))
                .append(arena.text("}: {"));

            // Type annotation: {a: TypeA, b: TypeB}
            let param_types: Vec<Doc> = component
                .parameters
                .iter()
                .map(|param| {
                    arena
                        .text(param.name.as_str())
                        .append(arena.text(": "))
                        .append(self.transpile_type(arena, &param.typ))
                })
                .collect();

            result = result
                .append(arena.intersperse(param_types, arena.text(", ")))
                .append(arena.text("}"));
        }

        // Function body
        let body = arena
            .nil()
            .append(arena.line())
            .append(arena.text("let output: string = \"\";"))
            .append(arena.line());

        let body = body
            .append(self.transpile_statements(arena, &component.body))
            .append(arena.line())
            .append(arena.text("return output;"))
            .append(arena.line());

        result
            .append(arena.text("): string {"))
            .append(body.nest(4))
            .append(arena.text("}"))
    }

    fn transpile_component_call_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        args: &'a [IrArgument],
    ) -> Doc<'a> {
        let mut doc = arena
            .nil()
            .append(arena.text("output += render"))
            .append(arena.text(name.as_ref()))
            .append(arena.text("("));

        if !args.is_empty() {
            // Build named arguments
            let arg_docs: Vec<Doc> = args
                .iter()
                .map(|arg| {
                    arena
                        .text(arg.name.as_str())
                        .append(arena.text(": "))
                        .append(self.transpile_expr(arena, &arg.expr))
                })
                .collect();

            doc = doc
                .append(arena.text("{"))
                .append(arena.intersperse(arg_docs, arena.text(", ")))
                .append(arena.text("}"));
        }

        doc.append(arena.text(");"))
    }

    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("output += "))
            .append(arena.text(self.quote_string(content)))
            .append(arena.text(";"))
    }

    fn transpile_write_expr_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a IrExpr,
        escape: bool,
    ) -> Doc<'a> {
        if escape {
            self.needs_escape_html = true;
            arena
                .nil()
                .append(arena.text("output += escapeHtml("))
                .append(self.transpile_expr(arena, expr))
                .append(arena.text(");"))
        } else {
            arena
                .nil()
                .append(arena.text("output += "))
                .append(self.transpile_expr(arena, expr))
                .append(arena.text(";"))
        }
    }

    fn transpile_if_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("if ("))
            .append(self.transpile_expr(arena, condition))
            .append(arena.text(") {"))
            .append(
                arena
                    .nil()
                    .append(arena.hardline())
                    .append(self.transpile_statements(arena, body))
                    .append(arena.hardline())
                    .nest(4),
            )
            .append(arena.text("}"))
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        let var_name = var.unwrap_or("_");
        match source {
            IrForSource::Array(array) => arena
                .nil()
                .append(arena.text("for (const "))
                .append(arena.text(var_name))
                .append(arena.text(" of "))
                .append(self.transpile_expr(arena, array))
                .append(arena.text(") {"))
                .append(
                    arena
                        .nil()
                        .append(arena.hardline())
                        .append(self.transpile_statements(arena, body))
                        .append(arena.hardline())
                        .nest(4),
                )
                .append(arena.text("}")),
            IrForSource::RangeInclusive { start, end } => arena
                .nil()
                .append(arena.text("for (let "))
                .append(arena.text(var_name))
                .append(arena.text(" = "))
                .append(self.transpile_expr(arena, start))
                .append(arena.text("; "))
                .append(arena.text(var_name))
                .append(arena.text(" <= "))
                .append(self.transpile_expr(arena, end))
                .append(arena.text("; "))
                .append(arena.text(var_name))
                .append(arena.text("++) {"))
                .append(
                    arena
                        .nil()
                        .append(arena.hardline())
                        .append(self.transpile_statements(arena, body))
                        .append(arena.hardline())
                        .nest(4),
                )
                .append(arena.text("}")),
        }
    }

    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        arena
            .text("const ")
            .append(arena.text(var))
            .append(arena.text(" = "))
            .append(self.transpile_expr(arena, value))
            .append(arena.text(";"))
            .append(arena.hardline())
            .append(self.transpile_statements(arena, body))
    }

    fn transpile_let_fragment_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a str,
        fragment_body: &'a [IrStatement],
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        self.needs_fragment = true;
        arena
            .text("const ")
            .append(arena.text(var))
            .append(arena.text(" = (() => {"))
            .append(
                arena
                    .nil()
                    .append(arena.line())
                    .append(arena.text("let output: string = \"\";"))
                    .append(arena.line())
                    .append(self.transpile_statements(arena, fragment_body))
                    .append(arena.line())
                    .append(arena.text("return output as Fragment;"))
                    .append(arena.line())
                    .nest(4),
            )
            .append(arena.text("})();"))
            .append(arena.hardline())
            .append(self.transpile_statements(arena, body))
    }

    fn transpile_let_record_destructure_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        arena
            .text("const ")
            .append(self.transpile_record_destructure_pattern(arena, subject, bindings))
            .append(arena.text(";"))
            .append(arena.hardline())
            .append(self.transpile_statements(arena, body))
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<IrExpr, Vec<IrStatement>>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => arena
                .text("if (")
                .append(self.transpile_expr(arena, subject))
                .append(arena.text(") {"))
                .append(
                    arena
                        .nil()
                        .append(arena.hardline())
                        .append(self.transpile_statements(arena, true_body))
                        .append(arena.hardline())
                        .nest(4),
                )
                .append(arena.text("} else {"))
                .append(
                    arena
                        .nil()
                        .append(arena.hardline())
                        .append(self.transpile_statements(arena, false_body))
                        .append(arena.hardline())
                        .nest(4),
                )
                .append(arena.text("}")),
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                self.needs_option = true;
                let subject_name = Self::subject_ref(subject);
                let some_case = if let Some(var_name) = some_arm_binding {
                    arena
                        .text("case \"Some\": {")
                        .append(
                            arena
                                .hardline()
                                .append(arena.text("const "))
                                .append(arena.text(var_name.as_str()))
                                .append(arena.text(" = "))
                                .append(arena.text(subject_name))
                                .append(arena.text(".value;"))
                                .append(arena.hardline())
                                .append(self.transpile_statements(arena, some_arm_body))
                                .append(arena.hardline())
                                .append(arena.text("break;"))
                                .nest(4),
                        )
                        .append(arena.hardline())
                        .append(arena.text("}"))
                } else {
                    arena
                        .text("case \"Some\": {")
                        .append(
                            arena
                                .hardline()
                                .append(self.transpile_statements(arena, some_arm_body))
                                .append(arena.hardline())
                                .append(arena.text("break;"))
                                .nest(4),
                        )
                        .append(arena.hardline())
                        .append(arena.text("}"))
                };

                let none_case = arena
                    .text("case \"None\": {")
                    .append(
                        arena
                            .hardline()
                            .append(self.transpile_statements(arena, none_arm_body))
                            .append(arena.hardline())
                            .append(arena.text("break;"))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"));

                self.wrap_match_subject_stmt(
                    arena,
                    subject,
                    arena
                        .text("switch (")
                        .append(arena.text(subject_name))
                        .append(arena.text(".tag) {"))
                        .append(
                            arena
                                .hardline()
                                .append(some_case)
                                .append(arena.hardline())
                                .append(none_case)
                                .nest(4),
                        )
                        .append(arena.hardline())
                        .append(arena.text("}")),
                )
            }
            Match::Enum { subject, arms } => {
                let subject_name = Self::subject_ref(subject);
                let case_docs: Vec<Doc> = arms
                    .iter()
                    .map(|arm| match &arm.pattern {
                        EnumPattern::Variant {
                            enum_name: _,
                            variant_name,
                        } => {
                            // Generate binding destructuring if there are bindings
                            let bindings_doc = if arm.bindings.is_empty() {
                                arena.nil()
                            } else {
                                let destructure_docs: Vec<Doc> = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, var)| {
                                        arena
                                            .text(field.as_str())
                                            .append(arena.text(": "))
                                            .append(arena.text(var.as_str()))
                                    })
                                    .collect();
                                arena
                                    .text("const { ")
                                    .append(arena.intersperse(destructure_docs, arena.text(", ")))
                                    .append(arena.text(" } = "))
                                    .append(arena.text(subject_name))
                                    .append(arena.text(";"))
                                    .append(arena.hardline())
                            };

                            arena
                                .text("case \"")
                                .append(arena.text(variant_name.as_str()))
                                .append(arena.text("\": {"))
                                .append(
                                    arena
                                        .hardline()
                                        .append(bindings_doc)
                                        .append(self.transpile_statements(arena, &arm.body))
                                        .append(arena.hardline())
                                        .append(arena.text("break;"))
                                        .nest(4),
                                )
                                .append(arena.hardline())
                                .append(arena.text("}"))
                        }
                    })
                    .collect();
                let cases = arena.intersperse(case_docs, arena.hardline());

                self.wrap_match_subject_stmt(
                    arena,
                    subject,
                    arena
                        .text("switch (")
                        .append(arena.text(subject_name))
                        .append(arena.text(".tag) {"))
                        .append(arena.hardline().append(cases).nest(4))
                        .append(arena.hardline())
                        .append(arena.text("}")),
                )
            }
        }
    }

    fn transpile_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [IrStatement],
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for stmt in statements {
            docs.push(self.transpile_statement(arena, stmt));
        }
        arena.intersperse(docs, arena.hardline())
    }

    fn transpile_var<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a> {
        arena.text(name)
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(self.transpile_expr(arena, object))
            .append(arena.text("."))
            .append(arena.text(field.as_str()))
    }

    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a> {
        arena.text(self.quote_string(value))
    }

    fn transpile_fragment_empty<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        self.needs_fragment = true;
        arena.text("(\"\" as Fragment)")
    }

    fn transpile_boolean_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a> {
        match value {
            true => arena.text("true"),
            false => arena.text("false"),
        }
    }

    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a> {
        arena.text(format!("{}", value))
    }

    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i64) -> Doc<'a> {
        arena.text(format!("{}", value))
    }

    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [IrExpr],
        _elem_type: &'a Type,
    ) -> Doc<'a> {
        let elem_docs: Vec<Doc> = elements
            .iter()
            .map(|e| self.transpile_expr(arena, e))
            .collect();
        arena
            .nil()
            .append(arena.text("["))
            .append(arena.intersperse(elem_docs, arena.text(", ")))
            .append(arena.text("]"))
    }

    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> Doc<'a> {
        let field_docs: Vec<Doc> = fields
            .iter()
            .map(|(_key, value)| self.transpile_expr(arena, value))
            .collect();
        arena
            .text("new ")
            .append(arena.text(record_name))
            .append(arena.text("("))
            .append(arena.intersperse(field_docs, arena.text(", ")))
            .append(arena.text(")"))
    }

    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> Doc<'a> {
        // Call the namespace constructor function: Color.Red() or Result.Ok(value)
        let base = arena
            .text(enum_name)
            .append(arena.text("."))
            .append(arena.text(variant_name))
            .append(arena.text("("));
        if fields.is_empty() {
            base.append(arena.text(")"))
        } else {
            let field_docs: Vec<Doc> = fields
                .iter()
                .map(|(_, field_expr)| self.transpile_expr(arena, field_expr))
                .collect();
            base.append(arena.intersperse(field_docs, arena.text(", ")))
                .append(arena.text(")"))
        }
    }

    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" === "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" === "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" === "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" === "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" < "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" < "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" <= "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" <= "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: &'a IrExpr) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("!("))
            .append(self.transpile_expr(arena, operand))
            .append(arena.text(")"))
    }

    fn transpile_numeric_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("-("))
            .append(self.transpile_expr(arena, operand))
            .append(arena.text(")"))
    }

    fn transpile_string_concat<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" + "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_logical_and<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" && "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_logical_or<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" || "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" + "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" + "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" - "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" - "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" * "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" * "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> Doc<'a> {
        self.needs_option = true;
        match value {
            Some(inner) => arena
                .text("Option.some<")
                .append(self.transpile_type(arena, inner_type))
                .append(arena.text(">("))
                .append(self.transpile_expr(arena, inner))
                .append(arena.text(")")),
            None => arena
                .text("Option.none<")
                .append(self.transpile_type(arena, inner_type))
                .append(arena.text(">()")),
        }
    }

    fn transpile_match_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<IrExpr, IrExpr>,
    ) -> Doc<'a> {
        match match_ {
            Match::Enum { subject, arms } => {
                let subject_name = Self::subject_ref(subject);
                let case_docs: Vec<Doc> =
                    arms.iter()
                        .map(|arm| match &arm.pattern {
                            EnumPattern::Variant {
                                enum_name: _,
                                variant_name,
                            } => {
                                if arm.bindings.is_empty() {
                                    arena
                                        .text("case \"")
                                        .append(arena.text(variant_name.as_str()))
                                        .append(arena.text("\": return "))
                                        .append(self.transpile_expr(arena, &arm.body))
                                        .append(arena.text(";"))
                                } else {
                                    let destructure_docs: Vec<Doc> = arm
                                        .bindings
                                        .iter()
                                        .map(|(field, var)| {
                                            arena
                                                .text(field.as_str())
                                                .append(arena.text(": "))
                                                .append(arena.text(var.as_str()))
                                        })
                                        .collect();
                                    arena
                                        .text("case \"")
                                        .append(arena.text(variant_name.as_str()))
                                        .append(arena.text("\": {"))
                                        .append(
                                            arena
                                                .line()
                                                .append(arena.text("const { "))
                                                .append(arena.intersperse(
                                                    destructure_docs,
                                                    arena.text(", "),
                                                ))
                                                .append(arena.text(" } = "))
                                                .append(arena.text(subject_name))
                                                .append(arena.text(";"))
                                                .append(arena.line())
                                                .append(arena.text("return "))
                                                .append(self.transpile_expr(arena, &arm.body))
                                                .append(arena.text(";"))
                                                .nest(2),
                                        )
                                        .append(arena.line())
                                        .append(arena.text("}"))
                                }
                            }
                        })
                        .collect();
                let cases = arena.intersperse(case_docs, arena.line());

                let switch_body = arena
                    .text("switch (")
                    .append(arena.text(subject_name))
                    .append(arena.text(".tag) {"))
                    .append(arena.line().append(cases).nest(2))
                    .append(arena.line())
                    .append(arena.text("}"));

                self.wrap_match_subject_expr(arena, subject, switch_body)
            }
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => arena
                .text("(")
                .append(self.transpile_expr(arena, subject))
                .append(arena.text(" ? "))
                .append(self.transpile_expr(arena, true_body))
                .append(arena.text(" : "))
                .append(self.transpile_expr(arena, false_body))
                .append(arena.text(")")),
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                self.needs_option = true;
                let subject_name = Self::subject_ref(subject);
                let some_case = {
                    let body_doc = self.transpile_expr(arena, some_arm_body);
                    if let Some(var_name) = some_arm_binding {
                        arena
                            .text("case \"Some\": {")
                            .append(
                                arena
                                    .line()
                                    .append(arena.text("const "))
                                    .append(arena.text(var_name.as_str()))
                                    .append(arena.text(" = "))
                                    .append(arena.text(subject_name))
                                    .append(arena.text(".value;"))
                                    .append(arena.line())
                                    .append(arena.text("return "))
                                    .append(body_doc)
                                    .append(arena.text(";"))
                                    .nest(2),
                            )
                            .append(arena.line())
                            .append(arena.text("}"))
                    } else {
                        arena
                            .text("case \"Some\": return ")
                            .append(body_doc)
                            .append(arena.text(";"))
                    }
                };

                let none_case = arena
                    .text("case \"None\": return ")
                    .append(self.transpile_expr(arena, none_arm_body))
                    .append(arena.text(";"));

                let cases = arena.intersperse([some_case, none_case], arena.line());

                let switch_body = arena
                    .text("switch (")
                    .append(arena.text(subject_name))
                    .append(arena.text(".tag) {"))
                    .append(arena.line().append(cases).nest(2))
                    .append(arena.line())
                    .append(arena.text("}"));

                self.wrap_match_subject_expr(arena, subject, switch_body)
            }
        }
    }

    fn transpile_let<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("(() => {")
            .append(
                arena
                    .line()
                    .append(arena.text("const "))
                    .append(arena.text(var.as_str()))
                    .append(arena.text(" = "))
                    .append(self.transpile_expr(arena, value))
                    .append(arena.text(";"))
                    .append(arena.line())
                    .append(arena.text("return "))
                    .append(self.transpile_expr(arena, body))
                    .append(arena.text(";"))
                    .nest(2),
            )
            .append(arena.line())
            .append(arena.text("})()"))
    }

    fn transpile_let_record_destructure_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
        body: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("(() => {")
            .append(
                arena
                    .line()
                    .append(arena.text("const "))
                    .append(self.transpile_record_destructure_pattern(arena, subject, bindings))
                    .append(arena.text(";"))
                    .append(arena.line())
                    .append(arena.text("return "))
                    .append(self.transpile_expr(arena, body))
                    .append(arena.text(";"))
                    .nest(2),
            )
            .append(arena.line())
            .append(arena.text("})()"))
    }

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a> {
        self.transpile_expr(arena, array)
            .append(arena.text(".length"))
    }

    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a> {
        self.transpile_expr(arena, array)
            .append(arena.text(".length === 0"))
    }

    fn transpile_string_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        string: &'a IrExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, string)
            .append(arena.text(".length === 0"))
    }

    fn transpile_option_is_some<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a IrExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, option)
            .append(arena.text(".tag === \"Some\""))
    }

    fn transpile_option_is_none<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a IrExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, option)
            .append(arena.text(".tag === \"None\""))
    }

    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(").toString()"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        arena
            .text("Math.trunc(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(")"))
    }

    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        // In JavaScript, all numbers are floats, so no conversion needed
        self.transpile_expr(arena, value)
    }

    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("boolean")
    }

    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("string")
    }

    fn transpile_fragment_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        self.needs_fragment = true;
        arena.text("Fragment")
    }

    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("number")
    }

    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("number")
    }

    fn transpile_array_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_type: &'a Type,
    ) -> Doc<'a> {
        self.transpile_type(arena, element_type)
            .append(arena.text("[]"))
    }

    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &'a Type) -> Doc<'a> {
        self.needs_option = true;
        arena
            .text("Option.Option<")
            .append(self.transpile_type(arena, inner_type))
            .append(arena.text(">"))
    }

    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a> {
        arena.text(name)
    }

    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a> {
        arena
            .text(name)
            .append(arena.text("."))
            .append(arena.text(name))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};
    use expect_test::{Expect, expect};

    fn check(builder: IrModuleBuilder, expected: Expect) {
        let (module, registry) = builder.build_with_registry();
        let before = module.to_string();
        let after = TsTranspiler::new().transpile_module(&module, &registry);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        check(
            IrModuleBuilder::new().view_no_params("HelloWorld", |t| {
                t.write("<h1>Hello, World!</h1>\n");
            }),
            expect![[r#"
                -- before --
                view HelloWorld() {
                  write("<h1>Hello, World!</h1>\n")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function HelloWorld(): string {
                    let output: string = "";
                    output += "<h1>Hello, World!</h1>\n";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn component_with_params_and_escaping() {
        check(
            IrModuleBuilder::new().view("UserInfo", [("name", "String"), ("age", "String")], |t| {
                t.write("<div>\n");
                t.write("<h2>Name: ");
                t.write_expr_escaped(t.var("name"));
                t.write("</h2>\n");
                t.write("<p>Age: ");
                t.write_expr(t.var("age"), false);
                t.write("</p>\n");
                t.write("</div>\n");
            }),
            expect![[r#"
                -- before --
                view UserInfo(name: String, age: String) {
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

                export function UserInfo({name, age}: {name: string, age: string}): string {
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
            "#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            IrModuleBuilder::new().view(
                "ConditionalDisplay",
                [("title", "String"), ("show", "Bool")],
                |t| {
                    t.if_stmt(t.var("show"), |t| {
                        t.write("<h1>");
                        t.write_expr_escaped(t.var("title"));
                        t.write("</h1>\n");
                    });
                },
            ),
            expect![[r#"
                -- before --
                view ConditionalDisplay(title: String, show: Bool) {
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

                export function ConditionalDisplay({title, show}: {title: string, show: boolean}): string {
                    let output: string = "";
                    if (show) {
                        output += "<h1>";
                        output += escapeHtml(title);
                        output += "</h1>\n";
                    }
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_array() {
        check(
            IrModuleBuilder::new().view("ListItems", [("items", "Array[String]")], |t| {
                t.write("<ul>\n");
                t.for_loop("item", t.var("items"), |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("item"));
                    t.write("</li>\n");
                });
                t.write("</ul>\n");
            }),
            expect![[r#"
                -- before --
                view ListItems(items: Array[String]) {
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

                export function ListItems({items}: {items: string[]}): string {
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
            "#]],
        );
    }

    #[test]
    fn for_loop_with_range() {
        check(
            IrModuleBuilder::new().view_no_params("Counter", |t| {
                t.for_range("i", t.int(1), t.int(3), |t| {
                    t.write_expr(t.int_to_string(t.var("i")), false);
                    t.write(" ");
                });
            }),
            expect![[r#"
                -- before --
                view Counter() {
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
                }
            "#]],
        );
    }

    #[test]
    fn let_binding() {
        check(
            IrModuleBuilder::new().view_no_params("GreetingCard", |t| {
                t.let_stmt("greeting", t.str("Hello from hop!"), |t| {
                    t.write("<div class=\"card\">\n");
                    t.write("<p>");
                    t.write_expr_escaped(t.var("greeting"));
                    t.write("</p>\n");
                    t.write("</div>\n");
                });
            }),
            expect![[r#"
                -- before --
                view GreetingCard() {
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
                }
            "#]],
        );
    }

    #[test]
    fn nested_components_with_let_bindings() {
        check(
            IrModuleBuilder::new().view_no_params("TestMainComp", |t| {
                t.write("<div data-hop-id=\"test/card-comp\">");
                t.let_stmt("title", t.str("Hello World"), |t| {
                    t.write("<h2>");
                    t.write_expr_escaped(t.var("title"));
                    t.write("</h2>");
                });
                t.write("</div>");
            }),
            expect![[r#"
                -- before --
                view TestMainComp() {
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
                }
            "#]],
        );
    }

    #[test]
    fn fragment_type() {
        check(
            IrModuleBuilder::new().view(
                "RenderHtml",
                [("safe_content", "Fragment"), ("user_input", "String")],
                |t| {
                    t.write("<div>");
                    // Fragment should not be escaped
                    t.write_expr(t.var("safe_content"), false);
                    t.write("</div><div>");
                    // Regular strings should be escaped
                    t.write_expr_escaped(t.var("user_input"));
                    t.write("</div>");
                },
            ),
            expect![[r#"
                -- before --
                view RenderHtml(
                  safe_content: Fragment,
                  user_input: String,
                ) {
                  write("<div>")
                  write_expr(safe_content)
                  write("</div><div>")
                  write_escaped(user_input)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                type Fragment = string & { readonly __brand: unique symbol };

                /** Marks a string as trusted HTML, bypassing escaping. Only use with sanitized or trusted content. Calling this function with untrusted content causes XSS vulnerabilities. */
                export function trustHtml(str: string): Fragment {
                    return str as Fragment;
                }

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function RenderHtml({safe_content, user_input}: {safe_content: Fragment, user_input: string}): string {
                    let output: string = "";
                    output += "<div>";
                    output += safe_content;
                    output += "</div><div>";
                    output += escapeHtml(user_input);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn record_declarations() {
        check(
            IrModuleBuilder::new()
                .record(
                    "User",
                    [("name", "String"), ("age", "Int"), ("active", "Bool")],
                )
                .record("Address", [("street", "String"), ("city", "String")])
                .view("UserProfile", [("user", "User")], |t| {
                    t.write("<div>");
                    t.write_expr_escaped(t.field_access(t.var("user"), "name"));
                    t.write("</div>");
                }),
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
                view UserProfile(user: test::User) {
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

                export function UserProfile({user}: {user: User}): string {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml(user.name);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn record_literal() {
        check(
            IrModuleBuilder::new()
                .record("User", [("name", "String"), ("age", "Int")])
                .view_no_params("CreateUser", |t| {
                    t.write("<div>");
                    let user = t.record("User", vec![("name", t.str("John")), ("age", t.int(30))]);
                    t.write_expr_escaped(t.field_access(user, "name"));
                    t.write("</div>");
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                  age: Int,
                }
                view CreateUser() {
                  write("<div>")
                  write_escaped(User {name: "John", age: 30}.name)
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
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_declaration() {
        check(
            IrModuleBuilder::new()
                .record("Node", [("value", "Int"), ("next", "Option[Node]")])
                .view("Test", [("node", "Node")], |t| {
                    t.write_expr_escaped(t.int_to_string(t.field_access(t.var("node"), "value")));
                }),
            expect![[r#"
                -- before --
                record Node {
                  value: Int,
                  next: Option[test::Node],
                }
                view Test(node: test::Node) {
                  write_escaped(node.value.to_string())
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T = never>(): Option<T> {
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

                export class Node {
                    constructor(
                        public readonly value: number,
                        public readonly next: Option.Option<Node>,
                    ) {}
                }

                export function Test({node}: {node: Node}): string {
                    let output: string = "";
                    output += escapeHtml((node.value).toString());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_declaration() {
        check(
            IrModuleBuilder::new()
                .enum_(
                    "IntList",
                    [
                        ("Cons", vec![("head", "Int"), ("tail", "IntList")]),
                        ("Nil", vec![]),
                    ],
                )
                .view_no_params("Test", |t| {
                    t.write("hello");
                }),
            expect![[r#"
                -- before --
                enum IntList {
                  Cons {head: Int, tail: test::IntList},
                  Nil,
                }
                view Test() {
                  write("hello")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace IntList {
                    export type IntList = { readonly tag: "Cons", readonly head: number, readonly tail: IntList.IntList } | { readonly tag: "Nil" };

                    export function Cons(head: number, tail: IntList.IntList): IntList {
                        return { tag: "Cons", head, tail };
                    }
                    export function Nil(): IntList {
                        return { tag: "Nil" };
                    }
                }

                export function Test(): string {
                    let output: string = "";
                    output += "hello";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_literal() {
        check(
            IrModuleBuilder::new()
                .record("Node", [("value", "Int"), ("next", "Option[Node]")])
                .view_no_params("Test", |t| {
                    let inner =
                        t.record("Node", vec![("value", t.int(1)), ("next", t.none("Node"))]);
                    let node = t.record("Node", vec![("value", t.int(2)), ("next", t.some(inner))]);
                    t.let_stmt("node", node, |t| {
                        t.write_expr_escaped(
                            t.int_to_string(t.field_access(t.var("node"), "value")),
                        );
                    });
                }),
            expect![[r#"
                -- before --
                record Node {
                  value: Int,
                  next: Option[test::Node],
                }
                view Test() {
                  let node = Node {
                    value: 2,
                    next: Option[test::Node]::Some(Node {
                      value: 1,
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    write_escaped(node.value.to_string())
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Option {
                    export type Option<T> = { readonly tag: "None" } | { readonly tag: "Some", value: T };

                    export function some<T>(value: T): Option<T> {
                        return { tag: "Some", value };
                    }
                    export function none<T = never>(): Option<T> {
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

                export class Node {
                    constructor(
                        public readonly value: number,
                        public readonly next: Option.Option<Node>,
                    ) {}
                }

                export function Test(): string {
                    let output: string = "";
                    const node = new Node(2, Option.some<Node>(new Node(1, Option.none<Node>())));
                    output += escapeHtml((node.value).toString());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn match_expression() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Green", "Blue"])
                .view("ColorName", [("color", "Color")], |t| {
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
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                view ColorName(color: test::Color) {
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

                export function ColorName({color}: {color: Color.Color}): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      switch (color.tag) {
                        case "Red": return "red";
                        case "Green": return "green";
                        case "Blue": return "blue";
                      }
                    })());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn bool_match_expression() {
        check(
            IrModuleBuilder::new().view("IsActive", [("active", "Bool")], |t| {
                let match_result = t.bool_match_expr(t.var("active"), t.str("yes"), t.str("no"));
                t.write_expr_escaped(match_result);
            }),
            expect![[r#"
                -- before --
                view IsActive(active: Bool) {
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

                export function IsActive({active}: {active: boolean}): string {
                    let output: string = "";
                    output += escapeHtml((active ? "yes" : "no"));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_expression() {
        check(
            IrModuleBuilder::new().view("CheckOption", [("opt", "Option[Int]")], |t| {
                let match_result =
                    t.option_match_expr(t.var("opt"), t.str("has value"), t.str("empty"));
                t.write_expr_escaped(match_result);
            }),
            expect![[r#"
                -- before --
                view CheckOption(opt: Option[Int]) {
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
                    export function none<T = never>(): Option<T> {
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

                export function CheckOption({opt}: {opt: Option.Option<number>}): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      switch (opt.tag) {
                        case "Some": return "has value";
                        case "None": return "empty";
                      }
                    })());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn nested_option_match_expression() {
        check(
            IrModuleBuilder::new().view(
                "CheckNestedOption",
                [("opt", "Option[Option[Bool]]")],
                |t| {
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
                },
            ),
            expect![[r#"
                -- before --
                view CheckNestedOption(opt: Option[Option[Bool]]) {
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
                    export function none<T = never>(): Option<T> {
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

                export function CheckNestedOption({opt}: {opt: Option.Option<Option.Option<boolean>>}): string {
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
                }
            "#]],
        );
    }

    #[test]
    fn let_expression() {
        check(
            IrModuleBuilder::new().view("LetExpr", [("name", "String")], |t| {
                // let x = name in x
                let result = t.let_expr("x", t.var("name"), |t| t.var("x"));
                t.write_expr_escaped(result);
            }),
            expect![[r#"
                -- before --
                view LetExpr(name: String) {
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

                export function LetExpr({name}: {name: string}): string {
                    let output: string = "";
                    output += escapeHtml((() => {
                      const x = name;
                      return x;
                    })());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            IrModuleBuilder::new().view("DisplayOption", [("opt", "Option[String]")], |t| {
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
            }),
            expect![[r#"
                -- before --
                view DisplayOption(opt: Option[String]) {
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
                    export function none<T = never>(): Option<T> {
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

                export function DisplayOption({opt}: {opt: Option.Option<string>}): string {
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
                }
            "#]],
        );
    }

    #[test]
    fn option_literal() {
        check(
            IrModuleBuilder::new().view(
                "TestOptionLiteral",
                [("opt1", "Option[String]"), ("opt2", "Option[String]")],
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
            ),
            expect![[r#"
                -- before --
                view TestOptionLiteral(
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
                    export function none<T = never>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                export function TestOptionLiteral({opt1, opt2}: {opt1: Option.Option<string>, opt2: Option.Option<string>}): string {
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
                }
            "#]],
        );
    }

    #[test]
    fn option_literal_inline_match_stmt() {
        check(
            IrModuleBuilder::new().view_no_params("TestInlineMatch", |t| {
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
            }),
            expect![[r#"
                -- before --
                view TestInlineMatch() {
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
                    export function none<T = never>(): Option<T> {
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
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement_on_expression_subject() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.option_match_stmt(
                    t.some(t.str("x")),
                    Some("value"),
                    |t| {
                        t.option_match_stmt(
                            t.some(t.var("value")),
                            Some("inner"),
                            |t| {
                                t.write_expr(t.var("inner"), false);
                            },
                            |t| {
                                t.write("none2");
                            },
                        );
                    },
                    |t| {
                        t.write("none1");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(value) => {
                      match Option[String]::Some(value) {
                        Some(inner) => {
                          write_expr(inner)
                        }
                        None => {
                          write("none2")
                        }
                      }
                    }
                    None => {
                      write("none1")
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
                    export function none<T = never>(): Option<T> {
                        return { tag: "None" };
                    }
                }

                export function Test(): string {
                    let output: string = "";
                    {
                        const $subject = Option.some<string>("x");
                        switch ($subject.tag) {
                            case "Some": {
                                const value = $subject.value;
                                {
                                    const $subject = Option.some<string>(value);
                                    switch ($subject.tag) {
                                        case "Some": {
                                            const inner = $subject.value;
                                            output += inner;
                                            break;
                                        }
                                        case "None": {
                                            output += "none2";
                                            break;
                                        }
                                    }
                                }
                                break;
                            }
                            case "None": {
                                output += "none1";
                                break;
                            }
                        }
                    }
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn bool_match_expression_on_expression_subject() {
        check(
            IrModuleBuilder::new().view("IsActive", [("active", "Bool")], |t| {
                let match_result =
                    t.bool_match_expr(t.not(t.var("active")), t.str("yes"), t.str("no"));
                t.write_expr_escaped(match_result);
            }),
            expect![[r#"
                -- before --
                view IsActive(active: Bool) {
                  write_escaped(match (!active) {
                    true => "yes",
                    false => "no",
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

                export function IsActive({active}: {active: boolean}): string {
                    let output: string = "";
                    output += escapeHtml((!(active) ? "yes" : "no"));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_with_fields() {
        check(
            IrModuleBuilder::new()
                .enum_(
                    "Outcome",
                    [
                        ("Ok", vec![("value", "Int")]),
                        ("Err", vec![("message", "String")]),
                    ],
                )
                .view("ShowOutcome", [("r", "Outcome")], |t| {
                    t.write("<div>");
                    let ok =
                        t.enum_variant_with_fields("Outcome", "Ok", vec![("value", t.int(42))]);
                    t.let_stmt("ok", ok, |t| {
                        t.write_expr(t.str("Created Ok!"), false);
                    });
                    t.write("</div>");
                }),
            expect![[r#"
                -- before --
                enum Outcome {
                  Ok {value: Int},
                  Err {message: String},
                }
                view ShowOutcome(r: test::Outcome) {
                  write("<div>")
                  let ok = Outcome::Ok {value: 42} in {
                    write_expr("Created Ok!")
                  }
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Outcome {
                    export type Outcome = { readonly tag: "Ok", readonly value: number } | { readonly tag: "Err", readonly message: string };

                    export function Ok(value: number): Outcome {
                        return { tag: "Ok", value };
                    }
                    export function Err(message: string): Outcome {
                        return { tag: "Err", message };
                    }
                }

                export function ShowOutcome({r}: {r: Outcome.Outcome}): string {
                    let output: string = "";
                    output += "<div>";
                    const ok = Outcome.Ok(42);
                    output += "Created Ok!";
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_match_with_field_bindings() {
        check(
            IrModuleBuilder::new()
                .enum_(
                    "Outcome",
                    [
                        ("Ok", vec![("value", "String")]),
                        ("Err", vec![("message", "String")]),
                    ],
                )
                .view("ShowOutcome", [("r", "Outcome")], |t| {
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
                }),
            expect![[r#"
                -- before --
                enum Outcome {
                  Ok {value: String},
                  Err {message: String},
                }
                view ShowOutcome(r: test::Outcome) {
                  match r {
                    Outcome::Ok(value: v) => {
                      write("Value: ")
                      write_expr(v)
                    }
                    Outcome::Err(message: m) => {
                      write("Error: ")
                      write_expr(m)
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Outcome {
                    export type Outcome = { readonly tag: "Ok", readonly value: string } | { readonly tag: "Err", readonly message: string };

                    export function Ok(value: string): Outcome {
                        return { tag: "Ok", value };
                    }
                    export function Err(message: string): Outcome {
                        return { tag: "Err", message };
                    }
                }

                export function ShowOutcome({r}: {r: Outcome.Outcome}): string {
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
                }
            "#]],
        );
    }

    #[test]
    fn transpiles_let_fragment_as_nested_buffer() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.let_fragment(
                    "v_0",
                    |t| {
                        t.write("<b>hi</b>");
                    },
                    |t| {
                        t.write_expr(t.var("v_0"), false);
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v_0 = {
                    write("<b>hi</b>")
                  } in {
                    write_expr(v_0)
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                type Fragment = string & { readonly __brand: unique symbol };

                /** Marks a string as trusted HTML, bypassing escaping. Only use with sanitized or trusted content. Calling this function with untrusted content causes XSS vulnerabilities. */
                export function trustHtml(str: string): Fragment {
                    return str as Fragment;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_0 = (() => {
                        let output: string = "";
                        output += "<b>hi</b>";
                        return output as Fragment;
                    })();
                    output += v_0;
                    return output;
                }
            "#]],
        );
    }
}
