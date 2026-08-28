use pretty::{Arena, DocAllocator};

use super::{Doc, Transpiler};
use crate::expr::patterns::{EnumPattern, Match};
use crate::expr::typing::r#type::Type;
use crate::expr::typing::type_registry::TypeRegistry;
use crate::ir::ir_var::IrVar;
use crate::ir::var_id::{VarId, VarIdCounter};
use crate::ir::writer_module::{
    WriterArgument, WriterExpr, WriterForSource, WriterFunctionBody, WriterFunctionDeclaration,
    WriterModule, WriterParameter, WriterStatement, WriterViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;

/// Names every variable in the generated code, derived from the IR's variable
/// identity rather than the source name.
///
/// Each binder within a declaration has a distinct `VarId`, so this is unique
/// per scope by construction: no hop identifier can shadow another, and no name
/// can collide with a TypeScript reserved word or with the `output` buffer.
fn var_ident(var: &IrVar) -> String {
    var_id_ident(var.id)
}

/// The generated name for a `VarId`, whether it comes from a binder in the IR
/// or was minted by [`TsTranspiler::fresh_var`].
fn var_id_ident(id: VarId) -> String {
    format!("v_{id}")
}

/// Destructuring entry for a parameter: `name: v_0`. The property name stays
/// the source name, since it is the caller-facing argument name.
fn transpile_param_binding<'a>(arena: &'a Arena<'a>, param: &'a WriterParameter) -> Doc<'a> {
    arena
        .text(param.name().as_str())
        .append(arena.text(": "))
        .append(arena.text(var_ident(&param.var)))
}

pub struct TsTranspiler {
    /// Tracks whether Option type is used during transpilation
    needs_option: bool,
    /// Tracks whether escapeHtml function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether the floatToInt helper is used during transpilation
    needs_float_to_int: bool,
    /// Tracks whether Fragment type is used during transpilation
    needs_fragment: bool,
    /// Registry of the module currently being transpiled
    registry: TypeRegistry,
    /// Continues the module's variable numbering, so that names the transpiler
    /// needs for itself cannot collide with the ones bound in the IR.
    var_ids: VarIdCounter,
}

impl TsTranspiler {
    pub fn new() -> Self {
        Self {
            needs_option: false,
            needs_escape_html: false,
            needs_float_to_int: false,
            needs_fragment: false,
            registry: TypeRegistry::default(),
            var_ids: VarIdCounter::new(),
        }
    }

    /// Emit a binding as a plain `const` declaration followed by the statements
    /// it scopes over.
    fn const_binding<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        binding_type: Doc<'a>,
        value: Doc<'a>,
        body: &'a [WriterStatement],
    ) -> Doc<'a> {
        let declaration = arena
            .text("const ")
            .append(arena.text(var_ident(var)))
            .append(arena.text(": "))
            .append(binding_type)
            .append(arena.text(" = "))
            .append(value)
            .append(arena.text(";"));
        if body.is_empty() {
            declaration
        } else {
            declaration
                .append(arena.hardline())
                .append(self.transpile_statements(arena, body))
        }
    }

    fn escape_string(&mut self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    // Helper method to wrap a string in double quotes
    fn quote_string(&mut self, s: &str) -> String {
        format!("\"{}\"", self.escape_string(s))
    }

    /// Mint a name for a variable the generated code needs but the IR does not
    /// bind, such as the subject of a match.
    fn fresh_var(&mut self) -> String {
        var_id_ident(self.var_ids.next())
    }

    /// The destructuring parameter of a view or function: the binding pattern
    /// and the type literal that annotates it, as in `{a: v_0}: {a: string}`.
    fn transpile_parameter_list<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parameters: &'a [WriterParameter],
    ) -> Doc<'a> {
        if parameters.is_empty() {
            return arena.nil();
        }
        let binding_docs: Vec<_> = parameters
            .iter()
            .map(|param| transpile_param_binding(arena, param))
            .collect();
        let type_docs: Vec<_> = parameters
            .iter()
            .map(|param| {
                arena
                    .text(param.name().as_str())
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, &param.typ))
            })
            .collect();
        let bindings = arena.intersperse(binding_docs, arena.text(",").append(arena.line()));
        let types = arena.intersperse(type_docs, arena.text(",").append(arena.line()));
        arena
            .text("{")
            .append(arena.line_().append(bindings).nest(4))
            .append(arena.line_())
            .append(arena.text("}: {"))
            .append(arena.line_().append(types).nest(4))
            .append(arena.line_())
            .append(arena.text("}"))
            .group()
    }

    /// The argument of a record or enum constructor call: `({a: 1, b: 2})`, or
    /// `()` when the type has no fields.
    fn transpile_field_object<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        base: Doc<'a>,
        fields: &'a [(FieldName, WriterExpr)],
    ) -> Doc<'a> {
        if fields.is_empty() {
            return base.append(arena.text(")"));
        }
        let field_docs: Vec<_> = fields
            .iter()
            .map(|(name, value)| {
                arena
                    .text(name.as_str())
                    .append(arena.text(": "))
                    .append(self.transpile_expr(arena, value))
            })
            .collect();
        base.append(arena.text("{"))
            .append(
                arena
                    .line_()
                    .append(arena.intersperse(field_docs, arena.text(",").append(arena.line())))
                    .nest(4),
            )
            .append(arena.line_())
            .append(arena.text("})"))
            .group()
    }

    /// Bind a match subject to `name` and emit the switch over it.
    fn bind_match_subject_stmt<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a WriterExpr,
        name: String,
        switch_doc: Doc<'a>,
    ) -> Doc<'a> {
        arena
            .text("const ")
            .append(arena.text(name))
            .append(arena.text(": "))
            .append(self.transpile_type(arena, subject.as_type()))
            .append(arena.text(" = "))
            .append(self.transpile_expr(arena, subject))
            .append(arena.text(";"))
            .append(arena.hardline())
            .append(switch_doc)
    }

    /// The same in expression position, where the binding has to be an arrow
    /// function parameter because there is no statement to declare it in.
    fn bind_match_subject_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a WriterExpr,
        name: String,
        switch_body: Doc<'a>,
    ) -> Doc<'a> {
        arena
            .text("((")
            .append(arena.text(name))
            .append(arena.text(": "))
            .append(self.transpile_type(arena, subject.as_type()))
            .append(arena.text(") => {"))
            .append(arena.line().append(switch_body).nest(2))
            .append(arena.line())
            .append(arena.text("})("))
            .append(self.transpile_expr(arena, subject))
            .append(arena.text(")"))
            .group()
    }

    fn transpile_bool_subject<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a WriterExpr,
    ) -> Doc<'a> {
        match subject {
            WriterExpr::BooleanLiteral { .. } => self.transpile_expr(arena, subject),
            _ => arena
                .text("(")
                .append(self.transpile_expr(arena, subject))
                .append(arena.text(" as boolean)")),
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

    fn transpile_module(&mut self, module: &WriterModule, registry: &TypeRegistry) -> String {
        // Reset tracking flags for this module
        self.needs_option = false;
        self.needs_escape_html = false;
        self.needs_float_to_int = false;
        self.needs_fragment = false;
        self.registry = registry.clone();
        self.var_ids = module.var_ids;

        let arena = &Arena::new();

        let views = &module.views;
        let records = &module.records;

        let mut result = arena.nil();

        // Add enum type definitions (namespace-based)
        for enum_def in &module.enums {
            // Generate namespace with tagged union type and constructor functions
            let variant_type_docs: Vec<_> = enum_def
                .variants
                .iter()
                .map(|variant| {
                    let base = arena
                        .text("{ readonly _tag: \"")
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("\""));
                    if variant.fields.is_empty() {
                        base.append(arena.text(" }"))
                    } else {
                        let field_docs: Vec<_> = variant
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
                        .append(arena.text("        return { _tag: \""))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("\" };"))
                        .append(arena.line())
                        .append(arena.text("    }"));
                } else {
                    // Variant with fields: add parameters
                    let param_with_type_docs: Vec<_> = variant
                        .fields
                        .iter()
                        .map(|(field_name, field_type, _)| {
                            arena
                                .text(field_name.as_str())
                                .append(arena.text(": "))
                                .append(self.transpile_type(arena, field_type))
                        })
                        .collect();
                    let field_name_docs: Vec<_> = variant
                        .fields
                        .iter()
                        .map(|(field_name, _, _)| {
                            arena
                                .text(", ")
                                .append(arena.text(field_name.as_str()))
                                .append(arena.text(": init."))
                                .append(arena.text(field_name.as_str()))
                        })
                        .collect();
                    result = result
                        .append(arena.text("    export function "))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("(init: {"))
                        .append(arena.intersperse(param_with_type_docs, arena.text(", ")))
                        .append(arena.text("}): "))
                        .append(arena.text(enum_def.name.as_str()))
                        .append(arena.text(" {"))
                        .append(arena.line())
                        .append(arena.text("        return { _tag: \""))
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
                if record.fields.is_empty() {
                    result = result
                        .append(arena.text("export class "))
                        .append(arena.text(record.name.as_str()))
                        .append(arena.text(" {}"))
                        .append(arena.line())
                        .append(arena.line());
                } else {
                    let field_docs: Vec<_> = record
                        .fields
                        .iter()
                        .map(|(name, ty, _)| {
                            arena
                                .text("public readonly ")
                                .append(arena.text(name.as_str()))
                                .append(arena.text(": "))
                                .append(self.transpile_type(arena, ty))
                                .append(arena.text(";"))
                        })
                        .collect();
                    let param_with_type_docs: Vec<_> = record
                        .fields
                        .iter()
                        .map(|(field_name, field_type, _)| {
                            arena
                                .text(field_name.as_str())
                                .append(arena.text(": "))
                                .append(self.transpile_type(arena, field_type))
                        })
                        .collect();
                    let assignment_docs: Vec<_> = record
                        .fields
                        .iter()
                        .map(|(name, _, _)| {
                            arena
                                .text("this.")
                                .append(arena.text(name.as_str()))
                                .append(arena.text(" = init."))
                                .append(arena.text(name.as_str()))
                                .append(arena.text(";"))
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
                                .append(arena.intersperse(field_docs.clone(), arena.line()))
                                .append(arena.line())
                                .nest(4),
                        )
                        .append(
                            arena
                                .nil()
                                .append(arena.line())
                                .append(arena.text("constructor(init: {"))
                                .append(arena.intersperse(param_with_type_docs, arena.text(", ")))
                                .append(arena.text("}) {"))
                                .append(
                                    arena
                                        .nil()
                                        .append(arena.line())
                                        .append(arena.intersperse(assignment_docs, arena.line()))
                                        .append(arena.line())
                                        .nest(4),
                                )
                                .append(arena.text("}"))
                                .append(arena.line())
                                .nest(4),
                        )
                        .append(arena.text("}"))
                        .append(arena.line())
                        .append(arena.line());
                }
            }
        }

        // Add function definitions
        for function in &module.functions {
            result = result
                .append(self.transpile_function_def(arena, function))
                .append(arena.hardline())
                .append(arena.hardline());
        }

        let view_docs: Vec<_> = views
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

        if self.needs_float_to_int {
            let float_to_int_fn = arena
                .nil()
                .append(arena.intersperse(
                    [
                        arena.text("function floatToInt(f: number): number {"),
                        arena.text("    if (globalThis.Number.isNaN(f)) return 0;"),
                        arena.text("    if (f >= 2147483647) return 2147483647;"),
                        arena.text("    if (f <= -2147483648) return -2147483648;"),
                        arena.text("    return globalThis.Math.trunc(f);"),
                        arena.text("}"),
                    ],
                    arena.hardline(),
                ))
                .append(arena.line())
                .append(arena.line());
            result = float_to_int_fn.append(result);
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
        view: &'a WriterViewDeclaration,
    ) -> Doc<'a> {
        let parameters = self.transpile_parameter_list(arena, &view.parameters);
        arena
            .text("export function ")
            .append(arena.text(name.as_ref()))
            .append(arena.text("("))
            .append(parameters)
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

    fn transpile_write_function_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a FunctionName,
        args: &'a [WriterArgument],
    ) -> Doc<'a> {
        let mut doc = arena
            .nil()
            .append(arena.text("output += render"))
            .append(arena.text(name.to_pascal_case()))
            .append(arena.text("("));

        if !args.is_empty() {
            // Build named arguments
            let arg_docs: Vec<_> = args
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

    fn transpile_function_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a WriterFunctionDeclaration,
    ) -> Doc<'a> {
        let parameters = self.transpile_parameter_list(arena, &function.parameters);
        let head = arena
            .text("function render")
            .append(arena.text(function.name.to_pascal_case()))
            .append(arena.text("("))
            .append(parameters);

        match &function.body {
            WriterFunctionBody::Writes(statements) => {
                let body = arena
                    .nil()
                    .append(arena.line())
                    .append(arena.text("let output: string = \"\";"))
                    .append(arena.line())
                    .append(self.transpile_statements(arena, statements))
                    .append(arena.line())
                    .append(arena.text("return output;"))
                    .append(arena.line());

                head.append(arena.text("): string {"))
                    .append(body.nest(4))
                    .append(arena.text("}"))
            }
            WriterFunctionBody::Returns(expr) => {
                let return_type = self.transpile_type(arena, &function.return_type);
                let body = self.transpile_expr(arena, expr);
                head.append(arena.text("): "))
                    .append(return_type)
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .nil()
                            .append(arena.line())
                            .append(arena.text("return "))
                            .append(body)
                            .append(arena.text(";"))
                            .append(arena.line())
                            .nest(4),
                    )
                    .append(arena.text("}"))
            }
        }
    }

    fn transpile_function_call_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a FunctionName,
        args: &'a [WriterArgument],
    ) -> Doc<'a> {
        let mut doc = arena
            .nil()
            .append(arena.text("render"))
            .append(arena.text(name.to_pascal_case()))
            .append(arena.text("("));

        if !args.is_empty() {
            let arg_docs: Vec<_> = args
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

        doc.append(arena.text(")"))
    }

    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("output += "))
            .append(arena.text(self.quote_string(content)))
            .append(arena.text(";"))
    }

    fn transpile_write_string_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a WriterExpr,
    ) -> Doc<'a> {
        self.needs_escape_html = true;
        arena
            .nil()
            .append(arena.text("output += escapeHtml("))
            .append(self.transpile_expr(arena, expr))
            .append(arena.text(");"))
    }

    fn transpile_write_fragment_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("output += "))
            .append(self.transpile_expr(arena, expr))
            .append(arena.text(";"))
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrVar>,
        source: &'a WriterForSource,
        body: &'a [WriterStatement],
    ) -> Doc<'a> {
        let var_name = var.map_or_else(|| "_".to_string(), var_ident);
        match source {
            WriterForSource::Array(array) => {
                let source_name = self.fresh_var();
                arena
                    .text("const ")
                    .append(arena.text(source_name.clone()))
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, array.as_type()))
                    .append(arena.text(" = "))
                    .append(self.transpile_expr(arena, array))
                    .append(arena.text(";"))
                    .append(arena.hardline())
                    .append(arena.text("for (const "))
                    .append(arena.text(var_name))
                    .append(arena.text(" of "))
                    .append(arena.text(source_name))
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
            WriterForSource::RangeInclusive { start, end } => {
                let start_name = self.fresh_var();
                let end_name = self.fresh_var();
                arena
                    .text("const ")
                    .append(arena.text(start_name.clone()))
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, start.as_type()))
                    .append(arena.text(" = "))
                    .append(self.transpile_expr(arena, start))
                    .append(arena.text(";"))
                    .append(arena.hardline())
                    .append(arena.text("const "))
                    .append(arena.text(end_name.clone()))
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, end.as_type()))
                    .append(arena.text(" = "))
                    .append(self.transpile_expr(arena, end))
                    .append(arena.text(";"))
                    .append(arena.hardline())
                    .append(arena.text("for (let "))
                    .append(arena.text(var_name.clone()))
                    .append(arena.text(" = "))
                    .append(arena.text(start_name))
                    .append(arena.text("; "))
                    .append(arena.text(var_name.clone()))
                    .append(arena.text(" <= "))
                    .append(arena.text(end_name))
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
                    .append(arena.text("}"))
            }
        }
    }

    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        value: &'a WriterExpr,
        body: &'a [WriterStatement],
    ) -> Doc<'a> {
        let binding_type = self.transpile_type(arena, value.as_type());
        let value = self.transpile_expr(arena, value);
        self.const_binding(arena, var, binding_type, value, body)
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<WriterExpr, Vec<WriterStatement>, IrVar>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let if_doc = arena
                    .text("if (")
                    .append(self.transpile_bool_subject(arena, subject))
                    .append(arena.text(") {"))
                    .append(
                        arena
                            .nil()
                            .append(arena.hardline())
                            .append(self.transpile_statements(arena, true_body))
                            .append(arena.hardline())
                            .nest(4),
                    )
                    .append(arena.text("}"));
                // An empty false arm emits no `else` branch.
                if false_body.is_empty() {
                    if_doc
                } else {
                    if_doc
                        .append(arena.text(" else {"))
                        .append(
                            arena
                                .nil()
                                .append(arena.hardline())
                                .append(self.transpile_statements(arena, false_body))
                                .append(arena.hardline())
                                .nest(4),
                        )
                        .append(arena.text("}"))
                }
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                self.needs_option = true;
                let subject_name = self.fresh_var();
                let some_case = if let Some(var_name) = some_arm_binding {
                    arena
                        .text("case \"Some\": {")
                        .append(
                            arena
                                .hardline()
                                .append(arena.text("const "))
                                .append(arena.text(var_ident(var_name)))
                                .append(arena.text(" = "))
                                .append(arena.text(subject_name.clone()))
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

                self.bind_match_subject_stmt(
                    arena,
                    subject,
                    subject_name.clone(),
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
                let subject_name = self.fresh_var();
                let case_docs: Vec<_> = arms
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
                                let destructure_docs: Vec<_> = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, var)| {
                                        arena
                                            .text(field.as_str())
                                            .append(arena.text(": "))
                                            .append(arena.text(var_ident(var)))
                                    })
                                    .collect();
                                arena
                                    .text("const { ")
                                    .append(arena.intersperse(destructure_docs, arena.text(", ")))
                                    .append(arena.text(" } = "))
                                    .append(arena.text(subject_name.clone()))
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

                self.bind_match_subject_stmt(
                    arena,
                    subject,
                    subject_name.clone(),
                    arena
                        .text("switch (")
                        .append(arena.text(subject_name.clone()))
                        .append(arena.text("._tag) {"))
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
        statements: &'a [WriterStatement],
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for stmt in statements {
            docs.push(self.transpile_statement(arena, stmt));
        }
        arena.intersperse(docs, arena.hardline())
    }

    fn transpile_var<'a>(&mut self, arena: &'a Arena<'a>, var: &'a IrVar) -> Doc<'a> {
        arena.text(var_ident(var))
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        object: &'a WriterExpr,
        field: &'a FieldName,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(self.transpile_expr(arena, object))
            .append(arena.text("."))
            .append(arena.text(field.as_str()))
    }

    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a> {
        arena
            .text("(")
            .append(arena.text(self.quote_string(value)))
            .append(arena.text(" as string)"))
    }

    /// The fragment body gets its own `output` buffer, so it is built by an
    /// immediately invoked arrow function rather than inline.
    fn transpile_fragment<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        body: &'a [WriterStatement],
    ) -> Doc<'a> {
        self.needs_fragment = true;
        arena
            .text("(() => {")
            .append(
                arena
                    .nil()
                    .append(arena.line())
                    .append(arena.text("let output: string = \"\";"))
                    .append(arena.line())
                    .append(self.transpile_statements(arena, body))
                    .append(arena.line())
                    .append(arena.text("return output as Fragment;"))
                    .append(arena.line())
                    .nest(4),
            )
            .append(arena.text("})()"))
    }

    fn transpile_boolean_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a> {
        match value {
            true => arena.text("(true as boolean)"),
            false => arena.text("(false as boolean)"),
        }
    }

    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a> {
        let text = if value.is_nan() {
            "(globalThis.NaN as number)".to_string()
        } else if value == f64::INFINITY {
            "(globalThis.Infinity as number)".to_string()
        } else if value == f64::NEG_INFINITY {
            "(-globalThis.Infinity as number)".to_string()
        } else {
            format!("({:?} as number)", value)
        };
        arena.text(text)
    }

    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i32) -> Doc<'a> {
        arena.text(format!("({} as number)", value))
    }

    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [WriterExpr],
        elem_type: &'a Type,
    ) -> Doc<'a> {
        if elements.is_empty() {
            return arena
                .text("([] as ")
                .append(self.transpile_array_type(arena, elem_type))
                .append(arena.text(")"));
        }
        let elem_docs: Vec<_> = elements
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
        fields: &'a [(FieldName, WriterExpr)],
    ) -> Doc<'a> {
        let base = arena
            .text("new ")
            .append(arena.text(record_name))
            .append(arena.text("("));
        self.transpile_field_object(arena, base, fields)
    }

    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, WriterExpr)],
    ) -> Doc<'a> {
        // Call the namespace constructor function: Color.Red() or Result.Ok(value)
        let base = arena
            .text(enum_name)
            .append(arena.text("."))
            .append(arena.text(variant_name))
            .append(arena.text("("));
        self.transpile_field_object(arena, base, fields)
    }

    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" <= "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: &'a WriterExpr) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("!("))
            .append(self.transpile_expr(arena, operand))
            .append(arena.text(")"))
    }

    fn transpile_int_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("((-("))
            .append(self.transpile_expr(arena, operand))
            .append(arena.text(")) | 0)"))
    }

    fn transpile_float_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a WriterExpr,
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
        parts: &'a [WriterExpr],
    ) -> Doc<'a> {
        if parts.is_empty() {
            return arena.text("\"\"");
        }
        arena
            .nil()
            .append(arena.text("("))
            .append(arena.intersperse(
                parts.iter().map(|part| self.transpile_expr(arena, part)),
                arena.text(" + "),
            ))
            .append(arena.text(")"))
    }

    fn transpile_logical_and<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("(("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" + "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(") | 0)"))
    }

    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("(("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" - "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(") | 0)"))
    }

    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("globalThis.Math.imul("))
            .append(self.transpile_expr(arena, left))
            .append(arena.text(", "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
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
        value: Option<&'a WriterExpr>,
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
        match_: &'a Match<WriterExpr, WriterExpr, IrVar>,
    ) -> Doc<'a> {
        match match_ {
            Match::Enum { subject, arms } => {
                let subject_name = self.fresh_var();
                let case_docs: Vec<_> =
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
                                    let destructure_docs: Vec<_> = arm
                                        .bindings
                                        .iter()
                                        .map(|(field, var)| {
                                            arena
                                                .text(field.as_str())
                                                .append(arena.text(": "))
                                                .append(arena.text(var_ident(var)))
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
                                                .append(arena.text(subject_name.clone()))
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
                    .append(arena.text(subject_name.clone()))
                    .append(arena.text("._tag) {"))
                    .append(arena.line().append(cases).nest(2))
                    .append(arena.line())
                    .append(arena.text("}"));

                self.bind_match_subject_expr(arena, subject, subject_name, switch_body)
            }
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => arena
                .text("(")
                .append(self.transpile_bool_subject(arena, subject))
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
                let subject_name = self.fresh_var();
                let some_case = {
                    let body_doc = self.transpile_expr(arena, some_arm_body);
                    if let Some(var_name) = some_arm_binding {
                        arena
                            .text("case \"Some\": {")
                            .append(
                                arena
                                    .line()
                                    .append(arena.text("const "))
                                    .append(arena.text(var_ident(var_name)))
                                    .append(arena.text(" = "))
                                    .append(arena.text(subject_name.clone()))
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
                    .append(arena.text(subject_name.clone()))
                    .append(arena.text(".tag) {"))
                    .append(arena.line().append(cases).nest(2))
                    .append(arena.line())
                    .append(arena.text("}"));

                self.bind_match_subject_expr(arena, subject, subject_name, switch_body)
            }
        }
    }

    /// A `let` in expression position.
    fn transpile_let<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        value: &'a WriterExpr,
        body: &'a WriterExpr,
    ) -> Doc<'a> {
        let param_type = self.transpile_type(arena, value.as_type());
        let value = self.transpile_expr(arena, value);
        arena
            .text("((")
            .append(arena.text(var_ident(var)))
            .append(arena.text(": "))
            .append(param_type)
            .append(arena.text(") => {"))
            .append(
                arena
                    .line()
                    .append(arena.text("return "))
                    .append(self.transpile_expr(arena, body))
                    .append(arena.text(";"))
                    .nest(2),
            )
            .append(arena.line())
            .append(arena.text("})("))
            .append(value)
            .append(arena.text(")"))
    }

    fn transpile_array_length<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        array: &'a WriterExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, array)
            .append(arena.text(".length"))
    }

    fn transpile_array_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        array: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, array))
            .append(arena.text(".length === 0)"))
    }

    fn transpile_string_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        string: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, string))
            .append(arena.text(".length === 0)"))
    }

    fn transpile_option_is_some<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, option))
            .append(arena.text(".tag === \"Some\")"))
    }

    fn transpile_option_is_none<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, option))
            .append(arena.text(".tag === \"None\")"))
    }

    fn transpile_int_to_string<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a WriterExpr,
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(").toString()"))
    }

    fn transpile_float_to_int<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a WriterExpr,
    ) -> Doc<'a> {
        self.needs_float_to_int = true;
        arena
            .text("floatToInt(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(")"))
    }

    fn transpile_int_to_float<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a WriterExpr,
    ) -> Doc<'a> {
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
    use super::*;
    use crate::ir::lower_pure;
    use crate::ir::pure_module_builder::{PureModuleBodiesBuilder, PureModuleBuilder};
    use expect_test::{Expect, expect};

    fn check(builder: impl Into<PureModuleBodiesBuilder>, expected: Expect) {
        let (module, registry) = builder.into().build_with_registry();
        let module = lower_pure(module);
        let before = module.to_string();
        let after = TsTranspiler::new().transpile_module(&module, &registry);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_view() {
        check(
            PureModuleBuilder::new()
                .view_no_params("HelloWorld", |t| t.raw("<h1>Hello, World!</h1>\n")),
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
    fn view_with_params_and_escaping() {
        check(
            PureModuleBuilder::new().view(
                "UserInfo",
                [("name", "String"), ("age", "String")],
                |t| {
                    t.concat(vec![
                        t.raw("<div>\n"),
                        t.raw("<h2>Name: "),
                        t.escape(t.var("name")),
                        t.raw("</h2>\n"),
                        t.raw("<p>Age: "),
                        t.escape(t.var("age")),
                        t.raw("</p>\n"),
                        t.raw("</div>\n"),
                    ])
                },
            ),
            expect![[r#"
                -- before --
                view UserInfo(name@v0: String, age@v1: String) {
                  write("<div>\n")
                  write("<h2>Name: ")
                  write_string(v0)
                  write("</h2>\n")
                  write("<p>Age: ")
                  write_string(v1)
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

                export function UserInfo({
                    name: v_0,
                    age: v_1
                }: {
                    name: string,
                    age: string
                }): string {
                    let output: string = "";
                    output += "<div>\n";
                    output += "<h2>Name: ";
                    output += escapeHtml(v_0);
                    output += "</h2>\n";
                    output += "<p>Age: ";
                    output += escapeHtml(v_1);
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
            PureModuleBuilder::new().view(
                "ConditionalDisplay",
                [("title", "String"), ("show", "Bool")],
                |t| {
                    t.bool_match_expr(
                        t.var("show"),
                        t.concat(vec![
                            t.raw("<h1>"),
                            t.escape(t.var("title")),
                            t.raw("</h1>\n"),
                        ]),
                        t.concat(vec![]),
                    )
                },
            ),
            expect![[r#"
                -- before --
                view ConditionalDisplay(title@v0: String, show@v1: Bool) {
                  match v1 {
                    true => {
                      write("<h1>")
                      write_string(v0)
                      write("</h1>\n")
                    }
                    false => {
                    }
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

                export function ConditionalDisplay({
                    title: v_0,
                    show: v_1
                }: {
                    title: string,
                    show: boolean
                }): string {
                    let output: string = "";
                    if ((v_1 as boolean)) {
                        output += "<h1>";
                        output += escapeHtml(v_0);
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
            PureModuleBuilder::new().view("ListItems", [("items", "Array[String]")], |t| {
                t.concat(vec![
                    t.raw("<ul>\n"),
                    t.fragment_for(Some("item"), t.var("items"), |t| {
                        t.concat(vec![
                            t.raw("<li>"),
                            t.escape(t.var("item")),
                            t.raw("</li>\n"),
                        ])
                    }),
                    t.raw("</ul>\n"),
                ])
            }),
            expect![[r#"
                -- before --
                view ListItems(items@v0: Array[String]) {
                  write("<ul>\n")
                  for v1 in v0 {
                    write("<li>")
                    write_string(v1)
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

                export function ListItems({items: v_0}: {items: string[]}): string {
                    let output: string = "";
                    output += "<ul>\n";
                    const v_2: string[] = v_0;
                    for (const v_1 of v_2) {
                        output += "<li>";
                        output += escapeHtml(v_1);
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
            PureModuleBuilder::new().view_no_params("Counter", |t| {
                t.fragment_for_range(Some("i"), t.int(1), t.int(3), |t| {
                    t.concat(vec![t.escape(t.int_to_string(t.var("i"))), t.raw(" ")])
                })
            }),
            expect![[r#"
                -- before --
                view Counter() {
                  for v0 in 1..=3 {
                    write_string(v0.to_string())
                    write(" ")
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

                export function Counter(): string {
                    let output: string = "";
                    const v_1: number = (1 as number);
                    const v_2: number = (3 as number);
                    for (let v_0 = v_1; v_0 <= v_2; v_0++) {
                        output += escapeHtml((v_0).toString());
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
            PureModuleBuilder::new().view_no_params("GreetingCard", |t| {
                t.let_expr("greeting", t.str("Hello from hop!"), |t| {
                    t.concat(vec![
                        t.raw("<div class=\"card\">\n"),
                        t.raw("<p>"),
                        t.escape(t.var("greeting")),
                        t.raw("</p>\n"),
                        t.raw("</div>\n"),
                    ])
                })
            }),
            expect![[r#"
                -- before --
                view GreetingCard() {
                  let v0 = "Hello from hop!" in {
                    write("<div class=\"card\">\n")
                    write("<p>")
                    write_string(v0)
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
                    const v_0: string = ("Hello from hop!" as string);
                    output += "<div class=\"card\">\n";
                    output += "<p>";
                    output += escapeHtml(v_0);
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
            PureModuleBuilder::new().view_no_params("TestMainComp", |t| {
                t.concat(vec![
                    t.raw("<div data-hop-id=\"test/card-comp\">"),
                    t.let_expr("title", t.str("Hello World"), |t| {
                        t.concat(vec![
                            t.raw("<h2>"),
                            t.escape(t.var("title")),
                            t.raw("</h2>"),
                        ])
                    }),
                    t.raw("</div>"),
                ])
            }),
            expect![[r#"
                -- before --
                view TestMainComp() {
                  write("<div data-hop-id=\"test/card-comp\">")
                  let v0 = "Hello World" in {
                    write("<h2>")
                    write_string(v0)
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
                    const v_0: string = ("Hello World" as string);
                    output += "<h2>";
                    output += escapeHtml(v_0);
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
            PureModuleBuilder::new().view(
                "RenderHtml",
                [("safe_content", "Fragment"), ("user_input", "String")],
                |t| {
                    t.concat(vec![
                        t.raw("<div>"),
                        t.var("safe_content"),
                        t.raw("</div><div>"),
                        t.escape(t.var("user_input")),
                        t.raw("</div>"),
                    ])
                },
            ),
            expect![[r#"
                -- before --
                view RenderHtml(
                  safe_content@v0: Fragment,
                  user_input@v1: String,
                ) {
                  write("<div>")
                  write_fragment(v0)
                  write("</div><div>")
                  write_string(v1)
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

                export function RenderHtml({
                    safe_content: v_0,
                    user_input: v_1
                }: {
                    safe_content: Fragment,
                    user_input: string
                }): string {
                    let output: string = "";
                    output += "<div>";
                    output += v_0;
                    output += "</div><div>";
                    output += escapeHtml(v_1);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn record_declarations() {
        check(
            PureModuleBuilder::new()
                .record(
                    "User",
                    [("name", "String"), ("age", "Int"), ("active", "Bool")],
                )
                .record("Address", [("street", "String"), ("city", "String")])
                .view("UserProfile", [("user", "User")], |t| {
                    t.concat(vec![
                        t.raw("<div>"),
                        t.escape(t.field_access(t.var("user"), "name")),
                        t.raw("</div>"),
                    ])
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
                view UserProfile(user@v0: test::User) {
                  write("<div>")
                  write_string(v0.name)
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
                    public readonly street: string;
                    public readonly city: string;

                    constructor(init: {street: string, city: string}) {
                        this.street = init.street;
                        this.city = init.city;
                    }
                }

                export class User {
                    public readonly name: string;
                    public readonly age: number;
                    public readonly active: boolean;

                    constructor(init: {name: string, age: number, active: boolean}) {
                        this.name = init.name;
                        this.age = init.age;
                        this.active = init.active;
                    }
                }

                export function UserProfile({user: v_0}: {user: User}): string {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml(v_0.name);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn record_literal() {
        check(
            PureModuleBuilder::new()
                .record("User", [("name", "String"), ("age", "Int")])
                .view_no_params("CreateUser", |t| {
                    let user = t.record("User", vec![("name", t.str("John")), ("age", t.int(30))]);
                    t.concat(vec![
                        t.raw("<div>"),
                        t.escape(t.field_access(user, "name")),
                        t.raw("</div>"),
                    ])
                }),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                  age: Int,
                }
                view CreateUser() {
                  write("<div>")
                  write_string(User {name: "John", age: 30}.name)
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
                    public readonly name: string;
                    public readonly age: number;

                    constructor(init: {name: string, age: number}) {
                        this.name = init.name;
                        this.age = init.age;
                    }
                }

                export function CreateUser(): string {
                    let output: string = "";
                    output += "<div>";
                    output += escapeHtml(new User({
                        name: ("John" as string),
                        age: (30 as number)
                    }).name);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_declaration() {
        check(
            PureModuleBuilder::new()
                .record("Node", [("value", "Int"), ("next", "Option[Node]")])
                .view("Test", [("node", "Node")], |t| {
                    t.escape(t.int_to_string(t.field_access(t.var("node"), "value")))
                }),
            expect![[r#"
                -- before --
                record Node {
                  value: Int,
                  next: Option[test::Node],
                }
                view Test(node@v0: test::Node) {
                  write_string(v0.value.to_string())
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
                    public readonly value: number;
                    public readonly next: Option.Option<Node>;

                    constructor(init: {value: number, next: Option.Option<Node>}) {
                        this.value = init.value;
                        this.next = init.next;
                    }
                }

                export function Test({node: v_0}: {node: Node}): string {
                    let output: string = "";
                    output += escapeHtml((v_0.value).toString());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_declaration() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "IntList",
                    [
                        ("Cons", vec![("head", "Int"), ("tail", "IntList")]),
                        ("Nil", vec![]),
                    ],
                )
                .view_no_params("Test", |t| t.raw("hello")),
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
                    export type IntList = { readonly _tag: "Cons", readonly head: number, readonly tail: IntList.IntList } | { readonly _tag: "Nil" };

                    export function Cons(init: {head: number, tail: IntList.IntList}): IntList {
                        return { _tag: "Cons", head: init.head, tail: init.tail };
                    }
                    export function Nil(): IntList {
                        return { _tag: "Nil" };
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
            PureModuleBuilder::new()
                .record("Node", [("value", "Int"), ("next", "Option[Node]")])
                .view_no_params("Test", |t| {
                    let inner =
                        t.record("Node", vec![("value", t.int(1)), ("next", t.none("Node"))]);
                    let node = t.record("Node", vec![("value", t.int(2)), ("next", t.some(inner))]);
                    t.let_expr("node", node, |t| {
                        t.escape(t.int_to_string(t.field_access(t.var("node"), "value")))
                    })
                }),
            expect![[r#"
                -- before --
                record Node {
                  value: Int,
                  next: Option[test::Node],
                }
                view Test() {
                  let v0 = Node {
                    value: 2,
                    next: Option[test::Node]::Some(Node {
                      value: 1,
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    write_string(v0.value.to_string())
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
                    public readonly value: number;
                    public readonly next: Option.Option<Node>;

                    constructor(init: {value: number, next: Option.Option<Node>}) {
                        this.value = init.value;
                        this.next = init.next;
                    }
                }

                export function Test(): string {
                    let output: string = "";
                    const v_0: Node = new Node({
                        value: (2 as number),
                        next: Option.some<Node>(new Node({
                            value: (1 as number),
                            next: Option.none<Node>()
                        }))
                    });
                    output += escapeHtml((v_0.value).toString());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn match_expression() {
        check(
            PureModuleBuilder::new()
                .enum_unit("Color", ["Red", "Green", "Blue"])
                .view("ColorName", [("color", "Color")], |t| {
                    // Use match expression to convert color to string
                    let match_result = t.enum_match_expr(t.var("color"), |m| {
                        m.arm("Red", |t| t.str("red"));
                        m.arm("Green", |t| t.str("green"));
                        m.arm("Blue", |t| t.str("blue"));
                    });
                    t.escape(match_result)
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                view ColorName(color@v0: test::Color) {
                  write_string(match v0 {
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
                    export type Color = { readonly _tag: "Red" } | { readonly _tag: "Green" } | { readonly _tag: "Blue" };

                    export function Red(): Color {
                        return { _tag: "Red" };
                    }
                    export function Green(): Color {
                        return { _tag: "Green" };
                    }
                    export function Blue(): Color {
                        return { _tag: "Blue" };
                    }
                }

                export function ColorName({color: v_0}: {color: Color.Color}): string {
                    let output: string = "";
                    output += escapeHtml(((v_1: Color.Color) => {
                      switch (v_1._tag) {
                        case "Red": return ("red" as string);
                        case "Green": return ("green" as string);
                        case "Blue": return ("blue" as string);
                      }
                    })(v_0));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn bool_match_expression() {
        check(
            PureModuleBuilder::new().view("IsActive", [("active", "Bool")], |t| {
                let match_result = t.bool_match_expr(t.var("active"), t.str("yes"), t.str("no"));
                t.escape(match_result)
            }),
            expect![[r#"
                -- before --
                view IsActive(active@v0: Bool) {
                  write_string(match v0 {true => "yes", false => "no"})
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

                export function IsActive({active: v_0}: {active: boolean}): string {
                    let output: string = "";
                    output += escapeHtml(((v_0 as boolean) ? ("yes" as string) : ("no" as string)));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_expression() {
        check(
            PureModuleBuilder::new().view("CheckOption", [("opt", "Option[Int]")], |t| {
                let match_result =
                    t.option_match_expr(t.var("opt"), t.str("has value"), t.str("empty"));
                t.escape(match_result)
            }),
            expect![[r#"
                -- before --
                view CheckOption(opt@v0: Option[Int]) {
                  write_string(match v0 {
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

                export function CheckOption({opt: v_0}: {opt: Option.Option<number>}): string {
                    let output: string = "";
                    output += escapeHtml(((v_1: Option.Option<number>) => {
                      switch (v_1.tag) {
                        case "Some": return ("has value" as string);
                        case "None": return ("empty" as string);
                      }
                    })(v_0));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn nested_option_match_expression() {
        check(
            PureModuleBuilder::new().view(
                "CheckNestedOption",
                [("opt", "Option[Option[Bool]]")],
                |t| {
                    // Outer match on opt: Some(v0) => middle_match, None => "none"
                    let outer_match = t.option_match_expr_with_binding(
                        t.var("opt"),
                        "v0",
                        |t| {
                            // Middle match on v0 (Option[Bool]): Some(v1) => innermost_match, None => "some-none"
                            t.option_match_expr_with_binding(
                                t.var("v0"),
                                "v1",
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

                    t.escape(outer_match)
                },
            ),
            expect![[r#"
                -- before --
                view CheckNestedOption(opt@v0: Option[Option[Bool]]) {
                  write_string(match v0 {
                    Some(v1) => match v1 {
                      Some(v2) => match v2 {
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

                export function CheckNestedOption({
                    opt: v_0
                }: {
                    opt: Option.Option<Option.Option<boolean>>
                }): string {
                    let output: string = "";
                    output += escapeHtml(((v_3: Option.Option<Option.Option<boolean>>) => {
                      switch (v_3.tag) {
                        case "Some": {
                          const v_1 = v_3.value;
                          return ((v_4: Option.Option<boolean>) => {
                            switch (v_4.tag) {
                              case "Some": {
                                const v_2 = v_4.value;
                                return ((v_2 as boolean) ? ("some-some-true" as string) : ("some-some-false" as string));
                              }
                              case "None": return ("some-none" as string);
                            }
                          })(v_1);
                        }
                        case "None": return ("none" as string);
                      }
                    })(v_0));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn let_expression() {
        check(
            PureModuleBuilder::new().view("LetExpr", [("name", "String")], |t| {
                // let x = name in x
                let result = t.let_expr("x", t.var("name"), |t| t.var("x"));
                t.escape(result)
            }),
            expect![[r#"
                -- before --
                view LetExpr(name@v0: String) {
                  write_string(let v1 = v0 in v1)
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

                export function LetExpr({name: v_0}: {name: string}): string {
                    let output: string = "";
                    output += escapeHtml(((v_1: string) => {
                      return v_1;
                    })(v_0));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            PureModuleBuilder::new().view("DisplayOption", [("opt", "Option[String]")], |t| {
                t.option_match_expr_with_binding(
                    t.var("opt"),
                    "value",
                    |t| {
                        t.concat(vec![
                            t.raw("<span>Found: "),
                            t.escape(t.var("value")),
                            t.raw("</span>"),
                        ])
                    },
                    t.concat(vec![t.raw("<span>Nothing</span>")]),
                )
            }),
            expect![[r#"
                -- before --
                view DisplayOption(opt@v0: Option[String]) {
                  match v0 {
                    Some(v1) => {
                      write("<span>Found: ")
                      write_string(v1)
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

                export function DisplayOption({
                    opt: v_0
                }: {
                    opt: Option.Option<string>
                }): string {
                    let output: string = "";
                    const v_2: Option.Option<string> = v_0;
                    switch (v_2.tag) {
                        case "Some": {
                            const v_1 = v_2.value;
                            output += "<span>Found: ";
                            output += escapeHtml(v_1);
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
            PureModuleBuilder::new().view(
                "TestOptionLiteral",
                [("opt1", "Option[String]"), ("opt2", "Option[String]")],
                |t| {
                    // Test Some literal
                    let match_result =
                        t.option_match_expr(t.var("opt1"), t.str("has value"), t.str("empty"));

                    // Test None literal
                    let match_result2 =
                        t.option_match_expr(t.var("opt2"), t.str("HAS"), t.str("EMPTY"));

                    t.concat(vec![t.escape(match_result), t.escape(match_result2)])
                },
            ),
            expect![[r#"
                -- before --
                view TestOptionLiteral(
                  opt1@v0: Option[String],
                  opt2@v1: Option[String],
                ) {
                  write_string(match v0 {
                    Some(_) => "has value",
                    None => "empty",
                  })
                  write_string(match v1 {Some(_) => "HAS", None => "EMPTY"})
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

                export function TestOptionLiteral({
                    opt1: v_0,
                    opt2: v_1
                }: {
                    opt1: Option.Option<string>,
                    opt2: Option.Option<string>
                }): string {
                    let output: string = "";
                    output += escapeHtml(((v_2: Option.Option<string>) => {
                      switch (v_2.tag) {
                        case "Some": return ("has value" as string);
                        case "None": return ("empty" as string);
                      }
                    })(v_0));
                    output += escapeHtml(((v_3: Option.Option<string>) => {
                      switch (v_3.tag) {
                        case "Some": return ("HAS" as string);
                        case "None": return ("EMPTY" as string);
                      }
                    })(v_1));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_literal_inline_match_stmt() {
        check(
            PureModuleBuilder::new().view_no_params("TestInlineMatch", |t| {
                t.let_expr("opt", t.some(t.str("world")), |t| {
                    t.option_match_expr_with_binding(
                        t.var("opt"),
                        "val",
                        |t| t.concat(vec![t.raw("Got:"), t.escape(t.var("val"))]),
                        t.concat(vec![t.raw("Empty")]),
                    )
                })
            }),
            expect![[r#"
                -- before --
                view TestInlineMatch() {
                  let v0 = Option[String]::Some("world") in {
                    match v0 {
                      Some(v1) => {
                        write("Got:")
                        write_string(v1)
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

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function TestInlineMatch(): string {
                    let output: string = "";
                    const v_0: Option.Option<string> = Option.some<string>(("world" as string));
                    const v_2: Option.Option<string> = v_0;
                    switch (v_2.tag) {
                        case "Some": {
                            const v_1 = v_2.value;
                            output += "Got:";
                            output += escapeHtml(v_1);
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
            PureModuleBuilder::new().view_no_params("Test", |t| {
                t.option_match_expr_with_binding(
                    t.some(t.str("x")),
                    "value",
                    |t| {
                        t.option_match_expr_with_binding(
                            t.some(t.var("value")),
                            "inner",
                            |t| t.escape(t.var("inner")),
                            t.concat(vec![t.raw("none2")]),
                        )
                    },
                    t.concat(vec![t.raw("none1")]),
                )
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(v0) => {
                      match Option[String]::Some(v0) {
                        Some(v1) => {
                          write_string(v1)
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

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function Test(): string {
                    let output: string = "";
                    const v_2: Option.Option<string> = Option.some<string>(("x" as string));
                    switch (v_2.tag) {
                        case "Some": {
                            const v_0 = v_2.value;
                            const v_3: Option.Option<string> = Option.some<string>(v_0);
                            switch (v_3.tag) {
                                case "Some": {
                                    const v_1 = v_3.value;
                                    output += escapeHtml(v_1);
                                    break;
                                }
                                case "None": {
                                    output += "none2";
                                    break;
                                }
                            }
                            break;
                        }
                        case "None": {
                            output += "none1";
                            break;
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
            PureModuleBuilder::new().view("IsActive", [("active", "Bool")], |t| {
                let match_result =
                    t.bool_match_expr(t.not(t.var("active")), t.str("yes"), t.str("no"));
                t.escape(match_result)
            }),
            expect![[r#"
                -- before --
                view IsActive(active@v0: Bool) {
                  write_string(match (!v0) {true => "yes", false => "no"})
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

                export function IsActive({active: v_0}: {active: boolean}): string {
                    let output: string = "";
                    output += escapeHtml(((!(v_0) as boolean) ? ("yes" as string) : ("no" as string)));
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_with_fields() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Outcome",
                    [
                        ("Success", vec![("value", "Int")]),
                        ("Failure", vec![("message", "String")]),
                    ],
                )
                .view("ShowOutcome", [("r", "Outcome")], |t| {
                    let ok = t.enum_variant_with_fields(
                        "Outcome",
                        "Success",
                        vec![("value", t.int(42))],
                    );
                    t.concat(vec![
                        t.raw("<div>"),
                        t.let_expr("ok", ok, |t| t.escape(t.str("Created Ok!"))),
                        t.raw("</div>"),
                    ])
                }),
            expect![[r#"
                -- before --
                enum Outcome {
                  Success {value: Int},
                  Failure {message: String},
                }
                view ShowOutcome(r@v0: test::Outcome) {
                  write("<div>")
                  let v1 = Outcome::Success {value: 42} in {
                    write_string("Created Ok!")
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

                export namespace Outcome {
                    export type Outcome = { readonly _tag: "Success", readonly value: number } | { readonly _tag: "Failure", readonly message: string };

                    export function Success(init: {value: number}): Outcome {
                        return { _tag: "Success", value: init.value };
                    }
                    export function Failure(init: {message: string}): Outcome {
                        return { _tag: "Failure", message: init.message };
                    }
                }

                export function ShowOutcome({r: v_0}: {r: Outcome.Outcome}): string {
                    let output: string = "";
                    output += "<div>";
                    const v_1: Outcome.Outcome = Outcome.Success({value: (42 as number)});
                    output += escapeHtml(("Created Ok!" as string));
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_match_with_field_bindings() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Outcome",
                    [
                        ("Success", vec![("value", "String")]),
                        ("Failure", vec![("message", "String")]),
                    ],
                )
                .view("ShowOutcome", [("r", "Outcome")], |t| {
                    t.enum_match_expr(t.var("r"), |m| {
                        m.arm_bound("Success", [("value", "v")], |t| {
                            t.concat(vec![t.raw("Value: "), t.escape(t.var("v"))])
                        });
                        m.arm_bound("Failure", [("message", "m")], |t| {
                            t.concat(vec![t.raw("Error: "), t.escape(t.var("m"))])
                        });
                    })
                }),
            expect![[r#"
                -- before --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view ShowOutcome(r@v0: test::Outcome) {
                  match v0 {
                    Outcome::Success(value: v1) => {
                      write("Value: ")
                      write_string(v1)
                    }
                    Outcome::Failure(message: v2) => {
                      write("Error: ")
                      write_string(v2)
                    }
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

                export namespace Outcome {
                    export type Outcome = { readonly _tag: "Success", readonly value: string } | { readonly _tag: "Failure", readonly message: string };

                    export function Success(init: {value: string}): Outcome {
                        return { _tag: "Success", value: init.value };
                    }
                    export function Failure(init: {message: string}): Outcome {
                        return { _tag: "Failure", message: init.message };
                    }
                }

                export function ShowOutcome({r: v_0}: {r: Outcome.Outcome}): string {
                    let output: string = "";
                    const v_3: Outcome.Outcome = v_0;
                    switch (v_3._tag) {
                        case "Success": {
                            const { value: v_1 } = v_3;
                            output += "Value: ";
                            output += escapeHtml(v_1);
                            break;
                        }
                        case "Failure": {
                            const { message: v_2 } = v_3;
                            output += "Error: ";
                            output += escapeHtml(v_2);
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
            PureModuleBuilder::new().view_no_params("Test", |t| {
                t.let_expr("v_0", t.concat(vec![t.raw("<b>hi</b>")]), |t| t.var("v_0"))
            }),
            expect![[r#"
                -- before --
                view Test() {
                  let v0 = {
                    write("<b>hi</b>")
                  } in {
                    write_fragment(v0)
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
                    const v_0: Fragment = (() => {
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

    #[test]
    fn fragment_returning_function_called_in_value_position() {
        check(
            PureModuleBuilder::new()
                .function("Frag", [], "Fragment", |t| t.raw("<b>hi</b>"))
                .view_no_params("Test", |t| {
                    t.let_expr("x", t.call("Frag", vec![]), |t| t.var("x"))
                }),
            expect![[r#"
                -- before --
                fn Frag() -> Fragment {
                  write("<b>hi</b>")
                }
                view Test() {
                  let v0 = {
                    call Frag()
                  } in {
                    write_fragment(v0)
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                type Fragment = string & { readonly __brand: unique symbol };

                /** Marks a string as trusted HTML, bypassing escaping. Only use with sanitized or trusted content. Calling this function with untrusted content causes XSS vulnerabilities. */
                export function trustHtml(str: string): Fragment {
                    return str as Fragment;
                }

                function renderFrag(): string {
                    let output: string = "";
                    output += "<b>hi</b>";
                    return output;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_0: Fragment = (() => {
                        let output: string = "";
                        output += renderFrag();
                        return output as Fragment;
                    })();
                    output += v_0;
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn snake_case_function_name_mangling() {
        check(
            PureModuleBuilder::new()
                .function("format_price", [("price", "Int")], "Int", |t| {
                    t.var("price")
                })
                .view_no_params("Test", |t| {
                    t.escape(t.int_to_string(t.call("format_price", vec![("price", t.int(5))])))
                }),
            expect![[r#"
                -- before --
                fn format_price(price@v0: Int) -> Int {
                  v0
                }
                view Test() {
                  write_string(call format_price(price = 5).to_string())
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

                function renderFormatPrice({price: v_0}: {price: number}): number {
                    return v_0;
                }

                export function Test(): string {
                    let output: string = "";
                    output += escapeHtml((renderFormatPrice({price: (5 as number)})).toString());
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn function_called_in_range_bound_and_interpolation() {
        check(
            PureModuleBuilder::new()
                .function("foo", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.int(10))
                })
                .view_no_params("Test", |t| {
                    t.concat(vec![
                        t.raw("<div>"),
                        t.fragment_for_range(
                            Some("x"),
                            t.int(0),
                            t.call("foo", vec![("x", t.int(-7))]),
                            |t| t.concat(vec![t.escape(t.int_to_string(t.var("x"))), t.raw(",")]),
                        ),
                        t.escape(t.int_to_string(t.call("foo", vec![("x", t.int(10))]))),
                        t.raw("</div>"),
                    ])
                }),
            expect![[r#"
                -- before --
                fn foo(x@v0: Int) -> Int {
                  (v0 + 10)
                }
                view Test() {
                  write("<div>")
                  for v1 in 0..=call foo(x = -7) {
                    write_string(v1.to_string())
                    write(",")
                  }
                  write_string(call foo(x = 10).to_string())
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

                function renderFoo({x: v_0}: {x: number}): number {
                    return ((v_0 + (10 as number)) | 0);
                }

                export function Test(): string {
                    let output: string = "";
                    output += "<div>";
                    const v_2: number = (0 as number);
                    const v_3: number = renderFoo({x: (-7 as number)});
                    for (let v_1 = v_2; v_1 <= v_3; v_1++) {
                        output += escapeHtml((v_1).toString());
                        output += ",";
                    }
                    output += escapeHtml((renderFoo({x: (10 as number)})).toString());
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }
}
