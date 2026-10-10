use std::collections::HashMap;

use pretty::{Arena, DocAllocator};

use super::Doc;
use super::transpiler::Transpiler;
use crate::hop::typing::{Type, TypeRegistry};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::ir_parameter::IrParameter;
use crate::ir::writer_module::{
    ForSource, Let, Name, Stmt, ValueBlock, WriterFunctionBody, WriterFunctionDeclaration,
    WriterModule, WriterPageDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// Names every variable in the generated code after the name that binds it
/// in the IR rather than the source name.
///
/// Names are unique across the module, a let's as `v_` and a binder's as
/// `b_`, so no hop identifier can shadow another, and no name can collide
/// with a TypeScript reserved word or with the `output` buffer.
fn name_ident(name: Name) -> String {
    match name {
        Name::Binding(var) => format!("v_{}", var.index()),
        Name::Binder(binder) => format!("b_{}", binder.index()),
    }
}

/// Destructuring entry for a parameter: `name: v_0`. The property name stays
/// the source name, since it is the caller-facing argument name.
fn transpile_param_binding<'a>(arena: &'a Arena<'a>, param: &'a IrParameter) -> Doc<'a> {
    arena
        .text(param.name().as_str())
        .append(arena.text(": "))
        .append(arena.text(name_ident(Name::Binder(param.var))))
}

fn function_ident(function: &IrFunction) -> String {
    format!("render{}_{}", function.name.to_pascal_case(), function.id)
}

pub struct TsTranspiler {
    /// Tracks whether Option type is used during transpilation
    needs_option: bool,
    /// Tracks whether escapeHtml function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether the floatToInt helper is used during transpilation
    needs_float_to_int: bool,
    /// Tracks whether Html type is used during transpilation
    needs_html: bool,
    /// Registry of the module currently being transpiled
    registry: TypeRegistry,
    /// The type of every name bound so far. A match binds its subject to
    /// a fresh constant of the subject's declared type, so a nested match
    /// on the same name is not narrowed by the arm it sits in.
    name_types: HashMap<Name, Type>,
    /// Numbers the subject constants.
    subjects: usize,
}

impl TsTranspiler {
    pub fn new() -> Self {
        Self {
            needs_option: false,
            needs_escape_html: false,
            needs_float_to_int: false,
            needs_html: false,
            registry: TypeRegistry::default(),
            name_types: HashMap::new(),
            subjects: 0,
        }
    }

    /// Bind the subject of a match to a fresh constant of its declared
    /// type, and return the constant's name with the binding statement.
    fn bind_subject<'a>(&mut self, arena: &'a Arena<'a>, subject: Name) -> (String, Doc<'a>) {
        let name = format!("s_{}", self.subjects);
        self.subjects += 1;
        let typ = self.name_types[&subject].clone();
        let binding = arena
            .text("const ")
            .append(arena.text(name.clone()))
            .append(arena.text(": "))
            .append(self.transpile_type(arena, &typ))
            .append(arena.text(" = "))
            .append(arena.text(name_ident(subject)))
            .append(arena.text(";"))
            .append(arena.hardline());
        (name, binding)
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

    /// The destructuring parameter of a page: the binding pattern and the
    /// type literal that annotates it, as in `{a: v_0}: {a: string}`. The
    /// parameters' types are recorded.
    fn transpile_parameter_list<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parameters: &'a [IrParameter],
    ) -> Doc<'a> {
        for param in parameters {
            self.name_types
                .insert(Name::Binder(param.var), param.typ.clone());
        }
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

    /// The argument of a record or enum constructor call: `({a: e_1, b: e_2})`,
    /// or `()` when the type has no fields.
    fn transpile_field_object<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        base: Doc<'a>,
        fields: &'a [(FieldName, Name)],
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
                    .append(arena.text(name_ident(*value)))
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

    /// The arguments of a call, in the order of the callee's parameters:
    /// `(v_1, v_2)`.
    fn transpile_arguments<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        base: Doc<'a>,
        args: &'a [Name],
    ) -> Doc<'a> {
        let arg_docs: Vec<_> = args
            .iter()
            .map(|arg| arena.text(name_ident(*arg)))
            .collect();
        base.append(arena.intersperse(arg_docs, arena.text(", ")))
            .append(arena.text(")"))
    }

    /// A statement block, indented, between the braces the caller writes.
    fn transpile_block<'a>(&mut self, arena: &'a Arena<'a>, statements: &'a [Stmt]) -> Doc<'a> {
        arena
            .nil()
            .append(arena.hardline())
            .append(self.transpile_statements(arena, statements))
            .append(arena.hardline())
            .nest(4)
    }

    /// The arms of an enum match as switch cases, each ending in `tail`,
    /// with the arm's bindings destructured from `subject` first.
    fn transpile_enum_cases<'a, Body>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: Name,
        arms: &'a [EnumMatchArm<Body>],
        mut body: impl FnMut(&mut Self, &'a Body) -> Doc<'a>,
        tail: &'static str,
    ) -> Doc<'a> {
        let (subject_name, subject_binding) = self.bind_subject(arena, subject);
        let tail_doc = if tail.is_empty() {
            arena.nil()
        } else {
            arena.hardline().append(arena.text(tail))
        };
        let case_docs: Vec<_> = arms
            .iter()
            .map(|arm| {
                let EnumPattern::Variant { variant_name, .. } = &arm.pattern;
                for (_, binder) in &arm.bindings {
                    self.name_types
                        .insert(Name::Binder(binder.var), binder.typ.clone());
                }
                let bindings_doc = if arm.bindings.is_empty() {
                    arena.nil()
                } else {
                    let destructure_docs: Vec<_> = arm
                        .bindings
                        .iter()
                        .map(|(field, binder)| {
                            arena
                                .text(field.as_str())
                                .append(arena.text(": "))
                                .append(arena.text(name_ident(Name::Binder(binder.var))))
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
                            .append(body(self, &arm.body))
                            .append(tail_doc.clone())
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"))
            })
            .collect();
        subject_binding
            .append(arena.text("switch ("))
            .append(arena.text(subject_name))
            .append(arena.text("._tag) {"))
            .append(
                arena
                    .hardline()
                    .append(arena.intersperse(case_docs, arena.hardline()))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"))
    }

    /// The arms of an option match as switch cases, each ending in `tail`,
    /// with the some arm's binding read from `subject` first.
    fn transpile_option_cases<'a, Body>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: Name,
        some_arm_binding: Option<&'a IrBinder>,
        some_arm_body: &'a Body,
        none_arm_body: &'a Body,
        mut body: impl FnMut(&mut Self, &'a Body) -> Doc<'a>,
        tail: &'static str,
    ) -> Doc<'a> {
        self.needs_option = true;
        let (subject_name, subject_binding) = self.bind_subject(arena, subject);
        let tail_doc = if tail.is_empty() {
            arena.nil()
        } else {
            arena.hardline().append(arena.text(tail))
        };
        let binding_doc = match some_arm_binding {
            Some(binder) => {
                self.name_types
                    .insert(Name::Binder(binder.var), binder.typ.clone());
                arena
                    .text("const ")
                    .append(arena.text(name_ident(Name::Binder(binder.var))))
                    .append(arena.text(" = "))
                    .append(arena.text(subject_name.clone()))
                    .append(arena.text(".value;"))
                    .append(arena.hardline())
            }
            None => arena.nil(),
        };
        let some_case = arena
            .text("case \"Some\": {")
            .append(
                arena
                    .hardline()
                    .append(binding_doc)
                    .append(body(self, some_arm_body))
                    .append(tail_doc.clone())
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"));
        let none_case = arena
            .text("case \"None\": {")
            .append(
                arena
                    .hardline()
                    .append(body(self, none_arm_body))
                    .append(tail_doc)
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"));
        subject_binding
            .append(arena.text("switch ("))
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
            .append(arena.text("}"))
    }

    /// Wrap statements that return a value in an immediately invoked arrow
    /// function, which is how a value is computed by statements in
    /// expression position.
    fn transpile_iife<'a>(&mut self, arena: &'a Arena<'a>, body: Doc<'a>) -> Doc<'a> {
        arena
            .text("(() => {")
            .append(arena.hardline().append(body).nest(4))
            .append(arena.hardline())
            .append(arena.text("})()"))
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
        self.needs_html = false;
        self.registry = registry.clone();
        self.name_types.clear();
        self.subjects = 0;

        let arena = &Arena::new();

        let pages = &module.pages;

        let mut result = arena.nil();

        // Add enum type definitions (namespace-based)
        for (enum_name, variants) in registry.enums() {
            // Generate namespace with tagged union type and constructor functions

            result = result
                .append(arena.text("export namespace "))
                .append(arena.text(enum_name.as_str()))
                .append(arena.text(" {"))
                .append(arena.line())
                .append(arena.text("    export type "))
                .append(arena.text(enum_name.as_str()))
                .append(arena.text(" = "))
                .append(if variants.is_empty() {
                    arena.text("never")
                } else {
                    let docs: Vec<_> = variants
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
                                    .map(|field| {
                                        arena
                                            .text(", readonly ")
                                            .append(arena.text(field.name.as_str()))
                                            .append(arena.text(": "))
                                            .append(self.transpile_type(arena, &field.typ))
                                    })
                                    .collect();
                                base.append(arena.intersperse(field_docs, arena.nil()))
                                    .append(arena.text(" }"))
                            }
                        })
                        .collect();
                    arena.intersperse(docs, arena.text(" | "))
                })
                .append(arena.text(";"))
                .append(arena.line());

            // Generate constructor function for each variant
            for variant in variants {
                result = result.append(arena.line());

                if variant.fields.is_empty() {
                    // Unit variant: no parameters
                    result = result
                        .append(arena.text("    export function "))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("(): "))
                        .append(arena.text(enum_name.as_str()))
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
                        .map(|field| {
                            arena
                                .text(field.name.as_str())
                                .append(arena.text(": "))
                                .append(self.transpile_type(arena, &field.typ))
                        })
                        .collect();
                    let field_name_docs: Vec<_> = variant
                        .fields
                        .iter()
                        .map(|field| {
                            arena
                                .text(", ")
                                .append(arena.text(field.name.as_str()))
                                .append(arena.text(": init."))
                                .append(arena.text(field.name.as_str()))
                        })
                        .collect();
                    result = result
                        .append(arena.text("    export function "))
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text("(init: {"))
                        .append(arena.intersperse(param_with_type_docs, arena.text(", ")))
                        .append(arena.text("}): "))
                        .append(arena.text(enum_name.as_str()))
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
        for (record_name, fields) in registry.records() {
            if fields.is_empty() {
                result = result
                    .append(arena.text("export class "))
                    .append(arena.text(record_name.as_str()))
                    .append(arena.text(" {}"))
                    .append(arena.line())
                    .append(arena.line());
            } else {
                let field_docs: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        arena
                            .text("public readonly ")
                            .append(arena.text(field.name.as_str()))
                            .append(arena.text(": "))
                            .append(self.transpile_type(arena, &field.typ))
                            .append(arena.text(";"))
                    })
                    .collect();
                let param_with_type_docs: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        arena
                            .text(field.name.as_str())
                            .append(arena.text(": "))
                            .append(self.transpile_type(arena, &field.typ))
                    })
                    .collect();
                let assignment_docs: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        arena
                            .text("this.")
                            .append(arena.text(field.name.as_str()))
                            .append(arena.text(" = init."))
                            .append(arena.text(field.name.as_str()))
                            .append(arena.text(";"))
                    })
                    .collect();
                result = result
                    .append(arena.text("export class "))
                    .append(arena.text(record_name.as_str()))
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

        // Add function definitions
        for function in &module.functions {
            result = result
                .append(self.transpile_function_def(arena, function))
                .append(arena.hardline())
                .append(arena.hardline());
        }

        let page_docs: Vec<_> = pages
            .iter()
            .map(|page| self.transpile_page(arena, &page.name, page))
            .collect();
        result =
            result.append(arena.intersperse(page_docs, arena.hardline().append(arena.hardline())));

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
                                        arena.text(".replace(/\"/g, '&quot;');"),
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

        // Prepend Html type if needed (after transpilation determined it's used)
        if self.needs_html {
            let fragment = arena
                .nil()
                .append(arena.text("type Html = string & { readonly __brand: unique symbol };"))
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

    fn transpile_page<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        page: &'a WriterPageDeclaration,
    ) -> Doc<'a> {
        let parameters = self.transpile_parameter_list(arena, &page.parameters);
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
                    .append(self.transpile_statements(arena, &page.body))
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
        function: &'a IrFunction,
        args: &'a [Name],
    ) -> Doc<'a> {
        let base = arena
            .nil()
            .append(arena.text("output += "))
            .append(arena.text(function_ident(function)))
            .append(arena.text("("));
        self.transpile_arguments(arena, base, args)
            .append(arena.text(";"))
    }

    fn transpile_function_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a WriterFunctionDeclaration,
    ) -> Doc<'a> {
        // A function is internal to the module, so it takes its parameters
        // in order, as a call passes them.
        let param_docs: Vec<_> = function
            .parameters
            .iter()
            .map(|param| {
                self.name_types
                    .insert(Name::Binder(param.var), param.typ.clone());
                arena
                    .text(name_ident(Name::Binder(param.var)))
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, &param.typ))
            })
            .collect();
        let head = arena
            .text("function ")
            .append(arena.text(function_ident(&function.function)))
            .append(arena.text("("))
            .append(arena.intersperse(param_docs, arena.text(", ")));

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
            WriterFunctionBody::Returns(block) => {
                let return_type = self.transpile_type(arena, &function.return_type);
                let body = self.transpile_value_block(arena, block);
                head.append(arena.text("): "))
                    .append(return_type)
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .nil()
                            .append(arena.line())
                            .append(body)
                            .append(arena.line())
                            .nest(4),
                    )
                    .append(arena.text("}"))
            }
        }
    }

    fn transpile_function_call_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [Name],
    ) -> Doc<'a> {
        let base = arena
            .nil()
            .append(arena.text(function_ident(function)))
            .append(arena.text("("));
        self.transpile_arguments(arena, base, args)
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
        name: Name,
    ) -> Doc<'a> {
        self.needs_escape_html = true;
        arena
            .nil()
            .append(arena.text("output += escapeHtml("))
            .append(arena.text(name_ident(name)))
            .append(arena.text(");"))
    }

    fn transpile_write_html_statement<'a>(&mut self, arena: &'a Arena<'a>, name: Name) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("output += "))
            .append(arena.text(name_ident(name)))
            .append(arena.text(";"))
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrBinder>,
        source: &'a ForSource,
        body: &'a [Stmt],
    ) -> Doc<'a> {
        let var_name = match var {
            Some(binder) => {
                self.name_types
                    .insert(Name::Binder(binder.var), binder.typ.clone());
                name_ident(Name::Binder(binder.var))
            }
            None => "_".to_string(),
        };
        match source {
            ForSource::Array(array) => arena
                .text("for (const ")
                .append(arena.text(var_name))
                .append(arena.text(" of "))
                .append(arena.text(name_ident(*array)))
                .append(arena.text(") {"))
                .append(self.transpile_block(arena, body))
                .append(arena.text("}")),
            ForSource::RangeInclusive { start, end } => arena
                .text("for (let ")
                .append(arena.text(var_name.clone()))
                .append(arena.text(" = "))
                .append(arena.text(name_ident(*start)))
                .append(arena.text("; "))
                .append(arena.text(var_name.clone()))
                .append(arena.text(" <= "))
                .append(arena.text(name_ident(*end)))
                .append(arena.text("; "))
                .append(arena.text(var_name))
                .append(arena.text("++) {"))
                .append(self.transpile_block(arena, body))
                .append(arena.text("}")),
        }
    }

    fn transpile_let_statement<'a>(&mut self, arena: &'a Arena<'a>, let_: &'a Let) -> Doc<'a> {
        self.name_types
            .insert(Name::Binding(let_.name), let_.typ.clone());
        let binding_type = self.transpile_type(arena, &let_.typ);
        let value = self.transpile_value(arena, &let_.value, &let_.typ);
        arena
            .text("const ")
            .append(arena.text(name_ident(Name::Binding(let_.name))))
            .append(arena.text(": "))
            .append(binding_type)
            .append(arena.text(" = "))
            .append(value)
            .append(arena.text(";"))
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<Name, Vec<Stmt>>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let if_doc = arena
                    .text("if (")
                    .append(arena.text(name_ident(**subject)))
                    .append(arena.text(") {"))
                    .append(self.transpile_block(arena, true_body))
                    .append(arena.text("}"));
                // An empty false arm emits no `else` branch.
                if false_body.is_empty() {
                    if_doc
                } else {
                    if_doc
                        .append(arena.text(" else {"))
                        .append(self.transpile_block(arena, false_body))
                        .append(arena.text("}"))
                }
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => self.transpile_option_cases(
                arena,
                **subject,
                some_arm_binding.as_ref(),
                some_arm_body.as_ref(),
                none_arm_body.as_ref(),
                |this, body| this.transpile_statements(arena, body),
                "break;",
            ),
            Match::Enum { subject, arms } => self.transpile_enum_cases(
                arena,
                **subject,
                arms,
                |this, body| this.transpile_statements(arena, body),
                "break;",
            ),
        }
    }

    fn transpile_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [Stmt],
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for stmt in statements {
            docs.push(self.transpile_statement(arena, stmt));
        }
        arena.intersperse(docs, arena.hardline())
    }

    /// The lets as `const` declarations, then a `return` of the result.
    fn transpile_value_block<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        block: &'a ValueBlock,
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for let_ in &block.lets {
            docs.push(self.transpile_let_statement(arena, let_));
        }
        docs.push(
            arena
                .text("return ")
                .append(arena.text(name_ident(block.result)))
                .append(arena.text(";")),
        );
        arena.intersperse(docs, arena.hardline())
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record: Name,
        field: &'a FieldName,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text(name_ident(record)))
            .append(arena.text("."))
            .append(arena.text(field.as_str()))
    }

    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a> {
        arena.text(self.quote_string(value))
    }

    /// The fragment body gets its own `output` buffer, so it is built by an
    /// immediately invoked arrow function rather than inline.
    fn transpile_html<'a>(&mut self, arena: &'a Arena<'a>, body: &'a [Stmt]) -> Doc<'a> {
        self.needs_html = true;
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
                    .append(arena.text("return output as Html;"))
                    .append(arena.line())
                    .nest(4),
            )
            .append(arena.text("})()"))
    }

    /// The cast keeps the `const` at `boolean`. A union declared type is
    /// narrowed to the type of what is assigned, so without it a later
    /// comparison against the other literal would not typecheck.
    fn transpile_bool_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a> {
        match value {
            true => arena.text("true as boolean"),
            false => arena.text("false as boolean"),
        }
    }

    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a> {
        let text = if value.is_nan() {
            "globalThis.NaN".to_string()
        } else if value == f64::INFINITY {
            "globalThis.Infinity".to_string()
        } else if value == f64::NEG_INFINITY {
            "-globalThis.Infinity".to_string()
        } else {
            format!("{:?}", value)
        };
        arena.text(text)
    }

    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i32) -> Doc<'a> {
        arena.text(value.to_string())
    }

    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [Name],
        _elem_type: &'a Type,
    ) -> Doc<'a> {
        let elem_docs: Vec<_> = elements
            .iter()
            .map(|e| arena.text(name_ident(*e)))
            .collect();
        arena
            .nil()
            .append(arena.text("["))
            .append(arena.intersperse(elem_docs, arena.text(", ")))
            .append(arena.text("]"))
    }

    fn transpile_tuple_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [Name],
        _element_types: &'a [Type],
    ) -> Doc<'a> {
        let elem_docs: Vec<_> = elements
            .iter()
            .map(|e| arena.text(name_ident(*e)))
            .collect();
        arena
            .text("[")
            .append(arena.intersperse(elem_docs, arena.text(", ")))
            .append(arena.text("]"))
    }

    fn transpile_tuple_index<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        tuple: Name,
        index: usize,
    ) -> Doc<'a> {
        arena
            .text(name_ident(tuple))
            .append(arena.text("["))
            .append(arena.text(index.to_string()))
            .append(arena.text("]"))
    }

    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, Name)],
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
        fields: &'a [(FieldName, Name)],
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
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" < "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" < "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" <= "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" <= "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: Name) -> Doc<'a> {
        arena.text("!").append(arena.text(name_ident(operand)))
    }

    fn transpile_int_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: Name) -> Doc<'a> {
        arena
            .text("-")
            .append(arena.text(name_ident(operand)))
            .append(arena.text(" | 0"))
    }

    fn transpile_float_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: Name) -> Doc<'a> {
        arena.text("-").append(arena.text(name_ident(operand)))
    }

    fn transpile_string_concat<'a>(&mut self, arena: &'a Arena<'a>, parts: &'a [Name]) -> Doc<'a> {
        if parts.is_empty() {
            return arena.text("\"\"");
        }
        arena.intersperse(
            parts.iter().map(|part| arena.text(name_ident(*part))),
            arena.text(" + "),
        )
    }

    fn transpile_int_add<'a>(&mut self, arena: &'a Arena<'a>, left: Name, right: Name) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(arena.text(name_ident(left)))
            .append(arena.text(" + "))
            .append(arena.text(name_ident(right)))
            .append(arena.text(") | 0"))
    }

    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" + "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("("))
            .append(arena.text(name_ident(left)))
            .append(arena.text(" - "))
            .append(arena.text(name_ident(right)))
            .append(arena.text(") | 0"))
    }

    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" - "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("globalThis.Math.imul("))
            .append(arena.text(name_ident(left)))
            .append(arena.text(", "))
            .append(arena.text(name_ident(right)))
            .append(arena.text(")"))
    }

    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" * "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<Name>,
        inner_type: &'a Type,
    ) -> Doc<'a> {
        self.needs_option = true;
        match value {
            Some(inner) => arena
                .text("Option.some<")
                .append(self.transpile_type(arena, inner_type))
                .append(arena.text(">("))
                .append(arena.text(name_ident(inner)))
                .append(arena.text(")")),
            None => arena
                .text("Option.none<")
                .append(self.transpile_type(arena, inner_type))
                .append(arena.text(">()")),
        }
    }

    /// A match in value position. The arms compute their value by
    /// statements, so unless both arms of a bool match are bare results
    /// the match becomes an immediately invoked arrow function whose arms
    /// return.
    fn transpile_match_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<Name, ValueBlock>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                if true_body.lets.is_empty() && false_body.lets.is_empty() {
                    return arena
                        .text(name_ident(**subject))
                        .append(arena.text(" ? "))
                        .append(arena.text(name_ident(true_body.result)))
                        .append(arena.text(" : "))
                        .append(arena.text(name_ident(false_body.result)));
                }
                let body = arena
                    .text("if (")
                    .append(arena.text(name_ident(**subject)))
                    .append(arena.text(") {"))
                    .append(
                        arena
                            .hardline()
                            .append(self.transpile_value_block(arena, true_body))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("} else {"))
                    .append(
                        arena
                            .hardline()
                            .append(self.transpile_value_block(arena, false_body))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"));
                self.transpile_iife(arena, body)
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let body = self.transpile_option_cases(
                    arena,
                    **subject,
                    some_arm_binding.as_ref(),
                    some_arm_body.as_ref(),
                    none_arm_body.as_ref(),
                    |this, block| this.transpile_value_block(arena, block),
                    "",
                );
                self.transpile_iife(arena, body)
            }
            Match::Enum { subject, arms } => {
                let body = self.transpile_enum_cases(
                    arena,
                    **subject,
                    arms,
                    |this, block| this.transpile_value_block(arena, block),
                    "",
                );
                self.transpile_iife(arena, body)
            }
        }
    }

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: Name) -> Doc<'a> {
        arena.text(name_ident(array)).append(arena.text(".length"))
    }

    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: Name) -> Doc<'a> {
        arena
            .text(name_ident(array))
            .append(arena.text(".length === 0"))
    }

    fn transpile_string_is_empty<'a>(&mut self, arena: &'a Arena<'a>, string: Name) -> Doc<'a> {
        arena
            .text(name_ident(string))
            .append(arena.text(".length === 0"))
    }

    fn transpile_option_is_some<'a>(&mut self, arena: &'a Arena<'a>, option: Name) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".tag === \"Some\""))
    }

    fn transpile_option_is_none<'a>(&mut self, arena: &'a Arena<'a>, option: Name) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".tag === \"None\""))
    }

    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: Name) -> Doc<'a> {
        arena
            .text(name_ident(value))
            .append(arena.text(".toString()"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: Name) -> Doc<'a> {
        self.needs_float_to_int = true;
        arena
            .text("floatToInt(")
            .append(arena.text(name_ident(value)))
            .append(arena.text(")"))
    }

    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: Name) -> Doc<'a> {
        // In JavaScript, all numbers are floats, so no conversion needed
        arena.text(name_ident(value))
    }

    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("boolean")
    }

    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("string")
    }

    fn transpile_html_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        self.needs_html = true;
        arena.text("Html")
    }

    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("number")
    }

    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("number")
    }

    fn transpile_array_type<'a>(&mut self, arena: &'a Arena<'a>, element_type: &Type) -> Doc<'a> {
        self.transpile_type(arena, element_type)
            .append(arena.text("[]"))
    }

    fn transpile_tuple_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_types: &[Type],
    ) -> Doc<'a> {
        arena
            .text("[")
            .append(
                arena.intersperse(
                    element_types
                        .iter()
                        .map(|element| self.transpile_type(arena, element))
                        .collect::<Vec<_>>(),
                    arena.text(", "),
                ),
            )
            .append(arena.text("]"))
    }

    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &Type) -> Doc<'a> {
        self.needs_option = true;
        arena
            .text("Option.Option<")
            .append(self.transpile_type(arena, inner_type))
            .append(arena.text(">"))
    }

    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        arena.text(name.to_string())
    }

    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        arena
            .text(name.to_string())
            .append(arena.text("."))
            .append(arena.text(name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::flat_to_writer::flat_to_writer;
    use crate::ir::pure_module_builder::{PureModuleBodiesBuilder, PureModuleBuilder};
    use crate::ir::pure_to_flat::pure_to_flat;
    use expect_test::{Expect, expect};

    fn check<'a>(builder: impl Into<PureModuleBodiesBuilder<'a>>, expected: Expect) {
        let (module, registry) = builder.into().build_with_registry();
        let module = flat_to_writer(pure_to_flat(module), None);
        let before = module.to_string();
        let after = TsTranspiler::new().transpile_module(&module, &registry);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_page() {
        check(
            PureModuleBuilder::new().page_no_params("HelloWorld", |t| {
                t.concat(vec![
                    t.element("h1", vec![], vec![t.text("Hello, World!")]),
                    t.text("\n"),
                ])
            }),
            expect![[r#"
                -- before --
                page HelloWorld() {
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
    fn page_with_empty_tuple() {
        check(
            PureModuleBuilder::new()
                .record("Holder", [("nothing", "()")])
                .page("Test", [("unit", "()")], |t| {
                    let held = t.record("Holder", vec![("nothing", t.tuple(vec![]))]);
                    t.escape(t.int_to_string(t.array_length(t.array_typed(
                        t.resolve_type("()"),
                        vec![t.var("unit"), t.field_access(held, "nothing")],
                    ))))
                }),
            expect![[r#"
                -- before --
                page Test(unit@b0: ()) {
                  let v2: () = ()
                  let v3: Holder = {nothing: v2}
                  let v4: () = v3.nothing
                  let v5: Array[()] = [b0, v4]
                  let v6: Int = v5.len()
                  let v7: String = v6.to_string()
                  write_string(v7)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export class Holder {
                    public readonly nothing: [];

                    constructor(init: {nothing: []}) {
                        this.nothing = init.nothing;
                    }
                }

                export function Test({unit: b_0}: {unit: []}): string {
                    let output: string = "";
                    const v_2: [] = [];
                    const v_3: Holder = new Holder({nothing: v_2});
                    const v_4: [] = v_3.nothing;
                    const v_5: [][] = [b_0, v_4];
                    const v_6: number = v_5.length;
                    const v_7: string = v_6.toString();
                    output += escapeHtml(v_7);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn page_with_tuple_parameter() {
        check(
            PureModuleBuilder::new().page("Row", [("cell", "(Int, String)")], |t| {
                t.concat(vec![
                    t.escape(t.int_to_string(t.tuple_index(t.var("cell"), 0))),
                    t.text(": "),
                    t.escape(t.tuple_index(t.var("cell"), 1)),
                ])
            }),
            expect![[r#"
                -- before --
                page Row(cell@b0: (Int, String)) {
                  let v2: Int = b0.0
                  let v3: String = v2.to_string()
                  let v7: String = b0.1
                  write_string(v3)
                  write(": ")
                  write_string(v7)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function Row({cell: b_0}: {cell: [number, string]}): string {
                    let output: string = "";
                    const v_2: number = b_0[0];
                    const v_3: string = v_2.toString();
                    const v_7: string = b_0[1];
                    output += escapeHtml(v_3);
                    output += ": ";
                    output += escapeHtml(v_7);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn page_with_params_and_escaping() {
        check(
            PureModuleBuilder::new().page(
                "UserInfo",
                [("name", "String"), ("age", "String")],
                |t| {
                    t.concat(vec![
                        t.element(
                            "div",
                            vec![],
                            vec![
                                t.text("\n"),
                                t.element(
                                    "h2",
                                    vec![],
                                    vec![t.text("Name: "), t.escape(t.var("name"))],
                                ),
                                t.text("\n"),
                                t.element(
                                    "p",
                                    vec![],
                                    vec![t.text("Age: "), t.escape(t.var("age"))],
                                ),
                                t.text("\n"),
                            ],
                        ),
                        t.text("\n"),
                    ])
                },
            ),
            expect![[r#"
                -- before --
                page UserInfo(name@b0: String, age@b1: String) {
                  write("<div>\n<h2>Name: ")
                  write_string(b0)
                  write("</h2>\n<p>Age: ")
                  write_string(b1)
                  write("</p>\n</div>\n")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function UserInfo({
                    name: b_0,
                    age: b_1
                }: {
                    name: string,
                    age: string
                }): string {
                    let output: string = "";
                    output += "<div>\n<h2>Name: ";
                    output += escapeHtml(b_0);
                    output += "</h2>\n<p>Age: ";
                    output += escapeHtml(b_1);
                    output += "</p>\n</div>\n";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            PureModuleBuilder::new().page(
                "ConditionalDisplay",
                [("title", "String"), ("show", "Bool")],
                |t| {
                    t.bool_match_expr(
                        t.var("show"),
                        t.concat(vec![
                            t.element("h1", vec![], vec![t.escape(t.var("title"))]),
                            t.text("\n"),
                        ]),
                        t.concat(vec![]),
                    )
                },
            ),
            expect![[r#"
                -- before --
                page ConditionalDisplay(title@b0: String, show@b1: Bool) {
                  match b1 {
                    true => {
                      write("<h1>")
                      write_string(b0)
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
                        .replace(/"/g, '&quot;');
                }

                export function ConditionalDisplay({
                    title: b_0,
                    show: b_1
                }: {
                    title: string,
                    show: boolean
                }): string {
                    let output: string = "";
                    if (b_1) {
                        output += "<h1>";
                        output += escapeHtml(b_0);
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
            PureModuleBuilder::new().page("ListItems", [("items", "Array[String]")], |t| {
                t.concat(vec![
                    t.element(
                        "ul",
                        vec![],
                        vec![
                            t.text("\n"),
                            t.html_for(Some("item"), t.var("items"), |t| {
                                t.concat(vec![
                                    t.element("li", vec![], vec![t.escape(t.var("item"))]),
                                    t.text("\n"),
                                ])
                            }),
                        ],
                    ),
                    t.text("\n"),
                ])
            }),
            expect![[r#"
                -- before --
                page ListItems(items@b0: Array[String]) {
                  write("<ul>\n")
                  for b1: String in b0 {
                    write("<li>")
                    write_string(b1)
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
                        .replace(/"/g, '&quot;');
                }

                export function ListItems({items: b_0}: {items: string[]}): string {
                    let output: string = "";
                    output += "<ul>\n";
                    for (const b_1 of b_0) {
                        output += "<li>";
                        output += escapeHtml(b_1);
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
            PureModuleBuilder::new().page_no_params("Counter", |t| {
                t.html_for_range(Some("i"), t.int(1), t.int(3), |t| {
                    t.concat(vec![t.escape(t.int_to_string(t.var("i"))), t.text(" ")])
                })
            }),
            expect![[r#"
                -- before --
                page Counter() {
                  let v1: Int = 1
                  let v2: Int = 3
                  for b0: Int in v1..=v2 {
                    let v4: String = b0.to_string()
                    write_string(v4)
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
                        .replace(/"/g, '&quot;');
                }

                export function Counter(): string {
                    let output: string = "";
                    const v_1: number = 1;
                    const v_2: number = 3;
                    for (let b_0 = v_1; b_0 <= v_2; b_0++) {
                        const v_4: string = b_0.toString();
                        output += escapeHtml(v_4);
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
            PureModuleBuilder::new().page_no_params("GreetingCard", |t| {
                t.let_expr("greeting", t.str("Hello from hop!"), |t| {
                    t.concat(vec![
                        t.element(
                            "div",
                            vec![t.attr("class", t.str("card"))],
                            vec![
                                t.text("\n"),
                                t.element("p", vec![], vec![t.escape(t.var("greeting"))]),
                                t.text("\n"),
                            ],
                        ),
                        t.text("\n"),
                    ])
                })
            }),
            expect![[r#"
                -- before --
                page GreetingCard() {
                  write("<div class=\"card\">\n<p>Hello from hop!</p>\n</div>\n")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function GreetingCard(): string {
                    let output: string = "";
                    output += "<div class=\"card\">\n<p>Hello from hop!</p>\n</div>\n";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn nested_functions_with_let_bindings() {
        check(
            PureModuleBuilder::new().page_no_params("TestMainComp", |t| {
                t.element(
                    "div",
                    vec![t.attr("data-hop-id", t.str("test/card-comp"))],
                    vec![t.let_expr("title", t.str("Hello World"), |t| {
                        t.element("h2", vec![], vec![t.escape(t.var("title"))])
                    })],
                )
            }),
            expect![[r#"
                -- before --
                page TestMainComp() {
                  write("<div data-hop-id=\"test/card-comp\"><h2>Hello World</h2>")
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export function TestMainComp(): string {
                    let output: string = "";
                    output += "<div data-hop-id=\"test/card-comp\"><h2>Hello World</h2>";
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn fragment_type() {
        check(
            PureModuleBuilder::new().page("RenderHtml", [("user_input", "String")], |t| {
                t.let_expr(
                    "safe_content",
                    t.element("b", vec![], vec![t.text("hi")]),
                    |t| {
                        t.concat(vec![
                            t.element("div", vec![], vec![t.var("safe_content")]),
                            t.element("div", vec![], vec![t.escape(t.var("user_input"))]),
                        ])
                    },
                )
            }),
            expect![[r#"
                -- before --
                page RenderHtml(user_input@b0: String) {
                  write("<div><b>hi</b></div><div>")
                  write_string(b0)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function RenderHtml({user_input: b_0}: {user_input: string}): string {
                    let output: string = "";
                    output += "<div><b>hi</b></div><div>";
                    output += escapeHtml(b_0);
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
                .page("UserProfile", [("user", "User")], |t| {
                    t.element(
                        "div",
                        vec![],
                        vec![t.escape(t.field_access(t.var("user"), "name"))],
                    )
                }),
            expect![[r#"
                -- before --
                page UserProfile(user@b0: User) {
                  let v2: String = b0.name
                  write("<div>")
                  write_string(v2)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
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

                export function UserProfile({user: b_0}: {user: User}): string {
                    let output: string = "";
                    const v_2: string = b_0.name;
                    output += "<div>";
                    output += escapeHtml(v_2);
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
                .page_no_params("CreateUser", |t| {
                    let user = t.record("User", vec![("name", t.str("John")), ("age", t.int(30))]);
                    t.element("div", vec![], vec![t.escape(t.field_access(user, "name"))])
                }),
            expect![[r#"
                -- before --
                page CreateUser() {
                  let v1: String = "John"
                  let v2: Int = 30
                  let v3: User = {name: v1, age: v2}
                  let v4: String = v3.name
                  write("<div>")
                  write_string(v4)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
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
                    const v_1: string = "John";
                    const v_2: number = 30;
                    const v_3: User = new User({name: v_1, age: v_2});
                    const v_4: string = v_3.name;
                    output += "<div>";
                    output += escapeHtml(v_4);
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
                .page("Test", [("node", "Node")], |t| {
                    t.escape(t.int_to_string(t.field_access(t.var("node"), "value")))
                }),
            expect![[r#"
                -- before --
                page Test(node@b0: Node) {
                  let v2: Int = b0.value
                  let v3: String = v2.to_string()
                  write_string(v3)
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
                        .replace(/"/g, '&quot;');
                }

                export class Node {
                    public readonly value: number;
                    public readonly next: Option.Option<Node>;

                    constructor(init: {value: number, next: Option.Option<Node>}) {
                        this.value = init.value;
                        this.next = init.next;
                    }
                }

                export function Test({node: b_0}: {node: Node}): string {
                    let output: string = "";
                    const v_2: number = b_0.value;
                    const v_3: string = v_2.toString();
                    output += escapeHtml(v_3);
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
                .page_no_params("Test", |t| t.text("hello")),
            expect![[r#"
                -- before --
                page Test() {
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
                .page_no_params("Test", |t| {
                    let inner =
                        t.record("Node", vec![("value", t.int(1)), ("next", t.none("Node"))]);
                    let node = t.record("Node", vec![("value", t.int(2)), ("next", t.some(inner))]);
                    t.let_expr("node", node, |t| {
                        t.escape(t.int_to_string(t.field_access(t.var("node"), "value")))
                    })
                }),
            expect![[r#"
                -- before --
                page Test() {
                  let v1: Int = 2
                  let v2: Int = 1
                  let v3: Option[Node] = None
                  let v4: Node = {value: v2, next: v3}
                  let v5: Option[Node] = Some(v4)
                  let v6: Node = {value: v1, next: v5}
                  let v7: Int = v6.value
                  let v8: String = v7.to_string()
                  write_string(v8)
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
                        .replace(/"/g, '&quot;');
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
                    const v_1: number = 2;
                    const v_2: number = 1;
                    const v_3: Option.Option<Node> = Option.none<Node>();
                    const v_4: Node = new Node({value: v_2, next: v_3});
                    const v_5: Option.Option<Node> = Option.some<Node>(v_4);
                    const v_6: Node = new Node({value: v_1, next: v_5});
                    const v_7: number = v_6.value;
                    const v_8: string = v_7.toString();
                    output += escapeHtml(v_8);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_without_variants() {
        check(
            PureModuleBuilder::new()
                .enum_unit("Color", [])
                .page_no_params("Test", |t| t.text("hi")),
            expect![[r#"
                -- before --
                page Test() {
                  write("hi")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Color {
                    export type Color = never;

                }

                export function Test(): string {
                    let output: string = "";
                    output += "hi";
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
                .page("ColorName", [("color", "Color")], |t| {
                    let match_result = t.enum_match_expr(t.var("color"), |m| {
                        m.arm("Red", |t| t.str("red"));
                        m.arm("Green", |t| t.str("green"));
                        m.arm("Blue", |t| t.str("blue"));
                    });
                    t.escape(match_result)
                }),
            expect![[r#"
                -- before --
                page ColorName(color@b0: Color) {
                  let v5: String = match b0 {
                    Color::Red => {
                      let v2: String = "red"
                      v2
                    }
                    Color::Green => {
                      let v3: String = "green"
                      v3
                    }
                    Color::Blue => {
                      let v4: String = "blue"
                      v4
                    }
                  }
                  write_string(v5)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
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

                export function ColorName({color: b_0}: {color: Color.Color}): string {
                    let output: string = "";
                    const v_5: string = (() => {
                        const s_0: Color.Color = b_0;
                        switch (s_0._tag) {
                            case "Red": {
                                const v_2: string = "red";
                                return v_2;
                            }
                            case "Green": {
                                const v_3: string = "green";
                                return v_3;
                            }
                            case "Blue": {
                                const v_4: string = "blue";
                                return v_4;
                            }
                        }
                    })();
                    output += escapeHtml(v_5);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn bool_match_expression() {
        check(
            PureModuleBuilder::new().page("IsActive", [("active", "Bool")], |t| {
                let match_result = t.bool_match_expr(t.var("active"), t.str("yes"), t.str("no"));
                t.escape(match_result)
            }),
            expect![[r#"
                -- before --
                page IsActive(active@b0: Bool) {
                  let v4: String = match b0 {
                    true => {
                      let v2: String = "yes"
                      v2
                    }
                    false => {
                      let v3: String = "no"
                      v3
                    }
                  }
                  write_string(v4)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function IsActive({active: b_0}: {active: boolean}): string {
                    let output: string = "";
                    const v_4: string = (() => {
                        if (b_0) {
                            const v_2: string = "yes";
                            return v_2;
                        } else {
                            const v_3: string = "no";
                            return v_3;
                        }
                    })();
                    output += escapeHtml(v_4);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_expression() {
        check(
            PureModuleBuilder::new().page("CheckOption", [("opt", "Option[Int]")], |t| {
                let match_result =
                    t.option_match_expr(t.var("opt"), t.str("has value"), t.str("empty"));
                t.escape(match_result)
            }),
            expect![[r#"
                -- before --
                page CheckOption(opt@b0: Option[Int]) {
                  let v4: String = match b0 {
                    Some(_) => {
                      let v2: String = "has value"
                      v2
                    }
                    None => {
                      let v3: String = "empty"
                      v3
                    }
                  }
                  write_string(v4)
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
                        .replace(/"/g, '&quot;');
                }

                export function CheckOption({opt: b_0}: {opt: Option.Option<number>}): string {
                    let output: string = "";
                    const v_4: string = (() => {
                        const s_0: Option.Option<number> = b_0;
                        switch (s_0.tag) {
                            case "Some": {
                                const v_2: string = "has value";
                                return v_2;
                            }
                            case "None": {
                                const v_3: string = "empty";
                                return v_3;
                            }
                        }
                    })();
                    output += escapeHtml(v_4);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn nested_option_match_expression() {
        check(
            PureModuleBuilder::new().page(
                "CheckNestedOption",
                [("opt", "Option[Option[Bool]]")],
                |t| {
                    let outer_match = t.option_match_expr_with_binding(
                        t.var("opt"),
                        "v0",
                        |t| {
                            t.option_match_expr_with_binding(
                                t.var("v0"),
                                "v1",
                                |t| {
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
                page CheckNestedOption(opt@b0: Option[Option[Bool]]) {
                  let v10: String = match b0 {
                    Some(b1: Option[Bool]) => {
                      let v8: String = match b1 {
                        Some(b2: Bool) => {
                          let v6: String = match b2 {
                            true => {
                              let v4: String = "some-some-true"
                              v4
                            }
                            false => {
                              let v5: String = "some-some-false"
                              v5
                            }
                          }
                          v6
                        }
                        None => {
                          let v7: String = "some-none"
                          v7
                        }
                      }
                      v8
                    }
                    None => {
                      let v9: String = "none"
                      v9
                    }
                  }
                  write_string(v10)
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
                        .replace(/"/g, '&quot;');
                }

                export function CheckNestedOption({
                    opt: b_0
                }: {
                    opt: Option.Option<Option.Option<boolean>>
                }): string {
                    let output: string = "";
                    const v_10: string = (() => {
                        const s_0: Option.Option<Option.Option<boolean>> = b_0;
                        switch (s_0.tag) {
                            case "Some": {
                                const b_1 = s_0.value;
                                const v_8: string = (() => {
                                    const s_1: Option.Option<boolean> = b_1;
                                    switch (s_1.tag) {
                                        case "Some": {
                                            const b_2 = s_1.value;
                                            const v_6: string = (() => {
                                                if (b_2) {
                                                    const v_4: string = "some-some-true";
                                                    return v_4;
                                                } else {
                                                    const v_5: string = "some-some-false";
                                                    return v_5;
                                                }
                                            })();
                                            return v_6;
                                        }
                                        case "None": {
                                            const v_7: string = "some-none";
                                            return v_7;
                                        }
                                    }
                                })();
                                return v_8;
                            }
                            case "None": {
                                const v_9: string = "none";
                                return v_9;
                            }
                        }
                    })();
                    output += escapeHtml(v_10);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn let_expression() {
        check(
            PureModuleBuilder::new().page("LetExpr", [("name", "String")], |t| {
                let result = t.let_expr("x", t.var("name"), |t| t.var("x"));
                t.escape(result)
            }),
            expect![[r#"
                -- before --
                page LetExpr(name@b0: String) {
                  write_string(b0)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function LetExpr({name: b_0}: {name: string}): string {
                    let output: string = "";
                    output += escapeHtml(b_0);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            PureModuleBuilder::new().page("DisplayOption", [("opt", "Option[String]")], |t| {
                t.option_match_expr_with_binding(
                    t.var("opt"),
                    "value",
                    |t| {
                        t.element(
                            "span",
                            vec![],
                            vec![t.text("Found: "), t.escape(t.var("value"))],
                        )
                    },
                    t.element("span", vec![], vec![t.text("Nothing")]),
                )
            }),
            expect![[r#"
                -- before --
                page DisplayOption(opt@b0: Option[String]) {
                  match b0 {
                    Some(b1: String) => {
                      write("<span>Found: ")
                      write_string(b1)
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
                        .replace(/"/g, '&quot;');
                }

                export function DisplayOption({
                    opt: b_0
                }: {
                    opt: Option.Option<string>
                }): string {
                    let output: string = "";
                    const s_0: Option.Option<string> = b_0;
                    switch (s_0.tag) {
                        case "Some": {
                            const b_1 = s_0.value;
                            output += "<span>Found: ";
                            output += escapeHtml(b_1);
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
            PureModuleBuilder::new().page(
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
                page TestOptionLiteral(opt1@b0: Option[String], opt2@b1: Option[String]) {
                  let v4: String = match b0 {
                    Some(_) => {
                      let v2: String = "has value"
                      v2
                    }
                    None => {
                      let v3: String = "empty"
                      v3
                    }
                  }
                  let v9: String = match b1 {
                    Some(_) => {
                      let v7: String = "HAS"
                      v7
                    }
                    None => {
                      let v8: String = "EMPTY"
                      v8
                    }
                  }
                  write_string(v4)
                  write_string(v9)
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
                        .replace(/"/g, '&quot;');
                }

                export function TestOptionLiteral({
                    opt1: b_0,
                    opt2: b_1
                }: {
                    opt1: Option.Option<string>,
                    opt2: Option.Option<string>
                }): string {
                    let output: string = "";
                    const v_4: string = (() => {
                        const s_0: Option.Option<string> = b_0;
                        switch (s_0.tag) {
                            case "Some": {
                                const v_2: string = "has value";
                                return v_2;
                            }
                            case "None": {
                                const v_3: string = "empty";
                                return v_3;
                            }
                        }
                    })();
                    const v_9: string = (() => {
                        const s_1: Option.Option<string> = b_1;
                        switch (s_1.tag) {
                            case "Some": {
                                const v_7: string = "HAS";
                                return v_7;
                            }
                            case "None": {
                                const v_8: string = "EMPTY";
                                return v_8;
                            }
                        }
                    })();
                    output += escapeHtml(v_4);
                    output += escapeHtml(v_9);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_literal_inline_match_stmt() {
        check(
            PureModuleBuilder::new().page_no_params("TestInlineMatch", |t| {
                t.let_expr("opt", t.some(t.str("world")), |t| {
                    t.option_match_expr_with_binding(
                        t.var("opt"),
                        "val",
                        |t| t.concat(vec![t.text("Got:"), t.escape(t.var("val"))]),
                        t.concat(vec![t.text("Empty")]),
                    )
                })
            }),
            expect![[r#"
                -- before --
                page TestInlineMatch() {
                  let v1: String = "world"
                  let v2: Option[String] = Some(v1)
                  match v2 {
                    Some(b1: String) => {
                      write("Got:")
                      write_string(b1)
                    }
                    None => {
                      write("Empty")
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
                        .replace(/"/g, '&quot;');
                }

                export function TestInlineMatch(): string {
                    let output: string = "";
                    const v_1: string = "world";
                    const v_2: Option.Option<string> = Option.some<string>(v_1);
                    const s_0: Option.Option<string> = v_2;
                    switch (s_0.tag) {
                        case "Some": {
                            const b_1 = s_0.value;
                            output += "Got:";
                            output += escapeHtml(b_1);
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
            PureModuleBuilder::new().page_no_params("Test", |t| {
                t.option_match_expr_with_binding(
                    t.some(t.str("x")),
                    "value",
                    |t| {
                        t.option_match_expr_with_binding(
                            t.some(t.var("value")),
                            "inner",
                            |t| t.escape(t.var("inner")),
                            t.concat(vec![t.text("none2")]),
                        )
                    },
                    t.concat(vec![t.text("none1")]),
                )
            }),
            expect![[r#"
                -- before --
                page Test() {
                  let v1: String = "x"
                  let v2: Option[String] = Some(v1)
                  match v2 {
                    Some(b0: String) => {
                      let v4: Option[String] = Some(b0)
                      match v4 {
                        Some(b1: String) => {
                          write_string(b1)
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
                        .replace(/"/g, '&quot;');
                }

                export function Test(): string {
                    let output: string = "";
                    const v_1: string = "x";
                    const v_2: Option.Option<string> = Option.some<string>(v_1);
                    const s_0: Option.Option<string> = v_2;
                    switch (s_0.tag) {
                        case "Some": {
                            const b_0 = s_0.value;
                            const v_4: Option.Option<string> = Option.some<string>(b_0);
                            const s_1: Option.Option<string> = v_4;
                            switch (s_1.tag) {
                                case "Some": {
                                    const b_1 = s_1.value;
                                    output += escapeHtml(b_1);
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
            PureModuleBuilder::new().page("IsActive", [("active", "Bool")], |t| {
                let match_result =
                    t.bool_match_expr(t.not(t.var("active")), t.str("yes"), t.str("no"));
                t.escape(match_result)
            }),
            expect![[r#"
                -- before --
                page IsActive(active@b0: Bool) {
                  let v2: Bool = !b0
                  let v5: String = match v2 {
                    true => {
                      let v3: String = "yes"
                      v3
                    }
                    false => {
                      let v4: String = "no"
                      v4
                    }
                  }
                  write_string(v5)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function IsActive({active: b_0}: {active: boolean}): string {
                    let output: string = "";
                    const v_2: boolean = !b_0;
                    const v_5: string = (() => {
                        if (v_2) {
                            const v_3: string = "yes";
                            return v_3;
                        } else {
                            const v_4: string = "no";
                            return v_4;
                        }
                    })();
                    output += escapeHtml(v_5);
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
                .page("ShowOutcome", [("r", "Outcome")], |t| {
                    let ok = t.enum_variant_with_fields(
                        "Outcome",
                        "Success",
                        vec![("value", t.int(42))],
                    );
                    t.element(
                        "div",
                        vec![],
                        vec![t.let_expr("ok", ok, |t| t.escape(t.str("Created Ok!")))],
                    )
                }),
            expect![[r#"
                -- before --
                page ShowOutcome(r@b0: Outcome) {
                  let v1: Int = 42
                  let v2: Outcome = Success {value: v1}
                  write("<div>Created Ok!</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                export namespace Outcome {
                    export type Outcome = { readonly _tag: "Success", readonly value: number } | { readonly _tag: "Failure", readonly message: string };

                    export function Success(init: {value: number}): Outcome {
                        return { _tag: "Success", value: init.value };
                    }
                    export function Failure(init: {message: string}): Outcome {
                        return { _tag: "Failure", message: init.message };
                    }
                }

                export function ShowOutcome({r: b_0}: {r: Outcome.Outcome}): string {
                    let output: string = "";
                    const v_1: number = 42;
                    const v_2: Outcome.Outcome = Outcome.Success({value: v_1});
                    output += "<div>Created Ok!</div>";
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
                .page("ShowOutcome", [("r", "Outcome")], |t| {
                    t.enum_match_expr(t.var("r"), |m| {
                        m.arm_bound("Success", [("value", "v")], |t| {
                            t.concat(vec![t.text("Value: "), t.escape(t.var("v"))])
                        });
                        m.arm_bound("Failure", [("message", "m")], |t| {
                            t.concat(vec![t.text("Error: "), t.escape(t.var("m"))])
                        });
                    })
                }),
            expect![[r#"
                -- before --
                page ShowOutcome(r@b0: Outcome) {
                  match b0 {
                    Outcome::Success {value@b1: String} => {
                      write("Value: ")
                      write_string(b1)
                    }
                    Outcome::Failure {message@b2: String} => {
                      write("Error: ")
                      write_string(b2)
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
                        .replace(/"/g, '&quot;');
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

                export function ShowOutcome({r: b_0}: {r: Outcome.Outcome}): string {
                    let output: string = "";
                    const s_0: Outcome.Outcome = b_0;
                    switch (s_0._tag) {
                        case "Success": {
                            const { value: b_1 } = s_0;
                            output += "Value: ";
                            output += escapeHtml(b_1);
                            break;
                        }
                        case "Failure": {
                            const { message: b_2 } = s_0;
                            output += "Error: ";
                            output += escapeHtml(b_2);
                            break;
                        }
                    }
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn transpiles_a_fragment_read_twice_as_a_nested_buffer() {
        check(
            PureModuleBuilder::new().page_no_params("Test", |t| {
                t.let_expr(
                    "v_0",
                    t.concat(vec![t.element("b", vec![], vec![t.text("hi")])]),
                    |t| t.concat(vec![t.var("v_0"), t.var("v_0")]),
                )
            }),
            expect![[r#"
                -- before --
                page Test() {
                  let v4: Html = html {
                    write("<b>hi</b>")
                  }
                  write_html(v4)
                  write_html(v4)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                type Html = string & { readonly __brand: unique symbol };

                export function Test(): string {
                    let output: string = "";
                    const v_4: Html = (() => {
                        let output: string = "";
                        output += "<b>hi</b>";
                        return output as Html;
                    })();
                    output += v_4;
                    output += v_4;
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn fragment_returning_function_called_in_value_position() {
        check(
            PureModuleBuilder::new()
                .function("Frag", [], "Html", |t| {
                    t.element("b", vec![], vec![t.text("hi")])
                })
                .page_no_params("Test", |t| {
                    t.let_expr("x", t.call("Frag", vec![]), |t| {
                        t.concat(vec![t.var("x"), t.var("x")])
                    })
                }),
            expect![[r#"
                -- before --
                fn Frag@f0() -> Html {
                  write("<b>hi</b>")
                }
                page Test() {
                  let v1: Html = html {
                    write_function Frag@f0()
                  }
                  write_html(v1)
                  write_html(v1)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                type Html = string & { readonly __brand: unique symbol };

                function renderFrag_0(): string {
                    let output: string = "";
                    output += "<b>hi</b>";
                    return output;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_1: Html = (() => {
                        let output: string = "";
                        output += renderFrag_0();
                        return output as Html;
                    })();
                    output += v_1;
                    output += v_1;
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
                .page_no_params("Test", |t| {
                    t.escape(t.int_to_string(t.call("format_price", vec![("price", t.int(5))])))
                }),
            expect![[r#"
                -- before --
                fn format_price@f0(price@b0: Int) -> Int {
                  b0
                }
                page Test() {
                  let v1: Int = 5
                  let v2: Int = call format_price@f0(v1)
                  let v3: String = v2.to_string()
                  write_string(v3)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                function renderFormatPrice_0(b_0: number): number {
                    return b_0;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_1: number = 5;
                    const v_2: number = renderFormatPrice_0(v_1);
                    const v_3: string = v_2.toString();
                    output += escapeHtml(v_3);
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
                .page_no_params("Test", |t| {
                    t.element(
                        "div",
                        vec![],
                        vec![
                            t.html_for_range(
                                Some("x"),
                                t.int(0),
                                t.call("foo", vec![("x", t.int(-7))]),
                                |t| {
                                    t.concat(vec![
                                        t.escape(t.int_to_string(t.var("x"))),
                                        t.text(","),
                                    ])
                                },
                            ),
                            t.escape(t.int_to_string(t.call("foo", vec![("x", t.int(10))]))),
                        ],
                    )
                }),
            expect![[r#"
                -- before --
                fn foo@f0(x@b0: Int) -> Int {
                  let v17: Int = 10
                  let v18: Int = b0 + v17
                  v18
                }
                page Test() {
                  let v1: Int = 0
                  let v2: Int = -7
                  let v3: Int = call foo@f0(v2)
                  let v10: Int = 10
                  let v11: Int = call foo@f0(v10)
                  let v12: String = v11.to_string()
                  write("<div>")
                  for b1: Int in v1..=v3 {
                    let v5: String = b1.to_string()
                    write_string(v5)
                    write(",")
                  }
                  write_string(v12)
                  write("</div>")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                function renderFoo_0(b_0: number): number {
                    const v_17: number = 10;
                    const v_18: number = (b_0 + v_17) | 0;
                    return v_18;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_1: number = 0;
                    const v_2: number = -7;
                    const v_3: number = renderFoo_0(v_2);
                    const v_10: number = 10;
                    const v_11: number = renderFoo_0(v_10);
                    const v_12: string = v_11.toString();
                    output += "<div>";
                    for (let b_1 = v_1; b_1 <= v_3; b_1++) {
                        const v_5: string = b_1.toString();
                        output += escapeHtml(v_5);
                        output += ",";
                    }
                    output += escapeHtml(v_12);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn passes_a_parameter_whose_name_is_not_an_identifier_by_position() {
        check(
            PureModuleBuilder::new()
                .function("Button", [("data-x", "String")], "Html", |t| {
                    t.element("button", vec![t.attr("data-x", t.var("data-x"))], vec![])
                })
                .page_no_params("Test", |t| t.call("Button", vec![("data-x", t.str("1"))])),
            expect![[r#"
                -- before --
                fn Button@f0(data-x@b0: String) -> Html {
                  write("<button data-x=\"")
                  write_string(b0)
                  write("\"></button>")
                }
                page Test() {
                  let v1: String = "1"
                  write_function Button@f0(v1)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                function renderButton_0(b_0: string): string {
                    let output: string = "";
                    output += "<button data-x=\"";
                    output += escapeHtml(b_0);
                    output += "\"></button>";
                    return output;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_1: string = "1";
                    output += renderButton_0(v_1);
                    return output;
                }
            "#]],
        );
    }
}
