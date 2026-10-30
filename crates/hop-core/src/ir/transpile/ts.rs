use std::collections::HashMap;

use pretty::{Arena, DocAllocator};

use super::Doc;
use super::transpiler::Transpiler;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::ir_switch::{EnumMatchArm, EnumPattern, IrSwitch};
use crate::ir::ir_type::{IrDataType, IrType};
use crate::ir::ir_type_declarations::{IrEnum, IrRecord};
use crate::ir::value_id::ValueId;
use crate::ir::writer_program::{
    WriterBinding, WriterBlock, WriterFunctionBody, WriterFunctionDeclaration,
    WriterPageDeclaration, WriterProgram, WriterStmt,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// Names every variable in the generated code after the name that binds it
/// in the IR rather than the source name.
///
/// Names are unique across the program, so no hop identifier can shadow
/// another, and no name can collide with a TypeScript reserved word or with
/// the `output` buffer.
fn name_ident(name: ValueId) -> String {
    format!("v_{}", name.index())
}

/// Destructuring entry for a parameter: `name: v_0`. The property name stays
/// the source name, since it is the caller-facing argument name.
fn transpile_param_binding<'a>(arena: &'a Arena<'a>, param: &'a IrParameter<IrType>) -> Doc<'a> {
    arena
        .text(param.name().as_str())
        .append(arena.text(": "))
        .append(arena.text(name_ident(param.binder.var)))
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
    /// The type of every name bound so far. A match binds its subject to
    /// a fresh constant of the subject's declared type, so a nested match
    /// on the same name is not narrowed by the arm it sits in.
    name_types: HashMap<ValueId, IrType>,
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
            name_types: HashMap::new(),
            subjects: 0,
        }
    }

    /// Bind the subject of a match to a fresh constant of its declared
    /// type, and return the constant's name with the binding statement.
    fn bind_subject<'a>(&mut self, arena: &'a Arena<'a>, subject: ValueId) -> (String, Doc<'a>) {
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

    fn quote_string(&mut self, s: &str) -> String {
        format!("\"{}\"", self.escape_string(s))
    }

    /// The destructuring parameter of a page: the binding pattern and the
    /// type literal that annotates it, as in `{a: v_0}: {a: string}`. The
    /// parameters' types are recorded.
    fn transpile_parameter_list<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parameters: &'a [IrParameter<IrType>],
    ) -> Doc<'a> {
        for param in parameters {
            self.name_types
                .insert(param.binder.var, param.binder.typ.clone());
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
                    .append(self.transpile_type(arena, &param.binder.typ))
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
        fields: &'a [(FieldName, ValueId)],
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
        args: &'a [ValueId],
    ) -> Doc<'a> {
        let arg_docs: Vec<_> = args
            .iter()
            .map(|arg| arena.text(name_ident(*arg)))
            .collect();
        base.append(arena.intersperse(arg_docs, arena.text(", ")))
            .append(arena.text(")"))
    }

    /// A statement block, indented, between the braces the caller writes.
    fn transpile_block<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [WriterStmt],
    ) -> Doc<'a> {
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
        subject: ValueId,
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
                        .insert(binder.var, IrType::Data(binder.typ.clone()));
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
                                .append(arena.text(name_ident(binder.var)))
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
        subject: ValueId,
        some_arm_binding: Option<&'a IrBinder<IrDataType>>,
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
                    .insert(binder.var, IrType::Data(binder.typ.clone()));
                arena
                    .text("const ")
                    .append(arena.text(name_ident(binder.var)))
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
    fn name_type(&self, name: ValueId) -> &IrType {
        self.name_types
            .get(&name)
            .unwrap_or_else(|| unreachable!("every name records its type, and {name} has none"))
    }

    fn transpile_program(&mut self, program: &WriterProgram) -> String {
        self.needs_option = false;
        self.needs_escape_html = false;
        self.needs_float_to_int = false;
        self.needs_html = false;
        self.name_types.clear();
        self.subjects = 0;

        let arena = &Arena::new();
        let pages = &program.pages;
        let mut result = arena.nil();

        for IrEnum {
            name: enum_name,
            variants,
        } in &program.types.enums
        {
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
                                            .append(self.transpile_data_type(arena, &field.typ))
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

            for variant in variants {
                result = result.append(arena.line());

                if variant.fields.is_empty() {
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
                    let param_with_type_docs: Vec<_> = variant
                        .fields
                        .iter()
                        .map(|field| {
                            arena
                                .text(field.name.as_str())
                                .append(arena.text(": "))
                                .append(self.transpile_data_type(arena, &field.typ))
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

        for IrRecord {
            name: record_name,
            fields,
        } in &program.types.records
        {
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
                            .append(self.transpile_data_type(arena, &field.typ))
                            .append(arena.text(";"))
                    })
                    .collect();
                let param_with_type_docs: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        arena
                            .text(field.name.as_str())
                            .append(arena.text(": "))
                            .append(self.transpile_data_type(arena, &field.typ))
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

        for function in &program.functions {
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

        if self.needs_escape_html {
            let escape_fn = arena.intersperse(
                [
                    arena.text("function escapeHtml(str: string): string {"),
                    arena.text("    return str"),
                    arena.text("        .replace(/&/g, '&amp;')"),
                    arena.text("        .replace(/</g, '&lt;')"),
                    arena.text("        .replace(/>/g, '&gt;')"),
                    arena.text("        .replace(/\"/g, '&quot;');"),
                    arena.text("}"),
                ],
                arena.hardline(),
            );
            result = escape_fn
                .append(arena.hardline())
                .append(arena.hardline())
                .append(result);
        }

        if self.needs_float_to_int {
            let float_to_int_fn = arena.intersperse(
                [
                    arena.text("function floatToInt(f: number): number {"),
                    arena.text("    if (globalThis.Number.isNaN(f)) return 0;"),
                    arena.text("    if (f >= 2147483647) return 2147483647;"),
                    arena.text("    if (f <= -2147483648) return -2147483648;"),
                    arena.text("    return globalThis.Math.trunc(f);"),
                    arena.text("}"),
                ],
                arena.hardline(),
            );
            result = float_to_int_fn
                .append(arena.hardline())
                .append(arena.hardline())
                .append(result);
        }

        if self.needs_option {
            let option_ns = arena.intersperse(
                [
                    arena.text("export namespace Option {"),
                    arena.text(
                        "    export type Option<T> = { readonly tag: \"None\" } | { readonly tag: \"Some\", value: T };",
                    ),
                    arena.text(""),
                    arena.text("    export function some<T>(value: T): Option<T> {"),
                    arena.text("        return { tag: \"Some\", value };"),
                    arena.text("    }"),
                    arena.text("    export function none<T = never>(): Option<T> {"),
                    arena.text("        return { tag: \"None\" };"),
                    arena.text("    }"),
                    arena.text("}"),
                ],
                arena.hardline(),
            );
            result = option_ns
                .append(arena.hardline())
                .append(arena.hardline())
                .append(result);
        }

        if self.needs_html {
            let fragment = arena.text("type Html = string & { readonly __brand: unique symbol };");
            result = fragment
                .append(arena.hardline())
                .append(arena.hardline())
                .append(result);
        }

        let preamble = arena
            .text("// Code generated by the hop compiler. DO NOT EDIT.")
            .append(arena.line())
            .append(arena.line());
        result = preamble.append(result);

        let output = result.pretty(80).to_string();

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
        args: &'a [ValueId],
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
        let param_docs: Vec<_> = function
            .parameters
            .iter()
            .map(|param| {
                self.name_types
                    .insert(param.binder.var, param.binder.typ.clone());
                arena
                    .text(name_ident(param.binder.var))
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, &param.binder.typ))
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
        args: &'a [ValueId],
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
        name: ValueId,
    ) -> Doc<'a> {
        self.needs_escape_html = true;
        arena
            .nil()
            .append(arena.text("output += escapeHtml("))
            .append(arena.text(name_ident(name)))
            .append(arena.text(");"))
    }

    fn transpile_write_html_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: ValueId,
    ) -> Doc<'a> {
        arena
            .nil()
            .append(arena.text("output += "))
            .append(arena.text(name_ident(name)))
            .append(arena.text(";"))
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrBinder<IrDataType>>,
        array: ValueId,
        body: &'a [WriterStmt],
    ) -> Doc<'a> {
        let var_name = match var {
            Some(binder) => {
                self.name_types
                    .insert(binder.var, IrType::Data(binder.typ.clone()));
                name_ident(binder.var)
            }
            None => "_".to_string(),
        };
        arena
            .text("for (const ")
            .append(arena.text(var_name))
            .append(arena.text(" of "))
            .append(arena.text(name_ident(array)))
            .append(arena.text(") {"))
            .append(self.transpile_block(arena, body))
            .append(arena.text("}"))
    }

    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        let_: &'a WriterBinding,
    ) -> Doc<'a> {
        self.name_types.insert(let_.name, let_.typ.clone());
        let binding_type = self.transpile_type(arena, &let_.typ);
        let value = self.transpile_op(arena, &let_.op, &let_.typ);
        arena
            .text("const ")
            .append(arena.text(name_ident(let_.name)))
            .append(arena.text(": "))
            .append(binding_type)
            .append(arena.text(" = "))
            .append(value)
            .append(arena.text(";"))
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a IrSwitch<Vec<WriterStmt>>,
    ) -> Doc<'a> {
        match match_ {
            IrSwitch::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let if_doc = arena
                    .text("if (")
                    .append(arena.text(name_ident(*subject)))
                    .append(arena.text(") {"))
                    .append(self.transpile_block(arena, true_body))
                    .append(arena.text("}"));
                if false_body.is_empty() {
                    if_doc
                } else {
                    if_doc
                        .append(arena.text(" else {"))
                        .append(self.transpile_block(arena, false_body))
                        .append(arena.text("}"))
                }
            }
            IrSwitch::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => self.transpile_option_cases(
                arena,
                *subject,
                some_arm_binding.as_ref(),
                some_arm_body,
                none_arm_body,
                |this, body| this.transpile_statements(arena, body),
                "break;",
            ),
            IrSwitch::Enum { subject, arms } => self.transpile_enum_cases(
                arena,
                *subject,
                arms,
                |this, body| this.transpile_statements(arena, body),
                "break;",
            ),
        }
    }

    fn transpile_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [WriterStmt],
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
        block: &'a WriterBlock,
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for let_ in &block.bindings {
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
        record: ValueId,
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

    fn transpile_html<'a>(&mut self, arena: &'a Arena<'a>, body: &'a [WriterStmt]) -> Doc<'a> {
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
        elements: &'a [ValueId],
        _elem_type: &'a IrDataType,
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

    /// A length below zero is taken as zero, so a range whose start is
    /// greater than its end is empty.
    fn transpile_array_from_range<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        start: ValueId,
        end: ValueId,
    ) -> Doc<'a> {
        let start = name_ident(start);
        let end = name_ident(end);
        arena.text(format!(
            "Array.from({{ length: {end} - {start} + 1 }}, (_, index) => {start} + index)"
        ))
    }

    fn transpile_tuple_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [ValueId],
        _element_types: &'a [IrDataType],
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
        tuple: ValueId,
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
        fields: &'a [(FieldName, ValueId)],
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
        fields: &'a [(FieldName, ValueId)],
    ) -> Doc<'a> {
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
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" === "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" < "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" < "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" <= "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" <= "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: ValueId) -> Doc<'a> {
        arena.text("!").append(arena.text(name_ident(operand)))
    }

    fn transpile_int_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: ValueId) -> Doc<'a> {
        arena
            .text("-")
            .append(arena.text(name_ident(operand)))
            .append(arena.text(" | 0"))
    }

    fn transpile_float_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: ValueId) -> Doc<'a> {
        arena.text("-").append(arena.text(name_ident(operand)))
    }

    fn transpile_string_concat<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parts: &'a [ValueId],
    ) -> Doc<'a> {
        if parts.is_empty() {
            return arena.text("\"\"");
        }
        arena.intersperse(
            parts.iter().map(|part| arena.text(name_ident(*part))),
            arena.text(" + "),
        )
    }

    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
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
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" + "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
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
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" - "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
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
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" * "))
            .append(arena.text(name_ident(right)))
    }

    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<ValueId>,
        inner_type: &'a IrDataType,
    ) -> Doc<'a> {
        self.needs_option = true;
        match value {
            Some(inner) => arena
                .text("Option.some<")
                .append(self.transpile_data_type(arena, inner_type))
                .append(arena.text(">("))
                .append(arena.text(name_ident(inner)))
                .append(arena.text(")")),
            None => arena
                .text("Option.none<")
                .append(self.transpile_data_type(arena, inner_type))
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
        match_: &'a IrSwitch<WriterBlock>,
    ) -> Doc<'a> {
        match match_ {
            IrSwitch::Bool {
                subject,
                true_body,
                false_body,
            } => {
                if true_body.bindings.is_empty() && false_body.bindings.is_empty() {
                    return arena
                        .text(name_ident(*subject))
                        .append(arena.text(" ? "))
                        .append(arena.text(name_ident(true_body.result)))
                        .append(arena.text(" : "))
                        .append(arena.text(name_ident(false_body.result)));
                }
                let body = arena
                    .text("if (")
                    .append(arena.text(name_ident(*subject)))
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
            IrSwitch::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let body = self.transpile_option_cases(
                    arena,
                    *subject,
                    some_arm_binding.as_ref(),
                    some_arm_body,
                    none_arm_body,
                    |this, block| this.transpile_value_block(arena, block),
                    "",
                );
                self.transpile_iife(arena, body)
            }
            IrSwitch::Enum { subject, arms } => {
                let body = self.transpile_enum_cases(
                    arena,
                    *subject,
                    arms,
                    |this, block| this.transpile_value_block(arena, block),
                    "",
                );
                self.transpile_iife(arena, body)
            }
        }
    }

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: ValueId) -> Doc<'a> {
        arena.text(name_ident(array)).append(arena.text(".length"))
    }

    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(array))
            .append(arena.text(".length === 0"))
    }

    fn transpile_string_is_empty<'a>(&mut self, arena: &'a Arena<'a>, string: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(string))
            .append(arena.text(".length === 0"))
    }

    fn transpile_option_is_some<'a>(&mut self, arena: &'a Arena<'a>, option: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".tag === \"Some\""))
    }

    fn transpile_option_is_none<'a>(&mut self, arena: &'a Arena<'a>, option: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".tag === \"None\""))
    }

    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(value))
            .append(arena.text(".toString()"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a> {
        self.needs_float_to_int = true;
        arena
            .text("floatToInt(")
            .append(arena.text(name_ident(value)))
            .append(arena.text(")"))
    }

    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a> {
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

    fn transpile_array_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_type: &IrDataType,
    ) -> Doc<'a> {
        self.transpile_data_type(arena, element_type)
            .append(arena.text("[]"))
    }

    fn transpile_tuple_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_types: &[IrDataType],
    ) -> Doc<'a> {
        arena
            .text("[")
            .append(
                arena.intersperse(
                    element_types
                        .iter()
                        .map(|element| self.transpile_data_type(arena, element))
                        .collect::<Vec<_>>(),
                    arena.text(", "),
                ),
            )
            .append(arena.text("]"))
    }

    fn transpile_option_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        inner_type: &IrDataType,
    ) -> Doc<'a> {
        self.needs_option = true;
        arena
            .text("Option.Option<")
            .append(self.transpile_data_type(arena, inner_type))
            .append(arena.text(">"))
    }

    fn transpile_record_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
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
    use crate::document::Document;
    use crate::ir::flat_to_writer;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_flat};
    use crate::root_contained_file_path::RootContainedFilePath;
    use crate::session::Session;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut session = Session::new();
        session.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = session.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (flat, pages) = orchestrate_flat(
            session.typed_modules(),
            session.type_registry(),
            OrchestrateOptions {
                ..Default::default()
            },
        );
        let writer = flat_to_writer(flat, &pages, None);
        let output = TsTranspiler::new().transpile_program(&writer);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_page() {
        check(
            indoc! {r#"
                page HelloWorld() {
                  fn body() -> Html {
                    <h1>Hello, World!</h1>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                export function HelloWorld(): string {
                    let output: string = "";
                    output += "<h1>Hello, World!</h1>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn page_with_empty_tuple() {
        check(
            indoc! {r#"
                record Holder {
                  nothing: (),
                }

                page Test(unit: ()) {
                  fn body() -> Html {
                    let held = Holder {nothing: ()};
                    <>{[unit, held.nothing].len().to_string()}</>
                  }
                }
            "#},
            expect![[r#"
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

                export function Test({unit: v_0}: {unit: []}): string {
                    let output: string = "";
                    const v_1: [] = [];
                    const v_2: Holder = new Holder({nothing: v_1});
                    const v_3: [] = v_2.nothing;
                    const v_4: [][] = [v_0, v_3];
                    const v_5: number = v_4.length;
                    const v_6: string = v_5.toString();
                    output += escapeHtml(v_6);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn page_with_params_and_escaping() {
        check(
            indoc! {r#"
                page UserInfo(name: String, age: String) {
                  fn body() -> Html {
                    <div>
                      <h2>Name: {name}</h2>
                      <p>Age: {age}</p>
                    </div>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function UserInfo({
                    name: v_0,
                    age: v_1
                }: {
                    name: string,
                    age: string
                }): string {
                    let output: string = "";
                    output += "<div><h2>Name: ";
                    output += escapeHtml(v_0);
                    output += "</h2><p>Age: ";
                    output += escapeHtml(v_1);
                    output += "</p></div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            indoc! {r#"
                page ConditionalDisplay(title: String, show: Bool) {
                  fn body() -> Html {
                    match show {
                      true => <h1>{title}</h1>,
                      false => <></>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function ConditionalDisplay({
                    title: v_0,
                    show: v_1
                }: {
                    title: string,
                    show: boolean
                }): string {
                    let output: string = "";
                    if (v_1) {
                        output += "<h1>";
                        output += escapeHtml(v_0);
                        output += "</h1>";
                    }
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_array() {
        check(
            indoc! {r#"
                page ListItems(items: Array[String]) {
                  fn body() -> Html {
                    <ul>
                      {for item in items {
                        <li>{item}</li>
                      }}
                    </ul>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function ListItems({items: v_0}: {items: string[]}): string {
                    let output: string = "";
                    output += "<ul>";
                    for (const v_1 of v_0) {
                        output += "<li>";
                        output += escapeHtml(v_1);
                        output += "</li>";
                    }
                    output += "</ul>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_range() {
        check(
            indoc! {r#"
                page Counter() {
                  fn body() -> Html {
                    for i in 1..=3 {
                      <>{i.to_string()},</>
                    }
                  }
                }
            "#},
            expect![[r#"
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
                    const v_0: number = 1;
                    const v_1: number = 3;
                    const v_2: number[] = Array.from({ length: v_1 - v_0 + 1 }, (_, index) => v_0 + index);
                    for (const v_3 of v_2) {
                        const v_4: string = v_3.toString();
                        output += escapeHtml(v_4);
                        output += ",";
                    }
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn record_declarations() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                  age: Int,
                  active: Bool,
                }

                record Address {
                  street: String,
                  city: String,
                }

                page UserProfile(user: User) {
                  fn body() -> Html {
                    <div>{user.name}</div>
                  }
                }
            "#},
            expect![[r#"
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

                export function UserProfile({user: v_0}: {user: User}): string {
                    let output: string = "";
                    const v_1: string = v_0.name;
                    output += "<div>";
                    output += escapeHtml(v_1);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn record_literal() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                  age: Int,
                }

                page CreateUser() {
                  fn body() -> Html {
                    let user = User {name: "John", age: 30};
                    <div>{user.name}</div>
                  }
                }
            "#},
            expect![[r#"
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
                    const v_0: string = "John";
                    const v_1: number = 30;
                    const v_2: User = new User({name: v_0, age: v_1});
                    const v_3: string = v_2.name;
                    output += "<div>";
                    output += escapeHtml(v_3);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_declaration() {
        check(
            indoc! {r#"
                record Node {
                  value: Int,
                  next: Option[Node],
                }

                page Test(node: Node) {
                  fn body() -> Html {
                    <>{node.value.to_string()}</>
                  }
                }
            "#},
            expect![[r#"
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

                export function Test({node: v_0}: {node: Node}): string {
                    let output: string = "";
                    const v_1: number = v_0.value;
                    const v_2: string = v_1.toString();
                    output += escapeHtml(v_2);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_declaration() {
        check(
            indoc! {r#"
                enum IntList {
                  Cons {
                    head: Int,
                    tail: IntList,
                  },
                  Nil,
                }

                page Test() {
                  fn body() -> Html {
                    <>hello</>
                  }
                }
            "#},
            expect![[r#"
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
            indoc! {r#"
                record Node {
                  value: Int,
                  next: Option[Node],
                }

                page Test() {
                  fn body() -> Html {
                    let node = Node {value: 2, next: Some(Node {value: 1, next: None})};
                    <>{node.value.to_string()}</>
                  }
                }
            "#},
            expect![[r#"
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
                    const v_0: number = 2;
                    const v_1: number = 1;
                    const v_2: Option.Option<Node> = Option.none<Node>();
                    const v_3: Node = new Node({value: v_1, next: v_2});
                    const v_4: Option.Option<Node> = Option.some<Node>(v_3);
                    const v_5: Node = new Node({value: v_0, next: v_4});
                    const v_6: number = v_5.value;
                    const v_7: string = v_6.toString();
                    output += escapeHtml(v_7);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_without_variants() {
        check(
            indoc! {r#"
                enum Color {}

                page Test() {
                  fn body() -> Html {
                    <>hi</>
                  }
                }
            "#},
            expect![[r#"
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
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                page ColorName(color: Color) {
                  fn body() -> Html {
                    <>{match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}</>
                  }
                }
            "#},
            expect![[r#"
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

                export function ColorName({color: v_0}: {color: Color.Color}): string {
                    let output: string = "";
                    const v_7: string = (() => {
                        const s_0: Color.Color = v_0;
                        switch (s_0._tag) {
                            case "Red": {
                                const v_1: string = "red";
                                return v_1;
                            }
                            case "Green": {
                                const v_2: string = "green";
                                return v_2;
                            }
                            case "Blue": {
                                const v_3: string = "blue";
                                return v_3;
                            }
                        }
                    })();
                    output += escapeHtml(v_7);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn bool_match_expression() {
        check(
            indoc! {r#"
                page IsActive(active: Bool) {
                  fn body() -> Html {
                    <>{match active {true => "yes", false => "no"}}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function IsActive({active: v_0}: {active: boolean}): string {
                    let output: string = "";
                    const v_6: string = (() => {
                        if (v_0) {
                            const v_1: string = "yes";
                            return v_1;
                        } else {
                            const v_2: string = "no";
                            return v_2;
                        }
                    })();
                    output += escapeHtml(v_6);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_expression() {
        check(
            indoc! {r#"
                page CheckOption(opt: Option[Int]) {
                  fn body() -> Html {
                    <>{match opt {Some(_) => "has value", None => "empty"}}</>
                  }
                }
            "#},
            expect![[r#"
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

                export function CheckOption({opt: v_0}: {opt: Option.Option<number>}): string {
                    let output: string = "";
                    const v_6: string = (() => {
                        const s_0: Option.Option<number> = v_0;
                        switch (s_0.tag) {
                            case "Some": {
                                const v_1: string = "has value";
                                return v_1;
                            }
                            case "None": {
                                const v_2: string = "empty";
                                return v_2;
                            }
                        }
                    })();
                    output += escapeHtml(v_6);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn nested_option_match_expression() {
        check(
            indoc! {r#"
                page CheckNestedOption(opt: Option[Option[Bool]]) {
                  fn body() -> Html {
                    <>{match opt {
                      Some(Some(true)) => "some-some-true",
                      Some(Some(false)) => "some-some-false",
                      Some(None) => "some-none",
                      None => "none",
                    }}</>
                  }
                }
            "#},
            expect![[r#"
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
                    opt: v_0
                }: {
                    opt: Option.Option<Option.Option<boolean>>
                }): string {
                    let output: string = "";
                    const v_12: string = (() => {
                        const s_0: Option.Option<Option.Option<boolean>> = v_0;
                        switch (s_0.tag) {
                            case "Some": {
                                const v_8 = s_0.value;
                                const v_11: string = (() => {
                                    const s_1: Option.Option<boolean> = v_8;
                                    switch (s_1.tag) {
                                        case "Some": {
                                            const v_9 = s_1.value;
                                            const v_10: string = (() => {
                                                if (v_9) {
                                                    const v_1: string = "some-some-true";
                                                    return v_1;
                                                } else {
                                                    const v_2: string = "some-some-false";
                                                    return v_2;
                                                }
                                            })();
                                            return v_10;
                                        }
                                        case "None": {
                                            const v_3: string = "some-none";
                                            return v_3;
                                        }
                                    }
                                })();
                                return v_11;
                            }
                            case "None": {
                                const v_4: string = "none";
                                return v_4;
                            }
                        }
                    })();
                    output += escapeHtml(v_12);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            indoc! {r#"
                page DisplayOption(opt: Option[String]) {
                  fn body() -> Html {
                    match opt {
                      Some(value) => <span>Found: {value}</span>,
                      None => <span>Nothing</span>,
                    }
                  }
                }
            "#},
            expect![[r#"
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
                    opt: v_0
                }: {
                    opt: Option.Option<string>
                }): string {
                    let output: string = "";
                    const s_0: Option.Option<string> = v_0;
                    switch (s_0.tag) {
                        case "Some": {
                            const v_10 = s_0.value;
                            output += "<span>Found: ";
                            output += escapeHtml(v_10);
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
            indoc! {r#"
                page TestOptionLiteral() {
                  fn body() -> Html {
                    let opt1: Option[String] = Some("hello");
                    let opt2: Option[String] = None;
                    <>
                      {match opt1 {Some(_) => "has value", None => "empty"}}
                      {match opt2 {Some(_) => "HAS", None => "EMPTY"}}
                    </>
                  }
                }
            "#},
            expect![[r#"
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

                export function TestOptionLiteral(): string {
                    let output: string = "";
                    const v_0: string = "hello";
                    const v_1: Option.Option<string> = Option.some<string>(v_0);
                    const v_2: Option.Option<string> = Option.none<string>();
                    const v_12: string = (() => {
                        const s_0: Option.Option<string> = v_1;
                        switch (s_0.tag) {
                            case "Some": {
                                const v_3: string = "has value";
                                return v_3;
                            }
                            case "None": {
                                const v_4: string = "empty";
                                return v_4;
                            }
                        }
                    })();
                    const v_13: string = (() => {
                        const s_1: Option.Option<string> = v_2;
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
                    output += escapeHtml(v_12);
                    output += escapeHtml(v_13);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn option_literal_inline_match_stmt() {
        check(
            indoc! {r#"
                page TestInlineMatch() {
                  fn body() -> Html {
                    let opt = Some("world");
                    match opt {
                      Some(v) => <>Got:{v}</>,
                      None => <>Empty</>,
                    }
                  }
                }
            "#},
            expect![[r#"
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
                    const v_0: string = "world";
                    const v_1: Option.Option<string> = Option.some<string>(v_0);
                    const s_0: Option.Option<string> = v_1;
                    switch (s_0.tag) {
                        case "Some": {
                            const v_9 = s_0.value;
                            output += "Got:";
                            output += escapeHtml(v_9);
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
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    match Some("x") {
                      Some(value) => {
                        match Some(value) {
                          Some(inner) => <>{inner}</>,
                          None => <>none2</>,
                        }
                      },
                      None => <>none1</>,
                    }
                  }
                }
            "#},
            expect![[r#"
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
                    const v_0: string = "x";
                    const v_1: Option.Option<string> = Option.some<string>(v_0);
                    const s_0: Option.Option<string> = v_1;
                    switch (s_0.tag) {
                        case "Some": {
                            const v_13 = s_0.value;
                            const v_3: Option.Option<string> = Option.some<string>(v_13);
                            const s_1: Option.Option<string> = v_3;
                            switch (s_1.tag) {
                                case "Some": {
                                    const v_14 = s_1.value;
                                    output += escapeHtml(v_14);
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
            indoc! {r#"
                page IsActive(active: Bool) {
                  fn body() -> Html {
                    <>{match !active {true => "yes", false => "no"}}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                export function IsActive({active: v_0}: {active: boolean}): string {
                    let output: string = "";
                    const v_1: boolean = !v_0;
                    const v_7: string = (() => {
                        if (v_1) {
                            const v_2: string = "yes";
                            return v_2;
                        } else {
                            const v_3: string = "no";
                            return v_3;
                        }
                    })();
                    output += escapeHtml(v_7);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_with_fields() {
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: Int,
                  },
                  Failure {
                    message: String,
                  },
                }

                page ShowOutcome() {
                  fn body() -> Html {
                    let ok = Outcome::Success {value: 42};
                    <div>
                      {match ok {
                        Outcome::Success {value: v} => v.to_string(),
                        Outcome::Failure {message: m} => m,
                      }}
                    </div>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
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

                export function ShowOutcome(): string {
                    let output: string = "";
                    const v_0: number = 42;
                    const v_1: Outcome.Outcome = Outcome.Success({value: v_0});
                    const v_11: string = (() => {
                        const s_0: Outcome.Outcome = v_1;
                        switch (s_0._tag) {
                            case "Success": {
                                const { value: v_9 } = s_0;
                                const v_3: string = v_9.toString();
                                return v_3;
                            }
                            case "Failure": {
                                const { message: v_10 } = s_0;
                                return v_10;
                            }
                        }
                    })();
                    output += "<div>";
                    output += escapeHtml(v_11);
                    output += "</div>";
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn enum_match_with_field_bindings() {
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page ShowOutcome(r: Outcome) {
                  fn body() -> Html {
                    match r {
                      Outcome::Success {value: v} => <>Value: {v}</>,
                      Outcome::Failure {message: m} => <>Error: {m}</>,
                    }
                  }
                }
            "#},
            expect![[r#"
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

                export function ShowOutcome({r: v_0}: {r: Outcome.Outcome}): string {
                    let output: string = "";
                    const s_0: Outcome.Outcome = v_0;
                    switch (s_0._tag) {
                        case "Success": {
                            const { value: v_10 } = s_0;
                            output += "Value: ";
                            output += escapeHtml(v_10);
                            break;
                        }
                        case "Failure": {
                            const { message: v_11 } = s_0;
                            output += "Error: ";
                            output += escapeHtml(v_11);
                            break;
                        }
                    }
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn fragment_returning_function_called_in_value_position() {
        check(
            indoc! {r#"
                fn Frag() -> Html {
                  <b>hi</b>
                }

                page Test() {
                  fn body() -> Html {
                    let x = Frag();
                    <>{x}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function renderFrag_0(): string {
                    let output: string = "";
                    output += "<b>hi</b>";
                    return output;
                }

                export function Test(): string {
                    let output: string = "";
                    output += renderFrag_0();
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn snake_case_function_name_mangling() {
        check(
            indoc! {r#"
                fn format_price(price: Int) -> Int {
                  price
                }

                page Test() {
                  fn body() -> Html {
                    <>{format_price(price: 5).to_string()}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                function renderFormatPrice_0(v_5: number): number {
                    return v_5;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_0: number = 5;
                    const v_1: number = renderFormatPrice_0(v_0);
                    const v_2: string = v_1.toString();
                    output += escapeHtml(v_2);
                    return output;
                }
            "#]],
        );
    }

    #[test]
    fn function_called_in_range_bound_and_interpolation() {
        check(
            indoc! {r#"
                fn foo(x: Int) -> Int {
                  x + 10
                }

                page Test() {
                  fn body() -> Html {
                    <div>
                      {for x in 0..=foo(x: -7) {
                        <>{x.to_string()},</>
                      }}
                      {foo(x: 10).to_string()}
                    </div>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                function renderFoo_0(v_16: number): number {
                    const v_17: number = 10;
                    const v_18: number = (v_16 + v_17) | 0;
                    return v_18;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_0: number = 0;
                    const v_1: number = -7;
                    const v_2: number = renderFoo_0(v_1);
                    const v_3: number[] = Array.from({ length: v_2 - v_0 + 1 }, (_, index) => v_0 + index);
                    const v_10: number = 10;
                    const v_11: number = renderFoo_0(v_10);
                    const v_12: string = v_11.toString();
                    output += "<div>";
                    for (const v_4 of v_3) {
                        const v_5: string = v_4.toString();
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
            indoc! {r#"
                fn Button(...rest) -> Html {
                  <button ...rest></button>
                }

                page Test() {
                  fn body() -> Html {
                    <Button data-x="1"/>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.

                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;');
                }

                function renderButton_0(v_2: string): string {
                    let output: string = "";
                    output += "<button data-x=\"";
                    output += escapeHtml(v_2);
                    output += "\"></button>";
                    return output;
                }

                export function Test(): string {
                    let output: string = "";
                    const v_0: string = "1";
                    output += renderButton_0(v_0);
                    return output;
                }
            "#]],
        );
    }
}
