use std::collections::{BTreeSet, HashMap, HashSet};

use pretty::{Arena, DocAllocator};

use super::Doc;
use super::transpiler::Transpiler;
use crate::dependency_graph::DependencyGraph;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::value_id::ValueId;
use crate::ir::writer_program::{
    WriterBlock, WriterFunctionBody, WriterFunctionDeclaration, WriterOp, WriterPageDeclaration,
    WriterProgram, WriterStmt,
};
use crate::ir::writer_switch::{WriterEnumMatchArm, WriterEnumPattern, WriterSwitch};
use crate::ir::writer_type::{
    WriterDataType, WriterEnum, WriterRecord, WriterResult, WriterSignature, WriterType,
    WriterTypeDeclarations, WriterVariant,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// Names every variable in the generated code after the name that binds it
/// in the IR rather than the source name.
///
/// Names are unique across the program, so no hop identifier can shadow
/// another, and no name can collide with a keyword, the `output` buffer or
/// the `arena`.
fn name_ident(name: ValueId) -> String {
    format!("v_{}", name.index())
}

fn function_ident(function: &IrFunction) -> String {
    format!(
        "render_{}_{}",
        function.name.to_snake_case(),
        function.id.index()
    )
}

pub struct RustTranspiler {
    /// Tracks whether escape_html function is used during transpilation
    needs_escape_html: bool,
    /// Field positions carrying `&'a` indirection.
    /// Both directions carry it for mutually recursive types.
    indirect_edges: HashSet<(TypeName, TypeName)>,
    /// The declared types that hold a reference, and so take `<'a>`.
    borrowing_types: HashSet<TypeName>,
    types: WriterTypeDeclarations,
    /// The type of every name. Every parameter, let and binder inserts its
    /// entry before anything reads it.
    names: HashMap<ValueId, WriterType>,
}

impl RustTranspiler {
    pub fn new() -> Self {
        Self {
            needs_escape_html: false,
            indirect_edges: HashSet::new(),
            borrowing_types: HashSet::new(),
            types: WriterTypeDeclarations::default(),
            names: HashMap::new(),
        }
    }

    /// Escape an identifier that Rust would otherwise read as a keyword.
    fn escape_ident(name: &str) -> String {
        match name {
            "crate" | "self" | "Self" | "super" => format!("{}_", name),
            "as" | "break" | "const" | "continue" | "else" | "enum" | "extern" | "false" | "fn"
            | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" | "mut"
            | "pub" | "ref" | "return" | "static" | "struct" | "trait" | "true" | "type"
            | "unsafe" | "use" | "where" | "while" | "async" | "await" | "dyn" | "gen"
            | "abstract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv"
            | "typeof" | "unsized" | "virtual" | "yield" | "try" => format!("r#{}", name),
            _ => name.to_string(),
        }
    }

    fn escape_string(&mut self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    /// Collect the named types that `t` stores inline.
    fn inline_refs(t: &WriterDataType, out: &mut BTreeSet<TypeName>) {
        match t {
            WriterDataType::Record(name) | WriterDataType::Enum(name) => {
                out.insert(name.clone());
            }
            WriterDataType::Option(inner) => Self::inline_refs(inner, out),
            WriterDataType::Tuple(elements) => {
                for element in elements {
                    Self::inline_refs(element, out);
                }
            }
            _ => {}
        }
    }

    /// The field positions that need indirection for every declared type to be
    /// finitely sized, as `(declaring type, referenced type)` pairs.
    fn compute_indirect_edges(types: &WriterTypeDeclarations) -> HashSet<(TypeName, TypeName)> {
        let mut graph = DependencyGraph::new();
        for WriterRecord { name, fields } in &types.records {
            let mut refs = BTreeSet::new();
            for field in fields {
                Self::inline_refs(&field.typ, &mut refs);
            }
            graph.set_dependencies(name.clone(), refs);
        }
        for WriterEnum { name, variants } in &types.enums {
            let mut refs = BTreeSet::new();
            for variant in variants {
                for field in &variant.fields {
                    Self::inline_refs(&field.typ, &mut refs);
                }
            }
            graph.set_dependencies(name.clone(), refs);
        }

        let mut edges = HashSet::new();
        for scc in graph.sorted_sccs() {
            for owner in &scc {
                for target in &scc {
                    if graph.depends_on(owner, target) {
                        edges.insert((owner.clone(), target.clone()));
                    }
                }
            }
        }
        edges
    }

    /// Whether a field of `owner` declaring type `t` carries indirection, which is
    /// the case when the field names inline a type `owner` has an indirect
    /// edge to. Mirrors `inline_refs`: descend `Option` and tuples, stop at
    /// `Array`.
    fn field_type_is_indirect(&self, t: &WriterDataType, owner: &str) -> bool {
        match t {
            WriterDataType::Record(name) | WriterDataType::Enum(name) => self
                .indirect_edges
                .iter()
                .any(|(o, target)| o.as_str() == owner && target == name),
            WriterDataType::Option(inner) => self.field_type_is_indirect(inner, owner),
            WriterDataType::Tuple(elements) => elements
                .iter()
                .any(|element| self.field_type_is_indirect(element, owner)),
            _ => false,
        }
    }

    fn compute_borrowing_types(&self) -> HashSet<TypeName> {
        let fields = |name: &TypeName| -> Vec<WriterDataType> {
            match self.types.record(name) {
                Some(record) => record.fields.iter().map(|f| f.typ.clone()).collect(),
                None => (self.types.enumeration(name).into_iter())
                    .flat_map(|e| e.variants.iter())
                    .flat_map(|v| v.fields.iter().map(|f| f.typ.clone()))
                    .collect(),
            }
        };
        let names: Vec<TypeName> = (self.types.records.iter().map(|r| r.name.clone()))
            .chain(self.types.enums.iter().map(|e| e.name.clone()))
            .collect();
        let mut borrowing = HashSet::new();
        loop {
            let before = borrowing.len();
            for name in &names {
                if fields(name).iter().any(|t| {
                    self.field_type_is_indirect(t, name.as_str())
                        || Self::holds_reference(&borrowing, t)
                }) {
                    borrowing.insert(name.clone());
                }
            }
            if borrowing.len() == before {
                return borrowing;
            }
        }
    }

    fn holds_reference(borrowing: &HashSet<TypeName>, t: &WriterDataType) -> bool {
        match t {
            WriterDataType::Bool | WriterDataType::Int | WriterDataType::Float => false,
            WriterDataType::String | WriterDataType::Array(_) => true,
            WriterDataType::Option(inner) => Self::holds_reference(borrowing, inner),
            WriterDataType::Tuple(elements) => {
                (elements.iter()).any(|element| Self::holds_reference(borrowing, element))
            }
            WriterDataType::Record(name) | WriterDataType::Enum(name) => borrowing.contains(name),
        }
    }

    fn named_type<'a>(&self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        if self.borrowing_types.iter().any(|t| t.as_str() == name) {
            arena.text(format!("{name}<'a>"))
        } else {
            arena.text(name.to_string())
        }
    }

    /// Transpile a field type, inserting `&'a` where the field needs it.
    fn transpile_field_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        t: &'a WriterDataType,
        owner: &str,
    ) -> Doc<'a> {
        if self.field_type_is_indirect(t, owner) {
            arena
                .text("&'a ")
                .append(self.transpile_data_type(arena, t))
        } else {
            self.transpile_data_type(arena, t)
        }
    }

    /// Transpile a value stored into a field of `owner`, allocating it in the
    /// arena where the field carries indirection. The IR is well typed, so the
    /// value's own type is the field's declared type.
    fn transpile_field_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        owner: &str,
        value: ValueId,
    ) -> Doc<'a> {
        if let WriterType::Data(value_type) = self.name_type(value)
            && self.field_type_is_indirect(value_type, owner)
        {
            arena.text(format!("arena.alloc({})", name_ident(value)))
        } else {
            arena.text(name_ident(value))
        }
    }

    /// Whether reads of `field` off `record` go through a reference.
    fn field_access_is_indirect(&self, record: ValueId, field: &FieldName) -> bool {
        let WriterType::Data(WriterDataType::Record(name)) = self.name_type(record) else {
            unreachable!("field access objects are records");
        };
        let field_type = self
            .types
            .record(name)
            .expect("field access objects are declared records")
            .fields
            .iter()
            .find(|f| f.name == *field)
            .map(|f| &f.typ)
            .expect("field access fields exist on the record");
        self.field_type_is_indirect(field_type, name.as_str())
    }

    /// The pattern of an enum arm, and the bindings it introduces recorded.
    /// A field carrying indirection is matched through its reference, so
    /// every binding holds a copy of the value.
    fn enum_arm_pattern(
        &mut self,
        variants: &[WriterVariant],
        arm_pattern: &WriterEnumPattern,
        bindings: &[(FieldName, IrBinder<WriterDataType>)],
    ) -> String {
        let WriterEnumPattern::Variant {
            type_name,
            variant_name,
        } = arm_pattern;
        for (_, binder) in bindings {
            self.names
                .insert(binder.var, WriterType::Data(binder.typ.clone()));
        }
        if bindings.is_empty() {
            let has_fields = variants
                .iter()
                .find(|v| &v.name == variant_name)
                .map(|v| !v.fields.is_empty())
                .unwrap_or(false);
            if has_fields {
                format!("{}::{} {{ .. }}", type_name, variant_name)
            } else {
                format!("{}::{}", type_name, variant_name)
            }
        } else {
            let bound: Vec<String> = bindings
                .iter()
                .map(|(field, binder)| {
                    let deref = if self.field_type_is_indirect(&binder.typ, type_name.as_str()) {
                        "&"
                    } else {
                        ""
                    };
                    format!(
                        "{}: {}{}",
                        Self::escape_ident(field.as_str()),
                        deref,
                        name_ident(binder.var)
                    )
                })
                .collect();
            let variant_field_count = variants
                .iter()
                .find(|v| &v.name == variant_name)
                .map(|v| v.fields.len())
                .unwrap_or(0);
            let rest = if bindings.len() < variant_field_count {
                ", .."
            } else {
                ""
            };
            format!(
                "{}::{} {{ {}{} }}",
                type_name,
                variant_name,
                bound.join(", "),
                rest,
            )
        }
    }

    /// The variants of the enum a match subject holds.
    fn subject_variants(&self, subject: ValueId) -> Vec<WriterVariant> {
        let WriterType::Data(WriterDataType::Enum(name)) = self.name_type(subject) else {
            unreachable!("Enum match subject must have an Enum type")
        };
        self.types
            .enumeration(name)
            .expect("Enum match subjects are declared enums")
            .variants
            .clone()
    }

    // A closure is a reference to a trait object in the arena, and an Html
    // closure takes the buffer it writes to first, as an Html function does.
    fn transpile_fn_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        output: Option<Doc<'a>>,
        parameters: &[WriterType],
        result: Option<Doc<'a>>,
    ) -> Doc<'a> {
        let mut param_docs: Vec<Doc<'a>> = output.into_iter().collect();
        for param in parameters {
            param_docs.push(self.transpile_type(arena, param));
        }
        let doc = arena
            .text("&'a (dyn Fn(")
            .append(arena.intersperse(param_docs, arena.text(", ")))
            .append(arena.text(")"));
        let doc = match result {
            Some(result) => doc.append(arena.text(" -> ")).append(result),
            None => doc,
        };
        doc.append(arena.text(" + 'a)"))
    }

    fn transpile_params<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parameters: impl IntoIterator<Item = &'a IrBinder<WriterType>>,
    ) -> Vec<Doc<'a>> {
        (parameters.into_iter())
            .map(|param| {
                self.names.insert(param.var, param.typ.clone());
                arena
                    .text(name_ident(param.var))
                    .append(arena.text(": "))
                    .append(self.transpile_type(arena, &param.typ))
            })
            .collect()
    }

    fn transpile_arguments<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        args: &'a [ValueId],
    ) -> Vec<Doc<'a>> {
        args.iter()
            .map(|arg| arena.text(name_ident(*arg)))
            .collect()
    }

    fn transpile_page_struct<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        page: &'a WriterPageDeclaration,
    ) -> Doc<'a> {
        let struct_name = self.page_type(arena, page);
        if page.parameters.is_empty() {
            arena
                .text("pub struct ")
                .append(struct_name)
                .append(arena.text(" {}"))
        } else {
            let fields = arena.intersperse(
                page.parameters.iter().map(|param| {
                    arena
                        .text("pub ")
                        .append(arena.text(Self::escape_ident(param.name().as_str())))
                        .append(arena.text(": "))
                        .append(self.transpile_type(arena, &param.binder.typ))
                        .append(arena.text(","))
                }),
                arena.hardline(),
            );
            arena
                .text("pub struct ")
                .append(struct_name)
                .append(arena.text(" {"))
                .append(arena.hardline().append(fields).nest(4))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
    }

    fn page_type<'a>(&self, arena: &'a Arena<'a>, page: &WriterPageDeclaration) -> Doc<'a> {
        let borrows = page.parameters.iter().any(|param| match &param.binder.typ {
            WriterType::Data(t) => Self::holds_reference(&self.borrowing_types, t),
            WriterType::Function(_) => unreachable!("a page parameter is data"),
        });
        if borrows {
            arena.text(format!("{}<'a>", page.name.as_str()))
        } else {
            arena.text(page.name.as_str().to_string())
        }
    }

    // An empty block is `{}`, which leaves no line of only indentation.
    fn transpile_braced_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [WriterStmt],
    ) -> Doc<'a> {
        if statements.is_empty() {
            return arena.text("{}");
        }
        arena
            .text("{")
            .append(
                arena
                    .hardline()
                    .append(self.transpile_statements(arena, statements))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"))
    }

    /// A value block as the body of a match arm: the result alone when the
    /// block binds nothing, otherwise a block expression.
    fn transpile_arm_block<'a>(&mut self, arena: &'a Arena<'a>, block: &'a WriterBlock) -> Doc<'a> {
        if block.bindings.is_empty() {
            return arena.text(name_ident(block.result));
        }
        arena
            .text("{")
            .append(
                arena
                    .hardline()
                    .append(self.transpile_value_block(arena, block))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"))
    }
}

impl Default for RustTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Transpiler for RustTranspiler {
    fn name_type(&self, name: ValueId) -> &WriterType {
        self.names
            .get(&name)
            .unwrap_or_else(|| unreachable!("every name records its type, and {name:?} has none"))
    }

    fn transpile_program(&mut self, program: &WriterProgram) -> String {
        self.needs_escape_html = false;
        self.indirect_edges = Self::compute_indirect_edges(&program.types);
        self.types = program.types.clone();
        self.names.clear();
        self.borrowing_types = self.compute_borrowing_types();

        let arena = &Arena::new();

        let pages = &program.pages;

        let mut result = arena.nil();

        for WriterEnum {
            name: enum_name,
            variants,
        } in &program.types.enums
        {
            result = result
                .append(arena.text("#[derive(Clone, Copy, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub enum "))
                .append(self.named_type(arena, enum_name.as_str()))
                .append(arena.text(" {"))
                .append(arena.line());

            for variant in variants {
                result = result.append(arena.text("    "));
                if variant.fields.is_empty() {
                    result = result
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text(","));
                } else {
                    result = result
                        .append(arena.text(variant.name.as_str()))
                        .append(arena.text(" { "));

                    let field_docs: Vec<_> = variant
                        .fields
                        .iter()
                        .map(|field| {
                            let ft =
                                self.transpile_field_type(arena, &field.typ, enum_name.as_str());
                            arena
                                .text(Self::escape_ident(field.name.as_str()))
                                .append(arena.text(": "))
                                .append(ft)
                        })
                        .collect();

                    result = result
                        .append(arena.intersperse(field_docs, arena.text(", ")))
                        .append(arena.text(" },"));
                }
                result = result.append(arena.line());
            }

            result = result
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
        }

        for WriterRecord {
            name: record_name,
            fields,
        } in &program.types.records
        {
            result = result
                .append(arena.text("#[derive(Clone, Copy, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub struct "))
                .append(self.named_type(arena, record_name.as_str()))
                .append(arena.text(" {"))
                .append(arena.line());

            for field in fields {
                let ft = self.transpile_field_type(arena, &field.typ, record_name.as_str());
                result = result
                    .append(arena.text("    pub "))
                    .append(arena.text(Self::escape_ident(field.name.as_str())))
                    .append(arena.text(": "))
                    .append(ft)
                    .append(arena.text(","))
                    .append(arena.line());
            }

            result = result
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
        }

        for page in pages {
            result = result
                .append(self.transpile_page_struct(arena, page))
                .append(arena.line())
                .append(arena.line());
        }

        for function in &program.functions {
            result = result
                .append(self.transpile_function_def(arena, function))
                .append(arena.line());
        }

        for (i, page) in pages.iter().enumerate() {
            result = result.append(self.transpile_page(arena, &page.name, page));
            if i < pages.len() - 1 {
                result = result.append(arena.line());
            }
        }

        if self.needs_escape_html {
            let escape_fn = arena.intersperse(
                [
                    arena.text("fn write_escaped_html(s: &str, output: &mut String) {"),
                    arena.text("    for c in s.chars() {"),
                    arena.text("        match c {"),
                    arena.text("            '&' => output.push_str(\"&amp;\"),"),
                    arena.text("            '<' => output.push_str(\"&lt;\"),"),
                    arena.text("            '>' => output.push_str(\"&gt;\"),"),
                    arena.text("            '\"' => output.push_str(\"&quot;\"),"),
                    arena.text("            _ => output.push(c),"),
                    arena.text("        }"),
                    arena.text("    }"),
                    arena.text("}"),
                ],
                arena.hardline(),
            );
            result = escape_fn
                .append(arena.hardline())
                .append(arena.hardline())
                .append(result);
        }

        if !program.pages.is_empty() {
            let view_trait = arena.intersperse(
                [
                    arena.text("pub trait View<'a> {"),
                    arena.text("    fn render(self, arena: &'a impl Arena) -> String;"),
                    arena.text("    fn write(self, arena: &'a impl Arena, output: &mut String);"),
                    arena.text("}"),
                ],
                arena.hardline(),
            );
            result = view_trait
                .append(arena.hardline())
                .append(arena.hardline())
                .append(result);
        }

        let arena_trait = arena.intersperse(
            [
                arena.text("pub trait Arena {"),
                arena.text("    fn alloc<T: Copy>(&self, value: T) -> &T;"),
                arena.text("    fn alloc_str(&self, value: &str) -> &str;"),
                arena.text(
                    "    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];",
                ),
                arena.text("}"),
            ],
            arena.hardline(),
        );
        result = arena_trait
            .append(arena.hardline())
            .append(arena.hardline())
            .append(result);

        let preamble = arena.intersperse(
            [
                arena.text("// Code generated by the hop compiler. DO NOT EDIT."),
                arena.text("#![cfg_attr(rustfmt, rustfmt_skip)]"),
                arena.text("#![allow(unused_parens, unused_variables, dead_code, clippy::all)]"),
            ],
            arena.hardline(),
        );
        result = preamble
            .append(arena.hardline())
            .append(arena.hardline())
            .append(result);

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
        let struct_name = name.as_str();
        let struct_type = self.page_type(arena, page);

        let mut write_body = arena.nil();

        if !page.parameters.is_empty() {
            let field_names = arena.intersperse(
                page.parameters.iter().map(|param| {
                    arena
                        .text(Self::escape_ident(param.name().as_str()))
                        .append(arena.text(": "))
                        .append(arena.text(name_ident(param.binder.var)))
                }),
                arena.text(", "),
            );
            for param in &page.parameters {
                self.names
                    .insert(param.binder.var, param.binder.typ.clone());
            }
            write_body = write_body
                .append(arena.text("let "))
                .append(arena.text(struct_name))
                .append(arena.text(" { "))
                .append(field_names)
                .append(arena.text(" } = self;"))
                .append(arena.hardline());
        }

        write_body = write_body.append(self.transpile_statements(arena, &page.body));

        let write_fn = arena
            .text("fn write(self, arena: &'a impl Arena, output: &mut String) {")
            .append(arena.hardline().append(write_body).nest(4))
            .append(arena.hardline())
            .append(arena.text("}"));

        let render_fn = arena
            .text("fn render(self, arena: &'a impl Arena) -> String {")
            .append(
                arena
                    .nil()
                    .append(arena.hardline())
                    .append(arena.text("let mut output: String = String::new();"))
                    .append(arena.hardline())
                    .append(arena.text("self.write(arena, &mut output);"))
                    .append(arena.hardline())
                    .append(arena.text("output"))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"));

        arena
            .text("impl<'a> View<'a> for ")
            .append(struct_type)
            .append(arena.text(" {"))
            .append(arena.hardline().append(render_fn).nest(4))
            .append(arena.hardline())
            .append(arena.hardline().append(write_fn).nest(4))
            .append(arena.hardline())
            .append(arena.text("}"))
            .append(arena.hardline())
    }

    fn transpile_write_function_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [ValueId],
    ) -> Doc<'a> {
        let mut all_args: Vec<Doc<'a>> = vec![arena.text("arena"), arena.text("output")];
        all_args.extend(self.transpile_arguments(arena, args));
        arena
            .text(function_ident(function))
            .append(arena.text("("))
            .append(arena.intersperse(all_args, arena.text(", ")))
            .append(arena.text(");"))
    }

    fn transpile_function_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a WriterFunctionDeclaration,
    ) -> Doc<'a> {
        let result = arena
            .text("fn ")
            .append(arena.text(function_ident(&function.function)))
            .append(arena.text("<'a>"));
        let binders = function.parameters.iter().map(|param| &param.binder);

        match &function.body {
            WriterFunctionBody::Writes(statements) => {
                let mut params: Vec<Doc<'a>> = vec![
                    arena.text("arena: &'a impl Arena"),
                    arena.text("output: &mut String"),
                ];
                params.extend(self.transpile_params(arena, binders));

                let result = result
                    .append(arena.text("("))
                    .append(arena.intersperse(params, arena.text(", ")))
                    .append(arena.text(") {"));

                let body = self.transpile_statements(arena, statements);

                result
                    .append(arena.hardline().append(body).nest(4))
                    .append(arena.hardline())
                    .append(arena.text("}"))
                    .append(arena.hardline())
            }
            WriterFunctionBody::Returns(block) => {
                let mut params: Vec<Doc<'a>> = vec![arena.text("arena: &'a impl Arena")];
                params.extend(self.transpile_params(arena, binders));
                let WriterResult::Returns(return_type) = &function.return_type else {
                    unreachable!("a function that returns has the type of a value as its result");
                };

                let result = result
                    .append(arena.text("("))
                    .append(arena.intersperse(params, arena.text(", ")))
                    .append(arena.text(") -> "))
                    .append(self.transpile_type(arena, return_type))
                    .append(arena.text(" {"));

                let body = self.transpile_value_block(arena, block);

                result
                    .append(arena.hardline().append(body).nest(4))
                    .append(arena.hardline())
                    .append(arena.text("}"))
                    .append(arena.hardline())
            }
        }
    }

    fn transpile_function_call_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [ValueId],
    ) -> Doc<'a> {
        let mut all_args: Vec<Doc<'a>> = vec![arena.text("arena")];
        all_args.extend(self.transpile_arguments(arena, args));
        arena
            .text(function_ident(function))
            .append(arena.text("("))
            .append(arena.intersperse(all_args, arena.text(", ")))
            .append(arena.text(")"))
    }

    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a> {
        arena
            .text("output.push_str(\"")
            .append(arena.text(self.escape_string(content)))
            .append(arena.text("\");"))
    }

    fn transpile_write_string_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: ValueId,
    ) -> Doc<'a> {
        self.needs_escape_html = true;
        arena
            .text("write_escaped_html(")
            .append(arena.text(name_ident(name)))
            .append(arena.text(", output);"))
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrBinder<WriterDataType>>,
        array: ValueId,
        body: &'a [WriterStmt],
    ) -> Doc<'a> {
        let var_name = match var {
            Some(binder) => name_ident(binder.var),
            None => "_".to_string(),
        };

        let doc = arena
            .text("for ")
            .append(arena.text(var_name))
            .append(arena.text(" in "))
            .append(arena.text(name_ident(array)))
            .append(arena.text(".iter().copied() {"));

        if let Some(binder) = var {
            self.names
                .insert(binder.var, WriterType::Data(binder.typ.clone()));
        }
        doc.append(
            arena
                .hardline()
                .append(self.transpile_statements(arena, body))
                .nest(4),
        )
        .append(arena.hardline())
        .append(arena.text("}"))
    }

    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        let_: &'a IrBinding<WriterType, WriterOp>,
    ) -> Doc<'a> {
        self.names.insert(let_.binder.var, let_.binder.typ.clone());
        arena
            .text("let ")
            .append(arena.text(name_ident(let_.binder.var)))
            .append(arena.text(": "))
            .append(self.transpile_type(arena, &let_.binder.typ))
            .append(arena.text(" = "))
            .append(self.transpile_op(arena, &let_.op, &let_.binder.typ))
            .append(arena.text(";"))
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a WriterSwitch<Vec<WriterStmt>>,
    ) -> Doc<'a> {
        match match_ {
            WriterSwitch::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let if_doc = arena
                    .text("if ")
                    .append(arena.text(name_ident(*subject)))
                    .append(arena.text(" "))
                    .append(self.transpile_braced_statements(arena, true_body));
                if false_body.is_empty() {
                    if_doc
                } else {
                    if_doc
                        .append(arena.text(" else "))
                        .append(self.transpile_braced_statements(arena, false_body))
                }
            }
            WriterSwitch::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(binder) => format!("Some({})", name_ident(binder.var)),
                    None => "Some(_)".to_string(),
                };
                if let Some(binder) = some_arm_binding {
                    self.names
                        .insert(binder.var, WriterType::Data(binder.typ.clone()));
                }

                let some_arm = arena
                    .text(some_pattern)
                    .append(arena.text(" => {"))
                    .append(
                        arena
                            .hardline()
                            .append(self.transpile_statements(arena, some_arm_body))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"));

                let none_arm = arena
                    .text("None => {")
                    .append(
                        arena
                            .hardline()
                            .append(self.transpile_statements(arena, none_arm_body))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"));

                arena
                    .text("match ")
                    .append(arena.text(name_ident(*subject)))
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .hardline()
                            .append(some_arm)
                            .append(arena.hardline())
                            .append(none_arm)
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"))
            }
            WriterSwitch::Enum { subject, arms } => {
                let variants = self.subject_variants(*subject);
                let arm_docs: Vec<Doc<'a>> = arms
                    .iter()
                    .map(|arm| {
                        let pattern = self.enum_arm_pattern(&variants, &arm.pattern, &arm.bindings);
                        arena
                            .text(pattern)
                            .append(arena.text(" => {"))
                            .append(
                                arena
                                    .hardline()
                                    .append(self.transpile_statements(arena, &arm.body))
                                    .nest(4),
                            )
                            .append(arena.hardline())
                            .append(arena.text("}"))
                    })
                    .collect();

                arena
                    .text("match ")
                    .append(arena.text(name_ident(*subject)))
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .hardline()
                            .append(arena.intersperse(arm_docs, arena.hardline()))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"))
            }
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

    fn transpile_value_block<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        block: &'a WriterBlock,
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for let_ in &block.bindings {
            docs.push(self.transpile_let_statement(arena, let_));
        }
        docs.push(arena.text(name_ident(block.result)));
        arena.intersperse(docs, arena.hardline())
    }

    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("bool")
    }

    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("&'a str")
    }

    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("f64")
    }

    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("i32")
    }

    fn transpile_array_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_type: &WriterDataType,
    ) -> Doc<'a> {
        arena
            .text("&'a [")
            .append(self.transpile_data_type(arena, element_type))
            .append(arena.text("]"))
    }

    fn transpile_tuple_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_types: &[WriterDataType],
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(
                arena.intersperse(
                    element_types
                        .iter()
                        .map(|element| self.transpile_data_type(arena, element))
                        .collect::<Vec<_>>(),
                    arena.text(", "),
                ),
            )
            .append(if element_types.len() == 1 {
                arena.text(",")
            } else {
                arena.nil()
            })
            .append(arena.text(")"))
    }

    fn transpile_option_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        inner_type: &WriterDataType,
    ) -> Doc<'a> {
        arena
            .text("Option<")
            .append(self.transpile_data_type(arena, inner_type))
            .append(arena.text(">"))
    }

    fn transpile_record_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        self.named_type(arena, name)
    }

    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        self.named_type(arena, name)
    }

    fn transpile_closure_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        signature: &WriterSignature,
    ) -> Doc<'a> {
        match signature {
            WriterSignature {
                parameters,
                result: WriterResult::Writes,
            } => {
                let output = arena.text("&mut String");
                self.transpile_fn_type(arena, Some(output), parameters, None)
            }
            WriterSignature {
                parameters,
                result: WriterResult::Returns(result),
            } => {
                let result = self.transpile_type(arena, result);
                self.transpile_fn_type(arena, None, parameters, Some(result))
            }
        }
    }

    // A closure is moved into the arena. It captures only values that are
    // Copy and live as long as the arena, which makes it Copy too.
    fn transpile_lambda<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        lambda: &'a IrLambda<WriterType, WriterFunctionBody>,
        typ: &'a WriterType,
    ) -> Doc<'a> {
        let mut params = Vec::new();
        if matches!(lambda.body, WriterFunctionBody::Writes(_)) {
            params.push(arena.text("output: &mut String"));
        }
        params.extend(self.transpile_params(arena, &lambda.parameters));
        let head = arena
            .text("move |")
            .append(arena.intersperse(params, arena.text(", ")))
            .append(arena.text("|"));
        let closure = match (&lambda.body, typ) {
            (WriterFunctionBody::Writes(statements), _) => {
                let body = self.transpile_braced_statements(arena, statements);
                head.append(arena.text(" ")).append(body)
            }
            (
                WriterFunctionBody::Returns(block),
                WriterType::Function(WriterSignature {
                    result: WriterResult::Returns(result),
                    ..
                }),
            ) => {
                let result = self.transpile_type(arena, result);
                let body = self.transpile_value_block(arena, block);
                head.append(arena.text(" -> "))
                    .append(result)
                    .append(arena.text(" {"))
                    .append(arena.hardline().append(body).nest(4))
                    .append(arena.hardline())
                    .append(arena.text("}"))
            }
            (WriterFunctionBody::Returns(_), _) => {
                unreachable!("a lambda that returns has the type of a closure of a value")
            }
        };
        arena
            .text("arena.alloc(")
            .append(closure)
            .append(arena.text(")"))
    }

    fn transpile_apply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        closure: ValueId,
        args: &'a [ValueId],
    ) -> Doc<'a> {
        let args = self.transpile_arguments(arena, args);
        arena
            .text(name_ident(closure))
            .append(arena.text("("))
            .append(arena.intersperse(args, arena.text(", ")))
            .append(arena.text(")"))
    }

    // The buffer is reborrowed, since a call through `Fn` takes its
    // arguments as a tuple, where a `&mut` is not reborrowed by itself.
    fn transpile_write_apply_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        closure: ValueId,
        args: &'a [ValueId],
    ) -> Doc<'a> {
        let mut all_args: Vec<Doc<'a>> = vec![arena.text("&mut *output")];
        all_args.extend(self.transpile_arguments(arena, args));
        arena
            .text(name_ident(closure))
            .append(arena.text("("))
            .append(arena.intersperse(all_args, arena.text(", ")))
            .append(arena.text(");"))
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record: ValueId,
        field: &'a FieldName,
    ) -> Doc<'a> {
        let indirect = self.field_access_is_indirect(record, field);
        let access = arena
            .text(name_ident(record))
            .append(arena.text("."))
            .append(arena.text(Self::escape_ident(field.as_str())));

        if indirect {
            arena.text("*").append(access)
        } else {
            access
        }
    }

    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a> {
        arena
            .text("\"")
            .append(arena.text(self.escape_string(value)))
            .append(arena.text("\""))
    }

    fn transpile_bool_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a> {
        if value {
            arena.text("true")
        } else {
            arena.text("false")
        }
    }

    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a> {
        let text = if value.is_nan() {
            "f64::NAN".to_string()
        } else if value == f64::INFINITY {
            "f64::INFINITY".to_string()
        } else if value == f64::NEG_INFINITY {
            "f64::NEG_INFINITY".to_string()
        } else {
            format!("{:?}_f64", value)
        };
        arena.text(text)
    }

    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i32) -> Doc<'a> {
        if value == i32::MIN {
            arena.text("i32::MIN")
        } else {
            arena.text(format!("{}_i32", value))
        }
    }

    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [ValueId],
        _elem_type: &'a WriterDataType,
    ) -> Doc<'a> {
        if elements.is_empty() {
            arena.text("&[]")
        } else {
            let items: Vec<Doc<'a>> = elements
                .iter()
                .map(|e| arena.text(name_ident(*e)))
                .collect();
            arena
                .text("arena.alloc([")
                .append(arena.intersperse(items, arena.text(", ")))
                .append(arena.text("])"))
        }
    }

    fn transpile_array_from_range<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        start: ValueId,
        end: ValueId,
    ) -> Doc<'a> {
        arena
            .text("arena.alloc_iter(")
            .append(arena.text(name_ident(start)))
            .append(arena.text("..="))
            .append(arena.text(name_ident(end)))
            .append(arena.text(")"))
    }

    fn transpile_tuple_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [ValueId],
        _element_types: &'a [WriterDataType],
    ) -> Doc<'a> {
        let items: Vec<Doc<'a>> = elements
            .iter()
            .map(|e| arena.text(name_ident(*e)))
            .collect();
        arena
            .text("(")
            .append(arena.intersperse(items, arena.text(", ")))
            .append(if elements.len() == 1 {
                arena.text(",")
            } else {
                arena.nil()
            })
            .append(arena.text(")"))
    }

    fn transpile_tuple_index<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        tuple: ValueId,
        index: usize,
    ) -> Doc<'a> {
        arena
            .text(name_ident(tuple))
            .append(arena.text("."))
            .append(arena.text(index.to_string()))
    }

    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(" == "))
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
            .append(arena.text(" == "))
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
            .append(arena.text(" == "))
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
            .append(arena.text(" == "))
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
            .text(name_ident(operand))
            .append(arena.text(".wrapping_neg()"))
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
        let parts = parts.iter().map(|part| arena.text(name_ident(*part)));
        arena
            .text("arena.alloc_str(&[")
            .append(arena.intersperse(parts, arena.text(", ")))
            .append(arena.text("].concat())"))
    }

    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(".wrapping_add("))
            .append(arena.text(name_ident(right)))
            .append(arena.text(")"))
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
            .text(name_ident(left))
            .append(arena.text(".wrapping_sub("))
            .append(arena.text(name_ident(right)))
            .append(arena.text(")"))
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
            .text(name_ident(left))
            .append(arena.text(".wrapping_mul("))
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

    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, ValueId)],
    ) -> Doc<'a> {
        if fields.is_empty() {
            arena.text(record_name).append(arena.text(" {}"))
        } else {
            let field_docs: Vec<Doc<'a>> = fields
                .iter()
                .map(|(name, value)| {
                    let val_doc = self.transpile_field_value(arena, record_name, *value);
                    arena
                        .text(Self::escape_ident(name.as_str()))
                        .append(arena.text(": "))
                        .append(val_doc)
                })
                .collect();
            arena
                .text(record_name)
                .append(arena.text(" { "))
                .append(arena.intersperse(field_docs, arena.text(", ")))
                .append(arena.text(" }"))
        }
    }

    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, ValueId)],
    ) -> Doc<'a> {
        if fields.is_empty() {
            arena
                .text(enum_name)
                .append(arena.text("::"))
                .append(arena.text(variant_name))
        } else {
            let field_docs: Vec<Doc<'a>> = fields
                .iter()
                .map(|(name, value)| {
                    let val_doc = self.transpile_field_value(arena, enum_name, *value);
                    arena
                        .text(Self::escape_ident(name.as_str()))
                        .append(arena.text(": "))
                        .append(val_doc)
                })
                .collect();
            arena
                .text(enum_name)
                .append(arena.text("::"))
                .append(arena.text(variant_name))
                .append(arena.text(" { "))
                .append(arena.intersperse(field_docs, arena.text(", ")))
                .append(arena.text(" }"))
        }
    }

    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<ValueId>,
        inner_type: &'a WriterDataType,
    ) -> Doc<'a> {
        match value {
            Some(inner) => arena
                .text("Some(")
                .append(arena.text(name_ident(inner)))
                .append(arena.text(")")),
            None => arena
                .text("None::<")
                .append(self.transpile_data_type(arena, inner_type))
                .append(arena.text(">")),
        }
    }

    fn transpile_match_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a WriterSwitch<WriterBlock>,
    ) -> Doc<'a> {
        match match_ {
            WriterSwitch::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject_doc = arena.text(name_ident(*subject));
                let true_arm = arena
                    .text("true => ")
                    .append(self.transpile_arm_block(arena, true_body))
                    .append(arena.text(","));
                let false_arm = arena
                    .text("false => ")
                    .append(self.transpile_arm_block(arena, false_body))
                    .append(arena.text(","));
                arena
                    .text("match ")
                    .append(subject_doc)
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .hardline()
                            .append(true_arm)
                            .append(arena.hardline())
                            .append(false_arm)
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"))
            }
            WriterSwitch::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(binder) => format!("Some({})", name_ident(binder.var)),
                    None => "Some(_)".to_string(),
                };
                if let Some(binder) = some_arm_binding {
                    self.names
                        .insert(binder.var, WriterType::Data(binder.typ.clone()));
                }
                let subject_doc = arena.text(name_ident(*subject));
                let some_arm = arena
                    .text(some_pattern)
                    .append(arena.text(" => "))
                    .append(self.transpile_arm_block(arena, some_arm_body))
                    .append(arena.text(","));
                let none_arm = arena
                    .text("None => ")
                    .append(self.transpile_arm_block(arena, none_arm_body))
                    .append(arena.text(","));
                arena
                    .text("match ")
                    .append(subject_doc)
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .hardline()
                            .append(some_arm)
                            .append(arena.hardline())
                            .append(none_arm)
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"))
            }
            WriterSwitch::Enum { subject, arms } => {
                let variants = self.subject_variants(*subject);
                let subject_doc = arena.text(name_ident(*subject));
                let arm_docs: Vec<Doc<'a>> = arms
                    .iter()
                    .map(|arm: &'a WriterEnumMatchArm<WriterBlock>| {
                        let pattern = self.enum_arm_pattern(&variants, &arm.pattern, &arm.bindings);
                        arena
                            .text(pattern)
                            .append(arena.text(" => "))
                            .append(self.transpile_arm_block(arena, &arm.body))
                            .append(arena.text(","))
                    })
                    .collect();

                arena
                    .text("match ")
                    .append(subject_doc)
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .hardline()
                            .append(arena.intersperse(arm_docs, arena.hardline()))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"))
            }
        }
    }

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(array))
            .append(arena.text(".len() as i32"))
    }

    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(array))
            .append(arena.text(".is_empty()"))
    }

    /// A negative index fails the conversion to `usize`, so it reads as
    /// out of bounds.
    fn transpile_array_get<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        array: ValueId,
        index: ValueId,
        _element_type: &'a WriterDataType,
    ) -> Doc<'a> {
        arena
            .text("usize::try_from(")
            .append(arena.text(name_ident(index)))
            .append(arena.text(").ok().and_then(|index| "))
            .append(arena.text(name_ident(array)))
            .append(arena.text(".get(index)).copied()"))
    }

    fn transpile_array_enumerate<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        array: ValueId,
        _element_type: &'a WriterDataType,
    ) -> Doc<'a> {
        arena
            .text("arena.alloc_iter(")
            .append(arena.text(name_ident(array)))
            .append(arena.text(
                ".iter().copied().enumerate().map(|(index, element)| (index as i32, element)))",
            ))
    }

    fn transpile_string_is_empty<'a>(&mut self, arena: &'a Arena<'a>, string: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(string))
            .append(arena.text(".is_empty()"))
    }

    fn transpile_option_is_some<'a>(&mut self, arena: &'a Arena<'a>, option: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".is_some()"))
    }

    fn transpile_option_is_none<'a>(&mut self, arena: &'a Arena<'a>, option: ValueId) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".is_none()"))
    }

    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a> {
        arena
            .text("arena.alloc_str(&")
            .append(arena.text(name_ident(value)))
            .append(arena.text(".to_string())"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a> {
        arena.text(name_ident(value)).append(arena.text(" as i32"))
    }

    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a> {
        arena.text(name_ident(value)).append(arena.text(" as f64"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::hop::typing::{ComparableType, NumericType, Ty};
    use crate::ir::flat_program::FlatProgram;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_to_writer;
    use crate::ir::flat_type::FlatType;
    use crate::ir::ir_page::IrPage;
    use crate::ir::optimize_flat;
    use crate::root_contained_file_path::RootContainedFilePath;
    use crate::session::Session;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    // Builder programs reach closures that hop source cannot write yet.
    fn check_program(program: FlatProgram, expected: Expect) {
        let pages = IrPage::for_entrypoints(&program);
        let writer = flat_to_writer(optimize_flat(program), &pages, None);
        let output = RustTranspiler::new().transpile_program(&writer);
        expected.assert_eq(&output);
    }

    #[test]
    fn closure_that_a_recursive_function_writes_for_each_number() {
        // The function is recursive, so the inliner keeps the closure.
        check_program(
            FlatProgramBuilder::new()
                .render_function(
                    "Countdown",
                    [
                        ("n", Ty::Int),
                        ("row", Ty::render_function([Ty::Int], Ty::Html)),
                    ],
                    Ty::Html,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.html_concat([]));
                            m.arm(Pat::False, |t, _| {
                                let row = t.apply(t.param("row"), [t.param("n")]);
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Countdown", [rest, t.param("row")]);
                                t.html_concat([row, rest])
                            });
                        })
                    },
                )
                .entrypoint("Test", [("prefix", Ty::String)], |t| {
                    let prefix = t.param("prefix");
                    let row = t.render_lambda([FlatType::INT], |t, params| {
                        let text = t.string_concat([prefix, t.int_to_string(params[0])]);
                        t.element(t.mint(), "li", [], Some(t.escape(text)))
                    });
                    t.element(
                        t.mint(),
                        "ul",
                        [],
                        Some(t.call("Countdown", [t.int(3), row])),
                    )
                })
                .build()
                .unwrap(),
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test<'a> {
                    pub prefix: &'a str,
                }

                fn render_countdown_0<'a>(arena: &'a impl Arena, output: &mut String, v_0: i32, v_1: &'a (dyn Fn(&mut String, i32) + 'a)) {
                    let v_2: i32 = 0_i32;
                    let v_3: bool = v_0 <= v_2;
                    if v_3 {} else {
                        let v_6: i32 = 1_i32;
                        let v_7: i32 = v_0.wrapping_sub(v_6);
                        v_1(&mut *output, v_0);
                        render_countdown_0(arena, output, v_7, v_1);
                    }
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { prefix: v_11 } = self;
                        let v_18: &'a (dyn Fn(&mut String, i32) + 'a) = arena.alloc(move |output: &mut String, v_12: i32| {
                            let v_13: &'a str = arena.alloc_str(&v_12.to_string());
                            output.push_str("<li>");
                            write_escaped_html(v_11, output);
                            write_escaped_html(v_13, output);
                            output.push_str("</li>");
                        });
                        let v_20: i32 = 3_i32;
                        output.push_str("<ul>");
                        render_countdown_0(arena, output, v_20, v_18);
                        output.push_str("</ul>");
                    }
                }
            "#]],
        );
    }

    #[test]
    fn closure_that_matches_on_a_name_it_captures() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Sum",
                    [("n", Ty::Int), ("scale", Ty::function([Ty::Int], Ty::Int))],
                    Ty::Int,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.int(0));
                            m.arm(Pat::False, |t, _| {
                                let scaled = t.apply(t.param("scale"), [t.param("n")]);
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Sum", [rest, t.param("scale")]);
                                t.add(NumericType::Int, scaled, rest)
                            });
                        })
                    },
                )
                .entrypoint("Test", [("double", Ty::Bool)], |t| {
                    let scale = t.lambda([FlatType::INT], |t, params| {
                        t.match_(t.param("double"), |m| {
                            m.arm(Pat::True, |t, _| {
                                t.mul(NumericType::Int, params[0], t.int(2))
                            });
                            m.arm(Pat::False, |_, _| params[0]);
                        })
                    });
                    let sum = t.call("Sum", [t.int(3), scale]);
                    t.escape(t.int_to_string(sum))
                })
                .build()
                .unwrap(),
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {
                    pub double: bool,
                }

                fn render_sum_0<'a>(arena: &'a impl Arena, v_0: i32, v_1: &'a (dyn Fn(i32) -> i32 + 'a)) -> i32 {
                    let v_2: i32 = 0_i32;
                    let v_3: bool = v_0 <= v_2;
                    let v_22: i32 = match v_3 {
                        true => {
                            let v_4: i32 = 0_i32;
                            v_4
                        },
                        false => {
                            let v_5: i32 = v_1(v_0);
                            let v_6: i32 = 1_i32;
                            let v_7: i32 = v_0.wrapping_sub(v_6);
                            let v_8: i32 = render_sum_0(arena, v_7, v_1);
                            let v_9: i32 = v_5.wrapping_add(v_8);
                            v_9
                        },
                    };
                    v_22
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { double: v_11 } = self;
                        let v_16: &'a (dyn Fn(i32) -> i32 + 'a) = arena.alloc(move |v_12: i32| -> i32 {
                            let v_21: i32 = match v_11 {
                                true => {
                                    let v_13: i32 = 2_i32;
                                    let v_14: i32 = v_12.wrapping_mul(v_13);
                                    v_14
                                },
                                false => v_12,
                            };
                            v_21
                        });
                        let v_17: i32 = 3_i32;
                        let v_18: i32 = render_sum_0(arena, v_17, v_16);
                        let v_19: &'a str = arena.alloc_str(&v_18.to_string());
                        write_escaped_html(v_19, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn closure_that_takes_html() {
        check_program(
            FlatProgramBuilder::new()
                .render_function(
                    "Wrap",
                    [
                        ("n", Ty::Int),
                        ("wrap", Ty::render_function([Ty::Html], Ty::Html)),
                    ],
                    Ty::Html,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.text("x"));
                            m.arm(Pat::False, |t, _| {
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let inner = t.call("Wrap", [rest, t.param("wrap")]);
                                t.apply(t.param("wrap"), [inner])
                            });
                        })
                    },
                )
                .entrypoint("Test", [], |t| {
                    let wrap = t.render_lambda([FlatType::HTML], |t, params| {
                        t.element(t.mint(), "b", [], Some(params[0]))
                    });
                    t.call("Wrap", [t.int(2), wrap])
                })
                .build()
                .unwrap(),
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                pub struct Test {}

                fn render_wrap_0<'a>(arena: &'a impl Arena, output: &mut String, v_0: i32, v_1: &'a (dyn Fn(&mut String, &'a (dyn Fn(&mut String) + 'a)) + 'a)) {
                    let v_2: i32 = 0_i32;
                    let v_3: bool = v_0 <= v_2;
                    if v_3 {
                        output.push_str("x");
                    } else {
                        let v_5: i32 = 1_i32;
                        let v_6: i32 = v_0.wrapping_sub(v_5);
                        let v_16: &'a (dyn Fn(&mut String) + 'a) = arena.alloc(move |output: &mut String| {
                            render_wrap_0(arena, output, v_6, v_1);
                        });
                        v_1(&mut *output, v_16);
                    }
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_13: &'a (dyn Fn(&mut String, &'a (dyn Fn(&mut String) + 'a)) + 'a) = arena.alloc(move |output: &mut String, v_10: &'a (dyn Fn(&mut String) + 'a)| {
                            output.push_str("<b>");
                            v_10(&mut *output);
                            output.push_str("</b>");
                        });
                        let v_14: i32 = 2_i32;
                        render_wrap_0(arena, output, v_14, v_13);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn html_passed_to_a_function_that_returns_a_value() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Width",
                    [("icon", Ty::Html), ("n", Ty::Int)],
                    Ty::Int,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.int(0));
                            m.arm(Pat::False, |t, _| {
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Width", [t.param("icon"), rest]);
                                t.add(NumericType::Int, t.int(2), rest)
                            });
                        })
                    },
                )
                .entrypoint("Test", [("label", Ty::String)], |t| {
                    t.element(t.mint(), "i", [], Some(t.text("*")));
                    let badge = t.element(t.mint(), "b", [], Some(t.escape(t.param("label"))));
                    let width = t.call("Width", [badge, t.int(3)]);
                    t.escape(t.int_to_string(width))
                })
                .build()
                .unwrap(),
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test<'a> {
                    pub label: &'a str,
                }

                fn render_width_0<'a>(arena: &'a impl Arena, v_0: &'a (dyn Fn(&mut String) + 'a), v_1: i32) -> i32 {
                    let v_2: i32 = 0_i32;
                    let v_3: bool = v_1 <= v_2;
                    let v_23: i32 = match v_3 {
                        true => {
                            let v_4: i32 = 0_i32;
                            v_4
                        },
                        false => {
                            let v_5: i32 = 1_i32;
                            let v_6: i32 = v_1.wrapping_sub(v_5);
                            let v_7: i32 = render_width_0(arena, v_0, v_6);
                            let v_8: i32 = 2_i32;
                            let v_9: i32 = v_8.wrapping_add(v_7);
                            v_9
                        },
                    };
                    v_23
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { label: v_11 } = self;
                        let v_18: i32 = 3_i32;
                        let v_22: &'a (dyn Fn(&mut String) + 'a) = arena.alloc(move |output: &mut String| {
                            output.push_str("<b>");
                            write_escaped_html(v_11, output);
                            output.push_str("</b>");
                        });
                        let v_19: i32 = render_width_0(arena, v_22, v_18);
                        let v_20: &'a str = arena.alloc_str(&v_19.to_string());
                        write_escaped_html(v_20, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn closure_that_renders_html_bound_in_a_block_that_produces_a_value() {
        check_program(
            FlatProgramBuilder::new()
                .function(
                    "Count",
                    [
                        ("n", Ty::Int),
                        ("row", Ty::render_function([Ty::Int], Ty::Html)),
                    ],
                    Ty::Int,
                    |t| {
                        t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                            m.arm(Pat::True, |t, _| t.int(0));
                            m.arm(Pat::False, |t, _| {
                                let rest = t.sub(NumericType::Int, t.param("n"), t.int(1));
                                let rest = t.call("Count", [rest, t.param("row")]);
                                t.add(NumericType::Int, t.int(1), rest)
                            });
                        })
                    },
                )
                .entrypoint("Test", [("show", Ty::Bool)], |t| {
                    let count = t.match_(t.param("show"), |m| {
                        m.arm(Pat::True, |t, _| {
                            let row = t.render_lambda([FlatType::INT], |t, params| {
                                t.escape(t.int_to_string(params[0]))
                            });
                            t.call("Count", [t.int(3), row])
                        });
                        m.arm(Pat::False, |t, _| t.int(0));
                    });
                    t.escape(t.int_to_string(count))
                })
                .build()
                .unwrap(),
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {
                    pub show: bool,
                }

                fn render_count_0<'a>(arena: &'a impl Arena, v_0: i32, v_1: &'a (dyn Fn(&mut String, i32) + 'a)) -> i32 {
                    let v_2: i32 = 0_i32;
                    let v_3: bool = v_0 <= v_2;
                    let v_23: i32 = match v_3 {
                        true => {
                            let v_4: i32 = 0_i32;
                            v_4
                        },
                        false => {
                            let v_5: i32 = 1_i32;
                            let v_6: i32 = v_0.wrapping_sub(v_5);
                            let v_7: i32 = render_count_0(arena, v_6, v_1);
                            let v_8: i32 = 1_i32;
                            let v_9: i32 = v_8.wrapping_add(v_7);
                            v_9
                        },
                    };
                    v_23
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { show: v_11 } = self;
                        let v_22: i32 = match v_11 {
                            true => {
                                let v_15: &'a (dyn Fn(&mut String, i32) + 'a) = arena.alloc(move |output: &mut String, v_12: i32| {
                                    let v_13: &'a str = arena.alloc_str(&v_12.to_string());
                                    write_escaped_html(v_13, output);
                                });
                                let v_16: i32 = 3_i32;
                                let v_17: i32 = render_count_0(arena, v_16, v_15);
                                v_17
                            },
                            false => {
                                let v_18: i32 = 0_i32;
                                v_18
                            },
                        };
                        let v_20: &'a str = arena.alloc_str(&v_22.to_string());
                        write_escaped_html(v_20, output);
                    }
                }
            "#]],
        );
    }

    fn check(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut session = Session::new();
        session.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = session.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (flat, pages) = session.compile_flat(None, None);
        let writer = flat_to_writer(flat, &pages, None);
        let output = RustTranspiler::new().transpile_program(&writer);
        expected.assert_eq(&output);
    }

    #[test]
    fn function_that_returns_a_closure() {
        check(
            indoc! {r#"
                fn Decorate(suffix: String) -> fn(String) -> Html {
                  fn(label) { <b>{label}{suffix}</b> }
                }

                fn adder(n: Int) -> fn(Int) -> Int {
                  fn(m) { n + m }
                }

                page Test() {
                  fn body() -> Html {
                    let loud = Decorate("!");
                    let add_one = adder(1);
                    <>{loud(add_one(1).to_string())}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {}

                fn render_decorate_0<'a>(arena: &'a impl Arena, v_9: &'a str) -> &'a (dyn Fn(&mut String, &'a str) + 'a) {
                    let v_16: &'a (dyn Fn(&mut String, &'a str) + 'a) = arena.alloc(move |output: &mut String, v_10: &'a str| {
                        output.push_str("<b>");
                        write_escaped_html(v_10, output);
                        write_escaped_html(v_9, output);
                        output.push_str("</b>");
                    });
                    v_16
                }

                fn render_adder_1<'a>(arena: &'a impl Arena, v_17: i32) -> &'a (dyn Fn(i32) -> i32 + 'a) {
                    let v_20: &'a (dyn Fn(i32) -> i32 + 'a) = arena.alloc(move |v_18: i32| -> i32 {
                        let v_19: i32 = v_17.wrapping_add(v_18);
                        v_19
                    });
                    v_20
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: &'a str = "!";
                        let v_1: &'a (dyn Fn(&mut String, &'a str) + 'a) = render_decorate_0(arena, v_0);
                        let v_2: i32 = 1_i32;
                        let v_3: &'a (dyn Fn(i32) -> i32 + 'a) = render_adder_1(arena, v_2);
                        let v_4: i32 = 1_i32;
                        let v_5: i32 = v_3(v_4);
                        let v_6: &'a str = arena.alloc_str(&v_5.to_string());
                        v_1(&mut *output, v_6);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn html_passed_to_a_recursive_function() {
        check(
            indoc! {r#"
                fn Nest(depth: Int, children: Html) -> Html {
                  match depth > 0 {
                    true => <div><Nest depth={depth - 1}>{children}</Nest></div>,
                    false => children,
                  }
                }

                page Test(name: String) {
                  fn body() -> Html {
                    <Nest depth={2}>
                      <b>{name}</b>
                    </Nest>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test<'a> {
                    pub name: &'a str,
                }

                fn render_nest_0<'a>(arena: &'a impl Arena, output: &mut String, v_8: i32, v_9: &'a (dyn Fn(&mut String) + 'a)) {
                    let v_10: i32 = 0_i32;
                    let v_11: bool = v_10 < v_8;
                    if v_11 {
                        let v_12: i32 = 1_i32;
                        let v_13: i32 = v_8.wrapping_sub(v_12);
                        output.push_str("<div>");
                        render_nest_0(arena, output, v_13, v_9);
                        output.push_str("</div>");
                    } else {
                        v_9(&mut *output);
                    }
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { name: v_0 } = self;
                        let v_1: i32 = 2_i32;
                        let v_20: &'a (dyn Fn(&mut String) + 'a) = arena.alloc(move |output: &mut String| {
                            output.push_str("<b>");
                            write_escaped_html(v_0, output);
                            output.push_str("</b>");
                        });
                        render_nest_0(arena, output, v_1, v_20);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn page_with_empty_tuple() {
        check(
            indoc! {r#"
                record Holder {
                  nothing: Tuple[],
                }

                page Test(unit: Tuple[]) {
                  fn body() -> Html {
                    let held = Holder {nothing: ()};
                    <>{[unit, held.nothing].len().to_string()}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Holder {
                    pub nothing: (),
                }

                pub struct Test {
                    pub unit: (),
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { unit: v_0 } = self;
                        let v_1: () = ();
                        let v_2: Holder = Holder { nothing: v_1 };
                        let v_3: () = v_2.nothing;
                        let v_4: &'a [()] = arena.alloc([v_0, v_3]);
                        let v_5: i32 = v_4.len() as i32;
                        let v_6: &'a str = arena.alloc_str(&v_5.to_string());
                        write_escaped_html(v_6, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn simple_page() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <h1>Hello, World!</h1>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        output.push_str("<h1>Hello, World!</h1>");
                    }
                }
            "#]],
        );
    }

    #[test]
    fn page_structs_grouped_above_impls() {
        check(
            indoc! {r#"
                page First() {
                  fn body() -> Html {
                    <h1>First</h1>
                  }
                }

                page Second(title: String) {
                  fn body() -> Html {
                    <>{title}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct First {}

                pub struct Second<'a> {
                    pub title: &'a str,
                }

                impl<'a> View<'a> for First {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        output.push_str("<h1>First</h1>");
                    }
                }

                impl<'a> View<'a> for Second<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Second { title: v_4 } = self;
                        write_escaped_html(v_4, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            indoc! {r#"
                page Test(show: Bool) {
                  fn body() -> Html {
                    match show {
                      true => <h1>Visible</h1>,
                      false => <></>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                pub struct Test {
                    pub show: bool,
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { show: v_0 } = self;
                        if v_0 {
                            output.push_str("<h1>Visible</h1>");
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_range() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    for i in 1..=3 {
                      <>{i.to_string()}</>
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: i32 = 1_i32;
                        let v_1: i32 = 3_i32;
                        let v_2: &'a [i32] = arena.alloc_iter(v_0..=v_1);
                        for v_3 in v_2.iter().copied() {
                            let v_4: &'a str = arena.alloc_str(&v_3.to_string());
                            write_escaped_html(v_4, output);
                        }
                    }
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
                      Some(value) => <>some: {value}</>,
                      None => <>none</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: &'a str = "x";
                        let v_1: Option<&'a str> = Some(v_0);
                        match v_1 {
                            Some(v_9) => {
                                output.push_str("some: ");
                                write_escaped_html(v_9, output);
                            }
                            None => {
                                output.push_str("none");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            indoc! {r#"
                page Test(opt: Option[String]) {
                  fn body() -> Html {
                    match opt {
                      Some(value) => <>some: {value}</>,
                      None => <>none</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test<'a> {
                    pub opt: Option<&'a str>,
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { opt: v_0 } = self;
                        match v_0 {
                            Some(v_8) => {
                                output.push_str("some: ");
                                write_escaped_html(v_8, output);
                            }
                            None => {
                                output.push_str("none");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_refers_to_recursive_field() {
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
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Node<'a> {
                    pub value: i32,
                    pub next: &'a Option<Node<'a>>,
                }

                pub struct Test<'a> {
                    pub node: Node<'a>,
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { node: v_0 } = self;
                        let v_1: i32 = v_0.value;
                        let v_2: &'a str = arena.alloc_str(&v_1.to_string());
                        write_escaped_html(v_2, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_refers_to_recursive_field() {
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
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                #[derive(Clone, Copy, Debug)]
                pub enum IntList<'a> {
                    Cons { head: i32, tail: &'a IntList<'a> },
                    Nil,
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        output.push_str("hello");
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_literal_allocates_field_values() {
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
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Node<'a> {
                    pub value: i32,
                    pub next: &'a Option<Node<'a>>,
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: i32 = 2_i32;
                        let v_1: i32 = 1_i32;
                        let v_2: Option<Node<'a>> = None::<Node<'a>>;
                        let v_3: Node<'a> = Node { value: v_1, next: arena.alloc(v_2) };
                        let v_4: Option<Node<'a>> = Some(v_3);
                        let v_5: Node<'a> = Node { value: v_0, next: arena.alloc(v_4) };
                        let v_6: i32 = v_5.value;
                        let v_7: &'a str = arena.alloc_str(&v_6.to_string());
                        write_escaped_html(v_7, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn matching_a_recursive_enum_field_derefs_the_reference() {
        check(
            indoc! {r#"
                enum Expr {
                  Literal {
                    value: String,
                  },
                  Neg {
                    inner: Expr,
                  },
                }

                page Test(e: Expr) {
                  fn body() -> Html {
                    match e {
                      Expr::Neg {inner: i} => {
                        match i {
                          Expr::Literal {value: v} => <>{v}</>,
                          Expr::Neg {inner: _} => <>nested</>,
                        }
                      },
                      Expr::Literal {value: _} => <>lit</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub enum Expr<'a> {
                    Literal { value: &'a str },
                    Neg { inner: &'a Expr<'a> },
                }

                pub struct Test<'a> {
                    pub e: Expr<'a>,
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { e: v_0 } = self;
                        match v_0 {
                            Expr::Literal { .. } => {
                                output.push_str("lit");
                            }
                            Expr::Neg { inner: &v_11 } => {
                                match v_11 {
                                    Expr::Literal { value: v_12 } => {
                                        write_escaped_html(v_12, output);
                                    }
                                    Expr::Neg { .. } => {
                                        output.push_str("nested");
                                    }
                                }
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn matching_a_recursive_option_field_derefs_the_reference() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                page Test(node: Node) {
                  fn body() -> Html {
                    match node.next {
                      Some(n) => <>{n.value}</>,
                      None => <>end</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Node<'a> {
                    pub value: &'a str,
                    pub next: &'a Option<Node<'a>>,
                }

                pub struct Test<'a> {
                    pub node: Node<'a>,
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { node: v_0 } = self;
                        let v_1: Option<Node<'a>> = *v_0.next;
                        match v_1 {
                            Some(v_9) => {
                                let v_3: &'a str = v_9.value;
                                write_escaped_html(v_3, output);
                            }
                            None => {
                                output.push_str("end");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn matching_an_option_bound_from_a_recursive_enum_field() {
        check(
            indoc! {r#"
                enum Chain {
                  Link {
                    next: Option[Chain],
                  },
                  End,
                }

                page Test(c: Chain) {
                  fn body() -> Html {
                    match c {
                      Chain::Link {next: n} => {
                        match n {
                          Some(_) => <>more</>,
                          None => <>last</>,
                        }
                      },
                      Chain::End => <>end</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                #[derive(Clone, Copy, Debug)]
                pub enum Chain<'a> {
                    Link { next: &'a Option<Chain<'a>> },
                    End,
                }

                pub struct Test<'a> {
                    pub c: Chain<'a>,
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { c: v_0 } = self;
                        match v_0 {
                            Chain::Link { next: &v_10 } => {
                                match v_10 {
                                    Some(_) => {
                                        output.push_str("more");
                                    }
                                    None => {
                                        output.push_str("last");
                                    }
                                }
                            }
                            Chain::End => {
                                output.push_str("end");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_literal_allocates_field_values() {
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
                    let list = IntList::Cons {head: 1, tail: IntList::Nil};
                    match list {
                      IntList::Cons {head: h, tail: _} => <>{h.to_string()}</>,
                      IntList::Nil => <>done</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub enum IntList<'a> {
                    Cons { head: i32, tail: &'a IntList<'a> },
                    Nil,
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: i32 = 1_i32;
                        let v_1: IntList<'a> = IntList::Nil;
                        let v_2: IntList<'a> = IntList::Cons { head: v_0, tail: arena.alloc(v_1) };
                        match v_2 {
                            IntList::Cons { head: v_10, .. } => {
                                let v_4: &'a str = arena.alloc_str(&v_10.to_string());
                                write_escaped_html(v_4, output);
                            }
                            IntList::Nil => {
                                output.push_str("done");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn mutually_recursive_records_refer_in_both_directions() {
        check(
            indoc! {r#"
                record A {
                  b: B,
                }

                record B {
                  a: Option[A],
                }

                page Test() {
                  fn body() -> Html {
                    let b = B {a: Some(A {b: B {a: None}})};
                    match b.a {
                      Some(_) => <>done</>,
                      None => <>none</>,
                    }
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                #[derive(Clone, Copy, Debug)]
                pub struct A<'a> {
                    pub b: &'a B<'a>,
                }

                #[derive(Clone, Copy, Debug)]
                pub struct B<'a> {
                    pub a: &'a Option<A<'a>>,
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: Option<A<'a>> = None::<A<'a>>;
                        let v_1: B<'a> = B { a: arena.alloc(v_0) };
                        let v_2: A<'a> = A { b: arena.alloc(v_1) };
                        let v_3: Option<A<'a>> = Some(v_2);
                        let v_4: B<'a> = B { a: arena.alloc(v_3) };
                        let v_5: Option<A<'a>> = *v_4.a;
                        match v_5 {
                            Some(_) => {
                                output.push_str("done");
                            }
                            None => {
                                output.push_str("none");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn record_takes_a_lifetime_only_when_it_holds_a_reference() {
        check(
            indoc! {r#"
                record Point {
                  x: Int,
                  y: Int,
                }

                record Name {
                  value: String,
                }

                record Label {
                  name: Name,
                  at: Point,
                }

                page Test(labels: Array[Label]) {
                  fn body() -> Html {
                    let none: Array[Label] = [];
                    <>
                      {for pair in labels.enumerate() {
                        match pair {
                          (i, label) => <>{i.to_string()}{label.name.value}</>,
                        }
                      }}
                      {match none.get(0) {
                        Some(label) => <>{label.at.x.to_string()}</>,
                        None => <>none</>,
                      }}
                    </>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Label<'a> {
                    pub name: Name<'a>,
                    pub at: Point,
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Name<'a> {
                    pub value: &'a str,
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Point {
                    pub x: i32,
                    pub y: i32,
                }

                pub struct Test<'a> {
                    pub labels: &'a [Label<'a>],
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { labels: v_0 } = self;
                        let v_1: &'a [Label<'a>] = &[];
                        let v_2: &'a [(i32, Label<'a>)] = arena.alloc_iter(v_0.iter().copied().enumerate().map(|(index, element)| (index as i32, element)));
                        let v_14: i32 = 0_i32;
                        let v_15: Option<Label<'a>> = usize::try_from(v_14).ok().and_then(|index| v_1.get(index)).copied();
                        for v_3 in v_2.iter().copied() {
                            let v_26: i32 = v_3.0;
                            let v_27: Label<'a> = v_3.1;
                            let v_6: &'a str = arena.alloc_str(&v_26.to_string());
                            let v_8: Name<'a> = v_27.name;
                            let v_9: &'a str = v_8.value;
                            write_escaped_html(v_6, output);
                            write_escaped_html(v_9, output);
                        }
                        match v_15 {
                            Some(v_28) => {
                                let v_17: Point = v_28.at;
                                let v_18: i32 = v_17.x;
                                let v_19: &'a str = arena.alloc_str(&v_18.to_string());
                                write_escaped_html(v_19, output);
                            }
                            None => {
                                output.push_str("none");
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn wide_match_expression_breaks_one_arm_per_line() {
        check(
            indoc! {r#"
                enum TimeAgo {
                  JustNow,
                  MinutesAgo {
                    count: Int,
                  },
                  HoursAgo {
                    count: Int,
                  },
                }

                page Test(time: TimeAgo) {
                  fn body() -> Html {
                    <>{match time {
                      TimeAgo::JustNow => "just now",
                      TimeAgo::MinutesAgo {count: count} => {
                        match count == 1 {
                          true => "1 minute ago",
                          false => count.to_string() + " minutes ago",
                        }
                      },
                      TimeAgo::HoursAgo {count: count} => {
                        match count == 1 {
                          true => "1 hour ago",
                          false => count.to_string() + " hours ago",
                        }
                      },
                    }}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub enum TimeAgo {
                    JustNow,
                    MinutesAgo { count: i32 },
                    HoursAgo { count: i32 },
                }

                pub struct Test {
                    pub time: TimeAgo,
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { time: v_0 } = self;
                        let v_25: &'a str = match v_0 {
                            TimeAgo::JustNow => {
                                let v_1: &'a str = "just now";
                                v_1
                            },
                            TimeAgo::MinutesAgo { count: v_21 } => {
                                let v_3: i32 = 1_i32;
                                let v_4: bool = v_21 == v_3;
                                let v_22: &'a str = match v_4 {
                                    true => {
                                        let v_5: &'a str = "1 minute ago";
                                        v_5
                                    },
                                    false => {
                                        let v_6: &'a str = arena.alloc_str(&v_21.to_string());
                                        let v_7: &'a str = " minutes ago";
                                        let v_8: &'a str = arena.alloc_str(&[v_6, v_7].concat());
                                        v_8
                                    },
                                };
                                v_22
                            },
                            TimeAgo::HoursAgo { count: v_23 } => {
                                let v_11: i32 = 1_i32;
                                let v_12: bool = v_23 == v_11;
                                let v_24: &'a str = match v_12 {
                                    true => {
                                        let v_13: &'a str = "1 hour ago";
                                        v_13
                                    },
                                    false => {
                                        let v_14: &'a str = arena.alloc_str(&v_23.to_string());
                                        let v_15: &'a str = " hours ago";
                                        let v_16: &'a str = arena.alloc_str(&[v_14, v_15].concat());
                                        v_16
                                    },
                                };
                                v_24
                            },
                        };
                        write_escaped_html(v_25, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn option_match_expression_breaks_one_arm_per_line() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <>{match Some("world") {
                      Some(value) => "hello " + value,
                      None => "nobody",
                    }}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {}

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: &'a str = "world";
                        let v_1: Option<&'a str> = Some(v_0);
                        let v_10: &'a str = match v_1 {
                            Some(v_9) => {
                                let v_3: &'a str = "hello ";
                                let v_4: &'a str = arena.alloc_str(&[v_3, v_9].concat());
                                v_4
                            },
                            None => {
                                let v_5: &'a str = "nobody";
                                v_5
                            },
                        };
                        write_escaped_html(v_10, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn function_with_enum_param() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                fn Badge(color: Color) -> Html {
                  match color {
                    Color::Red => <>red</>,
                    Color::Green => <>green</>,
                    Color::Blue => <>blue</>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Badge color={Color::Green}/>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                #[derive(Clone, Copy, Debug)]
                pub enum Color {
                    Red,
                    Green,
                    Blue,
                }

                pub struct Test {}

                fn render_badge_0<'a>(arena: &'a impl Arena, output: &mut String, v_2: Color) {
                    match v_2 {
                        Color::Red => {
                            output.push_str("red");
                        }
                        Color::Green => {
                            output.push_str("green");
                        }
                        Color::Blue => {
                            output.push_str("blue");
                        }
                    }
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: Color = Color::Green;
                        render_badge_0(arena, output, v_0);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn record_and_array_params_are_passed_by_value() {
        check(
            indoc! {r#"
                record Post {
                  title: String,
                  views: Int,
                }

                fn Card(p: Post, tags: Array[String]) -> Html {
                  <>{p.title}{p.views.to_string()}{tags.len().to_string()}</>
                }

                page Test(post: Post, tags: Array[String]) {
                  fn body() -> Html {
                    <Card p={post} tags={tags}/>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Post<'a> {
                    pub title: &'a str,
                    pub views: i32,
                }

                pub struct Test<'a> {
                    pub post: Post<'a>,
                    pub tags: &'a [&'a str],
                }

                fn render_card_0<'a>(arena: &'a impl Arena, output: &mut String, v_3: Post<'a>, v_4: &'a [&'a str]) {
                    let v_5: &'a str = v_3.title;
                    let v_7: i32 = v_3.views;
                    let v_8: &'a str = arena.alloc_str(&v_7.to_string());
                    let v_10: i32 = v_4.len() as i32;
                    let v_11: &'a str = arena.alloc_str(&v_10.to_string());
                    write_escaped_html(v_5, output);
                    write_escaped_html(v_8, output);
                    write_escaped_html(v_11, output);
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { post: v_0, tags: v_1 } = self;
                        render_card_0(arena, output, v_0, v_1);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn param_is_stored_in_a_record_without_a_clone() {
        check(
            indoc! {r#"
                record Tag {
                  name: String,
                }

                record Wrap {
                  tag: Tag,
                }

                fn Show(t: Tag) -> Html {
                  let w = Wrap {tag: t};
                  <>{w.tag.name}</>
                }

                page Test(tag: Tag) {
                  fn body() -> Html {
                    <Show t={tag}/>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Tag<'a> {
                    pub name: &'a str,
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Wrap<'a> {
                    pub tag: Tag<'a>,
                }

                pub struct Test<'a> {
                    pub tag: Tag<'a>,
                }

                fn render_show_0<'a>(arena: &'a impl Arena, output: &mut String, v_2: Tag<'a>) {
                    let v_3: Wrap<'a> = Wrap { tag: v_2 };
                    let v_4: Tag<'a> = v_3.tag;
                    let v_5: &'a str = v_4.name;
                    write_escaped_html(v_5, output);
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { tag: v_0 } = self;
                        render_show_0(arena, output, v_0);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn let_binding_a_field_copies_it() {
        check(
            indoc! {r#"
                record Post {
                  title: String,
                  body: String,
                }

                page Test(post: Post) {
                  fn body() -> Html {
                    let title = post.title;
                    <>{title}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                #[derive(Clone, Copy, Debug)]
                pub struct Post<'a> {
                    pub title: &'a str,
                    pub body: &'a str,
                }

                pub struct Test<'a> {
                    pub post: Post<'a>,
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { post: v_0 } = self;
                        let v_1: &'a str = v_0.title;
                        write_escaped_html(v_1, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn string_equality_compares_str_slices() {
        check(
            indoc! {r#"
                fn Role(role: String) -> Html {
                  match role == "admin" {
                    true => <>yes</>,
                    false => <>no</>,
                  }
                }

                page Test(role: String) {
                  fn body() -> Html {
                    <Role role={role}/>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                pub struct Test<'a> {
                    pub role: &'a str,
                }

                fn render_role_0<'a>(arena: &'a impl Arena, output: &mut String, v_2: &'a str) {
                    let v_3: &'a str = "admin";
                    let v_4: bool = v_2 == v_3;
                    if v_4 {
                        output.push_str("yes");
                    } else {
                        output.push_str("no");
                    }
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { role: v_0 } = self;
                        render_role_0(arena, output, v_0);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn function_with_option_param_takes_it_by_value() {
        check(
            indoc! {r#"
                fn Label(text: Option[String]) -> Html {
                  match text {
                    Some(value) => <>{value}</>,
                    None => <>none</>,
                  }
                }

                page Test(label: Option[String]) {
                  fn body() -> Html {
                    <Label text={label}/>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test<'a> {
                    pub label: Option<&'a str>,
                }

                fn render_label_0<'a>(arena: &'a impl Arena, output: &mut String, v_2: Option<&'a str>) {
                    match v_2 {
                        Some(v_9) => {
                            write_escaped_html(v_9, output);
                        }
                        None => {
                            output.push_str("none");
                        }
                    }
                }

                impl<'a> View<'a> for Test<'a> {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let Test { label: v_0 } = self;
                        render_label_0(arena, output, v_0);
                    }
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
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                pub struct Test {}

                fn render_frag_0<'a>(arena: &'a impl Arena, output: &mut String) {
                    output.push_str("<b>hi</b>");
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        render_frag_0(arena, output);
                    }
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
                    <>{format_price(5).to_string()}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {}

                fn render_format_price_0<'a>(arena: &'a impl Arena, v_5: i32) -> i32 {
                    v_5
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: i32 = 5_i32;
                        let v_1: i32 = render_format_price_0(arena, v_0);
                        let v_2: &'a str = arena.alloc_str(&v_1.to_string());
                        write_escaped_html(v_2, output);
                    }
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
                      {for x in 0..=foo(-7) {
                        <>{x.to_string()},</>
                      }}
                      {foo(10).to_string()}
                    </div>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, unused_variables, dead_code, clippy::all)]

                pub trait Arena {
                    fn alloc<T: Copy>(&self, value: T) -> &T;
                    fn alloc_str(&self, value: &str) -> &str;
                    fn alloc_iter<T: Copy>(&self, values: impl IntoIterator<Item = T>) -> &[T];
                }

                pub trait View<'a> {
                    fn render(self, arena: &'a impl Arena) -> String;
                    fn write(self, arena: &'a impl Arena, output: &mut String);
                }

                fn write_escaped_html(s: &str, output: &mut String) {
                    for c in s.chars() {
                        match c {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            '"' => output.push_str("&quot;"),
                            _ => output.push(c),
                        }
                    }
                }

                pub struct Test {}

                fn render_foo_0<'a>(arena: &'a impl Arena, v_17: i32) -> i32 {
                    let v_18: i32 = 10_i32;
                    let v_19: i32 = v_17.wrapping_add(v_18);
                    v_19
                }

                impl<'a> View<'a> for Test {
                    fn render(self, arena: &'a impl Arena) -> String {
                        let mut output: String = String::new();
                        self.write(arena, &mut output);
                        output
                    }

                    fn write(self, arena: &'a impl Arena, output: &mut String) {
                        let v_0: i32 = 0_i32;
                        let v_1: i32 = -7_i32;
                        let v_2: i32 = render_foo_0(arena, v_1);
                        let v_3: &'a [i32] = arena.alloc_iter(v_0..=v_2);
                        let v_10: i32 = 10_i32;
                        let v_11: i32 = render_foo_0(arena, v_10);
                        let v_12: &'a str = arena.alloc_str(&v_11.to_string());
                        output.push_str("<div>");
                        for v_4 in v_3.iter().copied() {
                            let v_5: &'a str = arena.alloc_str(&v_4.to_string());
                            write_escaped_html(v_5, output);
                            output.push_str(",");
                        }
                        write_escaped_html(v_12, output);
                        output.push_str("</div>");
                    }
                }
            "#]],
        );
    }
}
