use std::collections::{BTreeSet, HashMap, HashSet};

use pretty::{Arena, DocAllocator};

use super::Doc;
use super::transpiler::Transpiler;
use crate::dependency_graph::DependencyGraph;
use crate::hop::typing::{EnumVariant, ResolvedType, Type, TypeRegistry};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::writer_module::{
    WriterForSource, WriterFunctionBody, WriterFunctionDeclaration, WriterLet, WriterModule,
    WriterName, WriterOp, WriterPageDeclaration, WriterStmt, WriterValueBlock,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// Names every variable in the generated code after the name that binds it
/// in the IR rather than the source name.
///
/// Names are unique across the module, a let's as `v_` and a binder's as
/// `b_`, so no hop identifier can shadow another, and no name can collide
/// with a keyword or with the `output` buffer.
fn name_ident(name: WriterName) -> String {
    match name {
        WriterName::Binding(var) => format!("v_{}", var.index()),
        WriterName::Binder(binder) => format!("b_{}", binder.index()),
    }
}

fn function_ident(function: &IrFunction) -> String {
    format!("render_{}_{}", function.name.to_snake_case(), function.id)
}

/// What a binding in the generated code holds.
///
/// Function parameters and pattern bindings are references into a value the
/// generated code does not own, so holding the value is not the common case.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Binding {
    /// Bound to the value.
    Owned,
    /// Bound to a reference to the value, or to an unsized view of it.
    Borrowed,
    /// Bound to a reference to a `Box` holding the value, which is how a
    /// pattern binds a field that carries `Box` indirection.
    BorrowedBoxed,
}

/// What a value transpiles to, before any demand is placed on it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NaturalForm {
    /// A reference to the value: a string literal, a borrowed binding.
    Reference,
    /// A place holding the value: an owned binding, a field read.
    Place,
    /// A temporary: a fresh value, which nothing else holds.
    Temporary,
}

pub struct RustTranspiler {
    /// Tracks whether escape_html function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether Html type is used during transpilation
    needs_html: bool,
    /// Field positions carrying `Box` indirection.
    /// We box in both directions for mutually recursive types.
    boxed_edges: HashSet<(TypeName, TypeName)>,
    /// Registry used to resolve named type structure.
    registry: TypeRegistry,
    /// What the binding for every name holds, and the name's type. Every
    /// parameter, let and binder inserts its entry before anything reads it.
    names: HashMap<WriterName, (Binding, Type)>,
}

impl RustTranspiler {
    pub fn new() -> Self {
        Self {
            needs_escape_html: false,
            needs_html: false,
            boxed_edges: HashSet::new(),
            registry: TypeRegistry::default(),
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

    fn name_binding(&self, name: WriterName) -> Binding {
        self.names
            .get(&name)
            .map(|(binding, _)| *binding)
            .unwrap_or_else(|| unreachable!("every name records a binding, and {name} has none"))
    }

    fn name_type(&self, name: WriterName) -> &Type {
        self.names
            .get(&name)
            .map(|(_, typ)| typ)
            .unwrap_or_else(|| unreachable!("every name records its type, and {name} has none"))
    }

    /// What the op a let binds transpiles to.
    fn natural_form(&self, op: &WriterOp) -> NaturalForm {
        match op {
            WriterOp::StringLiteral(_) => NaturalForm::Reference,
            WriterOp::FieldAccess { .. } | WriterOp::TupleIndex { .. } => NaturalForm::Place,
            WriterOp::IntLiteral(_)
            | WriterOp::FloatLiteral(_)
            | WriterOp::BoolLiteral(_)
            | WriterOp::Array(_)
            | WriterOp::Tuple(_)
            | WriterOp::Record { .. }
            | WriterOp::Enum { .. }
            | WriterOp::Option(_)
            | WriterOp::StringConcat(_)
            | WriterOp::Binary { .. }
            | WriterOp::Unary { .. }
            | WriterOp::Call { .. }
            | WriterOp::HtmlLiteral(_)
            | WriterOp::Match(_) => NaturalForm::Temporary,
        }
    }

    /// A name where a reference to its value is wanted.
    fn name_ref<'a>(&self, arena: &'a Arena<'a>, name: WriterName) -> Doc<'a> {
        match self.name_binding(name) {
            Binding::Borrowed => arena.text(name_ident(name)),
            Binding::Owned => arena.text("&").append(arena.text(name_ident(name))),
            Binding::BorrowedBoxed => arena.text("&**").append(arena.text(name_ident(name))),
        }
    }

    /// A name where the place holding its value is wanted as an operand: of
    /// an operator, a cast, a range, a condition or a match.
    ///
    /// Operands are why this is not left to auto-dereferencing, which covers
    /// receivers and field reads but not operators: `&i32 < i32` does not
    /// typecheck. A `Box` is stripped here rather than left in place:
    /// patterns do not auto-dereference, so a match would not see past it.
    /// A dereference binds tighter than any operator, so it needs no
    /// parentheses. Receivers and field reads take the binding itself, since
    /// auto-dereferencing sees through references and `Box` alike.
    fn name_place<'a>(&self, arena: &'a Arena<'a>, name: WriterName) -> Doc<'a> {
        match self.name_binding(name) {
            Binding::Borrowed if Self::is_scalar(self.name_type(name)) => {
                arena.text("*").append(arena.text(name_ident(name)))
            }
            Binding::Borrowed | Binding::Owned => arena.text(name_ident(name)),
            Binding::BorrowedBoxed => arena.text("**").append(arena.text(name_ident(name))),
        }
    }

    /// A name where an owned value is wanted. A scalar copies, and anything
    /// else clones, since the binding keeps holding the value. `Clone`
    /// resolves on a `Box` itself, handing back a `Box` where the value is
    /// wanted, so a boxed binding is dereferenced before the clone.
    fn name_owned<'a>(&self, arena: &'a Arena<'a>, name: WriterName) -> Doc<'a> {
        let typ = self.name_type(name);
        if Self::is_scalar(typ) {
            return self.name_place(arena, name);
        }
        let method = match typ {
            Type::Array(_) => ".to_vec()",
            Type::String => ".to_string()",
            _ => ".clone()",
        };
        let receiver = match self.name_binding(name) {
            Binding::Owned | Binding::Borrowed => arena.text(name_ident(name)),
            Binding::BorrowedBoxed => arena
                .text("(**")
                .append(arena.text(name_ident(name)))
                .append(arena.text(")")),
        };
        receiver.append(arena.text(method))
    }

    /// The expression a value block produces. A result the block itself
    /// bound and owns moves out, since nothing after the block can read it.
    /// Anything else is copied or cloned.
    fn block_result<'a>(&self, arena: &'a Arena<'a>, block: &WriterValueBlock) -> Doc<'a> {
        let bound_here = block
            .lets
            .iter()
            .any(|let_| WriterName::Binding(let_.name) == block.result);
        if bound_here && self.name_binding(block.result) == Binding::Owned {
            arena.text(name_ident(block.result))
        } else {
            self.name_owned(arena, block.result)
        }
    }

    /// Collect the named types that `t` stores inline.
    fn inline_refs(t: &Type, out: &mut BTreeSet<TypeName>) {
        match t {
            Type::Named { name, .. } => {
                out.insert(name.clone());
            }
            Type::Option(inner) => Self::inline_refs(inner, out),
            Type::Tuple(elements) => {
                for element in elements {
                    Self::inline_refs(element, out);
                }
            }
            _ => {}
        }
    }

    /// The field positions that need `Box` for every declared type to be
    /// finitely sized, as `(declaring type, referenced type)` pairs.
    fn compute_boxed_edges(registry: &TypeRegistry) -> HashSet<(TypeName, TypeName)> {
        let mut graph = DependencyGraph::new();
        for (name, fields) in registry.records() {
            let mut refs = BTreeSet::new();
            for field in fields {
                Self::inline_refs(&field.typ, &mut refs);
            }
            graph.set_dependencies(name.clone(), refs);
        }
        for (name, variants) in registry.enums() {
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

    /// Whether a field of `owner` declaring type `t` carries a `Box`, which is
    /// the case when the field stores an inline reference to a type `owner`
    /// boxes. Mirrors `inline_refs`: descend `Option` and tuples, stop at
    /// `Array`.
    fn field_type_is_boxed(&self, t: &Type, owner: &str) -> bool {
        match t {
            Type::Named { name, .. } => self
                .boxed_edges
                .iter()
                .any(|(o, target)| o.as_str() == owner && target == name),
            Type::Option(inner) => self.field_type_is_boxed(inner, owner),
            Type::Tuple(elements) => elements
                .iter()
                .any(|element| self.field_type_is_boxed(element, owner)),
            _ => false,
        }
    }

    /// Transpile a field type, inserting `Box` where the field needs it.
    fn transpile_field_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        t: &'a Type,
        owner: &str,
    ) -> Doc<'a> {
        if self.field_type_is_boxed(t, owner) {
            arena
                .text("Box<")
                .append(self.transpile_type(arena, t))
                .append(arena.text(">"))
        } else {
            self.transpile_type(arena, t)
        }
    }

    /// Transpile a value stored into a field of `owner`, adding the `Box`
    /// wrapping the field's declared type expects. The IR is well typed, so the
    /// value's own type is that declared type.
    fn transpile_field_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        owner: &str,
        value: WriterName,
    ) -> Doc<'a> {
        if self.field_type_is_boxed(self.name_type(value), owner) {
            arena
                .text("Box::new(")
                .append(self.name_owned(arena, value))
                .append(arena.text(")"))
        } else {
            self.name_owned(arena, value)
        }
    }

    /// Whether reads of `field` off `record` have to strip a `Box`.
    fn field_access_is_boxed(&self, record: WriterName, field: &FieldName) -> bool {
        let Some(ResolvedType::Record { name, fields, .. }) =
            self.registry.resolve(self.name_type(record))
        else {
            unreachable!("field access objects resolve to a record");
        };
        let field_type = fields
            .iter()
            .find(|f| f.name == *field)
            .map(|f| &f.typ)
            .expect("field access fields exist on the record");
        self.field_type_is_boxed(field_type, name.as_str())
    }

    /// The pattern of an enum arm, and the bindings it introduces recorded.
    /// Matching is on a reference, so a binding is a reference into the
    /// subject, one `Box` deeper when the field carries indirection.
    fn enum_arm_pattern(
        &mut self,
        variants: &[EnumVariant],
        arm_pattern: &EnumPattern,
        bindings: &[(FieldName, IrBinder)],
    ) -> String {
        let EnumPattern::Variant {
            type_name,
            variant_name,
        } = arm_pattern;
        for (_, binder) in bindings {
            let binding = if self.field_type_is_boxed(&binder.typ, type_name.as_str()) {
                Binding::BorrowedBoxed
            } else {
                Binding::Borrowed
            };
            self.names.insert(
                WriterName::Binder(binder.var),
                (binding, binder.typ.clone()),
            );
        }
        if bindings.is_empty() {
            // Check if this variant has fields by looking at the type
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
                    format!(
                        "{}: {}",
                        Self::escape_ident(field.as_str()),
                        name_ident(WriterName::Binder(binder.var))
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
    fn subject_variants(&self, subject: WriterName) -> Vec<EnumVariant> {
        let Some(ResolvedType::Enum { variants, .. }) =
            self.registry.resolve(self.name_type(subject))
        else {
            unreachable!("Enum match subject must have Named enum type")
        };
        variants.to_vec()
    }

    /// Whether `t` is one of hop's scalar types.
    fn is_scalar(t: &Type) -> bool {
        match t {
            Type::Bool | Type::Int | Type::Float => true,
            Type::String
            | Type::Html
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Named { .. }
            | Type::Option(_) => false,
        }
    }

    /// Transpile a type for use in function parameters (uses references without explicit lifetimes)
    fn transpile_param_type<'a>(&mut self, arena: &'a Arena<'a>, t: &'a Type) -> Doc<'a> {
        match t {
            Type::Bool => arena.text("bool"),
            Type::String => arena.text("&str"),
            Type::Float => arena.text("f64"),
            Type::Int => arena.text("i32"),
            Type::Html => {
                self.needs_html = true;
                arena.text("&Html")
            }
            Type::Array(elem) => arena
                .text("&[")
                .append(self.transpile_type(arena, elem))
                .append(arena.text("]")),
            Type::Option(inner) => arena
                .text("&Option<")
                .append(self.transpile_type(arena, inner))
                .append(arena.text(">")),
            Type::Tuple(_) => arena.text("&").append(self.transpile_type(arena, t)),
            Type::Named { name, .. } => arena.text("&").append(arena.text(name.as_str())),
        }
    }

    /// The parameters of a function, recorded as owned scalars and borrowed
    /// everything else.
    fn transpile_function_params<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parameters: &'a [crate::ir::ir_parameter::IrParameter],
    ) -> Vec<Doc<'a>> {
        parameters
            .iter()
            .map(|param| {
                let binding = if Self::is_scalar(&param.typ) {
                    Binding::Owned
                } else {
                    Binding::Borrowed
                };
                self.names
                    .insert(WriterName::Binder(param.var), (binding, param.typ.clone()));
                arena
                    .text(name_ident(WriterName::Binder(param.var)))
                    .append(arena.text(": "))
                    .append(self.transpile_param_type(arena, &param.typ))
            })
            .collect()
    }

    /// The arguments of a call: scalars by value, everything else by
    /// reference.
    fn transpile_arguments<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        args: &'a [WriterName],
    ) -> Vec<Doc<'a>> {
        args.iter()
            .map(|arg| {
                if Self::is_scalar(self.name_type(*arg)) {
                    self.name_owned(arena, *arg)
                } else {
                    self.name_ref(arena, *arg)
                }
            })
            .collect()
    }

    fn transpile_page_struct<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        page: &'a WriterPageDeclaration,
    ) -> Doc<'a> {
        let struct_name = page.name.as_str();
        if page.parameters.is_empty() {
            arena
                .text("pub struct ")
                .append(arena.text(struct_name))
                .append(arena.text(" {}"))
        } else {
            let fields = arena.intersperse(
                page.parameters.iter().map(|param| {
                    arena
                        .text("pub ")
                        .append(arena.text(Self::escape_ident(param.name().as_str())))
                        .append(arena.text(": "))
                        .append(self.transpile_type(arena, &param.typ))
                        .append(arena.text(","))
                }),
                arena.hardline(),
            );
            arena
                .text("pub struct ")
                .append(arena.text(struct_name))
                .append(arena.text(" {"))
                .append(arena.hardline().append(fields).nest(4))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
    }

    /// A value block as the body of a match arm: the result alone when the
    /// block binds nothing, otherwise a block expression.
    fn transpile_arm_block<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        block: &'a WriterValueBlock,
    ) -> Doc<'a> {
        if block.lets.is_empty() {
            return self.block_result(arena, block);
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
    fn registry(&self) -> &TypeRegistry {
        &self.registry
    }

    fn transpile_module(&mut self, module: &WriterModule, registry: &TypeRegistry) -> String {
        // Reset tracking flags for this module
        self.needs_escape_html = false;
        self.needs_html = false;
        self.boxed_edges = Self::compute_boxed_edges(registry);
        self.registry = registry.clone();
        self.names.clear();

        let arena = &Arena::new();

        let pages = &module.pages;

        let mut result = arena.nil();

        // Add enum type definitions
        for (enum_name, variants) in registry.enums() {
            result = result
                .append(arena.text("#[derive(Clone, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub enum "))
                .append(arena.text(enum_name.as_str()))
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

        // Add record struct definitions
        for (record_name, fields) in registry.records() {
            result = result
                .append(arena.text("#[derive(Clone, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub struct "))
                .append(arena.text(record_name.as_str()))
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

        // Add page struct definitions
        for page in pages {
            result = result
                .append(self.transpile_page_struct(arena, page))
                .append(arena.line())
                .append(arena.line());
        }

        // Transpile each function declaration
        for function in &module.functions {
            result = result
                .append(self.transpile_function_def(arena, function))
                .append(arena.line());
        }

        // Transpile each page's View impl
        for (i, page) in pages.iter().enumerate() {
            result = result.append(self.transpile_page(arena, &page.name, page));
            if i < pages.len() - 1 {
                result = result.append(arena.line());
            }
        }

        // Prepend write_escaped_html helper function if needed (after transpilation determined it's used)
        if self.needs_escape_html {
            let escape_fn = arena
                .nil()
                .append(arena.text("fn write_escaped_html(s: &str, output: &mut String) {"))
                .append(arena.line())
                .append(arena.text("    for c in s.chars() {"))
                .append(arena.line())
                .append(arena.text("        match c {"))
                .append(arena.line())
                .append(arena.text("            '&' => output.push_str(\"&amp;\"),"))
                .append(arena.line())
                .append(arena.text("            '<' => output.push_str(\"&lt;\"),"))
                .append(arena.line())
                .append(arena.text("            '>' => output.push_str(\"&gt;\"),"))
                .append(arena.line())
                .append(arena.text("            '\"' => output.push_str(\"&quot;\"),"))
                .append(arena.line())
                .append(arena.text("            _ => output.push(c),"))
                .append(arena.line())
                .append(arena.text("        }"))
                .append(arena.line())
                .append(arena.text("    }"))
                .append(arena.line())
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
            result = escape_fn.append(result);
        }

        // Prepend Html type definition if needed (after transpilation determined it's used)
        if self.needs_html {
            let fragment = arena
                .nil()
                .append(arena.text("#[derive(Clone, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub struct Html(String);"))
                .append(arena.line())
                .append(arena.line());
            result = fragment.append(result);
        }

        // Prepend View trait definition
        if !module.pages.is_empty() {
            let view_trait = arena
                .text("pub trait View {")
                .append(
                    arena
                        .nil()
                        .append(arena.line())
                        .append(arena.text("fn render(self) -> String;"))
                        .append(arena.line())
                        .append(arena.text("fn write(self, output: &mut String);"))
                        .append(arena.line())
                        .nest(4),
                )
                .append(arena.text("}"))
                .append(arena.line())
                .append(arena.line());
            result = view_trait.append(result);
        }

        // Prepend warning header (must be last prepend to appear first in output)
        let warning = arena
            .text("// Code generated by the hop compiler. DO NOT EDIT.")
            .append(arena.line())
            .append(arena.text("#![cfg_attr(rustfmt, rustfmt_skip)]"))
            .append(arena.line())
            .append(arena.text("#![allow(unused_parens, dead_code, clippy::all)]"))
            .append(arena.line())
            .append(arena.line());
        result = warning.append(result);

        // Render to string
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
        let struct_name = name.as_str();

        // render method body
        let mut write_body = arena.nil();

        // Destructure self into local variables
        if !page.parameters.is_empty() {
            let field_names = arena.intersperse(
                page.parameters.iter().map(|param| {
                    arena
                        .text(Self::escape_ident(param.name().as_str()))
                        .append(arena.text(": "))
                        .append(arena.text(name_ident(WriterName::Binder(param.var))))
                }),
                arena.text(", "),
            );
            for param in &page.parameters {
                self.names.insert(
                    WriterName::Binder(param.var),
                    (Binding::Owned, param.typ.clone()),
                );
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
            .text("fn write(self, output: &mut String) {")
            .append(arena.hardline().append(write_body).nest(4))
            .append(arena.hardline())
            .append(arena.text("}"));

        let render_fn = arena
            .text("fn render(self) -> String {")
            .append(
                arena
                    .nil()
                    .append(arena.hardline())
                    .append(arena.text("let mut output: String = String::new();"))
                    .append(arena.hardline())
                    .append(arena.text("self.write(&mut output);"))
                    .append(arena.hardline())
                    .append(arena.text("output"))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"));

        arena
            .text("impl View for ")
            .append(arena.text(struct_name))
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
        args: &'a [WriterName],
    ) -> Doc<'a> {
        let mut all_args: Vec<Doc<'a>> = vec![arena.text("output")];
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
            .append(arena.text(function_ident(&function.function)));

        match &function.body {
            WriterFunctionBody::Writes(statements) => {
                let mut params: Vec<Doc<'a>> = vec![arena.text("output: &mut String")];
                params.extend(self.transpile_function_params(arena, &function.parameters));

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
                let params = self.transpile_function_params(arena, &function.parameters);

                let result = result
                    .append(arena.text("("))
                    .append(arena.intersperse(params, arena.text(", ")))
                    .append(arena.text(") -> "))
                    .append(self.transpile_type(arena, &function.return_type))
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
        args: &'a [WriterName],
    ) -> Doc<'a> {
        let all_args = self.transpile_arguments(arena, args);
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
        name: WriterName,
    ) -> Doc<'a> {
        self.needs_escape_html = true;
        arena
            .text("write_escaped_html(")
            .append(self.name_ref(arena, name))
            .append(arena.text(", output);"))
    }

    fn transpile_write_html_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: WriterName,
    ) -> Doc<'a> {
        arena
            .text("output.push_str(&")
            .append(arena.text(name_ident(name)))
            .append(arena.text(".0"))
            .append(arena.text(");"))
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrBinder>,
        source: &'a WriterForSource,
        body: &'a [WriterStmt],
    ) -> Doc<'a> {
        let var_name = match var {
            Some(binder) => name_ident(WriterName::Binder(binder.var)),
            None => "_".to_string(),
        };

        let doc = match source {
            WriterForSource::Array(array) => arena
                .text("for ")
                .append(arena.text(var_name))
                .append(arena.text(" in "))
                .append(arena.text(name_ident(*array)))
                .append(arena.text(".iter() {")),
            WriterForSource::RangeInclusive { start, end } => arena
                .text("for ")
                .append(arena.text(var_name))
                .append(arena.text(" in "))
                .append(self.name_place(arena, *start))
                .append(arena.text("..="))
                .append(self.name_place(arena, *end))
                .append(arena.text(" {")),
        };

        if let Some(binder) = var {
            let binding = match source {
                WriterForSource::Array(_) => Binding::Borrowed,
                WriterForSource::RangeInclusive { .. } => Binding::Owned,
            };
            self.names.insert(
                WriterName::Binder(binder.var),
                (binding, binder.typ.clone()),
            );
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

    /// Bind the value, and record how. A value something else already holds
    /// is bound by reference, since nothing mutates it, and a use that wants
    /// it owned copies at that use instead of here. A scalar is bound by
    /// copy either way.
    ///
    /// A borrowed binding is annotated with the type a borrowed parameter
    /// has, so a reference to a `String` or a `Vec` coerces to its unsized
    /// view and every borrowed binding of a type has the same Rust type.
    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        let_: &'a WriterLet,
    ) -> Doc<'a> {
        let natural = self.natural_form(&let_.op);
        let scalar = Self::is_scalar(&let_.typ);
        let binding = match natural {
            NaturalForm::Temporary => Binding::Owned,
            NaturalForm::Reference | NaturalForm::Place if scalar => Binding::Owned,
            NaturalForm::Reference | NaturalForm::Place => Binding::Borrowed,
        };
        self.names
            .insert(WriterName::Binding(let_.name), (binding, let_.typ.clone()));
        let typ = match binding {
            Binding::Owned => self.transpile_type(arena, &let_.typ),
            Binding::Borrowed => self.transpile_param_type(arena, &let_.typ),
            Binding::BorrowedBoxed => unreachable!("a let never binds through a Box"),
        };
        let value = self.transpile_op(arena, &let_.op, &let_.typ);
        let value = match natural {
            NaturalForm::Temporary => value,
            NaturalForm::Reference if scalar => arena.text("*").append(value),
            NaturalForm::Reference | NaturalForm::Place if scalar => value,
            NaturalForm::Reference => value,
            NaturalForm::Place => arena.text("&").append(value),
        };
        arena
            .text("let ")
            .append(arena.text(name_ident(WriterName::Binding(let_.name))))
            .append(arena.text(": "))
            .append(typ)
            .append(arena.text(" = "))
            .append(value)
            .append(arena.text(";"))
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<WriterName, Vec<WriterStmt>>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let if_doc = arena
                    .text("if ")
                    .append(self.name_place(arena, **subject))
                    .append(arena.text(" {"))
                    .append(
                        arena
                            .hardline()
                            .append(self.transpile_statements(arena, true_body))
                            .nest(4),
                    )
                    .append(arena.hardline())
                    .append(arena.text("}"));
                // An empty false arm emits no `else` branch.
                if false_body.is_empty() {
                    if_doc
                } else {
                    if_doc
                        .append(arena.text(" else {"))
                        .append(
                            arena
                                .hardline()
                                .append(self.transpile_statements(arena, false_body))
                                .nest(4),
                        )
                        .append(arena.hardline())
                        .append(arena.text("}"))
                }
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(binder) => format!("Some({})", name_ident(WriterName::Binder(binder.var))),
                    None => "Some(_)".to_string(),
                };
                if let Some(binder) = some_arm_binding {
                    self.names.insert(
                        WriterName::Binder(binder.var),
                        (Binding::Borrowed, binder.typ.clone()),
                    );
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
                    .append(self.name_ref(arena, **subject))
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
            Match::Enum { subject, arms } => {
                let variants = self.subject_variants(**subject);
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
                    .append(self.name_ref(arena, **subject))
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

    /// The lets as statements, then the result as the tail expression.
    fn transpile_value_block<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        block: &'a WriterValueBlock,
    ) -> Doc<'a> {
        let mut docs: Vec<Doc<'a>> = Vec::new();
        for let_ in &block.lets {
            docs.push(self.transpile_let_statement(arena, let_));
        }
        docs.push(self.block_result(arena, block));
        arena.intersperse(docs, arena.hardline())
    }

    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("bool")
    }

    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("String")
    }

    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("f64")
    }

    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        arena.text("i32")
    }

    fn transpile_html_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        self.needs_html = true;
        arena.text("Html")
    }

    fn transpile_array_type<'a>(&mut self, arena: &'a Arena<'a>, element_type: &Type) -> Doc<'a> {
        arena
            .text("Vec<")
            .append(self.transpile_type(arena, element_type))
            .append(arena.text(">"))
    }

    fn transpile_tuple_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_types: &[Type],
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(
                arena.intersperse(
                    element_types
                        .iter()
                        .map(|element| self.transpile_type(arena, element))
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

    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &Type) -> Doc<'a> {
        arena
            .text("Option<")
            .append(self.transpile_type(arena, inner_type))
            .append(arena.text(">"))
    }

    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        arena.text(name.to_string())
    }

    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a> {
        arena.text(name.to_string())
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record: WriterName,
        field: &'a FieldName,
    ) -> Doc<'a> {
        let boxed = self.field_access_is_boxed(record, field);
        let access = arena
            .text(name_ident(record))
            .append(arena.text("."))
            .append(arena.text(Self::escape_ident(field.as_str())));

        if boxed {
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

    /// The fragment body renders into its own `output` buffer, so it is
    /// emitted as a block expression that shadows `output`.
    fn transpile_html<'a>(&mut self, arena: &'a Arena<'a>, body: &'a [WriterStmt]) -> Doc<'a> {
        self.needs_html = true;
        arena
            .text("{")
            .append(
                arena
                    .nil()
                    .append(arena.hardline())
                    .append(arena.text("let mut buf: String = String::new();"))
                    .append(arena.hardline())
                    .append(arena.text("let mut output: &mut String = &mut buf;"))
                    .append(arena.hardline())
                    .append(self.transpile_statements(arena, body))
                    .append(arena.hardline())
                    .append(arena.text("Html(buf)"))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"))
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
        elements: &'a [WriterName],
        elem_type: &'a Type,
    ) -> Doc<'a> {
        if elements.is_empty() {
            arena
                .text("Vec::<")
                .append(self.transpile_type(arena, elem_type))
                .append(arena.text(">::new()"))
        } else {
            let items: Vec<Doc<'a>> = elements
                .iter()
                .map(|e| self.name_owned(arena, *e))
                .collect();
            arena
                .text("vec![")
                .append(arena.intersperse(items, arena.text(", ")))
                .append(arena.text("]"))
        }
    }

    fn transpile_tuple_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [WriterName],
        _element_types: &'a [Type],
    ) -> Doc<'a> {
        let items: Vec<Doc<'a>> = elements
            .iter()
            .map(|e| self.name_owned(arena, *e))
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
        tuple: WriterName,
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
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        // Strings compare by reference. The operands arrive as a mix of
        // `String`, `str` and `&str`, and `str` compares with neither `&str`
        // nor itself behind a reference, but `&_` compares across all of them.
        self.name_ref(arena, left)
            .append(arena.text(" == "))
            .append(self.name_ref(arena, right))
    }

    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" == "))
            .append(self.name_place(arena, right))
    }

    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" == "))
            .append(self.name_place(arena, right))
    }

    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" == "))
            .append(self.name_place(arena, right))
    }

    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" < "))
            .append(self.name_place(arena, right))
    }

    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" < "))
            .append(self.name_place(arena, right))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" <= "))
            .append(self.name_place(arena, right))
    }

    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" <= "))
            .append(self.name_place(arena, right))
    }

    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: WriterName) -> Doc<'a> {
        arena.text("!").append(self.name_place(arena, operand))
    }

    fn transpile_int_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: WriterName) -> Doc<'a> {
        arena
            .text(name_ident(operand))
            .append(arena.text(".wrapping_neg()"))
    }

    fn transpile_float_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: WriterName,
    ) -> Doc<'a> {
        arena.text("-").append(self.name_place(arena, operand))
    }

    fn transpile_string_concat<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parts: &'a [WriterName],
    ) -> Doc<'a> {
        if parts.is_empty() {
            return arena.text("String::new()");
        }
        let mut body = arena
            .nil()
            .append(arena.hardline())
            .append(arena.text("let mut s: String = String::new();"));
        for part in parts {
            body = body
                .append(arena.hardline())
                .append(arena.text("s.push_str("))
                .append(self.name_ref(arena, *part))
                .append(arena.text(");"));
        }
        arena
            .text("{")
            .append(
                body.append(arena.hardline())
                    .append(arena.text("s"))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("}"))
    }

    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(".wrapping_add("))
            .append(self.name_place(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" + "))
            .append(self.name_place(arena, right))
    }

    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(".wrapping_sub("))
            .append(self.name_place(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" - "))
            .append(self.name_place(arena, right))
    }

    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        arena
            .text(name_ident(left))
            .append(arena.text(".wrapping_mul("))
            .append(self.name_place(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: WriterName,
        right: WriterName,
    ) -> Doc<'a> {
        self.name_place(arena, left)
            .append(arena.text(" * "))
            .append(self.name_place(arena, right))
    }

    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, WriterName)],
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
        fields: &'a [(FieldName, WriterName)],
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
        value: Option<WriterName>,
        inner_type: &'a Type,
    ) -> Doc<'a> {
        match value {
            Some(inner) => arena
                .text("Some(")
                .append(self.name_owned(arena, inner))
                .append(arena.text(")")),
            None => arena
                .text("None::<")
                .append(self.transpile_type(arena, inner_type))
                .append(arena.text(">")),
        }
    }

    fn transpile_match_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<WriterName, WriterValueBlock>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject_doc = self.name_place(arena, **subject);
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
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(binder) => format!("Some({})", name_ident(WriterName::Binder(binder.var))),
                    None => "Some(_)".to_string(),
                };
                if let Some(binder) = some_arm_binding {
                    self.names.insert(
                        WriterName::Binder(binder.var),
                        (Binding::Borrowed, binder.typ.clone()),
                    );
                }
                let subject_doc = self.name_ref(arena, **subject);
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
            Match::Enum { subject, arms } => {
                let variants = self.subject_variants(**subject);
                let subject_doc = self.name_ref(arena, **subject);
                let arm_docs: Vec<Doc<'a>> = arms
                    .iter()
                    .map(|arm: &'a EnumMatchArm<WriterValueBlock>| {
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

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: WriterName) -> Doc<'a> {
        arena
            .text(name_ident(array))
            .append(arena.text(".len() as i32"))
    }

    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: WriterName) -> Doc<'a> {
        arena
            .text(name_ident(array))
            .append(arena.text(".is_empty()"))
    }

    fn transpile_string_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        string: WriterName,
    ) -> Doc<'a> {
        arena
            .text(name_ident(string))
            .append(arena.text(".is_empty()"))
    }

    fn transpile_option_is_some<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: WriterName,
    ) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".is_some()"))
    }

    fn transpile_option_is_none<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: WriterName,
    ) -> Doc<'a> {
        arena
            .text(name_ident(option))
            .append(arena.text(".is_none()"))
    }

    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: WriterName) -> Doc<'a> {
        arena
            .text(name_ident(value))
            .append(arena.text(".to_string()"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: WriterName) -> Doc<'a> {
        self.name_place(arena, value).append(arena.text(" as i32"))
    }

    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: WriterName) -> Doc<'a> {
        self.name_place(arena, value).append(arena.text(" as f64"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::ir::flat_to_writer::flat_to_writer;
    use crate::ir::pure_to_flat::pure_to_flat;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
    use crate::program::Program;
    use crate::root_contained_file_path::RootContainedFilePath;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    /// Compile a module written in source, without optimization, and
    /// transpile it to Rust.
    fn check(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut program = Program::new();
        program.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = program.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let pure = orchestrate_pure(
            program.typed_modules(),
            OrchestrateOptions {
                ..Default::default()
            },
        );
        let module = flat_to_writer(pure_to_flat(pure), None);
        let output = RustTranspiler::new().transpile_module(&module, program.type_registry());
        expected.assert_eq(&output);
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
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Holder {
                    pub nothing: (),
                }

                pub struct Test {
                    pub unit: (),
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { unit: b_0 } = self;
                        let v_0: () = ();
                        let v_1: Holder = Holder { nothing: v_0.clone() };
                        let v_3: &() = &v_1.nothing;
                        let v_4: Vec<()> = vec![b_0.clone(), v_3.clone()];
                        let v_5: i32 = v_4.len() as i32;
                        let v_6: String = v_5.to_string();
                        write_escaped_html(&v_6, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                pub struct Second {
                    pub title: String,
                }

                impl View for First {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        output.push_str("<h1>First</h1>");
                    }
                }

                impl View for Second {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Second { title: b_0 } = self;
                        write_escaped_html(&b_0, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                pub struct Test {
                    pub show: bool,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { show: b_0 } = self;
                        if b_0 {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_0: i32 = 1_i32;
                        let v_1: i32 = 3_i32;
                        for b_0 in v_0..=v_1 {
                            let v_3: String = b_0.to_string();
                            write_escaped_html(&v_3, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_0: &str = "x";
                        let v_1: Option<String> = Some(v_0.to_string());
                        match &v_1 {
                            Some(b_1) => {
                                output.push_str("some: ");
                                write_escaped_html(b_1, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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
                    pub opt: Option<String>,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { opt: b_0 } = self;
                        match &b_0 {
                            Some(b_2) => {
                                output.push_str("some: ");
                                write_escaped_html(b_2, output);
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
    fn recursive_record_boxes_recursive_field() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Node {
                    pub value: i32,
                    pub next: Box<Option<Node>>,
                }

                pub struct Test {
                    pub node: Node,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { node: b_0 } = self;
                        let v_1: i32 = b_0.value;
                        let v_2: String = v_1.to_string();
                        write_escaped_html(&v_2, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_boxes_recursive_field() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                #[derive(Clone, Debug)]
                pub enum IntList {
                    Cons { head: i32, tail: Box<IntList> },
                    Nil,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        output.push_str("hello");
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_literal_boxes_field_values() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Node {
                    pub value: i32,
                    pub next: Box<Option<Node>>,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_0: i32 = 2_i32;
                        let v_1: i32 = 1_i32;
                        let v_2: Option<Node> = None::<Node>;
                        let v_3: Node = Node { value: v_1, next: Box::new(v_2.clone()) };
                        let v_4: Option<Node> = Some(v_3.clone());
                        let v_5: Node = Node { value: v_0, next: Box::new(v_4.clone()) };
                        let v_6: i32 = v_5.value;
                        let v_7: String = v_6.to_string();
                        write_escaped_html(&v_7, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn matching_a_boxed_enum_field_derefs_past_the_box() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub enum Expr {
                    Literal { value: String },
                    Neg { inner: Box<Expr> },
                }

                pub struct Test {
                    pub e: Expr,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { e: b_0 } = self;
                        match &b_0 {
                            Expr::Literal { .. } => {
                                output.push_str("lit");
                            }
                            Expr::Neg { inner: b_2 } => {
                                match &**b_2 {
                                    Expr::Literal { value: b_4 } => {
                                        write_escaped_html(b_4, output);
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
    fn matching_a_boxed_option_field_derefs_past_the_box() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Node {
                    pub value: String,
                    pub next: Box<Option<Node>>,
                }

                pub struct Test {
                    pub node: Node,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { node: b_0 } = self;
                        let v_1: &Option<Node> = &*b_0.next;
                        match v_1 {
                            Some(b_2) => {
                                let v_3: &str = &b_2.value;
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
    fn matching_an_option_bound_from_a_boxed_enum_field() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                #[derive(Clone, Debug)]
                pub enum Chain {
                    Link { next: Box<Option<Chain>> },
                    End,
                }

                pub struct Test {
                    pub c: Chain,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { c: b_0 } = self;
                        match &b_0 {
                            Chain::Link { next: b_2 } => {
                                match &**b_2 {
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
    fn recursive_enum_literal_boxes_field_values() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub enum IntList {
                    Cons { head: i32, tail: Box<IntList> },
                    Nil,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_0: i32 = 1_i32;
                        let v_1: IntList = IntList::Nil;
                        let v_2: IntList = IntList::Cons { head: v_0, tail: Box::new(v_1.clone()) };
                        match &v_2 {
                            IntList::Cons { head: b_2, .. } => {
                                let v_4: String = b_2.to_string();
                                write_escaped_html(&v_4, output);
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
    fn mutually_recursive_records_boxes_in_both_directions() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                #[derive(Clone, Debug)]
                pub struct A {
                    pub b: Box<B>,
                }

                #[derive(Clone, Debug)]
                pub struct B {
                    pub a: Box<Option<A>>,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_0: Option<A> = None::<A>;
                        let v_1: B = B { a: Box::new(v_0.clone()) };
                        let v_2: A = A { b: Box::new(v_1.clone()) };
                        let v_3: Option<A> = Some(v_2.clone());
                        let v_4: B = B { a: Box::new(v_3.clone()) };
                        let v_5: &Option<A> = &*v_4.a;
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub enum TimeAgo {
                    JustNow,
                    MinutesAgo { count: i32 },
                    HoursAgo { count: i32 },
                }

                pub struct Test {
                    pub time: TimeAgo,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { time: b_0 } = self;
                        let v_20: String = match &b_0 {
                            TimeAgo::JustNow => {
                                let v_1: &str = "just now";
                                v_1.to_string()
                            },
                            TimeAgo::MinutesAgo { count: b_2 } => {
                                let v_3: i32 = 1_i32;
                                let v_4: bool = *b_2 == v_3;
                                let v_10: String = match v_4 {
                                    true => {
                                        let v_5: &str = "1 minute ago";
                                        v_5.to_string()
                                    },
                                    false => {
                                        let v_7: String = b_2.to_string();
                                        let v_8: &str = " minutes ago";
                                        let v_9: String = {
                                            let mut s: String = String::new();
                                            s.push_str(&v_7);
                                            s.push_str(v_8);
                                            s
                                        };
                                        v_9
                                    },
                                };
                                v_10
                            },
                            TimeAgo::HoursAgo { count: b_4 } => {
                                let v_12: i32 = 1_i32;
                                let v_13: bool = *b_4 == v_12;
                                let v_19: String = match v_13 {
                                    true => {
                                        let v_14: &str = "1 hour ago";
                                        v_14.to_string()
                                    },
                                    false => {
                                        let v_16: String = b_4.to_string();
                                        let v_17: &str = " hours ago";
                                        let v_18: String = {
                                            let mut s: String = String::new();
                                            s.push_str(&v_16);
                                            s.push_str(v_17);
                                            s
                                        };
                                        v_18
                                    },
                                };
                                v_19
                            },
                        };
                        write_escaped_html(&v_20, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_0: &str = "world";
                        let v_1: Option<String> = Some(v_0.to_string());
                        let v_6: String = match &v_1 {
                            Some(b_1) => {
                                let v_2: &str = "hello ";
                                let v_4: String = {
                                    let mut s: String = String::new();
                                    s.push_str(v_2);
                                    s.push_str(b_1);
                                    s
                                };
                                v_4
                            },
                            None => {
                                let v_5: &str = "nobody";
                                v_5.to_string()
                            },
                        };
                        write_escaped_html(&v_6, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                #[derive(Clone, Debug)]
                pub enum Color {
                    Red,
                    Green,
                    Blue,
                }

                pub struct Test {}

                fn render_badge_0(output: &mut String, b_0: &Color) {
                    match b_0 {
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

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_8: Color = Color::Green;
                        render_badge_0(output, &v_8);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn borrowed_params_are_read_through_the_reference() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Post {
                    pub title: String,
                    pub views: i32,
                }

                pub struct Test {
                    pub post: Post,
                    pub tags: Vec<String>,
                }

                fn render_card_0(output: &mut String, b_2: &Post, b_3: &[String]) {
                    let v_1: &str = &b_2.title;
                    let v_4: i32 = b_2.views;
                    let v_5: String = v_4.to_string();
                    let v_8: i32 = b_3.len() as i32;
                    let v_9: String = v_8.to_string();
                    write_escaped_html(v_1, output);
                    write_escaped_html(&v_5, output);
                    write_escaped_html(&v_9, output);
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { post: b_0, tags: b_1 } = self;
                        render_card_0(output, &b_0, &b_1);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn borrowed_binding_is_cloned_where_a_value_is_stored() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Tag {
                    pub name: String,
                }

                #[derive(Clone, Debug)]
                pub struct Wrap {
                    pub tag: Tag,
                }

                pub struct Test {
                    pub tag: Tag,
                }

                fn render_show_0(output: &mut String, b_1: &Tag) {
                    let v_1: Wrap = Wrap { tag: b_1.clone() };
                    let v_2: &Tag = &v_1.tag;
                    let v_3: &str = &v_2.name;
                    write_escaped_html(v_3, output);
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { tag: b_0 } = self;
                        render_show_0(output, &b_0);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn let_binding_a_place_borrows_it() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                #[derive(Clone, Debug)]
                pub struct Post {
                    pub title: String,
                    pub body: String,
                }

                pub struct Test {
                    pub post: Post,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { post: b_0 } = self;
                        let v_1: &str = &b_0.title;
                        write_escaped_html(v_1, output);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn string_equality_compares_references() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                pub struct Test {
                    pub role: String,
                }

                fn render_role_0(output: &mut String, b_1: &str) {
                    let v_1: &str = "admin";
                    let v_2: bool = b_1 == v_1;
                    if v_2 {
                        output.push_str("yes");
                    } else {
                        output.push_str("no");
                    }
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { role: b_0 } = self;
                        render_role_0(output, &b_0);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn function_with_option_param_takes_a_reference() {
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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
                    pub label: Option<String>,
                }

                fn render_label_0(output: &mut String, b_1: &Option<String>) {
                    match b_1 {
                        Some(b_3) => {
                            write_escaped_html(b_3, output);
                        }
                        None => {
                            output.push_str("none");
                        }
                    }
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let Test { label: b_0 } = self;
                        render_label_0(output, &b_0);
                    }
                }
            "#]],
        );
    }

    #[test]
    fn transpiles_a_fragment_read_twice_as_a_rust_block() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    let frag = <b>hi</b>;
                    <>{frag}{frag}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                #[derive(Clone, Debug)]
                pub struct Html(String);

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_2: Html = {
                            let mut buf: String = String::new();
                            let mut output: &mut String = &mut buf;
                            output.push_str("<b>hi</b>");
                            Html(buf)
                        };
                        output.push_str(&v_2.0);
                        output.push_str(&v_2.0);
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
                    <>{x}{x}</>
                  }
                }
            "#},
            expect![[r#"
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
                }

                #[derive(Clone, Debug)]
                pub struct Html(String);

                pub struct Test {}

                fn render_frag_0(output: &mut String) {
                    output.push_str("<b>hi</b>");
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_3: Html = {
                            let mut buf: String = String::new();
                            let mut output: &mut String = &mut buf;
                            render_frag_0(output);
                            Html(buf)
                        };
                        output.push_str(&v_3.0);
                        output.push_str(&v_3.0);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                fn render_format_price_0(b_0: i32) -> i32 {
                    b_0
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_1: i32 = 5_i32;
                        let v_2: i32 = render_format_price_0(v_1);
                        let v_3: String = v_2.to_string();
                        write_escaped_html(&v_3, output);
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
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                    fn write(self, output: &mut String);
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

                fn render_foo_0(b_1: i32) -> i32 {
                    let v_1: i32 = 10_i32;
                    let v_2: i32 = b_1.wrapping_add(v_1);
                    v_2
                }

                impl View for Test {
                    fn render(self) -> String {
                        let mut output: String = String::new();
                        self.write(&mut output);
                        output
                    }

                    fn write(self, output: &mut String) {
                        let v_3: i32 = 0_i32;
                        let v_4: i32 = -7_i32;
                        let v_5: i32 = render_foo_0(v_4);
                        let v_12: i32 = 10_i32;
                        let v_13: i32 = render_foo_0(v_12);
                        let v_14: String = v_13.to_string();
                        output.push_str("<div>");
                        for b_0 in v_3..=v_5 {
                            let v_7: String = b_0.to_string();
                            write_escaped_html(&v_7, output);
                            output.push_str(",");
                        }
                        write_escaped_html(&v_14, output);
                        output.push_str("</div>");
                    }
                }
            "#]],
        );
    }
}
