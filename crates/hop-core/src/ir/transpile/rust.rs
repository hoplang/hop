use std::collections::{BTreeSet, HashSet};

use pretty::{Arena, DocAllocator};

use super::{Doc, Transpiler};
use crate::dependency_graph::DependencyGraph;
use crate::expr::patterns::{EnumPattern, Match};
use crate::expr::typing::r#type::{EnumVariant, Type};
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::ir::ir_module::{
    IrArgument, IrComponentDeclaration, IrExpr, IrForSource, IrModule, IrStatement, IrVar,
    IrViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// Names every variable in the generated code, derived from the IR's variable
/// identity rather than the source name.
///
/// Each binder within a declaration has a distinct `VarId`, so this is unique
/// per scope by construction: no hop identifier can shadow another, and none
/// can collide with a Rust keyword.
fn var_ident(var: &IrVar) -> String {
    format!("v_{}", var.id)
}

pub struct RustTranspiler {
    /// Tracks whether escape_html function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether Fragment type is used during transpilation
    needs_fragment: bool,
    /// Field positions carrying `Box` indirection.
    /// We box in both directions for mutually recursive types.
    boxed_edges: HashSet<(TypeName, TypeName)>,
    /// Registry used to resolve named type structure.
    registry: TypeRegistry,
}

/// How a field value converts between the IR representation of its type and
/// the boxed representation the field's declared type carries.
enum BoxConversion {
    /// The value itself is the boxed occurrence.
    /// Wrap in `Box::new` to store, dereference to read.
    Direct,
    /// The `Box` sits under `Option` layers.
    /// Map this closure over the value.
    Mapped(String),
}

impl RustTranspiler {
    pub fn new() -> Self {
        Self {
            needs_escape_html: false,
            needs_fragment: false,
            boxed_edges: HashSet::new(),
            registry: TypeRegistry::default(),
        }
    }

    /// Rebind pattern bindings, which are references into the matched value, to
    /// owned values of the type the IR expects. Each entry is the variable and
    /// the expression to bind it to.
    fn stmts_with_rebinds<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        rebinds: &[(String, String)],
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        let mut doc = arena.nil();
        for (var, value) in rebinds {
            doc = doc
                .append(arena.text(format!("let {var} = {value};")))
                .append(arena.hardline());
        }
        doc.append(self.transpile_statements(arena, body))
    }

    fn expr_with_rebinds<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        rebinds: &[(String, String)],
        body: &'a IrExpr,
    ) -> Doc<'a> {
        if rebinds.is_empty() {
            return self.transpile_expr_owned(arena, body);
        }
        let mut doc = arena.text("{ ");
        for (var, value) in rebinds {
            doc = doc.append(arena.text(format!("let {var} = {value}; ")));
        }
        doc.append(self.transpile_expr_owned(arena, body))
            .append(arena.text(" }"))
    }

    /// Render a match subject in head position, parenthesizing a non-variable
    /// subject so the parser does not read its braces as the match body.
    fn transpile_match_subject<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
    ) -> Doc<'a> {
        match subject {
            IrExpr::Var { .. } => self.transpile_expr(arena, subject),
            _ => arena
                .text("(")
                .append(self.transpile_expr(arena, subject))
                .append(arena.text(")")),
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

    fn transpile_expr_owned<'a>(&mut self, arena: &'a Arena<'a>, expr: &'a IrExpr) -> Doc<'a> {
        match expr {
            // Unboxing a field read already produces an owned value.
            IrExpr::FieldAccess { record, field, .. }
                if self.field_unboxing(record, field).is_some() =>
            {
                self.transpile_expr(arena, expr)
            }
            IrExpr::FieldAccess { .. } | IrExpr::Var { .. } => {
                let method = match expr.as_type() {
                    Type::Array(_) => ".to_vec()",
                    Type::String => ".to_string()",
                    _ => ".clone()",
                };
                self.transpile_expr(arena, expr).append(arena.text(method))
            }
            _ => self.transpile_expr(arena, expr),
        }
    }

    /// Collect the named types that `t` stores inline.
    fn inline_refs(t: &Type, out: &mut BTreeSet<TypeName>) {
        match t {
            Type::Named { name, .. } => {
                out.insert(name.clone());
            }
            Type::Option(inner) => Self::inline_refs(inner, out),
            _ => {}
        }
    }

    /// The field positions that need `Box` for every declared type to be
    /// finitely sized, as `(declaring type, referenced type)` pairs.
    fn compute_boxed_edges(module: &IrModule) -> HashSet<(TypeName, TypeName)> {
        let mut graph = DependencyGraph::new();
        for record in &module.records {
            let mut refs = BTreeSet::new();
            for (_, field_type, _) in &record.fields {
                Self::inline_refs(field_type, &mut refs);
            }
            graph.set_dependencies(record.name.clone(), refs);
        }
        for enum_def in &module.enums {
            let mut refs = BTreeSet::new();
            for variant in &enum_def.variants {
                for (_, field_type, _) in &variant.fields {
                    Self::inline_refs(field_type, &mut refs);
                }
            }
            graph.set_dependencies(enum_def.name.clone(), refs);
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

    /// Whether fields of `owner` box their inline references to `target`.
    fn boxes(&self, owner: &str, target: &TypeName) -> bool {
        self.boxed_edges
            .iter()
            .any(|(o, t)| o.as_str() == owner && t == target)
    }

    /// The conversion between values of `t` and the representation a field of
    /// `owner` declares, built around `leaf` as the innermost step. `None`
    /// when the two representations agree.
    fn conversion(&self, t: &Type, owner: &str, leaf: &str) -> Option<BoxConversion> {
        match t {
            Type::Named { name, .. } if self.boxes(owner, name) => Some(BoxConversion::Direct),
            Type::Option(inner) => self.conversion(inner, owner, leaf).map(|c| {
                BoxConversion::Mapped(match c {
                    BoxConversion::Direct => leaf.to_string(),
                    BoxConversion::Mapped(inner) => format!("|v| v.map({inner})"),
                })
            }),
            _ => None,
        }
    }

    /// The conversion adding the `Box` wrapping a field of `owner` expects
    /// when a value of type `t` is stored into it.
    fn boxing(&self, t: &Type, owner: &str) -> Option<BoxConversion> {
        self.conversion(t, owner, "Box::new")
    }

    /// The inverse of boxing, read a field back out.
    fn unboxing(&self, t: &Type, owner: &str) -> Option<BoxConversion> {
        self.conversion(t, owner, "|v| *v")
    }

    /// Transpile a field type, inserting `Box` where the field needs it.
    fn transpile_field_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        t: &'a Type,
        owner: &str,
    ) -> Doc<'a> {
        match t {
            Type::Named { name, .. } if self.boxes(owner, name) => arena
                .text("Box<")
                .append(arena.text(name.as_str()))
                .append(arena.text(">")),
            Type::Option(inner) if self.boxing(inner, owner).is_some() => arena
                .text("Option<")
                .append(self.transpile_field_type(arena, inner, owner))
                .append(arena.text(">")),
            _ => self.transpile_type(arena, t),
        }
    }

    /// Transpile a value stored into a field of `owner`, adding the `Box`
    /// wrapping the field's declared type expects. The IR is well typed, so the
    /// value's own type is that declared type.
    fn transpile_field_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        owner: &str,
        value: &'a IrExpr,
    ) -> Doc<'a> {
        match self.boxing(value.as_type(), owner) {
            Some(BoxConversion::Direct) => arena
                .text("Box::new(")
                .append(self.transpile_expr_owned(arena, value))
                .append(arena.text(")")),
            Some(BoxConversion::Mapped(mapper)) => self
                .transpile_expr_owned(arena, value)
                .append(arena.text(format!(".map({mapper})"))),
            None => self.transpile_expr_owned(arena, value),
        }
    }

    /// The conversion undoing the `Box` on reads of `field` off `object`.
    fn field_unboxing(&self, object: &IrExpr, field: &FieldName) -> Option<BoxConversion> {
        let Some(ResolvedType::Record { name, fields, .. }) =
            self.registry.resolve(object.as_type())
        else {
            unreachable!("field access objects resolve to a record");
        };
        let field_type = fields
            .iter()
            .find(|(f, _, _)| f == field)
            .map(|(_, t, _)| t)
            .expect("field access fields exist on the record");
        self.unboxing(field_type, name.as_str())
    }

    fn arm_rebind_value(
        &self,
        variants: &[EnumVariant],
        pattern: &EnumPattern,
        field: &FieldName,
        var: &IrVar,
    ) -> String {
        let var = var_ident(var);
        let EnumPattern::Variant {
            enum_name,
            variant_name,
        } = pattern;
        let field_type = variants
            .iter()
            .find(|v| v.name == *variant_name)
            .and_then(|v| v.fields.iter().find(|(f, _, _)| f == field))
            .map(|(_, t, _)| t);
        match field_type.and_then(|t| self.unboxing(t, enum_name.as_str())) {
            Some(BoxConversion::Direct) => format!("(**{var}).clone()"),
            Some(BoxConversion::Mapped(mapper)) => format!("{var}.clone().map({mapper})"),
            None => format!("{var}.clone()"),
        }
    }

    fn passed_by_ref(t: &Type) -> bool {
        match t {
            Type::Bool | Type::Int | Type::Float | Type::Option(_) => false,
            Type::String | Type::Fragment | Type::Array(_) | Type::Named { .. } => true,
        }
    }

    /// Transpile a type for use in function parameters (uses references without explicit lifetimes)
    fn transpile_param_type<'a>(&mut self, arena: &'a Arena<'a>, t: &'a Type) -> Doc<'a> {
        match t {
            Type::Bool => arena.text("bool"),
            Type::String => arena.text("&str"),
            Type::Float => arena.text("f64"),
            Type::Int => arena.text("i32"),
            Type::Fragment => {
                self.needs_fragment = true;
                arena.text("&Fragment")
            }
            Type::Array(elem) => arena
                .text("&[")
                .append(self.transpile_type(arena, elem))
                .append(arena.text("]")),
            Type::Option(inner) => arena
                .text("Option<")
                .append(self.transpile_type(arena, inner))
                .append(arena.text(">")),
            Type::Named { name, .. } => arena.text("&").append(arena.text(name.as_str())),
        }
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

    fn transpile_module(&mut self, module: &IrModule, registry: &TypeRegistry) -> String {
        // Reset tracking flags for this module
        self.needs_escape_html = false;
        self.needs_fragment = false;
        self.boxed_edges = Self::compute_boxed_edges(module);
        self.registry = registry.clone();

        let arena = &Arena::new();

        let views = &module.views;
        let records = &module.records;

        let mut result = arena.nil();

        // Add enum type definitions
        for enum_def in &module.enums {
            result = result
                .append(arena.text("#[derive(Clone, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub enum "))
                .append(arena.text(enum_def.name.as_str()))
                .append(arena.text(" {"))
                .append(arena.line());

            for variant in &enum_def.variants {
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
                        .map(|(field_name, field_type, _)| {
                            let ft = self.transpile_field_type(
                                arena,
                                field_type,
                                enum_def.name.as_str(),
                            );
                            arena
                                .text(Self::escape_ident(field_name.as_str()))
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
        for record in records {
            result = result
                .append(arena.text("#[derive(Clone, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub struct "))
                .append(arena.text(record.name.as_str()))
                .append(arena.text(" {"))
                .append(arena.line());

            for (field_name, field_type, _) in &record.fields {
                let ft = self.transpile_field_type(arena, field_type, record.name.as_str());
                result = result
                    .append(arena.text("    pub "))
                    .append(arena.text(Self::escape_ident(field_name.as_str())))
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

        // Transpile each component as a function
        for component in &module.components {
            result = result
                .append(self.transpile_component_def(arena, component))
                .append(arena.line());
        }

        // Transpile each view as a function
        for (i, view) in views.iter().enumerate() {
            result = result.append(self.transpile_view(arena, &view.name, view));
            if i < views.len() - 1 {
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
                .append(arena.text("            '\\'' => output.push_str(\"&#39;\"),"))
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

        // Prepend Fragment type definition if needed (after transpilation determined it's used)
        if self.needs_fragment {
            let fragment = arena
                .nil()
                .append(arena.text("#[derive(Clone, Debug)]"))
                .append(arena.line())
                .append(arena.text("pub struct Fragment(pub String);"))
                .append(arena.line())
                .append(arena.line());
            result = fragment.append(result);
        }

        // Prepend View trait definition
        if !module.views.is_empty() {
            let view_trait = arena
                .text("pub trait View {")
                .append(arena.line())
                .append(arena.text("    fn render(self) -> String;"))
                .append(arena.line())
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

    fn transpile_view<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        view: &'a IrViewDeclaration,
    ) -> Doc<'a> {
        let struct_name = name.as_str();

        // Struct definition
        let struct_def = if view.parameters.is_empty() {
            arena
                .text("pub struct ")
                .append(arena.text(struct_name))
                .append(arena.text(" {}"))
        } else {
            let fields = arena.intersperse(
                view.parameters.iter().map(|param| {
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
        };

        // render method body
        let mut body = arena.nil();

        // Destructure self into local variables
        if !view.parameters.is_empty() {
            let field_names = arena.intersperse(
                view.parameters.iter().map(|param| {
                    arena
                        .text(Self::escape_ident(param.name().as_str()))
                        .append(arena.text(": "))
                        .append(arena.text(var_ident(&param.var)))
                }),
                arena.text(", "),
            );
            body = body
                .append(arena.text("let "))
                .append(arena.text(struct_name))
                .append(arena.text(" { "))
                .append(field_names)
                .append(arena.text(" } = self;"))
                .append(arena.hardline());
        }

        body = body
            .append(arena.text("let mut output = String::new();"))
            .append(arena.hardline())
            .append(self.transpile_statements(arena, &view.body))
            .append(arena.hardline())
            .append(arena.text("output"));

        // render method
        let render_fn = arena
            .text("fn render(self) -> String {")
            .append(arena.hardline().append(body).nest(4))
            .append(arena.hardline())
            .append(arena.text("}"));

        // impl View for StructName
        let impl_block = arena
            .text("impl View for ")
            .append(arena.text(struct_name))
            .append(arena.text(" {"))
            .append(arena.hardline().append(render_fn).nest(4))
            .append(arena.hardline())
            .append(arena.text("}"));

        struct_def
            .append(arena.hardline())
            .append(arena.hardline())
            .append(impl_block)
            .append(arena.hardline())
    }

    fn transpile_component_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        component: &'a IrComponentDeclaration,
    ) -> Doc<'a> {
        let func_name = format!("render_{}", component.name.to_snake_case());

        let mut result = arena.text("fn ").append(arena.text(func_name));

        // Build parameter list
        let params: Vec<Doc<'a>> = component
            .parameters
            .iter()
            .map(|param| {
                arena
                    .text(var_ident(&param.var))
                    .append(arena.text(": "))
                    .append(self.transpile_param_type(arena, &param.typ))
            })
            .collect();

        if params.is_empty() {
            result = result.append(arena.text("() -> String {"));
        } else {
            result = result
                .append(arena.text("("))
                .append(arena.intersperse(params, arena.text(", ")))
                .append(arena.text(") -> String {"));
        }

        // Build function body
        let mut body = arena.nil();

        // Initialize output string
        body = body
            .append(arena.text("let mut output = String::new();"))
            .append(arena.hardline());

        // Transpile body statements
        body = body
            .append(self.transpile_statements(arena, &component.body))
            .append(arena.hardline());

        // Return output
        body = body.append(arena.text("output"));

        result
            .append(arena.hardline().append(body).nest(4))
            .append(arena.hardline())
            .append(arena.text("}"))
            .append(arena.hardline())
    }

    fn transpile_component_call_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        args: &'a [IrArgument],
    ) -> Doc<'a> {
        let func_name = format!("render_{}", name.to_snake_case());

        let mut doc = arena
            .text("output.push_str(&")
            .append(arena.text(func_name));

        doc = doc.append(arena.text("("));

        let all_args: Vec<Doc<'a>> = args
            .iter()
            .map(|arg| {
                if Self::passed_by_ref(arg.expr.as_type()) {
                    arena
                        .text("&")
                        .append(self.transpile_expr(arena, &arg.expr))
                } else {
                    self.transpile_expr_owned(arena, &arg.expr)
                }
            })
            .collect();

        if !all_args.is_empty() {
            doc = doc.append(arena.intersperse(all_args, arena.text(", ")));
        }

        doc.append(arena.text("));"))
    }

    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a> {
        arena
            .text("output.push_str(\"")
            .append(arena.text(self.escape_string(content)))
            .append(arena.text("\");"))
    }

    fn transpile_write_expr_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a IrExpr,
        escape: bool,
    ) -> Doc<'a> {
        let unwrap = match expr.as_type() {
            Type::Fragment => ".0",
            _ => "",
        };
        if escape {
            self.needs_escape_html = true;
            arena
                .text("write_escaped_html(&")
                .append(self.transpile_expr(arena, expr))
                .append(arena.text(unwrap))
                .append(arena.text(", &mut output);"))
        } else {
            arena
                .text("output.push_str(&")
                .append(self.transpile_expr(arena, expr))
                .append(arena.text(unwrap))
                .append(arena.text(");"))
        }
    }

    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrVar>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        let var_name = var.map_or_else(|| "_".to_string(), var_ident);

        let doc = match source {
            IrForSource::Array(array) => arena
                .text("for ")
                .append(arena.text(var_name))
                .append(arena.text(" in "))
                .append(self.transpile_expr(arena, array))
                .append(arena.text(".iter() {")),
            IrForSource::RangeInclusive { start, end } => arena
                .text("for ")
                .append(arena.text(var_name))
                .append(arena.text(" in "))
                .append(self.transpile_expr(arena, start))
                .append(arena.text("..="))
                .append(self.transpile_expr(arena, end))
                .append(arena.text(" {")),
        };

        let rebinds: Vec<(String, String)> = match source {
            IrForSource::Array(_) => var
                .into_iter()
                .map(var_ident)
                .map(|v| (v.clone(), format!("{}.clone()", v)))
                .collect(),
            IrForSource::RangeInclusive { .. } => Vec::new(),
        };
        doc.append(
            arena
                .hardline()
                .append(self.stmts_with_rebinds(arena, &rebinds, body))
                .nest(4),
        )
        .append(arena.hardline())
        .append(arena.text("}"))
    }

    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        let binding = arena
            .text("let ")
            .append(arena.text(var_ident(var)))
            .append(arena.text(" = "))
            .append(self.transpile_expr_owned(arena, value))
            .append(arena.text(";"));
        let body = if body.is_empty() {
            arena.nil()
        } else {
            arena
                .hardline()
                .append(self.transpile_statements(arena, body))
        };
        binding.append(body)
    }

    fn transpile_let_fragment_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        fragment_body: &'a [IrStatement],
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        self.needs_fragment = true;
        let binding = arena
            .text("let ")
            .append(arena.text(var_ident(var)))
            .append(arena.text(" = {"))
            .append(
                arena
                    .nil()
                    .append(arena.hardline())
                    .append(arena.text("let mut output = String::new();"))
                    .append(arena.hardline())
                    .append(self.transpile_statements(arena, fragment_body))
                    .append(arena.hardline())
                    .append(arena.text("Fragment(output)"))
                    .nest(4),
            )
            .append(arena.hardline())
            .append(arena.text("};"));
        let body = if body.is_empty() {
            arena.nil()
        } else {
            arena
                .hardline()
                .append(self.transpile_statements(arena, body))
        };
        binding.append(body)
    }

    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<IrExpr, Vec<IrStatement>, IrVar>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let if_doc = arena
                    .text("if ")
                    .append(self.transpile_match_subject(arena, subject))
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
                    Some(var) => format!("Some({})", var_ident(var)),
                    None => "Some(_)".to_string(),
                };
                let some_rebind: Vec<(String, String)> = some_arm_binding
                    .iter()
                    .map(var_ident)
                    .map(|v| (v.clone(), format!("{}.clone()", v)))
                    .collect();

                let some_arm = arena
                    .text(some_pattern)
                    .append(arena.text(" => {"))
                    .append(
                        arena
                            .hardline()
                            .append(self.stmts_with_rebinds(arena, &some_rebind, some_arm_body))
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
                    .text("match &")
                    .append(self.transpile_match_subject(arena, subject))
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
                // Extract variant information from the subject's type
                let subject_type = subject.get_type();
                let Some(ResolvedType::Enum { variants, .. }) =
                    self.registry.resolve(subject_type.as_ref())
                else {
                    unreachable!("Enum match subject must have Named enum type")
                };
                let variants = variants.to_vec();

                let arms_doc = arena.intersperse(
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
                                        .find(|v| &v.name == variant_name)
                                        .map(|v| !v.fields.is_empty())
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
                                        .map(|(field, var)| {
                                            format!(
                                                "{}: {}",
                                                Self::escape_ident(field.as_str()),
                                                var_ident(var)
                                            )
                                        })
                                        .collect();
                                    let variant_field_count = variants
                                        .iter()
                                        .find(|v| &v.name == variant_name)
                                        .map(|v| v.fields.len())
                                        .unwrap_or(0);
                                    let rest = if arm.bindings.len() < variant_field_count {
                                        ", .."
                                    } else {
                                        ""
                                    };
                                    format!(
                                        "{}::{} {{ {}{} }}",
                                        enum_name,
                                        variant_name,
                                        bindings.join(", "),
                                        rest,
                                    )
                                }
                            }
                        };
                        let arm_rebind: Vec<(String, String)> = arm
                            .bindings
                            .iter()
                            .map(|(field, var)| {
                                (
                                    var_ident(var),
                                    self.arm_rebind_value(&variants, &arm.pattern, field, var),
                                )
                            })
                            .collect();

                        arena
                            .text(pattern)
                            .append(arena.text(" => {"))
                            .append(
                                arena
                                    .hardline()
                                    .append(self.stmts_with_rebinds(arena, &arm_rebind, &arm.body))
                                    .nest(4),
                            )
                            .append(arena.hardline())
                            .append(arena.text("}"))
                    }),
                    arena.hardline(),
                );

                arena
                    .text("match &")
                    .append(self.transpile_match_subject(arena, subject))
                    .append(arena.text(" {"))
                    .append(arena.hardline().append(arms_doc).nest(4))
                    .append(arena.hardline())
                    .append(arena.text("}"))
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

    fn transpile_fragment_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        self.needs_fragment = true;
        arena.text("Fragment")
    }

    fn transpile_array_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_type: &'a Type,
    ) -> Doc<'a> {
        arena
            .text("Vec<")
            .append(self.transpile_type(arena, element_type))
            .append(arena.text(">"))
    }

    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &'a Type) -> Doc<'a> {
        arena
            .text("Option<")
            .append(self.transpile_type(arena, inner_type))
            .append(arena.text(">"))
    }

    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a> {
        arena.text(name)
    }

    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a> {
        arena.text(name)
    }

    fn transpile_var<'a>(&mut self, arena: &'a Arena<'a>, var: &'a IrVar) -> Doc<'a> {
        arena.text(var_ident(var))
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> Doc<'a> {
        let boxed = self.field_unboxing(object, field);
        let object_doc = match object {
            IrExpr::RecordLiteral { .. } => arena
                .text("(")
                .append(self.transpile_expr(arena, object))
                .append(arena.text(")")),
            _ => self.transpile_expr(arena, object),
        };
        let access = object_doc
            .append(arena.text("."))
            .append(arena.text(Self::escape_ident(field.as_str())));

        match boxed {
            None => access,
            Some(BoxConversion::Direct) => arena
                .text("(*")
                .append(access)
                .append(arena.text(").clone()")),
            Some(BoxConversion::Mapped(mapper)) => {
                access.append(arena.text(format!(".clone().map({mapper})")))
            }
        }
    }

    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a> {
        arena
            .text("\"")
            .append(arena.text(self.escape_string(value)))
            .append(arena.text("\".to_string()"))
    }

    fn transpile_fragment_empty<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a> {
        self.needs_fragment = true;
        arena.text("Fragment(String::new())")
    }

    fn transpile_boolean_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a> {
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
        elements: &'a [IrExpr],
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
                .map(|e| self.transpile_expr_owned(arena, e))
                .collect();
            arena
                .text("vec![")
                .append(arena.intersperse(items, arena.text(", ")))
                .append(arena.text("]"))
        }
    }

    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" == "))
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" == "))
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" == "))
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" == "))
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
            .text("(")
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
            .text("(")
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
            .text("(")
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" <= "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: &'a IrExpr) -> Doc<'a> {
        arena.text("!").append(self.transpile_expr(arena, operand))
    }

    fn transpile_int_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, operand))
            .append(arena.text(").wrapping_neg()"))
    }

    fn transpile_float_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("(-")
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
            .text("format!(\"{}{}\", ")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(", "))
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
            .text("(")
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
            .text("(")
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(").wrapping_add("))
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
            .text("(")
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(").wrapping_sub("))
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
            .text("(")
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(").wrapping_mul("))
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
            .text("(")
            .append(self.transpile_expr(arena, left))
            .append(arena.text(" * "))
            .append(self.transpile_expr(arena, right))
            .append(arena.text(")"))
    }

    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> Doc<'a> {
        if fields.is_empty() {
            arena.text(record_name).append(arena.text(" {}"))
        } else {
            let field_docs: Vec<Doc<'a>> = fields
                .iter()
                .map(|(name, value)| {
                    let val_doc = self.transpile_field_value(arena, record_name, value);
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
        fields: &'a [(FieldName, IrExpr)],
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
                    let val_doc = self.transpile_field_value(arena, enum_name, value);
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
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> Doc<'a> {
        match value {
            Some(expr) => arena
                .text("Some(")
                .append(self.transpile_expr_owned(arena, expr))
                .append(arena.text(")")),
            None => arena
                .text("None::<")
                .append(self.transpile_type(arena, inner_type))
                .append(arena.text(">")),
        }
    }

    fn transpile_match_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<IrExpr, IrExpr, IrVar>,
    ) -> Doc<'a> {
        match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => arena
                .text("if ")
                .append(self.transpile_match_subject(arena, subject))
                .append(arena.text(" { "))
                .append(self.transpile_expr_owned(arena, true_body))
                .append(arena.text(" } else { "))
                .append(self.transpile_expr_owned(arena, false_body))
                .append(arena.text(" }")),
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(var) => format!("Some({})", var_ident(var)),
                    None => "Some(_)".to_string(),
                };
                let some_rebind: Vec<(String, String)> = some_arm_binding
                    .iter()
                    .map(var_ident)
                    .map(|v| (v.clone(), format!("{}.clone()", v)))
                    .collect();
                let some_arm_doc = self.expr_with_rebinds(arena, &some_rebind, some_arm_body);
                arena
                    .text("match &")
                    .append(self.transpile_match_subject(arena, subject))
                    .append(arena.text(" { "))
                    .append(arena.text(some_pattern))
                    .append(arena.text(" => "))
                    .append(some_arm_doc)
                    .append(arena.text(", None => "))
                    .append(self.transpile_expr_owned(arena, none_arm_body))
                    .append(arena.text(" }"))
            }
            Match::Enum { subject, arms } => {
                // Extract variant information from the subject's type
                let subject_type = subject.get_type();
                let Some(ResolvedType::Enum { variants, .. }) =
                    self.registry.resolve(subject_type.as_ref())
                else {
                    unreachable!("Enum match subject must have Named enum type")
                };
                let variants = variants.to_vec();

                let mut doc = arena
                    .text("match &")
                    .append(self.transpile_match_subject(arena, subject))
                    .append(arena.text(" { "));

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
                                    .find(|v| &v.name == variant_name)
                                    .map(|v| !v.fields.is_empty())
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
                                    .map(|(field, var)| {
                                        format!(
                                            "{}: {}",
                                            Self::escape_ident(field.as_str()),
                                            var_ident(var)
                                        )
                                    })
                                    .collect();
                                let variant_field_count = variants
                                    .iter()
                                    .find(|v| &v.name == variant_name)
                                    .map(|v| v.fields.len())
                                    .unwrap_or(0);
                                let rest = if arm.bindings.len() < variant_field_count {
                                    ", .."
                                } else {
                                    ""
                                };
                                format!(
                                    "{}::{} {{ {}{} }}",
                                    enum_name,
                                    variant_name,
                                    bindings.join(", "),
                                    rest,
                                )
                            }
                        }
                    };

                    let arm_rebind: Vec<(String, String)> = arm
                        .bindings
                        .iter()
                        .map(|(field, var)| {
                            (
                                var_ident(var),
                                self.arm_rebind_value(&variants, &arm.pattern, field, var),
                            )
                        })
                        .collect();
                    let arm_body_doc = self.expr_with_rebinds(arena, &arm_rebind, &arm.body);

                    doc = doc
                        .append(arena.text(pattern))
                        .append(arena.text(" => "))
                        .append(arm_body_doc);

                    if i < arms.len() - 1 {
                        doc = doc.append(arena.text(", "));
                    }
                }

                doc.append(arena.text(" }"))
            }
        }
    }

    fn transpile_let<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("{ let ")
            .append(arena.text(var_ident(var)))
            .append(arena.text(" = "))
            .append(self.transpile_expr_owned(arena, value))
            .append(arena.text("; "))
            .append(self.transpile_expr_owned(arena, body))
            .append(arena.text(" }"))
    }

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, array))
            .append(arena.text(".len() as i32)"))
    }

    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a> {
        self.transpile_expr(arena, array)
            .append(arena.text(".is_empty()"))
    }

    fn transpile_string_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        string: &'a IrExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, string)
            .append(arena.text(".is_empty()"))
    }

    fn transpile_option_is_some<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a IrExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, option)
            .append(arena.text(".is_some()"))
    }

    fn transpile_option_is_none<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a IrExpr,
    ) -> Doc<'a> {
        self.transpile_expr(arena, option)
            .append(arena.text(".is_none()"))
    }

    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(").to_string()"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(" as i32)"))
    }

    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(" as f64)"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ir_module_builder::{IrModuleBodiesBuilder, IrModuleBuilder};
    use expect_test::{Expect, expect};

    fn check(builder: impl Into<IrModuleBodiesBuilder>, expected: Expect) {
        let (module, registry) = builder.into().build_with_registry();
        let before = module.to_string();
        let after = RustTranspiler::new().transpile_module(&module, &registry);
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.write("<h1>Hello, World!</h1>\n");
            }),
            expect![[r#"
                -- before --
                view Test() {
                  write("<h1>Hello, World!</h1>\n")
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        output.push_str("<h1>Hello, World!</h1>\n");
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn conditional_display() {
        check(
            IrModuleBuilder::new().view("Test", [("show", "Bool")], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<h1>Visible</h1>");
                });
            }),
            expect![[r#"
                -- before --
                view Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      write("<h1>Visible</h1>")
                    }
                    false => {
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                pub struct Test {
                    pub show: bool,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let Test { show: v_0 } = self;
                        let mut output = String::new();
                        if v_0 {
                            output.push_str("<h1>Visible</h1>");
                        }
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn for_loop_with_range() {
        check(
            IrModuleBuilder::new().view_no_params("Test", |t| {
                t.for_range(Some("i"), t.int(1), t.int(3), |t| {
                    t.write_expr(t.int_to_string(t.var("i")), false);
                });
            }),
            expect![[r#"
                -- before --
                view Test() {
                  for v0 in 1..=3 {
                    write_expr(v0.to_string())
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        for v_0 in 1_i32..=3_i32 {
                            output.push_str(&(v_0).to_string());
                        }
                        output
                    }
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
                        t.write("some: ");
                        t.write_expr(t.var("value"), false);
                    },
                    |t| {
                        t.write("none");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x") {
                    Some(v0) => {
                      write("some: ")
                      write_expr(v0)
                    }
                    None => {
                      write("none")
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        match &(Some("x".to_string())) {
                            Some(v_0) => {
                                let v_0 = v_0.clone();
                                output.push_str("some: ");
                                output.push_str(&v_0);
                            }
                            None => {
                                output.push_str("none");
                            }
                        }
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            IrModuleBuilder::new().view("Test", [("opt", "Option[String]")], |t| {
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
            }),
            expect![[r#"
                -- before --
                view Test(opt@v0: Option[String]) {
                  match v0 {
                    Some(v1) => {
                      write("some: ")
                      write_expr(v1)
                    }
                    None => {
                      write("none")
                    }
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                pub struct Test {
                    pub opt: Option<String>,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let Test { opt: v_0 } = self;
                        let mut output = String::new();
                        match &v_0 {
                            Some(v_1) => {
                                let v_1 = v_1.clone();
                                output.push_str("some: ");
                                output.push_str(&v_1);
                            }
                            None => {
                                output.push_str("none");
                            }
                        }
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_boxes_recursive_field() {
        check(
            IrModuleBuilder::new()
                .record("Node", [("value", "Int"), ("next", "Option[Node]")])
                .view("Test", [("node", "Node")], |t| {
                    t.write_expr(
                        t.int_to_string(t.field_access(t.var("node"), "value")),
                        false,
                    );
                }),
            expect![[r#"
                -- before --
                record Node {
                  value: Int,
                  next: Option[test::Node],
                }
                view Test(node@v0: test::Node) {
                  write_expr(v0.value.to_string())
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub struct Node {
                    pub value: i32,
                    pub next: Option<Box<Node>>,
                }

                pub struct Test {
                    pub node: Node,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let Test { node: v_0 } = self;
                        let mut output = String::new();
                        output.push_str(&(v_0.value).to_string());
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_boxes_recursive_field() {
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
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub enum IntList {
                    Cons { head: i32, tail: Box<IntList> },
                    Nil,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        output.push_str("hello");
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_record_literal_boxes_field_values() {
        check(
            IrModuleBuilder::new()
                .record("Node", [("value", "Int"), ("next", "Option[Node]")])
                .view_no_params("Test", |t| {
                    let inner =
                        t.record("Node", vec![("value", t.int(1)), ("next", t.none("Node"))]);
                    let node = t.record("Node", vec![("value", t.int(2)), ("next", t.some(inner))]);
                    t.let_stmt("node", node, |t| {
                        t.write_expr(
                            t.int_to_string(t.field_access(t.var("node"), "value")),
                            false,
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
                  let v0 = Node {
                    value: 2,
                    next: Option[test::Node]::Some(Node {
                      value: 1,
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    write_expr(v0.value.to_string())
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub struct Node {
                    pub value: i32,
                    pub next: Option<Box<Node>>,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        let v_0 = Node { value: 2_i32, next: Some(Node { value: 1_i32, next: None::<Node>.map(Box::new) }).map(Box::new) };
                        output.push_str(&(v_0.value).to_string());
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn recursive_enum_literal_boxes_field_values() {
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
                    let list = t.enum_variant_with_fields(
                        "IntList",
                        "Cons",
                        vec![
                            ("head", t.int(1)),
                            ("tail", t.enum_variant("IntList", "Nil")),
                        ],
                    );
                    t.let_stmt("list", list, |t| {
                        t.write("done");
                    });
                }),
            expect![[r#"
                -- before --
                enum IntList {
                  Cons {head: Int, tail: test::IntList},
                  Nil,
                }
                view Test() {
                  let v0 = IntList::Cons {head: 1, tail: IntList::Nil} in {
                    write("done")
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub enum IntList {
                    Cons { head: i32, tail: Box<IntList> },
                    Nil,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        let v_0 = IntList::Cons { head: 1_i32, tail: Box::new(IntList::Nil) };
                        output.push_str("done");
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn mutually_recursive_records_boxes_in_both_directions() {
        check(
            IrModuleBuilder::new()
                .record("A", [("b", "B")])
                .record("B", [("a", "Option[A]")])
                .view_no_params("Test", |t| {
                    let inner_b = t.record("B", vec![("a", t.none("A"))]);
                    let a = t.record("A", vec![("b", inner_b)]);
                    let b = t.record("B", vec![("a", t.some(a))]);
                    t.let_stmt("b", b, |t| {
                        t.write("done");
                    });
                }),
            expect![[r#"
                -- before --
                record A {
                  b: B,
                }
                record B {
                  a: Option[test::A],
                }
                view Test() {
                  let v0 = B {
                    a: Option[test::A]::Some(A {
                      b: B {a: Option[test::A]::None},
                    }),
                  } in {
                    write("done")
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub struct A {
                    pub b: Box<B>,
                }

                #[derive(Clone, Debug)]
                pub struct B {
                    pub a: Option<Box<A>>,
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        let v_0 = B { a: Some(A { b: Box::new(B { a: None::<A>.map(Box::new) }) }).map(Box::new) };
                        output.push_str("done");
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn component_with_enum_param() {
        check(
            IrModuleBuilder::new()
                .enum_unit("Color", ["Red", "Green", "Blue"])
                .view_no_params("Test", |t| {
                    t.invoke_component("Badge", vec![("color", t.enum_variant("Color", "Green"))]);
                })
                .component("Badge", [("color", "Color")], |t| {
                    t.enum_match_stmt(t.var("color"), |m| {
                        m.arm("Red", |t| t.write("red"));
                        m.arm("Green", |t| t.write("green"));
                        m.arm("Blue", |t| t.write("blue"));
                    });
                }),
            expect![[r#"
                -- before --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                component Badge(color@v0: test::Color) {
                  match v0 {
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
                view Test() {
                  call Badge(color = Color::Green)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub enum Color {
                    Red,
                    Green,
                    Blue,
                }

                fn render_badge(v_0: &Color) -> String {
                    let mut output = String::new();
                    match &v_0 {
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
                    output
                }

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        output.push_str(&render_badge(&Color::Green));
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn transpiles_let_fragment_as_rust_block() {
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
                  let v0 = {
                    write("<b>hi</b>")
                  } in {
                    write_expr(v0)
                  }
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub struct Fragment(pub String);

                pub struct Test {}

                impl View for Test {
                    fn render(self) -> String {
                        let mut output = String::new();
                        let v_0 = {
                            let mut output = String::new();
                            output.push_str("<b>hi</b>");
                            Fragment(output)
                        };
                        output.push_str(&v_0.0);
                        output
                    }
                }
            "#]],
        );
    }
}
