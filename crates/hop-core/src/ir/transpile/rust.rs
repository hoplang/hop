use pretty::{Arena, DocAllocator};

use super::{Doc, Transpiler};
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::typing::r#type::Type;
use crate::ir::ast::{
    IrArgument, IrComponentDeclaration, IrExpr, IrForSource, IrModule, IrStatement,
    IrViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

pub struct RustTranspiler {
    /// Tracks whether escape_html function is used during transpilation
    needs_escape_html: bool,
    /// Tracks whether Fragment type is used during transpilation
    needs_fragment: bool,
    /// Set of type names that are self-referential and need Box indirection
    recursive_types: std::collections::HashSet<TypeName>,
}

impl RustTranspiler {
    pub fn new() -> Self {
        Self {
            needs_escape_html: false,
            needs_fragment: false,
            recursive_types: std::collections::HashSet::new(),
        }
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

    /// Render `RecordName { field: var, .. } = &subject`. Destructuring a
    /// reference binds fields as borrows; `..` ignores unbound fields.
    fn transpile_record_destructure_pattern<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
    ) -> Doc<'a> {
        let record_name = match subject.get_type().as_ref() {
            Type::Record { name, .. } => name.as_str().to_string(),
            _ => unreachable!("LetRecordDestructure subject must have Record type"),
        };
        let bindings_doc = arena.intersperse(
            bindings.iter().map(|(field, var)| {
                arena
                    .text(Self::escape_field_name(field.as_str()))
                    .append(arena.text(": "))
                    .append(arena.text(var.as_str()))
            }),
            arena.text(", "),
        );
        arena
            .text(record_name)
            .append(arena.text(" { "))
            .append(bindings_doc)
            .append(arena.text(", .. } = &"))
            .append(self.transpile_match_subject(arena, subject))
    }

    fn escape_field_name(name: &str) -> String {
        match name {
            "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern"
            | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod"
            | "move" | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct"
            | "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while"
            | "async" | "await" | "dyn" | "abstract" | "become" | "box" | "do" | "final"
            | "macro" | "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield"
            | "try" => format!("r#{}", name),
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
            IrExpr::FieldAccess { kind, .. } | IrExpr::Var { kind, .. } => match kind.as_ref() {
                Type::Component { .. } => self.transpile_expr(arena, expr),
                _ => self
                    .transpile_expr(arena, expr)
                    .append(arena.text(".clone()")),
            },
            _ => self.transpile_expr(arena, expr),
        }
    }

    /// Compute which types are self-referential and need Box indirection
    fn compute_recursive_types(module: &IrModule) -> std::collections::HashSet<TypeName> {
        let mut recursive = std::collections::HashSet::new();
        for record in &module.records {
            for (_, field_type, _) in &record.fields {
                if Self::type_needs_box(field_type, record.name.as_str()) {
                    recursive.insert(record.name.clone());
                    break;
                }
            }
        }
        for enum_def in &module.enums {
            'outer: for variant in &enum_def.variants {
                for (_, field_type, _) in &variant.fields {
                    if Self::type_needs_box(field_type, enum_def.name.as_str()) {
                        recursive.insert(enum_def.name.clone());
                        break 'outer;
                    }
                }
            }
        }
        recursive
    }

    /// Check if a field type contains a reference to the containing type (through Option)
    fn type_needs_box(t: &Type, containing_type: &str) -> bool {
        match t {
            Type::Record { name, .. } | Type::Enum { name, .. } => name.as_str() == containing_type,
            Type::Option(inner) => Self::type_needs_box(inner, containing_type),
            _ => false,
        }
    }

    /// Transpile a type for a field definition, adding Box for self-referential fields
    fn transpile_type_with_box<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        t: &'a Type,
        containing_type: &str,
    ) -> Doc<'a> {
        match t {
            Type::Record { name, .. } | Type::Enum { name, .. }
                if name.as_str() == containing_type =>
            {
                arena
                    .text("Box<")
                    .append(arena.text(name.as_str()))
                    .append(arena.text(">"))
            }
            Type::Option(inner) if Self::type_needs_box(inner, containing_type) => arena
                .text("Option<")
                .append(self.transpile_type_with_box(arena, inner, containing_type))
                .append(arena.text(">")),
            _ => self.transpile_type(arena, t),
        }
    }

    /// Transpile an expression used as a field value of a recursive type,
    /// adding Box::new() wrapping where needed for self-referential fields.
    fn transpile_expr_for_recursive_field<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a IrExpr,
        recursive_type: &str,
    ) -> Doc<'a> {
        let expr_type = expr.as_type();
        // Direct recursive type → Box::new(...)
        match expr_type {
            Type::Record { name, .. } | Type::Enum { name, .. }
                if name.as_str() == recursive_type =>
            {
                return arena
                    .text("Box::new(")
                    .append(self.transpile_expr_owned(arena, expr))
                    .append(arena.text(")"));
            }
            _ => {}
        }
        // Option<RecursiveType> → special handling for Some/None
        if let IrExpr::OptionLiteral { value, kind, .. } = expr {
            let inner_type = match kind.as_ref() {
                Type::Option(inner) => inner.as_ref(),
                _ => unreachable!(),
            };
            if matches!(inner_type,
                Type::Record { name, .. } | Type::Enum { name, .. }
                if name.as_str() == recursive_type)
            {
                return match value {
                    Some(inner_expr) => arena
                        .text("Some(Box::new(")
                        .append(self.transpile_expr_owned(arena, inner_expr))
                        .append(arena.text("))")),
                    None => arena
                        .text("None::<Box<")
                        .append(self.transpile_type(arena, inner_type))
                        .append(arena.text(">>")),
                };
            }
        }
        self.transpile_expr_owned(arena, expr)
    }

    fn passed_by_ref(t: &Type) -> bool {
        match t {
            Type::Bool | Type::Int | Type::Float | Type::Option(_) => false,
            Type::String
            | Type::Fragment
            | Type::Array(_)
            | Type::Record { .. }
            | Type::Enum { .. }
            | Type::Component { .. } => true,
        }
    }

    /// Transpile a type for use in function parameters (uses references without explicit lifetimes)
    fn transpile_param_type<'a>(&mut self, arena: &'a Arena<'a>, t: &'a Type) -> Doc<'a> {
        match t {
            Type::Bool => arena.text("bool"),
            Type::String => arena.text("&str"),
            Type::Float => arena.text("f64"),
            Type::Int => arena.text("i64"),
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
            Type::Record { name, .. } => arena.text("&").append(arena.text(name.as_str())),
            Type::Enum { name, .. } => arena.text("&").append(arena.text(name.as_str())),
            Type::Component { name, .. } => arena.text("&").append(arena.text(name.as_str())),
        }
    }
}

impl Default for RustTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Transpiler for RustTranspiler {
    fn transpile_module(&mut self, module: &IrModule) -> String {
        // Reset tracking flags for this module
        self.needs_escape_html = false;
        self.needs_fragment = false;
        self.recursive_types = Self::compute_recursive_types(module);

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

                    let is_recursive_enum = self.recursive_types.contains(&enum_def.name);
                    let field_docs: Vec<Doc> = variant
                        .fields
                        .iter()
                        .map(|(field_name, field_type, _)| {
                            let ft = if is_recursive_enum {
                                self.transpile_type_with_box(
                                    arena,
                                    field_type,
                                    enum_def.name.as_str(),
                                )
                            } else {
                                self.transpile_type(arena, field_type)
                            };
                            arena
                                .text(Self::escape_field_name(field_name.as_str()))
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

            let is_recursive_record = self.recursive_types.contains(&record.name);
            for (field_name, field_type, _) in &record.fields {
                let ft = if is_recursive_record {
                    self.transpile_type_with_box(arena, field_type, record.name.as_str())
                } else {
                    self.transpile_type(arena, field_type)
                };
                result = result
                    .append(arena.text("    pub "))
                    .append(arena.text(Self::escape_field_name(field_name.as_str())))
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
                        .append(arena.text(param.name.as_str()))
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
                view.parameters
                    .iter()
                    .map(|param| arena.text(param.name.as_str())),
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
                    .text(param.name.as_str())
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
                let arg_doc = self.transpile_expr(arena, &arg.expr);
                if Self::passed_by_ref(arg.expr.as_type()) {
                    arena.text("&").append(arg_doc)
                } else {
                    arg_doc
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
        let expr_type = expr.as_type();

        if escape {
            self.needs_escape_html = true;
            // HTML escaping needed
            match expr_type {
                Type::String => arena
                    .text("write_escaped_html(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(", &mut output);")),
                Type::Int => arena
                    .text("write_escaped_html(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(".to_string(), &mut output);")),
                _ => arena
                    .text("write_escaped_html(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(", &mut output);")),
            }
        } else {
            // No escaping needed
            match expr_type {
                Type::String => arena
                    .text("output.push_str(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(");")),
                Type::Fragment => arena
                    .text("output.push_str(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(".0);")),
                Type::Int => arena
                    .text("output.push_str(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(".to_string());")),
                _ => arena
                    .text("output.push_str(&")
                    .append(self.transpile_expr(arena, expr))
                    .append(arena.text(");")),
            }
        }
    }

    fn transpile_if_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        arena
            .text("if ")
            .append(self.transpile_expr(arena, condition))
            .append(arena.text(" {"))
            .append(
                arena
                    .hardline()
                    .append(self.transpile_statements(arena, body))
                    .nest(4),
            )
            .append(arena.hardline())
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

        let doc = match source {
            IrForSource::Array(array) => arena
                .text("for ")
                .append(arena.text(var_name))
                .append(arena.text(" in &"))
                .append(self.transpile_expr(arena, array))
                .append(arena.text(" {")),
            IrForSource::RangeInclusive { start, end } => arena
                .text("for ")
                .append(arena.text(var_name))
                .append(arena.text(" in "))
                .append(self.transpile_expr(arena, start))
                .append(arena.text("..="))
                .append(self.transpile_expr(arena, end))
                .append(arena.text(" {")),
        };

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
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a> {
        arena
            .text("let ")
            .append(arena.text(var))
            .append(arena.text(" = "))
            .append(self.transpile_expr_owned(arena, value))
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
            .text("let ")
            .append(arena.text(var))
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
            .append(arena.text("};"))
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
            .text("let ")
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
                .append(arena.text("} else {"))
                .append(
                    arena
                        .hardline()
                        .append(self.transpile_statements(arena, false_body))
                        .nest(4),
                )
                .append(arena.hardline())
                .append(arena.text("}")),
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
                let variants = match subject_type.as_ref() {
                    Type::Enum { variants, .. } => variants,
                    _ => unreachable!("Enum match subject must have Enum type"),
                };

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
                                                Self::escape_field_name(field.as_str()),
                                                var
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
        arena.text("i64")
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

    fn transpile_var<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a> {
        arena.text(name)
    }

    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> Doc<'a> {
        self.transpile_expr(arena, object)
            .append(arena.text("."))
            .append(arena.text(Self::escape_field_name(field.as_str())))
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
        // Ensure float literals have a decimal point
        let s = value.to_string();
        if s.contains('.') || s.contains('e') || s.contains('E') {
            arena.text(format!("{}_f64", s))
        } else {
            arena.text(format!("{}.0_f64", s))
        }
    }

    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i64) -> Doc<'a> {
        arena.text(format!("{}_i64", value))
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

    fn transpile_numeric_negation<'a>(
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
        let is_recursive = self
            .recursive_types
            .iter()
            .any(|n| n.as_str() == record_name);
        if fields.is_empty() {
            arena.text(record_name).append(arena.text(" {}"))
        } else {
            let field_docs: Vec<Doc<'a>> = fields
                .iter()
                .map(|(name, value)| {
                    let val_doc = if is_recursive {
                        self.transpile_expr_for_recursive_field(arena, value, record_name)
                    } else {
                        self.transpile_expr_owned(arena, value)
                    };
                    arena
                        .text(Self::escape_field_name(name.as_str()))
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
        let is_recursive = self.recursive_types.iter().any(|n| n.as_str() == enum_name);
        if fields.is_empty() {
            arena
                .text(enum_name)
                .append(arena.text("::"))
                .append(arena.text(variant_name))
        } else {
            let field_docs: Vec<Doc<'a>> = fields
                .iter()
                .map(|(name, value)| {
                    let val_doc = if is_recursive {
                        self.transpile_expr_for_recursive_field(arena, value, enum_name)
                    } else {
                        self.transpile_expr_owned(arena, value)
                    };
                    arena
                        .text(Self::escape_field_name(name.as_str()))
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
        match_: &'a Match<IrExpr, IrExpr>,
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
                    Some(var) => format!("Some({})", var.as_str()),
                    None => "Some(_)".to_string(),
                };
                arena
                    .text("match &")
                    .append(self.transpile_match_subject(arena, subject))
                    .append(arena.text(" { "))
                    .append(arena.text(some_pattern))
                    .append(arena.text(" => "))
                    .append(self.transpile_expr_owned(arena, some_arm_body))
                    .append(arena.text(", None => "))
                    .append(self.transpile_expr_owned(arena, none_arm_body))
                    .append(arena.text(" }"))
            }
            Match::Enum { subject, arms } => {
                // Extract variant information from the subject's type
                let subject_type = subject.get_type();
                let variants = match subject_type.as_ref() {
                    Type::Enum { variants, .. } => variants,
                    _ => unreachable!("Enum match subject must have Enum type"),
                };

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
                                            Self::escape_field_name(field.as_str()),
                                            var
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

                    doc = doc
                        .append(arena.text(pattern))
                        .append(arena.text(" => "))
                        .append(self.transpile_expr_owned(arena, &arm.body));

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
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("{ let ")
            .append(arena.text(var.as_str()))
            .append(arena.text(" = "))
            .append(self.transpile_expr_owned(arena, value))
            .append(arena.text("; "))
            .append(self.transpile_expr(arena, body))
            .append(arena.text(" }"))
    }

    fn transpile_let_record_destructure_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
        body: &'a IrExpr,
    ) -> Doc<'a> {
        arena
            .text("{ let ")
            .append(self.transpile_record_destructure_pattern(arena, subject, bindings))
            .append(arena.text("; "))
            .append(self.transpile_expr(arena, body))
            .append(arena.text(" }"))
    }

    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, array))
            .append(arena.text(".len() as i64)"))
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
        self.transpile_expr(arena, value)
            .append(arena.text(".to_string()"))
    }

    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a> {
        arena
            .text("(")
            .append(self.transpile_expr(arena, value))
            .append(arena.text(" as i64)"))
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
    use std::sync::Arc;

    use super::*;
    use crate::dop::typing::r#type::EnumVariant;
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
            IrModuleBuilder::new()
                .component("Test", [("show", Type::Bool)], |t| {
                    t.if_stmt(t.var("show"), |t| {
                        t.write("<h1>Visible</h1>");
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test(show: Bool) {
                  if show {
                    write("<h1>Visible</h1>")
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
                        let Test { show } = self;
                        let mut output = String::new();
                        if show {
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
            IrModuleBuilder::new()
                .component_no_params("Test", |t| {
                    t.for_range("i", t.int(1), t.int(3), |t| {
                        t.write_expr(t.int_to_string(t.var("i")), false);
                    });
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  for i in 1..=3 {
                    write_expr(i.to_string())
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
                        for i in 1_i64..=3_i64 {
                            output.push_str(&i.to_string());
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
            IrModuleBuilder::new()
                .component_no_params("Test", |t| {
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
                })
                .build(),
            expect![[r#"
                -- before --
                view Test() {
                  match Option[String]::Some("x") {
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
                }
            "#]],
        );
    }

    #[test]
    fn option_match_statement() {
        check(
            IrModuleBuilder::new()
                .component(
                    "Test",
                    [("opt", Type::Option(Arc::new(Type::String)))],
                    |t| {
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
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                view Test(opt: Option[String]) {
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
                        let Test { opt } = self;
                        let mut output = String::new();
                        match &opt {
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
                }
            "#]],
        );
    }

    #[test]
    fn record_with_record_field_uses_arc() {
        check(
            IrModuleBuilder::new()
                .record("Address", |r| {
                    r.field("street", Type::String);
                })
                .record("Person", |r| {
                    r.field("name", Type::String);
                    r.field(
                        "address",
                        Type::Record {
                            module: crate::document_id::DocumentId::new("test.hop").unwrap(),
                            name: crate::symbols::type_name::TypeName::new("Address").unwrap(),
                            fields: vec![(
                                crate::symbols::field_name::FieldName::new("street").unwrap(),
                                Arc::new(Type::String),
                                None,
                            )],
                        },
                    );
                })
                .component(
                    "Test",
                    [(
                        "person",
                        Type::Record {
                            module: crate::document_id::DocumentId::new("test.hop").unwrap(),
                            name: crate::symbols::type_name::TypeName::new("Person").unwrap(),
                            fields: vec![
                                (
                                    crate::symbols::field_name::FieldName::new("name").unwrap(),
                                    Arc::new(Type::String),
                                    None,
                                ),
                                (
                                    crate::symbols::field_name::FieldName::new("address").unwrap(),
                                    Arc::new(Type::Record {
                                        module: crate::document_id::DocumentId::new("test.hop")
                                            .unwrap(),
                                        name: crate::symbols::type_name::TypeName::new("Address")
                                            .unwrap(),
                                        fields: vec![(
                                            crate::symbols::field_name::FieldName::new("street")
                                                .unwrap(),
                                            Arc::new(Type::String),
                                            None,
                                        )],
                                    }),
                                    None,
                                ),
                            ],
                        },
                    )],
                    |t| {
                        t.write_expr(t.field_access(t.var("person"), "name"), false);
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                record Address {
                  street: String,
                }
                record Person {
                  name: String,
                  address: Address,
                }
                view Test(person: test::Person) {
                  write_expr(person.name)
                }

                -- after --
                // Code generated by the hop compiler. DO NOT EDIT.
                #![cfg_attr(rustfmt, rustfmt_skip)]
                #![allow(unused_parens, dead_code, clippy::all)]

                pub trait View {
                    fn render(self) -> String;
                }

                #[derive(Clone, Debug)]
                pub struct Address {
                    pub street: String,
                }

                #[derive(Clone, Debug)]
                pub struct Person {
                    pub name: String,
                    pub address: Address,
                }

                pub struct Test {
                    pub person: Person,
                }

                impl View for Test {
                    fn render(self) -> String {
                        let Test { person } = self;
                        let mut output = String::new();
                        output.push_str(&person.name);
                        output
                    }
                }
            "#]],
        );
    }

    #[test]
    fn enum_with_record_field_uses_arc() {
        check(
            IrModuleBuilder::new()
                .record("Address", |r| {
                    r.field("street", Type::String);
                })
                .enum_with_fields("Shape", |e| {
                    e.variant_with_fields(
                        "Located",
                        vec![(
                            "location",
                            Type::Record {
                                module: crate::document_id::DocumentId::new("test.hop").unwrap(),
                                name: crate::symbols::type_name::TypeName::new("Address").unwrap(),
                                fields: vec![(
                                    crate::symbols::field_name::FieldName::new("street").unwrap(),
                                    Arc::new(Type::String),
                                    None,
                                )],
                            },
                        )],
                    );
                    e.variant("Empty");
                })
                .component_no_params("Test", |t| {
                    t.write("hello");
                })
                .build(),
            expect![[r#"
                -- before --
                enum Shape {
                  Located {location: test::Address},
                  Empty,
                }
                record Address {
                  street: String,
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
                pub enum Shape {
                    Located { location: Address },
                    Empty,
                }

                #[derive(Clone, Debug)]
                pub struct Address {
                    pub street: String,
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
    fn record_with_only_primitives_no_arc() {
        check(
            IrModuleBuilder::new()
                .record("Point", |r| {
                    r.field("x", Type::Float);
                    r.field("y", Type::Float);
                })
                .component_no_params("Test", |t| {
                    t.write("hello");
                })
                .build(),
            expect![[r#"
                -- before --
                record Point {
                  x: Float,
                  y: Float,
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
                pub struct Point {
                    pub x: f64,
                    pub y: f64,
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
    fn component_with_enum_param() {
        use crate::dop::Type;
        use crate::dop::patterns::{EnumMatchArm, EnumPattern, Match};
        use crate::ir::ast::{
            IrArgument, IrComponentDeclaration, IrEnumDeclaration, IrExpr, IrModule, IrParameter,
            IrStatement, IrViewDeclaration,
        };
        use crate::symbols::type_name::TypeName;

        let color_type = Arc::new(Type::Enum {
            module: crate::document_id::DocumentId::new("test.hop").unwrap(),
            name: TypeName::new("Color").unwrap(),
            variants: vec![
                EnumVariant {
                    name: TypeName::new("Red").unwrap(),
                    fields: vec![],
                },
                EnumVariant {
                    name: TypeName::new("Green").unwrap(),
                    fields: vec![],
                },
                EnumVariant {
                    name: TypeName::new("Blue").unwrap(),
                    fields: vec![],
                },
            ],
        });

        let color_typename = TypeName::new("Color").unwrap();

        let module = IrModule {
            views: vec![IrViewDeclaration {
                name: TypeName::new("Test").unwrap(),
                parameters: vec![],
                body: vec![IrStatement::ComponentInvocation {
                    id: 1,
                    component_name: TypeName::new("Badge").unwrap(),
                    args: vec![IrArgument {
                        name: VarName::new("color").unwrap(),
                        expr: IrExpr::EnumLiteral {
                            enum_name: color_typename.clone(),
                            variant_name: TypeName::new("Green").unwrap(),
                            fields: vec![],
                            kind: color_type.clone(),
                            id: 2,
                        },
                    }],
                }],
            }],
            components: vec![IrComponentDeclaration {
                name: TypeName::new("Badge").unwrap(),
                parameters: vec![IrParameter {
                    name: VarName::new("color").unwrap(),
                    typ: color_type.clone(),
                    default_value: None,
                }],
                body: vec![IrStatement::Match {
                    id: 10,
                    match_: Match::Enum {
                        subject: Box::new(IrExpr::Var {
                            value: VarName::new("color").unwrap(),
                            kind: color_type.clone(),
                            id: 0,
                        }),
                        arms: vec![
                            EnumMatchArm {
                                pattern: EnumPattern::Variant {
                                    enum_name: color_typename.clone(),
                                    variant_name: TypeName::new("Red").unwrap(),
                                },
                                bindings: vec![],
                                body: vec![IrStatement::Write {
                                    id: 11,
                                    content: "red".to_string(),
                                }],
                            },
                            EnumMatchArm {
                                pattern: EnumPattern::Variant {
                                    enum_name: color_typename.clone(),
                                    variant_name: TypeName::new("Green").unwrap(),
                                },
                                bindings: vec![],
                                body: vec![IrStatement::Write {
                                    id: 12,
                                    content: "green".to_string(),
                                }],
                            },
                            EnumMatchArm {
                                pattern: EnumPattern::Variant {
                                    enum_name: color_typename.clone(),
                                    variant_name: TypeName::new("Blue").unwrap(),
                                },
                                bindings: vec![],
                                body: vec![IrStatement::Write {
                                    id: 13,
                                    content: "blue".to_string(),
                                }],
                            },
                        ],
                    },
                }],
            }],
            records: vec![],
            enums: vec![IrEnumDeclaration {
                name: TypeName::new("Color").unwrap(),
                variants: vec![
                    EnumVariant {
                        name: TypeName::new("Red").unwrap(),
                        fields: vec![],
                    },
                    EnumVariant {
                        name: TypeName::new("Green").unwrap(),
                        fields: vec![],
                    },
                    EnumVariant {
                        name: TypeName::new("Blue").unwrap(),
                        fields: vec![],
                    },
                ],
            }],
        };

        let result = RustTranspiler::new().transpile_module(&module);
        let expected = expect![[r#"
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

            fn render_badge(color: &Color) -> String {
                let mut output = String::new();
                match &color {
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
        "#]];
        expected.assert_eq(&result);
    }

    #[test]
    fn transpiles_let_fragment_as_rust_block() {
        check(
            IrModuleBuilder::new()
                .component_no_params("Test", |t| {
                    t.let_fragment(
                        "v_0",
                        |t| {
                            t.write("<b>hi</b>");
                        },
                        |t| {
                            t.write_expr(t.var("v_0"), false);
                        },
                    );
                })
                .build(),
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
