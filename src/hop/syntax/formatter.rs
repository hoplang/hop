use std::collections::VecDeque;

use super::parsed_ast::{
    ParsedAst, ParsedAttribute, ParsedAttributeValue, ParsedComponentDeclaration,
    ParsedDeclaration, ParsedEnumDeclaration, ParsedEnumDeclarationVariant,
    ParsedImportDeclaration, ParsedParameter, ParsedRecordDeclaration,
    ParsedRecordDeclarationField,
};
use super::parsed_node::{ParsedLetBinding, ParsedMatchCase, ParsedNode};
use crate::common::is_void_element;
use crate::document::{DocumentRange, Ranged};
use crate::dop::syntax::parsed::{
    Constructor, ParsedExpr, ParsedLoopSource, ParsedMatchArm, ParsedMatchPattern, ParsedType,
};
use pretty::{Arena, DocAllocator, DocBuilder};

pub fn format(ast: ParsedAst) -> String {
    let arena = Arena::new();
    format_ast(&ast, &arena).pretty(60).to_string()
}

/// Wraps items in a grouped body with trailing comma that disappear
/// when on single line. Used for arrays, records, match arms, macros, etc.
fn soft_block<'a>(
    arena: &'a Arena<'a>,
    items: DocBuilder<'a, Arena<'a>>,
) -> DocBuilder<'a, Arena<'a>> {
    arena
        .line_()
        .append(items)
        .append(arena.text(",").flat_alt(arena.nil()))
        .append(arena.line_())
        .nest(2)
        .group()
}

fn drain_comments_before<'a>(
    arena: &'a Arena<'a>,
    comments: &mut VecDeque<&'a DocumentRange>,
    position: usize,
) -> DocBuilder<'a, Arena<'a>> {
    let mut doc = arena.nil();
    while let Some(comment) = comments.front() {
        if comment.start() < position {
            let comment = comments.pop_front().unwrap();
            doc = doc
                .append(arena.text(comment.as_str()))
                .append(arena.hardline());
        } else {
            break;
        }
    }
    doc
}

fn format_braced_list<'a, T, F>(
    arena: &'a Arena<'a>,
    items: &'a [T],
    mut format_item: F,
    comments: &mut VecDeque<&'a DocumentRange>,
    end_position: usize,
) -> DocBuilder<'a, Arena<'a>>
where
    F: FnMut(&'a Arena<'a>, &'a T, &mut VecDeque<&'a DocumentRange>) -> DocBuilder<'a, Arena<'a>>,
{
    let has_trailing_comments = comments.front().is_some_and(|c| c.start() < end_position);
    if items.is_empty() && !has_trailing_comments {
        return arena.nil();
    }
    let mut items_doc = arena.nil();
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            items_doc = items_doc.append(arena.text(",")).append(arena.line());
        }
        items_doc = items_doc.append(format_item(arena, item, comments));
    }
    if !items.is_empty() {
        items_doc = items_doc.append(arena.text(","));
    }
    let trailing_comments = drain_comments_before(arena, comments, end_position);
    let content = if items.is_empty() {
        trailing_comments
    } else if has_trailing_comments {
        items_doc.append(arena.line()).append(trailing_comments)
    } else {
        items_doc
    };
    let body = arena.line().append(content).nest(2);
    if has_trailing_comments {
        body
    } else {
        body.append(arena.line())
    }
}

fn format_ast<'a>(ast: &'a ParsedAst, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
    let declarations = ast.get_declarations();
    let mut comments: VecDeque<_> = ast.comments().iter().collect();
    if declarations.is_empty() {
        arena.nil()
    } else {
        let mut doc = arena.nil();
        let mut prev_was_import = false;
        for (i, decl) in declarations.iter().enumerate() {
            if i > 0 {
                doc = doc.append(arena.line());
                let curr_is_import = matches!(decl, ParsedDeclaration::Import(_));
                if !(prev_was_import && curr_is_import) {
                    doc = doc.append(arena.line());
                }
            }
            doc = doc.append(format_declaration(arena, decl, &mut comments));
            prev_was_import = matches!(decl, ParsedDeclaration::Import(_));
        }
        doc.append(arena.line())
    }
}

fn format_declaration<'a>(
    arena: &'a Arena<'a>,
    decl: &'a ParsedDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match decl {
        ParsedDeclaration::Import(import) => format_import_declaration(arena, import, comments),
        ParsedDeclaration::Record(record) => format_record_declaration(arena, record, comments),
        ParsedDeclaration::Enum(e) => format_enum_declaration(arena, e, comments),
        ParsedDeclaration::Component(component) => {
            format_component_declaration(arena, component, comments)
        }
    }
}

fn format_import_declaration<'a>(
    arena: &'a Arena<'a>,
    import: &'a ParsedImportDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, import.path.start());
    leading_comments
        .append(arena.text("import"))
        .append(arena.space())
        .append(arena.text(import.module_name.to_string()))
        .append(arena.text("::"))
        .append(arena.text(import.type_name.as_str()))
}

fn format_record_declaration<'a>(
    arena: &'a Arena<'a>,
    record: &'a ParsedRecordDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, record.name_range.start());
    leading_comments
        .append(arena.text("record"))
        .append(arena.space())
        .append(arena.text(record.name.as_str()))
        .append(arena.space())
        .append(arena.text("{"))
        .append(format_braced_list(
            arena,
            &record.fields,
            format_record_declaration_field,
            comments,
            record.range.end(),
        ))
        .append(arena.text("}"))
}

fn format_record_declaration_field<'a>(
    arena: &'a Arena<'a>,
    field: &'a ParsedRecordDeclarationField,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, field.name_range.start());
    leading_comments
        .append(arena.text(field.name.as_str()))
        .append(arena.text(": "))
        .append(format_type(arena, &field.field_type))
}

fn format_enum_declaration<'a>(
    arena: &'a Arena<'a>,
    e: &'a ParsedEnumDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, e.name_range.start());
    leading_comments
        .append(arena.text("enum"))
        .append(arena.space())
        .append(arena.text(e.name.as_str()))
        .append(arena.space())
        .append(arena.text("{"))
        .append(format_braced_list(
            arena,
            &e.variants,
            format_enum_declaration_variant,
            comments,
            e.range.end(),
        ))
        .append(arena.text("}"))
}

fn format_enum_declaration_variant<'a>(
    arena: &'a Arena<'a>,
    variant: &'a ParsedEnumDeclarationVariant,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, variant.name_range.start());
    if variant.fields.is_empty() {
        leading_comments.append(arena.text(variant.name.as_str()))
    } else {
        leading_comments
            .append(arena.text(variant.name.as_str()))
            .append(arena.text("("))
            .append(arena.intersperse(
                variant.fields.iter().map(|(field_name, _, field_type)| {
                    arena
                        .text(field_name.to_string())
                        .append(arena.text(": "))
                        .append(format_type(arena, field_type))
                }),
                arena.text(", "),
            ))
            .append(arena.text(")"))
    }
}

fn format_component_declaration<'a>(
    arena: &'a Arena<'a>,
    component: &'a ParsedComponentDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, component.range.start());
    let params_doc = match &component.params {
        Some((params, params_range)) => {
            let mut params_inner = arena.nil();
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    params_inner = params_inner.append(arena.text(",")).append(arena.line());
                }
                params_inner = params_inner.append(format_parameter(arena, param, comments));
            }
            let has_trailing_comments = comments
                .front()
                .is_some_and(|c| c.start() < params_range.end());
            let trailing_comments = drain_comments_before(arena, comments, params_range.end());
            if params.is_empty() && !has_trailing_comments {
                arena.nil()
            } else {
                let body = if params.is_empty() {
                    arena.line_().append(trailing_comments).nest(2)
                } else if has_trailing_comments {
                    arena
                        .line_()
                        .append(params_inner)
                        .append(arena.text(","))
                        .append(arena.line())
                        .append(trailing_comments)
                        .nest(2)
                } else {
                    arena
                        .line_()
                        .append(params_inner)
                        .append(arena.text(",").flat_alt(arena.nil()))
                        .nest(2)
                        .append(arena.line_())
                };
                arena
                    .text(" {")
                    .append(body)
                    .append(arena.text("}"))
                    .group()
            }
        }
        None => arena.nil(),
    };

    leading_comments
        .append(arena.text("<"))
        .append(arena.text(component.component_name.as_str()))
        .append(params_doc)
        .append(arena.text(">"))
        .append(format_children(arena, &component.children, comments))
        .append(arena.text("</"))
        .append(arena.text(component.component_name.as_str()))
        .append(arena.text(">"))
}

fn format_parameter<'a>(
    arena: &'a Arena<'a>,
    param: &'a ParsedParameter,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, param.var_name_range.start());
    let base = arena
        .text(param.var_name.as_str())
        .append(arena.text(": "))
        .append(format_type(arena, &param.var_type));
    let param_doc = match &param.default_value {
        Some(default) => base
            .append(arena.text(" = "))
            .append(format_expr(arena, default, comments)),
        None => base,
    };
    leading_comments.append(param_doc)
}

fn format_attribute<'a>(
    arena: &'a Arena<'a>,
    attr: &'a ParsedAttribute,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let name_doc = arena.text(attr.name.as_str());
    match &attr.value {
        Some(value) => name_doc
            .append(arena.text("="))
            .append(format_attribute_value(arena, value, comments)),
        None => name_doc,
    }
}

fn format_attribute_value<'a>(
    arena: &'a Arena<'a>,
    value: &'a ParsedAttributeValue,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match value {
        ParsedAttributeValue::Expression(expr) => arena
            .text("{")
            .append(
                arena
                    .line_()
                    .append(format_expr(arena, expr, comments))
                    .nest(2),
            )
            .append(arena.line_())
            .append(arena.text("}"))
            .group(),
        ParsedAttributeValue::String(range) => {
            let content = range.as_ref().map(|r| r.as_str()).unwrap_or("");
            arena
                .text("\"")
                .append(arena.text(content))
                .append(arena.text("\""))
        }
    }
}

fn format_node<'a>(
    arena: &'a Arena<'a>,
    node: &'a ParsedNode,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match node {
        ParsedNode::Text { range } => arena.text(range.as_str()),
        // Newline nodes are handled by format_children (they signal where to break).
        // This case is here for completeness but shouldn't be reached in normal formatting.
        ParsedNode::Newline { .. } => arena.nil(),
        ParsedNode::TextExpression { expression, .. } => arena
            .text("{")
            .append(format_expr(arena, expression, comments))
            .append(arena.text("}")),
        ParsedNode::ComponentReference {
            component_name,
            args,
            children,
            ..
        } => {
            let component_name_str = component_name.as_str();
            let opening_tag_doc = if args.is_empty() {
                arena.text("<").append(arena.text(component_name_str))
            } else if args.len() == 1 {
                // Single attribute: keep on same line as tag
                arena
                    .text("<")
                    .append(arena.text(component_name_str))
                    .append(arena.text(" "))
                    .append(format_attribute(arena, &args[0], comments))
            } else {
                let mut args_doc = arena.nil();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        args_doc = args_doc.append(arena.line());
                    }
                    args_doc = args_doc.append(format_attribute(arena, arg, comments));
                }
                arena
                    .text("<")
                    .append(arena.text(component_name_str))
                    .append(arena.line().append(args_doc).nest(2))
                    .append(arena.line_())
                    .group()
            };
            if children.is_empty() {
                // Empty or only whitespace/newlines - use self-closing tag
                opening_tag_doc.append(arena.text("/>"))
            } else {
                opening_tag_doc
                    .append(arena.text(">"))
                    .append(format_children(arena, children, comments))
                    .append(arena.text("</"))
                    .append(arena.text(component_name_str))
                    .append(arena.text(">"))
            }
        }
        ParsedNode::If {
            condition,
            children,
            ..
        } => {
            let children_doc = format_children(arena, children, comments);
            arena
                .text("<if {")
                .append(format_expr(arena, condition, comments))
                .append(arena.text("}>"))
                .append(children_doc)
                .append(arena.text("</if>"))
        }
        ParsedNode::For {
            var_name,
            source,
            children,
            ..
        } => {
            let children_doc = format_children(arena, children, comments);
            let source_doc = match source {
                ParsedLoopSource::Array(expr) => format_expr(arena, expr, comments),
                ParsedLoopSource::RangeInclusive { start, end } => {
                    format_expr(arena, start, comments)
                        .append(arena.text("..="))
                        .append(format_expr(arena, end, comments))
                }
            };
            let var_doc = match var_name {
                Some(name) => arena.text(name.as_str()),
                None => arena.text("_"),
            };
            arena
                .text("<for {")
                .append(var_doc)
                .append(arena.text(" in "))
                .append(source_doc)
                .append(arena.text("}>"))
                .append(children_doc)
                .append(arena.text("</for>"))
        }
        ParsedNode::Let {
            bindings,
            bindings_range,
            children,
            ..
        } => {
            let end_position = bindings_range.end();
            let has_trailing_comments = comments.front().is_some_and(|c| c.start() < end_position);

            let mut bindings_doc = arena.nil();
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    bindings_doc = bindings_doc.append(arena.text(",")).append(arena.line());
                }
                bindings_doc = bindings_doc.append(format_let_binding(arena, binding, comments));
            }

            let trailing_comments = drain_comments_before(arena, comments, end_position);

            let body = if bindings.is_empty() && !has_trailing_comments {
                arena.nil()
            } else if bindings.is_empty() {
                arena.line_().append(trailing_comments).nest(2)
            } else if has_trailing_comments {
                arena
                    .line_()
                    .append(bindings_doc)
                    .append(arena.text(","))
                    .append(arena.line())
                    .append(trailing_comments)
                    .nest(2)
            } else {
                soft_block(arena, bindings_doc)
            };

            let children_doc = format_children(arena, children, comments);
            arena
                .text("<let {")
                .append(body)
                .append(arena.text("}>"))
                .append(children_doc)
                .append(arena.text("</let>"))
        }
        ParsedNode::Doctype { value, .. } => arena.text(value.as_str()),
        ParsedNode::Match { subject, cases, .. } => {
            let cases_doc = if cases.is_empty() {
                arena.nil()
            } else {
                let mut doc = arena.nil();
                for (i, case) in cases.iter().enumerate() {
                    if i > 0 {
                        doc = doc.append(arena.line());
                    }
                    doc = doc.append(format_match_case(arena, case, comments));
                }
                arena.line().append(doc).nest(2).append(arena.line())
            };
            arena
                .text("<match {")
                .append(format_expr(arena, subject, comments))
                .append(arena.text("}>"))
                .append(cases_doc)
                .append(arena.text("</match>"))
        }
        ParsedNode::Html {
            tag_name,
            attributes,
            children,
            ..
        } => {
            let tag_name_str = tag_name.as_str();
            let opening_tag_doc = if attributes.is_empty() {
                arena
                    .text("<")
                    .append(arena.text(tag_name_str))
                    .append(arena.text(">"))
            } else if attributes.len() == 1 {
                // Single attribute: keep on same line as tag
                arena
                    .text("<")
                    .append(arena.text(tag_name_str))
                    .append(arena.text(" "))
                    .append(format_attribute(arena, &attributes[0], comments))
                    .append(arena.text(">"))
            } else {
                let mut attrs_doc = arena.nil();
                for (i, attr) in attributes.iter().enumerate() {
                    if i > 0 {
                        attrs_doc = attrs_doc.append(arena.line());
                    }
                    attrs_doc = attrs_doc.append(format_attribute(arena, attr, comments));
                }
                arena
                    .text("<")
                    .append(arena.text(tag_name_str))
                    .append(arena.line().append(attrs_doc).nest(2))
                    .append(arena.line_())
                    .append(arena.text(">"))
                    .group()
            };

            if is_void_element(tag_name_str) {
                opening_tag_doc
            } else if children.is_empty() {
                // Empty or only whitespace/newlines - compact closing tag
                opening_tag_doc
                    .append(arena.text("</"))
                    .append(arena.text(tag_name_str))
                    .append(arena.text(">"))
            } else {
                opening_tag_doc
                    .append(format_children(arena, children, comments))
                    .append(arena.text("</"))
                    .append(arena.text(tag_name_str))
                    .append(arena.text(">"))
            }
        }
    }
}

/// Returns true if a node is "inline" content (text or expression).
/// Inline nodes can stay on the same line unless separated by a Newline.
fn is_inline(node: &ParsedNode) -> bool {
    matches!(
        node,
        ParsedNode::Text { .. } | ParsedNode::TextExpression { .. }
    )
}

fn format_children<'a>(
    arena: &'a Arena<'a>,
    children: &'a [ParsedNode],
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    if children.is_empty() {
        return arena.nil();
    }

    // Collect non-Newline nodes with their "preceded by newline" flag.
    // Newline nodes signal where to insert line breaks between inline content.
    let mut items: Vec<(bool, &ParsedNode)> = Vec::new();
    let mut prev_was_newline = false;

    for child in children {
        match child {
            ParsedNode::Newline { .. } => {
                prev_was_newline = true;
            }
            _ => {
                items.push((prev_was_newline, child));
                prev_was_newline = false;
            }
        }
    }

    if items.is_empty() {
        return arena.nil();
    }

    let mut doc = arena.nil();

    for i in 0..items.len() {
        let (preceded_by_newline, child) = items[i];
        let prev_node = if i > 0 { Some(items[i - 1].1) } else { None };
        let next_node = items.get(i + 1).map(|(_, n)| *n);

        // Decide separator: line break if preceded by newline or adjacent to block element
        let need_break = if let Some(prev) = prev_node {
            preceded_by_newline || !is_inline(prev) || !is_inline(child)
        } else {
            false
        };

        if need_break {
            doc = doc.append(arena.line());
        }

        // Special handling for Text nodes: convert whitespace adjacent to blocks to {" "}
        if let ParsedNode::Text { range } = child {
            let text = range.as_str();
            let has_leading_ws = text.starts_with(|c: char| c.is_whitespace());
            let has_trailing_ws = text.ends_with(|c: char| c.is_whitespace());
            let prev_is_block = prev_node.is_some_and(|n| !is_inline(n));
            let next_is_block = next_node.is_some_and(|n| !is_inline(n));

            // Determine if we need to convert whitespace to {" "}
            // Leading: if prev is block and we inserted a break (not from original newline)
            let leading_needs_space = has_leading_ws && prev_is_block && !preceded_by_newline;
            // Trailing: if next is block
            let trailing_needs_space = has_trailing_ws && next_is_block;

            if leading_needs_space || trailing_needs_space {
                let trimmed = text.trim();
                if trimmed.is_empty() {
                    // Pure whitespace - output single {" "}
                    doc = doc.append(arena.text("{\" \"}"));
                } else {
                    if leading_needs_space {
                        doc = doc.append(arena.text("{\" \"}"));
                    }
                    doc = doc.append(arena.text(trimmed));
                    if trailing_needs_space {
                        doc = doc.append(arena.text("{\" \"}"));
                    }
                }
            } else {
                doc = doc.append(arena.text(text));
            }
        } else {
            doc = doc.append(format_node(arena, child, comments));
        }
    }

    arena.line().append(doc).nest(2).append(arena.line())
}

fn format_match_case<'a>(
    arena: &'a Arena<'a>,
    case: &'a ParsedMatchCase,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let children_doc = format_children(arena, &case.children, comments);
    arena
        .text("<case {")
        .append(format_match_pattern(arena, &case.pattern))
        .append(arena.text("}>"))
        .append(children_doc)
        .append(arena.text("</case>"))
}

fn format_let_binding<'a>(
    arena: &'a Arena<'a>,
    binding: &'a ParsedLetBinding,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, binding.var_name_range.start());
    leading_comments
        .append(arena.text(binding.var_name.as_str()))
        .append(arena.text(": "))
        .append(format_type(arena, &binding.var_type))
        .append(arena.text(" = "))
        .append(format_expr(arena, &binding.value_expr, comments))
}

fn format_type<'a>(arena: &'a Arena<'a>, ty: &ParsedType) -> DocBuilder<'a, Arena<'a>> {
    match ty {
        ParsedType::String { .. } => arena.text("String"),
        ParsedType::Bool { .. } => arena.text("Bool"),
        ParsedType::Int { .. } => arena.text("Int"),
        ParsedType::Float { .. } => arena.text("Float"),
        ParsedType::TrustedHTML { .. } => arena.text("TrustedHTML"),
        ParsedType::Option { element, .. } => arena
            .text("Option[")
            .append(format_type(arena, element))
            .append(arena.text("]")),
        ParsedType::Array { element, .. } => arena
            .text("Array[")
            .append(format_type(arena, element))
            .append(arena.text("]")),
        ParsedType::Named { name, .. } => arena.text(name.to_string()),
    }
}

fn format_expr<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match expr {
        ParsedExpr::Var { value, .. } => arena.text(value.as_str()),
        expr @ ParsedExpr::FieldAccess {
            record: object,
            field,
            ..
        } => format_expr_with_precedence(arena, object, expr.precedence(), comments)
            .append(arena.text("."))
            .append(arena.text(field.as_str())),
        expr @ ParsedExpr::MethodCall {
            receiver, method, ..
        } => format_expr_with_precedence(arena, receiver, expr.precedence(), comments)
            .append(arena.text("."))
            .append(arena.text(method.as_str()))
            .append(arena.text("()")),
        ParsedExpr::StringLiteral { value, .. } => arena
            .text("\"")
            .append(arena.text(value.as_str()))
            .append(arena.text("\"")),
        ParsedExpr::BooleanLiteral { range, .. } => arena.text(range.as_str()),
        ParsedExpr::IntLiteral { range, .. } => arena.text(range.as_str()),
        ParsedExpr::FloatLiteral { range, .. } => arena.text(range.as_str()),
        ParsedExpr::ArrayLiteral { elements, .. } => {
            if elements.is_empty() {
                arena.text("[]")
            } else {
                let mut elements_doc = arena.nil();
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        elements_doc = elements_doc.append(arena.text(",")).append(arena.line());
                    }
                    elements_doc = elements_doc.append(format_expr(arena, elem, comments));
                }
                arena
                    .text("[")
                    .append(soft_block(arena, elements_doc))
                    .append(arena.text("]"))
            }
        }
        ParsedExpr::RecordLiteral {
            record_name,
            fields,
            ..
        } => {
            if fields.is_empty() {
                arena.text(record_name.as_str()).append(arena.text("()"))
            } else {
                let mut fields_doc = arena.nil();
                for (i, (key, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
                    }
                    fields_doc = fields_doc
                        .append(arena.text(key.as_str()))
                        .append(arena.text(": "))
                        .append(format_expr(arena, value, comments));
                }
                arena
                    .text(record_name.as_str())
                    .append(arena.text("("))
                    .append(soft_block(arena, fields_doc))
                    .append(arena.text(")"))
            }
        }
        expr @ ParsedExpr::BinaryOp {
            left,
            operator,
            right,
            ..
        } => {
            let prec = expr.precedence();
            format_expr_with_precedence(arena, left, prec, comments)
                .append(arena.text(" "))
                .append(arena.text(operator.as_str()))
                .append(arena.text(" "))
                .append(format_expr_with_precedence(arena, right, prec, comments))
        }
        expr @ ParsedExpr::Negation { operand, .. } => arena.text("!").append(
            format_expr_with_precedence(arena, operand, expr.precedence(), comments),
        ),
        expr @ ParsedExpr::NumericNegation { operand, .. } => arena.text("-").append(
            format_expr_with_precedence(arena, operand, expr.precedence(), comments),
        ),
        ParsedExpr::EnumLiteral {
            enum_name,
            variant_name,
            fields,
            ..
        } => {
            let base = arena
                .text(enum_name.as_str())
                .append(arena.text("::"))
                .append(arena.text(variant_name.as_str()));
            if fields.is_empty() {
                base
            } else {
                let mut fields_doc = arena.nil();
                for (i, (field_name, _, field_value)) in fields.iter().enumerate() {
                    if i > 0 {
                        fields_doc = fields_doc.append(arena.text(", "));
                    }
                    fields_doc = fields_doc
                        .append(arena.text(field_name.to_string()))
                        .append(arena.text(": "))
                        .append(format_expr(arena, field_value, comments));
                }
                base.append(arena.text("("))
                    .append(fields_doc)
                    .append(arena.text(")"))
            }
        }
        ParsedExpr::Match { subject, arms, .. } => {
            let end_position = expr.range().end();
            let has_trailing_comments = comments.front().is_some_and(|c| c.start() < end_position);

            if arms.is_empty() && !has_trailing_comments {
                arena
                    .text("match ")
                    .append(format_expr(arena, subject, comments))
                    .append(arena.text(" {}"))
            } else if arms.is_empty() {
                let trailing_comments = drain_comments_before(arena, comments, end_position);
                arena
                    .text("match ")
                    .append(format_expr(arena, subject, comments))
                    .append(arena.text(" {"))
                    .append(arena.line_().append(trailing_comments).nest(2))
                    .append(arena.text("}"))
            } else {
                let mut arms_doc = arena.nil();
                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        arms_doc = arms_doc.append(arena.text(",")).append(arena.line());
                    }
                    arms_doc = arms_doc.append(format_match_arm(arena, arm, comments));
                }

                let trailing_comments = drain_comments_before(arena, comments, end_position);

                let body = if has_trailing_comments {
                    arena
                        .line_()
                        .append(arms_doc)
                        .append(arena.text(","))
                        .append(arena.line())
                        .append(trailing_comments)
                        .nest(2)
                } else {
                    soft_block(arena, arms_doc)
                };

                arena
                    .text("match ")
                    .append(format_expr(arena, subject, comments))
                    .append(arena.text(" {"))
                    .append(body)
                    .append(arena.text("}"))
            }
        }
        ParsedExpr::OptionLiteral { value, .. } => match value {
            Some(inner) => arena
                .text("Some(")
                .append(
                    arena
                        .line_()
                        .append(format_expr(arena, inner, comments))
                        .nest(2),
                )
                .append(arena.line_())
                .append(arena.text(")"))
                .group(),
            None => arena.text("None"),
        },
        ParsedExpr::MacroInvocation { name, args, .. } => {
            let mut expanded_docs: Vec<DocBuilder<'a, Arena<'a>>> = Vec::new();
            if name == "classes" {
                for e in args.iter() {
                    let leading_comments =
                        drain_comments_before(arena, comments, e.range().start());
                    match e {
                        ParsedExpr::StringLiteral { value, .. } => {
                            let parts: Vec<_> = value.split_whitespace().collect();
                            for (i, part) in parts.iter().enumerate() {
                                let quoted = arena
                                    .text("\"")
                                    .append(arena.text(*part))
                                    .append(arena.text("\""));
                                if i == 0 {
                                    expanded_docs.push(leading_comments.clone().append(quoted));
                                } else {
                                    expanded_docs.push(quoted);
                                }
                            }
                        }
                        _ => expanded_docs
                            .push(leading_comments.append(format_expr(arena, e, comments))),
                    }
                }
            } else {
                for e in args.iter() {
                    let leading_comments =
                        drain_comments_before(arena, comments, e.range().start());
                    expanded_docs.push(leading_comments.append(format_expr(arena, e, comments)));
                }
            }

            let end_position = expr.range().end();
            let has_trailing_comments = comments.front().is_some_and(|c| c.start() < end_position);
            let trailing_comments = drain_comments_before(arena, comments, end_position);

            if expanded_docs.is_empty() && !has_trailing_comments {
                arena.text(name.as_str()).append(arena.text("!()"))
            } else if expanded_docs.is_empty() {
                arena
                    .text(name.as_str())
                    .append(arena.text("!("))
                    .append(arena.line_().append(trailing_comments).nest(2))
                    .append(arena.text(")"))
            } else {
                let mut args_doc = arena.nil();
                for (i, doc) in expanded_docs.into_iter().enumerate() {
                    if i > 0 {
                        args_doc = args_doc.append(arena.text(",")).append(arena.line());
                    }
                    args_doc = args_doc.append(doc);
                }

                let body = if has_trailing_comments {
                    arena
                        .line_()
                        .append(args_doc)
                        .append(arena.text(","))
                        .append(arena.line())
                        .append(trailing_comments)
                        .nest(2)
                        .group()
                } else {
                    soft_block(arena, args_doc)
                };

                arena
                    .text(name.as_str())
                    .append(arena.text("!("))
                    .append(body)
                    .append(arena.text(")"))
            }
        }
    }
}

fn format_expr_with_precedence<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    parent_precedence: u8,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    if expr.precedence() < parent_precedence {
        arena
            .text("(")
            .append(format_expr(arena, expr, comments))
            .append(arena.text(")"))
    } else {
        format_expr(arena, expr, comments)
    }
}

fn format_match_arm<'a>(
    arena: &'a Arena<'a>,
    arm: &'a ParsedMatchArm,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, arm.pattern.range().start());
    leading_comments
        .append(format_match_pattern(arena, &arm.pattern))
        .append(arena.text(" => "))
        .append(format_expr(arena, &arm.body, comments))
}

fn format_match_pattern<'a>(
    arena: &'a Arena<'a>,
    pattern: &'a ParsedMatchPattern,
) -> DocBuilder<'a, Arena<'a>> {
    match pattern {
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            fields,
            ..
        } => {
            let base = format_constructor(arena, constructor);
            if !fields.is_empty() {
                let fields_doc = arena.intersperse(
                    fields.iter().map(|(name, _, pat)| {
                        arena
                            .text(name.as_str())
                            .append(arena.text(": "))
                            .append(format_match_pattern(arena, pat))
                    }),
                    arena.text(", "),
                );
                base.append(arena.text("("))
                    .append(fields_doc)
                    .append(arena.text(")"))
            } else if args.is_empty() {
                base
            } else {
                let args_doc = arena.intersperse(
                    args.iter().map(|p| format_match_pattern(arena, p)),
                    arena.text(", "),
                );
                base.append(arena.text("("))
                    .append(args_doc)
                    .append(arena.text(")"))
            }
        }
        ParsedMatchPattern::Wildcard { .. } => arena.text("_"),
        ParsedMatchPattern::Binding { name, .. } => arena.text(name.as_str()),
    }
}

fn format_constructor<'a>(
    arena: &'a Arena<'a>,
    constructor: &'a Constructor,
) -> DocBuilder<'a, Arena<'a>> {
    match constructor {
        Constructor::EnumVariant {
            enum_name,
            variant_name,
        } => arena
            .text(enum_name.as_str())
            .append(arena.text("::"))
            .append(arena.text(variant_name.as_str())),
        Constructor::BooleanTrue => arena.text("true"),
        Constructor::BooleanFalse => arena.text("false"),
        Constructor::OptionSome => arena.text("Some"),
        Constructor::OptionNone => arena.text("None"),
        Constructor::Record { type_name } => arena.text(type_name.as_str()),
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::document::Document;
    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            Document::new(source.to_string()),
            &mut errors,
        );
        if !errors.is_empty() {
            panic!("Parse errors: {:?}", errors);
        }
        let formatted = super::format(ast);
        expected.assert_eq(&formatted);

        // Check idempotency: formatting the output should give the same result
        let formatted_twice = super::format(parser::parse(
            ModuleName::new("test").unwrap(),
            Document::new(formatted.clone()),
            &mut errors,
        ));
        assert_eq!(formatted, formatted_twice, "Formatter is not idempotent");
    }

    #[test]
    fn import_declaration_to_doc() {
        check(
            indoc! {"
                import foo::Bar
            "},
            expect![[r#"
                import foo::Bar
            "#]],
        );
    }

    #[test]
    fn multiple_import_declarations_to_doc() {
        check(
            indoc! {"
                import foo::Bar
                import baz::Qux
                import components::Button
                record User { name: String }
            "},
            expect![[r#"
                import foo::Bar
                import baz::Qux
                import components::Button

                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn record_declaration_single_field_to_doc() {
        check(
            indoc! {"
                record User { name: String }
            "},
            expect![[r#"
                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn enum_declaration_multiple_variants_to_doc() {
        check(
            indoc! {"
                enum Color { Red, Green, Blue }
            "},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
            "#]],
        );
    }

    #[test]
    fn two_record_declarations_to_doc() {
        check(
            indoc! {"
                record User { name: String, age: Int }
                record Post { title: String, author: User }
            "},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                record Post {
                  title: String,
                  author: User,
                }
            "#]],
        );
    }

    #[test]
    fn component_declaration_to_doc() {
        check(
            indoc! {"
                <Main {name: String, count: Int}>
                  <div>{name}</div>
                </Main>
            "},
            expect![[r#"
                <Main {name: String, count: Int}>
                  <div>
                    {name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_declaration_with_many_parameters_to_doc() {
        check(
            indoc! {"
                <Main {first_name: String, last_name: String, email: String, age: Int, active: Bool, role: String}></Main>
            "},
            expect![[r#"
                <Main {
                  first_name: String,
                  last_name: String,
                  email: String,
                  age: Int,
                  active: Bool,
                  role: String,
                }></Main>
            "#]],
        );
    }

    #[test]
    fn component_with_match_expression_to_doc() {
        check(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                <Main {color: Color}>
                  <div class={match color { Color::Red => "red", Color::Green => "green", Color::Blue => "blue" }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {color: Color}>
                  <div class={
                    match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }
                  }></div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_empty_parens_omits_parens() {
        // Empty parens in enum patterns should be normalized away
        check(
            indoc! {r#"
                enum Color { Red }
                <Main {color: Color}>
                  <div class={match color { Color::Red() => "red" }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                }

                <Main {color: Color}>
                  <div class={match color {Color::Red => "red"}}></div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn enum_declaration_with_empty_parens_omits_parens() {
        // Empty parens in enum declarations should be normalized away
        check(
            indoc! {r#"
                enum Foo { Bar() }
                <Main></Main>
            "#},
            expect![[r#"
                enum Foo {
                  Bar,
                }

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn component_declaration_with_text_child_to_doc() {
        check(
            indoc! {"
                <Main>hello</Main>
            "},
            expect![[r#"
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn html_with_class_and_expression_to_doc() {
        check(
            indoc! {r#"
                record Character { name: String }
                <Main {character: Character}>
                  <h1 class="text-2xl font-bold">{character.name}</h1>
                </Main>
            "#},
            expect![[r#"
                record Character {
                  name: String,
                }

                <Main {character: Character}>
                  <h1 class="text-2xl font-bold">
                    {character.name}
                  </h1>
                </Main>
            "#]],
        );
    }

    #[test]
    fn html_with_single_class_expression_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <div class={"p-2"}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div class={"p-2"}></div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_equality_condition_to_doc() {
        check(
            indoc! {"
                <Main {a: String, b: String}>
                  <if {a == b}>
                    <div>equal</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {a: String, b: String}>
                  <if {a == b}>
                    <div>
                      equal
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_logical_and_condition_to_doc() {
        check(
            indoc! {"
                <Main {a: Bool, b: Bool}>
                  <if {a && b}>
                    <div>both true</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {a: Bool, b: Bool}>
                  <if {a && b}>
                    <div>
                      both true
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_nested_logical_operators_to_doc() {
        check(
            indoc! {"
                <Main {a: Bool, b: Bool, c: Bool}>
                  <if {a && b || c}>
                    <div>complex</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {a: Bool, b: Bool, c: Bool}>
                  <if {a && b || c}>
                    <div>
                      complex
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_negation_to_doc() {
        check(
            indoc! {"
                <Main {a: Bool}>
                  <if {!a}>
                    <div>not a</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {a: Bool}>
                  <if {!a}>
                    <div>
                      not a
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn if_with_negated_equality_to_doc() {
        check(
            indoc! {"
                <Main {a: String, b: String}>
                  <if {!(a == b)}>
                    <div>not equal</div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {a: String, b: String}>
                  <if {!(a == b)}>
                    <div>
                      not equal
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_multiline_text() {
        check(
            indoc! {"
                <Main>
                  hello
                  world
                </Main>
            "},
            expect![[r#"
                <Main>
                  hello
                  world
                </Main>
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_nested_html() {
        check(
            indoc! {"
                <Main>
                  <div>
                    content
                  </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    content
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_empty_lines() {
        check(
            indoc! {"
                <Main>

                  hello

                </Main>
            "},
            expect![[r#"
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_components_with_record_attributes_to_doc() {
        check(
            indoc! {r#"
                <IconsPage>
                  <div class="flex">
                    <div class="border-r max-w-80 h-screen">
                      <Sidebar />
                    </div>
                    <div class="flex gap-4 p-8">
                      <IconItem id="radix-icons" title="Radix Icons" img_src="/img/iphone.jpg" description="A crisp set of 15x15 icons." />
                    </div>
                  </div>
                </IconsPage>
            "#},
            expect![[r#"
                <IconsPage>
                  <div class="flex">
                    <div class="border-r max-w-80 h-screen">
                      <Sidebar/>
                    </div>
                    <div class="flex gap-4 p-8">
                      <IconItem
                        id="radix-icons"
                        title="Radix Icons"
                        img_src="/img/iphone.jpg"
                        description="A crisp set of 15x15 icons."
                      />
                    </div>
                  </div>
                </IconsPage>
            "#]],
        );
    }

    #[test]
    fn component_with_expression_attribute_to_doc() {
        check(
            indoc! {r#"
                import hop::ui::lucide::ChevronDown

                <NativeSelect>
                  <ChevronDown
                    class={"text-muted-foreground"}
                  />
                </NativeSelect>
            "#},
            expect![[r#"
                import hop::ui::lucide::ChevronDown

                <NativeSelect>
                  <ChevronDown class={"text-muted-foreground"}/>
                </NativeSelect>
            "#]],
        );
    }

    #[test]
    fn component_with_string_concatenation_attribute_to_doc() {
        check(
            indoc! {r#"
                record Product { id: String }
                <IconShowPage {product: Product}>
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                </IconShowPage>
            "#},
            expect![[r#"
                record Product {
                  id: String,
                }

                <IconShowPage {product: Product}>
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                </IconShowPage>
            "#]],
        );
    }

    #[test]
    fn component_with_default_string_parameter_to_doc() {
        check(
            indoc! {r#"
                <Greeting {name: String = "World"}>
                  Hello, {name}!
                </Greeting>
            "#},
            expect![[r#"
                <Greeting {name: String = "World"}>
                  Hello, {name}!
                </Greeting>
            "#]],
        );
    }

    #[test]
    fn component_with_default_int_parameter_to_doc() {
        check(
            indoc! {"
                <Counter {count: Int = 0}>
                  {count}
                </Counter>
            "},
            expect![[r#"
                <Counter {count: Int = 0}>
                  {count}
                </Counter>
            "#]],
        );
    }

    #[test]
    fn component_with_default_bool_parameter_to_doc() {
        check(
            indoc! {"
                <Toggle {enabled: Bool = true}>
                </Toggle>
            "},
            expect![[r#"
                <Toggle {enabled: Bool = true}></Toggle>
            "#]],
        );
    }

    #[test]
    fn component_with_mixed_required_and_default_parameters_to_doc() {
        check(
            indoc! {r#"
                <UserCard {name: String, role: String = "user", active: Bool = true}>
                  {name}
                </UserCard>
            "#},
            expect![[r#"
                <UserCard {
                  name: String,
                  role: String = "user",
                  active: Bool = true,
                }>
                  {name}
                </UserCard>
            "#]],
        );
    }

    #[test]
    fn component_with_default_array_parameter_to_doc() {
        check(
            indoc! {r#"
                <ItemList {items: Array[String] = ["one", "two"]}>
                </ItemList>
            "#},
            expect![[r#"
                <ItemList {
                  items: Array[String] = ["one", "two"],
                }></ItemList>
            "#]],
        );
    }

    #[test]
    fn component_with_default_empty_array_parameter_to_doc() {
        check(
            indoc! {"
                <ItemList {items: Array[String] = []}>
                </ItemList>
            "},
            expect![[r#"
                <ItemList {items: Array[String] = []}></ItemList>
            "#]],
        );
    }

    #[test]
    fn component_with_default_record_parameter_to_doc() {
        check(
            indoc! {r#"
                record Config { debug: Bool, timeout: Int }
                <Settings {config: Config = Config(debug: false, timeout: 30)}>
                </Settings>
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                <Settings {
                  config: Config = Config(debug: false, timeout: 30),
                }></Settings>
            "#]],
        );
    }

    #[test]
    fn component_with_default_enum_parameter_to_doc() {
        check(
            indoc! {"
                enum Status { Active, Inactive, Pending }
                <Badge {status: Status = Status::Active}>
                </Badge>
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                <Badge {status: Status = Status::Active}></Badge>
            "#]],
        );
    }

    #[test]
    fn some_literal_stays_on_one_line_when_short() {
        check(
            indoc! {r#"
                <Main>
                  <let {x: Option[String] = Some("short")}></let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {x: Option[String] = Some("short")}></let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn some_literal_inserts_soft_lines_when_long() {
        check(
            indoc! {r#"
                <Main {x: Option[String] = Some("this is a very long string that causes a line break because Some uses soft lines")}>
                </Main>
            "#},
            expect![[r#"
                <Main {
                  x: Option[String] = Some(
                    "this is a very long string that causes a line break because Some uses soft lines"
                  ),
                }></Main>
            "#]],
        );
    }

    #[test]
    fn html_with_string_and_expression_attributes_to_doc() {
        check(
            indoc! {r#"
                record Product { img_src: String }
                <ProductImage {product: Product}>
                  <img class="rounded-lg" src={product.img_src}>
                </ProductImage>
            "#},
            expect![[r#"
                record Product {
                  img_src: String,
                }

                <ProductImage {product: Product}>
                  <img class="rounded-lg" src={product.img_src}>
                </ProductImage>
            "#]],
        );
    }

    #[test]
    fn component_with_match_node_to_doc() {
        check(
            indoc! {"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {
                  c: Option[String],
                }>
                  <match {c}>
                    <case {Some(x)}>
                      {x}
                    </case>
                    <case {None}>
                      green
                    </case>
                  </match>
                </Main>
            "},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {c: Option[String]}>
                  <match {c}>
                    <case {Some(x)}>
                      {x}
                    </case>
                    <case {None}>
                      green
                    </case>
                  </match>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_classes_macro_in_class_attribute_to_doc() {
        check(
            indoc! {r#"
                <Card {base_class: String, extra_class: String}>
                  <div class={classes!(base_class, extra_class)}></div>
                </Card>
            "#},
            expect![[r#"
                <Card {base_class: String, extra_class: String}>
                  <div class={classes!(base_class, extra_class)}></div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn component_with_classes_macro_multiple_classes_to_doc() {
        check(
            indoc! {r#"
                <Button {size_class: String, variant_class: String, custom_class: String}>
                  <button class={classes!(size_class, variant_class, custom_class)}>Click</button>
                </Button>
            "#},
            expect![[r#"
                <Button {
                  size_class: String,
                  variant_class: String,
                  custom_class: String,
                }>
                  <button class={
                    classes!(size_class, variant_class, custom_class)
                  }>
                    Click
                  </button>
                </Button>
            "#]],
        );
    }

    #[test]
    fn component_with_classes_macro_long_args_breaks_to_multiple_lines() {
        check(
            indoc! {r#"
                <Component {base_styles: String, responsive_styles: String, interactive_styles: String, custom_overrides: String}>
                  <div class={classes!(base_styles, responsive_styles, interactive_styles, custom_overrides)}></div>
                </Component>
            "#},
            expect![[r#"
                <Component {
                  base_styles: String,
                  responsive_styles: String,
                  interactive_styles: String,
                  custom_overrides: String,
                }>
                  <div class={
                    classes!(
                      base_styles,
                      responsive_styles,
                      interactive_styles,
                      custom_overrides,
                    )
                  }></div>
                </Component>
            "#]],
        );
    }

    #[test]
    fn classes_macro_expands_spaces_in_string_literals() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("foo bar")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("foo", "bar")}></div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_expands_multiple_spaces_in_string_literals() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("a b c")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("a", "b", "c")}></div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_expands_mixed_variables_and_literals() {
        check(
            indoc! {r#"
                <Card {a: String, b: String, c: String}>
                  <div class={classes!(a, "foo bar", b, "baz qux", c)}></div>
                </Card>
            "#},
            expect![[r#"
                <Card {a: String, b: String, c: String}>
                  <div class={
                    classes!(a, "foo", "bar", b, "baz", "qux", c)
                  }></div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_collapses_multiple_spaces() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("foo    bar")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("foo", "bar")}></div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn classes_macro_trims_and_collapses_whitespace() {
        check(
            indoc! {r#"
                <Card>
                  <div class={classes!("   foo  bar   baz  ")}></div>
                </Card>
            "#},
            expect![[r#"
                <Card>
                  <div class={classes!("foo", "bar", "baz")}></div>
                </Card>
            "#]],
        );
    }

    #[test]
    fn should_format_deeply_nested_elements() {
        check(
            indoc! {r#"
                <Main>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  <p><p><p><p><p><p><p><p><p><p>
                  content
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                  </p></p></p></p></p></p></p></p></p></p>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <p>
                    <p>
                      <p>
                        <p>
                          <p>
                            <p>
                              <p>
                                <p>
                                  <p>
                                    <p>
                                      <p>
                                        <p>
                                          <p>
                                            <p>
                                              <p>
                                                <p>
                                                  <p>
                                                    <p>
                                                      <p>
                                                        <p>
                                                          <p>
                                                            <p>
                                                              <p>
                                                                <p>
                                                                  <p>
                                                                    <p>
                                                                      <p>
                                                                        <p>
                                                                          <p>
                                                                            <p>
                                                                              <p>
                                                                                <p>
                                                                                  <p>
                                                                                    <p>
                                                                                      <p>
                                                                                        <p>
                                                                                          <p>
                                                                                            <p>
                                                                                              <p>
                                                                                                <p>
                                                                                                  <p>
                                                                                                    <p>
                                                                                                      <p>
                                                                                                        <p>
                                                                                                          <p>
                                                                                                            <p>
                                                                                                              <p>
                                                                                                                <p>
                                                                                                                  <p>
                                                                                                                    <p>
                                                                                                                      content
                                                                                                                    </p>
                                                                                                                  </p>
                                                                                                                </p>
                                                                                                              </p>
                                                                                                            </p>
                                                                                                          </p>
                                                                                                        </p>
                                                                                                      </p>
                                                                                                    </p>
                                                                                                  </p>
                                                                                                </p>
                                                                                              </p>
                                                                                            </p>
                                                                                          </p>
                                                                                        </p>
                                                                                      </p>
                                                                                    </p>
                                                                                  </p>
                                                                                </p>
                                                                              </p>
                                                                            </p>
                                                                          </p>
                                                                        </p>
                                                                      </p>
                                                                    </p>
                                                                  </p>
                                                                </p>
                                                              </p>
                                                            </p>
                                                          </p>
                                                        </p>
                                                      </p>
                                                    </p>
                                                  </p>
                                                </p>
                                              </p>
                                            </p>
                                          </p>
                                        </p>
                                      </p>
                                    </p>
                                  </p>
                                </p>
                              </p>
                            </p>
                          </p>
                        </p>
                      </p>
                    </p>
                  </p>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_single_string_binding_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {name: String = "World"}>
                    Hello, {name}!
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    Hello, {name}!
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_single_int_binding_to_doc() {
        check(
            indoc! {"
                <Main>
                  <let {count: Int = 42}>
                    <span>{count}</span>
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {count: Int = 42}>
                    <span>
                      {count}
                    </span>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_trailing_comma_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {name: String = "World",}>
                    {name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    {name}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_multiple_bindings_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {first: String = "Hello", second: String = "World"}>
                    {first} {second}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {first: String = "Hello", second: String = "World"}>
                    {first} {second}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_three_bindings_to_doc() {
        check(
            indoc! {"
                <Main>
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>{a} + {b} + {c}</div>
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>
                      {a} + {b} + {c}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_expression_value_to_doc() {
        check(
            indoc! {"
                <Main {x: Int, y: Int}>
                  <let {sum: Int = x + y}>
                    <span>{sum}</span>
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main {x: Int, y: Int}>
                  <let {sum: Int = x + y}>
                    <span>
                      {sum}
                    </span>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_field_access_value_to_doc() {
        check(
            indoc! {"
                record User { name: String }
                <Main {user: User}>
                  <let {name: String = user.name}>
                    <div>{name}</div>
                  </let>
                </Main>
            "},
            expect![[r#"
                record User {
                  name: String,
                }

                <Main {user: User}>
                  <let {name: String = user.name}>
                    <div>
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_let_tags_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {a: String = "outer"}>
                    <let {b: String = "inner"}>
                      {a} {b}
                    </let>
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: String = "outer"}>
                    <let {b: String = "inner"}>
                      {a} {b}
                    </let>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_inside_if_to_doc() {
        check(
            indoc! {r#"
                <Main {show: Bool}>
                  <if {show}>
                    <let {msg: String = "visible"}>
                      {msg}
                    </let>
                  </if>
                </Main>
            "#},
            expect![[r#"
                <Main {show: Bool}>
                  <if {show}>
                    <let {msg: String = "visible"}>
                      {msg}
                    </let>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_inside_for_to_doc() {
        check(
            indoc! {"
                <Main {items: Array[Int]}>
                  <for {item in items}>
                    <let {doubled: Int = item * 2}>
                      <span>{doubled}</span>
                    </let>
                  </for>
                </Main>
            "},
            expect![[r#"
                <Main {items: Array[Int]}>
                  <for {item in items}>
                    <let {doubled: Int = item * 2}>
                      <span>
                        {doubled}
                      </span>
                    </let>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn multiple_sibling_let_tags_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {a: String = "first"}>
                    {a}
                  </let>
                  <let {b: String = "second"}>
                    {b}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: String = "first"}>
                    {a}
                  </let>
                  <let {b: String = "second"}>
                    {b}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_empty_children_to_doc() {
        check(
            indoc! {r#"
                <Main>
                  <let {x: String = "unused"}></let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {x: String = "unused"}></let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn let_with_long_bindings_breaks_to_multiple_lines() {
        check(
            indoc! {r#"
                <Main>
                  <let {
                    // a
                    first_name: String = "Hello",
                    // b
                    last_name: String = "World",
                    // c
                  }>
                    {first_name} {last_name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {
                    // a
                    first_name: String = "Hello",
                    // b
                    last_name: String = "World",
                    // c
                  }>
                    {first_name} {last_name}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn comment_before_import_declaration() {
        check(
            indoc! {"
                // External component
                import components::Button
                <Main></Main>
            "},
            expect![[r#"
                // External component
                import components::Button

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn comments_before_imports() {
        check(
            indoc! {"
                // c
                import c::Baz
                // b
                import b::Bar
                // a
                import a::Foo
            "},
            expect![[r#"
                // c
                import c::Baz
                // b
                import b::Bar
                // a
                import a::Foo
            "#]],
        );
    }

    #[test]
    fn comment_before_record_declaration() {
        check(
            indoc! {"
                // This is a comment
                record User { name: String }
            "},
            expect![[r#"
                // This is a comment
                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_enum_declaration() {
        check(
            indoc! {"
                // Color enum
                enum Color { Red, Green, Blue }
            "},
            expect![[r#"
                // Color enum
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_component_declaration() {
        check(
            indoc! {"
                // Main component
                <Main>
                  hello
                </Main>
            "},
            expect![[r#"
                // Main component
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn multiple_comments_before_declarations() {
        check(
            indoc! {"
                // User record
                record User { name: String }

                // Status enum
                enum Status { Active, Inactive }

                // Main component
                <Main></Main>
            "},
            expect![[r#"
                // User record
                record User {
                  name: String,
                }

                // Status enum
                enum Status {
                  Active,
                  Inactive,
                }

                // Main component
                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn comment_before_record_field() {
        check(
            indoc! {"
                record User {
                // The name of the user
                name: String,
                // The age of the user
                age: Int,
                // End of fields
                }
            "},
            expect![[r#"
                record User {
                  // The name of the user
                  name: String,
                  // The age of the user
                  age: Int,
                  // End of fields
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_enum_variant() {
        check(
            indoc! {"
                enum Status {
                // User is active
                Active,
                // User is inactive
                Inactive,
                // More statuses to come
                }
            "},
            expect![[r#"
                enum Status {
                  // User is active
                  Active,
                  // User is inactive
                  Inactive,
                  // More statuses to come
                }
            "#]],
        );
    }

    #[test]
    fn comments_in_all_positions_enum() {
        check(
            indoc! {"
                // a
                enum Status {
                // b
                Active,
                // c
                Inactive,
                // d
                }
                // e
                <Main></Main>
            "},
            expect![[r#"
                // a
                enum Status {
                  // b
                  Active,
                  // c
                  Inactive,
                  // d
                }

                // e
                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn empty_enum_with_comment() {
        check(
            indoc! {"
                enum X {
                // Empty
                }
            "},
            expect![[r#"
                enum X {
                  // Empty
                }
            "#]],
        );
    }

    #[test]
    fn empty_record_with_comment() {
        check(
            indoc! {"
                record X {
                // Empty
                }
            "},
            expect![[r#"
                record X {
                  // Empty
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_match_arm() {
        check(
            indoc! {r#"
                enum Orientation { Horizontal, Vertical }
                <Main {orientation: Orientation}>
                  <div class={match orientation {
                    // a
                    Orientation::Horizontal => "horizontal",
                    // b
                    Orientation::Vertical => "vertical",
                    // c
                  }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Orientation {
                  Horizontal,
                  Vertical,
                }

                <Main {orientation: Orientation}>
                  <div class={
                    match orientation {
                      // a
                      Orientation::Horizontal => "horizontal",
                      // b
                      Orientation::Vertical => "vertical",
                      // c
                    }
                  }></div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn comment_before_macro_arg() {
        check(
            indoc! {r#"
                <Main>
                  <div class={classes!(
                    // base styles
                    "flex",
                    // conditional style
                    "items-center",
                    // more to come
                  )}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div class={
                    classes!(
                      // base styles
                      "flex",
                      // conditional style
                      "items-center",
                      // more to come
                    )
                  }></div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn comment_before_macro_arg_with_string_expansion() {
        check(
            indoc! {r#"
                <Main>
                  <div class={classes!(
                    // base styles
                    "flex items-center",
                    // conditional style
                    "justify-between gap-4",
                    // more to come
                  )}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div class={
                    classes!(
                      // base styles
                      "flex",
                      "items-center",
                      // conditional style
                      "justify-between",
                      "gap-4",
                      // more to come
                    )
                  }></div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn comment_before_component_parameter() {
        check(
            indoc! {r#"
                <Button {
                    // The button label
                    label: String,
                    // Whether the button is disabled
                    disabled: Bool = false,
                    // More params to come
                }>
                  {label}
                </Button>
            "#},
            expect![[r#"
                <Button {
                  // The button label
                  label: String,
                  // Whether the button is disabled
                  disabled: Bool = false,
                  // More params to come
                }>
                  {label}
                </Button>
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_component_parameters_single_param() {
        check(
            indoc! {r#"
                <X {
                  x: String,
                  // ?
                }>
                  {x}
                </X>
            "#},
            expect![[r#"
                <X {
                  x: String,
                  // ?
                }>
                  {x}
                </X>
            "#]],
        );
    }

    #[test]
    fn classes_macro_with_multiple_string_literals() {
        check(
            indoc! {r#"
                <Main>
                  <h1 class={classes!("text-4xl", "font-bold", "tracking-tight", "dark:hover:text-blue-300")}>Hello</h1>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <h1 class={
                    classes!(
                      "text-4xl",
                      "font-bold",
                      "tracking-tight",
                      "dark:hover:text-blue-300",
                    )
                  }>
                    Hello
                  </h1>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_reference_with_single_long_attribute() {
        check(
            indoc! {r#"
                <Main>
                  <Button class={classes!("text-4xl", "font-bold", "tracking-tight", "dark:hover:text-blue-300")}>Click me</Button>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <Button class={
                    classes!(
                      "text-4xl",
                      "font-bold",
                      "tracking-tight",
                      "dark:hover:text-blue-300",
                    )
                  }>
                    Click me
                  </Button>
                </Main>
            "#]],
        );
    }

    #[test]
    fn simple_method_call_to_doc() {
        check(
            indoc! {"
                <Main {x: String}>
                  <div>{x.foo()}</div>
                </Main>
            "},
            expect![[r#"
                <Main {x: String}>
                  <div>
                    {x.foo()}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn chained_method_calls_to_doc() {
        check(
            indoc! {"
                <Main {x: String}>
                  <div>{x.foo().bar().baz()}</div>
                </Main>
            "},
            expect![[r#"
                <Main {x: String}>
                  <div>
                    {x.foo().bar().baz()}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn mixed_field_access_and_method_call_to_doc() {
        check(
            indoc! {"
                <Main {x: String}>
                  <div>{x.field.method()}</div>
                </Main>
            "},
            expect![[r#"
                <Main {x: String}>
                  <div>
                    {x.field.method()}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn method_call_then_field_access_to_doc() {
        check(
            indoc! {"
                <Main {x: String}>
                  <div>{x.method().field}</div>
                </Main>
            "},
            expect![[r#"
                <Main {x: String}>
                  <div>
                    {x.method().field}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn float_literal_to_doc() {
        check(
            indoc! {"
                <Main>
                  <let {x: Float = 5.0}>
                    {x}
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {x: Float = 5.0}>
                    {x}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn float_literals_small_values_to_doc() {
        check(
            indoc! {"
                <Main>
                  <let {a: Float = 0.000, b: Float = 0.001, c: Float = 0.002}>
                    {a}
                  </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {
                    a: Float = 0.000,
                    b: Float = 0.001,
                    c: Float = 0.002,
                  }>
                    {a}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn inline_text_with_nested_element_to_doc() {
        check(
            indoc! {"
                <Main>
                  <div>
                    foo<p>bar</p>
                  </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    foo
                    <p>
                      bar
                    </p>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_elements_inline_to_doc() {
        check(
            indoc! {"
                <Main>
                  <div><p>x</p></div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    <p>
                      x
                    </p>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn text_around_inline_element_to_doc() {
        check(
            indoc! {"
                <Main>
                  <div>hello <b>world</b>!</div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    hello{" "}
                    <b>
                      world
                    </b>
                    !
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn text_with_multiple_inline_links() {
        check(
            indoc! {r#"
                <Main>
                  <p>By clicking continue, you agree to our <a href="/tos">Terms of Service</a> and <a href="/privacy">Privacy Policy</a>.</p>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <p>
                    By clicking continue, you agree to our{" "}
                    <a href="/tos">
                      Terms of Service
                    </a>
                    {" "}and{" "}
                    <a href="/privacy">
                      Privacy Policy
                    </a>
                    .
                  </p>
                </Main>
            "#]],
        );
    }

    #[test]
    fn empty_lines_between_text_collapsed_to_doc() {
        check(
            indoc! {"
                <Main>
                  <div>

                  foo

                  bar

                  </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    foo
                    bar
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn text_around_void_element_to_doc() {
        check(
            indoc! {"
                <Main>
                    <div>hello <br> world</div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    hello{" "}
                    <br>
                    {" "}world
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn void_element_on_separate_line_to_doc() {
        check(
            indoc! {"
                <Main>
                    <div>
                        hello
                        <br>
                        world
                    </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    hello
                    <br>
                    world
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn text_around_input_element_to_doc() {
        check(
            indoc! {r#"
                <Main>
                    <label>Name: <input type="text"> (required)</label>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <label>
                    Name:{" "}
                    <input type="text">
                    {" "}(required)
                  </label>
                </Main>
            "#]],
        );
    }

    #[test]
    fn input_element_on_separate_line_to_doc() {
        check(
            indoc! {r#"
                <Main>
                    <label>
                        Name:
                        <input type="text">
                        (required)
                    </label>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <label>
                    Name:
                    <input type="text">
                    (required)
                  </label>
                </Main>
            "#]],
        );
    }

    #[test]
    fn text_with_multiple_expressions_to_doc() {
        check(
            indoc! {"
                <Main {rating: String, num_reviews: String}>
                  <span>{rating} ({num_reviews} reviews)</span>
                </Main>
            "},
            expect![[r#"
                <Main {rating: String, num_reviews: String}>
                  <span>
                    {rating} ({num_reviews} reviews)
                  </span>
                </Main>
            "#]],
        );
    }

    #[test]
    fn method_call_on_negated_int_preserves_parens() {
        check(
            indoc! {"
                <Main>
                  <div>{(-42).to_string()}</div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    {(-42).to_string()}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn method_call_on_negated_float_preserves_parens() {
        check(
            indoc! {"
                <Main>
                  <div>{(-3.14).to_string()}</div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    {(-3.14).to_string()}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn method_call_on_binary_expr_preserves_parens() {
        check(
            indoc! {"
                <Main>
                  <div>{(1 + 2).to_string()}</div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    {(1 + 2).to_string()}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn field_access_on_negated_int_preserves_parens() {
        check(
            indoc! {"
                <Main>
                  <div>{(-42).foo}</div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    {(-42).foo}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn binary_expr_with_parens_preserves_precedence() {
        check(
            indoc! {"
                <Main {x: Int}>
                  <div>{(1 + 2) * 3}</div>
                </Main>
            "},
            expect![[r#"
                <Main {x: Int}>
                  <div>
                    {(1 + 2) * 3}
                  </div>
                </Main>
            "#]],
        );
    }
}
