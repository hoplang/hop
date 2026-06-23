use super::parsed_ast::{
    ParsedAst, ParsedAttribute, ParsedAttributeValue, ParsedComponentDeclaration,
    ParsedDeclaration, ParsedEnumDeclaration, ParsedEnumDeclarationVariant,
    ParsedImportDeclaration, ParsedParameter, ParsedRecordDeclaration,
    ParsedRecordDeclarationField, ParsedViewDeclaration,
};
use super::parsed_node::{ParsedLetBinding, ParsedMatchCase, ParsedNode};
use crate::document::DocumentRange;
use crate::expr::parsing::ParsedType;
use crate::expr::parsing::parsed_expr::{
    Constructor, ParsedExpr, ParsedMatchArm, ParsedMatchPattern,
};
use crate::hop::parsing::parsed_node::ParsedLoopSource;
use crate::html::HtmlElement;
use pretty::{Arena, DocAllocator, DocBuilder};
use std::collections::VecDeque;

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
        ParsedDeclaration::View(view) => format_view_declaration(arena, view, comments),
    }
}

fn format_import_declaration<'a>(
    arena: &'a Arena<'a>,
    import: &'a ParsedImportDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, import.import_range.start());
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
    let pub_prefix = if record.pub_range.is_some() {
        arena.text("pub ")
    } else {
        arena.nil()
    };
    leading_comments
        .append(pub_prefix)
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
    let base = if let Some(examples) = &field.examples {
        leading_comments
            .append(arena.text(examples.to_annotation_string()))
            .append(arena.hardline())
    } else {
        leading_comments
    };
    base.append(arena.text(field.name.as_str()))
        .append(arena.text(": "))
        .append(format_type(arena, &field.field_type))
}

fn format_enum_declaration<'a>(
    arena: &'a Arena<'a>,
    e: &'a ParsedEnumDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, e.name_range.start());
    let pub_prefix = if e.pub_range.is_some() {
        arena.text("pub ")
    } else {
        arena.nil()
    };
    leading_comments
        .append(pub_prefix)
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
        let mut fields_doc = arena.nil();
        for (i, (field_name, field_name_range, field_type, examples)) in
            variant.fields.iter().enumerate()
        {
            if i > 0 {
                fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
            }
            let field_comments = drain_comments_before(arena, comments, field_name_range.start());
            let base = if let Some(e) = examples {
                field_comments
                    .append(arena.text(e.to_annotation_string()))
                    .append(arena.hardline())
            } else {
                field_comments
            };
            fields_doc = fields_doc
                .append(base)
                .append(arena.text(field_name.to_string()))
                .append(arena.text(": "))
                .append(format_type(arena, field_type));
        }
        fields_doc = fields_doc.append(arena.text(","));

        leading_comments
            .append(arena.text(variant.name.as_str()))
            .append(arena.text(" {"))
            .append(arena.line().append(fields_doc).nest(2).append(arena.line()))
            .append(arena.text("}"))
    }
}

fn format_component_declaration<'a>(
    arena: &'a Arena<'a>,
    component: &'a ParsedComponentDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, component.range.start());

    // Format parameters (omit parentheses if no parameters)
    let params_doc = match &component.params {
        Some((params, params_range)) if !params.is_empty() => {
            let mut params_inner = arena.nil();
            let force_multiline = params.len() >= 2;
            let line_break = if force_multiline {
                arena.hardline()
            } else {
                arena.line()
            };
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    params_inner = params_inner
                        .append(arena.text(","))
                        .append(line_break.clone());
                }
                params_inner = params_inner.append(format_parameter(arena, param, comments));
            }
            let has_trailing_comments = comments
                .front()
                .is_some_and(|c| c.start() < params_range.end());
            let trailing_comments = drain_comments_before(arena, comments, params_range.end());
            if has_trailing_comments {
                let body = arena
                    .hardline()
                    .append(params_inner)
                    .append(arena.text(","))
                    .append(arena.hardline())
                    .append(trailing_comments)
                    .nest(2);
                arena.text("(").append(body).append(arena.text(")"))
            } else if force_multiline {
                let body = arena
                    .hardline()
                    .append(params_inner)
                    .append(arena.text(","))
                    .nest(2)
                    .append(arena.hardline());
                arena.text("(").append(body).append(arena.text(")"))
            } else {
                let body = arena
                    .line_()
                    .append(params_inner)
                    .append(arena.text(",").flat_alt(arena.nil()))
                    .nest(2)
                    .append(arena.line_());
                arena.text("(").append(body).append(arena.text(")")).group()
            }
        }
        _ => arena.nil(),
    };

    let pub_prefix = if component.pub_range.is_some() {
        arena.text("pub ")
    } else {
        arena.nil()
    };
    leading_comments
        .append(pub_prefix)
        .append(arena.text("component"))
        .append(arena.text(" "))
        .append(arena.text(component.component_name.as_str()))
        .append(params_doc)
        .append(arena.text(" {"))
        .append(format_children(arena, &component.children, comments))
        .append(arena.text("}"))
}

fn format_view_declaration<'a>(
    arena: &'a Arena<'a>,
    view: &'a ParsedViewDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, view.range.start());

    // Format parameters (omit parentheses if no parameters)
    let params_doc = if view.params.is_empty() {
        arena.nil()
    } else {
        let mut params_inner = arena.nil();
        let force_multiline = view.params.len() >= 2;
        let line_break = if force_multiline {
            arena.hardline()
        } else {
            arena.line()
        };
        for (i, param) in view.params.iter().enumerate() {
            if i > 0 {
                params_inner = params_inner
                    .append(arena.text(","))
                    .append(line_break.clone());
            }
            params_inner = params_inner.append(format_parameter(arena, param, comments));
        }
        if force_multiline {
            let body = arena
                .hardline()
                .append(params_inner)
                .append(arena.text(","))
                .nest(2)
                .append(arena.hardline());
            arena.text("(").append(body).append(arena.text(")"))
        } else {
            let body = arena
                .line_()
                .append(params_inner)
                .append(arena.text(",").flat_alt(arena.nil()))
                .nest(2)
                .append(arena.line_());
            arena.text("(").append(body).append(arena.text(")")).group()
        }
    };

    let pub_prefix = if view.pub_range.is_some() {
        arena.text("pub ")
    } else {
        arena.nil()
    };
    leading_comments
        .append(pub_prefix)
        .append(arena.text("view"))
        .append(arena.text(" "))
        .append(arena.text(view.name.as_str()))
        .append(params_doc)
        .append(arena.text(" {"))
        .append(format_children(arena, &view.children, comments))
        .append(arena.text("}"))
}

fn format_parameter<'a>(
    arena: &'a Arena<'a>,
    param: &'a ParsedParameter,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, param.var_name_range.start());
    let prefix = if let Some(examples) = &param.examples {
        leading_comments
            .append(arena.text(examples.to_annotation_string()))
            .append(arena.hardline())
    } else {
        leading_comments
    };
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
    prefix.append(param_doc)
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
        ParsedNode::ComponentInvocation {
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
            let source_doc = match &**source {
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
        ParsedNode::Comment { range } => arena.text(range.as_str()),
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
            element,
            attributes,
            children,
            ..
        } => {
            let element_str = element.as_str();
            let opening_tag_doc = if attributes.is_empty() {
                arena
                    .text("<")
                    .append(arena.text(element_str))
                    .append(arena.text(">"))
            } else if attributes.len() == 1 {
                // Single attribute: keep on same line as tag
                arena
                    .text("<")
                    .append(arena.text(element_str))
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
                    .append(arena.text(element_str))
                    .append(arena.line().append(attrs_doc).nest(2))
                    .append(arena.line_())
                    .append(arena.text(">"))
                    .group()
            };

            if element.is_void() {
                opening_tag_doc
            } else if children.is_empty() {
                // Empty element - put opening and closing tags on separate lines,
                // except for script/style where whitespace would become content
                let sep = if *element == HtmlElement::Script || *element == HtmlElement::Style {
                    arena.nil()
                } else {
                    arena.line()
                };
                opening_tag_doc
                    .append(sep)
                    .append(arena.text("</"))
                    .append(arena.text(element_str))
                    .append(arena.text(">"))
            } else if *element == HtmlElement::Script || *element == HtmlElement::Style {
                // For script/style, preserve text content exactly as written
                // to avoid altering semantically significant whitespace
                let mut doc = opening_tag_doc;
                for child in children {
                    if let ParsedNode::Text { range } = child {
                        doc = doc.append(arena.text(range.as_str()));
                    }
                }
                doc.append(arena.text("</"))
                    .append(arena.text(element_str))
                    .append(arena.text(">"))
            } else {
                opening_tag_doc
                    .append(format_children(arena, children, comments))
                    .append(arena.text("</"))
                    .append(arena.text(element_str))
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
        return arena.hardline();
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
    let mut doc = leading_comments.append(arena.text(binding.var_name.as_str()));
    if let Some(var_type) = &binding.var_type {
        doc = doc
            .append(arena.text(": "))
            .append(format_type(arena, var_type));
    }
    doc.append(arena.text(" = "))
        .append(format_expr(arena, &binding.value_expr, comments))
}

fn format_type<'a>(arena: &'a Arena<'a>, ty: &ParsedType) -> DocBuilder<'a, Arena<'a>> {
    match ty {
        ParsedType::String { .. } => arena.text("String"),
        ParsedType::Bool { .. } => arena.text("Bool"),
        ParsedType::Int { .. } => arena.text("Int"),
        ParsedType::Float { .. } => arena.text("Float"),
        ParsedType::Fragment { .. } => arena.text("Fragment"),
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
                arena.text(record_name.as_str()).append(arena.text(" {}"))
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
                    .append(arena.text(" {"))
                    .append(soft_block(arena, fields_doc))
                    .append(arena.text("}"))
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
        expr @ ParsedExpr::BooleanNegation { operand, .. } => arena.text("!").append(
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
                        fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
                    }
                    fields_doc = fields_doc
                        .append(arena.text(field_name.to_string()))
                        .append(arena.text(": "))
                        .append(format_expr(arena, field_value, comments));
                }
                base.append(arena.text(" {"))
                    .append(soft_block(arena, fields_doc))
                    .append(arena.text("}"))
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
            if name == "join" {
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
                } else if name == "join" {
                    // Always break join! macro onto multiple lines for easier
                    // editing and minimal version control diffs
                    arena
                        .hardline()
                        .append(args_doc)
                        .append(arena.text(","))
                        .nest(2)
                        .append(arena.hardline())
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
        ParsedExpr::FragmentEmpty { .. } => arena.text("Fragment::empty()"),
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
                let mut fields_doc = arena.nil();
                for (i, (name, _, pat)) in fields.iter().enumerate() {
                    if i > 0 {
                        fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
                    }
                    if let ParsedMatchPattern::Binding { name: var_name, .. } = pat {
                        if var_name.as_str() == name.as_str() {
                            fields_doc = fields_doc.append(arena.text(name.as_str()));
                            continue;
                        }
                    }
                    fields_doc = fields_doc.append(
                        arena
                            .text(name.as_str())
                            .append(arena.text(": "))
                            .append(format_match_pattern(arena, pat)),
                    );
                }
                base.append(arena.text(" {"))
                    .append(soft_block(arena, fields_doc))
                    .append(arena.text("}"))
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
    use crate::document_id::DocumentId;
    use crate::hop::parsing::parser;

    fn check(source: &str, expected: Expect) {
        let mut errors = Vec::new();
        let document_id = DocumentId::new("test.hop").unwrap();
        let ast = parser::parse(
            document_id.clone(),
            Document::new(document_id, source.to_string()),
            &mut errors,
        );
        if !errors.is_empty() {
            panic!("Parse errors: {:?}", errors);
        }
        let formatted = super::format(ast);
        expected.assert_eq(&formatted);

        // Check idempotency: formatting the output should give the same result
        let document_id = DocumentId::new("test.hop").unwrap();
        let formatted_twice = super::format(parser::parse(
            document_id.clone(),
            Document::new(document_id, formatted.clone()),
            &mut errors,
        ));
        assert_eq!(formatted, formatted_twice, "Formatter is not idempotent");
    }

    #[test]
    fn pub_record_to_doc() {
        check(
            indoc! {"
                pub record User { name: String }
            "},
            expect![[r#"
                pub record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn pub_enum_to_doc() {
        check(
            indoc! {"
                pub enum Color { Red, Green, Blue }
            "},
            expect![[r#"
                pub enum Color {
                  Red,
                  Green,
                  Blue,
                }
            "#]],
        );
    }

    #[test]
    fn pub_component_to_doc() {
        check(
            indoc! {"
                pub component Button(label: String) {
                  <button>{label}</button>
                }
            "},
            expect![[r#"
                pub component Button(label: String) {
                  <button>
                    {label}
                  </button>
                }
            "#]],
        );
    }

    #[test]
    fn pub_view_to_doc() {
        check(
            indoc! {"
                pub view Home {
                  <div>hi</div>
                }
            "},
            expect![[r#"
                pub view Home {
                  <div>
                    hi
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn pub_preserves_leading_comment() {
        check(
            indoc! {"
                // The user record
                pub record User { name: String }
            "},
            expect![[r#"
                // The user record
                pub record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn mixed_pub_and_non_pub_declarations() {
        check(
            indoc! {"
                pub record A { x: Int }
                record B { y: Int }
                pub component C {<p>hi</p>}
            "},
            expect![[r#"
                pub record A {
                  x: Int,
                }

                record B {
                  y: Int,
                }

                pub component C {
                  <p>
                    hi
                  </p>
                }
            "#]],
        );
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
                component Main(name: String, count: Int) {
                  <div>{name}</div>
                }
            "},
            expect![[r#"
                component Main(
                  name: String,
                  count: Int,
                ) {
                  <div>
                    {name}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn component_declaration_with_many_parameters_to_doc() {
        check(
            indoc! {"
                component Main(first_name: String, last_name: String, email: String, age: Int, active: Bool, role: String) {}
            "},
            expect![[r#"
                component Main(
                  first_name: String,
                  last_name: String,
                  email: String,
                  age: Int,
                  active: Bool,
                  role: String,
                ) {
                }
            "#]],
        );
    }

    #[test]
    fn component_with_match_expression_to_doc() {
        check(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                component Main(color: Color) {
                  <div class={match color { Color::Red => "red", Color::Green => "green", Color::Blue => "blue" }}></div>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(color: Color) {
                  <div class={
                    match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_empty_braces_omits_braces() {
        // Empty braces in enum patterns should be normalized away
        check(
            indoc! {r#"
                enum Color { Red }
                component Main(color: Color) {
                  <div class={match color { Color::Red{} => "red" }}></div>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                }

                component Main(color: Color) {
                  <div class={match color {Color::Red => "red"}}>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_shorthand_field_destructuring() {
        check(
            indoc! {r#"
                enum Outcome { Success {value: String}, Failure {message: String} }
                component Main(result: Outcome) {
                  <match {result}>
                    <case {Outcome::Success {value}}>
                      {value}
                    </case>
                    <case {Outcome::Failure {message}}>
                      {message}
                    </case>
                  </match>
                }
            "#},
            expect![[r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                component Main(result: Outcome) {
                  <match {result}>
                    <case {Outcome::Success {value}}>
                      {value}
                    </case>
                    <case {Outcome::Failure {message}}>
                      {message}
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_mixed_shorthand_and_explicit_bindings() {
        check(
            indoc! {r#"
                enum Event { Click {x: Int, y: Int} }
                component Main(event: Event) {
                  <div>{match event { Event::Click {x, y: b} => x + b }}</div>
                }
            "#},
            expect![[r#"
                enum Event {
                  Click {
                    x: Int,
                    y: Int,
                  },
                }

                component Main(event: Event) {
                  <div>
                    {match event {Event::Click {x, y: b} => x + b}}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn match_pattern_explicit_binding_shortened_when_name_matches_field() {
        check(
            indoc! {r#"
                enum Outcome { Success {value: String} }
                component Main(result: Outcome) {
                  <match {result}>
                    <case {Outcome::Success {value: value}}>
                      {value}
                    </case>
                  </match>
                }
            "#},
            expect![[r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                }

                component Main(result: Outcome) {
                  <match {result}>
                    <case {Outcome::Success {value}}>
                      {value}
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_many_fields_breaks_over_multiple_lines() {
        check(
            indoc! {r#"
                enum Event {
                  Button {
                    type: String,
                    name: String,
                    value: String,
                    dialog_trigger: Boolean,
                    popover_trigger: Boolean,
                  }
                }
                component Main(event: Event) {
                  <match {event}>
                    <case {Event::Button {type, name, value, dialog_trigger, popover_trigger}}>
                    </case>
                  </match>
                }
            "#},
            expect![[r#"
                enum Event {
                  Button {
                    type: String,
                    name: String,
                    value: String,
                    dialog_trigger: Boolean,
                    popover_trigger: Boolean,
                  },
                }

                component Main(event: Event) {
                  <match {event}>
                    <case {Event::Button {
                      type,
                      name,
                      value,
                      dialog_trigger,
                      popover_trigger,
                    }}>
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn enum_declaration_with_empty_braces_omits_braces() {
        // Empty braces in enum declarations should be normalized away
        check(
            indoc! {r#"
                enum Foo { Bar{} }
                component Main {}
            "#},
            expect![[r#"
                enum Foo {
                  Bar,
                }

                component Main {
                }
            "#]],
        );
    }

    #[test]
    fn enum_variant_field_with_pattern_annotation() {
        check(
            indoc! {r#"
                enum ContactMethod { Email {#[examples(pattern = "[a-z][a-z]")] address: String}, Phone {number: String} }
            "#},
            expect![[r#"
                enum ContactMethod {
                  Email {
                    #[examples(pattern = "[a-z][a-z]")]
                    address: String,
                  },
                  Phone {
                    number: String,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn record_field_with_min_max_annotation() {
        check(
            indoc! {r#"
                record Product { #[examples(min = 1, max = 999)] price: Int, name: String }
            "#},
            expect![[r#"
                record Product {
                  #[examples(min = 1, max = 999)]
                  price: Int,
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn record_field_with_min_len_max_len_annotation() {
        check(
            indoc! {r#"
                record Post { #[examples(min_len = 2, max_len = 5)] tags: Array[String] }
            "#},
            expect![[r#"
                record Post {
                  #[examples(min_len = 2, max_len = 5)]
                  tags: Array[String],
                }
            "#]],
        );
    }

    #[test]
    fn component_declaration_with_text_child_to_doc() {
        check(
            indoc! {"
                component Main {hello}
            "},
            expect![[r#"
                component Main {
                  hello
                }
            "#]],
        );
    }

    #[test]
    fn html_with_class_and_expression_to_doc() {
        check(
            indoc! {r#"
                record Character { name: String }
                component Main(character: Character) {
                  <h1 class="text-2xl font-bold">{character.name}</h1>
                }
            "#},
            expect![[r#"
                record Character {
                  name: String,
                }

                component Main(character: Character) {
                  <h1 class="text-2xl font-bold">
                    {character.name}
                  </h1>
                }
            "#]],
        );
    }

    #[test]
    fn html_with_single_class_expression_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <div class={"p-2"}></div>
                }
            "#},
            expect![[r#"
                component Main {
                  <div class={"p-2"}>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn if_with_equality_condition_to_doc() {
        check(
            indoc! {"
                component Main(a: String, b: String) {
                  <if {a == b}>
                    <div>equal</div>
                  </if>
                }
            "},
            expect![[r#"
                component Main(
                  a: String,
                  b: String,
                ) {
                  <if {a == b}>
                    <div>
                      equal
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn if_with_logical_and_condition_to_doc() {
        check(
            indoc! {"
                component Main(a: Bool, b: Bool) {
                  <if {a && b}>
                    <div>both true</div>
                  </if>
                }
            "},
            expect![[r#"
                component Main(
                  a: Bool,
                  b: Bool,
                ) {
                  <if {a && b}>
                    <div>
                      both true
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn if_with_nested_logical_operators_to_doc() {
        check(
            indoc! {"
                component Main(a: Bool, b: Bool, c: Bool) {
                  <if {a && b || c}>
                    <div>complex</div>
                  </if>
                }
            "},
            expect![[r#"
                component Main(
                  a: Bool,
                  b: Bool,
                  c: Bool,
                ) {
                  <if {a && b || c}>
                    <div>
                      complex
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn if_with_negation_to_doc() {
        check(
            indoc! {"
                component Main(a: Bool) {
                  <if {!a}>
                    <div>not a</div>
                  </if>
                }
            "},
            expect![[r#"
                component Main(a: Bool) {
                  <if {!a}>
                    <div>
                      not a
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn if_with_negated_equality_to_doc() {
        check(
            indoc! {"
                component Main(a: String, b: String) {
                  <if {!(a == b)}>
                    <div>not equal</div>
                  </if>
                }
            "},
            expect![[r#"
                component Main(
                  a: String,
                  b: String,
                ) {
                  <if {!(a == b)}>
                    <div>
                      not equal
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_multiline_text() {
        check(
            indoc! {"
                component Main {
                  hello
                  world
                }
            "},
            expect![[r#"
                component Main {
                  hello
                  world
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_nested_html() {
        check(
            indoc! {"
                component Main {
                  <div>
                    content
                  </div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    content
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_empty_lines() {
        check(
            indoc! {"
                component Main {

                  hello

                }
            "},
            expect![[r#"
                component Main {
                  hello
                }
            "#]],
        );
    }

    #[test]
    fn nested_components_with_record_attributes_to_doc() {
        check(
            indoc! {r#"
                component IconsPage {
                  <div class="flex">
                    <div class="border-r max-w-80 h-screen">
                      <Sidebar />
                    </div>
                    <div class="flex gap-4 p-8">
                      <IconItem id="radix-icons" title="Radix Icons" img_src="/img/iphone.jpg" description="A crisp set of 15x15 icons." />
                    </div>
                  </div>
                }
            "#},
            expect![[r#"
                component IconsPage {
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
                }
            "#]],
        );
    }

    #[test]
    fn component_with_expression_attribute_to_doc() {
        check(
            indoc! {r#"
                import hop::ui::lucide::ChevronDown

                component NativeSelect {
                  <ChevronDown
                    class={"text-muted-foreground"}
                  />
                }
            "#},
            expect![[r#"
                import hop::ui::lucide::ChevronDown

                component NativeSelect {
                  <ChevronDown class={"text-muted-foreground"}/>
                }
            "#]],
        );
    }

    #[test]
    fn component_with_string_concatenation_attribute_to_doc() {
        check(
            indoc! {r#"
                record Product { id: String }
                component IconShowPage(product: Product) {
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                }
            "#},
            expect![[r#"
                record Product {
                  id: String,
                }

                component IconShowPage(product: Product) {
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_string_parameter_to_doc() {
        check(
            indoc! {r#"
                component Greeting(name: String = "World") {
                  Hello, {name}!
                }
            "#},
            expect![[r#"
                component Greeting(name: String = "World") {
                  Hello, {name}!
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_int_parameter_to_doc() {
        check(
            indoc! {"
                component Counter(count: Int = 0) {
                  {count}
                }
            "},
            expect![[r#"
                component Counter(count: Int = 0) {
                  {count}
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_bool_parameter_to_doc() {
        check(
            indoc! {"
                component Toggle(enabled: Bool = true) {
                }
            "},
            expect![[r#"
                component Toggle(enabled: Bool = true) {
                }
            "#]],
        );
    }

    #[test]
    fn component_with_mixed_required_and_default_parameters_to_doc() {
        check(
            indoc! {r#"
                component UserCard(name: String, role: String = "user", active: Bool = true) {
                  {name}
                }
            "#},
            expect![[r#"
                component UserCard(
                  name: String,
                  role: String = "user",
                  active: Bool = true,
                ) {
                  {name}
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_array_parameter_to_doc() {
        check(
            indoc! {r#"
                component ItemList(items: Array[String] = ["one", "two"]) {
                }
            "#},
            expect![[r#"
                component ItemList(items: Array[String] = ["one", "two"]) {
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_empty_array_parameter_to_doc() {
        check(
            indoc! {"
                component ItemList(items: Array[String] = []) {
                }
            "},
            expect![[r#"
                component ItemList(items: Array[String] = []) {
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_record_parameter_to_doc() {
        check(
            indoc! {r#"
                record Config { debug: Bool, timeout: Int }
                component Settings(config: Config = Config {debug: false, timeout: 30}) {
                }
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                component Settings(
                  config: Config = Config {debug: false, timeout: 30},
                ) {
                }
            "#]],
        );
    }

    #[test]
    fn component_with_default_enum_parameter_to_doc() {
        check(
            indoc! {"
                enum Status { Active, Inactive, Pending }
                component Badge(status: Status = Status::Active) {
                }
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                component Badge(status: Status = Status::Active) {
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_in_expression_to_doc() {
        check(
            indoc! {r#"
                record User { name: String, age: Int }
                component Main {
                  <let {user: User = User {name: "Alice", age: 30}}>
                    {user.name}
                  </let>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                component Main {
                  <let {user: User = User {name: "Alice", age: 30}}>
                    {user.name}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn inferred_let_binding_is_preserved() {
        check(
            indoc! {r#"
                component Main {
                  <let {name = "World"}>
                    {name}
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {name = "World"}>
                    {name}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn empty_record_literal_to_doc() {
        check(
            indoc! {"
                record Empty {}
                component Main {
                  <let {e: Empty = Empty {}}>
                  </let>
                }
            "},
            expect![[r#"
                record Empty {}

                component Main {
                  <let {e: Empty = Empty {}}>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_with_fields_to_doc() {
        check(
            indoc! {r#"
                enum Shape { Circle {radius: Float}, Rect {w: Float, h: Float} }
                component Main {
                  <let {s: Shape = Shape::Circle {radius: 5.0}}>
                  </let>
                }
            "#},
            expect![[r#"
                enum Shape {
                  Circle {
                    radius: Float,
                  },
                  Rect {
                    w: Float,
                    h: Float,
                  },
                }

                component Main {
                  <let {s: Shape = Shape::Circle {radius: 5.0}}>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_with_multiple_fields_to_doc() {
        check(
            indoc! {"
                enum Shape { Circle {radius: Float}, Rect {w: Float, h: Float} }
                component Main {
                  <let {s: Shape = Shape::Rect {w: 3.0, h: 4.0}}>
                  </let>
                }
            "},
            expect![[r#"
                enum Shape {
                  Circle {
                    radius: Float,
                  },
                  Rect {
                    w: Float,
                    h: Float,
                  },
                }

                component Main {
                  <let {s: Shape = Shape::Rect {w: 3.0, h: 4.0}}>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_with_many_fields_wraps_to_multiple_lines() {
        check(
            indoc! {r#"
                enum PopoverMenuItemElement { Link {href: String}, Button {href: String, name: String, value: String} }
                component Main {
                  <let {el: PopoverMenuItemElement = PopoverMenuItemElement::Button {href: "/path/to/some/page", name: "button_name", value: "button_value"}}>
                  </let>
                }
            "#},
            expect![[r#"
                enum PopoverMenuItemElement {
                  Link {
                    href: String,
                  },
                  Button {
                    href: String,
                    name: String,
                    value: String,
                  },
                }

                component Main {
                  <let {
                    el: PopoverMenuItemElement = PopoverMenuItemElement::Button {
                      href: "/path/to/some/page",
                      name: "button_name",
                      value: "button_value",
                    },
                  }>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_with_many_fields_wraps_to_multiple_lines() {
        check(
            indoc! {r#"
                record Button { href: String, name: String, value: String, dialog_trigger: String }
                component Main {
                  <let {btn: Button = Button {href: "/path/to/some/page", name: "button_name", value: "button_value", dialog_trigger: "dialog_trigger_value"}}>
                  </let>
                }
            "#},
            expect![[r#"
                record Button {
                  href: String,
                  name: String,
                  value: String,
                  dialog_trigger: String,
                }

                component Main {
                  <let {
                    btn: Button = Button {
                      href: "/path/to/some/page",
                      name: "button_name",
                      value: "button_value",
                      dialog_trigger: "dialog_trigger_value",
                    },
                  }>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn some_literal_stays_on_one_line_when_short() {
        check(
            indoc! {r#"
                component Main {
                  <let {x: Option[String] = Some("short")}></let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {x: Option[String] = Some("short")}>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn some_literal_inserts_soft_lines_when_long() {
        check(
            indoc! {r#"
                component Main(x: Option[String] = Some("this is a very long string that causes a line break because Some uses soft lines")) {
                }
            "#},
            expect![[r#"
                component Main(
                  x: Option[String] = Some(
                    "this is a very long string that causes a line break because Some uses soft lines"
                  ),
                ) {
                }
            "#]],
        );
    }

    #[test]
    fn html_with_string_and_expression_attributes_to_doc() {
        check(
            indoc! {r#"
                record Product { img_src: String }
                component ProductImage(product: Product) {
                  <img class="rounded-lg" src={product.img_src}>
                }
            "#},
            expect![[r#"
                record Product {
                  img_src: String,
                }

                component ProductImage(product: Product) {
                  <img class="rounded-lg" src={product.img_src}>
                }
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

                component Main(c: Option[String]) {
                  <match {c}>
                    <case {Some(x)}>
                      {x}
                    </case>
                    <case {None}>
                      green
                    </case>
                  </match>
                }
            "},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(c: Option[String]) {
                  <match {c}>
                    <case {Some(x)}>
                      {x}
                    </case>
                    <case {None}>
                      green
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn join_macro_expands_spaces_in_string_literals() {
        check(
            indoc! {r#"
                component Card {
                  <div class={join!("foo bar")}></div>
                }
            "#},
            expect![[r#"
                component Card {
                  <div class={
                    join!(
                      "foo",
                      "bar",
                    )
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn join_macro_expands_mixed_variables_and_literals() {
        check(
            indoc! {r#"
                component Card(a: String, b: String) {
                  <div class={join!(a, "foo bar", b)}></div>
                }
            "#},
            expect![[r#"
                component Card(
                  a: String,
                  b: String,
                ) {
                  <div class={
                    join!(
                      a,
                      "foo",
                      "bar",
                      b,
                    )
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn asset_macro_formats_inline() {
        check(
            indoc! {r#"
                component Main {
                  <img src={asset!("/logo.svg")} />
                }
            "#},
            expect![[r#"
                component Main {
                  <img src={asset!("/logo.svg")}>
                }
            "#]],
        );
    }

    #[test]
    fn should_format_deeply_nested_elements() {
        check(
            indoc! {r#"
                component Main {
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
                }
            "#},
            expect![[r#"
                component Main {
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
                }
            "#]],
        );
    }

    #[test]
    fn let_with_single_string_binding_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <let {name: String = "World"}>
                    Hello, {name}!
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {name: String = "World"}>
                    Hello, {name}!
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_single_int_binding_to_doc() {
        check(
            indoc! {"
                component Main {
                  <let {count: Int = 42}>
                    <span>{count}</span>
                  </let>
                }
            "},
            expect![[r#"
                component Main {
                  <let {count: Int = 42}>
                    <span>
                      {count}
                    </span>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_trailing_comma_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <let {name: String = "World",}>
                    {name}
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {name: String = "World"}>
                    {name}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_multiple_bindings_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <let {first: String = "Hello", second: String = "World"}>
                    {first} {second}
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {first: String = "Hello", second: String = "World"}>
                    {first} {second}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_three_bindings_to_doc() {
        check(
            indoc! {"
                component Main {
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>{a} + {b} + {c}</div>
                  </let>
                }
            "},
            expect![[r#"
                component Main {
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>
                      {a} + {b} + {c}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_expression_value_to_doc() {
        check(
            indoc! {"
                component Main(x: Int, y: Int) {
                  <let {sum: Int = x + y}>
                    <span>{sum}</span>
                  </let>
                }
            "},
            expect![[r#"
                component Main(
                  x: Int,
                  y: Int,
                ) {
                  <let {sum: Int = x + y}>
                    <span>
                      {sum}
                    </span>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_field_access_value_to_doc() {
        check(
            indoc! {"
                record User { name: String }
                component Main(user: User) {
                  <let {name: String = user.name}>
                    <div>{name}</div>
                  </let>
                }
            "},
            expect![[r#"
                record User {
                  name: String,
                }

                component Main(user: User) {
                  <let {name: String = user.name}>
                    <div>
                      {name}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn nested_let_tags_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <let {a: String = "outer"}>
                    <let {b: String = "inner"}>
                      {a} {b}
                    </let>
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {a: String = "outer"}>
                    <let {b: String = "inner"}>
                      {a} {b}
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_inside_if_to_doc() {
        check(
            indoc! {r#"
                component Main (show: Bool) {
                  <if {show}>
                    <let {msg: String = "visible"}>
                      {msg}
                    </let>
                  </if>
                }
            "#},
            expect![[r#"
                component Main(show: Bool) {
                  <if {show}>
                    <let {msg: String = "visible"}>
                      {msg}
                    </let>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn let_inside_for_to_doc() {
        check(
            indoc! {"
                component Main(items: Array[Int]) {
                  <for {item in items}>
                    <let {doubled: Int = item * 2}>
                      <span>{doubled}</span>
                    </let>
                  </for>
                }
            "},
            expect![[r#"
                component Main(items: Array[Int]) {
                  <for {item in items}>
                    <let {doubled: Int = item * 2}>
                      <span>
                        {doubled}
                      </span>
                    </let>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn multiple_sibling_let_tags_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <let {a: String = "first"}>
                    {a}
                  </let>
                  <let {b: String = "second"}>
                    {b}
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {a: String = "first"}>
                    {a}
                  </let>
                  <let {b: String = "second"}>
                    {b}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_empty_children_to_doc() {
        check(
            indoc! {r#"
                component Main {
                  <let {x: String = "unused"}></let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {x: String = "unused"}>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn let_with_long_bindings_breaks_to_multiple_lines() {
        check(
            indoc! {r#"
                component Main {
                  <let {
                    // a
                    first_name: String = "Hello",
                    // b
                    last_name: String = "World",
                    // c
                  }>
                    {first_name} {last_name}
                  </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {
                    // a
                    first_name: String = "Hello",
                    // b
                    last_name: String = "World",
                    // c
                  }>
                    {first_name} {last_name}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_import_declaration() {
        check(
            indoc! {"
                // External component
                import components::Button
                component Main {}
            "},
            expect![[r#"
                // External component
                import components::Button

                component Main {
                }
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
                component Main {
                  hello
                }
            "},
            expect![[r#"
                // Main component
                component Main {
                  hello
                }
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
                component Main {}
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
                component Main {
                }
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
                component Main {}
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
                component Main {
                }
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
                component Main(orientation: Orientation) {
                  <div class={match orientation {
                    // a
                    Orientation::Horizontal => "horizontal",
                    // b
                    Orientation::Vertical => "vertical",
                    // c
                  }}></div>
                }
            "#},
            expect![[r#"
                enum Orientation {
                  Horizontal,
                  Vertical,
                }

                component Main(orientation: Orientation) {
                  <div class={
                    match orientation {
                      // a
                      Orientation::Horizontal => "horizontal",
                      // b
                      Orientation::Vertical => "vertical",
                      // c
                    }
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_macro_arg() {
        check(
            indoc! {r#"
                component Main {
                  <div class={join!(
                    // base styles
                    "flex",
                    // conditional style
                    "items-center",
                    // more to come
                  )}></div>
                }
            "#},
            expect![[r#"
                component Main {
                  <div class={
                    join!(
                      // base styles
                      "flex",
                      // conditional style
                      "items-center",
                      // more to come
                    )
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_macro_arg_with_string_expansion() {
        check(
            indoc! {r#"
                component Main {
                  <div class={join!(
                    // base styles
                    "flex items-center",
                    // conditional style
                    "justify-between gap-4",
                    // more to come
                  )}></div>
                }
            "#},
            expect![[r#"
                component Main {
                  <div class={
                    join!(
                      // base styles
                      "flex",
                      "items-center",
                      // conditional style
                      "justify-between",
                      "gap-4",
                      // more to come
                    )
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_component_parameter() {
        check(
            indoc! {r#"
                component Button(
                    // The button label
                    label: String,
                    // Whether the button is disabled
                    disabled: Bool = false,
                    // More params to come
                ) {
                  {label}
                }
            "#},
            expect![[r#"
                component Button(
                  // The button label
                  label: String,
                  // Whether the button is disabled
                  disabled: Bool = false,
                  // More params to come
                ) {
                  {label}
                }
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_component_parameters_single_param() {
        check(
            indoc! {r#"
                component X(
                  x: String,
                  // ?
                ) {
                  {x}
                }
            "#},
            expect![[r#"
                component X(
                  x: String,
                  // ?
                ) {
                  {x}
                }
            "#]],
        );
    }

    #[test]
    fn join_macro_with_multiple_string_literals() {
        check(
            indoc! {r#"
                component Main {
                  <h1 class={join!("text-4xl", "font-bold", "tracking-tight", "dark:hover:text-blue-300")}>Hello</h1>
                }
            "#},
            expect![[r#"
                component Main {
                  <h1 class={
                    join!(
                      "text-4xl",
                      "font-bold",
                      "tracking-tight",
                      "dark:hover:text-blue-300",
                    )
                  }>
                    Hello
                  </h1>
                }
            "#]],
        );
    }

    #[test]
    fn component_invocation_with_single_long_attribute() {
        check(
            indoc! {r#"
                component Main {
                  <Button class={join!("text-4xl", "font-bold", "tracking-tight", "dark:hover:text-blue-300")}>Click me</Button>
                }
            "#},
            expect![[r#"
                component Main {
                  <Button class={
                    join!(
                      "text-4xl",
                      "font-bold",
                      "tracking-tight",
                      "dark:hover:text-blue-300",
                    )
                  }>
                    Click me
                  </Button>
                }
            "#]],
        );
    }

    #[test]
    fn simple_method_call_to_doc() {
        check(
            indoc! {"
                component Main(x: String) {
                  <div>{x.foo()}</div>
                }
            "},
            expect![[r#"
                component Main(x: String) {
                  <div>
                    {x.foo()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn chained_method_calls_to_doc() {
        check(
            indoc! {"
                component Main(x: String) {
                  <div>{x.foo().bar().baz()}</div>
                }
            "},
            expect![[r#"
                component Main(x: String) {
                  <div>
                    {x.foo().bar().baz()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn mixed_field_access_and_method_call_to_doc() {
        check(
            indoc! {"
                component Main(x: String) {
                  <div>{x.field.method()}</div>
                }
            "},
            expect![[r#"
                component Main(x: String) {
                  <div>
                    {x.field.method()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_then_field_access_to_doc() {
        check(
            indoc! {"
                component Main(x: String) {
                  <div>{x.method().field}</div>
                }
            "},
            expect![[r#"
                component Main(x: String) {
                  <div>
                    {x.method().field}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn float_literal_to_doc() {
        check(
            indoc! {"
                component Main {
                  <let {x: Float = 5.0}>
                    {x}
                  </let>
                }
            "},
            expect![[r#"
                component Main {
                  <let {x: Float = 5.0}>
                    {x}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn float_literals_small_values_to_doc() {
        check(
            indoc! {"
                component Main {
                  <let {a: Float = 0.000, b: Float = 0.001, c: Float = 0.002}>
                    {a}
                  </let>
                }
            "},
            expect![[r#"
                component Main {
                  <let {
                    a: Float = 0.000,
                    b: Float = 0.001,
                    c: Float = 0.002,
                  }>
                    {a}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn inline_text_with_nested_element_to_doc() {
        check(
            indoc! {"
                component Main {
                  <div>
                    foo<p>bar</p>
                  </div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    foo
                    <p>
                      bar
                    </p>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn nested_elements_inline_to_doc() {
        check(
            indoc! {"
                component Main {
                  <div><p>x</p></div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    <p>
                      x
                    </p>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn text_around_inline_element_to_doc() {
        check(
            indoc! {"
                component Main {
                  <div>hello <b>world</b>!</div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    hello{" "}
                    <b>
                      world
                    </b>
                    !
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn text_with_multiple_inline_links() {
        check(
            indoc! {r#"
                component Main {
                  <p>By clicking continue, you agree to our <a href="/tos">Terms of Service</a> and <a href="/privacy">Privacy Policy</a>.</p>
                }
            "#},
            expect![[r#"
                component Main {
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
                }
            "#]],
        );
    }

    #[test]
    fn empty_lines_between_text_collapsed_to_doc() {
        check(
            indoc! {"
                component Main {
                  <div>

                  foo

                  bar

                  </div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    foo
                    bar
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn text_around_void_element_to_doc() {
        check(
            indoc! {"
                component Main {
                    <div>hello <br> world</div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    hello{" "}
                    <br>
                    {" "}world
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn void_element_on_separate_line_to_doc() {
        check(
            indoc! {"
                component Main {
                    <div>
                        hello
                        <br>
                        world
                    </div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    hello
                    <br>
                    world
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn text_around_input_element_to_doc() {
        check(
            indoc! {r#"
                component Main {
                    <label>Name: <input type="text"> (required)</label>
                }
            "#},
            expect![[r#"
                component Main {
                  <label>
                    Name:{" "}
                    <input type="text">
                    {" "}(required)
                  </label>
                }
            "#]],
        );
    }

    #[test]
    fn input_element_on_separate_line_to_doc() {
        check(
            indoc! {r#"
                component Main {
                    <label>
                        Name:
                        <input type="text">
                        (required)
                    </label>
                }
            "#},
            expect![[r#"
                component Main {
                  <label>
                    Name:
                    <input type="text">
                    (required)
                  </label>
                }
            "#]],
        );
    }

    #[test]
    fn text_with_multiple_expressions_to_doc() {
        check(
            indoc! {"
                component Main(rating: String, num_reviews: String) {
                  <span>{rating} ({num_reviews} reviews)</span>
                }
            "},
            expect![[r#"
                component Main(
                  rating: String,
                  num_reviews: String,
                ) {
                  <span>
                    {rating} ({num_reviews} reviews)
                  </span>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_on_negated_int_preserves_parens() {
        check(
            indoc! {"
                component Main {
                  <div>{(-42).to_string()}</div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    {(-42).to_string()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_on_negated_float_preserves_parens() {
        check(
            indoc! {"
                component Main {
                  <div>{(-3.14).to_string()}</div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    {(-3.14).to_string()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_on_binary_expr_preserves_parens() {
        check(
            indoc! {"
                component Main {
                  <div>{(1 + 2).to_string()}</div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    {(1 + 2).to_string()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn field_access_on_negated_int_preserves_parens() {
        check(
            indoc! {"
                component Main {
                  <div>{(-42).foo}</div>
                }
            "},
            expect![[r#"
                component Main {
                  <div>
                    {(-42).foo}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn binary_expr_with_parens_preserves_precedence() {
        check(
            indoc! {"
                component Main(x: Int) {
                  <div>{(1 + 2) * 3}</div>
                }
            "},
            expect![[r#"
                component Main(x: Int) {
                  <div>
                    {(1 + 2) * 3}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn view_with_parameters_to_doc() {
        check(
            indoc! {r#"
                record LoginLogo { url: String, name: String }

                view LoginFormEntry(x: String, logo: LoginLogo) {
                  <div>
                    <div class="flex w-full max-w-sm flex-col gap-6">
                      <a
                        href={logo.url}
                        class="flex items-center gap-2 self-center font-medium"
                      >
                        {logo.name}
                      </a>
                    </div>
                    <div>
                      {x}
                    </div>
                  </div>
                }
            "#},
            expect![[r#"
                record LoginLogo {
                  url: String,
                  name: String,
                }

                view LoginFormEntry(
                  x: String,
                  logo: LoginLogo,
                ) {
                  <div>
                    <div class="flex w-full max-w-sm flex-col gap-6">
                      <a
                        href={logo.url}
                        class="flex items-center gap-2 self-center font-medium"
                      >
                        {logo.name}
                      </a>
                    </div>
                    <div>
                      {x}
                    </div>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn view_multiline_text() {
        check(
            indoc! {"
                view Test {
                  hello
                  world
                }
            "},
            expect![[r#"
                view Test {
                  hello
                  world
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_in_view() {
        check(
            indoc! {"
                view Test {
                  <!-- This is a comment -->
                  <div>hello</div>
                }
            "},
            expect![[r#"
                view Test {
                  <!-- This is a comment -->
                  <div>
                    hello
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_between_elements() {
        check(
            indoc! {"
                view Test {
                  <div>hello</div>
                  <!-- separator -->
                  <div>world</div>
                }
            "},
            expect![[r#"
                view Test {
                  <div>
                    hello
                  </div>
                  <!-- separator -->
                  <div>
                    world
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_inside_element() {
        check(
            indoc! {"
                view Test {
                  <div>
                    <!-- inner comment -->
                    <span>text</span>
                  </div>
                }
            "},
            expect![[r#"
                view Test {
                  <div>
                    <!-- inner comment -->
                    <span>
                      text
                    </span>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_only() {
        check(
            indoc! {"
                view Test {
                  <!-- just a comment -->
                }
            "},
            expect![[r#"
                view Test {
                  <!-- just a comment -->
                }
            "#]],
        );
    }
}
