use crate::document::DocumentRange;
use crate::hop::parsing::ParsedType;
use crate::hop::parsing::parsed_ast::{
    ParsedAst, ParsedDeclaration, ParsedEnumDeclaration, ParsedEnumDeclarationVariant,
    ParsedFieldDeclaration, ParsedFunctionDeclaration, ParsedImportDeclaration,
    ParsedPageDeclaration, ParsedParameter, ParsedRecordDeclaration,
};
use crate::hop::parsing::parsed_expr::{
    Constructor, ParsedArguments, ParsedExpr, ParsedLoopSource, ParsedMatchArm, ParsedMatchPattern,
};
use crate::hop::parsing::parsed_node::{ParsedAttribute, ParsedLetBinding, ParsedNode};
use crate::html::HtmlElementKind;
use pretty::{Arena, DocAllocator, DocBuilder};
use std::collections::VecDeque;

pub fn format(ast: &ParsedAst) -> String {
    let arena = Arena::new();
    format_ast(ast, &arena).pretty(60).to_string()
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
    let declarations = ast.declarations();
    let mut comments: VecDeque<_> = ast.comments().iter().collect();
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
    if !declarations.is_empty() {
        doc = doc.append(arena.line());
    }
    if !comments.is_empty() {
        if !declarations.is_empty() {
            doc = doc.append(arena.line());
        }
        doc = doc.append(drain_comments_before(arena, &mut comments, usize::MAX));
    }
    doc
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
        ParsedDeclaration::Page(page) => format_page_declaration(arena, page, comments),
        ParsedDeclaration::Function(function) => {
            format_function_declaration(arena, function, comments)
        }
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
        .append(arena.text(import.name.as_str()))
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
    field: &'a ParsedFieldDeclaration,
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
        for (i, field) in variant.fields.iter().enumerate() {
            if i > 0 {
                fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
            }
            let field_comments = drain_comments_before(arena, comments, field.name_range.start());
            let base = if let Some(e) = &field.examples {
                field_comments
                    .append(arena.text(e.to_annotation_string()))
                    .append(arena.hardline())
            } else {
                field_comments
            };
            fields_doc = fields_doc
                .append(base)
                .append(arena.text(field.name.to_string()))
                .append(arena.text(": "))
                .append(format_type(arena, &field.field_type));
        }
        fields_doc = fields_doc.append(arena.text(","));

        leading_comments
            .append(arena.text(variant.name.as_str()))
            .append(arena.text(" {"))
            .append(arena.line().append(fields_doc).nest(2).append(arena.line()))
            .append(arena.text("}"))
    }
}

fn format_page_declaration<'a>(
    arena: &'a Arena<'a>,
    page: &'a ParsedPageDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, page.range.start());

    // Format parameters (omit parentheses if no parameters)
    let params_doc = if page.params.is_empty() {
        arena.nil()
    } else {
        let mut params_inner = arena.nil();
        let force_multiline = page.params.len() >= 2;
        let line_break = if force_multiline {
            arena.hardline()
        } else {
            arena.line()
        };
        for (i, param) in page.params.iter().enumerate() {
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

    let pub_prefix = if page.pub_range.is_some() {
        arena.text("pub ")
    } else {
        arena.nil()
    };

    let mut members_doc = arena.nil();
    for (i, member) in page.members().enumerate() {
        if i > 0 {
            members_doc = members_doc.append(arena.hardline());
        }
        members_doc = members_doc.append(format_function_declaration(arena, member, comments));
    }

    let has_trailing_comments = comments
        .front()
        .is_some_and(|c| c.start() < page.range.end());
    let body_doc = if has_trailing_comments {
        let trailing_comments = drain_comments_before(arena, comments, page.range.end());
        arena
            .hardline()
            .append(members_doc)
            .append(arena.hardline())
            .append(trailing_comments)
            .nest(2)
    } else {
        arena
            .hardline()
            .append(members_doc)
            .nest(2)
            .append(arena.hardline())
    };

    leading_comments
        .append(pub_prefix)
        .append(arena.text("page "))
        .append(arena.text(page.name.as_str()))
        .append(params_doc)
        .append(arena.text(" {"))
        .append(body_doc)
        .append(arena.text("}"))
}

fn format_function_declaration<'a>(
    arena: &'a Arena<'a>,
    function: &'a ParsedFunctionDeclaration,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let leading_comments = drain_comments_before(arena, comments, function.name_range.start());

    let rest_param_doc = function
        .rest_param
        .as_ref()
        .map(|(name, _)| arena.text("...").append(arena.text(name.as_str())));
    let params_doc = if function.params.is_empty() && rest_param_doc.is_none() {
        arena.text("()")
    } else {
        let mut params_inner = arena.nil();
        let force_multiline = function.params.len() + rest_param_doc.is_some() as usize >= 2;
        let line_break = if force_multiline {
            arena.hardline()
        } else {
            arena.line()
        };
        for (i, param) in function.params.iter().enumerate() {
            if i > 0 {
                params_inner = params_inner
                    .append(arena.text(","))
                    .append(line_break.clone());
            }
            params_inner = params_inner.append(format_parameter(arena, param, comments));
        }
        if let Some(rest_doc) = rest_param_doc {
            if !function.params.is_empty() {
                params_inner = params_inner.append(arena.text(",")).append(line_break);
            }
            params_inner = params_inner.append(rest_doc);
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

    let body_leading_comments =
        drain_comments_before(arena, comments, function.body.range().start());
    let body_content =
        body_leading_comments.append(format_block_body(arena, &function.body, comments));
    let has_trailing_comments = comments
        .front()
        .is_some_and(|c| c.start() < function.range.end());
    let body_doc = if has_trailing_comments {
        let trailing_comments = drain_comments_before(arena, comments, function.range.end());
        arena
            .hardline()
            .append(body_content)
            .append(arena.hardline())
            .append(trailing_comments)
            .nest(2)
    } else {
        arena
            .hardline()
            .append(body_content)
            .nest(2)
            .append(arena.hardline())
    };

    let pub_prefix = if function.pub_range.is_some() {
        arena.text("pub ")
    } else {
        arena.nil()
    };
    leading_comments
        .append(pub_prefix)
        .append(arena.text("fn"))
        .append(arena.text(" "))
        .append(arena.text(function.name.as_str()))
        .append(params_doc)
        .append(arena.text(" -> "))
        .append(format_type(arena, &function.return_type))
        .append(arena.text(" {"))
        .append(body_doc)
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
    item: &'a ParsedAttribute,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match item {
        ParsedAttribute::KeyOnly { name } => arena.text(name.as_str()),
        ParsedAttribute::Expression { name, value } if matches!(value, ParsedExpr::Let { .. }) => {
            arena
                .text(name.as_str())
                .append(arena.text("="))
                .append(format_expr(arena, value, comments))
        }
        ParsedAttribute::Expression { name, value } => arena
            .text(name.as_str())
            .append(arena.text("={"))
            .append(
                arena
                    .line_()
                    .append(format_expr(arena, value, comments))
                    .nest(2),
            )
            .append(arena.line_())
            .append(arena.text("}"))
            .group(),
        ParsedAttribute::String { name, content, .. } => {
            let content = content.as_ref().map(|r| r.as_str()).unwrap_or("");
            arena
                .text(name.as_str())
                .append(arena.text("=\""))
                .append(arena.text(content))
                .append(arena.text("\""))
        }
        ParsedAttribute::Spread { name, .. } => arena.text("...").append(arena.text(name.as_str())),
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
        ParsedNode::Interpolation { expression, .. } => {
            format_braced_expr(arena, expression, comments)
        }
        ParsedNode::FunctionInvocation {
            function_name,
            attributes,
            children,
            ..
        } => {
            let function_name_str = function_name.as_str();
            let opening_tag_doc = if attributes.is_empty() {
                arena.text("<").append(arena.text(function_name_str))
            } else if attributes.len() == 1 {
                // Single attribute: keep on same line as tag
                arena
                    .text("<")
                    .append(arena.text(function_name_str))
                    .append(arena.text(" "))
                    .append(format_attribute(arena, &attributes[0], comments))
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
                    .append(arena.text(function_name_str))
                    .append(arena.line().append(attrs_doc).nest(2))
                    .append(arena.line_())
                    .group()
            };
            // Preserve the authored form, `<Card/>` vs `<Card></Card>`.
            match children {
                None => opening_tag_doc.append(arena.text("/>")),
                Some(children) => opening_tag_doc
                    .append(arena.text(">"))
                    .append(format_children(arena, children, comments))
                    .append(arena.text("</"))
                    .append(arena.text(function_name_str))
                    .append(arena.text(">")),
            }
        }
        ParsedNode::Fragment { children, .. } if children.is_empty() => arena.text("<></>"),
        ParsedNode::Fragment { children, .. } => arena
            .text("<>")
            .append(format_children(arena, children, comments))
            .append(arena.text("</>")),
        ParsedNode::Comment { range } => arena.text(range.as_str()),
        ParsedNode::HtmlElement {
            kind: element,
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
                let sep =
                    if *element == HtmlElementKind::Script || *element == HtmlElementKind::Style {
                        arena.nil()
                    } else {
                        arena.line()
                    };
                opening_tag_doc
                    .append(sep)
                    .append(arena.text("</"))
                    .append(arena.text(element_str))
                    .append(arena.text(">"))
            } else if *element == HtmlElementKind::Script || *element == HtmlElementKind::Style {
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

        // Decide separator: line break if preceded by a newline, or if either
        // side is not Text, in which case the break emits nothing.
        let need_break = if let Some(prev) = prev_node {
            preceded_by_newline
                || !matches!(prev, ParsedNode::Text { .. })
                || !matches!(child, ParsedNode::Text { .. })
        } else {
            false
        };

        if need_break {
            doc = doc.append(arena.line_());
        }

        if let ParsedNode::Text { range } = child {
            let text = range.as_str();
            let leading_ws = &text[..text.len() - text.trim_start().len()];
            let trailing_ws = &text[text.trim_end().len()..];
            let trimmed = text.trim();
            let prev_forces_break =
                prev_node.is_some_and(|n| !matches!(n, ParsedNode::Text { .. }));
            let next_forces_break =
                next_node.is_some_and(|n| !matches!(n, ParsedNode::Text { .. }));

            let leading_needs_space =
                !leading_ws.is_empty() && prev_forces_break && !preceded_by_newline;
            let trailing_needs_space = !trailing_ws.is_empty() && next_forces_break;

            if trimmed.is_empty() {
                if leading_needs_space || trailing_needs_space {
                    doc = doc.append(escaped_whitespace(arena, text));
                } else {
                    doc = doc.append(arena.text(text));
                }
            } else {
                if leading_needs_space {
                    doc = doc
                        .append(escaped_whitespace(arena, leading_ws))
                        .append(arena.line_());
                } else {
                    doc = doc.append(arena.text(leading_ws));
                }
                doc = doc.append(arena.text(trimmed));
                if trailing_needs_space {
                    doc = doc
                        .append(arena.line_())
                        .append(escaped_whitespace(arena, trailing_ws));
                } else {
                    doc = doc.append(arena.text(trailing_ws));
                }
            }
        } else {
            doc = doc.append(format_node(arena, child, comments));
        }
    }

    arena.line_().append(doc).nest(2).append(arena.line_())
}

/// Render a run of whitespace as an interpolation, so that it survives the
/// line break the formatter puts next to it.
fn escaped_whitespace<'a>(arena: &'a Arena<'a>, whitespace: &str) -> DocBuilder<'a, Arena<'a>> {
    arena.text(format!("{{\"{whitespace}\"}}"))
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

fn format_loop_source<'a>(
    arena: &'a Arena<'a>,
    source: &'a ParsedLoopSource,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match source {
        ParsedLoopSource::Array(expr) => format_expr_before_brace(arena, expr, comments),
        ParsedLoopSource::RangeInclusive { start, end } => {
            format_expr_before_brace(arena, start, comments)
                .append(arena.text("..="))
                .append(format_expr_before_brace(arena, end, comments))
        }
    }
}

/// Format an expression that a `{` follows, as the subject of a `match` or
/// the source of a `for`. The parser refuses a record or enum literal with
/// a field list there unless it is inside a delimiter, so an expression
/// that has one outside any is parenthesized as a whole.
fn format_expr_before_brace<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    if contains_exterior_record_literal(expr) {
        arena
            .text("(")
            .append(format_expr(arena, expr, comments))
            .append(arena.text(")"))
    } else {
        format_expr(arena, expr, comments)
    }
}

/// Whether the expression, as printed, holds a record or enum literal with
/// a field list outside every delimiter. Only operands that print without
/// parentheses of their own are looked into.
fn contains_exterior_record_literal(expr: &ParsedExpr) -> bool {
    match expr {
        ParsedExpr::RecordLiteral { .. } => true,
        ParsedExpr::EnumLiteral { fields, .. } => !fields.is_empty(),
        ParsedExpr::BinaryOp {
            left,
            operator,
            right,
            ..
        } => {
            let (left_power, right_power) = operator.binding_power();
            (left.binding_power() >= left_power && contains_exterior_record_literal(left))
                || (right.binding_power() >= right_power && contains_exterior_record_literal(right))
        }
        ParsedExpr::BooleanNegation { operand, .. }
        | ParsedExpr::NumericNegation { operand, .. } => {
            operand.binding_power() >= ParsedExpr::PREFIX_BINDING_POWER
                && contains_exterior_record_literal(operand)
        }
        ParsedExpr::FieldAccess { record, .. } => {
            record.binding_power() >= ParsedExpr::POSTFIX_BINDING_POWER
                && contains_exterior_record_literal(record)
        }
        ParsedExpr::MethodCall { receiver, .. } => {
            receiver.binding_power() >= ParsedExpr::POSTFIX_BINDING_POWER
                && contains_exterior_record_literal(receiver)
        }
        _ => false,
    }
}

fn format_type<'a>(arena: &'a Arena<'a>, ty: &ParsedType) -> DocBuilder<'a, Arena<'a>> {
    match ty {
        ParsedType::String { .. } => arena.text("String"),
        ParsedType::Bool { .. } => arena.text("Bool"),
        ParsedType::Int { .. } => arena.text("Int"),
        ParsedType::Float { .. } => arena.text("Float"),
        ParsedType::Html { .. } => arena.text("Html"),
        ParsedType::Option { element, .. } => arena
            .text("Option[")
            .append(format_type(arena, element))
            .append(arena.text("]")),
        ParsedType::Array { element, .. } => arena
            .text("Array[")
            .append(format_type(arena, element))
            .append(arena.text("]")),
        ParsedType::Tuple { elements, .. } => arena
            .text("(")
            .append(arena.intersperse(
                elements.iter().map(|element| format_type(arena, element)),
                arena.text(", "),
            ))
            .append(if elements.len() == 1 {
                arena.text(",")
            } else {
                arena.nil()
            })
            .append(arena.text(")")),
        ParsedType::Named { name, .. } => arena.text(name.to_string()),
    }
}

/// Formats an expression that the surrounding syntax wraps in `{` `}`. A
/// `let` chain brings its own braces and layout, so it is not wrapped again.
fn format_braced_expr<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    if let ParsedExpr::Let { .. } = expr {
        format_expr(arena, expr, comments)
    } else {
        arena
            .text("{")
            .append(format_expr(arena, expr, comments))
            .append(arena.text("}"))
    }
}

/// Formats the inside of a block whose braces are supplied by the caller:
/// one `let` statement per line, then the tail expression.
fn format_block_body<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    let mut doc = arena.nil();
    let mut expr = expr;
    while let ParsedExpr::Let { binding, body, .. } = expr {
        doc = doc
            .append(drain_comments_before(arena, comments, expr.range().start()))
            .append(arena.text("let "))
            .append(format_let_binding(arena, binding, comments))
            .append(arena.text(";"))
            .append(arena.hardline());
        expr = body;
    }
    doc.append(drain_comments_before(arena, comments, expr.range().start()))
        .append(format_expr(arena, expr, comments))
}

fn format_expr<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    match expr {
        ParsedExpr::VariableReference { value, .. } => arena.text(value.as_str()),
        ParsedExpr::Let { .. } => arena
            .text("{")
            .append(
                arena
                    .hardline()
                    .append(format_block_body(arena, expr, comments))
                    .nest(2),
            )
            .append(arena.hardline())
            .append(arena.text("}")),
        ParsedExpr::For {
            var_name,
            source,
            body,
            ..
        } => arena
            .text("for ")
            .append(match var_name {
                Some(name) => arena.text(name.as_str()),
                None => arena.text("_"),
            })
            .append(arena.text(" in "))
            .append(format_loop_source(arena, source, comments))
            .append(arena.text(" {"))
            .append(
                arena
                    .hardline()
                    .append(format_block_body(arena, body, comments))
                    .nest(2),
            )
            .append(arena.hardline())
            .append(arena.text("}")),
        ParsedExpr::Markup { node } => format_node(arena, node, comments),
        ParsedExpr::FieldAccess {
            record: object,
            field,
            ..
        } => format_expr_in_slot(arena, object, ParsedExpr::POSTFIX_BINDING_POWER, comments)
            .append(arena.text("."))
            .append(arena.text(field.as_str())),
        ParsedExpr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let callee =
                format_expr_in_slot(arena, receiver, ParsedExpr::POSTFIX_BINDING_POWER, comments)
                    .append(arena.text("."))
                    .append(arena.text(method.as_str()));
            if args.is_empty() {
                callee.append(arena.text("()"))
            } else {
                let mut args_doc = arena.nil();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        args_doc = args_doc.append(arena.text(",")).append(arena.line());
                    }
                    args_doc = args_doc.append(format_expr(arena, arg, comments));
                }
                callee
                    .append(arena.text("("))
                    .append(soft_block(arena, args_doc))
                    .append(arena.text(")"))
            }
        }
        ParsedExpr::StringLiteral { value, .. } => arena
            .text("\"")
            .append(arena.text(value.as_raw_str()))
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
            spread,
            ..
        } => {
            if fields.is_empty() && spread.is_none() {
                arena.text(record_name.as_str()).append(arena.text(" {}"))
            } else {
                // The spread is canonicalized to first position.
                let mut fields_doc = arena.nil();
                if let Some(subject) = spread {
                    fields_doc = fields_doc
                        .append(arena.text("..."))
                        .append(format_expr(arena, subject, comments));
                }
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 || spread.is_some() {
                        fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
                    }
                    fields_doc = fields_doc
                        .append(arena.text(field.name.as_str()))
                        .append(arena.text(": "))
                        .append(format_expr(arena, &field.value, comments));
                }
                arena
                    .text(record_name.as_str())
                    .append(arena.text(" {"))
                    .append(soft_block(arena, fields_doc))
                    .append(arena.text("}"))
            }
        }
        ParsedExpr::BinaryOp {
            left,
            operator,
            right,
            ..
        } => {
            let (left_power, right_power) = operator.binding_power();
            format_expr_in_slot(arena, left, left_power, comments)
                .append(arena.text(" "))
                .append(arena.text(operator.as_str()))
                .append(arena.text(" "))
                .append(format_expr_in_slot(arena, right, right_power, comments))
        }
        ParsedExpr::BooleanNegation { operand, .. } => arena.text("!").append(format_expr_in_slot(
            arena,
            operand,
            ParsedExpr::PREFIX_BINDING_POWER,
            comments,
        )),
        ParsedExpr::NumericNegation { operand, .. } => arena.text("-").append(format_expr_in_slot(
            arena,
            operand,
            ParsedExpr::PREFIX_BINDING_POWER,
            comments,
        )),
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
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        fields_doc = fields_doc.append(arena.text(",")).append(arena.line());
                    }
                    fields_doc = fields_doc
                        .append(arena.text(field.name.to_string()))
                        .append(arena.text(": "))
                        .append(format_expr(arena, &field.value, comments));
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
                    .append(format_expr_before_brace(arena, subject, comments))
                    .append(arena.text(" {}"))
            } else if arms.is_empty() {
                let trailing_comments = drain_comments_before(arena, comments, end_position);
                arena
                    .text("match ")
                    .append(format_expr_before_brace(arena, subject, comments))
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
                    .append(format_expr_before_brace(arena, subject, comments))
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
                for e in args {
                    let leading_comments =
                        drain_comments_before(arena, comments, e.range().start());
                    match e {
                        ParsedExpr::StringLiteral { value, .. } => {
                            let parts: Vec<_> = value.as_raw_str().split_whitespace().collect();
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
                for e in args {
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
        ParsedExpr::FunctionCall { name, args, .. } => {
            let is_empty = match args {
                ParsedArguments::Positional(values) => values.is_empty(),
                ParsedArguments::Named(named) => named.is_empty(),
            };
            if is_empty {
                arena.text(name.as_str()).append(arena.text("()"))
            } else {
                let mut args_doc = arena.nil();
                match args {
                    ParsedArguments::Positional(values) => {
                        for (i, value) in values.iter().enumerate() {
                            if i > 0 {
                                args_doc = args_doc.append(arena.text(",")).append(arena.line());
                            }
                            args_doc = args_doc.append(format_expr(arena, value, comments));
                        }
                    }
                    ParsedArguments::Named(named) => {
                        for (i, arg) in named.iter().enumerate() {
                            if i > 0 {
                                args_doc = args_doc.append(arena.text(",")).append(arena.line());
                            }
                            args_doc = args_doc
                                .append(arena.text(arg.name.as_str()))
                                .append(arena.text(": "))
                                .append(format_expr(arena, &arg.value, comments));
                        }
                    }
                }
                arena
                    .text(name.as_str())
                    .append(arena.text("("))
                    .append(soft_block(arena, args_doc))
                    .append(arena.text(")"))
            }
        }
    }
}

/// Formats an expression placed in an operand slot of the given binding power,
/// adding parentheses if the expression does not bind tightly enough for it.
fn format_expr_in_slot<'a>(
    arena: &'a Arena<'a>,
    expr: &'a ParsedExpr,
    slot_binding_power: u8,
    comments: &mut VecDeque<&'a DocumentRange>,
) -> DocBuilder<'a, Arena<'a>> {
    if expr.binding_power() < slot_binding_power {
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
    let body = format_expr(arena, &arm.body, comments);
    let body = match &arm.body {
        ParsedExpr::Markup { .. } => arena
            .text("{")
            .flat_alt(arena.nil())
            .append(arena.line_().append(body).nest(2))
            .append(arena.line_())
            .append(arena.text("}").flat_alt(arena.nil())),
        _ => body,
    };
    leading_comments
        .append(format_match_pattern(arena, &arm.pattern))
        .append(arena.text(" => "))
        .append(body)
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
                if matches!(constructor, Constructor::Record { .. }) {
                    base.append(arena.text(" {}"))
                } else {
                    base
                }
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
    use super::format;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::document::Document;
    use crate::document_id::DocumentId;
    use crate::hop::parsing::parse;
    use crate::hop::parsing::source_generator;

    fn check(source: &str, expected: Expect) {
        let mut errors = Vec::new();
        let document_id = DocumentId::new("test.hop").unwrap();
        let ast = parse::parse(
            document_id.clone(),
            Document::new(document_id, source.to_string()),
            &mut errors,
        );
        if !errors.is_empty() {
            panic!("Parse errors: {:?}", errors);
        }
        let formatted = format(&ast);
        expected.assert_eq(&formatted);

        let document_id = DocumentId::new("test.hop").unwrap();
        let formatted_twice = format(&parse::parse(
            document_id.clone(),
            Document::new(document_id, formatted.clone()),
            &mut errors,
        ));
        if !errors.is_empty() {
            panic!("Formatted output does not parse: {:?}", errors);
        }
        assert_eq!(formatted, formatted_twice, "Formatter is not idempotent");
    }

    #[test]
    fn fuzz_generated_sources_parse_after_formatting_and_format_is_idempotent() {
        arbtest::arbtest(|u| {
            let source = source_generator::random_source(u)?;
            let document_id = DocumentId::new("test.hop").unwrap();
            let mut errors = Vec::new();
            let ast = parse::parse(
                document_id.clone(),
                Document::new(document_id.clone(), source.clone()),
                &mut errors,
            );
            assert!(
                errors.is_empty(),
                "parse errors: {errors:?}\n\nsource:\n{source}"
            );
            let formatted = format(&ast);
            let formatted_ast = parse::parse(
                document_id.clone(),
                Document::new(document_id, formatted.clone()),
                &mut errors,
            );
            assert!(
                errors.is_empty(),
                "formatted output does not parse: {errors:?}\n\nsource:\n{source}\n\nformatted:\n{formatted}"
            );
            let formatted_twice = format(&formatted_ast);
            assert_eq!(
                formatted, formatted_twice,
                "formatting is not idempotent\n\nsource:\n{source}\n\nformatted:\n{formatted}"
            );
            Ok(())
        });
    }

    #[test]
    fn subject_before_brace_is_parenthesized_only_around_braced_literals() {
        check(
            indoc! {"
                fn f() -> Int {
                  match (Color::Red) { Color::Red => 1, _ => 0 }
                }

                fn g() -> Int {
                  match (Point { x: 1 }) { _ => 0 }
                }

                fn h() -> Int {
                  match (Point { x: 1 }).x == 1 { true => 1, false => 0 }
                }

                fn i() -> Int {
                  match !(Point::XY { x: 1 } == p) { true => 1, false => 0 }
                }

                fn j() -> Int {
                  match g(Point { x: 1 }) { _ => 0 }
                }

                fn k() -> Int {
                  for x in (Point { x: 1 }) { 1 }
                }

                fn l() -> Int {
                  for x in (Point { x: 1 }).x..=(Point { x: 2 }.x) { 1 }
                }
            "},
            expect![[r#"
                fn f() -> Int {
                  match Color::Red {Color::Red => 1, _ => 0}
                }

                fn g() -> Int {
                  match (Point {x: 1}) {_ => 0}
                }

                fn h() -> Int {
                  match (Point {x: 1}.x == 1) {true => 1, false => 0}
                }

                fn i() -> Int {
                  match !(Point::XY {x: 1} == p) {true => 1, false => 0}
                }

                fn j() -> Int {
                  match g(Point {x: 1}) {_ => 0}
                }

                fn k() -> Int {
                  for x in (Point {x: 1}) {
                    1
                  }
                }

                fn l() -> Int {
                  for x in (Point {x: 1}.x)..=(Point {x: 2}.x) {
                    1
                  }
                }
            "#]],
        );
    }

    #[test]
    fn named_call_arguments() {
        check(
            indoc! {r#"
                fn label(prefix: String, count: Int) -> String {
                  prefix + count.to_string()
                }

                page Test() {
                  fn body() -> Html {
                    <div>{label(count: 2, prefix: "n")}{label("a", 1)}</div>
                  }
                }
            "#},
            expect![[r#"
                fn label(
                  prefix: String,
                  count: Int,
                ) -> String {
                  prefix + count.to_string()
                }

                page Test {
                  fn body() -> Html {
                    <div>
                      {label(count: 2, prefix: "n")}
                      {label("a", 1)}
                    </div>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn pub_record() {
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
    fn pub_enum() {
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
    fn pub_function() {
        check(
            indoc! {"
                pub fn Button(label: String) -> Html {
                  <button>{label}</button>
                }

                pub fn label(x: Int) -> Int { x + 10 }
            "},
            expect![[r#"
                pub fn Button(label: String) -> Html {
                  <button>
                    {label}
                  </button>
                }

                pub fn label(x: Int) -> Int {
                  x + 10
                }
            "#]],
        );
    }

    #[test]
    fn pub_page() {
        check(
            indoc! {"
                pub page Home() {
                  fn body() -> Html {
                    <div>hi</div>
                  }
                }
            "},
            expect![[r#"
                pub page Home {
                  fn body() -> Html {
                    <div>
                      hi
                    </div>
                  }
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
                pub fn C() -> Html {<p>hi</p>}
            "},
            expect![[r#"
                pub record A {
                  x: Int,
                }

                record B {
                  y: Int,
                }

                pub fn C() -> Html {
                  <p>
                    hi
                  </p>
                }
            "#]],
        );
    }

    #[test]
    fn import_declaration() {
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
    fn import_of_function_declaration() {
        check(
            indoc! {"
                import foo::bar
            "},
            expect![[r#"
                import foo::bar
            "#]],
        );
    }

    #[test]
    fn import_declaration_with_trivia_around_path_separators() {
        check(
            indoc! {"
                import foo :: nested
                  :: Bar
            "},
            expect![[r#"
                import foo::nested::Bar
            "#]],
        );
    }

    #[test]
    fn multiple_import_declarations() {
        check(
            indoc! {"
                import foo::Bar
                import baz::Qux
                import functions::Button
                record User { name: String }
            "},
            expect![[r#"
                import foo::Bar
                import baz::Qux
                import functions::Button

                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn record_declaration_single_field() {
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
    fn enum_declaration_multiple_variants() {
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
    fn two_record_declarations() {
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
    fn function_declaration() {
        check(
            indoc! {"
                fn Main(name: String, count: Int) -> Html {
                  <div>{name}</div>
                }
            "},
            expect![[r#"
                fn Main(
                  name: String,
                  count: Int,
                ) -> Html {
                  <div>
                    {name}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn function_declaration_with_many_parameters() {
        check(
            indoc! {"
                fn Main(first_name: String, last_name: String, email: String, age: Int, active: Bool, role: String) -> Html {<></>}
            "},
            expect![[r#"
                fn Main(
                  first_name: String,
                  last_name: String,
                  email: String,
                  age: Int,
                  active: Bool,
                  role: String,
                ) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn let_in_function_body() {
        check(
            indoc! {r#"
                fn Greeting(first: String, last: String) -> Html {
                  let name = first + " " + last; let greeting: String = "Hello " + name;
                  <h1>{greeting}</h1>
                }
            "#},
            expect![[r#"
                fn Greeting(
                  first: String,
                  last: String,
                ) -> Html {
                  let name = first + " " + last;
                  let greeting: String = "Hello " + name;
                  <h1>
                    {greeting}
                  </h1>
                }
            "#]],
        );
    }

    #[test]
    fn let_in_match_arm_and_redundant_braces() {
        check(
            indoc! {r#"
                fn Main(title: Option[String]) -> String {
                  match title { Some(t) => { let s = t + " "; s }, None => { "" } }
                }
            "#},
            expect![[r#"
                fn Main(title: Option[String]) -> String {
                  match title {
                    Some(t) => {
                      let s = t + " ";
                      s
                    },
                    None => "",
                  }
                }
            "#]],
        );
    }

    #[test]
    fn let_in_interpolation_and_attribute() {
        check(
            indoc! {r#"
                fn Main(a: Int) -> Html {
                  <div class={ let base = "btn"; base }>{ let b = a + 1; b }</div>
                }
            "#},
            expect![[r#"
                fn Main(a: Int) -> Html {
                  <div class={
                    let base = "btn";
                    base
                  }>
                    {
                      let b = a + 1;
                      b
                    }
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn let_as_match_subject() {
        check(
            indoc! {r#"
                fn Main(a: Int) -> Html {
                  match { let n = a; n == 1 } {
                    true => {
                      match { let m = a; m == 1 } {
                        true => <>one</>,
                        false => <></>,
                      }
                    },
                    false => <></>,
                  }
                }
            "#},
            expect![[r#"
                fn Main(a: Int) -> Html {
                  match {
                    let n = a;
                    n == 1
                  } {
                    true => match {
                      let m = a;
                      m == 1
                    } {true => <>one</>, false => <></>},
                    false => {
                      <></>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn let_as_operand_and_as_let_value() {
        check(
            indoc! {r#"
                fn Main() -> Int {
                  let x = { let a = 1; a };
                  x + { let b = 2; b }
                }
            "#},
            expect![[r#"
                fn Main() -> Int {
                  let x = {
                    let a = 1;
                    a
                  };
                  x + {
                    let b = 2;
                    b
                  }
                }
            "#]],
        );
    }

    #[test]
    fn comments_between_let_statements() {
        check(
            indoc! {r#"
                fn Main() -> Int {
                  // first
                  let a = 1;
                  // second
                  let b = 2;
                  // tail
                  a + b
                }
            "#},
            expect![[r#"
                fn Main() -> Int {
                  // first
                  let a = 1;
                  // second
                  let b = 2;
                  // tail
                  a + b
                }
            "#]],
        );
    }

    #[test]
    fn let_in_interpolation_with_markup_tail() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <ul>
                      {
                        let label = "Item";
                        let count = 2;
                        <li class={ let base = "row"; base + "-" + "odd" }>{label}: {count.to_string()}</li>
                      }
                    </ul>
                  }
                }
            "#},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <ul>
                      {
                        let label = "Item";
                        let count = 2;
                        <li class={
                          let base = "row";
                          base + "-" + "odd"
                        }>
                          {label}
                          :
                          {" "}
                          {count.to_string()}
                        </li>
                      }
                    </ul>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn function_with_match_expression() {
        check(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                fn Main(color: Color) -> Html {
                  <div class={match color { Color::Red => "red", Color::Green => "green", Color::Blue => "blue" }}></div>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                fn Main(color: Color) -> Html {
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
                fn Main(color: Color) -> Html {
                  <div class={match color { Color::Red{} => "red" }}></div>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                }

                fn Main(color: Color) -> Html {
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
                fn Main(result: Outcome) -> Html {
                  match result {
                    Outcome::Success {value} => <>{value}</>,
                    Outcome::Failure {message} => <>{message}</>,
                  }
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

                fn Main(result: Outcome) -> Html {
                  match result {
                    Outcome::Success {value} => {
                      <>
                        {value}
                      </>
                    },
                    Outcome::Failure {message} => {
                      <>
                        {message}
                      </>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn match_pattern_with_mixed_shorthand_and_explicit_bindings() {
        check(
            indoc! {r#"
                enum Event { Click {x: Int, y: Int} }
                fn Main(event: Event) -> Html {
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

                fn Main(event: Event) -> Html {
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
                fn Main(result: Outcome) -> Html {
                  match result {
                    Outcome::Success {value: value} => <>{value}</>,
                  }
                }
            "#},
            expect![[r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                }

                fn Main(result: Outcome) -> Html {
                  match result {Outcome::Success {value} => <>{value}</>}
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
                fn Main(event: Event) -> Html {
                  match event {
                    Event::Button {type, name, value, dialog_trigger, popover_trigger} => <></>,
                  }
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

                fn Main(event: Event) -> Html {
                  match event {
                    Event::Button {
                      type,
                      name,
                      value,
                      dialog_trigger,
                      popover_trigger,
                    } => {
                      <></>
                    },
                  }
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
                fn Main() -> Html {<></>}
            "#},
            expect![[r#"
                enum Foo {
                  Bar,
                }

                fn Main() -> Html {
                  <></>
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
    fn function_declaration_with_text_child() {
        check(
            indoc! {"
                fn Main() -> Html {hello}
            "},
            expect![[r#"
                fn Main() -> Html {
                  hello
                }
            "#]],
        );
    }

    #[test]
    fn html_with_class_and_expression() {
        check(
            indoc! {r#"
                record Character { name: String }
                fn Main(character: Character) -> Html {
                  <h1 class="text-2xl font-bold">{character.name}</h1>
                }
            "#},
            expect![[r#"
                record Character {
                  name: String,
                }

                fn Main(character: Character) -> Html {
                  <h1 class="text-2xl font-bold">
                    {character.name}
                  </h1>
                }
            "#]],
        );
    }

    #[test]
    fn html_with_single_class_expression() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  <div class={"p-2"}></div>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <div class={"p-2"}>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn match_subject_with_equality() {
        check(
            indoc! {"
                fn Main(a: String, b: String) -> Html {
                  match a == b {
                    true => <div>equal</div>,
                    false => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(
                  a: String,
                  b: String,
                ) -> Html {
                  match a == b {true => <div>equal</div>, false => <></>}
                }
            "#]],
        );
    }

    #[test]
    fn match_subject_with_logical_and() {
        check(
            indoc! {"
                fn Main(a: Bool, b: Bool) -> Html {
                  match a && b {
                    true => <div>both true</div>,
                    false => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(
                  a: Bool,
                  b: Bool,
                ) -> Html {
                  match a && b {
                    true => {
                      <div>
                        both true
                      </div>
                    },
                    false => {
                      <></>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn match_subject_with_nested_logical_operators() {
        check(
            indoc! {"
                fn Main(a: Bool, b: Bool, c: Bool) -> Html {
                  match a && b || c {
                    true => <div>complex</div>,
                    false => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(
                  a: Bool,
                  b: Bool,
                  c: Bool,
                ) -> Html {
                  match a && b || c {
                    true => {
                      <div>
                        complex
                      </div>
                    },
                    false => {
                      <></>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn match_subject_with_negation() {
        check(
            indoc! {"
                fn Main(a: Bool) -> Html {
                  match !a {
                    true => <div>not a</div>,
                    false => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(a: Bool) -> Html {
                  match !a {true => <div>not a</div>, false => <></>}
                }
            "#]],
        );
    }

    #[test]
    fn match_subject_with_negated_equality() {
        check(
            indoc! {"
                fn Main(a: String, b: String) -> Html {
                  match !(a == b) {
                    true => <div>not equal</div>,
                    false => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(
                  a: String,
                  b: String,
                ) -> Html {
                  match !(a == b) {
                    true => {
                      <div>
                        not equal
                      </div>
                    },
                    false => {
                      <></>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_multiline_text() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <>
                    hello
                    world
                  </>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <>
                    hello
                    world
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_nested_html() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <div>
                    content
                  </div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <div>
                    content
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn splits_two_text_expressions_onto_their_own_lines() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  let hello = "Hello";
                  let world = "World";
                  <>{hello} {world}</>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  let hello = "Hello";
                  let world = "World";
                  <>
                    {hello}
                    {" "}
                    {world}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn adds_a_space_between_a_text_expression_and_a_tag_on_the_same_line() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  let hello = "Hello";
                  let world = "World";
                  <>{hello} <b>{world}</b></>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  let hello = "Hello";
                  let world = "World";
                  <>
                    {hello}
                    {" "}
                    <b>
                      {world}
                    </b>
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn adds_a_space_between_two_tags_on_same_line() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                    <><i>i</i> <b>b</b></>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <>
                    <i>
                      i
                    </i>
                    {" "}
                    <b>
                      b
                    </b>
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn adds_a_space_expression_between_text_and_tag_on_single_line() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <>hello <b>world</b></>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <>
                    hello
                    {" "}
                    <b>
                      world
                    </b>
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn keeps_a_run_of_spaces_beside_a_tag() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <>a  <b>x</b></>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <>
                    a
                    {"  "}
                    <b>
                      x
                    </b>
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn keeps_whitespace_on_the_side_that_has_no_linebreak() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  <><b>x</b>  a {"y"}</>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <>
                    <b>
                      x
                    </b>
                    {"  "}
                    a
                    {" "}
                    {"y"}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_removal_empty_lines() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <>

                    hello

                  </>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <>
                    hello
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn whitespace_between_script_tags_is_stripped() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  <script src="/app.js">
                        </script>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <script src="/app.js"></script>
                }
            "#]],
        );
    }

    #[test]
    fn nested_functions_with_record_attributes() {
        check(
            indoc! {r#"
                fn IconsPage() -> Html {
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
                fn IconsPage() -> Html {
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
    fn function_with_expression_attribute() {
        check(
            indoc! {r#"
                import hop::ui::lucide::ChevronDown

                fn NativeSelect() -> Html {
                  <ChevronDown
                    class={"text-muted-foreground"}
                  />
                }
            "#},
            expect![[r#"
                import hop::ui::lucide::ChevronDown

                fn NativeSelect() -> Html {
                  <ChevronDown class={"text-muted-foreground"}/>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_string_concatenation_attribute() {
        check(
            indoc! {r#"
                record Product { id: String }
                fn IconShowPage(product: Product) -> Html {
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                }
            "#},
            expect![[r#"
                record Product {
                  id: String,
                }

                fn IconShowPage(product: Product) -> Html {
                  <Button href={"/download/" + product.id}>
                    hello
                  </Button>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_string_parameter() {
        check(
            indoc! {r#"
                fn Greeting(name: String = "World") -> Html {
                  <>Hello, {name}!</>
                }
            "#},
            expect![[r#"
                fn Greeting(name: String = "World") -> Html {
                  <>
                    Hello,
                    {" "}
                    {name}
                    !
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_int_parameter() {
        check(
            indoc! {"
                fn Counter(count: Int = 0) -> Html {
                  <>
                    {count}
                  </>
                }
            "},
            expect![[r#"
                fn Counter(count: Int = 0) -> Html {
                  <>
                    {count}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_bool_parameter() {
        check(
            indoc! {"
                fn Toggle(enabled: Bool = true) -> Html {<></>}
            "},
            expect![[r#"
                fn Toggle(enabled: Bool = true) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_mixed_required_and_default_parameters() {
        check(
            indoc! {r#"
                fn UserCard(name: String, role: String = "user", active: Bool = true) -> Html {
                  <>
                    {name}
                  </>
                }
            "#},
            expect![[r#"
                fn UserCard(
                  name: String,
                  role: String = "user",
                  active: Bool = true,
                ) -> Html {
                  <>
                    {name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_empty_tuple_parameter() {
        check(
            indoc! {r#"
                fn Row(nothing: (  ), rows: Array[()]) -> Html {<></>}
            "#},
            expect![[r#"
                fn Row(
                  nothing: (),
                  rows: Array[()],
                ) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_tuple_parameter() {
        check(
            indoc! {r#"
                fn Row(cell:   ( Int ,Array[String] )) -> Html {<></>}
            "#},
            expect![[r#"
                fn Row(cell: (Int, Array[String])) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_one_tuple_parameter_keeps_the_trailing_comma() {
        check(
            indoc! {r#"
                fn Row(only: ( Int , ), plain: (Int)) -> Html {<></>}
            "#},
            expect![[r#"
                fn Row(
                  only: (Int,),
                  plain: Int,
                ) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_array_parameter() {
        check(
            indoc! {r#"
                fn ItemList(items: Array[String] = ["one", "two"]) -> Html {<></>}
            "#},
            expect![[r#"
                fn ItemList(items: Array[String] = ["one", "two"]) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_empty_array_parameter() {
        check(
            indoc! {"
                fn ItemList(items: Array[String] = []) -> Html {<></>}
            "},
            expect![[r#"
                fn ItemList(items: Array[String] = []) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_empty_fragment_parameter() {
        check(
            indoc! {"
                fn Card(children: Html = <></>) -> Html {<></>}
            "},
            expect![[r#"
                fn Card(children: Html = <></>) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_record_parameter() {
        check(
            indoc! {r#"
                record Config { debug: Bool, timeout: Int }
                fn Settings(config: Config = Config {debug: false, timeout: 30}) -> Html {<></>}
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                fn Settings(
                  config: Config = Config {debug: false, timeout: 30},
                ) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_default_enum_parameter() {
        check(
            indoc! {"
                enum Status { Active, Inactive, Pending }
                fn Badge(status: Status = Status::Active) -> Html {<></>}
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                fn Badge(status: Status = Status::Active) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_in_expression() {
        check(
            indoc! {r#"
                record User { name: String, age: Int }
                fn Main() -> Html {
                  let user: User = User {name: "Alice", age: 30};
                  <>{user.name}</>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                fn Main() -> Html {
                  let user: User = User {name: "Alice", age: 30};
                  <>
                    {user.name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_spread() {
        check(
            indoc! {r#"
                record User { name: String, age: Int }
                fn Main(base: User) -> Html {
                  let user: User = User {...base, name: "Alice"};
                  <>{user.name}</>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                fn Main(base: User) -> Html {
                  let user: User = User {...base, name: "Alice"};
                  <>
                    {user.name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_spread_is_canonicalized_to_first_position() {
        check(
            indoc! {r#"
                record User { name: String, age: Int }
                fn Main(base: User) -> Html {
                  let user: User = User {name: "Alice", ...base};
                  <>{user.name}</>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                fn Main(base: User) -> Html {
                  let user: User = User {...base, name: "Alice"};
                  <>
                    {user.name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_with_only_spread() {
        check(
            indoc! {r#"
                record User { name: String, age: Int }
                fn Main(base: User) -> Html {
                  let user: User = User {...base};
                  <>{user.name}</>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                fn Main(base: User) -> Html {
                  let user: User = User {...base};
                  <>
                    {user.name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_spread_wraps_to_multiple_lines() {
        check(
            indoc! {r#"
                record User { name: String, age: Int, email: String }
                fn Main(base: User) -> Html {
                  let user: User = User {...base, name: "Alexandra", email: "alexandra@example.com"};
                  <>{user.name}</>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                  email: String,
                }

                fn Main(base: User) -> Html {
                  let user: User = User {
                    ...base,
                    name: "Alexandra",
                    email: "alexandra@example.com",
                  };
                  <>
                    {user.name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn inferred_let_binding_is_preserved() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  let name = "World";
                  <>{name}</>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  let name = "World";
                  <>
                    {name}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn empty_record_literal() {
        check(
            indoc! {"
                record Empty {}
                fn Main() -> Html {
                  let e: Empty = Empty {};
                  <></>
                }
            "},
            expect![[r#"
                record Empty {}

                fn Main() -> Html {
                  let e: Empty = Empty {};
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn empty_record_pattern_keeps_braces() {
        check(
            indoc! {r#"
                record Empty {}
                fn Main(e: Empty) -> Html {
                  <div class={match e { Empty {} => "yes" }}>
                  </div>
                }
            "#},
            expect![[r#"
                record Empty {}

                fn Main(e: Empty) -> Html {
                  <div class={match e {Empty {} => "yes"}}>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn record_pattern_without_listed_fields_keeps_braces() {
        check(
            indoc! {r#"
                record Point {x: Int, y: Int}
                fn Main(p: Point) -> Html {
                  <div class={match p { Point {} => "any" }}>
                  </div>
                }
            "#},
            expect![[r#"
                record Point {
                  x: Int,
                  y: Int,
                }

                fn Main(p: Point) -> Html {
                  <div class={match p {Point {} => "any"}}>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_with_fields() {
        check(
            indoc! {r#"
                enum Shape { Circle {radius: Float}, Rect {w: Float, h: Float} }
                fn Main() -> Html {
                  let s: Shape = Shape::Circle {radius: 5.0};
                  <></>
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

                fn Main() -> Html {
                  let s: Shape = Shape::Circle {radius: 5.0};
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_with_multiple_fields() {
        check(
            indoc! {"
                enum Shape { Circle {radius: Float}, Rect {w: Float, h: Float} }
                fn Main() -> Html {
                  let s: Shape = Shape::Rect {w: 3.0, h: 4.0};
                  <></>
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

                fn Main() -> Html {
                  let s: Shape = Shape::Rect {w: 3.0, h: 4.0};
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn enum_literal_with_many_fields_wraps_to_multiple_lines() {
        check(
            indoc! {r#"
                enum PopoverMenuItemElement { Link {href: String}, Button {href: String, name: String, value: String} }
                fn Main() -> Html {
                  let el: PopoverMenuItemElement = PopoverMenuItemElement::Button {href: "/path/to/some/page", name: "button_name", value: "button_value"};
                  <></>
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

                fn Main() -> Html {
                  let el: PopoverMenuItemElement = PopoverMenuItemElement::Button {
                    href: "/path/to/some/page",
                    name: "button_name",
                    value: "button_value",
                  };
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn record_literal_with_many_fields_wraps_to_multiple_lines() {
        check(
            indoc! {r#"
                record Button { href: String, name: String, value: String, dialog_trigger: String }
                fn Main() -> Html {
                  let btn: Button = Button {href: "/path/to/some/page", name: "button_name", value: "button_value", dialog_trigger: "dialog_trigger_value"};
                  <></>
                }
            "#},
            expect![[r#"
                record Button {
                  href: String,
                  name: String,
                  value: String,
                  dialog_trigger: String,
                }

                fn Main() -> Html {
                  let btn: Button = Button {
                    href: "/path/to/some/page",
                    name: "button_name",
                    value: "button_value",
                    dialog_trigger: "dialog_trigger_value",
                  };
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn some_literal_stays_on_one_line_when_short() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  let x: Option[String] = Some("short");
                  <></>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  let x: Option[String] = Some("short");
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn some_literal_inserts_soft_lines_when_long() {
        check(
            indoc! {r#"
                fn Main(x: Option[String] = Some("this is a very long string that causes a line break because Some uses soft lines")) -> Html {<></>}
            "#},
            expect![[r#"
                fn Main(
                  x: Option[String] = Some(
                    "this is a very long string that causes a line break because Some uses soft lines"
                  ),
                ) -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn html_with_string_and_expression_attributes() {
        check(
            indoc! {r#"
                record Product { img_src: String }
                fn ProductImage(product: Product) -> Html {
                  <img class="rounded-lg" src={product.img_src}>
                }
            "#},
            expect![[r#"
                record Product {
                  img_src: String,
                }

                fn ProductImage(product: Product) -> Html {
                  <img class="rounded-lg" src={product.img_src}>
                }
            "#]],
        );
    }

    #[test]
    fn join_macro_expands_spaces_in_string_literals() {
        check(
            indoc! {r#"
                fn Card() -> Html {
                  <div class={join!("foo bar")}></div>
                }
            "#},
            expect![[r#"
                fn Card() -> Html {
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
    fn join_macro_splits_on_written_spaces_not_escaped_ones() {
        check(
            indoc! {r#"
                fn Card() -> Html {
                  <div class={join!("foo\nbar baz")}></div>
                }
            "#},
            expect![[r#"
                fn Card() -> Html {
                  <div class={
                    join!(
                      "foo\nbar",
                      "baz",
                    )
                  }>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn preserves_escape_sequences_in_string_literals() {
        check(
            indoc! {r#"
                fn Greeting() -> String {
                  "tab\there, \"quoted\", back\\slash, newline\n"
                }
            "#},
            expect![[r#"
                fn Greeting() -> String {
                  "tab\there, \"quoted\", back\\slash, newline\n"
                }
            "#]],
        );
    }

    #[test]
    fn join_macro_expands_mixed_variables_and_literals() {
        check(
            indoc! {r#"
                fn Card(a: String, b: String) -> Html {
                  <div class={join!(a, "foo bar", b)}></div>
                }
            "#},
            expect![[r#"
                fn Card(
                  a: String,
                  b: String,
                ) -> Html {
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
                fn Main() -> Html {
                  <img src={asset!("/logo.svg")} />
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <img src={asset!("/logo.svg")}>
                }
            "#]],
        );
    }

    #[test]
    fn format_macro_formats_inline() {
        check(
            indoc! {r#"
                fn Main(name: String, count: Int) -> Html {
                  <p>{format!(
                     "a: {}, b: {}", 
                     name,   count)}</p>
                }
            "#},
            expect![[r#"
                fn Main(
                  name: String,
                  count: Int,
                ) -> Html {
                  <p>
                    {format!("a: {}, b: {}", name, count)}
                  </p>
                }
            "#]],
        );
    }

    #[test]
    fn should_format_deeply_nested_elements() {
        check(
            indoc! {r#"
                fn Main() -> Html {
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
                fn Main() -> Html {
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
    fn comment_before_import_declaration() {
        check(
            indoc! {"
                // External function
                import functions::Button
                fn Main() -> Html {<></>}
            "},
            expect![[r#"
                // External function
                import functions::Button

                fn Main() -> Html {
                  <></>
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
    fn comment_before_function_declaration() {
        check(
            indoc! {"
                // Main function
                fn Main() -> Html {
                  <>
                    hello
                  </>
                }
            "},
            expect![[r#"
                // Main function
                fn Main() -> Html {
                  <>
                    hello
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn comment_before_page_member() {
        check(
            indoc! {"
                page Index() {
                  // the head
                  fn head() -> Html {
                    <title>Hi</title>
                  }
                  fn body() -> Html {
                    <div>Hello</div>
                  }
                }
            "},
            expect![[r#"
                page Index {
                  // the head
                  fn head() -> Html {
                    <title>
                      Hi
                    </title>
                  }
                  fn body() -> Html {
                    <div>
                      Hello
                    </div>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn comment_between_page_members() {
        check(
            indoc! {"
                page Index() {
                  fn head() -> Html {
                    <title>Hi</title>
                  }
                  // now the body
                  fn body() -> Html {
                    <div>Hello</div>
                  }
                }
            "},
            expect![[r#"
                page Index {
                  fn head() -> Html {
                    <title>
                      Hi
                    </title>
                  }
                  // now the body
                  fn body() -> Html {
                    <div>
                      Hello
                    </div>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn trailing_comment_inside_a_page_member_body() {
        check(
            indoc! {"
                page Index() {
                  fn body() -> Html {
                    <div>Hello</div>
                    // done
                  }
                }
            "},
            expect![[r#"
                page Index {
                  fn body() -> Html {
                    <div>
                      Hello
                    </div>
                    // done
                  }
                }
            "#]],
        );
    }

    #[test]
    fn trailing_comment_after_the_last_page_member() {
        check(
            indoc! {"
                page Index() {
                  fn body() -> Html {
                    <div>Hello</div>
                  }
                  // that is all
                }
            "},
            expect![[r#"
                page Index {
                  fn body() -> Html {
                    <div>
                      Hello
                    </div>
                  }
                  // that is all
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

                // Main function
                fn Main() -> Html {<></>}
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

                // Main function
                fn Main() -> Html {
                  <></>
                }
            "#]],
        );
    }

    #[test]
    fn comment_at_end_of_file() {
        check(
            indoc! {"
                record User { name: String }
                // trailing
            "},
            expect![[r#"
                record User {
                  name: String,
                }

                // trailing
            "#]],
        );
    }

    #[test]
    fn comment_only_file() {
        check(
            indoc! {"
                // only a comment
            "},
            expect![[r#"
                // only a comment
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
                fn Main() -> Html {<></>}
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
                fn Main() -> Html {
                  <></>
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
                fn Main(orientation: Orientation) -> Html {
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

                fn Main(orientation: Orientation) -> Html {
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
                fn Main() -> Html {
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
                fn Main() -> Html {
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
                fn Main() -> Html {
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
                fn Main() -> Html {
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
    fn comment_before_function_parameter() {
        check(
            indoc! {r#"
                fn Button(
                    // The button label
                    label: String,
                    // Whether the button is disabled
                    disabled: Bool = false,
                    // More params to come
                ) -> Html {
                  <>{label}</>
                }
            "#},
            expect![[r#"
                fn Button(
                  // The button label
                  label: String,
                  // Whether the button is disabled
                  disabled: Bool = false,
                ) -> Html {
                  // More params to come
                  <>
                    {label}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_function_parameters_single_param() {
        check(
            indoc! {r#"
                fn X(
                  x: String,
                  // ?
                ) -> Html {
                  <>{x}</>
                }
            "#},
            expect![[r#"
                fn X(x: String) -> Html {
                  // ?
                  <>
                    {x}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn join_macro_with_multiple_string_literals() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  <h1 class={join!("text-4xl", "font-bold", "tracking-tight", "dark:hover:text-blue-300")}>Hello</h1>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
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
    fn function_invocation_with_single_long_attribute() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                  <Button class={join!("text-4xl", "font-bold", "tracking-tight", "dark:hover:text-blue-300")}>Click me</Button>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
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
    fn simple_method_call() {
        check(
            indoc! {"
                fn Main(x: String) -> Html {
                  <div>{x.foo()}</div>
                }
            "},
            expect![[r#"
                fn Main(x: String) -> Html {
                  <div>
                    {x.foo()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_with_arguments() {
        check(
            indoc! {"
                fn Main(x: String) -> Html {
                  <div>{x.foo(1,2 , 3)}</div>
                }
            "},
            expect![[r#"
                fn Main(x: String) -> Html {
                  <div>
                    {x.foo(1, 2, 3)}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn chained_method_calls() {
        check(
            indoc! {"
                fn Main(x: String) -> Html {
                  <div>{x.foo().bar().baz()}</div>
                }
            "},
            expect![[r#"
                fn Main(x: String) -> Html {
                  <div>
                    {x.foo().bar().baz()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn mixed_field_access_and_method_call() {
        check(
            indoc! {"
                fn Main(x: String) -> Html {
                  <div>{x.field.method()}</div>
                }
            "},
            expect![[r#"
                fn Main(x: String) -> Html {
                  <div>
                    {x.field.method()}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_then_field_access() {
        check(
            indoc! {"
                fn Main(x: String) -> Html {
                  <div>{x.method().field}</div>
                }
            "},
            expect![[r#"
                fn Main(x: String) -> Html {
                  <div>
                    {x.method().field}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn float_literal() {
        check(
            indoc! {"
                fn Main() -> Html {
                  let x: Float = 5.0;
                  <>{x}</>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  let x: Float = 5.0;
                  <>
                    {x}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn float_literals_small_values() {
        check(
            indoc! {"
                fn Main() -> Html {
                  let a: Float = 0.000;
                  let b: Float = 0.001;
                  let c: Float = 0.002;
                  <>{a}</>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  let a: Float = 0.000;
                  let b: Float = 0.001;
                  let c: Float = 0.002;
                  <>
                    {a}
                  </>
                }
            "#]],
        );
    }

    #[test]
    fn inline_text_with_nested_element() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <div>
                    foo<p>bar</p>
                  </div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
    fn nested_elements_inline() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <div><p>x</p></div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
    fn text_around_inline_element() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <div>hello <b>world</b>!</div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <div>
                    hello
                    {" "}
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
                fn Main() -> Html {
                  <p>By clicking continue, you agree to our <a href="/tos">Terms of Service</a> and <a href="/privacy">Privacy Policy</a>.</p>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <p>
                    By clicking continue, you agree to our
                    {" "}
                    <a href="/tos">
                      Terms of Service
                    </a>
                    {" "}
                    and
                    {" "}
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
    fn empty_lines_between_text_collapsed() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <div>

                  foo

                  bar

                  </div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <div>
                    foo
                    bar
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn text_around_void_element() {
        check(
            indoc! {"
                fn Main() -> Html {
                    <div>hello <br> world</div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  <div>
                    hello
                    {" "}
                    <br>
                    {" "}
                    world
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn void_element_on_separate_line() {
        check(
            indoc! {"
                fn Main() -> Html {
                    <div>
                        hello
                        <br>
                        world
                    </div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
    fn text_around_input_element() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                    <label>Name: <input type="text"> (required)</label>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  <label>
                    Name:
                    {" "}
                    <input type="text">
                    {" "}
                    (required)
                  </label>
                }
            "#]],
        );
    }

    #[test]
    fn input_element_on_separate_line() {
        check(
            indoc! {r#"
                fn Main() -> Html {
                    <label>
                        Name:
                        <input type="text">
                        (required)
                    </label>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
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
    fn text_with_multiple_expressions() {
        check(
            indoc! {"
                fn Main(rating: String, num_reviews: String) -> Html {
                  <span>{rating} ({num_reviews} reviews)</span>
                }
            "},
            expect![[r#"
                fn Main(
                  rating: String,
                  num_reviews: String,
                ) -> Html {
                  <span>
                    {rating}
                    {" "}
                    (
                    {num_reviews}
                    {" "}
                    reviews)
                  </span>
                }
            "#]],
        );
    }

    #[test]
    fn method_call_on_negated_int_preserves_parens() {
        check(
            indoc! {"
                fn Main() -> Html {
                  <div>{(-42).to_string()}</div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
                fn Main() -> Html {
                  <div>{(-3.14).to_string()}</div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
                fn Main() -> Html {
                  <div>{(1 + 2).to_string()}</div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
                fn Main() -> Html {
                  <div>{(-42).foo}</div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
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
                fn Main(x: Int) -> Html {
                  <div>{(1 + 2) * 3}</div>
                }
            "},
            expect![[r#"
                fn Main(x: Int) -> Html {
                  <div>
                    {(1 + 2) * 3}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn binary_expr_keeps_parens_around_right_operand_of_same_precedence() {
        check(
            indoc! {"
                fn Main(a: Bool, b: Bool, c: Bool) -> Html {
                  <div>{1 - (1 - 1)}{1 - (2 + 3)}{2 * (3 * 4)}{a == (b == c)}{a || (b || c)}</div>
                }
            "},
            expect![[r#"
                fn Main(
                  a: Bool,
                  b: Bool,
                  c: Bool,
                ) -> Html {
                  <div>
                    {1 - (1 - 1)}
                    {1 - (2 + 3)}
                    {2 * (3 * 4)}
                    {a == (b == c)}
                    {a || (b || c)}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn binary_expr_drops_redundant_parens_around_left_operand_of_same_precedence() {
        check(
            indoc! {"
                fn Main(a: Bool, b: Bool, c: Bool) -> Html {
                  <div>{(1 - 1) - 1}{(1 + 2) - 3}{1 - 2 * 3}{(a == b) == c}</div>
                }
            "},
            expect![[r#"
                fn Main(
                  a: Bool,
                  b: Bool,
                  c: Bool,
                ) -> Html {
                  <div>
                    {1 - 1 - 1}
                    {1 + 2 - 3}
                    {1 - 2 * 3}
                    {a == b == c}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn page_with_parameters() {
        check(
            indoc! {r#"
                record LoginLogo { url: String, name: String }

                page LoginFormEntry(x: String, logo: LoginLogo) {
                  fn body() -> Html {
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
                }
            "#},
            expect![[r#"
                record LoginLogo {
                  url: String,
                  name: String,
                }

                page LoginFormEntry(
                  x: String,
                  logo: LoginLogo,
                ) {
                  fn body() -> Html {
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
                }
            "#]],
        );
    }

    #[test]
    fn page_multiline_text() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>
                      hello
                      world
                    </>
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <>
                      hello
                      world
                    </>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_in_page() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>
                      <!-- This is a comment -->
                      <div>hello</div>
                    </>
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <>
                      <!-- This is a comment -->
                      <div>
                        hello
                      </div>
                    </>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_between_elements() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>
                      <div>hello</div>
                      <!-- separator -->
                      <div>world</div>
                    </>
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <>
                      <div>
                        hello
                      </div>
                      <!-- separator -->
                      <div>
                        world
                      </div>
                    </>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_inside_element() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>
                      <!-- inner comment -->
                      <span>text</span>
                    </div>
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <div>
                      <!-- inner comment -->
                      <span>
                        text
                      </span>
                    </div>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn html_comment_only() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <!-- just a comment -->
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <!-- just a comment -->
                  }
                }
            "#]],
        );
    }

    #[test]
    fn spread_attribute_on_html_element_formats_correctly() {
        check(
            indoc! {"
                fn Foo(...rest) -> Html {
                  <button ...rest></button>
                }
            "},
            expect![[r#"
                fn Foo(...rest) -> Html {
                  <button ...rest>
                  </button>
                }
            "#]],
        );
    }

    #[test]
    fn spread_attribute_on_function_invocation_formats_correctly() {
        check(
            indoc! {"
                fn Bar(...rest) -> Html {
                  <Foo ...rest></Foo>
                }
            "},
            expect![[r#"
                fn Bar(...rest) -> Html {
                  <Foo ...rest>
                  </Foo>
                }
            "#]],
        );
    }

    #[test]
    fn formats_rest_param_and_spread() {
        check(
            indoc! {"
                fn Foo(class: String, ...rest) -> Html {
                  <button ...rest></button>
                }
            "},
            expect![[r#"
                fn Foo(
                  class: String,
                  ...rest,
                ) -> Html {
                  <button ...rest>
                  </button>
                }
            "#]],
        );
    }

    #[test]
    fn formats_only_rest_param() {
        check(
            indoc! {"
                fn Foo(...rest) -> Html {
                  <button ...rest></button>
                }
            "},
            expect![[r#"
                fn Foo(...rest) -> Html {
                  <button ...rest>
                  </button>
                }
            "#]],
        );
    }

    #[test]
    fn function_with_empty_params() {
        check(
            indoc! {"
                fn answer() -> Int {
                  42
                }
            "},
            expect![[r#"
                fn answer() -> Int {
                  42
                }
            "#]],
        );
    }

    #[test]
    fn function_with_multiple_params() {
        check(
            indoc! {r#"
                fn pick(cond: Bool, a: String, b: String) -> String { "x" }
            "#},
            expect![[r#"
                fn pick(
                  cond: Bool,
                  a: String,
                  b: String,
                ) -> String {
                  "x"
                }
            "#]],
        );
    }

    #[test]
    fn function_preserves_leading_comment() {
        check(
            indoc! {"
                // Adds ten
                fn foo(x: Int) -> Int {
                  x + 10
                }
            "},
            expect![[r#"
                // Adds ten
                fn foo(x: Int) -> Int {
                  x + 10
                }
            "#]],
        );
    }

    #[test]
    fn function_preserves_comments_in_body() {
        check(
            indoc! {"
                fn foo(x: Int) -> Int {
                  // before the expression
                  x + 10
                  // after the expression
                }
            "},
            expect![[r#"
                fn foo(x: Int) -> Int {
                  // before the expression
                  x + 10
                  // after the expression
                }
            "#]],
        );
    }

    #[test]
    fn function_with_match_body() {
        check(
            indoc! {r#"
                fn label(flag: Bool) -> String {
                  match flag { true => "on", false => "off" }
                }
            "#},
            expect![[r#"
                fn label(flag: Bool) -> String {
                  match flag {true => "on", false => "off"}
                }
            "#]],
        );
    }

    #[test]
    fn markup_as_a_function_body() {
        check(
            indoc! {"
                fn card() -> Html {
                  <div>hello</div>
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  <div>
                    hello
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn markup_in_an_interpolation() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>{<span>hello</span>}</div>
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <div>
                      {<span>
                        hello
                      </span>}
                    </div>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn markup_as_a_call_argument() {
        check(
            indoc! {"
                fn card() -> Html {
                  wrap(<span>a<b>c</b></span>)
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  wrap(<span>a<b>c</b></span>)
                }
            "#]],
        );
    }

    #[test]
    fn markup_as_an_attribute_value() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <Card slot={<span>a<b>c</b></span>}/>
                  }
                }
            "},
            expect![[r#"
                page Test {
                  fn body() -> Html {
                    <Card slot={<span>a<b>c</b></span>}/>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn significant_whitespace_in_flat_markup() {
        check(
            indoc! {"
                fn card() -> Html {
                  wrap(<span>a <b>c</b> d</span>)
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  wrap(<span>a{" "}<b>c</b>{" "}d</span>)
                }
            "#]],
        );
    }

    #[test]
    fn markup_in_an_interpolation_short() {
        check(
            indoc! {"
                fn card() -> Html {
                  wrap(<span>a{<b>c</b>}d</span>)
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  wrap(<span>a{<b>c</b>}d</span>)
                }
            "#]],
        );
    }

    #[test]
    fn markup_in_match_arms() {
        check(
            indoc! {"
                fn badge(on: Bool) -> Html {
                  match on {true => <b>yes</b>, false => <i>no</i>}
                }
            "},
            expect![[r#"
                fn badge(on: Bool) -> Html {
                  match on {true => <b>yes</b>, false => <i>no</i>}
                }
            "#]],
        );
    }

    #[test]
    fn markup_in_match_arms_long() {
        check(
            indoc! {r#"
                fn badge(on: Bool) -> Html {
                  match on {true => <span class="a-fairly-long-class">yes <b>indeed</b></span>, false => <i>no</i>}
                }
            "#},
            expect![[r#"
                fn badge(on: Bool) -> Html {
                  match on {
                    true => {
                      <span class="a-fairly-long-class">
                        yes
                        {" "}
                        <b>
                          indeed
                        </b>
                      </span>
                    },
                    false => {
                      <i>
                        no
                      </i>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn markup_as_multiple_call_arguments() {
        check(
            indoc! {"
                fn card() -> Html {
                  pair(<b>a</b>, <i>c</i>)
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  pair(<b>a</b>, <i>c</i>)
                }
            "#]],
        );
    }

    #[test]
    fn markup_as_multiple_call_arguments_long() {
        check(
            indoc! {r#"
                fn card() -> Html {
                  pair(<span class="a-fairly-long-class">first <b>one</b></span>, <span class="another-long-one">second one</span>)
                }
            "#},
            expect![[r#"
                fn card() -> Html {
                  pair(
                    <span class="a-fairly-long-class">
                      first
                      {" "}
                      <b>
                        one
                      </b>
                    </span>,
                    <span class="another-long-one">
                      second one
                    </span>,
                  )
                }
            "#]],
        );
    }

    #[test]
    fn for_expression_over_array() {
        check(
            indoc! {"
                fn Dots(items: Array[String]) -> Html { for item in items { <span>{item}</span> } }
            "},
            expect![[r#"
                fn Dots(items: Array[String]) -> Html {
                  for item in items {
                    <span>
                      {item}
                    </span>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn for_expression_over_range() {
        check(
            indoc! {"
                fn Dots(n: Int) -> Html {
                  for _ in 1..=n {
                    <span>.</span>
                  }
                }
            "},
            expect![[r#"
                fn Dots(n: Int) -> Html {
                  for _ in 1..=n {
                    <span>
                      .
                    </span>
                  }
                }
            "#]],
        );
    }

    #[test]
    fn for_expression_with_let_statements_in_body() {
        check(
            indoc! {"
                fn ItemList(items: Array[Item]) -> Html {
                  <ul>
                    {for item in items { let name = item.name; let title = name; <li>{title}</li> }}
                  </ul>
                }
            "},
            expect![[r#"
                fn ItemList(items: Array[Item]) -> Html {
                  <ul>
                    {for item in items {
                      let name = item.name;
                      let title = name;
                      <li>
                        {title}
                      </li>
                    }}
                  </ul>
                }
            "#]],
        );
    }

    #[test]
    fn for_expression_in_match_arm() {
        check(
            indoc! {"
                fn Main(x: Option[Array[String]]) -> Html {
                  match x {
                    Some(items) => for item in items { <li>{item}</li> },
                    None => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(x: Option[Array[String]]) -> Html {
                  match x {
                    Some(items) => for item in items {
                      <li>
                        {item}
                      </li>
                    },
                    None => {
                      <></>
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn for_expression_with_comments() {
        check(
            indoc! {"
                fn Main(items: Array[String]) -> Html {
                  // before
                  for item in items {
                    // inside
                    let name = item;
                    <li>{name}</li>
                  }
                }
            "},
            expect![[r#"
                fn Main(items: Array[String]) -> Html {
                  // before
                  for item in items {
                    // inside
                    let name = item;
                    <li>
                      {name}
                    </li>
                  }
                }
            "#]],
        );
    }
}
