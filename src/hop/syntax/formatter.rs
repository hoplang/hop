use super::parsed_ast::{
    ParsedAst, ParsedAttribute, ParsedAttributeValue, ParsedComponentDeclaration,
    ParsedDeclaration, ParsedEnumDeclaration, ParsedEnumDeclarationVariant,
    ParsedImportDeclaration, ParsedParameter, ParsedRecordDeclaration,
    ParsedRecordDeclarationField,
};
use super::parsed_node::{ParsedLetBinding, ParsedMatchCase, ParsedNode};
use super::transform::sort_imports::sort_imports;
use super::transform::whitespace_removal::remove_whitespace;
use crate::common::is_void_element;
use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::syntax::parsed::{
    Constructor, ParsedExpr, ParsedMatchArm, ParsedMatchPattern, ParsedType,
};
use pretty::BoxDoc;

fn drain_comments_before<'a>(
    comments: &mut Vec<&'a (String, DocumentRange)>,
    position: usize,
) -> BoxDoc<'a> {
    let mut doc = BoxDoc::nil();
    while let Some(comment) = comments.first() {
        if comment.1.start() < position {
            let comment = comments.remove(0);
            doc = doc
                .append(BoxDoc::text("//"))
                .append(BoxDoc::text(comment.0.as_str()))
                .append(BoxDoc::hardline());
        } else {
            break;
        }
    }
    doc
}

fn format_braced_list<'a, T, F>(
    items: &'a [T],
    mut format_item: F,
    comments: &mut Vec<&'a (String, DocumentRange)>,
    end_position: usize,
) -> BoxDoc<'a>
where
    F: FnMut(&'a T, &mut Vec<&'a (String, DocumentRange)>) -> BoxDoc<'a>,
{
    let has_trailing_comments = comments.first().is_some_and(|c| c.1.start() < end_position);
    if items.is_empty() && !has_trailing_comments {
        return BoxDoc::nil();
    }
    let mut items_doc = BoxDoc::nil();
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            items_doc = items_doc.append(BoxDoc::text(",")).append(BoxDoc::line());
        }
        items_doc = items_doc.append(format_item(item, comments));
    }
    if !items.is_empty() {
        items_doc = items_doc.append(BoxDoc::text(","));
    }
    let trailing_comments = drain_comments_before(comments, end_position);
    let content = if items.is_empty() {
        trailing_comments
    } else if has_trailing_comments {
        items_doc.append(BoxDoc::line()).append(trailing_comments)
    } else {
        items_doc
    };
    let body = BoxDoc::line().append(content).nest(2);
    if has_trailing_comments {
        body
    } else {
        body.append(BoxDoc::line())
    }
}

pub fn format(ast: ParsedAst) -> String {
    let ast = remove_whitespace(ast);
    let ast = sort_imports(ast);
    format_ast(&ast).pretty(60).to_string()
}

fn format_ast(ast: &ParsedAst) -> BoxDoc<'_> {
    let declarations = ast.get_declarations();
    let mut comments: Vec<&(String, DocumentRange)> = ast.comments().iter().collect();
    comments.sort_by_key(|(_, range)| range.start());
    if declarations.is_empty() {
        BoxDoc::nil()
    } else {
        let mut doc = BoxDoc::nil();
        let mut prev_was_import = false;
        for (i, decl) in declarations.iter().enumerate() {
            if i > 0 {
                doc = doc.append(BoxDoc::line());
                let curr_is_import = matches!(decl, ParsedDeclaration::Import(_));
                if !(prev_was_import && curr_is_import) {
                    doc = doc.append(BoxDoc::line());
                }
            }
            doc = doc.append(format_declaration(decl, &mut comments));
            prev_was_import = matches!(decl, ParsedDeclaration::Import(_));
        }
        doc.append(BoxDoc::line())
    }
}

fn format_declaration<'a>(
    decl: &'a ParsedDeclaration,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    match decl {
        ParsedDeclaration::Import(import) => format_import_declaration(import),
        ParsedDeclaration::Record(record) => format_record_declaration(record, comments),
        ParsedDeclaration::Enum(e) => format_enum_declaration(e, comments),
        ParsedDeclaration::Component(component) => {
            format_component_declaration(component, comments)
        }
    }
}

fn format_import_declaration(import: &ParsedImportDeclaration) -> BoxDoc<'_> {
    BoxDoc::text("import")
        .append(BoxDoc::space())
        .append(BoxDoc::text(import.module_name.to_string()))
        .append(BoxDoc::text("::"))
        .append(BoxDoc::text(import.type_name.as_str()))
}

fn format_record_declaration<'a>(
    record: &'a ParsedRecordDeclaration,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, record.name_range.start());
    leading_comments
        .append(BoxDoc::text("record"))
        .append(BoxDoc::space())
        .append(BoxDoc::text(record.name.as_str()))
        .append(BoxDoc::space())
        .append(BoxDoc::text("{"))
        .append(format_braced_list(
            &record.fields,
            format_record_declaration_field,
            comments,
            record.range.end(),
        ))
        .append(BoxDoc::text("}"))
}

fn format_record_declaration_field<'a>(
    field: &'a ParsedRecordDeclarationField,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, field.name_range.start());
    leading_comments
        .append(BoxDoc::text(field.name.as_str()))
        .append(BoxDoc::text(": "))
        .append(format_type(&field.field_type))
}

fn format_enum_declaration<'a>(
    e: &'a ParsedEnumDeclaration,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, e.name_range.start());
    leading_comments
        .append(BoxDoc::text("enum"))
        .append(BoxDoc::space())
        .append(BoxDoc::text(e.name.as_str()))
        .append(BoxDoc::space())
        .append(BoxDoc::text("{"))
        .append(format_braced_list(
            &e.variants,
            format_enum_declaration_variant,
            comments,
            e.range.end(),
        ))
        .append(BoxDoc::text("}"))
}

fn format_enum_declaration_variant<'a>(
    variant: &'a ParsedEnumDeclarationVariant,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, variant.name_range.start());
    if variant.fields.is_empty() {
        leading_comments.append(BoxDoc::text(variant.name.as_str()))
    } else {
        leading_comments
            .append(BoxDoc::text(variant.name.as_str()))
            .append(BoxDoc::text("("))
            .append(BoxDoc::intersperse(
                variant.fields.iter().map(|(field_name, _, field_type)| {
                    BoxDoc::text(field_name.to_string())
                        .append(BoxDoc::text(": "))
                        .append(format_type(field_type))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(")"))
    }
}

fn format_component_declaration<'a>(
    component: &'a ParsedComponentDeclaration,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, component.range.start());
    let params_doc = match &component.params {
        Some((params, params_range)) => {
            let mut params_inner = BoxDoc::nil();
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    params_inner = params_inner
                        .append(BoxDoc::text(","))
                        .append(BoxDoc::line());
                }
                params_inner = params_inner.append(format_parameter(param, comments));
            }
            let has_trailing_comments = comments
                .first()
                .is_some_and(|c| c.1.start() < params_range.end());
            let trailing_comments = drain_comments_before(comments, params_range.end());
            if params.is_empty() && !has_trailing_comments {
                BoxDoc::nil()
            } else {
                let body = if params.is_empty() {
                    BoxDoc::line_().append(trailing_comments).nest(2)
                } else if has_trailing_comments {
                    BoxDoc::line_()
                        .append(params_inner)
                        .append(BoxDoc::text(","))
                        .append(BoxDoc::line())
                        .append(trailing_comments)
                        .nest(2)
                } else {
                    BoxDoc::line_()
                        .append(params_inner)
                        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                        .nest(2)
                        .append(BoxDoc::line_())
                };
                BoxDoc::text(" {")
                    .append(body)
                    .append(BoxDoc::text("}"))
                    .group()
            }
        }
        None => BoxDoc::nil(),
    };

    let children_doc = if component.children.is_empty() {
        BoxDoc::nil()
    } else {
        let mut children_inner = BoxDoc::nil();
        for (i, child) in component.children.iter().enumerate() {
            if i > 0 {
                children_inner = children_inner.append(BoxDoc::line());
            }
            children_inner = children_inner.append(format_node(child, comments));
        }
        BoxDoc::line()
            .append(children_inner)
            .nest(2)
            .append(BoxDoc::line())
    };

    leading_comments
        .append(BoxDoc::text("<"))
        .append(BoxDoc::text(component.component_name.as_str()))
        .append(params_doc)
        .append(BoxDoc::text(">"))
        .append(children_doc)
        .append(BoxDoc::text("</"))
        .append(BoxDoc::text(component.component_name.as_str()))
        .append(BoxDoc::text(">"))
}

fn format_parameter<'a>(
    param: &'a ParsedParameter,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, param.var_name_range.start());
    let base = BoxDoc::text(param.var_name.as_str())
        .append(BoxDoc::text(": "))
        .append(format_type(&param.var_type));
    let param_doc = match &param.default_value {
        Some(default) => base
            .append(BoxDoc::text(" = "))
            .append(format_expr(default, comments)),
        None => base,
    };
    leading_comments.append(param_doc)
}

fn format_attribute<'a>(
    attr: &'a ParsedAttribute,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let name_doc = BoxDoc::text(attr.name.as_str());
    match &attr.value {
        Some(value) => name_doc
            .append(BoxDoc::text("="))
            .append(format_attribute_value(value, comments)),
        None => name_doc,
    }
}

fn format_attribute_value<'a>(
    value: &'a ParsedAttributeValue,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    match value {
        ParsedAttributeValue::Expression(expr) => BoxDoc::text("{")
            .append(BoxDoc::line_().append(format_expr(expr, comments)).nest(2))
            .append(BoxDoc::line_())
            .append(BoxDoc::text("}"))
            .group(),
        ParsedAttributeValue::String(range) => {
            let content = range.as_ref().map(|r| r.as_str()).unwrap_or("");
            BoxDoc::text(format!("\"{}\"", content))
        }
    }
}

fn format_node<'a>(
    node: &'a ParsedNode,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    match node {
        ParsedNode::Text { value, .. } => BoxDoc::text(value.as_str()),
        ParsedNode::TextExpression { expression, .. } => BoxDoc::text("{")
            .append(format_expr(expression, comments))
            .append(BoxDoc::text("}")),
        ParsedNode::ComponentReference {
            component_name,
            args,
            children,
            ..
        } => {
            let component_name_str = component_name.as_str();
            let opening_tag_doc = if args.is_empty() {
                BoxDoc::text("<").append(BoxDoc::text(component_name_str))
            } else {
                let mut args_doc = BoxDoc::nil();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        args_doc = args_doc.append(BoxDoc::line());
                    }
                    args_doc = args_doc.append(format_attribute(arg, comments));
                }
                BoxDoc::text("<")
                    .append(BoxDoc::text(component_name_str))
                    .append(BoxDoc::line().append(args_doc).nest(2))
                    .append(BoxDoc::line_())
                    .group()
            };
            if children.is_empty() {
                opening_tag_doc.append(BoxDoc::text("/>"))
            } else {
                let mut children_doc = BoxDoc::nil();
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        children_doc = children_doc.append(BoxDoc::line());
                    }
                    children_doc = children_doc.append(format_node(child, comments));
                }
                opening_tag_doc
                    .append(BoxDoc::text(">"))
                    .append(BoxDoc::line().append(children_doc).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</"))
                    .append(BoxDoc::text(component_name_str))
                    .append(BoxDoc::text(">"))
            }
        }
        ParsedNode::If {
            condition,
            children,
            ..
        } => {
            let children_doc = format_children(children, comments);
            BoxDoc::text("<if {")
                .append(format_expr(condition, comments))
                .append(BoxDoc::text("}>"))
                .append(children_doc)
                .append(BoxDoc::text("</if>"))
        }
        ParsedNode::For {
            var_name,
            array_expr,
            children,
            ..
        } => {
            let children_doc = format_children(children, comments);
            BoxDoc::text("<for {")
                .append(BoxDoc::text(var_name.as_str()))
                .append(BoxDoc::text(" in "))
                .append(format_expr(array_expr, comments))
                .append(BoxDoc::text("}>"))
                .append(children_doc)
                .append(BoxDoc::text("</for>"))
        }
        ParsedNode::Let {
            bindings, children, ..
        } => {
            let mut bindings_doc = BoxDoc::nil();
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    bindings_doc = bindings_doc
                        .append(BoxDoc::text(","))
                        .append(BoxDoc::line());
                }
                bindings_doc = bindings_doc.append(format_let_binding(binding, comments));
            }
            let bindings_wrapped = BoxDoc::line_()
                .append(bindings_doc)
                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                .nest(2)
                .append(BoxDoc::line_())
                .group();
            let children_doc = format_children(children, comments);
            BoxDoc::text("<let {")
                .append(bindings_wrapped)
                .append(BoxDoc::text("}>"))
                .append(children_doc)
                .append(BoxDoc::text("</let>"))
        }
        ParsedNode::Doctype { value, .. } => BoxDoc::text(value.as_str()),
        ParsedNode::Match { subject, cases, .. } => {
            let cases_doc = if cases.is_empty() {
                BoxDoc::nil()
            } else {
                let mut doc = BoxDoc::nil();
                for (i, case) in cases.iter().enumerate() {
                    if i > 0 {
                        doc = doc.append(BoxDoc::line());
                    }
                    doc = doc.append(format_match_case(case, comments));
                }
                BoxDoc::line().append(doc).nest(2).append(BoxDoc::line())
            };
            BoxDoc::text("<match {")
                .append(format_expr(subject, comments))
                .append(BoxDoc::text("}>"))
                .append(cases_doc)
                .append(BoxDoc::text("</match>"))
        }
        ParsedNode::Html {
            tag_name,
            attributes,
            children,
            ..
        } => {
            let tag_name_str = tag_name.as_str();
            let opening_tag_doc = if attributes.is_empty() {
                BoxDoc::text("<")
                    .append(BoxDoc::text(tag_name_str))
                    .append(BoxDoc::text(">"))
            } else {
                let mut attrs_doc = BoxDoc::nil();
                for (i, attr) in attributes.iter().enumerate() {
                    if i > 0 {
                        attrs_doc = attrs_doc.append(BoxDoc::line());
                    }
                    attrs_doc = attrs_doc.append(format_attribute(attr, comments));
                }
                BoxDoc::text("<")
                    .append(BoxDoc::text(tag_name_str))
                    .append(BoxDoc::line().append(attrs_doc).nest(2))
                    .append(BoxDoc::line_())
                    .append(BoxDoc::text(">"))
                    .group()
            };

            if is_void_element(tag_name_str) {
                opening_tag_doc
            } else if children.is_empty() {
                opening_tag_doc
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</"))
                    .append(BoxDoc::text(tag_name_str))
                    .append(BoxDoc::text(">"))
            } else {
                let mut children_doc = BoxDoc::nil();
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        children_doc = children_doc.append(BoxDoc::line());
                    }
                    children_doc = children_doc.append(format_node(child, comments));
                }
                opening_tag_doc
                    .append(BoxDoc::line().append(children_doc).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</"))
                    .append(BoxDoc::text(tag_name_str))
                    .append(BoxDoc::text(">"))
            }
        }
        ParsedNode::Placeholder { children, .. } => {
            if children.is_empty() {
                BoxDoc::text("<placeholder />")
            } else {
                let mut children_doc = BoxDoc::nil();
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        children_doc = children_doc.append(BoxDoc::line());
                    }
                    children_doc = children_doc.append(format_node(child, comments));
                }
                BoxDoc::text("<placeholder>")
                    .append(BoxDoc::line().append(children_doc).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</placeholder>"))
            }
        }
    }
}

fn format_children<'a>(
    children: &'a [ParsedNode],
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    if children.is_empty() {
        BoxDoc::nil()
    } else {
        let mut doc = BoxDoc::nil();
        for (i, child) in children.iter().enumerate() {
            if i > 0 {
                doc = doc.append(BoxDoc::line());
            }
            doc = doc.append(format_node(child, comments));
        }
        BoxDoc::line().append(doc).nest(2).append(BoxDoc::line())
    }
}

fn format_match_case<'a>(
    case: &'a ParsedMatchCase,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let children_doc = format_children(&case.children, comments);
    BoxDoc::text("<case {")
        .append(format_match_pattern(&case.pattern))
        .append(BoxDoc::text("}>"))
        .append(children_doc)
        .append(BoxDoc::text("</case>"))
}

fn format_let_binding<'a>(
    binding: &'a ParsedLetBinding,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    BoxDoc::text(binding.var_name.as_str())
        .append(BoxDoc::text(": "))
        .append(format_type(&binding.var_type))
        .append(BoxDoc::text(" = "))
        .append(format_expr(&binding.value_expr, comments))
}

fn format_type(ty: &ParsedType) -> BoxDoc<'_> {
    match ty {
        ParsedType::String { .. } => BoxDoc::text("String"),
        ParsedType::Bool { .. } => BoxDoc::text("Bool"),
        ParsedType::Int { .. } => BoxDoc::text("Int"),
        ParsedType::Float { .. } => BoxDoc::text("Float"),
        ParsedType::TrustedHTML { .. } => BoxDoc::text("TrustedHTML"),
        ParsedType::Option { element, .. } => BoxDoc::nil()
            .append(BoxDoc::text("Option["))
            .append(format_type(element))
            .append(BoxDoc::text("]")),
        ParsedType::Array { element, .. } => BoxDoc::nil()
            .append(BoxDoc::text("Array["))
            .append(format_type(element))
            .append(BoxDoc::text("]")),
        ParsedType::Named { name, .. } => BoxDoc::text(name.clone()),
    }
}

fn format_expr<'a>(
    expr: &'a ParsedExpr,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    match expr {
        ParsedExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
        ParsedExpr::FieldAccess {
            record: object,
            field,
            ..
        } => format_expr(object, comments)
            .append(BoxDoc::text("."))
            .append(BoxDoc::text(field.as_str())),
        ParsedExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
        ParsedExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
        ParsedExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
        ParsedExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
        ParsedExpr::ArrayLiteral { elements, .. } => {
            if elements.is_empty() {
                BoxDoc::text("[]")
            } else {
                let mut elements_doc = BoxDoc::nil();
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        elements_doc = elements_doc
                            .append(BoxDoc::text(","))
                            .append(BoxDoc::line());
                    }
                    elements_doc = elements_doc.append(format_expr(elem, comments));
                }
                BoxDoc::text("[")
                    .append(
                        BoxDoc::line_()
                            .append(elements_doc)
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text("]"))
            }
        }
        ParsedExpr::RecordLiteral {
            record_name,
            fields,
            ..
        } => {
            if fields.is_empty() {
                BoxDoc::text(record_name.as_str()).append(BoxDoc::text("()"))
            } else {
                let mut fields_doc = BoxDoc::nil();
                for (i, (key, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        fields_doc = fields_doc.append(BoxDoc::text(",")).append(BoxDoc::line());
                    }
                    fields_doc = fields_doc
                        .append(BoxDoc::text(key.as_str()))
                        .append(BoxDoc::text(": "))
                        .append(format_expr(value, comments));
                }
                BoxDoc::text(record_name.as_str())
                    .append(BoxDoc::text("("))
                    .append(
                        BoxDoc::line_()
                            .append(fields_doc)
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text(")"))
            }
        }
        ParsedExpr::BinaryOp {
            left,
            operator,
            right,
            ..
        } => {
            let prec = operator.precedence();
            format_expr_with_precedence(left, prec, comments)
                .append(BoxDoc::text(format!(" {} ", operator)))
                .append(format_expr_with_precedence(right, prec, comments))
        }
        ParsedExpr::Negation { operand, .. } => {
            if is_atomic(operand) {
                BoxDoc::text("!").append(format_expr(operand, comments))
            } else {
                BoxDoc::text("!(")
                    .append(format_expr(operand, comments))
                    .append(BoxDoc::text(")"))
            }
        }
        ParsedExpr::EnumLiteral {
            enum_name,
            variant_name,
            fields,
            ..
        } => {
            let base = BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str()));
            if fields.is_empty() {
                base
            } else {
                let mut fields_doc = BoxDoc::nil();
                for (i, (field_name, _, field_value)) in fields.iter().enumerate() {
                    if i > 0 {
                        fields_doc = fields_doc.append(BoxDoc::text(", "));
                    }
                    fields_doc = fields_doc
                        .append(BoxDoc::text(field_name.to_string()))
                        .append(BoxDoc::text(": "))
                        .append(format_expr(field_value, comments));
                }
                base.append(BoxDoc::text("("))
                    .append(fields_doc)
                    .append(BoxDoc::text(")"))
            }
        }
        ParsedExpr::Match { subject, arms, .. } => {
            if arms.is_empty() {
                BoxDoc::text("match ")
                    .append(format_expr(subject, comments))
                    .append(BoxDoc::text(" {}"))
            } else {
                let mut arms_doc = BoxDoc::nil();
                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        arms_doc = arms_doc.append(BoxDoc::text(",")).append(BoxDoc::line());
                    }
                    arms_doc = arms_doc.append(format_match_arm(arm, comments));
                }
                BoxDoc::text("match ")
                    .append(format_expr(subject, comments))
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::line_()
                            .append(arms_doc)
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text("}"))
            }
        }
        ParsedExpr::OptionLiteral { value, .. } => match value {
            Some(inner) => BoxDoc::text("Some(")
                .append(format_expr(inner, comments))
                .append(BoxDoc::text(")")),
            None => BoxDoc::text("None"),
        },
        ParsedExpr::MacroInvocation { name, args, .. } => {
            let mut expanded_docs: Vec<BoxDoc<'a>> = Vec::new();
            if name == "classes" {
                for e in args.iter() {
                    let leading_comments = drain_comments_before(comments, e.range().start());
                    match e {
                        ParsedExpr::StringLiteral { value, .. } => {
                            let parts: Vec<_> = value.split_whitespace().collect();
                            for (i, part) in parts.iter().enumerate() {
                                if i == 0 {
                                    expanded_docs.push(
                                        leading_comments
                                            .clone()
                                            .append(BoxDoc::text(format!("\"{}\"", part))),
                                    );
                                } else {
                                    expanded_docs.push(BoxDoc::text(format!("\"{}\"", part)));
                                }
                            }
                        }
                        _ => expanded_docs.push(leading_comments.append(format_expr(e, comments))),
                    }
                }
            } else {
                for e in args.iter() {
                    let leading_comments = drain_comments_before(comments, e.range().start());
                    expanded_docs.push(leading_comments.append(format_expr(e, comments)));
                }
            }

            let end_position = expr.range().end();
            let has_trailing_comments = comments
                .first()
                .is_some_and(|c| c.1.start() < end_position);
            let trailing_comments = drain_comments_before(comments, end_position);

            if expanded_docs.is_empty() && !has_trailing_comments {
                BoxDoc::text(name.as_str()).append(BoxDoc::text("!()"))
            } else if expanded_docs.is_empty() {
                BoxDoc::text(name.as_str())
                    .append(BoxDoc::text("!("))
                    .append(BoxDoc::line_().append(trailing_comments).nest(2))
                    .append(BoxDoc::text(")"))
            } else {
                let mut args_doc = BoxDoc::nil();
                for (i, doc) in expanded_docs.into_iter().enumerate() {
                    if i > 0 {
                        args_doc = args_doc.append(BoxDoc::text(",")).append(BoxDoc::line());
                    }
                    args_doc = args_doc.append(doc);
                }

                let body = if has_trailing_comments {
                    BoxDoc::line_()
                        .append(args_doc)
                        .append(BoxDoc::text(","))
                        .append(BoxDoc::line())
                        .append(trailing_comments)
                        .nest(2)
                } else {
                    BoxDoc::line_()
                        .append(args_doc)
                        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                        .append(BoxDoc::line_())
                        .nest(2)
                };

                BoxDoc::text(name.as_str())
                    .append(BoxDoc::text("!("))
                    .append(body.group())
                    .append(BoxDoc::text(")"))
            }
        }
    }
}

fn format_expr_with_precedence<'a>(
    expr: &'a ParsedExpr,
    parent_precedence: u8,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    match expr {
        ParsedExpr::BinaryOp { operator, .. } => {
            let needs_parens = operator.precedence() < parent_precedence;
            if needs_parens {
                BoxDoc::text("(")
                    .append(format_expr(expr, comments))
                    .append(BoxDoc::text(")"))
            } else {
                format_expr(expr, comments)
            }
        }
        _ => format_expr(expr, comments),
    }
}

fn is_atomic(expr: &ParsedExpr) -> bool {
    matches!(
        expr,
        ParsedExpr::Var { .. }
            | ParsedExpr::FieldAccess { .. }
            | ParsedExpr::StringLiteral { .. }
            | ParsedExpr::BooleanLiteral { .. }
            | ParsedExpr::IntLiteral { .. }
            | ParsedExpr::FloatLiteral { .. }
            | ParsedExpr::ArrayLiteral { .. }
            | ParsedExpr::RecordLiteral { .. }
            | ParsedExpr::EnumLiteral { .. }
            | ParsedExpr::Match { .. }
            | ParsedExpr::OptionLiteral { .. }
            | ParsedExpr::MacroInvocation { .. }
    )
}

fn format_match_arm<'a>(
    arm: &'a ParsedMatchArm,
    comments: &mut Vec<&'a (String, DocumentRange)>,
) -> BoxDoc<'a> {
    let leading_comments = drain_comments_before(comments, arm.pattern.range().start());
    leading_comments
        .append(format_match_pattern(&arm.pattern))
        .append(BoxDoc::text(" => "))
        .append(format_expr(&arm.body, comments))
}

fn format_match_pattern(pattern: &ParsedMatchPattern) -> BoxDoc<'_> {
    match pattern {
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            fields,
            ..
        } => {
            let base = format_constructor(constructor);
            if !fields.is_empty() {
                let fields_doc = BoxDoc::intersperse(
                    fields.iter().map(|(name, _, pat)| {
                        BoxDoc::text(name.as_str())
                            .append(BoxDoc::text(": "))
                            .append(format_match_pattern(pat))
                    }),
                    BoxDoc::text(", "),
                );
                base.append(BoxDoc::text("("))
                    .append(fields_doc)
                    .append(BoxDoc::text(")"))
            } else if args.is_empty() {
                base
            } else {
                let args_doc =
                    BoxDoc::intersperse(args.iter().map(format_match_pattern), BoxDoc::text(", "));
                base.append(BoxDoc::text("("))
                    .append(args_doc)
                    .append(BoxDoc::text(")"))
            }
        }
        ParsedMatchPattern::Wildcard { .. } => BoxDoc::text("_"),
        ParsedMatchPattern::Binding { name, .. } => BoxDoc::text(name.as_str()),
    }
}

fn format_constructor(constructor: &Constructor) -> BoxDoc<'_> {
    match constructor {
        Constructor::EnumVariant {
            enum_name,
            variant_name,
        } => BoxDoc::text(enum_name.as_str().to_string())
            .append(BoxDoc::text("::"))
            .append(BoxDoc::text(variant_name.as_str())),
        Constructor::BooleanTrue => BoxDoc::text("true"),
        Constructor::BooleanFalse => BoxDoc::text("false"),
        Constructor::OptionSome => BoxDoc::text("Some"),
        Constructor::OptionNone => BoxDoc::text("None"),
        Constructor::Record { type_name } => BoxDoc::text(type_name.as_str().to_string()),
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            source.to_string(),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        expected.assert_eq(&super::format(ast));
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
                import baz::Qux
                import components::Button
                import foo::Bar

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
                  <div
                    class={
                      match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                      }
                    }
                  >
                  </div>
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
                  <div class={match color {Color::Red => "red"}}>
                  </div>
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
                  <div class={"p-2"}>
                  </div>
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
                  Hello,
                  {name}
                  !
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
                  <div class={classes!(base_class, extra_class)}>
                  </div>
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
                  <button
                    class={
                      classes!(size_class, variant_class, custom_class)
                    }
                  >
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
                  <div
                    class={
                      classes!(
                        base_styles,
                        responsive_styles,
                        interactive_styles,
                        custom_overrides,
                      )
                    }
                  >
                  </div>
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
                  <div class={classes!("foo", "bar")}>
                  </div>
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
                  <div class={classes!("a", "b", "c")}>
                  </div>
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
                  <div
                    class={classes!(a, "foo", "bar", b, "baz", "qux", c)}
                  >
                  </div>
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
                  <div class={classes!("foo", "bar")}>
                  </div>
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
                  <div class={classes!("foo", "bar", "baz")}>
                  </div>
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
                    Hello,
                    {name}
                    !
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
                    {first}
                    {second}
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
                      {a}
                      +
                      {b}
                      +
                      {c}
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
                      {a}
                      {b}
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
                  <let {first_name: String = "Hello", last_name: String = "World"}>
                    {first_name} {last_name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {
                    first_name: String = "Hello",
                    last_name: String = "World",
                  }>
                    {first_name}
                    {last_name}
                  </let>
                </Main>
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
                }
            "},
            expect![[r#"
                record User {
                  // The name of the user
                  name: String,
                  // The age of the user
                  age: Int,
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
                }
            "},
            expect![[r#"
                enum Status {
                  // User is active
                  Active,
                  // User is inactive
                  Inactive,
                }
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_enum() {
        check(
            indoc! {"
                enum Status {
                  Active,
                  Inactive,
                  // More statuses to come
                }
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  // More statuses to come
                }
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_record() {
        check(
            indoc! {"
                record User {
                  name: String,
                  // More fields to come
                }
            "},
            expect![[r#"
                record User {
                  name: String,
                  // More fields to come
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
                  }}></div>
                </Main>
            "#},
            expect![[r#"
                enum Orientation {
                  Horizontal,
                  Vertical,
                }

                <Main {orientation: Orientation}>
                  <div
                    class={
                      match orientation {
                        // a
                        Orientation::Horizontal => "horizontal",
                        // b
                        Orientation::Vertical => "vertical",
                      }
                    }
                  >
                  </div>
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
                  )}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div
                    class={
                      classes!(
                        // base styles
                        "flex",
                        // conditional style
                        "items-center",
                      )
                    }
                  >
                  </div>
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
                  )}></div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div
                    class={
                      classes!(
                        // base styles
                        "flex",
                        "items-center",
                        // conditional style
                        "justify-between",
                        "gap-4",
                      )
                    }
                  >
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_macro() {
        check(
            indoc! {r#"
                <Main {class: String}>
                  <div class={classes!(
                    "mx-auto",
                    "flex",
                    "w-full",
                    "justify-center",
                    class,
                    // more to come!
                  )}></div>
                </Main>
            "#},
            expect![[r#"
                <Main {class: String}>
                  <div
                    class={
                      classes!(
                        "mx-auto",
                        "flex",
                        "w-full",
                        "justify-center",
                        class,
                        // more to come!
                      )
                    }
                  >
                  </div>
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
                }>
                  {label}
                </Button>
            "#]],
        );
    }

    #[test]
    fn trailing_comment_in_component_parameters() {
        check(
            indoc! {r#"
                <Button {
                    label: String,
                    disabled: Bool = false,
                    // More params to come
                }>
                  {label}
                </Button>
            "#},
            expect![[r#"
                <Button {
                  label: String,
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
}
