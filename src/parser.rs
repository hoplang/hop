use crate::common::{
    Attribute, ComponentDefinitionNode, ComponentReferenceNode, DoctypeNode, DopExprAttribute,
    DopVarNameAttribute, ErrorNode, ForNode, IfNode, ImportNode, NativeHTMLNode, Node, Range,
    RangeError, RenderNode, SlotDefinitionNode, SlotReferenceNode, TextExpressionNode, TextNode,
    XExecNode, XLoadJsonNode, XRawNode, is_void_element,
};
use crate::dop::{self, DopTokenizer};
use crate::tokenizer::Token;
use crate::tokenizer::Tokenizer;
use std::collections::HashSet;

fn is_valid_component_name(name: &str) -> bool {
    if name.is_empty() || name.starts_with('-') || name.ends_with('-') {
        return false;
    }
    if name.starts_with("hop-") {
        return false;
    }
    name.contains('-')
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub components: Vec<ComponentDefinitionNode>,
    pub imports: Vec<ImportNode>,
    pub renders: Vec<RenderNode>,
}

#[derive(Debug, Clone, PartialEq)]
enum ToplevelNode {
    Import(ImportNode),
    ComponentDefinition(ComponentDefinitionNode),
    Render(RenderNode),
}

pub fn parse(module_name: String, tokenizer: Tokenizer, errors: &mut Vec<RangeError>) -> Module {
    let tree = build_tree(tokenizer, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut renders = Vec::new();

    for child in &tree.children {
        if let Some(toplevel_node) = construct_toplevel_node(child, errors) {
            match toplevel_node {
                ToplevelNode::Import(import_data) => imports.push(import_data),
                ToplevelNode::ComponentDefinition(component_data) => {
                    components.push(component_data)
                }
                ToplevelNode::Render(render_data) => renders.push(render_data),
            }
        }
    }

    Module {
        name: module_name,
        components,
        imports,
        renders,
    }
}

#[derive(Debug, Clone)]
struct TokenTree {
    token: (Token, Range),
    children: Vec<TokenTree>,
    end_token: Option<(Token, Range)>,
}

impl TokenTree {
    fn new(token: (Token, Range)) -> Self {
        TokenTree {
            token,
            children: Vec::new(),
            end_token: None,
        }
    }

    fn append_node(&mut self, token: (Token, Range)) {
        self.children.push(TokenTree::new(token));
    }

    fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    fn set_end_token(&mut self, token: (Token, Range)) {
        self.end_token = Some(token);
    }
}

fn build_tree(tokenizer: Tokenizer, errors: &mut Vec<RangeError>) -> TokenTree {
    let mut stack: Vec<(TokenTree, String)> = Vec::new();

    let root_token = Token::StartTag {
        self_closing: false,
        value: "root".to_string(),
        attributes: Vec::new(),
        name_range: Range::default(),
        expression: None,
    };
    stack.push((
        TokenTree::new((root_token, Range::default())),
        "root".to_string(),
    ));

    for t in tokenizer {
        match t {
            Err(err) => errors.push(err),
            Ok((token, range)) => {
                match token {
                    Token::Comment => {
                        // skip comments
                        continue;
                    }
                    Token::Doctype | Token::Text { .. } | Token::Expression { .. } => {
                        stack.last_mut().unwrap().0.append_node((token, range));
                    }
                    Token::StartTag {
                        ref value,
                        self_closing,
                        ..
                    } => {
                        if is_void_element(value) || self_closing {
                            stack.last_mut().unwrap().0.append_node((token, range));
                        } else {
                            stack.push((TokenTree::new((token.clone(), range)), value.clone()));
                        }
                    }
                    Token::EndTag { ref value, .. } => {
                        if is_void_element(value) {
                            errors.push(RangeError::closed_void_tag(value, range));
                        } else if !stack.iter().any(|(_, v)| *v == *value) {
                            errors.push(RangeError::unmatched_closing_tag(value, range));
                        } else {
                            while stack.last().unwrap().1 != *value {
                                let (unclosed, value) = stack.pop().unwrap();
                                errors.push(RangeError::unclosed_tag(&value, unclosed.token.1));
                                stack.last_mut().unwrap().0.append_tree(unclosed);
                            }
                            let mut completed = stack.pop().unwrap();
                            completed.0.set_end_token((token, range));
                            stack.last_mut().unwrap().0.append_tree(completed.0);
                        }
                    }
                    Token::Eof => {
                        break;
                    }
                }
            }
        }
    }

    while stack.len() > 1 {
        let (tree, value) = stack.pop().unwrap();
        errors.push(RangeError::unclosed_tag(&value, tree.token.1));
    }

    stack.pop().unwrap().0
}

fn collect_slots_from_children(
    children: &[Node],
    slots: &mut HashSet<String>,
    errors: &mut Vec<RangeError>,
) {
    for child in children {
        match child {
            Node::Text(_) => {}
            Node::TextExpression(_) => {}
            Node::Doctype(_) => {}
            Node::SlotDefinition(SlotDefinitionNode { name, range, .. }) => {
                if slots.contains(name) {
                    errors.push(RangeError::slot_already_defined(name, *range));
                } else {
                    // Check if trying to mix default slot with other slots
                    if (name == "default" && !slots.is_empty())
                        || (slots.contains("default") && name != "default")
                    {
                        errors.push(RangeError::default_slot_with_other_slots(*range));
                    } else {
                        slots.insert(name.clone());
                    }
                }
            }
            Node::XExec(XExecNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::If(IfNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::For(ForNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::NativeHTML(NativeHTMLNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::ComponentReference(ComponentReferenceNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::SlotReference(SlotReferenceNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::Error(ErrorNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::XRaw(XRawNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::XLoadJson(XLoadJsonNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
        }
    }
}

fn find_attribute(attrs: &[Attribute], value: &str) -> Option<Attribute> {
    attrs.iter().find(|attr| attr.name == value).cloned()
}

fn construct_toplevel_node(tree: &TokenTree, errors: &mut Vec<RangeError>) -> Option<ToplevelNode> {
    let (t, range) = &tree.token;

    match t {
        Token::StartTag {
            value,
            attributes,
            expression,
            ..
        } => {
            match value.as_str() {
                "import" => {
                    let component_attr = find_attribute(attributes, "component").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "component",
                            *range,
                        ));
                        None
                    });
                    let from_attr = find_attribute(attributes, "from").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value, "from", *range,
                        ));
                        None
                    });

                    match (component_attr, from_attr) {
                        (Some(component_attr), Some(from_attr)) => {
                            Some(ToplevelNode::Import(ImportNode {
                                component_attr,
                                from_attr,
                                range: *range,
                            }))
                        }
                        _ => None,
                    }
                }
                "render" => {
                    let children: Vec<Node> = tree
                        .children
                        .iter()
                        .map(|child| construct_node(child, errors))
                        .collect();

                    let file_attr = find_attribute(attributes, "file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value, "file", *range,
                        ));
                        None
                    });

                    file_attr.map(|file_attr| {
                        ToplevelNode::Render(RenderNode {
                            file_attr,
                            range: *range,
                            children,
                        })
                    })
                }
                name => {
                    // Handle as component definition
                    if !is_valid_component_name(name) {
                        errors.push(RangeError::invalid_component_name(name, *range));
                        return None;
                    }

                    // Separate preview content from main children
                    let mut main_children = Vec::new();
                    let mut preview_children = None;

                    for child in &tree.children {
                        if let (Token::StartTag { value, .. }, _) = &child.token {
                            if value == "hop-x-preview" {
                                preview_children = Some(
                                    child
                                        .children
                                        .iter()
                                        .map(|c| construct_node(c, errors))
                                        .collect(),
                                );
                                continue; // Don't add to main children
                            }
                        }
                        main_children.push(construct_node(child, errors));
                    }

                    let children = main_children;

                    let as_attr = find_attribute(attributes, "as");
                    let entrypoint = find_attribute(attributes, "entrypoint").is_some();
                    let params_as_attr = expression.as_ref().and_then(|(expr_string, range)| {
                        let mut tokenizer = match DopTokenizer::new(expr_string, range.start) {
                            Ok(tokenizer) => tokenizer,
                            Err(err) => {
                                errors.push(err);
                                return None;
                            }
                        };
                        match dop::parse_variable_with_type(&mut tokenizer) {
                            Ok((var_name, type_annotation)) => Some(DopVarNameAttribute {
                                var_name,
                                type_annotation,
                                range: *range,
                            }),
                            Err(error) => {
                                errors.push(error);
                                None
                            }
                        }
                    });

                    let mut slots = HashSet::new();
                    collect_slots_from_children(&children, &mut slots, errors);

                    Some(ToplevelNode::ComponentDefinition(ComponentDefinitionNode {
                        name: name.to_string(),
                        param: params_as_attr,
                        as_attr,
                        attributes: attributes.clone(),
                        range: *range,
                        children,
                        preview: preview_children,
                        entrypoint,
                        slots: slots.into_iter().collect(),
                    }))
                }
            }
        }
        _ => None,
    }
}

fn construct_node(tree: &TokenTree, errors: &mut Vec<RangeError>) -> Node {
    let children: Vec<Node> = tree
        .children
        .iter()
        .map(|child| construct_node(child, errors))
        .collect();

    let (t, token_range) = &tree.token;

    match t {
        Token::Doctype => Node::Doctype(DoctypeNode {
            value: "".to_string(),
            range: *token_range,
        }),
        Token::Text { value } => Node::Text(TextNode {
            value: value.clone(),
            range: *token_range,
        }),
        Token::Expression { value, range } => {
            // Expression tokens represent {expression} in text content
            let mut tokenizer = match DopTokenizer::new(value, range.start) {
                Ok(tokenizer) => tokenizer,
                Err(_) => {
                    return Node::Error(ErrorNode {
                        range: *range,
                        children: vec![],
                    });
                }
            };
            match dop::parse_expr(&mut tokenizer) {
                Ok(expr) => Node::TextExpression(TextExpressionNode {
                    expression: expr,
                    range: *range,
                }),
                Err(err) => {
                    errors.push(err);
                    Node::Error(ErrorNode {
                        range: *token_range,
                        children: vec![],
                    })
                }
            }
        }
        Token::StartTag {
            value,
            expression,
            attributes,
            ..
        } => {
            match value.as_str() {
                "if" => match &expression {
                    Some((expr_string, expr_range)) => {
                        let mut tokenizer = match DopTokenizer::new(expr_string, expr_range.start) {
                            Ok(tokenizer) => tokenizer,
                            Err(_) => {
                                return Node::Error(ErrorNode {
                                    range: *expr_range,
                                    children: vec![],
                                });
                            }
                        };
                        match dop::parse_expr(&mut tokenizer) {
                            Ok(condition) => Node::If(IfNode {
                                condition,
                                range: *expr_range,
                                children,
                            }),
                            Err(err) => {
                                errors.push(err);
                                Node::Error(ErrorNode {
                                    range: *token_range,
                                    children,
                                })
                            }
                        }
                    }
                    None => {
                        errors.push(RangeError::new(
                            "Missing expression in <if> tag".to_string(),
                            *token_range,
                        ));
                        Node::Error(ErrorNode {
                            range: *token_range,
                            children,
                        })
                    }
                },
                "for" => match expression {
                    Some((expr_string, expr_range)) => {
                        let mut tokenizer = match DopTokenizer::new(expr_string, expr_range.start) {
                            Ok(tokenizer) => tokenizer,
                            Err(_) => {
                                return Node::Error(ErrorNode {
                                    range: *expr_range,
                                    children: vec![],
                                });
                            }
                        };
                        match dop::parse_loop_header(&mut tokenizer) {
                            Ok((var_name, array_expr)) => Node::For(ForNode {
                                var_name,
                                array_expr,
                                range: *token_range,
                                children,
                            }),
                            Err(error) => {
                                errors.push(error);
                                Node::Error(ErrorNode {
                                    range: *token_range,
                                    children,
                                })
                            }
                        }
                    }
                    None => {
                        errors.push(RangeError::new(
                            "Missing loop generator expression in <for> tag".to_string(),
                            *token_range,
                        ));
                        Node::Error(ErrorNode {
                            range: *token_range,
                            children,
                        })
                    }
                },
                "hop-x-exec" => {
                    let cmd_attr = find_attribute(attributes, "cmd").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "cmd",
                            *token_range,
                        ));
                        None
                    });

                    match cmd_attr {
                        Some(cmd_attr) => Node::XExec(XExecNode {
                            cmd_attr,
                            range: *token_range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: *token_range,
                            children,
                        }),
                    }
                }
                "hop-x-raw" => {
                    let has_trim = attributes.iter().any(|attr| attr.name == "trim");
                    Node::XRaw(XRawNode {
                        trim: has_trim,
                        range: *token_range,
                        children,
                    })
                }
                "hop-x-load-json" => {
                    let file_attr = find_attribute(attributes, "file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "file",
                            *token_range,
                        ));
                        None
                    });

                    let as_attr = find_attribute(attributes, "as").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "as",
                            *token_range,
                        ));
                        None
                    });

                    match (file_attr, as_attr) {
                        (Some(file_attr), Some(as_attr)) => Node::XLoadJson(XLoadJsonNode {
                            file_attr,
                            as_attr,
                            range: *token_range,
                            children,
                        }),
                        _ => Node::Error(ErrorNode {
                            range: *token_range,
                            children,
                        }),
                    }
                }
                tag_name if tag_name.starts_with("slot-") => {
                    let slot_name = &tag_name[5..]; // Remove "slot-" prefix
                    Node::SlotDefinition(SlotDefinitionNode {
                        name: slot_name.to_string(),
                        range: *token_range,
                        children,
                    })
                }
                tag_name if tag_name.starts_with("with-") => {
                    let slot_name = &tag_name[5..]; // Remove "with-" prefix
                    Node::SlotReference(SlotReferenceNode {
                        name: slot_name.to_string(),
                        range: *token_range,
                        children,
                    })
                }
                tag_name if is_valid_component_name(tag_name) => {
                    // This is a component render (contains dash)
                    let params_attr = match &expression {
                        Some((expr_string, range)) => {
                            let mut tokenizer = match DopTokenizer::new(expr_string, range.start) {
                                Ok(tokenizer) => tokenizer,
                                Err(err) => {
                                    errors.push(err);
                                    return Node::Error(ErrorNode {
                                        range: *range,
                                        children: vec![],
                                    });
                                }
                            };
                            match dop::parse_expr(&mut tokenizer) {
                                Ok(expression) => Some((expression, *range)),
                                Err(err) => {
                                    errors.push(err);
                                    None
                                }
                            }
                        }
                        None => None,
                    };

                    Node::ComponentReference(ComponentReferenceNode {
                        component: tag_name.to_string(),
                        params: params_attr,
                        attributes: attributes.clone(),
                        range: *token_range,
                        children,
                    })
                }
                _ => {
                    let mut set_attributes = Vec::new();
                    for attr in attributes {
                        if attr.name.starts_with("set-") {
                            if let Some(expr_attr) = {
                                let mut tokenizer =
                                    match DopTokenizer::new(&attr.value, attr.range.start) {
                                        Ok(tokenizer) => tokenizer,
                                        Err(err) => {
                                            errors.push(err);
                                            continue;
                                        }
                                    };
                                match dop::parse_expr(&mut tokenizer) {
                                    Ok(expression) => Some(DopExprAttribute::new(
                                        attr.name.to_string(),
                                        expression,
                                        attr.range,
                                    )),
                                    Err(err) => {
                                        errors.push(err);
                                        None
                                    }
                                }
                            } {
                                set_attributes.push(expr_attr);
                            }
                        }
                    }

                    Node::NativeHTML(NativeHTMLNode {
                        tag_name: value.clone(),
                        attributes: attributes.clone(),
                        range: *token_range,
                        children,
                        set_attributes,
                    })
                }
            }
        }
        _ => {
            panic!("Unexpected token type in construct_node");
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error_formatter::ErrorFormatter;
    use crate::test_utils::parse_test_cases;

    use super::*;
    use pretty_assertions::assert_eq;

    use std::fs;
    use std::path::PathBuf;

    pub fn format_component_definition(d: &ComponentDefinitionNode) -> String {
        let mut lines = Vec::new();
        for child in &d.children {
            let s = format_tree(child, 0);
            if !s.is_empty() {
                lines.push(s);
            }
        }
        lines.join("\n")
    }

    pub fn format_tree(root: &Node, depth: usize) -> String {
        let mut lines = Vec::new();

        fn format_node(node: &Node, depth: usize, lines: &mut Vec<String>) {
            let indent = "\t".repeat(depth);

            match node {
                Node::Doctype(_) => {
                    lines.push(format!("{}doctype", indent));
                }
                Node::ComponentReference(ComponentReferenceNode { children, .. }) => {
                    lines.push(format!("{}render", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::If(IfNode { children, .. }) => {
                    lines.push(format!("{}if", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::For(ForNode { children, .. }) => {
                    lines.push(format!("{}for", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::NativeHTML(NativeHTMLNode {
                    tag_name, children, ..
                }) => {
                    lines.push(format!("{}{}", indent, tag_name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::SlotDefinition(SlotDefinitionNode { name, children, .. }) => {
                    lines.push(format!("{}slot-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::SlotReference(SlotReferenceNode { name, children, .. }) => {
                    lines.push(format!("{}with-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::XExec(XExecNode { children, .. }) => {
                    lines.push(format!("{}hop-x-exec", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::XRaw(XRawNode { children, .. }) => {
                    lines.push(format!("{}hop-x-raw", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                _ => {}
            }
        }

        format_node(root, depth, &mut lines);
        lines.join("\n")
    }

    #[test]
    fn test_parser() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/parser.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (archive, line_number)) in test_cases.iter().enumerate() {
            let input = archive
                .get("main.hop")
                .expect("Missing 'main.hop' section in test case")
                .content
                .trim();
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim_start();

            println!("Test case {} (line {})", case_num + 1, line_number);

            let mut errors = Vec::new();
            assert!(errors.is_empty());
            let module = parse("test".to_string(), Tokenizer::new(input), &mut errors);

            if !errors.is_empty() {
                let mut efmt = ErrorFormatter::new().without_location_info();
                efmt.add_errors("test".to_string(), input.to_string(), errors.clone());
                assert_eq!(
                    efmt.format_all_errors(),
                    expected,
                    "Mismatch in test case {} (line {})",
                    case_num + 1,
                    line_number
                );
            } else {
                for component in module.components {
                    if component.name == "main-comp" {
                        let output = format_component_definition(&component);
                        assert_eq!(
                            output.trim(),
                            expected.trim(),
                            "Mismatch in test case {} (line {})",
                            case_num + 1,
                            line_number
                        );
                    }
                }
            }
        }
    }
}
