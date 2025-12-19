use crate::document::document_cursor::{DocumentRange, StringSpan};

use super::parsed_ast::{ParsedAst, ParsedComponentDeclaration, ParsedDeclaration};
use super::parsed_node::ParsedNode;

/// Removes leading and trailing whitespace from text nodes in a ParsedAst.
///
/// This works by:
/// 1. Splitting text on newlines
/// 2. Trimming leading and trailing whitespace from each piece
/// 3. Filtering out empty pieces
pub fn remove_whitespace(ast: ParsedAst) -> ParsedAst {
    let declarations = ast
        .get_declarations()
        .iter()
        .cloned()
        .map(transform_declaration)
        .collect();
    ParsedAst::new(ast.name.clone(), declarations)
}

fn transform_declaration(decl: ParsedDeclaration) -> ParsedDeclaration {
    match decl {
        ParsedDeclaration::Component(comp) => {
            ParsedDeclaration::Component(transform_component(comp))
        }
        other => other,
    }
}

fn transform_component(comp: ParsedComponentDeclaration) -> ParsedComponentDeclaration {
    ParsedComponentDeclaration {
        children: transform_nodes(comp.children),
        ..comp
    }
}

fn transform_nodes(nodes: Vec<ParsedNode>) -> Vec<ParsedNode> {
    nodes.into_iter().flat_map(transform_node).collect()
}

fn transform_node(node: ParsedNode) -> Vec<ParsedNode> {
    match node {
        ParsedNode::Text { value, range } => split_text_node(&value, &range),
        ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children,
            range,
        } => vec![ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::If {
            condition,
            children,
            range,
        } => vec![ParsedNode::If {
            condition,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::For {
            var_name,
            var_name_range,
            array_expr,
            children,
            range,
        } => vec![ParsedNode::For {
            var_name,
            var_name_range,
            array_expr,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children,
            range,
        } => vec![ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::Placeholder { children, range } => vec![ParsedNode::Placeholder {
            children: transform_nodes(children),
            range,
        }],
        other => vec![other],
    }
}

fn split_text_node(value: &StringSpan, range: &DocumentRange) -> Vec<ParsedNode> {
    value
        .as_str()
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| ParsedNode::Text {
            value: StringSpan::new(line.to_string()),
            range: range.clone(),
        })
        .collect()
}
