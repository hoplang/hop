use crate::common::Range;
use crate::dop::{DopArgument, DopExpr, DopParameter, parser::DopVarName};

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
    pub value_range: Range,
}

impl Attribute {
    pub fn new(name: String, value: String, range: Range, value_range: Range) -> Self {
        Attribute {
            name,
            value,
            range,
            value_range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopExprAttribute {
    pub name: String,
    pub expression: DopExpr,
    pub range: Range,
}

impl DopExprAttribute {
    pub fn new(name: String, expression: DopExpr, range: Range) -> Self {
        DopExprAttribute {
            name,
            expression,
            range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoctypeNode {
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextNode {
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentReferenceNode {
    pub component: String,
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub args: Vec<DopArgument>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfNode {
    pub condition: DopExpr,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportNode {
    pub component_attr: Attribute,
    pub from_attr: Attribute,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentDefinitionNode {
    pub name: String,
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub params: Vec<DopParameter>,
    pub as_attr: Option<Attribute>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
    pub preview: Option<Vec<HopNode>>,
    pub entrypoint: bool,
    pub slots: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeHTMLNode {
    pub tag_name: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
    pub set_attributes: Vec<DopExprAttribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorNode {
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SlotDefinitionNode {
    pub name: String,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SlotReferenceNode {
    pub name: String,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderNode {
    pub file_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XExecNode {
    pub cmd_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XRawNode {
    pub trim: bool,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub var_name: DopVarName,
    pub array_expr: DopExpr,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XLoadJsonNode {
    pub file_attr: Attribute,
    pub as_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextExpressionNode {
    pub expression: DopExpr,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelHopNode {
    Import(ImportNode),
    ComponentDefinition(ComponentDefinitionNode),
    Render(RenderNode),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HopNode {
    Doctype(DoctypeNode),
    Text(TextNode),
    TextExpression(TextExpressionNode),
    ComponentReference(ComponentReferenceNode),
    SlotDefinition(SlotDefinitionNode),
    SlotReference(SlotReferenceNode),
    If(IfNode),
    For(ForNode),
    NativeHTML(NativeHTMLNode),
    Error(ErrorNode),
    XExec(XExecNode),
    XRaw(XRawNode),
    XLoadJson(XLoadJsonNode),
}

impl HopNode {
    pub fn children(&self) -> &[HopNode] {
        match self {
            HopNode::ComponentReference(node) => &node.children,
            HopNode::SlotDefinition(node) => &node.children,
            HopNode::SlotReference(node) => &node.children,
            HopNode::If(node) => &node.children,
            HopNode::For(node) => &node.children,
            HopNode::NativeHTML(node) => &node.children,
            HopNode::Error(node) => &node.children,
            HopNode::XExec(node) => &node.children,
            HopNode::XRaw(node) => &node.children,
            HopNode::XLoadJson(node) => &node.children,
            // Leaf nodes with no children
            HopNode::Doctype(_) => &[],
            HopNode::Text(_) => &[],
            HopNode::TextExpression(_) => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator {
        DepthFirstIterator::new(self)
    }
}

pub struct DepthFirstIterator<'a> {
    stack: Vec<&'a HopNode>,
}

impl<'a> DepthFirstIterator<'a> {
    fn new(root: &'a HopNode) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a> Iterator for DepthFirstIterator<'a> {
    type Item = &'a HopNode;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;

        // Add children in reverse order so they're visited in correct order
        for child in current.children().iter().rev() {
            self.stack.push(child);
        }

        Some(current)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Range;

    #[test]
    fn test_depth_first_iterator() {
        // Create a simple tree: div -> (p, span -> text)
        let text_node = HopNode::Text(TextNode {
            value: "hello".to_string(),
            range: Range::default(),
        });

        let span_node = HopNode::NativeHTML(NativeHTMLNode {
            tag_name: "span".to_string(),
            attributes: vec![],
            range: Range::default(),
            children: vec![text_node],
            set_attributes: vec![],
        });

        let p_node = HopNode::NativeHTML(NativeHTMLNode {
            tag_name: "p".to_string(),
            attributes: vec![],
            range: Range::default(),
            children: vec![],
            set_attributes: vec![],
        });

        let div_node = HopNode::NativeHTML(NativeHTMLNode {
            tag_name: "div".to_string(),
            attributes: vec![],
            range: Range::default(),
            children: vec![p_node, span_node],
            set_attributes: vec![],
        });

        let nodes: Vec<&HopNode> = div_node.iter_depth_first().collect();

        // Should visit: div, p, span, text
        assert_eq!(nodes.len(), 4);

        // Check the order
        assert!(matches!(nodes[0], HopNode::NativeHTML(node) if node.tag_name == "div"));
        assert!(matches!(nodes[1], HopNode::NativeHTML(node) if node.tag_name == "p"));
        assert!(matches!(nodes[2], HopNode::NativeHTML(node) if node.tag_name == "span"));
        assert!(matches!(nodes[3], HopNode::Text(node) if node.value == "hello"));
    }
}
