use crate::common::Range;
use crate::dop::{DopArgument, DopExpr, DopParameter, parser::DopVarName};

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
    pub value_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopExprAttribute {
    pub name: String,
    pub expression: DopExpr,
    pub range: Range,
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
pub struct RenderNode {
    pub file_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelHopNode {
    Import(ImportNode),
    ComponentDefinition(ComponentDefinitionNode),
    Render(RenderNode),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HopNode {
    Doctype {
        value: String,
        range: Range,
    },
    Text {
        value: String,
        range: Range,
    },
    TextExpression {
        expression: DopExpr,
        range: Range,
    },
    ComponentReference {
        component: String,
        opening_name_range: Range,
        closing_name_range: Option<Range>,
        args: Vec<DopArgument>,
        attributes: Vec<Attribute>,
        range: Range,
        children: Vec<HopNode>,
    },
    SlotDefinition {
        name: String,
        range: Range,
        children: Vec<HopNode>,
    },
    SlotReference {
        name: String,
        range: Range,
        children: Vec<HopNode>,
    },
    If {
        condition: DopExpr,
        range: Range,
        children: Vec<HopNode>,
    },
    For {
        var_name: DopVarName,
        array_expr: DopExpr,
        range: Range,
        children: Vec<HopNode>,
    },
    NativeHTML {
        tag_name: String,
        attributes: Vec<Attribute>,
        range: Range,
        children: Vec<HopNode>,
        set_attributes: Vec<DopExprAttribute>,
    },
    Error {
        range: Range,
        children: Vec<HopNode>,
    },
    XExec {
        cmd_attr: Attribute,
        range: Range,
        children: Vec<HopNode>,
    },
    XRaw {
        trim: bool,
        range: Range,
        children: Vec<HopNode>,
    },
    XLoadJson {
        file_attr: Attribute,
        as_attr: Attribute,
        range: Range,
        children: Vec<HopNode>,
    },
}

impl HopNode {
    pub fn children(&self) -> &[HopNode] {
        match self {
            HopNode::ComponentReference { children, .. } => children,
            HopNode::SlotDefinition { children, .. } => children,
            HopNode::SlotReference { children, .. } => children,
            HopNode::If { children, .. } => children,
            HopNode::For { children, .. } => children,
            HopNode::NativeHTML { children, .. } => children,
            HopNode::Error { children, .. } => children,
            HopNode::XExec { children, .. } => children,
            HopNode::XRaw { children, .. } => children,
            HopNode::XLoadJson { children, .. } => children,
            // Leaf nodes with no children
            HopNode::Doctype { .. } => &[],
            HopNode::Text { .. } => &[],
            HopNode::TextExpression { .. } => &[],
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
        let text_node = HopNode::Text {
            value: "hello".to_string(),
            range: Range::default(),
        };

        let span_node = HopNode::NativeHTML {
            tag_name: "span".to_string(),
            attributes: vec![],
            range: Range::default(),
            children: vec![text_node],
            set_attributes: vec![],
        };

        let p_node = HopNode::NativeHTML {
            tag_name: "p".to_string(),
            attributes: vec![],
            range: Range::default(),
            children: vec![],
            set_attributes: vec![],
        };

        let div_node = HopNode::NativeHTML {
            tag_name: "div".to_string(),
            attributes: vec![],
            range: Range::default(),
            children: vec![p_node, span_node],
            set_attributes: vec![],
        };

        let nodes: Vec<&HopNode> = div_node.iter_depth_first().collect();

        // Should visit: div, p, span, text
        assert_eq!(nodes.len(), 4);

        // Check the order
        assert!(matches!(nodes[0], HopNode::NativeHTML { tag_name, .. } if tag_name == "div"));
        assert!(matches!(nodes[1], HopNode::NativeHTML { tag_name, .. } if tag_name == "p"));
        assert!(matches!(nodes[2], HopNode::NativeHTML { tag_name, .. } if tag_name == "span"));
        assert!(matches!(nodes[3], HopNode::Text { value, .. } if value == "hello"));
    }
}
