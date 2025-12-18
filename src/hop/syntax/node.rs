use std::collections::BTreeMap;
use std::fmt::{self, Display};

use crate::document::{
    DocumentPosition,
    document_cursor::{DocumentRange, Ranged, StringSpan},
};
use crate::dop::ParsedExpr;
use crate::dop::VarName;

use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::ast::Attribute;

/// An ParsedArgument represents a parsed argument with a name and a value.
/// E.g. <my-comp {x: [1,2], y: 2}>
///                ^^^^^^^^
#[derive(Debug, Clone)]
pub struct ParsedArgument {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_expr: ParsedExpr,
}

impl Display for ParsedArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_expr)
    }
}

#[derive(Debug, Clone)]
pub enum ParsedNode {
    /// A Text node represents text in the document.
    /// E.g. <div>hello world</div>
    ///           ^^^^^^^^^^^
    Text {
        value: StringSpan,
        range: DocumentRange,
    },

    /// A TextExpression represents an expression that occurs in a text position.
    /// E.g. <div>hello {world}</div>
    ///                 ^^^^^^^
    TextExpression {
        expression: ParsedExpr,
        range: DocumentRange,
    },

    /// A ComponentReference represents a reference to a component.
    /// E.g.
    /// <my-component>
    ///   <other-component/>
    ///   ^^^^^^^^^^^^^^^^^^
    /// </my-component>
    ComponentReference {
        component_name: ComponentName,
        component_name_opening_range: DocumentRange,
        component_name_closing_range: Option<DocumentRange>,
        definition_module: Option<ModuleName>,
        args: Option<(Vec<ParsedArgument>, DocumentRange)>,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// An If node contains content that is only evaluated when its condition
    /// expression evaluates to true.
    If {
        condition: ParsedExpr,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A For node contains content that is evaluated once for each item of
    /// an array.
    For {
        var_name: VarName,
        var_name_range: DocumentRange,
        array_expr: ParsedExpr,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A Doctype node represents a doctype, e.g. a <!DOCTYPE html>
    Doctype {
        value: StringSpan,
        range: DocumentRange,
    },

    /// An HTML node represents a plain HTML node.
    /// E.g. <div>...</div>
    ///      ^^^^^^^^^^^^^^
    Html {
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: BTreeMap<StringSpan, Attribute>,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A Placeholder node represents a node that could not be constructed (because
    /// it is missing a required attribute or similar).
    ///
    /// We use Placeholder nodes to be able to construct the child nodes of the node that could not be
    /// constructed. This is useful for e.g. go-to-definition in the language server.
    Placeholder {
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },
}

impl ParsedNode {
    /// Get the direct children of a node.
    pub fn children(&self) -> &[Self] {
        match self {
            ParsedNode::ComponentReference { children, .. } => children,
            ParsedNode::If { children, .. } => children,
            ParsedNode::For { children, .. } => children,
            ParsedNode::Html { children, .. } => children,
            ParsedNode::Placeholder { children, .. } => children,
            ParsedNode::Doctype { .. } => &[],
            ParsedNode::Text { .. } => &[],
            ParsedNode::TextExpression { .. } => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator<'_> {
        DepthFirstIterator::new(self)
    }

    /// Get the range for the opening tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedNode::ComponentReference {
                component_name_opening_range: tag_name,
                ..
            } => Some(tag_name),
            ParsedNode::Html { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    /// Get the range for the closing tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///                   ^^^
    pub fn closing_tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedNode::ComponentReference {
                component_name_closing_range: closing_tag_name,
                ..
            } => closing_tag_name.as_ref(),
            ParsedNode::Html {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            _ => None,
        }
    }

    /// Get the name ranges for the tags of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^              ^^^
    pub fn tag_names(&self) -> impl Iterator<Item = &DocumentRange> {
        self.tag_name().into_iter().chain(self.closing_tag_name())
    }

    pub fn find_node_at_position(&self, position: DocumentPosition) -> Option<&Self> {
        if !self.range().contains_position(position) {
            return None;
        }
        for child in self.children() {
            if let Some(node) = child.find_node_at_position(position) {
                return Some(node);
            }
        }
        Some(self)
    }
}

impl Ranged for ParsedNode {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedNode::Text { range, .. }
            | ParsedNode::TextExpression { range, .. }
            | ParsedNode::ComponentReference { range, .. }
            | ParsedNode::If { range, .. }
            | ParsedNode::For { range, .. }
            | ParsedNode::Html { range, .. }
            | ParsedNode::Placeholder { range, .. }
            | ParsedNode::Doctype { range, .. } => range,
        }
    }
}

pub struct DepthFirstIterator<'a> {
    stack: Vec<&'a ParsedNode>,
}

impl<'a> DepthFirstIterator<'a> {
    fn new(root: &'a ParsedNode) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a> Iterator for DepthFirstIterator<'a> {
    type Item = &'a ParsedNode;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;

        // Add children in reverse order so they're visited in correct order
        for child in current.children().iter().rev() {
            self.stack.push(child);
        }

        Some(current)
    }
}
