use std::collections::BTreeMap;

use crate::document::{
    DocumentPosition,
    document_cursor::{DocumentRange, Ranged, StringSpan},
};
use crate::dop::Argument;
use crate::dop::Expr;
use crate::dop::ParseTree;
use crate::dop::VarName;

use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::ast::Attribute;

pub type UntypedNode = Node<ParseTree>;
pub type TypedNode = Node<Expr>;

#[derive(Debug, Clone)]
pub enum Node<E> {
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
    TextExpression { expression: E, range: DocumentRange },

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
        args: Option<(Vec<Argument<E>>, DocumentRange)>,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// An If node contains content that is only evaluated when its condition
    /// expression evaluates to true.
    If {
        condition: E,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A For node contains content that is evaluated once for each item of
    /// an array.
    For {
        var_name: VarName,
        var_name_range: DocumentRange,
        array_expr: E,
        children: Vec<Self>,
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
        attributes: BTreeMap<StringSpan, Attribute<E>>,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A Placeholder node represents a node that could not be constructed (because
    /// it is missing a required attribute or similar).
    ///
    /// We use Placeholder nodes to be able to construct the child nodes of the node that could not be
    /// constructed. This is useful for e.g. go-to-definition in the language server.
    Placeholder {
        children: Vec<Self>,
        range: DocumentRange,
    },
}

impl<T> Node<T> {
    /// Get the direct children of a node.
    pub fn children(&self) -> &[Self] {
        match self {
            Node::ComponentReference { children, .. } => children,
            Node::If { children, .. } => children,
            Node::For { children, .. } => children,
            Node::Html { children, .. } => children,
            Node::Placeholder { children, .. } => children,
            Node::Doctype { .. } => &[],
            Node::Text { .. } => &[],
            Node::TextExpression { .. } => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator<'_, T> {
        DepthFirstIterator::new(self)
    }

    /// Get the range for the opening tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            Node::ComponentReference {
                component_name_opening_range: tag_name,
                ..
            } => Some(tag_name),
            Node::Html { tag_name, .. } => Some(tag_name),
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
            Node::ComponentReference {
                component_name_closing_range: closing_tag_name,
                ..
            } => closing_tag_name.as_ref(),
            Node::Html {
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
}

impl<T> Node<T> {
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

impl<T> Ranged for Node<T> {
    fn range(&self) -> &DocumentRange {
        match self {
            Node::Text { range, .. }
            | Node::TextExpression { range, .. }
            | Node::ComponentReference { range, .. }
            | Node::If { range, .. }
            | Node::For { range, .. }
            | Node::Html { range, .. }
            | Node::Placeholder { range, .. }
            | Node::Doctype { range, .. } => range,
        }
    }
}

pub struct DepthFirstIterator<'a, T> {
    stack: Vec<&'a Node<T>>,
}

impl<'a, T> DepthFirstIterator<'a, T> {
    fn new(root: &'a Node<T>) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a, T> Iterator for DepthFirstIterator<'a, T> {
    type Item = &'a Node<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;

        // Add children in reverse order so they're visited in correct order
        for child in current.children().iter().rev() {
            self.stack.push(child);
        }

        Some(current)
    }
}
