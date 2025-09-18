use std::collections::BTreeMap;

use crate::{
    document::{
        DocumentPosition,
        document_cursor::{DocumentRange, Ranged, StringSpan},
    },
    dop::{
        Argument, VarName,
        expr::{TypedExpr, UntypedExpr},
    },
};

use super::{ast::Attribute, module_name::ModuleName};

pub type UntypedNode = Node<UntypedExpr, DocumentRange>;
pub type TypedNode = Node<TypedExpr, DocumentRange>;

#[derive(Debug, Clone)]
pub enum Node<E, R> {
    /// A Text node represents text in the document.
    /// E.g. <div>hello world</div>
    ///           ^^^^^^^^^^^
    Text { value: StringSpan, range: R },

    /// A TextExpression represents an expression that occurs in a text position.
    /// E.g. <div>hello {world}</div>
    ///                 ^^^^^^^
    TextExpression { expression: E, range: R },

    /// A ComponentReference represents a reference to a component.
    /// E.g.
    /// <my-component>
    ///   <other-component/>
    ///   ^^^^^^^^^^^^^^^^^^
    /// </my-component>
    ComponentReference {
        tag_name: DocumentRange,
        definition_module: Option<ModuleName>,
        closing_tag_name: Option<DocumentRange>,
        args: Option<(Vec<Argument<E>>, DocumentRange)>,
        attributes: BTreeMap<StringSpan, Attribute<E>>,
        children: Vec<Self>,
        range: R,
    },

    /// A SlotDefinition node represents the definition of a slot, e.g.
    /// the <slot-default/> in
    ///
    /// <my-component>
    ///   <slot-default/>
    /// </my-component>
    SlotDefinition { range: R },

    /// An If node contains content that is only evaluated when its condition
    /// expression evaluates to true.
    If {
        condition: E,
        children: Vec<Self>,
        range: R,
    },

    /// A For node contains content that is evaluated once for each item of
    /// an array.
    For {
        var_name: VarName,
        array_expr: E,
        children: Vec<Self>,
        range: R,
    },

    /// A Doctype node represents a doctype, e.g. a <!DOCTYPE html>
    Doctype { value: StringSpan, range: R },

    /// An HTML node represents a plain HTML node.
    /// E.g. <div>...</div>
    ///      ^^^^^^^^^^^^^^
    Html {
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: BTreeMap<StringSpan, Attribute<E>>,
        children: Vec<Self>,
        range: R,
    },

    /// A Placeholder node represents a node that could not be constructed (because
    /// it is missing a required attribute or similar).
    ///
    /// We use Placeholder nodes to be able to construct the child nodes of the node that could not be
    /// constructed. This is useful for e.g. go-to-definition in the language server.
    Placeholder { children: Vec<Self>, range: R },
}

#[derive(Debug, Clone)]
pub enum InlinedNode {
    Text {
        value: StringSpan,
    },
    TextExpression {
        expression: TypedExpr,
    },
    If {
        condition: TypedExpr,
        children: Vec<Self>,
    },
    For {
        var_name: VarName,
        array_expr: TypedExpr,
        children: Vec<Self>,
    },
    Doctype {
        value: StringSpan,
    },
    Html {
        tag_name: StringSpan,
        attributes: BTreeMap<StringSpan, Attribute<TypedExpr>>,
        children: Vec<Self>,
    },
    Let {
        var: VarName,
        value: TypedExpr,
        children: Vec<Self>,
    },
}

impl<T, R> Node<T, R> {
    /// Get the direct children of a node.
    pub fn children(&self) -> &[Self] {
        match self {
            Node::ComponentReference { children, .. } => children,
            Node::If { children, .. } => children,
            Node::For { children, .. } => children,
            Node::Html { children, .. } => children,
            Node::Placeholder { children, .. } => children,
            Node::SlotDefinition { .. } => &[],
            Node::Doctype { .. } => &[],
            Node::Text { .. } => &[],
            Node::TextExpression { .. } => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator<T, R> {
        DepthFirstIterator::new(self)
    }

    /// Get the range for the opening tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            Node::ComponentReference { tag_name, .. } => Some(tag_name),
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
                closing_tag_name, ..
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

impl<T> Node<T, DocumentRange> {
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

impl<T> Ranged for Node<T, DocumentRange> {
    fn range(&self) -> &DocumentRange {
        match self {
            Node::Text { range, .. }
            | Node::TextExpression { range, .. }
            | Node::ComponentReference { range, .. }
            | Node::SlotDefinition { range, .. }
            | Node::If { range, .. }
            | Node::For { range, .. }
            | Node::Html { range, .. }
            | Node::Placeholder { range, .. }
            | Node::Doctype { range, .. } => range,
        }
    }
}

pub struct DepthFirstIterator<'a, T, A> {
    stack: Vec<&'a Node<T, A>>,
}

impl<'a, T, A> DepthFirstIterator<'a, T, A> {
    fn new(root: &'a Node<T, A>) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a, T, A> Iterator for DepthFirstIterator<'a, T, A> {
    type Item = &'a Node<T, A>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;

        // Add children in reverse order so they're visited in correct order
        for child in current.children().iter().rev() {
            self.stack.push(child);
        }

        Some(current)
    }
}
