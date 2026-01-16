use crate::document::{CheapString, DocumentRange, Ranged};
use crate::dop::ParsedExpr;
use crate::dop::VarName;
use crate::dop::syntax::parsed::{ParsedLoopSource, ParsedMatchPattern, ParsedType};

use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::parsed_ast::ParsedAttribute;

/// A case in a match node.
/// E.g. <match {x}>
///        <case {Some(y)}>...</case>
///        ^^^^^^^^^^^^^^^^^^^^^^^
///      </match>
#[derive(Debug, Clone)]
pub struct ParsedMatchCase {
    pub pattern: ParsedMatchPattern,
    pub pattern_range: DocumentRange,
    pub children: Vec<ParsedNode>,
    pub range: DocumentRange,
}

/// A single binding in a let node.
/// E.g. `name: String = "World"` in `<let {name: String = "World", count: Int = 0}>`
#[derive(Debug, Clone)]
pub struct ParsedLetBinding {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: ParsedType,
    pub value_expr: ParsedExpr,
}

#[derive(Debug, Clone)]
pub enum ParsedNode {
    /// A Text node represents text in the document.
    /// E.g. <div>hello world</div>
    ///           ^^^^^^^^^^^
    Text {
        value: CheapString,
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
        declaring_module: Option<ModuleName>,
        args: Vec<ParsedAttribute>,
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
    /// an array or each value in a range.
    /// When var_name is None, the loop variable is discarded (underscore syntax).
    For {
        var_name: Option<VarName>,
        var_name_range: Option<DocumentRange>,
        source: ParsedLoopSource,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A Match node contains pattern matching over a subject expression.
    /// E.g. <match {x}>
    ///        <case {Some(y)}>found {y}!</case>
    ///        <case {None}>not found</case>
    ///      </match>
    Match {
        subject: ParsedExpr,
        cases: Vec<ParsedMatchCase>,
        range: DocumentRange,
    },

    /// A Let node introduces one or more local variable bindings.
    /// E.g. <let {name: String = "World", count: Int = 0}>Hello {name}</let>
    Let {
        bindings: Vec<ParsedLetBinding>,
        bindings_range: DocumentRange,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A Doctype node represents a doctype, e.g. a <!DOCTYPE html>
    Doctype {
        value: CheapString,
        range: DocumentRange,
    },

    /// An HTML node represents a plain HTML node.
    /// E.g. <div>...</div>
    ///      ^^^^^^^^^^^^^^
    Html {
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
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

    /// A LineBreak node represents a line break in formatted output.
    /// It is inserted by whitespace removal to indicate where line breaks should occur.
    /// LineBreak nodes are only created during formatting, never by the parser.
    LineBreak { range: DocumentRange },
}

impl ParsedNode {
    /// Get the direct children of a node.
    pub fn children(&self) -> &[Self] {
        match self {
            ParsedNode::ComponentReference { children, .. } => children,
            ParsedNode::If { children, .. } => children,
            ParsedNode::For { children, .. } => children,
            ParsedNode::Let { children, .. } => children,
            ParsedNode::Html { children, .. } => children,
            ParsedNode::Placeholder { children, .. } => children,
            ParsedNode::Match { .. } => &[], // children are inside cases
            ParsedNode::Doctype { .. } => &[],
            ParsedNode::Text { .. } => &[],
            ParsedNode::TextExpression { .. } => &[],
            ParsedNode::LineBreak { .. } => &[],
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
}

impl Ranged for ParsedNode {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedNode::Text { range, .. }
            | ParsedNode::TextExpression { range, .. }
            | ParsedNode::ComponentReference { range, .. }
            | ParsedNode::If { range, .. }
            | ParsedNode::For { range, .. }
            | ParsedNode::Let { range, .. }
            | ParsedNode::Match { range, .. }
            | ParsedNode::Html { range, .. }
            | ParsedNode::Placeholder { range, .. }
            | ParsedNode::Doctype { range, .. }
            | ParsedNode::LineBreak { range } => range,
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
