use std::fmt::{self, Display};

use pretty::BoxDoc;

use crate::common::is_void_element;
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

impl ParsedMatchCase {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("<case {")
            .append(BoxDoc::text(self.pattern.to_string()))
            .append(BoxDoc::text("}>"))
            .append(if self.children.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.children.iter().map(|c| c.to_doc()),
                        BoxDoc::line(),
                    ))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("</case>"))
    }
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

    /// A Newline node represents a newline character in text position.
    /// This is separate from Text to allow precise whitespace handling.
    Newline { range: DocumentRange },

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
            ParsedNode::Newline { .. } => &[],
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

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedNode::Text { value, .. } => BoxDoc::text(value.as_str()),
            ParsedNode::Newline { .. } => BoxDoc::line(),
            ParsedNode::TextExpression { expression, .. } => BoxDoc::text("{")
                .append(expression.to_doc())
                .append(BoxDoc::text("}")),
            ParsedNode::ComponentReference {
                component_name,
                args,
                children,
                ..
            } => {
                let component_name_str = component_name.as_str();

                // Build opening tag with attributes (same format as HTML)
                let opening_tag_doc = if args.is_empty() {
                    BoxDoc::text("<").append(BoxDoc::text(component_name_str))
                } else {
                    BoxDoc::text("<")
                        .append(BoxDoc::text(component_name_str))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    args.iter().map(|a| a.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
                        .append(BoxDoc::line_())
                        .group()
                };

                if children.is_empty() {
                    opening_tag_doc.append(BoxDoc::text("/>"))
                } else {
                    opening_tag_doc
                        .append(BoxDoc::text(">"))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    children.iter().map(|c| c.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
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
            } => BoxDoc::text("<if {")
                .append(condition.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if children.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2)
                        .append(BoxDoc::line())
                })
                .append(BoxDoc::text("</if>")),
            ParsedNode::For {
                var_name,
                source,
                children,
                ..
            } => {
                let source_doc = match source {
                    ParsedLoopSource::Array(expr) => expr.to_doc(),
                    ParsedLoopSource::RangeInclusive { start, end } => start
                        .to_doc()
                        .append(BoxDoc::text("..="))
                        .append(end.to_doc()),
                };
                let var_doc = match var_name {
                    Some(name) => BoxDoc::text(name.as_str()),
                    None => BoxDoc::text("_"),
                };
                BoxDoc::text("<for {")
                    .append(var_doc)
                    .append(BoxDoc::text(" in "))
                    .append(source_doc)
                    .append(BoxDoc::text("}>"))
                    .append(if children.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2)
                            .append(BoxDoc::line())
                    })
                    .append(BoxDoc::text("</for>"))
            }
            ParsedNode::Let {
                bindings, children, ..
            } => {
                let bindings_doc = BoxDoc::line_()
                    .append(BoxDoc::intersperse(
                        bindings.iter().map(|b| {
                            BoxDoc::text(b.var_name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(BoxDoc::text(b.var_type.to_string()))
                                .append(BoxDoc::text(" = "))
                                .append(b.value_expr.to_doc())
                        }),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                    .nest(2)
                    .append(BoxDoc::line_())
                    .group();
                BoxDoc::text("<let {")
                    .append(bindings_doc)
                    .append(BoxDoc::text("}>"))
                    .append(if children.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2)
                            .append(BoxDoc::line())
                    })
                    .append(BoxDoc::text("</let>"))
            }
            ParsedNode::Doctype { value, .. } => BoxDoc::text(value.as_str()),
            ParsedNode::Match { subject, cases, .. } => BoxDoc::text("<match {")
                .append(subject.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if cases.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            cases.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2)
                        .append(BoxDoc::line())
                })
                .append(BoxDoc::text("</match>")),
            ParsedNode::Html {
                tag_name,
                attributes,
                children,
                ..
            } => {
                let tag_name_str = tag_name.as_str();

                // Build the opening tag with attributes
                // When attributes break to multiple lines, each attribute goes on its own line
                // and the closing > goes on its own line
                let opening_tag_doc = if attributes.is_empty() {
                    BoxDoc::text("<")
                        .append(BoxDoc::text(tag_name_str))
                        .append(BoxDoc::text(">"))
                } else {
                    BoxDoc::text("<")
                        .append(BoxDoc::text(tag_name_str))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    attributes.iter().map(|attr| attr.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
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
                    opening_tag_doc
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    children.iter().map(|c| c.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
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
                    BoxDoc::text("<placeholder>")
                        .append(BoxDoc::line())
                        .append(BoxDoc::intersperse(
                            children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2)
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("</placeholder>"))
                }
            }
        }
    }
}

impl Ranged for ParsedNode {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedNode::Text { range, .. }
            | ParsedNode::Newline { range }
            | ParsedNode::TextExpression { range, .. }
            | ParsedNode::ComponentReference { range, .. }
            | ParsedNode::If { range, .. }
            | ParsedNode::For { range, .. }
            | ParsedNode::Let { range, .. }
            | ParsedNode::Match { range, .. }
            | ParsedNode::Html { range, .. }
            | ParsedNode::Placeholder { range, .. }
            | ParsedNode::Doctype { range, .. } => range,
        }
    }
}

impl Display for ParsedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
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
