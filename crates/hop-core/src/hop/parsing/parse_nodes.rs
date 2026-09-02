use std::collections::VecDeque;
use std::iter::Peekable;

use super::parsed_ast::{ParsedAttribute, ParsedAttributeValue};
use super::parsed_node::{ParsedLetBinding, ParsedLoopSource, ParsedMatchCase, ParsedNode};
use super::tokenizer;
use super::tokenizer::{RawTextToken, TagToken, Token};
use super::whitespace;
use crate::document::{DocumentCursor, DocumentRange};
use crate::expr::parsing::ParsedType;
use crate::expr::parsing::parse_type::parse_type;
use crate::expr::parsing::parsed_expr::ParsedMatchPattern;
use crate::expr::{self, ParsedExpr};
use crate::html::{HtmlElement, has_raw_content, is_void_element};
use crate::parse_error::{ParseError, ParseErrorKind};
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

/// An item in a markup sequence.
enum MarkupItem {
    Node(ParsedNode),
    /// A `<case>` is not a node: it carries a pattern and only means anything as
    /// a child of a `<match>`. Parsing collects both kinds uniformly and each
    /// element then takes the kind it accepts, so `<case>` needs no special
    /// handling on the way in.
    Case {
        case: ParsedMatchCase,
        /// The range of the name in the opening tag.
        tag_name_range: DocumentRange,
    },
}

/// A closing tag that ended an element.
struct ClosingTag {
    tag_name_range: DocumentRange,
    range: DocumentRange,
}

/// An element whose opening tag has been read but whose closing tag has not.
struct OpenElement {
    /// The range of the name in the opening tag. E.g.
    /// ```text
    /// <div class="x">
    ///  ^^^
    /// ```
    tag_name: DocumentRange,
    /// The range of the opening tag. E.g.
    /// ```text
    /// <div class="x">
    /// ^^^^^^^^^^^^^^^
    /// ```
    opening_range: DocumentRange,
    /// What was read off the opening tag, waiting for the children.
    header: TagHeader,
    children: Vec<MarkupItem>,
}

/// What an opening tag carried, kept until the element can be built.
enum TagHeader {
    If(Option<ParsedExpr>),
    For(Option<ParsedLoopHeader>),
    Let(Option<(Vec<ParsedLetBinding>, DocumentRange)>),
    Match(Option<ParsedExpr>),
    Case(Option<ParsedMatchPattern>),
    Component {
        name: Option<TypeName>,
        args: Vec<ParsedAttribute>,
    },
    Html {
        element: Option<HtmlElement>,
        attributes: Vec<ParsedAttribute>,
    },
}

/// The markup built so far.
///
/// An item lands in the innermost element still open, or at the top level
/// when nothing is open.
#[derive(Default)]
struct MarkupBuilder {
    /// Items that nothing encloses.
    items: Vec<MarkupItem>,
    /// Elements whose opening tag has been read but whose closing tag has
    /// not, outermost first.
    open: Vec<OpenElement>,
}

impl MarkupBuilder {
    /// Add an item to the innermost open element, or to the top level.
    fn append(&mut self, item: MarkupItem) {
        match self.open.last_mut() {
            Some(element) => element.children.push(item),
            None => self.items.push(item),
        }
    }

    /// Add a node to the innermost open element, or to the top level.
    fn append_node(&mut self, node: ParsedNode) {
        self.append(MarkupItem::Node(node));
    }

    /// Start an element, so that what follows becomes its children.
    fn enter(&mut self, element: OpenElement) {
        self.open.push(element);
    }

    /// Build an element and add it where it belongs.
    fn append_element(
        &mut self,
        element: OpenElement,
        closing: Option<ClosingTag>,
        errors: &mut Vec<ParseError>,
    ) {
        if let Some(item) = close_element(element, closing, errors) {
            self.append(item);
        }
    }

    /// Whether a closing tag with this name would close anything.
    fn is_open(&self, tag_name: &str) -> bool {
        self.open.iter().any(|el| el.tag_name.as_str() == tag_name)
    }

    /// Close the element this tag names, along with everything opened inside
    /// it that was never closed.
    ///
    /// Expects `is_open` to hold for the tag.
    fn close(&mut self, closing: ClosingTag, errors: &mut Vec<ParseError>) {
        while self.open.last().unwrap().tag_name.as_str() != closing.tag_name_range.as_str() {
            self.close_innermost(errors);
        }
        let element = self.open.pop().unwrap();
        self.append_element(element, Some(closing), errors);
    }

    /// Close the innermost open element, which never got a closing tag.
    fn close_innermost(&mut self, errors: &mut Vec<ParseError>) {
        let element = self.open.pop().unwrap();
        errors.push(ParseError::new(
            ParseErrorKind::UnclosedTag {
                tag: element.tag_name.to_cheap_string(),
            },
            element.tag_name.clone(),
        ));
        self.append_element(element, None, errors);
    }

    /// Take the markup, closing anything left open.
    fn finish(mut self, errors: &mut Vec<ParseError>) -> Vec<MarkupItem> {
        while !self.open.is_empty() {
            self.close_innermost(errors);
        }
        self.items
    }
}

/// Parse markup until the input runs out.
///
/// Whitespace is normalized before the nodes are handed back, so that every
/// caller gets the same thing and none of them has to remember to ask.
///
/// We do our best here to build as much markup as possible even when we
/// encounter errors.
pub fn parse_nodes(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Vec<ParsedNode> {
    let mut builder = MarkupBuilder::default();

    while let Some(token) = tokenizer::next(iter, errors) {
        match token {
            Token::Text { range } => builder.append_node(ParsedNode::Text { range }),
            Token::Newline { range } => builder.append_node(ParsedNode::Newline { range }),
            Token::Comment { range } => builder.append_node(ParsedNode::Comment { range }),

            Token::ExpressionStart { left_brace } => {
                if let Some(expression) =
                    expr::parse_expr::parse_expr(iter, comments, errors, &left_brace)
                    && let Some(right_brace) = expr::tokenizer::expect_opposite(
                        iter,
                        comments,
                        errors,
                        &expr::Token::LeftBrace,
                        &left_brace,
                    )
                {
                    builder.append_node(ParsedNode::TextExpression {
                        expression,
                        range: left_brace.to(right_brace),
                    });
                }
            }

            Token::OpeningTagStart { tag_name, range } => {
                let (element, end) = read_opening_tag(tag_name, range, iter, comments, errors);
                match end {
                    TagEnd::Open => builder.enter(element),
                    TagEnd::Closed => builder.append_element(element, None, errors),
                }
            }

            Token::ClosingTag { tag_name, range } => {
                if is_void_element(tag_name.as_str()) {
                    errors.push(ParseError::new(
                        ParseErrorKind::ClosedVoidTag {
                            tag: tag_name.to_cheap_string(),
                        },
                        range,
                    ));
                } else if !builder.is_open(tag_name.as_str()) {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnmatchedClosingTag {
                            tag: tag_name.to_cheap_string(),
                        },
                        range,
                    ));
                } else {
                    builder.close(
                        ClosingTag {
                            tag_name_range: tag_name,
                            range,
                        },
                        errors,
                    );
                }
            }
        }
    }

    let mut nodes = expect_nodes(builder.finish(errors), errors);
    whitespace::normalize(&mut nodes);
    nodes
}

/// Where an opening tag left the parse.
enum TagEnd {
    /// Children follow, then a closing tag.
    Open,
    /// The element is finished as it stands.
    Closed,
}

/// What a tag's `{...}` held. Its name decides which.
enum TagExpression {
    Expr(ParsedExpr),
    LoopHeader(ParsedLoopHeader),
    Bindings(Vec<ParsedLetBinding>),
    Pattern(ParsedMatchPattern),
}

impl TagExpression {
    fn expr(self) -> Option<ParsedExpr> {
        match self {
            Self::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    fn loop_header(self) -> Option<ParsedLoopHeader> {
        match self {
            Self::LoopHeader(header) => Some(header),
            _ => None,
        }
    }

    fn bindings(self) -> Option<Vec<ParsedLetBinding>> {
        match self {
            Self::Bindings(bindings) => Some(bindings),
            _ => None,
        }
    }

    fn pattern(self) -> Option<ParsedMatchPattern> {
        match self {
            Self::Pattern(pattern) => Some(pattern),
            _ => None,
        }
    }
}

/// Read an opening tag from just after its name, and say whether children
/// follow it.
fn read_opening_tag(
    tag_name: DocumentRange,
    start: DocumentRange,
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> (OpenElement, TagEnd) {
    let mut attributes: Vec<ParsedAttribute> = Vec::new();
    let mut expression: Option<TagExpression> = None;
    // Set as soon as a `{` is seen.
    let mut expression_range: Option<DocumentRange> = None;
    let mut self_closing = false;
    let mut opening_range = start.clone();

    loop {
        let Some(part) = tokenizer::next_tag_token(iter, errors) else {
            errors.push(ParseError::new(
                ParseErrorKind::UnterminatedOpeningTag {},
                tag_name.clone(),
            ));
            break;
        };
        match part {
            TagToken::End { range } => {
                opening_range = start.clone().to(range);
                break;
            }

            TagToken::SelfClosingEnd { range } => {
                self_closing = true;
                opening_range = start.clone().to(range);
                break;
            }

            TagToken::Attribute { name, value } => {
                let value = value.map(|value| ParsedAttributeValue::String {
                    content: value.content,
                    quoted_range: value.quoted_range,
                });
                push_attribute(&mut attributes, name, value, errors);
            }

            TagToken::AttributeExpressionStart { name, left_brace } => {
                if let Some(value) =
                    expr::parse_expr::parse_expr(iter, comments, errors, &left_brace)
                    && expr::tokenizer::expect_opposite(
                        iter,
                        comments,
                        errors,
                        &expr::Token::LeftBrace,
                        &left_brace,
                    )
                    .is_some()
                {
                    let value = ParsedAttributeValue::Expression(value);
                    push_attribute(&mut attributes, name, Some(value), errors);
                }
            }

            TagToken::Spread { name, range } => match VarName::new(name.as_str()) {
                Ok(var_name) => attributes.push(ParsedAttribute::Spread {
                    name: var_name,
                    range,
                }),
                Err(error) => errors.push(ParseError::new(
                    ParseErrorKind::InvalidVariableName {
                        name: name.to_cheap_string(),
                        error,
                    },
                    name,
                )),
            },

            TagToken::ExpressionStart { left_brace } => {
                match read_tag_expression(&tag_name, left_brace.clone(), iter, comments, errors) {
                    Some((parsed, range)) => {
                        expression = Some(parsed);
                        expression_range = Some(range);
                    }
                    None => expression_range = Some(left_brace),
                }
            }
        }
    }

    // A raw text element holds text rather than markup, so its content and
    // closing tag are read here.
    let raw_text = !self_closing && has_raw_content(tag_name.as_str());
    let mut children = Vec::new();
    if raw_text {
        match tokenizer::next_raw_text_token(iter, &tag_name) {
            Some(RawTextToken {
                content,
                closing_tag_end,
            }) => {
                children.extend(content.map(|range| MarkupItem::Node(ParsedNode::Text { range })));
                opening_range = start.to(closing_tag_end);
            }
            None => errors.push(ParseError::new(
                ParseErrorKind::UnterminatedOpeningTag {},
                tag_name.clone(),
            )),
        }
    }

    let header = build_header(
        &tag_name,
        attributes,
        expression,
        expression_range,
        &opening_range,
        errors,
    );
    let end = if raw_text || self_closing || is_void_element(tag_name.as_str()) {
        TagEnd::Closed
    } else {
        TagEnd::Open
    };
    (
        OpenElement {
            tag_name,
            opening_range,
            header,
            children,
        },
        end,
    )
}

/// Read the `{...}` on a tag, with the sub-parser its name calls for.
fn read_tag_expression(
    tag_name: &DocumentRange,
    left_brace: DocumentRange,
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<(TagExpression, DocumentRange)> {
    let expression = match tag_name.as_str() {
        "for" => {
            parse_loop_header(iter, comments, errors, &left_brace).map(TagExpression::LoopHeader)?
        }
        "let" => TagExpression::Bindings(
            parse_let_bindings(iter, comments, errors, &left_brace)?
                .into_iter()
                .map(
                    |(var_name, var_name_range, var_type, value_expr)| ParsedLetBinding {
                        var_name,
                        var_name_range,
                        var_type,
                        value_expr,
                    },
                )
                .collect(),
        ),
        "case" => expr::parse_expr::parse_match_pattern(iter, comments, errors, &left_brace)
            .map(TagExpression::Pattern)?,
        _ => expr::parse_expr::parse_expr(iter, comments, errors, &left_brace)
            .map(TagExpression::Expr)?,
    };
    let right_brace = expr::tokenizer::expect_opposite(
        iter,
        comments,
        errors,
        &expr::Token::LeftBrace,
        &left_brace,
    )?;
    Some((expression, left_brace.to(right_brace)))
}

/// Build a tag's header from what was read off it.
fn build_header(
    tag_name: &DocumentRange,
    attributes: Vec<ParsedAttribute>,
    expression: Option<TagExpression>,
    expression_range: Option<DocumentRange>,
    opening_range: &DocumentRange,
    errors: &mut Vec<ParseError>,
) -> TagHeader {
    let written = expression_range.is_some();
    match tag_name.as_str() {
        "if" => {
            reject_attributes(&attributes, tag_name, errors);
            let expression = require_expression(
                expression,
                written,
                ParseErrorKind::MissingIfExpression {},
                opening_range,
                errors,
            );
            TagHeader::If(expression.and_then(TagExpression::expr))
        }

        "for" => {
            reject_attributes(&attributes, tag_name, errors);
            let expression = require_expression(
                expression,
                written,
                ParseErrorKind::MissingForExpression {},
                opening_range,
                errors,
            );
            TagHeader::For(expression.and_then(TagExpression::loop_header))
        }

        "let" => {
            reject_attributes(&attributes, tag_name, errors);
            let expression = require_expression(
                expression,
                written,
                ParseErrorKind::MissingLetBinding {},
                opening_range,
                errors,
            );
            TagHeader::Let(
                expression
                    .and_then(TagExpression::bindings)
                    .zip(expression_range),
            )
        }

        "match" => {
            reject_attributes(&attributes, tag_name, errors);
            let expression = require_expression(
                expression,
                written,
                ParseErrorKind::MissingMatchExpression {},
                opening_range,
                errors,
            );
            TagHeader::Match(expression.and_then(TagExpression::expr))
        }

        "case" => {
            let expression = require_expression(
                expression,
                written,
                ParseErrorKind::MissingCasePattern {},
                opening_range,
                errors,
            );
            TagHeader::Case(expression.and_then(TagExpression::pattern))
        }

        // A PascalCase tag names a component.
        name if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
            let name = match TypeName::new(name) {
                Ok(name) => Some(name),
                Err(error) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::InvalidTypeName { error },
                        tag_name.clone(),
                    ));
                    None
                }
            };
            // A component takes its arguments as attributes, never as a bare {..}.
            if let Some(range) = expression_range {
                errors.push(ParseError::new(
                    ParseErrorKind::UnexpectedComponentExpression {
                        tag_name: tag_name.to_cheap_string(),
                    },
                    range,
                ));
            }
            TagHeader::Component {
                name,
                args: attributes,
            }
        }

        _ => {
            let element = HtmlElement::parse(tag_name.as_str());
            if element.is_none() {
                errors.push(ParseError::new(
                    ParseErrorKind::UnknownHtmlElement {
                        tag: tag_name.to_cheap_string(),
                    },
                    tag_name.clone(),
                ));
            }
            TagHeader::Html {
                element,
                attributes,
            }
        }
    }
}

/// Report a tag's `{...}` as missing when it has none.
fn require_expression(
    expression: Option<TagExpression>,
    written: bool,
    missing: ParseErrorKind,
    opening_range: &DocumentRange,
    errors: &mut Vec<ParseError>,
) -> Option<TagExpression> {
    if !written {
        errors.push(ParseError::new(missing, opening_range.clone()));
    }
    expression
}

/// Add a named attribute, rejecting one the tag already has.
fn push_attribute(
    attributes: &mut Vec<ParsedAttribute>,
    name: DocumentRange,
    value: Option<ParsedAttributeValue>,
    errors: &mut Vec<ParseError>,
) {
    let duplicate = attributes.iter().any(|attribute| match attribute {
        ParsedAttribute::Named { name: existing, .. } => existing.as_str() == name.as_str(),
        ParsedAttribute::Spread { .. } => false,
    });
    if duplicate {
        errors.push(ParseError::new(
            ParseErrorKind::DuplicateAttribute {
                name: name.to_cheap_string(),
            },
            name,
        ));
        return;
    }
    attributes.push(ParsedAttribute::Named { name, value });
}

/// Report the attributes on a tag that takes none.
fn reject_attributes(
    attributes: &[ParsedAttribute],
    tag_name: &DocumentRange,
    errors: &mut Vec<ParseError>,
) {
    for attribute in attributes {
        let (attr_name, range) = match attribute {
            ParsedAttribute::Named { name, .. } => (name.to_cheap_string(), name.clone()),
            ParsedAttribute::Spread { range, .. } => (range.to_cheap_string(), range.clone()),
        };
        errors.push(ParseError::new(
            ParseErrorKind::UnrecognizedAttribute {
                tag_name: tag_name.to_cheap_string(),
                attr_name,
            },
            range,
        ));
    }
}

/// Build an element from its header and the children that were collected for
/// it.
///
/// An element with no closing tag spans only its opening tag, since we cannot
/// tell how far the author meant it to reach.
fn close_element(
    element: OpenElement,
    closing: Option<ClosingTag>,
    errors: &mut Vec<ParseError>,
) -> Option<MarkupItem> {
    let OpenElement {
        tag_name,
        opening_range,
        header,
        children,
    } = element;
    let (closing_tag_name, range) = match closing {
        Some(closing) => (
            Some(closing.tag_name_range),
            opening_range.to(closing.range),
        ),
        None => (None, opening_range),
    };

    match header {
        TagHeader::If(condition) => {
            let children = expect_nodes(children, errors);
            condition.map(|condition| {
                MarkupItem::Node(ParsedNode::If {
                    condition,
                    range,
                    children,
                })
            })
        }

        TagHeader::For(header) => {
            let children = expect_nodes(children, errors);
            header.map(|header| {
                MarkupItem::Node(ParsedNode::For {
                    var_name: header.var_name,
                    var_name_range: header.var_name_range,
                    source: header.loop_source,
                    range,
                    children,
                })
            })
        }

        TagHeader::Let(bindings) => {
            let children = expect_nodes(children, errors);
            bindings.map(|(bindings, bindings_range)| {
                MarkupItem::Node(ParsedNode::Let {
                    bindings,
                    bindings_range,
                    range,
                    children,
                })
            })
        }

        TagHeader::Match(subject) => {
            let cases = expect_cases(children, errors);
            subject.map(|subject| {
                MarkupItem::Node(ParsedNode::Match {
                    subject,
                    cases,
                    range,
                })
            })
        }

        TagHeader::Case(pattern) => {
            let children = expect_nodes(children, errors);
            pattern.map(|pattern| MarkupItem::Case {
                case: ParsedMatchCase { pattern, children },
                tag_name_range: tag_name,
            })
        }

        TagHeader::Component { name, args } => {
            let children = expect_nodes(children, errors);
            let children = closing_tag_name.is_some().then_some(children);
            name.map(|component_name| {
                MarkupItem::Node(ParsedNode::ComponentInvocation {
                    component_name,
                    component_name_opening_range: tag_name,
                    component_name_closing_range: closing_tag_name,
                    args,
                    range,
                    children,
                })
            })
        }

        TagHeader::Html {
            element,
            attributes,
        } => {
            let children = expect_nodes(children, errors);
            element.map(|element| {
                MarkupItem::Node(ParsedNode::Html {
                    element,
                    tag_name,
                    closing_tag_name,
                    attributes,
                    range,
                    children,
                })
            })
        }
    }
}

/// Take the nodes out of a markup sequence.
///
/// A `<case>` here is not inside a `<match>`, which is the only place it
/// means anything.
fn expect_nodes(items: Vec<MarkupItem>, errors: &mut Vec<ParseError>) -> Vec<ParsedNode> {
    let mut nodes = Vec::new();
    for item in items {
        match item {
            MarkupItem::Node(node) => nodes.push(node),
            MarkupItem::Case {
                tag_name_range: tag_name,
                ..
            } => {
                errors.push(ParseError::new(
                    ParseErrorKind::CaseOutsideMatch {},
                    tag_name,
                ));
            }
        }
    }
    nodes
}

/// Take the cases out of the body of a `<match>`.
///
/// Layout between the cases is dropped, and anything else is rejected.
fn expect_cases(items: Vec<MarkupItem>, errors: &mut Vec<ParseError>) -> Vec<ParsedMatchCase> {
    let mut cases = Vec::new();
    for item in items {
        let node = match item {
            MarkupItem::Case { case, .. } => {
                cases.push(case);
                continue;
            }
            MarkupItem::Node(node) => node,
        };
        match node {
            ParsedNode::Newline { .. } => {}
            ParsedNode::Text { ref range } if range.as_str().trim().is_empty() => {}
            node => errors.push(ParseError::new(
                ParseErrorKind::InvalidMatchChild {},
                node.range().clone(),
            )),
        }
    }
    cases
}

struct ParsedLoopHeader {
    var_name: Option<VarName>,
    var_name_range: Option<DocumentRange>,
    loop_source: Box<ParsedLoopSource>,
}

fn parse_loop_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedLoopHeader> {
    let (var_name, var_name_range) = if let Some(underscore_range) =
        expr::tokenizer::advance_if(iter, comments, errors, expr::Token::Underscore)
    {
        (None, Some(underscore_range))
    } else {
        let (name, name_range) =
            expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
        (Some(name), Some(name_range))
    };
    expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::In)?;
    let start_expr = expr::parse_expr::parse_expr(iter, comments, errors, range)?;
    let source =
        if expr::tokenizer::advance_if(iter, comments, errors, expr::Token::DotDotEq).is_some() {
            let end_expr = expr::parse_expr::parse_expr(iter, comments, errors, range)?;
            ParsedLoopSource::RangeInclusive {
                start: start_expr,
                end: end_expr,
            }
        } else {
            ParsedLoopSource::Array(start_expr)
        };
    Some(ParsedLoopHeader {
        var_name,
        var_name_range,
        loop_source: Box::new(source),
    })
}

fn parse_let_bindings(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<Vec<(VarName, DocumentRange, Option<ParsedType>, ParsedExpr)>> {
    let bindings = expr::tokenizer::parse_comma_separated(
        iter,
        comments,
        errors,
        range,
        |iter, comments, errors, range| {
            let (var_name, var_name_range) =
                expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
            let var_type = if let Some((expr::Token::Colon, _)) =
                expr::tokenizer::peek_past_comments(iter)
            {
                expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Colon)?;
                Some(parse_type(iter, comments, errors, range)?)
            } else {
                None
            };
            expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Assign)?;
            let value_expr = expr::parse_expr::parse_expr(iter, comments, errors, range)?;
            Some((var_name, var_name_range, var_type, value_expr))
        },
        Some(&expr::Token::RightBrace),
    )?;
    Some(bindings)
}
