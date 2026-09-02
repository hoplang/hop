use std::collections::VecDeque;
use std::iter::Peekable;

use super::parsed_ast::{self, ParsedAttribute};
use super::parsed_node::{ParsedLetBinding, ParsedLoopSource, ParsedMatchCase, ParsedNode};
use super::token::{Token, TokenizedAttribute, TokenizedAttributeValue};
use super::tokenizer;
use super::whitespace;
use crate::document::{DocumentCursor, DocumentRange};
use crate::expr::parsing::ParsedType;
use crate::expr::parsing::parse_type::parse_type;
use crate::expr::parsing::parsed_expr::ParsedMatchPattern;
use crate::expr::{self, ParsedExpr};
use crate::html::{HtmlElement, is_void_element};
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
        /// The name in the opening tag, so a `<case>` that is not inside a
        /// `<match>` can be reported where it was written.
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

            Token::TextExpression { content, range } => {
                let mut expr_iter = content.cursor().peekable();
                if let Some(expression) =
                    expr::parse_expr::parse_expr(&mut expr_iter, comments, errors, &content)
                {
                    builder.append_node(ParsedNode::TextExpression { expression, range });
                }
            }

            // A raw text element carries its content verbatim, so the
            // tokenizer hands over the whole element in one token.
            Token::RawTextTag {
                tag_name,
                attributes,
                content,
                range,
                ..
            } => {
                let attributes = parse_attributes(&attributes, comments, errors);
                let children = content
                    .map(|content| vec![ParsedNode::Text { range: content }])
                    .unwrap_or_default();
                match HtmlElement::parse(tag_name.as_str()) {
                    Some(element) => builder.append_node(ParsedNode::Html {
                        element,
                        tag_name,
                        closing_tag_name: None,
                        attributes,
                        range,
                        children,
                    }),
                    None => errors.push(ParseError::new(
                        ParseErrorKind::UnknownHtmlElement {
                            tag: tag_name.to_cheap_string(),
                        },
                        tag_name,
                    )),
                }
            }

            Token::OpeningTag {
                tag_name,
                attributes,
                expression,
                self_closing,
                range,
            } => {
                // A void or self-closing tag has no children and no closing
                // tag, so it is finished as soon as it is read.
                let closes_here = self_closing || is_void_element(tag_name.as_str());
                let element =
                    read_opening_tag(tag_name, attributes, expression, range, comments, errors);
                if closes_here {
                    builder.append_element(element, None, errors);
                } else {
                    builder.enter(element);
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

/// Read what an opening tag carries, which is decided by its name.
fn read_opening_tag(
    tag_name: DocumentRange,
    attributes: Vec<TokenizedAttribute>,
    expression: Option<DocumentRange>,
    opening_range: DocumentRange,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> OpenElement {
    let header = match tag_name.as_str() {
        "if" => {
            errors.extend(disallow_attributes(&attributes, &tag_name));
            TagHeader::If(
                tag_expression(
                    expression,
                    opening_range.clone(),
                    ParseErrorKind::MissingIfExpression {},
                    errors,
                )
                .and_then(|e| {
                    let mut iter = e.cursor().peekable();
                    expr::parse_expr::parse_expr(&mut iter, comments, errors, &e)
                }),
            )
        }

        "for" => {
            errors.extend(disallow_attributes(&attributes, &tag_name));
            TagHeader::For(
                tag_expression(
                    expression,
                    opening_range.clone(),
                    ParseErrorKind::MissingForExpression {},
                    errors,
                )
                .and_then(|e| {
                    let mut iter = e.cursor().peekable();
                    parse_loop_header(&mut iter, comments, errors, &e)
                }),
            )
        }

        "let" => {
            errors.extend(disallow_attributes(&attributes, &tag_name));
            TagHeader::Let(
                tag_expression(
                    expression,
                    opening_range.clone(),
                    ParseErrorKind::MissingLetBinding {},
                    errors,
                )
                .and_then(|bindings_range| {
                    let mut iter = bindings_range.cursor().peekable();
                    let parsed = parse_let_bindings(&mut iter, comments, errors, &bindings_range)?;
                    let bindings = parsed
                        .into_iter()
                        .map(
                            |(var_name, var_name_range, var_type, value_expr)| ParsedLetBinding {
                                var_name,
                                var_name_range,
                                var_type,
                                value_expr,
                            },
                        )
                        .collect();
                    Some((bindings, bindings_range))
                }),
            )
        }

        "match" => {
            errors.extend(disallow_attributes(&attributes, &tag_name));
            TagHeader::Match(
                tag_expression(
                    expression,
                    opening_range.clone(),
                    ParseErrorKind::MissingMatchExpression {},
                    errors,
                )
                .and_then(|e| {
                    let mut iter = e.cursor().peekable();
                    expr::parse_expr::parse_expr(&mut iter, comments, errors, &e)
                }),
            )
        }

        "case" => TagHeader::Case(
            tag_expression(
                expression,
                opening_range.clone(),
                ParseErrorKind::MissingCasePattern {},
                errors,
            )
            .and_then(|pattern_range| {
                let mut iter = pattern_range.cursor().peekable();
                expr::parse_expr::parse_match_pattern(&mut iter, comments, errors, &pattern_range)
            }),
        ),

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
            if let Some(expr_range) = expression {
                errors.push(ParseError::new(
                    ParseErrorKind::UnexpectedComponentExpression {
                        tag_name: tag_name.to_cheap_string(),
                    },
                    expr_range,
                ));
            }
            TagHeader::Component {
                name,
                args: parse_attributes(&attributes, comments, errors),
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
                attributes: parse_attributes(&attributes, comments, errors),
            }
        }
    };

    OpenElement {
        tag_name,
        opening_range,
        header,
        children: Vec::new(),
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
            // `<Card/>` and `<Card></Card>` are not the same call: only the
            // second passes a `children` argument, so a `children` parameter
            // keeps its default in the first. The formatter preserves the
            // authored form for the same reason.
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

/// Take the `{...}` off an opening tag, reporting `missing` when there is none.
fn tag_expression(
    expression: Option<DocumentRange>,
    opening_range: DocumentRange,
    missing: ParseErrorKind,
    errors: &mut Vec<ParseError>,
) -> Option<DocumentRange> {
    if expression.is_none() {
        errors.push(ParseError::new(missing, opening_range));
    }
    expression
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
    let start_expr = expr::parse_expr::parse_logical(iter, comments, errors, range)?;
    let source =
        if expr::tokenizer::advance_if(iter, comments, errors, expr::Token::DotDotEq).is_some() {
            let end_expr = expr::parse_expr::parse_logical(iter, comments, errors, range)?;
            ParsedLoopSource::RangeInclusive {
                start: start_expr,
                end: end_expr,
            }
        } else {
            ParsedLoopSource::Array(start_expr)
        };
    expr::tokenizer::expect_eof(iter, comments, errors)?;
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
            let value_expr = expr::parse_expr::parse_logical(iter, comments, errors, range)?;
            Some((var_name, var_name_range, var_type, value_expr))
        },
        None,
    )?;
    expr::tokenizer::expect_eof(iter, comments, errors)?;
    Some(bindings)
}

fn parse_attribute(
    item: &TokenizedAttribute,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<parsed_ast::ParsedAttribute> {
    match item {
        TokenizedAttribute::Named { name, value, .. } => {
            let value = match value {
                Some(TokenizedAttributeValue::String {
                    content,
                    quoted_range,
                }) => Some(parsed_ast::ParsedAttributeValue::String {
                    content: content.clone(),
                    quoted_range: quoted_range.clone(),
                }),
                Some(TokenizedAttributeValue::Expression(range)) => {
                    let mut iter = range.cursor().peekable();
                    let result = expr::parse_expr::parse_expr(&mut iter, comments, errors, range);
                    Some(result.map(parsed_ast::ParsedAttributeValue::Expression)?)
                }
                None => None,
            };
            Some(parsed_ast::ParsedAttribute::Named {
                name: name.clone(),
                value,
            })
        }
        TokenizedAttribute::Spread { name, range } => match VarName::new(name.as_str()) {
            Ok(var_name) => Some(parsed_ast::ParsedAttribute::Spread {
                name: var_name,
                range: range.clone(),
            }),
            Err(error) => {
                errors.push(ParseError::new(
                    ParseErrorKind::InvalidVariableName {
                        name: name.to_cheap_string(),
                        error,
                    },
                    name.clone(),
                ));
                None
            }
        },
    }
}

fn parse_attributes(
    attributes: &[TokenizedAttribute],
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Vec<parsed_ast::ParsedAttribute> {
    attributes
        .iter()
        .filter_map(|item| parse_attribute(item, comments, errors))
        .collect()
}

fn disallow_attributes<'a>(
    attributes: &'a [TokenizedAttribute],
    tag_name: &'a DocumentRange,
) -> impl Iterator<Item = ParseError> + 'a {
    attributes.iter().map(move |item| {
        let (name, range) = (item.name().to_cheap_string(), item.range().clone());
        ParseError::new(
            ParseErrorKind::UnrecognizedAttribute {
                tag_name: tag_name.to_cheap_string(),
                attr_name: name,
            },
            range,
        )
    })
}
