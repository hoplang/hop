use std::collections::VecDeque;
use std::iter::Peekable;

use super::expr_tokenizer;
use super::parse_expr;
use super::parsed_expr::ParsedExpr;
use super::parsed_node::{
    ParsedAttribute, ParsedAttributeValue, ParsedLetBinding, ParsedLoopSource, ParsedMatchCase,
    ParsedNode,
};
use super::token;
use super::tokenizer;
use super::tokenizer::{RawTextToken, TagToken, Token};
use super::whitespace;
use crate::document::{DocumentCursor, DocumentRange};
use crate::hop::parsing::parse_type::parse_type;
use crate::hop::parsing::parsed_expr::ParsedMatchPattern;
use crate::html::{HtmlElement, is_raw_content_tag, is_void_element_tag};
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
    /// The range of the name, or `None` for `</>`.
    tag_name_range: Option<DocumentRange>,
    range: DocumentRange,
}

impl ClosingTag {
    /// The name this tag has to repeat to close an element, or `None` for
    /// `</>`.
    fn name(&self) -> Option<&str> {
        self.tag_name_range.as_ref().map(|range| range.as_str())
    }
}

/// An element whose opening tag has been read but whose closing tag has not.
struct OpenElement {
    /// The range identifying the tag: the name in the opening tag, or the
    /// whole range of a `<>`. E.g.
    /// ```text
    /// <div class="x">
    ///  ^^^
    /// ```
    tag_name_range: DocumentRange,
    /// The range of the opening tag. E.g.
    /// ```text
    /// <div class="x">
    /// ^^^^^^^^^^^^^^^
    /// ```
    opening_range: DocumentRange,
    /// The range of the `{...}` the opening tag carried, if it carried one.
    /// E.g.
    /// ```text
    /// <if {done}>
    ///     ^^^^^^
    /// ```
    expression_range: Option<DocumentRange>,
    /// What was read off the opening tag, waiting for the children.
    header: TagHeader,
    children: Vec<MarkupItem>,
}

impl OpenElement {
    /// The name a closing tag has to repeat to close this element, or `None`
    /// for a `<>`, which is closed by the equally nameless `</>`.
    fn name(&self) -> Option<&str> {
        match self.header {
            TagHeader::Fragment => None,
            _ => Some(self.tag_name_range.as_str()),
        }
    }
}

/// What an opening tag carried, kept until the element can be built.
enum TagHeader {
    /// A `<>`, which carries nothing at all.
    Fragment,
    If {
        cond: Option<ParsedExpr>,
    },
    For {
        expr: Option<LoopHeader>,
    },
    Let {
        bindings: Option<Vec<ParsedLetBinding>>,
    },
    Match {
        expr: Option<ParsedExpr>,
    },
    Case {
        pattern: Option<ParsedMatchPattern>,
    },
    Component {
        name: Option<TypeName>,
        attributes: Vec<ParsedAttribute>,
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
    /// A finished item.
    item: Option<MarkupItem>,
    /// Elements whose opening tag has been read but whose closing tag has
    /// not, outermost first.
    open: Vec<OpenElement>,
}

impl MarkupBuilder {
    /// Add an item to the innermost open element, or to the top level.
    fn append(&mut self, item: MarkupItem) {
        match self.open.last_mut() {
            Some(element) => element.children.push(item),
            None => {
                debug_assert!(self.item.is_none());
                self.item = Some(item);
            }
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

    /// Whether a closing tag with this name would close anything, where
    /// `None` is the name a `</>` repeats.
    fn is_open(&self, tag_name: Option<&str>) -> bool {
        self.open.iter().any(|el| el.name() == tag_name)
    }

    /// Close the element this tag names, along with everything opened inside
    /// it that was never closed.
    ///
    /// Expects `is_open` to hold for the tag.
    fn close(&mut self, closing: ClosingTag, errors: &mut Vec<ParseError>) {
        while self.open.last().unwrap().name() != closing.name() {
            self.close_innermost(errors);
        }
        let element = self.open.pop().unwrap();
        self.append_element(element, Some(closing), errors);
    }

    /// Close the innermost open element, which never got a closing tag.
    fn close_innermost(&mut self, errors: &mut Vec<ParseError>) {
        let element = self.open.pop().unwrap();
        let kind = match element.header {
            TagHeader::Fragment => ParseErrorKind::UnclosedFragment {},
            _ => ParseErrorKind::UnclosedTag {
                tag: element.tag_name_range.to_cheap_string(),
            },
        };
        errors.push(ParseError::new(kind, element.tag_name_range.clone()));
        self.append_element(element, None, errors);
    }

    /// Take the markup, closing anything left open.
    fn finish(mut self, errors: &mut Vec<ParseError>) -> Option<MarkupItem> {
        while !self.open.is_empty() {
            self.close_innermost(errors);
        }
        self.item
    }
}

/// Parse one node, from a token that has already been lexed.
///
/// Returns `None` when the token neither built a node nor opened an element.
///
/// We do our best here to build as much markup as possible even when we
/// encounter errors.
fn parse_node(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    first: Token,
) -> Option<ParsedNode> {
    let mut builder = MarkupBuilder::default();
    let mut token = first;

    loop {
        match token {
            Token::Text { range } => builder.append_node(ParsedNode::Text { range }),
            Token::Newline { range } => builder.append_node(ParsedNode::Newline { range }),
            Token::Comment { range } => builder.append_node(ParsedNode::Comment { range }),

            Token::ExpressionStart { left_brace } => {
                if let Some(expression) =
                    parse_expr::parse_expr(iter, comments, errors, &left_brace)
                    && let Some(right_brace) = expr_tokenizer::expect_opposite(
                        iter,
                        comments,
                        errors,
                        &token::Token::LeftBrace,
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
                let (element, end) = parse_opening_tag(tag_name, range, iter, comments, errors);
                match end {
                    TagEnd::Open => builder.enter(element),
                    TagEnd::Closed => builder.append_element(element, None, errors),
                }
            }

            Token::ClosingTag { tag_name, range } => {
                if is_void_element_tag(tag_name.as_str()) {
                    errors.push(ParseError::new(
                        ParseErrorKind::ClosedVoidTag {
                            tag: tag_name.to_cheap_string(),
                        },
                        range,
                    ));
                } else if !builder.is_open(Some(tag_name.as_str())) {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnmatchedClosingTag {
                            tag: tag_name.to_cheap_string(),
                        },
                        range,
                    ));
                } else {
                    builder.close(
                        ClosingTag {
                            tag_name_range: Some(tag_name),
                            range,
                        },
                        errors,
                    );
                }
            }

            Token::FragmentStart { range } => builder.enter(OpenElement {
                tag_name_range: range.clone(),
                opening_range: range,
                expression_range: None,
                header: TagHeader::Fragment,
                children: Vec::new(),
            }),

            Token::FragmentEnd { range } => {
                if builder.is_open(None) {
                    builder.close(
                        ClosingTag {
                            tag_name_range: None,
                            range,
                        },
                        errors,
                    );
                } else {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnmatchedClosingFragment {},
                        range,
                    ));
                }
            }
        }

        if let Some(item) = builder.item.take() {
            return expect_node(item, errors);
        }
        if builder.open.is_empty() {
            return None;
        }
        match tokenizer::next(iter, errors) {
            Some(next) => token = next,
            None => {
                return builder
                    .finish(errors)
                    .and_then(|item| expect_node(item, errors));
            }
        }
    }
}

/// Parse markup for a body.
/// A body that has no root, or more than one, is an error.
pub fn parse_body(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    left_brace: &DocumentRange,
) -> ParsedExpr {
    let errors_before = errors.len();
    let mut nodes = Vec::new();
    while let Some(token) = tokenizer::next(iter, errors) {
        nodes.extend(parse_node(iter, comments, errors, token));
    }
    whitespace::normalize(&mut nodes);
    if nodes.len() == 1 {
        let body = nodes.pop().unwrap();
        return ParsedExpr::Markup {
            node: Box::new(body),
        };
    }
    let (kind, error_range) = match nodes.get(1) {
        Some(second) => (ParseErrorKind::MultipleRoots {}, second.range().clone()),
        None => (ParseErrorKind::EmptyBody {}, left_brace.clone()),
    };
    if !nodes.is_empty() || errors.len() == errors_before {
        errors.push(ParseError::new(kind, error_range));
    }
    let range = match (nodes.first(), nodes.last()) {
        (Some(first), Some(last)) => first.range().clone().to(last.range().clone()),
        _ => left_brace.clone(),
    };

    ParsedExpr::Markup {
        node: Box::new(ParsedNode::Fragment {
            children: nodes,
            range,
        }),
    }
}

/// Where an opening tag left the parse.
enum TagEnd {
    /// Children follow, then a closing tag.
    Open,
    /// The element is finished as it stands.
    Closed,
}

/// Parse an opening tag from just after its name, and say whether children
/// follow it.
fn parse_opening_tag(
    tag_name_range: DocumentRange,
    tag_start_range: DocumentRange,
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> (OpenElement, TagEnd) {
    let mut header = match tag_name_range.as_str() {
        "if" => TagHeader::If { cond: None },
        "for" => TagHeader::For { expr: None },
        "let" => TagHeader::Let { bindings: None },
        "match" => TagHeader::Match { expr: None },
        "case" => TagHeader::Case { pattern: None },
        name if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
            TagHeader::Component {
                attributes: Vec::new(),
                name: {
                    match TypeName::new(name) {
                        Ok(name) => Some(name),
                        Err(error) => {
                            errors.push(ParseError::new(
                                ParseErrorKind::InvalidTypeName { error },
                                tag_name_range.clone(),
                            ));
                            None
                        }
                    }
                },
            }
        }
        _ => TagHeader::Html {
            attributes: Vec::new(),
            element: {
                let element = HtmlElement::parse(tag_name_range.as_str());
                if element.is_none() {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnknownHtmlElement {
                            tag: tag_name_range.to_cheap_string(),
                        },
                        tag_name_range.clone(),
                    ));
                }
                element
            },
        },
    };
    let mut self_closing = false;
    let mut full_range = tag_start_range.clone();
    let mut expression_range: Option<DocumentRange> = None;

    loop {
        let Some(part) = tokenizer::next_tag_token(iter, errors) else {
            errors.push(ParseError::new(
                ParseErrorKind::UnterminatedOpeningTag {},
                tag_name_range.clone(),
            ));
            break;
        };
        match part {
            TagToken::End { range } => {
                full_range = tag_start_range.clone().to(range);
                break;
            }

            TagToken::SelfClosingEnd { range } => {
                self_closing = true;
                full_range = tag_start_range.clone().to(range);
                break;
            }

            TagToken::Attribute { name, value } => {
                let value = value.map(|value| ParsedAttributeValue::String {
                    content: value.content,
                    quoted_range: value.quoted_range,
                });
                push_attribute(
                    &mut header,
                    &tag_name_range,
                    ParsedAttribute::Named { name, value },
                    errors,
                );
            }

            TagToken::AttributeExpressionStart { name, left_brace } => {
                if let Some(value) = parse_expr::parse_expr(iter, comments, errors, &left_brace)
                    && expr_tokenizer::expect_opposite(
                        iter,
                        comments,
                        errors,
                        &token::Token::LeftBrace,
                        &left_brace,
                    )
                    .is_some()
                {
                    push_attribute(
                        &mut header,
                        &tag_name_range,
                        ParsedAttribute::Named {
                            name,
                            value: Some(ParsedAttributeValue::Expression(value)),
                        },
                        errors,
                    );
                }
            }

            TagToken::Spread { name, range } => match VarName::new(name.as_str()) {
                Ok(var_name) => push_attribute(
                    &mut header,
                    &tag_name_range,
                    ParsedAttribute::Spread {
                        name: var_name,
                        range,
                    },
                    errors,
                ),
                Err(error) => errors.push(ParseError::new(
                    ParseErrorKind::InvalidVariableName {
                        name: name.to_cheap_string(),
                        error,
                    },
                    name,
                )),
            },

            TagToken::ExpressionStart { left_brace } => {
                let parse_succeeded = match &mut header {
                    TagHeader::If { cond: expr } | TagHeader::Match { expr } => {
                        *expr = parse_expr::parse_expr(iter, comments, errors, &left_brace);
                        expr.is_some()
                    }

                    TagHeader::For { expr } => {
                        *expr = parse_loop_header(iter, comments, errors, &left_brace);
                        expr.is_some()
                    }

                    TagHeader::Case { pattern } => {
                        *pattern =
                            parse_expr::parse_match_pattern(iter, comments, errors, &left_brace);
                        pattern.is_some()
                    }

                    TagHeader::Let { bindings } => {
                        *bindings = parse_let_bindings(iter, comments, errors, &left_brace);
                        bindings.is_some()
                    }

                    TagHeader::Component { .. } | TagHeader::Html { .. } => {
                        parse_expr::parse_expr(iter, comments, errors, &left_brace).is_some()
                    }

                    TagHeader::Fragment => unreachable!(),
                };
                expression_range = Some(left_brace.clone());
                if parse_succeeded {
                    let right_brace = expr_tokenizer::expect_opposite(
                        iter,
                        comments,
                        errors,
                        &token::Token::LeftBrace,
                        &left_brace,
                    );
                    if let Some(right_brace) = right_brace {
                        expression_range = Some(left_brace.to(right_brace));
                    }
                }
            }
        }
    }

    let error = match (&header, &expression_range) {
        (TagHeader::If { .. }, None) => Some((ParseErrorKind::MissingIfExpression {}, &full_range)),
        (TagHeader::For { .. }, None) => {
            Some((ParseErrorKind::MissingForExpression {}, &full_range))
        }
        (TagHeader::Let { .. }, None) => Some((ParseErrorKind::MissingLetBinding {}, &full_range)),
        (TagHeader::Match { .. }, None) => {
            Some((ParseErrorKind::MissingMatchExpression {}, &full_range))
        }
        (TagHeader::Case { .. }, None) => {
            Some((ParseErrorKind::MissingCasePattern {}, &full_range))
        }
        (TagHeader::Component { .. }, Some(range)) => Some((
            ParseErrorKind::UnexpectedComponentExpression {
                tag_name: tag_name_range.to_cheap_string(),
            },
            range,
        )),
        _ => None,
    };
    if let Some((kind, range)) = error {
        errors.push(ParseError::new(kind, range.clone()));
    }

    // A raw text element holds text rather than markup, so its content and
    // closing tag are read here.
    let raw_text = !self_closing && is_raw_content_tag(tag_name_range.as_str());
    let mut children = Vec::new();
    if raw_text {
        match tokenizer::next_raw_text_token(iter, &tag_name_range) {
            Some(RawTextToken {
                content,
                closing_tag_end,
            }) => {
                children.extend(content.map(|range| MarkupItem::Node(ParsedNode::Text { range })));
                full_range = tag_start_range.to(closing_tag_end);
            }
            None => errors.push(ParseError::new(
                ParseErrorKind::UnterminatedOpeningTag {},
                tag_name_range.clone(),
            )),
        }
    }

    let end = if raw_text || self_closing || is_void_element_tag(tag_name_range.as_str()) {
        TagEnd::Closed
    } else {
        TagEnd::Open
    };
    (
        OpenElement {
            tag_name_range,
            opening_range: full_range,
            expression_range,
            header,
            children,
        },
        end,
    )
}

/// Add an attribute to the tag it was written on, rejecting one on a tag
/// that takes none and a name the tag already has.
fn push_attribute(
    header: &mut TagHeader,
    tag_name: &DocumentRange,
    attribute: ParsedAttribute,
    errors: &mut Vec<ParseError>,
) {
    let (TagHeader::Component { attributes, .. } | TagHeader::Html { attributes, .. }) = header
    else {
        let (attr_name, range) = match &attribute {
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
        return;
    };
    if let ParsedAttribute::Named { name, .. } = &attribute
        && attributes.iter().any(|existing| match existing {
            ParsedAttribute::Named { name: existing, .. } => existing.as_str() == name.as_str(),
            ParsedAttribute::Spread { .. } => false,
        })
    {
        errors.push(ParseError::new(
            ParseErrorKind::DuplicateAttribute {
                name: name.to_cheap_string(),
            },
            name.clone(),
        ));
        return;
    }
    attributes.push(attribute);
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
        tag_name_range,
        opening_range,
        expression_range,
        header,
        children,
    } = element;
    // A `</>` only ever closes a fragment, which has no name to record, so
    // flattening the two levels of `Option` loses nothing.
    let (closing_tag_name, range) = match closing {
        Some(closing) => (closing.tag_name_range, opening_range.to(closing.range)),
        None => (None, opening_range),
    };

    match header {
        TagHeader::Fragment => Some(MarkupItem::Node(ParsedNode::Fragment {
            children: expect_nodes(children, errors),
            range,
        })),

        TagHeader::If { cond: condition } => {
            let children = expect_nodes(children, errors);
            condition.map(|condition| {
                MarkupItem::Node(ParsedNode::If {
                    condition,
                    range,
                    children,
                })
            })
        }

        TagHeader::For { expr: header } => {
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

        TagHeader::Let { bindings } => {
            let children = expect_nodes(children, errors);
            bindings
                .zip(expression_range)
                .map(|(bindings, bindings_range)| {
                    MarkupItem::Node(ParsedNode::Let {
                        bindings,
                        bindings_range,
                        range,
                        children,
                    })
                })
        }

        TagHeader::Match { expr: subject } => {
            let cases = expect_cases(children, errors);
            subject.map(|subject| {
                MarkupItem::Node(ParsedNode::Match {
                    subject,
                    cases,
                    range,
                })
            })
        }

        TagHeader::Case { pattern } => {
            let children = expect_nodes(children, errors);
            pattern.map(|pattern| MarkupItem::Case {
                case: ParsedMatchCase { pattern, children },
                tag_name_range,
            })
        }

        TagHeader::Component { name, attributes } => {
            let children = expect_nodes(children, errors);
            let children = closing_tag_name.is_some().then_some(children);
            name.map(|component_name| {
                MarkupItem::Node(ParsedNode::ComponentInvocation {
                    component_name,
                    component_name_opening_range: tag_name_range,
                    component_name_closing_range: closing_tag_name,
                    attributes,
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
                    tag_name: tag_name_range,
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
    items
        .into_iter()
        .filter_map(|item| expect_node(item, errors))
        .collect()
}

/// Take the node out of a markup item.
fn expect_node(item: MarkupItem, errors: &mut Vec<ParseError>) -> Option<ParsedNode> {
    match item {
        MarkupItem::Node(node) => Some(node),
        MarkupItem::Case {
            tag_name_range: tag_name,
            ..
        } => {
            errors.push(ParseError::new(
                ParseErrorKind::CaseOutsideMatch {},
                tag_name,
            ));
            None
        }
    }
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

struct LoopHeader {
    var_name: Option<VarName>,
    var_name_range: Option<DocumentRange>,
    loop_source: Box<ParsedLoopSource>,
}

fn parse_loop_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<LoopHeader> {
    let (var_name, var_name_range) = if let Some(underscore_range) =
        expr_tokenizer::advance_if(iter, comments, errors, token::Token::Underscore)
    {
        (None, Some(underscore_range))
    } else {
        let (name, name_range) =
            expr_tokenizer::expect_variable_name(iter, comments, errors, range)?;
        (Some(name), Some(name_range))
    };
    expr_tokenizer::expect_token(iter, comments, errors, range, &token::Token::In)?;
    let start_expr = parse_expr::parse_expr(iter, comments, errors, range)?;
    let source =
        if expr_tokenizer::advance_if(iter, comments, errors, token::Token::DotDotEq).is_some() {
            let end_expr = parse_expr::parse_expr(iter, comments, errors, range)?;
            ParsedLoopSource::RangeInclusive {
                start: start_expr,
                end: end_expr,
            }
        } else {
            ParsedLoopSource::Array(start_expr)
        };
    Some(LoopHeader {
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
) -> Option<Vec<ParsedLetBinding>> {
    let bindings = expr_tokenizer::parse_comma_separated(
        iter,
        comments,
        errors,
        range,
        |iter, comments, errors, range| {
            let (var_name, var_name_range) =
                expr_tokenizer::expect_variable_name(iter, comments, errors, range)?;
            let var_type = if let Some((token::Token::Colon, _)) =
                expr_tokenizer::peek_past_comments(iter)
            {
                expr_tokenizer::expect_token(iter, comments, errors, range, &token::Token::Colon)?;
                Some(parse_type(iter, comments, errors, range)?)
            } else {
                None
            };
            expr_tokenizer::expect_token(iter, comments, errors, range, &token::Token::Assign)?;
            let value_expr = parse_expr::parse_expr(iter, comments, errors, range)?;
            Some(ParsedLetBinding {
                var_name,
                var_name_range,
                var_type,
                value_expr,
            })
        },
        Some(&token::Token::RightBrace),
    )?;
    Some(bindings)
}
