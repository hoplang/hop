use std::collections::VecDeque;
use std::iter::Peekable;

use super::parse_expr;
use super::parse_helpers;
use super::parsed_expr::ParsedExpr;
use super::parsed_node::{
    ParsedAttribute, ParsedLetBinding, ParsedLoopSource, ParsedMatchCase, ParsedNode,
};
use super::token;
use super::tokenize_expr;
use super::tokenize_markup;
use super::whitespace;
use crate::document::{DocumentCursor, DocumentRange};
use crate::hop::parsing::parse_type::parse_type;
use crate::hop::parsing::parsed_expr::ParsedMatchPattern;
use crate::hop::parsing::token::LangTokenPair;
use crate::hop::parsing::token::MarkupToken;
use crate::hop::parsing::token::RawTextToken;
use crate::hop::parsing::token::TagToken;
use crate::html::{HtmlElementKind, is_raw_content_tag, is_void_element_tag};
use crate::parse_error::{ErrorEmitted, ParseErrorKind, ParseErrors};
use crate::symbols::function_name::FunctionName;
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
    /// What was read off the opening tag, waiting for the children.
    header: ElementHeader,
    children: Vec<MarkupItem>,
}

impl OpenElement {
    /// The name a closing tag has to repeat to close this element, or `None`
    /// for a `<>`, which is closed by the equally nameless `</>`.
    fn name(&self) -> Option<&str> {
        match self.header {
            ElementHeader::Fragment => None,
            ElementHeader::Tag(_) => Some(self.tag_name_range.as_str()),
        }
    }

    /// Build the element without a closing tag, reporting that it never got
    /// one.
    fn close_unclosed(self, errors: &mut ParseErrors) -> Result<MarkupItem, ErrorEmitted> {
        let kind = match self.header {
            ElementHeader::Fragment => ParseErrorKind::UnclosedFragment {},
            ElementHeader::Tag(_) => ParseErrorKind::UnclosedTag {
                tag: self.tag_name_range.to_cheap_string(),
            },
        };
        let _ = errors.emit(kind, self.tag_name_range.clone());
        close_element(self, None, errors)
    }
}

/// What an opening tag carried, kept until the element can be built.
#[allow(clippy::large_enum_variant)]
enum ElementHeader {
    /// A `<>`, which carries nothing at all.
    Fragment,
    /// A named tag, and what was read off it.
    Tag(TagHeader),
}

/// What a named opening tag carried.
///
/// Every tag has a slot for a `{...}`, whether or not it takes one, so that
/// one written on a tag that takes none is kept until the element is built
/// and rejected there. E.g.
/// ```text
/// <if {done}>
///     ^^^^^^
/// ```
enum TagHeader {
    If {
        cond: Slot<ParsedExpr>,
    },
    For {
        expr: Slot<LoopHeader>,
    },
    Let {
        bindings: Slot<Vec<ParsedLetBinding>>,
    },
    Match {
        expr: Slot<ParsedExpr>,
    },
    Case {
        pattern: Slot<ParsedMatchPattern>,
    },
    Function {
        name: Result<FunctionName, ErrorEmitted>,
        attributes: Vec<ParsedAttribute>,
        expression: Slot<ParsedExpr>,
    },
    Html {
        element: Result<HtmlElementKind, ErrorEmitted>,
        attributes: Vec<ParsedAttribute>,
        expression: Slot<ParsedExpr>,
    },
}

/// The braced part of a tag: not seen, parsed, or failed to parse. A seen
/// one carries the range of the braces, or of just the `{` when what was
/// inside could not be parsed.
struct Slot<T>(Option<(DocumentRange, Result<T, ErrorEmitted>)>);

impl<T> Slot<T> {
    fn empty() -> Self {
        Slot(None)
    }

    /// Put a `{...}` in the slot. If the tag already carries one, the new
    /// one is reported and the first kept.
    fn fill(
        &mut self,
        parsed: Result<(T, DocumentRange), ErrorEmitted>,
        left_brace: DocumentRange,
        tag_name_range: &DocumentRange,
        errors: &mut ParseErrors,
    ) {
        let (range, value) = match parsed {
            Ok((value, braces)) => (braces, Ok(value)),
            Err(reported) => (left_brace, Err(reported)),
        };
        if self.0.is_some() {
            let _ = errors.emit(
                ParseErrorKind::DuplicateTagExpression {
                    tag_name: tag_name_range.to_cheap_string(),
                },
                range,
            );
            return;
        }
        self.0 = Some((range, value));
    }

    /// Take what the tag has to carry, with the range of its braces.
    /// Reports `missing` at `range` when the tag carries nothing.
    fn require(
        self,
        missing: ParseErrorKind,
        range: &DocumentRange,
        errors: &mut ParseErrors,
    ) -> Result<(T, DocumentRange), ErrorEmitted> {
        match self.0 {
            Some((braces, value)) => Ok((value?, braces)),
            None => Err(errors.emit(missing, range.clone())),
        }
    }

    /// Reject whatever the tag carries, since it takes nothing.
    fn reject(
        self,
        tag_name_range: &DocumentRange,
        errors: &mut ParseErrors,
    ) -> Result<(), ErrorEmitted> {
        match self.0 {
            Some((range, _)) => Err(errors.emit(
                ParseErrorKind::UnexpectedTagExpression {
                    tag_name: tag_name_range.to_cheap_string(),
                },
                range,
            )),
            None => Ok(()),
        }
    }
}

/// The markup built so far.
///
/// An item lands in the innermost element still open. With nothing open it
/// is the finished markup, which every step that could finish it returns.
#[derive(Default)]
struct MarkupBuilder {
    /// Elements whose opening tag has been read but whose closing tag has
    /// not, outermost first.
    open: Vec<OpenElement>,
}

impl MarkupBuilder {
    /// Add an item to the innermost open element, or return it as the
    /// finished markup when nothing is open.
    fn append(
        &mut self,
        item: Result<MarkupItem, ErrorEmitted>,
    ) -> Option<Result<MarkupItem, ErrorEmitted>> {
        match self.open.last_mut() {
            // A dropped child is just missing from its parent.
            Some(element) => {
                element.children.extend(item.ok());
                None
            }
            None => Some(item),
        }
    }

    /// Add a node to the innermost open element, or to the top level.
    fn append_node(&mut self, node: ParsedNode) -> Option<Result<MarkupItem, ErrorEmitted>> {
        self.append(Ok(MarkupItem::Node(node)))
    }

    /// Drop an item that could not be built, keeping the proof of why.
    fn drop_item(&mut self, guar: ErrorEmitted) -> Option<Result<MarkupItem, ErrorEmitted>> {
        self.append(Err(guar))
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
        errors: &mut ParseErrors,
    ) -> Option<Result<MarkupItem, ErrorEmitted>> {
        self.append(close_element(element, closing, errors))
    }

    /// Close the element this tag names, along with everything opened inside
    /// it that was never closed. A tag that names nothing open is reported
    /// and dropped.
    fn close(
        &mut self,
        closing: ClosingTag,
        errors: &mut ParseErrors,
    ) -> Option<Result<MarkupItem, ErrorEmitted>> {
        let Some(depth) = self
            .open
            .iter()
            .rposition(|element| element.name() == closing.name())
        else {
            let reported = match closing.tag_name_range {
                Some(tag_name) => errors.emit(
                    ParseErrorKind::UnmatchedClosingTag {
                        tag: tag_name.to_cheap_string(),
                    },
                    closing.range,
                ),
                None => errors.emit(ParseErrorKind::UnmatchedClosingFragment {}, closing.range),
            };
            return self.drop_item(reported);
        };
        let mut unclosed = self.open.split_off(depth + 1);
        let mut element = self.open.pop().expect("`depth` indexes an open element");
        // Innermost first, each unclosed element lands in the one around it,
        // and the outermost of them in the element being closed.
        while let Some(inner) = unclosed.pop() {
            let item = inner.close_unclosed(errors);
            unclosed
                .last_mut()
                .unwrap_or(&mut element)
                .children
                .extend(item.ok());
        }
        self.append_element(element, Some(closing), errors)
    }

    /// Take the markup, closing everything left open. Something is open:
    /// the markup would otherwise have finished with its last item.
    fn finish(mut self, errors: &mut ParseErrors) -> Result<MarkupItem, ErrorEmitted> {
        loop {
            let element = self.open.pop().expect("an element is open");
            if let Some(item) = self.append(element.close_unclosed(errors)) {
                return item;
            }
        }
    }
}

/// Parse one node, from a `<` the caller has already consumed.
///
/// Fails when nothing there built a node.
///
/// We do our best here to build as much markup as possible even when we
/// encounter errors.
fn parse_node(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    left_angle: DocumentRange,
) -> Result<ParsedNode, ErrorEmitted> {
    let mut builder = MarkupBuilder::default();
    let mut token = tokenize_markup::lex_tag(iter, errors, left_angle)?;

    loop {
        let finished = match token {
            MarkupToken::Text { range } => builder.append_node(ParsedNode::Text { range }),
            MarkupToken::Newline { range } => builder.append_node(ParsedNode::Newline { range }),
            MarkupToken::Comment { range } => builder.append_node(ParsedNode::Comment { range }),

            MarkupToken::ExpressionStart { left_brace } => {
                match parse_helpers::parse_delimited(
                    iter,
                    comments,
                    errors,
                    &left_brace,
                    LangTokenPair::Braces,
                    &left_brace,
                    parse_expr::parse_expr,
                ) {
                    Ok((expression, range)) => {
                        builder.append_node(ParsedNode::Interpolation { expression, range })
                    }
                    Err(guar) => builder.drop_item(guar),
                }
            }

            MarkupToken::OpeningTagStart { tag_name, range } => {
                let (element, end) = parse_opening_tag(tag_name, range, iter, comments, errors);
                match end {
                    TagEnd::Open => {
                        builder.enter(element);
                        None
                    }
                    TagEnd::Closed => builder.append_element(element, None, errors),
                }
            }

            MarkupToken::ClosingTag { tag_name, range } => {
                if is_void_element_tag(tag_name.as_str()) {
                    builder.drop_item(errors.emit(
                        ParseErrorKind::ClosedVoidTag {
                            tag: tag_name.to_cheap_string(),
                        },
                        range,
                    ))
                } else {
                    builder.close(
                        ClosingTag {
                            tag_name_range: Some(tag_name),
                            range,
                        },
                        errors,
                    )
                }
            }

            MarkupToken::FragmentStart { range } => {
                builder.enter(OpenElement {
                    tag_name_range: range.clone(),
                    opening_range: range,
                    header: ElementHeader::Fragment,
                    children: Vec::new(),
                });
                None
            }

            MarkupToken::FragmentEnd { range } => builder.close(
                ClosingTag {
                    tag_name_range: None,
                    range,
                },
                errors,
            ),
        };

        if let Some(item) = finished {
            return expect_node(item?, errors);
        }
        match tokenize_markup::next(iter, errors) {
            Some(next) => token = next,
            None => break,
        }
    }
    expect_node(builder.finish(errors)?, errors)
}

/// Parse markup in expression position, from a '<' the caller has
/// already consumed.
pub fn parse_markup(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    left_angle: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut node = parse_node(iter, comments, errors, left_angle)?;
    whitespace::normalize_node(&mut node);
    Ok(ParsedExpr::Markup {
        node: Box::new(node),
    })
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
    errors: &mut ParseErrors,
) -> (OpenElement, TagEnd) {
    let mut header = match tag_name_range.as_str() {
        "if" => TagHeader::If {
            cond: Slot::empty(),
        },
        "for" => TagHeader::For {
            expr: Slot::empty(),
        },
        "let" => TagHeader::Let {
            bindings: Slot::empty(),
        },
        "match" => TagHeader::Match {
            expr: Slot::empty(),
        },
        "case" => TagHeader::Case {
            pattern: Slot::empty(),
        },
        name if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
            TagHeader::Function {
                attributes: Vec::new(),
                expression: Slot::empty(),
                name: FunctionName::new(name).map_err(|error| {
                    errors.emit(
                        ParseErrorKind::InvalidFunctionName { error },
                        tag_name_range.clone(),
                    )
                }),
            }
        }
        _ => TagHeader::Html {
            attributes: Vec::new(),
            expression: Slot::empty(),
            element: HtmlElementKind::parse(tag_name_range.as_str()).ok_or_else(|| {
                errors.emit(
                    ParseErrorKind::UnknownHtmlElement {
                        tag: tag_name_range.to_cheap_string(),
                    },
                    tag_name_range.clone(),
                )
            }),
        },
    };
    let mut self_closing = false;
    let mut full_range = tag_start_range.clone();

    // The loop ends at an `Err`: the tag ended without a `>`, which was
    // reported.
    while let Ok(part) = tokenize_markup::next_tag_token(iter, errors, &tag_name_range) {
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
                let attribute = match value {
                    Some(value) => ParsedAttribute::String {
                        name,
                        content: value.content_range,
                        quoted_range: value.quoted_range,
                    },
                    None => ParsedAttribute::KeyOnly { name },
                };
                push_attribute(&mut header, &tag_name_range, attribute, errors);
            }

            TagToken::AttributeExpressionStart { name, left_brace } => {
                if let Ok((value, _)) = parse_helpers::parse_delimited(
                    iter,
                    comments,
                    errors,
                    &left_brace,
                    LangTokenPair::Braces,
                    &left_brace,
                    parse_expr::parse_expr,
                ) {
                    push_attribute(
                        &mut header,
                        &tag_name_range,
                        ParsedAttribute::Expression { name, value },
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
                Err(error) => {
                    let _ = errors.emit(
                        ParseErrorKind::InvalidVariableName {
                            name: name.to_cheap_string(),
                            error,
                        },
                        name,
                    );
                }
            },

            TagToken::ExpressionStart { left_brace } => match &mut header {
                TagHeader::If { cond: slot }
                | TagHeader::Match { expr: slot }
                | TagHeader::Function {
                    expression: slot, ..
                }
                | TagHeader::Html {
                    expression: slot, ..
                } => {
                    let parsed = parse_helpers::parse_delimited(
                        iter,
                        comments,
                        errors,
                        &left_brace,
                        LangTokenPair::Braces,
                        &left_brace,
                        parse_expr::parse_expr,
                    );
                    slot.fill(parsed, left_brace, &tag_name_range, errors);
                }

                TagHeader::For { expr: slot } => {
                    let parsed = parse_helpers::parse_delimited(
                        iter,
                        comments,
                        errors,
                        &left_brace,
                        LangTokenPair::Braces,
                        &left_brace,
                        parse_loop_header,
                    );
                    slot.fill(parsed, left_brace, &tag_name_range, errors);
                }

                TagHeader::Case { pattern: slot } => {
                    let parsed = parse_helpers::parse_delimited(
                        iter,
                        comments,
                        errors,
                        &left_brace,
                        LangTokenPair::Braces,
                        &left_brace,
                        parse_expr::parse_match_pattern,
                    );
                    slot.fill(parsed, left_brace, &tag_name_range, errors);
                }

                TagHeader::Let { bindings: slot } => {
                    let parsed = parse_let_bindings(iter, comments, errors, &left_brace);
                    slot.fill(parsed, left_brace, &tag_name_range, errors);
                }
            },
        }
    }

    // A raw text element holds text rather than markup, so its content and
    // closing tag are read here.
    let raw_text = !self_closing && is_raw_content_tag(tag_name_range.as_str());
    let mut children = Vec::new();
    let mut closed = self_closing || is_void_element_tag(tag_name_range.as_str());
    if raw_text {
        let RawTextToken {
            content,
            closing_tag_end,
        } = tokenize_markup::next_raw_text_token(iter, &tag_name_range);
        children.extend(content.map(|range| MarkupItem::Node(ParsedNode::Text { range })));
        // Without a closing tag the element stays open, and is reported as
        // unclosed with everything else still open when the markup ends.
        if let Some(closing_tag_end) = closing_tag_end {
            full_range = tag_start_range.to(closing_tag_end);
            closed = true;
        }
    }

    let end = if closed { TagEnd::Closed } else { TagEnd::Open };
    (
        OpenElement {
            tag_name_range,
            opening_range: full_range,
            header: ElementHeader::Tag(header),
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
    errors: &mut ParseErrors,
) {
    let (TagHeader::Function { attributes, .. } | TagHeader::Html { attributes, .. }) = header
    else {
        let (attr_name, range) = match &attribute {
            ParsedAttribute::KeyOnly { name }
            | ParsedAttribute::Expression { name, .. }
            | ParsedAttribute::String { name, .. } => (name.to_cheap_string(), name.clone()),
            ParsedAttribute::Spread { range, .. } => (range.to_cheap_string(), range.clone()),
        };
        let _ = errors.emit(
            ParseErrorKind::UnrecognizedAttribute {
                tag_name: tag_name.to_cheap_string(),
                attr_name,
            },
            range,
        );
        return;
    };
    if let Some(name) = attribute.name_range()
        && attributes.iter().any(|existing| {
            existing
                .name_range()
                .is_some_and(|existing| existing.as_str() == name.as_str())
        })
    {
        let _ = errors.emit(
            ParseErrorKind::DuplicateAttribute {
                name: name.to_cheap_string(),
            },
            name.clone(),
        );
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
    errors: &mut ParseErrors,
) -> Result<MarkupItem, ErrorEmitted> {
    let OpenElement {
        tag_name_range,
        opening_range,
        header,
        children,
    } = element;
    // A `</>` only ever closes a fragment, which has no name to record, so
    // flattening the two levels of `Option` loses nothing.
    let (closing_tag_name, range) = match closing {
        Some(closing) => (
            closing.tag_name_range,
            opening_range.clone().to(closing.range),
        ),
        None => (None, opening_range.clone()),
    };

    // Children are checked before the header so that their errors are
    // reported even when the element itself is dropped.
    let header = match header {
        ElementHeader::Fragment => {
            return Ok(MarkupItem::Node(ParsedNode::Fragment {
                children: expect_nodes(children, errors),
                range,
            }));
        }
        ElementHeader::Tag(header) => header,
    };
    match header {
        TagHeader::If { cond } => {
            let children = expect_nodes(children, errors);
            let (condition, _) = cond.require(
                ParseErrorKind::MissingIfExpression {},
                &opening_range,
                errors,
            )?;
            Ok(MarkupItem::Node(ParsedNode::If {
                condition,
                range,
                children,
            }))
        }

        TagHeader::For { expr } => {
            let children = expect_nodes(children, errors);
            let (header, _) = expr.require(
                ParseErrorKind::MissingForExpression {},
                &opening_range,
                errors,
            )?;
            Ok(MarkupItem::Node(ParsedNode::For {
                var_name: header.var_name,
                var_name_range: header.var_name_range,
                source: header.loop_source,
                range,
                children,
            }))
        }

        TagHeader::Let { bindings } => {
            let children = expect_nodes(children, errors);
            let (bindings, bindings_range) =
                bindings.require(ParseErrorKind::MissingLetBinding {}, &opening_range, errors)?;
            Ok(MarkupItem::Node(ParsedNode::Let {
                bindings,
                bindings_range,
                range,
                children,
            }))
        }

        TagHeader::Match { expr } => {
            let cases = expect_cases(children, errors);
            let (subject, _) = expr.require(
                ParseErrorKind::MissingMatchExpression {},
                &opening_range,
                errors,
            )?;
            Ok(MarkupItem::Node(ParsedNode::Match {
                subject,
                cases,
                range,
            }))
        }

        TagHeader::Case { pattern } => {
            let children = expect_nodes(children, errors);
            let (pattern, _) = pattern.require(
                ParseErrorKind::MissingCasePattern {},
                &opening_range,
                errors,
            )?;
            Ok(MarkupItem::Case {
                case: ParsedMatchCase { pattern, children },
                tag_name_range,
            })
        }

        TagHeader::Function {
            name,
            attributes,
            expression,
        } => {
            let children = expect_nodes(children, errors);
            let children = closing_tag_name.is_some().then_some(children);
            expression.reject(&tag_name_range, errors)?;
            Ok(MarkupItem::Node(ParsedNode::FunctionInvocation {
                function_name: name?,
                function_name_opening_range: tag_name_range,
                function_name_closing_range: closing_tag_name,
                attributes,
                range,
                children,
            }))
        }

        TagHeader::Html {
            element,
            attributes,
            expression,
        } => {
            let children = expect_nodes(children, errors);
            expression.reject(&tag_name_range, errors)?;
            Ok(MarkupItem::Node(ParsedNode::HtmlElement {
                kind: element?,
                tag_name: tag_name_range,
                closing_tag_name,
                attributes,
                range,
                children,
            }))
        }
    }
}

/// Take the nodes out of a markup sequence.
///
/// A `<case>` here is not inside a `<match>`, which is the only place it
/// means anything.
fn expect_nodes(items: Vec<MarkupItem>, errors: &mut ParseErrors) -> Vec<ParsedNode> {
    items
        .into_iter()
        .filter_map(|item| expect_node(item, errors).ok())
        .collect()
}

/// Take the node out of a markup item.
fn expect_node(item: MarkupItem, errors: &mut ParseErrors) -> Result<ParsedNode, ErrorEmitted> {
    match item {
        MarkupItem::Node(node) => Ok(node),
        MarkupItem::Case {
            tag_name_range: tag_name,
            ..
        } => Err(errors.emit(ParseErrorKind::CaseOutsideMatch {}, tag_name)),
    }
}

/// Take the cases out of the body of a `<match>`.
///
/// Layout between the cases is dropped, and anything else is rejected.
fn expect_cases(items: Vec<MarkupItem>, errors: &mut ParseErrors) -> Vec<ParsedMatchCase> {
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
            node => {
                let _ = errors.emit(ParseErrorKind::InvalidMatchChild {}, node.range().clone());
            }
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
    errors: &mut ParseErrors,
    range: &DocumentRange,
) -> Result<LoopHeader, ErrorEmitted> {
    let (var_name, var_name_range) = if let Some(underscore_range) =
        parse_helpers::advance_if(iter, comments, errors, token::LangToken::Underscore)
    {
        (None, Some(underscore_range))
    } else {
        let (name, name_range) =
            parse_helpers::expect_variable_name(iter, comments, errors, range)?;
        (Some(name), Some(name_range))
    };
    parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::In)?;
    let start_expr = parse_expr::parse_expr(iter, comments, errors, range)?;
    let source = if parse_helpers::advance_if(iter, comments, errors, token::LangToken::DotDotEq)
        .is_some()
    {
        let end_expr = parse_expr::parse_expr(iter, comments, errors, range)?;
        ParsedLoopSource::RangeInclusive {
            start: start_expr,
            end: end_expr,
        }
    } else {
        ParsedLoopSource::Array(start_expr)
    };
    Ok(LoopHeader {
        var_name,
        var_name_range,
        loop_source: Box::new(source),
    })
}

/// Parse the bindings of a `<let>` from a `{` the caller has already
/// consumed, through the `}` that closes them. Returns the bindings with the
/// range of the whole `{...}`.
fn parse_let_bindings(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    left_brace: &DocumentRange,
) -> Result<(Vec<ParsedLetBinding>, DocumentRange), ErrorEmitted> {
    if let Some((token::LangToken::RightBrace, right_brace)) = tokenize_expr::peek(iter) {
        let _ = errors.emit(
            ParseErrorKind::MissingLetBinding {},
            left_brace.clone().to(right_brace),
        );
    }
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        left_brace,
        LangTokenPair::Braces,
        left_brace,
        &[],
        |iter, comments, errors, range| {
            let (var_name, var_name_range) =
                parse_helpers::expect_variable_name(iter, comments, errors, range)?;
            let var_type = if let Some((token::LangToken::Colon, _)) = tokenize_expr::peek(iter) {
                parse_helpers::expect_token(
                    iter,
                    comments,
                    errors,
                    range,
                    &token::LangToken::Colon,
                )?;
                Some(parse_type(iter, comments, errors, range)?)
            } else {
                None
            };
            parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Assign)?;
            let value_expr = parse_expr::parse_expr(iter, comments, errors, range)?;
            Ok(ParsedLetBinding {
                var_name,
                var_name_range,
                var_type,
                value_expr,
            })
        },
    )
}
