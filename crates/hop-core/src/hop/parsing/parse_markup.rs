use super::parse_error::{Emit, ErrorEmitted, OrEmit, ParseError, ParseErrorKind};
use super::parse_expr;
use super::parsed_expr::ParsedExpr;
use super::parsed_markup::{ParsedAttribute, ParsedMarkup};
use super::token::MarkupToken;
use super::token::RawTextToken;
use super::token::TagToken;
use super::tokenize_markup;
use super::whitespace;

use crate::document::{DocumentCursor, DocumentRange};
use crate::html::{HtmlElementKind, is_raw_content_tag};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;

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
    children: Vec<ParsedMarkup>,
}

impl OpenElement {
    /// The name a closing tag has to repeat to close this element, or `None`
    /// for a `<>`, which is closed by the equally nameless `</>`.
    fn name(&self) -> Option<&str> {
        match self.header {
            ElementHeader::Fragment => None,
            ElementHeader::Function { .. } | ElementHeader::Html { .. } => {
                Some(self.tag_name_range.as_str())
            }
        }
    }

    /// Build the element without a closing tag, reporting that it never got
    /// one.
    fn close_unclosed(self, errors: &mut Vec<ParseError>) -> Result<ParsedMarkup, ErrorEmitted> {
        let kind = match self.header {
            ElementHeader::Fragment => ParseErrorKind::UnclosedFragment {},
            ElementHeader::Function { .. } | ElementHeader::Html { .. } => {
                ParseErrorKind::UnclosedTag {
                    tag: self.tag_name_range.to_cheap_string(),
                }
            }
        };
        let _ = errors.emit(kind, self.tag_name_range.clone());
        close_element(self, None)
    }
}

/// What an opening tag carried, kept until the element can be built.
#[allow(clippy::large_enum_variant)]
enum ElementHeader {
    /// A `<>`, which carries nothing at all.
    Fragment,
    /// An uppercase tag, naming the function of a markup call.
    Function {
        name: Result<FunctionName, ErrorEmitted>,
        attributes: Vec<ParsedAttribute>,
    },
    /// Any other tag, naming an HTML element.
    Html {
        element: Result<HtmlElementKind, ErrorEmitted>,
        attributes: Vec<ParsedAttribute>,
    },
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
        item: Result<ParsedMarkup, ErrorEmitted>,
    ) -> Option<Result<ParsedMarkup, ErrorEmitted>> {
        match self.open.last_mut() {
            // A dropped child is just missing from its parent.
            Some(element) => {
                element.children.extend(item.ok());
                None
            }
            None => Some(item),
        }
    }

    /// Add markup to the innermost open element, or to the top level.
    fn append_markup(
        &mut self,
        markup: ParsedMarkup,
    ) -> Option<Result<ParsedMarkup, ErrorEmitted>> {
        self.append(Ok(markup))
    }

    /// Drop an item that could not be built, keeping the proof of why.
    fn drop_item(&mut self, guar: ErrorEmitted) -> Option<Result<ParsedMarkup, ErrorEmitted>> {
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
    ) -> Option<Result<ParsedMarkup, ErrorEmitted>> {
        self.append(close_element(element, closing))
    }

    /// Close the element this tag names, along with everything opened inside
    /// it that was never closed. A tag that names nothing open is reported
    /// and dropped.
    fn close(
        &mut self,
        closing: ClosingTag,
        errors: &mut Vec<ParseError>,
    ) -> Option<Result<ParsedMarkup, ErrorEmitted>> {
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
        self.append_element(element, Some(closing))
    }

    /// Take the markup, closing everything left open. Something is open:
    /// the markup would otherwise have finished with its last item.
    fn finish(mut self, errors: &mut Vec<ParseError>) -> Result<ParsedMarkup, ErrorEmitted> {
        loop {
            let element = self.open.pop().expect("an element is open");
            if let Some(item) = self.append(element.close_unclosed(errors)) {
                return item;
            }
        }
    }
}

/// Parse one piece of markup, from a `<` the caller has already consumed.
///
/// Fails when nothing there built any markup.
///
/// We do our best here to build as much markup as possible even when we
/// encounter errors.
fn parse_markup(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    left_angle: DocumentRange,
) -> Result<ParsedMarkup, ErrorEmitted> {
    let mut builder = MarkupBuilder::default();
    let mut token = tokenize_markup::lex_tag(iter, errors, left_angle)?;

    loop {
        let finished = match token {
            MarkupToken::Text { range } => builder.append_markup(ParsedMarkup::Text { range }),
            MarkupToken::Newline { range } => {
                builder.append_markup(ParsedMarkup::Newline { range })
            }
            // A comment in content is collected like a `//` comment. A comment
            // that opens the markup stands where an expression is expected.
            MarkupToken::Comment { range } => {
                if builder.open.is_empty() {
                    builder.drop_item(
                        errors.emit(ParseErrorKind::MarkupCommentOutsideMarkup {}, range),
                    )
                } else {
                    comments.push(range);
                    None
                }
            }

            MarkupToken::ExpressionStart { left_brace } => {
                match parse_expr::parse_block(iter, comments, errors, &left_brace) {
                    Ok((expression, range)) => {
                        builder.append_markup(ParsedMarkup::Interpolation { expression, range })
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
                    TagEnd::Closed(closing) => builder.append_element(element, closing),
                }
            }

            MarkupToken::ClosingTag { tag_name, range } => builder.close(
                ClosingTag {
                    tag_name_range: Some(tag_name),
                    range,
                },
                errors,
            ),

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
            return item;
        }
        match tokenize_markup::next(iter, errors) {
            Some(next) => token = next,
            None => break,
        }
    }
    builder.finish(errors)
}

/// Parse markup in expression position, from a '<' the caller has
/// already consumed.
pub fn parse_markup_expr(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    left_angle: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut markup = parse_markup(iter, comments, errors, left_angle)?;
    whitespace::normalize_markup(&mut markup);
    Ok(ParsedExpr::Markup {
        markup: Box::new(markup),
    })
}

/// Where an opening tag left the parse.
enum TagEnd {
    /// Children follow, then a closing tag.
    Open,
    /// The element is finished as it stands, with the closing tag a raw text
    /// element was read up to, or none for a self-closing tag.
    Closed(Option<ClosingTag>),
}

/// Parse an opening tag from just after its name, and say whether children
/// follow it.
fn parse_opening_tag(
    tag_name_range: DocumentRange,
    tag_start_range: DocumentRange,
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> (OpenElement, TagEnd) {
    let mut attributes = Vec::new();
    let mut self_closing = false;
    let mut full_range = tag_start_range.clone();

    // The loop ends at an `Err`: the tag ended without a `>`, which was
    // reported.
    while let Ok(part) = tokenize_markup::next_tag_token(iter, errors, &tag_name_range) {
        match part {
            TagToken::End { range } => {
                full_range = tag_start_range.to(range);
                break;
            }

            TagToken::SelfClosingEnd { range } => {
                self_closing = true;
                full_range = tag_start_range.to(range);
                break;
            }

            TagToken::Attribute {
                name: name_range,
                value,
            } => {
                let Ok(name) =
                    AttributeName::new(name_range.to_cheap_string()).or_emit(errors, &name_range)
                else {
                    continue;
                };
                let attribute = match value {
                    Some(value) => ParsedAttribute::String {
                        name,
                        name_range,
                        value: value.value,
                        quoted_range: value.quoted_range,
                    },
                    None => ParsedAttribute::KeyOnly { name, name_range },
                };
                attributes.push(attribute);
            }

            TagToken::AttributeExpressionStart {
                name: name_range,
                left_brace,
            } => {
                let name =
                    AttributeName::new(name_range.to_cheap_string()).or_emit(errors, &name_range);
                if let (Ok(name), Ok((value, _))) = (
                    name,
                    parse_expr::parse_block(iter, comments, errors, &left_brace),
                ) {
                    attributes.push(ParsedAttribute::Expression {
                        name,
                        name_range,
                        value,
                    });
                }
            }

            TagToken::Spread { name, range } => {
                if let Ok(var_name) = VarName::new(name.to_cheap_string()).or_emit(errors, &name) {
                    attributes.push(ParsedAttribute::Spread {
                        name: var_name,
                        range,
                    });
                }
            }

            TagToken::ExpressionStart { left_brace } => {
                let range = match parse_expr::parse_block(iter, comments, errors, &left_brace) {
                    Ok((_, braces)) => braces,
                    Err(_) => left_brace,
                };
                let _ = errors.emit(
                    ParseErrorKind::UnexpectedTagExpression {
                        tag_name: tag_name_range.to_cheap_string(),
                    },
                    range,
                );
            }
        }
    }

    // Styling goes through the project stylesheet, so a <style> element is
    // rejected whatever it holds. Its content is still read below, to keep the
    // rest of the markup parsing as it would otherwise.
    if tag_name_range.as_str() == "style" {
        let _ = errors.emit(
            ParseErrorKind::StyleElementNotAllowed,
            tag_name_range.clone(),
        );
    }

    // A raw text element holds text rather than markup, so its content and
    // closing tag are read here.
    let raw_text = !self_closing && is_raw_content_tag(tag_name_range.as_str());
    let mut children = Vec::new();
    let mut closed = self_closing;
    let mut closing_tag = None;
    if raw_text {
        let RawTextToken {
            content,
            closing_tag: raw_closing_tag,
        } = tokenize_markup::next_raw_text_token(iter, &tag_name_range);
        // A <script> may only reference an external file, so anything but
        // whitespace between its tags is rejected.
        if tag_name_range.as_str() == "script"
            && let Some(content) = content.as_ref().map(DocumentRange::trim)
            && !content.is_empty()
        {
            let _ = errors.emit(ParseErrorKind::InlineScriptNotAllowed, content);
        }
        children.extend(content.map(|range| ParsedMarkup::Text { range }));
        // Without a closing tag the element stays open, and is reported as
        // unclosed with everything else still open when the markup ends.
        if let Some(raw_closing_tag) = raw_closing_tag {
            closing_tag = Some(ClosingTag {
                tag_name_range: Some(raw_closing_tag.tag_name_range),
                range: raw_closing_tag.range,
            });
            closed = true;
        }
    }

    // An uppercase tag starts a markup call, anything else an HTML element.
    let header = match tag_name_range.as_str() {
        name if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
            ElementHeader::Function {
                name: FunctionName::new(tag_name_range.to_cheap_string())
                    .or_emit(errors, &tag_name_range),
                attributes,
            }
        }
        // A <base> changes how every URL on the page resolves, and <embed>
        // and <object> load external documents that <iframe>, <img> and
        // <video> cover, so they are rejected.
        "base" | "embed" | "object" => ElementHeader::Html {
            element: Err(errors.emit(
                ParseErrorKind::ElementNotAllowed {
                    tag: tag_name_range.to_cheap_string(),
                },
                tag_name_range.clone(),
            )),
            attributes,
        },
        _ => ElementHeader::Html {
            element: HtmlElementKind::parse(tag_name_range.as_str()).ok_or_else(|| {
                errors.emit(
                    ParseErrorKind::UnknownHtmlElement {
                        tag: tag_name_range.to_cheap_string(),
                    },
                    tag_name_range.clone(),
                )
            }),
            attributes,
        },
    };

    let end = if closed {
        TagEnd::Closed(closing_tag)
    } else {
        TagEnd::Open
    };
    (
        OpenElement {
            tag_name_range,
            opening_range: full_range,
            header,
            children,
        },
        end,
    )
}

/// Build an element from its header and the children that were collected for
/// it.
///
/// An element with no closing tag spans only its opening tag, since we cannot
/// tell how far the author meant it to reach.
fn close_element(
    element: OpenElement,
    closing: Option<ClosingTag>,
) -> Result<ParsedMarkup, ErrorEmitted> {
    let OpenElement {
        tag_name_range,
        opening_range,
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
        ElementHeader::Fragment => Ok(ParsedMarkup::Fragment { children, range }),

        ElementHeader::Function { name, attributes } => {
            let children = closing_tag_name.is_some().then_some(children);
            Ok(ParsedMarkup::Call {
                function_name: name?,
                function_name_opening_range: tag_name_range,
                function_name_closing_range: closing_tag_name,
                attributes,
                range,
                children,
            })
        }

        ElementHeader::Html {
            element,
            attributes,
        } => Ok(ParsedMarkup::Element {
            kind: element?,
            tag_name: tag_name_range,
            closing_tag_name,
            attributes,
            range,
            children,
        }),
    }
}
