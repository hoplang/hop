use crate::hop::parse_error::ParseError;
use crate::hop::token_tree::{TokenTree, build_tree};
use crate::hop::tokenizer::{Attribute, AttributeValue, Token, Tokenizer};
use pretty::RcDoc;

pub trait TokenTreePrettyPrint {
    fn to_doc(&self) -> RcDoc<()>;
}

impl TokenTreePrettyPrint for TokenTree {
    fn to_doc(&self) -> RcDoc<()> {
        match &self.token {
            Token::Doctype { .. } => RcDoc::text("<!DOCTYPE html>"),

            Token::Comment { range } => RcDoc::text("<!--")
                .append(RcDoc::text(
                    range
                        .as_str()
                        .trim_start_matches("<!--")
                        .trim_end_matches("-->"),
                ))
                .append(RcDoc::text("-->")),

            Token::Text { range } => RcDoc::text(range.as_str()),

            Token::Expression { expression, .. } => RcDoc::text("{")
                .append(RcDoc::text(expression.as_str()))
                .append(RcDoc::text("}")),

            Token::OpeningTag {
                tag_name,
                attributes,
                expression,
                self_closing,
                ..
            } => {
                let mut doc = RcDoc::text("<").append(RcDoc::text(tag_name.as_str()));

                // Add attributes
                for attr in attributes {
                    doc = doc.append(RcDoc::space()).append(format_attribute(attr));
                }

                // Add expression (for component parameters)
                if let Some(expr) = expression {
                    doc = doc
                        .append(RcDoc::space())
                        .append(RcDoc::text("{"))
                        .append(RcDoc::text(expr.as_str()))
                        .append(RcDoc::text("}"));
                }

                if *self_closing {
                    doc.append(RcDoc::text("/>"))
                } else if self.children.is_empty() && self.closing_tag_name.is_some() {
                    // Has closing tag but no children
                    doc.append(RcDoc::text("></"))
                        .append(RcDoc::text(tag_name.as_str()))
                        .append(RcDoc::text(">"))
                } else if !self.children.is_empty() {
                    doc = doc.append(RcDoc::text(">"));

                    // Check if all children are inline
                    let all_inline = self.children.iter().all(is_inline_token_tree);

                    if all_inline {
                        // All children are inline - keep them on same line to preserve spacing
                        doc = doc.append(format_children(&self.children));
                    } else {
                        // Has block-level children - safe to format with newlines
                        doc = doc
                            .append(
                                RcDoc::hardline()
                                    .append(format_children(&self.children))
                                    .nest(2),
                            )
                            .append(RcDoc::hardline());
                    }

                    // Add closing tag if present
                    if self.closing_tag_name.is_some() {
                        doc = doc
                            .append(RcDoc::text("</"))
                            .append(RcDoc::text(tag_name.as_str()))
                            .append(RcDoc::text(">"));
                    }

                    doc
                } else {
                    // Self-closing or void element
                    doc.append(RcDoc::text(">"))
                }
            }

            Token::ClosingTag { .. } => {
                // Closing tags are handled by the opening tag
                RcDoc::nil()
            }
        }
    }
}

fn format_attribute(attr: &Attribute) -> RcDoc<()> {
    match &attr.value {
        Some(AttributeValue::String(val)) => RcDoc::text(attr.name.as_str())
            .append(RcDoc::text("=\""))
            .append(RcDoc::text(val.as_str()))
            .append(RcDoc::text("\"")),
        Some(AttributeValue::Expression(val)) => RcDoc::text("{")
            .append(RcDoc::text(attr.name.as_str()))
            .append(RcDoc::text(": "))
            .append(RcDoc::text(val.as_str()))
            .append(RcDoc::text("}")),
        None => RcDoc::text(attr.name.as_str()),
    }
}

fn format_children(children: &[TokenTree]) -> RcDoc<()> {
    // Group inline content together
    let mut docs = Vec::new();
    let mut current_inline_group = Vec::new();

    for child in children {
        if is_inline_token_tree(child) {
            current_inline_group.push(child.to_doc());
        } else {
            // Flush any accumulated inline content
            if !current_inline_group.is_empty() {
                docs.push(RcDoc::intersperse(
                    current_inline_group.drain(..),
                    RcDoc::nil(),
                ));
            }
            docs.push(child.to_doc());
        }
    }

    // Flush any remaining inline content
    if !current_inline_group.is_empty() {
        docs.push(RcDoc::intersperse(
            current_inline_group.drain(..),
            RcDoc::nil(),
        ));
    }

    RcDoc::intersperse(docs, RcDoc::hardline())
}

fn is_inline_token_tree(tree: &TokenTree) -> bool {
    match &tree.token {
        Token::Text { .. } | Token::Expression { .. } => true,
        Token::OpeningTag { tag_name, .. } => {
            is_inline_element(tag_name.as_str()) && tree.children.iter().all(is_inline_token_tree)
        }
        _ => false,
    }
}

fn is_inline_element(tag: &str) -> bool {
    matches!(
        tag,
        "a" | "abbr"
            | "b"
            | "bdi"
            | "bdo"
            | "br"
            | "cite"
            | "code"
            | "data"
            | "dfn"
            | "em"
            | "i"
            | "kbd"
            | "mark"
            | "q"
            | "rp"
            | "rt"
            | "ruby"
            | "s"
            | "samp"
            | "small"
            | "span"
            | "strong"
            | "sub"
            | "sup"
            | "time"
            | "u"
            | "var"
            | "wbr"
    )
}

/// Pretty print from a token tree with the specified width
pub fn pretty_print_token_tree(trees: &[TokenTree], width: usize) -> String {
    let doc = RcDoc::intersperse(
        trees.iter().map(|tree| tree.to_doc()),
        RcDoc::hardline().append(RcDoc::hardline()),
    );
    doc.pretty(width).to_string()
}

/// Pretty print from source code directly using token trees
pub fn pretty_print_from_source(source: &str, width: usize) -> Result<String, Vec<ParseError>> {
    let mut errors = Vec::new();
    let trees = build_tree(Tokenizer::new(source.to_string()), &mut errors);

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(pretty_print_token_tree(&trees, width))
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check_pretty_print(input: &str, expected: Expect) {
        match pretty_print_from_source(input, 80) {
            Ok(output) => expected.assert_eq(&output),
            Err(errors) => panic!("Parse errors: {:?}", errors),
        }
    }

    #[test]
    fn test_mixed_content_with_spaces() {
        check_pretty_print(
            "<p>Hello <em>world</em>, this is <strong>important</strong>!</p>",
            expect![[r#"
                <p>Hello <em>world</em>, this is <strong>important</strong>!</p>"#]],
        );
    }

    #[test]
    fn test_text_with_multiple_spaces() {
        // Without format_text_with_spaces, we preserve the original spacing
        check_pretty_print(
            "<p>Hello     world    with    spaces</p>",
            expect![[r#"
                <p>Hello     world    with    spaces</p>"#]],
        );
    }

    #[test]
    fn test_inline_elements_flow() {
        check_pretty_print(
            "<p>This is <em>emphasized</em> and <strong>bold</strong> text that should flow nicely.</p>",
            expect![[r#"
                <p>This is <em>emphasized</em> and <strong>bold</strong> text that should flow nicely.</p>"#]],
        );
    }

    #[test]
    fn test_long_text_wrapping() {
        check_pretty_print(
            "<p>This is a very long piece of text that should absolutely wrap when the line gets too long for the specified width, at least that is the idea.</p>",
            expect![
                "<p>This is a very long piece of text that should absolutely wrap when the line gets too long for the specified width, at least that is the idea.</p>"
            ],
        );
    }

    #[test]
    fn test_block_children_get_newlines() {
        check_pretty_print(
            "<div><p>First paragraph</p><p>Second paragraph</p></div>",
            expect![[r#"
                <div>
                  <p>First paragraph</p>
                  <p>Second paragraph</p>
                </div>"#]],
        );
    }

    #[test]
    fn test_mixed_block_and_inline() {
        check_pretty_print(
            "<div>Some text <span>inline</span> more text<p>Block element</p>final text</div>",
            expect![[r#"
                <div>
                  Some text <span>inline</span> more text
                  <p>Block element</p>
                  final text
                </div>"#]],
        );
    }
}
