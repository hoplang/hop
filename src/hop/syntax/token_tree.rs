use std::fmt::{self, Display};
use std::iter::Peekable;

use super::tokenizer::{Token, Tokenizer};
use crate::common::is_void_element;
use crate::document::{DocumentCursor, DocumentRange, Ranged as _};
use crate::error_collector::ErrorCollector;
use crate::parse_error::ParseError;

/// A TokenTree represents a tree of tokens.
///
/// In a token tree the opening tags have been matched to their
/// corresponding closing tag and the nodes in between have been
/// collected as children.
#[derive(Debug)]
pub struct TokenTree {
    pub token: Token,

    /// The closing_tag_name is the range of the name for the closing
    /// tag.
    ///
    /// E.g.
    /// <div></div>
    ///        ^^^
    /// This information is needed by the parser.
    pub closing_tag_name: Option<DocumentRange>,
    pub children: Vec<TokenTree>,
    pub range: DocumentRange,
}

impl TokenTree {
    pub fn new(opening_token: Token) -> Self {
        let range = opening_token.range().clone();
        TokenTree {
            range,
            token: opening_token,
            closing_tag_name: None,
            children: Vec::new(),
        }
    }

    pub fn append_node(&mut self, token: Token) {
        self.children.push(TokenTree::new(token));
    }

    pub fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    pub fn set_closing_tag(&mut self, closing_token: Token) {
        match closing_token {
            Token::ClosingTag { tag_name, range } => {
                self.closing_tag_name = Some(tag_name);
                self.range = self.range.clone().to(range);
            }
            _ => panic!("Called set_closing_tag with a token that was not a ClosingTag"),
        }
    }
}

/// Build a single TokenTree from a tokenizer.
///
/// Returns `None` if the document cursor is exhausted, otherwise returns
/// the next top-level token tree. The cursor is left positioned after
/// the consumed tokens, allowing the caller to parse additional trees.
///
/// We do our best here to construct as much of the tree as possible even
/// when we encounter errors.
pub fn parse_tree(
    tokenizer: &mut Tokenizer,
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<TokenTree> {
    loop {
        let token = tokenizer.next(iter, errors)?;

        match token {
            // Leaf tokens - return immediately as single-node trees
            Token::Comment { .. }
            | Token::Doctype { .. }
            | Token::Text { .. }
            | Token::Newline { .. }
            | Token::TextExpression { .. }
            | Token::RawTextTag { .. } => return Some(TokenTree::new(token)),

            Token::OpeningTag {
                ref tag_name,
                self_closing,
                ..
            } => {
                if is_void_element(tag_name.as_str()) || self_closing {
                    return Some(TokenTree::new(token));
                }
                let tag_name = tag_name.clone();
                return Some(parse_nested_tree(tokenizer, token, tag_name, iter, errors));
            }

            Token::ClosingTag { ref tag_name, .. } => {
                // Top-level closing tag is always an error - report and continue loop
                if is_void_element(tag_name.as_str()) {
                    errors.push(ParseError::ClosedVoidTag {
                        tag: tag_name.to_cheap_string(),
                        range: token.range().clone(),
                    });
                } else {
                    errors.push(ParseError::UnmatchedClosingTag {
                        tag: tag_name.to_cheap_string(),
                        range: token.range().clone(),
                    });
                }
            }
        }
    }
}

/// Build a tree for a non-void, non-self-closing opening tag.
///
/// Uses a stack-based approach to handle nested content until the
/// matching closing tag is found.
fn parse_nested_tree(
    tokenizer: &mut Tokenizer,
    opening_token: Token,
    opening_tag_name: DocumentRange,
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> TokenTree {
    struct StackElement {
        tree: TokenTree,
        tag_name: DocumentRange,
    }

    let mut stack: Vec<StackElement> = vec![StackElement {
        tree: TokenTree::new(opening_token),
        tag_name: opening_tag_name,
    }];

    while let Some(token) = tokenizer.next(iter, errors) {
        match token {
            Token::Comment { .. }
            | Token::Doctype { .. }
            | Token::Text { .. }
            | Token::Newline { .. }
            | Token::TextExpression { .. }
            | Token::RawTextTag { .. } => {
                stack.last_mut().unwrap().tree.append_node(token);
            }

            Token::OpeningTag {
                ref tag_name,
                self_closing,
                ..
            } => {
                if is_void_element(tag_name.as_str()) || self_closing {
                    stack.last_mut().unwrap().tree.append_node(token);
                } else {
                    stack.push(StackElement {
                        tag_name: tag_name.clone(),
                        tree: TokenTree::new(token),
                    });
                }
            }

            Token::ClosingTag { ref tag_name, .. } => {
                if is_void_element(tag_name.as_str()) {
                    errors.push(ParseError::ClosedVoidTag {
                        tag: tag_name.to_cheap_string(),
                        range: token.range().clone(),
                    });
                } else if !stack
                    .iter()
                    .any(|el| el.tag_name.as_str() == tag_name.as_str())
                {
                    errors.push(ParseError::UnmatchedClosingTag {
                        tag: tag_name.to_cheap_string(),
                        range: token.range().clone(),
                    });
                } else {
                    // Pop until we find the matching tag
                    while stack.last().unwrap().tag_name.as_str() != tag_name.as_str() {
                        let unclosed = stack.pop().unwrap();
                        errors.push(ParseError::UnclosedTag {
                            tag: unclosed.tag_name.to_cheap_string(),
                            range: unclosed.tag_name.clone(),
                        });
                        stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                    }

                    let mut completed = stack.pop().unwrap();
                    completed.tree.set_closing_tag(token);

                    if stack.is_empty() {
                        return completed.tree;
                    } else {
                        stack.last_mut().unwrap().tree.append_tree(completed.tree);
                    }
                }
            }
        }
    }

    // EOF with unclosed tags - collapse everything onto the root
    while stack.len() > 1 {
        let unclosed = stack.pop().unwrap();
        errors.push(ParseError::UnclosedTag {
            tag: unclosed.tag_name.to_cheap_string(),
            range: unclosed.tag_name.clone(),
        });
        stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
    }

    let unclosed = stack.pop().unwrap();
    errors.push(ParseError::UnclosedTag {
        tag: unclosed.tag_name.to_cheap_string(),
        range: unclosed.tag_name.clone(),
    });
    unclosed.tree
}

impl Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_tree(tree: &TokenTree, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            let indent_str = "  ".repeat(indent);
            writeln!(f, "{}TokenTree(", indent_str)?;

            // Write token with proper indentation for each line
            let token_str = tree.token.to_string();
            for line in token_str.lines() {
                writeln!(f, "{}  {}", indent_str, line)?;
            }

            // Write closing_tag_name
            let closing_str = match &tree.closing_tag_name {
                Some(name) => format!("Some({:?})", name.to_string()),
                None => "None".to_string(),
            };
            writeln!(f, "{}  closing_tag_name: {},", indent_str, closing_str)?;

            // Write children
            write!(f, "{}  children: [", indent_str)?;
            if tree.children.is_empty() {
                writeln!(f, "],")?;
            } else {
                writeln!(f)?;
                for child in &tree.children {
                    fmt_tree(child, f, indent + 2)?;
                }
                writeln!(f, "{}  ],", indent_str)?;
            }

            writeln!(f, "{})", indent_str)?;
            Ok(())
        }

        fmt_tree(self, f, 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let mut errors = ErrorCollector::new();
        let mut iter = DocumentCursor::new(input.to_string()).peekable();
        let mut tokenizer = Tokenizer::new();

        let mut trees = Vec::new();
        while let Some(tree) = parse_tree(&mut tokenizer, &mut iter, &mut errors) {
            trees.push(tree);
        }

        if !errors.is_empty() {
            panic!("Expected errors to be empty");
        }

        let mut actual = trees
            .iter()
            .map(|tree| tree.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        actual.push('\n');

        expected.assert_eq(&actual);
    }

    #[test]
    fn simple_tag() {
        check(
            indoc! {"
                <div>Hello</div>
            "},
            expect![[r#"
                TokenTree(
                  OpeningTag(
                    tag_name: "div",
                    attributes: {},
                    expression: None,
                    self_closing: false,
                  )
                  closing_tag_name: Some("div"),
                  children: [
                    TokenTree(
                      Text [5 byte, "Hello"]
                      closing_tag_name: None,
                      children: [],
                    )
                  ],
                )

            "#]],
        );
    }

    #[test]
    fn void_element() {
        check(
            indoc! {"
                <div>
                    <br>
                    <hr/>
                </div>
            "},
            expect![[r#"
                TokenTree(
                  OpeningTag(
                    tag_name: "div",
                    attributes: {},
                    expression: None,
                    self_closing: false,
                  )
                  closing_tag_name: Some("div"),
                  children: [
                    TokenTree(
                      OpeningTag(
                        tag_name: "br",
                        attributes: {},
                        expression: None,
                        self_closing: false,
                      )
                      closing_tag_name: None,
                      children: [],
                    )
                    TokenTree(
                      OpeningTag(
                        tag_name: "hr",
                        attributes: {},
                        expression: None,
                        self_closing: true,
                      )
                      closing_tag_name: None,
                      children: [],
                    )
                  ],
                )

            "#]],
        );
    }

    #[test]
    fn nested_tags() {
        check(
            indoc! {"
                <div>
                    <p>Hello</p>
                    <span>World</span>
                </div>
            "},
            expect![[r#"
                TokenTree(
                  OpeningTag(
                    tag_name: "div",
                    attributes: {},
                    expression: None,
                    self_closing: false,
                  )
                  closing_tag_name: Some("div"),
                  children: [
                    TokenTree(
                      OpeningTag(
                        tag_name: "p",
                        attributes: {},
                        expression: None,
                        self_closing: false,
                      )
                      closing_tag_name: Some("p"),
                      children: [
                        TokenTree(
                          Text [5 byte, "Hello"]
                          closing_tag_name: None,
                          children: [],
                        )
                      ],
                    )
                    TokenTree(
                      OpeningTag(
                        tag_name: "span",
                        attributes: {},
                        expression: None,
                        self_closing: false,
                      )
                      closing_tag_name: Some("span"),
                      children: [
                        TokenTree(
                          Text [5 byte, "World"]
                          closing_tag_name: None,
                          children: [],
                        )
                      ],
                    )
                  ],
                )

            "#]],
        );
    }
}
