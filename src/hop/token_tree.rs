use crate::common::is_void_element;
use crate::document::document_cursor::{DocumentRange, Ranged as _};
use crate::hop::parse_error::ParseError;
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;
use std::fmt::{self, Display};

/// A TokenTree represents a tree of tokens.
///
/// In a token tree the opening tags have been matched to their
/// corresponding closing tag and the nodes in between have been
/// collected as children.
#[derive(Debug)]
pub struct TokenTree {
    pub token: Token,

    /// The closing_token_name_range is the range of the name for the closing
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

impl Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_tree(tree: &TokenTree, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            let indent_str = "  ".repeat(indent);
            write!(f, "{}{}", indent_str, tree.token)?;

            if let Some(ref closing_name) = tree.closing_tag_name {
                write!(f, " [has closing: {}]", closing_name)?;
            }

            if !tree.children.is_empty() {
                writeln!(f)?;
                for (i, child) in tree.children.iter().enumerate() {
                    fmt_tree(child, f, indent + 1)?;
                    if i < tree.children.len() - 1 {
                        writeln!(f)?;
                    }
                }
            }

            Ok(())
        }

        fmt_tree(self, f, 0)
    }
}

/// Build a TokenTree from a tokenizer.
///
/// We do our best here to construct as much of the tree as possible even
/// when we encounter errors.
pub fn build_tree(tokenizer: Tokenizer, errors: &mut Vec<ParseError>) -> Vec<TokenTree> {
    struct StackElement {
        tree: TokenTree,
        tag_name: DocumentRange,
    }

    let mut stack: Vec<StackElement> = Vec::new();
    let mut top_level_nodes: Vec<TokenTree> = Vec::new();

    for (token, err) in tokenizer {
        errors.extend(err);

        let Some(token) = token else {
            continue;
        };

        match token {
            Token::Comment { .. } => {
                continue;
            }
            Token::Doctype { .. } | Token::Text { .. } | Token::Expression { .. } => {
                if let Some(parent) = stack.last_mut() {
                    parent.tree.append_node(token);
                } else {
                    top_level_nodes.push(TokenTree::new(token));
                }
            }
            Token::OpeningTag {
                ref tag_name,
                self_closing,
                ..
            } => {
                if is_void_element(tag_name.as_str()) || self_closing {
                    if let Some(parent) = stack.last_mut() {
                        parent.tree.append_node(token);
                    } else {
                        top_level_nodes.push(TokenTree::new(token));
                    }
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
                        tag: tag_name.to_string(),
                        range: token.range().clone(),
                    });
                } else if !stack
                    .iter()
                    .any(|el| el.tag_name.as_str() == tag_name.as_str())
                {
                    errors.push(ParseError::UnmatchedClosingTag {
                        tag: tag_name.to_string(),
                        range: token.range().clone(),
                    });
                } else {
                    while stack.last().unwrap().tag_name.as_str() != tag_name.as_str() {
                        let unclosed = stack.pop().unwrap();
                        errors.push(ParseError::UnclosedTag {
                            tag: unclosed.tag_name.to_string(),
                            range: unclosed.tree.token.range().clone(),
                        });
                        stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                    }
                    let mut completed = stack.pop().unwrap();
                    completed.tree.set_closing_tag(token);
                    if let Some(parent) = stack.last_mut() {
                        parent.tree.append_tree(completed.tree);
                    } else {
                        top_level_nodes.push(completed.tree);
                    }
                }
            }
        }
    }

    for unclosed in stack {
        errors.push(ParseError::UnclosedTag {
            tag: unclosed.tag_name.to_string(),
            range: unclosed.tree.token.range().clone(),
        });
    }

    top_level_nodes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::tokenizer::Tokenizer;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let mut errors = Vec::new();
        let trees = build_tree(Tokenizer::new(input.to_string()), &mut errors);
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
    fn test_simple_tag() {
        check(
            indoc! {"
                <div>Hello</div>
            "},
            expect![[r#"
                OpeningTag <div> [has closing: div]
                  Text [5 byte, "Hello"]
                Text [1 byte, "\n"]
            "#]],
        );
    }

    #[test]
    fn test_void_element() {
        check(
            indoc! {"
                <div>
                    <br>
                    <hr/>
                </div>
            "},
            expect![[r#"
                OpeningTag <div> [has closing: div]
                  Text [5 byte, "\n    "]
                  OpeningTag <br>
                  Text [5 byte, "\n    "]
                  OpeningTag <hr/>
                  Text [1 byte, "\n"]
                Text [1 byte, "\n"]
            "#]],
        );
    }

    #[test]
    fn test_nested_tags() {
        check(
            indoc! {"
                <div>
                    <p>Hello</p>
                    <span>World</span>
                </div>
            "},
            expect![[r#"
                OpeningTag <div> [has closing: div]
                  Text [5 byte, "\n    "]
                  OpeningTag <p> [has closing: p]
                    Text [5 byte, "Hello"]
                  Text [5 byte, "\n    "]
                  OpeningTag <span> [has closing: span]
                    Text [5 byte, "World"]
                  Text [1 byte, "\n"]
                Text [1 byte, "\n"]
            "#]],
        );
    }
}
