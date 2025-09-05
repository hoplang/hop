use crate::common::{ParseError, is_void_element};
use crate::range::{Range, Ranged};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;

/// A TokenTree represents a tree of tokens.
///
/// In a token tree the opening tags have been matched to their
/// corresponding closing tag and the nodes in between have been
/// collected as children.
#[derive(Debug, Clone)]
pub struct TokenTree {
    pub token: Token,

    /// The closing_token_name_range is the range of the name for the closing
    /// tag.
    ///
    /// E.g.
    /// <div></div>
    ///        ^^^
    /// This information is needed by the parser.
    pub closing_tag_name_range: Option<Range>,
    pub children: Vec<TokenTree>,
    pub range: Range,
}

impl TokenTree {
    pub fn new(opening_token: Token) -> Self {
        TokenTree {
            range: opening_token.range(),
            token: opening_token,
            closing_tag_name_range: None,
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
        self.range.end = closing_token.range().end;
        self.closing_tag_name_range = match closing_token {
            Token::ClosingTag {
                tag_name: (_, tag_name_range),
                ..
            } => Some(tag_name_range),
            _ => panic!("Called set_closing_tag with a token that was not a ClosingTag"),
        }
    }
}

/// Build a TokenTree from a tokenizer.
///
/// We do our best here to construct as much of the tree as possible even
/// when we encounter errors.
pub fn build_tree(tokenizer: Tokenizer, errors: &mut Vec<ParseError>) -> Vec<TokenTree> {
    struct StackElement {
        tree: TokenTree,
        tag_name: String,
    }

    let mut stack: Vec<StackElement> = Vec::new();
    let mut top_level_trees: Vec<TokenTree> = Vec::new();

    for t in tokenizer {
        match t {
            Err(err) => errors.push(err),
            Ok(token) => {
                match token {
                    Token::Comment { .. } => {
                        // skip comments
                        continue;
                    }
                    Token::Doctype { .. } | Token::Text { .. } | Token::Expression { .. } => {
                        if let Some(parent) = stack.last_mut() {
                            parent.tree.append_node(token);
                        } else {
                            // Top-level text/expression/doctype
                            top_level_trees.push(TokenTree::new(token));
                        }
                    }
                    Token::OpeningTag {
                        tag_name: (ref tag_name_value, _),
                        self_closing,
                        ..
                    } => {
                        if is_void_element(tag_name_value) || self_closing {
                            if let Some(parent) = stack.last_mut() {
                                parent.tree.append_node(token);
                            } else {
                                // Top-level void/self-closing tag
                                top_level_trees.push(TokenTree::new(token));
                            }
                        } else {
                            stack.push(StackElement {
                                tree: TokenTree::new(token.clone()),
                                tag_name: tag_name_value.clone(),
                            });
                        }
                    }
                    Token::ClosingTag {
                        tag_name: (ref tag_name_value, _),
                        ..
                    } => {
                        if is_void_element(tag_name_value) {
                            errors.push(ParseError::closed_void_tag(tag_name_value, token.range()));
                        } else if !stack.iter().any(|el| el.tag_name == *tag_name_value) {
                            errors.push(ParseError::unmatched_closing_tag(
                                tag_name_value,
                                token.range(),
                            ));
                        } else {
                            while stack.last().unwrap().tag_name != *tag_name_value {
                                let unclosed = stack.pop().unwrap();
                                errors.push(ParseError::unclosed_tag(
                                    &unclosed.tag_name,
                                    unclosed.tree.token.range(),
                                ));
                                stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                            }
                            let mut completed = stack.pop().unwrap();
                            completed.tree.set_closing_tag(token);
                            if let Some(parent) = stack.last_mut() {
                                parent.tree.append_tree(completed.tree);
                            } else {
                                // This was a top-level element
                                top_level_trees.push(completed.tree);
                            }
                        }
                    }
                }
            }
        }
    }

    // Report errors for any unclosed tags
    for unclosed in stack {
        errors.push(ParseError::unclosed_tag(
            &unclosed.tag_name,
            unclosed.tree.token.range(),
        ));
    }

    top_level_trees
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::tokenizer::Tokenizer;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let mut errors = Vec::new();
        let trees = build_tree(Tokenizer::new(input), &mut errors);
        if !errors.is_empty() {
            panic!("Expected errors to be empty");
        }

        let actual = format!("{:#?}", trees);

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_simple_tag() {
        check(
            "<div>Hello</div>",
            expect![[r#"
                [
                    TokenTree {
                        token: OpeningTag {
                            tag_name: (
                                "div",
                                1:2-1:5,
                            ),
                            attributes: {},
                            expression: None,
                            self_closing: false,
                            range: 1:1-1:6,
                        },
                        closing_tag_name_range: Some(
                            1:13-1:16,
                        ),
                        children: [
                            TokenTree {
                                token: Text {
                                    value: "Hello",
                                    range: 1:6-1:11,
                                },
                                closing_tag_name_range: None,
                                children: [],
                                range: 1:6-1:11,
                            },
                        ],
                        range: 1:1-1:17,
                    },
                ]"#]],
        );
    }

    #[test]
    fn test_void_element() {
        check(
            "<div><br><hr/></div>",
            expect![[r#"
                [
                    TokenTree {
                        token: OpeningTag {
                            tag_name: (
                                "div",
                                1:2-1:5,
                            ),
                            attributes: {},
                            expression: None,
                            self_closing: false,
                            range: 1:1-1:6,
                        },
                        closing_tag_name_range: Some(
                            1:17-1:20,
                        ),
                        children: [
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: (
                                        "br",
                                        1:7-1:9,
                                    ),
                                    attributes: {},
                                    expression: None,
                                    self_closing: false,
                                    range: 1:6-1:10,
                                },
                                closing_tag_name_range: None,
                                children: [],
                                range: 1:6-1:10,
                            },
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: (
                                        "hr",
                                        1:11-1:13,
                                    ),
                                    attributes: {},
                                    expression: None,
                                    self_closing: true,
                                    range: 1:10-1:15,
                                },
                                closing_tag_name_range: None,
                                children: [],
                                range: 1:10-1:15,
                            },
                        ],
                        range: 1:1-1:21,
                    },
                ]"#]],
        );
    }

    #[test]
    fn test_nested_tags() {
        check(
            "<div><p>Hello</p><span>World</span></div>",
            expect![[r#"
                [
                    TokenTree {
                        token: OpeningTag {
                            tag_name: (
                                "div",
                                1:2-1:5,
                            ),
                            attributes: {},
                            expression: None,
                            self_closing: false,
                            range: 1:1-1:6,
                        },
                        closing_tag_name_range: Some(
                            1:38-1:41,
                        ),
                        children: [
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: (
                                        "p",
                                        1:7-1:8,
                                    ),
                                    attributes: {},
                                    expression: None,
                                    self_closing: false,
                                    range: 1:6-1:9,
                                },
                                closing_tag_name_range: Some(
                                    1:16-1:17,
                                ),
                                children: [
                                    TokenTree {
                                        token: Text {
                                            value: "Hello",
                                            range: 1:9-1:14,
                                        },
                                        closing_tag_name_range: None,
                                        children: [],
                                        range: 1:9-1:14,
                                    },
                                ],
                                range: 1:6-1:18,
                            },
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: (
                                        "span",
                                        1:19-1:23,
                                    ),
                                    attributes: {},
                                    expression: None,
                                    self_closing: false,
                                    range: 1:18-1:24,
                                },
                                closing_tag_name_range: Some(
                                    1:31-1:35,
                                ),
                                children: [
                                    TokenTree {
                                        token: Text {
                                            value: "World",
                                            range: 1:24-1:29,
                                        },
                                        closing_tag_name_range: None,
                                        children: [],
                                        range: 1:24-1:29,
                                    },
                                ],
                                range: 1:18-1:36,
                            },
                        ],
                        range: 1:1-1:42,
                    },
                ]"#]],
        );
    }
}
