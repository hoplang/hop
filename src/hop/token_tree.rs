use crate::common::{ParseError, is_void_element};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;
use crate::range::string_cursor::StringSpan;
use crate::range::{Range, Ranged};

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
    pub closing_tag_name: Option<StringSpan>,
    pub children: Vec<TokenTree>,
    pub range: Range,
}

impl TokenTree {
    pub fn new(opening_token: Token) -> Self {
        TokenTree {
            range: opening_token.range(),
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
        self.range = self.range.spanning(closing_token.range());
        self.closing_tag_name = match closing_token {
            Token::ClosingTag { tag_name, .. } => Some(tag_name),
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
                        ref tag_name,
                        self_closing,
                        ..
                    } => {
                        if is_void_element(tag_name.as_str()) || self_closing {
                            if let Some(parent) = stack.last_mut() {
                                parent.tree.append_node(token);
                            } else {
                                // Top-level void/self-closing tag
                                top_level_trees.push(TokenTree::new(token));
                            }
                        } else {
                            stack.push(StackElement {
                                tree: TokenTree::new(token.clone()),
                                tag_name: tag_name.to_string(),
                            });
                        }
                    }
                    Token::ClosingTag { ref tag_name, .. } => {
                        if is_void_element(tag_name.as_str()) {
                            errors.push(ParseError::closed_void_tag(
                                tag_name.as_str(),
                                token.range(),
                            ));
                        } else if !stack.iter().any(|el| el.tag_name == tag_name.as_str()) {
                            errors.push(ParseError::unmatched_closing_tag(
                                tag_name.as_str(),
                                token.range(),
                            ));
                        } else {
                            while stack.last().unwrap().tag_name != tag_name.as_str() {
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
                            tag_name: StringSpan {
                                source: SourceInfo {
                                    text: "<div>Hello</div>",
                                    line_starts: [
                                        0,
                                    ],
                                },
                                start: 1,
                                end: 4,
                            },
                            attributes: {},
                            expression: None,
                            self_closing: false,
                            range: 1:1-1:6,
                        },
                        closing_tag_name: Some(
                            StringSpan {
                                source: SourceInfo {
                                    text: "<div>Hello</div>",
                                    line_starts: [
                                        0,
                                    ],
                                },
                                start: 12,
                                end: 15,
                            },
                        ),
                        children: [
                            TokenTree {
                                token: Text {
                                    value: StringSpan {
                                        source: SourceInfo {
                                            text: "<div>Hello</div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 5,
                                        end: 10,
                                    },
                                },
                                closing_tag_name: None,
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
                            tag_name: StringSpan {
                                source: SourceInfo {
                                    text: "<div><br><hr/></div>",
                                    line_starts: [
                                        0,
                                    ],
                                },
                                start: 1,
                                end: 4,
                            },
                            attributes: {},
                            expression: None,
                            self_closing: false,
                            range: 1:1-1:6,
                        },
                        closing_tag_name: Some(
                            StringSpan {
                                source: SourceInfo {
                                    text: "<div><br><hr/></div>",
                                    line_starts: [
                                        0,
                                    ],
                                },
                                start: 16,
                                end: 19,
                            },
                        ),
                        children: [
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: StringSpan {
                                        source: SourceInfo {
                                            text: "<div><br><hr/></div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 6,
                                        end: 8,
                                    },
                                    attributes: {},
                                    expression: None,
                                    self_closing: false,
                                    range: 1:6-1:10,
                                },
                                closing_tag_name: None,
                                children: [],
                                range: 1:6-1:10,
                            },
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: StringSpan {
                                        source: SourceInfo {
                                            text: "<div><br><hr/></div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 10,
                                        end: 12,
                                    },
                                    attributes: {},
                                    expression: None,
                                    self_closing: true,
                                    range: 1:10-1:15,
                                },
                                closing_tag_name: None,
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
                            tag_name: StringSpan {
                                source: SourceInfo {
                                    text: "<div><p>Hello</p><span>World</span></div>",
                                    line_starts: [
                                        0,
                                    ],
                                },
                                start: 1,
                                end: 4,
                            },
                            attributes: {},
                            expression: None,
                            self_closing: false,
                            range: 1:1-1:6,
                        },
                        closing_tag_name: Some(
                            StringSpan {
                                source: SourceInfo {
                                    text: "<div><p>Hello</p><span>World</span></div>",
                                    line_starts: [
                                        0,
                                    ],
                                },
                                start: 37,
                                end: 40,
                            },
                        ),
                        children: [
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: StringSpan {
                                        source: SourceInfo {
                                            text: "<div><p>Hello</p><span>World</span></div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 6,
                                        end: 7,
                                    },
                                    attributes: {},
                                    expression: None,
                                    self_closing: false,
                                    range: 1:6-1:9,
                                },
                                closing_tag_name: Some(
                                    StringSpan {
                                        source: SourceInfo {
                                            text: "<div><p>Hello</p><span>World</span></div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 15,
                                        end: 16,
                                    },
                                ),
                                children: [
                                    TokenTree {
                                        token: Text {
                                            value: StringSpan {
                                                source: SourceInfo {
                                                    text: "<div><p>Hello</p><span>World</span></div>",
                                                    line_starts: [
                                                        0,
                                                    ],
                                                },
                                                start: 8,
                                                end: 13,
                                            },
                                        },
                                        closing_tag_name: None,
                                        children: [],
                                        range: 1:9-1:14,
                                    },
                                ],
                                range: 1:6-1:18,
                            },
                            TokenTree {
                                token: OpeningTag {
                                    tag_name: StringSpan {
                                        source: SourceInfo {
                                            text: "<div><p>Hello</p><span>World</span></div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 18,
                                        end: 22,
                                    },
                                    attributes: {},
                                    expression: None,
                                    self_closing: false,
                                    range: 1:18-1:24,
                                },
                                closing_tag_name: Some(
                                    StringSpan {
                                        source: SourceInfo {
                                            text: "<div><p>Hello</p><span>World</span></div>",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 30,
                                        end: 34,
                                    },
                                ),
                                children: [
                                    TokenTree {
                                        token: Text {
                                            value: StringSpan {
                                                source: SourceInfo {
                                                    text: "<div><p>Hello</p><span>World</span></div>",
                                                    line_starts: [
                                                        0,
                                                    ],
                                                },
                                                start: 23,
                                                end: 28,
                                            },
                                        },
                                        closing_tag_name: None,
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
