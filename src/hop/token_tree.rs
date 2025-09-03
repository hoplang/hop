use std::collections::BTreeMap;

use crate::common::{ParseError, Range, Ranged, is_void_element};
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
            Token::ClosingTag { name_range, .. } => Some(name_range),
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

    let root_token = Token::OpeningTag {
        self_closing: false,
        tag_name: "root".to_string(),
        attributes: BTreeMap::new(),
        name_range: Range::default(),
        expression: None,
        range: Range::default(),
    };
    stack.push(StackElement {
        tree: TokenTree::new(root_token),
        tag_name: "root".to_string(),
    });

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
                        stack.last_mut().unwrap().tree.append_node(token);
                    }
                    Token::OpeningTag {
                        tag_name: ref value,
                        self_closing,
                        ..
                    } => {
                        if is_void_element(value) || self_closing {
                            stack.last_mut().unwrap().tree.append_node(token);
                        } else {
                            stack.push(StackElement {
                                tree: TokenTree::new(token.clone()),
                                tag_name: value.clone(),
                            });
                        }
                    }
                    Token::ClosingTag { ref value, .. } => {
                        if is_void_element(value) {
                            errors.push(ParseError::closed_void_tag(value, token.range()));
                        } else if !stack.iter().any(|el| el.tag_name == *value) {
                            errors.push(ParseError::unmatched_closing_tag(value, token.range()));
                        } else {
                            while stack.last().unwrap().tag_name != *value {
                                let unclosed = stack.pop().unwrap();
                                errors.push(ParseError::unclosed_tag(
                                    &unclosed.tag_name,
                                    unclosed.tree.token.range(),
                                ));
                                stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                            }
                            let mut completed = stack.pop().unwrap();
                            completed.tree.set_closing_tag(token);
                            stack.last_mut().unwrap().tree.append_tree(completed.tree);
                        }
                    }
                }
            }
        }
    }

    while stack.len() > 1 {
        let unclosed = stack.pop().unwrap();
        errors.push(ParseError::unclosed_tag(
            &unclosed.tag_name,
            unclosed.tree.token.range(),
        ));
    }

    stack.pop().unwrap().tree.children
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
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {},
                            expression: None,
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
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:6,
                        },
                        closing_tag_name_range: Some(
                            1:17-1:20,
                        ),
                        children: [
                            TokenTree {
                                token: OpeningTag {
                                    self_closing: false,
                                    name_range: 1:7-1:9,
                                    value: "br",
                                    attributes: {},
                                    expression: None,
                                    range: 1:6-1:10,
                                },
                                closing_tag_name_range: None,
                                children: [],
                                range: 1:6-1:10,
                            },
                            TokenTree {
                                token: OpeningTag {
                                    self_closing: true,
                                    name_range: 1:11-1:13,
                                    value: "hr",
                                    attributes: {},
                                    expression: None,
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
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:6,
                        },
                        closing_tag_name_range: Some(
                            1:38-1:41,
                        ),
                        children: [
                            TokenTree {
                                token: OpeningTag {
                                    self_closing: false,
                                    name_range: 1:7-1:8,
                                    value: "p",
                                    attributes: {},
                                    expression: None,
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
                                    self_closing: false,
                                    name_range: 1:19-1:23,
                                    value: "span",
                                    attributes: {},
                                    expression: None,
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
