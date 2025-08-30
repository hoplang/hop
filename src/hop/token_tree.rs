use crate::common::{ParseError, Range, is_void_element};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;

#[derive(Debug, Clone)]
pub struct TokenTree {
    pub opening_token: Token,
    pub closing_token: Option<Token>,
    pub children: Vec<TokenTree>,
}

impl TokenTree {
    pub fn new(opening_token: Token) -> Self {
        TokenTree {
            opening_token,
            closing_token: None,
            children: Vec::new(),
        }
    }

    pub fn append_node(&mut self, token: Token) {
        self.children.push(TokenTree::new(token));
    }

    pub fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    pub fn set_closing_token(&mut self, closing_token: Token) {
        self.closing_token = Some(closing_token);
    }

    pub fn range(&self) -> Range {
        match &self.closing_token {
            Some(closing_token) => Range {
                start: self.opening_token.range().start,
                end: closing_token.range().end,
            },
            None => self.opening_token.range(),
        }
    }
}

pub fn build_tree(tokenizer: Tokenizer, errors: &mut Vec<ParseError>) -> Vec<TokenTree> {
    struct StackElement {
        tree: TokenTree,
        tag_name: String,
    }

    let mut stack: Vec<StackElement> = Vec::new();

    let root_token = Token::OpeningTag {
        self_closing: false,
        value: "root".to_string(),
        attributes: Vec::new(),
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
                        ref value,
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
                                    unclosed.tree.opening_token.range(),
                                ));
                                stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                            }
                            let mut completed = stack.pop().unwrap();
                            completed.tree.set_closing_token(token);
                            stack.last_mut().unwrap().tree.append_tree(completed.tree);
                        }
                    }
                    Token::Eof { .. } => {
                        break;
                    }
                }
            }
        }
    }

    while stack.len() > 1 {
        let unclosed = stack.pop().unwrap();
        errors.push(ParseError::unclosed_tag(
            &unclosed.tag_name,
            unclosed.tree.opening_token.range(),
        ));
    }

    stack.pop().unwrap().tree.children
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::tokenizer::Tokenizer;
    use expect_test::{Expect, expect};

    fn check_tree(input: &str, expected: Expect) {
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
        check_tree(
            "<div>Hello</div>",
            expect![[r#"
                [
                    TokenTree {
                        opening_token: OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: [],
                            expression: None,
                            range: 1:1-1:6,
                        },
                        closing_token: Some(
                            ClosingTag {
                                value: "div",
                                name_range: 1:13-1:16,
                                range: 1:11-1:17,
                            },
                        ),
                        children: [
                            TokenTree {
                                opening_token: Text {
                                    value: "Hello",
                                    range: 1:6-1:11,
                                },
                                closing_token: None,
                                children: [],
                            },
                        ],
                    },
                ]"#]],
        );
    }

    #[test]
    fn test_void_element() {
        check_tree(
            "<div><br><hr/></div>",
            expect![[r#"
                [
                    TokenTree {
                        opening_token: OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: [],
                            expression: None,
                            range: 1:1-1:6,
                        },
                        closing_token: Some(
                            ClosingTag {
                                value: "div",
                                name_range: 1:17-1:20,
                                range: 1:15-1:21,
                            },
                        ),
                        children: [
                            TokenTree {
                                opening_token: OpeningTag {
                                    self_closing: false,
                                    name_range: 1:7-1:9,
                                    value: "br",
                                    attributes: [],
                                    expression: None,
                                    range: 1:6-1:10,
                                },
                                closing_token: None,
                                children: [],
                            },
                            TokenTree {
                                opening_token: OpeningTag {
                                    self_closing: true,
                                    name_range: 1:11-1:13,
                                    value: "hr",
                                    attributes: [],
                                    expression: None,
                                    range: 1:10-1:15,
                                },
                                closing_token: None,
                                children: [],
                            },
                        ],
                    },
                ]"#]],
        );
    }

    #[test]
    fn test_nested_tags() {
        check_tree(
            "<div><p>Hello</p><span>World</span></div>",
            expect![[r#"
                [
                    TokenTree {
                        opening_token: OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: [],
                            expression: None,
                            range: 1:1-1:6,
                        },
                        closing_token: Some(
                            ClosingTag {
                                value: "div",
                                name_range: 1:38-1:41,
                                range: 1:36-1:42,
                            },
                        ),
                        children: [
                            TokenTree {
                                opening_token: OpeningTag {
                                    self_closing: false,
                                    name_range: 1:7-1:8,
                                    value: "p",
                                    attributes: [],
                                    expression: None,
                                    range: 1:6-1:9,
                                },
                                closing_token: Some(
                                    ClosingTag {
                                        value: "p",
                                        name_range: 1:16-1:17,
                                        range: 1:14-1:18,
                                    },
                                ),
                                children: [
                                    TokenTree {
                                        opening_token: Text {
                                            value: "Hello",
                                            range: 1:9-1:14,
                                        },
                                        closing_token: None,
                                        children: [],
                                    },
                                ],
                            },
                            TokenTree {
                                opening_token: OpeningTag {
                                    self_closing: false,
                                    name_range: 1:19-1:23,
                                    value: "span",
                                    attributes: [],
                                    expression: None,
                                    range: 1:18-1:24,
                                },
                                closing_token: Some(
                                    ClosingTag {
                                        value: "span",
                                        name_range: 1:31-1:35,
                                        range: 1:29-1:36,
                                    },
                                ),
                                children: [
                                    TokenTree {
                                        opening_token: Text {
                                            value: "World",
                                            range: 1:24-1:29,
                                        },
                                        closing_token: None,
                                        children: [],
                                    },
                                ],
                            },
                        ],
                    },
                ]"#]],
        );
    }
}
