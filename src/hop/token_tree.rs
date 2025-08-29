use crate::common::{Range, RangeError, is_void_element};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;

#[derive(Debug, Clone)]
pub struct TokenTree {
    pub token: Token,
    pub children: Vec<TokenTree>,
    pub opening_tag_range: Range,
    pub closing_tag_name_range: Option<Range>,
}

impl TokenTree {
    pub fn new(token: Token, range: Range) -> Self {
        TokenTree {
            token,
            children: Vec::new(),
            opening_tag_range: range,
            closing_tag_name_range: None,
        }
    }

    pub fn append_node(&mut self, token: Token, range: Range) {
        self.children.push(TokenTree::new(token, range));
    }

    pub fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    pub fn set_closing_tag_name_range(&mut self, range: Range) {
        self.closing_tag_name_range = Some(range);
    }
}

pub fn build_tree(tokenizer: Tokenizer, errors: &mut Vec<RangeError>) -> Vec<TokenTree> {
    struct StackElement {
        tree: TokenTree,
        tag_name: String,
    }

    let mut stack: Vec<StackElement> = Vec::new();

    let root_token = Token::StartTag {
        self_closing: false,
        value: "root".to_string(),
        attributes: Vec::new(),
        name_range: Range::default(),
        expression: None,
    };
    stack.push(StackElement {
        tree: TokenTree::new(root_token, Range::default()),
        tag_name: "root".to_string(),
    });

    for t in tokenizer {
        match t {
            Err(err) => errors.push(err),
            Ok((token, range)) => {
                match token {
                    Token::Comment => {
                        // skip comments
                        continue;
                    }
                    Token::Doctype | Token::Text { .. } | Token::Expression { .. } => {
                        stack.last_mut().unwrap().tree.append_node(token, range);
                    }
                    Token::StartTag {
                        ref value,
                        self_closing,
                        ..
                    } => {
                        if is_void_element(value) || self_closing {
                            stack.last_mut().unwrap().tree.append_node(token, range);
                        } else {
                            stack.push(StackElement {
                                tree: TokenTree::new(token.clone(), range),
                                tag_name: value.clone(),
                            });
                        }
                    }
                    Token::EndTag {
                        ref value,
                        name_range,
                        ..
                    } => {
                        if is_void_element(value) {
                            errors.push(RangeError::closed_void_tag(value, range));
                        } else if !stack.iter().any(|el| el.tag_name == *value) {
                            errors.push(RangeError::unmatched_closing_tag(value, range));
                        } else {
                            while stack.last().unwrap().tag_name != *value {
                                let unclosed = stack.pop().unwrap();
                                errors.push(RangeError::unclosed_tag(
                                    &unclosed.tag_name,
                                    unclosed.tree.opening_tag_range,
                                ));
                                stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                            }
                            let mut completed = stack.pop().unwrap();
                            completed.tree.set_closing_tag_name_range(name_range);
                            stack.last_mut().unwrap().tree.append_tree(completed.tree);
                        }
                    }
                    Token::Eof => {
                        break;
                    }
                }
            }
        }
    }

    while stack.len() > 1 {
        let unclosed = stack.pop().unwrap();
        errors.push(RangeError::unclosed_tag(
            &unclosed.tag_name,
            unclosed.tree.opening_tag_range,
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
                        token: StartTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: [],
                            expression: None,
                        },
                        children: [
                            TokenTree {
                                token: Text {
                                    value: "Hello",
                                },
                                children: [],
                                opening_tag_range: 1:6-1:11,
                                closing_tag_name_range: None,
                            },
                        ],
                        opening_tag_range: 1:1-1:6,
                        closing_tag_name_range: Some(
                            1:13-1:16,
                        ),
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
                        token: StartTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: [],
                            expression: None,
                        },
                        children: [
                            TokenTree {
                                token: StartTag {
                                    self_closing: false,
                                    name_range: 1:7-1:9,
                                    value: "br",
                                    attributes: [],
                                    expression: None,
                                },
                                children: [],
                                opening_tag_range: 1:6-1:10,
                                closing_tag_name_range: None,
                            },
                            TokenTree {
                                token: StartTag {
                                    self_closing: true,
                                    name_range: 1:11-1:13,
                                    value: "hr",
                                    attributes: [],
                                    expression: None,
                                },
                                children: [],
                                opening_tag_range: 1:10-1:15,
                                closing_tag_name_range: None,
                            },
                        ],
                        opening_tag_range: 1:1-1:6,
                        closing_tag_name_range: Some(
                            1:17-1:20,
                        ),
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
                        token: StartTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: [],
                            expression: None,
                        },
                        children: [
                            TokenTree {
                                token: StartTag {
                                    self_closing: false,
                                    name_range: 1:7-1:8,
                                    value: "p",
                                    attributes: [],
                                    expression: None,
                                },
                                children: [
                                    TokenTree {
                                        token: Text {
                                            value: "Hello",
                                        },
                                        children: [],
                                        opening_tag_range: 1:9-1:14,
                                        closing_tag_name_range: None,
                                    },
                                ],
                                opening_tag_range: 1:6-1:9,
                                closing_tag_name_range: Some(
                                    1:16-1:17,
                                ),
                            },
                            TokenTree {
                                token: StartTag {
                                    self_closing: false,
                                    name_range: 1:19-1:23,
                                    value: "span",
                                    attributes: [],
                                    expression: None,
                                },
                                children: [
                                    TokenTree {
                                        token: Text {
                                            value: "World",
                                        },
                                        children: [],
                                        opening_tag_range: 1:24-1:29,
                                        closing_tag_name_range: None,
                                    },
                                ],
                                opening_tag_range: 1:18-1:24,
                                closing_tag_name_range: Some(
                                    1:31-1:35,
                                ),
                            },
                        ],
                        opening_tag_range: 1:1-1:6,
                        closing_tag_name_range: Some(
                            1:38-1:41,
                        ),
                    },
                ]"#]],
        );
    }
}
