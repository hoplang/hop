use std::collections::BTreeMap;
use std::mem;

use crate::common::{ParseError, Position, Range};
use crate::hop::ast::Attribute;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Doctype {
        range: Range,
    },
    Comment {
        range: Range,
    },

    /// An Expression token represents an expression in the text position
    /// E.g. <div>hello {expression}<div>
    Expression {
        value: String,
        expression_range: Range,
        range: Range,
    },
    OpeningTag {
        self_closing: bool,
        name_range: Range,
        value: String,
        attributes: BTreeMap<String, Attribute>,
        expression: Option<(String, Range)>,
        range: Range,
    },
    ClosingTag {
        value: String,
        name_range: Range,
        range: Range,
    },
    Text {
        value: String,
        range: Range,
    },
}

impl Token {
    pub fn range(&self) -> Range {
        match self {
            Token::Doctype { range } => *range,
            Token::Comment { range } => *range,
            Token::Expression { range, .. } => *range,
            Token::OpeningTag { range, .. } => *range,
            Token::ClosingTag { range, .. } => *range,
            Token::Text { range, .. } => *range,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenizerState {
    Text,
    TagStart,
    OpeningTagName,
    ClosingTagStart,
    ClosingTagName,
    AfterClosingTagName,
    BeforeAttrName,
    AttrName,
    BeforeAttrValue,
    AttrValueDoubleQuote,
    AttrValueSingleQuote,
    SelfClosing,
    MarkupDeclaration,
    Comment,
    Doctype,
    BeforeDoctypeName,
    DoctypeName,
    RawtextData,
    TextExpressionContent,
    TagExpressionContent,
}

struct Cursor {
    input: Vec<char>,
    /// Current position (0-indexed, in characters)
    position: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column index (1-indexed, in bytes)
    column: usize,
}

impl Cursor {
    fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    fn peek(&self) -> Option<(char, Range)> {
        if self.position >= self.input.len() {
            None
        } else {
            let start = Position::new(self.line, self.column);
            let mut end = Position::new(self.line, self.column);
            let ch = self.input[self.position];
            let byte_len = ch.len_utf8();
            if ch == '\n' {
                end.line += 1;
                end.column = 1;
            } else {
                end.column += byte_len;
            }
            Some((ch, Range::new(start, end)))
        }
    }

    fn next(&mut self) -> Option<(char, Range)> {
        if self.position >= self.input.len() {
            None
        } else {
            let start = Position::new(self.line, self.column);
            let ch = self.input[self.position];
            let byte_len = ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += byte_len;
            }
            self.position += 1;
            Some((ch, Range::new(start, Position::new(self.line, self.column))))
        }
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    fn get_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn match_str(&self, s: &str) -> bool {
        if self.position + s.len() > self.input.len() {
            return false;
        }

        let chars: Vec<char> = s.chars().collect();
        for (i, &ch) in chars.iter().enumerate() {
            if self.input[self.position + i] != ch {
                return false;
            }
        }
        true
    }
}

pub struct Tokenizer {
    cursor: Cursor,
    state: TokenizerState,
    stored_tag_name: String,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Self {
            cursor: Cursor::new(input),
            state: TokenizerState::Text,
            stored_tag_name: String::new(),
        }
    }

    fn advance(&mut self) -> Option<Result<Token, ParseError>> {
        let (_, first_range) = self.cursor.peek()?;
        let mut token_value = String::new();
        let token_start = self.cursor.get_position();
        let mut token_attributes = BTreeMap::new();
        let mut token_expression = None;
        let mut tag_name_range = first_range;
        let mut expression_content = String::new();
        let mut expression_start = self.cursor.get_position();
        let mut attribute_name = String::new();
        let mut attribute_value = String::new();
        let mut attribute_start = self.cursor.get_position();
        let mut attribute_value_start = self.cursor.get_position();
        let mut doctype_name_buffer = String::new();
        while self.cursor.peek().is_some() {
            let (ch, ch_range) = self.cursor.peek().unwrap();

            match self.state {
                TokenizerState::Text => {
                    if ch == '<' {
                        if !token_value.is_empty() {
                            return Some(Ok(Token::Text {
                                value: token_value,
                                range: Range::new(token_start, ch_range.start),
                            }));
                        }
                        self.cursor.next();
                        self.state = TokenizerState::TagStart;
                    } else if ch == '{' {
                        if !token_value.is_empty() {
                            return Some(Ok(Token::Text {
                                value: token_value,
                                range: Range::new(token_start, ch_range.start),
                            }));
                        }
                        self.cursor.next();
                        let (_, expr_start) = self.cursor.peek().unwrap();
                        expression_start = expr_start.start;
                        self.state = TokenizerState::TextExpressionContent;
                    } else {
                        token_value.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::TagStart => {
                    if ch.is_ascii_alphabetic() {
                        token_value.push(ch);
                        self.cursor.next();
                        tag_name_range = ch_range;
                        self.state = TokenizerState::OpeningTagName;
                    } else if ch == '/' {
                        self.cursor.next();
                        self.state = TokenizerState::ClosingTagStart;
                    } else if ch == '!' {
                        self.cursor.next();
                        self.state = TokenizerState::MarkupDeclaration;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character after '<'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::OpeningTagName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        token_value.push(ch);
                        self.cursor.next();
                        tag_name_range = tag_name_range.extend_to(ch_range);
                        self.state = TokenizerState::OpeningTagName;
                    } else if ch.is_whitespace() {
                        self.cursor.next();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch == '{' {
                        self.cursor.next();
                        expression_start = self.cursor.get_position();
                        self.state = TokenizerState::TagExpressionContent;
                    } else if ch == '>' {
                        if is_tag_name_with_raw_content(&token_value) {
                            self.stored_tag_name = token_value.clone();
                            self.cursor.next();
                            self.state = TokenizerState::RawtextData;
                            return Some(Ok(Token::OpeningTag {
                                value: token_value,
                                self_closing: false,
                                attributes: token_attributes,
                                name_range: tag_name_range,
                                expression: token_expression,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        } else {
                            self.cursor.next();
                            self.state = TokenizerState::Text;
                            return Some(Ok(Token::OpeningTag {
                                value: token_value,
                                self_closing: false,
                                attributes: token_attributes,
                                name_range: tag_name_range,
                                expression: token_expression,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        }
                    } else if ch == '/' {
                        self.cursor.next();
                        self.state = TokenizerState::SelfClosing;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character after '<'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::ClosingTagStart => {
                    if ch.is_ascii_alphabetic() {
                        tag_name_range = ch_range;
                        token_value.push(ch);
                        self.cursor.next();
                        tag_name_range = tag_name_range.extend_to(ch_range);
                        self.state = TokenizerState::ClosingTagName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character after '</'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::ClosingTagName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        token_value.push(ch);
                        self.cursor.next();
                        tag_name_range = tag_name_range.extend_to(ch_range);
                        self.state = TokenizerState::ClosingTagName;
                    } else if ch == '>' {
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Ok(Token::ClosingTag {
                            value: token_value,
                            name_range: tag_name_range,
                            range: Range::new(token_start, self.cursor.get_position()),
                        }));
                    } else if ch.is_whitespace() {
                        self.cursor.next();
                        self.state = TokenizerState::AfterClosingTagName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character in end tag name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::AfterClosingTagName => {
                    if ch.is_whitespace() {
                        self.cursor.next();
                        self.state = TokenizerState::AfterClosingTagName;
                    } else if ch == '>' {
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Ok(Token::ClosingTag {
                            value: token_value,
                            name_range: tag_name_range,
                            range: Range::new(token_start, self.cursor.get_position()),
                        }));
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character after end tag name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::BeforeAttrName => {
                    if ch.is_whitespace() {
                        self.cursor.next();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch.is_ascii_alphabetic() {
                        attribute_start = self.cursor.get_position();
                        attribute_name.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::AttrName;
                    } else if ch == '{' {
                        self.cursor.next();
                        expression_start = self.cursor.get_position();
                        self.state = TokenizerState::TagExpressionContent;
                    } else if ch == '/' {
                        self.cursor.next();
                        self.state = TokenizerState::SelfClosing;
                    } else if ch == '>' {
                        if is_tag_name_with_raw_content(&token_value) {
                            self.stored_tag_name = token_value.clone();
                            self.cursor.next();
                            self.state = TokenizerState::RawtextData;
                            return Some(Ok(Token::OpeningTag {
                                self_closing: false,
                                value: token_value,
                                attributes: token_attributes,
                                expression: token_expression,
                                name_range: tag_name_range,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        } else {
                            self.cursor.next();
                            self.state = TokenizerState::Text;
                            return Some(Ok(Token::OpeningTag {
                                self_closing: false,
                                value: token_value,
                                attributes: token_attributes,
                                expression: token_expression,
                                name_range: tag_name_range,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        }
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character before attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::AttrName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        attribute_name.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::AttrName;
                    } else if ch == '=' {
                        self.cursor.next();
                        self.state = TokenizerState::BeforeAttrValue;
                    } else if ch.is_whitespace() {
                        // Push current attribute
                        let attr_name = mem::take(&mut attribute_name);
                        if token_attributes.contains_key(&attr_name) {
                            self.state = TokenizerState::Text;
                            return Some(Err(ParseError::duplicate_attribute(
                                &attr_name,
                                Range {
                                    start: attribute_start,
                                    end: self.cursor.get_position(),
                                },
                            )));
                        }
                        token_attributes.insert(
                            attr_name.clone(),
                            Attribute {
                                name: attr_name,
                                value: None,
                                range: Range {
                                    start: attribute_start,
                                    end: self.cursor.get_position(),
                                },
                            },
                        );
                        attribute_start = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch == '>' {
                        // Push current attribute
                        let attr_name = mem::take(&mut attribute_name);
                        if token_attributes.contains_key(&attr_name) {
                            self.state = TokenizerState::Text;
                            return Some(Err(ParseError::duplicate_attribute(
                                &attr_name,
                                Range {
                                    start: attribute_start,
                                    end: self.cursor.get_position(),
                                },
                            )));
                        }
                        token_attributes.insert(
                            attr_name.clone(),
                            Attribute {
                                name: attr_name,
                                value: None,
                                range: Range {
                                    start: attribute_start,
                                    end: self.cursor.get_position(),
                                },
                            },
                        );
                        if is_tag_name_with_raw_content(&token_value) {
                            self.stored_tag_name = token_value.clone();
                            self.cursor.next();
                            self.state = TokenizerState::RawtextData;
                            return Some(Ok(Token::OpeningTag {
                                self_closing: false,
                                value: token_value,
                                attributes: token_attributes,
                                expression: token_expression,
                                name_range: tag_name_range,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        } else {
                            self.cursor.next();
                            self.state = TokenizerState::Text;
                            return Some(Ok(Token::OpeningTag {
                                self_closing: false,
                                value: token_value,
                                attributes: token_attributes,
                                expression: token_expression,
                                name_range: tag_name_range,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        }
                    } else if ch == '/' {
                        // Push current attribute
                        let attr_name = mem::take(&mut attribute_name);
                        if token_attributes.contains_key(&attr_name) {
                            self.state = TokenizerState::Text;
                            return Some(Err(ParseError::duplicate_attribute(
                                &attr_name,
                                Range::new(attribute_start, self.cursor.get_position()),
                            )));
                        }
                        token_attributes.insert(
                            attr_name.clone(),
                            Attribute {
                                name: attr_name,
                                value: None,
                                range: Range {
                                    start: attribute_start,
                                    end: self.cursor.get_position(),
                                },
                            },
                        );
                        self.cursor.next();
                        self.state = TokenizerState::SelfClosing;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character in attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::BeforeAttrValue => {
                    if ch == '"' {
                        self.cursor.next();
                        self.state = TokenizerState::AttrValueDoubleQuote;
                        attribute_value_start = self.cursor.get_position();
                    } else if ch == '\'' {
                        self.cursor.next();
                        self.state = TokenizerState::AttrValueSingleQuote;
                        attribute_value_start = self.cursor.get_position();
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Expected quoted attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::AttrValueDoubleQuote => {
                    if ch == '"' {
                        let attribute_value_end = self.cursor.get_position();
                        self.cursor.next();
                        // Push current attribute
                        let attr_name = mem::take(&mut attribute_name);
                        if token_attributes.contains_key(&attr_name) {
                            self.state = TokenizerState::Text;
                            return Some(Err(ParseError::duplicate_attribute(
                                &attr_name,
                                Range::new(attribute_start, self.cursor.get_position()),
                            )));
                        }
                        token_attributes.insert(
                            attr_name.clone(),
                            Attribute {
                                name: attr_name,
                                value: if attribute_value_start == attribute_value_end {
                                    None
                                } else {
                                    Some((
                                        mem::take(&mut attribute_value),
                                        Range::new(attribute_value_start, attribute_value_end),
                                    ))
                                },
                                range: Range {
                                    start: attribute_start,
                                    end: self.cursor.get_position(),
                                },
                            },
                        );
                        attribute_start = self.cursor.get_position();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        attribute_value.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::AttrValueDoubleQuote;
                    }
                }

                TokenizerState::AttrValueSingleQuote => {
                    if ch == '\'' {
                        let attribute_value_end = self.cursor.get_position();
                        self.cursor.next();
                        // Push current attribute
                        let attr_name = mem::take(&mut attribute_name);
                        if token_attributes.contains_key(&attr_name) {
                            self.state = TokenizerState::Text;
                            return Some(Err(ParseError::duplicate_attribute(
                                &attr_name,
                                Range::new(attribute_start, self.cursor.get_position()),
                            )));
                        }
                        token_attributes.insert(
                            attr_name.clone(),
                            Attribute {
                                name: attr_name,
                                value: if attribute_value_start != attribute_value_end {
                                    Some((
                                        mem::take(&mut attribute_value),
                                        Range::new(attribute_value_start, attribute_value_end),
                                    ))
                                } else {
                                    None
                                },
                                range: Range::new(attribute_start, self.cursor.get_position()),
                            },
                        );
                        attribute_start = self.cursor.get_position();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        attribute_value.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::AttrValueSingleQuote;
                    }
                }

                TokenizerState::SelfClosing => {
                    if ch == '>' {
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Ok(Token::OpeningTag {
                            self_closing: true,
                            value: token_value,
                            attributes: token_attributes,
                            expression: token_expression,
                            name_range: tag_name_range,
                            range: Range::new(token_start, self.cursor.get_position()),
                        }));
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Expected '>' after '/'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::MarkupDeclaration => {
                    if self.cursor.match_str("--") {
                        self.cursor.advance_n(2);
                        self.state = TokenizerState::Comment;
                    } else if self.cursor.match_str("DOCTYPE") {
                        self.cursor.advance_n(7);
                        self.state = TokenizerState::Doctype;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid markup declaration".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::Comment => {
                    if self.cursor.match_str("-->") {
                        self.cursor.advance_n(3);
                        self.state = TokenizerState::Text;
                        return Some(Ok(Token::Comment {
                            range: Range::new(token_start, self.cursor.get_position()),
                        }));
                    } else {
                        token_value.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::Comment;
                    }
                }

                TokenizerState::Doctype => {
                    if ch.is_whitespace() {
                        self.cursor.next();
                        self.state = TokenizerState::BeforeDoctypeName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Expected whitespace after DOCTYPE".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::BeforeDoctypeName => {
                    if ch.is_whitespace() {
                        self.cursor.next();
                        self.state = TokenizerState::BeforeDoctypeName;
                    } else if ch.is_ascii_alphabetic() {
                        doctype_name_buffer.clear();
                        doctype_name_buffer.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::DoctypeName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Expected DOCTYPE name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::DoctypeName => {
                    if ch.is_ascii_alphabetic() {
                        doctype_name_buffer.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::DoctypeName;
                    } else if ch == '>' {
                        if doctype_name_buffer.to_lowercase() == "html" {
                            for ch in doctype_name_buffer.chars() {
                                token_value.push(ch);
                            }
                            self.cursor.next();
                            self.state = TokenizerState::Text;
                            return Some(Ok(Token::Doctype {
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        } else {
                            let start_pos = self.cursor.get_position();
                            self.cursor.next();
                            self.state = TokenizerState::Text;
                            return Some(Err(ParseError::new(
                                "Invalid DOCTYPE name".to_string(),
                                Range::new(start_pos, self.cursor.get_position()),
                            )));
                        }
                    } else {
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Err(ParseError::new(
                            "Invalid character in DOCTYPE name".to_string(),
                            Range::new(self.cursor.get_position(), self.cursor.get_position()),
                        )));
                    }
                }

                TokenizerState::RawtextData => {
                    let end_tag = format!("</{}>", self.stored_tag_name);
                    if self.cursor.match_str(&end_tag) {
                        if !token_value.is_empty() {
                            self.state = TokenizerState::RawtextData;
                            return Some(Ok(Token::Text {
                                value: token_value,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        } else {
                            // No accumulated content, create and return end tag token directly
                            let tag_name = self.stored_tag_name.clone();
                            self.cursor.advance_n(2); // consume </
                            tag_name_range.start = self.cursor.get_position();
                            self.cursor.advance_n(self.stored_tag_name.len()); // consume tag name 
                            tag_name_range.end = self.cursor.get_position();
                            self.cursor.advance_n(1); // consume >
                            self.state = TokenizerState::Text;
                            return Some(Ok(Token::ClosingTag {
                                value: tag_name,
                                name_range: tag_name_range,
                                range: Range::new(token_start, self.cursor.get_position()),
                            }));
                        }
                    } else {
                        token_value.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::RawtextData;
                    }
                }

                TokenizerState::TextExpressionContent => {
                    if ch == '}' {
                        let expression_range = Range {
                            start: expression_start,
                            end: self.cursor.get_position(),
                        };
                        self.cursor.next();
                        self.state = TokenizerState::Text;
                        return Some(Ok(Token::Expression {
                            value: expression_content,
                            expression_range,
                            range: Range::new(token_start, self.cursor.get_position()),
                        }));
                    } else {
                        expression_content.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::TextExpressionContent;
                    }
                }

                TokenizerState::TagExpressionContent => {
                    // Temporary fix until we embed hop tokenizer here
                    if self.cursor.match_str("}>")
                        || self.cursor.match_str("} >")
                        || self.cursor.match_str("}/>")
                        || self.cursor.match_str("} />")
                    {
                        // Store expression on current token and continue parsing tag
                        token_expression = Some((
                            mem::take(&mut expression_content),
                            Range {
                                start: expression_start,
                                end: self.cursor.get_position(),
                            },
                        ));
                        expression_start = self.cursor.get_position();
                        self.cursor.next();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        expression_content.push(ch);
                        self.cursor.next();
                        self.state = TokenizerState::TagExpressionContent;
                    }
                }
            }
        }

        // End of input - return any accumulated token
        if !token_value.is_empty() {
            return Some(Ok(Token::Text {
                value: token_value,
                range: Range::new(token_start, self.cursor.get_position()),
            }));
        }

        // No more tokens - return Eof token
        None
    }
}

impl Iterator for Tokenizer {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance()
    }
}

fn is_tag_name_with_raw_content(name: &str) -> bool {
    matches!(name, "script" | "style" | "hop-x-raw")
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let tokenizer = Tokenizer::new(input);
        let result: Vec<_> = tokenizer.collect();

        // Validate that ranges are contiguous
        let mut iter = result.iter().peekable();
        while let Some(token_result) = iter.next() {
            if let (Ok(current_token), Some(Ok(next_token))) = (token_result, iter.peek()) {
                let current_range = current_token.range();
                let next_range = next_token.range();
                if current_range.end != next_range.start {
                    panic!(
                        "Non-contiguous ranges detected: token ends at {:?}, but next token starts at {:?}. \
                         Current token: {:?}, Next token: {:?}",
                        current_range.end, next_range.start, current_token, next_token
                    );
                }
            }
        }

        let actual = format!("{:#?}", result);
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_tokenize_empty() {
        check("", expect!["[]"]);
    }

    #[test]
    fn test_tokenize_input_with_attributes() {
        check(
            r#"<input type="" value="" disabled="">"#,
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:7,
                            value: "input",
                            attributes: {
                                "disabled": Attribute {
                                    name: "disabled",
                                    value: None,
                                    range: 1:25-1:36,
                                },
                                "type": Attribute {
                                    name: "type",
                                    value: None,
                                    range: 1:8-1:15,
                                },
                                "value": Attribute {
                                    name: "value",
                                    value: None,
                                    range: 1:16-1:24,
                                },
                            },
                            expression: None,
                            range: 1:1-1:37,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_attributes_without_spaces() {
        check(
            r#"<h1 foo="bar"x="y">"#,
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {
                                "foo": Attribute {
                                    name: "foo",
                                    value: Some(
                                        (
                                            "bar",
                                            1:10-1:13,
                                        ),
                                    ),
                                    range: 1:5-1:14,
                                },
                                "x": Attribute {
                                    name: "x",
                                    value: Some(
                                        (
                                            "y",
                                            1:17-1:18,
                                        ),
                                    ),
                                    range: 1:14-1:19,
                                },
                            },
                            expression: None,
                            range: 1:1-1:20,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_with_attributes() {
        check(
            "<h1 foo bar/>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: true,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {
                                "bar": Attribute {
                                    name: "bar",
                                    value: None,
                                    range: 1:9-1:12,
                                },
                                "foo": Attribute {
                                    name: "foo",
                                    value: None,
                                    range: 1:5-1:8,
                                },
                            },
                            expression: None,
                            range: 1:1-1:14,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing() {
        check(
            "<h1 foo/>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: true,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {
                                "foo": Attribute {
                                    name: "foo",
                                    value: None,
                                    range: 1:5-1:8,
                                },
                            },
                            expression: None,
                            range: 1:1-1:10,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_comment_with_text() {
        check(
            indoc! {"
                <p><!-- -->
                </p>
            "},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:3,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:4,
                        },
                    ),
                    Ok(
                        Comment {
                            range: 1:4-1:12,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 1:12-2:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 2:3-2:4,
                            range: 2:1-2:5,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 2:5-3:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_textarea_with_content() {
        check(
            indoc! {"
                <textarea>
                	<div></div>
                </textarea>
            "},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:10,
                            value: "textarea",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:11,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t",
                            range: 1:11-2:2,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:3-2:6,
                            value: "div",
                            attributes: {},
                            expression: None,
                            range: 2:2-2:7,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 2:9-2:12,
                            range: 2:7-2:13,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 2:13-3:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "textarea",
                            name_range: 3:3-3:11,
                            range: 3:1-3:12,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 3:12-4:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_title_with_self_closing() {
        check(
            "<title><slot-title/></title>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:7,
                            value: "title",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:8,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: true,
                            name_range: 1:9-1:19,
                            value: "slot-title",
                            attributes: {},
                            expression: None,
                            range: 1:8-1:21,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "title",
                            name_range: 1:23-1:28,
                            range: 1:21-1:29,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_comment_simple() {
        check(
            "<p><!-- --></p>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:3,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:4,
                        },
                    ),
                    Ok(
                        Comment {
                            range: 1:4-1:12,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 1:14-1:15,
                            range: 1:12-1:16,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_comment_standalone() {
        check(
            "<!-- Comment with -- dashes -- inside -->",
            expect![[r#"
                [
                    Ok(
                        Comment {
                            range: 1:1-1:42,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_multiline_comment() {
        check(
            indoc! {"
                <p><!--
                A comment
                that stretches
                over several
                lines --></p>
            "},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:3,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:4,
                        },
                    ),
                    Ok(
                        Comment {
                            range: 1:4-5:10,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 5:12-5:13,
                            range: 5:10-5:14,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:14-6:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_comment_with_quotes() {
        check(
            r#"<!-- This comment has <tags> and "quotes" and 'apostrophes' -->"#,
            expect![[r#"
                [
                    Ok(
                        Comment {
                            range: 1:1-1:64,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_doctype() {
        check(
            "<!DOCTYPE   html>",
            expect![[r#"
                [
                    Ok(
                        Doctype {
                            range: 1:1-1:18,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_end_tag_with_space() {
        check(
            "</div >",
            expect![[r#"
                [
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 1:3-1:6,
                            range: 1:1-1:8,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_simple() {
        check(
            "<h1/>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: true,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:6,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_with_space() {
        check(
            "<h1 />",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: true,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:7,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_style_with_content() {
        check(
            indoc! {"
                <style>
                  body { color: red; }
                  .class { font-size: 12px; }
                </style>
            "},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:7,
                            value: "style",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:8,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n  body { color: red; }\n  .class { font-size: 12px; }\n",
                            range: 1:8-4:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "style",
                            name_range: 4:3-4:8,
                            range: 4:1-4:9,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:9-5:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_multiple_headings() {
        check(
            indoc! {"
                <h1></h1>
                <h2></h2>
                <h3></h3>
                <h4></h4>
                <h5></h5>
                <h6></h6>
            "},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:5,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h1",
                            name_range: 1:7-1:9,
                            range: 1:5-1:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 1:10-2:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:2-2:4,
                            value: "h2",
                            attributes: {},
                            expression: None,
                            range: 2:1-2:5,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h2",
                            name_range: 2:7-2:9,
                            range: 2:5-2:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 2:10-3:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 3:2-3:4,
                            value: "h3",
                            attributes: {},
                            expression: None,
                            range: 3:1-3:5,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h3",
                            name_range: 3:7-3:9,
                            range: 3:5-3:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 3:10-4:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 4:2-4:4,
                            value: "h4",
                            attributes: {},
                            expression: None,
                            range: 4:1-4:5,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h4",
                            name_range: 4:7-4:9,
                            range: 4:5-4:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:10-5:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 5:2-5:4,
                            value: "h5",
                            attributes: {},
                            expression: None,
                            range: 5:1-5:5,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h5",
                            name_range: 5:7-5:9,
                            range: 5:5-5:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:10-6:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 6:2-6:4,
                            value: "h6",
                            attributes: {},
                            expression: None,
                            range: 6:1-6:5,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h6",
                            name_range: 6:7-6:9,
                            range: 6:5-6:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 6:10-7:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_multiline_attributes() {
        check(
            indoc! {r#"
                <div
                  class="multiline"
                  id="test"
                  data-value="something">
                </div>
            "#},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {
                                "class": Attribute {
                                    name: "class",
                                    value: Some(
                                        (
                                            "multiline",
                                            2:10-2:19,
                                        ),
                                    ),
                                    range: 2:3-2:20,
                                },
                                "data-value": Attribute {
                                    name: "data-value",
                                    value: Some(
                                        (
                                            "something",
                                            4:15-4:24,
                                        ),
                                    ),
                                    range: 4:3-4:25,
                                },
                                "id": Attribute {
                                    name: "id",
                                    value: Some(
                                        (
                                            "test",
                                            3:7-3:11,
                                        ),
                                    ),
                                    range: 3:3-3:12,
                                },
                            },
                            expression: None,
                            range: 1:1-4:26,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:26-5:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 5:3-5:6,
                            range: 5:1-5:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:7-6:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_script_with_content() {
        check(
            indoc! {r#"
                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </script>
            "#},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:8,
                            value: "script",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:9,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n",
                            range: 1:9-4:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "script",
                            name_range: 4:3-4:9,
                            range: 4:1-4:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:10-5:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_hop_x_raw_simple() {
        check(
            "<hop-x-raw>foo bar</hop-x-raw>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:11,
                            value: "hop-x-raw",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:12,
                        },
                    ),
                    Ok(
                        Text {
                            value: "foo bar",
                            range: 1:12-1:19,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "hop-x-raw",
                            name_range: 1:21-1:30,
                            range: 1:19-1:31,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_hop_x_raw_with_html() {
        check(
            indoc! {"
                <hop-x-raw>
                  <div>some html</div>
                </hop-x-raw>
            "},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:11,
                            value: "hop-x-raw",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:12,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n  <div>some html</div>\n",
                            range: 1:12-3:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "hop-x-raw",
                            name_range: 3:3-3:12,
                            range: 3:1-3:13,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 3:13-4:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_adjacent_elements() {
        check(
            "<p></p><p></p>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:3,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:4,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 1:6-1:7,
                            range: 1:4-1:8,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:9-1:10,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:8-1:11,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 1:13-1:14,
                            range: 1:11-1:15,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_expression() {
        check(
            indoc! {r#"
                <main-comp {foo}>
                	<script>
                		const x = "<div></div>";
                	</script>
                </main-comp>
            "#},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:11,
                            value: "main-comp",
                            attributes: {},
                            expression: Some(
                                (
                                    "foo",
                                    1:13-1:16,
                                ),
                            ),
                            range: 1:1-1:18,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t",
                            range: 1:18-2:2,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:3-2:9,
                            value: "script",
                            attributes: {},
                            expression: None,
                            range: 2:2-2:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t\tconst x = \"<div></div>\";\n\t",
                            range: 2:10-4:2,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "script",
                            name_range: 4:4-4:10,
                            range: 4:2-4:11,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:11-5:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "main-comp",
                            name_range: 5:3-5:12,
                            range: 5:1-5:13,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:13-6:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_full_html_document() {
        check(
            indoc! {r#"
                <!DOCTYPE html>
                <html>
                <head>
                  <title>My Page</title>
                </head>
                <body>
                  <div class="container">
                    Hello, world!
                  </div>
                </body>
                </html>
            "#},
            expect![[r#"
                [
                    Ok(
                        Doctype {
                            range: 1:1-1:16,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 1:16-2:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:2-2:6,
                            value: "html",
                            attributes: {},
                            expression: None,
                            range: 2:1-2:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 2:7-3:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 3:2-3:6,
                            value: "head",
                            attributes: {},
                            expression: None,
                            range: 3:1-3:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n  ",
                            range: 3:7-4:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 4:4-4:9,
                            value: "title",
                            attributes: {},
                            expression: None,
                            range: 4:3-4:10,
                        },
                    ),
                    Ok(
                        Text {
                            value: "My Page",
                            range: 4:10-4:17,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "title",
                            name_range: 4:19-4:24,
                            range: 4:17-4:25,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:25-5:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "head",
                            name_range: 5:3-5:7,
                            range: 5:1-5:8,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:8-6:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 6:2-6:6,
                            value: "body",
                            attributes: {},
                            expression: None,
                            range: 6:1-6:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n  ",
                            range: 6:7-7:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 7:4-7:7,
                            value: "div",
                            attributes: {
                                "class": Attribute {
                                    name: "class",
                                    value: Some(
                                        (
                                            "container",
                                            7:15-7:24,
                                        ),
                                    ),
                                    range: 7:8-7:25,
                                },
                            },
                            expression: None,
                            range: 7:3-7:26,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n    Hello, world!\n  ",
                            range: 7:26-9:3,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 9:5-9:8,
                            range: 9:3-9:9,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 9:9-10:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "body",
                            name_range: 10:3-10:7,
                            range: 10:1-10:8,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 10:8-11:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "html",
                            name_range: 11:3-11:7,
                            range: 11:1-11:8,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 11:8-12:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_svg_with_many_attributes() {
        check(
            indoc! {r#"
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                <line x1="12" y1="22.08" x2="12" y2="12"></line>
                </svg>
            "#},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "svg",
                            attributes: {
                                "fill": Attribute {
                                    name: "fill",
                                    value: Some(
                                        (
                                            "none",
                                            1:90-1:94,
                                        ),
                                    ),
                                    range: 1:84-1:95,
                                },
                                "height": Attribute {
                                    name: "height",
                                    value: Some(
                                        (
                                            "24",
                                            1:60-1:62,
                                        ),
                                    ),
                                    range: 1:52-1:63,
                                },
                                "stroke": Attribute {
                                    name: "stroke",
                                    value: Some(
                                        (
                                            "currentColor",
                                            1:104-1:116,
                                        ),
                                    ),
                                    range: 1:96-1:117,
                                },
                                "stroke-linecap": Attribute {
                                    name: "stroke-linecap",
                                    value: Some(
                                        (
                                            "round",
                                            1:151-1:156,
                                        ),
                                    ),
                                    range: 1:135-1:157,
                                },
                                "stroke-linejoin": Attribute {
                                    name: "stroke-linejoin",
                                    value: Some(
                                        (
                                            "round",
                                            1:175-1:180,
                                        ),
                                    ),
                                    range: 1:158-1:181,
                                },
                                "stroke-width": Attribute {
                                    name: "stroke-width",
                                    value: Some(
                                        (
                                            "2",
                                            1:132-1:133,
                                        ),
                                    ),
                                    range: 1:118-1:134,
                                },
                                "viewBox": Attribute {
                                    name: "viewBox",
                                    value: Some(
                                        (
                                            "0 0 24 24",
                                            1:73-1:82,
                                        ),
                                    ),
                                    range: 1:64-1:83,
                                },
                                "width": Attribute {
                                    name: "width",
                                    value: Some(
                                        (
                                            "24",
                                            1:48-1:50,
                                        ),
                                    ),
                                    range: 1:41-1:51,
                                },
                                "xmlns": Attribute {
                                    name: "xmlns",
                                    value: Some(
                                        (
                                            "http://www.w3.org/2000/svg",
                                            1:13-1:39,
                                        ),
                                    ),
                                    range: 1:6-1:40,
                                },
                            },
                            expression: None,
                            range: 1:1-1:182,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 1:182-2:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:2-2:6,
                            value: "line",
                            attributes: {
                                "x1": Attribute {
                                    name: "x1",
                                    value: Some(
                                        (
                                            "16.5",
                                            2:11-2:15,
                                        ),
                                    ),
                                    range: 2:7-2:16,
                                },
                                "x2": Attribute {
                                    name: "x2",
                                    value: Some(
                                        (
                                            "7.5",
                                            2:30-2:33,
                                        ),
                                    ),
                                    range: 2:26-2:34,
                                },
                                "y1": Attribute {
                                    name: "y1",
                                    value: Some(
                                        (
                                            "9.4",
                                            2:21-2:24,
                                        ),
                                    ),
                                    range: 2:17-2:25,
                                },
                                "y2": Attribute {
                                    name: "y2",
                                    value: Some(
                                        (
                                            "4.21",
                                            2:39-2:43,
                                        ),
                                    ),
                                    range: 2:35-2:44,
                                },
                            },
                            expression: None,
                            range: 2:1-2:45,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "line",
                            name_range: 2:47-2:51,
                            range: 2:45-2:52,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 2:52-3:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 3:2-3:6,
                            value: "path",
                            attributes: {
                                "d": Attribute {
                                    name: "d",
                                    value: Some(
                                        (
                                            "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z",
                                            3:10-3:131,
                                        ),
                                    ),
                                    range: 3:7-3:132,
                                },
                            },
                            expression: None,
                            range: 3:1-3:133,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "path",
                            name_range: 3:135-3:139,
                            range: 3:133-3:140,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 3:140-4:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 4:2-4:10,
                            value: "polyline",
                            attributes: {
                                "points": Attribute {
                                    name: "points",
                                    value: Some(
                                        (
                                            "3.27 6.96 12 12.01 20.73 6.96",
                                            4:19-4:48,
                                        ),
                                    ),
                                    range: 4:11-4:49,
                                },
                            },
                            expression: None,
                            range: 4:1-4:50,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "polyline",
                            name_range: 4:52-4:60,
                            range: 4:50-4:61,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 4:61-5:1,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 5:2-5:6,
                            value: "line",
                            attributes: {
                                "x1": Attribute {
                                    name: "x1",
                                    value: Some(
                                        (
                                            "12",
                                            5:11-5:13,
                                        ),
                                    ),
                                    range: 5:7-5:14,
                                },
                                "x2": Attribute {
                                    name: "x2",
                                    value: Some(
                                        (
                                            "12",
                                            5:30-5:32,
                                        ),
                                    ),
                                    range: 5:26-5:33,
                                },
                                "y1": Attribute {
                                    name: "y1",
                                    value: Some(
                                        (
                                            "22.08",
                                            5:19-5:24,
                                        ),
                                    ),
                                    range: 5:15-5:25,
                                },
                                "y2": Attribute {
                                    name: "y2",
                                    value: Some(
                                        (
                                            "12",
                                            5:38-5:40,
                                        ),
                                    ),
                                    range: 5:34-5:41,
                                },
                            },
                            expression: None,
                            range: 5:1-5:42,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "line",
                            name_range: 5:44-5:48,
                            range: 5:42-5:49,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:49-6:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "svg",
                            name_range: 6:3-6:6,
                            range: 6:1-6:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 6:7-7:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_nested_svg() {
        check(
            indoc! {r#"
                <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                	<g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                		<path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                		<path d="M17.54 47.09v48l35.099 12.775"></path>
                		<path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                	</g>
                </svg>
            "#},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "svg",
                            attributes: {
                                "class": Attribute {
                                    name: "class",
                                    value: Some(
                                        (
                                            "size-12",
                                            1:109-1:116,
                                        ),
                                    ),
                                    range: 1:102-1:117,
                                },
                                "height": Attribute {
                                    name: "height",
                                    value: Some(
                                        (
                                            "128",
                                            1:61-1:64,
                                        ),
                                    ),
                                    range: 1:53-1:65,
                                },
                                "version": Attribute {
                                    name: "version",
                                    value: Some(
                                        (
                                            "1.1",
                                            1:75-1:78,
                                        ),
                                    ),
                                    range: 1:66-1:79,
                                },
                                "viewBox": Attribute {
                                    name: "viewBox",
                                    value: Some(
                                        (
                                            "0 0 128 128",
                                            1:89-1:100,
                                        ),
                                    ),
                                    range: 1:80-1:101,
                                },
                                "width": Attribute {
                                    name: "width",
                                    value: Some(
                                        (
                                            "128",
                                            1:48-1:51,
                                        ),
                                    ),
                                    range: 1:41-1:52,
                                },
                                "xmlns": Attribute {
                                    name: "xmlns",
                                    value: Some(
                                        (
                                            "http://www.w3.org/2000/svg",
                                            1:13-1:39,
                                        ),
                                    ),
                                    range: 1:6-1:40,
                                },
                            },
                            expression: None,
                            range: 1:1-1:118,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t",
                            range: 1:118-2:2,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:3-2:4,
                            value: "g",
                            attributes: {
                                "style": Attribute {
                                    name: "style",
                                    value: Some(
                                        (
                                            "fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;",
                                            2:12-2:111,
                                        ),
                                    ),
                                    range: 2:5-2:112,
                                },
                            },
                            expression: None,
                            range: 2:2-2:113,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t\t",
                            range: 2:113-3:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 3:4-3:8,
                            value: "path",
                            attributes: {
                                "d": Attribute {
                                    name: "d",
                                    value: Some(
                                        (
                                            "M20.04 38 64 22l43.96 16L64 54Z",
                                            3:12-3:43,
                                        ),
                                    ),
                                    range: 3:9-3:44,
                                },
                            },
                            expression: None,
                            range: 3:3-3:45,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "path",
                            name_range: 3:47-3:51,
                            range: 3:45-3:52,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t\t",
                            range: 3:52-4:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 4:4-4:8,
                            value: "path",
                            attributes: {
                                "d": Attribute {
                                    name: "d",
                                    value: Some(
                                        (
                                            "M17.54 47.09v48l35.099 12.775",
                                            4:12-4:41,
                                        ),
                                    ),
                                    range: 4:9-4:42,
                                },
                            },
                            expression: None,
                            range: 4:3-4:43,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "path",
                            name_range: 4:45-4:49,
                            range: 4:43-4:50,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t\t",
                            range: 4:50-5:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 5:4-5:8,
                            value: "path",
                            attributes: {
                                "d": Attribute {
                                    name: "d",
                                    value: Some(
                                        (
                                            "M64 112V64l46.46-16.91v48L77.988 106.91",
                                            5:12-5:51,
                                        ),
                                    ),
                                    range: 5:9-5:52,
                                },
                            },
                            expression: None,
                            range: 5:3-5:53,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "path",
                            name_range: 5:55-5:59,
                            range: 5:53-5:60,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t",
                            range: 5:60-6:2,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "g",
                            name_range: 6:4-6:5,
                            range: 6:2-6:6,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 6:6-7:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "svg",
                            name_range: 7:3-7:6,
                            range: 7:1-7:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 7:7-8:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_form_with_input() {
        check(
            indoc! {r#"
                <main-comp>
                	<form id="form">
                		<input type="text" required>
                		<button type="submit">Send</button>
                	</form>
                </main-comp>
            "#},
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:11,
                            value: "main-comp",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:12,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t",
                            range: 1:12-2:2,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 2:3-2:7,
                            value: "form",
                            attributes: {
                                "id": Attribute {
                                    name: "id",
                                    value: Some(
                                        (
                                            "form",
                                            2:12-2:16,
                                        ),
                                    ),
                                    range: 2:8-2:17,
                                },
                            },
                            expression: None,
                            range: 2:2-2:18,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t\t",
                            range: 2:18-3:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 3:4-3:9,
                            value: "input",
                            attributes: {
                                "required": Attribute {
                                    name: "required",
                                    value: None,
                                    range: 3:22-3:30,
                                },
                                "type": Attribute {
                                    name: "type",
                                    value: Some(
                                        (
                                            "text",
                                            3:16-3:20,
                                        ),
                                    ),
                                    range: 3:10-3:21,
                                },
                            },
                            expression: None,
                            range: 3:3-3:31,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t\t",
                            range: 3:31-4:3,
                        },
                    ),
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 4:4-4:10,
                            value: "button",
                            attributes: {
                                "type": Attribute {
                                    name: "type",
                                    value: Some(
                                        (
                                            "submit",
                                            4:17-4:23,
                                        ),
                                    ),
                                    range: 4:11-4:24,
                                },
                            },
                            expression: None,
                            range: 4:3-4:25,
                        },
                    ),
                    Ok(
                        Text {
                            value: "Send",
                            range: 4:25-4:29,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "button",
                            name_range: 4:31-4:37,
                            range: 4:29-4:38,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n\t",
                            range: 4:38-5:2,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "form",
                            name_range: 5:4-5:8,
                            range: 5:2-5:9,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 5:9-6:1,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "main-comp",
                            name_range: 6:3-6:12,
                            range: 6:1-6:13,
                        },
                    ),
                    Ok(
                        Text {
                            value: "\n",
                            range: 6:13-7:1,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_if_with_expression() {
        check(
            "<if {foo}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:4,
                            value: "if",
                            attributes: {},
                            expression: Some(
                                (
                                    "foo",
                                    1:6-1:9,
                                ),
                            ),
                            range: 1:1-1:11,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_class_and_expression() {
        check(
            "<div class=\"test\" {bar}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {
                                "class": Attribute {
                                    name: "class",
                                    value: Some(
                                        (
                                            "test",
                                            1:13-1:17,
                                        ),
                                    ),
                                    range: 1:6-1:18,
                                },
                            },
                            expression: Some(
                                (
                                    "bar",
                                    1:20-1:23,
                                ),
                            ),
                            range: 1:1-1:25,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_if_with_equality_expression() {
        check(
            "<if {user.name == 'John'}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:4,
                            value: "if",
                            attributes: {},
                            expression: Some(
                                (
                                    "user.name == 'John'",
                                    1:6-1:25,
                                ),
                            ),
                            range: 1:1-1:27,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_property_access() {
        check(
            "<component {obj.prop.subprop}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:11,
                            value: "component",
                            attributes: {},
                            expression: Some(
                                (
                                    "obj.prop.subprop",
                                    1:13-1:29,
                                ),
                            ),
                            range: 1:1-1:31,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_button_with_attribute_and_expression() {
        check(
            "<button disabled {enabled == 'yes'}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:8,
                            value: "button",
                            attributes: {
                                "disabled": Attribute {
                                    name: "disabled",
                                    value: None,
                                    range: 1:9-1:17,
                                },
                            },
                            expression: Some(
                                (
                                    "enabled == 'yes'",
                                    1:19-1:35,
                                ),
                            ),
                            range: 1:1-1:37,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_input_with_variable_name() {
        check(
            "<input {variable_name_123}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:7,
                            value: "input",
                            attributes: {},
                            expression: Some(
                                (
                                    "variable_name_123",
                                    1:9-1:26,
                                ),
                            ),
                            range: 1:1-1:28,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_spaced_expression() {
        check(
            "<div class=\"test\" {  user.name  }>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {
                                "class": Attribute {
                                    name: "class",
                                    value: Some(
                                        (
                                            "test",
                                            1:13-1:17,
                                        ),
                                    ),
                                    range: 1:6-1:18,
                                },
                            },
                            expression: Some(
                                (
                                    "  user.name  ",
                                    1:20-1:33,
                                ),
                            ),
                            range: 1:1-1:35,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_span_with_string_expression() {
        check(
            "<span {'hello world'}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:6,
                            value: "span",
                            attributes: {},
                            expression: Some(
                                (
                                    "'hello world'",
                                    1:8-1:21,
                                ),
                            ),
                            range: 1:1-1:23,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_form_with_parenthesized_expression() {
        check(
            "<form {(user.role == 'admin')}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:6,
                            value: "form",
                            attributes: {},
                            expression: Some(
                                (
                                    "(user.role == 'admin')",
                                    1:8-1:30,
                                ),
                            ),
                            range: 1:1-1:32,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_section_with_chained_equality() {
        check(
            "<section {a == b == c}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:9,
                            value: "section",
                            attributes: {},
                            expression: Some(
                                (
                                    "a == b == c",
                                    1:11-1:22,
                                ),
                            ),
                            range: 1:1-1:24,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_for_with_in_expression() {
        check(
            "<for {user in users}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "for",
                            attributes: {},
                            expression: Some(
                                (
                                    "user in users",
                                    1:7-1:20,
                                ),
                            ),
                            range: 1:1-1:22,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_for_with_property_access() {
        check(
            "<for {item in user.items}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "for",
                            attributes: {},
                            expression: Some(
                                (
                                    "item in user.items",
                                    1:7-1:25,
                                ),
                            ),
                            range: 1:1-1:27,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_in_expression() {
        check(
            "<div {foo in bars}>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {},
                            expression: Some(
                                (
                                    "foo in bars",
                                    1:7-1:18,
                                ),
                            ),
                            range: 1:1-1:20,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_single_expression() {
        check(
            "<h1>Hello {name}!</h1>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:4,
                            value: "h1",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:5,
                        },
                    ),
                    Ok(
                        Text {
                            value: "Hello ",
                            range: 1:5-1:11,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "name",
                            expression_range: 1:12-1:16,
                            range: 1:11-1:17,
                        },
                    ),
                    Ok(
                        Text {
                            value: "!",
                            range: 1:17-1:18,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h1",
                            name_range: 1:20-1:22,
                            range: 1:18-1:23,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_multiple_expressions() {
        check(
            "<p>User {user.name} has {user.count} items</p>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:3,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:4,
                        },
                    ),
                    Ok(
                        Text {
                            value: "User ",
                            range: 1:4-1:9,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "user.name",
                            expression_range: 1:10-1:19,
                            range: 1:9-1:20,
                        },
                    ),
                    Ok(
                        Text {
                            value: " has ",
                            range: 1:20-1:25,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "user.count",
                            expression_range: 1:26-1:36,
                            range: 1:25-1:37,
                        },
                    ),
                    Ok(
                        Text {
                            value: " items",
                            range: 1:37-1:43,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 1:45-1:46,
                            range: 1:43-1:47,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_at_start() {
        check(
            "<span>{greeting} world!</span>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:6,
                            value: "span",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:7,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "greeting",
                            expression_range: 1:8-1:16,
                            range: 1:7-1:17,
                        },
                    ),
                    Ok(
                        Text {
                            value: " world!",
                            range: 1:17-1:24,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "span",
                            name_range: 1:26-1:30,
                            range: 1:24-1:31,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_at_end() {
        check(
            "<div>Price: {price}</div>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:6,
                        },
                    ),
                    Ok(
                        Text {
                            value: "Price: ",
                            range: 1:6-1:13,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "price",
                            expression_range: 1:14-1:19,
                            range: 1:13-1:20,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 1:22-1:25,
                            range: 1:20-1:26,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_only_expression() {
        check(
            "<h2>{title}</h2>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:4,
                            value: "h2",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:5,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "title",
                            expression_range: 1:6-1:11,
                            range: 1:5-1:12,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "h2",
                            name_range: 1:14-1:16,
                            range: 1:12-1:17,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_complex_expression_in_text() {
        check(
            "<p>Status: {user.profile.status == 'active'}</p>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:3,
                            value: "p",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:4,
                        },
                    ),
                    Ok(
                        Text {
                            value: "Status: ",
                            range: 1:4-1:12,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "user.profile.status == 'active'",
                            expression_range: 1:13-1:44,
                            range: 1:12-1:45,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "p",
                            name_range: 1:47-1:48,
                            range: 1:45-1:49,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_expression_with_property_access() {
        check(
            "<span>Item: {item.title}</span>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:6,
                            value: "span",
                            attributes: {},
                            expression: None,
                            range: 1:1-1:7,
                        },
                    ),
                    Ok(
                        Text {
                            value: "Item: ",
                            range: 1:7-1:13,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "item.title",
                            expression_range: 1:14-1:24,
                            range: 1:13-1:25,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "span",
                            name_range: 1:27-1:31,
                            range: 1:25-1:32,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_mixed_tag_and_text_expressions() {
        check(
            "<div {className}>Content: {content}</div>",
            expect![[r#"
                [
                    Ok(
                        OpeningTag {
                            self_closing: false,
                            name_range: 1:2-1:5,
                            value: "div",
                            attributes: {},
                            expression: Some(
                                (
                                    "className",
                                    1:7-1:16,
                                ),
                            ),
                            range: 1:1-1:18,
                        },
                    ),
                    Ok(
                        Text {
                            value: "Content: ",
                            range: 1:18-1:27,
                        },
                    ),
                    Ok(
                        Expression {
                            value: "content",
                            expression_range: 1:28-1:35,
                            range: 1:27-1:36,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 1:38-1:41,
                            range: 1:36-1:42,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_error() {
        check(
            r#"<div class="foo" class="bar"></div>"#,
            expect![[r#"
                [
                    Err(
                        ParseError {
                            message: "Duplicate attribute 'class'",
                            range: 1:18-1:29,
                        },
                    ),
                    Ok(
                        Text {
                            value: ">",
                            range: 1:29-1:30,
                        },
                    ),
                    Ok(
                        ClosingTag {
                            value: "div",
                            name_range: 1:32-1:35,
                            range: 1:30-1:36,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_different_quotes() {
        check(
            r#"<input type="text" type='number'/>"#,
            expect![[r#"
                [
                    Err(
                        ParseError {
                            message: "Duplicate attribute 'type'",
                            range: 1:20-1:33,
                        },
                    ),
                    Ok(
                        Text {
                            value: "/>",
                            range: 1:33-1:35,
                        },
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_no_value() {
        check(
            r#"<input required required />"#,
            expect![[r#"
                [
                    Err(
                        ParseError {
                            message: "Duplicate attribute 'required'",
                            range: 1:17-1:25,
                        },
                    ),
                    Ok(
                        Text {
                            value: " />",
                            range: 1:25-1:28,
                        },
                    ),
                ]"#]],
        );
    }
}
