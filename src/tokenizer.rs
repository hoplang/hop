use std::mem;

use crate::common::{Attribute, Position, Range, RangeError};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Doctype,
    Comment,
    Eof,
    Expression {
        value: String,
        range: Range,
    },
    StartTag {
        self_closing: bool,
        name_range: Range,
        value: String,
        attributes: Vec<Attribute>,
        expression: Option<(String, Range)>,
    },
    EndTag {
        value: String,
        name_range: Range,
    },
    Text {
        value: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenizerState {
    Text,
    TagOpen,
    StartTagName,
    EndTagOpen,
    EndTagName,
    AfterEndTagName,
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

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            let ch = self.input[self.position];
            let byte_len = ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += byte_len;
            }
            self.position += 1;
        }
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    fn get_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
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

    fn advance(&mut self) -> Result<(Token, Range), RangeError> {
        let mut token_value = String::new();
        let token_start = self.cursor.get_position();
        let mut token_attributes = Vec::new();
        let mut token_expression = None;
        let mut tag_name_start = self.cursor.get_position();
        let mut tag_name_end = self.cursor.get_position();
        let mut expression_content = String::new();
        let mut expression_start = self.cursor.get_position();
        let mut attribute_name = String::new();
        let mut attribute_value = String::new();
        let mut attribute_start = self.cursor.get_position();
        let mut attribute_value_start = self.cursor.get_position();
        let mut doctype_name_buffer = String::new();
        while !self.cursor.is_at_end() {
            let ch = self.cursor.peek();

            match self.state {
                TokenizerState::Text => {
                    if ch == '<' {
                        if !token_value.is_empty() {
                            return Ok((
                                Token::Text { value: token_value },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        }
                        self.cursor.advance();
                        tag_name_start = self.cursor.get_position();
                        self.state = TokenizerState::TagOpen;
                    } else if ch == '{' {
                        if !token_value.is_empty() {
                            return Ok((
                                Token::Text { value: token_value },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        }
                        self.cursor.advance();
                        expression_start = self.cursor.get_position();
                        self.state = TokenizerState::TextExpressionContent;
                    } else {
                        token_value.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::TagOpen => {
                    if ch.is_ascii_alphabetic() {
                        token_value.push(ch);
                        self.cursor.advance();
                        tag_name_end = self.cursor.get_position();
                        self.state = TokenizerState::StartTagName;
                    } else if ch == '/' {
                        self.cursor.advance();
                        tag_name_start = self.cursor.get_position();
                        self.state = TokenizerState::EndTagOpen;
                    } else if ch == '!' {
                        self.cursor.advance();
                        self.state = TokenizerState::MarkupDeclaration;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character after '<'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::StartTagName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        token_value.push(ch);
                        self.cursor.advance();
                        tag_name_end = self.cursor.get_position();
                        self.state = TokenizerState::StartTagName;
                    } else if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch == '{' {
                        self.cursor.advance();
                        expression_start = self.cursor.get_position();
                        self.state = TokenizerState::TagExpressionContent;
                    } else if ch == '>' {
                        if is_tag_name_with_raw_content(&token_value) {
                            self.stored_tag_name = token_value.clone();
                            self.cursor.advance();
                            self.state = TokenizerState::RawtextData;
                            return Ok((
                                Token::StartTag {
                                    value: token_value,
                                    self_closing: false,
                                    attributes: token_attributes,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                    expression: token_expression,
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        } else {
                            self.cursor.advance();
                            self.state = TokenizerState::Text;
                            return Ok((
                                Token::StartTag {
                                    value: token_value,
                                    self_closing: false,
                                    attributes: token_attributes,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                    expression: token_expression,
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        }
                    } else if ch == '/' {
                        self.cursor.advance();
                        self.state = TokenizerState::SelfClosing;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character after '<'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::EndTagOpen => {
                    if ch.is_ascii_alphabetic() {
                        token_value.push(ch);
                        self.cursor.advance();
                        tag_name_end = self.cursor.get_position();
                        self.state = TokenizerState::EndTagName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character after '</'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::EndTagName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        token_value.push(ch);
                        self.cursor.advance();
                        tag_name_end = self.cursor.get_position();
                        self.state = TokenizerState::EndTagName;
                    } else if ch == '>' {
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Ok((
                            Token::EndTag {
                                value: token_value,
                                name_range: Range::new(tag_name_start, tag_name_end),
                            },
                            Range::new(token_start, self.cursor.get_position()),
                        ));
                    } else if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::AfterEndTagName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character in end tag name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::AfterEndTagName => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::AfterEndTagName;
                    } else if ch == '>' {
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Ok((
                            Token::EndTag {
                                value: token_value,
                                name_range: Range::new(tag_name_start, tag_name_end),
                            },
                            Range::new(token_start, self.cursor.get_position()),
                        ));
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character after end tag name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::BeforeAttrName => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch.is_ascii_alphabetic() {
                        attribute_start = self.cursor.get_position();
                        attribute_name.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::AttrName;
                    } else if ch == '{' {
                        self.cursor.advance();
                        expression_start = self.cursor.get_position();
                        self.state = TokenizerState::TagExpressionContent;
                    } else if ch == '/' {
                        self.cursor.advance();
                        self.state = TokenizerState::SelfClosing;
                    } else if ch == '>' {
                        if is_tag_name_with_raw_content(&token_value) {
                            self.stored_tag_name = token_value.clone();
                            self.cursor.advance();
                            self.state = TokenizerState::RawtextData;
                            return Ok((
                                Token::StartTag {
                                    self_closing: false,
                                    value: token_value,
                                    attributes: token_attributes,
                                    expression: token_expression,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        } else {
                            self.cursor.advance();
                            self.state = TokenizerState::Text;
                            return Ok((
                                Token::StartTag {
                                    self_closing: false,
                                    value: token_value,
                                    attributes: token_attributes,
                                    expression: token_expression,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        }
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character before attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::AttrName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        attribute_name.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::AttrName;
                    } else if ch == '=' {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrValue;
                    } else if ch.is_whitespace() {
                        // Push current attribute
                        token_attributes.push(Attribute::new(
                            mem::take(&mut attribute_name),
                            mem::take(&mut attribute_value),
                            Range {
                                start: attribute_start,
                                end: self.cursor.get_position(),
                            },
                            Range::default(),
                        ));
                        attribute_start = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch == '>' {
                        // Push current attribute
                        token_attributes.push(Attribute::new(
                            mem::take(&mut attribute_name),
                            mem::take(&mut attribute_value),
                            Range {
                                start: attribute_start,
                                end: self.cursor.get_position(),
                            },
                            Range::default(),
                        ));
                        if is_tag_name_with_raw_content(&token_value) {
                            self.stored_tag_name = token_value.clone();
                            self.cursor.advance();
                            self.state = TokenizerState::RawtextData;
                            return Ok((
                                Token::StartTag {
                                    self_closing: false,
                                    value: token_value,
                                    attributes: token_attributes,
                                    expression: token_expression,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        } else {
                            self.cursor.advance();
                            self.state = TokenizerState::Text;
                            return Ok((
                                Token::StartTag {
                                    self_closing: false,
                                    value: token_value,
                                    attributes: token_attributes,
                                    expression: token_expression,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        }
                    } else if ch == '/' {
                        // Push current attribute
                        token_attributes.push(Attribute::new(
                            mem::take(&mut attribute_name),
                            mem::take(&mut attribute_value),
                            Range {
                                start: attribute_start,
                                end: self.cursor.get_position(),
                            },
                            Range::default(),
                        ));
                        self.cursor.advance();
                        self.state = TokenizerState::SelfClosing;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character in attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::BeforeAttrValue => {
                    if ch == '"' {
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueDoubleQuote;
                        attribute_value_start = self.cursor.get_position();
                    } else if ch == '\'' {
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueSingleQuote;
                        attribute_value_start = self.cursor.get_position();
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Expected quoted attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::AttrValueDoubleQuote => {
                    if ch == '"' {
                        let attribute_value_end = self.cursor.get_position();
                        self.cursor.advance();
                        // Push current attribute
                        token_attributes.push(Attribute::new(
                            mem::take(&mut attribute_name),
                            mem::take(&mut attribute_value),
                            Range {
                                start: attribute_start,
                                end: self.cursor.get_position(),
                            },
                            Range::new(attribute_value_start, attribute_value_end),
                        ));
                        attribute_start = self.cursor.get_position();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        attribute_value.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueDoubleQuote;
                    }
                }

                TokenizerState::AttrValueSingleQuote => {
                    if ch == '\'' {
                        let attribute_value_end = self.cursor.get_position();
                        self.cursor.advance();
                        // Push current attribute
                        token_attributes.push(Attribute::new(
                            mem::take(&mut attribute_name),
                            mem::take(&mut attribute_value),
                            Range {
                                start: attribute_start,
                                end: self.cursor.get_position(),
                            },
                            Range::new(attribute_value_start, attribute_value_end),
                        ));
                        attribute_start = self.cursor.get_position();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        attribute_value.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueSingleQuote;
                    }
                }

                TokenizerState::SelfClosing => {
                    if ch == '>' {
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Ok((
                            Token::StartTag {
                                self_closing: true,
                                value: token_value,
                                attributes: token_attributes,
                                expression: token_expression,
                                name_range: Range::new(tag_name_start, tag_name_end),
                            },
                            Range::new(token_start, self.cursor.get_position()),
                        ));
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Expected '>' after '/'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
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
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid markup declaration".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::Comment => {
                    if self.cursor.match_str("-->") {
                        self.cursor.advance_n(3);
                        self.state = TokenizerState::Text;
                        return Ok((
                            Token::Comment,
                            Range::new(token_start, self.cursor.get_position()),
                        ));
                    } else {
                        token_value.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::Comment;
                    }
                }

                TokenizerState::Doctype => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeDoctypeName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Expected whitespace after DOCTYPE".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::BeforeDoctypeName => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeDoctypeName;
                    } else if ch.is_ascii_alphabetic() {
                        doctype_name_buffer.clear();
                        doctype_name_buffer.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::DoctypeName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Expected DOCTYPE name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::DoctypeName => {
                    if ch.is_ascii_alphabetic() {
                        doctype_name_buffer.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::DoctypeName;
                    } else if ch == '>' {
                        if doctype_name_buffer.to_lowercase() == "html" {
                            for ch in doctype_name_buffer.chars() {
                                token_value.push(ch);
                            }
                            self.cursor.advance();
                            self.state = TokenizerState::Text;
                            return Ok((
                                Token::Doctype,
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        } else {
                            let start_pos = self.cursor.get_position();
                            self.cursor.advance();
                            self.state = TokenizerState::Text;
                            return Err(RangeError::new(
                                "Invalid DOCTYPE name".to_string(),
                                Range::new(start_pos, self.cursor.get_position()),
                            ));
                        }
                    } else {
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Err(RangeError::new(
                            "Invalid character in DOCTYPE name".to_string(),
                            Range::new(self.cursor.get_position(), self.cursor.get_position()),
                        ));
                    }
                }

                TokenizerState::RawtextData => {
                    let end_tag = format!("</{}>", self.stored_tag_name);
                    if self.cursor.match_str(&end_tag) {
                        if !token_value.is_empty() {
                            self.state = TokenizerState::RawtextData;
                            return Ok((
                                Token::Text { value: token_value },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        } else {
                            // No accumulated content, create and return end tag token directly
                            let tag_name = self.stored_tag_name.clone();
                            self.cursor.advance_n(2); // consume </
                            tag_name_start = self.cursor.get_position();
                            self.cursor.advance_n(self.stored_tag_name.len()); // consume tag name 
                            tag_name_end = self.cursor.get_position();
                            self.cursor.advance_n(1); // consume >
                            self.state = TokenizerState::Text;
                            return Ok((
                                Token::EndTag {
                                    value: tag_name,
                                    name_range: Range::new(tag_name_start, tag_name_end),
                                },
                                Range::new(token_start, self.cursor.get_position()),
                            ));
                        }
                    } else {
                        token_value.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::RawtextData;
                    }
                }

                TokenizerState::TextExpressionContent => {
                    if ch == '}' {
                        let expression_range = Range {
                            start: expression_start,
                            end: self.cursor.get_position(),
                        };
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                        return Ok((
                            Token::Expression {
                                value: expression_content,
                                range: expression_range,
                            },
                            Range::new(token_start, self.cursor.get_position()),
                        ));
                    } else {
                        expression_content.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::TextExpressionContent;
                    }
                }

                TokenizerState::TagExpressionContent => {
                    if ch == '}' {
                        // Store expression on current token and continue parsing tag
                        token_expression = Some((
                            mem::take(&mut expression_content),
                            Range {
                                start: expression_start,
                                end: self.cursor.get_position(),
                            },
                        ));
                        expression_start = self.cursor.get_position();
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        expression_content.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::TagExpressionContent;
                    }
                }
            }
        }

        // End of input - return any accumulated token
        if !token_value.is_empty() {
            return Ok((
                Token::Text { value: token_value },
                Range::new(token_start, self.cursor.get_position()),
            ));
        }

        // No more tokens - return Eof token
        Ok((
            Token::Eof,
            Range::new(token_start, self.cursor.get_position()),
        ))
    }
}

impl Iterator for Tokenizer {
    type Item = Result<(Token, Range), RangeError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.advance() {
            Err(err) => Some(Err(err)),
            Ok((Token::Eof, _)) => None,
            Ok((t, range)) => Some(Ok((t, range))),
        }
    }
}

fn is_tag_name_with_raw_content(name: &str) -> bool {
    matches!(name, "script" | "style" | "hop-x-raw")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::parse_test_cases;
    use pretty_assertions::assert_eq;

    use std::fs;
    use std::path::PathBuf;

    fn format_attr(attr: &Attribute) -> String {
        format!("{}=[{}] {}", attr.name, attr.value, attr.range,)
    }

    fn format_token(val: Result<(Token, Range), RangeError>) -> String {
        match val.unwrap() {
            (Token::Text { .. }, range) => {
                format!("Text {}", range)
            }
            (Token::Expression { value, .. }, token_range) => {
                format!("Expression [] {{{}}} {}", value, token_range)
            }
            (Token::Doctype, range) => {
                format!("Doctype {}", range)
            }
            (Token::Comment, range) => {
                format!("Comment {}", range)
            }
            (Token::Eof, range) => {
                format!("Eof {}", range)
            }
            (Token::EndTag { value, name_range }, range) => {
                format!("EndTag({} {}) {}", value, name_range, range)
            }
            (
                Token::StartTag {
                    attributes,
                    name_range,
                    expression,
                    self_closing,
                    value,
                },
                range,
            ) => {
                let attrs = attributes
                    .iter()
                    .map(format_attr)
                    .collect::<Vec<_>>()
                    .join(" ");

                let expr_part = if let Some((expr_string, range)) = &expression {
                    format!(" {{{}, {}}}", expr_string, *range)
                } else {
                    String::new()
                };

                format!(
                    "{}({} {}) [{}]{} {}",
                    match self_closing {
                        true => "SelfClosingTag",
                        false => "StartTag",
                    },
                    value,
                    name_range,
                    attrs,
                    expr_part,
                    range
                )
            }
        }
    }

    #[test]
    fn test_tokenizer() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/tokenizer.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (archive, line_number)) in test_cases.iter().enumerate() {
            let input = archive
                .get("in")
                .expect("Missing 'in' section in test case")
                .content
                .trim();
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();

            let actual = Tokenizer::new(input)
                .map(format_token)
                .collect::<Vec<_>>()
                .join("\n");

            assert_eq!(
                actual,
                expected,
                "Mismatch in test case {} (line {}), left = actual, right = expected",
                case_num + 1,
                line_number
            );
        }
    }
}
