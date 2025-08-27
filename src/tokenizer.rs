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
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let tokenizer = Tokenizer::new(input);
        let result: Vec<_> = tokenizer.collect();
        let actual = format!("{:#?}", result);
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_tokenize_empty() {
        check(
            "",
            expect!["[]"],
        );
    }

    #[test]
    fn test_tokenize_input_with_attributes() {
        check(
            r#"<input type="" value="" disabled="">"#,
            expect![[r#"
                [
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:7,
                                value: "input",
                                attributes: [
                                    Attribute {
                                        name: "type",
                                        value: "",
                                        range: 1:8-1:15,
                                        value_range: 1:14-1:14,
                                    },
                                    Attribute {
                                        name: "value",
                                        value: "",
                                        range: 1:16-1:24,
                                        value_range: 1:23-1:23,
                                    },
                                    Attribute {
                                        name: "disabled",
                                        value: "",
                                        range: 1:25-1:36,
                                        value_range: 1:35-1:35,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-1:37,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [
                                    Attribute {
                                        name: "foo",
                                        value: "bar",
                                        range: 1:5-1:14,
                                        value_range: 1:10-1:13,
                                    },
                                    Attribute {
                                        name: "x",
                                        value: "y",
                                        range: 1:14-1:19,
                                        value_range: 1:17-1:18,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-1:20,
                        ),
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
                        (
                            StartTag {
                                self_closing: true,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [
                                    Attribute {
                                        name: "foo",
                                        value: "",
                                        range: 1:5-1:8,
                                        value_range: 1:1-1:1,
                                    },
                                    Attribute {
                                        name: "bar",
                                        value: "",
                                        range: 1:9-1:12,
                                        value_range: 1:1-1:1,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-1:14,
                        ),
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
                        (
                            StartTag {
                                self_closing: true,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [
                                    Attribute {
                                        name: "foo",
                                        value: "",
                                        range: 1:5-1:8,
                                        value_range: 1:1-1:1,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-1:10,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:3,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:4,
                        ),
                    ),
                    Ok(
                        (
                            Comment,
                            1:4-1:12,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            1:12-2:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 2:3-2:4,
                            },
                            2:1-2:5,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            2:5-3:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:10,
                                value: "textarea",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:11,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t",
                            },
                            1:11-2:2,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:3-2:6,
                                value: "div",
                                attributes: [],
                                expression: None,
                            },
                            2:2-2:7,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "div",
                                name_range: 2:9-2:12,
                            },
                            2:7-2:13,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            2:13-3:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "textarea",
                                name_range: 3:3-3:11,
                            },
                            3:1-3:12,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            3:12-4:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:7,
                                value: "title",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:8,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: true,
                                name_range: 1:9-1:19,
                                value: "slot-title",
                                attributes: [],
                                expression: None,
                            },
                            1:8-1:21,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "title",
                                name_range: 1:23-1:28,
                            },
                            1:21-1:29,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:3,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:4,
                        ),
                    ),
                    Ok(
                        (
                            Comment,
                            1:4-1:12,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 1:14-1:15,
                            },
                            1:12-1:16,
                        ),
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
                        (
                            Comment,
                            1:1-1:42,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:3,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:4,
                        ),
                    ),
                    Ok(
                        (
                            Comment,
                            1:4-5:10,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 5:12-5:13,
                            },
                            5:10-5:14,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:14-6:1,
                        ),
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
                        (
                            Comment,
                            1:1-1:64,
                        ),
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
                        (
                            Doctype,
                            1:1-1:18,
                        ),
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
                        (
                            EndTag {
                                value: "div",
                                name_range: 1:3-1:6,
                            },
                            1:1-1:8,
                        ),
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
                        (
                            StartTag {
                                self_closing: true,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:6,
                        ),
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
                        (
                            StartTag {
                                self_closing: true,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:7,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:7,
                                value: "style",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:8,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n  body { color: red; }\n  .class { font-size: 12px; }\n",
                            },
                            1:8-4:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "style",
                                name_range: 4:3-4:8,
                            },
                            4:1-4:9,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:9-5:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:5,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h1",
                                name_range: 1:7-1:9,
                            },
                            1:5-1:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            1:10-2:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:2-2:4,
                                value: "h2",
                                attributes: [],
                                expression: None,
                            },
                            2:1-2:5,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h2",
                                name_range: 2:7-2:9,
                            },
                            2:5-2:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            2:10-3:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 3:2-3:4,
                                value: "h3",
                                attributes: [],
                                expression: None,
                            },
                            3:1-3:5,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h3",
                                name_range: 3:7-3:9,
                            },
                            3:5-3:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            3:10-4:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 4:2-4:4,
                                value: "h4",
                                attributes: [],
                                expression: None,
                            },
                            4:1-4:5,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h4",
                                name_range: 4:7-4:9,
                            },
                            4:5-4:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:10-5:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 5:2-5:4,
                                value: "h5",
                                attributes: [],
                                expression: None,
                            },
                            5:1-5:5,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h5",
                                name_range: 5:7-5:9,
                            },
                            5:5-5:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:10-6:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 6:2-6:4,
                                value: "h6",
                                attributes: [],
                                expression: None,
                            },
                            6:1-6:5,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h6",
                                name_range: 6:7-6:9,
                            },
                            6:5-6:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            6:10-7:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "div",
                                attributes: [
                                    Attribute {
                                        name: "class",
                                        value: "multiline",
                                        range: 2:3-2:20,
                                        value_range: 2:10-2:19,
                                    },
                                    Attribute {
                                        name: "id",
                                        value: "test",
                                        range: 3:3-3:12,
                                        value_range: 3:7-3:11,
                                    },
                                    Attribute {
                                        name: "data-value",
                                        value: "something",
                                        range: 4:3-4:25,
                                        value_range: 4:15-4:24,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-4:26,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:26-5:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "div",
                                name_range: 5:3-5:6,
                            },
                            5:1-5:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:7-6:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:8,
                                value: "script",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:9,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n",
                            },
                            1:9-4:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "script",
                                name_range: 4:3-4:9,
                            },
                            4:1-4:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:10-5:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:11,
                                value: "hop-x-raw",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:12,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "foo bar",
                            },
                            1:12-1:19,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "hop-x-raw",
                                name_range: 1:21-1:30,
                            },
                            1:19-1:31,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:11,
                                value: "hop-x-raw",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:12,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n  <div>some html</div>\n",
                            },
                            1:12-3:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "hop-x-raw",
                                name_range: 3:3-3:12,
                            },
                            3:1-3:13,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            3:13-4:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:3,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:4,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 1:6-1:7,
                            },
                            1:4-1:8,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:9-1:10,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:8-1:11,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 1:13-1:14,
                            },
                            1:11-1:15,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:11,
                                value: "main-comp",
                                attributes: [],
                                expression: Some(
                                    (
                                        "foo",
                                        1:13-1:16,
                                    ),
                                ),
                            },
                            1:1-1:18,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t",
                            },
                            1:18-2:2,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:3-2:9,
                                value: "script",
                                attributes: [],
                                expression: None,
                            },
                            2:2-2:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t\tconst x = \"<div></div>\";\n\t",
                            },
                            2:10-4:2,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "script",
                                name_range: 4:4-4:10,
                            },
                            4:2-4:11,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:11-5:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "main-comp",
                                name_range: 5:3-5:12,
                            },
                            5:1-5:13,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:13-6:1,
                        ),
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
                        (
                            Doctype,
                            1:1-1:16,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            1:16-2:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:2-2:6,
                                value: "html",
                                attributes: [],
                                expression: None,
                            },
                            2:1-2:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            2:7-3:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 3:2-3:6,
                                value: "head",
                                attributes: [],
                                expression: None,
                            },
                            3:1-3:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n  ",
                            },
                            3:7-4:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 4:4-4:9,
                                value: "title",
                                attributes: [],
                                expression: None,
                            },
                            4:3-4:10,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "My Page",
                            },
                            4:10-4:17,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "title",
                                name_range: 4:19-4:24,
                            },
                            4:17-4:25,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:25-5:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "head",
                                name_range: 5:3-5:7,
                            },
                            5:1-5:8,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:8-6:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 6:2-6:6,
                                value: "body",
                                attributes: [],
                                expression: None,
                            },
                            6:1-6:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n  ",
                            },
                            6:7-7:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 7:4-7:7,
                                value: "div",
                                attributes: [
                                    Attribute {
                                        name: "class",
                                        value: "container",
                                        range: 7:8-7:25,
                                        value_range: 7:15-7:24,
                                    },
                                ],
                                expression: None,
                            },
                            7:3-7:26,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n    Hello, world!\n  ",
                            },
                            7:26-9:3,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "div",
                                name_range: 9:5-9:8,
                            },
                            9:3-9:9,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            9:9-10:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "body",
                                name_range: 10:3-10:7,
                            },
                            10:1-10:8,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            10:8-11:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "html",
                                name_range: 11:3-11:7,
                            },
                            11:1-11:8,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            11:8-12:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "svg",
                                attributes: [
                                    Attribute {
                                        name: "xmlns",
                                        value: "http://www.w3.org/2000/svg",
                                        range: 1:6-1:40,
                                        value_range: 1:13-1:39,
                                    },
                                    Attribute {
                                        name: "width",
                                        value: "24",
                                        range: 1:41-1:51,
                                        value_range: 1:48-1:50,
                                    },
                                    Attribute {
                                        name: "height",
                                        value: "24",
                                        range: 1:52-1:63,
                                        value_range: 1:60-1:62,
                                    },
                                    Attribute {
                                        name: "viewBox",
                                        value: "0 0 24 24",
                                        range: 1:64-1:83,
                                        value_range: 1:73-1:82,
                                    },
                                    Attribute {
                                        name: "fill",
                                        value: "none",
                                        range: 1:84-1:95,
                                        value_range: 1:90-1:94,
                                    },
                                    Attribute {
                                        name: "stroke",
                                        value: "currentColor",
                                        range: 1:96-1:117,
                                        value_range: 1:104-1:116,
                                    },
                                    Attribute {
                                        name: "stroke-width",
                                        value: "2",
                                        range: 1:118-1:134,
                                        value_range: 1:132-1:133,
                                    },
                                    Attribute {
                                        name: "stroke-linecap",
                                        value: "round",
                                        range: 1:135-1:157,
                                        value_range: 1:151-1:156,
                                    },
                                    Attribute {
                                        name: "stroke-linejoin",
                                        value: "round",
                                        range: 1:158-1:181,
                                        value_range: 1:175-1:180,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-1:182,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            1:182-2:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:2-2:6,
                                value: "line",
                                attributes: [
                                    Attribute {
                                        name: "x1",
                                        value: "16.5",
                                        range: 2:7-2:16,
                                        value_range: 2:11-2:15,
                                    },
                                    Attribute {
                                        name: "y1",
                                        value: "9.4",
                                        range: 2:17-2:25,
                                        value_range: 2:21-2:24,
                                    },
                                    Attribute {
                                        name: "x2",
                                        value: "7.5",
                                        range: 2:26-2:34,
                                        value_range: 2:30-2:33,
                                    },
                                    Attribute {
                                        name: "y2",
                                        value: "4.21",
                                        range: 2:35-2:44,
                                        value_range: 2:39-2:43,
                                    },
                                ],
                                expression: None,
                            },
                            2:1-2:45,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "line",
                                name_range: 2:47-2:51,
                            },
                            2:45-2:52,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            2:52-3:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 3:2-3:6,
                                value: "path",
                                attributes: [
                                    Attribute {
                                        name: "d",
                                        value: "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z",
                                        range: 3:7-3:132,
                                        value_range: 3:10-3:131,
                                    },
                                ],
                                expression: None,
                            },
                            3:1-3:133,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "path",
                                name_range: 3:135-3:139,
                            },
                            3:133-3:140,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            3:140-4:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 4:2-4:10,
                                value: "polyline",
                                attributes: [
                                    Attribute {
                                        name: "points",
                                        value: "3.27 6.96 12 12.01 20.73 6.96",
                                        range: 4:11-4:49,
                                        value_range: 4:19-4:48,
                                    },
                                ],
                                expression: None,
                            },
                            4:1-4:50,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "polyline",
                                name_range: 4:52-4:60,
                            },
                            4:50-4:61,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            4:61-5:1,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 5:2-5:6,
                                value: "line",
                                attributes: [
                                    Attribute {
                                        name: "x1",
                                        value: "12",
                                        range: 5:7-5:14,
                                        value_range: 5:11-5:13,
                                    },
                                    Attribute {
                                        name: "y1",
                                        value: "22.08",
                                        range: 5:15-5:25,
                                        value_range: 5:19-5:24,
                                    },
                                    Attribute {
                                        name: "x2",
                                        value: "12",
                                        range: 5:26-5:33,
                                        value_range: 5:30-5:32,
                                    },
                                    Attribute {
                                        name: "y2",
                                        value: "12",
                                        range: 5:34-5:41,
                                        value_range: 5:38-5:40,
                                    },
                                ],
                                expression: None,
                            },
                            5:1-5:42,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "line",
                                name_range: 5:44-5:48,
                            },
                            5:42-5:49,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:49-6:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "svg",
                                name_range: 6:3-6:6,
                            },
                            6:1-6:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            6:7-7:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "svg",
                                attributes: [
                                    Attribute {
                                        name: "xmlns",
                                        value: "http://www.w3.org/2000/svg",
                                        range: 1:6-1:40,
                                        value_range: 1:13-1:39,
                                    },
                                    Attribute {
                                        name: "width",
                                        value: "128",
                                        range: 1:41-1:52,
                                        value_range: 1:48-1:51,
                                    },
                                    Attribute {
                                        name: "height",
                                        value: "128",
                                        range: 1:53-1:65,
                                        value_range: 1:61-1:64,
                                    },
                                    Attribute {
                                        name: "version",
                                        value: "1.1",
                                        range: 1:66-1:79,
                                        value_range: 1:75-1:78,
                                    },
                                    Attribute {
                                        name: "viewBox",
                                        value: "0 0 128 128",
                                        range: 1:80-1:101,
                                        value_range: 1:89-1:100,
                                    },
                                    Attribute {
                                        name: "class",
                                        value: "size-12",
                                        range: 1:102-1:117,
                                        value_range: 1:109-1:116,
                                    },
                                ],
                                expression: None,
                            },
                            1:1-1:118,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t",
                            },
                            1:118-2:2,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:3-2:4,
                                value: "g",
                                attributes: [
                                    Attribute {
                                        name: "style",
                                        value: "fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;",
                                        range: 2:5-2:112,
                                        value_range: 2:12-2:111,
                                    },
                                ],
                                expression: None,
                            },
                            2:2-2:113,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t\t",
                            },
                            2:113-3:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 3:4-3:8,
                                value: "path",
                                attributes: [
                                    Attribute {
                                        name: "d",
                                        value: "M20.04 38 64 22l43.96 16L64 54Z",
                                        range: 3:9-3:44,
                                        value_range: 3:12-3:43,
                                    },
                                ],
                                expression: None,
                            },
                            3:3-3:45,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "path",
                                name_range: 3:47-3:51,
                            },
                            3:45-3:52,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t\t",
                            },
                            3:52-4:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 4:4-4:8,
                                value: "path",
                                attributes: [
                                    Attribute {
                                        name: "d",
                                        value: "M17.54 47.09v48l35.099 12.775",
                                        range: 4:9-4:42,
                                        value_range: 4:12-4:41,
                                    },
                                ],
                                expression: None,
                            },
                            4:3-4:43,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "path",
                                name_range: 4:45-4:49,
                            },
                            4:43-4:50,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t\t",
                            },
                            4:50-5:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 5:4-5:8,
                                value: "path",
                                attributes: [
                                    Attribute {
                                        name: "d",
                                        value: "M64 112V64l46.46-16.91v48L77.988 106.91",
                                        range: 5:9-5:52,
                                        value_range: 5:12-5:51,
                                    },
                                ],
                                expression: None,
                            },
                            5:3-5:53,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "path",
                                name_range: 5:55-5:59,
                            },
                            5:53-5:60,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t",
                            },
                            5:60-6:2,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "g",
                                name_range: 6:4-6:5,
                            },
                            6:2-6:6,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            6:6-7:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "svg",
                                name_range: 7:3-7:6,
                            },
                            7:1-7:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            7:7-8:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:11,
                                value: "main-comp",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:12,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t",
                            },
                            1:12-2:2,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 2:3-2:7,
                                value: "form",
                                attributes: [
                                    Attribute {
                                        name: "id",
                                        value: "form",
                                        range: 2:8-2:17,
                                        value_range: 2:12-2:16,
                                    },
                                ],
                                expression: None,
                            },
                            2:2-2:18,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t\t",
                            },
                            2:18-3:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 3:4-3:9,
                                value: "input",
                                attributes: [
                                    Attribute {
                                        name: "type",
                                        value: "text",
                                        range: 3:10-3:21,
                                        value_range: 3:16-3:20,
                                    },
                                    Attribute {
                                        name: "required",
                                        value: "",
                                        range: 3:22-3:30,
                                        value_range: 1:1-1:1,
                                    },
                                ],
                                expression: None,
                            },
                            3:3-3:31,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t\t",
                            },
                            3:31-4:3,
                        ),
                    ),
                    Ok(
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 4:4-4:10,
                                value: "button",
                                attributes: [
                                    Attribute {
                                        name: "type",
                                        value: "submit",
                                        range: 4:11-4:24,
                                        value_range: 4:17-4:23,
                                    },
                                ],
                                expression: None,
                            },
                            4:3-4:25,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "Send",
                            },
                            4:25-4:29,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "button",
                                name_range: 4:31-4:37,
                            },
                            4:29-4:38,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n\t",
                            },
                            4:38-5:2,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "form",
                                name_range: 5:4-5:8,
                            },
                            5:2-5:9,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            5:9-6:1,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "main-comp",
                                name_range: 6:3-6:12,
                            },
                            6:1-6:13,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "\n",
                            },
                            6:13-7:1,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:4,
                                value: "if",
                                attributes: [],
                                expression: Some(
                                    (
                                        "foo",
                                        1:6-1:9,
                                    ),
                                ),
                            },
                            1:1-1:11,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "div",
                                attributes: [
                                    Attribute {
                                        name: "class",
                                        value: "test",
                                        range: 1:6-1:18,
                                        value_range: 1:13-1:17,
                                    },
                                ],
                                expression: Some(
                                    (
                                        "bar",
                                        1:20-1:23,
                                    ),
                                ),
                            },
                            1:1-1:25,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:4,
                                value: "if",
                                attributes: [],
                                expression: Some(
                                    (
                                        "user.name == 'John'",
                                        1:6-1:25,
                                    ),
                                ),
                            },
                            1:1-1:27,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:11,
                                value: "component",
                                attributes: [],
                                expression: Some(
                                    (
                                        "obj.prop.subprop",
                                        1:13-1:29,
                                    ),
                                ),
                            },
                            1:1-1:31,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:8,
                                value: "button",
                                attributes: [
                                    Attribute {
                                        name: "disabled",
                                        value: "",
                                        range: 1:9-1:17,
                                        value_range: 1:1-1:1,
                                    },
                                ],
                                expression: Some(
                                    (
                                        "enabled == 'yes'",
                                        1:19-1:35,
                                    ),
                                ),
                            },
                            1:1-1:37,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:7,
                                value: "input",
                                attributes: [],
                                expression: Some(
                                    (
                                        "variable_name_123",
                                        1:9-1:26,
                                    ),
                                ),
                            },
                            1:1-1:28,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "div",
                                attributes: [
                                    Attribute {
                                        name: "class",
                                        value: "test",
                                        range: 1:6-1:18,
                                        value_range: 1:13-1:17,
                                    },
                                ],
                                expression: Some(
                                    (
                                        "  user.name  ",
                                        1:20-1:33,
                                    ),
                                ),
                            },
                            1:1-1:35,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:6,
                                value: "span",
                                attributes: [],
                                expression: Some(
                                    (
                                        "'hello world'",
                                        1:8-1:21,
                                    ),
                                ),
                            },
                            1:1-1:23,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:6,
                                value: "form",
                                attributes: [],
                                expression: Some(
                                    (
                                        "(user.role == 'admin')",
                                        1:8-1:30,
                                    ),
                                ),
                            },
                            1:1-1:32,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:9,
                                value: "section",
                                attributes: [],
                                expression: Some(
                                    (
                                        "a == b == c",
                                        1:11-1:22,
                                    ),
                                ),
                            },
                            1:1-1:24,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "for",
                                attributes: [],
                                expression: Some(
                                    (
                                        "user in users",
                                        1:7-1:20,
                                    ),
                                ),
                            },
                            1:1-1:22,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "for",
                                attributes: [],
                                expression: Some(
                                    (
                                        "item in user.items",
                                        1:7-1:25,
                                    ),
                                ),
                            },
                            1:1-1:27,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "div",
                                attributes: [],
                                expression: Some(
                                    (
                                        "foo in bars",
                                        1:7-1:18,
                                    ),
                                ),
                            },
                            1:1-1:20,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:4,
                                value: "h1",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:5,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "Hello ",
                            },
                            1:5-1:11,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "name",
                                range: 1:12-1:16,
                            },
                            1:11-1:17,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "!",
                            },
                            1:17-1:18,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h1",
                                name_range: 1:20-1:22,
                            },
                            1:18-1:23,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:3,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:4,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "User ",
                            },
                            1:4-1:9,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "user.name",
                                range: 1:10-1:19,
                            },
                            1:9-1:20,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: " has ",
                            },
                            1:20-1:25,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "user.count",
                                range: 1:26-1:36,
                            },
                            1:25-1:37,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: " items",
                            },
                            1:37-1:43,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 1:45-1:46,
                            },
                            1:43-1:47,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:6,
                                value: "span",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:7,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "greeting",
                                range: 1:8-1:16,
                            },
                            1:7-1:17,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: " world!",
                            },
                            1:17-1:24,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "span",
                                name_range: 1:26-1:30,
                            },
                            1:24-1:31,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "div",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:6,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "Price: ",
                            },
                            1:6-1:13,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "price",
                                range: 1:14-1:19,
                            },
                            1:13-1:20,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "div",
                                name_range: 1:22-1:25,
                            },
                            1:20-1:26,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:4,
                                value: "h2",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:5,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "title",
                                range: 1:6-1:11,
                            },
                            1:5-1:12,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "h2",
                                name_range: 1:14-1:16,
                            },
                            1:12-1:17,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:3,
                                value: "p",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:4,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "Status: ",
                            },
                            1:4-1:12,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "user.profile.status == 'active'",
                                range: 1:13-1:44,
                            },
                            1:12-1:45,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "p",
                                name_range: 1:47-1:48,
                            },
                            1:45-1:49,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:6,
                                value: "span",
                                attributes: [],
                                expression: None,
                            },
                            1:1-1:7,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "Item: ",
                            },
                            1:7-1:13,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "item.title",
                                range: 1:14-1:24,
                            },
                            1:13-1:25,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "span",
                                name_range: 1:27-1:31,
                            },
                            1:25-1:32,
                        ),
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
                        (
                            StartTag {
                                self_closing: false,
                                name_range: 1:2-1:5,
                                value: "div",
                                attributes: [],
                                expression: Some(
                                    (
                                        "className",
                                        1:7-1:16,
                                    ),
                                ),
                            },
                            1:1-1:18,
                        ),
                    ),
                    Ok(
                        (
                            Text {
                                value: "Content: ",
                            },
                            1:18-1:27,
                        ),
                    ),
                    Ok(
                        (
                            Expression {
                                value: "content",
                                range: 1:28-1:35,
                            },
                            1:27-1:36,
                        ),
                    ),
                    Ok(
                        (
                            EndTag {
                                value: "div",
                                name_range: 1:38-1:41,
                            },
                            1:36-1:42,
                        ),
                    ),
                ]"#]],
        );
    }
}
