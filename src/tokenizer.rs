use std::mem;

use crate::common::{Attribute, Position, Range, RangeError, Token, TokenKind};

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

struct Tokenizer {
    token_value: String,
    token_kind: TokenKind,
    token_start: Position,
    token_attributes: Vec<Attribute>,
    token_expression: Option<(String, Range)>,
    expression_content: String,
    expression_start: Position,
    attribute_name: String,
    attribute_value: String,
    attribute_start: Position,
    tokens: Vec<Token>,
    cursor: Cursor,
    state: TokenizerState,
    doctype_name_buffer: String,
    stored_tag_name: String,
}

impl Tokenizer {
    fn new(input: &str) -> Self {
        let cursor = Cursor::new(input);
        Self {
            token_value: String::new(),
            token_kind: TokenKind::Text,
            token_start: cursor.get_position(),
            token_attributes: Vec::new(),
            token_expression: None,
            expression_content: String::new(),
            expression_start: cursor.get_position(),
            attribute_name: String::new(),
            attribute_value: String::new(),
            attribute_start: cursor.get_position(),
            tokens: Vec::new(),
            cursor,
            state: TokenizerState::Text,
            doctype_name_buffer: String::new(),
            stored_tag_name: String::new(),
        }
    }

    fn push_current_token(&mut self) {
        self.tokens.push(Token::new(
            self.token_kind,
            mem::take(&mut self.token_value),
            mem::take(&mut self.token_attributes),
            mem::take(&mut self.token_expression),
            Range {
                start: self.token_start,
                end: self.cursor.get_position(),
            },
        ));
        self.token_kind = TokenKind::Text;
        self.token_start = self.cursor.get_position();
    }

    fn push_current_attribute(&mut self) {
        self.token_attributes.push(Attribute::new(
            mem::take(&mut self.attribute_name),
            mem::take(&mut self.attribute_value),
            Range {
                start: self.attribute_start,
                end: self.cursor.get_position(),
            },
        ));
        self.attribute_start = self.cursor.get_position();
    }

    fn append_to_current_token_value(&mut self, ch: char) {
        self.token_value.push(ch);
    }

    fn append_to_current_attribute_name(&mut self, s: &str) {
        self.attribute_name.push_str(s);
    }

    fn append_to_current_attribute_value(&mut self, s: &str) {
        self.attribute_value.push_str(s);
    }

    fn set_current_attribute_start(&mut self, pos: Position) {
        self.attribute_start = pos;
    }

    fn get_current_token_value(&self) -> &str {
        &self.token_value
    }

    fn set_current_token_kind(&mut self, kind: TokenKind) {
        self.token_kind = kind;
    }

    fn reset(&mut self) {
        self.token_value = String::new();
        self.token_attributes = Vec::new();
        self.token_expression = None;
        self.expression_content = String::new();
        self.token_kind = TokenKind::Text;
        self.token_start = self.cursor.get_position();
        self.attribute_start = self.cursor.get_position();
    }

    fn append_to_expression(&mut self, ch: char) {
        self.expression_content.push(ch);
    }

    fn push_current_expression(&mut self) {
        self.token_expression = Some((
            mem::take(&mut self.expression_content),
            Range {
                start: self.expression_start,
                end: self.cursor.get_position(),
            },
        ));
        self.expression_start = self.cursor.get_position();
    }

    fn set_current_expression_start(&mut self, pos: Position) {
        self.expression_start = pos;
    }

    fn get_tokens(self) -> Vec<Token> {
        self.tokens
    }

    pub fn tokenize(&mut self, errors: &mut Vec<RangeError>) -> Vec<Token> {
        while !self.cursor.is_at_end() {
            let ch = self.cursor.peek();

            match self.state {
                TokenizerState::Text => {
                    if ch == '<' {
                        if !self.get_current_token_value().is_empty() {
                            self.push_current_token();
                        }
                        self.cursor.advance();
                        self.state = TokenizerState::TagOpen;
                    } else if ch == '{' {
                        if !self.get_current_token_value().is_empty() {
                            self.push_current_token();
                        }
                        self.cursor.advance();
                        self.set_current_expression_start(self.cursor.get_position());
                        self.state = TokenizerState::TextExpressionContent;
                    } else {
                        self.append_to_current_token_value(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::TagOpen => {
                    if ch.is_ascii_alphabetic() {
                        self.set_current_token_kind(TokenKind::StartTag);
                        self.append_to_current_token_value(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::StartTagName;
                    } else if ch == '/' {
                        self.set_current_token_kind(TokenKind::EndTag);
                        self.cursor.advance();
                        self.state = TokenizerState::EndTagOpen;
                    } else if ch == '!' {
                        self.cursor.advance();
                        self.state = TokenizerState::MarkupDeclaration;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character after '<'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::StartTagName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        self.append_to_current_token_value(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::StartTagName;
                    } else if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch == '{' {
                        self.cursor.advance();
                        self.set_current_expression_start(self.cursor.get_position());
                        self.state = TokenizerState::TagExpressionContent;
                    } else if ch == '>' {
                        if is_tag_name_with_raw_content(self.get_current_token_value()) {
                            self.stored_tag_name = self.get_current_token_value().to_string();
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::RawtextData;
                        } else {
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::Text;
                        }
                    } else if ch == '/' {
                        self.set_current_token_kind(TokenKind::SelfClosingTag);
                        self.cursor.advance();
                        self.state = TokenizerState::SelfClosing;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character after '<'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::EndTagOpen => {
                    if ch.is_ascii_alphabetic() {
                        self.append_to_current_token_value(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::EndTagName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character after '</'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::EndTagName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        self.append_to_current_token_value(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::EndTagName;
                    } else if ch == '>' {
                        self.cursor.advance();
                        self.push_current_token();
                        self.state = TokenizerState::Text;
                    } else if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::AfterEndTagName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character in end tag name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::AfterEndTagName => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::AfterEndTagName;
                    } else if ch == '>' {
                        self.cursor.advance();
                        self.push_current_token();
                        self.state = TokenizerState::Text;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character after end tag name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::BeforeAttrName => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch.is_ascii_alphabetic() {
                        self.set_current_attribute_start(self.cursor.get_position());
                        self.append_to_current_attribute_name(&ch.to_string());
                        self.cursor.advance();
                        self.state = TokenizerState::AttrName;
                    } else if ch == '{' {
                        self.cursor.advance();
                        self.set_current_expression_start(self.cursor.get_position());
                        self.state = TokenizerState::TagExpressionContent;
                    } else if ch == '/' {
                        self.set_current_token_kind(TokenKind::SelfClosingTag);
                        self.cursor.advance();
                        self.state = TokenizerState::SelfClosing;
                    } else if ch == '>' {
                        if is_tag_name_with_raw_content(self.get_current_token_value()) {
                            self.stored_tag_name = self.get_current_token_value().to_string();
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::RawtextData;
                        } else {
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::Text;
                        }
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character before attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::AttrName => {
                    if ch == '-' || ch.is_ascii_alphanumeric() {
                        self.append_to_current_attribute_name(&ch.to_string());
                        self.cursor.advance();
                        self.state = TokenizerState::AttrName;
                    } else if ch == '=' {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrValue;
                    } else if ch.is_whitespace() {
                        self.push_current_attribute();
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else if ch == '>' {
                        self.push_current_attribute();
                        if is_tag_name_with_raw_content(self.get_current_token_value()) {
                            self.stored_tag_name = self.get_current_token_value().to_string();
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::RawtextData;
                        } else {
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::Text;
                        }
                    } else if ch == '/' {
                        self.push_current_attribute();
                        self.set_current_token_kind(TokenKind::SelfClosingTag);
                        self.cursor.advance();
                        self.state = TokenizerState::SelfClosing;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid character in attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::BeforeAttrValue => {
                    if ch == '"' {
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueDoubleQuote;
                    } else if ch == '\'' {
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueSingleQuote;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Expected quoted attribute name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::AttrValueDoubleQuote => {
                    if ch == '"' {
                        self.cursor.advance();
                        self.push_current_attribute();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        self.append_to_current_attribute_value(&ch.to_string());
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueDoubleQuote;
                    }
                }

                TokenizerState::AttrValueSingleQuote => {
                    if ch == '\'' {
                        self.cursor.advance();
                        self.push_current_attribute();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        self.append_to_current_attribute_value(&ch.to_string());
                        self.cursor.advance();
                        self.state = TokenizerState::AttrValueSingleQuote;
                    }
                }

                TokenizerState::SelfClosing => {
                    if ch == '>' {
                        self.cursor.advance();
                        self.push_current_token();
                        self.state = TokenizerState::Text;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Expected '>' after '/'".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::MarkupDeclaration => {
                    if self.cursor.match_str("--") {
                        self.set_current_token_kind(TokenKind::Comment);
                        self.cursor.advance_n(2);
                        self.state = TokenizerState::Comment;
                    } else if self.cursor.match_str("DOCTYPE") {
                        self.set_current_token_kind(TokenKind::Doctype);
                        self.cursor.advance_n(7);
                        self.state = TokenizerState::Doctype;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Invalid markup declaration".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::Comment => {
                    if self.cursor.match_str("-->") {
                        self.cursor.advance_n(3);
                        self.push_current_token();
                        self.state = TokenizerState::Text;
                    } else {
                        self.append_to_current_token_value(ch);
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
                        errors.push(RangeError::new(
                            "Expected whitespace after DOCTYPE".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::BeforeDoctypeName => {
                    if ch.is_whitespace() {
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeDoctypeName;
                    } else if ch.is_ascii_alphabetic() {
                        self.doctype_name_buffer.clear();
                        self.doctype_name_buffer.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::DoctypeName;
                    } else {
                        let start_pos = self.cursor.get_position();
                        self.cursor.advance();
                        errors.push(RangeError::new(
                            "Expected DOCTYPE name".to_string(),
                            Range::new(start_pos, self.cursor.get_position()),
                        ));
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::DoctypeName => {
                    if ch.is_ascii_alphabetic() {
                        self.doctype_name_buffer.push(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::DoctypeName;
                    } else if ch == '>' {
                        if self.doctype_name_buffer.to_lowercase() == "html" {
                            let buffer = self.doctype_name_buffer.clone();
                            for ch in buffer.chars() {
                                self.append_to_current_token_value(ch);
                            }
                            self.cursor.advance();
                            self.push_current_token();
                            self.state = TokenizerState::Text;
                        } else {
                            let start_pos = self.cursor.get_position();
                            self.cursor.advance();
                            errors.push(RangeError::new(
                                "Invalid DOCTYPE name".to_string(),
                                Range::new(start_pos, self.cursor.get_position()),
                            ));
                            self.reset();
                            self.state = TokenizerState::Text;
                        }
                    } else {
                        self.cursor.advance();
                        self.reset();
                        self.state = TokenizerState::Text;
                    }
                }

                TokenizerState::RawtextData => {
                    let end_tag = format!("</{}>", self.stored_tag_name);
                    if self.cursor.match_str(&end_tag) {
                        if !self.get_current_token_value().is_empty() {
                            self.push_current_token();
                        }
                        self.set_current_token_kind(TokenKind::EndTag);
                        let tag_name = self.stored_tag_name.clone();
                        for ch in tag_name.chars() {
                            self.append_to_current_token_value(ch);
                        }
                        self.cursor.advance_n(end_tag.len());
                        self.push_current_token();
                        self.state = TokenizerState::Text;
                    } else {
                        self.append_to_current_token_value(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::RawtextData;
                    }
                }

                TokenizerState::TextExpressionContent => {
                    if ch == '}' {
                        self.push_current_expression();
                        self.cursor.advance();
                        self.set_current_token_kind(TokenKind::Expression);
                        self.push_current_token();
                        self.state = TokenizerState::Text;
                    } else {
                        self.append_to_expression(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::TextExpressionContent;
                    }
                }

                TokenizerState::TagExpressionContent => {
                    if ch == '}' {
                        self.push_current_expression();
                        self.cursor.advance();
                        self.state = TokenizerState::BeforeAttrName;
                    } else {
                        self.append_to_expression(ch);
                        self.cursor.advance();
                        self.state = TokenizerState::TagExpressionContent;
                    }
                }
            }
        }

        if !self.get_current_token_value().is_empty() {
            self.push_current_token();
        }

        mem::take(&mut self.tokens)
    }
}

fn is_tag_name_with_raw_content(name: &str) -> bool {
    matches!(name, "script" | "style" | "hop-x-raw")
}

pub fn tokenize(input: &str, errors: &mut Vec<RangeError>) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.tokenize(errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    fn format_range(range: Range) -> String {
        format!(
            "{}:{}-{}:{}",
            range.start.line, range.start.column, range.end.line, range.end.column
        )
    }

    fn format_attr(attr: &Attribute) -> String {
        format!(
            "{}=[{}] {}",
            attr.name,
            attr.value,
            format_range(attr.range),
        )
    }

    fn format_token(token: &Token) -> String {
        match token.kind {
            TokenKind::Text => {
                if let Some((expr_string, _)) = &token.expression {
                    let attrs = token
                        .attributes
                        .iter()
                        .map(format_attr)
                        .collect::<Vec<_>>()
                        .join(" ");
                    format!(
                        "{:?} [{}] {{{}}} {}",
                        token.kind,
                        attrs,
                        expr_string,
                        format_range(token.range)
                    )
                } else {
                    format!("{:?} {}", token.kind, format_range(token.range))
                }
            }
            TokenKind::Expression => {
                if let Some((expr_string, _)) = &token.expression {
                    let attrs = token
                        .attributes
                        .iter()
                        .map(format_attr)
                        .collect::<Vec<_>>()
                        .join(" ");
                    format!(
                        "{:?} [{}] {{{}}} {}",
                        token.kind,
                        attrs,
                        expr_string,
                        format_range(token.range)
                    )
                } else {
                    format!("{:?} {}", token.kind, format_range(token.range))
                }
            }
            TokenKind::Doctype | TokenKind::Comment => {
                format!("{:?} {}", token.kind, format_range(token.range))
            }
            TokenKind::EndTag => {
                format!(
                    "{:?}({}) {}",
                    token.kind,
                    token.value,
                    format_range(token.range)
                )
            }
            TokenKind::StartTag | TokenKind::SelfClosingTag => {
                let attrs = token
                    .attributes
                    .iter()
                    .map(format_attr)
                    .collect::<Vec<_>>()
                    .join(" ");

                let expr_part = if let Some((expr_string, range)) = &token.expression {
                    format!(" {{{}, {}}}", expr_string, format_range(*range))
                } else {
                    String::new()
                };

                format!(
                    "{:?}({}) [{}]{} {}",
                    token.kind,
                    token.value,
                    attrs,
                    expr_part,
                    format_range(token.range)
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

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

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
            let mut errors = Vec::new();

            let actual = tokenize(input, &mut errors)
                .iter()
                .map(format_token)
                .collect::<Vec<_>>()
                .join("\n");

            assert!(errors.is_empty());

            assert_eq!(
                actual,
                expected,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            if line == "## BEGIN" {
                assert!(
                    !in_case,
                    "Found '## BEGIN' at line {} while already inside a test case",
                    line_number
                );
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(
                    in_case,
                    "Found '## END' at line {} without matching '## BEGIN'",
                    line_number
                );
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(
            !in_case,
            "Reached end of file while inside a test case (missing '## END')"
        );

        test_cases
    }
}
