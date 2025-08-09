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
        let chars: Vec<char> = s.chars().collect();
        if self.position + chars.len() > self.input.len() {
            return false;
        }

        for (i, &ch) in chars.iter().enumerate() {
            if self.input[self.position + i] != ch {
                return false;
            }
        }
        true
    }
}

struct TokenBuilder {
    token_value: String,
    token_kind: TokenKind,
    token_start: Position,
    token_attributes: Vec<Attribute>,
    attribute_name: String,
    attribute_value: String,
    attribute_start: Position,
    tokens: Vec<Token>,
}

impl TokenBuilder {
    fn new() -> Self {
        Self {
            token_value: String::new(),
            token_kind: TokenKind::Text,
            token_start: Position { line: 1, column: 1 },
            token_attributes: Vec::new(),
            attribute_name: String::new(),
            attribute_value: String::new(),
            attribute_start: Position { line: 1, column: 1 },
            tokens: Vec::new(),
        }
    }

    fn push_current_token(&mut self, cursor: &Cursor) {
        self.tokens.push(Token::new(
            self.token_kind,
            mem::take(&mut self.token_value),
            mem::take(&mut self.token_attributes),
            Range {
                start: self.token_start,
                end: cursor.get_position(),
            },
        ));
        self.token_kind = TokenKind::Text;
        self.token_start = cursor.get_position();
    }

    fn push_current_attribute(&mut self, cursor: &Cursor) {
        self.token_attributes.push(Attribute::new(
            mem::take(&mut self.attribute_name),
            mem::take(&mut self.attribute_value),
            Range {
                start: self.attribute_start,
                end: cursor.get_position(),
            },
        ));
        self.attribute_start = cursor.get_position();
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

    fn reset(&mut self, cursor: &Cursor) {
        self.token_value = String::new();
        self.token_attributes = Vec::new();
        self.token_kind = TokenKind::Text;
        self.token_start = cursor.get_position();
        self.attribute_start = cursor.get_position();
    }

    fn get_tokens(self) -> Vec<Token> {
        self.tokens
    }
}

fn is_special_tag_name(name: &str) -> bool {
    matches!(name, "script" | "style" | "hop-raw")
}

fn is_alphabetic(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_alphanumeric_or_dash(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '-'
}

fn is_whitespace(ch: char) -> bool {
    ch.is_whitespace()
}

pub fn tokenize(input: &str, errors: &mut Vec<RangeError>) -> Vec<Token> {
    let mut cursor = Cursor::new(input);
    let mut builder = TokenBuilder::new();
    let mut state = TokenizerState::Text;
    let mut doctype_name_buffer = String::new();
    let mut stored_tag_name = String::new();

    while !cursor.is_at_end() {
        let ch = cursor.peek();

        match state {
            TokenizerState::Text => {
                if ch == '<' {
                    if !builder.get_current_token_value().is_empty() {
                        builder.push_current_token(&cursor);
                    }
                    cursor.advance();
                    state = TokenizerState::TagOpen;
                } else {
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::TagOpen => {
                if is_alphabetic(ch) {
                    builder.set_current_token_kind(TokenKind::StartTag);
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::StartTagName;
                } else if ch == '/' {
                    builder.set_current_token_kind(TokenKind::EndTag);
                    cursor.advance();
                    state = TokenizerState::EndTagOpen;
                } else if ch == '!' {
                    cursor.advance();
                    state = TokenizerState::MarkupDeclaration;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character after '<'".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::StartTagName => {
                if is_alphanumeric_or_dash(ch) {
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::StartTagName;
                } else if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::BeforeAttrName;
                } else if ch == '>' {
                    if is_special_tag_name(builder.get_current_token_value()) {
                        stored_tag_name = builder.get_current_token_value().to_string();
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::RawtextData;
                    } else {
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::Text;
                    }
                } else if ch == '/' {
                    builder.set_current_token_kind(TokenKind::SelfClosingTag);
                    cursor.advance();
                    state = TokenizerState::SelfClosing;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character after '<'".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::EndTagOpen => {
                if is_alphabetic(ch) {
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::EndTagName;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character after '</'".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::EndTagName => {
                if is_alphanumeric_or_dash(ch) {
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::EndTagName;
                } else if ch == '>' {
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::AfterEndTagName;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character in end tag name".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::AfterEndTagName => {
                if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::AfterEndTagName;
                } else if ch == '>' {
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character after end tag name".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::BeforeAttrName => {
                if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::BeforeAttrName;
                } else if is_alphabetic(ch) {
                    builder.set_current_attribute_start(cursor.get_position());
                    builder.append_to_current_attribute_name(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::AttrName;
                } else if ch == '/' {
                    builder.set_current_token_kind(TokenKind::SelfClosingTag);
                    cursor.advance();
                    state = TokenizerState::SelfClosing;
                } else if ch == '>' {
                    if is_special_tag_name(builder.get_current_token_value()) {
                        stored_tag_name = builder.get_current_token_value().to_string();
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::RawtextData;
                    } else {
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::Text;
                    }
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character before attribute name".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::AttrName => {
                if is_alphanumeric_or_dash(ch) {
                    builder.append_to_current_attribute_name(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::AttrName;
                } else if ch == '=' {
                    cursor.advance();
                    state = TokenizerState::BeforeAttrValue;
                } else if is_whitespace(ch) {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::BeforeAttrName;
                } else if ch == '>' {
                    builder.push_current_attribute(&cursor);
                    if is_special_tag_name(builder.get_current_token_value()) {
                        stored_tag_name = builder.get_current_token_value().to_string();
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::RawtextData;
                    } else {
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::Text;
                    }
                } else if ch == '/' {
                    builder.push_current_attribute(&cursor);
                    builder.set_current_token_kind(TokenKind::SelfClosingTag);
                    cursor.advance();
                    state = TokenizerState::SelfClosing;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid character in attribute name".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::BeforeAttrValue => {
                if ch == '"' {
                    cursor.advance();
                    state = TokenizerState::AttrValueDoubleQuote;
                } else if ch == '\'' {
                    cursor.advance();
                    state = TokenizerState::AttrValueSingleQuote;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Expected quoted attribute name".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::AttrValueDoubleQuote => {
                if ch == '"' {
                    cursor.advance();
                    builder.push_current_attribute(&cursor);
                    state = TokenizerState::BeforeAttrName;
                } else {
                    builder.append_to_current_attribute_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::AttrValueDoubleQuote;
                }
            }

            TokenizerState::AttrValueSingleQuote => {
                if ch == '\'' {
                    cursor.advance();
                    builder.push_current_attribute(&cursor);
                    state = TokenizerState::BeforeAttrName;
                } else {
                    builder.append_to_current_attribute_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::AttrValueSingleQuote;
                }
            }

            TokenizerState::SelfClosing => {
                if ch == '>' {
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Expected '>' after '/'".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::MarkupDeclaration => {
                if cursor.match_str("--") {
                    builder.set_current_token_kind(TokenKind::Comment);
                    cursor.advance_n(2);
                    state = TokenizerState::Comment;
                } else if cursor.match_str("DOCTYPE") {
                    builder.set_current_token_kind(TokenKind::Doctype);
                    cursor.advance_n(7);
                    state = TokenizerState::Doctype;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Invalid markup declaration".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::Comment => {
                if cursor.match_str("-->") {
                    cursor.advance_n(3);
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::Comment;
                }
            }

            TokenizerState::Doctype => {
                if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::BeforeDoctypeName;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Expected whitespace after DOCTYPE".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::BeforeDoctypeName => {
                if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::BeforeDoctypeName;
                } else if is_alphabetic(ch) {
                    doctype_name_buffer.clear();
                    doctype_name_buffer.push(ch);
                    cursor.advance();
                    state = TokenizerState::DoctypeName;
                } else {
                    let start_pos = cursor.get_position();
                    cursor.advance();
                    errors.push(RangeError::new(
                        "Expected DOCTYPE name".to_string(),
                        Range::new(start_pos, cursor.get_position()),
                    ));
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::DoctypeName => {
                if is_alphabetic(ch) {
                    doctype_name_buffer.push(ch);
                    cursor.advance();
                    state = TokenizerState::DoctypeName;
                } else if ch == '>' {
                    if doctype_name_buffer.to_lowercase() == "html" {
                        for ch in doctype_name_buffer.chars() {
                            builder.append_to_current_token_value(ch);
                        }
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::Text;
                    } else {
                        let start_pos = cursor.get_position();
                        cursor.advance();
                        // TODO: better range?
                        errors.push(RangeError::new(
                            "Invalid DOCTYPE name".to_string(),
                            Range::new(start_pos, cursor.get_position()),
                        ));
                        builder.reset(&cursor);
                        state = TokenizerState::Text;
                    }
                } else {
                    cursor.advance();
                    // "Invalid character in DOCTYPE name"
                    builder.reset(&cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::RawtextData => {
                let end_tag = format!("</{}>", stored_tag_name);
                if cursor.match_str(&end_tag) {
                    if !builder.get_current_token_value().is_empty() {
                        builder.push_current_token(&cursor);
                    }
                    builder.set_current_token_kind(TokenKind::EndTag);
                    for ch in stored_tag_name.chars() {
                        builder.append_to_current_token_value(ch);
                    }
                    cursor.advance_n(end_tag.len());
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    builder.append_to_current_token_value(ch);
                    cursor.advance();
                    state = TokenizerState::RawtextData;
                }
            }
        }
    }

    // Handle any remaining token
    if !builder.get_current_token_value().is_empty() {
        builder.push_current_token(&cursor);
    }

    builder.get_tokens()
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
            TokenKind::Text | TokenKind::Doctype | TokenKind::Comment => {
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
                format!(
                    "{:?}({}) [{}] {}",
                    token.kind,
                    token.value,
                    attrs,
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
