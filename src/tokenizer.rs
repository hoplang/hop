use crate::common::{Attribute, Position, Range, Token, TokenType};

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
    position: usize,
    line: usize,
    column: usize,
}

impl Cursor {
    fn new(input: String) -> Self {
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
            if self.input[self.position] == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
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
        Position::new(self.line as i32, self.column as i32)
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
    token_type: TokenType,
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
            token_type: TokenType::Text,
            token_start: Position::new(1, 1),
            token_attributes: Vec::new(),
            attribute_name: String::new(),
            attribute_value: String::new(),
            attribute_start: Position::new(1, 1),
            tokens: Vec::new(),
        }
    }

    fn push_current_token(&mut self, cursor: &Cursor) {
        self.tokens.push(Token::new(
            self.token_type,
            self.token_value.clone(),
            self.token_attributes.clone(),
            Range::new(self.token_start, cursor.get_position()),
        ));
        self.token_type = TokenType::Text;
        self.token_value.clear();
        self.token_attributes.clear();
        self.token_start = cursor.get_position();
    }

    fn push_current_attribute(&mut self, cursor: &Cursor) {
        self.token_attributes.push(Attribute::new(
            self.attribute_name.clone(),
            self.attribute_value.clone(),
            Range::new(self.attribute_start, cursor.get_position()),
        ));
        self.attribute_name.clear();
        self.attribute_value.clear();
        self.attribute_start = cursor.get_position();
    }

    fn append_to_current_token_value(&mut self, s: &str) {
        self.token_value.push_str(s);
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

    fn set_current_token_type(&mut self, token_type: TokenType) {
        self.token_type = token_type;
    }

    fn push_error_token(&mut self, message: &str, cursor: &Cursor) {
        self.token_type = TokenType::Error;
        self.token_value = message.to_string();
        self.push_current_token(cursor);
    }

    fn get_tokens(self) -> Vec<Token> {
        self.tokens
    }
}

fn is_special_tag_name(name: &str) -> bool {
    matches!(name, "textarea" | "title" | "script" | "style" | "template")
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

pub fn tokenize(input: String) -> Vec<Token> {
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
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::TagOpen => {
                if is_alphabetic(ch) {
                    builder.set_current_token_type(TokenType::StartTag);
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::StartTagName;
                } else if ch == '/' {
                    builder.set_current_token_type(TokenType::EndTag);
                    cursor.advance();
                    state = TokenizerState::EndTagOpen;
                } else if ch == '!' {
                    cursor.advance();
                    state = TokenizerState::MarkupDeclaration;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character after '<'", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::StartTagName => {
                if is_alphanumeric_or_dash(ch) {
                    builder.append_to_current_token_value(&ch.to_string());
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
                    builder.set_current_token_type(TokenType::SelfClosingTag);
                    cursor.advance();
                    state = TokenizerState::SelfClosing;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character in tag name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::EndTagOpen => {
                if is_alphabetic(ch) {
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::EndTagName;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character after '</'", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::EndTagName => {
                if is_alphanumeric_or_dash(ch) {
                    builder.append_to_current_token_value(&ch.to_string());
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
                    cursor.advance();
                    builder.push_error_token("Invalid character in end tag name", &cursor);
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
                    cursor.advance();
                    builder.push_error_token("Invalid character after end tag name", &cursor);
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
                    builder.set_current_token_type(TokenType::SelfClosingTag);
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
                    cursor.advance();
                    builder.push_error_token("Invalid character before attribute name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::AttrName => {
                if is_alphabetic(ch) || ch == '-' {
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
                    builder.set_current_token_type(TokenType::SelfClosingTag);
                    cursor.advance();
                    state = TokenizerState::SelfClosing;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character in attribute name", &cursor);
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
                    cursor.advance();
                    builder.push_error_token("Expected quoted attribute value", &cursor);
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
                    cursor.advance();
                    builder.push_error_token("Expected '>' after '/'", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::MarkupDeclaration => {
                if cursor.match_str("--") {
                    builder.set_current_token_type(TokenType::Comment);
                    cursor.advance_n(2);
                    state = TokenizerState::Comment;
                } else if cursor.match_str("DOCTYPE") {
                    builder.set_current_token_type(TokenType::Doctype);
                    cursor.advance_n(7);
                    state = TokenizerState::Doctype;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid markup declaration", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::Comment => {
                if cursor.match_str("-->") {
                    cursor.advance_n(3);
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::Comment;
                }
            }

            TokenizerState::Doctype => {
                if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::BeforeDoctypeName;
                } else {
                    cursor.advance();
                    builder.push_error_token("Expected whitespace after DOCTYPE", &cursor);
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
                    cursor.advance();
                    builder.push_error_token("Expected DOCTYPE name", &cursor);
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
                        builder.append_to_current_token_value(&doctype_name_buffer);
                        cursor.advance();
                        builder.push_current_token(&cursor);
                        state = TokenizerState::Text;
                    } else {
                        cursor.advance();
                        builder.push_error_token("Invalid DOCTYPE name", &cursor);
                        state = TokenizerState::Text;
                    }
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character in DOCTYPE name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::RawtextData => {
                let end_tag = format!("</{}>", stored_tag_name);
                if cursor.match_str(&end_tag) {
                    if !builder.get_current_token_value().is_empty() {
                        builder.push_current_token(&cursor);
                    }
                    builder.set_current_token_type(TokenType::EndTag);
                    builder.append_to_current_token_value(&stored_tag_name);
                    cursor.advance_n(end_tag.len());
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    builder.append_to_current_token_value(&ch.to_string());
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
    use std::path::Path;

    fn format_attr(attr: &Attribute) -> String {
        format!(
            "{}=[{}] {}:{}-{}:{}",
            attr.name,
            attr.value,
            attr.range.start.line,
            attr.range.start.column,
            attr.range.end.line,
            attr.range.end.column
        )
    }

    fn format_token(token: &Token) -> String {
        let start = &token.range.start;
        let end = &token.range.end;

        match token.token_type {
            TokenType::Text | TokenType::Doctype | TokenType::Comment => {
                format!(
                    "{:?} {}:{}-{}:{}",
                    token.token_type, start.line, start.column, end.line, end.column
                )
            }
            TokenType::EndTag => {
                format!(
                    "{:?}({}) {}:{}-{}:{}",
                    token.token_type, token.value, start.line, start.column, end.line, end.column
                )
            }
            TokenType::StartTag | TokenType::SelfClosingTag => {
                let attrs = token
                    .attributes
                    .iter()
                    .map(format_attr)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(
                    "{:?}({}) [{}] {}:{}-{}:{}",
                    token.token_type,
                    token.value,
                    attrs,
                    start.line,
                    start.column,
                    end.line,
                    end.column
                )
            }
            TokenType::Error => {
                format!("{:?}", token.token_type)
            }
        }
    }

    #[test]
    fn test_tokenizer_with_txtar_files() {
        let test_data_dir = Path::new("test_data/tokenizer");

        let entries = fs::read_dir(test_data_dir).expect("Failed to read test_data directory");

        for entry in entries {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            println!("Running test for: {}", file_name);

            let content = fs::read_to_string(&path)
                .expect(&format!("Failed to read file: {}", path.display()));

            let archive = Archive::from(&content);

            let input_html = archive.get("input.html").unwrap().content.trim();
            let expected_tokens: Vec<&str> = archive
                .get("tokens.txt")
                .unwrap()
                .content
                .trim()
                .split('\n')
                .collect();

            let tokens = tokenize(input_html.to_string());
            let actual_tokens: Vec<String> = tokens.iter().map(format_token).collect();

            assert_eq!(
                actual_tokens, expected_tokens,
                "Mismatch in file: {}",
                file_name
            );
        }
    }
}
