use crate::common::{Attribute, Position, Range, Token, TokenType};

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenizerState {
    Text,
    TagOpen,
    TagName,
    BeforeAttributeName,
    AttributeName,
    AfterAttributeName,
    BeforeAttributeValue,
    AttributeValueDoubleQuoted,
    AttributeValueSingleQuoted,
    AttributeValueUnquoted,
    AfterAttributeValueQuoted,
    SelfClosingStartTag,
    CommentStart,
    Comment,
    CommentEnd,
    Doctype,
    DoctypeName,
}

struct Cursor {
    input: Vec<char>,
    position: usize,
    line: i32,
    column: i32,
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
        self.input.get(self.position).copied().unwrap_or('\0')
    }

    fn advance(&mut self) {
        if self.position < self.input.len() {
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
        Position::new(self.line, self.column)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn match_str(&mut self, s: &str) -> bool {
        let chars: Vec<char> = s.chars().collect();
        if self.position + chars.len() > self.input.len() {
            return false;
        }

        for (i, &ch) in chars.iter().enumerate() {
            if self.input[self.position + i] != ch {
                return false;
            }
        }

        self.advance_n(chars.len());
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
            token_start: Position { line: 1, column: 1 },
            token_attributes: Vec::new(),
            attribute_name: String::new(),
            attribute_value: String::new(),
            attribute_start: Position { line: 1, column: 1 },
            tokens: Vec::new(),
        }
    }

    fn push_current_token(&mut self, cursor: &Cursor) {
        self.tokens.push(Token {
            token_type: self.token_type,
            value: self.token_value.clone(),
            attributes: self.token_attributes.clone(),
            range: Range {
                start: self.token_start,
                end: cursor.get_position(),
            },
        });
        self.token_type = TokenType::Text;
        self.token_value.clear();
        self.token_attributes.clear();
        self.token_start = cursor.get_position();
    }

    fn push_current_attribute(&mut self, cursor: &Cursor) {
        self.token_attributes.push(Attribute {
            name: self.attribute_name.clone(),
            value: self.attribute_value.clone(),
            range: Range {
                start: self.attribute_start,
                end: cursor.get_position(),
            },
        });
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

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

fn is_alpha(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_alnum(ch: char) -> bool {
    ch.is_ascii_alphanumeric()
}

use std::fmt;

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Doctype => write!(f, "Doctype"),
            TokenType::StartTag => write!(f, "StartTag"),
            TokenType::EndTag => write!(f, "EndTag"),
            TokenType::SelfClosingTag => write!(f, "SelfClosingTag"),
            TokenType::Text => write!(f, "Text"),
            TokenType::Comment => write!(f, "Comment"),
            TokenType::Error => write!(f, "Error"),
        }
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
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
                }
            }

            TokenizerState::TagOpen => {
                if ch == '!' {
                    cursor.advance();
                    if cursor.match_str("--") {
                        state = TokenizerState::CommentStart;
                    } else if cursor.match_str("DOCTYPE") || cursor.match_str("doctype") {
                        builder.set_current_token_type(TokenType::Doctype);
                        state = TokenizerState::Doctype;
                    } else {
                        cursor.advance();
                        builder.push_error_token("Invalid markup declaration", &cursor);
                        state = TokenizerState::Text;
                    }
                } else if ch == '/' {
                    cursor.advance();
                    builder.set_current_token_type(TokenType::EndTag);
                    state = TokenizerState::TagName;
                } else if is_alpha(ch) {
                    builder.set_current_token_type(TokenType::StartTag);
                    state = TokenizerState::TagName;
                } else {
                    builder.append_to_current_token_value("<");
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::TagName => {
                if is_alnum(ch) || ch == '-' || ch == '_' || ch == ':' {
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                } else if is_whitespace(ch) {
                    stored_tag_name = builder.get_current_token_value().to_string();
                    cursor.advance();
                    state = TokenizerState::BeforeAttributeName;
                } else if ch == '>' {
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if ch == '/' {
                    stored_tag_name = builder.get_current_token_value().to_string();
                    cursor.advance();
                    state = TokenizerState::SelfClosingStartTag;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character in tag name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::BeforeAttributeName => {
                if is_whitespace(ch) {
                    cursor.advance();
                } else if ch == '>' {
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if ch == '/' {
                    cursor.advance();
                    state = TokenizerState::SelfClosingStartTag;
                } else if is_alpha(ch) || ch == '_' || ch == ':' {
                    builder.set_current_attribute_start(cursor.get_position());
                    state = TokenizerState::AttributeName;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character before attribute name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::AttributeName => {
                if is_alnum(ch) || ch == '-' || ch == '_' || ch == ':' || ch == '.' {
                    builder.append_to_current_attribute_name(&ch.to_string());
                    cursor.advance();
                } else if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::AfterAttributeName;
                } else if ch == '=' {
                    cursor.advance();
                    state = TokenizerState::BeforeAttributeValue;
                } else if ch == '>' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if ch == '/' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::SelfClosingStartTag;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character in attribute name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::AfterAttributeName => {
                if is_whitespace(ch) {
                    cursor.advance();
                } else if ch == '=' {
                    cursor.advance();
                    state = TokenizerState::BeforeAttributeValue;
                } else if ch == '>' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if ch == '/' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::SelfClosingStartTag;
                } else if is_alpha(ch) || ch == '_' || ch == ':' {
                    builder.push_current_attribute(&cursor);
                    builder.set_current_attribute_start(cursor.get_position());
                    state = TokenizerState::AttributeName;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character after attribute name", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::BeforeAttributeValue => {
                if is_whitespace(ch) {
                    cursor.advance();
                } else if ch == '"' {
                    cursor.advance();
                    state = TokenizerState::AttributeValueDoubleQuoted;
                } else if ch == '\'' {
                    cursor.advance();
                    state = TokenizerState::AttributeValueSingleQuoted;
                } else if ch == '>' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    state = TokenizerState::AttributeValueUnquoted;
                }
            }

            TokenizerState::AttributeValueDoubleQuoted => {
                if ch == '"' {
                    cursor.advance();
                    state = TokenizerState::AfterAttributeValueQuoted;
                } else {
                    builder.append_to_current_attribute_value(&ch.to_string());
                    cursor.advance();
                }
            }

            TokenizerState::AttributeValueSingleQuoted => {
                if ch == '\'' {
                    cursor.advance();
                    state = TokenizerState::AfterAttributeValueQuoted;
                } else {
                    builder.append_to_current_attribute_value(&ch.to_string());
                    cursor.advance();
                }
            }

            TokenizerState::AttributeValueUnquoted => {
                if is_whitespace(ch) {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::BeforeAttributeName;
                } else if ch == '>' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if ch == '/' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::SelfClosingStartTag;
                } else {
                    builder.append_to_current_attribute_value(&ch.to_string());
                    cursor.advance();
                }
            }

            TokenizerState::AfterAttributeValueQuoted => {
                if is_whitespace(ch) {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::BeforeAttributeName;
                } else if ch == '>' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else if ch == '/' {
                    builder.push_current_attribute(&cursor);
                    cursor.advance();
                    state = TokenizerState::SelfClosingStartTag;
                } else {
                    cursor.advance();
                    builder.push_error_token(
                        "Invalid character after quoted attribute value",
                        &cursor,
                    );
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::SelfClosingStartTag => {
                if ch == '>' {
                    builder.set_current_token_type(TokenType::SelfClosingTag);
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    cursor.advance();
                    builder.push_error_token("Expected '>' after '/'", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::CommentStart => {
                builder.set_current_token_type(TokenType::Comment);
                state = TokenizerState::Comment;
            }

            TokenizerState::Comment => {
                if cursor.match_str("-->") {
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    builder.append_to_current_token_value(&ch.to_string());
                    cursor.advance();
                }
            }

            TokenizerState::Doctype => {
                if is_whitespace(ch) {
                    cursor.advance();
                    state = TokenizerState::DoctypeName;
                } else if ch == '>' {
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    cursor.advance();
                    builder.push_error_token("Invalid character in DOCTYPE", &cursor);
                    state = TokenizerState::Text;
                }
            }

            TokenizerState::DoctypeName => {
                if is_whitespace(ch) {
                    if !doctype_name_buffer.is_empty() {
                        builder.append_to_current_token_value(&doctype_name_buffer);
                        doctype_name_buffer.clear();
                    }
                    cursor.advance();
                } else if ch == '>' {
                    if !doctype_name_buffer.is_empty() {
                        builder.append_to_current_token_value(&doctype_name_buffer);
                        doctype_name_buffer.clear();
                    }
                    cursor.advance();
                    builder.push_current_token(&cursor);
                    state = TokenizerState::Text;
                } else {
                    doctype_name_buffer.push(ch);
                    cursor.advance();
                }
            }

            TokenizerState::CommentEnd => {
                // This state is not used in the current implementation
                // but included for completeness
                cursor.advance();
                state = TokenizerState::Text;
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
                    "{} {}:{}-{}:{}",
                    token.token_type, start.line, start.column, end.line, end.column
                )
            }
            TokenType::EndTag => {
                format!(
                    "{}({}) {}:{}-{}:{}",
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
                    "{}({}) [{}] {}:{}-{}:{}",
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
                format!("{}", token.token_type)
            }
        }
    }

    #[test]
    fn test_tokenizer_with_txtar_files() {
        let test_data_dir = Path::new("test_data");

        if !test_data_dir.exists() {
            panic!("test_data directory not found");
        }

        let entries = fs::read_dir(test_data_dir).expect("Failed to read test_data directory");

        for entry in entries {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            if path.extension().map_or(false, |ext| ext == "txtar") {
                let file_name = path.file_name().unwrap().to_string_lossy();

                println!("Running test for: {}", file_name);

                let content = fs::read_to_string(&path)
                    .expect(&format!("Failed to read file: {}", path.display()));

                let archive = Archive::from(&content);

                let input_html = archive.get("input.html").unwrap().content.clone();
                let expected_tokens: Vec<&str> = archive
                    .get("tokens.txt")
                    .unwrap()
                    .content
                    .trim()
                    .split('\n')
                    .collect();

                let tokens = tokenize(&input_html);
                let actual_tokens: Vec<String> = tokens.iter().map(format_token).collect();

                assert_eq!(
                    actual_tokens, expected_tokens,
                    "Mismatch in file: {}",
                    file_name
                );
            }
        }
    }
}
