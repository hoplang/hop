//! Tokenizer for declaration syntax (import and record declarations).

use itertools::Itertools as _;
use std::iter::Peekable;

use crate::document::document_cursor::{DocumentCursor, DocumentRange};
use crate::hop::parse_error::ParseError;

/// Tokens produced by the declaration tokenizer.
#[derive(Debug, Clone)]
pub enum Token {
    /// "import" keyword
    Import,
    /// "record" keyword
    Record,
    /// "from" keyword
    From,
    /// An identifier (component name, record name, field name)
    Identifier(DocumentRange),
    /// A quoted string literal (the path in import statements)
    String(DocumentRange),
    /// {
    LeftBrace,
    /// }
    RightBrace,
    /// :
    Colon,
    /// ,
    Comma,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
    /// End of input
    Eof,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Import, Token::Import) => true,
            (Token::Record, Token::Record) => true,
            (Token::From, Token::From) => true,
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::String(_), Token::String(_)) => true,
            (Token::LeftBrace, Token::LeftBrace) => true,
            (Token::RightBrace, Token::RightBrace) => true,
            (Token::Colon, Token::Colon) => true,
            (Token::Comma, Token::Comma) => true,
            (Token::LeftBracket, Token::LeftBracket) => true,
            (Token::RightBracket, Token::RightBracket) => true,
            (Token::Eof, Token::Eof) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Import => write!(f, "import"),
            Token::Record => write!(f, "record"),
            Token::From => write!(f, "from"),
            Token::Identifier(range) => write!(f, "{}", range.as_str()),
            Token::String(range) => write!(f, "\"{}\"", range.as_str()),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

/// Tokenizer for declaration syntax.
pub struct Tokenizer {
    iter: Peekable<DocumentCursor>,
    /// The original range being tokenized (used for EOF error reporting).
    full_range: DocumentRange,
}

impl Tokenizer {
    /// Create a new tokenizer from a document range.
    pub fn new(range: DocumentRange) -> Self {
        Self {
            iter: range.clone().cursor().peekable(),
            full_range: range,
        }
    }

    /// Skip whitespace characters.
    fn skip_whitespace(&mut self) {
        while self.iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            self.iter.next();
        }
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Result<(Token, DocumentRange), ParseError> {
        self.skip_whitespace();

        let Some(start) = self.iter.next() else {
            // Return an error with the full range for context
            return Err(ParseError::GenericError {
                message: "Unexpected end of input".to_string(),
                range: self.full_range.clone(),
            });
        };

        match start.ch() {
            '{' => Ok((Token::LeftBrace, start)),
            '}' => Ok((Token::RightBrace, start)),
            ':' => Ok((Token::Colon, start)),
            ',' => Ok((Token::Comma, start)),
            '[' => Ok((Token::LeftBracket, start)),
            ']' => Ok((Token::RightBracket, start)),
            '"' => self.tokenize_string(start),
            ch if ch.is_ascii_alphabetic() || ch == '_' => self.tokenize_identifier(start),
            ch => Err(ParseError::GenericError {
                message: format!("Unexpected character: '{}'", ch),
                range: start,
            }),
        }
    }

    /// Check if there are more tokens available.
    pub fn has_more(&mut self) -> bool {
        self.skip_whitespace();
        self.iter.peek().is_some()
    }

    /// Peek at the next token without consuming it.
    pub fn peek_token(&mut self) -> Result<Token, ParseError> {
        self.skip_whitespace();

        let Some(start) = self.iter.peek().cloned() else {
            return Ok(Token::Eof);
        };

        match start.ch() {
            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            ':' => Ok(Token::Colon),
            ',' => Ok(Token::Comma),
            '[' => Ok(Token::LeftBracket),
            ']' => Ok(Token::RightBracket),
            '"' => Ok(Token::String(start)), // Just indicate it's a string
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                // Need to look ahead to determine keyword vs identifier
                let mut check_iter = self.iter.clone();
                // Skip the first character (we already have it in `start`)
                check_iter.next();
                let identifier = start.extend(check_iter.peeking_take_while(|s| {
                    s.ch().is_ascii_alphanumeric() || s.ch() == '_'
                }));
                match identifier.as_str() {
                    "import" => Ok(Token::Import),
                    "record" => Ok(Token::Record),
                    "from" => Ok(Token::From),
                    _ => Ok(Token::Identifier(identifier)),
                }
            }
            _ => Ok(Token::Eof),
        }
    }

    /// Tokenize a string literal (the content inside quotes).
    fn tokenize_string(&mut self, open_quote: DocumentRange) -> Result<(Token, DocumentRange), ParseError> {
        let mut content_start: Option<DocumentRange> = None;
        let mut content_end: Option<DocumentRange> = None;

        while let Some(s) = self.iter.next_if(|s| s.ch() != '"') {
            if content_start.is_none() {
                content_start = Some(s.clone());
            }
            content_end = Some(s);
        }

        match self.iter.next() {
            Some(close_quote) => {
                // Return the content range (without quotes)
                let content_range = match (content_start, content_end) {
                    (Some(start), Some(end)) => start.to(end),
                    (Some(start), None) => start,
                    _ => {
                        // Empty string - return the range between quotes
                        // We need a valid range, so use open_quote position
                        return Err(ParseError::GenericError {
                            message: "Empty string literal".to_string(),
                            range: open_quote.to(close_quote),
                        });
                    }
                };
                Ok((Token::String(content_range), open_quote.to(close_quote)))
            }
            None => Err(ParseError::UnmatchedCharacter {
                ch: '"',
                range: open_quote,
            }),
        }
    }

    /// Tokenize an identifier or keyword.
    fn tokenize_identifier(&mut self, start: DocumentRange) -> Result<(Token, DocumentRange), ParseError> {
        let identifier = start.extend(
            self.iter
                .peeking_take_while(|s| s.ch().is_ascii_alphanumeric() || s.ch() == '_'),
        );

        let token = match identifier.as_str() {
            "import" => Token::Import,
            "record" => Token::Record,
            "from" => Token::From,
            _ => Token::Identifier(identifier.clone()),
        };

        Ok((token, identifier))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let range = DocumentCursor::new(input.to_string()).range();
        let mut tokenizer = Tokenizer::new(range);
        let mut tokens = Vec::new();

        while tokenizer.has_more() {
            match tokenizer.next_token() {
                Ok((token, _range)) => tokens.push(format!("{}", token)),
                Err(e) => tokens.push(format!("Error: {}", e)),
            }
        }

        expected.assert_eq(&tokens.join("\n"));
    }

    #[test]
    fn test_import_statement() {
        check(
            r#"import UserList from "@/user_list""#,
            expect![[r#"
                import
                UserList
                from
                "@/user_list""#]],
        );
    }

    #[test]
    fn test_record_statement() {
        check(
            "record User {name: String, age: Int}",
            expect![[r#"
                record
                User
                {
                name
                :
                String
                ,
                age
                :
                Int
                }"#]],
        );
    }

    #[test]
    fn test_record_with_array() {
        check(
            "record UserList {users: Array[User]}",
            expect![[r#"
                record
                UserList
                {
                users
                :
                Array
                [
                User
                ]
                }"#]],
        );
    }

    #[test]
    fn test_multiline_record() {
        check(
            "record User {\n    name: String,\n    age: Int,\n}",
            expect![[r#"
                record
                User
                {
                name
                :
                String
                ,
                age
                :
                Int
                ,
                }"#]],
        );
    }
}
