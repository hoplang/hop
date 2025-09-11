use std::fmt;

use crate::document::document_cursor::DocumentRange;

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(DocumentRange),
    StringLiteral(String),
    BooleanLiteral(bool),
    NumberLiteral(serde_json::Number),
    Equal,
    Not,
    Dot,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
    In,
    // Type tokens
    TypeString,
    TypeNumber,
    TypeBoolean,
    TypeArray,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Identifier(a), Token::Identifier(b)) => a.as_str() == b.as_str(),
            (Token::StringLiteral(a), Token::StringLiteral(b)) => a == b,
            (Token::BooleanLiteral(a), Token::BooleanLiteral(b)) => a == b,
            (Token::NumberLiteral(a), Token::NumberLiteral(b)) => a == b,
            (Token::Equal, Token::Equal) => true,
            (Token::Not, Token::Not) => true,
            (Token::Dot, Token::Dot) => true,
            (Token::LeftParen, Token::LeftParen) => true,
            (Token::RightParen, Token::RightParen) => true,
            (Token::LeftBracket, Token::LeftBracket) => true,
            (Token::RightBracket, Token::RightBracket) => true,
            (Token::LeftBrace, Token::LeftBrace) => true,
            (Token::RightBrace, Token::RightBrace) => true,
            (Token::Colon, Token::Colon) => true,
            (Token::Comma, Token::Comma) => true,
            (Token::In, Token::In) => true,
            (Token::TypeString, Token::TypeString) => true,
            (Token::TypeNumber, Token::TypeNumber) => true,
            (Token::TypeBoolean, Token::TypeBoolean) => true,
            (Token::TypeArray, Token::TypeArray) => true,
            _ => false,
        }
    }
}

impl Token {
    pub fn opposite_token(&self) -> Token {
        match self {
            Token::LeftBrace => Token::RightBrace,
            Token::LeftBracket => Token::RightBracket,
            Token::LeftParen => Token::RightParen,
            _ => {
                panic!("matching_closing_token called on {}", self)
            }
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(name) => write!(f, "{}", name),
            Token::StringLiteral(s) => write!(f, "'{}'", s),
            Token::BooleanLiteral(b) => write!(f, "{}", b),
            Token::NumberLiteral(n) => write!(f, "{}", n),
            Token::Equal => write!(f, "=="),
            Token::Not => write!(f, "!"),
            Token::Dot => write!(f, "."),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::In => write!(f, "in"),
            Token::TypeString => write!(f, "string"),
            Token::TypeNumber => write!(f, "number"),
            Token::TypeBoolean => write!(f, "boolean"),
            Token::TypeArray => write!(f, "array"),
        }
    }
}
