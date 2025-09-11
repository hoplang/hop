use std::fmt;

use crate::document::document_cursor::DocumentRange;

#[derive(Debug, Clone)]
pub enum DopToken {
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

impl PartialEq for DopToken {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DopToken::Identifier(a), DopToken::Identifier(b)) => a.as_str() == b.as_str(),
            (DopToken::StringLiteral(a), DopToken::StringLiteral(b)) => a == b,
            (DopToken::BooleanLiteral(a), DopToken::BooleanLiteral(b)) => a == b,
            (DopToken::NumberLiteral(a), DopToken::NumberLiteral(b)) => a == b,
            (DopToken::Equal, DopToken::Equal) => true,
            (DopToken::Not, DopToken::Not) => true,
            (DopToken::Dot, DopToken::Dot) => true,
            (DopToken::LeftParen, DopToken::LeftParen) => true,
            (DopToken::RightParen, DopToken::RightParen) => true,
            (DopToken::LeftBracket, DopToken::LeftBracket) => true,
            (DopToken::RightBracket, DopToken::RightBracket) => true,
            (DopToken::LeftBrace, DopToken::LeftBrace) => true,
            (DopToken::RightBrace, DopToken::RightBrace) => true,
            (DopToken::Colon, DopToken::Colon) => true,
            (DopToken::Comma, DopToken::Comma) => true,
            (DopToken::In, DopToken::In) => true,
            (DopToken::TypeString, DopToken::TypeString) => true,
            (DopToken::TypeNumber, DopToken::TypeNumber) => true,
            (DopToken::TypeBoolean, DopToken::TypeBoolean) => true,
            (DopToken::TypeArray, DopToken::TypeArray) => true,
            _ => false,
        }
    }
}

impl DopToken {
    pub fn opposite_token(&self) -> DopToken {
        match self {
            DopToken::LeftBrace => DopToken::RightBrace,
            DopToken::LeftBracket => DopToken::RightBracket,
            DopToken::LeftParen => DopToken::RightParen,
            _ => {
                panic!("matching_closing_token called on {}", self)
            }
        }
    }
}

impl fmt::Display for DopToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopToken::Identifier(name) => write!(f, "{}", name),
            DopToken::StringLiteral(s) => write!(f, "'{}'", s),
            DopToken::BooleanLiteral(b) => write!(f, "{}", b),
            DopToken::NumberLiteral(n) => write!(f, "{}", n),
            DopToken::Equal => write!(f, "=="),
            DopToken::Not => write!(f, "!"),
            DopToken::Dot => write!(f, "."),
            DopToken::LeftParen => write!(f, "("),
            DopToken::RightParen => write!(f, ")"),
            DopToken::LeftBracket => write!(f, "["),
            DopToken::RightBracket => write!(f, "]"),
            DopToken::LeftBrace => write!(f, "{{"),
            DopToken::RightBrace => write!(f, "}}"),
            DopToken::Colon => write!(f, ":"),
            DopToken::Comma => write!(f, ","),
            DopToken::In => write!(f, "in"),
            DopToken::TypeString => write!(f, "string"),
            DopToken::TypeNumber => write!(f, "number"),
            DopToken::TypeBoolean => write!(f, "boolean"),
            DopToken::TypeArray => write!(f, "array"),
        }
    }
}
