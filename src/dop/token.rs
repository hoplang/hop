use std::fmt;

use crate::document::document_cursor::DocumentRange;

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(DocumentRange),
    StringLiteral(String),
    BooleanLiteral(bool),
    IntLiteral(i64),
    FloatLiteral(f64),
    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    Plus,
    Minus,
    Multiply,
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
    TypeInt,
    TypeFloat,
    TypeBoolean,
    TypeTrustedHTML,
    TypeArray,
    TypeRecord,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Identifier(a), Token::Identifier(b)) => a.as_str() == b.as_str(),
            (Token::StringLiteral(a), Token::StringLiteral(b)) => a == b,
            (Token::BooleanLiteral(a), Token::BooleanLiteral(b)) => a == b,
            (Token::IntLiteral(a), Token::IntLiteral(b)) => a == b,
            (Token::FloatLiteral(a), Token::FloatLiteral(b)) => a == b,
            (Token::Eq, Token::Eq) => true,
            (Token::NotEq, Token::NotEq) => true,
            (Token::LessThan, Token::LessThan) => true,
            (Token::GreaterThan, Token::GreaterThan) => true,
            (Token::LessThanOrEqual, Token::LessThanOrEqual) => true,
            (Token::GreaterThanOrEqual, Token::GreaterThanOrEqual) => true,
            (Token::LogicalAnd, Token::LogicalAnd) => true,
            (Token::LogicalOr, Token::LogicalOr) => true,
            (Token::Plus, Token::Plus) => true,
            (Token::Minus, Token::Minus) => true,
            (Token::Multiply, Token::Multiply) => true,
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
            (Token::TypeInt, Token::TypeInt) => true,
            (Token::TypeFloat, Token::TypeFloat) => true,
            (Token::TypeBoolean, Token::TypeBoolean) => true,
            (Token::TypeTrustedHTML, Token::TypeTrustedHTML) => true,
            (Token::TypeArray, Token::TypeArray) => true,
            (Token::TypeRecord, Token::TypeRecord) => true,
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
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::BooleanLiteral(b) => write!(f, "{}", b),
            Token::IntLiteral(i) => write!(f, "{}", i),
            Token::FloatLiteral(float_val) => write!(f, "{}", float_val),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThanOrEqual => write!(f, "<="),
            Token::GreaterThanOrEqual => write!(f, ">="),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
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
            Token::TypeString => write!(f, "String"),
            Token::TypeInt => write!(f, "Int"),
            Token::TypeFloat => write!(f, "Float"),
            Token::TypeBoolean => write!(f, "Bool"),
            Token::TypeTrustedHTML => write!(f, "TrustedHTML"),
            Token::TypeArray => write!(f, "Array"),
            Token::TypeRecord => write!(f, "Record"),
        }
    }
}
