use std::fmt;

use crate::document::CheapString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(CheapString),
    TypeName(CheapString),
    StringLiteral(CheapString),
    IntLiteral(i64),
    FloatLiteral(f64),
    Comment(CheapString),
    Reserved(CheapString),
    Underscore,
    Assign,
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
    Asterisk,
    Not,
    Dot,
    DotDotEq,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Colon,
    ColonColon,
    Comma,
    Arrow,
    FatArrow,
    // Keywords
    True,
    False,
    In,
    Import,
    Record,
    Match,
    Enum,
    Entrypoint,
    Some,
    None,
    // Type tokens
    TypeString,
    TypeInt,
    TypeFloat,
    TypeBoolean,
    TypeTrustedHTML,
    TypeArray,
    TypeOption,
}

impl Token {
    pub fn opposite_token(&self) -> Token {
        match self {
            Token::LeftBrace => Token::RightBrace,
            Token::LeftBracket => Token::RightBracket,
            Token::LeftParen => Token::RightParen,
            _ => {
                panic!("opposite_token called on {}", self)
            }
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(name) => write!(f, "{}", name),
            Token::TypeName(name) => write!(f, "{}", name),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::IntLiteral(i) => write!(f, "{}", i),
            Token::FloatLiteral(float_val) => write!(f, "{}", float_val),
            Token::Comment(text) => write!(f, "{}", text),
            Token::Reserved(keyword) => write!(f, "{}", keyword),
            Token::Underscore => write!(f, "_"),
            Token::Assign => write!(f, "="),
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
            Token::Asterisk => write!(f, "*"),
            Token::Not => write!(f, "!"),
            Token::Dot => write!(f, "."),
            Token::DotDotEq => write!(f, "..="),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::ColonColon => write!(f, "::"),
            Token::Comma => write!(f, ","),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::In => write!(f, "in"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Import => write!(f, "import"),
            Token::Record => write!(f, "record"),
            Token::Match => write!(f, "match"),
            Token::Enum => write!(f, "enum"),
            Token::Entrypoint => write!(f, "entrypoint"),
            Token::Some => write!(f, "Some"),
            Token::None => write!(f, "None"),
            Token::TypeString => write!(f, "String"),
            Token::TypeInt => write!(f, "Int"),
            Token::TypeFloat => write!(f, "Float"),
            Token::TypeBoolean => write!(f, "Bool"),
            Token::TypeTrustedHTML => write!(f, "TrustedHTML"),
            Token::TypeArray => write!(f, "Array"),
            Token::TypeOption => write!(f, "Option"),
        }
    }
}
