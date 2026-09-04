use std::fmt;

use crate::document::{CheapString, DocumentRange};

/// A token in the surface languge, outside of the markup tokenization mode.
#[derive(Debug, Clone, PartialEq)]
pub enum LangToken {
    Identifier(CheapString),
    TypeName(CheapString),
    StringLiteral(CheapString),
    IntLiteral(i32),
    FloatLiteral(f64),
    Comment(CheapString),
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
    DotDotDot,
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
    HashBracket,
    // Keywords
    True,
    False,
    In,
    Import,
    Record,
    Match,
    Enum,
    View,
    Page,
    Component,
    Fn,
    Pub,
    Some,
    None,
    // Type tokens
    TypeString,
    TypeInt,
    TypeFloat,
    TypeBoolean,
    TypeFragment,
    TypeArray,
    TypeOption,
}

/// A token in the markup tokenization mode.
#[derive(Debug)]
pub enum MarkupToken {
    /// An HTML comment. E.g.
    /// ```text
    /// <!-- hello -->
    /// ^^^^^^^^^^^^^^
    /// ```
    Comment { range: DocumentRange },
    /// The start of a tag. E.g.
    /// ```text
    /// <div class="foo">
    /// ^^^^
    /// ```
    OpeningTagStart {
        tag_name: DocumentRange,
        range: DocumentRange,
    },
    /// A closing tag, read whole. E.g.
    /// ```text
    /// </div>
    /// ^^^^^^
    /// ```
    ClosingTag {
        tag_name: DocumentRange,
        range: DocumentRange,
    },
    /// The opening tag of a fragment. E.g.
    /// ```text
    /// <>hello</>
    /// ^^
    /// ```
    FragmentStart { range: DocumentRange },
    /// The closing tag of a fragment. E.g.
    /// ```text
    /// <>hello</>
    ///        ^^^
    /// ```
    FragmentEnd { range: DocumentRange },
    /// Static text. E.g.
    /// ```text
    /// <div>hello world</div>
    ///      ^^^^^^^^^^^
    /// ```
    Text { range: DocumentRange },
    /// A newline in text position, kept out of the surrounding Text.
    Newline { range: DocumentRange },
    /// The `{` that opens an expression in text position. E.g.
    /// ```text
    /// <div>{x.to_string()}</div>
    ///      ^
    /// ```
    ExpressionStart { left_brace: DocumentRange },
}

/// A token that follows an MarkupToken::OpeningTagStart token.
pub enum TagToken {
    /// An attribute. E.g.
    /// ```text
    /// <div foo="bar">
    ///      ^^^^^^^^^
    /// ```
    /// The `value` field is `None` for value-less attributes.
    Attribute {
        name: DocumentRange,
        value: Option<AttributeString>,
    },
    /// The start of an expression-valued attribute. E.g.
    /// ```text
    /// <div foo={...}>
    ///      ^^^^^
    /// ```
    AttributeExpressionStart {
        name: DocumentRange,
        left_brace: DocumentRange,
    },
    /// A spread on a tag, e.g.
    /// ```text
    /// <div ...foo>
    ///      ^^^^^^
    /// ```
    Spread {
        name: DocumentRange,
        range: DocumentRange,
    },
    /// A `{` inside a tag, starting a tag header. E.g.
    /// ```text
    /// <if {true}>
    ///     ^
    /// ```
    ExpressionStart { left_brace: DocumentRange },
    /// The `>` that ends the tag. E.g.
    /// ```text
    /// <div>
    ///     ^
    /// ```
    End { range: DocumentRange },
    /// The `/>` that ends the tag. E.g.
    /// ```text
    /// <div/>
    ///     ^^
    /// ```
    SelfClosingEnd { range: DocumentRange },
}

/// A raw text element's body: everything up to and including its closing tag. E.g.
/// ```text
/// <script>let x = 20;</script>
///         ^^^^^^^^^^^^^^^^^^^^
/// ```
pub struct RawTextToken {
    /// The text between the tags. None when the element was empty.
    /// E.g.
    /// ```text
    /// <script>let x = 20;</script>
    ///         ^^^^^^^^^^^
    /// ```
    pub content: Option<DocumentRange>,
    /// The `>` that closed the element. E.g.
    /// ```text
    /// <script>let x = 20;</script>
    ///                            ^
    /// ```
    pub closing_tag_end: DocumentRange,
}

/// A quoted attribute value.
/// The `content` field is `None` for `a=""`.
pub struct AttributeString {
    pub content_range: Option<DocumentRange>,
    pub quoted_range: DocumentRange,
}

impl LangToken {
    pub fn opposite_token(&self) -> LangToken {
        match self {
            LangToken::LeftBrace => LangToken::RightBrace,
            LangToken::LeftBracket => LangToken::RightBracket,
            LangToken::LeftParen => LangToken::RightParen,
            _ => {
                panic!("opposite_token called on {}", self)
            }
        }
    }
}

impl fmt::Display for LangToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LangToken::Identifier(name) => write!(f, "{}", name),
            LangToken::TypeName(name) => write!(f, "{}", name),
            LangToken::StringLiteral(s) => write!(f, "\"{}\"", s),
            LangToken::IntLiteral(i) => write!(f, "{}", i),
            LangToken::FloatLiteral(float_val) => write!(f, "{}", float_val),
            LangToken::Comment(text) => write!(f, "{}", text),
            LangToken::Underscore => write!(f, "_"),
            LangToken::Assign => write!(f, "="),
            LangToken::Eq => write!(f, "=="),
            LangToken::NotEq => write!(f, "!="),
            LangToken::LessThan => write!(f, "<"),
            LangToken::GreaterThan => write!(f, ">"),
            LangToken::LessThanOrEqual => write!(f, "<="),
            LangToken::GreaterThanOrEqual => write!(f, ">="),
            LangToken::LogicalAnd => write!(f, "&&"),
            LangToken::LogicalOr => write!(f, "||"),
            LangToken::Plus => write!(f, "+"),
            LangToken::Minus => write!(f, "-"),
            LangToken::Asterisk => write!(f, "*"),
            LangToken::Not => write!(f, "!"),
            LangToken::Dot => write!(f, "."),
            LangToken::DotDotDot => write!(f, "..."),
            LangToken::DotDotEq => write!(f, "..="),
            LangToken::LeftParen => write!(f, "("),
            LangToken::RightParen => write!(f, ")"),
            LangToken::LeftBracket => write!(f, "["),
            LangToken::RightBracket => write!(f, "]"),
            LangToken::LeftBrace => write!(f, "{{"),
            LangToken::RightBrace => write!(f, "}}"),
            LangToken::Colon => write!(f, ":"),
            LangToken::ColonColon => write!(f, "::"),
            LangToken::Comma => write!(f, ","),
            LangToken::Arrow => write!(f, "->"),
            LangToken::FatArrow => write!(f, "=>"),
            LangToken::In => write!(f, "in"),
            LangToken::True => write!(f, "true"),
            LangToken::False => write!(f, "false"),
            LangToken::Import => write!(f, "import"),
            LangToken::Record => write!(f, "record"),
            LangToken::Match => write!(f, "match"),
            LangToken::Enum => write!(f, "enum"),
            LangToken::View => write!(f, "view"),
            LangToken::Page => write!(f, "page"),
            LangToken::Component => write!(f, "component"),
            LangToken::Fn => write!(f, "fn"),
            LangToken::Pub => write!(f, "pub"),
            LangToken::Some => write!(f, "Some"),
            LangToken::None => write!(f, "None"),
            LangToken::TypeString => write!(f, "String"),
            LangToken::TypeInt => write!(f, "Int"),
            LangToken::TypeFloat => write!(f, "Float"),
            LangToken::TypeBoolean => write!(f, "Bool"),
            LangToken::TypeFragment => write!(f, "Fragment"),
            LangToken::TypeArray => write!(f, "Array"),
            LangToken::TypeOption => write!(f, "Option"),
            LangToken::HashBracket => write!(f, "#["),
        }
    }
}
