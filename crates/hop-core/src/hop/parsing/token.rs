use crate::document::DocumentRange;
use std::fmt::{self, Display};

#[derive(Debug)]
pub enum Token {
    Doctype {
        range: DocumentRange,
    },
    Comment {
        range: DocumentRange,
    },
    OpeningTag {
        tag_name: DocumentRange,
        attributes: Vec<TokenizedAttribute>,
        expression: Option<DocumentRange>,
        self_closing: bool,
        range: DocumentRange,
    },
    ClosingTag {
        tag_name: DocumentRange,
        range: DocumentRange,
    },
    Text {
        range: DocumentRange,
    },
    /// A newline character in text position.
    /// This is emitted separately from Text tokens to allow the parser
    /// to handle whitespace normalization more precisely.
    Newline {
        range: DocumentRange,
    },
    TextExpression {
        content: DocumentRange,
        range: DocumentRange,
    },
    RawTextTag {
        tag_name: DocumentRange,
        attributes: Vec<TokenizedAttribute>,
        expression: Option<DocumentRange>,
        content: Option<DocumentRange>,
        range: DocumentRange,
    },
}

#[derive(Debug, Clone)]
pub enum TokenizedAttribute {
    Named {
        name: DocumentRange,
        value: Option<TokenizedAttributeValue>,

        /// This is the range for the whole attribute,
        /// including quotes.
        ///
        /// E.g. <div foo="bar">
        ///           ^^^^^^^^^
        range: DocumentRange,
    },
    Spread {
        name: DocumentRange,
        range: DocumentRange,
    },
}

#[derive(Debug, Clone)]
pub enum TokenizedAttributeValue {
    /// A quoted string attribute value. Content is None for empty strings like `a=""`
    String {
        content: Option<DocumentRange>,
        /// Range of the whole value including the surrounding quotes, e.g. `"bar"`.
        quoted_range: DocumentRange,
    },
    Expression(DocumentRange),
}

impl Token {
    pub fn range(&self) -> &DocumentRange {
        match self {
            Token::Doctype { range }
            | Token::Comment { range }
            | Token::OpeningTag { range, .. }
            | Token::ClosingTag { range, .. }
            | Token::Text { range }
            | Token::Newline { range }
            | Token::TextExpression { range, .. }
            | Token::RawTextTag { range, .. } => range,
        }
    }
}

impl TokenizedAttribute {
    pub fn name(&self) -> &DocumentRange {
        match self {
            TokenizedAttribute::Named { name, .. } | TokenizedAttribute::Spread { name, .. } => {
                name
            }
        }
    }

    pub fn range(&self) -> &DocumentRange {
        match self {
            TokenizedAttribute::Named { range, .. } | TokenizedAttribute::Spread { range, .. } => {
                range
            }
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Text { range: value } => {
                write!(
                    f,
                    "Text [{} byte, {:#?}]",
                    value.as_str().len(),
                    value.to_string()
                )
            }
            Token::Newline { .. } => {
                write!(f, "Newline")
            }
            Token::Doctype { .. } => {
                write!(f, "Doctype")
            }
            Token::ClosingTag { tag_name, .. } => {
                writeln!(f, "ClosingTag(")?;
                writeln!(f, "  tag_name: {:?},", tag_name.to_string())?;
                write!(f, ")")
            }
            Token::OpeningTag {
                tag_name,
                attributes,
                expression,
                self_closing,
                ..
            } => {
                writeln!(f, "OpeningTag(")?;
                writeln!(f, "  tag_name: {:?},", tag_name.to_string())?;
                write!(f, "  attributes: {{")?;
                if attributes.is_empty() {
                    writeln!(f, "}},")?;
                } else {
                    writeln!(f)?;
                    for item in attributes {
                        match item {
                            TokenizedAttribute::Named { name, value, .. } => {
                                let value_str = match value {
                                    Some(TokenizedAttributeValue::String { content, .. }) => {
                                        let val = content
                                            .as_ref()
                                            .map(|r| r.to_string())
                                            .unwrap_or_default();
                                        format!("String({:?})", val)
                                    }
                                    Some(TokenizedAttributeValue::Expression(val)) => {
                                        format!("Expression({:?})", val.to_string())
                                    }
                                    None => "None".to_string(),
                                };
                                writeln!(f, "    {}: {},", name, value_str)?;
                            }
                            TokenizedAttribute::Spread { name, .. } => {
                                writeln!(f, "    ...{},", name)?;
                            }
                        }
                    }
                    writeln!(f, "  }},")?;
                }
                let expr_str = match expression {
                    Some(expr) => format!("Some({:?})", expr.to_string()),
                    None => "None".to_string(),
                };
                writeln!(f, "  expression: {},", expr_str)?;
                writeln!(f, "  self_closing: {},", self_closing)?;
                write!(f, ")")
            }
            Token::Comment { .. } => {
                write!(f, "Comment")
            }
            Token::TextExpression { content, .. } => {
                write!(f, "TextExpression({:?})", content.to_string())
            }
            Token::RawTextTag {
                tag_name,
                attributes,
                expression,
                content,
                ..
            } => {
                writeln!(f, "RawTextTag(")?;
                writeln!(f, "  tag_name: {:?},", tag_name.to_string())?;
                write!(f, "  attributes: {{")?;
                if attributes.is_empty() {
                    writeln!(f, "}},")?;
                } else {
                    writeln!(f)?;
                    for item in attributes {
                        match item {
                            TokenizedAttribute::Named { name, value, .. } => {
                                let value_str = match value {
                                    Some(TokenizedAttributeValue::String { content, .. }) => {
                                        let val = content
                                            .as_ref()
                                            .map(|r| r.to_string())
                                            .unwrap_or_default();
                                        format!("String({:?})", val)
                                    }
                                    Some(TokenizedAttributeValue::Expression(val)) => {
                                        format!("Expression({:?})", val.to_string())
                                    }
                                    None => "None".to_string(),
                                };
                                writeln!(f, "    {}: {},", name, value_str)?;
                            }
                            TokenizedAttribute::Spread { name, .. } => {
                                writeln!(f, "    ...{},", name)?;
                            }
                        }
                    }
                    writeln!(f, "  }},")?;
                }
                let expr_str = match expression {
                    Some(expr) => format!("Some({:?})", expr.to_string()),
                    None => "None".to_string(),
                };
                writeln!(f, "  expression: {},", expr_str)?;
                let content_str = match content {
                    Some(c) => format!("Some({:?})", c.to_string()),
                    None => "None".to_string(),
                };
                writeln!(f, "  content: {},", content_str)?;
                write!(f, ")")
            }
        }
    }
}
