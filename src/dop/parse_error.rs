use crate::dop::tokenizer::DopToken;
use crate::span::string_cursor::StringSpan;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEof,
    Spanned { message: String, span: StringSpan },
}

impl ParseError {
    pub fn new(message: String, span: StringSpan) -> Self {
        Self::Spanned { message, span }
    }

    pub fn unterminated_string_literal(span: StringSpan) -> Self {
        Self::new("Unterminated string literal".to_string(), span)
    }

    pub fn unmatched_token(token: &DopToken, span: StringSpan) -> Self {
        Self::new(format!("Unmatched '{token}'"), span)
    }

    pub fn invalid_variable_name(name: StringSpan) -> Self {
        Self::new(
            format!(
                "Invalid variable name '{name}'. Variable names must start with a letter and contain only letters, digits, and underscores"
            ),
            name,
        )
    }

    pub fn expected_tokens_but_got(
        expected: &[DopToken],
        actual: &DopToken,
        span: StringSpan,
    ) -> Self {
        Self::new(
            format!(
                "Expected {} but got '{}'",
                expected
                    .iter()
                    .map(|t| format!("'{}'", t))
                    .collect::<Vec<_>>()
                    .join(" or "),
                actual
            ),
            span,
        )
    }

    pub fn expected_token_but_got(
        expected: &DopToken,
        actual: &DopToken,
        span: StringSpan,
    ) -> Self {
        Self::new(
            format!("Expected token '{expected}' but got '{actual}'"),
            span,
        )
    }

    pub fn unexpected_token(token: &DopToken, span: StringSpan) -> Self {
        Self::new(format!("Unexpected token '{token}'"), span)
    }

    pub fn expected_variable_name_but_got(actual: &DopToken, span: StringSpan) -> Self {
        Self::new(format!("Expected variable name but got {actual}"), span)
    }

    pub fn expected_property_name_but_got(actual: &DopToken, span: StringSpan) -> Self {
        Self::new(format!("Expected property name but got {actual}"), span)
    }

    pub fn expected_identifier_after_dot(span: StringSpan) -> Self {
        Self::new("Expected identifier after '.'".to_string(), span)
    }

    pub fn duplicate_argument(name: StringSpan) -> Self {
        Self::new(format!("Duplicate argument '{name}'"), name)
    }

    pub fn duplicate_parameter(name: StringSpan) -> Self {
        Self::new(format!("Duplicate parameter '{name}'"), name)
    }

    pub fn duplicate_property(name: StringSpan) -> Self {
        Self::new(format!("Duplicate property '{name}'"), name)
    }
}
