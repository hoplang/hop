use crate::dop::tokenizer::DopToken;
use crate::range::Range;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEof,
    Ranged { message: String, range: Range },
}

impl ParseError {
    pub fn new(message: String, range: Range) -> Self {
        Self::Ranged { message, range }
    }

    pub fn unterminated_string_literal(range: Range) -> Self {
        Self::new("Unterminated string literal".to_string(), range)
    }

    pub fn unmatched_token(token: &DopToken, range: Range) -> Self {
        Self::new(format!("Unmatched '{token}'"), range)
    }

    pub fn expected_digit_after_decimal_point(range: Range) -> Self {
        Self::new("Expected digit after decimal point".to_string(), range)
    }

    pub fn invalid_variable_name(name: &str, range: Range) -> Self {
        Self::new(
            format!("Invalid variable name '{name}'. Variable names must start with a letter and contain only letters, digits, and underscores"),
            range,
        )
    }

    pub fn expected_tokens_but_got(expected: &[DopToken], actual: &DopToken, range: Range) -> Self {
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
            range,
        )
    }

    pub fn expected_token_but_got(expected: &DopToken, actual: &DopToken, range: Range) -> Self {
        Self::new(
            format!("Expected token '{expected}' but got '{actual}'"),
            range,
        )
    }

    pub fn unexpected_token(token: &DopToken, range: Range) -> Self {
        Self::new(format!("Unexpected token '{token}'"), range)
    }

    pub fn expected_variable_name_but_got(actual: &DopToken, range: Range) -> Self {
        Self::new(format!("Expected variable name but got {actual}"), range)
    }

    pub fn expected_property_name_but_got(actual: &DopToken, range: Range) -> Self {
        Self::new(format!("Expected property name but got {actual}"), range)
    }

    pub fn expected_identifier_after_dot(range: Range) -> Self {
        Self::new("Expected identifier after '.'".to_string(), range)
    }

    pub fn duplicate_argument(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate argument '{name}'"), range)
    }

    pub fn duplicate_parameter(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate parameter '{name}'"), range)
    }

    pub fn duplicate_property(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate property '{name}'"), range)
    }
}