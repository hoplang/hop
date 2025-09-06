use std::collections::BTreeMap;
use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::dop::DopType;
use crate::dop::tokenizer::{DopToken, DopTokenizer};
use crate::dop::typechecker::RangeDopType;
use crate::range::string_cursor::StringSpan;
use crate::range::{Range, Ranged};

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
            format!("Invalid variable name '{name}'. Variable names must match [a-z][a-z0-9_]*"),
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

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Equal => write!(f, "=="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DopExpr {
    Variable {
        value: StringSpan,
    },
    PropertyAccess {
        object: Box<DopExpr>,
        property: StringSpan,
        range: Range,
    },
    StringLiteral {
        value: String,
        range: Range,
    },
    BooleanLiteral {
        value: bool,
        range: Range,
    },
    NumberLiteral {
        value: serde_json::Number,
        range: Range,
    },
    /// An array literal, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<DopExpr>,
        range: Range,
    },
    ObjectLiteral {
        properties: BTreeMap<String, DopExpr>,
        range: Range,
    },
    BinaryOp {
        left: Box<DopExpr>,
        operator: BinaryOp,
        right: Box<DopExpr>,
        range: Range,
    },
    UnaryOp {
        operator: UnaryOp,
        operand: Box<DopExpr>,
        range: Range,
    },
}

impl Ranged for DopExpr {
    /// Returns the range of this expression in the source code
    fn range(&self) -> Range {
        match self {
            DopExpr::Variable { value, .. } => value.range(),
            DopExpr::PropertyAccess { range, .. } => *range,
            DopExpr::StringLiteral { range, .. } => *range,
            DopExpr::BooleanLiteral { range, .. } => *range,
            DopExpr::NumberLiteral { range, .. } => *range,
            DopExpr::ArrayLiteral { range, .. } => *range,
            DopExpr::ObjectLiteral { range, .. } => *range,
            DopExpr::BinaryOp { range, .. } => *range,
            DopExpr::UnaryOp { range, .. } => *range,
        }
    }
}

impl Display for DopExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopExpr::Variable { value } => write!(f, "{}", value),
            DopExpr::PropertyAccess { object, property, .. } => {
                write!(f, "{}.{}", object, property)
            }
            DopExpr::StringLiteral { value, .. } => write!(f, "\"{}\"", value),
            DopExpr::BooleanLiteral { value, .. } => write!(f, "{}", value),
            DopExpr::NumberLiteral { value, .. } => write!(f, "{}", value),
            DopExpr::ArrayLiteral { elements, .. } => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            DopExpr::ObjectLiteral { properties, .. } => {
                write!(f, "{{")?;
                for (i, (key, value)) in properties.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            DopExpr::BinaryOp { left, operator, right, .. } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            DopExpr::UnaryOp { operator, operand, .. } => {
                write!(f, "({}{})", operator, operand)
            }
        }
    }
}

/// A DopVarName represents a validated variable name in dop.
#[derive(Debug, Clone)]
pub struct DopVarName {
    value: StringSpan,
}

impl Display for DopVarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl DopVarName {
    pub fn new(value: StringSpan) -> Result<Self, ParseError> {
        let mut chars = value.as_str().chars();
        if !chars.next().is_some_and(|c| c.is_ascii_lowercase())
            || !chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
        {
            return Err(ParseError::invalid_variable_name(
                value.as_str(),
                value.range(),
            ));
        }
        Ok(DopVarName { value })
    }
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
    pub fn to_string(&self) -> String {
        self.value.to_string()
    }
    pub fn range(&self) -> Range {
        self.value.range()
    }
}

/// A DopParameter represents a parsed parameter with type annotation.
#[derive(Debug, Clone)]
pub struct DopParameter {
    pub var_name: DopVarName,
    pub type_annotation: DopType,
}

impl Display for DopParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.type_annotation)
    }
}

/// A DopArgument represents a parsed argument with a name and expression.
/// E.g. <my-comp {x: [1,2], y: 2}>
///                ^^^^^^^^
#[derive(Debug, Clone)]
pub struct DopArgument {
    pub var_name: DopVarName,
    pub expression: DopExpr,
}

impl Display for DopArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.expression)
    }
}

fn advance_if(tokenizer: &mut Peekable<DopTokenizer>, token: DopToken) -> Option<Range> {
    if let Some(Ok((_, range))) =
        tokenizer.next_if(|res| res.as_ref().is_ok_and(|(t, _)| *t == token))
    {
        Some(range)
    } else {
        None
    }
}

fn expect_token(
    tokenizer: &mut Peekable<DopTokenizer>,
    expected: DopToken,
) -> Result<Range, ParseError> {
    match tokenizer.next().ok_or(ParseError::UnexpectedEof)?? {
        (token, range) if token == expected => Ok(range),
        (actual, range) => Err(ParseError::expected_token_but_got(
            &expected, &actual, range,
        )),
    }
}

fn expect_variable_name(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopVarName, ParseError> {
    match tokenizer.next().ok_or(ParseError::UnexpectedEof)?? {
        (DopToken::Identifier(name), _) => DopVarName::new(name),
        (actual, range) => Err(ParseError::expected_variable_name_but_got(&actual, range)),
    }
}

fn expect_property_name(tokenizer: &mut Peekable<DopTokenizer>) -> Result<StringSpan, ParseError> {
    match tokenizer.next().ok_or(ParseError::UnexpectedEof)?? {
        (DopToken::Identifier(name), _) => Ok(name),
        (token, range) => Err(ParseError::expected_property_name_but_got(&token, range)),
    }
}

fn expect_eof(tokenizer: &mut Peekable<DopTokenizer>) -> Result<(), ParseError> {
    match tokenizer.next().transpose()? {
        None => Ok(()),
        Some((token, range)) => Err(ParseError::unexpected_token(&token, range)),
    }
}

// expr = equality Eof
pub fn parse_expr(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopExpr, ParseError> {
    let result = parse_equality(tokenizer)?;
    expect_eof(tokenizer)?;
    Ok(result)
}

// loop_header = Identifier "in" equality Eof
pub fn parse_loop_header(
    tokenizer: &mut Peekable<DopTokenizer>,
) -> Result<(DopVarName, DopExpr), ParseError> {
    let var_name = expect_variable_name(tokenizer)?;
    expect_token(tokenizer, DopToken::In)?;
    let array_expr = parse_equality(tokenizer)?;
    expect_eof(tokenizer)?;
    Ok((var_name, array_expr))
}

// parameter_with_type = Identifier ":" type
fn parse_parameter(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopParameter, ParseError> {
    let var_name = expect_variable_name(tokenizer)?;
    expect_token(tokenizer, DopToken::Colon)?;
    let typ = parse_type(tokenizer)?;
    Ok(DopParameter {
        var_name,
        type_annotation: typ.dop_type,
    })
}

// named_argument = Identifier ":" expr
fn parse_argument(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopArgument, ParseError> {
    let var_name = expect_variable_name(tokenizer)?;
    expect_token(tokenizer, DopToken::Colon)?;
    let expression = parse_equality(tokenizer)?;
    Ok(DopArgument {
        var_name,
        expression,
    })
}

// parameters = parameter ("," parameter)* Eof
pub fn parse_parameters(
    tokenizer: &mut Peekable<DopTokenizer>,
) -> Result<BTreeMap<String, DopParameter>, ParseError> {
    let mut params = BTreeMap::new();
    let first_param = parse_parameter(tokenizer)?;
    params.insert(first_param.var_name.value.to_string(), first_param);

    while advance_if(tokenizer, DopToken::Comma).is_some() {
        // Handle trailing comma
        if tokenizer.peek().is_none() {
            break;
        }
        let param = parse_parameter(tokenizer)?;
        if params.contains_key(param.var_name.value.as_str()) {
            return Err(ParseError::duplicate_parameter(
                param.var_name.value.as_str(),
                param.var_name.value.range(),
            ));
        }
        params.insert(param.var_name.value.to_string(), param);
    }

    expect_eof(tokenizer)?;

    Ok(params)
}

// arguments = argument ("," argument)* Eof
pub fn parse_arguments(
    tokenizer: &mut Peekable<DopTokenizer>,
) -> Result<BTreeMap<String, DopArgument>, ParseError> {
    let mut args = BTreeMap::new();
    let first_arg = parse_argument(tokenizer)?;
    args.insert(first_arg.var_name.value.to_string(), first_arg);

    while advance_if(tokenizer, DopToken::Comma).is_some() {
        // Handle trailing comma
        if tokenizer.peek().is_none() {
            break;
        }
        let arg = parse_argument(tokenizer)?;
        if args.contains_key(arg.var_name.value.as_str()) {
            return Err(ParseError::duplicate_argument(
                arg.var_name.value.as_str(),
                arg.var_name.value.range(),
            ));
        }
        args.insert(arg.var_name.value.to_string(), arg);
    }

    expect_eof(tokenizer)?;

    Ok(args)
}

// type = TypeString
//      | TypeNumber
//      | TypeBoolean
//      | TypeVoid
//      | TypeArray "[" type "]"
//      | "{" (Identifier ":" type ("," Identifier ":" type)*)? "}"
fn parse_type(tokenizer: &mut Peekable<DopTokenizer>) -> Result<RangeDopType, ParseError> {
    use crate::dop::DopType;
    use std::collections::BTreeMap;

    match tokenizer.next().ok_or(ParseError::UnexpectedEof)?? {
        (DopToken::TypeString, range) => Ok(RangeDopType {
            dop_type: DopType::String,
            range,
        }),
        (DopToken::TypeNumber, range) => Ok(RangeDopType {
            dop_type: DopType::Number,
            range,
        }),
        (DopToken::TypeBoolean, range) => Ok(RangeDopType {
            dop_type: DopType::Bool,
            range,
        }),
        (DopToken::TypeArray, type_array_range) => {
            expect_token(tokenizer, DopToken::LeftBracket)?;
            let inner_type = parse_type(tokenizer)?;
            let right_bracket_range = expect_token(tokenizer, DopToken::RightBracket)?;

            Ok(RangeDopType {
                dop_type: DopType::Array(Some(Box::new(inner_type.dop_type))),
                range: type_array_range.spanning(right_bracket_range),
            })
        }
        (DopToken::LeftBrace, left_brace_range) => {
            let mut properties = BTreeMap::new();

            loop {
                let prop_name = expect_property_name(tokenizer)?;
                expect_token(tokenizer, DopToken::Colon)?;
                let typ = parse_type(tokenizer)?;
                if properties.contains_key(prop_name.as_str()) {
                    return Err(ParseError::duplicate_property(
                        prop_name.as_str(),
                        prop_name.range(),
                    ));
                }
                properties.insert(prop_name.to_string(), typ.dop_type);

                match tokenizer.next().ok_or_else(|| {
                    ParseError::unmatched_token(&DopToken::LeftBrace, left_brace_range)
                })?? {
                    (DopToken::Comma, _) => {
                        // Check for trailing comma (closing brace after comma)
                        if let Some(right_brace_range) = advance_if(tokenizer, DopToken::RightBrace)
                        {
                            return Ok(RangeDopType {
                                dop_type: DopType::Object(properties),
                                range: left_brace_range.spanning(right_brace_range),
                            });
                        }
                        continue;
                    }
                    (DopToken::RightBrace, right_brace_range) => {
                        return Ok(RangeDopType {
                            dop_type: DopType::Object(properties),
                            range: left_brace_range.spanning(right_brace_range),
                        });
                    }
                    (t, range) => {
                        return Err(ParseError::expected_tokens_but_got(
                            &[DopToken::Comma, DopToken::RightBrace],
                            &t,
                            range,
                        ));
                    }
                };
            }
        }
        (_, range) => Err(ParseError::new("Expected type name".to_string(), range)),
    }
}

// equality = unary ( "==" unary )*
fn parse_equality(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopExpr, ParseError> {
    let mut expr = parse_unary(tokenizer)?;

    while advance_if(tokenizer, DopToken::Equal).is_some() {
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp {
            range: expr.range().spanning(right.range()),
            left: Box::new(expr),
            operator: BinaryOp::Equal,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

// unary = ( "!" )* primary
fn parse_unary(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopExpr, ParseError> {
    if let Some(operator_range) = advance_if(tokenizer, DopToken::Not) {
        let expr = parse_unary(tokenizer)?; // Right associative for multiple !
        Ok(DopExpr::UnaryOp {
            range: operator_range.spanning(expr.range()),
            operator: UnaryOp::Not,
            operand: Box::new(expr),
        })
    } else {
        parse_primary(tokenizer)
    }
}

// primary = Identifier ( "." Identifier )*
//         | StringLiteral
//         | BooleanLiteral
//         | NumberLiteral
//         | "[" ( equality ("," equality)* )? "]"
//         | "{" ( Identifier ":" equality ("," Identifier ":" equality)* )? "}"
//         | "(" equality ")"
fn parse_primary(tokenizer: &mut Peekable<DopTokenizer>) -> Result<DopExpr, ParseError> {
    match tokenizer.next().ok_or(ParseError::UnexpectedEof)?? {
        (DopToken::Identifier(name), _) => {
            let mut expr = DopExpr::Variable { value: name };

            // Handle property access
            while advance_if(tokenizer, DopToken::Dot).is_some() {
                match tokenizer.next().ok_or(ParseError::UnexpectedEof)?? {
                    (DopToken::Identifier(prop), property_range) => {
                        expr = DopExpr::PropertyAccess {
                            range: expr.range().spanning(property_range),
                            object: Box::new(expr),
                            property: prop,
                        };
                    }
                    (_, range) => {
                        return Err(ParseError::new(
                            "Expected identifier after '.'".to_string(),
                            range,
                        ));
                    }
                }
            }

            Ok(expr)
        }
        (DopToken::StringLiteral(value), range) => Ok(DopExpr::StringLiteral { value, range }),
        (DopToken::BooleanLiteral(value), range) => Ok(DopExpr::BooleanLiteral { value, range }),
        (DopToken::NumberLiteral(value), range) => Ok(DopExpr::NumberLiteral { value, range }),
        (DopToken::LeftBracket, left_bracket_range) => {
            let mut elements = Vec::new();

            // Handle empty array
            if let Some(right_bracket_range) = advance_if(tokenizer, DopToken::RightBracket) {
                return Ok(DopExpr::ArrayLiteral {
                    elements,
                    range: left_bracket_range.spanning(right_bracket_range),
                });
            }

            // Parse array elements
            loop {
                elements.push(parse_equality(tokenizer)?);

                match tokenizer.next().ok_or_else(|| {
                    ParseError::unmatched_token(&DopToken::LeftBracket, left_bracket_range)
                })?? {
                    (DopToken::Comma, _) => {
                        // Handle trailing comma
                        if let Some(right_bracket_range) =
                            advance_if(tokenizer, DopToken::RightBracket)
                        {
                            return Ok(DopExpr::ArrayLiteral {
                                elements,
                                range: left_bracket_range.spanning(right_bracket_range),
                            });
                        }
                        continue;
                    }
                    (DopToken::RightBracket, right_bracket_range) => {
                        return Ok(DopExpr::ArrayLiteral {
                            elements,
                            range: left_bracket_range.spanning(right_bracket_range),
                        });
                    }
                    (t, range) => {
                        return Err(ParseError::expected_tokens_but_got(
                            &[DopToken::Comma, DopToken::RightBracket],
                            &t,
                            range,
                        ));
                    }
                }
            }
        }
        (DopToken::LeftBrace, left_brace_range) => {
            // Parse {key1: value1, key2: value2, ...}
            let mut properties = BTreeMap::new();

            // Handle empty object
            if let Some(right_brace_range) = advance_if(tokenizer, DopToken::RightBrace) {
                return Ok(DopExpr::ObjectLiteral {
                    properties,
                    range: left_brace_range.spanning(right_brace_range),
                });
            }

            // Parse object properties
            loop {
                let prop_name = expect_property_name(tokenizer)?;
                if properties.contains_key(prop_name.as_str()) {
                    return Err(ParseError::duplicate_property(
                        prop_name.as_str(),
                        prop_name.range(),
                    ));
                }

                expect_token(tokenizer, DopToken::Colon)?;

                properties.insert(prop_name.to_string(), parse_equality(tokenizer)?);

                // Expect comma or right brace
                match tokenizer.next().ok_or_else(|| {
                    ParseError::unmatched_token(&DopToken::LeftBrace, left_brace_range)
                })?? {
                    (DopToken::Comma, _) => {
                        // Check for trailing comma (closing brace after comma)
                        if let Some(right_brace_range) = advance_if(tokenizer, DopToken::RightBrace)
                        {
                            return Ok(DopExpr::ObjectLiteral {
                                properties,
                                range: left_brace_range.spanning(right_brace_range),
                            });
                        }
                        continue;
                    }
                    (DopToken::RightBrace, right_brace_range) => {
                        return Ok(DopExpr::ObjectLiteral {
                            properties,
                            range: left_brace_range.spanning(right_brace_range),
                        });
                    }
                    (t, range) => {
                        return Err(ParseError::expected_tokens_but_got(
                            &[DopToken::Comma, DopToken::RightBrace],
                            &t,
                            range,
                        ));
                    }
                }
            }
        }
        (DopToken::LeftParen, left_paren_range) => {
            let expr = parse_equality(tokenizer)?;
            match tokenizer.next().ok_or_else(|| {
                ParseError::unmatched_token(&DopToken::LeftParen, left_paren_range)
            })?? {
                (DopToken::RightParen, _) => Ok(expr),
                (t, range) => Err(ParseError::expected_token_but_got(
                    &DopToken::RightParen,
                    &t,
                    range,
                )),
            }
        }
        (token, range) => Err(ParseError::unexpected_token(&token, range)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::range::{SimpleAnnotation, SourceAnnotator};
    use expect_test::{Expect, expect};

    fn annotate_error(input: &str, error: ParseError) -> String {
        let annotator = SourceAnnotator::new()
            .with_label("error")
            .without_location()
            .without_line_numbers();
        match error {
            ParseError::UnexpectedEof => "Unexpected end of expression".to_string(),
            ParseError::Ranged { message, range } => {
                annotator.annotate(None, input, [SimpleAnnotation { message, range }])
            }
        }
    }

    fn check_parse_expr(input: &str, expected: Expect) {
        let mut tokenizer = DopTokenizer::from(input).peekable();
        let actual = match parse_expr(&mut tokenizer) {
            Ok(result) => format!("{}\n", result),
            Err(err) => annotate_error(input, err),
        };
        expected.assert_eq(&actual);
    }

    fn check_parse_parameters(input: &str, expected: Expect) {
        let mut tokenizer = DopTokenizer::from(input).peekable();

        let actual = match parse_parameters(&mut tokenizer) {
            Ok(result) => {
                let params: Vec<String> = result
                    .iter()
                    .map(|(_, param)| param.to_string())
                    .collect();
                format!("[{}]\n", params.join(", "))
            },
            Err(err) => annotate_error(input, err),
        };

        expected.assert_eq(&actual);
    }

    fn check_parse_arguments(input: &str, expected: Expect) {
        let mut tokenizer = DopTokenizer::from(input).peekable();

        let actual = match parse_arguments(&mut tokenizer) {
            Ok(result) => {
                let args: Vec<String> = result
                    .iter()
                    .map(|(_, arg)| arg.to_string())
                    .collect();
                format!("[{}]\n", args.join(", "))
            },
            Err(err) => annotate_error(input, err),
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_parse_parameters_duplicate_parameters_with_different_type_error() {
        check_parse_parameters(
            "foo: string, foo: number",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: string, foo: number
                             ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_duplicate_parameters_with_same_type_error() {
        check_parse_parameters(
            "foo: string, foo: string",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: string, foo: string
                             ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_extra_closing_brace() {
        check_parse_parameters(
            "params: {i: {j: {k: {l: boolean}}}}}",
            expect![[r#"
                error: Unexpected token '}'
                params: {i: {j: {k: {l: boolean}}}}}
                                                   ^
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_missing_closing_brace() {
        check_parse_parameters(
            "params: {i: {j: {k: {l: boolean}}}",
            expect![[r#"
                error: Unmatched '{'
                params: {i: {j: {k: {l: boolean}}}
                        ^
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_duplicate_keys_in_object_type() {
        check_parse_parameters(
            "user: {name: string, name: number}",
            expect![[r#"
                error: Duplicate property 'name'
                user: {name: string, name: number}
                                     ^^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_trailing_tokens() {
        check_parse_expr(
            "x y",
            expect![[r#"
                error: Unexpected token 'y'
                x y
                  ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_unmatched_left_bracket() {
        check_parse_expr(
            "[1,2",
            expect![[r#"
                error: Unmatched '['
                [1,2
                ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_unexpected_token() {
        check_parse_expr(
            "[1,2 id",
            expect![[r#"
                error: Expected ',' or ']' but got 'id'
                [1,2 id
                     ^^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_dot_no_identifier() {
        check_parse_expr("user.", expect!["Unexpected end of expression"]);
    }

    #[test]
    fn test_parse_expr_error_dot_number() {
        check_parse_expr(
            "user.123",
            expect![[r#"
                error: Expected identifier after '.'
                user.123
                     ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_invalid_start() {
        check_parse_expr(
            "== x",
            expect![[r#"
                error: Unexpected token '=='
                == x
                ^^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_unclosed_paren() {
        check_parse_expr(
            "(x == y",
            expect![[r#"
                error: Unmatched '('
                (x == y
                ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_unmatched_closing_paren() {
        check_parse_expr(
            "x == y)",
            expect![[r#"
                error: Unexpected token ')'
                x == y)
                      ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_empty_parens() {
        check_parse_expr(
            "()",
            expect![[r#"
                error: Unexpected token ')'
                ()
                 ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_invalid_after_equals() {
        check_parse_expr(
            "x == )",
            expect![[r#"
                error: Unexpected token ')'
                x == )
                     ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_dot_at_start() {
        check_parse_expr(
            ".property",
            expect![[r#"
                error: Unexpected token '.'
                .property
                ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_double_dot() {
        check_parse_expr(
            "user..name",
            expect![[r#"
                error: Expected identifier after '.'
                user..name
                     ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_operator_at_end() {
        check_parse_expr("x ==", expect!["Unexpected end of expression"]);
    }

    #[test]
    fn test_parse_expr_error_not_without_operand() {
        check_parse_expr("!", expect!["Unexpected end of expression"]);
    }

    #[test]
    fn test_parse_expr_error_trailing_not() {
        check_parse_expr(
            "x !",
            expect![[r#"
                error: Unexpected token '!'
                x !
                  ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_binary_op_chained() {
        check_parse_expr(
            "a == b == c",
            expect![[r#"
                ((a == b) == c)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_access_comparison() {
        check_parse_expr(
            "user.name == admin.name",
            expect![[r#"
                (user.name == admin.name)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_access() {
        check_parse_expr(
            "app.user.profile.settings.theme",
            expect![[r#"
                app.user.profile.settings.theme
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_string() {
        check_parse_expr(
            "''",
            expect![[r#"
                ""
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_number_literal_integer() {
        check_parse_expr(
            "99",
            expect![[r#"
                99
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_number_literal_float() {
        check_parse_expr(
            "3.14",
            expect![[r#"
                3.14
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_parenthesized() {
        check_parse_expr(
            "(x == y)",
            expect![[r#"
                (x == y)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_simple_property_access() {
        check_parse_expr(
            "user.name",
            expect![[r#"
                user.name
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_comparison() {
        check_parse_expr(
            "'guest' == user.role",
            expect![[r#"
                ("guest" == user.role)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_variable_comparison() {
        check_parse_expr(
            "x == y",
            expect![[r#"
                (x == y)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_literal() {
        check_parse_expr(
            "'hello'",
            expect![[r#"
                "hello"
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_variable() {
        check_parse_expr(
            "x",
            expect![[r#"
                x
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_literal_comparison() {
        check_parse_expr(
            "'apple' == 'orange'",
            expect![[r#"
                ("apple" == "orange")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_string_comparison() {
        check_parse_expr(
            "user.name == 'admin'",
            expect![[r#"
                (user.name == "admin")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_with_space() {
        check_parse_expr(
            "'hello world'",
            expect![[r#"
                "hello world"
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_whitespace_handling() {
        check_parse_expr(
            "  user . name   ==   admin . name  ",
            expect![[r#"
                (user.name == admin.name)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_array() {
        check_parse_expr(
            "[]",
            expect![[r#"
                []
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_numbers() {
        check_parse_expr(
            "[1, 2, 3]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_mixed_types() {
        check_parse_expr(
            "[1, 'hello', true]",
            expect![[r#"
                [1, "hello", true]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_nested_arrays() {
        check_parse_expr(
            "[[1, 2], [3, 4]]",
            expect![[r#"
                [[1, 2], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_variables() {
        check_parse_expr(
            "[x, user.name]",
            expect![[r#"
                [x, user.name]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_object() {
        check_parse_expr(
            "{}",
            expect![[r#"
                {}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_single_property() {
        check_parse_expr(
            "{name: 'John'}",
            expect![[r#"
                {name: "John"}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_multiple_properties() {
        check_parse_expr(
            "{a: 'foo', b: 1}",
            expect![[r#"
                {a: "foo", b: 1}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_complex_expressions() {
        check_parse_expr(
            "{user: user.name, active: !user.disabled}",
            expect![[r#"
                {active: (!user.disabled), user: user.name}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_nested() {
        check_parse_expr(
            "{nested: {inner: 'value'}}",
            expect![[r#"
                {nested: {inner: "value"}}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_multiline() {
        check_parse_expr(
            "[\n\t1,\n\t2,\n\t3,\n]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_single() {
        check_parse_expr(
            "[\n\t1,\n]",
            expect![[r#"
                [1]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_complex() {
        check_parse_expr(
            "[\n\tuser.name,\n\t!user.disabled,\n]",
            expect![[r#"
                [user.name, (!user.disabled)]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_multiline() {
        check_parse_expr(
            "{\n\ta: 'foo',\n\tb: 1,\n}",
            expect![[r#"
                {a: "foo", b: 1}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_single() {
        check_parse_expr(
            "{\n\tname: 'John',\n}",
            expect![[r#"
                {name: "John"}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_complex() {
        check_parse_expr(
            "{\n\tuser: user.name,\n\tactive: !user.disabled,\n}",
            expect![[r#"
                {active: (!user.disabled), user: user.name}
            "#]],
        );
    }

    #[test]
    fn test_parse_arguments_single() {
        check_parse_arguments(
            "name: 'John'",
            expect![[r#"
                [name: "John"]
            "#]],
        );
    }

    #[test]
    fn test_parse_arguments_multiple() {
        check_parse_arguments(
            "name: 'John', age: 25, active: true",
            expect![[r#"
                [active: true, age: 25, name: "John"]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_complex_expressions() {
        check_parse_arguments(
            "user: user.name, enabled: !user.disabled",
            expect![[r#"
                [enabled: (!user.disabled), user: user.name]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_trailing_comma() {
        check_parse_arguments(
            "name: 'John', age: 25,",
            expect![[r#"
                [age: 25, name: "John"]
            "#]],
        );
    }

    #[test]
    fn test_parse_arguments_duplicate_argument_error() {
        check_parse_arguments(
            "name: 'John', name: 'Jane'",
            expect![[r#"
                error: Duplicate argument 'name'
                name: 'John', name: 'Jane'
                              ^^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_missing_colon_error() {
        check_parse_arguments(
            "name 'John'",
            expect![[r#"
                error: Expected token ':' but got ''John''
                name 'John'
                     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_missing_value_error() {
        check_parse_arguments("name:", expect!["Unexpected end of expression"]);
    }

    #[test]
    fn test_parse_named_arguments_invalid_start_error() {
        check_parse_arguments(
            "123: 'value'",
            expect![[r#"
                error: Expected variable name but got 123
                123: 'value'
                ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_unexpected_token_error() {
        check_parse_arguments(
            "name: 'John' age: 25",
            expect![[r#"
                error: Unexpected token 'age'
                name: 'John' age: 25
                             ^^^
            "#]],
        );
    }
}
