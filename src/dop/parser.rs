use std::collections::BTreeMap;

use crate::common::{Range, RangeError};
use crate::dop::DopType;
use crate::dop::tokenizer::{DopToken, DopTokenizer};
use crate::dop::typechecker::RangeDopType;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DopExpr {
    Variable {
        name: String,
        range: Range,
    },
    PropertyAccess {
        object: Box<DopExpr>,
        property: String,
        property_range: Range,
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
        operator_range: Range,
        right: Box<DopExpr>,
        range: Range,
    },
    UnaryOp {
        operator: UnaryOp,
        operator_range: Range,
        operand: Box<DopExpr>,
        range: Range,
    },
}

impl DopExpr {
    /// Returns the range of this expression in the source code
    pub fn range(&self) -> Range {
        match self {
            DopExpr::Variable { range, .. } => *range,
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

/// A DopVarName represents a validated variable name in dop.
#[derive(Debug, Clone, PartialEq)]
pub struct DopVarName {
    pub value: String,
    pub range: Range,
}

impl DopVarName {
    pub fn new(value: String, range: Range) -> Result<Self, RangeError> {
        let mut chars = value.chars();
        if !chars.next().is_some_and(|c| c.is_ascii_lowercase())
            || !chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
        {
            return Err(RangeError::invalid_variable_name(&value, range));
        }
        Ok(DopVarName { value, range })
    }
}

/// A DopParameter represents a parsed parameter with type annotation.
#[derive(Debug, Clone, PartialEq)]
pub struct DopParameter {
    pub var_name: DopVarName,
    pub type_annotation: DopType,
}

/// A DopArgument represents a parsed argument with name and expression.
#[derive(Debug, Clone, PartialEq)]
pub struct DopArgument {
    pub var_name: DopVarName,
    pub expression: DopExpr,
}

// expr = equality Eof
pub fn parse_expr(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    let result = parse_equality(tokenizer)?;
    tokenizer.expect_eof()?;
    Ok(result)
}

// loop_header = Identifier "in" equality Eof
pub fn parse_loop_header(
    tokenizer: &mut DopTokenizer,
) -> Result<(DopVarName, DopExpr), RangeError> {
    let var_name = tokenizer.expect_variable_name()?;
    tokenizer.expect_token(DopToken::In)?;
    let array_expr = parse_equality(tokenizer)?;
    tokenizer.expect_eof()?;
    Ok((var_name, array_expr))
}

// parameter_with_type = Identifier ":" type
fn parse_parameter(tokenizer: &mut DopTokenizer) -> Result<DopParameter, RangeError> {
    let var_name = tokenizer.expect_variable_name()?;
    tokenizer.expect_token(DopToken::Colon)?;
    let typ = parse_type(tokenizer)?;
    Ok(DopParameter {
        var_name,
        type_annotation: typ.dop_type,
    })
}

// named_argument = Identifier ":" expr
fn parse_argument(tokenizer: &mut DopTokenizer) -> Result<DopArgument, RangeError> {
    let var_name = tokenizer.expect_variable_name()?;
    tokenizer.expect_token(DopToken::Colon)?;
    let expression = parse_equality(tokenizer)?;
    Ok(DopArgument {
        var_name,
        expression,
    })
}

// parameters = parameter ("," parameter)* Eof
pub fn parse_parameters(tokenizer: &mut DopTokenizer) -> Result<Vec<DopParameter>, RangeError> {
    let mut params = Vec::new();
    params.push(parse_parameter(tokenizer)?);
    loop {
        match tokenizer.peek() {
            (DopToken::Comma, _) => {
                tokenizer.advance()?; // consume comma
                // Handle trailing comma
                if let (DopToken::Eof, _) = tokenizer.peek() {
                    break;
                }
                // TODO: Check for duplicates
                params.push(parse_parameter(tokenizer)?);
            }
            _ => {
                tokenizer.expect_eof()?;
                break;
            }
        }
    }

    Ok(params)
}

// arguments = argument ("," argument)* Eof
pub fn parse_arguments(tokenizer: &mut DopTokenizer) -> Result<Vec<DopArgument>, RangeError> {
    let mut args = Vec::new();
    args.push(parse_argument(tokenizer)?);
    loop {
        match tokenizer.peek() {
            (DopToken::Comma, _) => {
                tokenizer.advance()?; // consume comma
                // Handle trailing comma
                if let (DopToken::Eof, _) = tokenizer.peek() {
                    break;
                }
                let arg = parse_argument(tokenizer)?;
                // Check for duplicates
                if args
                    .iter()
                    .any(|other| other.var_name.value == arg.var_name.value)
                {
                    return Err(RangeError::duplicate_argument(
                        &arg.var_name.value,
                        arg.var_name.range,
                    ));
                }
                args.push(arg);
            }
            _ => {
                tokenizer.expect_eof()?;
                break;
            }
        }
    }

    Ok(args)
}

// type = TypeString
//      | TypeNumber
//      | TypeBoolean
//      | TypeVoid
//      | TypeArray "[" type "]"
//      | "{" (Identifier ":" type ("," Identifier ":" type)*)? "}"
fn parse_type(tokenizer: &mut DopTokenizer) -> Result<RangeDopType, RangeError> {
    use crate::dop::DopType;
    use std::collections::BTreeMap;

    match tokenizer.advance()? {
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
        (DopToken::TypeArray, start_range) => {
            tokenizer.expect_token(DopToken::LeftBracket)?;
            let inner_type = parse_type(tokenizer)?;
            let (_, end_range) = tokenizer.expect_token(DopToken::RightBracket)?;

            Ok(RangeDopType {
                dop_type: DopType::Array(Some(Box::new(inner_type.dop_type))),
                range: Range::new(start_range.start, end_range.end),
            })
        }
        (DopToken::LeftBrace, start_range) => {
            let mut properties = BTreeMap::new();

            // TODO: Do we really need to handle empty object types?

            // Handle empty object type
            if let (DopToken::RightBrace, _) = tokenizer.peek() {
                let (_, end_range) = tokenizer.advance()?; // consume }
                return Ok(RangeDopType {
                    dop_type: DopType::Object(properties),
                    range: Range::new(start_range.start, end_range.end),
                });
            }

            loop {
                let (prop_name, prop_name_range) = tokenizer.expect_property_name()?;
                tokenizer.expect_token(DopToken::Colon)?;
                let typ = parse_type(tokenizer)?;
                if properties.contains_key(&prop_name) {
                    return Err(RangeError::duplicate_property(&prop_name, prop_name_range));
                }
                properties.insert(prop_name, typ.dop_type);

                // Check for comma or closing brace
                match tokenizer.peek() {
                    (DopToken::Comma, _) => {
                        tokenizer.advance()?; // consume ,

                        // Check for trailing comma (closing brace after comma)
                        if let (DopToken::RightBrace, _) = tokenizer.peek() {
                            let (_, end_range) = tokenizer.advance()?; // consume }
                            return Ok(RangeDopType {
                                dop_type: DopType::Object(properties),
                                range: Range::new(start_range.start, end_range.end),
                            });
                        }
                        continue;
                    }
                    (DopToken::RightBrace, _) => {
                        let (_, end_range) = tokenizer.advance()?; // consume }
                        return Ok(RangeDopType {
                            dop_type: DopType::Object(properties),
                            range: Range::new(start_range.start, end_range.end),
                        });
                    }
                    (DopToken::Eof, range) => {
                        return Err(RangeError::new("Missing closing '}'".to_string(), *range));
                    }
                    (token, range) => {
                        return Err(RangeError::new(
                            format!("Expected ',' or '}}' but got '{}'", token),
                            *range,
                        ));
                    }
                }
            }
        }
        (_, range) => Err(RangeError::new("Expected type name".to_string(), range)),
    }
}

// equality = unary ( "==" unary )*
fn parse_equality(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    let mut expr = parse_unary(tokenizer)?;

    while let (DopToken::Equal, _) = tokenizer.peek() {
        let (_, operator_range) = tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        let start = expr.range().start;
        let end = right.range().end;
        expr = DopExpr::BinaryOp {
            left: Box::new(expr),
            operator: BinaryOp::Equal,
            operator_range,
            right: Box::new(right),
            range: Range::new(start, end),
        };
    }

    Ok(expr)
}

// unary = ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.peek() {
        (DopToken::Not, _) => {
            let (_, operator_range) = tokenizer.advance()?; // consume !
            let expr = parse_unary(tokenizer)?; // Right associative for multiple !
            let end = expr.range().end;
            Ok(DopExpr::UnaryOp {
                operator: UnaryOp::Not,
                operator_range,
                operand: Box::new(expr),
                range: Range::new(operator_range.start, end),
            })
        }
        _ => parse_primary(tokenizer),
    }
}

// primary = Identifier ( "." Identifier )*
//         | StringLiteral
//         | BooleanLiteral
//         | NumberLiteral
//         | "[" ( equality ("," equality)* )? "]"
//         | "{" ( Identifier ":" equality ("," Identifier ":" equality)* )? "}"
//         | "(" equality ")"
fn parse_primary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.advance()? {
        (DopToken::Identifier(name), range) => {
            let mut expr = DopExpr::Variable { name, range };

            // Handle property access
            while let (DopToken::Dot, _) = tokenizer.peek() {
                tokenizer.advance()?; // consume .

                match tokenizer.advance()? {
                    (DopToken::Identifier(prop), property_range) => {
                        let start = expr.range().start;
                        expr = DopExpr::PropertyAccess {
                            object: Box::new(expr),
                            property: prop,
                            property_range,
                            range: Range {
                                start,
                                end: property_range.end,
                            },
                        };
                    }
                    (_, range) => {
                        return Err(RangeError::new(
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
        (DopToken::LeftBracket, start_range) => {
            let mut elements = Vec::new();

            // Handle empty array
            if let (DopToken::RightBracket, _) = tokenizer.peek() {
                let (_, end_range) = tokenizer.advance()?; // consume ]
                return Ok(DopExpr::ArrayLiteral {
                    elements,
                    range: Range {
                        start: start_range.start,
                        end: end_range.end,
                    },
                });
            }

            let end_range;

            // Parse array elements
            loop {
                elements.push(parse_equality(tokenizer)?);

                match tokenizer.advance()? {
                    (DopToken::Comma, _) => {
                        // Handle trailing comma
                        if let (DopToken::RightBracket, bracket_range) = tokenizer.peek() {
                            end_range = *bracket_range;
                            tokenizer.advance()?; // consume ]
                            break;
                        }
                        continue;
                    }
                    (DopToken::RightBracket, bracket_range) => {
                        end_range = bracket_range;
                        break;
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ',' or ']' in array literal".to_string(),
                            range,
                        ));
                    }
                }
            }

            Ok(DopExpr::ArrayLiteral {
                elements,
                range: Range {
                    start: start_range.start,
                    end: end_range.end,
                },
            })
        }
        (DopToken::LeftBrace, start_range) => {
            // Parse {key1: value1, key2: value2, ...}
            let mut properties = BTreeMap::new();

            // Handle empty object
            if let (DopToken::RightBrace, _) = tokenizer.peek() {
                let (_, end_range) = tokenizer.advance()?; // consume }
                let range = Range {
                    start: start_range.start,
                    end: end_range.end,
                };
                return Ok(DopExpr::ObjectLiteral { properties, range });
            }

            // Parse object properties
            loop {
                let (prop_name, prop_name_range) = tokenizer.expect_property_name()?;
                if properties.contains_key(&prop_name) {
                    return Err(RangeError::duplicate_property(&prop_name, prop_name_range));
                }

                tokenizer.expect_token(DopToken::Colon)?;

                properties.insert(prop_name, parse_equality(tokenizer)?);

                // Check for comma or closing brace
                match tokenizer.peek() {
                    (DopToken::Comma, _) => {
                        tokenizer.advance()?; // consume ,
                        // Check for trailing comma (closing brace after comma)
                        if let (DopToken::RightBrace, _) = tokenizer.peek() {
                            let (_, end_range) = tokenizer.advance()?; // consume }
                            return Ok(DopExpr::ObjectLiteral {
                                properties,
                                range: Range {
                                    start: start_range.start,
                                    end: end_range.end,
                                },
                            });
                        }
                        continue;
                    }
                    (DopToken::RightBrace, _) => {
                        let (_, end_range) = tokenizer.advance()?; // consume }
                        return Ok(DopExpr::ObjectLiteral {
                            properties,
                            range: Range {
                                start: start_range.start,
                                end: end_range.end,
                            },
                        });
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ',' or '}' after property value".to_string(),
                            *range,
                        ));
                    }
                }
            }
        }
        (DopToken::LeftParen, _) => {
            let expr = parse_equality(tokenizer)?;
            tokenizer.expect_token(DopToken::RightParen)?;
            Ok(expr)
        }
        (DopToken::Eof, range) => Err(RangeError::new(
            "Unexpected end of expression".to_string(),
            range,
        )),
        (token, range) => Err(RangeError::unexpected_token(&token, range)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tui::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};

    fn check_parse_expr(input: &str, expected: Expect) {
        let mut tokenizer = DopTokenizer::new(input, crate::common::Position::new(1, 1))
            .expect("Failed to create tokenizer");

        let actual = match parse_expr(&mut tokenizer) {
            Ok(result) => format!("{:#?}\n", result),
            Err(e) => {
                let annotator = SourceAnnotator::new()
                    .with_label("error")
                    .with_underline('^')
                    .without_location()
                    .without_line_numbers();

                annotator.annotate(input, &[e]).to_string()
            }
        };

        expected.assert_eq(&actual);
    }

    fn check_parse_parameters_with_types(input: &str, expected: Expect) {
        let mut tokenizer = DopTokenizer::new(input, crate::common::Position::new(1, 1))
            .expect("Failed to create tokenizer");

        let actual = match parse_parameters(&mut tokenizer) {
            Ok(result) => format!("{:#?}\n", result),
            Err(e) => {
                let annotator = SourceAnnotator::new()
                    .with_label("error")
                    .with_underline('^')
                    .without_location()
                    .without_line_numbers();

                annotator.annotate(input, &[e]).to_string()
            }
        };

        expected.assert_eq(&actual);
    }

    fn check_parse_named_arguments(input: &str, expected: Expect) {
        let mut tokenizer = DopTokenizer::new(input, crate::common::Position::new(1, 1))
            .expect("Failed to create tokenizer");

        let actual = match parse_arguments(&mut tokenizer) {
            Ok(result) => format!("{:#?}\n", result),
            Err(e) => {
                let annotator = SourceAnnotator::new()
                    .with_label("error")
                    .with_underline('^')
                    .without_location()
                    .without_line_numbers();

                annotator.annotate(input, &[e]).to_string()
            }
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_parse_parameters_with_types_trailing_brace() {
        check_parse_parameters_with_types(
            "params: {i: {j: {k: {l: boolean}}}}}",
            expect![[r#"
                error: Unexpected token '}'
                params: {i: {j: {k: {l: boolean}}}}}
                                                   ^
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_with_types_missing_brace() {
        check_parse_parameters_with_types(
            "params: {i: {j: {k: {l: boolean}}}",
            expect![[r#"
                error: Missing closing '}'
                params: {i: {j: {k: {l: boolean}}}
                                                  ^
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
    fn test_parse_expr_error_dot_no_identifier() {
        check_parse_expr(
            "user.",
            expect![[r#"
                error: Expected identifier after '.'
                user.
                     ^
            "#]],
        );
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
                error: Expected token ')'
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
        check_parse_expr(
            "x ==",
            expect![[r#"
                error: Unexpected end of expression
                x ==
                    ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_not_without_operand() {
        check_parse_expr(
            "!",
            expect![[r#"
                error: Unexpected end of expression
                !
                 ^
            "#]],
        );
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
                BinaryOp {
                    left: BinaryOp {
                        left: Variable {
                            name: "a",
                            range: 1:1-1:2,
                        },
                        operator: Equal,
                        operator_range: 1:3-1:5,
                        right: Variable {
                            name: "b",
                            range: 1:6-1:7,
                        },
                        range: 1:1-1:7,
                    },
                    operator: Equal,
                    operator_range: 1:8-1:10,
                    right: Variable {
                        name: "c",
                        range: 1:11-1:12,
                    },
                    range: 1:1-1:12,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_access_comparison() {
        check_parse_expr(
            "user.name == admin.name",
            expect![[r#"
                BinaryOp {
                    left: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: 1:1-1:5,
                        },
                        property: "name",
                        property_range: 1:6-1:10,
                        range: 1:1-1:10,
                    },
                    operator: Equal,
                    operator_range: 1:11-1:13,
                    right: PropertyAccess {
                        object: Variable {
                            name: "admin",
                            range: 1:14-1:19,
                        },
                        property: "name",
                        property_range: 1:20-1:24,
                        range: 1:14-1:24,
                    },
                    range: 1:1-1:24,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_access() {
        check_parse_expr(
            "app.user.profile.settings.theme",
            expect![[r#"
                PropertyAccess {
                    object: PropertyAccess {
                        object: PropertyAccess {
                            object: PropertyAccess {
                                object: Variable {
                                    name: "app",
                                    range: 1:1-1:4,
                                },
                                property: "user",
                                property_range: 1:5-1:9,
                                range: 1:1-1:9,
                            },
                            property: "profile",
                            property_range: 1:10-1:17,
                            range: 1:1-1:17,
                        },
                        property: "settings",
                        property_range: 1:18-1:26,
                        range: 1:1-1:26,
                    },
                    property: "theme",
                    property_range: 1:27-1:32,
                    range: 1:1-1:32,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_string() {
        check_parse_expr(
            "''",
            expect![[r#"
                StringLiteral {
                    value: "",
                    range: 1:1-1:3,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_number_literal_integer() {
        check_parse_expr(
            "99",
            expect![[r#"
                NumberLiteral {
                    value: Number(99),
                    range: 1:1-1:3,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_number_literal_float() {
        check_parse_expr(
            "3.14",
            expect![[r#"
                NumberLiteral {
                    value: Number(3.14),
                    range: 1:1-1:5,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_parenthesized() {
        check_parse_expr(
            "(x == y)",
            expect![[r#"
                BinaryOp {
                    left: Variable {
                        name: "x",
                        range: 1:2-1:3,
                    },
                    operator: Equal,
                    operator_range: 1:4-1:6,
                    right: Variable {
                        name: "y",
                        range: 1:7-1:8,
                    },
                    range: 1:2-1:8,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_simple_property_access() {
        check_parse_expr(
            "user.name",
            expect![[r#"
                PropertyAccess {
                    object: Variable {
                        name: "user",
                        range: 1:1-1:5,
                    },
                    property: "name",
                    property_range: 1:6-1:10,
                    range: 1:1-1:10,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_comparison() {
        check_parse_expr(
            "'guest' == user.role",
            expect![[r#"
                BinaryOp {
                    left: StringLiteral {
                        value: "guest",
                        range: 1:1-1:8,
                    },
                    operator: Equal,
                    operator_range: 1:9-1:11,
                    right: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: 1:12-1:16,
                        },
                        property: "role",
                        property_range: 1:17-1:21,
                        range: 1:12-1:21,
                    },
                    range: 1:1-1:21,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_variable_comparison() {
        check_parse_expr(
            "x == y",
            expect![[r#"
                BinaryOp {
                    left: Variable {
                        name: "x",
                        range: 1:1-1:2,
                    },
                    operator: Equal,
                    operator_range: 1:3-1:5,
                    right: Variable {
                        name: "y",
                        range: 1:6-1:7,
                    },
                    range: 1:1-1:7,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_literal() {
        check_parse_expr(
            "'hello'",
            expect![[r#"
                StringLiteral {
                    value: "hello",
                    range: 1:1-1:8,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_variable() {
        check_parse_expr(
            "x",
            expect![[r#"
                Variable {
                    name: "x",
                    range: 1:1-1:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_literal_comparison() {
        check_parse_expr(
            "'apple' == 'orange'",
            expect![[r#"
                BinaryOp {
                    left: StringLiteral {
                        value: "apple",
                        range: 1:1-1:8,
                    },
                    operator: Equal,
                    operator_range: 1:9-1:11,
                    right: StringLiteral {
                        value: "orange",
                        range: 1:12-1:20,
                    },
                    range: 1:1-1:20,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_string_comparison() {
        check_parse_expr(
            "user.name == 'admin'",
            expect![[r#"
                BinaryOp {
                    left: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: 1:1-1:5,
                        },
                        property: "name",
                        property_range: 1:6-1:10,
                        range: 1:1-1:10,
                    },
                    operator: Equal,
                    operator_range: 1:11-1:13,
                    right: StringLiteral {
                        value: "admin",
                        range: 1:14-1:21,
                    },
                    range: 1:1-1:21,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_with_space() {
        check_parse_expr(
            "'hello world'",
            expect![[r#"
                StringLiteral {
                    value: "hello world",
                    range: 1:1-1:14,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_whitespace_handling() {
        check_parse_expr(
            "  user . name   ==   admin . name  ",
            expect![[r#"
                BinaryOp {
                    left: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: 1:3-1:7,
                        },
                        property: "name",
                        property_range: 1:10-1:14,
                        range: 1:3-1:14,
                    },
                    operator: Equal,
                    operator_range: 1:17-1:19,
                    right: PropertyAccess {
                        object: Variable {
                            name: "admin",
                            range: 1:22-1:27,
                        },
                        property: "name",
                        property_range: 1:30-1:34,
                        range: 1:22-1:34,
                    },
                    range: 1:3-1:34,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_array() {
        check_parse_expr(
            "[]",
            expect![[r#"
                ArrayLiteral {
                    elements: [],
                    range: 1:1-1:3,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_numbers() {
        check_parse_expr(
            "[1, 2, 3]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: 1:2-1:3,
                        },
                        NumberLiteral {
                            value: Number(2),
                            range: 1:5-1:6,
                        },
                        NumberLiteral {
                            value: Number(3),
                            range: 1:8-1:9,
                        },
                    ],
                    range: 1:1-1:10,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_mixed_types() {
        check_parse_expr(
            "[1, 'hello', true]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: 1:2-1:3,
                        },
                        StringLiteral {
                            value: "hello",
                            range: 1:5-1:12,
                        },
                        BooleanLiteral {
                            value: true,
                            range: 1:14-1:18,
                        },
                    ],
                    range: 1:1-1:19,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_nested_arrays() {
        check_parse_expr(
            "[[1, 2], [3, 4]]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        ArrayLiteral {
                            elements: [
                                NumberLiteral {
                                    value: Number(1),
                                    range: 1:3-1:4,
                                },
                                NumberLiteral {
                                    value: Number(2),
                                    range: 1:6-1:7,
                                },
                            ],
                            range: 1:2-1:8,
                        },
                        ArrayLiteral {
                            elements: [
                                NumberLiteral {
                                    value: Number(3),
                                    range: 1:11-1:12,
                                },
                                NumberLiteral {
                                    value: Number(4),
                                    range: 1:14-1:15,
                                },
                            ],
                            range: 1:10-1:16,
                        },
                    ],
                    range: 1:1-1:17,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_variables() {
        check_parse_expr(
            "[x, user.name]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        Variable {
                            name: "x",
                            range: 1:2-1:3,
                        },
                        PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: 1:5-1:9,
                            },
                            property: "name",
                            property_range: 1:10-1:14,
                            range: 1:5-1:14,
                        },
                    ],
                    range: 1:1-1:15,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_object() {
        check_parse_expr(
            "{}",
            expect![[r#"
                ObjectLiteral {
                    properties: {},
                    range: 1:1-1:3,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_single_property() {
        check_parse_expr(
            "{name: 'John'}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "name": StringLiteral {
                            value: "John",
                            range: 1:8-1:14,
                        },
                    },
                    range: 1:1-1:15,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_multiple_properties() {
        check_parse_expr(
            "{a: 'foo', b: 1}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "a": StringLiteral {
                            value: "foo",
                            range: 1:5-1:10,
                        },
                        "b": NumberLiteral {
                            value: Number(1),
                            range: 1:15-1:16,
                        },
                    },
                    range: 1:1-1:17,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_complex_expressions() {
        check_parse_expr(
            "{user: user.name, active: !user.disabled}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "active": UnaryOp {
                            operator: Not,
                            operator_range: 1:27-1:28,
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: 1:28-1:32,
                                },
                                property: "disabled",
                                property_range: 1:33-1:41,
                                range: 1:28-1:41,
                            },
                            range: 1:27-1:41,
                        },
                        "user": PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: 1:8-1:12,
                            },
                            property: "name",
                            property_range: 1:13-1:17,
                            range: 1:8-1:17,
                        },
                    },
                    range: 1:1-1:42,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_nested() {
        check_parse_expr(
            "{nested: {inner: 'value'}}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "nested": ObjectLiteral {
                            properties: {
                                "inner": StringLiteral {
                                    value: "value",
                                    range: 1:18-1:25,
                                },
                            },
                            range: 1:10-1:26,
                        },
                    },
                    range: 1:1-1:27,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_multiline() {
        check_parse_expr(
            "[\n\t1,\n\t2,\n\t3,\n]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: 2:2-2:3,
                        },
                        NumberLiteral {
                            value: Number(2),
                            range: 3:2-3:3,
                        },
                        NumberLiteral {
                            value: Number(3),
                            range: 4:2-4:3,
                        },
                    ],
                    range: 1:1-5:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_single() {
        check_parse_expr(
            "[\n\t1,\n]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: 2:2-2:3,
                        },
                    ],
                    range: 1:1-3:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_complex() {
        check_parse_expr(
            "[\n\tuser.name,\n\t!user.disabled,\n]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: 2:2-2:6,
                            },
                            property: "name",
                            property_range: 2:7-2:11,
                            range: 2:2-2:11,
                        },
                        UnaryOp {
                            operator: Not,
                            operator_range: 3:2-3:3,
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: 3:3-3:7,
                                },
                                property: "disabled",
                                property_range: 3:8-3:16,
                                range: 3:3-3:16,
                            },
                            range: 3:2-3:16,
                        },
                    ],
                    range: 1:1-4:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_multiline() {
        check_parse_expr(
            "{\n\ta: 'foo',\n\tb: 1,\n}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "a": StringLiteral {
                            value: "foo",
                            range: 2:5-2:10,
                        },
                        "b": NumberLiteral {
                            value: Number(1),
                            range: 3:5-3:6,
                        },
                    },
                    range: 1:1-4:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_single() {
        check_parse_expr(
            "{\n\tname: 'John',\n}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "name": StringLiteral {
                            value: "John",
                            range: 2:8-2:14,
                        },
                    },
                    range: 1:1-3:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_complex() {
        check_parse_expr(
            "{\n\tuser: user.name,\n\tactive: !user.disabled,\n}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "active": UnaryOp {
                            operator: Not,
                            operator_range: 3:10-3:11,
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: 3:11-3:15,
                                },
                                property: "disabled",
                                property_range: 3:16-3:24,
                                range: 3:11-3:24,
                            },
                            range: 3:10-3:24,
                        },
                        "user": PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: 2:8-2:12,
                            },
                            property: "name",
                            property_range: 2:13-2:17,
                            range: 2:8-2:17,
                        },
                    },
                    range: 1:1-4:2,
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_single() {
        check_parse_named_arguments(
            "name: 'John'",
            expect![[r#"
                [
                    DopArgument {
                        var_name: DopVarName {
                            value: "name",
                            range: 1:1-1:5,
                        },
                        expression: StringLiteral {
                            value: "John",
                            range: 1:7-1:13,
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_multiple() {
        check_parse_named_arguments(
            "name: 'John', age: 25, active: true",
            expect![[r#"
                [
                    DopArgument {
                        var_name: DopVarName {
                            value: "name",
                            range: 1:1-1:5,
                        },
                        expression: StringLiteral {
                            value: "John",
                            range: 1:7-1:13,
                        },
                    },
                    DopArgument {
                        var_name: DopVarName {
                            value: "age",
                            range: 1:15-1:18,
                        },
                        expression: NumberLiteral {
                            value: Number(25),
                            range: 1:20-1:22,
                        },
                    },
                    DopArgument {
                        var_name: DopVarName {
                            value: "active",
                            range: 1:24-1:30,
                        },
                        expression: BooleanLiteral {
                            value: true,
                            range: 1:32-1:36,
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_complex_expressions() {
        check_parse_named_arguments(
            "user: user.name, enabled: !user.disabled",
            expect![[r#"
                [
                    DopArgument {
                        var_name: DopVarName {
                            value: "user",
                            range: 1:1-1:5,
                        },
                        expression: PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: 1:7-1:11,
                            },
                            property: "name",
                            property_range: 1:12-1:16,
                            range: 1:7-1:16,
                        },
                    },
                    DopArgument {
                        var_name: DopVarName {
                            value: "enabled",
                            range: 1:18-1:25,
                        },
                        expression: UnaryOp {
                            operator: Not,
                            operator_range: 1:27-1:28,
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: 1:28-1:32,
                                },
                                property: "disabled",
                                property_range: 1:33-1:41,
                                range: 1:28-1:41,
                            },
                            range: 1:27-1:41,
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_trailing_comma() {
        check_parse_named_arguments(
            "name: 'John', age: 25,",
            expect![[r#"
                [
                    DopArgument {
                        var_name: DopVarName {
                            value: "name",
                            range: 1:1-1:5,
                        },
                        expression: StringLiteral {
                            value: "John",
                            range: 1:7-1:13,
                        },
                    },
                    DopArgument {
                        var_name: DopVarName {
                            value: "age",
                            range: 1:15-1:18,
                        },
                        expression: NumberLiteral {
                            value: Number(25),
                            range: 1:20-1:22,
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_duplicate_error() {
        check_parse_named_arguments(
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
        check_parse_named_arguments(
            "name 'John'",
            expect![[r#"
                error: Expected token ':'
                name 'John'
                     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_missing_value_error() {
        check_parse_named_arguments(
            "name:",
            expect![[r#"
                error: Unexpected end of expression
                name:
                     ^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_invalid_start_error() {
        check_parse_named_arguments(
            "123: 'value'",
            expect![[r#"
                error: Expected variable name
                123: 'value'
                ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_unexpected_token_error() {
        check_parse_named_arguments(
            "name: 'John' age: 25",
            expect![[r#"
                error: Unexpected token 'age'
                name: 'John' age: 25
                             ^^^
            "#]],
        );
    }
}
