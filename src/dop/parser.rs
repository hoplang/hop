use std::collections::BTreeMap;

use crate::common::{Range, RangeError};
use crate::dop::DopType;
use crate::dop::tokenizer::{DopToken, DopTokenizer};

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
}

impl DopVarName {
    pub fn new(value: String) -> Option<Self> {
        let mut chars = value.chars();
        if !chars.next()?.is_ascii_lowercase() {
            return None;
        }
        if !chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_') {
            return None;
        }
        Some(DopVarName { value })
    }
}

// expr = equality Eof
pub fn parse_expr(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    let result = parse_equality(tokenizer)?;

    // expect Eof
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Unexpected token at end of expression".to_string(),
                *range,
            ));
        }
    }

    Ok(result)
}

// loop_header = Identifier "in" equality Eof
pub fn parse_loop_header(
    tokenizer: &mut DopTokenizer,
) -> Result<((DopVarName, Range), DopExpr), RangeError> {
    // expect Identifier
    let var_name = match tokenizer.advance()? {
        (DopToken::Identifier(name), range) => match DopVarName::new(name.clone()) {
            Some(var_name) => (var_name, range),
            None => return Err(RangeError::invalid_variable_name(&name, range)),
        },
        (_, range) => {
            return Err(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                range,
            ));
        }
    };

    // expect In
    match tokenizer.advance()? {
        (DopToken::In, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Expected 'in' keyword in <for> tag".to_string(),
                range,
            ));
        }
    }

    let array_expr = parse_equality(tokenizer)?;

    // expect Eof
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Unexpected token at end of <for> expression".to_string(),
                *range,
            ));
        }
    };

    Ok((var_name, array_expr))
}

// parameter_with_type = Identifier ":" type
fn parse_parameter_with_type(
    tokenizer: &mut DopTokenizer,
) -> Result<((DopVarName, Range), (DopType, Range)), RangeError> {
    // Parse parameter name
    let (var_name, param_range) = match tokenizer.advance()? {
        (DopToken::Identifier(name), range) => match DopVarName::new(name.clone()) {
            Some(var_name) => (var_name, range),
            None => {
                return Err(RangeError::invalid_variable_name(&name, range));
            }
        },
        (_, range) => {
            return Err(RangeError::new("Expected variable name".to_string(), range));
        }
    };

    // Require type annotation
    let (type_annotation, type_range) = match tokenizer.advance()? {
        (DopToken::Colon, _) => parse_type(tokenizer)?,
        (_, range) => {
            return Err(RangeError::new(
                "Expected ':' for type annotation".to_string(),
                range,
            ));
        }
    };

    Ok(((var_name, param_range), (type_annotation, type_range)))
}

// parameters_with_types = parameter_with_type ("," parameter_with_type)* Eof
pub fn parse_parameters_with_types(
    tokenizer: &mut DopTokenizer,
) -> Result<Vec<((DopVarName, Range), (DopType, Range))>, RangeError> {
    let mut params = Vec::new();

    // Parse first parameter
    params.push(parse_parameter_with_type(tokenizer)?);

    // Parse additional parameters if any (comma-separated)
    loop {
        match tokenizer.peek() {
            (DopToken::Comma, _) => {
                tokenizer.advance()?; // consume comma

                // Check for trailing comma (EOF after comma)
                if let (DopToken::Eof, _) = tokenizer.peek() {
                    break;
                }
                params.push(parse_parameter_with_type(tokenizer)?);
            }
            (DopToken::Eof, _) => break,
            (_, range) => {
                return Err(RangeError::new(
                    "Unexpected token after parameter".to_string(),
                    *range,
                ));
            }
        }
    }

    Ok(params)
}

// named_arguments = named_argument ("," named_argument)* Eof
// named_argument = Identifier ":" expr
pub fn parse_named_arguments(
    tokenizer: &mut DopTokenizer,
) -> Result<BTreeMap<String, DopExpr>, RangeError> {
    let mut args = BTreeMap::new();

    // Parse first argument
    let (name, expr) = parse_named_argument(tokenizer)?;
    args.insert(name, expr);

    // Parse additional arguments if any (comma-separated)
    loop {
        match tokenizer.peek() {
            (DopToken::Comma, _) => {
                tokenizer.advance()?; // consume comma

                // Check for trailing comma (EOF after comma)
                if let (DopToken::Eof, _) = tokenizer.peek() {
                    break;
                }
                let (name, expr) = parse_named_argument(tokenizer)?;
                if args.contains_key(&name) {
                    return Err(RangeError::new(
                        format!("Duplicate argument name '{}'", name),
                        tokenizer.peek().1,
                    ));
                }
                args.insert(name, expr);
            }
            (DopToken::Eof, _) => break,
            (_, range) => {
                return Err(RangeError::new(
                    "Unexpected token after argument".to_string(),
                    *range,
                ));
            }
        }
    }

    Ok(args)
}

// named_argument = Identifier ":" expr
fn parse_named_argument(tokenizer: &mut DopTokenizer) -> Result<(String, DopExpr), RangeError> {
    // Parse argument name
    let arg_name = match tokenizer.advance()? {
        (DopToken::Identifier(name), _) => name,
        (_, range) => {
            return Err(RangeError::new("Expected argument name".to_string(), range));
        }
    };

    // Expect colon
    match tokenizer.advance()? {
        (DopToken::Colon, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Expected ':' after argument name".to_string(),
                range,
            ));
        }
    }

    // Parse expression
    let expr = parse_equality(tokenizer)?;

    Ok((arg_name, expr))
}

// type = TypeString
//      | TypeNumber
//      | TypeBoolean
//      | TypeVoid
//      | TypeArray "[" type "]"
//      | TypeObject "[" (Identifier ":" type ("," Identifier ":" type)*)? "]"
//      | "{" (Identifier ":" type ("," Identifier ":" type)*)? "}"
fn parse_type(tokenizer: &mut DopTokenizer) -> Result<(DopType, Range), RangeError> {
    use crate::dop::DopType;
    use std::collections::BTreeMap;

    match tokenizer.advance()? {
        (DopToken::TypeString, range) => Ok((DopType::String, range)),
        (DopToken::TypeNumber, range) => Ok((DopType::Number, range)),
        (DopToken::TypeBoolean, range) => Ok((DopType::Bool, range)),
        (DopToken::TypeArray, start_range) => {
            // Expect [inner_type]
            match tokenizer.peek() {
                (DopToken::LeftBracket, _) => {
                    tokenizer.advance()?; // consume [
                    let (inner_type, _inner_range) = parse_type(tokenizer)?;

                    match tokenizer.peek() {
                        (DopToken::RightBracket, _) => {
                            let (_, end_range) = tokenizer.advance()?; // consume ]
                            Ok((
                                DopType::Array(Some(Box::new(inner_type))),
                                Range::new(start_range.start, end_range.end),
                            ))
                        }
                        (_, range) => Err(RangeError::new(
                            "Expected ']' after array type".to_string(),
                            *range,
                        )),
                    }
                }
                (_, range) => Err(RangeError::new(
                    "Expected '[' after 'array'".to_string(),
                    *range,
                )),
            }
        }
        (DopToken::TypeObject, start_range) => {
            // Expect [prop1: type1, prop2: type2, ...]
            match tokenizer.peek() {
                (DopToken::LeftBracket, _) => {
                    tokenizer.advance()?; // consume [
                    let mut properties = BTreeMap::new();

                    // Handle empty object
                    if let (DopToken::RightBracket, _) = tokenizer.peek() {
                        let (_, end_range) = tokenizer.advance()?; // consume ]
                        let complete_range = Range::new(start_range.start, end_range.end);
                        return Ok((DopType::Object(properties), complete_range));
                    }

                    loop {
                        // Parse property name
                        let prop_name = match tokenizer.advance()? {
                            (DopToken::Identifier(name), _) => name,
                            (_, range) => {
                                return Err(RangeError::new(
                                    "Expected property name".to_string(),
                                    range,
                                ));
                            }
                        };

                        // Expect colon
                        match tokenizer.peek() {
                            (DopToken::Colon, _) => {
                                tokenizer.advance()?; // consume :
                            }
                            (_, range) => {
                                return Err(RangeError::new(
                                    "Expected ':' after property name".to_string(),
                                    *range,
                                ));
                            }
                        }

                        // Parse property type
                        let (prop_type, _prop_range) = parse_type(tokenizer)?;
                        properties.insert(prop_name, prop_type);

                        // Check for comma or closing bracket
                        match tokenizer.peek() {
                            (DopToken::Comma, _) => {
                                tokenizer.advance()?; // consume ,
                                continue;
                            }
                            (DopToken::RightBracket, _) => {
                                let (_, end_range) = tokenizer.advance()?; // consume ]
                                return Ok((
                                    DopType::Object(properties),
                                    Range::new(start_range.start, end_range.end),
                                ));
                            }
                            (_, range) => {
                                return Err(RangeError::new(
                                    "Expected ',' or ']' after property type".to_string(),
                                    *range,
                                ));
                            }
                        }
                    }
                }
                (_, range) => Err(RangeError::new(
                    "Expected '[' after 'object'".to_string(),
                    *range,
                )),
            }
        }
        (DopToken::LeftBrace, start_range) => {
            // Parse {prop1: type1, prop2: type2, ...}
            let mut properties = BTreeMap::new();

            // Handle empty object type
            if let (DopToken::RightBrace, _) = tokenizer.peek() {
                let (_, end_range) = tokenizer.advance()?; // consume }
                return Ok((
                    DopType::Object(properties),
                    Range::new(start_range.start, end_range.end),
                ));
            }

            loop {
                // Parse property name
                let prop_name = match tokenizer.advance()? {
                    (DopToken::Identifier(name), _) => name,
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected property name in object type".to_string(),
                            range,
                        ));
                    }
                };

                // Expect colon
                match tokenizer.advance()? {
                    (DopToken::Colon, _) => {}
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ':' after property name in object type".to_string(),
                            range,
                        ));
                    }
                }

                // Parse property type
                let (prop_type, _prop_range) = parse_type(tokenizer)?;
                properties.insert(prop_name, prop_type);

                // Check for comma or closing brace
                match tokenizer.peek() {
                    (DopToken::Comma, _) => {
                        tokenizer.advance()?; // consume ,

                        // Check for trailing comma (closing brace after comma)
                        if let (DopToken::RightBrace, _) = tokenizer.peek() {
                            let (_, end_range) = tokenizer.advance()?; // consume }
                            return Ok((
                                DopType::Object(properties),
                                Range::new(start_range.start, end_range.end),
                            ));
                        }
                        continue;
                    }
                    (DopToken::RightBrace, _) => {
                        let (_, end_range) = tokenizer.advance()?; // consume }
                        return Ok((
                            DopType::Object(properties),
                            Range::new(start_range.start, end_range.end),
                        ));
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ',' or '}' after property type".to_string(),
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
            if let (DopToken::RightBracket, end_range) = tokenizer.peek() {
                let close_range = *end_range;
                tokenizer.advance()?; // consume ]
                let range = Range {
                    start: start_range.start,
                    end: close_range.end,
                };
                return Ok(DopExpr::ArrayLiteral { elements, range });
            }

            // Parse array elements
            loop {
                elements.push(parse_equality(tokenizer)?);

                match tokenizer.peek() {
                    (DopToken::Comma, _) => {
                        tokenizer.advance()?; // consume ,
                        // Check for trailing comma (closing bracket after comma)
                        if let (DopToken::RightBracket, end_range) = tokenizer.peek() {
                            let close_range = *end_range;
                            tokenizer.advance()?; // consume ]
                            let range = Range {
                                start: start_range.start,
                                end: close_range.end,
                            };
                            return Ok(DopExpr::ArrayLiteral { elements, range });
                        }
                        continue;
                    }
                    (DopToken::RightBracket, end_range) => {
                        let close_range = *end_range;
                        tokenizer.advance()?; // consume ]
                        let range = Range {
                            start: start_range.start,
                            end: close_range.end,
                        };
                        return Ok(DopExpr::ArrayLiteral { elements, range });
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ',' or ']' in array literal".to_string(),
                            *range,
                        ));
                    }
                }
            }
        }
        (DopToken::LeftBrace, start_range) => {
            // Parse {key1: value1, key2: value2, ...}
            let mut properties = BTreeMap::new();

            // Handle empty object
            if let (DopToken::RightBrace, end_range) = tokenizer.peek() {
                let close_range = *end_range;
                tokenizer.advance()?; // consume }
                let range = Range {
                    start: start_range.start,
                    end: close_range.end,
                };
                return Ok(DopExpr::ObjectLiteral { properties, range });
            }

            // Parse object properties
            loop {
                // Parse property name
                let prop_name = match tokenizer.advance()? {
                    (DopToken::Identifier(name), _) => name,
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected property name in object literal".to_string(),
                            range,
                        ));
                    }
                };

                // Expect colon
                match tokenizer.advance()? {
                    (DopToken::Colon, _) => {}
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ':' after property name".to_string(),
                            range,
                        ));
                    }
                }

                // Parse property value
                let prop_value = parse_equality(tokenizer)?;
                if properties.contains_key(&prop_name) {
                    return Err(RangeError::new(
                        format!("Duplicate property name '{}'", prop_name),
                        tokenizer.peek().1,
                    ));
                }
                properties.insert(prop_name, prop_value);

                // Check for comma or closing brace
                match tokenizer.peek() {
                    (DopToken::Comma, _) => {
                        tokenizer.advance()?; // consume ,
                        // Check for trailing comma (closing brace after comma)
                        if let (DopToken::RightBrace, end_range) = tokenizer.peek() {
                            let close_range = *end_range;
                            tokenizer.advance()?; // consume }
                            let range = Range {
                                start: start_range.start,
                                end: close_range.end,
                            };
                            return Ok(DopExpr::ObjectLiteral { properties, range });
                        }
                        continue;
                    }
                    (DopToken::RightBrace, end_range) => {
                        let close_range = *end_range;
                        tokenizer.advance()?; // consume }
                        let range = Range {
                            start: start_range.start,
                            end: close_range.end,
                        };
                        return Ok(DopExpr::ObjectLiteral { properties, range });
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

            match tokenizer.peek() {
                (DopToken::RightParen, _) => {
                    tokenizer.advance()?; // consume )
                    Ok(expr)
                }
                (_, range) => Err(RangeError::new(
                    "Missing closing parenthesis".to_string(),
                    *range,
                )),
            }
        }
        (_, range) => Err(RangeError::new(
            "Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis"
                .to_string(),
            range,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
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

    #[test]
    fn test_parse_expr_error_trailing_tokens() {
        check(
            "x y",
            expect![[r#"
                error: Unexpected token at end of expression
                x y
                  ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_dot_no_identifier() {
        check(
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
        check(
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
        check(
            "== x",
            expect![[r#"
                error: Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis
                == x
                ^^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_unclosed_paren() {
        check(
            "(x == y",
            expect![[r#"
                error: Missing closing parenthesis
                (x == y
                       ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_unmatched_closing_paren() {
        check(
            "x == y)",
            expect![[r#"
                error: Unexpected token at end of expression
                x == y)
                      ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_empty_parens() {
        check(
            "()",
            expect![[r#"
                error: Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis
                ()
                 ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_invalid_after_equals() {
        check(
            "x == )",
            expect![[r#"
                error: Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis
                x == )
                     ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_dot_at_start() {
        check(
            ".property",
            expect![[r#"
                error: Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis
                .property
                ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_double_dot() {
        check(
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
        check(
            "x ==",
            expect![[r#"
                error: Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis
                x ==
                    ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_not_without_operand() {
        check(
            "!",
            expect![[r#"
                error: Expected identifier, string literal, number literal, array literal, object literal, or opening parenthesis
                !
                 ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_trailing_not() {
        check(
            "x !",
            expect![[r#"
                error: Unexpected token at end of expression
                x !
                  ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_binary_op_chained() {
        check(
            "a == b == c",
            expect![[r#"
                BinaryOp {
                    left: BinaryOp {
                        left: Variable {
                            name: "a",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 2,
                                },
                            },
                        },
                        operator: Equal,
                        operator_range: Range {
                            start: Position {
                                line: 1,
                                column: 3,
                            },
                            end: Position {
                                line: 1,
                                column: 5,
                            },
                        },
                        right: Variable {
                            name: "b",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 6,
                                },
                                end: Position {
                                    line: 1,
                                    column: 7,
                                },
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 7,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 8,
                        },
                        end: Position {
                            line: 1,
                            column: 10,
                        },
                    },
                    right: Variable {
                        name: "c",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 11,
                            },
                            end: Position {
                                line: 1,
                                column: 12,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 12,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_access_comparison() {
        check(
            "user.name == admin.name",
            expect![[r#"
                BinaryOp {
                    left: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 5,
                                },
                            },
                        },
                        property: "name",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 6,
                            },
                            end: Position {
                                line: 1,
                                column: 10,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 10,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 11,
                        },
                        end: Position {
                            line: 1,
                            column: 13,
                        },
                    },
                    right: PropertyAccess {
                        object: Variable {
                            name: "admin",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 14,
                                },
                                end: Position {
                                    line: 1,
                                    column: 19,
                                },
                            },
                        },
                        property: "name",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 20,
                            },
                            end: Position {
                                line: 1,
                                column: 24,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 14,
                            },
                            end: Position {
                                line: 1,
                                column: 24,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 24,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_access() {
        check(
            "app.user.profile.settings.theme",
            expect![[r#"
                PropertyAccess {
                    object: PropertyAccess {
                        object: PropertyAccess {
                            object: PropertyAccess {
                                object: Variable {
                                    name: "app",
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 1,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 4,
                                        },
                                    },
                                },
                                property: "user",
                                property_range: Range {
                                    start: Position {
                                        line: 1,
                                        column: 5,
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 9,
                                    },
                                },
                                range: Range {
                                    start: Position {
                                        line: 1,
                                        column: 1,
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 9,
                                    },
                                },
                            },
                            property: "profile",
                            property_range: Range {
                                start: Position {
                                    line: 1,
                                    column: 10,
                                },
                                end: Position {
                                    line: 1,
                                    column: 17,
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 17,
                                },
                            },
                        },
                        property: "settings",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 18,
                            },
                            end: Position {
                                line: 1,
                                column: 26,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 26,
                            },
                        },
                    },
                    property: "theme",
                    property_range: Range {
                        start: Position {
                            line: 1,
                            column: 27,
                        },
                        end: Position {
                            line: 1,
                            column: 32,
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 32,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_string() {
        check(
            "''",
            expect![[r#"
                StringLiteral {
                    value: "",
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 3,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_number_literal_integer() {
        check(
            "99",
            expect![[r#"
                NumberLiteral {
                    value: Number(99),
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 3,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_number_literal_float() {
        check(
            "3.14",
            expect![[r#"
                NumberLiteral {
                    value: Number(3.14),
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 5,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_parenthesized() {
        check(
            "(x == y)",
            expect![[r#"
                BinaryOp {
                    left: Variable {
                        name: "x",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 2,
                            },
                            end: Position {
                                line: 1,
                                column: 3,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 4,
                        },
                        end: Position {
                            line: 1,
                            column: 6,
                        },
                    },
                    right: Variable {
                        name: "y",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 7,
                            },
                            end: Position {
                                line: 1,
                                column: 8,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 2,
                        },
                        end: Position {
                            line: 1,
                            column: 8,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_simple_property_access() {
        check(
            "user.name",
            expect![[r#"
                PropertyAccess {
                    object: Variable {
                        name: "user",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 5,
                            },
                        },
                    },
                    property: "name",
                    property_range: Range {
                        start: Position {
                            line: 1,
                            column: 6,
                        },
                        end: Position {
                            line: 1,
                            column: 10,
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 10,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_comparison() {
        check(
            "'guest' == user.role",
            expect![[r#"
                BinaryOp {
                    left: StringLiteral {
                        value: "guest",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 8,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            line: 1,
                            column: 11,
                        },
                    },
                    right: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 12,
                                },
                                end: Position {
                                    line: 1,
                                    column: 16,
                                },
                            },
                        },
                        property: "role",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 17,
                            },
                            end: Position {
                                line: 1,
                                column: 21,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 12,
                            },
                            end: Position {
                                line: 1,
                                column: 21,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 21,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_variable_comparison() {
        check(
            "x == y",
            expect![[r#"
                BinaryOp {
                    left: Variable {
                        name: "x",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 2,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 3,
                        },
                        end: Position {
                            line: 1,
                            column: 5,
                        },
                    },
                    right: Variable {
                        name: "y",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 6,
                            },
                            end: Position {
                                line: 1,
                                column: 7,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 7,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_literal() {
        check(
            "'hello'",
            expect![[r#"
                StringLiteral {
                    value: "hello",
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 8,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_variable() {
        check(
            "x",
            expect![[r#"
                Variable {
                    name: "x",
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_literal_comparison() {
        check(
            "'apple' == 'orange'",
            expect![[r#"
                BinaryOp {
                    left: StringLiteral {
                        value: "apple",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 8,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 9,
                        },
                        end: Position {
                            line: 1,
                            column: 11,
                        },
                    },
                    right: StringLiteral {
                        value: "orange",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 12,
                            },
                            end: Position {
                                line: 1,
                                column: 20,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 20,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_string_comparison() {
        check(
            "user.name == 'admin'",
            expect![[r#"
                BinaryOp {
                    left: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 5,
                                },
                            },
                        },
                        property: "name",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 6,
                            },
                            end: Position {
                                line: 1,
                                column: 10,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 1,
                            },
                            end: Position {
                                line: 1,
                                column: 10,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 11,
                        },
                        end: Position {
                            line: 1,
                            column: 13,
                        },
                    },
                    right: StringLiteral {
                        value: "admin",
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 14,
                            },
                            end: Position {
                                line: 1,
                                column: 21,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 21,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_with_space() {
        check(
            "'hello world'",
            expect![[r#"
                StringLiteral {
                    value: "hello world",
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 14,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_whitespace_handling() {
        check(
            "  user . name   ==   admin . name  ",
            expect![[r#"
                BinaryOp {
                    left: PropertyAccess {
                        object: Variable {
                            name: "user",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 3,
                                },
                                end: Position {
                                    line: 1,
                                    column: 7,
                                },
                            },
                        },
                        property: "name",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 10,
                            },
                            end: Position {
                                line: 1,
                                column: 14,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 3,
                            },
                            end: Position {
                                line: 1,
                                column: 14,
                            },
                        },
                    },
                    operator: Equal,
                    operator_range: Range {
                        start: Position {
                            line: 1,
                            column: 17,
                        },
                        end: Position {
                            line: 1,
                            column: 19,
                        },
                    },
                    right: PropertyAccess {
                        object: Variable {
                            name: "admin",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 22,
                                },
                                end: Position {
                                    line: 1,
                                    column: 27,
                                },
                            },
                        },
                        property: "name",
                        property_range: Range {
                            start: Position {
                                line: 1,
                                column: 30,
                            },
                            end: Position {
                                line: 1,
                                column: 34,
                            },
                        },
                        range: Range {
                            start: Position {
                                line: 1,
                                column: 22,
                            },
                            end: Position {
                                line: 1,
                                column: 34,
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 3,
                        },
                        end: Position {
                            line: 1,
                            column: 34,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_array() {
        check(
            "[]",
            expect![[r#"
                ArrayLiteral {
                    elements: [],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 3,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_numbers() {
        check(
            "[1, 2, 3]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 3,
                                },
                            },
                        },
                        NumberLiteral {
                            value: Number(2),
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        },
                        NumberLiteral {
                            value: Number(3),
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                },
                                end: Position {
                                    line: 1,
                                    column: 9,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 10,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_mixed_types() {
        check(
            "[1, 'hello', true]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 3,
                                },
                            },
                        },
                        StringLiteral {
                            value: "hello",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 12,
                                },
                            },
                        },
                        BooleanLiteral {
                            value: true,
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 14,
                                },
                                end: Position {
                                    line: 1,
                                    column: 18,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 19,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_nested_arrays() {
        check(
            "[[1, 2], [3, 4]]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        ArrayLiteral {
                            elements: [
                                NumberLiteral {
                                    value: Number(1),
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 3,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 4,
                                        },
                                    },
                                },
                                NumberLiteral {
                                    value: Number(2),
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 6,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 7,
                                        },
                                    },
                                },
                            ],
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 8,
                                },
                            },
                        },
                        ArrayLiteral {
                            elements: [
                                NumberLiteral {
                                    value: Number(3),
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 11,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 12,
                                        },
                                    },
                                },
                                NumberLiteral {
                                    value: Number(4),
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 14,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 15,
                                        },
                                    },
                                },
                            ],
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 10,
                                },
                                end: Position {
                                    line: 1,
                                    column: 16,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 17,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_variables() {
        check(
            "[x, user.name]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        Variable {
                            name: "x",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 3,
                                },
                            },
                        },
                        PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: Range {
                                    start: Position {
                                        line: 1,
                                        column: 5,
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 9,
                                    },
                                },
                            },
                            property: "name",
                            property_range: Range {
                                start: Position {
                                    line: 1,
                                    column: 10,
                                },
                                end: Position {
                                    line: 1,
                                    column: 14,
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 14,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 15,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_empty_object() {
        check(
            "{}",
            expect![[r#"
                ObjectLiteral {
                    properties: {},
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 3,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_single_property() {
        check(
            "{name: 'John'}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "name": StringLiteral {
                            value: "John",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                },
                                end: Position {
                                    line: 1,
                                    column: 14,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 15,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_multiple_properties() {
        check(
            "{a: 'foo', b: 1}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "a": StringLiteral {
                            value: "foo",
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 10,
                                },
                            },
                        },
                        "b": NumberLiteral {
                            value: Number(1),
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 15,
                                },
                                end: Position {
                                    line: 1,
                                    column: 16,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 17,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_complex_expressions() {
        check(
            "{user: user.name, active: !user.disabled}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "active": UnaryOp {
                            operator: Not,
                            operator_range: Range {
                                start: Position {
                                    line: 1,
                                    column: 27,
                                },
                                end: Position {
                                    line: 1,
                                    column: 28,
                                },
                            },
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 28,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 32,
                                        },
                                    },
                                },
                                property: "disabled",
                                property_range: Range {
                                    start: Position {
                                        line: 1,
                                        column: 33,
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 41,
                                    },
                                },
                                range: Range {
                                    start: Position {
                                        line: 1,
                                        column: 28,
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 41,
                                    },
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 27,
                                },
                                end: Position {
                                    line: 1,
                                    column: 41,
                                },
                            },
                        },
                        "user": PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: Range {
                                    start: Position {
                                        line: 1,
                                        column: 8,
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 12,
                                    },
                                },
                            },
                            property: "name",
                            property_range: Range {
                                start: Position {
                                    line: 1,
                                    column: 13,
                                },
                                end: Position {
                                    line: 1,
                                    column: 17,
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                },
                                end: Position {
                                    line: 1,
                                    column: 17,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 42,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_nested() {
        check(
            "{nested: {inner: 'value'}}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "nested": ObjectLiteral {
                            properties: {
                                "inner": StringLiteral {
                                    value: "value",
                                    range: Range {
                                        start: Position {
                                            line: 1,
                                            column: 18,
                                        },
                                        end: Position {
                                            line: 1,
                                            column: 25,
                                        },
                                    },
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 1,
                                    column: 10,
                                },
                                end: Position {
                                    line: 1,
                                    column: 26,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 1,
                            column: 27,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_multiline() {
        check(
            "[\n\t1,\n\t2,\n\t3,\n]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: Range {
                                start: Position {
                                    line: 2,
                                    column: 2,
                                },
                                end: Position {
                                    line: 2,
                                    column: 3,
                                },
                            },
                        },
                        NumberLiteral {
                            value: Number(2),
                            range: Range {
                                start: Position {
                                    line: 3,
                                    column: 2,
                                },
                                end: Position {
                                    line: 3,
                                    column: 3,
                                },
                            },
                        },
                        NumberLiteral {
                            value: Number(3),
                            range: Range {
                                start: Position {
                                    line: 4,
                                    column: 2,
                                },
                                end: Position {
                                    line: 4,
                                    column: 3,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 5,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_single() {
        check(
            "[\n\t1,\n]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        NumberLiteral {
                            value: Number(1),
                            range: Range {
                                start: Position {
                                    line: 2,
                                    column: 2,
                                },
                                end: Position {
                                    line: 2,
                                    column: 3,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 3,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_array_trailing_comma_complex() {
        check(
            "[\n\tuser.name,\n\t!user.disabled,\n]",
            expect![[r#"
                ArrayLiteral {
                    elements: [
                        PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: Range {
                                    start: Position {
                                        line: 2,
                                        column: 2,
                                    },
                                    end: Position {
                                        line: 2,
                                        column: 6,
                                    },
                                },
                            },
                            property: "name",
                            property_range: Range {
                                start: Position {
                                    line: 2,
                                    column: 7,
                                },
                                end: Position {
                                    line: 2,
                                    column: 11,
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 2,
                                    column: 2,
                                },
                                end: Position {
                                    line: 2,
                                    column: 11,
                                },
                            },
                        },
                        UnaryOp {
                            operator: Not,
                            operator_range: Range {
                                start: Position {
                                    line: 3,
                                    column: 2,
                                },
                                end: Position {
                                    line: 3,
                                    column: 3,
                                },
                            },
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: Range {
                                        start: Position {
                                            line: 3,
                                            column: 3,
                                        },
                                        end: Position {
                                            line: 3,
                                            column: 7,
                                        },
                                    },
                                },
                                property: "disabled",
                                property_range: Range {
                                    start: Position {
                                        line: 3,
                                        column: 8,
                                    },
                                    end: Position {
                                        line: 3,
                                        column: 16,
                                    },
                                },
                                range: Range {
                                    start: Position {
                                        line: 3,
                                        column: 3,
                                    },
                                    end: Position {
                                        line: 3,
                                        column: 16,
                                    },
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 3,
                                    column: 2,
                                },
                                end: Position {
                                    line: 3,
                                    column: 16,
                                },
                            },
                        },
                    ],
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 4,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_multiline() {
        check(
            "{\n\ta: 'foo',\n\tb: 1,\n}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "a": StringLiteral {
                            value: "foo",
                            range: Range {
                                start: Position {
                                    line: 2,
                                    column: 5,
                                },
                                end: Position {
                                    line: 2,
                                    column: 10,
                                },
                            },
                        },
                        "b": NumberLiteral {
                            value: Number(1),
                            range: Range {
                                start: Position {
                                    line: 3,
                                    column: 5,
                                },
                                end: Position {
                                    line: 3,
                                    column: 6,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 4,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_single() {
        check(
            "{\n\tname: 'John',\n}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "name": StringLiteral {
                            value: "John",
                            range: Range {
                                start: Position {
                                    line: 2,
                                    column: 8,
                                },
                                end: Position {
                                    line: 2,
                                    column: 14,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 3,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_complex() {
        check(
            "{\n\tuser: user.name,\n\tactive: !user.disabled,\n}",
            expect![[r#"
                ObjectLiteral {
                    properties: {
                        "active": UnaryOp {
                            operator: Not,
                            operator_range: Range {
                                start: Position {
                                    line: 3,
                                    column: 10,
                                },
                                end: Position {
                                    line: 3,
                                    column: 11,
                                },
                            },
                            operand: PropertyAccess {
                                object: Variable {
                                    name: "user",
                                    range: Range {
                                        start: Position {
                                            line: 3,
                                            column: 11,
                                        },
                                        end: Position {
                                            line: 3,
                                            column: 15,
                                        },
                                    },
                                },
                                property: "disabled",
                                property_range: Range {
                                    start: Position {
                                        line: 3,
                                        column: 16,
                                    },
                                    end: Position {
                                        line: 3,
                                        column: 24,
                                    },
                                },
                                range: Range {
                                    start: Position {
                                        line: 3,
                                        column: 11,
                                    },
                                    end: Position {
                                        line: 3,
                                        column: 24,
                                    },
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 3,
                                    column: 10,
                                },
                                end: Position {
                                    line: 3,
                                    column: 24,
                                },
                            },
                        },
                        "user": PropertyAccess {
                            object: Variable {
                                name: "user",
                                range: Range {
                                    start: Position {
                                        line: 2,
                                        column: 8,
                                    },
                                    end: Position {
                                        line: 2,
                                        column: 12,
                                    },
                                },
                            },
                            property: "name",
                            property_range: Range {
                                start: Position {
                                    line: 2,
                                    column: 13,
                                },
                                end: Position {
                                    line: 2,
                                    column: 17,
                                },
                            },
                            range: Range {
                                start: Position {
                                    line: 2,
                                    column: 8,
                                },
                                end: Position {
                                    line: 2,
                                    column: 17,
                                },
                            },
                        },
                    },
                    range: Range {
                        start: Position {
                            line: 1,
                            column: 1,
                        },
                        end: Position {
                            line: 4,
                            column: 2,
                        },
                    },
                }
            "#]],
        );
    }
}
