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
    },
    PropertyAccess {
        object: Box<DopExpr>,
        property: String,
    },
    StringLiteral {
        value: String,
    },
    BooleanLiteral {
        value: bool,
    },
    NumberLiteral {
        value: serde_json::Number,
    },
    ArrayLiteral {
        elements: Vec<DopExpr>,
    },
    ObjectLiteral {
        properties: BTreeMap<String, DopExpr>,
    },
    BinaryOp {
        left: Box<DopExpr>,
        operator: BinaryOp,
        right: Box<DopExpr>,
    },
    UnaryOp {
        operator: UnaryOp,
        operand: Box<DopExpr>,
    },
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
) -> Result<((DopVarName, Range), (DopExpr, Range)), RangeError> {
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

    // Get the start position of the array expression
    let (_, array_expr_start_range) = tokenizer.peek();
    let array_expr_start = array_expr_start_range.start;
    let array_expr = parse_equality(tokenizer)?;

    // expect Eof
    let array_expr_end = match tokenizer.peek() {
        (DopToken::Eof, range) => range.start,
        (_, range) => {
            return Err(RangeError::new(
                "Unexpected token at end of <for> expression".to_string(),
                *range,
            ));
        }
    };

    let array_expr_range = Range::new(array_expr_start, array_expr_end);
    Ok((var_name, (array_expr, array_expr_range)))
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
        tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp {
            left: Box::new(expr),
            operator: BinaryOp::Equal,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

// unary = ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.peek() {
        (DopToken::Not, _) => {
            tokenizer.advance()?; // consume !
            let expr = parse_unary(tokenizer)?; // Right associative for multiple !
            Ok(DopExpr::UnaryOp {
                operator: UnaryOp::Not,
                operand: Box::new(expr),
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
        (DopToken::Identifier(name), _) => {
            let mut expr = DopExpr::Variable { name };

            // Handle property access
            while let (DopToken::Dot, _) = tokenizer.peek() {
                tokenizer.advance()?; // consume .

                match tokenizer.advance()? {
                    (DopToken::Identifier(prop), _) => {
                        expr = DopExpr::PropertyAccess { object: Box::new(expr), property: prop };
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
        (DopToken::StringLiteral(value), _) => Ok(DopExpr::StringLiteral { value }),
        (DopToken::BooleanLiteral(value), _) => Ok(DopExpr::BooleanLiteral { value }),
        (DopToken::NumberLiteral(value), _) => Ok(DopExpr::NumberLiteral { value }),
        (DopToken::LeftBracket, _) => {
            let mut elements = Vec::new();

            // Handle empty array
            if let (DopToken::RightBracket, _) = tokenizer.peek() {
                tokenizer.advance()?; // consume ]
                return Ok(DopExpr::ArrayLiteral { elements });
            }

            // Parse array elements
            loop {
                elements.push(parse_equality(tokenizer)?);

                match tokenizer.peek() {
                    (DopToken::Comma, _) => {
                        tokenizer.advance()?; // consume ,
                        // Check for trailing comma (closing bracket after comma)
                        if let (DopToken::RightBracket, _) = tokenizer.peek() {
                            tokenizer.advance()?; // consume ]
                            break;
                        }
                        continue;
                    }
                    (DopToken::RightBracket, _) => {
                        tokenizer.advance()?; // consume ]
                        break;
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ',' or ']' in array literal".to_string(),
                            *range,
                        ));
                    }
                }
            }

            Ok(DopExpr::ArrayLiteral { elements })
        }
        (DopToken::LeftBrace, _) => {
            // Parse {key1: value1, key2: value2, ...}
            let mut properties = BTreeMap::new();

            // Handle empty object
            if let (DopToken::RightBrace, _) = tokenizer.peek() {
                tokenizer.advance()?; // consume }
                return Ok(DopExpr::ObjectLiteral { properties });
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
                        if let (DopToken::RightBrace, _) = tokenizer.peek() {
                            tokenizer.advance()?; // consume }
                            break;
                        }
                        continue;
                    }
                    (DopToken::RightBrace, _) => {
                        tokenizer.advance()?; // consume }
                        break;
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected ',' or '}' after property value".to_string(),
                            *range,
                        ));
                    }
                }
            }

            Ok(DopExpr::ObjectLiteral { properties })
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
    use crate::test_utils::parse_test_cases;

    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_parse_expr() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/parse_expr.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (archive, line_number)) in test_cases.iter().enumerate() {
            let input = archive
                .get("in")
                .expect("Missing 'in' section in test case")
                .content
                .trim();
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();

            println!("Test case {} (line {})", case_num + 1, line_number);

            let mut tokenizer = DopTokenizer::new(input, crate::common::Position::new(1, 1))
                .unwrap_or_else(|e| {
                    panic!(
                        "Failed to create tokenizer for '{}' in test case {} (line {}): {:?}",
                        input,
                        case_num + 1,
                        line_number,
                        e
                    );
                });

            let actual = match parse_expr(&mut tokenizer) {
                Ok(result) => format!("{:?}", result),
                Err(e) => {
                    let annotator = SourceAnnotator::new()
                        .with_label("error")
                        .with_underline('^')
                        .without_location()
                        .without_line_numbers();

                    annotator.annotate(input, &[e]).trim().to_string()
                }
            };

            assert_eq!(
                actual,
                expected,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
        }
    }
}
