use crate::common::{Range, RangeError};
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
    Variable(String),
    PropertyAccess(Box<DopExpr>, String),
    StringLiteral(String),
    BooleanLiteral(bool),
    NumberLiteral(serde_json::Number),
    BinaryOp(Box<DopExpr>, BinaryOp, Box<DopExpr>),
    UnaryOp(UnaryOp, Box<DopExpr>),
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
        if !chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit()) {
            return None;
        }
        Some(DopVarName { value })
    }
}

pub fn parse_expr(expr: &str, expr_range: Range) -> Result<DopExpr, RangeError> {
    if expr.trim().is_empty() {
        return Err(RangeError::new("Empty expression".to_string(), expr_range));
    }

    let mut tokenizer = DopTokenizer::new(expr, expr_range.start)?;
    let result = parse_equality(&mut tokenizer)?;

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

pub fn parse_loop_header(
    header: &str,
    header_range: Range,
) -> Result<((DopVarName, Range), (DopExpr, Range)), RangeError> {
    if header.trim().is_empty() {
        return Err(RangeError::new(
            "Empty loop header".to_string(),
            header_range,
        ));
    }

    let mut tokenizer = DopTokenizer::new(header, header_range.start)?;

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
    let array_expr_start = tokenizer.peek().1.start;
    let array_expr = parse_equality(&mut tokenizer)?;

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

pub fn parse_variable_name(
    variable: &str,
    variable_range: Range,
) -> Result<DopVarName, RangeError> {
    if variable.trim().is_empty() {
        return Err(RangeError::new(
            "Empty variable name".to_string(),
            variable_range,
        ));
    }

    let mut tokenizer = DopTokenizer::new(variable, variable_range.start)?;

    // expect Identifier
    let var_name = match tokenizer.advance()? {
        (DopToken::Identifier(name), range) => match DopVarName::new(name.clone()) {
            Some(var_name) => var_name,
            None => return Err(RangeError::invalid_variable_name(&name, range)),
        },
        (_, range) => {
            return Err(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                range,
            ));
        }
    };

    // expect Eof
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new("Unexpected token".to_string(), *range));
        }
    }

    Ok(var_name)
}

pub fn parse_variable_with_type(
    param_str: &str,
    param_range: Range,
) -> Result<(DopVarName, Option<crate::dop::DopType>), RangeError> {
    if param_str.trim().is_empty() {
        return Err(RangeError::new(
            "Empty parameter".to_string(),
            param_range,
        ));
    }

    let mut tokenizer = DopTokenizer::new(param_str, param_range.start)?;
    
    // Parse variable name
    let var_name = match tokenizer.peek() {
        (DopToken::Identifier(name), range) => {
            let var_name = match DopVarName::new(name.clone()) {
                Some(var_name) => var_name,
                None => {
                    return Err(RangeError::invalid_variable_name(name, *range));
                }
            };
            tokenizer.advance()?; // consume identifier
            var_name
        }
        (_, range) => {
            return Err(RangeError::new(
                "Expected variable name".to_string(),
                *range,
            ));
        }
    };

    // Check for optional type annotation
    let type_annotation = match tokenizer.peek() {
        (DopToken::Colon, _) => {
            tokenizer.advance()?; // consume :
            Some(parse_type(&mut tokenizer)?)
        }
        (DopToken::Eof, _) => None,
        (_, range) => {
            return Err(RangeError::new(
                "Expected ':' or end of parameter".to_string(),
                *range,
            ));
        }
    };

    // Expect end of input
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Unexpected token after parameter".to_string(),
                *range,
            ));
        }
    }

    Ok((var_name, type_annotation))
}

fn parse_type(tokenizer: &mut DopTokenizer) -> Result<crate::dop::DopType, RangeError> {
    use crate::dop::DopType;
    use std::collections::BTreeMap;
    
    match tokenizer.peek() {
        (DopToken::Identifier(name), range) => {
            let type_name = name.clone();
            let type_range = *range;
            tokenizer.advance()?;
            
            match type_name.as_str() {
                "string" => Ok(DopType::String),
                "number" => Ok(DopType::Number),
                "boolean" => Ok(DopType::Bool),
                "void" => Ok(DopType::Void),
                "array" => {
                    // Expect [inner_type]
                    match tokenizer.peek() {
                        (DopToken::LeftBracket, _) => {
                            tokenizer.advance()?; // consume [
                            let inner_type = parse_type(tokenizer)?;
                            
                            match tokenizer.peek() {
                                (DopToken::RightBracket, _) => {
                                    tokenizer.advance()?; // consume ]
                                    Ok(DopType::Array(Box::new(inner_type)))
                                }
                                (_, range) => Err(RangeError::new(
                                    "Expected ']' after array type".to_string(),
                                    *range,
                                ))
                            }
                        }
                        (_, range) => Err(RangeError::new(
                            "Expected '[' after 'array'".to_string(),
                            *range,
                        ))
                    }
                }
                "object" => {
                    // Expect [prop1: type1, prop2: type2, ...]
                    match tokenizer.peek() {
                        (DopToken::LeftBracket, _) => {
                            tokenizer.advance()?; // consume [
                            let mut properties = BTreeMap::new();
                            
                            // Handle empty object
                            if let (DopToken::RightBracket, _) = tokenizer.peek() {
                                tokenizer.advance()?; // consume ]
                                return Ok(DopType::Object(properties, 0));
                            }
                            
                            loop {
                                // Parse property name
                                let prop_name = match tokenizer.peek() {
                                    (DopToken::Identifier(name), _) => {
                                        let name = name.clone();
                                        tokenizer.advance()?;
                                        name
                                    }
                                    (_, range) => return Err(RangeError::new(
                                        "Expected property name".to_string(),
                                        *range,
                                    ))
                                };
                                
                                // Expect colon
                                match tokenizer.peek() {
                                    (DopToken::Colon, _) => {
                                        tokenizer.advance()?; // consume :
                                    }
                                    (_, range) => return Err(RangeError::new(
                                        "Expected ':' after property name".to_string(),
                                        *range,
                                    ))
                                }
                                
                                // Parse property type
                                let prop_type = parse_type(tokenizer)?;
                                properties.insert(prop_name, prop_type);
                                
                                // Check for comma or closing bracket
                                match tokenizer.peek() {
                                    (DopToken::Comma, _) => {
                                        tokenizer.advance()?; // consume ,
                                        continue;
                                    }
                                    (DopToken::RightBracket, _) => {
                                        tokenizer.advance()?; // consume ]
                                        break;
                                    }
                                    (_, range) => return Err(RangeError::new(
                                        "Expected ',' or ']' after property type".to_string(),
                                        *range,
                                    ))
                                }
                            }
                            
                            Ok(DopType::Object(properties, 0))
                        }
                        (_, range) => Err(RangeError::new(
                            "Expected '[' after 'object'".to_string(),
                            *range,
                        ))
                    }
                }
                _ => Err(RangeError::new(
                    format!("Unknown type: {}", type_name),
                    type_range,
                ))
            }
        }
        (_, range) => Err(RangeError::new(
            "Expected type name".to_string(),
            *range,
        ))
    }
}

// equality -> unary ( "==" unary )*
fn parse_equality(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    let mut expr = parse_unary(tokenizer)?;

    while let (DopToken::Equal, _) = tokenizer.peek() {
        tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
    }

    Ok(expr)
}

// unary -> ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.peek() {
        (DopToken::Not, _) => {
            tokenizer.advance()?; // consume !
            let expr = parse_unary(tokenizer)?; // Right associative for multiple !
            Ok(DopExpr::UnaryOp(UnaryOp::Not, Box::new(expr)))
        }
        _ => parse_primary(tokenizer),
    }
}

// primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
fn parse_primary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.peek() {
        (DopToken::Identifier(name), _) => {
            let mut expr = DopExpr::Variable(name.clone());
            tokenizer.advance()?; // consume identifier

            // Handle property access
            while let (DopToken::Dot, _) = tokenizer.peek() {
                tokenizer.advance()?; // consume .

                match tokenizer.peek() {
                    (DopToken::Identifier(prop), _) => {
                        expr = DopExpr::PropertyAccess(Box::new(expr), prop.clone());
                        tokenizer.advance()?;
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected identifier after '.'".to_string(),
                            *range,
                        ));
                    }
                }
            }

            Ok(expr)
        }
        (DopToken::BooleanLiteral(value), _) => {
            let expr = DopExpr::BooleanLiteral(*value);
            tokenizer.advance()?;
            Ok(expr)
        }
        (DopToken::StringLiteral(value), _) => {
            let expr = DopExpr::StringLiteral(value.clone());
            tokenizer.advance()?;
            Ok(expr)
        }
        (DopToken::NumberLiteral(value), _) => {
            let expr = DopExpr::NumberLiteral(value.clone());
            tokenizer.advance()?;
            Ok(expr)
        }
        (DopToken::LeftParen, _) => {
            tokenizer.advance()?; // consume (
            let expr = parse_equality(tokenizer)?;

            match tokenizer.peek() {
                (DopToken::RightParen, _) => {
                    tokenizer.advance()?; // consume )
                    Ok(expr)
                }
                (_, range) => Err(RangeError::new(
                    "Expected closing parenthesis".to_string(),
                    *range,
                )),
            }
        }
        (_, range) => Err(RangeError::new(
            "Expected identifier, string literal, number literal, or opening parenthesis"
                .to_string(),
            *range,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_parse_expr() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/parse_expr.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

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

            let result = parse_expr(input, Range::default()).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse expression '{}' in test case {} (line {}): {:?}",
                    input,
                    case_num + 1,
                    line_number,
                    e
                );
            });

            let actual = format!("{:?}", result);

            assert_eq!(
                actual,
                expected,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            if line == "## BEGIN" {
                assert!(
                    !in_case,
                    "Found '## BEGIN' at line {} while already inside a test case",
                    line_number
                );
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(
                    in_case,
                    "Found '## END' at line {} without matching '## BEGIN'",
                    line_number
                );
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(
            !in_case,
            "Reached end of file while inside a test case (missing '## END')"
        );

        test_cases
    }
}
