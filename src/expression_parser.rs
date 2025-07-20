use crate::common::{BinaryOp, Expression};

#[derive(Debug, Clone, PartialEq)]
enum ExprToken {
    Identifier(String),
    StringLiteral(String),
    Equal,
    Dot,
    LeftParen,
    RightParen,
    Eof,
}

struct ExprTokenizer {
    input: Vec<char>,
    position: usize,
}

impl ExprTokenizer {
    fn new(input: &str) -> Self {
        ExprTokenizer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn peek(&self) -> char {
        if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.peek();
        if ch != '\0' {
            self.position += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_whitespace() {
            self.advance();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        // First character must be letter or underscore
        let first = self.peek();
        if first.is_ascii_alphabetic() || first == '_' {
            result.push(self.advance());
        } else {
            return result;
        }

        // Subsequent characters can be letters, digits, or underscores
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            result.push(self.advance());
        }

        result
    }

    fn read_string_literal(&mut self) -> Result<String, String> {
        let mut result = String::new();

        // Consume opening quote
        if self.peek() != '\'' {
            return Err("Expected opening single quote".to_string());
        }
        self.advance();

        while self.peek() != '\'' && self.peek() != '\0' {
            result.push(self.advance());
        }

        if self.peek() != '\'' {
            return Err("Unterminated string literal".to_string());
        }

        // Consume closing quote
        self.advance();

        Ok(result)
    }

    fn next_token(&mut self) -> Result<ExprToken, String> {
        self.skip_whitespace();

        match self.peek() {
            '\0' => Ok(ExprToken::Eof),
            '.' => {
                self.advance();
                Ok(ExprToken::Dot)
            }
            '(' => {
                self.advance();
                Ok(ExprToken::LeftParen)
            }
            ')' => {
                self.advance();
                Ok(ExprToken::RightParen)
            }
            '=' => {
                self.advance();
                if self.peek() == '=' {
                    self.advance();
                    Ok(ExprToken::Equal)
                } else {
                    Err("Expected '==' but found single '='".to_string())
                }
            }
            '\'' => {
                let string_value = self.read_string_literal()?;
                Ok(ExprToken::StringLiteral(string_value))
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let identifier = self.read_identifier();
                if identifier.is_empty() {
                    Err("Invalid identifier".to_string())
                } else {
                    Ok(ExprToken::Identifier(identifier))
                }
            }
            ch => Err(format!("Unexpected character: '{}'", ch)),
        }
    }
}

struct ExprParser {
    tokenizer: ExprTokenizer,
    current_token: ExprToken,
}

impl ExprParser {
    fn new(input: &str) -> Result<Self, String> {
        let mut tokenizer = ExprTokenizer::new(input);
        let current_token = tokenizer.next_token()?;
        Ok(ExprParser {
            tokenizer,
            current_token,
        })
    }

    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.tokenizer.next_token()?;
        Ok(())
    }

    fn parse(&mut self) -> Result<Expression, String> {
        self.parse_equality()
    }

    // equality -> primary ( "==" primary )*
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;

        while matches!(self.current_token, ExprToken::Equal) {
            self.advance()?; // consume ==
            let right = self.parse_primary()?;
            expr = Expression::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
        }

        Ok(expr)
    }

    // primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
    fn parse_primary(&mut self) -> Result<Expression, String> {
        match &self.current_token {
            ExprToken::Identifier(name) => {
                let mut expr = Expression::Variable(name.clone());
                self.advance()?;

                // Handle property access
                while matches!(self.current_token, ExprToken::Dot) {
                    self.advance()?; // consume .

                    if let ExprToken::Identifier(prop) = &self.current_token {
                        expr = Expression::PropertyAccess(Box::new(expr), prop.clone());
                        self.advance()?;
                    } else {
                        return Err("Expected identifier after '.'".to_string());
                    }
                }

                Ok(expr)
            }
            ExprToken::StringLiteral(value) => {
                let expr = Expression::StringLiteral(value.clone());
                self.advance()?;
                Ok(expr)
            }
            ExprToken::LeftParen => {
                self.advance()?; // consume (
                let expr = self.parse_equality()?;

                if matches!(self.current_token, ExprToken::RightParen) {
                    self.advance()?; // consume )
                    Ok(expr)
                } else {
                    Err("Expected closing parenthesis".to_string())
                }
            }
            _ => Err("Expected identifier, string literal, or opening parenthesis".to_string()),
        }
    }
}

pub fn parse_expression(expr: &str) -> Result<Expression, String> {
    let expr = expr.trim();
    if expr.is_empty() {
        return Err("Empty expression".to_string());
    }

    let mut parser = ExprParser::new(expr)?;
    let result = parser.parse()?;

    // Ensure we've consumed all tokens
    if !matches!(parser.current_token, ExprToken::Eof) {
        return Err("Unexpected tokens at end of expression".to_string());
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_expression_parser() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/expression_parser");
        let entries = fs::read_dir(d).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let input = archive.get("in").unwrap().content.trim();
            let expected = archive.get("out").unwrap().content.trim();

            let result = parse_expression(input).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse expression '{}' in file {}: {}",
                    input, file_name, e
                );
            });

            let actual = format!("{:?}", result);

            assert_eq!(actual, expected, "Mismatch in file: {}", file_name);
        }
    }
}
