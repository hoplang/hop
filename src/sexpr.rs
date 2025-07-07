#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    Command(String, Vec<SExpr>),
    Symbol(String),
}

impl SExpr {
    /// Parse an S-expression from a string. Panics on invalid input.
    pub fn parse(input: &str) -> SExpr {
        let mut pos = 0;
        let result = Self::parse_expr(input, &mut pos);
        Self::skip_whitespace(input, &mut pos);
        assert!(
            pos >= input.len(),
            "Unexpected characters after S-expression"
        );
        result
    }

    fn parse_expr(input: &str, pos: &mut usize) -> SExpr {
        Self::skip_whitespace(input, pos);
        assert!(*pos < input.len(), "Unexpected end of input");
        let chars: Vec<char> = input.chars().collect();
        match chars[*pos] {
            '(' => Self::parse_command(input, pos),
            ')' => panic!("Unexpected closing parenthesis"),
            _ => Self::parse_symbol(input, pos),
        }
    }

    fn parse_command(input: &str, pos: &mut usize) -> SExpr {
        let chars: Vec<char> = input.chars().collect();
        assert!(
            *pos < chars.len() && chars[*pos] == '(',
            "Expected opening parenthesis"
        );
        *pos += 1;
        let mut elements = Vec::new();
        loop {
            Self::skip_whitespace(input, pos);
            assert!(*pos < chars.len(), "Unclosed command");
            if chars[*pos] == ')' {
                *pos += 1; // Consume closing parenthesis
                break;
            }
            elements.push(Self::parse_expr(input, pos));
        }
        assert!(
            !elements.is_empty(),
            "Command must have at least one element (name)"
        );
        let name = match &elements[0] {
            SExpr::Symbol(s) => s.clone(),
            SExpr::Command(_, _) => panic!("Command name must be a symbol, not a command"),
        };
        let args = elements.into_iter().skip(1).collect();
        SExpr::Command(name, args)
    }

    fn parse_symbol(input: &str, pos: &mut usize) -> SExpr {
        let chars: Vec<char> = input.chars().collect();
        let mut symbol = String::new();
        while *pos < chars.len() {
            let ch = chars[*pos];
            if ch.is_whitespace() || ch == '(' || ch == ')' {
                break;
            }
            symbol.push(ch);
            *pos += 1;
        }
        assert!(!symbol.is_empty(), "Empty symbol");
        SExpr::Symbol(symbol)
    }

    fn skip_whitespace(input: &str, pos: &mut usize) {
        let chars: Vec<char> = input.chars().collect();
        while *pos < chars.len() && chars[*pos].is_whitespace() {
            *pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol() {
        assert_eq!(SExpr::parse("hello"), SExpr::Symbol("hello".to_string()));
    }

    #[test]
    fn test_simple_command() {
        assert_eq!(
            SExpr::parse("(print)"),
            SExpr::Command("print".to_string(), vec![])
        );
    }

    #[test]
    fn test_command_with_args() {
        assert_eq!(
            SExpr::parse("(add a b)"),
            SExpr::Command(
                "add".to_string(),
                vec![
                    SExpr::Symbol("a".to_string()),
                    SExpr::Symbol("b".to_string())
                ]
            )
        );
    }

    #[test]
    fn test_nested_command() {
        assert_eq!(
            SExpr::parse("(if (> x 0) (print positive))"),
            SExpr::Command(
                "if".to_string(),
                vec![
                    SExpr::Command(
                        ">".to_string(),
                        vec![
                            SExpr::Symbol("x".to_string()),
                            SExpr::Symbol("0".to_string())
                        ]
                    ),
                    SExpr::Command(
                        "print".to_string(),
                        vec![SExpr::Symbol("positive".to_string())]
                    )
                ]
            )
        );
    }

    #[test]
    #[should_panic(expected = "Command must have at least one element")]
    fn test_empty_command() {
        SExpr::parse("()");
    }

    #[test]
    #[should_panic(expected = "Command name must be a symbol")]
    fn test_command_starting_with_command() {
        SExpr::parse("((nested) arg)");
    }

    #[test]
    #[should_panic]
    fn test_unclosed_command() {
        SExpr::parse("(add a b");
    }

    #[test]
    #[should_panic]
    fn test_unexpected_closing() {
        SExpr::parse("a)");
    }
}
