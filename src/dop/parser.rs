use std::collections::BTreeMap;
use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::dop::DopType;
use crate::dop::ast::{BinaryOp, DopExpr, UnaryOp};
use crate::dop::parse_error::ParseError;
use crate::dop::tokenizer::{DopToken, DopTokenizer};
use crate::dop::typechecker::SpannedDopType;
use crate::span::string_cursor::{StringCursor, StringSpan};

/// A DopVarName represents a validated variable name in dop.
#[derive(Debug, Clone)]
pub struct DopVarName {
    value: StringSpan,
}

impl Display for DopVarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl DopVarName {
    pub fn new(value: StringSpan) -> Result<Self, ParseError> {
        let mut chars = value.as_str().chars();
        if !chars.next().is_some_and(|c| c.is_ascii_alphabetic())
            || !chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            return Err(ParseError::InvalidVariableName {
                name: value.clone(),
            });
        }
        Ok(DopVarName { value })
    }
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
    pub fn span(&self) -> &StringSpan {
        &self.value
    }
}

/// A DopParameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct DopParameter {
    pub var_name: DopVarName,
    pub var_type: DopType,
}

impl Display for DopParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_type)
    }
}

/// A DopArgument represents a parsed argument with a name and a value.
/// E.g. <my-comp {x: [1,2], y: 2}>
///                ^^^^^^^^
#[derive(Debug, Clone)]
pub struct DopArgument {
    pub var_name: DopVarName,
    pub var_expr: DopExpr,
}

impl Display for DopArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_expr)
    }
}

pub struct DopParser {
    iter: Peekable<DopTokenizer>,
    span: StringSpan,
}

impl DopParser {}

impl From<StringSpan> for DopParser {
    fn from(span: StringSpan) -> Self {
        Self {
            iter: DopTokenizer::from(span.cursor()).peekable(),
            span: span.clone(),
        }
    }
}

impl From<&str> for DopParser {
    fn from(input: &str) -> Self {
        let cursor = StringCursor::new(input.to_string());
        let span = cursor.span();
        Self {
            iter: DopTokenizer::from(cursor).peekable(),
            span,
        }
    }
}

impl DopParser {
    fn advance_if(&mut self, token: DopToken) -> Option<StringSpan> {
        if let Some(Ok((_, span))) = self
            .iter
            .next_if(|res| res.as_ref().is_ok_and(|(t, _)| *t == token))
        {
            Some(span)
        } else {
            None
        }
    }

    fn expect_token(&mut self, expected: DopToken) -> Result<StringSpan, ParseError> {
        match self.iter.next().transpose()? {
            Some((token, span)) if token == expected => Ok(span),
            Some((actual, span)) => Err(ParseError::ExpectedTokenButGot {
                expected: expected.clone(),
                actual,
                span: span.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                span: self.span.clone(),
            }),
        }
    }

    fn expect_closing_token(
        &mut self,
        opening_token: &DopToken,
        opening_span: &StringSpan,
    ) -> Result<StringSpan, ParseError> {
        let expected = opening_token
            .matching_closing_token()
            .expect("expect_closing_token called with non-opening token");

        match self.iter.next().transpose()? {
            Some((token, span)) if token == expected => Ok(span),
            Some((actual, span)) => Err(ParseError::ExpectedTokenButGot {
                expected: expected.clone(),
                actual,
                span: span.clone(),
            }),
            None => Err(ParseError::UnmatchedToken {
                token: opening_token.clone(),
                span: opening_span.clone(),
            }),
        }
    }

    fn expect_variable_name(&mut self) -> Result<DopVarName, ParseError> {
        match self.iter.next().transpose()? {
            Some((DopToken::Identifier(name), _)) => DopVarName::new(name),
            Some((actual, span)) => Err(ParseError::ExpectedVariableNameButGot {
                actual,
                span: span.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                span: self.span.clone(),
            }),
        }
    }

    fn expect_property_name(&mut self) -> Result<StringSpan, ParseError> {
        match self.iter.next().transpose()? {
            Some((DopToken::Identifier(name), _)) => Ok(name),
            Some((token, span)) => Err(ParseError::ExpectedPropertyNameButGot {
                actual: token,
                span: span.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                span: self.span.clone(),
            }),
        }
    }

    fn expect_eof(&mut self) -> Result<(), ParseError> {
        match self.iter.next().transpose()? {
            None => Ok(()),
            Some((token, span)) => Err(ParseError::UnexpectedToken {
                token,
                span: span.clone(),
            }),
        }
    }

    fn parse_comma_separated<F>(
        &mut self,
        mut parse_and_process: F,
        end_token: Option<DopToken>, // None means EOF-terminated
    ) -> Result<(), ParseError>
    where
        F: FnMut(&mut Self) -> Result<(), ParseError>,
    {
        // Parse and process first item
        parse_and_process(self)?;

        // Parse remaining items
        while self.advance_if(DopToken::Comma).is_some() {
            // Handle trailing comma
            let at_end = match end_token.as_ref() {
                Some(token) => {
                    // Check if next token matches our end token
                    self.iter
                        .peek()
                        .and_then(|r| r.as_ref().ok())
                        .map(|(t, _)| t == token)
                        .unwrap_or(false)
                }
                None => self.iter.peek().is_none(), // EOF check
            };

            if at_end {
                break;
            }

            parse_and_process(self)?;
        }

        Ok(())
    }

    // expr = equality Eof
    pub fn parse_expr(&mut self) -> Result<DopExpr, ParseError> {
        let result = self.parse_equality()?;
        self.expect_eof()?;
        Ok(result)
    }

    // loop_header = Identifier "in" equality Eof
    pub fn parse_loop_header(&mut self) -> Result<(DopVarName, DopExpr), ParseError> {
        let var_name = self.expect_variable_name()?;
        self.expect_token(DopToken::In)?;
        let array_expr = self.parse_equality()?;
        self.expect_eof()?;
        Ok((var_name, array_expr))
    }

    // parameter_with_type = Identifier ":" type
    fn parse_parameter(&mut self) -> Result<DopParameter, ParseError> {
        let var_name = self.expect_variable_name()?;
        self.expect_token(DopToken::Colon)?;
        let typ = self.parse_type()?;
        Ok(DopParameter {
            var_name,
            var_type: typ.dop_type,
        })
    }

    // named_argument = Identifier ":" expr
    fn parse_argument(&mut self) -> Result<DopArgument, ParseError> {
        let var_name = self.expect_variable_name()?;
        self.expect_token(DopToken::Colon)?;
        let expression = self.parse_equality()?;
        Ok(DopArgument {
            var_name,
            var_expr: expression,
        })
    }

    // parameters = parameter ("," parameter)* Eof
    pub fn parse_parameters(&mut self) -> Result<BTreeMap<String, DopParameter>, ParseError> {
        let mut params = BTreeMap::new();

        self.parse_comma_separated(
            |p| {
                let param = p.parse_parameter()?;
                if params.contains_key(param.var_name.value.as_str()) {
                    return Err(ParseError::DuplicateParameter {
                        name: param.var_name.value.clone(),
                    });
                }
                params.insert(param.var_name.value.to_string(), param);
                Ok(())
            },
            None, // EOF-terminated
        )?;

        self.expect_eof()?;
        Ok(params)
    }

    // arguments = argument ("," argument)* Eof
    pub fn parse_arguments(&mut self) -> Result<BTreeMap<String, DopArgument>, ParseError> {
        let mut args = BTreeMap::new();

        self.parse_comma_separated(
            |p| {
                let arg = p.parse_argument()?;
                if args.contains_key(arg.var_name.value.as_str()) {
                    return Err(ParseError::DuplicateArgument {
                        name: arg.var_name.value.clone(),
                    });
                }
                args.insert(arg.var_name.value.to_string(), arg);
                Ok(())
            },
            None, // EOF-terminated
        )?;

        self.expect_eof()?;
        Ok(args)
    }

    // object_type = "{" (Identifier ":" type ("," Identifier ":" type)*)? "}"
    fn parse_object_type(
        &mut self,
        left_brace_span: StringSpan,
    ) -> Result<SpannedDopType, ParseError> {
        use std::collections::BTreeMap;

        // Handle empty object type
        if let Some(right_brace_span) = self.advance_if(DopToken::RightBrace) {
            return Ok(SpannedDopType {
                dop_type: DopType::Object(BTreeMap::new()),
                span: left_brace_span.to(right_brace_span),
            });
        }

        // Parse properties with duplicate checking
        let mut properties = BTreeMap::new();
        self.parse_comma_separated(
            |p| {
                let prop_name = p.expect_property_name()?;
                p.expect_token(DopToken::Colon)?;
                let typ = p.parse_type()?;

                if properties.contains_key(prop_name.as_str()) {
                    return Err(ParseError::DuplicateProperty {
                        name: prop_name.clone(),
                    });
                }
                properties.insert(prop_name.to_string(), typ.dop_type);
                Ok(())
            },
            Some(DopToken::RightBrace),
        )?;

        // Expect closing brace
        let right_brace_span = self.expect_closing_token(&DopToken::LeftBrace, &left_brace_span)?;
        Ok(SpannedDopType {
            dop_type: DopType::Object(properties),
            span: left_brace_span.to(right_brace_span),
        })
    }

    // type = TypeString
    //      | TypeNumber
    //      | TypeBoolean
    //      | TypeVoid
    //      | TypeArray "[" type "]"
    //      | "{" (Identifier ":" type ("," Identifier ":" type)*)? "}"
    fn parse_type(&mut self) -> Result<SpannedDopType, ParseError> {
        use crate::dop::DopType;

        match self.iter.next().transpose()? {
            Some((DopToken::TypeString, span)) => Ok(SpannedDopType {
                dop_type: DopType::String,
                span,
            }),
            Some((DopToken::TypeNumber, span)) => Ok(SpannedDopType {
                dop_type: DopType::Number,
                span,
            }),
            Some((DopToken::TypeBoolean, span)) => Ok(SpannedDopType {
                dop_type: DopType::Bool,
                span,
            }),
            // Array type
            Some((DopToken::TypeArray, type_array_span)) => {
                self.expect_token(DopToken::LeftBracket)?;
                let inner_type = self.parse_type()?;
                let right_bracket_span = self.expect_token(DopToken::RightBracket)?;

                Ok(SpannedDopType {
                    dop_type: DopType::Array(Some(Box::new(inner_type.dop_type))),
                    span: type_array_span.to(right_bracket_span),
                })
            }
            // Object type
            Some((DopToken::LeftBrace, left_brace_span)) => self.parse_object_type(left_brace_span),
            Some((_, span)) => Err(ParseError::ExpectedTypeName { span }),
            None => Err(ParseError::UnexpectedEof {
                span: self.span.clone(),
            }),
        }
    }

    // equality = unary ( "==" unary )*
    fn parse_equality(&mut self) -> Result<DopExpr, ParseError> {
        let mut expr = self.parse_unary()?;

        while self.advance_if(DopToken::Equal).is_some() {
            let right = self.parse_unary()?;
            expr = DopExpr::BinaryOp {
                span: expr.span().to(right.span()),
                left: Box::new(expr),
                operator: BinaryOp::Equal,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    // unary = ( "!" )* primary
    fn parse_unary(&mut self) -> Result<DopExpr, ParseError> {
        if let Some(operator_span) = self.advance_if(DopToken::Not) {
            let expr = self.parse_unary()?; // Right associative for multiple !
            Ok(DopExpr::UnaryOp {
                span: operator_span.to(expr.span()),
                operator: UnaryOp::Not,
                operand: Box::new(expr),
            })
        } else {
            self.parse_primary()
        }
    }

    // array_literal = "[" ( equality ("," equality)* )? "]"
    fn parse_array_literal(
        &mut self,
        left_bracket_span: StringSpan,
    ) -> Result<DopExpr, ParseError> {
        // Handle empty array
        if let Some(right_bracket_span) = self.advance_if(DopToken::RightBracket) {
            return Ok(DopExpr::ArrayLiteral {
                elements: Vec::new(),
                span: left_bracket_span.to(right_bracket_span),
            });
        }

        // Parse elements
        let mut elements = Vec::new();
        self.parse_comma_separated(
            |p| {
                elements.push(p.parse_equality()?);
                Ok(())
            },
            Some(DopToken::RightBracket),
        )?;

        // Expect closing bracket
        let right_bracket_span =
            self.expect_closing_token(&DopToken::LeftBracket, &left_bracket_span)?;
        Ok(DopExpr::ArrayLiteral {
            elements,
            span: left_bracket_span.to(right_bracket_span),
        })
    }

    // object_literal = "{" ( Identifier ":" equality ("," Identifier ":" equality)* )? "}"
    fn parse_object_literal(&mut self, left_brace_span: StringSpan) -> Result<DopExpr, ParseError> {
        // Handle empty object
        if let Some(right_brace_span) = self.advance_if(DopToken::RightBrace) {
            return Ok(DopExpr::ObjectLiteral {
                properties: Vec::new(),
                span: left_brace_span.to(right_brace_span),
            });
        }

        // Parse properties with duplicate checking
        let mut properties: Vec<(StringSpan, DopExpr)> = Vec::new();
        self.parse_comma_separated(
            |p| {
                let prop_name = p.expect_property_name()?;
                p.expect_token(DopToken::Colon)?;
                let value = p.parse_equality()?;

                // Check for duplicates
                if properties
                    .iter()
                    .any(|(n, _)| n.as_str() == prop_name.as_str())
                {
                    return Err(ParseError::DuplicateProperty {
                        name: prop_name.clone(),
                    });
                }
                properties.push((prop_name, value));
                Ok(())
            },
            Some(DopToken::RightBrace),
        )?;

        // Expect closing brace
        let right_brace_span = self.expect_closing_token(&DopToken::LeftBrace, &left_brace_span)?;
        Ok(DopExpr::ObjectLiteral {
            properties,
            span: left_brace_span.to(right_brace_span),
        })
    }

    // primary = Identifier ( "." Identifier )*
    //         | StringLiteral
    //         | BooleanLiteral
    //         | NumberLiteral
    //         | "[" ( equality ("," equality)* )? "]"
    //         | "{" ( Identifier ":" equality ("," Identifier ":" equality)* )? "}"
    //         | "(" equality ")"
    fn parse_primary(&mut self) -> Result<DopExpr, ParseError> {
        match self.iter.next().transpose()? {
            Some((DopToken::Identifier(name), _)) => {
                let var_name = DopVarName::new(name)?;
                let mut expr = DopExpr::Variable { value: var_name };

                // Handle property access
                while let Some(dot_span) = self.advance_if(DopToken::Dot) {
                    match self.iter.next().transpose()? {
                        Some((DopToken::Identifier(prop), prop_span)) => {
                            expr = DopExpr::PropertyAccess {
                                span: expr.span().to(prop_span.clone()),
                                object: Box::new(expr),
                                property: prop,
                            };
                        }
                        Some((_, span)) => {
                            return Err(ParseError::ExpectedIdentifierAfterDot {
                                span: span.clone(),
                            });
                        }
                        None => {
                            return Err(ParseError::UnexpectedEndOfPropertyAccess {
                                span: expr.span().to(dot_span),
                            });
                        }
                    }
                }

                Ok(expr)
            }
            Some((DopToken::StringLiteral(value), span)) => {
                Ok(DopExpr::StringLiteral { value, span })
            }
            Some((DopToken::BooleanLiteral(value), span)) => {
                Ok(DopExpr::BooleanLiteral { value, span })
            }
            Some((DopToken::NumberLiteral(value), span)) => {
                Ok(DopExpr::NumberLiteral { value, span })
            }
            Some((DopToken::LeftBracket, left_bracket)) => self.parse_array_literal(left_bracket),
            Some((DopToken::LeftBrace, left_brace_span)) => {
                self.parse_object_literal(left_brace_span)
            }
            Some((DopToken::LeftParen, left_paren_span)) => {
                let expr = self.parse_equality()?;
                self.expect_closing_token(&DopToken::LeftParen, &left_paren_span)?;
                Ok(expr)
            }
            Some((token, span)) => Err(ParseError::UnexpectedToken {
                token,
                span: span.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                span: self.span.clone(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::{SimpleAnnotation, SourceAnnotator, string_cursor::Spanned as _};
    use expect_test::{Expect, expect};

    fn annotate_error(error: ParseError) -> String {
        let annotator = SourceAnnotator::new()
            .with_label("error")
            .without_location()
            .without_line_numbers();
        annotator.annotate(
            None,
            [SimpleAnnotation {
                message: error.to_string(),
                span: error.span().clone(),
            }],
        )
    }

    fn check_parse_expr(input: &str, expected: Expect) {
        let mut parser = DopParser::from(input);
        let actual = match parser.parse_expr() {
            Ok(result) => format!("{}\n", result),
            Err(err) => annotate_error(err),
        };
        expected.assert_eq(&actual);
    }

    fn check_parse_parameters(input: &str, expected: Expect) {
        let mut parser = DopParser::from(input);

        let actual = match parser.parse_parameters() {
            Ok(result) => {
                let params: Vec<String> = result.values().map(|param| param.to_string()).collect();
                format!("[{}]\n", params.join(", "))
            }
            Err(err) => annotate_error(err),
        };

        expected.assert_eq(&actual);
    }

    fn check_parse_arguments(input: &str, expected: Expect) {
        let mut parser = DopParser::from(input);

        let actual = match parser.parse_arguments() {
            Ok(result) => {
                let args: Vec<String> = result.values().map(|arg| arg.to_string()).collect();
                format!("[{}]\n", args.join(", "))
            }
            Err(err) => annotate_error(err),
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
    fn test_parse_expr_error_invalid_token_in_array_literal() {
        check_parse_expr(
            "[foo, bar == [foo, bar]",
            expect![[r#"
                error: Unmatched '['
                [foo, bar == [foo, bar]
                ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_repeated_key_in_array_literal() {
        check_parse_expr(
            "{k: 2, k: 2}",
            expect![[r#"
                error: Duplicate property 'k'
                {k: 2, k: 2}
                       ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_invalid_token_in_array_literal_after_comma() {
        check_parse_expr(
            "[foo, bar, == [foo, bar]",
            expect![[r#"
                error: Unexpected token '=='
                [foo, bar, == [foo, bar]
                           ^^
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
                error: Expected token ']' but got 'id'
                [1,2 id
                     ^^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_error_dot_no_identifier() {
        check_parse_expr(
            "user == user.",
            expect![[r#"
                error: Unexpected end of property access
                user == user.
                        ^^^^^
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
        check_parse_expr(
            "x ==",
            expect![[r#"
            error: Unexpected end of expression
            x ==
            ^^^^
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
                [
                  [1, 2],
                  [3, 4]
                ]
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_nested_array_with_expressions() {
        check_parse_expr(
            "[[1 == [1 == 2], [] == []], [3, 4]]",
            expect![[r#"
                [
                  [(1 == [(1 == 2)]), ([] == [])],
                  [3, 4]
                ]
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
                {
                  a: "foo",
                  b: 1
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_complex_expressions() {
        check_parse_expr(
            "{user: user.name, active: !user.disabled}",
            expect![[r#"
                {
                  user: user.name,
                  active: (!user.disabled)
                }
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
    fn test_parse_expr_object_deeply_nested() {
        check_parse_expr(
            "{user: {profile: {settings: {theme: 'dark', notifications: {email: true, push: false}}, name: 'Alice'}}, status: 'active'}",
            expect![[r#"
                {
                  user: {
                    profile: {
                      settings: {
                        theme: "dark",
                        notifications: {
                          email: true,
                          push: false
                        }
                      },
                      name: "Alice"
                    }
                  },
                  status: "active"
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_mixed_arrays_and_objects() {
        check_parse_expr(
            "{users: [{name: 'Alice', tags: ['admin', 'user']}, {name: 'Bob', tags: ['user']}], config: {features: ['auth', 'api'], version: 2}}",
            expect![[r#"
                {
                  users: [
                    {
                      name: "Alice",
                      tags: ["admin", "user"]
                    },
                    {
                      name: "Bob",
                      tags: ["user"]
                    }
                  ],
                  config: {
                    features: ["auth", "api"],
                    version: 2
                  }
                }
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
                {
                  a: "foo",
                  b: 1
                }
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
                {
                  user: user.name,
                  active: (!user.disabled)
                }
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
        check_parse_arguments(
            "name:",
            expect![[r#"
            error: Unexpected end of expression
            name:
            ^^^^^
        "#]],
        );
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

    #[test]
    fn test_parse_expr_object_only_comma_error() {
        check_parse_expr(
            "{,}",
            expect![[r#"
                error: Expected property name but got ,
                {,}
                 ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_missing_value_after_colon_error() {
        check_parse_expr(
            "{k:,}",
            expect![[r#"
                error: Unexpected token ','
                {k:,}
                   ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_leading_comma_error() {
        check_parse_expr(
            "{,k:1}",
            expect![[r#"
                error: Expected property name but got ,
                {,k:1}
                 ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_double_comma_error() {
        check_parse_expr(
            "{k:1,,}",
            expect![[r#"
                error: Expected property name but got ,
                {k:1,,}
                     ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_leading_and_trailing_comma_error() {
        check_parse_expr(
            "{,k:1,}",
            expect![[r#"
                error: Expected property name but got ,
                {,k:1,}
                 ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_single_trailing_comma_allowed() {
        check_parse_expr(
            "{k:1,}",
            expect![[r#"
                {k: 1}
            "#]],
        );
    }
}
