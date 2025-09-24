use std::collections::{BTreeMap, HashSet};
use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::document::document_cursor::{DocumentCursor, DocumentRange, Ranged as _};
use crate::dop::Type;
use crate::dop::expr::{AnnotatedExpr, BinaryOp};
use crate::dop::parse_error::ParseError;
use crate::dop::token::Token;
use crate::dop::tokenizer::Tokenizer;
use crate::dop::var_name::VarName;

use super::Expr;
use super::typed_expr::SimpleTypedExpr;

/// A Parameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct Parameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: Type,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_type)
    }
}

pub type TypedArgument = Argument<SimpleTypedExpr>;

/// An Argument represents a parsed argument with a name and a value.
/// E.g. <my-comp {x: [1,2], y: 2}>
///                ^^^^^^^^
#[derive(Debug, Clone)]
pub struct Argument<T = Expr> {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_expr: T,
}

impl<T: Display> Display for Argument<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_expr)
    }
}

pub struct Parser {
    iter: Peekable<Tokenizer>,
    range: DocumentRange,
}

impl From<DocumentRange> for Parser {
    fn from(range: DocumentRange) -> Self {
        Self {
            iter: Tokenizer::from(range.cursor()).peekable(),
            range: range.clone(),
        }
    }
}

impl From<&str> for Parser {
    fn from(input: &str) -> Self {
        let cursor = DocumentCursor::new(input.to_string());
        let range = cursor.range();
        Self {
            iter: Tokenizer::from(cursor).peekable(),
            range,
        }
    }
}

impl Parser {
    fn advance_if(&mut self, token: Token) -> Option<DocumentRange> {
        if let Some(Ok((_, range))) = self
            .iter
            .next_if(|res| res.as_ref().is_ok_and(|(t, _)| *t == token))
        {
            Some(range)
        } else {
            None
        }
    }

    fn expect_token(&mut self, expected: &Token) -> Result<DocumentRange, ParseError> {
        match self.iter.next().transpose()? {
            Some((token, range)) if token == *expected => Ok(range),
            Some((actual, range)) => Err(ParseError::ExpectedTokenButGot {
                expected: expected.clone(),
                actual,
                range: range.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                range: self.range.clone(),
            }),
        }
    }

    fn expect_opposite(
        &mut self,
        token: &Token,
        range: &DocumentRange,
    ) -> Result<DocumentRange, ParseError> {
        let expected = token.opposite_token();

        match self.iter.next().transpose()? {
            Some((actual, range)) if actual == expected => Ok(range),
            Some((actual, range)) => Err(ParseError::ExpectedTokenButGot {
                expected,
                actual,
                range,
            }),
            None => Err(ParseError::UnmatchedToken {
                token: token.clone(),
                range: range.clone(),
            }),
        }
    }

    fn expect_variable_name(&mut self) -> Result<(VarName, DocumentRange), ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::Identifier(name), range)) => {
                let var_name = VarName::new(name.as_str()).map_err(|error| {
                    ParseError::InvalidVariableName {
                        name: name.to_string_span(),
                        error,
                        range: name.clone(),
                    }
                })?;
                Ok((var_name, range))
            }
            Some((actual, range)) => Err(ParseError::ExpectedVariableNameButGot { actual, range }),
            None => Err(ParseError::UnexpectedEof {
                range: self.range.clone(),
            }),
        }
    }

    fn expect_property_name(&mut self) -> Result<DocumentRange, ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::Identifier(name), _)) => Ok(name),
            Some((token, range)) => Err(ParseError::ExpectedPropertyNameButGot {
                actual: token,
                range: range.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                range: self.range.clone(),
            }),
        }
    }

    fn expect_eof(&mut self) -> Result<(), ParseError> {
        match self.iter.next().transpose()? {
            None => Ok(()),
            Some((token, range)) => Err(ParseError::UnexpectedToken {
                token,
                range: range.clone(),
            }),
        }
    }

    fn parse_comma_separated<F>(
        &mut self,
        mut parse: F,
        end_token: Option<&Token>,
    ) -> Result<(), ParseError>
    where
        F: FnMut(&mut Self) -> Result<(), ParseError>,
    {
        parse(self)?;
        while self.advance_if(Token::Comma).is_some() {
            let at_end = self
                .iter
                .peek()
                .and_then(|r| r.as_ref().ok())
                .map(|(t, _)| t)
                == end_token;
            if at_end {
                break;
            }
            parse(self)?;
        }

        Ok(())
    }

    fn parse_delimited_list<F>(
        &mut self,
        opening_token: &Token,
        opening_range: &DocumentRange,
        parse: F,
    ) -> Result<DocumentRange, ParseError>
    where
        F: FnMut(&mut Self) -> Result<(), ParseError>,
    {
        let closing_token = opening_token.opposite_token();
        if let Some(closing_range) = self.advance_if(closing_token.clone()) {
            return Ok(closing_range);
        }
        self.parse_comma_separated(parse, Some(&closing_token))?;
        self.expect_opposite(opening_token, opening_range)
    }

    // expr = equality Eof
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let result = self.parse_equality()?;
        self.expect_eof()?;
        Ok(result)
    }

    // loop_header = Identifier "in" equality Eof
    pub fn parse_loop_header(&mut self) -> Result<(VarName, DocumentRange, Expr), ParseError> {
        let (var_name, var_name_range) = self.expect_variable_name()?;
        self.expect_token(&Token::In)?;
        let array_expr = self.parse_equality()?;
        self.expect_eof()?;
        Ok((var_name, var_name_range, array_expr))
    }

    // parameter_with_type = Identifier ":" type
    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let (var_name, var_name_range) = self.expect_variable_name()?;
        self.expect_token(&Token::Colon)?;
        let (var_type, _) = self.parse_type()?;
        Ok(Parameter {
            var_name,
            var_name_range,
            var_type,
        })
    }

    // named_argument = Identifier ":" expr
    fn parse_argument(&mut self) -> Result<Argument, ParseError> {
        let (var_name, var_name_range) = self.expect_variable_name()?;
        self.expect_token(&Token::Colon)?;
        let expression = self.parse_equality()?;
        Ok(Argument {
            var_name,
            var_name_range,
            var_expr: expression,
        })
    }

    // parameters = parameter ("," parameter)* Eof
    pub fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = Vec::new();
        let mut seen_names = HashSet::new();
        self.parse_comma_separated(
            |this| {
                let param = this.parse_parameter()?;
                if !seen_names.insert(param.var_name.as_str().to_string()) {
                    return Err(ParseError::DuplicateParameter {
                        name: param.var_name_range.to_string_span(),
                        range: param.var_name_range.clone(),
                    });
                }
                params.push(param);
                Ok(())
            },
            None,
        )?;
        self.expect_eof()?;
        Ok(params)
    }

    // arguments = argument ("," argument)* Eof
    pub fn parse_arguments(&mut self) -> Result<Vec<Argument>, ParseError> {
        let mut args = Vec::new();
        let mut seen_names = HashSet::new();
        self.parse_comma_separated(
            |this| {
                let arg = this.parse_argument()?;
                if !seen_names.insert(arg.var_name.as_str().to_string()) {
                    return Err(ParseError::DuplicateArgument {
                        name: arg.var_name_range.to_string_span(),
                        range: arg.var_name_range.clone(),
                    });
                }
                args.push(arg);
                Ok(())
            },
            None,
        )?;
        self.expect_eof()?;
        Ok(args)
    }

    // object_type = "{" (Identifier ":" type ("," Identifier ":" type)*)? "}"
    fn parse_object_type(
        &mut self,
        left_brace: DocumentRange,
    ) -> Result<(Type, DocumentRange), ParseError> {
        let mut properties = BTreeMap::new();
        let right_brace = self.parse_delimited_list(&Token::LeftBrace, &left_brace, |this| {
            let prop_name = this.expect_property_name()?;
            this.expect_token(&Token::Colon)?;
            let (typ, _range) = this.parse_type()?;
            if properties.contains_key(prop_name.as_str()) {
                return Err(ParseError::DuplicateProperty {
                    name: prop_name.to_string_span(),
                    range: prop_name.clone(),
                });
            }
            properties.insert(prop_name.to_string(), typ);
            Ok(())
        })?;
        Ok((Type::Object(properties), left_brace.to(right_brace)))
    }

    fn parse_type(&mut self) -> Result<(Type, DocumentRange), ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::TypeString, range)) => Ok((Type::String, range)),
            Some((Token::TypeInt, range)) => Ok((Type::Int, range)),
            Some((Token::TypeFloat, range)) => Ok((Type::Float, range)),
            Some((Token::TypeBoolean, range)) => Ok((Type::Bool, range)),
            Some((Token::TypeArray, type_array)) => {
                let left_bracket = self.expect_token(&Token::LeftBracket)?;
                let inner_type = self.parse_type()?;
                let right_bracket = self.expect_opposite(&Token::LeftBracket, &left_bracket)?;
                Ok((
                    Type::Array(Some(Box::new(inner_type.0))),
                    type_array.to(right_bracket),
                ))
            }
            Some((Token::LeftBrace, left_brace_range)) => self.parse_object_type(left_brace_range),
            Some((_, range)) => Err(ParseError::ExpectedTypeName { range }),
            None => Err(ParseError::UnexpectedEof {
                range: self.range.clone(),
            }),
        }
    }

    // equality = relational ( ("==" | "!=") relational )*
    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_relational()?;
        loop {
            if self.advance_if(Token::Eq).is_some() {
                let right = self.parse_relational()?;
                expr = AnnotatedExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::Eq,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::NotEq).is_some() {
                let right = self.parse_relational()?;
                expr = AnnotatedExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::NotEq,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // relational = additive ( ("<" | ">" | "<=") additive )*
    fn parse_relational(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_additive()?;
        loop {
            if self.advance_if(Token::LessThan).is_some() {
                let right = self.parse_additive()?;
                expr = AnnotatedExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::LessThan,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::GreaterThan).is_some() {
                let right = self.parse_additive()?;
                expr = AnnotatedExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::GreaterThan,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::LessThanOrEqual).is_some() {
                let right = self.parse_additive()?;
                expr = AnnotatedExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::LessThanOrEqual,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // additive = unary ( "+" unary )*
    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        while self.advance_if(Token::Plus).is_some() {
            let right = self.parse_unary()?;
            expr = AnnotatedExpr::BinaryOp {
                annotation: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: BinaryOp::Plus,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    // unary = ( "!" )* primary
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(operator_range) = self.advance_if(Token::Not) {
            let expr = self.parse_unary()?; // Right associative for multiple !
            Ok(AnnotatedExpr::Negation {
                annotation: operator_range.to(expr.range().clone()),
                operand: Box::new(expr),
            })
        } else {
            self.parse_primary()
        }
    }

    // array_literal = "[" ( equality ("," equality)* )? "]"
    fn parse_array_literal(&mut self, left_bracket: DocumentRange) -> Result<Expr, ParseError> {
        let mut elements = Vec::new();
        let right_bracket =
            self.parse_delimited_list(&Token::LeftBracket, &left_bracket, |this| {
                elements.push(this.parse_equality()?);
                Ok(())
            })?;
        Ok(AnnotatedExpr::ArrayLiteral {
            elements,
            annotation: left_bracket.to(right_bracket),
        })
    }

    fn parse_object_literal(&mut self, left_brace: DocumentRange) -> Result<Expr, ParseError> {
        let mut properties = Vec::new();
        let mut seen_names = HashSet::new();
        let right_brace = self.parse_delimited_list(&Token::LeftBrace, &left_brace, |this| {
            let prop_name = this.expect_property_name()?;
            if !seen_names.insert(prop_name.to_string_span()) {
                return Err(ParseError::DuplicateProperty {
                    name: prop_name.to_string_span(),
                    range: prop_name.clone(),
                });
            }
            this.expect_token(&Token::Colon)?;
            properties.push((prop_name.to_string(), this.parse_equality()?));
            Ok(())
        })?;
        Ok(AnnotatedExpr::ObjectLiteral {
            properties,
            annotation: left_brace.to(right_brace),
        })
    }

    fn parse_property_access(&mut self, identifier: DocumentRange) -> Result<Expr, ParseError> {
        let var_name =
            VarName::new(identifier.as_str()).map_err(|error| ParseError::InvalidVariableName {
                name: identifier.to_string_span(),
                error,
                range: identifier.clone(),
            })?;
        let mut expr = AnnotatedExpr::Var {
            annotation: identifier.clone(),
            value: var_name,
        };

        while let Some(dot) = self.advance_if(Token::Dot) {
            match self.iter.next().transpose()? {
                Some((Token::Identifier(prop), _)) => {
                    let range = expr.range().clone().to(prop.clone());
                    expr = AnnotatedExpr::PropertyAccess {
                        object: Box::new(expr),
                        property: prop.to_string(),
                        annotation: range,
                    };
                }
                Some((_, range)) => {
                    return Err(ParseError::ExpectedIdentifierAfterDot { range });
                }
                None => {
                    return Err(ParseError::UnexpectedEndOfPropertyAccess {
                        range: expr.range().clone().to(dot.clone()),
                    });
                }
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::Identifier(name), _)) => self.parse_property_access(name),
            Some((Token::StringLiteral(value), range)) => Ok(AnnotatedExpr::StringLiteral {
                value,
                annotation: range,
            }),
            Some((Token::BooleanLiteral(value), range)) => Ok(AnnotatedExpr::BooleanLiteral {
                value,
                annotation: range,
            }),
            Some((Token::IntLiteral(value), range)) => Ok(AnnotatedExpr::IntLiteral {
                value,
                annotation: range,
            }),
            Some((Token::FloatLiteral(value), range)) => Ok(AnnotatedExpr::FloatLiteral {
                value,
                annotation: range,
            }),
            Some((Token::LeftBracket, left_bracket)) => self.parse_array_literal(left_bracket),
            Some((Token::LeftBrace, left_brace)) => self.parse_object_literal(left_brace),
            Some((Token::LeftParen, left_paren)) => {
                let expr = self.parse_equality()?;
                self.expect_opposite(&Token::LeftParen, &left_paren)?;
                Ok(expr)
            }
            Some((token, range)) => Err(ParseError::UnexpectedToken {
                token,
                range: range.clone(),
            }),
            None => Err(ParseError::UnexpectedEof {
                range: self.range.clone(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::{DocumentAnnotator, SimpleAnnotation};
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn annotate_error(error: ParseError) -> String {
        let annotator = DocumentAnnotator::new()
            .with_label("error")
            .without_location()
            .without_line_numbers();
        annotator.annotate(
            None,
            [SimpleAnnotation {
                message: error.to_string(),
                range: error.range().clone(),
            }],
        )
    }

    fn check_parse_expr(input: &str, expected: Expect) {
        let mut parser = Parser::from(input);
        let actual = match parser.parse_expr() {
            Ok(result) => format!("{}\n", result),
            Err(err) => annotate_error(err),
        };
        expected.assert_eq(&actual);
    }

    fn check_parse_parameters(input: &str, expected: Expect) {
        let mut parser = Parser::from(input);

        let actual = match parser.parse_parameters() {
            Ok(result) => {
                let params: Vec<String> = result.iter().map(|param| param.to_string()).collect();
                format!("[{}]\n", params.join(", "))
            }
            Err(err) => annotate_error(err),
        };

        expected.assert_eq(&actual);
    }

    fn check_parse_arguments(input: &str, expected: Expect) {
        let mut parser = Parser::from(input);

        let actual = match parser.parse_arguments() {
            Ok(result) => {
                let args: Vec<String> = result.iter().map(|arg| arg.to_string()).collect();
                format!("[{}]\n", args.join(", "))
            }
            Err(err) => annotate_error(err),
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_parse_parameters_type_keywords() {
        check_parse_parameters(
            "name: string, age: int, score: float, active: boolean, items: array[string]",
            expect![[r#"
                [name: string, age: int, score: float, active: boolean, items: array[string]]
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_duplicate_parameters_with_different_type_error() {
        check_parse_parameters(
            "foo: string, foo: float",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: string, foo: float
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
            "user: {name: string, name: float}",
            expect![[r#"
                error: Duplicate property 'name'
                user: {name: string, name: float}
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
    fn test_parse_expr_less_than_or_equal() {
        check_parse_expr(
            "x <= y",
            expect![[r#"
                (x <= y)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_less_than_or_equal_chained() {
        check_parse_expr(
            "a <= b <= c",
            expect![[r#"
                ((a <= b) <= c)
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
            r#""""#,
            expect![[r#"
                ""
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_int_literal() {
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
    fn test_parse_expr_mixed_int_and_float() {
        check_parse_expr(
            "42 + 3.14",
            expect![[r#"
                (42 + 3.14)
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
            r#""guest" == user.role"#,
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
            r#""hello""#,
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
            r#""apple" == "orange""#,
            expect![[r#"
                ("apple" == "orange")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_property_string_comparison() {
        check_parse_expr(
            r#"user.name == "admin""#,
            expect![[r#"
                (user.name == "admin")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_with_space() {
        check_parse_expr(
            r#""hello world""#,
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
    fn test_parse_expr_array_integers() {
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
            r#"[1, "hello", true]"#,
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
    fn test_parse_expr_nested_array_with_expressions() {
        check_parse_expr(
            "[[1 == [1 == 2], [] == []], [3, 4]]",
            expect![[r#"
                [[(1 == [(1 == 2)]), ([] == [])], [3, 4]]
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
            r#"{name: "John"}"#,
            expect![[r#"
                {name: "John"}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_multiple_properties() {
        check_parse_expr(
            r#"{a: "foo", b: 1}"#,
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
                {user: user.name, active: (!user.disabled)}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_nested() {
        check_parse_expr(
            r#"{nested: {inner: "value"}}"#,
            expect![[r#"
                {nested: {inner: "value"}}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_deeply_nested() {
        check_parse_expr(
            r#"{user: {profile: {settings: {theme: "dark", notifications: {email: true, push: false}}, name: "Alice"}}, status: "active"}"#,
            expect![[r#"
                {
                  user: {
                    profile: {
                      settings: {
                        theme: "dark",
                        notifications: {email: true, push: false},
                      },
                      name: "Alice",
                    },
                  },
                  status: "active",
                }
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_mixed_arrays_and_objects() {
        check_parse_expr(
            r#"{users: [{name: "Alice", tags: ["admin", "user"]}, {name: "Bob", tags: ["user"]}], config: {features: ["auth", "api"], version: 2}}"#,
            expect![[r#"
                {
                  users: [
                    {name: "Alice", tags: ["admin", "user"]},
                    {name: "Bob", tags: ["user"]},
                  ],
                  config: {features: ["auth", "api"], version: 2},
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
            indoc! {r#"
                {
                	a: "foo",
                	b: 1,
                }
            "#},
            expect![[r#"
                {a: "foo", b: 1}
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_object_trailing_comma_single() {
        check_parse_expr(
            indoc! {r#"
                {
                	name: "John",
                }
            "#},
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
                {user: user.name, active: (!user.disabled)}
            "#]],
        );
    }

    #[test]
    fn test_parse_arguments_single() {
        check_parse_arguments(
            r#"name: "John""#,
            expect![[r#"
                [name: "John"]
            "#]],
        );
    }

    #[test]
    fn test_parse_arguments_multiple() {
        check_parse_arguments(
            r#"name: "John", age: 25, active: true"#,
            expect![[r#"
                [name: "John", age: 25, active: true]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_complex_expressions() {
        check_parse_arguments(
            "user: user.name, enabled: !user.disabled",
            expect![[r#"
                [user: user.name, enabled: (!user.disabled)]
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_trailing_comma() {
        check_parse_arguments(
            r#"name: "John", age: 25,"#,
            expect![[r#"
                [name: "John", age: 25]
            "#]],
        );
    }

    #[test]
    fn test_parse_arguments_duplicate_argument_error() {
        check_parse_arguments(
            r#"name: "John", name: "Jane""#,
            expect![[r#"
                error: Duplicate argument 'name'
                name: "John", name: "Jane"
                              ^^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_missing_colon_error() {
        check_parse_arguments(
            r#"name "John""#,
            expect![[r#"
                error: Expected token ':' but got '"John"'
                name "John"
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
            r#"123: "value""#,
            expect![[r#"
                error: Expected variable name but got 123
                123: "value"
                ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_named_arguments_unexpected_token_error() {
        check_parse_arguments(
            r#"name: "John" age: 25"#,
            expect![[r#"
                error: Unexpected token 'age'
                name: "John" age: 25
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

    #[test]
    fn test_parse_expr_string_concatenation() {
        check_parse_expr(
            r#""hello" + "world""#,
            expect![[r#"
                ("hello" + "world")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_concatenation_multiple() {
        check_parse_expr(
            r#""hello" + " " + "world""#,
            expect![[r#"
                (("hello" + " ") + "world")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_concatenation_with_variables() {
        check_parse_expr(
            r#"greeting + " " + name"#,
            expect![[r#"
                ((greeting + " ") + name)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_concatenation_precedence() {
        check_parse_expr(
            r#""a" + "b" == "ab""#,
            expect![[r#"
                (("a" + "b") == "ab")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_string_concatenation_with_property_access() {
        check_parse_expr(
            r#"user.firstName + " " + user.lastName"#,
            expect![[r#"
                ((user.firstName + " ") + user.lastName)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_not_equals_simple() {
        check_parse_expr(
            "x != y",
            expect![[r#"
                (x != y)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_not_equals_string() {
        check_parse_expr(
            r#""hello" != "world""#,
            expect![[r#"
                ("hello" != "world")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_not_equals_chained() {
        check_parse_expr(
            "a != b != c",
            expect![[r#"
                ((a != b) != c)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_mixed_equals_not_equals() {
        check_parse_expr(
            "a == b != c",
            expect![[r#"
                ((a == b) != c)
            "#]],
        );
    }
}
