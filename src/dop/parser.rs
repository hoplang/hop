use std::collections::HashSet;
use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::document::document_cursor::{DocumentCursor, DocumentRange, Ranged as _};
use crate::dop::expr::{BinaryOp, SyntacticExpr};
use crate::dop::field_name::FieldName;
use crate::dop::parse_error::ParseError;
use crate::dop::syntax_type::SyntaxType;
use crate::dop::token::Token;
use crate::dop::tokenizer::Tokenizer;
use crate::dop::var_name::VarName;

use super::typed_expr::SimpleTypedExpr;

/// A Parameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct Parameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: SyntaxType,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_type)
    }
}

/// A RecordField represents a field in a record declaration.
/// E.g. record Foo {bar: String, baz: Int}
///                  ^^^^^^^^^^^
#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: FieldName,
    pub name_range: DocumentRange,
    pub field_type: SyntaxType,
}

impl Display for RecordField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.field_type)
    }
}

/// A RecordDeclaration represents a full record type declaration.
/// E.g. record User {name: String, age: Int}
#[derive(Debug, Clone)]
pub struct RecordDeclaration {
    pub name: DocumentRange,
    pub fields: Vec<RecordField>,
}

impl Display for RecordDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "record {} {{", self.name)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, "}}")
    }
}

pub type TypedArgument = Argument<SimpleTypedExpr>;

/// An Argument represents a parsed argument with a name and a value.
/// E.g. <my-comp {x: [1,2], y: 2}>
///                ^^^^^^^^
#[derive(Debug, Clone)]
pub struct Argument<T = SyntacticExpr> {
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

    fn expect_field_name(&mut self) -> Result<(FieldName, DocumentRange), ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::Identifier(name), range)) => {
                let prop_name = FieldName::new(name.as_str()).map_err(|error| {
                    ParseError::InvalidFieldName {
                        name: name.to_string_span(),
                        error,
                        range: name.clone(),
                    }
                })?;
                Ok((prop_name, range))
            }
            Some((token, range)) => Err(ParseError::ExpectedFieldNameButGot {
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

    // expr = logical Eof
    pub fn parse_expr(&mut self) -> Result<SyntacticExpr, ParseError> {
        let result = self.parse_logical()?;
        self.expect_eof()?;
        Ok(result)
    }

    // exprs = logical ("," logical)* ","? Eof
    pub fn parse_exprs(&mut self) -> Result<Vec<SyntacticExpr>, ParseError> {
        let mut exprs = Vec::new();
        self.parse_comma_separated(
            |this| {
                exprs.push(this.parse_logical()?);
                Ok(())
            },
            None,
        )?;
        self.expect_eof()?;
        Ok(exprs)
    }

    // loop_header = Identifier "in" logical Eof
    pub fn parse_loop_header(
        &mut self,
    ) -> Result<(VarName, DocumentRange, SyntacticExpr), ParseError> {
        let (var_name, var_name_range) = self.expect_variable_name()?;
        self.expect_token(&Token::In)?;
        let array_expr = self.parse_logical()?;
        self.expect_eof()?;
        Ok((var_name, var_name_range, array_expr))
    }

    // parameter_with_type = Identifier ":" type
    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let (var_name, var_name_range) = self.expect_variable_name()?;
        self.expect_token(&Token::Colon)?;
        let var_type = self.parse_type()?;
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
        let expression = self.parse_logical()?;
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

    // record_field = Identifier ":" type
    fn parse_record_field(&mut self) -> Result<RecordField, ParseError> {
        let (name, name_range) = self.expect_field_name()?;
        self.expect_token(&Token::Colon)?;
        let field_type = self.parse_type()?;
        Ok(RecordField {
            name,
            name_range,
            field_type,
        })
    }

    // record = "record" Identifier "{" (record_field ("," record_field)* ","?)? "}" Eof
    pub fn parse_record(&mut self) -> Result<RecordDeclaration, ParseError> {
        // Expect "record" keyword
        self.expect_token(&Token::Record)?;

        // Expect record name (a type name)
        let name = match self.iter.next().transpose()? {
            Some((Token::TypeName(name), _)) => name,
            Some((actual, range)) => {
                return Err(ParseError::ExpectedTypeNameButGot { actual, range });
            }
            None => {
                return Err(ParseError::UnexpectedEof {
                    range: self.range.clone(),
                });
            }
        };

        // Expect opening brace
        let left_brace = self.expect_token(&Token::LeftBrace)?;

        // Parse fields
        let mut fields = Vec::new();
        let mut seen_names = HashSet::new();
        self.parse_delimited_list(&Token::LeftBrace, &left_brace, |this| {
            let field = this.parse_record_field()?;
            if !seen_names.insert(field.name.as_str().to_string()) {
                return Err(ParseError::DuplicateField {
                    name: field.name_range.to_string_span(),
                    range: field.name_range.clone(),
                });
            }
            fields.push(field);
            Ok(())
        })?;

        self.expect_eof()?;

        Ok(RecordDeclaration { name, fields })
    }

    fn parse_type(&mut self) -> Result<SyntaxType, ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::TypeString, range)) => Ok(SyntaxType::String { range }),
            Some((Token::TypeInt, range)) => Ok(SyntaxType::Int { range }),
            Some((Token::TypeFloat, range)) => Ok(SyntaxType::Float { range }),
            Some((Token::TypeBoolean, range)) => Ok(SyntaxType::Bool { range }),
            Some((Token::TypeTrustedHTML, range)) => Ok(SyntaxType::TrustedHTML { range }),
            Some((Token::TypeArray, type_array)) => {
                let left_bracket = self.expect_token(&Token::LeftBracket)?;
                let element = self.parse_type()?;
                let right_bracket = self.expect_opposite(&Token::LeftBracket, &left_bracket)?;
                Ok(SyntaxType::Array {
                    element: Some(Box::new(element)),
                    range: type_array.to(right_bracket),
                })
            }
            Some((Token::TypeName(name), range)) => Ok(SyntaxType::Named {
                name: name.as_str().to_string(),
                range,
            }),
            Some((actual, range)) => Err(ParseError::ExpectedTypeNameButGot { actual, range }),
            None => Err(ParseError::UnexpectedEof {
                range: self.range.clone(),
            }),
        }
    }

    fn parse_logical(&mut self) -> Result<SyntacticExpr, ParseError> {
        let mut expr = self.parse_logical_and()?;
        while self.advance_if(Token::LogicalOr).is_some() {
            let right = self.parse_logical_and()?;
            expr = SyntacticExpr::BinaryOp {
                annotation: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: BinaryOp::LogicalOr,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<SyntacticExpr, ParseError> {
        let mut expr = self.parse_equality()?;
        while self.advance_if(Token::LogicalAnd).is_some() {
            let right = self.parse_equality()?;
            expr = SyntacticExpr::BinaryOp {
                annotation: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: BinaryOp::LogicalAnd,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    // equality = relational ( ("==" | "!=") relational )*
    fn parse_equality(&mut self) -> Result<SyntacticExpr, ParseError> {
        let mut expr = self.parse_relational()?;
        loop {
            if self.advance_if(Token::Eq).is_some() {
                let right = self.parse_relational()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::Eq,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::NotEq).is_some() {
                let right = self.parse_relational()?;
                expr = SyntacticExpr::BinaryOp {
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

    // relational = additive ( ("<" | ">" | "<=" | ">=") additive )*
    fn parse_relational(&mut self) -> Result<SyntacticExpr, ParseError> {
        let mut expr = self.parse_additive()?;
        loop {
            if self.advance_if(Token::LessThan).is_some() {
                let right = self.parse_additive()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::LessThan,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::GreaterThan).is_some() {
                let right = self.parse_additive()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::GreaterThan,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::LessThanOrEqual).is_some() {
                let right = self.parse_additive()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::LessThanOrEqual,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::GreaterThanOrEqual).is_some() {
                let right = self.parse_additive()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::GreaterThanOrEqual,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // additive = multiplicative ( ("+" | "-") multiplicative )*
    fn parse_additive(&mut self) -> Result<SyntacticExpr, ParseError> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            if self.advance_if(Token::Plus).is_some() {
                let right = self.parse_multiplicative()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::Plus,
                    right: Box::new(right),
                };
            } else if self.advance_if(Token::Minus).is_some() {
                let right = self.parse_multiplicative()?;
                expr = SyntacticExpr::BinaryOp {
                    annotation: expr.range().clone().to(right.range().clone()),
                    left: Box::new(expr),
                    operator: BinaryOp::Minus,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // multiplicative = unary ( "*" unary )*
    fn parse_multiplicative(&mut self) -> Result<SyntacticExpr, ParseError> {
        let mut expr = self.parse_unary()?;
        while self.advance_if(Token::Asterisk).is_some() {
            let right = self.parse_unary()?;
            expr = SyntacticExpr::BinaryOp {
                annotation: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: BinaryOp::Multiply,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    // unary = ( "!" )* primary
    fn parse_unary(&mut self) -> Result<SyntacticExpr, ParseError> {
        if let Some(operator_range) = self.advance_if(Token::Not) {
            let expr = self.parse_unary()?; // Right associative for multiple !
            Ok(SyntacticExpr::Negation {
                annotation: operator_range.to(expr.range().clone()),
                operand: Box::new(expr),
            })
        } else {
            self.parse_primary()
        }
    }

    // array_literal = "[" ( logical ("," logical)* )? "]"
    fn parse_array_literal(
        &mut self,
        left_bracket: DocumentRange,
    ) -> Result<SyntacticExpr, ParseError> {
        let mut elements = Vec::new();
        let right_bracket =
            self.parse_delimited_list(&Token::LeftBracket, &left_bracket, |this| {
                elements.push(this.parse_logical()?);
                Ok(())
            })?;
        Ok(SyntacticExpr::ArrayLiteral {
            elements,
            annotation: left_bracket.to(right_bracket),
        })
    }

    fn parse_field_access(
        &mut self,
        identifier: DocumentRange,
    ) -> Result<SyntacticExpr, ParseError> {
        let var_name =
            VarName::new(identifier.as_str()).map_err(|error| ParseError::InvalidVariableName {
                name: identifier.to_string_span(),
                error,
                range: identifier.clone(),
            })?;
        let mut expr = SyntacticExpr::Var {
            annotation: identifier.clone(),
            value: var_name,
        };

        while let Some(dot) = self.advance_if(Token::Dot) {
            match self.iter.next().transpose()? {
                Some((Token::Identifier(field_ident), _)) => {
                    // Validate field name
                    let field_name = FieldName::new(field_ident.as_str()).map_err(|error| {
                        ParseError::InvalidFieldName {
                            name: field_ident.to_string_span(),
                            error,
                            range: field_ident.clone(),
                        }
                    })?;
                    let range = expr.range().clone().to(field_ident.clone());
                    expr = SyntacticExpr::FieldAccess {
                        record: Box::new(expr),
                        field: field_name,
                        annotation: range,
                    };
                }
                Some((_, range)) => {
                    return Err(ParseError::ExpectedIdentifierAfterDot { range });
                }
                None => {
                    return Err(ParseError::UnexpectedEndOfFieldAccess {
                        range: expr.range().clone().to(dot.clone()),
                    });
                }
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<SyntacticExpr, ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::Identifier(name), _)) => self.parse_field_access(name),
            Some((Token::TypeName(name), name_range)) => {
                self.parse_record_instantiation(name, name_range)
            }
            Some((Token::StringLiteral(value), range)) => Ok(SyntacticExpr::StringLiteral {
                value,
                annotation: range,
            }),
            Some((Token::BooleanLiteral(value), range)) => Ok(SyntacticExpr::BooleanLiteral {
                value,
                annotation: range,
            }),
            Some((Token::IntLiteral(value), range)) => Ok(SyntacticExpr::IntLiteral {
                value,
                annotation: range,
            }),
            Some((Token::FloatLiteral(value), range)) => Ok(SyntacticExpr::FloatLiteral {
                value,
                annotation: range,
            }),
            Some((Token::LeftBracket, left_bracket)) => self.parse_array_literal(left_bracket),
            Some((Token::LeftParen, left_paren)) => {
                let expr = self.parse_logical()?;
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

    fn parse_record_instantiation(
        &mut self,
        name: DocumentRange,
        name_range: DocumentRange,
    ) -> Result<SyntacticExpr, ParseError> {
        let left_paren = self.expect_token(&Token::LeftParen)?;
        let mut fields = Vec::new();
        let right_paren = self.parse_delimited_list(&Token::LeftParen, &left_paren, |this| {
            let (field_name, _) = this.expect_field_name()?;
            this.expect_token(&Token::Colon)?;
            fields.push((field_name, this.parse_logical()?));
            Ok(())
        })?;
        Ok(SyntacticExpr::RecordInstantiation {
            record_name: name.as_str().to_string(),
            fields,
            annotation: name_range.to(right_paren),
        })
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

    fn check_parse_exprs(input: &str, expected: Expect) {
        let mut parser = Parser::from(input);
        let actual = match parser.parse_exprs() {
            Ok(result) => {
                let exprs: Vec<String> = result.iter().map(|expr| expr.to_string()).collect();
                format!("[{}]\n", exprs.join(", "))
            }
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

    fn check_parse_record(input: &str, expected: Expect) {
        let mut parser = Parser::from(input);

        let actual = match parser.parse_record() {
            Ok(result) => format!("{}\n", result),
            Err(err) => annotate_error(err),
        };

        expected.assert_eq(&actual);
    }

    ///////////////////////////////////////////////////////////////////////////
    /// RECORDS                                                             ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_parse_record_simple() {
        check_parse_record(
            "record Foo {bar: String}",
            expect![[r#"
                record Foo {bar: String}
            "#]],
        );
    }

    #[test]
    fn test_parse_record_multiple_fields() {
        check_parse_record(
            "record User {name: String, age: Int, active: Bool}",
            expect![[r#"
                record User {name: String, age: Int, active: Bool}
            "#]],
        );
    }

    #[test]
    fn test_parse_record_multiline_with_trailing_comma() {
        check_parse_record(
            indoc! {r#"
                record User {
                    name: String,
                    email: String,
                    age: Int,
                }
            "#},
            expect![[r#"
                record User {name: String, email: String, age: Int}
            "#]],
        );
    }

    #[test]
    fn test_parse_record_empty() {
        check_parse_record(
            "record Empty {}",
            expect![[r#"
                record Empty {}
            "#]],
        );
    }

    #[test]
    fn test_parse_record_with_array_type() {
        check_parse_record(
            "record Container {items: Array[String], count: Int}",
            expect![[r#"
                record Container {items: Array[String], count: Int}
            "#]],
        );
    }

    #[test]
    fn test_parse_record_duplicate_field_error() {
        check_parse_record(
            "record Foo {bar: String, bar: Int}",
            expect![[r#"
                error: Duplicate field 'bar'
                record Foo {bar: String, bar: Int}
                                         ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_record_missing_brace_error() {
        check_parse_record(
            "record Foo {bar: String",
            expect![[r#"
                error: Unmatched '{'
                record Foo {bar: String
                           ^
            "#]],
        );
    }

    #[test]
    fn test_parse_record_missing_name_error() {
        check_parse_record(
            "record {bar: String}",
            expect![[r#"
                error: Expected type name but got {
                record {bar: String}
                       ^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// RECORD INSTANTIATION                                                ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_parse_expr_record_instantiation_single_field() {
        check_parse_expr(
            r#"User(name: "John")"#,
            expect![[r#"
                User(name: "John")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_multiple_fields() {
        check_parse_expr(
            r#"User(name: "John", age: 30, active: true)"#,
            expect![[r#"
                User(name: "John", age: 30, active: true)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_empty() {
        check_parse_expr(
            "Empty()",
            expect![[r#"
                Empty()
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_trailing_comma() {
        check_parse_expr(
            r#"User(name: "John", age: 30,)"#,
            expect![[r#"
                User(name: "John", age: 30)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_multiline() {
        check_parse_expr(
            indoc! {r#"
                User(
                  name: "John",
                )
            "#},
            expect![[r#"
                User(name: "John")
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_nested() {
        check_parse_expr(
            r#"Wrapper(inner: Inner(value: 42))"#,
            expect![[r#"
                Wrapper(inner: Inner(value: 42))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_with_expressions() {
        check_parse_expr(
            "Point(x: a + b, y: c * 2)",
            expect![[r#"
                Point(x: (a + b), y: (c * 2))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_missing_paren_error() {
        check_parse_expr(
            r#"User(name: "John""#,
            expect![[r#"
                error: Unmatched '('
                User(name: "John"
                    ^
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_record_instantiation_missing_colon_error() {
        check_parse_expr(
            r#"User(name "John")"#,
            expect![[r#"
                error: Expected token ':' but got '"John"'
                User(name "John")
                          ^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// PARAMETERS                                                          ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_parse_parameters_type_keywords() {
        check_parse_parameters(
            "name: String, age: Int, score: Float, active: Bool, items: Array[String]",
            expect![[r#"
                [name: String, age: Int, score: Float, active: Bool, items: Array[String]]
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_trusted_html_type() {
        check_parse_parameters(
            "content: TrustedHTML",
            expect![[r#"
                [content: TrustedHTML]
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_named_type() {
        check_parse_parameters(
            "user: User, person: Person",
            expect![[r#"
                [user: User, person: Person]
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_named_type_in_array() {
        check_parse_parameters(
            "users: Array[User]",
            expect![[r#"
                [users: Array[User]]
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_duplicate_parameters_with_different_type_error() {
        check_parse_parameters(
            "foo: String, foo: Float",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: String, foo: Float
                             ^^^
            "#]],
        );
    }

    #[test]
    fn test_parse_parameters_duplicate_parameters_with_same_type_error() {
        check_parse_parameters(
            "foo: String, foo: String",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: String, foo: String
                             ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// ARGUMENTS                                                           ///
    ///////////////////////////////////////////////////////////////////////////

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

    ///////////////////////////////////////////////////////////////////////////
    /// EXPRESSION LISTS (parse_exprs)                                      ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_parse_exprs_single() {
        check_parse_exprs(
            "x",
            expect![[r#"
                [x]
            "#]],
        );
    }

    #[test]
    fn test_parse_exprs_multiple() {
        check_parse_exprs(
            "x, y, z",
            expect![[r#"
                [x, y, z]
            "#]],
        );
    }

    #[test]
    fn test_parse_exprs_with_field_access() {
        check_parse_exprs(
            "user.name, user.age, user.active",
            expect![[r#"
                [user.name, user.age, user.active]
            "#]],
        );
    }

    #[test]
    fn test_parse_exprs_with_complex_expressions() {
        check_parse_exprs(
            r#""hello", 123, true, [1, 2, 3]"#,
            expect![[r#"
                ["hello", 123, true, [1, 2, 3]]
            "#]],
        );
    }

    #[test]
    fn test_parse_exprs_with_operators() {
        check_parse_exprs(
            "x + y, a == b, !c",
            expect![[r#"
                [(x + y), (a == b), (!c)]
            "#]],
        );
    }

    #[test]
    fn test_parse_exprs_with_nested_commas() {
        check_parse_exprs(
            "[1, 2, 3], [4, 5, 6]",
            expect![[r#"
                [[1, 2, 3], [4, 5, 6]]
            "#]],
        );
    }

    #[test]
    fn test_parse_exprs_trailing_comma() {
        check_parse_exprs(
            "x, y,",
            expect![[r#"
                [x, y]
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS                                                         ///
    ///////////////////////////////////////////////////////////////////////////

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
                error: Unexpected end of field access
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
            ".field",
            expect![[r#"
                error: Unexpected token '.'
                .field
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
    fn test_parse_expr_field_access_comparison() {
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
    fn test_parse_expr_greater_than_or_equal() {
        check_parse_expr(
            "x >= y",
            expect![[r#"
                (x >= y)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_greater_than_or_equal_chained() {
        check_parse_expr(
            "a >= b >= c",
            expect![[r#"
                ((a >= b) >= c)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_field_access() {
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
    fn test_parse_expr_simple_field_access() {
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
    fn test_parse_expr_field_string_comparison() {
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
    fn test_parse_expr_string_concatenation_with_field_access() {
        check_parse_expr(
            r#"user.first_name + " " + user.last_name"#,
            expect![[r#"
                ((user.first_name + " ") + user.last_name)
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

    #[test]
    fn test_parse_expr_logical_and() {
        check_parse_expr(
            "a && b",
            expect![[r#"
                (a && b)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_and_chained() {
        check_parse_expr(
            "a && b && c",
            expect![[r#"
                ((a && b) && c)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_and_with_equals() {
        check_parse_expr(
            "a && b == c",
            expect![[r#"
                (a && (b == c))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_and_precedence() {
        check_parse_expr(
            "a == b && c != d",
            expect![[r#"
                ((a == b) && (c != d))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_and_with_comparison() {
        check_parse_expr(
            "x > y && a <= b",
            expect![[r#"
                ((x > y) && (a <= b))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_and_with_negation() {
        check_parse_expr(
            "!a && !b",
            expect![[r#"
                ((!a) && (!b))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_or() {
        check_parse_expr(
            "a || b",
            expect![[r#"
                (a || b)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_or_chained() {
        check_parse_expr(
            "a || b || c",
            expect![[r#"
                ((a || b) || c)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_or_with_equals() {
        check_parse_expr(
            "a || b == c",
            expect![[r#"
                (a || (b == c))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_mixed_logical_operators() {
        check_parse_expr(
            "a && b || c",
            expect![[r#"
                ((a && b) || c)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_mixed_logical_operators_complex() {
        check_parse_expr(
            "a || b && c || d",
            expect![[r#"
                ((a || (b && c)) || d)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_operators_precedence_with_comparison() {
        check_parse_expr(
            "x > y && a || b < c",
            expect![[r#"
                (((x > y) && a) || (b < c))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_logical_or_with_negation() {
        check_parse_expr(
            "!a || !b",
            expect![[r#"
                ((!a) || (!b))
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_addition_precedence() {
        check_parse_expr(
            "x + y == z",
            expect![[r#"
                ((x + y) == z)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_addition_with_logical_and() {
        check_parse_expr(
            "x + y > z && enabled",
            expect![[r#"
                (((x + y) > z) && enabled)
            "#]],
        );
    }

    #[test]
    fn test_parse_expr_chained_addition() {
        check_parse_expr(
            "x + y + z",
            expect![[r#"
                ((x + y) + z)
            "#]],
        );
    }
}
