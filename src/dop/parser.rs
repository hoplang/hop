use std::collections::HashSet;
use std::fmt::{self, Display};
use std::iter::Peekable;

use crate::document::document_cursor::{DocumentCursor, DocumentRange, Ranged as _, StringSpan};
use crate::dop::field_name::FieldName;
use crate::dop::parse_error::ParseError;
use crate::dop::syntactic_expr::{BinaryOp, MatchArm, SyntacticExpr};
use crate::dop::syntactic_type::SyntacticType;
use crate::dop::token::Token;
use crate::dop::tokenizer::Tokenizer;
use crate::dop::var_name::VarName;
use crate::error_collector::ErrorCollector;
use crate::hop::module_name::ModuleName;

use super::declaration::{
    Declaration, EnumDeclaration, EnumVariant, RecordDeclaration, RecordDeclarationField,
};
use super::expr::Expr;
use super::type_name::TypeName;

/// A Parameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct Parameter<T = SyntacticType> {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: T,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_type)
    }
}

pub type TypedArgument = Argument<Expr>;

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
                        name,
                        error,
                        range: range.clone(),
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
                        name,
                        error,
                        range: range.clone(),
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

    fn expect_type_name(&mut self) -> Result<(TypeName, DocumentRange), ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::TypeName(name), range)) => {
                let type_name =
                    TypeName::new(name.as_str()).map_err(|error| ParseError::InvalidTypeName {
                        error,
                        range: range.clone(),
                    })?;
                Ok((type_name, range))
            }
            Some((actual, range)) => Err(ParseError::ExpectedTypeNameButGot { actual, range }),
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

    pub fn parse_type(&mut self) -> Result<SyntacticType, ParseError> {
        match self.iter.next().transpose()? {
            Some((Token::TypeString, range)) => Ok(SyntacticType::String { range }),
            Some((Token::TypeInt, range)) => Ok(SyntacticType::Int { range }),
            Some((Token::TypeFloat, range)) => Ok(SyntacticType::Float { range }),
            Some((Token::TypeBoolean, range)) => Ok(SyntacticType::Bool { range }),
            Some((Token::TypeTrustedHTML, range)) => Ok(SyntacticType::TrustedHTML { range }),
            Some((Token::TypeArray, type_array)) => {
                let left_bracket = self.expect_token(&Token::LeftBracket)?;
                let element = self.parse_type()?;
                let right_bracket = self.expect_opposite(&Token::LeftBracket, &left_bracket)?;
                Ok(SyntacticType::Array {
                    element: Box::new(element),
                    range: type_array.to(right_bracket),
                })
            }
            Some((Token::TypeName(name), range)) => Ok(SyntacticType::Named {
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
        identifier: StringSpan,
        range: DocumentRange,
    ) -> Result<SyntacticExpr, ParseError> {
        let var_name =
            VarName::new(identifier.as_str()).map_err(|error| ParseError::InvalidVariableName {
                name: identifier,
                error,
                range: range.clone(),
            })?;
        let mut expr = SyntacticExpr::Var {
            annotation: range.clone(),
            value: var_name,
        };

        while let Some(dot) = self.advance_if(Token::Dot) {
            match self.iter.next().transpose()? {
                Some((Token::Identifier(field_ident), range)) => {
                    // Validate field name
                    let field_name = FieldName::new(field_ident.as_str()).map_err(|error| {
                        ParseError::InvalidFieldName {
                            name: field_ident,
                            error,
                            range: range.clone(),
                        }
                    })?;
                    let range = expr.range().clone().to(range.clone());
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
            Some((Token::Identifier(name), name_range)) => {
                self.parse_field_access(name, name_range)
            }
            Some((Token::TypeName(name), name_range)) => {
                let is_enum_variant = self
                    .iter
                    .peek()
                    .and_then(|r| r.as_ref().ok())
                    .map(|(t, _)| t)
                    == Some(&Token::ColonColon);
                if is_enum_variant {
                    self.parse_enum_instantiation(name, name_range)
                } else {
                    self.parse_record_instantiation(name, name_range)
                }
            }
            Some((Token::StringLiteral(value), range)) => Ok(SyntacticExpr::StringLiteral {
                value,
                annotation: range,
            }),
            Some((Token::True, range)) => Ok(SyntacticExpr::BooleanLiteral {
                value: true,
                annotation: range,
            }),
            Some((Token::False, range)) => Ok(SyntacticExpr::BooleanLiteral {
                value: false,
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
            Some((Token::Match, match_range)) => self.parse_match_expr(match_range),
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
        name: StringSpan,
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

    /// Parse an enum instantiation expression.
    ///
    /// Syntax: `EnumName::VariantName`
    fn parse_enum_instantiation(
        &mut self,
        enum_name: StringSpan,
        enum_name_range: DocumentRange,
    ) -> Result<SyntacticExpr, ParseError> {
        self.expect_token(&Token::ColonColon)?;
        let (variant_name, variant_range) = self.expect_type_name()?;
        Ok(SyntacticExpr::EnumInstantiation {
            enum_name: enum_name.as_str().to_string(),
            variant_name: variant_name.as_str().to_string(),
            annotation: enum_name_range.to(variant_range),
        })
    }

    /// Parse a match expression.
    ///
    /// Syntax: `match subject {Pattern1 => expr1, Pattern2 => expr2}`
    fn parse_match_expr(
        &mut self,
        match_range: DocumentRange,
    ) -> Result<SyntacticExpr, ParseError> {
        let subject = self.parse_primary()?;
        let left_brace = self.expect_token(&Token::LeftBrace)?;

        // Check for empty match
        if let Some(right_brace) = self.advance_if(Token::RightBrace) {
            return Err(ParseError::MatchNoArms {
                range: match_range.to(right_brace),
            });
        }

        let mut arms = Vec::new();
        let right_brace = self.parse_delimited_list(&Token::LeftBrace, &left_brace, |this| {
            let pattern = this.parse_primary()?;
            this.expect_token(&Token::FatArrow)?;
            let body = this.parse_logical()?;
            arms.push(MatchArm { pattern, body });
            Ok(())
        })?;

        Ok(SyntacticExpr::Match {
            subject: Box::new(subject),
            arms,
            annotation: match_range.to(right_brace),
        })
    }

    /// Parse an import declaration.
    ///
    /// Syntax: `import module::path::ComponentName`
    fn parse_import_declaration(&mut self) -> Result<Declaration, ParseError> {
        let import_range = self.expect_token(&Token::Import)?;

        let mut path_segments: Vec<DocumentRange> = Vec::new();

        let first_segment = match self.iter.next().transpose()? {
            Some((Token::Identifier(_), range)) | Some((Token::TypeName(_), range)) => range,
            Some((_, range)) => {
                return Err(ParseError::ExpectedModulePath { range });
            }
            None => {
                return Err(ParseError::ExpectedModulePath {
                    range: self.range.clone(),
                });
            }
        };
        path_segments.push(first_segment);

        while self.advance_if(Token::ColonColon).is_some() {
            let segment = match self.iter.next().transpose()? {
                Some((Token::Identifier(_), range)) | Some((Token::TypeName(_), range)) => range,
                Some((_, range)) => {
                    return Err(ParseError::ExpectedIdentifierAfterColonColon { range });
                }
                None => {
                    return Err(ParseError::ExpectedIdentifierAfterColonColon {
                        range: self.range.clone(),
                    });
                }
            };
            path_segments.push(segment);
        }

        if path_segments.len() < 2 {
            return Err(ParseError::ImportPathTooShort {
                range: path_segments[0].clone(),
            });
        }

        let name_range = path_segments.pop().unwrap();

        let name = TypeName::new(name_range.as_str()).map_err(|e| ParseError::InvalidTypeName {
            error: e,
            range: name_range.clone(),
        })?;

        let module_path_str = path_segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("/");

        let module_name =
            ModuleName::new(&module_path_str).map_err(|e| ParseError::InvalidModuleName {
                error: e,
                range: path_segments
                    .first()
                    .unwrap()
                    .clone()
                    .to(path_segments.last().unwrap().clone()),
            })?;

        let path_range = path_segments
            .first()
            .unwrap()
            .clone()
            .to(name_range.clone());

        Ok(Declaration::Import {
            name,
            name_range: name_range.clone(),
            path: path_range,
            module_name,
            range: import_range.to(name_range),
        })
    }

    /// Parse a record declaration.
    ///
    /// Syntax: `record Name {field: Type, ...}`
    fn parse_record_declaration(&mut self) -> Result<Declaration, ParseError> {
        let start_range = self.expect_token(&Token::Record)?;
        let (name, name_range) = self.expect_type_name()?;
        let left_brace = self.expect_token(&Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut seen_names = HashSet::new();
        let right_brace = self.parse_delimited_list(&Token::LeftBrace, &left_brace, |this| {
            let (name, name_range) = this.expect_field_name()?;
            this.expect_token(&Token::Colon)?;
            let field_type = this.parse_type()?;
            let field = RecordDeclarationField {
                name,
                name_range,
                field_type,
            };
            if !seen_names.insert(field.name.as_str().to_string()) {
                return Err(ParseError::DuplicateField {
                    name: field.name_range.to_string_span(),
                    range: field.name_range.clone(),
                });
            }
            fields.push(field);
            Ok(())
        })?;

        let declaration = RecordDeclaration {
            name,
            name_range,
            fields,
        };
        let full_range = start_range.to(right_brace);

        Ok(Declaration::Record {
            declaration,
            range: full_range,
        })
    }

    /// Parse an enum declaration.
    ///
    /// Syntax: `enum Name {Variant1, Variant2, ...}`
    fn parse_enum_declaration(&mut self) -> Result<Declaration, ParseError> {
        let start_range = self.expect_token(&Token::Enum)?;
        let (name, name_range) = self.expect_type_name()?;
        let left_brace = self.expect_token(&Token::LeftBrace)?;

        let mut variants = Vec::new();
        let mut seen_names = HashSet::new();
        let right_brace = self.parse_delimited_list(&Token::LeftBrace, &left_brace, |this| {
            let (variant_name, variant_range) = this.expect_type_name()?;
            let variant = EnumVariant {
                name: variant_name,
                name_range: variant_range,
            };
            if !seen_names.insert(variant.name.as_str().to_string()) {
                return Err(ParseError::DuplicateVariant {
                    name: variant.name_range.to_string_span(),
                    range: variant.name_range.clone(),
                });
            }
            variants.push(variant);
            Ok(())
        })?;

        let declaration = EnumDeclaration {
            name,
            name_range,
            variants,
        };
        let full_range = start_range.to(right_brace);

        Ok(Declaration::Enum {
            declaration,
            range: full_range,
        })
    }

    /// Parse all declarations from the source.
    ///
    /// This parses import and record declarations from a top-level
    /// text node. The text should only contain declarations and whitespace.
    pub fn parse_declarations(
        &mut self,
        errors: &mut ErrorCollector<ParseError>,
    ) -> Vec<Declaration> {
        let mut declarations = Vec::new();

        loop {
            match self.iter.peek() {
                Some(Ok((Token::Import, _))) => match self.parse_import_declaration() {
                    Ok(decl) => declarations.push(decl),
                    Err(err) => {
                        errors.push(err);
                        break;
                    }
                },
                Some(Ok((Token::Record, _))) => match self.parse_record_declaration() {
                    Ok(decl) => declarations.push(decl),
                    Err(err) => {
                        errors.push(err);
                        break;
                    }
                },
                Some(Ok((Token::Enum, _))) => match self.parse_enum_declaration() {
                    Ok(decl) => declarations.push(decl),
                    Err(err) => {
                        errors.push(err);
                        break;
                    }
                },
                Some(Ok((_, range))) => {
                    errors.push(ParseError::ExpectedDeclaration {
                        range: range.clone(),
                    });
                    break;
                }
                Some(Err(err)) => {
                    errors.push(err.clone());
                    break;
                }
                None => break,
            }
        }

        declarations
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

    fn check_parse_declarations(input: &str, expected: Expect) {
        use crate::document::document_cursor::DocumentCursor;
        use crate::error_collector::ErrorCollector;

        let mut errors = ErrorCollector::<ParseError>::new();
        let range = DocumentCursor::new(input.to_string()).range();
        let declarations = Parser::from(range).parse_declarations(&mut errors);

        let actual = if !errors.is_empty() {
            DocumentAnnotator::new()
                .with_label("error")
                .annotate(None, errors.to_vec())
        } else {
            declarations
                .iter()
                .map(|decl| format!("{:?}", decl))
                .collect::<Vec<_>>()
                .join("\n")
        };

        expected.assert_eq(&actual);
    }

    ///////////////////////////////////////////////////////////////////////////
    /// RECORD INSTANTIATION                                                ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_record_instantiation_with_single_field() {
        check_parse_expr(
            r#"User(name: "John")"#,
            expect![[r#"
                User(name: "John")
            "#]],
        );
    }

    #[test]
    fn should_accept_record_instantiation_with_multiple_fields() {
        check_parse_expr(
            r#"User(name: "John", age: 30, active: true)"#,
            expect![[r#"
                User(name: "John", age: 30, active: true)
            "#]],
        );
    }

    #[test]
    fn should_accept_record_instantiation_with_no_fields() {
        check_parse_expr(
            "Empty()",
            expect![[r#"
                Empty()
            "#]],
        );
    }

    #[test]
    fn should_accept_record_instantiation_with_trailing_comma() {
        check_parse_expr(
            r#"User(name: "John", age: 30,)"#,
            expect![[r#"
                User(name: "John", age: 30)
            "#]],
        );
    }

    #[test]
    fn should_accept_record_instantiation_with_multiline_fields() {
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
    fn should_accept_record_instantiation_with_nested_records() {
        check_parse_expr(
            r#"Wrapper(inner: Inner(value: 42))"#,
            expect![[r#"
                Wrapper(inner: Inner(value: 42))
            "#]],
        );
    }

    #[test]
    fn should_accept_record_instantiation_with_expression_values() {
        check_parse_expr(
            "Point(x: a + b, y: c * 2)",
            expect![[r#"
                Point(x: (a + b), y: (c * 2))
            "#]],
        );
    }

    #[test]
    fn should_reject_record_instantiation_when_closing_paren_is_missing() {
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
    fn should_reject_record_instantiation_when_colon_is_missing() {
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
    fn should_accept_parameters_with_builtin_type_keywords() {
        check_parse_parameters(
            "name: String, age: Int, score: Float, active: Bool, items: Array[String]",
            expect![[r#"
                [name: String, age: Int, score: Float, active: Bool, items: Array[String]]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_trusted_html_type() {
        check_parse_parameters(
            "content: TrustedHTML",
            expect![[r#"
                [content: TrustedHTML]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_named_type() {
        check_parse_parameters(
            "user: User, person: Person",
            expect![[r#"
                [user: User, person: Person]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_named_type_inside_array() {
        check_parse_parameters(
            "users: Array[User]",
            expect![[r#"
                [users: Array[User]]
            "#]],
        );
    }

    #[test]
    fn should_reject_parameters_when_name_is_duplicated_with_different_type() {
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
    fn should_reject_parameters_when_name_is_duplicated_with_same_type() {
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
    fn should_accept_arguments_with_single_argument() {
        check_parse_arguments(
            r#"name: "John""#,
            expect![[r#"
                [name: "John"]
            "#]],
        );
    }

    #[test]
    fn should_accept_arguments_with_multiple_arguments() {
        check_parse_arguments(
            r#"name: "John", age: 25, active: true"#,
            expect![[r#"
                [name: "John", age: 25, active: true]
            "#]],
        );
    }

    #[test]
    fn should_accept_arguments_with_complex_expressions() {
        check_parse_arguments(
            "user: user.name, enabled: !user.disabled",
            expect![[r#"
                [user: user.name, enabled: (!user.disabled)]
            "#]],
        );
    }

    #[test]
    fn should_accept_arguments_with_trailing_comma() {
        check_parse_arguments(
            r#"name: "John", age: 25,"#,
            expect![[r#"
                [name: "John", age: 25]
            "#]],
        );
    }

    #[test]
    fn should_reject_arguments_when_name_is_duplicated() {
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
    fn should_reject_arguments_when_colon_is_missing() {
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
    fn should_reject_arguments_when_value_is_missing() {
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
    fn should_reject_arguments_when_name_is_not_identifier() {
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
    fn should_reject_arguments_when_comma_is_missing_between_arguments() {
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
    fn should_accept_exprs_with_single_expression() {
        check_parse_exprs(
            "x",
            expect![[r#"
                [x]
            "#]],
        );
    }

    #[test]
    fn should_accept_exprs_with_multiple_expressions() {
        check_parse_exprs(
            "x, y, z",
            expect![[r#"
                [x, y, z]
            "#]],
        );
    }

    #[test]
    fn should_accept_exprs_containing_field_access() {
        check_parse_exprs(
            "user.name, user.age, user.active",
            expect![[r#"
                [user.name, user.age, user.active]
            "#]],
        );
    }

    #[test]
    fn should_accept_exprs_containing_mixed_literal_types() {
        check_parse_exprs(
            r#""hello", 123, true, [1, 2, 3]"#,
            expect![[r#"
                ["hello", 123, true, [1, 2, 3]]
            "#]],
        );
    }

    #[test]
    fn should_accept_exprs_containing_operators() {
        check_parse_exprs(
            "x + y, a == b, !c",
            expect![[r#"
                [(x + y), (a == b), (!c)]
            "#]],
        );
    }

    #[test]
    fn should_accept_exprs_containing_arrays_with_nested_commas() {
        check_parse_exprs(
            "[1, 2, 3], [4, 5, 6]",
            expect![[r#"
                [[1, 2, 3], [4, 5, 6]]
            "#]],
        );
    }

    #[test]
    fn should_accept_exprs_with_trailing_comma() {
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
    fn should_reject_expr_when_trailing_tokens_are_present() {
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
    fn should_reject_expr_when_array_bracket_is_unmatched() {
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
    fn should_reject_expr_when_array_has_invalid_token_after_comma() {
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
    fn should_reject_expr_when_array_closing_bracket_is_missing() {
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
    fn should_reject_expr_when_array_has_unexpected_token_instead_of_bracket() {
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
    fn should_reject_expr_when_field_access_ends_with_dot() {
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
    fn should_reject_expr_when_field_name_is_number() {
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
    fn should_reject_expr_when_starting_with_operator() {
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
    fn should_reject_expr_when_closing_paren_is_missing() {
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
    fn should_reject_expr_when_closing_paren_has_no_opening() {
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
    fn should_reject_expr_when_parens_are_empty() {
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
    fn should_reject_expr_when_right_operand_is_invalid() {
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
    fn should_reject_expr_when_starting_with_dot() {
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
    fn should_reject_expr_when_containing_double_dot() {
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
    fn should_reject_expr_when_ending_with_operator() {
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
    fn should_reject_expr_when_not_operator_has_no_operand() {
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
    fn should_reject_expr_when_not_operator_is_trailing() {
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
    fn should_accept_expr_with_chained_equality_operators() {
        check_parse_expr(
            "a == b == c",
            expect![[r#"
                ((a == b) == c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_field_accesses() {
        check_parse_expr(
            "user.name == admin.name",
            expect![[r#"
                (user.name == admin.name)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_less_than_or_equal_operator() {
        check_parse_expr(
            "x <= y",
            expect![[r#"
                (x <= y)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_less_than_or_equal_operators() {
        check_parse_expr(
            "a <= b <= c",
            expect![[r#"
                ((a <= b) <= c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_greater_than_or_equal_operator() {
        check_parse_expr(
            "x >= y",
            expect![[r#"
                (x >= y)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_greater_than_or_equal_operators() {
        check_parse_expr(
            "a >= b >= c",
            expect![[r#"
                ((a >= b) >= c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_deeply_nested_field_access() {
        check_parse_expr(
            "app.user.profile.settings.theme",
            expect![[r#"
                app.user.profile.settings.theme
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_empty_string_literal() {
        check_parse_expr(
            r#""""#,
            expect![[r#"
                ""
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_integer_literal() {
        check_parse_expr(
            "99",
            expect![[r#"
                99
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_float_literal() {
        check_parse_expr(
            "3.14",
            expect![[r#"
                3.14
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_mixed_int_and_float_operands() {
        check_parse_expr(
            "42 + 3.14",
            expect![[r#"
                (42 + 3.14)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_parenthesized_expression() {
        check_parse_expr(
            "(x == y)",
            expect![[r#"
                (x == y)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_simple_field_access() {
        check_parse_expr(
            "user.name",
            expect![[r#"
                user.name
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_string_literal_to_field() {
        check_parse_expr(
            r#""guest" == user.role"#,
            expect![[r#"
                ("guest" == user.role)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_two_variables() {
        check_parse_expr(
            "x == y",
            expect![[r#"
                (x == y)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_literal() {
        check_parse_expr(
            r#""hello""#,
            expect![[r#"
                "hello"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_single_variable() {
        check_parse_expr(
            "x",
            expect![[r#"
                x
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_two_string_literals() {
        check_parse_expr(
            r#""apple" == "orange""#,
            expect![[r#"
                ("apple" == "orange")
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_field_to_string_literal() {
        check_parse_expr(
            r#"user.name == "admin""#,
            expect![[r#"
                (user.name == "admin")
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_containing_space() {
        check_parse_expr(
            r#""hello world""#,
            expect![[r#"
                "hello world"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_ignoring_surrounding_whitespace() {
        check_parse_expr(
            "  user . name   ==   admin . name  ",
            expect![[r#"
                (user.name == admin.name)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_empty_array() {
        check_parse_expr(
            "[]",
            expect![[r#"
                []
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_of_integers() {
        check_parse_expr(
            "[1, 2, 3]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_of_mixed_types() {
        check_parse_expr(
            r#"[1, "hello", true]"#,
            expect![[r#"
                [1, "hello", true]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_nested_arrays() {
        check_parse_expr(
            "[[1, 2], [3, 4]]",
            expect![[r#"
                [[1, 2], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_nested_arrays_containing_expressions() {
        check_parse_expr(
            "[[1 == [1 == 2], [] == []], [3, 4]]",
            expect![[r#"
                [[(1 == [(1 == 2)]), ([] == [])], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_containing_variables() {
        check_parse_expr(
            "[x, user.name]",
            expect![[r#"
                [x, user.name]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_multiline_array_and_trailing_comma() {
        check_parse_expr(
            "[\n\t1,\n\t2,\n\t3,\n]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_single_element_array_and_trailing_comma() {
        check_parse_expr(
            "[\n\t1,\n]",
            expect![[r#"
                [1]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_containing_complex_expressions() {
        check_parse_expr(
            "[\n\tuser.name,\n\t!user.disabled,\n]",
            expect![[r#"
                [user.name, (!user.disabled)]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_concatenation() {
        check_parse_expr(
            r#""hello" + "world""#,
            expect![[r#"
                ("hello" + "world")
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_multiple_string_concatenations() {
        check_parse_expr(
            r#""hello" + " " + "world""#,
            expect![[r#"
                (("hello" + " ") + "world")
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_concatenation_using_variables() {
        check_parse_expr(
            r#"greeting + " " + name"#,
            expect![[r#"
                ((greeting + " ") + name)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_concatenation_having_lower_precedence_than_equality() {
        check_parse_expr(
            r#""a" + "b" == "ab""#,
            expect![[r#"
                (("a" + "b") == "ab")
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_concatenation_using_field_access() {
        check_parse_expr(
            r#"user.first_name + " " + user.last_name"#,
            expect![[r#"
                ((user.first_name + " ") + user.last_name)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_not_equals_operator() {
        check_parse_expr(
            "x != y",
            expect![[r#"
                (x != y)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_not_equals_comparing_strings() {
        check_parse_expr(
            r#""hello" != "world""#,
            expect![[r#"
                ("hello" != "world")
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_not_equals_operators() {
        check_parse_expr(
            "a != b != c",
            expect![[r#"
                ((a != b) != c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_mixed_equals_and_not_equals() {
        check_parse_expr(
            "a == b != c",
            expect![[r#"
                ((a == b) != c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_operator() {
        check_parse_expr(
            "a && b",
            expect![[r#"
                (a && b)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_logical_and_operators() {
        check_parse_expr(
            "a && b && c",
            expect![[r#"
                ((a && b) && c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_having_lower_precedence_than_equality() {
        check_parse_expr(
            "a && b == c",
            expect![[r#"
                (a && (b == c))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_equality_having_higher_precedence_than_logical_and() {
        check_parse_expr(
            "a == b && c != d",
            expect![[r#"
                ((a == b) && (c != d))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_combining_comparisons() {
        check_parse_expr(
            "x > y && a <= b",
            expect![[r#"
                ((x > y) && (a <= b))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_combining_negations() {
        check_parse_expr(
            "!a && !b",
            expect![[r#"
                ((!a) && (!b))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_or_operator() {
        check_parse_expr(
            "a || b",
            expect![[r#"
                (a || b)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_logical_or_operators() {
        check_parse_expr(
            "a || b || c",
            expect![[r#"
                ((a || b) || c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_or_having_lower_precedence_than_equality() {
        check_parse_expr(
            "a || b == c",
            expect![[r#"
                (a || (b == c))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_having_higher_precedence_than_or() {
        check_parse_expr(
            "a && b || c",
            expect![[r#"
                ((a && b) || c)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_mixed_logical_operators_respecting_precedence() {
        check_parse_expr(
            "a || b && c || d",
            expect![[r#"
                ((a || (b && c)) || d)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_operators_and_comparisons_respecting_precedence() {
        check_parse_expr(
            "x > y && a || b < c",
            expect![[r#"
                (((x > y) && a) || (b < c))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_or_combining_negations() {
        check_parse_expr(
            "!a || !b",
            expect![[r#"
                ((!a) || (!b))
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_addition_having_higher_precedence_than_equality() {
        check_parse_expr(
            "x + y == z",
            expect![[r#"
                ((x + y) == z)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_addition_and_comparison_and_logical_and() {
        check_parse_expr(
            "x + y > z && enabled",
            expect![[r#"
                (((x + y) > z) && enabled)
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_addition_operators() {
        check_parse_expr(
            "x + y + z",
            expect![[r#"
                ((x + y) + z)
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// DECLARATIONS                                                        ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_import_with_simple_path() {
        check_parse_declarations(
            indoc! {r#"
                import user_list::UserList
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  module_name: user_list,
                }"#]],
        );
    }

    #[test]
    fn should_accept_import_with_nested_path() {
        check_parse_declarations(
            indoc! {r#"
                import components::header::Header
            "#},
            expect![[r#"
                Import {
                  name: Header,
                  module_name: components::header,
                }"#]],
        );
    }

    #[test]
    fn should_reject_import_when_path_has_only_one_segment() {
        check_parse_declarations(
            indoc! {r#"
                import Header
            "#},
            expect![[r#"
                error: Import path must have at least two segments: module::Component
                1 | import Header
                  |        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_import_when_type_name_is_not_pascal_case() {
        check_parse_declarations(
            indoc! {r#"
                import foo::bar
            "#},
            expect![[r#"
                error: Type name must start with an uppercase letter
                1 | import foo::bar
                  |             ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_multiple_fields() {
        check_parse_declarations(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            expect![[r#"
                Record {
                  name: User,
                  fields: {
                    name: String,
                    age: Int,
                  },
                }"#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_nested_array_field_type() {
        check_parse_declarations(
            indoc! {"
                record UserList {
                    users: Array[User],
                }
            "},
            expect![[r#"
                Record {
                  name: UserList,
                  fields: {
                    users: Array[User],
                  },
                }"#]],
        );
    }

    #[test]
    fn should_reject_html_input_as_declaration() {
        check_parse_declarations(
            indoc! {"
                <div>hello</div>
            "},
            expect![[r#"
                error: Expected declaration (import, record, or enum)
                1 | <div>hello</div>
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_accept_import_with_leading_empty_lines() {
        check_parse_declarations(
            indoc! {r#"


                import user_list::UserList
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  module_name: user_list,
                }"#]],
        );
    }

    #[test]
    fn should_accept_multiple_mixed_declarations() {
        check_parse_declarations(
            indoc! {r#"
                import header::Header
                record User {
                    name: String,
                }
                import footer::Footer
            "#},
            expect![[r#"
                Import {
                  name: Header,
                  module_name: header,
                }
                Record {
                  name: User,
                  fields: {
                    name: String,
                  },
                }
                Import {
                  name: Footer,
                  module_name: footer,
                }"#]],
        );
    }

    #[test]
    fn should_reject_text_starting_with_import_keyword() {
        check_parse_declarations(
            "important information",
            expect![[r#"
                error: Expected declaration (import, record, or enum)
                1 | important information
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_text_starting_with_record_keyword() {
        check_parse_declarations(
            "recording started",
            expect![[r#"
                error: Expected declaration (import, record, or enum)
                1 | recording started
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unexpected_content_before_declaration() {
        check_parse_declarations(
            "• bullet point\nrecord User {name: String}",
            expect![[r#"
                error: Unexpected character: '•'
                1 | • bullet point
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_field_without_type() {
        check_parse_declarations(
            indoc! {"
                record X {
                    foo
                }
            "},
            expect![[r#"
                error: Expected token ':' but got '}'
                3 | }
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_no_fields() {
        check_parse_declarations(
            "record Empty {}",
            expect![[r#"
                Record {
                  name: Empty,
                  fields: {},
                }"#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_trailing_comma() {
        check_parse_declarations(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            expect![[r#"
                Record {
                  name: User,
                  fields: {
                    name: String,
                    age: Int,
                  },
                }"#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_with_duplicate_field() {
        check_parse_declarations(
            "record Foo {bar: String, bar: Int}",
            expect![[r#"
                error: Duplicate field 'bar'
                1 | record Foo {bar: String, bar: Int}
                  |                          ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_with_missing_closing_brace() {
        check_parse_declarations(
            "record Foo {bar: String",
            expect![[r#"
                error: Unmatched '{'
                1 | record Foo {bar: String
                  |            ^
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_with_missing_type_name() {
        check_parse_declarations(
            "record {bar: String}",
            expect![[r#"
                error: Expected type name but got {
                1 | record {bar: String}
                  |        ^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_with_single_variant() {
        check_parse_declarations(
            "enum Color {Red}",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                  },
                }"#]],
        );
    }

    #[test]
    fn should_accept_enum_with_multiple_variants() {
        check_parse_declarations(
            "enum Color {Red, Green, Blue}",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                    Green,
                    Blue,
                  },
                }"#]],
        );
    }

    #[test]
    fn should_accept_enum_with_trailing_comma() {
        check_parse_declarations(
            "enum Color {Red, Green, Blue,}",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                    Green,
                    Blue,
                  },
                }"#]],
        );
    }

    #[test]
    fn should_accept_empty_enum() {
        check_parse_declarations(
            "enum Empty {}",
            expect![[r#"
                Enum {
                  name: Empty,
                  variants: {},
                }"#]],
        );
    }

    #[test]
    fn should_reject_enum_with_duplicate_variant() {
        check_parse_declarations(
            "enum Color {Red, Red}",
            expect![[r#"
                error: Duplicate variant 'Red'
                1 | enum Color {Red, Red}
                  |                  ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_with_missing_closing_brace() {
        check_parse_declarations(
            "enum Color {Red",
            expect![[r#"
                error: Unmatched '{'
                1 | enum Color {Red
                  |            ^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_with_lowercase_variant() {
        check_parse_declarations(
            "enum Color {red}",
            expect![[r#"
                error: Expected type name but got red
                1 | enum Color {red}
                  |             ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// ENUM INSTANTIATION                                                  ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_enum_instantiation() {
        check_parse_expr(
            "Color::Red",
            expect![[r#"
                Color::Red
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_instantiation_in_equality() {
        check_parse_expr(
            "Color::Red == Color::Green",
            expect![[r#"
                (Color::Red == Color::Green)
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_instantiation_in_record_field() {
        check_parse_expr(
            r#"User(name: "Alice", status: Status::Active)"#,
            expect![[r#"
                User(name: "Alice", status: Status::Active)
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_instantiation_with_lowercase_variant() {
        check_parse_expr(
            "Color::red",
            expect![[r#"
                error: Expected type name but got red
                Color::red
                       ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_instantiation_missing_variant() {
        check_parse_expr(
            "Color::",
            expect![[r#"
                error: Unexpected end of expression
                Color::
                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_single_arm() {
        check_parse_expr(
            indoc! {r#"
                match color {Color::Red => "red"}
            "#},
            expect![[r#"
                match color {Color::Red => "red"}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_multiple_arms() {
        check_parse_expr(
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Blue => "blue",
                    Color::Green => "green",
                }
            "#},
            expect![[r#"
                match color {
                  Color::Red => "red",
                  Color::Blue => "blue",
                  Color::Green => "green",
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_trailing_comma() {
        check_parse_expr(
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Blue => "blue",
                }
            "#},
            expect![[r#"
                match color {Color::Red => "red", Color::Blue => "blue"}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_complex_body() {
        check_parse_expr(
            indoc! {r#"
                match status {
                    Status::Active => user.name,
                    Status::Inactive => "unknown",
                }
            "#},
            expect![[r#"
                match status {
                  Status::Active => user.name,
                  Status::Inactive => "unknown",
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_nested_match_expression() {
        check_parse_expr(
            indoc! {r#"
                match outer {
                    Outer::A => match inner {Inner::X => 1, Inner::Y => 2},
                    Outer::B => 3,
                }
            "#},
            expect![[r#"
                match outer {
                  Outer::A => match inner {Inner::X => 1, Inner::Y => 2},
                  Outer::B => 3,
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_empty_match_expression() {
        check_parse_expr(
            "match color {}",
            expect![[r#"
                error: Match expression must have at least one arm
                match color {}
                ^^^^^^^^^^^^^^
            "#]],
        );
    }
}
