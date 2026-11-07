use std::fmt::{self, Display, Write};
use std::ops::Bound;

use super::parsed_markup::ParsedMarkup;
use super::parsed_type::ParsedType;
use super::token::LangToken;
use crate::display::{Indent, join, write_tuple};
use crate::document::{CheapString, DocumentRange};
use crate::hop::uncooked_string::UncookedString;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    VariableReference {
        value: VarName,
        range: DocumentRange,
    },

    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        range: DocumentRange,
    },

    MethodCall {
        receiver: Box<Self>,
        method: FieldName,
        method_range: DocumentRange,
        args: Vec<Self>,
        range: DocumentRange,
    },

    StringLiteral {
        value: UncookedString,
        range: DocumentRange,
    },

    BoolLiteral {
        value: bool,
        range: DocumentRange,
    },

    IntLiteral {
        value: i32,
        minus_range: Option<DocumentRange>,
        digits_range: DocumentRange,
        range: DocumentRange,
    },

    FloatLiteral {
        value: f64,
        minus_range: Option<DocumentRange>,
        digits_range: DocumentRange,
        range: DocumentRange,
    },

    Array {
        elements: Vec<Self>,
        range: DocumentRange,
    },

    Tuple {
        elements: Vec<Self>,
        range: DocumentRange,
    },

    Record {
        type_name: TypeName,
        type_name_range: DocumentRange,
        fields: Vec<ParsedFieldInitializer>,
        spread: Option<Box<Self>>,
        range: DocumentRange,
    },

    Enum {
        type_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<ParsedFieldInitializer>,
        /// Range of just the constructor (e.g., `Point::XY` without the field values)
        constructor_range: DocumentRange,
        /// Range of just the type name (e.g., `Point` in `Point::XY`)
        type_name_range: DocumentRange,
        range: DocumentRange,
    },

    Unary {
        operator: ParsedUnaryOp,
        operand: Box<Self>,
        range: DocumentRange,
    },

    Binary {
        left: Box<Self>,
        operator: ParsedBinaryOp,
        right: Box<Self>,
        range: DocumentRange,
    },

    Match {
        subject: Box<Self>,
        arms: Vec<ParsedMatchArm>,
        range: DocumentRange,
    },

    For {
        /// The bound variable name, `None` when the variable is discarded
        /// using `_`.
        var_name: Option<VarName>,
        var_name_range: Option<DocumentRange>,
        source: Box<ParsedLoopSource>,
        key: Option<Box<Self>>,
        body: Box<Self>,
        range: DocumentRange,
    },

    CellWrite {
        cell: VarName,
        cell_range: DocumentRange,
        value: Box<Self>,
        range: DocumentRange,
    },

    Option {
        value: Option<Box<Self>>,
        range: DocumentRange,
    },

    Macro {
        /// The name of the macro, e.g. `join`.
        name: CheapString,
        /// The range of the function subject, e.g. `join!`.
        subject_range: DocumentRange,
        args: Vec<Self>,
        range: DocumentRange,
    },

    Let {
        binding: Box<ParsedLetBinding>,
        body: Box<Self>,
        range: DocumentRange,
    },

    Lambda {
        params: Vec<ParsedLambdaParameter>,
        return_type: Option<ParsedType>,
        body: Box<Self>,
        range: DocumentRange,
    },

    Markup {
        markup: Box<ParsedMarkup>,
    },

    Call {
        name: FunctionName,
        name_range: DocumentRange,
        args: ParsedArguments,
        range: DocumentRange,
    },
}

#[derive(Debug, Clone)]
pub enum ParsedArguments {
    Positional(Vec<ParsedExpr>),
    Named(Vec<ParsedNamedArgument>),
}

#[derive(Debug, Clone)]
pub enum ParsedNamedArgument {
    /// An argument with a name.
    ///
    /// ```text
    /// Button(kind: "k", "aria-label": "Save")
    ///        ^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^
    /// ```
    Value {
        /// The name without quotes.
        name: AttributeName,
        /// The range of the name as written, with its quotes if it has any.
        name_range: DocumentRange,
        value: ParsedExpr,
    },
    /// A spread of the caller's rest.
    ///
    /// ```text
    /// Button(kind: "k", ...rest)
    ///                   ^^^^^^^
    /// ```
    Spread { name: VarName, range: DocumentRange },
}

/// The source of iteration in a for loop.
#[derive(Debug, Clone)]
pub enum ParsedLoopSource {
    /// An array expression.
    ///
    /// ```text
    /// for item in [1, 2, 3] {
    ///             ^^^^^^^^^
    /// ```
    Array(ParsedExpr),
    /// An inclusive integer range.
    ///
    /// ```text
    /// for i in 0..=5 {
    ///          ^^^^^
    /// ```
    RangeInclusive { start: ParsedExpr, end: ParsedExpr },
}

#[derive(Debug, Clone)]
pub struct ParsedFieldInitializer {
    pub name: FieldName,
    pub name_range: DocumentRange,
    pub value: ParsedExpr,
}

/// A let binding in a block expression.
///
/// ```text
/// let name: String = "World";
///     ^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, Clone)]
pub struct ParsedLetBinding {
    pub mutable: bool,
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: Option<ParsedType>,
    pub value_expr: ParsedExpr,
}

/// A parameter of a lambda, whose type may be inferred.
///
/// ```text
/// fn(item: Item) { item.label }
///    ^^^^^^^^^^
/// ```
#[derive(Debug, Clone)]
pub struct ParsedLambdaParameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: Option<ParsedType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedUnaryOp {
    LogicalNot,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedBinaryOp {
    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    Plus,
    Minus,
    Multiply,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    CellWrite,
    LogicalOr,
    LogicalAnd,
    Comparison,
    Additive,
    Multiplicative,
    Prefix,
    Postfix,
    /// Expressions that never need parentheses as an operand, either
    /// because they are atomic or because they are delimited.
    Atom,
}

impl Precedence {
    pub fn satisfies(self, bound: Bound<Precedence>) -> bool {
        match bound {
            Bound::Included(min) => self >= min,
            Bound::Excluded(min) => self > min,
            Bound::Unbounded => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Left,
    None,
}

impl ParsedBinaryOp {
    pub fn from_token(token: &LangToken) -> Option<Self> {
        Some(match token {
            LangToken::LogicalOr => ParsedBinaryOp::LogicalOr,
            LangToken::LogicalAnd => ParsedBinaryOp::LogicalAnd,
            LangToken::Eq => ParsedBinaryOp::Eq,
            LangToken::NotEq => ParsedBinaryOp::NotEq,
            LangToken::LessThan => ParsedBinaryOp::LessThan,
            LangToken::GreaterThan => ParsedBinaryOp::GreaterThan,
            LangToken::LessThanOrEqual => ParsedBinaryOp::LessThanOrEqual,
            LangToken::GreaterThanOrEqual => ParsedBinaryOp::GreaterThanOrEqual,
            LangToken::Plus => ParsedBinaryOp::Plus,
            LangToken::Minus => ParsedBinaryOp::Minus,
            LangToken::Asterisk => ParsedBinaryOp::Multiply,
            _ => return None,
        })
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            ParsedBinaryOp::LogicalOr => Precedence::LogicalOr,
            ParsedBinaryOp::LogicalAnd => Precedence::LogicalAnd,
            ParsedBinaryOp::Eq
            | ParsedBinaryOp::NotEq
            | ParsedBinaryOp::LessThan
            | ParsedBinaryOp::GreaterThan
            | ParsedBinaryOp::LessThanOrEqual
            | ParsedBinaryOp::GreaterThanOrEqual => Precedence::Comparison,
            ParsedBinaryOp::Plus | ParsedBinaryOp::Minus => Precedence::Additive,
            ParsedBinaryOp::Multiply => Precedence::Multiplicative,
        }
    }

    pub fn fixity(&self) -> Fixity {
        match self.precedence() {
            Precedence::Comparison => Fixity::None,
            _ => Fixity::Left,
        }
    }

    pub fn operand_bounds(&self) -> (Bound<Precedence>, Bound<Precedence>) {
        let precedence = self.precedence();
        match self.fixity() {
            Fixity::Left => (Bound::Included(precedence), Bound::Excluded(precedence)),
            Fixity::None => (Bound::Excluded(precedence), Bound::Excluded(precedence)),
        }
    }
}

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone)]
pub struct ParsedMatchArm {
    /// The pattern being matched
    pub pattern: ParsedPattern,
    /// The expression to evaluate if this arm matches
    pub body: ParsedExpr,
}

/// A pattern in a match arm
#[derive(Debug, Clone)]
pub enum ParsedPattern {
    /// A wildcard pattern that matches anything, written as `_`
    Wildcard { range: DocumentRange },
    /// A binding pattern that matches anything and binds it to a name
    Binding { name: VarName, range: DocumentRange },
    /// A boolean true pattern
    BoolTrue { range: DocumentRange },
    /// A boolean false pattern
    BoolFalse { range: DocumentRange },
    /// An Option Some pattern, e.g. `Some(_)`
    OptionSome {
        inner: Box<ParsedPattern>,
        range: DocumentRange,
    },
    /// An Option None pattern, e.g. `None`
    OptionNone { range: DocumentRange },
    /// An enum variant pattern, e.g. `Color::Red`
    EnumVariant {
        type_name: TypeName,
        /// Range of just the type name (e.g., `Color` in `Color::Red`)
        type_name_range: DocumentRange,
        variant_name: TypeName,
        /// Named field patterns (e.g., `{x: a, y: b}` in `Point::XY {x: a, y: b}`)
        /// The tuple is (field_name, field_name_range, field_pattern)
        fields: Vec<(FieldName, DocumentRange, ParsedPattern)>,
        /// Range of just the constructor (e.g., `Point::XY` without the field patterns)
        constructor_range: DocumentRange,
        /// Range of the entire pattern including fields
        range: DocumentRange,
    },
    /// A record pattern, e.g. `User {name: x, age: y}`
    Record {
        type_name: TypeName,
        type_name_range: DocumentRange,
        /// Named field patterns
        /// The tuple is (field_name, field_name_range, field_pattern)
        fields: Vec<(FieldName, DocumentRange, ParsedPattern)>,
        /// Range of the entire pattern including fields
        range: DocumentRange,
    },
    /// A tuple pattern, e.g. `(x, _)`
    Tuple {
        args: Vec<ParsedPattern>,
        range: DocumentRange,
    },
}

impl ParsedPattern {
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedPattern::Wildcard { range }
            | ParsedPattern::Binding { range, .. }
            | ParsedPattern::BoolTrue { range }
            | ParsedPattern::BoolFalse { range }
            | ParsedPattern::OptionSome { range, .. }
            | ParsedPattern::OptionNone { range }
            | ParsedPattern::EnumVariant { range, .. }
            | ParsedPattern::Record { range, .. }
            | ParsedPattern::Tuple { range, .. } => range,
        }
    }
}

impl ParsedUnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            ParsedUnaryOp::LogicalNot => "!",
            ParsedUnaryOp::Minus => "-",
        }
    }
}

impl ParsedBinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            ParsedBinaryOp::Eq => "==",
            ParsedBinaryOp::NotEq => "!=",
            ParsedBinaryOp::LessThan => "<",
            ParsedBinaryOp::GreaterThan => ">",
            ParsedBinaryOp::LessThanOrEqual => "<=",
            ParsedBinaryOp::GreaterThanOrEqual => ">=",
            ParsedBinaryOp::LogicalAnd => "&&",
            ParsedBinaryOp::LogicalOr => "||",
            ParsedBinaryOp::Plus => "+",
            ParsedBinaryOp::Minus => "-",
            ParsedBinaryOp::Multiply => "*",
        }
    }
}

impl ParsedExpr {
    /// Apply `f` to each direct child expression, in source order.
    pub fn for_each_child<'a>(&'a self, f: &mut impl FnMut(&'a ParsedExpr)) {
        match self {
            ParsedExpr::FieldAccess { record: inner, .. }
            | ParsedExpr::Unary { operand: inner, .. } => f(inner),

            ParsedExpr::MethodCall { receiver, args, .. } => {
                f(receiver);
                for arg in args {
                    f(arg);
                }
            }

            ParsedExpr::Binary { left, right, .. } => {
                f(left);
                f(right);
            }
            ParsedExpr::Array { elements, .. }
            | ParsedExpr::Tuple { elements, .. }
            | ParsedExpr::Macro { args: elements, .. } => {
                for element in elements {
                    f(element);
                }
            }
            ParsedExpr::Call { args, .. } => match args {
                ParsedArguments::Positional(values) => {
                    for value in values {
                        f(value);
                    }
                }
                ParsedArguments::Named(named) => {
                    for arg in named {
                        match arg {
                            ParsedNamedArgument::Value { value, .. } => f(value),
                            ParsedNamedArgument::Spread { .. } => {}
                        }
                    }
                }
            },
            ParsedExpr::Record { fields, spread, .. } => {
                if let Some(spread) = spread {
                    f(spread);
                }
                for field in fields {
                    f(&field.value);
                }
            }
            ParsedExpr::Enum { fields, .. } => {
                for field in fields {
                    f(&field.value);
                }
            }
            ParsedExpr::Match { subject, arms, .. } => {
                f(subject);
                for arm in arms {
                    f(&arm.body);
                }
            }
            ParsedExpr::For {
                source, key, body, ..
            } => {
                match source.as_ref() {
                    ParsedLoopSource::Array(expr) => f(expr),
                    ParsedLoopSource::RangeInclusive { start, end } => {
                        f(start);
                        f(end);
                    }
                }
                if let Some(key) = key {
                    f(key);
                }
                f(body);
            }
            ParsedExpr::CellWrite { value, .. } => f(value),
            ParsedExpr::Option { value, .. } => {
                if let Some(value) = value {
                    f(value);
                }
            }
            ParsedExpr::Let { binding, body, .. } => {
                f(&binding.value_expr);
                f(body);
            }
            ParsedExpr::Lambda { body, .. } => f(body),

            ParsedExpr::Markup { .. }
            | ParsedExpr::VariableReference { .. }
            | ParsedExpr::StringLiteral { .. }
            | ParsedExpr::BoolLiteral { .. }
            | ParsedExpr::IntLiteral { .. }
            | ParsedExpr::FloatLiteral { .. } => {}
        }
    }

    /// The markup written in this expression, in source order.
    pub fn markup(&self) -> Vec<&ParsedMarkup> {
        let mut out = Vec::new();
        self.collect_markup(&mut out);
        out
    }

    fn collect_markup<'a>(&'a self, out: &mut Vec<&'a ParsedMarkup>) {
        if let ParsedExpr::Markup { markup } = self {
            out.push(markup);
        }
        self.for_each_child(&mut |child| child.collect_markup(out));
    }

    /// Whether this expression is a constant, i.e. a value written out in full
    /// with nothing left to evaluate.
    pub fn is_constant(&self) -> bool {
        match self {
            ParsedExpr::Let { .. } => false,
            ParsedExpr::StringLiteral { .. }
            | ParsedExpr::BoolLiteral { .. }
            | ParsedExpr::IntLiteral { .. }
            | ParsedExpr::FloatLiteral { .. } => true,
            ParsedExpr::Markup { markup } => {
                matches!(markup.as_ref(), ParsedMarkup::Fragment { children, .. } if children.is_empty())
            }
            ParsedExpr::Array { elements, .. } | ParsedExpr::Tuple { elements, .. } => {
                elements.iter().all(|element| element.is_constant())
            }
            ParsedExpr::Record { fields, spread, .. } => {
                spread.is_none() && fields.iter().all(|field| field.value.is_constant())
            }
            ParsedExpr::Enum { fields, .. } => fields.iter().all(|field| field.value.is_constant()),
            ParsedExpr::Option { value, .. } => {
                value.as_ref().is_none_or(|value| value.is_constant())
            }
            ParsedExpr::VariableReference { .. }
            | ParsedExpr::FieldAccess { .. }
            | ParsedExpr::MethodCall { .. }
            | ParsedExpr::Unary { .. }
            | ParsedExpr::Binary { .. }
            | ParsedExpr::Match { .. }
            | ParsedExpr::For { .. }
            | ParsedExpr::CellWrite { .. }
            | ParsedExpr::Macro { .. }
            | ParsedExpr::Call { .. }
            | ParsedExpr::Lambda { .. } => false,
        }
    }

    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedExpr::VariableReference { range, .. }
            | ParsedExpr::FieldAccess { range, .. }
            | ParsedExpr::MethodCall { range, .. }
            | ParsedExpr::StringLiteral { range, .. }
            | ParsedExpr::BoolLiteral { range, .. }
            | ParsedExpr::IntLiteral { range, .. }
            | ParsedExpr::FloatLiteral { range, .. }
            | ParsedExpr::Array { range, .. }
            | ParsedExpr::Tuple { range, .. }
            | ParsedExpr::Record { range, .. }
            | ParsedExpr::Enum { range, .. }
            | ParsedExpr::Unary { range, .. }
            | ParsedExpr::Binary { range, .. }
            | ParsedExpr::Match { range, .. }
            | ParsedExpr::For { range, .. }
            | ParsedExpr::CellWrite { range, .. }
            | ParsedExpr::Option { range, .. }
            | ParsedExpr::Macro { range, .. }
            | ParsedExpr::Call { range, .. }
            | ParsedExpr::Let { range, .. }
            | ParsedExpr::Lambda { range, .. } => range,
            ParsedExpr::Markup { markup } => markup.range(),
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            ParsedExpr::Binary { operator, .. } => operator.precedence(),
            ParsedExpr::CellWrite { .. } => Precedence::CellWrite,
            ParsedExpr::Unary { .. }
            | ParsedExpr::IntLiteral {
                minus_range: Some(_),
                ..
            }
            | ParsedExpr::FloatLiteral {
                minus_range: Some(_),
                ..
            } => Precedence::Prefix,
            ParsedExpr::FieldAccess { .. } | ParsedExpr::MethodCall { .. } => Precedence::Postfix,
            ParsedExpr::VariableReference { .. }
            | ParsedExpr::StringLiteral { .. }
            | ParsedExpr::BoolLiteral { .. }
            | ParsedExpr::IntLiteral {
                minus_range: None, ..
            }
            | ParsedExpr::FloatLiteral {
                minus_range: None, ..
            }
            | ParsedExpr::Array { .. }
            | ParsedExpr::Tuple { .. }
            | ParsedExpr::Record { .. }
            | ParsedExpr::Enum { .. }
            | ParsedExpr::Match { .. }
            | ParsedExpr::For { .. }
            | ParsedExpr::Option { .. }
            | ParsedExpr::Macro { .. }
            | ParsedExpr::Let { .. }
            | ParsedExpr::Lambda { .. }
            | ParsedExpr::Markup { .. }
            | ParsedExpr::Call { .. } => Precedence::Atom,
        }
    }
}

impl Display for ParsedPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fields = match self {
            ParsedPattern::Wildcard { .. } => return f.write_str("_"),
            ParsedPattern::Binding { name, .. } => return f.write_str(name.as_str()),
            ParsedPattern::BoolTrue { .. } => return f.write_str("true"),
            ParsedPattern::BoolFalse { .. } => return f.write_str("false"),
            ParsedPattern::OptionNone { .. } => return f.write_str("None"),
            ParsedPattern::OptionSome { inner, .. } => return write!(f, "Some({inner})"),
            ParsedPattern::Tuple { args, .. } => return write_tuple(f, args),
            ParsedPattern::EnumVariant {
                type_name,
                variant_name,
                fields,
                ..
            } => {
                write!(f, "{}::{}", type_name.as_str(), variant_name.as_str())?;
                fields
            }
            ParsedPattern::Record {
                type_name, fields, ..
            } => {
                f.write_str(type_name.as_str())?;
                fields
            }
        };
        if fields.is_empty() {
            return Ok(());
        }
        // Field patterns: User {name: x, age: y}
        f.write_str(" {")?;
        for (i, (name, _, pattern)) in fields.iter().enumerate() {
            let sep = if i > 0 { ", " } else { "" };
            write!(f, "{sep}{}: {pattern}", name.as_str())?;
        }
        f.write_str("}")
    }
}

impl Display for ParsedLoopSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedLoopSource::Array(expr) => write!(f, "{expr}"),
            ParsedLoopSource::RangeInclusive { start, end } => write!(f, "{start}..={end}"),
        }
    }
}

/// Markup, matches, blocks and the bodies of loops and lambdas span lines,
/// every other expression is one line.
impl Display for ParsedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedExpr::VariableReference { value, .. } => f.write_str(value.as_str()),
            ParsedExpr::FieldAccess { record, field, .. } => {
                write!(f, "{record}.{}", field.as_str())
            }
            ParsedExpr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => write!(f, "{receiver}.{}({})", method.as_str(), join(args, ", ")),
            ParsedExpr::StringLiteral { value, .. } => write!(f, "\"{value}\""),
            ParsedExpr::BoolLiteral { value, .. } => write!(f, "{value}"),
            ParsedExpr::IntLiteral { value, .. } => write!(f, "{value}"),
            ParsedExpr::FloatLiteral { value, .. } => write!(f, "{value}"),
            ParsedExpr::Array { elements, .. } => write!(f, "[{}]", join(elements, ", ")),
            ParsedExpr::Tuple { elements, .. } => write_tuple(f, elements),
            ParsedExpr::Record {
                type_name,
                fields,
                spread,
                ..
            } => {
                write!(f, "{} {{", type_name.as_str())?;
                // The spread is canonicalized to first position.
                let mut sep = "";
                if let Some(subject) = spread {
                    write!(f, "...{subject}")?;
                    sep = ", ";
                }
                for field in fields {
                    write!(f, "{sep}{}: {}", field.name.as_str(), field.value)?;
                    sep = ", ";
                }
                f.write_str("}")
            }
            // Operators print as calls, so the nesting shows without
            // parentheses.
            ParsedExpr::Unary {
                operator, operand, ..
            } => {
                let name = match operator {
                    ParsedUnaryOp::LogicalNot => "not",
                    ParsedUnaryOp::Minus => "neg",
                };
                write!(f, "{name}({operand})")
            }
            ParsedExpr::Binary {
                left,
                operator,
                right,
                ..
            } => {
                let name = match operator {
                    ParsedBinaryOp::Eq => "eq",
                    ParsedBinaryOp::NotEq => "not_eq",
                    ParsedBinaryOp::LessThan => "lt",
                    ParsedBinaryOp::GreaterThan => "gt",
                    ParsedBinaryOp::LessThanOrEqual => "lt_eq",
                    ParsedBinaryOp::GreaterThanOrEqual => "gt_eq",
                    ParsedBinaryOp::LogicalAnd => "and",
                    ParsedBinaryOp::LogicalOr => "or",
                    ParsedBinaryOp::Plus => "add",
                    ParsedBinaryOp::Minus => "sub",
                    ParsedBinaryOp::Multiply => "mul",
                };
                write!(f, "{name}({left}, {right})")
            }
            ParsedExpr::Enum {
                type_name,
                variant_name,
                fields,
                ..
            } => {
                write!(f, "{}::{}", type_name.as_str(), variant_name.as_str())?;
                if !fields.is_empty() {
                    f.write_str(" {")?;
                    for (i, field) in fields.iter().enumerate() {
                        let sep = if i > 0 { ", " } else { "" };
                        write!(f, "{sep}{}: {}", field.name.as_str(), field.value)?;
                    }
                    f.write_str("}")?;
                }
                Ok(())
            }
            ParsedExpr::Match { subject, arms, .. } => {
                write!(f, "match {subject} {{")?;
                {
                    let mut lines = Indent::new(f);
                    for arm in arms {
                        write!(lines, "\n{} => {},", arm.pattern, arm.body)?;
                    }
                }
                f.write_str(if arms.is_empty() { "}" } else { "\n}" })
            }
            ParsedExpr::For {
                var_name,
                source,
                key,
                body,
                ..
            } => {
                let var = var_name.as_ref().map_or("_", |name| name.as_str());
                write!(f, "for {var} in {source}")?;
                if let Some(key) = key {
                    write!(f, " key {key}")?;
                }
                f.write_str(" {")?;
                write!(Indent::new(f), "\n{body}")?;
                f.write_str("\n}")
            }
            ParsedExpr::Lambda {
                params,
                return_type,
                body,
                ..
            } => {
                f.write_str("fn(")?;
                for (i, param) in params.iter().enumerate() {
                    let sep = if i > 0 { ", " } else { "" };
                    write!(f, "{sep}{}", param.var_name.as_str())?;
                    if let Some(var_type) = &param.var_type {
                        write!(f, ": {var_type}")?;
                    }
                }
                f.write_str(")")?;
                if let Some(return_type) = return_type {
                    write!(f, " -> {return_type}")?;
                }
                f.write_str(" {")?;
                write!(Indent::new(f), "\n{body}")?;
                f.write_str("\n}")
            }
            ParsedExpr::CellWrite { cell, value, .. } => {
                write!(f, "write({}, {value})", cell.as_str())
            }
            ParsedExpr::Let { binding, body, .. } => {
                let mutable = if binding.mutable { "mut " } else { "" };
                write!(f, "let {mutable}{}", binding.var_name.as_str())?;
                if let Some(var_type) = &binding.var_type {
                    write!(f, ": {var_type}")?;
                }
                write!(f, " = {} in {body}", binding.value_expr)
            }
            ParsedExpr::Option { value, .. } => match value {
                Some(inner) => write!(f, "Some({inner})"),
                None => f.write_str("None"),
            },
            ParsedExpr::Macro { name, args, .. } => {
                write!(f, "{}!({})", name.as_str(), join(args, ", "))
            }
            ParsedExpr::Markup { markup } => write!(f, "{markup}"),
            ParsedExpr::Call { name, args, .. } => {
                write!(f, "{}(", name.as_str())?;
                match args {
                    ParsedArguments::Positional(values) => write!(f, "{}", join(values, ", "))?,
                    ParsedArguments::Named(named) => {
                        for (i, arg) in named.iter().enumerate() {
                            let sep = if i > 0 { ", " } else { "" };
                            match arg {
                                ParsedNamedArgument::Value {
                                    name_range, value, ..
                                } => write!(f, "{sep}{}: {value}", name_range.as_str())?,
                                ParsedNamedArgument::Spread { name, .. } => {
                                    write!(f, "{sep}...{}", name.as_str())?;
                                }
                            }
                        }
                    }
                }
                f.write_str(")")
            }
        }
    }
}
