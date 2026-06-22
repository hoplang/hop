use std::fmt::{self, Display};

use crate::document::{CheapString, DocumentRange};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    /// A variable name expression, e.g. `foo`
    Var {
        value: VarName,
        range: DocumentRange,
    },

    /// A field access expression, e.g. `foo.bar`
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        range: DocumentRange,
    },

    /// A method call expression, e.g. `foo.bar()`
    MethodCall {
        receiver: Box<Self>,
        method: FieldName,
        method_range: DocumentRange,
        range: DocumentRange,
    },

    /// A string literal expression, e.g. `"foo bar"`
    StringLiteral {
        value: CheapString,
        range: DocumentRange,
    },

    /// A boolean literal expression, e.g. `true`
    BooleanLiteral { value: bool, range: DocumentRange },

    /// An integer literal expression, e.g. `42`
    IntLiteral { value: i64, range: DocumentRange },

    /// A float literal expression, e.g. `2.5`
    FloatLiteral { value: f64, range: DocumentRange },

    /// An array literal expression, e.g. `[1, 2, 3]`
    ArrayLiteral {
        elements: Vec<Self>,
        range: DocumentRange,
    },

    /// A record literal expression, e.g. `User {name: "John", age: 30}`
    RecordLiteral {
        record_name: TypeName,
        record_name_range: DocumentRange,
        fields: Vec<(FieldName, Self)>,
        range: DocumentRange,
    },

    /// An enum literal expression, e.g. `Color::Red`
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        /// The tuple is (field_name, field_name_range, field_expr)
        fields: Vec<(FieldName, DocumentRange, Self)>,
        /// Range of just the constructor (e.g., `Point::XY` without the field values)
        constructor_range: DocumentRange,
        /// Range of just the enum name (e.g., `Point` in `Point::XY`)
        enum_name_range: DocumentRange,
        range: DocumentRange,
    },

    /// A binary operation expression
    BinaryOp {
        left: Box<Self>,
        operator: ParsedBinaryOp,
        right: Box<Self>,
        range: DocumentRange,
    },

    /// Boolean negation expression
    BooleanNegation {
        operand: Box<Self>,
        range: DocumentRange,
    },

    /// Numeric negation expression
    NumericNegation {
        operand: Box<Self>,
        range: DocumentRange,
    },

    /// A match expression, e.g. `match color {Color::Red => "red", Color::Blue => "blue"}`
    Match {
        subject: Box<Self>,
        arms: Vec<ParsedMatchArm>,
        range: DocumentRange,
    },

    /// An option literal expression, e.g. `Some(42)` or `None`
    OptionLiteral {
        value: Option<Box<Self>>,
        range: DocumentRange,
    },

    /// A macro invocation, e.g. `join!(a, b, c)`
    MacroInvocation {
        /// The name of the macro, e.g. `join`.
        name: CheapString,
        /// The range of the function subject, e.g. `join!`.
        subject_range: DocumentRange,
        args: Vec<Self>,
        range: DocumentRange,
    },

    /// An empty Fragment literal, e.g. `Fragment::empty()`
    FragmentEmpty { range: DocumentRange },
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

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone)]
pub struct ParsedMatchArm {
    /// The pattern being matched
    pub pattern: ParsedMatchPattern,
    /// The expression to evaluate if this arm matches
    pub body: ParsedExpr,
}

/// A pattern in a match arm
#[derive(Debug, Clone)]
pub enum ParsedMatchPattern {
    /// A constructor pattern that matches a specific value
    Constructor {
        constructor: Constructor,
        /// Positional arguments (e.g., the inner pattern in `Some(x)`)
        args: Vec<ParsedMatchPattern>,
        /// Named field patterns for record matching (e.g., `User {name: x, age: y}`)
        /// The tuple is (field_name, field_name_range, field_pattern)
        fields: Vec<(FieldName, DocumentRange, ParsedMatchPattern)>,
        /// Range of just the constructor (e.g., `Point::XY` without the field patterns)
        constructor_range: DocumentRange,
        /// Range of just the enum name for enum variant patterns (e.g., `Device` in `Device::Mobile`)
        enum_name_range: Option<DocumentRange>,
        /// Range of the entire pattern including fields
        range: DocumentRange,
    },
    /// A wildcard pattern that matches anything, written as `_`
    Wildcard { range: DocumentRange },
    /// A binding pattern that matches anything and binds it to a name
    Binding { name: VarName, range: DocumentRange },
}

/// A constructor pattern (non-wildcard pattern that matches a specific value)
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    /// A boolean true pattern
    BooleanTrue,
    /// A boolean false pattern
    BooleanFalse,
    /// An Option Some pattern, e.g. `Some(_)`
    OptionSome,
    /// An Option None pattern, e.g. `None`
    OptionNone,
    /// An enum variant pattern, e.g. `Color::Red`
    EnumVariant {
        enum_name: TypeName,
        variant_name: TypeName,
    },
    /// A record pattern, e.g. `User {name: x, age: y}`
    Record { type_name: TypeName },
}

impl Constructor {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Constructor::EnumVariant {
                enum_name,
                variant_name,
            } => BoxDoc::text(enum_name.as_str().to_string())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
            Constructor::BooleanTrue => BoxDoc::text("true"),
            Constructor::BooleanFalse => BoxDoc::text("false"),
            Constructor::OptionSome => BoxDoc::text("Some"),
            Constructor::OptionNone => BoxDoc::text("None"),
            Constructor::Record { type_name } => BoxDoc::text(type_name.as_str().to_string()),
        }
    }
}

impl std::fmt::Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

impl ParsedMatchPattern {
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedMatchPattern::Constructor { range, .. }
            | ParsedMatchPattern::Wildcard { range }
            | ParsedMatchPattern::Binding { range, .. } => range,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedMatchPattern::Constructor {
                constructor,
                args,
                fields,
                ..
            } => {
                let base = constructor.to_doc();
                if !fields.is_empty() {
                    // Record pattern: User {name: x, age: y}
                    let fields_doc = BoxDoc::intersperse(
                        fields.iter().map(|(name, _, pat)| {
                            if let ParsedMatchPattern::Binding { name: var_name, .. } = pat {
                                if var_name.as_str() == name.as_str() {
                                    return BoxDoc::text(name.as_str());
                                }
                            }
                            BoxDoc::text(name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(pat.to_doc())
                        }),
                        BoxDoc::text(", "),
                    );
                    base.append(BoxDoc::text("{"))
                        .append(fields_doc)
                        .append(BoxDoc::text("}"))
                } else if args.is_empty() {
                    base
                } else {
                    // Positional args (Option Some, etc.)
                    let args_doc =
                        BoxDoc::intersperse(args.iter().map(|a| a.to_doc()), BoxDoc::text(", "));
                    base.append(BoxDoc::text("("))
                        .append(args_doc)
                        .append(BoxDoc::text(")"))
                }
            }
            ParsedMatchPattern::Wildcard { .. } => BoxDoc::text("_"),
            ParsedMatchPattern::Binding { name, .. } => BoxDoc::text(name.as_str()),
        }
    }
}

impl std::fmt::Display for ParsedMatchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
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
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedExpr::Var { range, .. }
            | ParsedExpr::FieldAccess { range, .. }
            | ParsedExpr::MethodCall { range, .. }
            | ParsedExpr::StringLiteral { range, .. }
            | ParsedExpr::BooleanLiteral { range, .. }
            | ParsedExpr::IntLiteral { range, .. }
            | ParsedExpr::FloatLiteral { range, .. }
            | ParsedExpr::ArrayLiteral { range, .. }
            | ParsedExpr::RecordLiteral { range, .. }
            | ParsedExpr::EnumLiteral { range, .. }
            | ParsedExpr::BinaryOp { range, .. }
            | ParsedExpr::BooleanNegation { range, .. }
            | ParsedExpr::NumericNegation { range, .. }
            | ParsedExpr::Match { range, .. }
            | ParsedExpr::OptionLiteral { range, .. }
            | ParsedExpr::MacroInvocation { range, .. }
            | ParsedExpr::FragmentEmpty { range, .. } => range,
        }
    }

    /// Returns the binding strength of this expression (higher = binds tighter).
    /// Atomic expressions return u8::MAX, meaning they never need parentheses.
    pub fn precedence(&self) -> u8 {
        match self {
            ParsedExpr::BinaryOp { operator, .. } => match operator {
                ParsedBinaryOp::LogicalOr => 1,
                ParsedBinaryOp::LogicalAnd => 2,
                ParsedBinaryOp::Eq | ParsedBinaryOp::NotEq => 3,
                ParsedBinaryOp::LessThan
                | ParsedBinaryOp::GreaterThan
                | ParsedBinaryOp::LessThanOrEqual
                | ParsedBinaryOp::GreaterThanOrEqual => 4,
                ParsedBinaryOp::Plus | ParsedBinaryOp::Minus => 5,
                ParsedBinaryOp::Multiply => 6,
            },
            ParsedExpr::BooleanNegation { .. } | ParsedExpr::NumericNegation { .. } => 7,
            ParsedExpr::FieldAccess { .. } | ParsedExpr::MethodCall { .. } => 10,
            _ => u8::MAX,
        }
    }

    /// Converts this expression to a doc, adding parentheses if needed based on parent precedence.
    fn to_doc_with_precedence(&self, parent_precedence: u8) -> BoxDoc<'_> {
        if self.precedence() < parent_precedence {
            BoxDoc::text("(")
                .append(self.to_doc())
                .append(BoxDoc::text(")"))
        } else {
            self.to_doc()
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            ParsedExpr::FieldAccess {
                record: object,
                field,
                ..
            } => object
                .to_doc_with_precedence(self.precedence())
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            ParsedExpr::MethodCall {
                receiver, method, ..
            } => receiver
                .to_doc_with_precedence(self.precedence())
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(method.as_str()))
                .append(BoxDoc::text("()")),
            ParsedExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            ParsedExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            ParsedExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            ParsedExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            ParsedExpr::ArrayLiteral { elements, .. } => {
                if elements.is_empty() {
                    BoxDoc::text("[]")
                } else {
                    BoxDoc::text("[")
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    elements.iter().map(|e| e.to_doc()),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("]"))
                }
            }
            ParsedExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => {
                if fields.is_empty() {
                    BoxDoc::text(record_name.as_str()).append(BoxDoc::text(" {}"))
                } else {
                    BoxDoc::text(record_name.as_str())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    fields.iter().map(|(key, value)| {
                                        BoxDoc::text(key.as_str())
                                            .append(BoxDoc::text(": "))
                                            .append(value.to_doc())
                                    }),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
            }
            ParsedExpr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => {
                let prec = self.precedence();
                left.to_doc_with_precedence(prec)
                    .append(BoxDoc::text(format!(" {} ", operator)))
                    .append(right.to_doc_with_precedence(prec))
            }
            ParsedExpr::BooleanNegation { operand, .. } => {
                BoxDoc::text("!").append(operand.to_doc_with_precedence(self.precedence()))
            }
            ParsedExpr::NumericNegation { operand, .. } => {
                BoxDoc::text("-").append(operand.to_doc_with_precedence(self.precedence()))
            }
            ParsedExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                ..
            } => {
                let base = BoxDoc::text(enum_name.as_str())
                    .append(BoxDoc::text("::"))
                    .append(BoxDoc::text(variant_name.as_str()));
                if fields.is_empty() {
                    base
                } else {
                    base.append(BoxDoc::text(" {"))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(field_name, _, field_value)| {
                                BoxDoc::text(field_name.to_string())
                                    .append(BoxDoc::text(": "))
                                    .append(field_value.to_doc())
                            }),
                            BoxDoc::text(", "),
                        ))
                        .append(BoxDoc::text("}"))
                }
            }
            ParsedExpr::Match { subject, arms, .. } => {
                if arms.is_empty() {
                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {}"))
                } else {
                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    arms.iter().map(|arm| {
                                        arm.pattern
                                            .to_doc()
                                            .append(BoxDoc::text(" => "))
                                            .append(arm.body.to_doc())
                                    }),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
            }
            ParsedExpr::OptionLiteral { value, .. } => match value {
                Some(inner) => BoxDoc::text("Some(")
                    .append(inner.to_doc())
                    .append(BoxDoc::text(")")),
                None => BoxDoc::text("None"),
            },
            ParsedExpr::MacroInvocation { name, args, .. } => {
                if args.is_empty() {
                    BoxDoc::text(name.as_str()).append(BoxDoc::text("!()"))
                } else {
                    BoxDoc::text(name.as_str())
                        .append(BoxDoc::text("!("))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    args.iter().map(|e| e.to_doc()),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text(")"))
                }
            }
            ParsedExpr::FragmentEmpty { .. } => BoxDoc::text("Fragment::empty()"),
        }
    }
}

impl Display for ParsedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for ParsedBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
