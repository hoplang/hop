use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::field_name::FieldName;
use crate::dop::var_name::VarName;
use pretty::BoxDoc;

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// The pattern being matched (must be an EnumInstantiation, e.g., `Color::Red`)
    pub pattern: SyntacticExpr,
    /// The expression to evaluate if this arm matches
    pub body: SyntacticExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
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

#[derive(Debug, Clone)]
pub enum SyntacticExpr {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        annotation: DocumentRange,
    },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        annotation: DocumentRange,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral {
        value: String,
        annotation: DocumentRange,
    },

    /// A boolean literal expression, e.g. true
    BooleanLiteral {
        value: bool,
        annotation: DocumentRange,
    },

    /// An integer literal expression, e.g. 42
    IntLiteral {
        value: i64,
        annotation: DocumentRange,
    },

    /// A float literal expression, e.g. 2.5
    FloatLiteral {
        value: f64,
        annotation: DocumentRange,
    },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<Self>,
        annotation: DocumentRange,
    },

    /// A record instantiation expression, e.g. User(name: "John", age: 30)
    RecordInstantiation {
        record_name: String,
        fields: Vec<(FieldName, Self)>,
        annotation: DocumentRange,
    },

    /// An enum instantiation expression, e.g. Color::Red
    EnumInstantiation {
        enum_name: String,
        variant_name: String,
        annotation: DocumentRange,
    },

    BinaryOp {
        left: Box<Self>,
        operator: BinaryOp,
        right: Box<Self>,
        annotation: DocumentRange,
    },

    /// Boolean negation expression for negating boolean values
    Negation {
        operand: Box<Self>,
        annotation: DocumentRange,
    },

    /// A match expression, e.g. `match color {Red => "red", Blue => "blue"}`
    Match {
        subject: Box<Self>,
        arms: Vec<MatchArm>,
        annotation: DocumentRange,
    },
}

impl SyntacticExpr {
    pub fn annotation(&self) -> &DocumentRange {
        match self {
            SyntacticExpr::Var { annotation, .. }
            | SyntacticExpr::FieldAccess { annotation, .. }
            | SyntacticExpr::StringLiteral { annotation, .. }
            | SyntacticExpr::BooleanLiteral { annotation, .. }
            | SyntacticExpr::IntLiteral { annotation, .. }
            | SyntacticExpr::FloatLiteral { annotation, .. }
            | SyntacticExpr::ArrayLiteral { annotation, .. }
            | SyntacticExpr::RecordInstantiation { annotation, .. }
            | SyntacticExpr::EnumInstantiation { annotation, .. }
            | SyntacticExpr::BinaryOp { annotation, .. }
            | SyntacticExpr::Negation { annotation, .. }
            | SyntacticExpr::Match { annotation, .. } => annotation,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            SyntacticExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            SyntacticExpr::FieldAccess {
                record: object,
                field,
                ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            SyntacticExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            SyntacticExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            SyntacticExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            SyntacticExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            SyntacticExpr::ArrayLiteral { elements, .. } => {
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
            SyntacticExpr::RecordInstantiation {
                record_name,
                fields,
                ..
            } => {
                if fields.is_empty() {
                    BoxDoc::text(record_name.as_str()).append(BoxDoc::text("()"))
                } else {
                    BoxDoc::text(record_name.as_str())
                        .append(BoxDoc::text("("))
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
                        .append(BoxDoc::text(")"))
                }
            }
            SyntacticExpr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(format!(" {} ", operator)))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            SyntacticExpr::Negation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            SyntacticExpr::EnumInstantiation {
                enum_name,
                variant_name,
                ..
            } => BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
            SyntacticExpr::Match { subject, arms, .. } => {
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
        }
    }
}

impl Ranged for SyntacticExpr {
    fn range(&self) -> &DocumentRange {
        self.annotation()
    }
}

impl Display for SyntacticExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LessThanOrEqual => write!(f, "<="),
            BinaryOp::GreaterThanOrEqual => write!(f, ">="),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
        }
    }
}
