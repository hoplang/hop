use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::symbols::var_name::VarName;
use crate::hop::symbols::module_name::ModuleName;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub enum ParsedType {
    String {
        range: DocumentRange,
    },
    Bool {
        range: DocumentRange,
    },
    Int {
        range: DocumentRange,
    },
    Float {
        range: DocumentRange,
    },
    TrustedHTML {
        range: DocumentRange,
    },
    Array {
        element: Box<ParsedType>,
        range: DocumentRange,
    },
    Option {
        element: Box<ParsedType>,
        range: DocumentRange,
    },
    Named {
        name: String,
        range: DocumentRange,
    },
}

impl Ranged for ParsedType {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedType::String { range }
            | ParsedType::Bool { range }
            | ParsedType::Int { range }
            | ParsedType::Float { range }
            | ParsedType::TrustedHTML { range }
            | ParsedType::Array { range, .. }
            | ParsedType::Option { range, .. }
            | ParsedType::Named { range, .. } => range,
        }
    }
}

impl ParsedType {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedType::String { .. } => BoxDoc::text("String"),
            ParsedType::Bool { .. } => BoxDoc::text("Bool"),
            ParsedType::Int { .. } => BoxDoc::text("Int"),
            ParsedType::Float { .. } => BoxDoc::text("Float"),
            ParsedType::TrustedHTML { .. } => BoxDoc::text("TrustedHTML"),
            ParsedType::Option { element, .. } => BoxDoc::nil()
                .append(BoxDoc::text("Option["))
                .append(element.to_doc())
                .append(BoxDoc::text("]")),
            ParsedType::Array { element, .. } => BoxDoc::nil()
                .append(BoxDoc::text("Array["))
                .append(element.to_doc())
                .append(BoxDoc::text("]")),
            ParsedType::Named { name, .. } => BoxDoc::text(name.clone()),
        }
    }
}

impl Display for ParsedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
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
        variant_name: String,
    },
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
            Constructor::OptionSome => BoxDoc::text("Some(_)"),
            Constructor::OptionNone => BoxDoc::text("None"),
        }
    }
}

impl std::fmt::Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

/// A pattern in a match arm
#[derive(Debug, Clone)]
pub enum ParsedMatchPattern {
    /// A constructor pattern that matches a specific value
    Constructor {
        constructor: Constructor,
        range: DocumentRange,
    },
    /// A wildcard pattern that matches anything, written as `_`
    Wildcard { range: DocumentRange },
}

impl Ranged for ParsedMatchPattern {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedMatchPattern::Constructor { range, .. }
            | ParsedMatchPattern::Wildcard { range } => range,
        }
    }
}

impl ParsedMatchPattern {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedMatchPattern::Constructor { constructor, .. } => constructor.to_doc(),
            ParsedMatchPattern::Wildcard { .. } => BoxDoc::text("_"),
        }
    }
}

impl std::fmt::Display for ParsedMatchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone)]
pub struct ParsedMatchArm {
    /// The pattern being matched
    pub pattern: ParsedMatchPattern,
    /// The expression to evaluate if this arm matches
    pub body: ParsedExpr,
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

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        range: DocumentRange,
    },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        range: DocumentRange,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: String, range: DocumentRange },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, range: DocumentRange },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i64, range: DocumentRange },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64, range: DocumentRange },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<Self>,
        range: DocumentRange,
    },

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: String,
        fields: Vec<(FieldName, Self)>,
        range: DocumentRange,
    },

    /// An enum literal expression, e.g. Color::Red
    EnumLiteral {
        enum_name: String,
        variant_name: String,
        range: DocumentRange,
    },

    BinaryOp {
        left: Box<Self>,
        operator: ParsedBinaryOp,
        right: Box<Self>,
        range: DocumentRange,
    },

    /// Boolean negation expression for negating boolean values
    Negation {
        operand: Box<Self>,
        range: DocumentRange,
    },

    /// A match expression, e.g. `match color {Red => "red", Blue => "blue"}`
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
}

impl ParsedBinaryOp {
    /// Returns the precedence of this operator (higher = binds tighter).
    fn precedence(&self) -> u8 {
        match self {
            ParsedBinaryOp::LogicalOr => 1,
            ParsedBinaryOp::LogicalAnd => 2,
            ParsedBinaryOp::Eq | ParsedBinaryOp::NotEq => 3,
            ParsedBinaryOp::LessThan
            | ParsedBinaryOp::GreaterThan
            | ParsedBinaryOp::LessThanOrEqual
            | ParsedBinaryOp::GreaterThanOrEqual => 4,
            ParsedBinaryOp::Plus | ParsedBinaryOp::Minus => 5,
            ParsedBinaryOp::Multiply => 6,
        }
    }
}

impl ParsedExpr {
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedExpr::Var { range, .. }
            | ParsedExpr::FieldAccess { range, .. }
            | ParsedExpr::StringLiteral { range, .. }
            | ParsedExpr::BooleanLiteral { range, .. }
            | ParsedExpr::IntLiteral { range, .. }
            | ParsedExpr::FloatLiteral { range, .. }
            | ParsedExpr::ArrayLiteral { range, .. }
            | ParsedExpr::RecordLiteral { range, .. }
            | ParsedExpr::EnumLiteral { range, .. }
            | ParsedExpr::BinaryOp { range, .. }
            | ParsedExpr::Negation { range, .. }
            | ParsedExpr::Match { range, .. }
            | ParsedExpr::OptionLiteral { range, .. } => range,
        }
    }

    /// Returns true if this expression is "atomic" (never needs parentheses).
    fn is_atomic(&self) -> bool {
        matches!(
            self,
            ParsedExpr::Var { .. }
                | ParsedExpr::FieldAccess { .. }
                | ParsedExpr::StringLiteral { .. }
                | ParsedExpr::BooleanLiteral { .. }
                | ParsedExpr::IntLiteral { .. }
                | ParsedExpr::FloatLiteral { .. }
                | ParsedExpr::ArrayLiteral { .. }
                | ParsedExpr::RecordLiteral { .. }
                | ParsedExpr::EnumLiteral { .. }
                | ParsedExpr::Match { .. }
                | ParsedExpr::OptionLiteral { .. }
        )
    }

    /// Converts this expression to a doc, adding parentheses if needed based on parent precedence.
    fn to_doc_with_precedence(&self, parent_precedence: u8) -> BoxDoc<'_> {
        match self {
            ParsedExpr::BinaryOp { operator, .. } => {
                let needs_parens = operator.precedence() < parent_precedence;
                if needs_parens {
                    BoxDoc::text("(")
                        .append(self.to_doc())
                        .append(BoxDoc::text(")"))
                } else {
                    self.to_doc()
                }
            }
            _ => self.to_doc(),
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
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
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
            ParsedExpr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => {
                let prec = operator.precedence();
                left.to_doc_with_precedence(prec)
                    .append(BoxDoc::text(format!(" {} ", operator)))
                    .append(right.to_doc_with_precedence(prec))
            }
            ParsedExpr::Negation { operand, .. } => {
                if operand.is_atomic() {
                    BoxDoc::text("!").append(operand.to_doc())
                } else {
                    BoxDoc::text("!(")
                        .append(operand.to_doc())
                        .append(BoxDoc::text(")"))
                }
            }
            ParsedExpr::EnumLiteral {
                enum_name,
                variant_name,
                ..
            } => BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
            ParsedExpr::Match { subject, arms, .. } => {
                if arms.is_empty() {
                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {}"))
                } else {
                    // Pre-compute max pattern width for arrow alignment (multi-line only)
                    let max_pattern_width = arms
                        .iter()
                        .map(|arm| arm.pattern.to_doc().pretty(1000).to_string().len())
                        .max()
                        .unwrap_or(0);

                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    arms.iter().map(|arm| {
                                        let pattern_str =
                                            arm.pattern.to_doc().pretty(1000).to_string();
                                        let padding =
                                            " ".repeat(max_pattern_width - pattern_str.len());
                                        // Use flat_alt: no padding on single line, padding on multi-line
                                        BoxDoc::text(pattern_str.clone())
                                            .append(BoxDoc::text(padding).flat_alt(BoxDoc::nil()))
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
        }
    }
}

impl Ranged for ParsedExpr {
    fn range(&self) -> &DocumentRange {
        self.range()
    }
}

impl Display for ParsedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for ParsedBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedBinaryOp::Eq => write!(f, "=="),
            ParsedBinaryOp::NotEq => write!(f, "!="),
            ParsedBinaryOp::LessThan => write!(f, "<"),
            ParsedBinaryOp::GreaterThan => write!(f, ">"),
            ParsedBinaryOp::LessThanOrEqual => write!(f, "<="),
            ParsedBinaryOp::GreaterThanOrEqual => write!(f, ">="),
            ParsedBinaryOp::LogicalAnd => write!(f, "&&"),
            ParsedBinaryOp::LogicalOr => write!(f, "||"),
            ParsedBinaryOp::Plus => write!(f, "+"),
            ParsedBinaryOp::Minus => write!(f, "-"),
            ParsedBinaryOp::Multiply => write!(f, "*"),
        }
    }
}

/// A declaration: either an import, a record, or an enum.
#[derive(Clone)]
pub enum ParsedDeclaration {
    /// An import declaration: `import module::path::Name`
    Import {
        /// The name of the imported type.
        name: TypeName,
        /// The range of the type name in the source (for error reporting).
        name_range: DocumentRange,
        /// The full path range (module::path::Name) for error reporting.
        path: DocumentRange,
        /// The parsed module name.
        module_name: ModuleName,
        /// The full range of the declaration.
        range: DocumentRange,
    },
    /// A record declaration: `record Name {fields...}`
    Record {
        /// The name of the record type.
        name: TypeName,
        /// The range of the record name in the source.
        name_range: DocumentRange,
        /// The fields of the record (name, name_range, type).
        fields: Vec<(FieldName, DocumentRange, ParsedType)>,
        /// The full range of the declaration.
        range: DocumentRange,
    },
    /// An enum declaration: `enum Name {Variant1, Variant2, ...}`
    Enum {
        /// The name of the enum type.
        name: TypeName,
        /// The range of the enum name in the source.
        name_range: DocumentRange,
        /// The variants of the enum (name, range).
        variants: Vec<(TypeName, DocumentRange)>,
        /// The full range of the declaration.
        range: DocumentRange,
    },
}

impl ParsedDeclaration {
    /// Convert this declaration to a pretty-printable document.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedDeclaration::Import {
                name, module_name, ..
            } => BoxDoc::text("Import")
                .append(BoxDoc::space())
                .append(BoxDoc::text("{"))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::text("name: "))
                        .append(BoxDoc::text(name.to_string()))
                        .append(BoxDoc::text(","))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("module_name: "))
                        .append(BoxDoc::text(module_name.to_string()))
                        .append(BoxDoc::text(","))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            ParsedDeclaration::Record { name, fields, .. } => {
                let fields_doc = if fields.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(field_name, _, field_type)| {
                                BoxDoc::text(field_name.to_string())
                                    .append(BoxDoc::text(": "))
                                    .append(BoxDoc::text(field_type.to_string()))
                            }),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(","))
                        .nest(2)
                        .append(BoxDoc::line())
                };

                BoxDoc::text("Record")
                    .append(BoxDoc::space())
                    .append(BoxDoc::text("{"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("name: "))
                            .append(BoxDoc::text(name.as_str()))
                            .append(BoxDoc::text(","))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("fields: {"))
                            .append(fields_doc)
                            .append(BoxDoc::text("},"))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            ParsedDeclaration::Enum { name, variants, .. } => {
                let variants_doc = if variants.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            variants
                                .iter()
                                .map(|(name, _)| BoxDoc::text(name.to_string())),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(","))
                        .nest(2)
                        .append(BoxDoc::line())
                };

                BoxDoc::text("Enum")
                    .append(BoxDoc::space())
                    .append(BoxDoc::text("{"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("name: "))
                            .append(BoxDoc::text(name.as_str()))
                            .append(BoxDoc::text(","))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("variants: {"))
                            .append(variants_doc)
                            .append(BoxDoc::text("},"))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
        }
    }
}

impl fmt::Display for ParsedDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

impl fmt::Debug for ParsedDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Ranged for ParsedDeclaration {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedDeclaration::Import { range, .. }
            | ParsedDeclaration::Record { range, .. }
            | ParsedDeclaration::Enum { range, .. } => range,
        }
    }
}
