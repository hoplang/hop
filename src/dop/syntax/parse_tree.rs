use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::symbols::var_name::VarName;
use crate::hop::symbols::module_name::ModuleName;
use pretty::BoxDoc;

/// A syntax representation of a type, preserving document ranges.
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

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// The pattern being matched (must be an enum literal, e.g., `Color::Red`)
    pub pattern: ParseTree,
    /// The expression to evaluate if this arm matches
    pub body: ParseTree,
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
pub enum ParseTree {
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

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: String,
        fields: Vec<(FieldName, Self)>,
        annotation: DocumentRange,
    },

    /// An enum literal expression, e.g. Color::Red
    EnumLiteral {
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

impl ParseTree {
    pub fn annotation(&self) -> &DocumentRange {
        match self {
            ParseTree::Var { annotation, .. }
            | ParseTree::FieldAccess { annotation, .. }
            | ParseTree::StringLiteral { annotation, .. }
            | ParseTree::BooleanLiteral { annotation, .. }
            | ParseTree::IntLiteral { annotation, .. }
            | ParseTree::FloatLiteral { annotation, .. }
            | ParseTree::ArrayLiteral { annotation, .. }
            | ParseTree::RecordLiteral { annotation, .. }
            | ParseTree::EnumLiteral { annotation, .. }
            | ParseTree::BinaryOp { annotation, .. }
            | ParseTree::Negation { annotation, .. }
            | ParseTree::Match { annotation, .. } => annotation,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParseTree::Var { value, .. } => BoxDoc::text(value.as_str()),
            ParseTree::FieldAccess {
                record: object,
                field,
                ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            ParseTree::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            ParseTree::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            ParseTree::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            ParseTree::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            ParseTree::ArrayLiteral { elements, .. } => {
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
            ParseTree::RecordLiteral {
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
            ParseTree::BinaryOp {
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
            ParseTree::Negation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            ParseTree::EnumLiteral {
                enum_name,
                variant_name,
                ..
            } => BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
            ParseTree::Match { subject, arms, .. } => {
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

impl Ranged for ParseTree {
    fn range(&self) -> &DocumentRange {
        self.annotation()
    }
}

impl Display for ParseTree {
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


/// A RecordDeclarationField represents a field in a record declaration.
/// E.g. record Foo {bar: String, baz: Int}
///                  ^^^^^^^^^^^
#[derive(Debug, Clone)]
pub struct RecordDeclarationField<T = ParsedType> {
    pub name: FieldName,
    pub name_range: DocumentRange,
    pub field_type: T,
}

impl Display for RecordDeclarationField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.field_type)
    }
}

/// A RecordDeclaration represents a full record type declaration.
/// E.g. record User {name: String, age: Int}
#[derive(Debug, Clone)]
pub struct RecordDeclaration<A = ParsedType> {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub fields: Vec<RecordDeclarationField<A>>,
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

/// A declaration: either an import, a record, or an enum definition.
#[derive(Clone)]
pub enum Declaration {
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
        /// The fully parsed record declaration.
        declaration: RecordDeclaration,
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

impl Declaration {
    /// Convert this declaration to a pretty-printable document.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Declaration::Import {
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
            Declaration::Record { declaration, .. } => {
                let fields_doc = if declaration.fields.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            declaration.fields.iter().map(|f| {
                                BoxDoc::text(f.name.to_string())
                                    .append(BoxDoc::text(": "))
                                    .append(BoxDoc::text(f.field_type.to_string()))
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
                            .append(BoxDoc::text(declaration.name.as_str()))
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
            Declaration::Enum { name, variants, .. } => {
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

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

impl fmt::Debug for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Ranged for Declaration {
    fn range(&self) -> &DocumentRange {
        match self {
            Declaration::Import { range, .. }
            | Declaration::Record { range, .. }
            | Declaration::Enum { range, .. } => range,
        }
    }
}
