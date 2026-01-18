use std::fmt::{self, Display};

use crate::document::{CheapString, DocumentRange, Ranged};
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
        name: CheapString,
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
            ParsedType::Named { name, .. } => BoxDoc::text(name.to_string()),
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
        variant_name: CheapString,
    },
    /// A record pattern, e.g. `User(name: x, age: y)`
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

/// A pattern in a match arm
#[derive(Debug, Clone)]
pub enum ParsedMatchPattern {
    /// A constructor pattern that matches a specific value
    Constructor {
        constructor: Constructor,
        /// Positional arguments (e.g., the inner pattern in `Some(x)`)
        args: Vec<ParsedMatchPattern>,
        /// Named field patterns for record matching (e.g., `User(name: x, age: y)`)
        /// The tuple is (field_name, field_name_range, field_pattern)
        fields: Vec<(FieldName, DocumentRange, ParsedMatchPattern)>,
        /// Range of just the constructor (e.g., `Point::XY` without the field patterns)
        constructor_range: DocumentRange,
        /// Range of the entire pattern including fields
        range: DocumentRange,
    },
    /// A wildcard pattern that matches anything, written as `_`
    Wildcard { range: DocumentRange },
    /// A binding pattern that matches anything and binds it to a name
    Binding {
        name: CheapString,
        range: DocumentRange,
    },
}

impl Ranged for ParsedMatchPattern {
    fn range(&self) -> &DocumentRange {
        match self {
            ParsedMatchPattern::Constructor { range, .. }
            | ParsedMatchPattern::Wildcard { range }
            | ParsedMatchPattern::Binding { range, .. } => range,
        }
    }
}

impl ParsedMatchPattern {
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
                    // Record pattern: User(name: x, age: y)
                    let fields_doc = BoxDoc::intersperse(
                        fields.iter().map(|(name, _, pat)| {
                            BoxDoc::text(name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(pat.to_doc())
                        }),
                        BoxDoc::text(", "),
                    );
                    base.append(BoxDoc::text("("))
                        .append(fields_doc)
                        .append(BoxDoc::text(")"))
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

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone)]
pub struct ParsedMatchArm {
    /// The pattern being matched
    pub pattern: ParsedMatchPattern,
    /// The expression to evaluate if this arm matches
    pub body: ParsedExpr,
}

/// The source of iteration in a for loop - either an array or an inclusive range.
#[derive(Debug, Clone)]
pub enum ParsedLoopSource {
    /// Iterate over elements of an array, e.g. `item in items`
    Array(ParsedExpr),
    /// Iterate over an inclusive integer range, e.g. `i in 0..=5`
    RangeInclusive { start: ParsedExpr, end: ParsedExpr },
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

    /// A method call expression, e.g. foo.bar()
    MethodCall {
        receiver: Box<Self>,
        method: FieldName,
        range: DocumentRange,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral {
        value: CheapString,
        range: DocumentRange,
    },

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
        record_name: CheapString,
        fields: Vec<(FieldName, Self)>,
        range: DocumentRange,
    },

    /// An enum literal expression, e.g. Color::Red
    EnumLiteral {
        enum_name: CheapString,
        variant_name: CheapString,
        /// Field values for variants with fields (empty for unit variants)
        /// The tuple is (field_name, field_name_range, field_expr)
        fields: Vec<(FieldName, DocumentRange, Self)>,
        /// Range of just the constructor (e.g., `Point::XY` without the field values)
        constructor_range: DocumentRange,
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

    /// Numeric negation expression for negating numeric values
    NumericNegation {
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

    /// A macro invocation, e.g. `join!(a, b, c)`
    MacroInvocation {
        name: CheapString,
        args: Vec<Self>,
        range: DocumentRange,
    },
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
            | ParsedExpr::Negation { range, .. }
            | ParsedExpr::NumericNegation { range, .. }
            | ParsedExpr::Match { range, .. }
            | ParsedExpr::OptionLiteral { range, .. }
            | ParsedExpr::MacroInvocation { range, .. } => range,
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
            ParsedExpr::Negation { .. } | ParsedExpr::NumericNegation { .. } => 7,
            ParsedExpr::FieldAccess { .. } | ParsedExpr::MethodCall { .. } => 10,
            _ => u8::MAX,
        }
    }

    /// Returns true if this expression is "atomic" (never needs parentheses).
    fn is_atomic(&self) -> bool {
        matches!(
            self,
            ParsedExpr::Var { .. }
                | ParsedExpr::FieldAccess { .. }
                | ParsedExpr::MethodCall { .. }
                | ParsedExpr::StringLiteral { .. }
                | ParsedExpr::BooleanLiteral { .. }
                | ParsedExpr::IntLiteral { .. }
                | ParsedExpr::FloatLiteral { .. }
                | ParsedExpr::ArrayLiteral { .. }
                | ParsedExpr::RecordLiteral { .. }
                | ParsedExpr::EnumLiteral { .. }
                | ParsedExpr::Match { .. }
                | ParsedExpr::OptionLiteral { .. }
                | ParsedExpr::MacroInvocation { .. }
        )
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
                let prec = self.precedence();
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
            ParsedExpr::NumericNegation { operand, .. } => {
                if operand.is_atomic() {
                    BoxDoc::text("-").append(operand.to_doc())
                } else {
                    BoxDoc::text("-(")
                        .append(operand.to_doc())
                        .append(BoxDoc::text(")"))
                }
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
                    base.append(BoxDoc::text("("))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(field_name, _, field_value)| {
                                BoxDoc::text(field_name.to_string())
                                    .append(BoxDoc::text(": "))
                                    .append(field_value.to_doc())
                            }),
                            BoxDoc::text(", "),
                        ))
                        .append(BoxDoc::text(")"))
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
        f.write_str(self.as_str())
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
    /// An enum declaration: `enum Name {Variant1, Variant2(field: Type), ...}`
    Enum {
        /// The name of the enum type.
        name: TypeName,
        /// The range of the enum name in the source.
        name_range: DocumentRange,
        /// The variants of the enum (name, range, fields).
        /// Each variant can optionally have named fields like records.
        variants: Vec<(
            TypeName,
            DocumentRange,
            Vec<(FieldName, DocumentRange, ParsedType)>,
        )>,
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
                            variants.iter().map(|(variant_name, _, fields)| {
                                if fields.is_empty() {
                                    BoxDoc::text(variant_name.to_string())
                                } else {
                                    BoxDoc::text(variant_name.to_string())
                                        .append(BoxDoc::text("("))
                                        .append(BoxDoc::intersperse(
                                            fields.iter().map(|(field_name, _, field_type)| {
                                                BoxDoc::text(field_name.to_string())
                                                    .append(BoxDoc::text(": "))
                                                    .append(field_type.to_doc())
                                            }),
                                            BoxDoc::text(", "),
                                        ))
                                        .append(BoxDoc::text(")"))
                                }
                            }),
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
        writeln!(f, "{}", self.to_doc().pretty(80))
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
