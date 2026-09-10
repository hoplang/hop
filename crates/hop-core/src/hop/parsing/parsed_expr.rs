use std::fmt::{self, Display};

use super::parsed_node::ParsedNode;
use crate::document::{CheapString, DocumentRange};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

use pretty::BoxDoc;

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
        range: DocumentRange,
    },

    StringLiteral {
        value: CheapString,
        range: DocumentRange,
    },

    BooleanLiteral {
        value: bool,
        range: DocumentRange,
    },

    IntLiteral {
        value: i32,
        range: DocumentRange,
    },

    FloatLiteral {
        value: f64,
        range: DocumentRange,
    },

    ArrayLiteral {
        elements: Vec<Self>,
        range: DocumentRange,
    },

    RecordLiteral {
        record_name: TypeName,
        record_name_range: DocumentRange,
        fields: Vec<ParsedFieldInitializer>,
        spread: Option<Box<Self>>,
        range: DocumentRange,
    },

    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<ParsedFieldInitializer>,
        /// Range of just the constructor (e.g., `Point::XY` without the field values)
        constructor_range: DocumentRange,
        /// Range of just the enum name (e.g., `Point` in `Point::XY`)
        enum_name_range: DocumentRange,
        range: DocumentRange,
    },

    BinaryOp {
        left: Box<Self>,
        operator: ParsedBinaryOp,
        right: Box<Self>,
        range: DocumentRange,
    },

    BooleanNegation {
        operand: Box<Self>,
        range: DocumentRange,
    },

    NumericNegation {
        operand: Box<Self>,
        range: DocumentRange,
    },

    Match {
        subject: Box<Self>,
        arms: Vec<ParsedMatchArm>,
        range: DocumentRange,
    },

    OptionLiteral {
        value: Option<Box<Self>>,
        range: DocumentRange,
    },

    MacroInvocation {
        /// The name of the macro, e.g. `join`.
        name: CheapString,
        /// The range of the function subject, e.g. `join!`.
        subject_range: DocumentRange,
        args: Vec<Self>,
        range: DocumentRange,
    },

    Markup {
        node: Box<ParsedNode>,
    },

    FunctionCall {
        name: VarName,
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
pub struct ParsedNamedArgument {
    pub name: VarName,
    pub name_range: DocumentRange,
    pub value: ParsedExpr,
}

#[derive(Debug, Clone)]
pub struct ParsedFieldInitializer {
    pub name: FieldName,
    pub name_range: DocumentRange,
    pub value: ParsedExpr,
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

impl ParsedBinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        let left = match self {
            ParsedBinaryOp::LogicalOr => 1,
            ParsedBinaryOp::LogicalAnd => 3,
            ParsedBinaryOp::Eq | ParsedBinaryOp::NotEq => 5,
            ParsedBinaryOp::LessThan
            | ParsedBinaryOp::GreaterThan
            | ParsedBinaryOp::LessThanOrEqual
            | ParsedBinaryOp::GreaterThanOrEqual => 7,
            ParsedBinaryOp::Plus | ParsedBinaryOp::Minus => 9,
            ParsedBinaryOp::Multiply => 11,
        };
        (left, left + 1)
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
    /// Apply `f` to each direct child expression, in source order.
    pub fn for_each_child<'a>(&'a self, f: &mut impl FnMut(&'a ParsedExpr)) {
        match self {
            ParsedExpr::FieldAccess { record: inner, .. }
            | ParsedExpr::MethodCall {
                receiver: inner, ..
            }
            | ParsedExpr::BooleanNegation { operand: inner, .. }
            | ParsedExpr::NumericNegation { operand: inner, .. } => f(inner),

            ParsedExpr::BinaryOp { left, right, .. } => {
                f(left);
                f(right);
            }
            ParsedExpr::ArrayLiteral { elements, .. }
            | ParsedExpr::MacroInvocation { args: elements, .. } => {
                for element in elements {
                    f(element);
                }
            }
            ParsedExpr::FunctionCall { args, .. } => match args {
                ParsedArguments::Positional(values) => {
                    for value in values {
                        f(value);
                    }
                }
                ParsedArguments::Named(named) => {
                    for arg in named {
                        f(&arg.value);
                    }
                }
            },
            ParsedExpr::RecordLiteral { fields, spread, .. } => {
                if let Some(spread) = spread {
                    f(spread);
                }
                for field in fields {
                    f(&field.value);
                }
            }
            ParsedExpr::EnumLiteral { fields, .. } => {
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
            ParsedExpr::OptionLiteral { value, .. } => {
                if let Some(value) = value {
                    f(value);
                }
            }

            ParsedExpr::Markup { .. }
            | ParsedExpr::VariableReference { .. }
            | ParsedExpr::StringLiteral { .. }
            | ParsedExpr::BooleanLiteral { .. }
            | ParsedExpr::IntLiteral { .. }
            | ParsedExpr::FloatLiteral { .. } => {}
        }
    }

    /// The nodes written in this expression, in source order.
    pub fn nodes(&self) -> Vec<&ParsedNode> {
        let mut out = Vec::new();
        self.collect_nodes(&mut out);
        out
    }

    fn collect_nodes<'a>(&'a self, out: &mut Vec<&'a ParsedNode>) {
        if let ParsedExpr::Markup { node } = self {
            out.push(node);
        }
        self.for_each_child(&mut |child| child.collect_nodes(out));
    }

    /// Whether this expression is a constant, i.e. a value written out in full
    /// with nothing left to evaluate.
    pub fn is_constant(&self) -> bool {
        match self {
            ParsedExpr::StringLiteral { .. }
            | ParsedExpr::BooleanLiteral { .. }
            | ParsedExpr::IntLiteral { .. }
            | ParsedExpr::FloatLiteral { .. } => true,
            ParsedExpr::Markup { node } => {
                matches!(node.as_ref(), ParsedNode::Fragment { children, .. } if children.is_empty())
            }
            ParsedExpr::ArrayLiteral { elements, .. } => {
                elements.iter().all(|element| element.is_constant())
            }
            ParsedExpr::RecordLiteral { fields, spread, .. } => {
                spread.is_none() && fields.iter().all(|field| field.value.is_constant())
            }
            ParsedExpr::EnumLiteral { fields, .. } => {
                fields.iter().all(|field| field.value.is_constant())
            }
            ParsedExpr::OptionLiteral { value, .. } => {
                value.as_ref().is_none_or(|value| value.is_constant())
            }
            ParsedExpr::VariableReference { .. }
            | ParsedExpr::FieldAccess { .. }
            | ParsedExpr::MethodCall { .. }
            | ParsedExpr::BinaryOp { .. }
            | ParsedExpr::BooleanNegation { .. }
            | ParsedExpr::NumericNegation { .. }
            | ParsedExpr::Match { .. }
            | ParsedExpr::MacroInvocation { .. }
            | ParsedExpr::FunctionCall { .. } => false,
        }
    }

    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedExpr::VariableReference { range, .. }
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
            | ParsedExpr::FunctionCall { range, .. } => range,
            ParsedExpr::Markup { node } => node.range(),
        }
    }

    pub const PREFIX_BINDING_POWER: u8 = 13;
    pub const POSTFIX_BINDING_POWER: u8 = 15;

    pub fn binding_power(&self) -> u8 {
        match self {
            ParsedExpr::BinaryOp { operator, .. } => operator.binding_power().0,
            ParsedExpr::BooleanNegation { .. } | ParsedExpr::NumericNegation { .. } => {
                Self::PREFIX_BINDING_POWER
            }
            ParsedExpr::FieldAccess { .. } | ParsedExpr::MethodCall { .. } => {
                Self::POSTFIX_BINDING_POWER
            }
            _ => u8::MAX,
        }
    }

    /// Converts this expression to a doc, adding parentheses if it does not
    /// bind tightly enough for the operand slot it is placed in.
    fn to_doc_in_slot(&self, slot_binding_power: u8) -> BoxDoc<'_> {
        if self.binding_power() < slot_binding_power {
            BoxDoc::text("(")
                .append(self.to_doc())
                .append(BoxDoc::text(")"))
        } else {
            self.to_doc()
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedExpr::VariableReference { value, .. } => BoxDoc::text(value.as_str()),
            ParsedExpr::FieldAccess {
                record: object,
                field,
                ..
            } => object
                .to_doc_in_slot(Self::POSTFIX_BINDING_POWER)
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            ParsedExpr::MethodCall {
                receiver, method, ..
            } => receiver
                .to_doc_in_slot(Self::POSTFIX_BINDING_POWER)
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
                spread,
                ..
            } => {
                if fields.is_empty() && spread.is_none() {
                    BoxDoc::text(record_name.as_str()).append(BoxDoc::text(" {}"))
                } else {
                    // The spread is canonicalized to first position.
                    let entries = spread
                        .iter()
                        .map(|subject| BoxDoc::text("...").append(subject.to_doc()))
                        .chain(fields.iter().map(|field| {
                            BoxDoc::text(field.name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(field.value.to_doc())
                        }));
                    BoxDoc::text(record_name.as_str())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    entries,
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
                let (left_power, right_power) = operator.binding_power();
                left.to_doc_in_slot(left_power)
                    .append(BoxDoc::text(format!(" {} ", operator)))
                    .append(right.to_doc_in_slot(right_power))
            }
            ParsedExpr::BooleanNegation { operand, .. } => {
                BoxDoc::text("!").append(operand.to_doc_in_slot(Self::PREFIX_BINDING_POWER))
            }
            ParsedExpr::NumericNegation { operand, .. } => {
                BoxDoc::text("-").append(operand.to_doc_in_slot(Self::PREFIX_BINDING_POWER))
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
                            fields.iter().map(|field| {
                                BoxDoc::text(field.name.to_string())
                                    .append(BoxDoc::text(": "))
                                    .append(field.value.to_doc())
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
            ParsedExpr::Markup { node } => node.to_doc(),
            ParsedExpr::FunctionCall { name, args, .. } => {
                let arg_docs: Vec<BoxDoc<'_>> = match args {
                    ParsedArguments::Positional(values) => {
                        values.iter().map(|value| value.to_doc()).collect()
                    }
                    ParsedArguments::Named(named) => named
                        .iter()
                        .map(|arg| {
                            BoxDoc::text(arg.name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(arg.value.to_doc())
                        })
                        .collect(),
                };
                if arg_docs.is_empty() {
                    BoxDoc::text(name.as_str()).append(BoxDoc::text("()"))
                } else {
                    BoxDoc::text(name.as_str())
                        .append(BoxDoc::text("("))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    arg_docs,
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
