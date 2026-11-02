use std::fmt::{self, Display};

use crate::display::{join, write_tuple};
use crate::hop::typing::r#type::Type;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

/// A pattern of a match arm. `B` is what a binding binds, a TypedBinder in
/// the typed AST and an IrBinder in the IR.
#[derive(Debug, Clone)]
pub enum Pattern<B> {
    Wildcard,
    /// Binds the value it matches.
    Binding(B),
    Constructor {
        constructor: Constructor,
        args: Vec<Pattern<B>>,
        fields: Vec<PatternField<B>>,
    },
}

#[derive(Debug, Clone)]
pub struct PatternField<B> {
    pub name: FieldName,
    /// The position of the field in its record or variant declaration.
    pub index: usize,
    pub pattern: Pattern<B>,
}

/// A variable a pattern of the typed AST binds, with the type of the values
/// it takes.
#[derive(Debug, Clone)]
pub struct TypedBinder {
    pub var: VarName,
    pub typ: Type,
}

pub type TypedPattern = Pattern<TypedBinder>;

pub type TypedField = PatternField<TypedBinder>;

/// A constructor pattern (non-wildcard pattern that matches a specific value)
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    /// A boolean true pattern
    BoolTrue,
    /// A boolean false pattern
    BoolFalse,
    /// An Option Some pattern, e.g. `Some(_)`
    OptionSome,
    /// An Option None pattern, e.g. `None`
    OptionNone,
    /// An enum variant pattern, e.g. `Color::Red`
    EnumVariant {
        type_name: TypeName,
        variant_name: TypeName,
    },
    /// A record pattern, e.g. `User {name: x, age: y}`
    Record { type_name: TypeName },
    /// A tuple pattern, e.g. `(x, _)`
    Tuple,
}

impl Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constructor::EnumVariant {
                type_name,
                variant_name,
            } => write!(f, "{}::{}", type_name.as_str(), variant_name.as_str()),
            Constructor::BoolTrue => f.write_str("true"),
            Constructor::BoolFalse => f.write_str("false"),
            Constructor::OptionSome => f.write_str("Some"),
            Constructor::OptionNone => f.write_str("None"),
            Constructor::Record { type_name } => f.write_str(type_name.as_str()),
            Constructor::Tuple => Ok(()),
        }
    }
}

impl Display for TypedField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.as_str(), self.pattern)
    }
}

impl Display for TypedPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypedPattern::Constructor {
                constructor: Constructor::Tuple,
                args,
                ..
            } => write_tuple(f, args),
            TypedPattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                if !fields.is_empty() {
                    write!(f, "{constructor} {{{}}}", join(fields, ", "))
                } else if args.is_empty() {
                    write!(f, "{constructor}")
                } else {
                    write!(f, "{constructor}({})", join(args, ", "))
                }
            }
            TypedPattern::Wildcard => f.write_str("_"),
            TypedPattern::Binding(binder) => f.write_str(binder.var.as_str()),
        }
    }
}
