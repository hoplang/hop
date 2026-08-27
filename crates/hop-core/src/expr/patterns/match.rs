//! Generic match types that can be used across different AST representations.

use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

/// An enum variant pattern, e.g. `Color::Red`
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPattern {
    Variant {
        enum_name: TypeName,
        variant_name: TypeName,
    },
}

/// A single arm in an enum match, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumMatchArm<Body, Var = VarName> {
    pub pattern: EnumPattern,
    /// Field bindings for this arm, e.g. `Result::Ok(value: v)` binds field "value" to variable "v"
    pub bindings: Vec<(FieldName, Var)>,
    pub body: Body,
}

/// A match that can be used for different expression and statement types.
#[derive(Debug, Clone, PartialEq)]
pub enum Match<Subj, Body, Var = VarName> {
    /// An enum match, e.g. `match color { Color::Red => "red", ... }`
    Enum {
        subject: Box<Subj>,
        arms: Vec<EnumMatchArm<Body, Var>>,
    },

    /// A boolean match, e.g. `match flag { true => "yes", false => "no" }`
    Bool {
        subject: Box<Subj>,
        true_body: Box<Body>,
        false_body: Box<Body>,
    },

    /// An option match, e.g. `match opt { Some(x) => x, None => "empty" }`
    Option {
        subject: Box<Subj>,
        some_arm_binding: Option<Var>,
        some_arm_body: Box<Body>,
        none_arm_body: Box<Body>,
    },
}
