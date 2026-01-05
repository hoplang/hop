//! Generic match expression types that can be used across different AST representations.

use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::var_name::VarName;

/// An enum variant pattern, e.g. `Color::Red`
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPattern {
    Variant { enum_name: String, variant_name: String },
}

/// A single arm in an enum match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumMatchArm<Body> {
    pub pattern: EnumPattern,
    pub body: Body,
}

/// A match expression that can be used for different expression types.
///
/// - `Subject`: The type of the expression being matched on
/// - `Body`: The type of the arm bodies (expressions or statements)
#[derive(Debug, Clone, PartialEq)]
pub enum Match<Subject, Body> {
    /// An enum match expression, e.g. `match color { Color::Red => "red", ... }`
    Enum {
        subject: Box<Subject>,
        arms: Vec<EnumMatchArm<Body>>,
    },

    /// A boolean match expression, e.g. `match flag { true => "yes", false => "no" }`
    Bool {
        subject: Box<Subject>,
        true_body: Box<Body>,
        false_body: Box<Body>,
    },

    /// An option match expression, e.g. `match opt { Some(x) => x, None => "empty" }`
    Option {
        subject: Box<Subject>,
        some_arm_binding: Option<(VarName, Type)>,
        some_arm_body: Box<Body>,
        none_arm_body: Box<Body>,
    },
}
