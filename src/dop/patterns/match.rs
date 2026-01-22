//! Generic match types that can be used across different AST representations.

use std::sync::Arc;

use crate::document::CheapString;
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::var_name::VarName;

/// An enum variant pattern, e.g. `Color::Red`
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPattern {
    Variant {
        enum_name: CheapString,
        variant_name: CheapString,
    },
}

/// A single arm in an enum match, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumMatchArm<Body> {
    pub pattern: EnumPattern,
    /// Field bindings for this arm, e.g. `Result::Ok(value: v)` binds field "value" to variable "v"
    pub bindings: Vec<(FieldName, VarName)>,
    pub body: Body,
}

/// A match that can be used for different expression and statement types.
///
/// The subject must be a variable reference (VarName, Type), not an arbitrary expression.
/// If matching on an expression, create a let binding first.
///
/// - `Body`: The type of the arm bodies (expressions or statements)
#[derive(Debug, Clone, PartialEq)]
pub enum Match<Body> {
    /// An enum match, e.g. `match color { Color::Red => "red", ... }`
    Enum {
        subject: (VarName, Arc<Type>),
        arms: Vec<EnumMatchArm<Body>>,
    },

    /// A boolean match, e.g. `match flag { true => "yes", false => "no" }`
    Bool {
        subject: (VarName, Arc<Type>),
        true_body: Box<Body>,
        false_body: Box<Body>,
    },

    /// An option match, e.g. `match opt { Some(x) => x, None => "empty" }`
    Option {
        subject: (VarName, Arc<Type>),
        some_arm_binding: Option<VarName>,
        some_arm_body: Box<Body>,
        none_arm_body: Box<Body>,
    },
}

impl<Body> Match<Body> {
    /// Get references to all body branches
    pub fn bodies(&self) -> Vec<&Body> {
        match self {
            Match::Enum { arms, .. } => arms.iter().map(|arm| &arm.body).collect(),
            Match::Bool {
                true_body,
                false_body,
                ..
            } => vec![true_body.as_ref(), false_body.as_ref()],
            Match::Option {
                some_arm_body,
                none_arm_body,
                ..
            } => vec![some_arm_body.as_ref(), none_arm_body.as_ref()],
        }
    }

    /// Get mutable references to all body branches
    pub fn bodies_mut(&mut self) -> Vec<&mut Body> {
        match self {
            Match::Enum { arms, .. } => arms.iter_mut().map(|arm| &mut arm.body).collect(),
            Match::Bool {
                true_body,
                false_body,
                ..
            } => vec![true_body.as_mut(), false_body.as_mut()],
            Match::Option {
                some_arm_body,
                none_arm_body,
                ..
            } => vec![some_arm_body.as_mut(), none_arm_body.as_mut()],
        }
    }
}
