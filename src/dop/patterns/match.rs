//! Generic match types that can be used across different AST representations.

use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::var_name::VarName;

/// An enum variant pattern, e.g. `Color::Red`
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPattern {
    Variant { enum_name: String, variant_name: String },
}

/// A single arm in an enum match, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumMatchArm<Body> {
    pub pattern: EnumPattern,
    pub body: Body,
}

/// A match that can be used for different expression and statement types.
///
/// - `Subject`: The type of the expression being matched on
/// - `Body`: The type of the arm bodies (expressions or statements)
#[derive(Debug, Clone, PartialEq)]
pub enum Match<Subject, Body> {
    /// An enum match, e.g. `match color { Color::Red => "red", ... }`
    Enum {
        subject: Box<Subject>,
        arms: Vec<EnumMatchArm<Body>>,
    },

    /// A boolean match, e.g. `match flag { true => "yes", false => "no" }`
    Bool {
        subject: Box<Subject>,
        true_body: Box<Body>,
        false_body: Box<Body>,
    },

    /// An option match, e.g. `match opt { Some(x) => x, None => "empty" }`
    Option {
        subject: Box<Subject>,
        some_arm_binding: Option<(VarName, Type)>,
        some_arm_body: Box<Body>,
        none_arm_body: Box<Body>,
    },
}

impl<Subject, Body> Match<Subject, Body> {
    /// Get a reference to the subject expression
    pub fn subject(&self) -> &Subject {
        match self {
            Match::Enum { subject, .. }
            | Match::Bool { subject, .. }
            | Match::Option { subject, .. } => subject,
        }
    }

    /// Get a mutable reference to the subject expression
    pub fn subject_mut(&mut self) -> &mut Subject {
        match self {
            Match::Enum { subject, .. }
            | Match::Bool { subject, .. }
            | Match::Option { subject, .. } => subject,
        }
    }

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
