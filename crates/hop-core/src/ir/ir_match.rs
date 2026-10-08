//! Match types shared by the Pure, Flat and Writer IR.

use pretty::BoxDoc;

use crate::ir::ir_binder::IrBinder;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// An enum variant pattern, e.g. `Color::Red`
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPattern {
    Variant {
        type_name: TypeName,
        variant_name: TypeName,
    },
}

/// A single arm in an enum match, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumMatchArm<Body> {
    pub pattern: EnumPattern,
    /// Field bindings for this arm, e.g. `Result::Ok(value: v)` binds field "value" to variable "v"
    pub bindings: Vec<(FieldName, IrBinder)>,
    pub body: Body,
}

/// A match that can be used for different expression and statement types.
#[derive(Debug, Clone, PartialEq)]
pub enum Match<Subj, Body> {
    /// An enum match, e.g. `match color { Color::Red => "red", ... }`
    Enum {
        subject: Box<Subj>,
        arms: Vec<EnumMatchArm<Body>>,
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
        some_arm_binding: Option<IrBinder>,
        some_arm_body: Box<Body>,
        none_arm_body: Box<Body>,
    },
}

impl<Subj, Body> Match<Subj, Body> {
    /// The match head, then each arm as its pattern and its body on lines
    /// of their own. The body doc starts with a line break and the arm
    /// nests it.
    pub fn to_doc<'a>(
        &'a self,
        subject_to_doc: impl Fn(&'a Subj) -> BoxDoc<'a>,
        body_to_doc: impl Fn(&'a Body) -> BoxDoc<'a>,
    ) -> BoxDoc<'a> {
        let (subject, arms) = match self {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => (
                subject,
                vec![
                    ("true".to_string(), &**true_body),
                    ("false".to_string(), &**false_body),
                ],
            ),
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let some_pattern = match some_arm_binding {
                    Some(binder) => format!("Some({}: {})", binder.var, binder.typ),
                    None => "Some(_)".to_string(),
                };
                (
                    subject,
                    vec![
                        (some_pattern, &**some_arm_body),
                        ("None".to_string(), &**none_arm_body),
                    ],
                )
            }
            Match::Enum { subject, arms } => (
                subject,
                arms.iter()
                    .map(|enum_arm| {
                        let EnumPattern::Variant {
                            type_name,
                            variant_name,
                        } = &enum_arm.pattern;
                        let mut pattern =
                            format!("{}::{}", type_name.as_str(), variant_name.as_str());
                        if !enum_arm.bindings.is_empty() {
                            let bindings = enum_arm
                                .bindings
                                .iter()
                                .map(|(field, binder)| {
                                    format!("{}@{}: {}", field.as_str(), binder.var, binder.typ)
                                })
                                .collect::<Vec<_>>()
                                .join(", ");
                            pattern.push_str(&format!(" {{{bindings}}}"));
                        }
                        (pattern, &enum_arm.body)
                    })
                    .collect(),
            ),
        };
        let head = BoxDoc::text("match ").append(subject_to_doc(subject));
        if arms.is_empty() {
            return head.append(BoxDoc::text(" {}"));
        }
        head.append(BoxDoc::text(" {"))
            .append(
                BoxDoc::concat(arms.into_iter().map(|(pattern, body)| {
                    BoxDoc::line()
                        .append(BoxDoc::text(format!("{pattern} => {{")))
                        .append(body_to_doc(body).nest(2))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}"))
                }))
                .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}
