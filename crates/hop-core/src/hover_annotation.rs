use crate::document::DocumentRange;
use crate::hop::typing::Type;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub enum HoverAnnotation {
    TypeName {
        typ: Type,
        type_name: TypeName,
        range: DocumentRange,
    },
    VarName {
        typ: Type,
        var_name: VarName,
        range: DocumentRange,
    },
    Description {
        title: String,
        description: String,
        range: DocumentRange,
    },
}

impl HoverAnnotation {
    pub fn range(&self) -> &DocumentRange {
        match self {
            HoverAnnotation::Description { range, .. } => range,
            HoverAnnotation::VarName { range, .. } => range,
            HoverAnnotation::TypeName { range, .. } => range,
        }
    }
}

impl Display for HoverAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Render as:
            // | ```
            // | var_name : type
            // | ```
            HoverAnnotation::VarName { var_name, typ, .. } => {
                write!(f, "```\n{} : {}\n```", var_name, typ)
            }
            // Render as:
            // | ```
            // | type_name : type
            // | ```
            HoverAnnotation::TypeName { type_name, typ, .. } => {
                write!(f, "```\n{} : {}\n```", type_name, typ)
            }
            // Render as:
            // | ```
            // | title
            // | ```
            // |
            // | description
            HoverAnnotation::Description {
                title, description, ..
            } => write!(f, "```\n{}\n```\n\n{}", title, description),
        }
    }
}
