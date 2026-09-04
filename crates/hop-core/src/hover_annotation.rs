use crate::annotation::Annotation;
use crate::document::DocumentRange;
use crate::hop::typing::Type;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::fmt::{self, Display};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum HoverAnnotation {
    TypeForTypeName {
        typ: Arc<Type>,
        type_name: TypeName,
        range: DocumentRange,
    },
    TypeForVarName {
        typ: Arc<Type>,
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
            HoverAnnotation::TypeForVarName { range, .. } => range,
            HoverAnnotation::TypeForTypeName { range, .. } => range,
        }
    }
}

impl Annotation for HoverAnnotation {
    fn message(&self) -> String {
        self.to_string()
    }
    fn range(&self) -> &DocumentRange {
        self.range()
    }
}

impl Display for HoverAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Render as:
            // | ```
            // | var_name : type
            // | ```
            HoverAnnotation::TypeForVarName { var_name, typ, .. } => {
                write!(f, "```\n{} : {}\n```", var_name, typ)
            }
            // Render as:
            // | ```
            // | type_name : type
            // | ```
            HoverAnnotation::TypeForTypeName { type_name, typ, .. } => {
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
