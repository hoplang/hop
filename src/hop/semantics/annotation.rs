use crate::document::{Annotation, DocumentRange};
use crate::dop::Type;
use std::fmt::{self, Display};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    TypeInfo {
        typ: Arc<Type>,
        name: String,
        range: DocumentRange,
    },
    Description {
        title: String,
        description: String,
        range: DocumentRange,
    },
}

impl TypeAnnotation {
    pub fn range(&self) -> &DocumentRange {
        match self {
            TypeAnnotation::TypeInfo { range, .. } => range,
            TypeAnnotation::Description { range, .. } => range,
        }
    }
}

impl Annotation for TypeAnnotation {
    fn message(&self) -> String {
        self.to_string()
    }
    fn range(&self) -> &DocumentRange {
        self.range()
    }
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeAnnotation::TypeInfo { name, typ, .. } => write!(f, "`{}`: `{}`", name, typ),
            TypeAnnotation::Description {
                title, description, ..
            } => write!(f, "{}\n\n---\n\n{}", title, description),
        }
    }
}
