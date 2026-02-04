use crate::document::{DocumentRange, Ranged};
use crate::dop::Type;
use std::fmt::{self, Display};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Annotation {
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

impl Ranged for Annotation {
    fn range(&self) -> &DocumentRange {
        match self {
            Annotation::TypeInfo { range, .. } => range,
            Annotation::Description { range, .. } => range,
        }
    }
}

impl Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Annotation::TypeInfo { name, typ, .. } => write!(f, "`{}`: `{}`", name, typ),
            Annotation::Description {
                title, description, ..
            } => write!(f, "{}\n\n---\n\n{}", title, description),
        }
    }
}
