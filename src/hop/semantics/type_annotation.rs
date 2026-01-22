use crate::document::{DocumentRange, Ranged};
use crate::dop::Type;
use std::fmt::{self, Display};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub typ: Arc<Type>,
    pub name: String,
    pub range: DocumentRange,
}

impl Ranged for TypeAnnotation {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ)
    }
}
