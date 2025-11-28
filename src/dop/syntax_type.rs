use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use pretty::BoxDoc;

/// A syntax representation of a type, preserving document ranges.
#[derive(Debug, Clone)]
pub enum SyntaxType {
    String { range: DocumentRange },
    Bool { range: DocumentRange },
    Int { range: DocumentRange },
    Float { range: DocumentRange },
    TrustedHTML { range: DocumentRange },
    Array { element: Option<Box<SyntaxType>>, range: DocumentRange },
    Named { name: String, range: DocumentRange },
}

impl Ranged for SyntaxType {
    fn range(&self) -> &DocumentRange {
        match self {
            SyntaxType::String { range }
            | SyntaxType::Bool { range }
            | SyntaxType::Int { range }
            | SyntaxType::Float { range }
            | SyntaxType::TrustedHTML { range }
            | SyntaxType::Array { range, .. }
            | SyntaxType::Named { range, .. } => range,
        }
    }
}

impl SyntaxType {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            SyntaxType::String { .. } => BoxDoc::text("String"),
            SyntaxType::Bool { .. } => BoxDoc::text("Bool"),
            SyntaxType::Int { .. } => BoxDoc::text("Int"),
            SyntaxType::Float { .. } => BoxDoc::text("Float"),
            SyntaxType::TrustedHTML { .. } => BoxDoc::text("TrustedHTML"),
            SyntaxType::Array { element, .. } => match element {
                Some(elem) => BoxDoc::nil()
                    .append(BoxDoc::text("Array["))
                    .append(elem.to_doc())
                    .append(BoxDoc::text("]")),
                None => BoxDoc::text("Array"),
            },
            SyntaxType::Named { name, .. } => BoxDoc::text(name.clone()),
        }
    }
}

impl Display for SyntaxType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
