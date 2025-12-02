use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use pretty::BoxDoc;

/// A syntax representation of a type, preserving document ranges.
#[derive(Debug, Clone)]
pub enum SyntacticType {
    String {
        range: DocumentRange,
    },
    Bool {
        range: DocumentRange,
    },
    Int {
        range: DocumentRange,
    },
    Float {
        range: DocumentRange,
    },
    TrustedHTML {
        range: DocumentRange,
    },
    Array {
        element: Option<Box<SyntacticType>>,
        range: DocumentRange,
    },
    Named {
        name: String,
        range: DocumentRange,
    },
}

impl Ranged for SyntacticType {
    fn range(&self) -> &DocumentRange {
        match self {
            SyntacticType::String { range }
            | SyntacticType::Bool { range }
            | SyntacticType::Int { range }
            | SyntacticType::Float { range }
            | SyntacticType::TrustedHTML { range }
            | SyntacticType::Array { range, .. }
            | SyntacticType::Named { range, .. } => range,
        }
    }
}

impl SyntacticType {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            SyntacticType::String { .. } => BoxDoc::text("String"),
            SyntacticType::Bool { .. } => BoxDoc::text("Bool"),
            SyntacticType::Int { .. } => BoxDoc::text("Int"),
            SyntacticType::Float { .. } => BoxDoc::text("Float"),
            SyntacticType::TrustedHTML { .. } => BoxDoc::text("TrustedHTML"),
            SyntacticType::Array { element, .. } => match element {
                Some(elem) => BoxDoc::nil()
                    .append(BoxDoc::text("Array["))
                    .append(elem.to_doc())
                    .append(BoxDoc::text("]")),
                None => BoxDoc::text("Array"),
            },
            SyntacticType::Named { name, .. } => BoxDoc::text(name.clone()),
        }
    }
}

impl Display for SyntacticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
