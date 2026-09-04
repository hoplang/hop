use std::fmt::{self, Display};

use pretty::BoxDoc;

use crate::document::DocumentRange;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone)]
pub enum ParsedType {
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
    Fragment {
        range: DocumentRange,
    },
    Array {
        element: Box<ParsedType>,
        range: DocumentRange,
    },
    Option {
        element: Box<ParsedType>,
        range: DocumentRange,
    },
    Named {
        name: TypeName,
        range: DocumentRange,
    },
}

impl ParsedType {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedType::String { .. } => BoxDoc::text("String"),
            ParsedType::Bool { .. } => BoxDoc::text("Bool"),
            ParsedType::Int { .. } => BoxDoc::text("Int"),
            ParsedType::Float { .. } => BoxDoc::text("Float"),
            ParsedType::Fragment { .. } => BoxDoc::text("Fragment"),
            ParsedType::Option { element, .. } => BoxDoc::nil()
                .append(BoxDoc::text("Option["))
                .append(element.to_doc())
                .append(BoxDoc::text("]")),
            ParsedType::Array { element, .. } => BoxDoc::nil()
                .append(BoxDoc::text("Array["))
                .append(element.to_doc())
                .append(BoxDoc::text("]")),
            ParsedType::Named { name, .. } => BoxDoc::text(name.to_string()),
        }
    }
}

impl Display for ParsedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
