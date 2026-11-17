use std::fmt::{self, Display};

use crate::display::join;
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
    Html {
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
    Tuple {
        elements: Vec<ParsedType>,
        range: DocumentRange,
    },
    Named {
        name: TypeName,
        range: DocumentRange,
    },
    Function {
        mut_range: Option<DocumentRange>,
        parameters: Vec<ParsedType>,
        result: Box<ParsedType>,
        range: DocumentRange,
    },
}

impl ParsedType {
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedType::String { range }
            | ParsedType::Bool { range }
            | ParsedType::Int { range }
            | ParsedType::Float { range }
            | ParsedType::Html { range }
            | ParsedType::Array { range, .. }
            | ParsedType::Option { range, .. }
            | ParsedType::Tuple { range, .. }
            | ParsedType::Named { range, .. }
            | ParsedType::Function { range, .. } => range,
        }
    }
}

impl Display for ParsedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedType::String { .. } => f.write_str("String"),
            ParsedType::Bool { .. } => f.write_str("Bool"),
            ParsedType::Int { .. } => f.write_str("Int"),
            ParsedType::Float { .. } => f.write_str("Float"),
            ParsedType::Html { .. } => f.write_str("Html"),
            ParsedType::Option { element, .. } => write!(f, "Option[{element}]"),
            ParsedType::Array { element, .. } => write!(f, "Array[{element}]"),
            ParsedType::Tuple { elements, .. } => write!(f, "Tuple[{}]", join(elements, ", ")),
            ParsedType::Named { name, .. } => write!(f, "{name}"),
            ParsedType::Function {
                mut_range,
                parameters,
                result,
                ..
            } => {
                if mut_range.is_some() {
                    f.write_str("mut ")?;
                }
                write!(f, "fn({}) -> {result}", join(parameters, ", "))
            }
        }
    }
}
