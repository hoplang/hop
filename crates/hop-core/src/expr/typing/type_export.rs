use super::r#type::ComponentSignature;
use crate::document::DocumentRange;

/// A type or component exported by a module. Non-pub declarations are
/// included with is_pub set to false so that importers can distinguish
/// private names from undeclared ones.
#[derive(Debug, Clone)]
pub enum TypeExport {
    Type {
        definition_range: DocumentRange,
        is_pub: bool,
    },
    Component {
        signature: ComponentSignature,
        definition_range: DocumentRange,
        is_pub: bool,
    },
}

impl TypeExport {
    pub fn definition_range(&self) -> &DocumentRange {
        match self {
            TypeExport::Type {
                definition_range, ..
            }
            | TypeExport::Component {
                definition_range, ..
            } => definition_range,
        }
    }

    pub fn is_pub(&self) -> bool {
        match self {
            TypeExport::Type { is_pub, .. } | TypeExport::Component { is_pub, .. } => *is_pub,
        }
    }
}
