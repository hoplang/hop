use super::type_env::FunctionSignature;
use crate::document::DocumentRange;

/// A name a module exports. Non-pub declarations are included with is_pub
/// set to false so that importers can distinguish private names from
/// undeclared ones.
#[derive(Debug, Clone)]
pub enum Export {
    Type {
        definition_range: DocumentRange,
        is_pub: bool,
    },
    Function {
        signature: FunctionSignature,
        definition_range: DocumentRange,
        is_pub: bool,
    },
}

impl Export {
    pub fn definition_range(&self) -> &DocumentRange {
        match self {
            Export::Type {
                definition_range, ..
            }
            | Export::Function {
                definition_range, ..
            } => definition_range,
        }
    }

    pub fn is_pub(&self) -> bool {
        match self {
            Export::Type { is_pub, .. } | Export::Function { is_pub, .. } => *is_pub,
        }
    }
}
