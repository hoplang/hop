use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::Type;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, range: DocumentRange },

    #[error("Property {property} not found in object {dop_type}")]
    PropertyNotFoundInObject {
        property: String,
        dop_type: Type,
        range: DocumentRange,
    },

    #[error("{typ} can not be used as an object")]
    CannotUseAsObject { typ: String, range: DocumentRange },

    #[error("Can not compare {left} to {right}")]
    CannotCompareTypes {
        left: String,
        right: String,
        range: DocumentRange,
    },

    #[error("Negation operator can only be applied to Bool values")]
    NegationRequiresBoolean { range: DocumentRange },

    #[error("Array elements must all have the same type, found {expected} and {found}")]
    ArrayTypeMismatch {
        expected: String,
        found: String,
        range: DocumentRange,
    },

    #[error("Type {t} is not comparable")]
    TypeIsNotComparable { t: Type, range: DocumentRange },

    #[error("Logical AND operator can only be applied to Bool values")]
    LogicalAndRequiresBoolean { range: DocumentRange },

    #[error("Logical OR operator can only be applied to Bool values")]
    LogicalOrRequiresBoolean { range: DocumentRange },

    #[error("Cannot add values of incompatible types: {left_type} + {right_type}")]
    IncompatibleTypesForAddition {
        left_type: String,
        right_type: String,
        range: DocumentRange,
    },

    #[error("Cannot subtract values of incompatible types: {left_type} - {right_type}")]
    IncompatibleTypesForSubtraction {
        left_type: String,
        right_type: String,
        range: DocumentRange,
    },
}

impl Ranged for TypeError {
    fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedVariable { range, .. }
            | TypeError::PropertyNotFoundInObject { range, .. }
            | TypeError::CannotUseAsObject { range, .. }
            | TypeError::CannotCompareTypes { range, .. }
            | TypeError::NegationRequiresBoolean { range, .. }
            | TypeError::ArrayTypeMismatch { range, .. }
            | TypeError::TypeIsNotComparable { range, .. }
            | TypeError::LogicalAndRequiresBoolean { range, .. }
            | TypeError::LogicalOrRequiresBoolean { range, .. }
            | TypeError::IncompatibleTypesForAddition { range, .. }
            | TypeError::IncompatibleTypesForSubtraction { range, .. } => range,
        }
    }
}
