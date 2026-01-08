use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::symbols::type_name::TypeName;

use super::r#type::Type;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, range: DocumentRange },

    #[error("Field '{field}' not found in record '{record_name}'")]
    FieldNotFoundInRecord {
        field: String,
        record_name: TypeName,
        range: DocumentRange,
    },

    #[error("{typ} can not be used as a record")]
    CannotUseAsRecord { typ: String, range: DocumentRange },

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

    #[error("Cannot infer type of empty array")]
    CannotInferEmptyArrayType { range: DocumentRange },

    #[error("Cannot infer type of None without context")]
    CannotInferNoneType { range: DocumentRange },

    #[error("Type {t} is not comparable")]
    TypeIsNotComparable { t: Type, range: DocumentRange },

    #[error("&& operator can only be applied to Bool values")]
    LogicalAndRequiresBoolean { range: DocumentRange },

    #[error("|| operator can only be applied to Bool values")]
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

    #[error("Cannot multiply values of incompatible types: {left_type} * {right_type}")]
    IncompatibleTypesForMultiplication {
        left_type: String,
        right_type: String,
        range: DocumentRange,
    },

    #[error("Type '{type_name}' is not defined")]
    UndefinedType {
        type_name: String,
        range: DocumentRange,
    },

    #[error("Record type '{record_name}' is not defined")]
    UndefinedRecord {
        record_name: String,
        range: DocumentRange,
    },

    #[error("Missing field '{field_name}' in record literal '{record_name}'")]
    RecordLiteralMissingRecordField {
        field_name: String,
        record_name: String,
        range: DocumentRange,
    },

    #[error("Unknown field '{field_name}' in record literal '{record_name}'")]
    RecordLiteralUnknownRecordField {
        field_name: String,
        record_name: String,
        range: DocumentRange,
    },

    #[error("Field '{field_name}' expects type {expected}, but got {found}")]
    RecordLiteralFieldTypeMismatch {
        field_name: String,
        expected: String,
        found: String,
        range: DocumentRange,
    },

    #[error("Enum type '{enum_name}' is not defined")]
    UndefinedEnum {
        enum_name: String,
        range: DocumentRange,
    },

    #[error("Variant '{variant_name}' is not defined in enum '{enum_name}'")]
    UndefinedEnumVariant {
        enum_name: String,
        variant_name: String,
        range: DocumentRange,
    },

    #[error("Match is not implemented for type {found}")]
    MatchNotImplementedForType { found: String, range: DocumentRange },

    #[error("Match pattern enum '{pattern_enum}' does not match subject enum '{subject_enum}'")]
    MatchPatternEnumMismatch {
        pattern_enum: String,
        subject_enum: String,
        range: DocumentRange,
    },

    #[error(
        "Match pattern record '{pattern_record}' does not match subject record '{subject_record}'"
    )]
    MatchPatternRecordMismatch {
        pattern_record: String,
        subject_record: String,
        range: DocumentRange,
    },

    #[error(
        "Record pattern for '{record_name}' must specify all {expected} fields, but only {found} were provided"
    )]
    MatchRecordPatternFieldCount {
        record_name: String,
        expected: usize,
        found: usize,
        range: DocumentRange,
    },

    #[error("Unknown field '{field_name}' in record pattern for '{record_name}'")]
    MatchRecordPatternUnknownField {
        field_name: String,
        record_name: String,
        range: DocumentRange,
    },

    #[error("Match arms must all have the same type, expected {expected} but found {found}")]
    MatchArmTypeMismatch {
        expected: String,
        found: String,
        range: DocumentRange,
    },

    #[error("Match expression is missing arms for: {}", variants.join(", "))]
    MatchMissingVariants {
        variants: Vec<String>,
        range: DocumentRange,
    },

    #[error("Unreachable match arm for variant '{variant}'")]
    MatchUnreachableArm {
        variant: String,
        range: DocumentRange,
    },

    #[error("Match pattern type mismatch: expected {expected}, found {found}")]
    MatchPatternTypeMismatch {
        expected: String,
        found: String,
        range: DocumentRange,
    },

    #[error("Match expression must have at least one arm")]
    MatchNoArms { range: DocumentRange },

    #[error("Unused binding '{name}' in match arm")]
    MatchUnusedBinding { name: String, range: DocumentRange },

    #[error("Variable '{name}' is already defined")]
    VariableAlreadyDefined { name: String, range: DocumentRange },

    #[error("Macro '{macro_name}' expects {expected} arguments, but got {actual}")]
    MacroArgumentTypeMismatch {
        macro_name: String,
        expected: String,
        actual: String,
        range: DocumentRange,
    },
}

impl Ranged for TypeError {
    fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedVariable { range, .. }
            | TypeError::FieldNotFoundInRecord { range, .. }
            | TypeError::CannotUseAsRecord { range, .. }
            | TypeError::CannotCompareTypes { range, .. }
            | TypeError::NegationRequiresBoolean { range, .. }
            | TypeError::ArrayTypeMismatch { range, .. }
            | TypeError::CannotInferEmptyArrayType { range, .. }
            | TypeError::CannotInferNoneType { range, .. }
            | TypeError::TypeIsNotComparable { range, .. }
            | TypeError::LogicalAndRequiresBoolean { range, .. }
            | TypeError::LogicalOrRequiresBoolean { range, .. }
            | TypeError::IncompatibleTypesForAddition { range, .. }
            | TypeError::IncompatibleTypesForSubtraction { range, .. }
            | TypeError::IncompatibleTypesForMultiplication { range, .. }
            | TypeError::UndefinedType { range, .. }
            | TypeError::UndefinedRecord { range, .. }
            | TypeError::RecordLiteralMissingRecordField { range, .. }
            | TypeError::RecordLiteralUnknownRecordField { range, .. }
            | TypeError::RecordLiteralFieldTypeMismatch { range, .. }
            | TypeError::UndefinedEnum { range, .. }
            | TypeError::UndefinedEnumVariant { range, .. }
            | TypeError::MatchNotImplementedForType { range, .. }
            | TypeError::MatchPatternEnumMismatch { range, .. }
            | TypeError::MatchPatternRecordMismatch { range, .. }
            | TypeError::MatchRecordPatternFieldCount { range, .. }
            | TypeError::MatchRecordPatternUnknownField { range, .. }
            | TypeError::MatchArmTypeMismatch { range, .. }
            | TypeError::MatchMissingVariants { range, .. }
            | TypeError::MatchUnreachableArm { range, .. }
            | TypeError::MatchPatternTypeMismatch { range, .. }
            | TypeError::MatchNoArms { range, .. }
            | TypeError::MatchUnusedBinding { range, .. }
            | TypeError::VariableAlreadyDefined { range, .. }
            | TypeError::MacroArgumentTypeMismatch { range, .. } => range,
        }
    }
}
