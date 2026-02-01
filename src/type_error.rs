use std::sync::Arc;

use crate::document::{CheapString, DocumentRange, Ranged};
use crate::dop::VarName;
use crate::dop::semantics::r#type::Type;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::module_id::ModuleId;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Component {tag_name} is not defined")]
    UndefinedComponent { tag_name: DocumentRange },

    #[error("Module {module} does not declare a type {type_name}")]
    UndeclaredType {
        module: ModuleId,
        type_name: TypeName,
        range: DocumentRange,
    },

    #[error("Module {module} was not found")]
    ModuleNotFound {
        module: String,
        range: DocumentRange,
    },

    #[error("Unused variable {var_name}")]
    UnusedVariable { var_name: DocumentRange },

    #[error(
        "Component {component} does not accept children (missing `children: TrustedHTML` parameter)"
    )]
    ComponentDoesNotAcceptChildren {
        component: String,
        range: DocumentRange,
    },

    #[error("Component {component} requires children")]
    MissingChildren {
        component: String,
        range: DocumentRange,
    },

    #[error("The `children` argument cannot be passed explicitly; use component children instead")]
    ChildrenArgNotAllowed { range: DocumentRange },

    #[error(
        "Optional children (`children: Option[TrustedHTML]`) must have a default value (e.g., `= None`)"
    )]
    OptionalChildrenRequiresDefault { range: DocumentRange },

    #[error(
        "Import cycle: {importer_module} imports from {imported_component} which creates a dependency cycle: {cycle_display}"
    )]
    ImportCycle {
        importer_module: String,
        imported_component: String,
        cycle_display: String,
        range: DocumentRange,
    },

    #[error("Expected boolean condition, got {found}")]
    ExpectedBooleanCondition {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Missing required parameter '{param}'")]
    MissingRequiredParameter {
        param: VarName,
        range: DocumentRange,
    },

    #[error("Component requires arguments: {args}")]
    MissingArguments { args: String, range: DocumentRange },

    #[error("Unexpected argument '{arg}'")]
    UnexpectedArgument { arg: String, range: DocumentRange },

    #[error("Argument '{arg_name}' of type {found} is incompatible with expected type {expected}")]
    ArgumentIsIncompatible {
        expected: Arc<Type>,
        found: Arc<Type>,
        arg_name: DocumentRange,
        expr_range: DocumentRange,
    },

    #[error("Default value for parameter '{param_name}' has type {found}, expected {expected}")]
    DefaultValueTypeMismatch {
        param_name: String,
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Expected String or Bool attribute, got {found}")]
    ExpectedStringOrBoolAttribute {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Can not iterate over {typ}")]
    CannotIterateOver {
        typ: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Range bound must be Int, got {found}")]
    RangeBoundTypeMismatch {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Let binding has type {found}, expected {expected}")]
    LetBindingTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Expected string for text expression, got {found}")]
    ExpectedStringForTextExpression {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, range: DocumentRange },

    #[error("Field '{field}' not found in record '{record_name}'")]
    FieldNotFoundInRecord {
        field: String,
        record_name: TypeName,
        range: DocumentRange,
    },

    #[error("{typ} can not be used as a record")]
    CannotUseAsRecord {
        typ: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Can not compare {left} to {right}")]
    CannotCompareTypes {
        left: Arc<Type>,
        right: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Negation operator can only be applied to Bool values")]
    NegationRequiresBoolean { range: DocumentRange },

    #[error("Numeric negation operator can only be applied to Int or Float values")]
    NumericNegationRequiresNumber { range: DocumentRange },

    #[error("Array elements must all have the same type, found {expected} and {found}")]
    ArrayTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Cannot infer type of empty array")]
    CannotInferEmptyArrayType { range: DocumentRange },

    #[error("Cannot infer type of None without context")]
    CannotInferNoneType { range: DocumentRange },

    #[error("Type {t} is not comparable")]
    TypeIsNotComparable { t: Arc<Type>, range: DocumentRange },

    #[error("&& operator can only be applied to Bool values")]
    LogicalAndRequiresBoolean { range: DocumentRange },

    #[error("|| operator can only be applied to Bool values")]
    LogicalOrRequiresBoolean { range: DocumentRange },

    #[error("Cannot add values of incompatible types: {left_type} + {right_type}")]
    IncompatibleTypesForAddition {
        left_type: Arc<Type>,
        right_type: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Cannot subtract values of incompatible types: {left_type} - {right_type}")]
    IncompatibleTypesForSubtraction {
        left_type: Arc<Type>,
        right_type: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Cannot multiply values of incompatible types: {left_type} * {right_type}")]
    IncompatibleTypesForMultiplication {
        left_type: Arc<Type>,
        right_type: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Type '{type_name}' is not defined")]
    UndefinedType {
        type_name: CheapString,
        range: DocumentRange,
    },

    #[error("Record type '{record_name}' is not defined")]
    UndefinedRecord {
        record_name: String,
        range: DocumentRange,
    },

    #[error("Record '{record_name}' is missing fields: {}", missing_fields.join(", "))]
    RecordMissingFields {
        record_name: String,
        missing_fields: Vec<String>,
        range: DocumentRange,
    },

    #[error("Unknown field '{field_name}' in record '{record_name}'")]
    RecordUnknownField {
        field_name: FieldName,
        record_name: String,
        range: DocumentRange,
    },

    #[error("Field '{field_name}' expects type {expected}, but got {found}")]
    RecordLiteralFieldTypeMismatch {
        field_name: FieldName,
        expected: Arc<Type>,
        found: Arc<Type>,
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

    #[error("Enum variant '{enum_name}::{variant_name}' is missing fields: {}", missing_fields.join(", "))]
    EnumVariantMissingFields {
        enum_name: String,
        variant_name: String,
        missing_fields: Vec<String>,
        range: DocumentRange,
    },

    #[error("Unknown field '{field_name}' in enum variant '{enum_name}::{variant_name}'")]
    EnumVariantUnknownField {
        enum_name: String,
        variant_name: String,
        field_name: String,
        range: DocumentRange,
    },

    #[error(
        "Field '{field_name}' in '{enum_name}::{variant_name}' expects type {expected}, but got {found}"
    )]
    EnumVariantFieldTypeMismatch {
        enum_name: String,
        variant_name: String,
        field_name: String,
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Match is not implemented for type {found}")]
    MatchNotImplementedForType {
        found: Arc<Type>,
        range: DocumentRange,
    },

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

    #[error("Match arms must all have the same type, expected {expected} but found {found}")]
    MatchArmTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
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

    #[error("Useless match expression: does not branch or bind any variables")]
    MatchUseless { range: DocumentRange },

    #[error("Unused binding '{name}' in match arm")]
    MatchUnusedBinding { name: String, range: DocumentRange },

    #[error("Variable {name} is already defined")]
    VariableAlreadyDefined { name: String, range: DocumentRange },

    #[error("Macro '{macro_name}' expects {expected} arguments, but got {actual}")]
    MacroArgumentTypeMismatch {
        macro_name: String,
        expected: String,
        actual: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Method '{method}' is not available on type {typ}")]
    MethodNotAvailable {
        method: String,
        typ: Arc<Type>,
        range: DocumentRange,
    },
}

impl TypeError {
    pub fn import_cycle(
        importer_module: &str,
        imported_component: &str,
        cycle: &[String],
        range: DocumentRange,
    ) -> Self {
        let cycle_display = if let Some(first) = cycle.first() {
            format!("{} → {}", cycle.join(" → "), first)
        } else {
            cycle.join(" → ")
        };

        TypeError::ImportCycle {
            importer_module: importer_module.to_string(),
            imported_component: imported_component.to_string(),
            cycle_display,
            range,
        }
    }

    pub fn missing_arguments(params: &[(VarName, Arc<Type>, bool)], range: DocumentRange) -> Self {
        // Only list required (non-default) parameters
        let args = params
            .iter()
            .filter(|(_, _, has_default)| !has_default)
            .map(|(name, _, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        TypeError::MissingArguments { args, range }
    }
}

impl Ranged for TypeError {
    fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedComponent { tag_name: range }
            | TypeError::UndeclaredType { range, .. }
            | TypeError::UnusedVariable { var_name: range }
            | TypeError::ModuleNotFound { range, .. }
            | TypeError::ComponentDoesNotAcceptChildren { range, .. }
            | TypeError::MissingChildren { range, .. }
            | TypeError::ChildrenArgNotAllowed { range, .. }
            | TypeError::OptionalChildrenRequiresDefault { range, .. }
            | TypeError::ImportCycle { range, .. }
            | TypeError::ExpectedBooleanCondition { range, .. }
            | TypeError::MissingRequiredParameter { range, .. }
            | TypeError::MissingArguments { range, .. }
            | TypeError::UnexpectedArgument { range, .. }
            | TypeError::ArgumentIsIncompatible {
                expr_range: range, ..
            }
            | TypeError::DefaultValueTypeMismatch { range, .. }
            | TypeError::ExpectedStringOrBoolAttribute { range, .. }
            | TypeError::CannotIterateOver { range, .. }
            | TypeError::RangeBoundTypeMismatch { range, .. }
            | TypeError::LetBindingTypeMismatch { range, .. }
            | TypeError::ExpectedStringForTextExpression { range, .. }
            | TypeError::UndefinedVariable { range, .. }
            | TypeError::FieldNotFoundInRecord { range, .. }
            | TypeError::CannotUseAsRecord { range, .. }
            | TypeError::CannotCompareTypes { range, .. }
            | TypeError::NegationRequiresBoolean { range, .. }
            | TypeError::NumericNegationRequiresNumber { range, .. }
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
            | TypeError::RecordMissingFields { range, .. }
            | TypeError::RecordUnknownField { range, .. }
            | TypeError::RecordLiteralFieldTypeMismatch { range, .. }
            | TypeError::UndefinedEnum { range, .. }
            | TypeError::UndefinedEnumVariant { range, .. }
            | TypeError::EnumVariantMissingFields { range, .. }
            | TypeError::EnumVariantUnknownField { range, .. }
            | TypeError::EnumVariantFieldTypeMismatch { range, .. }
            | TypeError::MatchNotImplementedForType { range, .. }
            | TypeError::MatchPatternEnumMismatch { range, .. }
            | TypeError::MatchPatternRecordMismatch { range, .. }
            | TypeError::MatchArmTypeMismatch { range, .. }
            | TypeError::MatchMissingVariants { range, .. }
            | TypeError::MatchUnreachableArm { range, .. }
            | TypeError::MatchPatternTypeMismatch { range, .. }
            | TypeError::MatchNoArms { range, .. }
            | TypeError::MatchUseless { range, .. }
            | TypeError::MatchUnusedBinding { range, .. }
            | TypeError::VariableAlreadyDefined { range, .. }
            | TypeError::MacroArgumentTypeMismatch { range, .. }
            | TypeError::MethodNotAvailable { range, .. } => range,
        }
    }
}
