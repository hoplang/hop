use std::sync::Arc;

use crate::annotation::Annotation;
use crate::document::DocumentRange;
use crate::dop::TypedExpr;
use crate::dop::typing::r#type::Type;
use crate::symbols::field_name::FieldName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Component {tag_name} is not defined")]
    UndefinedComponent {
        tag_name: TypeName,
        range: DocumentRange,
    },

    #[error("Module {module} does not declare a type {type_name}")]
    UndeclaredType {
        module: ModuleName,
        type_name: TypeName,
        range: DocumentRange,
    },

    #[error("Type {type_name} from module {module} is not public")]
    NotPublic {
        module: ModuleName,
        type_name: TypeName,
        range: DocumentRange,
    },

    #[error("Module {module} was not found")]
    ModuleNotFound {
        module: ModuleName,
        range: DocumentRange,
    },

    #[error("Unused variable {var_name}")]
    UnusedVariable {
        var_name: VarName,
        range: DocumentRange,
    },

    #[error("Unused import '{import_name}'")]
    UnusedImport {
        import_name: TypeName,
        range: DocumentRange,
    },

    #[error("Component {component} does not accept slot content (missing `slot: Fragment` parameter)")]
    ComponentDoesNotAcceptChildren {
        component: TypeName,
        range: DocumentRange,
    },

    #[error("Component {component} requires slot content")]
    MissingChildren {
        component: TypeName,
        range: DocumentRange,
    },

    #[error("The slot argument cannot be passed explicitly")]
    ChildrenArgNotAllowed { range: DocumentRange },

    #[error("The slot parameter must be typed as Fragment")]
    SlotMustHaveFragmentType { range: DocumentRange },

    #[error("A recursive component cannot declare a slot")]
    RecursiveComponentCannotHaveSlot { range: DocumentRange },

    #[error(
        "`{{slot}}` can only be used inside a component that declares a `slot: Fragment` parameter"
    )]
    SlotOutsideSlottedComponent { range: DocumentRange },

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
        range: DocumentRange,
    },

    #[error("Default value for parameter '{param_name}' has type {found}, expected {expected}")]
    DefaultValueTypeMismatch {
        param_name: VarName,
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Expected String, Bool, or Option[String] attribute, got {found}")]
    InvalidAttributeType {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Attributes starting with 'on' are not allowed")]
    DisallowedEventHandlerAttribute { range: DocumentRange },

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
    UndefinedVariable { name: VarName, range: DocumentRange },

    #[error("Field '{field}' not found in record '{record_name}'")]
    FieldNotFoundInRecord {
        field: FieldName,
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
        type_name: TypeName,
        range: DocumentRange,
    },

    #[error("Record type '{record_name}' is not defined")]
    UndefinedRecord {
        record_name: TypeName,
        range: DocumentRange,
    },

    #[error("Record '{record_name}' is missing fields: {}", missing_fields.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "))]
    RecordMissingFields {
        record_name: TypeName,
        missing_fields: Vec<FieldName>,
        range: DocumentRange,
    },

    #[error("Unknown field '{field_name}' in record '{record_name}'")]
    RecordUnknownField {
        field_name: FieldName,
        record_name: TypeName,
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
        enum_name: TypeName,
        range: DocumentRange,
    },

    #[error("Variant '{variant_name}' is not defined in enum '{enum_name}'")]
    UndefinedEnumVariant {
        enum_name: TypeName,
        variant_name: TypeName,
        range: DocumentRange,
    },

    #[error("Enum variant '{enum_name}::{variant_name}' is missing fields: {}", missing_fields.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "))]
    EnumVariantMissingFields {
        enum_name: TypeName,
        variant_name: TypeName,
        missing_fields: Vec<FieldName>,
        range: DocumentRange,
    },

    #[error("Unknown field '{field_name}' in enum variant '{enum_name}::{variant_name}'")]
    EnumVariantUnknownField {
        enum_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
        range: DocumentRange,
    },

    #[error(
        "Field '{field_name}' in '{enum_name}::{variant_name}' expects type {expected}, but got {found}"
    )]
    EnumVariantFieldTypeMismatch {
        enum_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
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
        pattern_enum: TypeName,
        subject_enum: TypeName,
        range: DocumentRange,
    },

    #[error(
        "Match pattern record '{pattern_record}' does not match subject record '{subject_record}'"
    )]
    MatchPatternRecordMismatch {
        pattern_record: TypeName,
        subject_record: TypeName,
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
    MatchUnusedBinding { name: VarName, range: DocumentRange },

    #[error("Variable {name} is already defined")]
    VariableAlreadyDefined { name: VarName, range: DocumentRange },

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

    #[error("#[examples(pattern = ...)] is only valid on String fields, found {found}")]
    PatternOnNonString {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Invalid regex in #[examples(pattern = ...)]: {message}")]
    InvalidPatternRegex {
        message: String,
        range: DocumentRange,
    },

    #[error("#[examples(min = ..., max = ...)] is only valid on Int fields, found {found}")]
    MinMaxOnNonInt {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("#[examples(min = {min})] must be less than or equal to max = {max}")]
    MinGreaterThanMax {
        min: i64,
        max: i64,
        range: DocumentRange,
    },

    #[error(
        "#[examples(min_len = ..., max_len = ...)] is only valid on Array fields, found {found}"
    )]
    MinMaxLenOnNonArray {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("#[examples(min_len = ..., max_len = ...)] must be non-negative, found {value}")]
    NegativeLen { value: i64, range: DocumentRange },

    #[error("#[examples(min_len = {min_len})] must be less than or equal to max_len = {max_len}")]
    MinLenGreaterThanMaxLen {
        min_len: i64,
        max_len: i64,
        range: DocumentRange,
    },

    #[error("asset! takes exactly one argument, got {actual}")]
    AssetMacroArity { actual: usize, range: DocumentRange },

    #[error("asset! argument must be a string literal")]
    AssetMacroNonLiteralArg { range: DocumentRange },

    #[error("asset! path must start with '/'")]
    AssetPathMustBeAbsolute { range: DocumentRange },
}

impl TypeError {
    pub fn severity(&self) -> crate::program::Severity {
        use crate::program::Severity;
        match self {
            TypeError::UnusedVariable { .. }
            | TypeError::UnusedImport { .. }
            | TypeError::MatchUnusedBinding { .. } => Severity::Warning,
            _ => Severity::Error,
        }
    }

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

    pub fn missing_arguments(
        params: &[(VarName, Arc<Type>, Option<TypedExpr>)],
        range: DocumentRange,
    ) -> Self {
        // Only list required (non-default) parameters
        let args = params
            .iter()
            .filter(|(_, _, default)| default.is_none())
            .map(|(name, _, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        TypeError::MissingArguments { args, range }
    }

    pub fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedComponent { range, .. }
            | TypeError::UndeclaredType { range, .. }
            | TypeError::NotPublic { range, .. }
            | TypeError::UnusedVariable { range, .. }
            | TypeError::UnusedImport { range, .. }
            | TypeError::ModuleNotFound { range, .. }
            | TypeError::ComponentDoesNotAcceptChildren { range, .. }
            | TypeError::MissingChildren { range, .. }
            | TypeError::ChildrenArgNotAllowed { range, .. }
            | TypeError::SlotMustHaveFragmentType { range, .. }
            | TypeError::RecursiveComponentCannotHaveSlot { range, .. }
            | TypeError::SlotOutsideSlottedComponent { range, .. }
            | TypeError::ImportCycle { range, .. }
            | TypeError::ExpectedBooleanCondition { range, .. }
            | TypeError::MissingRequiredParameter { range, .. }
            | TypeError::MissingArguments { range, .. }
            | TypeError::UnexpectedArgument { range, .. }
            | TypeError::ArgumentIsIncompatible { range, .. }
            | TypeError::DefaultValueTypeMismatch { range, .. }
            | TypeError::InvalidAttributeType { range, .. }
            | TypeError::DisallowedEventHandlerAttribute { range, .. }
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
            | TypeError::MethodNotAvailable { range, .. }
            | TypeError::PatternOnNonString { range, .. }
            | TypeError::InvalidPatternRegex { range, .. }
            | TypeError::MinMaxOnNonInt { range, .. }
            | TypeError::MinGreaterThanMax { range, .. }
            | TypeError::MinMaxLenOnNonArray { range, .. }
            | TypeError::NegativeLen { range, .. }
            | TypeError::MinLenGreaterThanMaxLen { range, .. }
            | TypeError::AssetMacroArity { range, .. }
            | TypeError::AssetMacroNonLiteralArg { range, .. }
            | TypeError::AssetPathMustBeAbsolute { range, .. } => range,
        }
    }
}

impl Annotation for TypeError {
    fn message(&self) -> String {
        self.to_string()
    }
    fn range(&self) -> &DocumentRange {
        self.range()
    }
}
