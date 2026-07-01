use std::sync::Arc;

use crate::annotation::Annotation;
use crate::document::DocumentRange;
use crate::expr::parsing::parsed_expr::ParsedMatchPattern;
use crate::expr::typing::r#type::Type;
use crate::symbols::field_name::FieldName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct TypeError {
    kind: TypeErrorKind,
    range: DocumentRange,
}

impl TypeError {
    pub(crate) fn new(kind: TypeErrorKind, range: DocumentRange) -> Self {
        TypeError { kind, range }
    }

    pub(crate) fn import_cycle(
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

        TypeError::new(
            TypeErrorKind::ImportCycle {
                importer_module: importer_module.to_string(),
                imported_component: imported_component.to_string(),
                cycle_display,
            },
            range,
        )
    }

    pub(crate) fn severity(&self) -> crate::program::Severity {
        self.kind.severity()
    }

    pub(crate) fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl Annotation for TypeError {
    fn message(&self) -> String {
        self.kind.to_string()
    }
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

#[derive(Debug, Clone, Error)]
pub(crate) enum TypeErrorKind {
    #[error("Component {tag_name} is not defined")]
    UndefinedComponent { tag_name: TypeName },

    #[error("Module {module} does not declare a type {type_name}")]
    UndeclaredType {
        module: ModuleName,
        type_name: TypeName,
    },

    #[error("Type {type_name} from module {module} is not public")]
    NotPublic {
        module: ModuleName,
        type_name: TypeName,
    },

    #[error("Module {module} was not found")]
    ModuleNotFound { module: ModuleName },

    #[error("Unused variable {var_name}")]
    UnusedVariable { var_name: VarName },

    #[error("Unused import '{import_name}'")]
    UnusedImport { import_name: TypeName },

    #[error(
        "Component {component} does not accept content (missing `children: Fragment` parameter)"
    )]
    ComponentDoesNotAcceptChildren { component: TypeName },

    #[error("Content provided both as an explicit `children` argument and as element children")]
    ChildContentAmbiguous,

    #[error(
        "Import cycle: {importer_module} imports from {imported_component} which creates a dependency cycle: {cycle_display}"
    )]
    ImportCycle {
        importer_module: String,
        imported_component: String,
        cycle_display: String,
    },

    #[error("Mismatched type for condition: expected `Bool` got `{found}`")]
    ConditionTypeMismatch { found: Arc<Type> },

    #[error("Component requires arguments: {args}")]
    MissingArguments { args: String },

    #[error("Component `{component}` does not accept attribute `{attr}`")]
    ComponentDoesNotAcceptAttribute { component: TypeName, attr: String },

    #[error("Component {component} is recursive and cannot use rest parameters")]
    RecursiveComponentWithRest { component: TypeName },

    #[error("Rest cannot be forwarded into recursive component {component}")]
    RestForwardedIntoRecursive { component: TypeName },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    ArgumentTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    DefaultValueTypeMismatch {
        param_name: VarName,
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("`<{element}>` does not accept attribute `{attr}`")]
    ElementDoesNotAcceptAttribute { element: String, attr: String },

    #[error("Mismatched type: expected `Array[...]` got `{found}`")]
    IterateeTypeMismatch { found: Arc<Type> },

    #[error("Mismatched type for range bound: expected `Int` got `{found}`")]
    RangeBoundTypeMismatch { found: Arc<Type> },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    LetBindingTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Mismatched type for text expression: expected `String` got {found}")]
    TextExpressionTypeMismatch { found: Arc<Type> },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: VarName },

    #[error("Field '{field}' not found in record '{record_name}'")]
    FieldNotFoundInRecord {
        field: FieldName,
        record_name: TypeName,
    },

    #[error("{typ} can not be used as a record")]
    CannotUseAsRecord { typ: Arc<Type> },

    #[error("Cannot compare {left} to {right}")]
    CannotCompareTypes { left: Arc<Type>, right: Arc<Type> },

    #[error("Mismatched type for negation: expected `Bool` got `{found}`")]
    BooleanNegationTypeMismatch { found: Arc<Type> },

    #[error("Mismatched type for negation: expected `Int` or `Float` got {found}")]
    NumericNegationTypeMismatch { found: Arc<Type> },

    #[error("Mismatched type for array element: expected `{expected}` got `{found}`")]
    ArrayElementTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Cannot infer type of empty array")]
    CannotInferEmptyArrayType,

    #[error("Cannot infer type of None without context")]
    CannotInferNoneType,

    #[error("Type {t} is not comparable")]
    TypeIsNotComparable { t: Arc<Type> },

    #[error("&& operator can only be applied to Bool values")]
    LogicalAndTypeMismatch,

    #[error("|| operator can only be applied to Bool values")]
    LogicalOrTypeMismatch,

    #[error("Cannot add values of incompatible types: {left_type} + {right_type}")]
    IncompatibleTypesForAddition {
        left_type: Arc<Type>,
        right_type: Arc<Type>,
    },

    #[error("Cannot subtract values of incompatible types: {left_type} - {right_type}")]
    IncompatibleTypesForSubtraction {
        left_type: Arc<Type>,
        right_type: Arc<Type>,
    },

    #[error("Cannot multiply values of incompatible types: {left_type} * {right_type}")]
    IncompatibleTypesForMultiplication {
        left_type: Arc<Type>,
        right_type: Arc<Type>,
    },

    #[error("Type '{type_name}' is not defined")]
    UndefinedType { type_name: TypeName },

    #[error("`{name}` is a component and cannot be used as a type")]
    ComponentUsedAsType { name: TypeName },

    #[error("Record type '{record_name}' is not defined")]
    UndefinedRecord { record_name: TypeName },

    #[error("Record '{record_name}' is missing fields: {}", missing_fields.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "))]
    RecordMissingFields {
        record_name: TypeName,
        missing_fields: Vec<FieldName>,
    },

    #[error("Unknown field '{field_name}' in record '{record_name}'")]
    RecordUnknownField {
        field_name: FieldName,
        record_name: TypeName,
    },

    #[error("Mismatched type for `{field_name}`: expected `{expected}` got `{found}`")]
    RecordLiteralFieldTypeMismatch {
        field_name: FieldName,
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Enum type '{enum_name}' is not defined")]
    UndefinedEnum { enum_name: TypeName },

    #[error("Variant '{variant_name}' is not defined in enum '{enum_name}'")]
    UndefinedEnumVariant {
        enum_name: TypeName,
        variant_name: TypeName,
    },

    #[error("Enum variant '{enum_name}::{variant_name}' is missing fields: {}", missing_fields.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "))]
    EnumVariantMissingFields {
        enum_name: TypeName,
        variant_name: TypeName,
        missing_fields: Vec<FieldName>,
    },

    #[error("Unknown field '{field_name}' in enum variant '{enum_name}::{variant_name}'")]
    EnumVariantUnknownField {
        enum_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
    },

    #[error("Mismatched type for `{field_name}`: expected `{expected}` got `{found}`")]
    EnumVariantFieldTypeMismatch {
        enum_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Match is not implemented for type {found}")]
    MatchNotImplementedForType { found: Arc<Type> },

    #[error("Match pattern enum '{pattern_enum}' does not match subject enum '{subject_enum}'")]
    MatchPatternEnumMismatch {
        pattern_enum: TypeName,
        subject_enum: TypeName,
    },

    #[error(
        "Match pattern record '{pattern_record}' does not match subject record '{subject_record}'"
    )]
    MatchPatternRecordMismatch {
        pattern_record: TypeName,
        subject_record: TypeName,
    },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    MatchArmTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Match expression is missing arms for: {}", variants.join(", "))]
    MatchMissingVariants { variants: Vec<String> },

    #[error("Unreachable match arm for pattern '{pattern}'")]
    MatchUnreachableArm { pattern: Box<ParsedMatchPattern> },

    #[error("Mismatched pattern type: expected `{expected}` got `{found}`")]
    MatchPatternTypeMismatch {
        expected: Arc<Type>,
        // TODO: Make into Arc<Type>
        found: String,
    },

    #[error("Match expression must have at least one arm")]
    MatchNoArms,

    #[error("Useless match expression: does not branch or bind any variables")]
    MatchUseless,

    #[error("Unused binding '{name}' in match arm")]
    MatchUnusedBinding { name: VarName },

    #[error("Variable {name} is already defined")]
    VariableAlreadyDefined { name: VarName },

    #[error("Mismatched type for '{macro_name}': expected `{expected}` got `{found}`")]
    MacroArgumentTypeMismatch {
        macro_name: String,
        expected: Arc<Type>,
        found: Arc<Type>,
    },

    #[error("Method '{method}' is not available on type {typ}")]
    MethodNotAvailable { method: String, typ: Arc<Type> },

    #[error("#[examples(pattern = ...)] is only valid on String fields, found {found}")]
    PatternOnNonString { found: Arc<Type> },

    #[error("Invalid regex in #[examples(pattern = ...)]: {message}")]
    InvalidPatternRegex { message: String },

    #[error("#[examples(min = ..., max = ...)] is only valid on Int fields, found {found}")]
    MinMaxOnNonInt { found: Arc<Type> },

    #[error("#[examples(min = {min})] must be less than or equal to max = {max}")]
    MinGreaterThanMax { min: i64, max: i64 },

    #[error(
        "#[examples(min_len = ..., max_len = ...)] is only valid on Array fields, found {found}"
    )]
    MinMaxLenOnNonArray { found: Arc<Type> },

    #[error("#[examples(min_len = ..., max_len = ...)] must be non-negative, found {value}")]
    NegativeLen { value: i64 },

    #[error("#[examples(min_len = {min_len})] must be less than or equal to max_len = {max_len}")]
    MinLenGreaterThanMaxLen { min_len: i64, max_len: i64 },

    #[error("asset! takes exactly one argument, got {actual}")]
    AssetMacroArity { actual: usize },

    #[error("asset! argument must be a string literal")]
    AssetMacroNonLiteralArg,

    #[error("asset! path must start with '/'")]
    AssetPathMustBeAbsolute,
}

impl TypeErrorKind {
    pub fn severity(&self) -> crate::program::Severity {
        use crate::program::Severity;
        match self {
            TypeErrorKind::UnusedVariable { .. }
            | TypeErrorKind::UnusedImport { .. }
            | TypeErrorKind::MatchUnusedBinding { .. } => Severity::Warning,
            _ => Severity::Error,
        }
    }
}
