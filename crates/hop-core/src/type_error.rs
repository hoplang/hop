use crate::annotation::Annotation;
use crate::document::{CheapString, DocumentRange};
use crate::hop::patterns::typed::TypedMatchPattern;
use crate::hop::typing::r#type::Type;
use crate::program::Severity;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
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

    pub(crate) fn severity(&self) -> Severity {
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
    #[error("Module {module} does not declare {name}")]
    UndeclaredName {
        module: ModuleName,
        name: CheapString,
    },

    #[error("{name} from module {module} is not public")]
    NotPublic {
        module: ModuleName,
        name: CheapString,
    },

    #[error("Module {module} was not found")]
    ModuleNotFound { module: ModuleName },

    #[error("Unused variable {var_name}")]
    UnusedVariable { var_name: VarName },

    #[error("<{tag}> is not allowed here")]
    HtmlStructureTagNotAllowed { tag: &'static str },

    #[error("Unused import '{import_name}'")]
    UnusedImport { import_name: CheapString },

    #[error("Function {name} does not accept content (missing 'children: Html' parameter)")]
    FunctionDoesNotAcceptChildren { name: FunctionName },

    #[error("Only a function returning Html can be invoked as a tag")]
    FunctionTagReturnTypeMismatch { name: FunctionName, found: Type },

    #[error("Content provided both as an explicit 'children' argument and as element children")]
    ChildContentAmbiguous,

    #[error(
        "Import cycle: {importer_module} imports from {imported_component} which creates a dependency cycle: {cycle_display}"
    )]
    ImportCycle {
        importer_module: String,
        imported_component: String,
        cycle_display: String,
    },

    #[error("Function {name} requires arguments: {args}")]
    MissingArguments { name: FunctionName, args: String },

    #[error("Function {name} does not accept attribute '{attr}'")]
    FunctionDoesNotAcceptAttribute { name: FunctionName, attr: String },

    #[error("Mismatched type for attribute: expected String got {found}")]
    AttributeTypeMismatch { found: Type },

    #[error("Rest spread of {name} forms a cycle and never reaches an element")]
    RestSpreadCycle { name: FunctionName },

    #[error("Function {function} declares rest parameter '{name}' but never spreads it")]
    RestNeverSpread {
        function: FunctionName,
        name: VarName,
    },

    #[error("Rest parameter '{name}' is spread more than once")]
    RestSpreadMoreThanOnce { name: VarName },

    #[error("Spread '...{name}' does not refer to a declared rest parameter")]
    SpreadNotDeclaredRest { name: VarName },

    #[error("Default values must be constant")]
    DefaultValueMustBeConstant,

    #[error("Mismatched type: expected {expected} got {found}")]
    DefaultValueTypeMismatch {
        param_name: VarName,
        expected: Type,
        found: Type,
    },

    #[error("<{element}> does not accept attribute '{attr}'")]
    ElementDoesNotAcceptAttribute { element: String, attr: String },

    #[error("Mismatched type: expected Array[...] got {found}")]
    IterateeTypeMismatch { found: Type },

    #[error("Mismatched type for range bound: expected Int got {found}")]
    RangeBoundTypeMismatch { found: Type },

    #[error("Mismatched type for for body: expected Html got {found}")]
    ForBodyTypeMismatch { found: Type },

    #[error("Mismatched type: expected {expected} got {found}")]
    LetBindingTypeMismatch { expected: Type, found: Type },

    #[error("Mismatched type for interpolation: expected String or Html got {found}")]
    InterpolationTypeMismatch { found: Type },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: VarName },

    #[error("Field '{field}' not found in record '{record_name}'")]
    FieldNotFoundInRecord {
        field: FieldName,
        record_name: TypeName,
    },

    #[error("{typ} can not be used as a record")]
    CannotUseAsRecord { typ: Type },

    #[error("Cannot compare {left} to {right}")]
    CannotCompareTypes { left: Type, right: Type },

    #[error("Mismatched type for negation: expected Bool got {found}")]
    BooleanNegationTypeMismatch { found: Type },

    #[error("Mismatched type for negation: expected Int or Float got {found}")]
    NumericNegationTypeMismatch { found: Type },

    #[error("Mismatched type for array element: expected {expected} got {found}")]
    ArrayElementTypeMismatch { expected: Type, found: Type },

    #[error("Cannot infer type of empty array")]
    CannotInferEmptyArrayType,

    #[error("Cannot infer type of None without context")]
    CannotInferNoneType,

    #[error("Type {t} is not comparable")]
    TypeIsNotComparable { t: Type },

    #[error("&& operator can only be applied to Bool values")]
    LogicalAndTypeMismatch,

    #[error("|| operator can only be applied to Bool values")]
    LogicalOrTypeMismatch,

    #[error("Cannot add values of incompatible types: {left_type} + {right_type}")]
    IncompatibleTypesForAddition { left_type: Type, right_type: Type },

    #[error("Cannot subtract values of incompatible types: {left_type} - {right_type}")]
    IncompatibleTypesForSubtraction { left_type: Type, right_type: Type },

    #[error("Cannot multiply values of incompatible types: {left_type} * {right_type}")]
    IncompatibleTypesForMultiplication { left_type: Type, right_type: Type },

    #[error("Type '{type_name}' is not defined")]
    UndefinedType { type_name: TypeName },

    #[error("'{name}' is a function and cannot be used as a type")]
    FunctionUsedAsType { name: TypeName },

    #[error("'{name}' is a page and cannot be used as a type")]
    PageUsedAsType { name: TypeName },

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

    #[error("Mismatched type for '{field_name}': expected {expected} got {found}")]
    RecordLiteralFieldTypeMismatch {
        field_name: FieldName,
        expected: Type,
        found: Type,
    },

    #[error("Duplicate field '{field_name}' in record literal for '{record_name}'")]
    RecordDuplicateField {
        field_name: FieldName,
        record_name: TypeName,
    },

    #[error("Mismatched type for spread: expected {expected} got {found}")]
    RecordSpreadTypeMismatch { expected: Type, found: Type },

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

    #[error("Mismatched type for '{field_name}': expected {expected} got {found}")]
    EnumVariantFieldTypeMismatch {
        enum_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
        expected: Type,
        found: Type,
    },

    #[error(
        "Duplicate field '{field_name}' in enum variant literal for '{enum_name}::{variant_name}'"
    )]
    EnumVariantDuplicateField {
        enum_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
    },

    #[error("Match is not implemented for type {found}")]
    MatchNotImplementedForType { found: Type },

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

    #[error("Mismatched type: expected {expected} got {found}")]
    MatchArmTypeMismatch { expected: Type, found: Type },

    #[error("Match expression is missing arms for: {}", variants.join(", "))]
    MatchMissingVariants { variants: Vec<String> },

    #[error("Unreachable match arm for pattern '{pattern}'")]
    MatchUnreachableArm { pattern: Box<TypedMatchPattern> },

    #[error("Mismatched pattern type: expected {expected} got {found}")]
    MatchPatternTypeMismatch {
        expected: Type,
        // TODO: Make into Type
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

    #[error("Duplicate parameter '{name}'")]
    DuplicateParameter { name: VarName },

    #[error("{name} is already defined")]
    NameIsAlreadyDefined { name: CheapString },

    #[error("Mismatched type for '{macro_name}': expected {expected} got {found}")]
    MacroArgumentTypeMismatch {
        macro_name: String,
        expected: Type,
        found: Type,
    },

    #[error("Method '{method}' is not available on type {typ}")]
    MethodNotAvailable { method: FieldName, typ: Type },

    #[error("Method '{method}' takes no arguments, got {found}")]
    MethodTakesNoArguments { method: FieldName, found: usize },

    #[error("#[examples(pattern = ...)] is only valid on String fields, found {found}")]
    PatternOnNonString { found: Type },

    #[error("Invalid regex in #[examples(pattern = ...)]: {message}")]
    InvalidPatternRegex { message: String },

    #[error("Invalid escape sequence '\\{ch}'")]
    InvalidEscapeSequence { ch: char },

    #[error("#[examples(min = ..., max = ...)] is only valid on Int fields, found {found}")]
    MinMaxOnNonInt { found: Type },

    #[error("#[examples(min = {min})] must be less than or equal to max = {max}")]
    MinGreaterThanMax { min: i32, max: i32 },

    #[error(
        "#[examples(min_len = ..., max_len = ...)] is only valid on Array fields, found {found}"
    )]
    MinMaxLenOnNonArray { found: Type },

    #[error("#[examples(min_len = ..., max_len = ...)] must be non-negative, found {value}")]
    NegativeLen { value: i32 },

    #[error("#[examples(min_len = {min_len})] must be less than or equal to max_len = {max_len}")]
    MinLenGreaterThanMaxLen { min_len: i32, max_len: i32 },

    #[error("Unknown macro '{name}'")]
    UnknownMacro { name: CheapString },

    #[error("asset! takes exactly one argument, got {actual}")]
    AssetMacroArity { actual: usize },

    #[error("asset! argument must be a string literal")]
    AssetMacroNonLiteralArg,

    #[error("asset! path must start with '/'")]
    AssetPathMustBeAbsolute,

    #[error("format! requires a string literal as its first argument")]
    FormatMacroNonLiteralTemplate,

    #[error("format! only supports '{{}}' placeholders")]
    FormatMacroInvalidPlaceholder,

    #[error("format! expects {expected} argument(s) for the format string, got {found}")]
    FormatMacroArity { expected: usize, found: usize },

    #[error("format! arguments must be String or Int, got {found}")]
    FormatMacroUnsupportedArgument { found: Type },

    #[error("Function {name} is not defined")]
    UndefinedFunction { name: FunctionName },

    #[error("Function '{name}' expects {expected} argument(s), got {found}")]
    FunctionArgumentCountMismatch {
        name: FunctionName,
        expected: String,
        found: usize,
    },

    #[error(
        "Mismatched type for argument '{param_name}' of function '{name}': expected {expected} got {found}"
    )]
    FunctionArgumentTypeMismatch {
        name: FunctionName,
        param_name: VarName,
        expected: Type,
        found: Type,
    },

    #[error("Function {name} does not accept argument '{argument}'")]
    FunctionDoesNotAcceptArgument {
        name: FunctionName,
        argument: VarName,
    },

    #[error("Argument '{argument}' is supplied more than once")]
    DuplicateArgument { argument: VarName },

    #[error("Mismatched type for function body: expected {expected} got {found}")]
    FunctionBodyTypeMismatch { expected: Type, found: Type },

    #[error("Mismatched type for declaration: expected Html got {found}")]
    DeclarationBodyTypeMismatch { found: Type },
}

impl TypeErrorKind {
    pub fn severity(&self) -> Severity {
        use crate::program::Severity;
        match self {
            TypeErrorKind::UnusedVariable { .. }
            | TypeErrorKind::UnusedImport { .. }
            | TypeErrorKind::MatchUnusedBinding { .. } => Severity::Warning,
            _ => Severity::Error,
        }
    }
}
