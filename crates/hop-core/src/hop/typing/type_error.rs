use crate::diagnostic::Diagnostic;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::{CheapString, DocumentRange};
use crate::hop::typing::{Type, TypedMatchPattern};
use crate::root_relative_path::RootRelativePathError;
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
    pub fn new(kind: TypeErrorKind, range: DocumentRange) -> Self {
        TypeError { kind, range }
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

        TypeError::new(
            TypeErrorKind::ImportCycle {
                importer_module: importer_module.to_string(),
                imported_component: imported_component.to_string(),
                cycle_display,
            },
            range,
        )
    }

    pub fn severity(&self) -> DiagnosticSeverity {
        self.kind.severity()
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            message: self.kind.to_string(),
            range: self.range.clone(),
            severity: self.kind.severity(),
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum TypeErrorKind {
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

    #[error("<{tag}> is a void element and cannot have content")]
    VoidElementWithContent { tag: CheapString },

    #[error("Unused import '{import_name}'")]
    UnusedImport { import_name: CheapString },

    #[error("Function {name} does not accept content (missing 'children: Html' parameter)")]
    FunctionDoesNotAcceptChildren { name: FunctionName },

    #[error("Content provided both as a 'children' attribute and between the tags")]
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

    #[error("Html is not allowed in page parameters")]
    HtmlInPageParameter,

    // Named types print without their module, so two different types can
    // print the same.
    #[error(
        "Expected {expected} got {found}{}",
        if expected != found && expected.to_string() == found.to_string() {
            " (these are different types with the same name)"
        } else {
            ""
        }
    )]
    TypeMismatch {
        context: TypeMismatchContext,
        expected: Type,
        found: Type,
    },

    #[error("Only a function returning Html can be called by a markup call")]
    FunctionTagReturnTypeMismatch { name: FunctionName, found: Type },

    #[error("Expected Array[...] got {found}")]
    IterateeTypeMismatch { found: Type },

    #[error("Expected String or Html got {found}")]
    InterpolationTypeMismatch { found: Type },

    #[error("Expected Int or Float got {found}")]
    NumericNegationTypeMismatch { found: Type },

    #[error("Pattern does not match type {expected}")]
    MatchPatternTypeMismatch { expected: Type },

    #[error("<{element}> does not accept attribute '{attr}'")]
    ElementDoesNotAcceptAttribute { element: String, attr: String },

    #[error("<{element}> requires a string literal for attribute '{attr}'")]
    AttributeRequiresStringLiteral { element: String, attr: String },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: VarName },

    #[error("Field '{field}' not found in record '{type_name}'")]
    FieldNotFoundInRecord {
        field: FieldName,
        type_name: TypeName,
    },

    #[error("{typ} can not be used as a record")]
    CannotUseAsRecord { typ: Type },

    #[error("Cannot compare {left} to {right}")]
    CannotCompareTypes { left: Type, right: Type },

    #[error("Cannot infer type of []")]
    CannotInferEmptyArrayType,

    #[error("Cannot infer type of None")]
    CannotInferNoneType,

    #[error("Type {t} is not comparable")]
    TypeIsNotComparable { t: Type },

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

    #[error("Record '{type_name}' is missing fields: {}", missing_fields.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "))]
    RecordMissingFields {
        type_name: TypeName,
        missing_fields: Vec<FieldName>,
    },

    #[error("Unknown field '{field_name}' in record '{type_name}'")]
    RecordUnknownField {
        field_name: FieldName,
        type_name: TypeName,
    },

    #[error("Duplicate field '{field_name}' in record '{type_name}'")]
    RecordDuplicateField {
        field_name: FieldName,
        type_name: TypeName,
    },

    #[error("Variant '{variant_name}' is not defined in enum '{type_name}'")]
    UndefinedEnumVariant {
        type_name: TypeName,
        variant_name: TypeName,
    },

    #[error("Enum variant '{type_name}::{variant_name}' is missing fields: {}", missing_fields.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "))]
    EnumVariantMissingFields {
        type_name: TypeName,
        variant_name: TypeName,
        missing_fields: Vec<FieldName>,
    },

    #[error("Unknown field '{field_name}' in enum variant '{type_name}::{variant_name}'")]
    EnumVariantUnknownField {
        type_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
    },

    #[error("Duplicate field '{field_name}' in enum variant '{type_name}::{variant_name}'")]
    EnumVariantDuplicateField {
        type_name: TypeName,
        variant_name: TypeName,
        field_name: FieldName,
    },

    #[error("Match is not implemented for type {found}")]
    MatchNotImplementedForType { found: Type },

    #[error("Missing pattern(s) {}", patterns.join(", "))]
    MatchMissingPattern { patterns: Vec<String> },

    #[error("Unreachable pattern {pattern}")]
    MatchUnreachablePattern { pattern: Box<TypedMatchPattern> },

    #[error("Match expression must have at least one arm")]
    MatchNoArms,

    #[error("Useless match expression: does not branch or bind any variables")]
    MatchUseless,

    #[error("Variable {name} is already defined")]
    VariableAlreadyDefined { name: VarName },

    #[error("Variable {name} is bound more than once in the pattern")]
    DuplicatePatternBinding { name: VarName },

    #[error("Duplicate parameter '{name}'")]
    DuplicateParameter { name: VarName },

    #[error("{name} is already defined")]
    NameIsAlreadyDefined { name: CheapString },

    #[error("Method '{method}' is not available on type {typ}")]
    MethodNotAvailable { method: FieldName, typ: Type },

    #[error("Method '{method}' expects {expected} argument(s), got {found}")]
    MethodArgumentCountMismatch {
        method: FieldName,
        expected: usize,
        found: usize,
    },

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

    #[error("invalid asset! path: {source}")]
    InvalidAssetPath { source: RootRelativePathError },

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

    #[error("Function {name} does not accept argument '{argument}'")]
    FunctionDoesNotAcceptArgument {
        name: FunctionName,
        argument: VarName,
    },

    #[error("Argument '{argument}' is supplied more than once")]
    DuplicateArgument { argument: VarName },
}

/// Where a [`TypeErrorKind::TypeMismatch`] was found.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeMismatchContext {
    /// The default value of a parameter.
    DefaultValue,
    /// The value of a let binding with a declared type.
    LetBinding,
    /// The body of a match arm, compared to the earlier arms.
    MatchArm,
    /// The start or end of a range.
    RangeBound,
    /// The body of a for loop.
    ForBody,
    /// The value of an element attribute.
    Attribute,
    /// An element of an array literal, compared to the first element.
    ArrayElement,
    /// The operand of a boolean negation.
    BooleanNegation,
    /// An operand of a logical and.
    LogicalAnd,
    /// An operand of a logical or.
    LogicalOr,
    /// The value of a field in a record literal.
    RecordLiteralField,
    /// The value of a field in an enum variant literal.
    EnumVariantField,
    /// The subject of a record spread.
    RecordSpread,
    /// The body of a function, compared to its return type.
    FunctionBody,
    /// An argument of a macro.
    MacroArgument,
    /// An argument of a method call.
    MethodArgument,
    /// An argument of a function call.
    FunctionArgument,
}

impl TypeErrorKind {
    pub fn severity(&self) -> DiagnosticSeverity {
        match self {
            TypeErrorKind::UnusedVariable { .. } | TypeErrorKind::UnusedImport { .. } => {
                DiagnosticSeverity::Warning
            }
            _ => DiagnosticSeverity::Error,
        }
    }
}
