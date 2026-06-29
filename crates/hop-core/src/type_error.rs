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

    #[error(
        "Component {component} does not accept slot content (missing `slot: Fragment` parameter)"
    )]
    ComponentDoesNotAcceptChildren {
        component: TypeName,
        range: DocumentRange,
    },

    #[error("slot content provided both as an explicit `slot` argument and as element children")]
    SlotContentAmbiguous { range: DocumentRange },

    #[error(
        "Import cycle: {importer_module} imports from {imported_component} which creates a dependency cycle: {cycle_display}"
    )]
    ImportCycle {
        importer_module: String,
        imported_component: String,
        cycle_display: String,
        range: DocumentRange,
    },

    #[error("Mismatched type for condition: expected `Bool` got `{found}`")]
    ConditionTypeMismatch {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Component requires arguments: {args}")]
    MissingArguments { args: String, range: DocumentRange },

    #[error("Component `{component}` does not accept attribute `{attr}`")]
    ComponentDoesNotAcceptAttribute {
        component: TypeName,
        attr: String,
        range: DocumentRange,
    },

    #[error("Component {component} is recursive and cannot use rest parameters")]
    RecursiveComponentWithRest {
        component: TypeName,
        range: DocumentRange,
    },

    #[error("Rest cannot be forwarded into recursive component {component}")]
    RestForwardedIntoRecursive {
        component: TypeName,
        range: DocumentRange,
    },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    ArgumentTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    DefaultValueTypeMismatch {
        param_name: VarName,
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("`<{element}>` does not accept attribute `{attr}`")]
    ElementDoesNotAcceptAttribute {
        element: String,
        attr: String,
        range: DocumentRange,
    },

    #[error("Mismatched type: expected `Array[...]` got `{found}`")]
    IterateeTypeMismatch {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type for range bound: expected `Int` got `{found}`")]
    RangeBoundTypeMismatch {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
    LetBindingTypeMismatch {
        expected: Arc<Type>,
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type for text expression: expected `String` got {found}")]
    TextExpressionTypeMismatch {
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

    #[error("Cannot compare {left} to {right}")]
    CannotCompareTypes {
        left: Arc<Type>,
        right: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type for negation: expected `Bool` got `{found}`")]
    BooleanNegationTypeMismatch {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type for negation: expected `Int` or `Float` got {found}")]
    NumericNegationTypeMismatch {
        found: Arc<Type>,
        range: DocumentRange,
    },

    #[error("Mismatched type for array element: expected `{expected}` got `{found}`")]
    ArrayElementTypeMismatch {
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
    LogicalAndTypeMismatch { range: DocumentRange },

    #[error("|| operator can only be applied to Bool values")]
    LogicalOrTypeMismatch { range: DocumentRange },

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

    #[error("`{name}` is a component and cannot be used as a type")]
    ComponentUsedAsType {
        name: TypeName,
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

    #[error("Mismatched type for `{field_name}`: expected `{expected}` got `{found}`")]
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

    #[error("Mismatched type for `{field_name}`: expected `{expected}` got `{found}`")]
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

    #[error("Mismatched type: expected `{expected}` got `{found}`")]
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

    #[error("Unreachable match arm for pattern '{pattern}'")]
    MatchUnreachableArm {
        pattern: Box<ParsedMatchPattern>,
        range: DocumentRange,
    },

    #[error("Mismatched pattern type: expected `{expected}` got `{found}`")]
    MatchPatternTypeMismatch {
        expected: Arc<Type>,
        // TODO: Make into Arc<Type>
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

    #[error("Mismatched type for '{macro_name}': expected `{expected}` got `{found}`")]
    MacroArgumentTypeMismatch {
        macro_name: String,
        expected: Arc<Type>,
        found: Arc<Type>,
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

    pub fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedComponent { range, .. }
            | TypeError::UndeclaredType { range, .. }
            | TypeError::NotPublic { range, .. }
            | TypeError::UnusedVariable { range, .. }
            | TypeError::UnusedImport { range, .. }
            | TypeError::ModuleNotFound { range, .. }
            | TypeError::ComponentDoesNotAcceptChildren { range, .. }
            | TypeError::SlotContentAmbiguous { range, .. }
            | TypeError::ImportCycle { range, .. }
            | TypeError::ConditionTypeMismatch { range, .. }
            | TypeError::MissingArguments { range, .. }
            | TypeError::ComponentDoesNotAcceptAttribute { range, .. }
            | TypeError::RecursiveComponentWithRest { range, .. }
            | TypeError::RestForwardedIntoRecursive { range, .. }
            | TypeError::ArgumentTypeMismatch { range, .. }
            | TypeError::DefaultValueTypeMismatch { range, .. }
            | TypeError::ElementDoesNotAcceptAttribute { range, .. }
            | TypeError::IterateeTypeMismatch { range, .. }
            | TypeError::RangeBoundTypeMismatch { range, .. }
            | TypeError::LetBindingTypeMismatch { range, .. }
            | TypeError::TextExpressionTypeMismatch { range, .. }
            | TypeError::UndefinedVariable { range, .. }
            | TypeError::FieldNotFoundInRecord { range, .. }
            | TypeError::CannotUseAsRecord { range, .. }
            | TypeError::CannotCompareTypes { range, .. }
            | TypeError::BooleanNegationTypeMismatch { range, .. }
            | TypeError::NumericNegationTypeMismatch { range, .. }
            | TypeError::ArrayElementTypeMismatch { range, .. }
            | TypeError::CannotInferEmptyArrayType { range, .. }
            | TypeError::CannotInferNoneType { range, .. }
            | TypeError::TypeIsNotComparable { range, .. }
            | TypeError::LogicalAndTypeMismatch { range, .. }
            | TypeError::LogicalOrTypeMismatch { range, .. }
            | TypeError::IncompatibleTypesForAddition { range, .. }
            | TypeError::IncompatibleTypesForSubtraction { range, .. }
            | TypeError::IncompatibleTypesForMultiplication { range, .. }
            | TypeError::UndefinedType { range, .. }
            | TypeError::ComponentUsedAsType { range, .. }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentCursor;
    use crate::document_annotator::DocumentAnnotator;
    use crate::document_id::DocumentId;
    use expect_test::expect;

    #[test]
    fn type_mismatch_errors_use_consistent_wording() {
        let document_id = DocumentId::new("test.hop").unwrap();
        let range = DocumentCursor::new(document_id.clone(), "value".to_string()).range();

        let expected = || Arc::new(Type::String);
        let found = || Arc::new(Type::Int);

        let errors = vec![
            TypeError::ArgumentTypeMismatch {
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::DefaultValueTypeMismatch {
                param_name: VarName::new("x").unwrap(),
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::LetBindingTypeMismatch {
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::ArrayElementTypeMismatch {
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::RecordLiteralFieldTypeMismatch {
                field_name: FieldName::new("field").unwrap(),
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::EnumVariantFieldTypeMismatch {
                enum_name: TypeName::new("Shape").unwrap(),
                variant_name: TypeName::new("Circle").unwrap(),
                field_name: FieldName::new("field").unwrap(),
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::MatchArmTypeMismatch {
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::ConditionTypeMismatch {
                found: found(),
                range: range.clone(),
            },
            TypeError::BooleanNegationTypeMismatch {
                found: found(),
                range: range.clone(),
            },
            TypeError::RangeBoundTypeMismatch {
                found: found(),
                range: range.clone(),
            },
            TypeError::TextExpressionTypeMismatch {
                found: found(),
                range: range.clone(),
            },
            TypeError::MacroArgumentTypeMismatch {
                macro_name: String::from("join!"),
                expected: expected(),
                found: found(),
                range: range.clone(),
            },
            TypeError::IterateeTypeMismatch {
                found: found(),
                range: range.clone(),
            },
        ];

        let actual = DocumentAnnotator::new()
            .with_label("error")
            .annotate(&document_id, &errors)
            .render();

        expect![[r#"
            error: Mismatched type: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for array element: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for `field`: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for `field`: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type: expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for condition: expected `Bool` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for negation: expected `Bool` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for range bound: expected `Int` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type for text expression: expected `String` got Int
            1 | value
              | ^^^^^

            error: Mismatched type for 'join!': expected `String` got `Int`
            1 | value
              | ^^^^^

            error: Mismatched type: expected `Array[...]` got `Int`
            1 | value
              | ^^^^^
        "#]]
        .assert_eq(&actual);
    }
}
