use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::{DopParameter, DopType};
use std::collections::BTreeMap;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Component {tag_name} is not defined")]
    UndefinedComponent { tag_name: DocumentRange },

    #[error("Module {module_name} does not declare a component named {component_name}")]
    UndeclaredComponent {
        module_name: DocumentRange,
        component_name: DocumentRange,
    },

    #[error("Module {module} is not defined")]
    ImportFromUndefinedModule {
        module: String,
        range: DocumentRange,
    },

    #[error("Unused variable {var_name}")]
    UnusedVariable { var_name: DocumentRange },

    #[error("Variable {var} is already defined")]
    VariableIsAlreadyDefined { var: String, range: DocumentRange },

    #[error("Component {component} does not have a slot-default")]
    UndefinedSlot {
        component: String,
        range: DocumentRange,
    },

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
    ExpectedBooleanCondition { found: String, range: DocumentRange },

    #[error("Missing required parameter '{param}'")]
    MissingRequiredParameter { param: String, range: DocumentRange },

    #[error("Component requires arguments: {args}")]
    MissingArguments { args: String, range: DocumentRange },

    #[error("Component does not accept arguments")]
    UnexpectedArguments { range: DocumentRange },

    #[error("Unexpected argument '{arg}'")]
    UnexpectedArgument { arg: String, range: DocumentRange },

    #[error("Argument '{arg_name}' of type {found} is incompatible with expected type {expected}")]
    ArgumentIsIncompatible {
        expected: DopType,
        found: DopType,
        arg_name: DocumentRange,
        expr_range: DocumentRange,
    },

    #[error("Expected string attribute, got {found}")]
    ExpectedStringAttribute { found: String, range: DocumentRange },

    #[error("Cannot iterate over an empty array with unknown element type")]
    CannotIterateEmptyArray { range: DocumentRange },

    #[error("Can not iterate over {typ}")]
    CannotIterateOver { typ: String, range: DocumentRange },

    #[error("Expected string for text expression, got {found}")]
    ExpectedStringExpression {
        found: DopType,
        range: DocumentRange,
    },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, range: DocumentRange },

    #[error("Property {property} not found in object")]
    PropertyNotFoundInObject {
        property: String,
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

    #[error("Negation operator can only be applied to boolean values")]
    NegationRequiresBoolean { range: DocumentRange },

    #[error("Array elements must all have the same type, found {expected} and {found}")]
    ArrayTypeMismatch {
        expected: String,
        found: String,
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

    pub fn missing_arguments(
        params: &BTreeMap<StringSpan, DopParameter>,
        range: DocumentRange,
    ) -> Self {
        let args = params
            .iter()
            .map(|(_, p)| p.var_name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        TypeError::MissingArguments { args, range }
    }
}

impl Ranged for TypeError {
    fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedComponent { tag_name: range }
            | TypeError::UndeclaredComponent {
                component_name: range,
                ..
            }
            | TypeError::UnusedVariable { var_name: range }
            | TypeError::ImportFromUndefinedModule { range, .. }
            | TypeError::VariableIsAlreadyDefined { range, .. }
            | TypeError::UndefinedSlot { range, .. }
            | TypeError::ImportCycle { range, .. }
            | TypeError::ExpectedBooleanCondition { range, .. }
            | TypeError::MissingRequiredParameter { range, .. }
            | TypeError::MissingArguments { range, .. }
            | TypeError::UnexpectedArguments { range, .. }
            | TypeError::UnexpectedArgument { range, .. }
            | TypeError::ArgumentIsIncompatible {
                expr_range: range, ..
            }
            | TypeError::ExpectedStringAttribute { range, .. }
            | TypeError::CannotIterateEmptyArray { range, .. }
            | TypeError::CannotIterateOver { range, .. }
            | TypeError::ExpectedStringExpression { range, .. }
            | TypeError::UndefinedVariable { range, .. }
            | TypeError::PropertyNotFoundInObject { range, .. }
            | TypeError::CannotUseAsObject { range, .. }
            | TypeError::CannotCompareTypes { range, .. }
            | TypeError::NegationRequiresBoolean { range, .. }
            | TypeError::ArrayTypeMismatch { range, .. } => range,
        }
    }
}
