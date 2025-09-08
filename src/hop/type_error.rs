use crate::dop::{DopParameter, DopType};
use crate::span::string_cursor::{Spanned, StringSpan};
use std::collections::BTreeMap;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Component {tag_name} is not defined")]
    UndefinedComponent { tag_name: StringSpan },

    #[error("Module {module_name} does not declare a component named {component_name}")]
    UndeclaredComponent {
        module_name: StringSpan,
        component_name: StringSpan,
    },

    #[error("Module {module} is not defined")]
    ImportFromUndefinedModule { module: String, span: StringSpan },

    #[error("Unused variable {var_name}")]
    UnusedVariable { var_name: StringSpan },

    #[error("Variable {var} is already defined")]
    VariableIsAlreadyDefined { var: String, span: StringSpan },

    #[error("Component {component} does not have a slot-default")]
    UndefinedSlot { component: String, span: StringSpan },

    #[error(
        "Import cycle: {importer_module} imports from {imported_component} which creates a dependency cycle: {cycle_display}"
    )]
    ImportCycle {
        importer_module: String,
        imported_component: String,
        cycle_display: String,
        span: StringSpan,
    },

    #[error("Expected boolean condition, got {found}")]
    ExpectedBooleanCondition { found: String, span: StringSpan },

    #[error("Missing required parameter '{param}'")]
    MissingRequiredParameter { param: String, span: StringSpan },

    #[error("Component requires arguments: {args}")]
    MissingArguments { args: String, span: StringSpan },

    #[error("Component does not accept arguments")]
    UnexpectedArguments { span: StringSpan },

    #[error("Unexpected argument '{arg}'")]
    UnexpectedArgument { arg: String, span: StringSpan },

    #[error("Argument '{arg_name}' of type {found} is incompatible with expected type {expected}")]
    ArgumentIsIncompatible {
        expected: DopType,
        found: DopType,
        arg_name: StringSpan,
        expr_span: StringSpan,
    },

    #[error("Expected string attribute, got {found}")]
    ExpectedStringAttribute { found: String, span: StringSpan },

    #[error("Cannot iterate over an empty array with unknown element type")]
    CannotIterateEmptyArray { span: StringSpan },

    #[error("Can not iterate over {typ}")]
    CannotIterateOver { typ: String, span: StringSpan },

    #[error("Expected string for text expression, got {found}")]
    ExpectedStringExpression { found: DopType, span: StringSpan },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, span: StringSpan },

    #[error("Property {property} not found in object")]
    PropertyNotFoundInObject { property: String, span: StringSpan },

    #[error("{typ} can not be used as an object")]
    CannotUseAsObject { typ: String, span: StringSpan },

    #[error("Can not compare {left} to {right}")]
    CannotCompareTypes {
        left: String,
        right: String,
        span: StringSpan,
    },

    #[error("Negation operator can only be applied to boolean values")]
    NegationRequiresBoolean { span: StringSpan },

    #[error("Array elements must all have the same type, found {expected} and {found}")]
    ArrayTypeMismatch {
        expected: String,
        found: String,
        span: StringSpan,
    },
}

impl TypeError {
    pub fn import_cycle(
        importer_module: &str,
        imported_component: &str,
        cycle: &[String],
        span: StringSpan,
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
            span,
        }
    }

    pub fn missing_arguments(params: &BTreeMap<String, DopParameter>, span: StringSpan) -> Self {
        let args = params
            .iter()
            .map(|(_, p)| p.var_name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        TypeError::MissingArguments { args, span }
    }
}

impl Spanned for TypeError {
    fn span(&self) -> &StringSpan {
        match self {
            TypeError::UndefinedComponent { tag_name } => tag_name,
            TypeError::UndeclaredComponent { component_name, .. } => component_name,
            TypeError::UnusedVariable { var_name } => var_name,
            TypeError::ImportFromUndefinedModule { span, .. }
            | TypeError::VariableIsAlreadyDefined { span, .. }
            | TypeError::UndefinedSlot { span, .. }
            | TypeError::ImportCycle { span, .. }
            | TypeError::ExpectedBooleanCondition { span, .. }
            | TypeError::MissingRequiredParameter { span, .. }
            | TypeError::MissingArguments { span, .. }
            | TypeError::UnexpectedArguments { span, .. }
            | TypeError::UnexpectedArgument { span, .. }
            | TypeError::ArgumentIsIncompatible {
                expr_span: span, ..
            }
            | TypeError::ExpectedStringAttribute { span, .. }
            | TypeError::CannotIterateEmptyArray { span, .. }
            | TypeError::CannotIterateOver { span, .. }
            | TypeError::ExpectedStringExpression { span, .. }
            | TypeError::UndefinedVariable { span, .. }
            | TypeError::PropertyNotFoundInObject { span, .. }
            | TypeError::CannotUseAsObject { span, .. }
            | TypeError::CannotCompareTypes { span, .. }
            | TypeError::NegationRequiresBoolean { span, .. }
            | TypeError::ArrayTypeMismatch { span, .. } => span,
        }
    }
}
