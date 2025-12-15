use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::{self, Type};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Component {tag_name} is not defined")]
    UndefinedComponent { tag_name: DocumentRange },

    #[error("Module {module} does not declare a component named {component}")]
    UndeclaredComponent {
        module: String,
        component: String,
        range: DocumentRange,
    },

    #[error("Module {module} was not found")]
    ModuleNotFound {
        module: String,
        range: DocumentRange,
    },

    #[error("Unused variable {var_name}")]
    UnusedVariable { var_name: DocumentRange },

    #[error("Variable {var} is already defined")]
    VariableIsAlreadyDefined { var: String, range: DocumentRange },

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
        expected: Type,
        found: Type,
        arg_name: DocumentRange,
        expr_range: DocumentRange,
    },

    #[error("Expected string attribute, got {found}")]
    ExpectedStringAttribute { found: String, range: DocumentRange },

    #[error("Can not iterate over {typ}")]
    CannotIterateOver { typ: String, range: DocumentRange },

    #[error("Expected string for text expression, got {found}")]
    ExpectedStringForTextExpression { found: Type, range: DocumentRange },

    #[error("{err}")]
    DopError { err: dop::type_error::TypeError },
}

impl From<dop::type_error::TypeError> for TypeError {
    fn from(err: dop::type_error::TypeError) -> Self {
        Self::DopError { err }
    }
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

    pub fn missing_arguments(params: &[(String, crate::dop::Type)], range: DocumentRange) -> Self {
        let args = params
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        TypeError::MissingArguments { args, range }
    }
}

impl Ranged for TypeError {
    fn range(&self) -> &DocumentRange {
        match self {
            TypeError::UndefinedComponent { tag_name: range }
            | TypeError::UndeclaredComponent { range, .. }
            | TypeError::UnusedVariable { var_name: range }
            | TypeError::ModuleNotFound { range, .. }
            | TypeError::VariableIsAlreadyDefined { range, .. }
            | TypeError::ComponentDoesNotAcceptChildren { range, .. }
            | TypeError::MissingChildren { range, .. }
            | TypeError::ChildrenArgNotAllowed { range, .. }
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
            | TypeError::CannotIterateOver { range, .. }
            | TypeError::ExpectedStringForTextExpression { range, .. } => range,
            TypeError::DopError { err } => err.range(),
        }
    }
}
