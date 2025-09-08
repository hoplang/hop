use crate::dop::DopParameter;
use crate::span::string_cursor::{Spanned, StringSpan};
use std::collections::BTreeMap;
use std::fmt::{self, Display};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Component {component} is not defined")]
    UndefinedComponent { component: String, span: StringSpan },

    #[error("Module {module} does not declare a component named {component}")]
    UndeclaredComponent { module: String, component: String, span: StringSpan },

    #[error("Module {module} is not defined")]
    ImportFromUndefinedModule { module: String, span: StringSpan },

    #[error("Unused variable {var}")]
    UnusedVariable { var: String, span: StringSpan },

    #[error("Variable {var} is already defined")]
    VariableIsAlreadyDefined { var: String, span: StringSpan },

    #[error("Component {component} does not have a slot-default")]
    UndefinedSlot { component: String, span: StringSpan },

    #[error("Import cycle: {importer_module} imports from {imported_component} which creates a dependency cycle: {cycle_display}")]
    ImportCycle { importer_module: String, imported_component: String, cycle_display: String, span: StringSpan },

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
    ArgumentIsIncompatible { expected: String, found: String, arg_name: String, span: StringSpan },

    #[error("Expected string attribute, got {found}")]
    ExpectedStringAttribute { found: String, span: StringSpan },

    #[error("Cannot iterate over an empty array with unknown element type")]
    CannotIterateEmptyArray { span: StringSpan },

    #[error("Can not iterate over {typ}")]
    CannotIterateOver { typ: String, span: StringSpan },

    #[error("Expected string for text expression, got {found}")]
    ExpectedStringExpression { found: String, span: StringSpan },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, span: StringSpan },

    #[error("Property {property} not found in object")]
    PropertyNotFoundInObject { property: String, span: StringSpan },

    #[error("{typ} can not be used as an object")]
    CannotUseAsObject { typ: String, span: StringSpan },

    #[error("Can not compare {left} to {right}")]
    CannotCompareTypes { left: String, right: String, span: StringSpan },

    #[error("Negation operator can only be applied to boolean values")]
    NegationRequiresBoolean { span: StringSpan },

    #[error("Array elements must all have the same type, found {expected} and {found}")]
    ArrayTypeMismatch { expected: String, found: String, span: StringSpan },

    #[error("{message}")]
    Custom { message: String, span: StringSpan },
}

impl TypeError {
    pub fn new(message: String, span: StringSpan) -> Self {
        TypeError::Custom { message, span }
    }

    pub fn undefined_component(component: &str, span: StringSpan) -> Self {
        TypeError::UndefinedComponent {
            component: component.to_string(),
            span,
        }
    }

    pub fn undeclared_component(module: &str, component: &str, span: StringSpan) -> Self {
        TypeError::UndeclaredComponent {
            module: module.to_string(),
            component: component.to_string(),
            span,
        }
    }

    pub fn import_from_undefined_module(module: &str, span: StringSpan) -> Self {
        TypeError::ImportFromUndefinedModule {
            module: module.to_string(),
            span,
        }
    }

    pub fn unused_variable(var: &str, span: StringSpan) -> Self {
        TypeError::UnusedVariable {
            var: var.to_string(),
            span,
        }
    }

    pub fn variable_is_already_defined(var: &str, span: StringSpan) -> Self {
        TypeError::VariableIsAlreadyDefined {
            var: var.to_string(),
            span,
        }
    }

    pub fn undefined_slot(component: &str, span: StringSpan) -> Self {
        TypeError::UndefinedSlot {
            component: component.to_string(),
            span,
        }
    }

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

    pub fn expected_boolean_condition(found: &str, span: StringSpan) -> Self {
        TypeError::ExpectedBooleanCondition {
            found: found.to_string(),
            span,
        }
    }

    pub fn missing_required_parameter(param: &str, span: StringSpan) -> Self {
        TypeError::MissingRequiredParameter {
            param: param.to_string(),
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

    pub fn unexpected_arguments(span: StringSpan) -> Self {
        TypeError::UnexpectedArguments { span }
    }

    pub fn unexpected_argument(arg: &str, span: StringSpan) -> Self {
        TypeError::UnexpectedArgument {
            arg: arg.to_string(),
            span,
        }
    }

    pub fn argument_is_incompatible(
        expected: &str,
        found: &str,
        arg_name: &str,
        span: StringSpan,
    ) -> Self {
        TypeError::ArgumentIsIncompatible {
            expected: expected.to_string(),
            found: found.to_string(),
            arg_name: arg_name.to_string(),
            span,
        }
    }

    pub fn expected_string_attribute(found: &str, span: StringSpan) -> Self {
        TypeError::ExpectedStringAttribute {
            found: found.to_string(),
            span,
        }
    }

    pub fn cannot_iterate_empty_array(span: StringSpan) -> Self {
        TypeError::CannotIterateEmptyArray { span }
    }

    pub fn cannot_iterate_over(typ: &str, span: StringSpan) -> Self {
        TypeError::CannotIterateOver {
            typ: typ.to_string(),
            span,
        }
    }

    pub fn expected_string_expression(found: &str, span: StringSpan) -> Self {
        TypeError::ExpectedStringExpression {
            found: found.to_string(),
            span,
        }
    }

    pub fn undefined_variable(name: &str, span: StringSpan) -> Self {
        TypeError::UndefinedVariable {
            name: name.to_string(),
            span,
        }
    }

    pub fn property_not_found_in_object(property: &str, span: StringSpan) -> Self {
        TypeError::PropertyNotFoundInObject {
            property: property.to_string(),
            span,
        }
    }

    pub fn cannot_use_as_object(typ: &str, span: StringSpan) -> Self {
        TypeError::CannotUseAsObject {
            typ: typ.to_string(),
            span,
        }
    }

    pub fn cannot_compare_types(left: &str, right: &str, span: StringSpan) -> Self {
        TypeError::CannotCompareTypes {
            left: left.to_string(),
            right: right.to_string(),
            span,
        }
    }

    pub fn negation_requires_boolean(span: StringSpan) -> Self {
        TypeError::NegationRequiresBoolean { span }
    }

    pub fn array_type_mismatch(expected: &str, found: &str, span: StringSpan) -> Self {
        TypeError::ArrayTypeMismatch {
            expected: expected.to_string(),
            found: found.to_string(),
            span,
        }
    }
}

impl Spanned for TypeError {
    fn span(&self) -> &StringSpan {
        match self {
            TypeError::UndefinedComponent { span, .. }
            | TypeError::UndeclaredComponent { span, .. }
            | TypeError::ImportFromUndefinedModule { span, .. }
            | TypeError::UnusedVariable { span, .. }
            | TypeError::VariableIsAlreadyDefined { span, .. }
            | TypeError::UndefinedSlot { span, .. }
            | TypeError::ImportCycle { span, .. }
            | TypeError::ExpectedBooleanCondition { span, .. }
            | TypeError::MissingRequiredParameter { span, .. }
            | TypeError::MissingArguments { span, .. }
            | TypeError::UnexpectedArguments { span, .. }
            | TypeError::UnexpectedArgument { span, .. }
            | TypeError::ArgumentIsIncompatible { span, .. }
            | TypeError::ExpectedStringAttribute { span, .. }
            | TypeError::CannotIterateEmptyArray { span, .. }
            | TypeError::CannotIterateOver { span, .. }
            | TypeError::ExpectedStringExpression { span, .. }
            | TypeError::UndefinedVariable { span, .. }
            | TypeError::PropertyNotFoundInObject { span, .. }
            | TypeError::CannotUseAsObject { span, .. }
            | TypeError::CannotCompareTypes { span, .. }
            | TypeError::NegationRequiresBoolean { span, .. }
            | TypeError::ArrayTypeMismatch { span, .. }
            | TypeError::Custom { span, .. } => span,
        }
    }
}


#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: StringSpan,
}

impl ParseError {
    pub fn new(message: String, span: StringSpan) -> Self {
        ParseError { message, span }
    }

    pub fn slot_is_already_defined(span: StringSpan) -> Self {
        Self::new("slot-default is already defined".to_string(), span)
    }

    pub fn unmatched_closing_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(format!("Unmatched </{tag}>"), span)
    }

    pub fn unexpected_end_of_expression(span: StringSpan) -> Self {
        Self::new("Unexpected end of expression".to_string(), span)
    }

    pub fn missing_attribute_value(span: StringSpan) -> Self {
        Self::new("Missing attribute value".to_string(), span)
    }

    pub fn unclosed_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(format!("Unclosed <{tag}>"), span)
    }

    pub fn closed_void_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(
            format!("<{tag}> should not be closed using a closing tag"),
            span,
        )
    }

    pub fn unrecognized_hop_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(format!("Unrecognized hop tag: <{tag}>"), span)
    }

    pub fn missing_required_attribute(tag_name: StringSpan, attr: &str) -> Self {
        Self::new(
            format!("<{tag_name}> is missing required attribute {attr}"),
            tag_name,
        )
    }

    pub fn invalid_component_name(tag_name: StringSpan) -> Self {
        Self::new(
            format!(
                "Invalid component name '{tag_name}'. Component names must contain a dash and not start or end with one"
            ),
            tag_name,
        )
    }

    pub fn component_is_already_defined(component_attr_value: StringSpan) -> Self {
        Self::new(
            format!("Component {component_attr_value} is already defined"),
            component_attr_value,
        )
    }

    pub fn duplicate_attribute(name: &str, span: StringSpan) -> Self {
        Self::new(format!("Duplicate attribute '{name}'"), span)
    }

    pub fn invalid_import_path(span: StringSpan) -> Self {
        Self::new(
            format!("Import paths must start with '@/' where '@' indicates the root directory"),
            span,
        )
    }

    pub fn invalid_module_name(
        span: StringSpan,
        error: crate::hop::module_name::InvalidModuleNameError,
    ) -> Self {
        Self::new(format!("{}", error), span)
    }
}

impl Spanned for ParseError {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}