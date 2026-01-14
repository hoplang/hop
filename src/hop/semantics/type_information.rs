use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::{Type, VarName};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ComponentTypeInformation {
    /// Parameters: (name, type, has_default)
    pub parameters: Vec<(VarName, Type, bool)>,
}

#[derive(Debug, Clone)]
pub struct RecordTypeInformation {
    pub fields: Vec<(FieldName, Type)>,
}

#[derive(Debug, Clone)]
pub struct EnumTypeInformation {
    /// Variants with their fields: (variant_name, fields)
    pub variants: Vec<(TypeName, Vec<(FieldName, Type)>)>,
}

#[derive(Debug, Clone, Default)]
pub struct ModuleTypeInformation {
    pub components: HashMap<String, ComponentTypeInformation>,
    pub records: HashMap<String, RecordTypeInformation>,
    pub enums: HashMap<String, EnumTypeInformation>,
}

impl ModuleTypeInformation {
    pub fn get_parameter_types(&self, component_name: &str) -> Option<&[(VarName, Type, bool)]> {
        let params = &self.components.get(component_name)?.parameters;
        if params.is_empty() {
            None
        } else {
            Some(params.as_slice())
        }
    }

    /// Check if the component accepts children (has a `children` parameter)
    pub fn component_accepts_children(&self, component_name: &str) -> bool {
        self.components
            .get(component_name)
            .is_some_and(|c| c.parameters.iter().any(|(name, _, _)| name.as_str() == "children"))
    }

    /// Get the children parameter's type and has_default flag if present
    pub fn get_children_param(&self, component_name: &str) -> Option<(&Type, bool)> {
        self.components
            .get(component_name)?
            .parameters
            .iter()
            .find(|(name, _, _)| name.as_str() == "children")
            .map(|(_, typ, has_default)| (typ, *has_default))
    }

    /// Check if children are required (TrustedHTML without default)
    pub fn children_are_required(&self, component_name: &str) -> bool {
        matches!(
            self.get_children_param(component_name),
            Some((Type::TrustedHTML, false))
        )
    }

    pub fn component_is_declared(&self, component_name: &str) -> bool {
        self.components.contains_key(component_name)
    }

    pub fn set_component_type_info(
        &mut self,
        component_name: &str,
        type_info: ComponentTypeInformation,
    ) {
        self.components
            .insert(component_name.to_string(), type_info);
    }

    pub fn record_is_declared(&self, record_name: &str) -> bool {
        self.records.contains_key(record_name)
    }

    pub fn get_typed_record(&self, record_name: &str) -> Option<&RecordTypeInformation> {
        self.records.get(record_name)
    }

    pub fn set_typed_record(&mut self, record_name: &str, record: RecordTypeInformation) {
        self.records.insert(record_name.to_string(), record);
    }

    pub fn enum_is_declared(&self, enum_name: &str) -> bool {
        self.enums.contains_key(enum_name)
    }

    pub fn get_typed_enum(&self, enum_name: &str) -> Option<&EnumTypeInformation> {
        self.enums.get(enum_name)
    }

    pub fn set_typed_enum(&mut self, enum_name: &str, enum_info: EnumTypeInformation) {
        self.enums.insert(enum_name.to_string(), enum_info);
    }
}
