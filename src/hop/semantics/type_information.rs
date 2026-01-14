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

    pub fn get_component_type_info(
        &self,
        component_name: &str,
    ) -> Option<&ComponentTypeInformation> {
        self.components.get(component_name)
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
