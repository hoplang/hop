use crate::dop::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct ModuleTypeInformation {
    pub types: HashMap<String, Type>,
}

impl ModuleTypeInformation {
    pub fn get(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    pub fn insert(&mut self, name: &str, typ: Type) {
        self.types.insert(name.to_string(), typ);
    }
}
