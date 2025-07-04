use crate::common::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct UnificationError {
    pub message: String,
}

impl UnificationError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

pub struct Unifier {
    substitutions: HashMap<i32, Type>,
    next_type_var_id: i32,
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
            next_type_var_id: 0,
        }
    }

    pub fn next_type_var(&mut self) -> i32 {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;
        id
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Option<UnificationError> {
        if self.types_equal(a, b) {
            return None;
        }

        match (a, b) {
            (Type::TypeVar(id_a), _) => self.unify_type_var(*id_a, b),
            (_, Type::TypeVar(id_b)) => self.unify_type_var(*id_b, a),
            (Type::Array(type_a), Type::Array(type_b)) => self.unify(type_a, type_b),
            (Type::Object(props_a, rest_a), Type::Object(props_b, rest_b)) => {
                // Find common properties and unify them
                for (key, type_a) in props_a {
                    if let Some(type_b) = props_b.get(key) {
                        if let Some(err) = self.unify(type_a, type_b) {
                            return Some(err);
                        }
                    }
                }

                // Collect missing properties
                let missing_from_a: HashMap<String, Type> = props_b
                    .iter()
                    .filter(|(key, _)| !props_a.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                let missing_from_b: HashMap<String, Type> = props_a
                    .iter()
                    .filter(|(key, _)| !props_b.contains_key(*key))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                // Create shared rest type variable
                let shared_rest = self.next_type_var();

                // Unify rest types with missing properties
                if let Some(err) = self.unify(
                    &Type::TypeVar(*rest_a),
                    &Type::Object(missing_from_a, shared_rest),
                ) {
                    return Some(err);
                }

                if let Some(err) = self.unify(
                    &Type::TypeVar(*rest_b),
                    &Type::Object(missing_from_b, shared_rest),
                ) {
                    return Some(err);
                }

                None
            }
            _ => Some(UnificationError::new("Can not unify types".to_string())),
        }
    }

    fn unify_type_var(&mut self, var_id: i32, other_type: &Type) -> Option<UnificationError> {
        if let Some(substituted_type) = self.substitutions.get(&var_id) {
            return self.unify(&substituted_type.clone(), other_type);
        }

        if let Type::TypeVar(other_id) = other_type {
            if let Some(other_substituted) = self.substitutions.get(other_id) {
                return self.unify(&Type::TypeVar(var_id), &other_substituted.clone());
            }
        }

        self.substitutions.insert(var_id, other_type.clone());
        None
    }

    pub fn query(&self, t: &Type) -> Type {
        match t {
            Type::TypeVar(id) => {
                if let Some(substituted_type) = self.substitutions.get(id) {
                    self.query(substituted_type)
                } else {
                    t.clone()
                }
            }
            Type::Array(sub_type) => Type::Array(Box::new(self.query(sub_type))),
            Type::Object(props, rest) => {
                let queried_props: HashMap<String, Type> = props
                    .iter()
                    .map(|(k, v)| (k.clone(), self.query(v)))
                    .collect();

                match self.query(&Type::TypeVar(*rest)) {
                    Type::Object(rest_props, rest_rest) => {
                        let mut merged_props = queried_props;
                        for (k, v) in rest_props {
                            merged_props.insert(k, v);
                        }
                        Type::Object(merged_props, rest_rest)
                    }
                    Type::TypeVar(rest_id) => Type::Object(queried_props, rest_id),
                    _ => panic!("Invalid type substitution for object rest"),
                }
            }
            Type::Bool | Type::String | Type::Void => t.clone(),
        }
    }

    fn types_equal(&self, a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::TypeVar(id_a), Type::TypeVar(id_b)) => id_a == id_b,
            (Type::Array(type_a), Type::Array(type_b)) => self.types_equal(type_a, type_b),
            (Type::Object(props_a, rest_a), Type::Object(props_b, rest_b)) => {
                rest_a == rest_b
                    && props_a.len() == props_b.len()
                    && props_a
                        .iter()
                        .all(|(k, v)| props_b.get(k).map_or(false, |v2| self.types_equal(v, v2)))
            }
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}
