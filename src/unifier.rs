use crate::common::Type;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct UnificationError {
    pub message: String,
}

impl UnificationError {
    pub fn new(message: String) -> Self {
        UnificationError { message }
    }
}

pub struct Unifier {
    substitutions: HashMap<i32, Type>,
    next_type_var_id: i32,
}

impl Unifier {
    pub fn new() -> Self {
        Unifier {
            substitutions: HashMap::new(),
            next_type_var_id: 0,
        }
    }

    pub fn new_type_var(&mut self) -> Type {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;
        Type::TypeVar(id)
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
                    Type::TypeVar(_) => Type::Object(queried_props, *rest),
                    _ => panic!("Invalid rest type in ObjectType query"),
                }
            }
            Type::Bool | Type::String | Type::Void => t.clone(),
        }
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Option<UnificationError> {
        if self.types_equal(a, b) {
            return None;
        }

        match (a, b) {
            (Type::TypeVar(_), _) => self.unify_type_var(a, b),
            (_, Type::TypeVar(_)) => self.unify_type_var(b, a),
            (Type::Array(a_type), Type::Array(b_type)) => self.unify(a_type, b_type),
            (Type::Object(a_props, a_rest), Type::Object(b_props, b_rest)) => {
                // Unify common properties
                let a_keys: HashSet<&String> = a_props.keys().collect();
                let b_keys: HashSet<&String> = b_props.keys().collect();
                let common_keys: HashSet<&String> = a_keys.intersection(&b_keys).cloned().collect();

                for key in common_keys {
                    if let Some(err) = self.unify(&a_props[key], &b_props[key]) {
                        return Some(err);
                    }
                }

                // Handle missing properties
                let a_missing: HashMap<String, Type> = b_props
                    .iter()
                    .filter(|(k, _)| !a_props.contains_key(*k))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                let b_missing: HashMap<String, Type> = a_props
                    .iter()
                    .filter(|(k, _)| !b_props.contains_key(*k))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                let shared_rest = self.new_type_var();
                if let Type::TypeVar(shared_rest_id) = shared_rest {
                    if let Some(err) = self.unify(
                        &Type::TypeVar(*a_rest),
                        &Type::Object(a_missing, shared_rest_id),
                    ) {
                        return Some(err);
                    }
                    if let Some(err) = self.unify(
                        &Type::TypeVar(*b_rest),
                        &Type::Object(b_missing, shared_rest_id),
                    ) {
                        return Some(err);
                    }
                }

                None
            }
            _ => Some(UnificationError::new("Can not unify types".to_string())),
        }
    }

    fn unify_type_var(&mut self, a: &Type, b: &Type) -> Option<UnificationError> {
        if let Type::TypeVar(a_id) = a {
            if let Some(substituted_a) = self.substitutions.get(a_id).cloned() {
                return self.unify(&substituted_a, b);
            }

            if let Type::TypeVar(b_id) = b {
                if let Some(substituted_b) = self.substitutions.get(b_id).cloned() {
                    return self.unify(a, &substituted_b);
                }
            }

            self.substitutions.insert(*a_id, b.clone());
            None
        } else {
            panic!("unify_type_var called with non-TypeVar first argument");
        }
    }

    fn types_equal(&self, a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::TypeVar(a_id), Type::TypeVar(b_id)) => a_id == b_id,
            (Type::Array(a_type), Type::Array(b_type)) => self.types_equal(a_type, b_type),
            (Type::Object(a_props, a_rest), Type::Object(b_props, b_rest)) => {
                a_rest == b_rest
                    && a_props.len() == b_props.len()
                    && a_props
                        .iter()
                        .all(|(k, v)| b_props.get(k).map_or(false, |b_v| self.types_equal(v, b_v)))
            }
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}
