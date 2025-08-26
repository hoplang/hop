use super::typechecker::DopType;
use serde_json::Value;
use std::collections::BTreeMap;
use std::fs;

/// Infer the DopType from a JSON value
pub fn infer_type_from_value(value: &Value) -> DopType {
    match value {
        Value::Null => DopType::Void,
        Value::Bool(_) => DopType::Bool,
        Value::Number(_) => DopType::Number,
        Value::String(_) => DopType::String,
        Value::Array(arr) => {
            if arr.is_empty() {
                // Empty array defaults to array of void
                DopType::Array(Box::new(DopType::Void))
            } else {
                // Infer type from first element
                // In a more sophisticated type system, we might union all element types
                let element_type = infer_type_from_value(&arr[0]);
                DopType::Array(Box::new(element_type))
            }
        }
        Value::Object(obj) => {
            let mut properties = BTreeMap::new();
            for (key, value) in obj {
                properties.insert(key.clone(), infer_type_from_value(value));
            }
            DopType::Object(properties)
        }
    }
}

/// Load and infer type from a JSON file
pub fn infer_type_from_json_file(file_path: &str) -> Result<DopType, String> {
    // Read the file
    let content = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read JSON file '{}': {}", file_path, e))?;

    // Parse as JSON
    let value: Value = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse JSON file '{}': {}", file_path, e))?;

    // Infer the type
    Ok(infer_type_from_value(&value))
}

/// Load JSON data from a file
pub fn load_json_file(file_path: &str) -> Result<Value, String> {
    // Read the file
    let content = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read JSON file '{}': {}", file_path, e))?;

    // Parse as JSON
    serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse JSON file '{}': {}", file_path, e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_infer_null() {
        let value = json!(null);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Void);
    }

    #[test]
    fn test_infer_bool() {
        let value = json!(true);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Bool);
    }

    #[test]
    fn test_infer_number() {
        let value = json!(42);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Number);

        let value = json!(3.14);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Number);
    }

    #[test]
    fn test_infer_string() {
        let value = json!("hello");
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::String);
    }

    #[test]
    fn test_infer_empty_array() {
        let value = json!([]);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Array(Box::new(DopType::Void)));
    }

    #[test]
    fn test_infer_array_of_numbers() {
        let value = json!([1, 2, 3]);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Array(Box::new(DopType::Number)));
    }

    #[test]
    fn test_infer_array_of_strings() {
        let value = json!(["a", "b", "c"]);
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Array(Box::new(DopType::String)));
    }

    #[test]
    fn test_infer_nested_array() {
        let value = json!([[1, 2], [3, 4]]);
        let typ = infer_type_from_value(&value);
        assert_eq!(
            typ,
            DopType::Array(Box::new(DopType::Array(Box::new(DopType::Number))))
        );
    }

    #[test]
    fn test_infer_empty_object() {
        let value = json!({});
        let typ = infer_type_from_value(&value);
        assert_eq!(typ, DopType::Object(BTreeMap::new()));
    }

    #[test]
    fn test_infer_simple_object() {
        let value = json!({
            "name": "John",
            "age": 30,
            "active": true
        });
        let typ = infer_type_from_value(&value);

        let mut expected = BTreeMap::new();
        expected.insert("name".to_string(), DopType::String);
        expected.insert("age".to_string(), DopType::Number);
        expected.insert("active".to_string(), DopType::Bool);

        assert_eq!(typ, DopType::Object(expected));
    }

    #[test]
    fn test_infer_nested_object() {
        let value = json!({
            "user": {
                "name": "Alice",
                "id": 123
            },
            "scores": [95, 87, 92]
        });
        let typ = infer_type_from_value(&value);

        let mut user_props = BTreeMap::new();
        user_props.insert("name".to_string(), DopType::String);
        user_props.insert("id".to_string(), DopType::Number);

        let mut expected = BTreeMap::new();
        expected.insert("user".to_string(), DopType::Object(user_props));
        expected.insert(
            "scores".to_string(),
            DopType::Array(Box::new(DopType::Number)),
        );

        assert_eq!(typ, DopType::Object(expected));
    }

    #[test]
    fn test_infer_complex_structure() {
        let value = json!({
            "users": [
                {
                    "name": "Alice",
                    "email": "alice@example.com",
                    "roles": ["admin", "user"]
                },
                {
                    "name": "Bob",
                    "email": "bob@example.com",
                    "roles": ["user"]
                }
            ]
        });
        let typ = infer_type_from_value(&value);

        let mut user_props = BTreeMap::new();
        user_props.insert("name".to_string(), DopType::String);
        user_props.insert("email".to_string(), DopType::String);
        user_props.insert(
            "roles".to_string(),
            DopType::Array(Box::new(DopType::String)),
        );

        let mut expected = BTreeMap::new();
        expected.insert(
            "users".to_string(),
            DopType::Array(Box::new(DopType::Object(user_props))),
        );

        assert_eq!(typ, DopType::Object(expected));
    }
}
