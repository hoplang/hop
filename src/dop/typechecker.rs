use super::parser::{BinaryOp, DopExpr, UnaryOp};
use crate::common::{Environment, Range, RangeError};
use crate::typechecker::TypeAnnotation;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum DopType {
    Object(BTreeMap<String, DopType>),
    Array(Option<Box<DopType>>),
    Bool,
    String,
    Number,
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopType::Object(properties) => {
                write!(f, "object[")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "]")
            }
            DopType::Array(inner_type) => match inner_type {
                Some(typ) => write!(f, "array[{}]", typ),
                None => write!(f, "array[]"),
            },
            DopType::Bool => write!(f, "boolean"),
            DopType::String => write!(f, "string"),
            DopType::Number => write!(f, "number"),
        }
    }
}

/// Check if `subtype` is a subtype of `supertype`
pub fn is_subtype(subtype: &DopType, supertype: &DopType) -> bool {
    match (subtype, supertype) {
        // Exact matches
        (DopType::Bool, DopType::Bool) => true,
        (DopType::String, DopType::String) => true,
        (DopType::Number, DopType::Number) => true,

        // Arrays are covariant in their element type
        (DopType::Array(sub_elem), DopType::Array(super_elem)) => match (sub_elem, super_elem) {
            (Some(sub_type), Some(super_type)) => is_subtype(sub_type, super_type),
            (None, None) => true,
            (None, Some(_)) => true, // Empty array can be subtype of any array
            (Some(_), None) => false, // Typed array cannot be subtype of empty array
        },

        // Objects: subtype must have all properties of supertype with compatible types
        (DopType::Object(sub_props), DopType::Object(super_props)) => {
            super_props.iter().all(|(key, super_type)| {
                sub_props
                    .get(key)
                    .is_some_and(|sub_type| is_subtype(sub_type, super_type))
            })
        }

        // Otherwise, not a subtype
        _ => false,
    }
}

pub fn typecheck_expr(
    expr: &DopExpr,
    env: &mut Environment<DopType>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
    range: Range,
) -> Result<DopType, RangeError> {
    match expr {
        DopExpr::Variable { name, .. } => {
            if let Some(var_type) = env.lookup(name) {
                annotations.push(TypeAnnotation {
                    range,
                    typ: var_type.clone(),
                    name: name.clone(),
                });
                Ok(var_type.clone())
            } else {
                Err(RangeError::new(
                    format!("Undefined variable: {}", name),
                    range,
                ))
            }
        }
        DopExpr::BooleanLiteral { .. } => Ok(DopType::Bool),
        DopExpr::StringLiteral { .. } => Ok(DopType::String),
        DopExpr::NumberLiteral { .. } => Ok(DopType::Number),
        DopExpr::PropertyAccess {
            object: base_expr,
            property,
        } => {
            let base_type = typecheck_expr(base_expr, env, annotations, errors, range)?;

            match &base_type {
                DopType::Object(props) => {
                    if let Some(prop_type) = props.get(property) {
                        Ok(prop_type.clone())
                    } else {
                        Err(RangeError::new(
                            format!("Property {} not found in object", property),
                            range,
                        ))
                    }
                }
                _ => Err(RangeError::new(
                    format!("{} can not be used as an object", base_type),
                    range,
                )),
            }
        }
        DopExpr::BinaryOp {
            left,
            operator: BinaryOp::Equal,
            right,
        } => {
            let left_type = typecheck_expr(left, env, annotations, errors, range)?;
            let right_type = typecheck_expr(right, env, annotations, errors, range)?;

            // Both operands should have the same type for equality comparison
            if left_type != right_type {
                return Err(RangeError::new(
                    format!("Can not compare {} to {}", left_type, right_type),
                    range,
                ));
            }

            // The result of == is always boolean
            Ok(DopType::Bool)
        }
        DopExpr::UnaryOp {
            operator: UnaryOp::Not,
            operand: expr,
        } => {
            let expr_type = typecheck_expr(expr, env, annotations, errors, range)?;

            // Negation only works on boolean expressions
            if !is_subtype(&expr_type, &DopType::Bool) {
                return Err(RangeError::new(
                    "Negation operator can only be applied to boolean values".to_string(),
                    range,
                ));
            }

            // The result of ! is always boolean
            Ok(DopType::Bool)
        }
        DopExpr::ArrayLiteral { elements } => {
            if elements.is_empty() {
                // Empty array has unknown element type
                Ok(DopType::Array(None))
            } else {
                // Check the type of the first element
                let first_type = typecheck_expr(&elements[0], env, annotations, errors, range)?;

                // Check that all elements have the same type
                for element in elements.iter().skip(1) {
                    let element_type = typecheck_expr(element, env, annotations, errors, range)?;
                    if element_type != first_type {
                        return Err(RangeError::new(
                            format!(
                                "Array elements must all have the same type, found {} and {}",
                                first_type, element_type
                            ),
                            range,
                        ));
                    }
                }

                Ok(DopType::Array(Some(Box::new(first_type))))
            }
        }
        DopExpr::ObjectLiteral { properties } => {
            let mut object_properties = BTreeMap::new();

            for (key, value_expr) in properties {
                let value_type = typecheck_expr(value_expr, env, annotations, errors, range)?;
                object_properties.insert(key.clone(), value_type);
            }

            Ok(DopType::Object(object_properties))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::DopTokenizer;
    use crate::dop::parse_parameters_with_types;
    use crate::dop::parser::parse_expr;
    use expect_test::{Expect, expect};

    fn check_typecheck(env_str: &str, expr_str: &str, expected: Expect) {
        let mut env = Environment::new();

        if !env_str.is_empty() {
            let mut tokenizer = DopTokenizer::new(env_str, crate::common::Position::new(1, 1))
                .expect("Failed to create tokenizer");
            let params =
                parse_parameters_with_types(&mut tokenizer).expect("Failed to parse environment");
            for ((var_name, _), (var_type, _)) in params {
                env.push(var_name.value, var_type);
            }
        }

        let mut tokenizer = DopTokenizer::new(expr_str, crate::common::Position::new(1, 1))
            .expect("Failed to create tokenizer");
        let expr = parse_expr(&mut tokenizer).expect("Failed to parse expression");

        let mut annotations = Vec::new();
        let mut errors = Vec::new();

        let actual = match typecheck_expr(
            &expr,
            &mut env,
            &mut annotations,
            &mut errors,
            Range::default(),
        ) {
            Ok(typ) => typ.to_string(),
            Err(e) => format!("Error: {}", e.message),
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_typecheck_basic_variable_lookup() {
        check_typecheck("name: string", "name", expect!["string"]);
    }

    #[test]
    fn test_typecheck_undefined_variable() {
        check_typecheck(
            "",
            "undefined_var",
            expect!["Error: Undefined variable: undefined_var"],
        );
    }

    #[test]
    fn test_typecheck_string_literal() {
        check_typecheck("", "'hello world'", expect!["string"]);
    }

    #[test]
    fn test_typecheck_boolean_literal_true() {
        check_typecheck("", "true", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_boolean_literal_false() {
        check_typecheck("", "false", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_number_literal_integer() {
        check_typecheck("", "42", expect!["number"]);
    }

    #[test]
    fn test_typecheck_number_literal_float() {
        check_typecheck("", "3.14", expect!["number"]);
    }

    #[test]
    fn test_typecheck_property_access() {
        check_typecheck("user: object[name: string]", "user.name", expect!["string"]);
    }

    #[test]
    fn test_typecheck_nested_property_access() {
        check_typecheck(
            "app: object[user: object[profile: object[name: string]]]",
            "app.user.profile.name",
            expect!["string"],
        );
    }

    #[test]
    fn test_typecheck_property_access_on_non_object() {
        check_typecheck(
            "count: number",
            "count.value",
            expect!["Error: number can not be used as an object"],
        );
    }

    #[test]
    fn test_typecheck_equality_string() {
        check_typecheck("name: string", "name == 'alice'", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_equality_number() {
        check_typecheck("count: number", "count == 42", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_equality_boolean() {
        check_typecheck("enabled: boolean", "enabled == true", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_equality_same_object_properties() {
        check_typecheck(
            "user: object[name: string], admin: object[name: string]",
            "user.name == admin.name",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_equality_incompatible_string_number() {
        check_typecheck(
            "name: string, count: number",
            "name == count",
            expect!["Error: Can not compare string to number"],
        );
    }

    #[test]
    fn test_typecheck_equality_incompatible_boolean_string() {
        check_typecheck(
            "enabled: boolean, name: string",
            "enabled == name",
            expect!["Error: Can not compare boolean to string"],
        );
    }

    #[test]
    fn test_typecheck_complex_equality() {
        check_typecheck(
            "a: boolean, b: boolean",
            "a == b == true",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_negation_variable() {
        check_typecheck("enabled: boolean", "!enabled", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_negation_true() {
        check_typecheck("", "!true", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_negation_false() {
        check_typecheck("", "!false", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_negation_string_error() {
        check_typecheck(
            "name: string",
            "!name",
            expect!["Error: Negation operator can only be applied to boolean values"],
        );
    }

    #[test]
    fn test_typecheck_negation_number_error() {
        check_typecheck(
            "count: number",
            "!count",
            expect!["Error: Negation operator can only be applied to boolean values"],
        );
    }

    #[test]
    fn test_typecheck_complex_negation_equality() {
        check_typecheck(
            "user: object[active: boolean]",
            "!user.active == false",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_parenthesized_negation() {
        check_typecheck(
            "status: object[enabled: boolean], config: object[active: boolean]",
            "!(status.enabled == config.active)",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_array_property_access_error() {
        check_typecheck(
            "users: array[object[name: string]]",
            "users.name",
            expect!["Error: array[object[name: string]] can not be used as an object"],
        );
    }

    #[test]
    fn test_typecheck_object_array_property() {
        check_typecheck(
            "data: object[items: array[string]]",
            "data.items",
            expect!["array[string]"],
        );
    }

    #[test]
    fn test_typecheck_nested_array_property_error() {
        check_typecheck(
            "config: object[users: array[object[profile: object[name: string, active: boolean]]]]",
            "config.users.profile.name",
            expect![
                "Error: array[object[profile: object[active: boolean, name: string]]] can not be used as an object"
            ],
        );
    }

    #[test]
    fn test_typecheck_deep_property_access() {
        check_typecheck(
            "system: object[config: object[database: object[connection: object[host: string]]]]",
            "system.config.database.connection.host",
            expect!["string"],
        );
    }

    #[test]
    fn test_typecheck_multiple_property_access() {
        check_typecheck(
            "obj: object[name: string, title: string]",
            "obj.name == obj.title",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_unknown_property() {
        check_typecheck(
            "data: object[field: string]",
            "data.unknown",
            expect!["Error: Property unknown not found in object"],
        );
    }

    #[test]
    fn test_typecheck_empty_object_literal() {
        check_typecheck("", "{}", expect!["object[]"]);
    }

    #[test]
    fn test_typecheck_object_literal_single_property() {
        check_typecheck("", "{name: 'John'}", expect!["object[name: string]"]);
    }

    #[test]
    fn test_typecheck_object_literal_multiple_properties() {
        check_typecheck(
            "",
            "{a: 'foo', b: 1, c: true}",
            expect!["object[a: string, b: number, c: boolean]"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_complex_expressions() {
        check_typecheck(
            "user: object[name: string, disabled: boolean]",
            "{user: user.name, active: !user.disabled}",
            expect!["object[active: boolean, user: string]"],
        );
    }

    #[test]
    fn test_typecheck_nested_object_literal() {
        check_typecheck(
            "",
            "{nested: {inner: 'value'}}",
            expect!["object[nested: object[inner: string]]"],
        );
    }

    #[test]
    fn test_typecheck_array_trailing_comma_numbers() {
        check_typecheck("", "[\n\t1,\n\t2,\n\t3,\n]", expect!["array[number]"]);
    }

    #[test]
    fn test_typecheck_array_trailing_comma_single() {
        check_typecheck("", "[\n\t'hello',\n]", expect!["array[string]"]);
    }

    #[test]
    fn test_typecheck_object_literal_trailing_comma() {
        check_typecheck(
            "",
            "{\n\ta: 'foo',\n\tb: 1,\n}",
            expect!["object[a: string, b: number]"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_trailing_comma_single() {
        check_typecheck("", "{\n\tname: 'John',\n}", expect!["object[name: string]"]);
    }

    #[test]
    fn test_typecheck_brace_syntax_object() {
        check_typecheck(
            "user: {name: string, age: number}",
            "user.name",
            expect!["string"],
        );
    }

    #[test]
    fn test_typecheck_empty_brace_syntax() {
        check_typecheck("obj: {}", "obj", expect!["object[]"]);
    }

    #[test]
    fn test_typecheck_nested_brace_syntax() {
        check_typecheck(
            "config: {database: {host: string, port: number}, cache: {enabled: boolean}}",
            "config.database.host",
            expect!["string"],
        );
    }

    #[test]
    fn test_typecheck_brace_syntax_with_array() {
        check_typecheck(
            "data: {items: array[string], count: number}",
            "data.items",
            expect!["array[string]"],
        );
    }
}
