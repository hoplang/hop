use super::parser::{BinaryOp, DopExpr, UnaryOp};
use crate::common::{Range, Ranged, TypeError};
use crate::hop::environment::Environment;
use crate::hop::typechecker::TypeAnnotation;
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

#[derive(Debug, Clone, PartialEq)]
pub struct RangeDopType {
    pub dop_type: DopType,
    pub range: Range,
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopType::Object(properties) => {
                write!(f, "{{")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
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
    errors: &mut Vec<TypeError>,
) -> Result<DopType, TypeError> {
    match expr {
        DopExpr::Variable { name, .. } => {
            if let Some(var_type) = env.lookup(name) {
                annotations.push(TypeAnnotation {
                    range: expr.range(),
                    typ: var_type.clone(),
                    name: name.clone(),
                });
                Ok(var_type.clone())
            } else {
                Err(TypeError::new(
                    format!("Undefined variable: {}", name),
                    expr.range(),
                ))
            }
        }
        DopExpr::BooleanLiteral { .. } => Ok(DopType::Bool),
        DopExpr::StringLiteral { .. } => Ok(DopType::String),
        DopExpr::NumberLiteral { .. } => Ok(DopType::Number),
        DopExpr::PropertyAccess {
            object: base_expr,
            property,
            property_range,
            ..
        } => {
            let base_type = typecheck_expr(base_expr, env, annotations, errors)?;

            match &base_type {
                DopType::Object(props) => {
                    if let Some(prop_type) = props.get(property) {
                        Ok(prop_type.clone())
                    } else {
                        Err(TypeError::new(
                            format!("Property {} not found in object", property),
                            *property_range,
                        ))
                    }
                }
                _ => Err(TypeError::new(
                    format!("{} can not be used as an object", base_type),
                    base_expr.range(),
                )),
            }
        }
        DopExpr::BinaryOp {
            left,
            operator: BinaryOp::Equal,
            right,
            ..
        } => {
            let left_type = typecheck_expr(left, env, annotations, errors)?;
            let right_type = typecheck_expr(right, env, annotations, errors)?;

            // Both operands should have the same type for equality comparison
            if left_type != right_type {
                return Err(TypeError::new(
                    format!("Can not compare {} to {}", left_type, right_type),
                    expr.range(),
                ));
            }

            // The result of == is always boolean
            Ok(DopType::Bool)
        }
        DopExpr::UnaryOp {
            operator: UnaryOp::Not,
            operand: expr,
            ..
        } => {
            let expr_type = typecheck_expr(expr, env, annotations, errors)?;

            // Negation only works on boolean expressions
            if !is_subtype(&expr_type, &DopType::Bool) {
                return Err(TypeError::new(
                    "Negation operator can only be applied to boolean values".to_string(),
                    expr.range(),
                ));
            }

            // The result of ! is always boolean
            Ok(DopType::Bool)
        }
        DopExpr::ArrayLiteral { elements, .. } => {
            if elements.is_empty() {
                // Empty array has unknown element type
                Ok(DopType::Array(None))
            } else {
                // Check the type of the first element
                let first_type = typecheck_expr(&elements[0], env, annotations, errors)?;

                // Check that all elements have the same type
                for element in elements.iter().skip(1) {
                    let element_type = typecheck_expr(element, env, annotations, errors)?;
                    if element_type != first_type {
                        return Err(TypeError::new(
                            format!(
                                "Array elements must all have the same type, found {} and {}",
                                first_type, element_type
                            ),
                            expr.range(),
                        ));
                    }
                }

                Ok(DopType::Array(Some(Box::new(first_type))))
            }
        }
        DopExpr::ObjectLiteral { properties, .. } => {
            let mut object_properties = BTreeMap::new();

            for (key, value_expr) in properties {
                let value_type = typecheck_expr(value_expr, env, annotations, errors)?;
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
    use crate::dop::parse_parameters;
    use crate::dop::parser::parse_expr;
    use crate::tui::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};

    fn check(env_str: &str, expr_str: &str, expected: Expect) {
        let mut env = Environment::new();

        if !env_str.is_empty() {
            let mut tokenizer = DopTokenizer::new(env_str, crate::common::Position::new(1, 1))
                .expect("Failed to create tokenizer");
            let params = parse_parameters(&mut tokenizer).expect("Failed to parse environment");
            for param in params {
                env.push(param.var_name.value, param.type_annotation);
            }
        }

        let mut tokenizer = DopTokenizer::new(expr_str, crate::common::Position::new(1, 1))
            .expect("Failed to create tokenizer");
        let expr = parse_expr(&mut tokenizer).expect("Failed to parse expression");

        let mut annotations = Vec::new();
        let mut errors = Vec::new();

        let actual = match typecheck_expr(&expr, &mut env, &mut annotations, &mut errors) {
            Ok(typ) => typ.to_string(),
            Err(e) => SourceAnnotator::new()
                .with_label("error")
                .without_location()
                .without_line_numbers()
                .annotate(None, expr_str, &[e]),
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_typecheck_equality_incompatible_string_number() {
        check(
            "name: string, count: number",
            "name == count",
            expect![[r#"
                error: Can not compare string to number
                name == count
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_equality_incompatible_boolean_string() {
        check(
            "enabled: boolean, name: string",
            "enabled == name",
            expect![[r#"
                error: Can not compare boolean to string
                enabled == name
                ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_undefined_variable() {
        check(
            "",
            "undefined_var",
            expect![[r#"
                error: Undefined variable: undefined_var
                undefined_var
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_property_access_on_undefined_variable() {
        check(
            "",
            "notdefined.foo.bar",
            expect![[r#"
                error: Undefined variable: notdefined
                notdefined.foo.bar
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_property_access_on_non_object() {
        check(
            "count: number",
            "count.value",
            expect![[r#"
                error: number can not be used as an object
                count.value
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_negation_string_error() {
        check(
            "name: string",
            "!name",
            expect![[r#"
                error: Negation operator can only be applied to boolean values
                !name
                 ^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_negation_number_error() {
        check(
            "count: number",
            "!count",
            expect![[r#"
                error: Negation operator can only be applied to boolean values
                !count
                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_nested_array_property_error() {
        check(
            "config: {users: array[{profile: {name: string, active: boolean}}]}",
            "config.users.profile.name",
            expect![[r#"
                error: array[{profile: {active: boolean, name: string}}] can not be used as an object
                config.users.profile.name
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_array_property_access_error() {
        check(
            "users: array[{name: string}]",
            "users.name",
            expect![[r#"
                error: array[{name: string}] can not be used as an object
                users.name
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_unknown_property() {
        check(
            "data: {field: string}",
            "data.unknown",
            expect![[r#"
                error: Property unknown not found in object
                data.unknown
                     ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_basic_variable_lookup() {
        check("name: string", "name", expect!["string"]);
    }

    #[test]
    fn test_typecheck_string_literal() {
        check("", "'hello world'", expect!["string"]);
    }

    #[test]
    fn test_typecheck_boolean_literal_true() {
        check("", "true", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_boolean_literal_false() {
        check("", "false", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_number_literal_integer() {
        check("", "42", expect!["number"]);
    }

    #[test]
    fn test_typecheck_number_literal_float() {
        check("", "3.14", expect!["number"]);
    }

    #[test]
    fn test_typecheck_property_access() {
        check("user: {name: string}", "user.name", expect!["string"]);
    }

    #[test]
    fn test_typecheck_nested_property_access() {
        check(
            "app: {user: {profile: {name: string}}}",
            "app.user.profile.name",
            expect!["string"],
        );
    }

    #[test]
    fn test_typecheck_equality_string() {
        check("name: string", "name == 'alice'", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_equality_number() {
        check("count: number", "count == 42", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_equality_boolean() {
        check("enabled: boolean", "enabled == true", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_equality_same_object_properties() {
        check(
            "user: {name: string}, admin: {name: string}",
            "user.name == admin.name",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_complex_equality() {
        check(
            "a: boolean, b: boolean",
            "a == b == true",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_negation_variable() {
        check("enabled: boolean", "!enabled", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_negation_true() {
        check("", "!true", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_negation_false() {
        check("", "!false", expect!["boolean"]);
    }

    #[test]
    fn test_typecheck_complex_negation_equality() {
        check(
            "user: {active: boolean}",
            "!user.active == false",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_parenthesized_negation() {
        check(
            "status: {enabled: boolean}, config: {active: boolean}",
            "!(status.enabled == config.active)",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_object_array_property() {
        check(
            "data: {items: array[string]}",
            "data.items",
            expect!["array[string]"],
        );
    }

    #[test]
    fn test_typecheck_deep_property_access() {
        check(
            "system: {config: {database: {connection: {host: string}}}}",
            "system.config.database.connection.host",
            expect!["string"],
        );
    }

    #[test]
    fn test_typecheck_multiple_property_access() {
        check(
            "obj: {name: string, title: string}",
            "obj.name == obj.title",
            expect!["boolean"],
        );
    }

    #[test]
    fn test_typecheck_empty_object_literal() {
        check("", "{}", expect!["{}"]);
    }

    #[test]
    fn test_typecheck_object_literal_single_property() {
        check("", "{name: 'John'}", expect!["{name: string}"]);
    }

    #[test]
    fn test_typecheck_object_literal_multiple_properties() {
        check(
            "",
            "{a: 'foo', b: 1, c: true}",
            expect!["{a: string, b: number, c: boolean}"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_complex_expressions() {
        check(
            "user: {name: string, disabled: boolean}",
            "{user: user.name, active: !user.disabled}",
            expect!["{active: boolean, user: string}"],
        );
    }

    #[test]
    fn test_typecheck_nested_object_literal() {
        check(
            "",
            "{nested: {inner: 'value'}}",
            expect!["{nested: {inner: string}}"],
        );
    }

    #[test]
    fn test_typecheck_array_trailing_comma_numbers() {
        check("", "[\n\t1,\n\t2,\n\t3,\n]", expect!["array[number]"]);
    }

    #[test]
    fn test_typecheck_array_trailing_comma_single() {
        check("", "[\n\t'hello',\n]", expect!["array[string]"]);
    }

    #[test]
    fn test_typecheck_object_literal_trailing_comma() {
        check(
            "",
            "{\n\ta: 'foo',\n\tb: 1,\n}",
            expect!["{a: string, b: number}"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_trailing_comma_single() {
        check("", "{\n\tname: 'John',\n}", expect!["{name: string}"]);
    }
}
