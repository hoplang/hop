use super::Type;
use super::ast::{BinaryOp, Expr, TypedExpr, UnaryOp};
use super::type_error::TypeError;
use crate::document::document_cursor::Ranged as _;
use crate::hop::environment::Environment;
use crate::hop::type_checker::TypeAnnotation;
use std::collections::BTreeMap;

pub fn typecheck_expr(
    expr: &Expr,
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    match expr {
        Expr::Var { value: name, .. } => {
            if let Some(var_type) = env.lookup(name.as_str()) {
                annotations.push(TypeAnnotation {
                    range: expr.range().clone(),
                    typ: var_type.clone(),
                    name: name.to_string(),
                });
                Ok(Expr::Var {
                    value: name.clone(),
                    annotation: var_type.clone(),
                })
            } else {
                Err(TypeError::UndefinedVariable {
                    name: name.as_str().to_string(),
                    range: expr.range().clone(),
                })
            }
        }
        Expr::BooleanLiteral { value, .. } => Ok(Expr::BooleanLiteral {
            value: *value,
            annotation: Type::Bool,
        }),
        Expr::StringLiteral { value, .. } => Ok(Expr::StringLiteral {
            value: value.clone(),
            annotation: Type::String,
        }),
        Expr::NumberLiteral { value, .. } => Ok(Expr::NumberLiteral {
            value: value.clone(),
            annotation: Type::Number,
        }),
        Expr::PropertyAccess {
            object: base_expr,
            property,
            annotation: range,
            ..
        } => {
            let typed_base = typecheck_expr(base_expr, env, annotations)?;
            let base_type = typed_base.annotation().clone();

            match &base_type {
                Type::Object(props) => {
                    if let Some(prop_type) = props.get(property.as_str()) {
                        Ok(Expr::PropertyAccess {
                            object: Box::new(typed_base),
                            property: property.clone(),
                            annotation: prop_type.clone(),
                        })
                    } else {
                        Err(TypeError::PropertyNotFoundInObject {
                            property: property.to_string(),
                            dop_type: base_type.clone(),
                            range: range.clone(),
                        })
                    }
                }
                _ => Err(TypeError::CannotUseAsObject {
                    typ: base_type.to_string(),
                    range: base_expr.range().clone(),
                }),
            }
        }
        Expr::BinaryOp {
            left,
            operator: BinaryOp::Equal,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.annotation();
            let right_type = typed_right.annotation();

            // Both operands should have the same type for equality comparison
            if left_type != right_type {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            // The result of == is always boolean
            Ok(Expr::BinaryOp {
                left: Box::new(typed_left),
                operator: BinaryOp::Equal,
                right: Box::new(typed_right),
                annotation: Type::Bool,
            })
        }
        Expr::UnaryOp {
            operator: UnaryOp::Not,
            operand,
            ..
        } => {
            let typed_operand = typecheck_expr(operand, env, annotations)?;
            let operand_type = typed_operand.annotation();

            // Negation only works on boolean expressions
            if !operand_type.is_subtype(&Type::Bool) {
                return Err(TypeError::NegationRequiresBoolean {
                    range: operand.range().clone(),
                });
            }

            // The result of ! is always boolean
            Ok(Expr::UnaryOp {
                operator: UnaryOp::Not,
                operand: Box::new(typed_operand),
                annotation: Type::Bool,
            })
        }
        Expr::ArrayLiteral { elements, .. } => {
            if elements.is_empty() {
                // Empty array has unknown element type
                Ok(Expr::ArrayLiteral {
                    elements: vec![],
                    annotation: Type::Array(None),
                })
            } else {
                let mut typed_elements = Vec::new();

                // Check the type of the first element
                let first_typed = typecheck_expr(&elements[0], env, annotations)?;
                let first_type = first_typed.annotation().clone();
                typed_elements.push(first_typed);

                // Check that all elements have the same type
                for element in elements.iter().skip(1) {
                    let typed_element = typecheck_expr(element, env, annotations)?;
                    let element_type = typed_element.annotation();
                    if *element_type != first_type {
                        return Err(TypeError::ArrayTypeMismatch {
                            expected: first_type.to_string(),
                            found: element_type.to_string(),
                            range: element.range().clone(),
                        });
                    }
                    typed_elements.push(typed_element);
                }

                Ok(Expr::ArrayLiteral {
                    elements: typed_elements,
                    annotation: Type::Array(Some(Box::new(first_type))),
                })
            }
        }
        Expr::ObjectLiteral { properties, .. } => {
            let mut object_properties = BTreeMap::new();
            let mut typed_properties = Vec::new();

            for (key, value_expr) in properties {
                let typed_value = typecheck_expr(value_expr, env, annotations)?;
                let value_type = typed_value.annotation().clone();
                object_properties.insert(key.to_string(), value_type);
                typed_properties.push((key.clone(), typed_value));
            }

            Ok(Expr::ObjectLiteral {
                properties: typed_properties,
                annotation: Type::Object(object_properties),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::dop::Parser;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(env_str: &str, expr_str: &str, expected: Expect) {
        let mut env = Environment::new();

        if !env_str.is_empty() {
            let mut parser = Parser::from(env_str);
            let params = parser
                .parse_parameters()
                .expect("Failed to parse environment");
            for param in params {
                let _ = env.push(param.var_name.to_string(), param.var_type);
            }
        }

        let mut parser = Parser::from(expr_str);
        let expr = parser.parse_expr().expect("Failed to parse expression");

        let mut annotations = Vec::new();

        let actual = match typecheck_expr(&expr, &mut env, &mut annotations) {
            Ok(typed_expr) => typed_expr.annotation().to_string(),
            Err(e) => DocumentAnnotator::new()
                .with_label("error")
                .without_location()
                .without_line_numbers()
                .annotate(None, [e]),
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
                error: Property unknown not found in object {field: string}
                data.unknown
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_basic_variable_lookup() {
        check("name: string", "name", expect!["string"]);
    }

    #[test]
    fn test_typecheck_string_literal() {
        check("", r#""hello world""#, expect!["string"]);
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
        check("name: string", r#"name == "alice""#, expect!["boolean"]);
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
        check("", r#"{name: "John"}"#, expect!["{name: string}"]);
    }

    #[test]
    fn test_typecheck_object_literal_multiple_properties() {
        check(
            "",
            r#"{a: "foo", b: 1, c: true}"#,
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
            r#"{nested: {inner: "value"}}"#,
            expect!["{nested: {inner: string}}"],
        );
    }

    #[test]
    fn test_typecheck_array_different_types() {
        check(
            "",
            "[1, true]",
            expect![[r#"
                error: Array elements must all have the same type, found number and boolean
                [1, true]
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_array_trailing_comma_numbers() {
        check("", "[\n\t1,\n\t2,\n\t3,\n]", expect!["array[number]"]);
    }

    #[test]
    fn test_typecheck_array_trailing_comma_single() {
        check(
            "",
            indoc! {r#"
            [
            	"hello",
            ]
        "#},
            expect!["array[string]"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_trailing_comma() {
        check(
            "",
            indoc! {r#"
                {
                	a: "foo",
                	b: 1,
                }
            "#},
            expect!["{a: string, b: number}"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_trailing_comma_single() {
        check(
            "",
            indoc! {r#"
            {
            	name: "John",
            }
        "#},
            expect!["{name: string}"],
        );
    }
}
