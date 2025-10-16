use super::Type;
use super::expr::{AnnotatedExpr, BinaryOp, Expr};
use super::r#type::NumericType;
use super::type_error::TypeError;
use super::typed_expr::SimpleTypedExpr;
use crate::document::document_cursor::Ranged as _;
use crate::hop::environment::Environment;
use crate::hop::type_checker::TypeAnnotation;
use std::collections::BTreeMap;

pub fn typecheck_expr(
    expr: &Expr,
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<SimpleTypedExpr, TypeError> {
    match expr {
        AnnotatedExpr::Var { value: name, .. } => {
            if let Some(var_type) = env.lookup(name.as_str()) {
                annotations.push(TypeAnnotation {
                    range: expr.range().clone(),
                    typ: var_type.clone(),
                    name: name.to_string(),
                });
                Ok(SimpleTypedExpr::Var {
                    value: name.clone(),
                    kind: var_type.clone(),
                    annotation: (),
                })
            } else {
                Err(TypeError::UndefinedVariable {
                    name: name.as_str().to_string(),
                    range: expr.range().clone(),
                })
            }
        }
        AnnotatedExpr::BooleanLiteral { value, .. } => Ok(SimpleTypedExpr::BooleanLiteral {
            value: *value,
            annotation: (),
        }),
        AnnotatedExpr::StringLiteral { value, .. } => Ok(SimpleTypedExpr::StringLiteral {
            value: value.clone(),
            annotation: (),
        }),
        AnnotatedExpr::IntLiteral { value, .. } => Ok(SimpleTypedExpr::IntLiteral {
            value: *value,
            annotation: (),
        }),
        AnnotatedExpr::FloatLiteral { value, .. } => Ok(SimpleTypedExpr::FloatLiteral {
            value: *value,
            annotation: (),
        }),
        AnnotatedExpr::PropertyAccess {
            object: base_expr,
            property,
            annotation: range,
            ..
        } => {
            let typed_base = typecheck_expr(base_expr, env, annotations)?;
            let base_type = typed_base.as_type();

            match &base_type {
                Type::Object(props) => {
                    if let Some(prop_type) = props.get(property) {
                        Ok(SimpleTypedExpr::PropertyAccess {
                            kind: prop_type.clone(),
                            object: Box::new(typed_base),
                            property: property.clone(),
                            annotation: (),
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
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::Eq,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let Some(left_comparable) = left_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            let Some(right_comparable) = right_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::Equals {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
                annotation: (),
            })
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::NotEq,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let Some(left_comparable) = left_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            let Some(right_comparable) = right_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::NotEquals {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
                annotation: (),
            })
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::LessThan,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::LessThan {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
                annotation: (),
            })
        }

        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::GreaterThan,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::GreaterThan {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
                annotation: (),
            })
        }

        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::LessThanOrEqual,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::LessThanOrEqual {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
                annotation: (),
            })
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::GreaterThanOrEqual,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::GreaterThanOrEqual {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
                annotation: (),
            })
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::LogicalAnd,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // LogicalAnd only works with Bool expressions
            if !left_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalAndRequiresBoolean {
                    range: left.range().clone(),
                });
            }

            if !right_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalAndRequiresBoolean {
                    range: right.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::LogicalAnd {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                annotation: (),
            })
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::LogicalOr,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // LogicalOr only works with Bool expressions
            if !left_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalOrRequiresBoolean {
                    range: left.range().clone(),
                });
            }

            if !right_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalOrRequiresBoolean {
                    range: right.range().clone(),
                });
            }

            Ok(SimpleTypedExpr::LogicalOr {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                annotation: (),
            })
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::Plus,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // Plus operator works for:
            // 1. String concatenation (String + String)
            // 2. Integer addition (Int + Int)
            // 3. Float addition (Float + Float)

            match (left_type, right_type) {
                (Type::String, Type::String) => Ok(SimpleTypedExpr::StringConcat {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    annotation: (),
                }),
                (Type::Int, Type::Int) => Ok(SimpleTypedExpr::NumericAdd {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Int,
                    annotation: (),
                }),
                (Type::Float, Type::Float) => Ok(SimpleTypedExpr::NumericAdd {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Float,
                    annotation: (),
                }),
                _ => {
                    // Incompatible types for addition
                    Err(TypeError::IncompatibleTypesForAddition {
                        left_type: left_type.to_string(),
                        right_type: right_type.to_string(),
                        range: left.range().clone().to(right.range().clone()),
                    })
                }
            }
        }
        AnnotatedExpr::BinaryOp {
            left,
            operator: BinaryOp::Minus,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, annotations)?;
            let typed_right = typecheck_expr(right, env, annotations)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // Minus operator works for:
            // 1. Integer subtraction (Int - Int)
            // 2. Float subtraction (Float - Float)

            match (left_type, right_type) {
                (Type::Int, Type::Int) => Ok(SimpleTypedExpr::NumericSubtract {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Int,
                    annotation: (),
                }),
                (Type::Float, Type::Float) => Ok(SimpleTypedExpr::NumericSubtract {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Float,
                    annotation: (),
                }),
                _ => {
                    // Incompatible types for subtraction
                    Err(TypeError::IncompatibleTypesForSubtraction {
                        left_type: left_type.to_string(),
                        right_type: right_type.to_string(),
                        range: left.range().clone().to(right.range().clone()),
                    })
                }
            }
        }
        AnnotatedExpr::Negation { operand, .. } => {
            let typed_operand = typecheck_expr(operand, env, annotations)?;
            let operand_type = typed_operand.as_type();

            // Negation only works on Bool expressions
            if !operand_type.is_subtype(&Type::Bool) {
                return Err(TypeError::NegationRequiresBoolean {
                    range: operand.range().clone(),
                });
            }

            // The result of ! is always boolean
            Ok(SimpleTypedExpr::Negation {
                operand: Box::new(typed_operand),
                annotation: (),
            })
        }
        AnnotatedExpr::ArrayLiteral { elements, .. } => {
            if elements.is_empty() {
                // Empty array has unknown element type
                Ok(SimpleTypedExpr::ArrayLiteral {
                    elements: vec![],
                    kind: Type::Array(None),
                    annotation: (),
                })
            } else {
                let mut typed_elements = Vec::new();

                // Check the type of the first element
                let first_typed = typecheck_expr(&elements[0], env, annotations)?;
                let first_type = first_typed.as_type().clone();
                typed_elements.push(first_typed);

                // Check that all elements have the same type
                for element in elements.iter().skip(1) {
                    let typed_element = typecheck_expr(element, env, annotations)?;
                    let element_type = typed_element.as_type();
                    if *element_type != first_type {
                        return Err(TypeError::ArrayTypeMismatch {
                            expected: first_type.to_string(),
                            found: element_type.to_string(),
                            range: element.range().clone(),
                        });
                    }
                    typed_elements.push(typed_element);
                }

                Ok(SimpleTypedExpr::ArrayLiteral {
                    elements: typed_elements,
                    kind: Type::Array(Some(Box::new(first_type))),
                    annotation: (),
                })
            }
        }
        AnnotatedExpr::ObjectLiteral { properties, .. } => {
            let mut object_properties = BTreeMap::new();
            let mut typed_properties = Vec::new();

            for (key, value_expr) in properties {
                let typed_value = typecheck_expr(value_expr, env, annotations)?;
                let value_type = typed_value.as_type().clone();
                object_properties.insert(key.clone(), value_type);
                typed_properties.push((key.clone(), typed_value));
            }

            Ok(SimpleTypedExpr::ObjectLiteral {
                properties: typed_properties,
                kind: Type::Object(object_properties),
                annotation: (),
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
            Ok(typed_expr) => typed_expr.as_type().to_string(),
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
            "name: String, count: Float",
            "name == count",
            expect![[r#"
                error: Can not compare String to Float
                name == count
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_equality_incompatible_boolean_string() {
        check(
            "enabled: Bool, name: String",
            "enabled == name",
            expect![[r#"
                error: Can not compare Bool to String
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
            "count: Float",
            "count.value",
            expect![[r#"
                error: Float can not be used as an object
                count.value
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_negation_string_error() {
        check(
            "name: String",
            "!name",
            expect![[r#"
                error: Negation operator can only be applied to Bool values
                !name
                 ^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_negation_number_error() {
        check(
            "count: Float",
            "!count",
            expect![[r#"
                error: Negation operator can only be applied to Bool values
                !count
                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_nested_array_property_error() {
        check(
            "config: {users: Array[{profile: {name: String, active: Bool}}]}",
            "config.users.profile.name",
            expect![[r#"
                error: Array[{profile: {active: Bool, name: String}}] can not be used as an object
                config.users.profile.name
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_array_property_access_error() {
        check(
            "users: Array[{name: String}]",
            "users.name",
            expect![[r#"
                error: Array[{name: String}] can not be used as an object
                users.name
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_unknown_property() {
        check(
            "data: {field: String}",
            "data.unknown",
            expect![[r#"
                error: Property unknown not found in object {field: String}
                data.unknown
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_basic_variable_lookup() {
        check("name: String", "name", expect!["String"]);
    }

    #[test]
    fn test_typecheck_string_literal() {
        check("", r#""hello world""#, expect!["String"]);
    }

    #[test]
    fn test_typecheck_boolean_literal_true() {
        check("", "true", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_boolean_literal_false() {
        check("", "false", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_int_literal() {
        check("", "42", expect!["Int"]);
    }

    #[test]
    fn test_typecheck_number_literal_float() {
        check("", "3.14", expect!["Float"]);
    }

    #[test]
    fn test_typecheck_property_access() {
        check("user: {name: String}", "user.name", expect!["String"]);
    }

    #[test]
    fn test_typecheck_nested_property_access() {
        check(
            "app: {user: {profile: {name: String}}}",
            "app.user.profile.name",
            expect!["String"],
        );
    }

    #[test]
    fn test_typecheck_equality_string() {
        check("name: String", r#"name == "alice""#, expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_equality_number() {
        check(
            "count: Float",
            "count == 42",
            expect![[r#"
                error: Can not compare Float to Int
                count == 42
                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_equality_boolean() {
        check("enabled: Bool", "enabled == true", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_equality_same_object_properties() {
        check(
            "user: {name: String}, admin: {name: String}",
            "user.name == admin.name",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_complex_equality() {
        check(
            "a: Bool, b: Bool",
            "a == b == true",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_negation_variable() {
        check("enabled: Bool", "!enabled", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_negation_true() {
        check("", "!true", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_negation_false() {
        check("", "!false", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_greater_than_int() {
        check("x: Int, y: Int", "x > y", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_greater_than_float() {
        check("x: Float, y: Float", "x > y", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_greater_than_mixed_error() {
        check(
            "x: Int, y: Float",
            "x > y",
            expect![[r#"
                error: Can not compare Int to Float
                x > y
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_less_than_or_equal_int() {
        check("x: Int, y: Int", "x <= y", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_less_than_or_equal_float() {
        check("x: Float, y: Float", "x <= y", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_less_than_or_equal_mixed_error() {
        check(
            "x: Int, y: Float",
            "x <= y",
            expect![[r#"
                error: Can not compare Int to Float
                x <= y
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_greater_than_or_equal_int() {
        check("x: Int, y: Int", "x >= y", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_greater_than_or_equal_float() {
        check("x: Float, y: Float", "x >= y", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_greater_than_or_equal_mixed_error() {
        check(
            "x: Int, y: Float",
            "x >= y",
            expect![[r#"
                error: Can not compare Int to Float
                x >= y
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_complex_negation_equality() {
        check(
            "user: {active: Bool}",
            "!user.active == false",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_parenthesized_negation() {
        check(
            "status: {enabled: Bool}, config: {active: Bool}",
            "!(status.enabled == config.active)",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_object_array_property() {
        check(
            "data: {items: Array[String]}",
            "data.items",
            expect!["Array[String]"],
        );
    }

    #[test]
    fn test_typecheck_deep_property_access() {
        check(
            "system: {config: {database: {connection: {host: String}}}}",
            "system.config.database.connection.host",
            expect!["String"],
        );
    }

    #[test]
    fn test_typecheck_multiple_property_access() {
        check(
            "obj: {name: String, title: String}",
            "obj.name == obj.title",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_empty_object_literal() {
        check("", "{}", expect!["{}"]);
    }

    #[test]
    fn test_typecheck_object_literal_single_property() {
        check("", r#"{name: "John"}"#, expect!["{name: String}"]);
    }

    #[test]
    fn test_typecheck_object_literal_multiple_properties() {
        check(
            "",
            r#"{a: "foo", b: 1, c: true}"#,
            expect!["{a: String, b: Int, c: Bool}"],
        );
    }

    #[test]
    fn test_typecheck_object_literal_complex_expressions() {
        check(
            "user: {name: String, disabled: Bool}",
            "{user: user.name, active: !user.disabled}",
            expect!["{active: Bool, user: String}"],
        );
    }

    #[test]
    fn test_typecheck_nested_object_literal() {
        check(
            "",
            r#"{nested: {inner: "value"}}"#,
            expect!["{nested: {inner: String}}"],
        );
    }

    #[test]
    fn test_typecheck_array_different_types() {
        check(
            "",
            "[1, true]",
            expect![[r#"
                error: Array elements must all have the same type, found Int and Bool
                [1, true]
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_array_trailing_comma_numbers() {
        check("", "[\n\t1,\n\t2,\n\t3,\n]", expect!["Array[Int]"]);
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
            expect!["Array[String]"],
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
            expect!["{a: String, b: Int}"],
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
            expect!["{name: String}"],
        );
    }

    #[test]
    fn test_typecheck_string_concatenation() {
        check("", r#""hello" + "world""#, expect!["String"]);
    }

    #[test]
    fn test_typecheck_string_concatenation_multiple() {
        check("", r#""hello" + " " + "world""#, expect!["String"]);
    }

    #[test]
    fn test_typecheck_string_concatenation_with_variables() {
        check(
            "greeting: String, name: String",
            r#"greeting + " " + name"#,
            expect!["String"],
        );
    }

    #[test]
    fn test_typecheck_string_concatenation_error_left_number() {
        check(
            "",
            r#"42 + "hello""#,
            expect![[r#"
                error: Cannot add values of incompatible types: Int + String
                42 + "hello"
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_string_concatenation_error_right_boolean() {
        check(
            "",
            r#""hello" + true"#,
            expect![[r#"
                error: Cannot add values of incompatible types: String + Bool
                "hello" + true
                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_string_concatenation_error_both_numbers() {
        check("", r#"42 + 58"#, expect!["Int"]);
    }

    #[test]
    fn test_typecheck_string_concatenation_with_property_access() {
        check(
            "user: {first_name: String, last_name: String}",
            r#"user.first_name + " " + user.last_name"#,
            expect!["String"],
        );
    }

    #[test]
    fn test_typecheck_string_concatenation_result_comparison() {
        check("", r#""a" + "b" == "ab""#, expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_logical_and_boolean_variables() {
        check("a: Bool, b: Bool", "a && b", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_logical_and_boolean_literals() {
        check("", "true && false", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_logical_and_property_access() {
        check(
            "user: {enabled: Bool, active: Bool}",
            "user.enabled && user.active",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_logical_and_with_comparison() {
        check(
            "x: Int, y: Int, enabled: Bool",
            "x > y && enabled",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_logical_and_error_left_string() {
        check(
            "name: String, enabled: Bool",
            "name && enabled",
            expect![[r#"
                error: Logical AND operator can only be applied to Bool values
                name && enabled
                ^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_logical_and_error_right_int() {
        check(
            "enabled: Bool, count: Int",
            "enabled && count",
            expect![[r#"
                error: Logical AND operator can only be applied to Bool values
                enabled && count
                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_logical_and_error_both_strings() {
        check(
            "a: String, b: String",
            "a && b",
            expect![[r#"
                error: Logical AND operator can only be applied to Bool values
                a && b
                ^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_logical_and_precedence() {
        check(
            "a: Bool, b: Bool, c: Bool",
            "a && b == c",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_logical_or_boolean_variables() {
        check("a: Bool, b: Bool", "a || b", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_logical_or_boolean_literals() {
        check("", "true || false", expect!["Bool"]);
    }

    #[test]
    fn test_typecheck_logical_or_property_access() {
        check(
            "user: {enabled: Bool, active: Bool}",
            "user.enabled || user.active",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_logical_or_with_comparison() {
        check(
            "x: Int, y: Int, enabled: Bool",
            "x > y || enabled",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_logical_or_error_left_string() {
        check(
            "name: String, enabled: Bool",
            "name || enabled",
            expect![[r#"
                error: Logical OR operator can only be applied to Bool values
                name || enabled
                ^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_logical_or_error_right_int() {
        check(
            "enabled: Bool, count: Int",
            "enabled || count",
            expect![[r#"
                error: Logical OR operator can only be applied to Bool values
                enabled || count
                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_logical_or_error_both_strings() {
        check(
            "a: String, b: String",
            "a || b",
            expect![[r#"
                error: Logical OR operator can only be applied to Bool values
                a || b
                ^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_logical_or_precedence() {
        check(
            "a: Bool, b: Bool, c: Bool",
            "a || b == c",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_mixed_logical_operators() {
        check(
            "a: Bool, b: Bool, c: Bool",
            "a && b || c",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_logical_operator_precedence_complex() {
        check(
            "a: Bool, b: Bool, c: Bool, d: Bool",
            "a || b && c || d",
            expect!["Bool"],
        );
    }

    #[test]
    fn test_typecheck_int_addition() {
        check("x: Int, y: Int", "x + y", expect!["Int"]);
    }

    #[test]
    fn test_typecheck_float_addition() {
        check("x: Float, y: Float", "x + y", expect!["Float"]);
    }

    #[test]
    fn test_typecheck_string_addition() {
        check("s1: String, s2: String", "s1 + s2", expect!["String"]);
    }

    #[test]
    fn test_typecheck_int_literal_addition() {
        check("", "42 + 17", expect!["Int"]);
    }

    #[test]
    fn test_typecheck_float_literal_addition() {
        check("", "3.14 + 2.71", expect!["Float"]);
    }

    #[test]
    fn test_typecheck_string_literal_concatenation() {
        check("", r#""hello" + " world""#, expect!["String"]);
    }

    #[test]
    fn test_typecheck_addition_error_int_plus_float() {
        check(
            "x: Int, y: Float",
            "x + y",
            expect![[r#"
                error: Cannot add values of incompatible types: Int + Float
                x + y
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_addition_error_string_plus_int() {
        check(
            "name: String, count: Int",
            "name + count",
            expect![[r#"
                error: Cannot add values of incompatible types: String + Int
                name + count
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_addition_error_boolean_plus_int() {
        check(
            "flag: Bool, count: Int",
            "flag + count",
            expect![[r#"
                error: Cannot add values of incompatible types: Bool + Int
                flag + count
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_typecheck_addition_with_property_access() {
        check("user: {x: Int, y: Int}", "user.x + user.y", expect!["Int"]);
    }

    #[test]
    fn test_typecheck_mixed_addition_and_comparison() {
        check("a: Int, b: Int, c: Int", "a + b > c", expect!["Bool"]);
    }
}
