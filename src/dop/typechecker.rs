use super::ast::{BinaryOp, DopExpr, UnaryOp};
use crate::document::document_cursor::{DocumentRange, Ranged as _};
use crate::hop::environment::Environment;
use crate::hop::pretty::Pretty;
use crate::hop::type_error::TypeError;
use crate::hop::typechecker::TypeAnnotation;
use pretty::RcDoc;
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

impl DopType {
    /// Check if `subtype` is a subtype of `supertype`
    pub fn is_subtype(&self, supertype: &DopType) -> bool {
        match (self, supertype) {
            // Exact matches
            (DopType::Bool, DopType::Bool) => true,
            (DopType::String, DopType::String) => true,
            (DopType::Number, DopType::Number) => true,

            // Arrays are covariant in their element type
            (DopType::Array(sub_elem), DopType::Array(super_elem)) => {
                match (sub_elem, super_elem) {
                    (Some(sub_type), Some(super_type)) => sub_type.is_subtype(super_type),
                    (None, None) => true,
                    (None, Some(_)) => true, // Empty array can be subtype of any array
                    (Some(_), None) => false, // Typed array cannot be subtype of empty array
                }
            }

            // Objects: subtype must have all properties of supertype with compatible types
            (DopType::Object(sub_props), DopType::Object(super_props)) => {
                super_props.iter().all(|(key, super_type)| {
                    sub_props
                        .get(key)
                        .is_some_and(|sub_type| sub_type.is_subtype(super_type))
                })
            }

            // Otherwise, not a subtype
            _ => false,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RangedDopType {
    pub dop_type: DopType,
    pub range: DocumentRange,
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Pretty for DopType {
    fn to_doc(&self) -> RcDoc<'static> {
        match self {
            // Format a simple type.
            DopType::String => RcDoc::text("string"),
            DopType::Number => RcDoc::text("number"),
            DopType::Bool => RcDoc::text("boolean"),

            // Format an Array type.
            // E.g. {name: string}
            DopType::Array(elem_type) => match elem_type {
                Some(elem) => RcDoc::nil()
                    .append(RcDoc::text("array["))
                    .append(elem.to_doc())
                    .append(RcDoc::text("]")),
                None => RcDoc::text("array"),
            },

            // Format an Object type.
            // E.g. {name: string}
            DopType::Object(fields) => {
                RcDoc::nil()
                    .append(RcDoc::text("{"))
                    .append(
                        RcDoc::nil()
                            // soft line break
                            .append(RcDoc::line_())
                            .append(RcDoc::intersperse(
                                fields.iter().map(|(key, typ)| {
                                    RcDoc::nil()
                                        // key
                                        .append(RcDoc::text(key.clone()))
                                        // separator
                                        .append(RcDoc::text(": "))
                                        // value
                                        .append(typ.to_doc())
                                }),
                                // intersperse with comma followed by line that acts
                                // as space if laid out on a single line
                                RcDoc::text(",").append(RcDoc::line()),
                            ))
                            // trailing comma if laid out on multiple lines
                            .append(RcDoc::text(",").flat_alt(RcDoc::nil()))
                            // soft line break
                            .append(RcDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(RcDoc::text("}"))
            }
        }
    }
}

pub fn typecheck_expr(
    expr: &DopExpr,
    env: &mut Environment<DopType>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<DopType, TypeError> {
    match expr {
        DopExpr::Variable { value: name, .. } => {
            if let Some(var_type) = env.lookup(name.as_str()) {
                annotations.push(TypeAnnotation {
                    range: expr.range().clone(),
                    typ: var_type.clone(),
                    name: name.to_string(),
                });
                Ok(var_type.clone())
            } else {
                Err(TypeError::UndefinedVariable {
                    name: name.as_str().to_string(),
                    range: expr.range().clone(),
                })
            }
        }
        DopExpr::BooleanLiteral { .. } => Ok(DopType::Bool),
        DopExpr::StringLiteral { .. } => Ok(DopType::String),
        DopExpr::NumberLiteral { .. } => Ok(DopType::Number),
        DopExpr::PropertyAccess {
            object: base_expr,
            property,
            ..
        } => {
            let base_type = typecheck_expr(base_expr, env, annotations)?;

            match &base_type {
                DopType::Object(props) => {
                    if let Some(prop_type) = props.get(property.as_str()) {
                        Ok(prop_type.clone())
                    } else {
                        Err(TypeError::PropertyNotFoundInObject {
                            property: property.as_str().to_string(),
                            range: property.clone(),
                        })
                    }
                }
                _ => Err(TypeError::CannotUseAsObject {
                    typ: base_type.to_string(),
                    range: base_expr.range().clone(),
                }),
            }
        }
        DopExpr::BinaryOp {
            left,
            operator: BinaryOp::Equal,
            right,
            ..
        } => {
            let left_type = typecheck_expr(left, env, annotations)?;
            let right_type = typecheck_expr(right, env, annotations)?;

            // Both operands should have the same type for equality comparison
            if left_type != right_type {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: expr.range().clone(),
                });
            }

            // The result of == is always boolean
            Ok(DopType::Bool)
        }
        DopExpr::UnaryOp {
            operator: UnaryOp::Not,
            operand: expr,
            ..
        } => {
            let expr_type = typecheck_expr(expr, env, annotations)?;

            // Negation only works on boolean expressions
            if !expr_type.is_subtype(&DopType::Bool) {
                return Err(TypeError::NegationRequiresBoolean {
                    range: expr.range().clone(),
                });
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
                let first_type = typecheck_expr(&elements[0], env, annotations)?;

                // Check that all elements have the same type
                for element in elements.iter().skip(1) {
                    let element_type = typecheck_expr(element, env, annotations)?;
                    if element_type != first_type {
                        return Err(TypeError::ArrayTypeMismatch {
                            expected: first_type.to_string(),
                            found: element_type.to_string(),
                            range: element.range().clone(),
                        });
                    }
                }

                Ok(DopType::Array(Some(Box::new(first_type))))
            }
        }
        DopExpr::ObjectLiteral { properties, .. } => {
            let mut object_properties = BTreeMap::new();

            for (key, value_expr) in properties {
                let value_type = typecheck_expr(value_expr, env, annotations)?;
                object_properties.insert(key.to_string(), value_type);
            }

            Ok(DopType::Object(object_properties))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::dop::DopParser;
    use expect_test::{Expect, expect};

    fn check(env_str: &str, expr_str: &str, expected: Expect) {
        let mut env = Environment::new();

        if !env_str.is_empty() {
            let mut parser = DopParser::from(env_str);
            let params = parser
                .parse_parameters()
                .expect("Failed to parse environment");
            for param in params {
                let _ = env.push(param.var_name.to_string(), param.var_type);
            }
        }

        let mut parser = DopParser::from(expr_str);
        let expr = parser.parse_expr().expect("Failed to parse expression");

        let mut annotations = Vec::new();

        let actual = match typecheck_expr(&expr, &mut env, &mut annotations) {
            Ok(typ) => typ.to_string(),
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
