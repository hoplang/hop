pub mod go;
pub mod js;

pub use go::GoTranspiler;
pub use js::{JsTranspiler, LanguageMode};

use crate::ir::ast::{BinaryOp, IrExpr, UnaryOp};
use crate::dop::r#type::Type;
use std::collections::BTreeMap;

/// Trait for transpiling types to language-specific representations
pub trait TypeTranspiler {
    /// Transpile a primitive boolean type
    fn transpile_bool_type(&self) -> String;

    /// Transpile a primitive string type
    fn transpile_string_type(&self) -> String;

    /// Transpile a primitive number type
    fn transpile_number_type(&self) -> String;

    /// Transpile an array type
    fn transpile_array_type(&self, element_type: Option<&Type>) -> String;

    /// Transpile an object/struct type
    fn transpile_object_type(&self, fields: &BTreeMap<String, Type>) -> String;

    /// Main dispatcher for transpiling any type
    fn transpile_type(&self, ty: &Type) -> String {
        match ty {
            Type::Bool => self.transpile_bool_type(),
            Type::String => self.transpile_string_type(),
            Type::Number => self.transpile_number_type(),
            Type::Array(elem) => self.transpile_array_type(elem.as_deref()),
            Type::Object(fields) => self.transpile_object_type(fields),
        }
    }
}

/// Fine-grained expression transpilation trait
pub trait ExpressionTranspiler {
    // Variables and property access
    fn transpile_var(&self, name: &str) -> String;
    fn transpile_property_access(&self, object: &IrExpr, property: &str) -> String;

    // Literals
    fn transpile_string_literal(&self, value: &str) -> String;
    fn transpile_boolean_literal(&self, value: bool) -> String;
    fn transpile_number_literal(&self, value: &serde_json::Number) -> String;

    // Complex literals
    fn transpile_array_literal(&self, elements: &[IrExpr], elem_type: &Type) -> String;
    fn transpile_object_literal(&self, properties: &[(String, IrExpr)], field_types: &BTreeMap<String, Type>) -> String;

    // Binary operations (type-specific)
    fn transpile_string_equality(&self, left: &IrExpr, right: &IrExpr) -> String;
    fn transpile_bool_equality(&self, left: &IrExpr, right: &IrExpr) -> String;

    // Unary operations
    fn transpile_not(&self, operand: &IrExpr) -> String;

    // Special operations
    fn transpile_json_encode(&self, value: &IrExpr) -> String;

    // Main dispatcher (with default implementation)
    fn transpile_expr(&self, expr: &IrExpr) -> String {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(value.as_str()),
            IrExpr::PropertyAccess {
                object, property, ..
            } => self.transpile_property_access(object, property),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(value),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(*value),
            IrExpr::NumberLiteral { value, .. } => self.transpile_number_literal(value),
            IrExpr::ArrayLiteral {
                elements,
                annotation: (_, typ),
                ..
            } => self.transpile_array_literal(elements, typ),
            IrExpr::ObjectLiteral {
                properties,
                annotation: (_, typ),
                ..
            } => match typ {
                Type::Object(fields) => self.transpile_object_literal(properties, fields),
                _ => panic!("Object literal must have object type"),
            },
            IrExpr::BinaryOp {
                left,
                operator: BinaryOp::Eq,
                right,
                ..
            } => {
                // Check the types of both operands for safety
                match (left.typ(), right.typ()) {
                    (Type::Bool, Type::Bool) => self.transpile_bool_equality(left, right),
                    (Type::String, Type::String) => self.transpile_string_equality(left, right),
                    _ => panic!(
                        "Equality comparison only supported for matching bool or string types, got {:?} and {:?}",
                        left.typ(),
                        right.typ()
                    ),
                }
            }
            IrExpr::UnaryOp {
                operator: UnaryOp::Not,
                operand,
                ..
            } => self.transpile_not(operand),
            IrExpr::JsonEncode { value, .. } => self.transpile_json_encode(value),
        }
    }
}