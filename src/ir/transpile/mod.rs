pub mod go;
pub mod js;

pub use go::GoTranspiler;
pub use js::{JsTranspiler, LanguageMode};

use crate::ir::ast::{BinaryOp, IrExpr, IrStatement, UnaryOp};
use crate::dop::r#type::Type;
use std::collections::BTreeMap;

/// Trait for transpiling types to language-specific representations
pub trait TypeTranspiler {
    /// Transpile a primitive boolean type
    fn transpile_bool_type(&self, output: &mut String);

    /// Transpile a primitive string type
    fn transpile_string_type(&self, output: &mut String);

    /// Transpile a primitive number type
    fn transpile_number_type(&self, output: &mut String);

    /// Transpile an array type
    fn transpile_array_type(&self, output: &mut String, element_type: Option<&Type>);

    /// Transpile an object/struct type
    fn transpile_object_type(&self, output: &mut String, fields: &BTreeMap<String, Type>);

    /// Main dispatcher for transpiling any type
    fn transpile_type(&self, output: &mut String, ty: &Type) {
        match ty {
            Type::Bool => self.transpile_bool_type(output),
            Type::String => self.transpile_string_type(output),
            Type::Number => self.transpile_number_type(output),
            Type::Array(elem) => self.transpile_array_type(output, elem.as_deref()),
            Type::Object(fields) => self.transpile_object_type(output, fields),
        }
    }
}

/// Fine-grained expression transpilation trait
pub trait ExpressionTranspiler {
    // Variables and property access
    fn transpile_var(&self, output: &mut String, name: &str);
    fn transpile_property_access(&self, output: &mut String, object: &IrExpr, property: &str);

    // Literals
    fn transpile_string_literal(&self, output: &mut String, value: &str);
    fn transpile_boolean_literal(&self, output: &mut String, value: bool);
    fn transpile_number_literal(&self, output: &mut String, value: &serde_json::Number);

    // Complex literals
    fn transpile_array_literal(&self, output: &mut String, elements: &[IrExpr], elem_type: &Type);
    fn transpile_object_literal(&self, output: &mut String, properties: &[(String, IrExpr)], field_types: &BTreeMap<String, Type>);

    // Binary operations (type-specific)
    fn transpile_string_equality(&self, output: &mut String, left: &IrExpr, right: &IrExpr);
    fn transpile_bool_equality(&self, output: &mut String, left: &IrExpr, right: &IrExpr);

    // Unary operations
    fn transpile_not(&self, output: &mut String, operand: &IrExpr);

    // Special operations
    fn transpile_json_encode(&self, output: &mut String, value: &IrExpr);

    // Main dispatcher (with default implementation)
    fn transpile_expr(&self, output: &mut String, expr: &IrExpr) {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(output, value.as_str()),
            IrExpr::PropertyAccess {
                object, property, ..
            } => self.transpile_property_access(output, object, property),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(output, value),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(output, *value),
            IrExpr::NumberLiteral { value, .. } => self.transpile_number_literal(output, value),
            IrExpr::ArrayLiteral {
                elements,
                annotation: (_, typ),
                ..
            } => self.transpile_array_literal(output, elements, typ),
            IrExpr::ObjectLiteral {
                properties,
                annotation: (_, typ),
                ..
            } => match typ {
                Type::Object(fields) => self.transpile_object_literal(output, properties, fields),
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
                    (Type::Bool, Type::Bool) => self.transpile_bool_equality(output, left, right),
                    (Type::String, Type::String) => self.transpile_string_equality(output, left, right),
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
            } => self.transpile_not(output, operand),
            IrExpr::JsonEncode { value, .. } => self.transpile_json_encode(output, value),
        }
    }
}

/// Trait for transpiling statements to language-specific representations
pub trait StatementTranspiler: ExpressionTranspiler {
    /// Transpile a Write statement (literal string output)
    fn transpile_write(&mut self, output: &mut String, content: &str);

    /// Transpile a WriteExpr statement (expression output with optional escaping)
    fn transpile_write_expr(&mut self, output: &mut String, expr: &IrExpr, escape: bool);

    /// Transpile an If statement (conditional execution)
    fn transpile_if(&mut self, output: &mut String, condition: &IrExpr, body: &[IrStatement]);

    /// Transpile a For loop (iteration over array)
    fn transpile_for(&mut self, output: &mut String, var: &str, array: &IrExpr, body: &[IrStatement]);

    /// Transpile a Let statement (variable binding with scope)
    fn transpile_let(&mut self, output: &mut String, var: &str, value: &IrExpr, body: &[IrStatement]);

    /// Main dispatcher for transpiling statements
    fn transpile_statement(&mut self, output: &mut String, statement: &IrStatement) {
        match statement {
            IrStatement::Write { content, .. } => {
                self.transpile_write(output, content);
            }
            IrStatement::WriteExpr { expr, escape, .. } => {
                self.transpile_write_expr(output, expr, *escape);
            }
            IrStatement::If { condition, body, .. } => {
                self.transpile_if(output, condition, body);
            }
            IrStatement::For { var, array, body, .. } => {
                self.transpile_for(output, var.as_str(), array, body);
            }
            IrStatement::Let { var, value, body, .. } => {
                self.transpile_let(output, var.as_str(), value, body);
            }
        }
    }

    /// Transpile multiple statements
    fn transpile_statements(&mut self, output: &mut String, statements: &[IrStatement]) {
        for statement in statements {
            self.transpile_statement(output, statement);
        }
    }
}