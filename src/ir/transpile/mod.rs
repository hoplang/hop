pub mod pretty_go;
pub mod pretty_js;

use pretty::BoxDoc;
pub use pretty_go::GoTranspiler;
pub use pretty_js::{JsTranspiler, LanguageMode};

use crate::dop::r#type::Type;
use crate::ir::ast::{BinaryOp, IrEntrypoint, IrExpr, IrModule, IrStatement, UnaryOp};
use std::collections::BTreeMap;

pub trait Transpiler {
    fn transpile_entrypoint<'a>(&self, name: &'a str, entrypoint: &'a IrEntrypoint) -> BoxDoc<'a>;
    fn transpile_module(&self, ir_module: &IrModule) -> String;
}

pub trait StatementTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a>;
    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a>;
    fn transpile_if<'a>(&self, condition: &'a IrExpr, body: &'a [IrStatement]) -> BoxDoc<'a>;
    fn transpile_for<'a>(
        &self,
        var: &'a str,
        array: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a>;
    fn transpile_let<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a>;
    fn transpile_statement<'a>(&self, statement: &'a IrStatement) -> BoxDoc<'a> {
        match statement {
            IrStatement::Write { content, .. } => self.transpile_write(content),
            IrStatement::WriteExpr { expr, escape, .. } => self.transpile_write_expr(expr, *escape),
            IrStatement::If {
                condition, body, ..
            } => self.transpile_if(condition, body),
            IrStatement::For {
                var, array, body, ..
            } => self.transpile_for(var.as_str(), array, body),
            IrStatement::Let {
                var, value, body, ..
            } => self.transpile_let(var.as_str(), value, body),
        }
    }
    fn transpile_statements<'a>(&self, statements: &'a [IrStatement]) -> BoxDoc<'a> {
        BoxDoc::intersperse(
            statements.iter().map(|stmt| self.transpile_statement(stmt)),
            BoxDoc::hardline(),
        )
    }
}

pub trait TypeTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_string_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_number_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_array_type<'a>(&self, element_type: Option<&'a Type>) -> BoxDoc<'a>;
    fn transpile_object_type<'a>(&self, fields: &'a BTreeMap<String, Type>) -> BoxDoc<'a>;
    fn transpile_type<'a>(&self, ty: &'a Type) -> BoxDoc<'a> {
        match ty {
            Type::Bool => self.transpile_bool_type(),
            Type::String => self.transpile_string_type(),
            Type::Number => self.transpile_number_type(),
            Type::Array(elem) => self.transpile_array_type(elem.as_deref()),
            Type::Object(fields) => self.transpile_object_type(fields),
        }
    }
}

/// Expression transpilation trait using pretty-printing
pub trait ExpressionTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_property_access<'a>(&self, object: &'a IrExpr, property: &'a str) -> BoxDoc<'a>;
    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a>;
    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a>;
    fn transpile_number_literal<'a>(&self, value: &'a serde_json::Number) -> BoxDoc<'a>;
    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> BoxDoc<'a>;
    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(String, IrExpr)],
        field_types: &'a BTreeMap<String, Type>,
    ) -> BoxDoc<'a>;
    fn transpile_string_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_bool_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_expr<'a>(&self, expr: &'a IrExpr) -> BoxDoc<'a> {
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
