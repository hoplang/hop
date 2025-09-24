pub mod go;
pub mod js;
pub mod python;

#[cfg(test)]
pub mod integration_tests;

pub use go::GoTranspiler;
pub use js::{JsTranspiler, LanguageMode};
use pretty::BoxDoc;
pub use python::PythonTranspiler;

use crate::dop::r#type::{EquatableType, Type};
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::BTreeMap;

pub trait Transpiler {
    fn transpile_entrypoint<'a>(&self, name: &'a str, entrypoint: &'a IrEntrypoint) -> BoxDoc<'a>;
    fn transpile_module(&self, entrypoints: &[IrEntrypoint]) -> String;
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
    fn transpile_int_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_array_type<'a>(&self, element_type: Option<&'a Type>) -> BoxDoc<'a>;
    fn transpile_object_type<'a>(&self, fields: &'a BTreeMap<String, Type>) -> BoxDoc<'a>;
    fn transpile_type<'a>(&self, t: &'a Type) -> BoxDoc<'a> {
        match t {
            Type::Bool => self.transpile_bool_type(),
            Type::String => self.transpile_string_type(),
            Type::Float => self.transpile_number_type(),
            Type::Int => self.transpile_int_type(),
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
    fn transpile_float_literal<'a>(&self, value: f64) -> BoxDoc<'a>;
    fn transpile_int_literal<'a>(&self, value: i64) -> BoxDoc<'a>;
    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Option<Box<Type>>,
    ) -> BoxDoc<'a>;
    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(String, IrExpr)],
        property_types: &'a BTreeMap<String, Type>,
    ) -> BoxDoc<'a>;
    fn transpile_string_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_bool_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_string_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_bool_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_expr<'a>(&self, expr: &'a IrExpr) -> BoxDoc<'a> {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(value.as_str()),
            IrExpr::PropertyAccess {
                object, property, ..
            } => self.transpile_property_access(object, property),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(value),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(*value),
            IrExpr::FloatLiteral { value, .. } => self.transpile_float_literal(*value),
            IrExpr::IntLiteral { value, .. } => self.transpile_int_literal(*value),
            IrExpr::ArrayLiteral { elements, .. } => match expr.as_type() {
                Type::Array(elem_type) => self.transpile_array_literal(elements, elem_type),
                _ => {
                    unreachable!()
                }
            },
            IrExpr::ObjectLiteral {
                properties,
                kind: typ,
                ..
            } => match typ {
                Type::Object(property_types) => {
                    self.transpile_object_literal(properties, property_types)
                }
                _ => {
                    unreachable!()
                }
            },
            IrExpr::JsonEncode { value, .. } => self.transpile_json_encode(value),
            IrExpr::StringConcat { left, right, .. } => self.transpile_string_concat(left, right),
            IrExpr::Negation { operand, .. } => self.transpile_not(operand),
            IrExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                EquatableType::Bool => self.transpile_bool_equality(left, right),
                EquatableType::String => self.transpile_string_equality(left, right),
                EquatableType::Int => self.transpile_int_equality(left, right),
            },
            IrExpr::NotEquals {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                EquatableType::Bool => self.transpile_bool_not_equals(left, right),
                EquatableType::String => self.transpile_string_not_equals(left, right),
                EquatableType::Int => self.transpile_int_not_equals(left, right),
            },
        }
    }
}
