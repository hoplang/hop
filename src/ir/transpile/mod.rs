pub mod go;
pub mod python;
pub mod ts;

#[cfg(test)]
pub mod integration_tests;

pub use go::GoTranspiler;
use pretty::BoxDoc;
pub use python::PythonTranspiler;
pub use ts::TsTranspiler;

use crate::dop::patterns::Match;
use crate::dop::semantics::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::var_name::VarName;
use crate::hop::symbols::component_name::ComponentName;
use crate::ir::ast::{IrComponentDeclaration, IrExpr, IrModule, IrStatement};

pub trait Transpiler {
    fn transpile_entrypoint<'a>(
        &self,
        name: &'a ComponentName,
        entrypoint: &'a IrComponentDeclaration,
    ) -> BoxDoc<'a>;
    fn transpile_module(&self, module: &IrModule) -> String;
}

pub trait StatementTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a>;
    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a>;
    fn transpile_if<'a>(
        &self,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
        else_body: Option<&'a [IrStatement]>,
    ) -> BoxDoc<'a>;
    fn transpile_for<'a>(
        &self,
        var: &'a str,
        array: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a>;
    fn transpile_let_statement<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a>;
    fn transpile_match_statement<'a>(&self, match_: &'a Match<Vec<IrStatement>>) -> BoxDoc<'a>;
    fn transpile_statement<'a>(&self, statement: &'a IrStatement) -> BoxDoc<'a> {
        match statement {
            IrStatement::Write { content, .. } => self.transpile_write(content),
            IrStatement::WriteExpr { expr, escape, .. } => self.transpile_write_expr(expr, *escape),
            IrStatement::If {
                condition,
                body,
                else_body,
                ..
            } => self.transpile_if(condition, body, else_body.as_deref()),
            IrStatement::For {
                var, array, body, ..
            } => self.transpile_for(var.as_str(), array, body),
            IrStatement::Let {
                var, value, body, ..
            } => self.transpile_let_statement(var.as_str(), value, body),
            IrStatement::Match { match_, .. } => self.transpile_match_statement(match_),
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
    fn transpile_float_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_int_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_trusted_html_type<'a>(&self) -> BoxDoc<'a>;
    fn transpile_array_type<'a>(&self, element_type: &'a Type) -> BoxDoc<'a>;
    fn transpile_option_type<'a>(&self, inner_type: &'a Type) -> BoxDoc<'a>;
    fn transpile_named_type<'a>(&self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_enum_type<'a>(&self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_type<'a>(&self, t: &'a Type) -> BoxDoc<'a> {
        match t {
            Type::Bool => self.transpile_bool_type(),
            Type::String => self.transpile_string_type(),
            Type::Float => self.transpile_float_type(),
            Type::Int => self.transpile_int_type(),
            Type::TrustedHTML => self.transpile_trusted_html_type(),
            Type::Array(elem) => self.transpile_array_type(elem),
            Type::Option(inner) => self.transpile_option_type(inner),
            Type::Record { name, .. } => self.transpile_named_type(name.as_str()),
            Type::Enum { name, .. } => self.transpile_enum_type(name.as_str()),
            Type::Component { name, .. } => self.transpile_named_type(name.as_str()),
        }
    }
}

/// Expression transpilation trait using pretty-printing
pub trait ExpressionTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_field_access<'a>(&self, object: &'a IrExpr, field: &'a FieldName) -> BoxDoc<'a>;
    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a>;
    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a>;
    fn transpile_float_literal<'a>(&self, value: f64) -> BoxDoc<'a>;
    fn transpile_int_literal<'a>(&self, value: i64) -> BoxDoc<'a>;
    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> BoxDoc<'a>;
    fn transpile_string_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_bool_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_enum_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_less_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a>;
    fn transpile_float_less_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a>;
    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_env_lookup<'a>(&self, key: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_logical_and<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_logical_or<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_subtract<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_subtract<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_multiply<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_multiply<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_record_literal<'a>(
        &self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a>;
    fn transpile_enum_literal<'a>(
        &self,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a>;
    fn transpile_option_literal<'a>(
        &self,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> BoxDoc<'a>;
    fn transpile_match_expr<'a>(&self, match_: &'a Match<IrExpr>) -> BoxDoc<'a>;
    fn transpile_let<'a>(
        &self,
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> BoxDoc<'a>;
    fn transpile_merge_classes<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_expr<'a>(&self, expr: &'a IrExpr) -> BoxDoc<'a> {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(value.as_str()),
            IrExpr::FieldAccess {
                record: object,
                field,
                ..
            } => self.transpile_field_access(object, field),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(value),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(*value),
            IrExpr::FloatLiteral { value, .. } => self.transpile_float_literal(*value),
            IrExpr::IntLiteral { value, .. } => self.transpile_int_literal(*value),
            IrExpr::ArrayLiteral { elements, kind, .. } => match kind {
                Type::Array(elem_type) => self.transpile_array_literal(elements, elem_type),
                _ => {
                    unreachable!()
                }
            },
            IrExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => self.transpile_record_literal(record_name, fields),
            IrExpr::JsonEncode { value, .. } => self.transpile_json_encode(value),
            IrExpr::EnvLookup { key, .. } => self.transpile_env_lookup(key),
            IrExpr::StringConcat { left, right, .. } => self.transpile_string_concat(left, right),
            IrExpr::BooleanNegation { operand, .. } => self.transpile_not(operand),
            IrExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                EquatableType::Bool => self.transpile_bool_equals(left, right),
                EquatableType::String => self.transpile_string_equals(left, right),
                EquatableType::Int => self.transpile_int_equals(left, right),
                EquatableType::Float => self.transpile_float_equals(left, right),
                EquatableType::Enum { .. } => self.transpile_enum_equals(left, right),
                EquatableType::Option(_) => todo!(),
            },
            IrExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than(left, right),
                ComparableType::Float => self.transpile_float_less_than(left, right),
            },
            IrExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than_or_equal(left, right),
                ComparableType::Float => self.transpile_float_less_than_or_equal(left, right),
            },
            IrExpr::BooleanLogicalAnd { left, right, .. } => {
                self.transpile_logical_and(left, right)
            }
            IrExpr::BooleanLogicalOr { left, right, .. } => self.transpile_logical_or(left, right),
            IrExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_add(left, right),
                NumericType::Float => self.transpile_float_add(left, right),
            },
            IrExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_subtract(left, right),
                NumericType::Float => self.transpile_float_subtract(left, right),
            },
            IrExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_multiply(left, right),
                NumericType::Float => self.transpile_float_multiply(left, right),
            },
            IrExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                ..
            } => self.transpile_enum_literal(enum_name, variant_name, fields),
            IrExpr::OptionLiteral { value, kind, .. } => {
                let inner_type = match kind {
                    Type::Option(inner) => inner.as_ref(),
                    _ => unreachable!("OptionLiteral must have Option type"),
                };
                self.transpile_option_literal(value.as_ref().map(|v| v.as_ref()), inner_type)
            }
            IrExpr::Match { match_, .. } => self.transpile_match_expr(match_),
            IrExpr::Let {
                var, value, body, ..
            } => self.transpile_let(var, value, body),
            IrExpr::MergeClasses { left, right, .. } => self.transpile_merge_classes(left, right),
        }
    }
}
