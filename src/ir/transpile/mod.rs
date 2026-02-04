pub mod go;
pub mod python;
pub mod rust;
pub mod ts;

pub use go::GoTranspiler;
use pretty::BoxDoc;
pub use python::PythonTranspiler;
pub use rust::RustTranspiler;
pub use ts::TsTranspiler;

use crate::dop::patterns::Match;
use crate::dop::semantics::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::symbols::var_name::VarName;
use crate::ir::ast::{IrEntrypointDeclaration, IrExpr, IrForSource, IrModule, IrStatement};

pub trait Transpiler {
    // Module-level transpilation
    fn transpile_entrypoint<'a>(
        &mut self,
        name: &'a TypeName,
        entrypoint: &'a IrEntrypointDeclaration,
    ) -> BoxDoc<'a>;
    fn transpile_module(&mut self, module: &IrModule) -> String;

    // Statement transpilation
    fn transpile_write_statement<'a>(&mut self, content: &'a str) -> BoxDoc<'a>;
    fn transpile_write_expr_statement<'a>(&mut self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a>;
    fn transpile_if_statement<'a>(
        &mut self,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
        else_body: Option<&'a [IrStatement]>,
    ) -> BoxDoc<'a>;
    fn transpile_for_statement<'a>(
        &mut self,
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a>;
    fn transpile_let_statement<'a>(
        &mut self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a>;
    fn transpile_match_statement<'a>(&mut self, match_: &'a Match<Vec<IrStatement>>) -> BoxDoc<'a>;
    fn transpile_statement<'a>(&mut self, statement: &'a IrStatement) -> BoxDoc<'a> {
        match statement {
            IrStatement::Write { content, .. } => self.transpile_write_statement(content),
            IrStatement::WriteExpr { expr, escape, .. } => {
                self.transpile_write_expr_statement(expr, *escape)
            }
            IrStatement::If {
                condition,
                body,
                else_body,
                ..
            } => self.transpile_if_statement(condition, body, else_body.as_deref()),
            IrStatement::For {
                var, source, body, ..
            } => self.transpile_for_statement(var.as_ref().map(|v| v.as_str()), source, body),
            IrStatement::Let {
                var, value, body, ..
            } => self.transpile_let_statement(var.as_str(), value, body),
            IrStatement::Match { match_, .. } => self.transpile_match_statement(match_),
        }
    }
    fn transpile_statements<'a>(&mut self, statements: &'a [IrStatement]) -> BoxDoc<'a>;

    // Type transpilation
    fn transpile_bool_type<'a>(&mut self) -> BoxDoc<'a>;
    fn transpile_string_type<'a>(&mut self) -> BoxDoc<'a>;
    fn transpile_float_type<'a>(&mut self) -> BoxDoc<'a>;
    fn transpile_int_type<'a>(&mut self) -> BoxDoc<'a>;
    fn transpile_trusted_html_type<'a>(&mut self) -> BoxDoc<'a>;
    fn transpile_array_type<'a>(&mut self, element_type: &'a Type) -> BoxDoc<'a>;
    fn transpile_option_type<'a>(&mut self, inner_type: &'a Type) -> BoxDoc<'a>;
    fn transpile_named_type<'a>(&mut self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_enum_type<'a>(&mut self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_type<'a>(&mut self, t: &'a Type) -> BoxDoc<'a> {
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

    // Expression transpilation
    fn transpile_var<'a>(&mut self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_field_access<'a>(
        &mut self,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> BoxDoc<'a>;
    fn transpile_string_literal<'a>(&mut self, value: &'a str) -> BoxDoc<'a>;
    fn transpile_boolean_literal<'a>(&mut self, value: bool) -> BoxDoc<'a>;
    fn transpile_float_literal<'a>(&mut self, value: f64) -> BoxDoc<'a>;
    fn transpile_int_literal<'a>(&mut self, value: i64) -> BoxDoc<'a>;
    fn transpile_array_literal<'a>(
        &mut self,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> BoxDoc<'a>;
    fn transpile_string_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_bool_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_enum_equals<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_less_than<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_less_than<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a>;
    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a>;
    fn transpile_not<'a>(&mut self, operand: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_numeric_negation<'a>(&mut self, operand: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_string_concat<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_logical_and<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_logical_or<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_add<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_add<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_subtract<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_subtract<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_multiply<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_multiply<'a>(&mut self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_record_literal<'a>(
        &mut self,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a>;
    fn transpile_enum_literal<'a>(
        &mut self,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> BoxDoc<'a>;
    fn transpile_option_literal<'a>(
        &mut self,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> BoxDoc<'a>;
    fn transpile_match_expr<'a>(&mut self, match_: &'a Match<IrExpr>) -> BoxDoc<'a>;
    fn transpile_let<'a>(
        &mut self,
        var: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> BoxDoc<'a>;
    fn transpile_join<'a>(&mut self, args: &'a [IrExpr]) -> BoxDoc<'a>;
    fn transpile_array_length<'a>(&mut self, array: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_array_is_empty<'a>(&mut self, array: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_to_string<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_to_int<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_float_to_string<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_int_to_float<'a>(&mut self, value: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_expr<'a>(&mut self, expr: &'a IrExpr) -> BoxDoc<'a> {
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
            IrExpr::ArrayLiteral { elements, kind, .. } => match kind.as_ref() {
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
            IrExpr::StringConcat { left, right, .. } => self.transpile_string_concat(left, right),
            IrExpr::BooleanNegation { operand, .. } => self.transpile_not(operand),
            IrExpr::NumericNegation { operand, .. } => self.transpile_numeric_negation(operand),
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
            } => self.transpile_enum_literal(enum_name, variant_name.as_str(), fields),
            IrExpr::OptionLiteral { value, kind, .. } => {
                let inner_type = match kind.as_ref() {
                    Type::Option(inner) => inner.as_ref(),
                    _ => unreachable!("OptionLiteral must have Option type"),
                };
                self.transpile_option_literal(value.as_ref().map(|v| v.as_ref()), inner_type)
            }
            IrExpr::Match { match_, .. } => self.transpile_match_expr(match_),
            IrExpr::Let {
                var, value, body, ..
            } => self.transpile_let(var, value, body),
            IrExpr::Join { args, .. } => self.transpile_join(args),
            IrExpr::TwMerge { value, .. } => self.transpile_expr(value),
            IrExpr::ArrayLength { array, .. } => self.transpile_array_length(array),
            IrExpr::ArrayIsEmpty { array, .. } => self.transpile_array_is_empty(array),
            IrExpr::IntToString { value, .. } => self.transpile_int_to_string(value),
            IrExpr::FloatToInt { value, .. } => self.transpile_float_to_int(value),
            IrExpr::FloatToString { value, .. } => self.transpile_float_to_string(value),
            IrExpr::IntToFloat { value, .. } => self.transpile_int_to_float(value),
        }
    }
}
