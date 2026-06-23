pub mod rust;
pub mod ts;

use pretty::{Arena, DocBuilder};
pub use rust::RustTranspiler;
pub use ts::TsTranspiler;

use crate::expr::patterns::Match;
use crate::expr::typing::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::ir::ast::{
    IrArgument, IrComponentDeclaration, IrExpr, IrForSource, IrModule, IrStatement,
    IrViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

pub type Doc<'a> = DocBuilder<'a, Arena<'a>>;

pub trait Transpiler {
    // Module-level transpilation
    fn transpile_view<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        view: &'a IrViewDeclaration,
    ) -> Doc<'a>;
    fn transpile_module(&mut self, module: &IrModule) -> String;

    // Statement transpilation
    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a>;
    fn transpile_write_expr_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a IrExpr,
        escape: bool,
    ) -> Doc<'a>;
    fn transpile_if_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        condition: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a>;
    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a str>,
        source: &'a IrForSource,
        body: &'a [IrStatement],
    ) -> Doc<'a>;
    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> Doc<'a>;
    fn transpile_let_fragment_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a str,
        fragment_body: &'a [IrStatement],
        body: &'a [IrStatement],
    ) -> Doc<'a>;
    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<IrExpr, Vec<IrStatement>>,
    ) -> Doc<'a>;
    fn transpile_let_record_destructure_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
        body: &'a [IrStatement],
    ) -> Doc<'a>;
    fn transpile_component_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        component: &'a IrComponentDeclaration,
    ) -> Doc<'a>;
    fn transpile_component_call_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        args: &'a [IrArgument],
    ) -> Doc<'a>;
    fn transpile_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statement: &'a IrStatement,
    ) -> Doc<'a> {
        match statement {
            IrStatement::Write { content, .. } => self.transpile_write_statement(arena, content),
            IrStatement::WriteExpr { expr, escape, .. } => {
                self.transpile_write_expr_statement(arena, expr, *escape)
            }
            IrStatement::If {
                condition, body, ..
            } => self.transpile_if_statement(arena, condition, body),
            IrStatement::For {
                var, source, body, ..
            } => {
                self.transpile_for_statement(arena, var.as_ref().map(|v| v.as_str()), source, body)
            }
            IrStatement::Let {
                var, value, body, ..
            } => self.transpile_let_statement(arena, var.as_str(), value, body),
            IrStatement::LetFragment {
                var,
                fragment_body,
                body,
                ..
            } => self.transpile_let_fragment_statement(arena, var.as_str(), fragment_body, body),
            IrStatement::LetRecordDestructure {
                subject,
                bindings,
                body,
                ..
            } => self.transpile_let_record_destructure_statement(arena, subject, bindings, body),
            IrStatement::Match { match_, .. } => self.transpile_match_statement(arena, match_),
            IrStatement::ComponentInvocation {
                component_name,
                args,
                ..
            } => self.transpile_component_call_statement(arena, component_name, args.as_slice()),
        }
    }
    fn transpile_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [IrStatement],
    ) -> Doc<'a>;

    // Type transpilation
    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_fragment_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_array_type<'a>(&mut self, arena: &'a Arena<'a>, element_type: &'a Type)
    -> Doc<'a>;
    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &'a Type) -> Doc<'a>;
    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a>;
    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a>;
    fn transpile_type<'a>(&mut self, arena: &'a Arena<'a>, t: &'a Type) -> Doc<'a> {
        match t {
            Type::Bool => self.transpile_bool_type(arena),
            Type::String => self.transpile_string_type(arena),
            Type::Float => self.transpile_float_type(arena),
            Type::Int => self.transpile_int_type(arena),
            Type::Fragment => self.transpile_fragment_type(arena),
            Type::Array(elem) => self.transpile_array_type(arena, elem),
            Type::Option(inner) => self.transpile_option_type(arena, inner),
            Type::Record { name, .. } => self.transpile_named_type(arena, name.as_str()),
            Type::Enum { name, .. } => self.transpile_enum_type(arena, name.as_str()),
            Type::Component { name, .. } => self.transpile_named_type(arena, name.as_str()),
        }
    }

    // Expression transpilation
    fn transpile_var<'a>(&mut self, arena: &'a Arena<'a>, name: &'a str) -> Doc<'a>;
    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        object: &'a IrExpr,
        field: &'a FieldName,
    ) -> Doc<'a>;
    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a>;
    fn transpile_fragment_empty<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_boolean_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a>;
    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a>;
    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i64) -> Doc<'a>;
    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> Doc<'a>;
    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: &'a IrExpr) -> Doc<'a>;
    fn transpile_numeric_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_string_concat<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_logical_and<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_logical_or<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> Doc<'a>;
    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, IrExpr)],
    ) -> Doc<'a>;
    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<&'a IrExpr>,
        inner_type: &'a Type,
    ) -> Doc<'a>;
    fn transpile_match_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<IrExpr, IrExpr>,
    ) -> Doc<'a>;
    fn transpile_let<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var_name: &'a VarName,
        value: &'a IrExpr,
        body: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_let_record_destructure_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        subject: &'a IrExpr,
        bindings: &'a [(FieldName, VarName)],
        body: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a>;
    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: &'a IrExpr) -> Doc<'a>;
    fn transpile_string_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        string: &'a IrExpr,
    ) -> Doc<'a>;
    fn transpile_option_is_some<'a>(&mut self, arena: &'a Arena<'a>, option: &'a IrExpr)
    -> Doc<'a>;
    fn transpile_option_is_none<'a>(&mut self, arena: &'a Arena<'a>, option: &'a IrExpr)
    -> Doc<'a>;
    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a>;
    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a>;
    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: &'a IrExpr) -> Doc<'a>;
    fn transpile_expr<'a>(&mut self, arena: &'a Arena<'a>, expr: &'a IrExpr) -> Doc<'a> {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(arena, value.as_str()),
            IrExpr::FieldAccess {
                record: object,
                field,
                ..
            } => self.transpile_field_access(arena, object, field),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(arena, value),
            IrExpr::FragmentEmpty { .. } => self.transpile_fragment_empty(arena),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(arena, *value),
            IrExpr::FloatLiteral { value, .. } => self.transpile_float_literal(arena, *value),
            IrExpr::IntLiteral { value, .. } => self.transpile_int_literal(arena, *value),
            IrExpr::ArrayLiteral { elements, kind, .. } => match kind.as_ref() {
                Type::Array(elem_type) => self.transpile_array_literal(arena, elements, elem_type),
                _ => {
                    unreachable!()
                }
            },
            IrExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => self.transpile_record_literal(arena, record_name.as_str(), fields),
            IrExpr::StringConcat { left, right, .. } => {
                self.transpile_string_concat(arena, left, right)
            }
            IrExpr::BooleanNegation { operand, .. } => self.transpile_not(arena, operand),
            IrExpr::NumericNegation { operand, .. } => {
                self.transpile_numeric_negation(arena, operand)
            }
            IrExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                EquatableType::Bool => self.transpile_bool_equals(arena, left, right),
                EquatableType::String => self.transpile_string_equals(arena, left, right),
                EquatableType::Int => self.transpile_int_equals(arena, left, right),
                EquatableType::Float => self.transpile_float_equals(arena, left, right),
                EquatableType::Option(_) => todo!(),
            },
            IrExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than(arena, left, right),
                ComparableType::Float => self.transpile_float_less_than(arena, left, right),
            },
            IrExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than_or_equal(arena, left, right),
                ComparableType::Float => {
                    self.transpile_float_less_than_or_equal(arena, left, right)
                }
            },
            IrExpr::BooleanLogicalAnd { left, right, .. } => {
                self.transpile_logical_and(arena, left, right)
            }
            IrExpr::BooleanLogicalOr { left, right, .. } => {
                self.transpile_logical_or(arena, left, right)
            }
            IrExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_add(arena, left, right),
                NumericType::Float => self.transpile_float_add(arena, left, right),
            },
            IrExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_subtract(arena, left, right),
                NumericType::Float => self.transpile_float_subtract(arena, left, right),
            },
            IrExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_multiply(arena, left, right),
                NumericType::Float => self.transpile_float_multiply(arena, left, right),
            },
            IrExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                ..
            } => self.transpile_enum_literal(
                arena,
                enum_name.as_str(),
                variant_name.as_str(),
                fields,
            ),
            IrExpr::OptionLiteral { value, kind, .. } => {
                let inner_type = match kind.as_ref() {
                    Type::Option(inner) => inner.as_ref(),
                    _ => unreachable!("OptionLiteral must have Option type"),
                };
                self.transpile_option_literal(arena, value.as_ref().map(|v| v.as_ref()), inner_type)
            }
            IrExpr::Match { match_, .. } => self.transpile_match_expr(arena, match_),
            IrExpr::Let {
                var_name,
                value,
                body,
                ..
            } => self.transpile_let(arena, var_name, value, body),
            IrExpr::LetRecordDestructure {
                subject,
                bindings,
                body,
                ..
            } => self.transpile_let_record_destructure_expr(arena, subject, bindings, body),
            IrExpr::TwMerge { operand, .. } => self.transpile_expr(arena, operand),
            IrExpr::ArrayLength { array, .. } => self.transpile_array_length(arena, array),
            IrExpr::ArrayIsEmpty { array, .. } => self.transpile_array_is_empty(arena, array),
            IrExpr::StringIsEmpty { string, .. } => self.transpile_string_is_empty(arena, string),
            IrExpr::OptionIsSome { option, .. } => self.transpile_option_is_some(arena, option),
            IrExpr::OptionIsNone { option, .. } => self.transpile_option_is_none(arena, option),
            IrExpr::IntToString { value, .. } => self.transpile_int_to_string(arena, value),
            IrExpr::FloatToInt { value, .. } => self.transpile_float_to_int(arena, value),
            IrExpr::IntToFloat { value, .. } => self.transpile_int_to_float(arena, value),
        }
    }
}
