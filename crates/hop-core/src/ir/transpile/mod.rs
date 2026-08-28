pub mod rust;
pub mod ts;

use pretty::{Arena, DocBuilder};
pub use rust::RustTranspiler;
pub use ts::TsTranspiler;

use crate::expr::patterns::Match;
use crate::expr::typing::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::ir::ir_var::IrVar;
use crate::ir::writer_module::{
    WriterArgument, WriterExpr, WriterForSource, WriterFunctionDeclaration, WriterModule,
    WriterStatement, WriterViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;

pub type Doc<'a> = DocBuilder<'a, Arena<'a>>;

pub trait Transpiler {
    // Module-level transpilation
    fn transpile_view<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        view: &'a WriterViewDeclaration,
    ) -> Doc<'a>;
    fn transpile_module(&mut self, module: &WriterModule, registry: &TypeRegistry) -> String;
    /// The registry of the module currently being transpiled. Used to
    /// resolve named types during type transpilation.
    fn registry(&self) -> &TypeRegistry;

    // Statement transpilation
    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a>;
    fn transpile_write_string_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_write_fragment_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        expr: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrVar>,
        source: &'a WriterForSource,
        body: &'a [WriterStatement],
    ) -> Doc<'a>;
    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        value: &'a WriterExpr,
        body: &'a [WriterStatement],
    ) -> Doc<'a>;
    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<WriterExpr, Vec<WriterStatement>, IrVar>,
    ) -> Doc<'a>;
    fn transpile_write_function_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a FunctionName,
        args: &'a [WriterArgument],
    ) -> Doc<'a>;
    fn transpile_function_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a WriterFunctionDeclaration,
    ) -> Doc<'a>;
    fn transpile_function_call_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a FunctionName,
        args: &'a [WriterArgument],
    ) -> Doc<'a>;
    fn transpile_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statement: &'a WriterStatement,
    ) -> Doc<'a> {
        match statement {
            WriterStatement::Write { content, .. } => {
                self.transpile_write_statement(arena, content)
            }
            WriterStatement::WriteString { expr, .. } => {
                self.transpile_write_string_statement(arena, expr)
            }
            WriterStatement::WriteFragment { expr, .. } => {
                self.transpile_write_fragment_statement(arena, expr)
            }
            WriterStatement::For {
                var, source, body, ..
            } => self.transpile_for_statement(arena, var.as_ref(), source, body),
            WriterStatement::Let {
                var, value, body, ..
            } => self.transpile_let_statement(arena, var, value, body),
            WriterStatement::Match { match_, .. } => self.transpile_match_statement(arena, match_),
            WriterStatement::WriteFunction {
                function_name,
                args,
                ..
            } => self.transpile_write_function_statement(arena, function_name, args.as_slice()),
        }
    }
    fn transpile_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [WriterStatement],
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
            Type::Named { name, .. } => {
                let is_record = matches!(
                    self.registry()
                        .resolve(t)
                        .expect("named type must be registered"),
                    ResolvedType::Record { .. }
                );
                if is_record {
                    self.transpile_named_type(arena, name.as_str())
                } else {
                    self.transpile_enum_type(arena, name.as_str())
                }
            }
        }
    }

    // Expression transpilation
    fn transpile_var<'a>(&mut self, arena: &'a Arena<'a>, var: &'a IrVar) -> Doc<'a>;
    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        object: &'a WriterExpr,
        field: &'a FieldName,
    ) -> Doc<'a>;
    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a>;
    fn transpile_fragment<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        body: &'a [WriterStatement],
    ) -> Doc<'a>;
    fn transpile_boolean_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a>;
    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a>;
    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i32) -> Doc<'a>;
    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [WriterExpr],
        elem_type: &'a Type,
    ) -> Doc<'a>;
    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: &'a WriterExpr) -> Doc<'a>;
    fn transpile_int_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_negation<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        operand: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_string_concat<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parts: &'a [WriterExpr],
    ) -> Doc<'a>;
    fn transpile_logical_and<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_logical_or<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: &'a WriterExpr,
        right: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, WriterExpr)],
    ) -> Doc<'a>;
    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, WriterExpr)],
    ) -> Doc<'a>;
    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<&'a WriterExpr>,
        inner_type: &'a Type,
    ) -> Doc<'a>;
    fn transpile_match_expr<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<WriterExpr, WriterExpr, IrVar>,
    ) -> Doc<'a>;
    fn transpile_let<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: &'a IrVar,
        value: &'a WriterExpr,
        body: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_array_length<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        array: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_array_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        array: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_string_is_empty<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        string: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_option_is_some<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_option_is_none<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        option: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_to_string<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_float_to_int<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_int_to_float<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a WriterExpr,
    ) -> Doc<'a>;
    fn transpile_expr<'a>(&mut self, arena: &'a Arena<'a>, expr: &'a WriterExpr) -> Doc<'a> {
        match expr {
            WriterExpr::VariableReference { value, .. } => self.transpile_var(arena, value),
            WriterExpr::FieldAccess {
                record: object,
                field,
                ..
            } => self.transpile_field_access(arena, object, field),
            WriterExpr::StringLiteral { value, .. } => self.transpile_string_literal(arena, value),
            WriterExpr::FragmentLiteral { body, .. } => self.transpile_fragment(arena, body),
            WriterExpr::FunctionCall {
                function_name,
                args,
                ..
            } => self.transpile_function_call_expr(arena, function_name, args.as_slice()),
            WriterExpr::BooleanLiteral { value, .. } => {
                self.transpile_boolean_literal(arena, *value)
            }
            WriterExpr::FloatLiteral { value, .. } => self.transpile_float_literal(arena, *value),
            WriterExpr::IntLiteral { value, .. } => self.transpile_int_literal(arena, *value),
            WriterExpr::ArrayLiteral { elements, kind, .. } => match kind.as_ref() {
                Type::Array(elem_type) => self.transpile_array_literal(arena, elements, elem_type),
                _ => {
                    unreachable!()
                }
            },
            WriterExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => self.transpile_record_literal(arena, record_name.as_str(), fields),
            WriterExpr::StringConcat { parts, .. } => self.transpile_string_concat(arena, parts),
            WriterExpr::BooleanNegation { operand, .. } => self.transpile_not(arena, operand),
            WriterExpr::NumericNegation {
                operand,
                operand_type,
                ..
            } => match operand_type {
                NumericType::Int => self.transpile_int_negation(arena, operand),
                NumericType::Float => self.transpile_float_negation(arena, operand),
            },
            WriterExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                EquatableType::Bool => self.transpile_bool_equals(arena, left, right),
                EquatableType::String => self.transpile_string_equals(arena, left, right),
                EquatableType::Int => self.transpile_int_equals(arena, left, right),
                EquatableType::Float => self.transpile_float_equals(arena, left, right),
            },
            WriterExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than(arena, left, right),
                ComparableType::Float => self.transpile_float_less_than(arena, left, right),
            },
            WriterExpr::LessThanOrEqual {
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
            WriterExpr::BooleanLogicalAnd { left, right, .. } => {
                self.transpile_logical_and(arena, left, right)
            }
            WriterExpr::BooleanLogicalOr { left, right, .. } => {
                self.transpile_logical_or(arena, left, right)
            }
            WriterExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_add(arena, left, right),
                NumericType::Float => self.transpile_float_add(arena, left, right),
            },
            WriterExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_subtract(arena, left, right),
                NumericType::Float => self.transpile_float_subtract(arena, left, right),
            },
            WriterExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => match operand_types {
                NumericType::Int => self.transpile_int_multiply(arena, left, right),
                NumericType::Float => self.transpile_float_multiply(arena, left, right),
            },
            WriterExpr::EnumLiteral {
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
            WriterExpr::OptionLiteral { value, kind, .. } => {
                let inner_type = match kind.as_ref() {
                    Type::Option(inner) => inner.as_ref(),
                    _ => unreachable!("OptionLiteral must have Option type"),
                };
                self.transpile_option_literal(arena, value.as_ref().map(|v| v.as_ref()), inner_type)
            }
            WriterExpr::Match { match_, .. } => self.transpile_match_expr(arena, match_),
            WriterExpr::Let {
                var, value, body, ..
            } => self.transpile_let(arena, var, value, body),
            WriterExpr::TwMerge { operand, .. } => self.transpile_expr(arena, operand),
            WriterExpr::ArrayLength { array, .. } => self.transpile_array_length(arena, array),
            WriterExpr::ArrayIsEmpty { array, .. } => self.transpile_array_is_empty(arena, array),
            WriterExpr::StringIsEmpty { string, .. } => {
                self.transpile_string_is_empty(arena, string)
            }
            WriterExpr::OptionIsSome { option, .. } => self.transpile_option_is_some(arena, option),
            WriterExpr::OptionIsNone { option, .. } => self.transpile_option_is_none(arena, option),
            WriterExpr::IntToString { value, .. } => self.transpile_int_to_string(arena, value),
            WriterExpr::FloatToInt { value, .. } => self.transpile_float_to_int(arena, value),
            WriterExpr::IntToFloat { value, .. } => self.transpile_int_to_float(arena, value),
        }
    }
}
