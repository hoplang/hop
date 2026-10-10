use crate::hop::typing::{
    ComparableType, EquatableType, NumericType, ResolvedType, Type, TypeRegistry,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::Match;
use crate::ir::writer_module::{
    ForSource, Let, Name, Stmt, Value, ValueBlock, WriterFunctionDeclaration, WriterModule,
    WriterPageDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use pretty::Arena;

use super::Doc;

/// A backend for the Writer.
///
/// Operands are names, so a value never contains another value, and the
/// methods for values take the names they read. The type of a value is
/// the type of the let that holds it, and is passed along where the value
/// alone does not determine it.
pub trait Transpiler {
    fn transpile_module(&mut self, module: &WriterModule, registry: &TypeRegistry) -> String;
    /// The registry of the module currently being transpiled. Used to
    /// resolve named types during type transpilation.
    fn registry(&self) -> &TypeRegistry;
    fn transpile_page<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: &'a TypeName,
        page: &'a WriterPageDeclaration,
    ) -> Doc<'a>;
    fn transpile_function_def<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a WriterFunctionDeclaration,
    ) -> Doc<'a>;

    // Statements
    fn transpile_write_statement<'a>(&mut self, arena: &'a Arena<'a>, content: &'a str) -> Doc<'a>;
    fn transpile_write_string_statement<'a>(&mut self, arena: &'a Arena<'a>, name: Name)
    -> Doc<'a>;
    fn transpile_write_html_statement<'a>(&mut self, arena: &'a Arena<'a>, name: Name) -> Doc<'a>;
    fn transpile_write_function_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [Name],
    ) -> Doc<'a>;
    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrBinder>,
        source: &'a ForSource,
        body: &'a [Stmt],
    ) -> Doc<'a>;
    fn transpile_let_statement<'a>(&mut self, arena: &'a Arena<'a>, let_: &'a Let) -> Doc<'a>;
    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<Name, Vec<Stmt>>,
    ) -> Doc<'a>;
    fn transpile_statement<'a>(&mut self, arena: &'a Arena<'a>, statement: &'a Stmt) -> Doc<'a> {
        match statement {
            Stmt::Let(let_) => self.transpile_let_statement(arena, let_),
            Stmt::Write(content) => self.transpile_write_statement(arena, content),
            Stmt::WriteString(name) => self.transpile_write_string_statement(arena, *name),
            Stmt::WriteHtml(name) => self.transpile_write_html_statement(arena, *name),
            Stmt::WriteFunction { function, args } => {
                self.transpile_write_function_statement(arena, function, args)
            }
            Stmt::For { var, source, body } => {
                self.transpile_for_statement(arena, var.as_ref(), source, body)
            }
            Stmt::Match(match_) => self.transpile_match_statement(arena, match_),
        }
    }
    fn transpile_statements<'a>(&mut self, arena: &'a Arena<'a>, statements: &'a [Stmt])
    -> Doc<'a>;
    /// The lets of a value block followed by whatever delivers the result:
    /// a return in a function body or a match arm, or the result itself
    /// where a block is an expression.
    fn transpile_value_block<'a>(&mut self, arena: &'a Arena<'a>, block: &'a ValueBlock)
    -> Doc<'a>;

    // Types
    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_html_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_array_type<'a>(&mut self, arena: &'a Arena<'a>, element_type: &Type) -> Doc<'a>;
    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &Type) -> Doc<'a>;
    fn transpile_tuple_type<'a>(&mut self, arena: &'a Arena<'a>, element_types: &[Type])
    -> Doc<'a>;
    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a>;
    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a>;
    fn transpile_type<'a>(&mut self, arena: &'a Arena<'a>, t: &Type) -> Doc<'a> {
        match t {
            Type::Bool => self.transpile_bool_type(arena),
            Type::String => self.transpile_string_type(arena),
            Type::Float => self.transpile_float_type(arena),
            Type::Int => self.transpile_int_type(arena),
            Type::Html => self.transpile_html_type(arena),
            Type::Array(elem) => self.transpile_array_type(arena, elem),
            Type::Option(inner) => self.transpile_option_type(arena, inner),
            Type::Tuple(elements) => self.transpile_tuple_type(arena, elements),
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

    // Values
    fn transpile_field_access<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record: Name,
        field: &'a FieldName,
    ) -> Doc<'a>;
    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a>;
    fn transpile_html<'a>(&mut self, arena: &'a Arena<'a>, body: &'a [Stmt]) -> Doc<'a>;
    fn transpile_bool_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a>;
    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a>;
    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i32) -> Doc<'a>;
    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [Name],
        elem_type: &'a Type,
    ) -> Doc<'a>;
    fn transpile_tuple_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [Name],
        element_types: &'a [Type],
    ) -> Doc<'a>;
    fn transpile_tuple_index<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        tuple: Name,
        index: usize,
    ) -> Doc<'a>;
    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: Name) -> Doc<'a>;
    fn transpile_int_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: Name) -> Doc<'a>;
    fn transpile_float_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: Name) -> Doc<'a>;
    fn transpile_string_concat<'a>(&mut self, arena: &'a Arena<'a>, parts: &'a [Name]) -> Doc<'a>;
    fn transpile_int_add<'a>(&mut self, arena: &'a Arena<'a>, left: Name, right: Name) -> Doc<'a>;
    fn transpile_float_add<'a>(&mut self, arena: &'a Arena<'a>, left: Name, right: Name)
    -> Doc<'a>;
    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: Name,
        right: Name,
    ) -> Doc<'a>;
    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, Name)],
    ) -> Doc<'a>;
    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, Name)],
    ) -> Doc<'a>;
    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<Name>,
        inner_type: &'a Type,
    ) -> Doc<'a>;
    fn transpile_match_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a Match<Name, ValueBlock>,
    ) -> Doc<'a>;
    fn transpile_function_call_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [Name],
    ) -> Doc<'a>;
    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: Name) -> Doc<'a>;
    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: Name) -> Doc<'a>;
    fn transpile_string_is_empty<'a>(&mut self, arena: &'a Arena<'a>, string: Name) -> Doc<'a>;
    fn transpile_option_is_some<'a>(&mut self, arena: &'a Arena<'a>, option: Name) -> Doc<'a>;
    fn transpile_option_is_none<'a>(&mut self, arena: &'a Arena<'a>, option: Name) -> Doc<'a>;
    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: Name) -> Doc<'a>;
    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: Name) -> Doc<'a>;
    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: Name) -> Doc<'a>;
    /// The expression for a value, given the type of the let that holds it.
    fn transpile_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: &'a Value,
        typ: &'a Type,
    ) -> Doc<'a> {
        match value {
            Value::StringLiteral(value) => self.transpile_string_literal(arena, value.as_str()),
            Value::IntLiteral(value) => self.transpile_int_literal(arena, *value),
            Value::FloatLiteral(value) => self.transpile_float_literal(arena, *value),
            Value::BoolLiteral(value) => self.transpile_bool_literal(arena, *value),
            Value::FieldAccess { record, field } => {
                self.transpile_field_access(arena, *record, field)
            }
            Value::TupleIndex { tuple, index } => self.transpile_tuple_index(arena, *tuple, *index),
            Value::Array(elements) => match typ {
                Type::Array(elem_type) => self.transpile_array_literal(arena, elements, elem_type),
                _ => unreachable!("Array value must have Array type"),
            },
            Value::Tuple(elements) => match typ {
                Type::Tuple(element_types) => {
                    self.transpile_tuple_literal(arena, elements, element_types)
                }
                _ => unreachable!("Tuple value must have Tuple type"),
            },
            Value::Record { fields } => match typ {
                Type::Named { name, .. } => {
                    self.transpile_record_literal(arena, name.as_str(), fields)
                }
                _ => unreachable!("Record value must have a named type"),
            },
            Value::Enum {
                variant_name,
                fields,
            } => match typ {
                Type::Named { name, .. } => {
                    self.transpile_enum_literal(arena, name.as_str(), variant_name.as_str(), fields)
                }
                _ => unreachable!("Enum value must have a named type"),
            },
            Value::Option(value) => match typ {
                Type::Option(inner_type) => {
                    self.transpile_option_literal(arena, *value, inner_type)
                }
                _ => unreachable!("Option value must have Option type"),
            },
            Value::StringConcat(parts) => self.transpile_string_concat(arena, parts),
            Value::NumericAdd {
                left,
                right,
                operand_types,
            } => match operand_types {
                NumericType::Int => self.transpile_int_add(arena, *left, *right),
                NumericType::Float => self.transpile_float_add(arena, *left, *right),
            },
            Value::NumericSubtract {
                left,
                right,
                operand_types,
            } => match operand_types {
                NumericType::Int => self.transpile_int_subtract(arena, *left, *right),
                NumericType::Float => self.transpile_float_subtract(arena, *left, *right),
            },
            Value::NumericMultiply {
                left,
                right,
                operand_types,
            } => match operand_types {
                NumericType::Int => self.transpile_int_multiply(arena, *left, *right),
                NumericType::Float => self.transpile_float_multiply(arena, *left, *right),
            },
            Value::NumericNegation {
                operand,
                operand_type,
            } => match operand_type {
                NumericType::Int => self.transpile_int_negation(arena, *operand),
                NumericType::Float => self.transpile_float_negation(arena, *operand),
            },
            Value::BoolNegation(operand) => self.transpile_not(arena, *operand),
            Value::Equals {
                left,
                right,
                operand_types,
            } => match operand_types {
                EquatableType::String => self.transpile_string_equals(arena, *left, *right),
                EquatableType::Bool => self.transpile_bool_equals(arena, *left, *right),
                EquatableType::Int => self.transpile_int_equals(arena, *left, *right),
                EquatableType::Float => self.transpile_float_equals(arena, *left, *right),
            },
            Value::LessThan {
                left,
                right,
                operand_types,
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than(arena, *left, *right),
                ComparableType::Float => self.transpile_float_less_than(arena, *left, *right),
            },
            Value::LessThanOrEqual {
                left,
                right,
                operand_types,
            } => match operand_types {
                ComparableType::Int => self.transpile_int_less_than_or_equal(arena, *left, *right),
                ComparableType::Float => {
                    self.transpile_float_less_than_or_equal(arena, *left, *right)
                }
            },
            Value::ArrayLength(array) => self.transpile_array_length(arena, *array),
            Value::ArrayIsEmpty(array) => self.transpile_array_is_empty(arena, *array),
            Value::StringIsEmpty(string) => self.transpile_string_is_empty(arena, *string),
            Value::OptionIsSome(option) => self.transpile_option_is_some(arena, *option),
            Value::OptionIsNone(option) => self.transpile_option_is_none(arena, *option),
            Value::IntToString(value) => self.transpile_int_to_string(arena, *value),
            Value::FloatToInt(value) => self.transpile_float_to_int(arena, *value),
            Value::IntToFloat(value) => self.transpile_int_to_float(arena, *value),
            Value::Call { function, args } => {
                self.transpile_function_call_value(arena, function, args)
            }
            Value::HtmlLiteral(body) => self.transpile_html(arena, body),
            Value::Match(match_) => self.transpile_match_value(arena, match_),
        }
    }
}
