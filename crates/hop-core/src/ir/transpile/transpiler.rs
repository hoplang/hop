use crate::hop::typing::{
    ComparableType, DataType, EquatableType, NumericType, ResolvedType, Type, TypeRegistry,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_switch::IrSwitch;
use crate::ir::value_id::ValueId;
use crate::ir::writer_module::{
    WriterBinding, WriterBlock, WriterFunctionDeclaration, WriterModule, WriterOp,
    WriterPageDeclaration, WriterStmt,
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
    /// The type of a name in scope, as the parameter, binder or let that
    /// binds it declares it. Used to pick the form of an op by the types
    /// of its operands.
    fn name_type(&self, name: ValueId) -> &Type;
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
    fn transpile_write_string_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: ValueId,
    ) -> Doc<'a>;
    fn transpile_write_html_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        name: ValueId,
    ) -> Doc<'a>;
    fn transpile_write_function_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [ValueId],
    ) -> Doc<'a>;
    fn transpile_for_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        var: Option<&'a IrBinder<DataType>>,
        array: ValueId,
        body: &'a [WriterStmt],
    ) -> Doc<'a>;
    fn transpile_let_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        let_: &'a WriterBinding,
    ) -> Doc<'a>;
    fn transpile_match_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a IrSwitch<Vec<WriterStmt>>,
    ) -> Doc<'a>;
    fn transpile_statement<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statement: &'a WriterStmt,
    ) -> Doc<'a> {
        match statement {
            WriterStmt::Let(let_) => self.transpile_let_statement(arena, let_),
            WriterStmt::Write(content) => self.transpile_write_statement(arena, content),
            WriterStmt::WriteString(name) => self.transpile_write_string_statement(arena, *name),
            WriterStmt::WriteHtml(name) => self.transpile_write_html_statement(arena, *name),
            WriterStmt::WriteFunction(function, args) => {
                self.transpile_write_function_statement(arena, function, args)
            }
            WriterStmt::HtmlFor(IrFor { var, array, body }) => {
                self.transpile_for_statement(arena, var.as_ref(), *array, body)
            }
            WriterStmt::Match(match_) => self.transpile_match_statement(arena, match_),
        }
    }
    fn transpile_statements<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        statements: &'a [WriterStmt],
    ) -> Doc<'a>;
    /// The lets of a value block followed by whatever delivers the result:
    /// a return in a function body or a match arm, or the result itself
    /// where a block is an expression.
    fn transpile_value_block<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        block: &'a WriterBlock,
    ) -> Doc<'a>;

    // Types
    fn transpile_bool_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_string_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_float_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_int_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_html_type<'a>(&mut self, arena: &'a Arena<'a>) -> Doc<'a>;
    fn transpile_array_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_type: &DataType,
    ) -> Doc<'a>;
    fn transpile_option_type<'a>(&mut self, arena: &'a Arena<'a>, inner_type: &DataType)
    -> Doc<'a>;
    fn transpile_tuple_type<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        element_types: &[DataType],
    ) -> Doc<'a>;
    fn transpile_named_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a>;
    fn transpile_enum_type<'a>(&mut self, arena: &'a Arena<'a>, name: &str) -> Doc<'a>;
    fn transpile_type<'a>(&mut self, arena: &'a Arena<'a>, t: &Type) -> Doc<'a> {
        match t {
            Type::Data(data) => self.transpile_data_type(arena, data),
            Type::Html(_) => self.transpile_html_type(arena),
        }
    }
    fn transpile_data_type<'a>(&mut self, arena: &'a Arena<'a>, t: &DataType) -> Doc<'a> {
        match t {
            DataType::Bool => self.transpile_bool_type(arena),
            DataType::String => self.transpile_string_type(arena),
            DataType::Float => self.transpile_float_type(arena),
            DataType::Int => self.transpile_int_type(arena),
            DataType::Array(elem) => self.transpile_array_type(arena, elem),
            DataType::Option(inner) => self.transpile_option_type(arena, inner),
            DataType::Tuple(elements) => self.transpile_tuple_type(arena, elements),
            DataType::Named { name, .. } => {
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
        record: ValueId,
        field: &'a FieldName,
    ) -> Doc<'a>;
    fn transpile_string_literal<'a>(&mut self, arena: &'a Arena<'a>, value: &'a str) -> Doc<'a>;
    fn transpile_html<'a>(&mut self, arena: &'a Arena<'a>, body: &'a [WriterStmt]) -> Doc<'a>;
    fn transpile_bool_literal<'a>(&mut self, arena: &'a Arena<'a>, value: bool) -> Doc<'a>;
    fn transpile_float_literal<'a>(&mut self, arena: &'a Arena<'a>, value: f64) -> Doc<'a>;
    fn transpile_int_literal<'a>(&mut self, arena: &'a Arena<'a>, value: i32) -> Doc<'a>;
    fn transpile_array_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [ValueId],
        elem_type: &'a DataType,
    ) -> Doc<'a>;
    /// The array of the Ints from start to end inclusive.
    fn transpile_array_from_range<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        start: ValueId,
        end: ValueId,
    ) -> Doc<'a>;
    fn transpile_tuple_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        elements: &'a [ValueId],
        element_types: &'a [DataType],
    ) -> Doc<'a>;
    fn transpile_tuple_index<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        tuple: ValueId,
        index: usize,
    ) -> Doc<'a>;
    fn transpile_string_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_bool_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_int_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_float_equals<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_int_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_float_less_than<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_int_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_float_less_than_or_equal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_not<'a>(&mut self, arena: &'a Arena<'a>, operand: ValueId) -> Doc<'a>;
    fn transpile_int_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: ValueId) -> Doc<'a>;
    fn transpile_float_negation<'a>(&mut self, arena: &'a Arena<'a>, operand: ValueId) -> Doc<'a>;
    fn transpile_string_concat<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        parts: &'a [ValueId],
    ) -> Doc<'a>;
    fn transpile_int_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_float_add<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_int_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_float_subtract<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_int_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_float_multiply<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        left: ValueId,
        right: ValueId,
    ) -> Doc<'a>;
    fn transpile_record_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        record_name: &'a str,
        fields: &'a [(FieldName, ValueId)],
    ) -> Doc<'a>;
    fn transpile_enum_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        enum_name: &'a str,
        variant_name: &'a str,
        fields: &'a [(FieldName, ValueId)],
    ) -> Doc<'a>;
    fn transpile_option_literal<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        value: Option<ValueId>,
        inner_type: &'a DataType,
    ) -> Doc<'a>;
    fn transpile_match_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        match_: &'a IrSwitch<WriterBlock>,
    ) -> Doc<'a>;
    fn transpile_function_call_value<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        function: &'a IrFunction,
        args: &'a [ValueId],
    ) -> Doc<'a>;
    fn transpile_array_length<'a>(&mut self, arena: &'a Arena<'a>, array: ValueId) -> Doc<'a>;
    fn transpile_array_is_empty<'a>(&mut self, arena: &'a Arena<'a>, array: ValueId) -> Doc<'a>;
    fn transpile_string_is_empty<'a>(&mut self, arena: &'a Arena<'a>, string: ValueId) -> Doc<'a>;
    fn transpile_option_is_some<'a>(&mut self, arena: &'a Arena<'a>, option: ValueId) -> Doc<'a>;
    fn transpile_option_is_none<'a>(&mut self, arena: &'a Arena<'a>, option: ValueId) -> Doc<'a>;
    fn transpile_int_to_string<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a>;
    fn transpile_float_to_int<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a>;
    fn transpile_int_to_float<'a>(&mut self, arena: &'a Arena<'a>, value: ValueId) -> Doc<'a>;
    /// The expression for an op, given the type of the let that holds it.
    fn transpile_op<'a>(
        &mut self,
        arena: &'a Arena<'a>,
        op: &'a WriterOp,
        typ: &'a Type,
    ) -> Doc<'a> {
        match op {
            WriterOp::Data(IrDataOp::StringLiteral(value)) => {
                self.transpile_string_literal(arena, value.as_str())
            }
            WriterOp::Data(IrDataOp::IntLiteral(value)) => {
                self.transpile_int_literal(arena, *value)
            }
            WriterOp::Data(IrDataOp::FloatLiteral(value)) => {
                self.transpile_float_literal(arena, *value)
            }
            WriterOp::Data(IrDataOp::BoolLiteral(value)) => {
                self.transpile_bool_literal(arena, *value)
            }
            WriterOp::Data(IrDataOp::FieldAccess(record, field)) => {
                self.transpile_field_access(arena, *record, field)
            }
            WriterOp::Data(IrDataOp::TupleIndex(tuple, index)) => {
                self.transpile_tuple_index(arena, *tuple, *index)
            }
            WriterOp::Data(IrDataOp::Array(elements)) => match typ {
                Type::Data(DataType::Array(elem_type)) => {
                    self.transpile_array_literal(arena, elements, elem_type)
                }
                _ => unreachable!("Array value must have Array type"),
            },
            WriterOp::Data(IrDataOp::Tuple(elements)) => match typ {
                Type::Data(DataType::Tuple(element_types)) => {
                    self.transpile_tuple_literal(arena, elements, element_types)
                }
                _ => unreachable!("Tuple value must have Tuple type"),
            },
            WriterOp::Data(IrDataOp::Record(fields)) => match typ {
                Type::Data(DataType::Named { name, .. }) => {
                    self.transpile_record_literal(arena, name.as_str(), fields)
                }
                _ => unreachable!("Record value must have a named type"),
            },
            WriterOp::Data(IrDataOp::Enum(variant_name, fields)) => match typ {
                Type::Data(DataType::Named { name, .. }) => {
                    self.transpile_enum_literal(arena, name.as_str(), variant_name.as_str(), fields)
                }
                _ => unreachable!("Enum value must have a named type"),
            },
            WriterOp::Data(IrDataOp::OptionSome(value)) => match typ {
                Type::Data(DataType::Option(inner_type)) => {
                    self.transpile_option_literal(arena, Some(*value), inner_type)
                }
                _ => unreachable!("Option value must have Option type"),
            },
            WriterOp::Data(IrDataOp::OptionNone) => match typ {
                Type::Data(DataType::Option(inner_type)) => {
                    self.transpile_option_literal(arena, None, inner_type)
                }
                _ => unreachable!("Option value must have Option type"),
            },
            WriterOp::Data(IrDataOp::StringConcat(parts)) => {
                self.transpile_string_concat(arena, parts)
            }
            WriterOp::Data(IrDataOp::NumericAdd(left, right)) => {
                match self.name_type(*left).as_numeric_type() {
                    Some(NumericType::Int) => self.transpile_int_add(arena, *left, *right),
                    Some(NumericType::Float) => self.transpile_float_add(arena, *left, *right),
                    None => unreachable!("NumericAdd must read Ints or Floats"),
                }
            }
            WriterOp::Data(IrDataOp::NumericSubtract(left, right)) => {
                match self.name_type(*left).as_numeric_type() {
                    Some(NumericType::Int) => self.transpile_int_subtract(arena, *left, *right),
                    Some(NumericType::Float) => self.transpile_float_subtract(arena, *left, *right),
                    None => unreachable!("NumericSubtract must read Ints or Floats"),
                }
            }
            WriterOp::Data(IrDataOp::NumericMultiply(left, right)) => {
                match self.name_type(*left).as_numeric_type() {
                    Some(NumericType::Int) => self.transpile_int_multiply(arena, *left, *right),
                    Some(NumericType::Float) => self.transpile_float_multiply(arena, *left, *right),
                    None => unreachable!("NumericMultiply must read Ints or Floats"),
                }
            }
            WriterOp::Data(IrDataOp::Equals(left, right)) => match self
                .name_type(*left)
                .as_equatable_type()
            {
                Some(EquatableType::String) => self.transpile_string_equals(arena, *left, *right),
                Some(EquatableType::Bool) => self.transpile_bool_equals(arena, *left, *right),
                Some(EquatableType::Int) => self.transpile_int_equals(arena, *left, *right),
                Some(EquatableType::Float) => self.transpile_float_equals(arena, *left, *right),
                None => unreachable!("Equals must read Strings, Bools, Ints or Floats"),
            },
            WriterOp::Data(IrDataOp::LessThan(left, right)) => match self
                .name_type(*left)
                .as_comparable_type()
            {
                Some(ComparableType::Int) => self.transpile_int_less_than(arena, *left, *right),
                Some(ComparableType::Float) => self.transpile_float_less_than(arena, *left, *right),
                None => unreachable!("LessThan must read Ints or Floats"),
            },
            WriterOp::Data(IrDataOp::LessThanOrEqual(left, right)) => {
                match self.name_type(*left).as_comparable_type() {
                    Some(ComparableType::Int) => {
                        self.transpile_int_less_than_or_equal(arena, *left, *right)
                    }
                    Some(ComparableType::Float) => {
                        self.transpile_float_less_than_or_equal(arena, *left, *right)
                    }
                    None => unreachable!("LessThanOrEqual must read Ints or Floats"),
                }
            }
            WriterOp::Data(IrDataOp::ArrayFromRange(left, right)) => {
                self.transpile_array_from_range(arena, *left, *right)
            }
            WriterOp::Data(IrDataOp::NumericNegation(operand)) => {
                match self.name_type(*operand).as_numeric_type() {
                    Some(NumericType::Int) => self.transpile_int_negation(arena, *operand),
                    Some(NumericType::Float) => self.transpile_float_negation(arena, *operand),
                    None => unreachable!("NumericNegation must read an Int or a Float"),
                }
            }
            WriterOp::Data(IrDataOp::BoolNegation(operand)) => self.transpile_not(arena, *operand),
            WriterOp::Data(IrDataOp::ArrayLength(operand)) => {
                self.transpile_array_length(arena, *operand)
            }
            WriterOp::Data(IrDataOp::ArrayIsEmpty(operand)) => {
                self.transpile_array_is_empty(arena, *operand)
            }
            WriterOp::Data(IrDataOp::StringIsEmpty(operand)) => {
                self.transpile_string_is_empty(arena, *operand)
            }
            WriterOp::Data(IrDataOp::OptionIsSome(operand)) => {
                self.transpile_option_is_some(arena, *operand)
            }
            WriterOp::Data(IrDataOp::OptionIsNone(operand)) => {
                self.transpile_option_is_none(arena, *operand)
            }
            WriterOp::Data(IrDataOp::IntToString(operand)) => {
                self.transpile_int_to_string(arena, *operand)
            }
            WriterOp::Data(IrDataOp::FloatToInt(operand)) => {
                self.transpile_float_to_int(arena, *operand)
            }
            WriterOp::Data(IrDataOp::IntToFloat(operand)) => {
                self.transpile_int_to_float(arena, *operand)
            }
            WriterOp::Call(function, args) => {
                self.transpile_function_call_value(arena, function, args)
            }
            WriterOp::HtmlLiteral(body) => self.transpile_html(arena, body),
            WriterOp::Match(match_) => self.transpile_match_value(arena, match_),
        }
    }
}
