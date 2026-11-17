use std::fmt;

use crate::display::{join, write_tuple};
use crate::document::CheapString;
use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone)]
pub enum IrDataOp {
    StringLiteral(CheapString),
    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Array(Vec<ValueId>),
    Tuple(Vec<ValueId>),
    Record(Vec<(FieldName, ValueId)>),
    Enum(TypeName, Vec<(FieldName, ValueId)>),
    OptionSome(ValueId),
    OptionNone,
    StringConcat(Vec<ValueId>),
    /// `(T, T) -> T` for the `T` it stores.
    NumericAdd(NumericType, ValueId, ValueId),
    /// `(T, T) -> T` for the `T` it stores.
    NumericSubtract(NumericType, ValueId, ValueId),
    /// `(T, T) -> T` for the `T` it stores.
    NumericMultiply(NumericType, ValueId, ValueId),
    /// `(T, T) -> Bool` for the `T` it stores.
    Equals(EquatableType, ValueId, ValueId),
    /// `(T, T) -> Bool` for the `T` it stores.
    LessThan(ComparableType, ValueId, ValueId),
    /// `(T, T) -> Bool` for the `T` it stores.
    LessThanOrEqual(ComparableType, ValueId, ValueId),
    /// Produce an Array of the Ints from the first to the second inclusive, in
    /// increasing order, which is empty when the first is greater than the
    /// second.
    /// `(Int, Int) -> Array[Int]`.
    ArrayFromRange(ValueId, ValueId),
    /// `T -> T` for the `T` it stores.
    NumericNegation(NumericType, ValueId),
    /// `Bool -> Bool`.
    BoolNegation(ValueId),
    /// `Array[T] -> Int`.
    ArrayLength(ValueId),
    /// `Array[T] -> Bool`.
    ArrayIsEmpty(ValueId),
    /// `(Array[T], Int) -> Option[T]`.
    ArrayGet(ValueId, ValueId),
    /// `Array[T] -> Array[Tuple[Int, T]]`.
    ArrayEnumerate(ValueId),
    /// `String -> Bool`.
    StringIsEmpty(ValueId),
    /// `Option[T] -> Bool`.
    OptionIsSome(ValueId),
    /// `Option[T] -> Bool`.
    OptionIsNone(ValueId),
    /// `Int -> String`.
    IntToString(ValueId),
    /// Saturates at the i32 bounds and maps NaN to 0.
    /// `Float -> Int`.
    FloatToInt(ValueId),
    /// `Int -> Float`.
    IntToFloat(ValueId),
    FieldAccess(ValueId, FieldName),
}

impl IrDataOp {
    /// Apply `f` to each name this op reads, in order.
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            IrDataOp::StringLiteral(_)
            | IrDataOp::IntLiteral(_)
            | IrDataOp::FloatLiteral(_)
            | IrDataOp::BoolLiteral(_)
            | IrDataOp::OptionNone => {}

            IrDataOp::FieldAccess(name, _)
            | IrDataOp::OptionSome(name)
            | IrDataOp::NumericNegation(_, name)
            | IrDataOp::BoolNegation(name)
            | IrDataOp::ArrayLength(name)
            | IrDataOp::ArrayIsEmpty(name)
            | IrDataOp::ArrayEnumerate(name)
            | IrDataOp::StringIsEmpty(name)
            | IrDataOp::OptionIsSome(name)
            | IrDataOp::OptionIsNone(name)
            | IrDataOp::IntToString(name)
            | IrDataOp::FloatToInt(name)
            | IrDataOp::IntToFloat(name) => f(*name),

            IrDataOp::Array(names) | IrDataOp::Tuple(names) | IrDataOp::StringConcat(names) => {
                for name in names {
                    f(*name);
                }
            }

            IrDataOp::Record(fields) | IrDataOp::Enum(_, fields) => {
                for (_, name) in fields {
                    f(*name);
                }
            }

            IrDataOp::NumericAdd(_, left, right)
            | IrDataOp::NumericSubtract(_, left, right)
            | IrDataOp::NumericMultiply(_, left, right)
            | IrDataOp::Equals(_, left, right)
            | IrDataOp::LessThan(_, left, right)
            | IrDataOp::LessThanOrEqual(_, left, right)
            | IrDataOp::ArrayFromRange(left, right)
            | IrDataOp::ArrayGet(left, right) => {
                f(*left);
                f(*right);
            }
        }
    }

    /// Apply `f` to each name this op reads, to rewrite it. Visits the same
    /// names as `for_each_operand`.
    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        match self {
            IrDataOp::StringLiteral(_)
            | IrDataOp::IntLiteral(_)
            | IrDataOp::FloatLiteral(_)
            | IrDataOp::BoolLiteral(_)
            | IrDataOp::OptionNone => {}

            IrDataOp::FieldAccess(name, _)
            | IrDataOp::OptionSome(name)
            | IrDataOp::NumericNegation(_, name)
            | IrDataOp::BoolNegation(name)
            | IrDataOp::ArrayLength(name)
            | IrDataOp::ArrayIsEmpty(name)
            | IrDataOp::ArrayEnumerate(name)
            | IrDataOp::StringIsEmpty(name)
            | IrDataOp::OptionIsSome(name)
            | IrDataOp::OptionIsNone(name)
            | IrDataOp::IntToString(name)
            | IrDataOp::FloatToInt(name)
            | IrDataOp::IntToFloat(name) => f(name),

            IrDataOp::Array(names) | IrDataOp::Tuple(names) | IrDataOp::StringConcat(names) => {
                for name in names {
                    f(name);
                }
            }

            IrDataOp::Record(fields) | IrDataOp::Enum(_, fields) => {
                for (_, name) in fields {
                    f(name);
                }
            }

            IrDataOp::NumericAdd(_, left, right)
            | IrDataOp::NumericSubtract(_, left, right)
            | IrDataOp::NumericMultiply(_, left, right)
            | IrDataOp::Equals(_, left, right)
            | IrDataOp::LessThan(_, left, right)
            | IrDataOp::LessThanOrEqual(_, left, right)
            | IrDataOp::ArrayFromRange(left, right)
            | IrDataOp::ArrayGet(left, right) => {
                f(left);
                f(right);
            }
        }
    }
}

impl fmt::Display for IrDataOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrDataOp::StringLiteral(value) => write!(f, "{:?}", value.as_str()),
            IrDataOp::IntLiteral(value) => write!(f, "{value}"),
            IrDataOp::FloatLiteral(value) => write!(f, "{value}"),
            IrDataOp::BoolLiteral(value) => write!(f, "{value}"),
            IrDataOp::FieldAccess(record, field) => write!(f, "{record}.{}", field.as_str()),
            IrDataOp::Array(elements) => write!(f, "[{}]", join(elements, ", ")),
            IrDataOp::Tuple(elements) => write_tuple(f, elements),
            IrDataOp::Enum(variant_name, fields) if fields.is_empty() => {
                f.write_str(variant_name.as_str())
            }
            // Each field as its name and the name it reads.
            IrDataOp::Record(fields) | IrDataOp::Enum(_, fields) => {
                if let IrDataOp::Enum(variant_name, _) = self {
                    write!(f, "{} ", variant_name.as_str())?;
                }
                f.write_str("{")?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    let sep = if i > 0 { ", " } else { "" };
                    write!(f, "{sep}{}: {value}", name.as_str())?;
                }
                f.write_str("}")
            }
            IrDataOp::OptionSome(value) => write!(f, "Some({value})"),
            IrDataOp::OptionNone => f.write_str("None"),
            IrDataOp::StringConcat(parts) => write!(f, "string_concat({})", join(parts, ", ")),
            IrDataOp::NumericAdd(NumericType::Int, l, r) => write!(f, "int_add({l}, {r})"),
            IrDataOp::NumericAdd(NumericType::Float, l, r) => write!(f, "float_add({l}, {r})"),
            IrDataOp::NumericSubtract(NumericType::Int, l, r) => write!(f, "int_sub({l}, {r})"),
            IrDataOp::NumericSubtract(NumericType::Float, l, r) => {
                write!(f, "float_sub({l}, {r})")
            }
            IrDataOp::NumericMultiply(NumericType::Int, l, r) => write!(f, "int_mul({l}, {r})"),
            IrDataOp::NumericMultiply(NumericType::Float, l, r) => {
                write!(f, "float_mul({l}, {r})")
            }
            IrDataOp::Equals(EquatableType::String, l, r) => write!(f, "string_eq({l}, {r})"),
            IrDataOp::Equals(EquatableType::Bool, l, r) => write!(f, "bool_eq({l}, {r})"),
            IrDataOp::Equals(EquatableType::Int, l, r) => write!(f, "int_eq({l}, {r})"),
            IrDataOp::Equals(EquatableType::Float, l, r) => write!(f, "float_eq({l}, {r})"),
            IrDataOp::LessThan(ComparableType::Int, l, r) => write!(f, "int_lt({l}, {r})"),
            IrDataOp::LessThan(ComparableType::Float, l, r) => write!(f, "float_lt({l}, {r})"),
            IrDataOp::LessThanOrEqual(ComparableType::Int, l, r) => {
                write!(f, "int_lt_eq({l}, {r})")
            }
            IrDataOp::LessThanOrEqual(ComparableType::Float, l, r) => {
                write!(f, "float_lt_eq({l}, {r})")
            }
            IrDataOp::ArrayFromRange(l, r) => write!(f, "array_from_range({l}, {r})"),
            IrDataOp::NumericNegation(NumericType::Int, operand) => write!(f, "int_neg({operand})"),
            IrDataOp::NumericNegation(NumericType::Float, operand) => {
                write!(f, "float_neg({operand})")
            }
            IrDataOp::BoolNegation(operand) => write!(f, "bool_not({operand})"),
            IrDataOp::ArrayLength(operand) => write!(f, "array_length({operand})"),
            IrDataOp::ArrayIsEmpty(operand) => write!(f, "array_is_empty({operand})"),
            IrDataOp::ArrayGet(array, index) => write!(f, "array_get({array}, {index})"),
            IrDataOp::ArrayEnumerate(operand) => write!(f, "array_enumerate({operand})"),
            IrDataOp::StringIsEmpty(operand) => write!(f, "string_is_empty({operand})"),
            IrDataOp::OptionIsSome(operand) => write!(f, "option_is_some({operand})"),
            IrDataOp::OptionIsNone(operand) => write!(f, "option_is_none({operand})"),
            IrDataOp::IntToString(operand) => write!(f, "int_to_string({operand})"),
            IrDataOp::FloatToInt(operand) => write!(f, "float_to_int({operand})"),
            IrDataOp::IntToFloat(operand) => write!(f, "int_to_float({operand})"),
        }
    }
}
