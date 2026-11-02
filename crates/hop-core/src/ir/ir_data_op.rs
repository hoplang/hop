use std::fmt;

use crate::display::{join, write_tuple};
use crate::document::CheapString;
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
    /// Takes two Ints or two Floats. Produces that type.
    NumericAdd(ValueId, ValueId),
    /// Takes two Ints or two Floats. Produces that type.
    NumericSubtract(ValueId, ValueId),
    /// Takes two Ints or two Floats. Produces that type.
    NumericMultiply(ValueId, ValueId),
    /// Takes two Strings, two Bools, two Ints or two Floats. Produces a
    /// Bool.
    Equals(ValueId, ValueId),
    /// Takes two Ints or two Floats. Produces a Bool.
    LessThan(ValueId, ValueId),
    /// Takes two Ints or two Floats. Produces a Bool.
    LessThanOrEqual(ValueId, ValueId),
    /// Takes two Ints. Produces an Array of the Ints from the first to the
    /// second inclusive, in increasing order, which is empty when the first
    /// is greater than the second.
    ArrayFromRange(ValueId, ValueId),
    /// Takes an Int or a Float. Produces that type.
    NumericNegation(ValueId),
    /// Takes a Bool. Produces a Bool.
    BoolNegation(ValueId),
    /// Takes an Array. Produces an Int.
    ArrayLength(ValueId),
    /// Takes an Array. Produces a Bool.
    ArrayIsEmpty(ValueId),
    /// Takes a String. Produces a Bool.
    StringIsEmpty(ValueId),
    /// Takes an Option. Produces a Bool.
    OptionIsSome(ValueId),
    /// Takes an Option. Produces a Bool.
    OptionIsNone(ValueId),
    /// Takes an Int. Produces a String.
    IntToString(ValueId),
    /// Takes a Float. Produces an Int. Saturates at the i32 bounds and maps
    /// NaN to 0.
    FloatToInt(ValueId),
    /// Takes an Int. Produces a Float.
    IntToFloat(ValueId),
    FieldAccess(ValueId, FieldName),
    TupleIndex(ValueId, usize),
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
            | IrDataOp::TupleIndex(name, _)
            | IrDataOp::OptionSome(name)
            | IrDataOp::NumericNegation(name)
            | IrDataOp::BoolNegation(name)
            | IrDataOp::ArrayLength(name)
            | IrDataOp::ArrayIsEmpty(name)
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

            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right)
            | IrDataOp::Equals(left, right)
            | IrDataOp::LessThan(left, right)
            | IrDataOp::LessThanOrEqual(left, right)
            | IrDataOp::ArrayFromRange(left, right) => {
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
            | IrDataOp::TupleIndex(name, _)
            | IrDataOp::OptionSome(name)
            | IrDataOp::NumericNegation(name)
            | IrDataOp::BoolNegation(name)
            | IrDataOp::ArrayLength(name)
            | IrDataOp::ArrayIsEmpty(name)
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

            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right)
            | IrDataOp::Equals(left, right)
            | IrDataOp::LessThan(left, right)
            | IrDataOp::LessThanOrEqual(left, right)
            | IrDataOp::ArrayFromRange(left, right) => {
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
            IrDataOp::TupleIndex(tuple, index) => write!(f, "{tuple}.{index}"),
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
            IrDataOp::NumericAdd(left, right) => write!(f, "add({left}, {right})"),
            IrDataOp::NumericSubtract(left, right) => write!(f, "sub({left}, {right})"),
            IrDataOp::NumericMultiply(left, right) => write!(f, "mul({left}, {right})"),
            IrDataOp::Equals(left, right) => write!(f, "eq({left}, {right})"),
            IrDataOp::LessThan(left, right) => write!(f, "lt({left}, {right})"),
            IrDataOp::LessThanOrEqual(left, right) => write!(f, "lt_eq({left}, {right})"),
            IrDataOp::ArrayFromRange(left, right) => write!(f, "{left}..={right}"),
            IrDataOp::NumericNegation(operand) => write!(f, "neg({operand})"),
            IrDataOp::BoolNegation(operand) => write!(f, "not({operand})"),
            IrDataOp::ArrayLength(operand) => write!(f, "{operand}.len()"),
            IrDataOp::ArrayIsEmpty(operand) => write!(f, "{operand}.is_empty()"),
            IrDataOp::StringIsEmpty(operand) => write!(f, "{operand}.is_empty()"),
            IrDataOp::OptionIsSome(operand) => write!(f, "{operand}.is_some()"),
            IrDataOp::OptionIsNone(operand) => write!(f, "{operand}.is_none()"),
            IrDataOp::IntToString(operand) => write!(f, "{operand}.to_string()"),
            IrDataOp::FloatToInt(operand) => write!(f, "{operand}.to_int()"),
            IrDataOp::IntToFloat(operand) => write!(f, "{operand}.to_float()"),
        }
    }
}
