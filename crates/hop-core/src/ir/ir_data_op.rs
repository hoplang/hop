use pretty::BoxDoc;

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

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            IrDataOp::StringLiteral(value) => BoxDoc::text(format!("{:?}", value.as_str())),
            IrDataOp::IntLiteral(value) => BoxDoc::text(value.to_string()),
            IrDataOp::FloatLiteral(value) => BoxDoc::text(value.to_string()),
            IrDataOp::BoolLiteral(value) => BoxDoc::text(value.to_string()),
            IrDataOp::FieldAccess(record, field) => {
                BoxDoc::text(format!("{record}.{}", field.as_str()))
            }
            IrDataOp::TupleIndex(tuple, index) => BoxDoc::text(format!("{tuple}.{index}")),
            IrDataOp::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("[{elements}]"))
            }
            IrDataOp::Tuple(elements) => {
                let joined = elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                if elements.len() == 1 {
                    BoxDoc::text(format!("({joined},)"))
                } else {
                    BoxDoc::text(format!("({joined})"))
                }
            }
            IrDataOp::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| format!("{}: {value}", name.as_str()))
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("{{{fields}}}"))
            }
            IrDataOp::Enum(variant_name, fields) => {
                if fields.is_empty() {
                    BoxDoc::text(variant_name.as_str())
                } else {
                    let fields = fields
                        .iter()
                        .map(|(name, value)| format!("{}: {value}", name.as_str()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    BoxDoc::text(format!("{} {{{fields}}}", variant_name.as_str()))
                }
            }
            IrDataOp::OptionSome(value) => BoxDoc::text(format!("Some({value})")),
            IrDataOp::OptionNone => BoxDoc::text("None"),
            IrDataOp::StringConcat(parts) => {
                let parts = parts
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("concat({parts})"))
            }
            IrDataOp::NumericAdd(left, right) => BoxDoc::text(format!("{left} + {right}")),
            IrDataOp::NumericSubtract(left, right) => BoxDoc::text(format!("{left} - {right}")),
            IrDataOp::NumericMultiply(left, right) => BoxDoc::text(format!("{left} * {right}")),
            IrDataOp::Equals(left, right) => BoxDoc::text(format!("{left} == {right}")),
            IrDataOp::LessThan(left, right) => BoxDoc::text(format!("{left} < {right}")),
            IrDataOp::LessThanOrEqual(left, right) => BoxDoc::text(format!("{left} <= {right}")),
            IrDataOp::ArrayFromRange(left, right) => BoxDoc::text(format!("{left}..={right}")),
            IrDataOp::NumericNegation(operand) => BoxDoc::text(format!("-{operand}")),
            IrDataOp::BoolNegation(operand) => BoxDoc::text(format!("!{operand}")),
            IrDataOp::ArrayLength(operand) => BoxDoc::text(format!("{operand}.len()")),
            IrDataOp::ArrayIsEmpty(operand) => BoxDoc::text(format!("{operand}.is_empty()")),
            IrDataOp::StringIsEmpty(operand) => BoxDoc::text(format!("{operand}.is_empty()")),
            IrDataOp::OptionIsSome(operand) => BoxDoc::text(format!("{operand}.is_some()")),
            IrDataOp::OptionIsNone(operand) => BoxDoc::text(format!("{operand}.is_none()")),
            IrDataOp::IntToString(operand) => BoxDoc::text(format!("{operand}.to_string()")),
            IrDataOp::FloatToInt(operand) => BoxDoc::text(format!("{operand}.to_int()")),
            IrDataOp::IntToFloat(operand) => BoxDoc::text(format!("{operand}.to_float()")),
        }
    }
}
