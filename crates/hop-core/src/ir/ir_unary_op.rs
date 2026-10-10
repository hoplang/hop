use crate::hop::typing::NumericType;

/// An operation on one operand.
#[derive(Debug, Clone)]
pub enum IrUnaryOp {
    /// Takes a value of the operand type. Produces that type.
    NumericNegation(NumericType),
    /// Takes a Bool. Produces a Bool.
    BoolNegation,
    /// Takes an Array. Produces an Int.
    ArrayLength,
    /// Takes an Array. Produces a Bool.
    ArrayIsEmpty,
    /// Takes a String. Produces a Bool.
    StringIsEmpty,
    /// Takes an Option. Produces a Bool.
    OptionIsSome,
    /// Takes an Option. Produces a Bool.
    OptionIsNone,
    /// Takes an Int. Produces a String.
    IntToString,
    /// Takes a Float. Produces an Int. Saturates at the i32 bounds and maps
    /// NaN to 0.
    FloatToInt,
    /// Takes an Int. Produces a Float.
    IntToFloat,
}
