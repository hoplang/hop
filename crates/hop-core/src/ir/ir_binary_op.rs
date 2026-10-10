use crate::hop::typing::{ComparableType, EquatableType, NumericType};

/// An operation on two operands.
#[derive(Debug, Clone)]
pub enum IrBinaryOp {
    /// Takes two values of the operand type. Produces that type.
    NumericAdd(NumericType),
    /// Takes two values of the operand type. Produces that type.
    NumericSubtract(NumericType),
    /// Takes two values of the operand type. Produces that type.
    NumericMultiply(NumericType),
    /// Takes two values of the operand type. Produces a Bool.
    Equals(EquatableType),
    /// Takes two values of the operand type. Produces a Bool.
    LessThan(ComparableType),
    /// Takes two values of the operand type. Produces a Bool.
    LessThanOrEqual(ComparableType),
}
