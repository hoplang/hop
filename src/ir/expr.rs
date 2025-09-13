use std::fmt;

/// Unique identifier for each expression in the IR
pub type ExprId = u32;

/// Binary operators in IR
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Equal => write!(f, "=="),
        }
    }
}

/// Unary operators in IR
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrExpr {
    pub id: ExprId,
    pub value: IrExprValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrExprValue {
    /// Variable reference
    Var(String),

    /// Property access (e.g., obj.prop)
    PropertyAccess {
        object: Box<IrExpr>,
        property: String,
    },

    /// String literal
    String(String),

    /// Boolean literal
    Boolean(bool),

    /// Number literal
    Number(f64),

    /// Array literal
    Array(Vec<IrExpr>),

    /// Object literal
    Object(Vec<(String, IrExpr)>),

    /// Binary operation
    BinaryOp {
        left: Box<IrExpr>,
        op: BinaryOp,
        right: Box<IrExpr>,
    },

    /// Unary operation
    UnaryOp { op: UnaryOp, operand: Box<IrExpr> },
}

impl fmt::Display for IrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            IrExprValue::Var(name) => write!(f, "{}", name),
            IrExprValue::PropertyAccess { object, property } => {
                write!(f, "{}.{}", object, property)
            }
            IrExprValue::String(s) => write!(f, "{:?}", s),
            IrExprValue::Boolean(b) => write!(f, "{}", b),
            IrExprValue::Number(n) => write!(f, "{}", n),
            IrExprValue::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            IrExprValue::Object(props) => {
                write!(f, "{{")?;
                for (i, (key, value)) in props.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            IrExprValue::BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            IrExprValue::UnaryOp { op, operand } => {
                write!(f, "{}{}", op, operand)
            }
        }
    }
}
