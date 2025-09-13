use std::fmt;

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
pub enum IrExpr {
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
        match self {
            IrExpr::Var(name) => write!(f, "{}", name),
            IrExpr::PropertyAccess { object, property } => {
                write!(f, "{}.{}", object, property)
            }
            IrExpr::String(s) => write!(f, "{:?}", s),
            IrExpr::Boolean(b) => write!(f, "{}", b),
            IrExpr::Number(n) => write!(f, "{}", n),
            IrExpr::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            IrExpr::Object(props) => {
                write!(f, "{{")?;
                for (i, (key, value)) in props.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            IrExpr::BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            IrExpr::UnaryOp { op, operand } => {
                write!(f, "{}{}", op, operand)
            }
        }
    }
}
