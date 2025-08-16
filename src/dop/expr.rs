#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DopExpr {
    Variable(String),
    PropertyAccess(Box<DopExpr>, String),
    StringLiteral(String),
    BooleanLiteral(bool),
    BinaryOp(Box<DopExpr>, BinaryOp, Box<DopExpr>),
    UnaryOp(UnaryOp, Box<DopExpr>),
}