use std::collections::BTreeMap;
use std::fmt::{self, Display};

use crate::dop::parser::DopVarName;
use crate::range::string_cursor::StringSpan;
use crate::range::{Range, Ranged};

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Equal => write!(f, "=="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DopExpr {
    Variable {
        value: DopVarName,
    },
    PropertyAccess {
        object: Box<DopExpr>,
        property: StringSpan,
        span: StringSpan,
    },
    StringLiteral {
        value: String,
        span: StringSpan,
    },
    BooleanLiteral {
        value: bool,
        span: StringSpan,
    },
    NumberLiteral {
        value: serde_json::Number,
        span: StringSpan,
    },
    /// An array literal, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<DopExpr>,
        span: StringSpan,
    },
    ObjectLiteral {
        properties: BTreeMap<String, DopExpr>,
        span: StringSpan,
    },
    BinaryOp {
        left: Box<DopExpr>,
        operator: BinaryOp,
        right: Box<DopExpr>,
        span: StringSpan,
    },
    UnaryOp {
        operator: UnaryOp,
        operand: Box<DopExpr>,
        span: StringSpan,
    },
}

impl DopExpr {
    /// Returns the span of this expression in the source code
    pub fn span(&self) -> StringSpan {
        match self {
            DopExpr::Variable { value, .. } => value.span().clone(),
            DopExpr::PropertyAccess { span, .. } => span.clone(),
            DopExpr::StringLiteral { span, .. } => span.clone(),
            DopExpr::BooleanLiteral { span, .. } => span.clone(),
            DopExpr::NumberLiteral { span, .. } => span.clone(),
            DopExpr::ArrayLiteral { span, .. } => span.clone(),
            DopExpr::ObjectLiteral { span, .. } => span.clone(),
            DopExpr::BinaryOp { span, .. } => span.clone(),
            DopExpr::UnaryOp { span, .. } => span.clone(),
        }
    }
}

impl Ranged for DopExpr {
    /// Returns the range of this expression in the source code
    fn range(&self) -> Range {
        self.span().range()
    }
}

impl Display for DopExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_expr(expr: &DopExpr, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            let indent_str = "  ".repeat(indent);
            match expr {
                DopExpr::Variable { value } => write!(f, "{}", value),
                DopExpr::PropertyAccess { object, property, .. } => {
                    fmt_expr(object, f, indent)?;
                    write!(f, ".{}", property)
                }
                DopExpr::StringLiteral { value, .. } => write!(f, "\"{}\"", value),
                DopExpr::BooleanLiteral { value, .. } => write!(f, "{}", value),
                DopExpr::NumberLiteral { value, .. } => write!(f, "{}", value),
                DopExpr::ArrayLiteral { elements, .. } => {
                    if elements.is_empty() {
                        write!(f, "[]")
                    } else if elements.len() == 1 && !needs_indentation(&elements[0]) {
                        write!(f, "[")?;
                        fmt_expr(&elements[0], f, indent)?;
                        write!(f, "]")
                    } else if elements.iter().all(|e| !needs_indentation(e)) && elements.len() <= 3 {
                        // Simple elements on one line
                        write!(f, "[")?;
                        for (i, elem) in elements.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            fmt_expr(elem, f, indent)?;
                        }
                        write!(f, "]")
                    } else {
                        // Multi-line format
                        writeln!(f, "[")?;
                        for (i, elem) in elements.iter().enumerate() {
                            write!(f, "{}  ", indent_str)?;
                            fmt_expr(elem, f, indent + 1)?;
                            if i < elements.len() - 1 {
                                write!(f, ",")?;
                            }
                            writeln!(f)?;
                        }
                        write!(f, "{}]", indent_str)
                    }
                }
                DopExpr::ObjectLiteral { properties, .. } => {
                    if properties.is_empty() {
                        write!(f, "{{}}")
                    } else if properties.len() == 1 {
                        let (key, value) = properties.iter().next().unwrap();
                        if !needs_indentation(value) {
                            write!(f, "{{{}: ", key)?;
                            fmt_expr(value, f, indent)?;
                            write!(f, "}}")
                        } else {
                            // Single property with nested value - use inline format if it's a simple nested object
                            match value {
                                DopExpr::ObjectLiteral { properties, .. } if properties.len() == 1 => {
                                    write!(f, "{{{}: ", key)?;
                                    fmt_expr(value, f, indent)?;
                                    write!(f, "}}")
                                }
                                _ => {
                                    writeln!(f, "{{")?;
                                    write!(f, "{}  {}: ", indent_str, key)?;
                                    fmt_expr(value, f, indent + 1)?;
                                    writeln!(f)?;
                                    write!(f, "{}}}", indent_str)
                                }
                            }
                        }
                    } else {
                        // Multi-line format for multiple properties
                        writeln!(f, "{{")?;
                        for (i, (key, value)) in properties.iter().enumerate() {
                            write!(f, "{}  {}: ", indent_str, key)?;
                            fmt_expr(value, f, indent + 1)?;
                            if i < properties.len() - 1 {
                                write!(f, ",")?;
                            }
                            writeln!(f)?;
                        }
                        write!(f, "{}}}", indent_str)
                    }
                }
                DopExpr::BinaryOp { left, operator, right, .. } => {
                    write!(f, "(")?;
                    fmt_expr(left, f, indent)?;
                    write!(f, " {} ", operator)?;
                    fmt_expr(right, f, indent)?;
                    write!(f, ")")
                }
                DopExpr::UnaryOp { operator, operand, .. } => {
                    write!(f, "({}",  operator)?;
                    fmt_expr(operand, f, indent)?;
                    write!(f, ")")
                }
            }
        }
        
        fn needs_indentation(expr: &DopExpr) -> bool {
            match expr {
                DopExpr::ArrayLiteral { elements, .. } => !elements.is_empty(),
                DopExpr::ObjectLiteral { properties, .. } => !properties.is_empty(),
                _ => false,
            }
        }
        
        fmt_expr(self, f, 0)
    }
}