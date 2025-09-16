use super::ast::{BinaryOp, ExprId, IrExpr, IrExprValue, IrStatement, StatementId, UnaryOp};
use crate::dop::Type;
use std::cell::RefCell;
use std::collections::BTreeMap;

pub struct IrTestBuilder {
    next_expr_id: RefCell<ExprId>,
    next_node_id: RefCell<StatementId>,
}

impl IrTestBuilder {
    pub fn new() -> Self {
        Self {
            next_expr_id: RefCell::new(1),
            next_node_id: RefCell::new(1),
        }
    }

    fn next_expr_id(&self) -> ExprId {
        let id = *self.next_expr_id.borrow();
        *self.next_expr_id.borrow_mut() = id + 1;
        id
    }

    fn next_node_id(&self) -> StatementId {
        let id = *self.next_node_id.borrow();
        *self.next_node_id.borrow_mut() = id + 1;
        id
    }

    // Expression builders
    pub fn str(&self, s: &str) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::StringLiteral(s.to_string()),
            typ: Type::String,
        }
    }

    pub fn num(&self, n: f64) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::NumberLiteral(n),
            typ: Type::Number,
        }
    }

    pub fn bool(&self, b: bool) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::BooleanLiteral(b),
            typ: Type::Bool,
        }
    }

    pub fn var(&self, name: &str) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::Var(name.to_string()),
            // TODO: Do we need to construct the correct type here?
            typ: Type::String,
        }
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::BinaryOp {
                left: Box::new(left),
                op: BinaryOp::Eq,
                right: Box::new(right),
            },
            typ: Type::Bool,
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            },
            typ: Type::Bool,
        }
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        // Determine the array element type from the first element
        let element_type = elements.first().map(|first| Box::new(first.typ.clone()));

        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::ArrayLiteral(elements),
            typ: Type::Array(element_type),
        }
    }

    pub fn object(&self, props: Vec<(&str, IrExpr)>) -> IrExpr {
        // Build a type map from the property types
        let mut type_map = BTreeMap::new();
        for (key, expr) in &props {
            type_map.insert(key.to_string(), expr.typ.clone());
        }

        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::ObjectLiteral(
                props.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            ),
            typ: Type::Object(type_map),
        }
    }

    pub fn prop_access(&self, object: IrExpr, property: &str) -> IrExpr {
        // TODO: Do we need to construct the correct type here?
        let property_type = match &object.typ {
            Type::Object(type_map) => type_map.get(property).cloned().unwrap_or(Type::String),
            _ => Type::String,
        };

        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::PropertyAccess {
                object: Box::new(object),
                property: property.to_string(),
            },
            typ: property_type,
        }
    }

    // Node builders
    pub fn write(&self, s: &str) -> IrStatement {
        IrStatement::Write {
            id: self.next_node_id(),
            content: s.to_string(),
        }
    }

    pub fn write_expr(&self, expr: IrExpr, escape: bool) -> IrStatement {
        IrStatement::WriteExpr {
            id: self.next_node_id(),
            expr,
            escape,
        }
    }

    pub fn if_stmt(&self, cond: IrExpr, body: Vec<IrStatement>) -> IrStatement {
        IrStatement::If {
            id: self.next_node_id(),
            condition: cond,
            body,
        }
    }

    pub fn for_loop(&self, var: &str, array: IrExpr, body: Vec<IrStatement>) -> IrStatement {
        IrStatement::For {
            id: self.next_node_id(),
            var: var.to_string(),
            array,
            body,
        }
    }

    pub fn let_stmt(&self, var: &str, value: IrExpr, body: Vec<IrStatement>) -> IrStatement {
        IrStatement::Let {
            id: self.next_node_id(),
            var: var.to_string(),
            value,
            body,
        }
    }
}
