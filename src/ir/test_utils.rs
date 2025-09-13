use super::ast::{BinaryOp, ExprId, IrExpr, IrExprValue, IrNode, NodeId, UnaryOp};
use std::cell::RefCell;

pub struct IrTestBuilder {
    next_expr_id: RefCell<ExprId>,
    next_node_id: RefCell<NodeId>,
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

    fn next_node_id(&self) -> NodeId {
        let id = *self.next_node_id.borrow();
        *self.next_node_id.borrow_mut() = id + 1;
        id
    }

    // Expression builders
    pub fn str(&self, s: &str) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::String(s.to_string()),
        }
    }

    pub fn num(&self, n: f64) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::Number(n),
        }
    }

    pub fn boolean(&self, b: bool) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::Boolean(b),
        }
    }

    pub fn var(&self, name: &str) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::Var(name.to_string()),
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
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            },
        }
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::Array(elements),
        }
    }

    pub fn object(&self, props: Vec<(&str, IrExpr)>) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::Object(
                props.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            ),
        }
    }

    pub fn prop_access(&self, object: IrExpr, property: &str) -> IrExpr {
        IrExpr {
            id: self.next_expr_id(),
            value: IrExprValue::PropertyAccess {
                object: Box::new(object),
                property: property.to_string(),
            },
        }
    }

    // Node builders
    pub fn write(&self, s: &str) -> IrNode {
        IrNode::Write {
            id: self.next_node_id(),
            content: s.to_string(),
        }
    }

    pub fn write_expr(&self, expr: IrExpr, escape: bool) -> IrNode {
        IrNode::WriteExpr {
            id: self.next_node_id(),
            expr,
            escape,
        }
    }

    pub fn if_stmt(&self, cond: IrExpr, body: Vec<IrNode>) -> IrNode {
        IrNode::If {
            id: self.next_node_id(),
            condition: cond,
            body,
        }
    }

    pub fn for_loop(&self, var: &str, array: IrExpr, body: Vec<IrNode>) -> IrNode {
        IrNode::For {
            id: self.next_node_id(),
            var: var.to_string(),
            array,
            body,
        }
    }

    pub fn let_stmt(&self, var: &str, value: IrExpr, body: Vec<IrNode>) -> IrNode {
        IrNode::Let {
            id: self.next_node_id(),
            var: var.to_string(),
            value,
            body,
        }
    }
}

