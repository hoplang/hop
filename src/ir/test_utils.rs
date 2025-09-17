use super::ast::IrEntrypoint;
use super::ast::{BinaryOp, ExprId, IrExpr, IrStatement, StatementId, UnaryOp};
use crate::dop::{Type, VarName};
use crate::dop::expr::Expr;
use std::cell::RefCell;
use std::collections::BTreeMap;

pub struct IrTestBuilder {
    next_expr_id: RefCell<ExprId>,
    next_node_id: RefCell<StatementId>,
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<(VarName, Type)>,
}

impl IrTestBuilder {
    pub fn new(params: Vec<(String, Type)>) -> Self {
        let initial_vars = params.clone();

        Self {
            next_expr_id: RefCell::new(1),
            next_node_id: RefCell::new(1),
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(s, t)| (VarName::try_from(s).unwrap(), t))
                .collect(),
        }
    }

    pub fn build(&self, body: Vec<IrStatement>) -> IrEntrypoint {
        IrEntrypoint {
            parameters: self.params.clone(),
            body,
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
        Expr::StringLiteral {
            value: s.to_string(),
            annotation: (self.next_expr_id(), Type::String),
        }
    }

    pub fn num(&self, n: f64) -> IrExpr {
        Expr::NumberLiteral {
            value: serde_json::Number::from_f64(n).unwrap_or_else(|| serde_json::Number::from(0)),
            annotation: (self.next_expr_id(), Type::Number),
        }
    }

    pub fn bool(&self, b: bool) -> IrExpr {
        Expr::BooleanLiteral {
            value: b,
            annotation: (self.next_expr_id(), Type::Bool),
        }
    }

    pub fn var(&self, name: &str) -> IrExpr {
        // Look up variable in the stack (search from end to beginning for most recent binding)
        let typ = self
            .var_stack
            .borrow()
            .iter()
            .rev()
            .find(|(var_name, _)| var_name == name)
            .map(|(_, typ)| typ.clone())
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' not found in scope. Available variables: {:?}",
                    name,
                    self.var_stack
                        .borrow()
                        .iter()
                        .map(|(n, _)| n.as_str())
                        .collect::<Vec<_>>()
                )
            });

        Expr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            annotation: (self.next_expr_id(), typ),
        }
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        Expr::BinaryOp {
            left: Box::new(left),
            operator: BinaryOp::Eq,
            right: Box::new(right),
            annotation: (self.next_expr_id(), Type::Bool),
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        Expr::UnaryOp {
            operator: UnaryOp::Not,
            operand: Box::new(operand),
            annotation: (self.next_expr_id(), Type::Bool),
        }
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        // Determine the array element type from the first element
        let element_type = elements.first().map(|first| Box::new(first.typ().clone()));

        Expr::ArrayLiteral {
            elements,
            annotation: (self.next_expr_id(), Type::Array(element_type)),
        }
    }

    pub fn object(&self, props: Vec<(&str, IrExpr)>) -> IrExpr {
        // Build a type map from the property types
        let mut type_map = BTreeMap::new();
        for (key, expr) in &props {
            type_map.insert(key.to_string(), expr.typ().clone());
        }

        Expr::ObjectLiteral {
            properties: props.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            annotation: (self.next_expr_id(), Type::Object(type_map)),
        }
    }

    pub fn prop_access(&self, object: IrExpr, property: &str) -> IrExpr {
        let property_type = match object.typ() {
            Type::Object(type_map) => type_map
                .get(property)
                .cloned()
                .unwrap_or_else(|| panic!("Property '{}' not found in object type", property)),
            _ => panic!("Cannot access property '{}' on non-object type", property),
        };

        Expr::PropertyAccess {
            object: Box::new(object),
            property: property.to_string(),
            annotation: (self.next_expr_id(), property_type),
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
        assert_eq!(*expr.typ(), Type::String, "{}", expr);
        IrStatement::WriteExpr {
            id: self.next_node_id(),
            expr,
            escape,
        }
    }

    pub fn if_stmt(&self, cond: IrExpr, body: Vec<IrStatement>) -> IrStatement {
        assert_eq!(*cond.typ(), Type::Bool, "{}", cond);
        IrStatement::If {
            id: self.next_node_id(),
            condition: cond,
            body,
        }
    }

    pub fn for_loop<F>(&self, var: &str, array: IrExpr, body_fn: F) -> IrStatement
    where
        F: FnOnce(&Self) -> Vec<IrStatement>,
    {
        // Extract element type from array
        let element_type = match array.typ() {
            Type::Array(Some(elem_type)) => *elem_type.clone(),
            Type::Array(None) => panic!("Cannot iterate over array with unknown element type"),
            _ => panic!("Cannot iterate over non-array type"),
        };

        // Push the loop variable onto the stack
        self.var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        // Evaluate the body with the variable in scope
        let body = body_fn(self);

        // Pop the loop variable from the stack
        self.var_stack.borrow_mut().pop();

        IrStatement::For {
            id: self.next_node_id(),
            var: VarName::try_from(var.to_string()).unwrap(),
            array,
            body,
        }
    }

    pub fn let_stmt<F>(&self, var: &str, value: IrExpr, body_fn: F) -> IrStatement
    where
        F: FnOnce(&Self) -> Vec<IrStatement>,
    {
        // Get the type from the value expression
        let value_type = value.typ().clone();

        // Push the variable onto the stack
        self.var_stack
            .borrow_mut()
            .push((var.to_string(), value_type));

        // Evaluate the body with the variable in scope
        let body = body_fn(self);

        // Pop the variable from the stack
        self.var_stack.borrow_mut().pop();

        IrStatement::Let {
            id: self.next_node_id(),
            var: VarName::try_from(var.to_string()).unwrap(),
            value,
            body,
        }
    }

    pub fn json_encode(&self, value: IrExpr) -> IrExpr {
        Expr::JsonEncode {
            value: Box::new(value),
            annotation: (self.next_expr_id(), Type::String),
        }
    }
}
