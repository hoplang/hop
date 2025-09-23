use crate::dop::TypedExpr;
use crate::dop::r#type::ComparableType;
use crate::dop::{Type, VarName};
use crate::ir::ast::IrEntrypoint;
use crate::ir::ast::{ExprId, IrExpr, IrStatement, StatementId};
use std::cell::RefCell;
use std::collections::BTreeMap;

pub fn build_ir_auto<F>(name: &str, params: Vec<(&str, Type)>, body_fn: F) -> IrEntrypoint
where
    F: FnOnce(&mut IrAutoBuilder),
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
    let inner_builder = IrTestBuilder::new(params_owned);
    let mut builder = IrAutoBuilder::new(inner_builder);
    body_fn(&mut builder);
    builder.build(name)
}

pub struct IrTestBuilder {
    next_expr_id: RefCell<ExprId>,
    next_node_id: RefCell<StatementId>,
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<(VarName, Type)>,
}

impl IrTestBuilder {
    fn new(params: Vec<(String, Type)>) -> Self {
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

    fn build(&self, name: &str, body: Vec<IrStatement>) -> IrEntrypoint {
        IrEntrypoint {
            name: name.to_string(),
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
        TypedExpr::StringLiteral {
            value: s.to_string(),
            annotation: self.next_expr_id(),
        }
    }

    pub fn num(&self, n: f64) -> IrExpr {
        TypedExpr::NumberLiteral {
            value: serde_json::Number::from_f64(n).unwrap_or_else(|| serde_json::Number::from(0)),
            annotation: self.next_expr_id(),
        }
    }

    pub fn bool(&self, b: bool) -> IrExpr {
        TypedExpr::BooleanLiteral {
            value: b,
            annotation: self.next_expr_id(),
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

        TypedExpr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            kind: typ,
            annotation: self.next_expr_id(),
        }
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        match (left.as_type(), right.as_type()) {
            (Type::Bool, Type::Bool) => TypedExpr::Comparison {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Bool,
                annotation: self.next_expr_id(),
            },
            (Type::String, Type::String) => TypedExpr::Comparison {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::String,
                annotation: self.next_expr_id(),
            },
            _ => panic!(
                "Unsupported type for equality comparison: {:?}",
                left.as_type()
            ),
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        TypedExpr::Negation {
            operand: Box::new(operand),
            annotation: self.next_expr_id(),
        }
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        // Determine the array element type from the first element
        let element_type = elements
            .first()
            .map(|first| Box::new(first.as_type().clone()));

        TypedExpr::ArrayLiteral {
            elements,
            kind: Type::Array(element_type),
            annotation: self.next_expr_id(),
        }
    }

    pub fn object(&self, props: Vec<(&str, IrExpr)>) -> IrExpr {
        // Build a type map from the property types
        let mut type_map = BTreeMap::new();
        for (key, expr) in &props {
            type_map.insert(key.to_string(), expr.as_type().clone());
        }

        TypedExpr::ObjectLiteral {
            properties: props.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            kind: Type::Object(type_map),
            annotation: self.next_expr_id(),
        }
    }

    pub fn prop_access(&self, object: IrExpr, property: &str) -> IrExpr {
        let property_type = match object.as_type() {
            Type::Object(type_map) => type_map
                .get(property)
                .cloned()
                .unwrap_or_else(|| panic!("Property '{}' not found in object type", property)),
            _ => panic!("Cannot access property '{}' on non-object type", property),
        };

        TypedExpr::PropertyAccess {
            object: Box::new(object),
            property: property.to_string(),
            kind: property_type,
            annotation: self.next_expr_id(),
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
        assert_eq!(*expr.as_type(), Type::String, "{}", expr);
        IrStatement::WriteExpr {
            id: self.next_node_id(),
            expr,
            escape,
        }
    }

    pub fn if_stmt(&self, cond: IrExpr, body: Vec<IrStatement>) -> IrStatement {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
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
        let element_type = match array.as_type() {
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
        let value_type = value.as_type().clone();

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
        TypedExpr::JsonEncode {
            value: Box::new(value),
            annotation: self.next_expr_id(),
        }
    }

    pub fn string_concat(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        TypedExpr::StringConcat {
            left: Box::new(left),
            right: Box::new(right),
            annotation: self.next_expr_id(),
        }
    }
}

pub struct IrAutoBuilder {
    inner: IrTestBuilder,
    statements: Vec<IrStatement>,
}

impl IrAutoBuilder {
    fn new(inner: IrTestBuilder) -> Self {
        Self {
            inner,
            statements: Vec::new(),
        }
    }

    fn new_scoped(&self) -> Self {
        Self {
            inner: IrTestBuilder {
                next_expr_id: self.inner.next_expr_id.clone(),
                next_node_id: self.inner.next_node_id.clone(),
                var_stack: self.inner.var_stack.clone(),
                params: self.inner.params.clone(),
            },
            statements: Vec::new(),
        }
    }

    fn build(self, name: &str) -> IrEntrypoint {
        self.inner.build(name, self.statements)
    }

    // Statement methods that auto-collect
    pub fn write(&mut self, s: &str) {
        self.statements.push(self.inner.write(s));
    }

    pub fn write_expr(&mut self, expr: IrExpr, escape: bool) {
        self.statements.push(self.inner.write_expr(expr, escape));
    }

    pub fn write_expr_escaped(&mut self, expr: IrExpr) {
        self.write_expr(expr, true);
    }

    pub fn if_stmt<F>(&mut self, cond: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let body = inner_builder.statements;
        self.statements.push(self.inner.if_stmt(cond, body));
    }

    pub fn for_loop<F>(&mut self, var: &str, array: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Extract element type from array
        let element_type = match array.as_type() {
            Type::Array(Some(elem_type)) => *elem_type.clone(),
            Type::Array(None) => panic!("Cannot iterate over array with unknown element type"),
            _ => panic!("Cannot iterate over non-array type"),
        };

        // Push the loop variable onto the stack
        self.inner
            .var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        // Create scoped builder and evaluate the body
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let body = inner_builder.statements;

        // Pop the loop variable from the stack
        self.inner.var_stack.borrow_mut().pop();

        self.statements
            .push(self.inner.for_loop(var, array, |_| body));
    }

    pub fn let_stmt<F>(&mut self, var: &str, value: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Get the type from the value expression
        let value_type = value.as_type().clone();

        // Push the variable onto the stack
        self.inner
            .var_stack
            .borrow_mut()
            .push((var.to_string(), value_type));

        // Create scoped builder and evaluate the body
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        let body = inner_builder.statements;

        // Pop the variable from the stack
        self.inner.var_stack.borrow_mut().pop();

        self.statements
            .push(self.inner.let_stmt(var, value, |_| body));
    }

    // Expression methods - delegate to inner builder
    pub fn str(&self, s: &str) -> IrExpr {
        self.inner.str(s)
    }

    pub fn num(&self, n: f64) -> IrExpr {
        self.inner.num(n)
    }

    pub fn bool(&self, b: bool) -> IrExpr {
        self.inner.bool(b)
    }

    pub fn var(&self, name: &str) -> IrExpr {
        self.inner.var(name)
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        self.inner.eq(left, right)
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        self.inner.not(operand)
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        self.inner.array(elements)
    }

    pub fn object(&self, props: Vec<(&str, IrExpr)>) -> IrExpr {
        self.inner.object(props)
    }

    pub fn prop_access(&self, object: IrExpr, property: &str) -> IrExpr {
        self.inner.prop_access(object, property)
    }

    pub fn json_encode(&self, value: IrExpr) -> IrExpr {
        self.inner.json_encode(value)
    }

    pub fn string_concat(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        self.inner.string_concat(left, right)
    }
}
