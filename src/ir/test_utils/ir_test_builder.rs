use crate::dop::Expr;
use crate::dop::field_name::FieldName;
use crate::dop::r#type::{ComparableType, EquatableType};
use crate::dop::{Type, VarName};
use crate::hop::component_name::ComponentName;
use crate::hop::module_name::ModuleName;
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

pub fn build_ir_with_records<F>(
    name: &str,
    params: Vec<(&str, Type)>,
    records: Vec<(&str, Vec<(&str, Type)>)>,
    body_fn: F,
) -> IrEntrypoint
where
    F: FnOnce(&mut IrAutoBuilder),
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
    let inner_builder = IrTestBuilder::with_records(params_owned, records);
    let mut builder = IrAutoBuilder::new(inner_builder);
    body_fn(&mut builder);
    builder.build(name)
}

pub struct IrTestBuilder {
    next_expr_id: RefCell<ExprId>,
    next_node_id: RefCell<StatementId>,
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<(VarName, Type)>,
    records: BTreeMap<String, BTreeMap<String, Type>>,
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
            records: BTreeMap::new(),
        }
    }

    fn with_records(params: Vec<(String, Type)>, records: Vec<(&str, Vec<(&str, Type)>)>) -> Self {
        let initial_vars = params.clone();

        let records_map: BTreeMap<String, BTreeMap<String, Type>> = records
            .into_iter()
            .map(|(name, fields)| {
                let fields_map: BTreeMap<String, Type> = fields
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v))
                    .collect();
                (name.to_string(), fields_map)
            })
            .collect();

        Self {
            next_expr_id: RefCell::new(1),
            next_node_id: RefCell::new(1),
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(s, t)| (VarName::try_from(s).unwrap(), t))
                .collect(),
            records: records_map,
        }
    }

    fn build(&self, name: &str, body: Vec<IrStatement>) -> IrEntrypoint {
        IrEntrypoint {
            name: ComponentName::new(name.to_string())
                .expect("Test component name should be valid"),
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
            annotation: self.next_expr_id(),
        }
    }

    pub fn int(&self, n: i64) -> IrExpr {
        Expr::IntLiteral {
            value: n,
            annotation: self.next_expr_id(),
        }
    }

    pub fn float(&self, n: f64) -> IrExpr {
        Expr::FloatLiteral {
            value: n,
            annotation: self.next_expr_id(),
        }
    }

    pub fn bool(&self, b: bool) -> IrExpr {
        Expr::BooleanLiteral {
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

        Expr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            kind: typ,
            annotation: self.next_expr_id(),
        }
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        match (left.as_type(), right.as_type()) {
            (Type::Bool, Type::Bool) => Expr::Equals {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: EquatableType::Bool,
                annotation: self.next_expr_id(),
            },
            (Type::String, Type::String) => Expr::Equals {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: EquatableType::String,
                annotation: self.next_expr_id(),
            },
            (Type::Int, Type::Int) => Expr::Equals {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: EquatableType::Int,
                annotation: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => Expr::Equals {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: EquatableType::Float,
                annotation: self.next_expr_id(),
            },
            _ => panic!(
                "Unsupported type for equality comparison: {:?}",
                left.as_type()
            ),
        }
    }

    pub fn less_than(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => Expr::LessThan {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Int,
                annotation: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => Expr::LessThan {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Float,
                annotation: self.next_expr_id(),
            },
            _ => panic!(
                "Unsupported type for less than comparison: {:?}",
                left.as_type()
            ),
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        Expr::BooleanNegation {
            operand: Box::new(operand),
            annotation: self.next_expr_id(),
        }
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        // Determine the array element type from the first element
        let element_type = elements
            .first()
            .map(|first| Box::new(first.as_type().clone()))
            .expect("Cannot create empty array literal in test builder - use typed_array for empty arrays");

        Expr::ArrayLiteral {
            elements,
            kind: Type::Array(element_type),
            annotation: self.next_expr_id(),
        }
    }

    pub fn typed_array(&self, element_type: Type, elements: Vec<IrExpr>) -> IrExpr {
        Expr::ArrayLiteral {
            elements,
            kind: Type::Array(Box::new(element_type)),
            annotation: self.next_expr_id(),
        }
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, IrExpr)>) -> IrExpr {
        // Verify the record exists and check field types
        let record_fields = self
            .records
            .get(record_name)
            .unwrap_or_else(|| panic!("Record '{}' not found in test builder", record_name));

        for (field_name, _) in &fields {
            if !record_fields.contains_key(*field_name) {
                panic!(
                    "Field '{}' not found in record '{}'",
                    field_name, record_name
                );
            }
        }

        let test_module = ModuleName::new("test").unwrap();
        let record_fields = self
            .records
            .get(record_name)
            .unwrap_or_else(|| panic!("Record '{}' not found in test builder", record_name));
        Expr::RecordInstantiation {
            record_name: record_name.to_string(),
            fields: fields
                .into_iter()
                .map(|(k, v)| (FieldName::new(k).unwrap(), v))
                .collect(),
            kind: Type::Record {
                module: test_module,
                name: record_name.to_string(),
                fields: record_fields
                    .iter()
                    .map(|(k, v)| (FieldName::new(k).unwrap(), v.clone()))
                    .collect(),
            },
            annotation: self.next_expr_id(),
        }
    }

    pub fn field_access(&self, object: IrExpr, field_str: &str) -> IrExpr {
        let field_name = FieldName::new(field_str).unwrap();
        let field_type = match object.as_type() {
            Type::Record {
                fields,
                name: record_name,
                ..
            } => fields
                .iter()
                .find(|(f, _)| f.as_str() == field_str)
                .map(|(_, t)| t.clone())
                .unwrap_or_else(|| {
                    panic!(
                        "Field '{}' not found in record type '{}'",
                        field_str, record_name
                    )
                }),
            _ => panic!("Cannot access field '{}' on non-record type", field_str),
        };

        Expr::FieldAccess {
            record: Box::new(object),
            field: field_name,
            kind: field_type,
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
        assert!(
            matches!(expr.as_type(), Type::String | Type::TrustedHTML),
            "WriteExpr expects String or TrustedHTML, got: {}",
            expr
        );
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
            else_body: None,
        }
    }

    pub fn if_else_stmt(
        &self,
        cond: IrExpr,
        body: Vec<IrStatement>,
        else_body: Vec<IrStatement>,
    ) -> IrStatement {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
        IrStatement::If {
            id: self.next_node_id(),
            condition: cond,
            body,
            else_body: Some(else_body),
        }
    }

    pub fn for_loop<F>(&self, var: &str, array: IrExpr, body_fn: F) -> IrStatement
    where
        F: FnOnce(&Self) -> Vec<IrStatement>,
    {
        // Extract element type from array
        let element_type = match array.as_type() {
            Type::Array(elem_type) => (**elem_type).clone(),
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
        Expr::JsonEncode {
            value: Box::new(value),
            annotation: self.next_expr_id(),
        }
    }

    pub fn string_concat(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        Expr::StringConcat {
            left: Box::new(left),
            right: Box::new(right),
            annotation: self.next_expr_id(),
        }
    }

    pub fn env_lookup(&self, key: IrExpr) -> IrExpr {
        Expr::EnvLookup {
            key: Box::new(key),
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
                records: self.inner.records.clone(),
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

    pub fn if_else_stmt<F, G>(&mut self, cond: IrExpr, body_fn: F, else_body_fn: G)
    where
        F: FnOnce(&mut Self),
        G: FnOnce(&mut Self),
    {
        let mut body_builder = self.new_scoped();
        body_fn(&mut body_builder);
        let body = body_builder.statements;

        let mut else_builder = self.new_scoped();
        else_body_fn(&mut else_builder);
        let else_body = else_builder.statements;

        self.statements
            .push(self.inner.if_else_stmt(cond, body, else_body));
    }

    pub fn for_loop<F>(&mut self, var: &str, array: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Extract element type from array
        let element_type = match array.as_type() {
            Type::Array(elem_type) => (**elem_type).clone(),
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

    pub fn int(&self, n: i64) -> IrExpr {
        self.inner.int(n)
    }

    pub fn float(&self, n: f64) -> IrExpr {
        self.inner.float(n)
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

    pub fn less_than(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        self.inner.less_than(left, right)
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        self.inner.not(operand)
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        self.inner.array(elements)
    }

    pub fn typed_array(&self, element_type: Type, elements: Vec<IrExpr>) -> IrExpr {
        self.inner.typed_array(element_type, elements)
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, IrExpr)>) -> IrExpr {
        self.inner.record(record_name, fields)
    }

    pub fn field_access(&self, object: IrExpr, field: &str) -> IrExpr {
        self.inner.field_access(object, field)
    }

    pub fn json_encode(&self, value: IrExpr) -> IrExpr {
        self.inner.json_encode(value)
    }

    pub fn string_concat(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        self.inner.string_concat(left, right)
    }

    pub fn env_lookup(&self, key: IrExpr) -> IrExpr {
        self.inner.env_lookup(key)
    }
}
