use crate::dop::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::dop::semantics::r#type::{ComparableType, EquatableType};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::{Type, VarName};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use crate::ir::ast::{ExprId, IrExpr, IrStatement, StatementId};
use crate::ir::ast::{IrComponentDeclaration, IrEnumDeclaration, IrModule, IrRecordDeclaration};
use std::cell::RefCell;
use std::collections::BTreeMap;

/// Helper function to extract (VarName, Type) from an IrExpr.
/// Panics if the expression is not a variable reference.
fn extract_var_subject(expr: &IrExpr) -> (VarName, Type) {
    match expr {
        IrExpr::Var { value, kind, .. } => (value.clone(), kind.clone()),
        _ => panic!("Match subject must be a variable reference, got {:?}", expr),
    }
}

pub fn build_ir<F, P>(name: &str, params: P, body_fn: F) -> IrComponentDeclaration
where
    F: FnOnce(&mut IrBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
    let mut builder = IrBuilder::new(params_owned, BTreeMap::new(), BTreeMap::new());
    body_fn(&mut builder);
    builder.build(name)
}

pub fn build_ir_with_records<F, P>(
    name: &str,
    params: P,
    records: Vec<(&str, Vec<(&str, Type)>)>,
    body_fn: F,
) -> IrComponentDeclaration
where
    F: FnOnce(&mut IrBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();

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

    let mut builder = IrBuilder::new(params_owned, records_map, BTreeMap::new());
    body_fn(&mut builder);
    builder.build(name)
}

pub fn build_ir_with_enums<F, P>(
    name: &str,
    params: P,
    enums: Vec<(&str, Vec<&str>)>,
    body_fn: F,
) -> IrComponentDeclaration
where
    F: FnOnce(&mut IrBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
{
    let params_owned: Vec<(String, Type)> = params
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();

    let enums_map: BTreeMap<String, Vec<String>> = enums
        .into_iter()
        .map(|(name, variants)| {
            (
                name.to_string(),
                variants.into_iter().map(|v| v.to_string()).collect(),
            )
        })
        .collect();

    let mut builder = IrBuilder::new(params_owned, BTreeMap::new(), enums_map);
    body_fn(&mut builder);
    builder.build(name)
}

/// Build a simple module with just components (no records or enums)
pub fn build_module<F, P>(name: &str, params: P, body_fn: F) -> IrModule
where
    F: FnOnce(&mut IrBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
{
    IrModule {
        components: vec![build_ir(name, params, body_fn)],
        records: vec![],
        enums: vec![],
    }
}

/// Build a module with records
pub fn build_module_with_records<F, P>(
    name: &str,
    params: P,
    records: Vec<(&str, Vec<(&str, Type)>)>,
    body_fn: F,
) -> IrModule
where
    F: FnOnce(&mut IrBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
{
    let record_declarations: Vec<IrRecordDeclaration> = records
        .iter()
        .map(|(name, fields)| IrRecordDeclaration {
            name: name.to_string(),
            fields: fields
                .iter()
                .map(|(k, v)| (FieldName::new(k).unwrap(), v.clone()))
                .collect(),
        })
        .collect();

    IrModule {
        components: vec![build_ir_with_records(name, params, records, body_fn)],
        records: record_declarations,
        enums: vec![],
    }
}

/// Build a module with enums
pub fn build_module_with_enums<F, P>(
    name: &str,
    params: P,
    enums: Vec<(&str, Vec<&str>)>,
    body_fn: F,
) -> IrModule
where
    F: FnOnce(&mut IrBuilder),
    P: IntoIterator<Item = (&'static str, Type)>,
{
    let enum_declarations: Vec<IrEnumDeclaration> = enums
        .iter()
        .map(|(name, variants)| IrEnumDeclaration {
            name: name.to_string(),
            variants: variants.iter().map(|v| TypeName::new(v).unwrap()).collect(),
        })
        .collect();

    IrModule {
        components: vec![build_ir_with_enums(name, params, enums, body_fn)],
        records: vec![],
        enums: enum_declarations,
    }
}

pub struct IrBuilder {
    next_expr_id: RefCell<ExprId>,
    next_node_id: RefCell<StatementId>,
    var_stack: RefCell<Vec<(String, Type)>>,
    params: Vec<(VarName, Type)>,
    records: BTreeMap<String, BTreeMap<String, Type>>,
    enums: BTreeMap<String, Vec<String>>,
    statements: Vec<IrStatement>,
}

impl IrBuilder {
    fn new(
        params: Vec<(String, Type)>,
        records: BTreeMap<String, BTreeMap<String, Type>>,
        enums: BTreeMap<String, Vec<String>>,
    ) -> Self {
        let initial_vars = params.clone();

        Self {
            next_expr_id: RefCell::new(1),
            next_node_id: RefCell::new(1),
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(s, t)| (VarName::try_from(s).unwrap(), t))
                .collect(),
            records,
            enums,
            statements: Vec::new(),
        }
    }

    fn new_scoped(&self) -> Self {
        Self {
            next_expr_id: self.next_expr_id.clone(),
            next_node_id: self.next_node_id.clone(),
            var_stack: self.var_stack.clone(),
            params: self.params.clone(),
            records: self.records.clone(),
            enums: self.enums.clone(),
            statements: Vec::new(),
        }
    }

    fn build(self, name: &str) -> IrComponentDeclaration {
        IrComponentDeclaration {
            name: ComponentName::new(name.to_string())
                .expect("Test component name should be valid"),
            parameters: self.params,
            body: self.statements,
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
        IrExpr::StringLiteral {
            value: s.to_string(),
            id: self.next_expr_id(),
        }
    }

    pub fn int(&self, n: i64) -> IrExpr {
        IrExpr::IntLiteral {
            value: n,
            id: self.next_expr_id(),
        }
    }

    pub fn float(&self, n: f64) -> IrExpr {
        IrExpr::FloatLiteral {
            value: n,
            id: self.next_expr_id(),
        }
    }

    pub fn bool(&self, b: bool) -> IrExpr {
        IrExpr::BooleanLiteral {
            value: b,
            id: self.next_expr_id(),
        }
    }

    pub fn var(&self, name: &str) -> IrExpr {
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

        IrExpr::Var {
            value: VarName::try_from(name.to_string()).unwrap(),
            kind: typ,
            id: self.next_expr_id(),
        }
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Bool, Type::Bool) => EquatableType::Bool,
            (Type::String, Type::String) => EquatableType::String,
            (Type::Int, Type::Int) => EquatableType::Int,
            (Type::Float, Type::Float) => EquatableType::Float,
            (
                Type::Enum { module, name, .. },
                Type::Enum {
                    module: module2,
                    name: name2,
                    ..
                },
            ) if module == module2 && name == name2 => EquatableType::Enum {
                module: module.clone(),
                name: name.clone(),
            },
            _ => panic!(
                "Unsupported type for equality comparison: {:?}",
                left.as_type()
            ),
        };
        IrExpr::Equals {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn less_than(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => IrExpr::LessThan {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Int,
                id: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => IrExpr::LessThan {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Float,
                id: self.next_expr_id(),
            },
            _ => panic!(
                "Unsupported type for less than comparison: {:?}",
                left.as_type()
            ),
        }
    }

    pub fn less_than_or_equal(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => IrExpr::LessThanOrEqual {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Int,
                id: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => IrExpr::LessThanOrEqual {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: ComparableType::Float,
                id: self.next_expr_id(),
            },
            _ => panic!(
                "Unsupported type for less than or equal comparison: {:?}",
                left.as_type()
            ),
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        IrExpr::BooleanNegation {
            operand: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn and(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        IrExpr::BooleanLogicalAnd {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn or(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        IrExpr::BooleanLogicalOr {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn array(&self, elements: Vec<IrExpr>) -> IrExpr {
        let element_type = elements
            .first()
            .map(|first| Box::new(first.as_type().clone()))
            .expect("Cannot create empty array literal in test builder - use typed_array for empty arrays");

        IrExpr::ArrayLiteral {
            elements,
            kind: Type::Array(element_type),
            id: self.next_expr_id(),
        }
    }

    pub fn typed_array(&self, element_type: Type, elements: Vec<IrExpr>) -> IrExpr {
        IrExpr::ArrayLiteral {
            elements,
            kind: Type::Array(Box::new(element_type)),
            id: self.next_expr_id(),
        }
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, IrExpr)>) -> IrExpr {
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
        IrExpr::RecordLiteral {
            record_name: record_name.to_string(),
            fields: fields
                .into_iter()
                .map(|(k, v)| (FieldName::new(k).unwrap(), v))
                .collect(),
            kind: Type::Record {
                module: test_module,
                name: TypeName::new(record_name).unwrap(),
                fields: record_fields
                    .iter()
                    .map(|(k, v)| (FieldName::new(k).unwrap(), v.clone()))
                    .collect(),
            },
            id: self.next_expr_id(),
        }
    }

    pub fn enum_variant(&self, enum_name: &str, variant_name: &str) -> IrExpr {
        let variants = self
            .enums
            .get(enum_name)
            .unwrap_or_else(|| panic!("Enum '{}' not found in test builder", enum_name));

        if !variants.iter().any(|v| v == variant_name) {
            panic!(
                "Variant '{}' not found in enum '{}'. Available variants: {:?}",
                variant_name, enum_name, variants
            );
        }

        let test_module = ModuleName::new("test").unwrap();
        IrExpr::EnumLiteral {
            enum_name: enum_name.to_string(),
            variant_name: variant_name.to_string(),
            kind: Type::Enum {
                module: test_module,
                name: TypeName::new(enum_name).unwrap(),
                variants: variants.iter().map(|v| TypeName::new(v).unwrap()).collect(),
            },
            id: self.next_expr_id(),
        }
    }

    /// Create a Some option literal
    pub fn some(&self, inner: IrExpr) -> IrExpr {
        let inner_type = inner.as_type().clone();
        IrExpr::OptionLiteral {
            value: Some(Box::new(inner)),
            kind: Type::Option(Box::new(inner_type)),
            id: self.next_expr_id(),
        }
    }

    /// Create a None option literal
    pub fn none(&self, inner_type: Type) -> IrExpr {
        IrExpr::OptionLiteral {
            value: None,
            kind: Type::Option(Box::new(inner_type)),
            id: self.next_expr_id(),
        }
    }

    /// Create a match expression over an enum value
    /// arms is a list of (variant_name, body_expr) tuples
    pub fn match_expr(&self, subject: IrExpr, arms: Vec<(&str, IrExpr)>) -> IrExpr {
        // Get the enum type from the subject
        let (enum_name, result_type) = match subject.as_type() {
            Type::Enum { name, .. } => {
                // Use the type of the first arm's body as the result type
                let result_type = arms
                    .first()
                    .map(|(_, body)| body.as_type().clone())
                    .unwrap_or(Type::String);
                (name.as_str().to_string(), result_type)
            }
            _ => panic!("Match subject must be an enum type"),
        };

        let ir_arms: Vec<EnumMatchArm<IrExpr>> = arms
            .into_iter()
            .map(|(variant_name, body)| EnumMatchArm {
                pattern: EnumPattern::Variant {
                    enum_name: enum_name.clone(),
                    variant_name: variant_name.to_string(),
                },
                body,
            })
            .collect();

        IrExpr::Match {
            match_: Match::Enum {
                subject: extract_var_subject(&subject),
                arms: ir_arms,
            },
            kind: result_type,
            id: self.next_expr_id(),
        }
    }

    /// Create a match expression over a boolean value
    pub fn bool_match_expr(
        &self,
        subject: IrExpr,
        true_body: IrExpr,
        false_body: IrExpr,
    ) -> IrExpr {
        let result_type = true_body.as_type().clone();

        IrExpr::Match {
            match_: Match::Bool {
                subject: extract_var_subject(&subject),
                true_body: Box::new(true_body),
                false_body: Box::new(false_body),
            },
            kind: result_type,
            id: self.next_expr_id(),
        }
    }

    /// Create a match expression over an option value
    pub fn option_match_expr(
        &self,
        subject: IrExpr,
        some_body: IrExpr,
        none_body: IrExpr,
    ) -> IrExpr {
        let result_type = some_body.as_type().clone();

        IrExpr::Match {
            match_: Match::Option {
                subject: extract_var_subject(&subject),
                some_arm_binding: None,
                some_arm_body: Box::new(some_body),
                none_arm_body: Box::new(none_body),
            },
            kind: result_type,
            id: self.next_expr_id(),
        }
    }

    /// Create a match expression over an option value with a binding for the Some case.
    /// The `some_body_fn` closure receives the builder with the binding in scope.
    pub fn option_match_expr_with_binding<F>(
        &self,
        subject: IrExpr,
        binding_name: &str,
        inner_type: Type,
        some_body_fn: F,
        none_body: IrExpr,
    ) -> IrExpr
    where
        F: FnOnce(&Self) -> IrExpr,
    {
        // Push the binding onto the variable stack
        self.var_stack
            .borrow_mut()
            .push((binding_name.to_string(), inner_type.clone()));

        let some_body = some_body_fn(self);

        // Pop the binding from the variable stack
        self.var_stack.borrow_mut().pop();

        let result_type = some_body.as_type().clone();

        IrExpr::Match {
            match_: Match::Option {
                subject: extract_var_subject(&subject),
                some_arm_binding: Some((VarName::new(binding_name).unwrap(), inner_type)),
                some_arm_body: Box::new(some_body),
                none_arm_body: Box::new(none_body),
            },
            kind: result_type,
            id: self.next_expr_id(),
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

        IrExpr::FieldAccess {
            record: Box::new(object),
            field: field_name,
            kind: field_type,
            id: self.next_expr_id(),
        }
    }

    pub fn json_encode(&self, value: IrExpr) -> IrExpr {
        IrExpr::JsonEncode {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    /// Create a let expression that binds a variable to a value and evaluates a body expression.
    pub fn let_expr<F>(&self, var_name: &str, value: IrExpr, body_fn: F) -> IrExpr
    where
        F: FnOnce(&Self) -> IrExpr,
    {
        let value_type = value.as_type().clone();

        // Push the binding onto the variable stack
        self.var_stack
            .borrow_mut()
            .push((var_name.to_string(), value_type));

        let body = body_fn(self);

        // Pop the binding from the variable stack
        self.var_stack.borrow_mut().pop();

        let kind = body.as_type().clone();

        IrExpr::Let {
            var: VarName::new(var_name).unwrap(),
            value: Box::new(value),
            body: Box::new(body),
            kind,
            id: self.next_expr_id(),
        }
    }

    pub fn string_concat(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        IrExpr::StringConcat {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn add(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        use crate::dop::semantics::r#type::NumericType;
        match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => IrExpr::NumericAdd {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: NumericType::Int,
                id: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => IrExpr::NumericAdd {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: NumericType::Float,
                id: self.next_expr_id(),
            },
            _ => panic!("Unsupported types for addition: {:?}", left.as_type()),
        }
    }

    pub fn subtract(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        use crate::dop::semantics::r#type::NumericType;
        match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => IrExpr::NumericSubtract {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: NumericType::Int,
                id: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => IrExpr::NumericSubtract {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: NumericType::Float,
                id: self.next_expr_id(),
            },
            _ => panic!("Unsupported types for subtraction: {:?}", left.as_type()),
        }
    }

    pub fn multiply(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        use crate::dop::semantics::r#type::NumericType;
        match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => IrExpr::NumericMultiply {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: NumericType::Int,
                id: self.next_expr_id(),
            },
            (Type::Float, Type::Float) => IrExpr::NumericMultiply {
                left: Box::new(left),
                right: Box::new(right),
                operand_types: NumericType::Float,
                id: self.next_expr_id(),
            },
            _ => panic!("Unsupported types for multiplication: {:?}", left.as_type()),
        }
    }

    pub fn env_lookup(&self, key: IrExpr) -> IrExpr {
        IrExpr::EnvLookup {
            key: Box::new(key),
            id: self.next_expr_id(),
        }
    }

    // Statement methods that auto-collect
    pub fn write(&mut self, s: &str) {
        self.statements.push(IrStatement::Write {
            id: self.next_node_id(),
            content: s.to_string(),
        });
    }

    pub fn write_expr(&mut self, expr: IrExpr, escape: bool) {
        assert!(
            matches!(expr.as_type(), Type::String | Type::TrustedHTML),
            "WriteExpr expects String or TrustedHTML, got: {}",
            expr
        );
        self.statements.push(IrStatement::WriteExpr {
            id: self.next_node_id(),
            expr,
            escape,
        });
    }

    pub fn write_expr_escaped(&mut self, expr: IrExpr) {
        self.write_expr(expr, true);
    }

    pub fn if_stmt<F>(&mut self, cond: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);
        self.statements.push(IrStatement::If {
            id: self.next_node_id(),
            condition: cond,
            body: inner_builder.statements,
            else_body: None,
        });
    }

    pub fn if_else_stmt<F, G>(&mut self, cond: IrExpr, body_fn: F, else_body_fn: G)
    where
        F: FnOnce(&mut Self),
        G: FnOnce(&mut Self),
    {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);

        let mut body_builder = self.new_scoped();
        body_fn(&mut body_builder);

        let mut else_builder = self.new_scoped();
        else_body_fn(&mut else_builder);

        self.statements.push(IrStatement::If {
            id: self.next_node_id(),
            condition: cond,
            body: body_builder.statements,
            else_body: Some(else_builder.statements),
        });
    }

    pub fn for_loop<F>(&mut self, var: &str, array: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let element_type = match array.as_type() {
            Type::Array(elem_type) => (**elem_type).clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), element_type));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);

        self.var_stack.borrow_mut().pop();

        self.statements.push(IrStatement::For {
            id: self.next_node_id(),
            var: VarName::try_from(var.to_string()).unwrap(),
            array,
            body: inner_builder.statements,
        });
    }

    pub fn let_stmt<F>(&mut self, var: &str, value: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let value_type = value.as_type().clone();

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), value_type));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);

        self.var_stack.borrow_mut().pop();

        self.statements.push(IrStatement::Let {
            id: self.next_node_id(),
            var: VarName::try_from(var.to_string()).unwrap(),
            value,
            body: inner_builder.statements,
        });
    }

    pub fn bool_match_stmt<FTrue, FFalse>(
        &mut self,
        subject: IrExpr,
        true_body_fn: FTrue,
        false_body_fn: FFalse,
    ) where
        FTrue: FnOnce(&mut Self),
        FFalse: FnOnce(&mut Self),
    {
        use crate::dop::patterns::Match;

        assert_eq!(*subject.as_type(), Type::Bool);

        let mut true_builder = self.new_scoped();
        true_body_fn(&mut true_builder);

        let mut false_builder = self.new_scoped();
        false_body_fn(&mut false_builder);

        self.statements.push(IrStatement::Match {
            id: self.next_node_id(),
            match_: Match::Bool {
                subject: extract_var_subject(&subject),
                true_body: Box::new(true_builder.statements),
                false_body: Box::new(false_builder.statements),
            },
        });
    }

    pub fn option_match_stmt<FSome, FNone>(
        &mut self,
        subject: IrExpr,
        binding_var: Option<&str>,
        some_body_fn: FSome,
        none_body_fn: FNone,
    ) where
        FSome: FnOnce(&mut Self),
        FNone: FnOnce(&mut Self),
    {
        use crate::dop::patterns::Match;

        let inner_type = match subject.as_type() {
            Type::Option(inner) => (**inner).clone(),
            _ => panic!("Cannot match on non-option type"),
        };

        // Build some body with optional binding
        let some_arm_binding = binding_var.map(|var| {
            self.var_stack
                .borrow_mut()
                .push((var.to_string(), inner_type.clone()));
            (VarName::try_from(var.to_string()).unwrap(), inner_type)
        });

        let mut some_builder = self.new_scoped();
        some_body_fn(&mut some_builder);

        if binding_var.is_some() {
            self.var_stack.borrow_mut().pop();
        }

        // Build none body
        let mut none_builder = self.new_scoped();
        none_body_fn(&mut none_builder);

        self.statements.push(IrStatement::Match {
            id: self.next_node_id(),
            match_: Match::Option {
                subject: extract_var_subject(&subject),
                some_arm_binding,
                some_arm_body: Box::new(some_builder.statements),
                none_arm_body: Box::new(none_builder.statements),
            },
        });
    }

    /// Create a match statement over an enum value
    /// arms is a list of (variant_name, body_fn) tuples
    pub fn enum_match_stmt(
        &mut self,
        subject: IrExpr,
        arms: Vec<(&str, Box<dyn FnOnce(&mut Self)>)>,
    ) {
        use crate::dop::patterns::Match;

        let enum_name = match subject.as_type() {
            Type::Enum { name, .. } => name.as_str().to_string(),
            _ => panic!("Match subject must be an enum type"),
        };

        let ir_arms: Vec<EnumMatchArm<Vec<IrStatement>>> = arms
            .into_iter()
            .map(|(variant_name, body_fn)| {
                let mut arm_builder = self.new_scoped();
                body_fn(&mut arm_builder);
                EnumMatchArm {
                    pattern: EnumPattern::Variant {
                        enum_name: enum_name.clone(),
                        variant_name: variant_name.to_string(),
                    },
                    body: arm_builder.statements,
                }
            })
            .collect();

        self.statements.push(IrStatement::Match {
            id: self.next_node_id(),
            match_: Match::Enum {
                subject: extract_var_subject(&subject),
                arms: ir_arms,
            },
        });
    }
}
