use crate::document::CheapString;
use crate::expr::Type;
use crate::expr::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::expr::typing::r#type::EquatableType;
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::expr::typing::type_registry_builder::{TestTypes, TypeRegistryBuilder};
use crate::ir::ast::{
    ExprId, IrArgument, IrExpr, IrForSource, IrParameter, IrStatement, StatementId,
};
use crate::ir::ast::{
    IrComponentDeclaration, IrEnumDeclaration, IrModule, IrRecordDeclaration, IrViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::Arc;

struct PendingView {
    name: String,
    params: Vec<(String, String)>,
    body_fn: Box<dyn FnOnce(&mut IrBuilder)>,
}

pub struct IrModuleBuilder {
    types_builder: TypeRegistryBuilder,
    record_names: BTreeSet<TypeName>,
    enum_names: BTreeSet<TypeName>,
    views: Vec<PendingView>,
    components: Vec<PendingView>,
}

impl IrModuleBuilder {
    pub fn new() -> Self {
        Self {
            types_builder: TypeRegistryBuilder::new(),
            record_names: BTreeSet::new(),
            enum_names: BTreeSet::new(),
            views: Vec::new(),
            components: Vec::new(),
        }
    }

    pub fn record<'a>(
        mut self,
        name: &str,
        fields: impl IntoIterator<Item = (&'a str, &'a str)>,
    ) -> Self {
        self.types_builder = self.types_builder.record(name, fields);
        self.record_names.insert(TypeName::new(name).unwrap());
        self
    }

    /// Define an enum with unit variants (no fields)
    pub fn enum_unit<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        self.types_builder = self.types_builder.enum_unit(name, variants);
        self.enum_names.insert(TypeName::new(name).unwrap());
        self
    }

    /// Define an enum with variants that may carry fields
    pub fn enum_<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = (&'a str, Vec<(&'a str, &'a str)>)>,
    ) -> Self {
        self.types_builder = self.types_builder.enum_(name, variants);
        self.enum_names.insert(TypeName::new(name).unwrap());
        self
    }

    pub fn view_no_params<F>(self, name: &str, body_fn: F) -> Self
    where
        F: FnOnce(&mut IrBuilder) + 'static,
    {
        self.view(name, [], body_fn)
    }

    pub fn view<'a, F>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> Self
    where
        F: FnOnce(&mut IrBuilder) + 'static,
    {
        self.views.push(Self::pending(name, params, body_fn));
        self
    }

    pub fn component<'a, F>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> Self
    where
        F: FnOnce(&mut IrBuilder) + 'static,
    {
        self.components.push(Self::pending(name, params, body_fn));
        self
    }

    fn pending<'a, F>(
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> PendingView
    where
        F: FnOnce(&mut IrBuilder) + 'static,
    {
        PendingView {
            name: name.to_string(),
            params: params
                .into_iter()
                .map(|(k, t)| (k.to_string(), t.to_string()))
                .collect(),
            body_fn: Box::new(body_fn),
        }
    }

    pub fn build(self) -> IrModule {
        self.build_with_registry().0
    }

    pub fn build_with_registry(self) -> (IrModule, TypeRegistry) {
        let types = self.types_builder.build();
        let build_pending = |pending: Vec<PendingView>, types: &TestTypes| {
            pending
                .into_iter()
                .map(|pending| {
                    let params = pending
                        .params
                        .iter()
                        .map(|(k, t)| (k.clone(), types.resolve(t)))
                        .collect();
                    let mut builder = IrBuilder::new(params, types.clone());
                    (pending.body_fn)(&mut builder);
                    builder.build(&pending.name)
                })
                .collect::<Vec<_>>()
        };
        let views = build_pending(self.views, &types);
        let components = build_pending(self.components, &types)
            .into_iter()
            .map(|view| IrComponentDeclaration {
                name: view.name,
                parameters: view.parameters,
                body: view.body,
            })
            .collect();
        let enum_declarations = self
            .enum_names
            .iter()
            .map(|name| IrEnumDeclaration {
                name: name.clone(),
                variants: types.enum_variants(name.as_str()).to_vec(),
            })
            .collect();
        let record_declarations = self
            .record_names
            .iter()
            .map(|name| IrRecordDeclaration {
                name: name.clone(),
                fields: types.record_fields(name.as_str()).to_vec(),
            })
            .collect();
        let ir_module = IrModule {
            views,
            components,
            records: record_declarations,
            enums: enum_declarations,
        };
        (ir_module, types.registry().clone())
    }
}

impl Default for IrModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub fn build_ir_no_params<F>(name: &str, body_fn: F) -> IrViewDeclaration
where
    F: FnOnce(&mut IrBuilder),
{
    let mut builder = IrBuilder::new(vec![], TestTypes::empty());
    body_fn(&mut builder);
    builder.build(name)
}

pub fn build_ir<'a, F>(
    name: &str,
    params: impl IntoIterator<Item = (&'a str, &'a str)>,
    body_fn: F,
) -> IrViewDeclaration
where
    F: FnOnce(&mut IrBuilder),
{
    let types = TestTypes::empty();
    let params = params
        .into_iter()
        .map(|(k, t)| (k.to_string(), types.resolve(t)))
        .collect();
    let mut builder = IrBuilder::new(params, types);
    body_fn(&mut builder);
    builder.build(name)
}

pub struct IrBuilder {
    next_expr_id: Rc<RefCell<ExprId>>,
    next_node_id: Rc<RefCell<StatementId>>,
    var_stack: RefCell<Vec<(String, Arc<Type>)>>,
    params: Vec<IrParameter>,
    types: TestTypes,
    statements: Vec<IrStatement>,
}

impl IrBuilder {
    fn new(params: Vec<(String, Arc<Type>)>, types: TestTypes) -> Self {
        let initial_vars = params.clone();

        Self {
            next_expr_id: Rc::new(RefCell::new(1)),
            next_node_id: Rc::new(RefCell::new(1)),
            var_stack: RefCell::new(initial_vars),
            params: params
                .into_iter()
                .map(|(s, t)| IrParameter {
                    name: VarName::try_from(s).unwrap(),
                    typ: t,
                })
                .collect(),
            types,
            statements: Vec::new(),
        }
    }

    fn new_scoped(&self) -> Self {
        Self {
            next_expr_id: self.next_expr_id.clone(),
            next_node_id: self.next_node_id.clone(),
            var_stack: self.var_stack.clone(),
            params: self.params.clone(),
            types: self.types.clone(),
            statements: Vec::new(),
        }
    }

    fn build(self, name: &str) -> IrViewDeclaration {
        IrViewDeclaration {
            name: TypeName::new(name).expect("Test component name should be valid"),
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
            value: CheapString::new(s.to_string()),
            id: self.next_expr_id(),
        }
    }

    pub fn int(&self, n: i64) -> IrExpr {
        IrExpr::IntLiteral {
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
            .map(|first| first.get_type())
            .expect("Cannot create empty array literal in test builder");

        IrExpr::ArrayLiteral {
            elements,
            kind: Arc::new(Type::Array(element_type)),
            id: self.next_expr_id(),
        }
    }

    pub fn int_to_string(&self, value: IrExpr) -> IrExpr {
        IrExpr::IntToString {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, IrExpr)>) -> IrExpr {
        let name = TypeName::new(record_name).unwrap();
        let record_fields = self.types.record_fields(record_name);

        for (field_name, _) in &fields {
            if !record_fields
                .iter()
                .any(|(f, _, _)| f.as_str() == *field_name)
            {
                panic!(
                    "Field '{}' not found in record '{}'",
                    field_name, record_name
                );
            }
        }

        IrExpr::RecordLiteral {
            record_name: name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (FieldName::new(k).unwrap(), v))
                .collect(),
            kind: self.types.named(record_name),
            id: self.next_expr_id(),
        }
    }

    /// Create a unit enum variant (no fields)
    pub fn enum_variant(&self, enum_name: &str, variant_name: &str) -> IrExpr {
        self.enum_variant_with_fields(enum_name, variant_name, vec![])
    }

    /// Create an enum variant with field values
    pub fn enum_variant_with_fields(
        &self,
        enum_name: &str,
        variant_name: &str,
        field_values: Vec<(&str, IrExpr)>,
    ) -> IrExpr {
        let name = TypeName::new(enum_name).unwrap();
        let variants = self.types.enum_variants(enum_name);

        if !variants.iter().any(|v| v.name.as_str() == variant_name) {
            let variant_names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
            panic!(
                "Variant '{}' not found in enum '{}'. Available variants: {:?}",
                variant_name, enum_name, variant_names
            );
        }

        IrExpr::EnumLiteral {
            enum_name: name,
            variant_name: TypeName::new(variant_name).unwrap(),
            fields: field_values
                .into_iter()
                .map(|(k, v)| (FieldName::new(k).unwrap(), v))
                .collect(),
            kind: self.types.named(enum_name),
            id: self.next_expr_id(),
        }
    }

    /// Create a Some option literal
    pub fn some(&self, inner: IrExpr) -> IrExpr {
        let inner_type = inner.get_type();
        IrExpr::OptionLiteral {
            value: Some(Box::new(inner)),
            kind: Arc::new(Type::Option(inner_type)),
            id: self.next_expr_id(),
        }
    }

    /// Create a None option literal, the inner type is given in source
    /// syntax, e.g. "String" or "Array[Node]"
    pub fn none(&self, inner_type: &str) -> IrExpr {
        IrExpr::OptionLiteral {
            value: None,
            kind: Arc::new(Type::Option(self.types.resolve(inner_type))),
            id: self.next_expr_id(),
        }
    }

    /// Create a match expression over an enum value
    /// arms is a list of (variant_name, body_expr) tuples
    pub fn match_expr(&self, subject: IrExpr, arms: Vec<(&str, IrExpr)>) -> IrExpr {
        // Get the enum type from the subject
        let (enum_name, result_type) = match self.types.registry().resolve(subject.as_type()) {
            Some(ResolvedType::Enum { name, .. }) => {
                // Use the type of the first arm's body as the result type
                let result_type = arms
                    .first()
                    .map(|(_, body)| body.get_type())
                    .unwrap_or_else(|| Arc::new(Type::String));
                (name.clone(), result_type)
            }
            _ => panic!("Match subject must be an enum type"),
        };

        let ir_arms: Vec<EnumMatchArm<IrExpr>> = arms
            .into_iter()
            .map(|(variant_name, body)| EnumMatchArm {
                pattern: EnumPattern::Variant {
                    enum_name: enum_name.clone(),
                    variant_name: TypeName::new(variant_name).unwrap(),
                },
                bindings: vec![],
                body,
            })
            .collect();

        IrExpr::Match {
            match_: Match::Enum {
                subject: Box::new(subject),
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
        let result_type = true_body.get_type();

        IrExpr::Match {
            match_: Match::Bool {
                subject: Box::new(subject),
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
        let result_type = some_body.get_type();

        IrExpr::Match {
            match_: Match::Option {
                subject: Box::new(subject),
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
    pub fn option_match_expr_with_binding<F, T>(
        &self,
        subject: IrExpr,
        binding_name: &str,
        inner_type: T,
        some_body_fn: F,
        none_body: IrExpr,
    ) -> IrExpr
    where
        F: FnOnce(&Self) -> IrExpr,
        T: Into<Arc<Type>>,
    {
        // Push the binding onto the variable stack
        self.var_stack
            .borrow_mut()
            .push((binding_name.to_string(), inner_type.into()));

        let some_body = some_body_fn(self);

        // Pop the binding from the variable stack
        self.var_stack.borrow_mut().pop();

        let result_type = some_body.get_type();

        IrExpr::Match {
            match_: Match::Option {
                subject: Box::new(subject),
                some_arm_binding: Some(VarName::new(binding_name).unwrap()),
                some_arm_body: Box::new(some_body),
                none_arm_body: Box::new(none_body),
            },
            kind: result_type,
            id: self.next_expr_id(),
        }
    }

    /// Create a match expression over an enum value with field bindings.
    /// Each arm is a tuple of (variant_name, field_bindings, body_fn).
    /// field_bindings is a list of (field_name, binding_name) pairs.
    pub fn match_expr_with_bindings(
        &self,
        subject: IrExpr,
        arms: Vec<(&str, Vec<(&str, &str)>, Box<dyn FnOnce(&Self) -> IrExpr>)>,
    ) -> IrExpr {
        let Some(ResolvedType::Enum { name, variants, .. }) =
            self.types.registry().resolve(subject.as_type())
        else {
            panic!("Match subject must be an enum type")
        };
        let enum_name = name.clone();

        // Use the type of the first arm's body as the result type (computed after building arms)
        let mut result_type: Option<Arc<Type>> = None;

        let ir_arms: Vec<EnumMatchArm<IrExpr>> = arms
            .into_iter()
            .map(|(variant_name, field_bindings, body_fn)| {
                // Look up the variant to get field types
                let variant_fields = variants
                    .iter()
                    .find(|v| v.name.as_str() == variant_name)
                    .map(|v| &v.fields)
                    .expect("Variant not found in enum");

                // Create binding names and push variables to scope
                let bindings: Vec<(FieldName, VarName)> = field_bindings
                    .iter()
                    .map(|(field_name, binding_name)| {
                        // Add bound variable to scope
                        let field_type = variant_fields
                            .iter()
                            .find(|(f, _, _)| f.as_str() == *field_name)
                            .map(|(_, t, _)| t.clone())
                            .expect("Field not found in variant");
                        self.var_stack
                            .borrow_mut()
                            .push((binding_name.to_string(), field_type));

                        (
                            FieldName::new(field_name).unwrap(),
                            VarName::new(binding_name).unwrap(),
                        )
                    })
                    .collect();

                let body = body_fn(self);

                // Pop bindings from scope
                for _ in &bindings {
                    self.var_stack.borrow_mut().pop();
                }

                if result_type.is_none() {
                    result_type = Some(body.get_type());
                }

                EnumMatchArm {
                    pattern: EnumPattern::Variant {
                        enum_name: enum_name.clone(),
                        variant_name: TypeName::new(variant_name).unwrap(),
                    },
                    bindings,
                    body,
                }
            })
            .collect();

        IrExpr::Match {
            match_: Match::Enum {
                subject: Box::new(subject),
                arms: ir_arms,
            },
            kind: result_type.unwrap_or_else(|| Arc::new(Type::String)),
            id: self.next_expr_id(),
        }
    }

    pub fn field_access(&self, object: IrExpr, field_str: &str) -> IrExpr {
        let field_name = FieldName::new(field_str).unwrap();
        let field_type = match self.types.registry().resolve(object.as_type()) {
            Some(ResolvedType::Record {
                name: record_name,
                fields,
                ..
            }) => fields
                .iter()
                .find(|(f, _, _)| f.as_str() == field_str)
                .map(|(_, t, _)| t.clone())
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

    /// Create a let expression that binds a variable to a value and evaluates a body expression.
    pub fn let_expr<F>(&self, var_name: &str, value: IrExpr, body_fn: F) -> IrExpr
    where
        F: FnOnce(&Self) -> IrExpr,
    {
        let value_type = value.get_type();

        // Push the binding onto the variable stack
        self.var_stack
            .borrow_mut()
            .push((var_name.to_string(), value_type));

        let body = body_fn(self);

        // Pop the binding from the variable stack
        self.var_stack.borrow_mut().pop();

        let kind = body.get_type();

        IrExpr::Let {
            var_name: VarName::new(var_name).unwrap(),
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

    pub fn join(&self, args: Vec<IrExpr>) -> IrExpr {
        match args.len() {
            0 => IrExpr::StringLiteral {
                value: CheapString::new(String::new()),
                id: self.next_expr_id(),
            },
            1 => args.into_iter().next().unwrap(),
            _ => {
                let mut iter = args.into_iter();
                let mut result = iter.next().unwrap();
                for arg in iter {
                    result = IrExpr::StringConcat {
                        left: Box::new(result),
                        right: Box::new(IrExpr::StringLiteral {
                            value: CheapString::new(" ".to_string()),
                            id: self.next_expr_id(),
                        }),
                        id: self.next_expr_id(),
                    };
                    result = IrExpr::StringConcat {
                        left: Box::new(result),
                        right: Box::new(arg),
                        id: self.next_expr_id(),
                    };
                }
                result
            }
        }
    }

    pub fn tw_merge(&self, value: IrExpr) -> IrExpr {
        IrExpr::TwMerge {
            operand: Box::new(value),
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
            matches!(expr.as_type(), Type::String | Type::Fragment),
            "WriteExpr expects String or Fragment, got: {}",
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
        });
    }

    pub fn for_loop<F>(&mut self, var: &str, array: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let element_type = match array.as_type() {
            Type::Array(elem_type) => elem_type.clone(),
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
            var: Some(VarName::try_from(var.to_string()).unwrap()),
            source: IrForSource::Array(array),
            body: inner_builder.statements,
        });
    }

    /// Create a for loop over an inclusive range (start..=end)
    pub fn for_range<F>(&mut self, var: &str, start: IrExpr, end: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Range loops iterate over integers
        self.var_stack
            .borrow_mut()
            .push((var.to_string(), Arc::new(Type::Int)));

        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);

        self.var_stack.borrow_mut().pop();

        self.statements.push(IrStatement::For {
            id: self.next_node_id(),
            var: Some(VarName::try_from(var.to_string()).unwrap()),
            source: IrForSource::RangeInclusive { start, end },
            body: inner_builder.statements,
        });
    }

    /// Create a for loop over an inclusive range without binding the loop variable (underscore)
    pub fn for_range_discarded<F>(&mut self, start: IrExpr, end: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut inner_builder = self.new_scoped();
        body_fn(&mut inner_builder);

        self.statements.push(IrStatement::For {
            id: self.next_node_id(),
            var: None,
            source: IrForSource::RangeInclusive { start, end },
            body: inner_builder.statements,
        });
    }

    pub fn let_stmt<F>(&mut self, var: &str, value: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let value_type = value.get_type();

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

    pub fn let_fragment<F1, F2>(&mut self, var: &str, fragment_body_fn: F1, body_fn: F2)
    where
        F1: FnOnce(&mut Self),
        F2: FnOnce(&mut Self),
    {
        let mut fragment_builder = self.new_scoped();
        fragment_body_fn(&mut fragment_builder);

        self.var_stack
            .borrow_mut()
            .push((var.to_string(), Arc::new(Type::Fragment)));

        let mut body_builder = self.new_scoped();
        body_fn(&mut body_builder);

        self.var_stack.borrow_mut().pop();

        self.statements.push(IrStatement::LetFragment {
            id: self.next_node_id(),
            var: VarName::try_from(var.to_string()).unwrap(),
            fragment_body: fragment_builder.statements,
            body: body_builder.statements,
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
        use crate::expr::patterns::Match;

        assert_eq!(*subject.as_type(), Type::Bool);

        let mut true_builder = self.new_scoped();
        true_body_fn(&mut true_builder);

        let mut false_builder = self.new_scoped();
        false_body_fn(&mut false_builder);

        self.statements.push(IrStatement::Match {
            id: self.next_node_id(),
            match_: Match::Bool {
                subject: Box::new(subject),
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
        use crate::expr::patterns::Match;

        let inner_type = match subject.as_type() {
            Type::Option(inner) => inner.clone(),
            _ => panic!("Cannot match on non-option type"),
        };

        // Build some body with optional binding
        let some_arm_binding = binding_var.map(|var| {
            self.var_stack
                .borrow_mut()
                .push((var.to_string(), inner_type));
            VarName::try_from(var.to_string()).unwrap()
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
                subject: Box::new(subject),
                some_arm_binding,
                some_arm_body: Box::new(some_builder.statements),
                none_arm_body: Box::new(none_builder.statements),
            },
        });
    }

    /// Create a match statement over an enum value with field bindings
    /// arms is a list of (variant_name, field_bindings, body_fn) tuples
    /// where field_bindings is a list of (field_name, binding_name) pairs
    pub fn enum_match_stmt_with_bindings(
        &mut self,
        subject: IrExpr,
        arms: Vec<(&str, Vec<(&str, &str)>, Box<dyn FnOnce(&mut Self)>)>,
    ) {
        use crate::expr::patterns::Match;

        let Some(ResolvedType::Enum { name, variants, .. }) =
            self.types.registry().resolve(subject.as_type())
        else {
            panic!("Match subject must be an enum type")
        };
        let enum_name = name.clone();

        let ir_arms: Vec<EnumMatchArm<Vec<IrStatement>>> = arms
            .into_iter()
            .map(|(variant_name, field_bindings, body_fn)| {
                // Look up the variant to get field types
                let variant_fields = variants
                    .iter()
                    .find(|v| v.name.as_str() == variant_name)
                    .map(|v| &v.fields)
                    .expect("Variant not found in enum");

                // Create binding names and push variables to scope
                let bindings: Vec<(FieldName, VarName)> = field_bindings
                    .iter()
                    .map(|(field_name, binding_name)| {
                        (
                            FieldName::new(field_name).unwrap(),
                            VarName::new(binding_name).unwrap(),
                        )
                    })
                    .collect();

                let mut arm_builder = self.new_scoped();

                // Add bound variables to the builder's stack
                for (field_name, binding_name) in &bindings {
                    let field_type = variant_fields
                        .iter()
                        .find(|(f, _, _)| f.as_str() == field_name.as_str())
                        .map(|(_, t, _)| t.clone())
                        .expect("Field not found in variant");
                    arm_builder
                        .var_stack
                        .borrow_mut()
                        .push((binding_name.as_str().to_string(), field_type));
                }

                body_fn(&mut arm_builder);

                EnumMatchArm {
                    pattern: EnumPattern::Variant {
                        enum_name: enum_name.clone(),
                        variant_name: TypeName::new(variant_name).unwrap(),
                    },
                    bindings,
                    body: arm_builder.statements,
                }
            })
            .collect();

        self.statements.push(IrStatement::Match {
            id: self.next_node_id(),
            match_: Match::Enum {
                subject: Box::new(subject),
                arms: ir_arms,
            },
        });
    }

    /// Create a record destructure statement.
    /// `bindings` is a list of (field_name, binding_name) pairs.
    pub fn record_destructure_stmt<F>(
        &mut self,
        subject: IrExpr,
        bindings: Vec<(&str, &str)>,
        body_fn: F,
    ) where
        F: FnOnce(&mut Self),
    {
        let Some(ResolvedType::Record {
            fields: record_fields,
            ..
        }) = self.types.registry().resolve(subject.as_type())
        else {
            panic!("record_destructure_stmt subject must be a record type")
        };

        let ir_bindings: Vec<(FieldName, VarName)> = bindings
            .iter()
            .map(|(field_name, binding_name)| {
                (
                    FieldName::new(field_name).unwrap(),
                    VarName::new(binding_name).unwrap(),
                )
            })
            .collect();

        let mut inner_builder = self.new_scoped();
        for (field_name, binding_name) in &ir_bindings {
            let field_type = record_fields
                .iter()
                .find(|(f, _, _)| f.as_str() == field_name.as_str())
                .map(|(_, t, _)| t.clone())
                .expect("Field not found in record");
            inner_builder
                .var_stack
                .borrow_mut()
                .push((binding_name.as_str().to_string(), field_type));
        }

        body_fn(&mut inner_builder);

        self.statements.push(IrStatement::LetRecordDestructure {
            id: self.next_node_id(),
            subject,
            bindings: ir_bindings,
            body: inner_builder.statements,
        });
    }

    /// Invoke a component by name with the given arguments.
    /// No validation is done against the invoked component's params,
    /// since components may still be pending at the point of the call.
    pub fn invoke_component(&mut self, name: &str, args: Vec<(&str, IrExpr)>) {
        let ir_args: Vec<IrArgument> = args
            .into_iter()
            .map(|(k, expr)| IrArgument {
                name: VarName::new(k).unwrap(),
                expr,
            })
            .collect();

        self.statements.push(IrStatement::ComponentInvocation {
            id: self.next_node_id(),
            component_name: TypeName::new(name).unwrap(),
            args: ir_args,
        });
    }
}
