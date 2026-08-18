use crate::document::CheapString;
use crate::expr::Type;
use crate::expr::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::expr::typing::r#type::{ComparableType, EnumVariant, EquatableType, NumericType};
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::expr::typing::type_registry_builder::{TestTypes, TypeRegistryBuilder};
use crate::ir::ir_module::{
    ExprId, ExprIdCounter, IrArgument, IrExpr, IrForSource, IrParameter, IrStatement, IrVar,
    StatementId, StatementIdCounter, VarIdCounter,
};
use crate::ir::ir_module::{
    IrComponentDeclaration, IrEnumDeclaration, IrModule, IrRecordDeclaration, IrViewDeclaration,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

/// Declares the record and enum types of a module under construction.
pub struct IrModuleBuilder {
    types_builder: TypeRegistryBuilder,
}

impl IrModuleBuilder {
    pub fn new() -> Self {
        Self {
            types_builder: TypeRegistryBuilder::new(),
        }
    }

    pub fn record<'a>(
        mut self,
        name: &str,
        fields: impl IntoIterator<Item = (&'a str, &'a str)>,
    ) -> Self {
        self.types_builder = self.types_builder.record(name, fields);
        self
    }

    /// Define an enum with unit variants (no fields)
    pub fn enum_unit<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        self.types_builder = self.types_builder.enum_unit(name, variants);
        self
    }

    /// Define an enum with variants that may carry fields
    pub fn enum_<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = (&'a str, Vec<(&'a str, &'a str)>)>,
    ) -> Self {
        self.types_builder = self.types_builder.enum_(name, variants);
        self
    }

    /// Freeze the declared types, enabling view and component bodies.
    pub fn freeze(self) -> IrModuleBodiesBuilder {
        IrModuleBodiesBuilder {
            types: Rc::new(self.types_builder.build()),
            expr_ids: Rc::new(RefCell::new(ExprIdCounter::new())),
            statement_ids: Rc::new(RefCell::new(StatementIdCounter::new())),
            var_ids: Rc::new(RefCell::new(VarIdCounter::new())),
            views: Vec::new(),
            components: Vec::new(),
        }
    }

    pub fn view_no_params<F>(self, name: &str, body_fn: F) -> IrModuleBodiesBuilder
    where
        F: FnOnce(&mut IrBuilder),
    {
        self.freeze().view_no_params(name, body_fn)
    }

    pub fn view<'a, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> IrModuleBodiesBuilder
    where
        F: FnOnce(&mut IrBuilder),
    {
        self.freeze().view(name, params, body_fn)
    }

    pub fn component<'a, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> IrModuleBodiesBuilder
    where
        F: FnOnce(&mut IrBuilder),
    {
        self.freeze().component(name, params, body_fn)
    }
}

impl Default for IrModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl From<IrModuleBuilder> for IrModuleBodiesBuilder {
    fn from(builder: IrModuleBuilder) -> Self {
        builder.freeze()
    }
}

/// Collects view and component bodies against a frozen set of types.
pub struct IrModuleBodiesBuilder {
    types: Rc<TestTypes>,
    expr_ids: Rc<RefCell<ExprIdCounter>>,
    statement_ids: Rc<RefCell<StatementIdCounter>>,
    var_ids: Rc<RefCell<VarIdCounter>>,
    views: Vec<IrViewDeclaration>,
    components: Vec<IrComponentDeclaration>,
}

impl IrModuleBodiesBuilder {
    pub fn view_no_params<F>(self, name: &str, body_fn: F) -> Self
    where
        F: FnOnce(&mut IrBuilder),
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
        F: FnOnce(&mut IrBuilder),
    {
        let (parameters, body) = self.declaration(params, body_fn);
        self.views.push(IrViewDeclaration {
            name: TypeName::new(name).expect("Test view name should be valid"),
            parameters,
            body,
        });
        self
    }

    pub fn component<'a, F>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> Self
    where
        F: FnOnce(&mut IrBuilder),
    {
        let (parameters, body) = self.declaration(params, body_fn);
        self.components.push(IrComponentDeclaration {
            name: TypeName::new(name).expect("Test component name should be valid"),
            parameters,
            body,
        });
        self
    }

    /// Resolve parameters and run the body closure immediately.
    fn declaration<'a, F>(
        &self,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> (Vec<IrParameter>, Vec<IrStatement>)
    where
        F: FnOnce(&mut IrBuilder),
    {
        let parameters: Vec<IrParameter> = params
            .into_iter()
            .map(|(name, typ)| {
                let id = self.var_ids.borrow_mut().next();
                IrParameter {
                    var: IrVar::new(id, VarName::new(name).unwrap()),
                    typ: self.types.resolve(typ),
                }
            })
            .collect();
        let vars = parameters
            .iter()
            .map(|p| (p.var.clone(), p.typ.clone()))
            .collect();
        let mut builder = IrBuilder {
            var_stack: vars,
            types: self.types.clone(),
            expr_ids: self.expr_ids.clone(),
            statement_ids: self.statement_ids.clone(),
            var_ids: self.var_ids.clone(),
            statements: Vec::new(),
        };
        body_fn(&mut builder);
        (parameters, builder.statements)
    }

    pub fn build(self) -> IrModule {
        self.build_with_registry().0
    }

    pub fn build_with_registry(self) -> (IrModule, TypeRegistry) {
        let mut record_declarations = Vec::new();
        let mut enum_declarations = Vec::new();
        for (name, resolved) in self.types.declared_types() {
            match resolved {
                ResolvedType::Record { fields, .. } => {
                    record_declarations.push(IrRecordDeclaration {
                        name: name.clone(),
                        fields: fields.to_vec(),
                    });
                }
                ResolvedType::Enum { variants, .. } => {
                    enum_declarations.push(IrEnumDeclaration {
                        name: name.clone(),
                        variants: variants.to_vec(),
                    });
                }
                _ => unreachable!("only records and enums can be declared"),
            }
        }
        let module = IrModule {
            views: self.views,
            components: self.components,
            records: record_declarations,
            enums: enum_declarations,
            expr_ids: *self.expr_ids.borrow(),
        };
        (module, self.types.registry().clone())
    }
}

pub struct IrBuilder {
    var_stack: Vec<(IrVar, Arc<Type>)>,
    types: Rc<TestTypes>,
    expr_ids: Rc<RefCell<ExprIdCounter>>,
    statement_ids: Rc<RefCell<StatementIdCounter>>,
    var_ids: Rc<RefCell<VarIdCounter>>,
    statements: Vec<IrStatement>,
}

impl IrBuilder {
    fn next_expr_id(&self) -> ExprId {
        self.expr_ids.borrow_mut().next()
    }

    fn next_statement_id(&self) -> StatementId {
        self.statement_ids.borrow_mut().next()
    }

    fn bind(&self, name: &str) -> IrVar {
        let id = self.var_ids.borrow_mut().next();
        IrVar::new(id, VarName::new(name).unwrap())
    }

    fn scoped(&self, bindings: impl IntoIterator<Item = (IrVar, Arc<Type>)>) -> Self {
        let mut var_stack = self.var_stack.clone();
        var_stack.extend(bindings);
        Self {
            var_stack,
            types: self.types.clone(),
            expr_ids: self.expr_ids.clone(),
            statement_ids: self.statement_ids.clone(),
            var_ids: self.var_ids.clone(),
            statements: Vec::new(),
        }
    }

    /// Run `body_fn` with `bindings` pushed onto the variable stack,
    /// returning the statements it emitted as a separate body.
    fn in_scope(
        &mut self,
        bindings: impl IntoIterator<Item = (IrVar, Arc<Type>)>,
        body_fn: impl FnOnce(&mut Self),
    ) -> Vec<IrStatement> {
        let saved_statements = std::mem::take(&mut self.statements);
        let saved_vars = self.var_stack.len();
        self.var_stack.extend(bindings);
        body_fn(self);
        self.var_stack.truncate(saved_vars);
        std::mem::replace(&mut self.statements, saved_statements)
    }

    /// In-scope variables, innermost last.
    pub fn vars(&self) -> &[(IrVar, Arc<Type>)] {
        &self.var_stack
    }

    /// Resolve a source-syntax type string, e.g. `Array[Int]`.
    pub fn resolve_type(&self, type_str: &str) -> Arc<Type> {
        self.types.resolve(type_str)
    }

    pub fn str(&self, s: &str) -> IrExpr {
        IrExpr::StringLiteral {
            value: CheapString::new(s.to_string()),
            id: self.next_expr_id(),
        }
    }

    pub fn int(&self, n: i32) -> IrExpr {
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

    pub fn float(&self, f: f64) -> IrExpr {
        IrExpr::FloatLiteral {
            value: f,
            id: self.next_expr_id(),
        }
    }

    pub fn fragment_empty(&self) -> IrExpr {
        IrExpr::FragmentEmpty {
            id: self.next_expr_id(),
        }
    }

    pub fn var(&self, name: &str) -> IrExpr {
        let (value, kind) = self
            .var_stack
            .iter()
            .rev()
            .find(|(var, _)| var.as_str() == name)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' not found in scope. Available variables: {:?}",
                    name,
                    self.var_stack
                        .iter()
                        .map(|(v, _)| v.as_str())
                        .collect::<Vec<_>>()
                )
            });

        IrExpr::Var {
            value,
            kind,
            id: self.next_expr_id(),
        }
    }

    pub fn eq(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Bool, Type::Bool) => EquatableType::Bool,
            (Type::String, Type::String) => EquatableType::String,
            (Type::Int, Type::Int) => EquatableType::Int,
            (Type::Float, Type::Float) => EquatableType::Float,
            (l, r) => panic!(
                "Unsupported types for equality comparison: {:?} == {:?}",
                l, r
            ),
        };
        IrExpr::Equals {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn lt(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => ComparableType::Int,
            (Type::Float, Type::Float) => ComparableType::Float,
            (l, r) => panic!(
                "Unsupported types for less-than comparison: {:?} < {:?}",
                l, r
            ),
        };
        IrExpr::LessThan {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn lte(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => ComparableType::Int,
            (Type::Float, Type::Float) => ComparableType::Float,
            (l, r) => panic!(
                "Unsupported types for less-than-or-equal comparison: {:?} <= {:?}",
                l, r
            ),
        };
        IrExpr::LessThanOrEqual {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn add(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for addition: {:?} + {:?}", l, r),
        };
        IrExpr::NumericAdd {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn sub(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for subtraction: {:?} - {:?}", l, r),
        };
        IrExpr::NumericSubtract {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn mul(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for multiplication: {:?} * {:?}", l, r),
        };
        IrExpr::NumericMultiply {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn not(&self, operand: IrExpr) -> IrExpr {
        assert_eq!(
            *operand.as_type(),
            Type::Bool,
            "BooleanNegation expects Bool operand, got: {}",
            operand
        );
        IrExpr::BooleanNegation {
            operand: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn neg(&self, operand: IrExpr) -> IrExpr {
        let operand_type = match operand.as_type() {
            Type::Int => NumericType::Int,
            Type::Float => NumericType::Float,
            t => panic!("Unsupported type for numeric negation: -{:?}", t),
        };
        IrExpr::NumericNegation {
            operand: Box::new(operand),
            operand_type,
            id: self.next_expr_id(),
        }
    }

    pub fn and(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        assert_eq!(
            *left.as_type(),
            Type::Bool,
            "BooleanLogicalAnd expects Bool operands, got: {}",
            left
        );
        assert_eq!(
            *right.as_type(),
            Type::Bool,
            "BooleanLogicalAnd expects Bool operands, got: {}",
            right
        );
        IrExpr::BooleanLogicalAnd {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn or(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        assert_eq!(
            *left.as_type(),
            Type::Bool,
            "BooleanLogicalOr expects Bool operands, got: {}",
            left
        );
        assert_eq!(
            *right.as_type(),
            Type::Bool,
            "BooleanLogicalOr expects Bool operands, got: {}",
            right
        );
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
        self.array_typed(element_type, elements)
    }

    /// An array literal with an explicit element type, allowing empty arrays.
    pub fn array_typed(&self, element_type: Arc<Type>, elements: Vec<IrExpr>) -> IrExpr {
        for element in &elements {
            assert_eq!(
                *element.as_type(),
                *element_type,
                "Array elements must all have the same type, got: {}",
                element
            );
        }

        IrExpr::ArrayLiteral {
            elements,
            kind: Arc::new(Type::Array(element_type)),
            id: self.next_expr_id(),
        }
    }

    pub fn int_to_string(&self, value: IrExpr) -> IrExpr {
        assert_eq!(
            *value.as_type(),
            Type::Int,
            "IntToString expects Int operand, got: {}",
            value
        );
        IrExpr::IntToString {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn float_to_int(&self, value: IrExpr) -> IrExpr {
        assert_eq!(
            *value.as_type(),
            Type::Float,
            "FloatToInt expects Float operand, got: {}",
            value
        );
        IrExpr::FloatToInt {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn int_to_float(&self, value: IrExpr) -> IrExpr {
        assert_eq!(
            *value.as_type(),
            Type::Int,
            "IntToFloat expects Int operand, got: {}",
            value
        );
        IrExpr::IntToFloat {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, IrExpr)>) -> IrExpr {
        let name = TypeName::new(record_name).unwrap();
        let record_fields = self.types.record_fields(record_name);

        for (field_name, value) in &fields {
            let declared_type = record_fields
                .iter()
                .find(|(f, _, _)| f.as_str() == *field_name)
                .map(|(_, t, _)| t)
                .unwrap_or_else(|| {
                    panic!(
                        "Field '{}' not found in record '{}'",
                        field_name, record_name
                    )
                });
            assert_eq!(
                value.as_type(),
                declared_type.as_ref(),
                "Field '{}' of record '{}' has mismatched type, got: {}",
                field_name,
                record_name,
                value
            );
        }

        let missing_fields: Vec<&str> = record_fields
            .iter()
            .filter(|(f, _, _)| !fields.iter().any(|(name, _)| *name == f.as_str()))
            .map(|(f, _, _)| f.as_str())
            .collect();
        assert!(
            missing_fields.is_empty(),
            "Record '{}' is missing fields: {:?}",
            record_name,
            missing_fields
        );

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

        let variant_fields = variants
            .iter()
            .find(|v| v.name.as_str() == variant_name)
            .map(|v| &v.fields)
            .unwrap_or_else(|| {
                let variant_names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
                panic!(
                    "Variant '{}' not found in enum '{}'. Available variants: {:?}",
                    variant_name, enum_name, variant_names
                )
            });

        for (field_name, value) in &field_values {
            let declared_type = variant_fields
                .iter()
                .find(|(f, _, _)| f.as_str() == *field_name)
                .map(|(_, t, _)| t)
                .unwrap_or_else(|| {
                    panic!(
                        "Field '{}' not found in variant '{}::{}'",
                        field_name, enum_name, variant_name
                    )
                });
            assert_eq!(
                value.as_type(),
                declared_type.as_ref(),
                "Field '{}' of variant '{}::{}' has mismatched type, got: {}",
                field_name,
                enum_name,
                variant_name,
                value
            );
        }

        let missing_fields: Vec<&str> = variant_fields
            .iter()
            .filter(|(f, _, _)| !field_values.iter().any(|(name, _)| *name == f.as_str()))
            .map(|(f, _, _)| f.as_str())
            .collect();
        assert!(
            missing_fields.is_empty(),
            "Enum variant '{}::{}' is missing fields: {:?}",
            enum_name,
            variant_name,
            missing_fields
        );

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
        self.none_typed(self.types.resolve(inner_type))
    }

    /// Create a None option literal from an already-resolved inner type.
    pub fn none_typed(&self, inner_type: Arc<Type>) -> IrExpr {
        IrExpr::OptionLiteral {
            value: None,
            kind: Arc::new(Type::Option(inner_type)),
            id: self.next_expr_id(),
        }
    }

    /// Create an exhaustive match expression over an enum value.
    pub fn enum_match_expr<F>(&self, subject: IrExpr, arms_fn: F) -> IrExpr
    where
        F: FnOnce(&mut EnumMatchExprArms<'_>),
    {
        let Some(ResolvedType::Enum { name, variants, .. }) =
            self.types.registry().resolve(subject.as_type())
        else {
            panic!("Match subject must be an enum type")
        };
        let (enum_name, variants) = (name.clone(), variants.to_vec());

        let mut arms = EnumMatchExprArms {
            builder: self,
            enum_name,
            variants,
            arms: Vec::new(),
            result_type: None,
        };
        arms_fn(&mut arms);
        assert_exhaustive(&arms.enum_name, &arms.variants, &arms.arms);
        let kind = arms
            .result_type
            .expect("enum_match_expr requires at least one arm");

        IrExpr::Match {
            match_: Match::Enum {
                subject: Box::new(subject),
                arms: arms.arms,
            },
            kind,
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
        assert_eq!(*subject.as_type(), Type::Bool, "{}", subject);
        assert_eq!(
            *true_body.as_type(),
            *false_body.as_type(),
            "Match arms must all have the same type, got: {} and {}",
            true_body,
            false_body
        );
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
        assert!(
            matches!(subject.as_type(), Type::Option(_)),
            "Match subject must be an option type, got: {}",
            subject
        );
        assert_eq!(
            *some_body.as_type(),
            *none_body.as_type(),
            "Match arms must all have the same type, got: {} and {}",
            some_body,
            none_body
        );
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
    pub fn option_match_expr_with_binding<F>(
        &self,
        subject: IrExpr,
        binding_name: &str,
        some_body_fn: F,
        none_body: IrExpr,
    ) -> IrExpr
    where
        F: FnOnce(&Self) -> IrExpr,
    {
        let inner_type = match subject.as_type() {
            Type::Option(inner) => inner.clone(),
            _ => panic!("Match subject must be an option type, got: {}", subject),
        };

        let binding = self.bind(binding_name);
        let some_body = some_body_fn(&self.scoped([(binding.clone(), inner_type)]));

        assert_eq!(
            *some_body.as_type(),
            *none_body.as_type(),
            "Match arms must all have the same type, got: {} and {}",
            some_body,
            none_body
        );
        let result_type = some_body.get_type();

        IrExpr::Match {
            match_: Match::Option {
                subject: Box::new(subject),
                some_arm_binding: Some(binding),
                some_arm_body: Box::new(some_body),
                none_arm_body: Box::new(none_body),
            },
            kind: result_type,
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

        let var = self.bind(var_name);
        let body = body_fn(&self.scoped([(var.clone(), value_type)]));

        let kind = body.get_type();

        IrExpr::Let {
            var,
            value: Box::new(value),
            body: Box::new(body),
            kind,
            id: self.next_expr_id(),
        }
    }

    pub fn string_concat(&self, left: IrExpr, right: IrExpr) -> IrExpr {
        assert_eq!(
            *left.as_type(),
            Type::String,
            "StringConcat expects String operands, got: {}",
            left
        );
        assert_eq!(
            *right.as_type(),
            Type::String,
            "StringConcat expects String operands, got: {}",
            right
        );
        IrExpr::StringConcat {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn join(&self, args: Vec<IrExpr>) -> IrExpr {
        for arg in &args {
            assert_eq!(
                *arg.as_type(),
                Type::String,
                "join expects String arguments, got: {}",
                arg
            );
        }
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
        assert_eq!(
            *value.as_type(),
            Type::String,
            "TwMerge expects String operand, got: {}",
            value
        );
        IrExpr::TwMerge {
            operand: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn array_length(&self, operand: IrExpr) -> IrExpr {
        assert!(
            matches!(operand.as_type(), Type::Array(_)),
            "ArrayLength expects Array operand, got: {}",
            operand
        );
        IrExpr::ArrayLength {
            array: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn array_is_empty(&self, operand: IrExpr) -> IrExpr {
        assert!(
            matches!(operand.as_type(), Type::Array(_)),
            "ArrayIsEmpty expects Array operand, got: {}",
            operand
        );
        IrExpr::ArrayIsEmpty {
            array: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn string_is_empty(&self, operand: IrExpr) -> IrExpr {
        assert_eq!(
            *operand.as_type(),
            Type::String,
            "StringIsEmpty expects String operand, got: {}",
            operand
        );
        IrExpr::StringIsEmpty {
            string: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn option_is_some(&self, operand: IrExpr) -> IrExpr {
        assert!(
            matches!(operand.as_type(), Type::Option(_)),
            "OptionIsSome expects Option operand, got: {}",
            operand
        );
        IrExpr::OptionIsSome {
            option: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn option_is_none(&self, operand: IrExpr) -> IrExpr {
        assert!(
            matches!(operand.as_type(), Type::Option(_)),
            "OptionIsNone expects Option operand, got: {}",
            operand
        );
        IrExpr::OptionIsNone {
            option: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    // Statement methods that auto-collect
    pub fn write(&mut self, s: &str) {
        self.statements.push(IrStatement::Write {
            id: self.next_statement_id(),
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
            id: self.next_statement_id(),
            expr,
            escape,
        });
    }

    pub fn write_expr_escaped(&mut self, expr: IrExpr) {
        self.write_expr(expr, true);
    }

    /// Sugar for a boolean match with an empty false arm.
    pub fn if_stmt<F>(&mut self, cond: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        assert_eq!(*cond.as_type(), Type::Bool, "{}", cond);
        let body = self.in_scope([], body_fn);
        self.statements.push(IrStatement::Match {
            id: self.next_statement_id(),
            match_: Match::Bool {
                subject: Box::new(cond),
                true_body: Box::new(body),
                false_body: Box::new(Vec::new()),
            },
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

        let var = self.bind(var);
        let body = self.in_scope([(var.clone(), element_type)], body_fn);

        self.statements.push(IrStatement::For {
            id: self.next_statement_id(),
            var: Some(var),
            source: IrForSource::Array(array),
            body,
        });
    }

    /// Create a for loop over an inclusive range (start..=end)
    /// Create a for loop over an inclusive range. Passing `None` for `var`
    /// leaves the loop variable unbound (underscore).
    pub fn for_range<F>(&mut self, var: Option<&str>, start: IrExpr, end: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        // Range loops iterate over integers
        assert_eq!(
            *start.as_type(),
            Type::Int,
            "Range bounds must be Int, got: {}",
            start
        );
        assert_eq!(
            *end.as_type(),
            Type::Int,
            "Range bounds must be Int, got: {}",
            end
        );
        let var = var.map(|v| self.bind(v));
        let bindings: Vec<_> = var
            .iter()
            .map(|v| (v.clone(), Arc::new(Type::Int)))
            .collect();
        let body = self.in_scope(bindings, body_fn);

        self.statements.push(IrStatement::For {
            id: self.next_statement_id(),
            var,
            source: IrForSource::RangeInclusive { start, end },
            body,
        });
    }

    pub fn let_stmt<F>(&mut self, var: &str, value: IrExpr, body_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let value_type = value.get_type();

        let var = self.bind(var);
        let body = self.in_scope([(var.clone(), value_type)], body_fn);

        self.statements.push(IrStatement::Let {
            id: self.next_statement_id(),
            var,
            value,
            body,
        });
    }

    pub fn let_fragment<F1, F2>(&mut self, var: &str, fragment_body_fn: F1, body_fn: F2)
    where
        F1: FnOnce(&mut Self),
        F2: FnOnce(&mut Self),
    {
        let fragment_body = self.in_scope([], fragment_body_fn);

        let var = self.bind(var);
        let body = self.in_scope([(var.clone(), Arc::new(Type::Fragment))], body_fn);

        self.statements.push(IrStatement::LetFragment {
            id: self.next_statement_id(),
            var,
            fragment_body,
            body,
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
        assert_eq!(*subject.as_type(), Type::Bool);

        let true_body = self.in_scope([], true_body_fn);
        let false_body = self.in_scope([], false_body_fn);

        self.statements.push(IrStatement::Match {
            id: self.next_statement_id(),
            match_: Match::Bool {
                subject: Box::new(subject),
                true_body: Box::new(true_body),
                false_body: Box::new(false_body),
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
        let inner_type = match subject.as_type() {
            Type::Option(inner) => inner.clone(),
            _ => panic!("Cannot match on non-option type"),
        };

        // Build some body with optional binding
        let some_arm_binding = binding_var.map(|var| self.bind(var));

        let some_arm_body = self.in_scope(
            some_arm_binding.clone().map(|var| (var, inner_type)),
            some_body_fn,
        );
        let none_arm_body = self.in_scope([], none_body_fn);

        self.statements.push(IrStatement::Match {
            id: self.next_statement_id(),
            match_: Match::Option {
                subject: Box::new(subject),
                some_arm_binding,
                some_arm_body: Box::new(some_arm_body),
                none_arm_body: Box::new(none_arm_body),
            },
        });
    }

    /// Create an exhaustive match statement over an enum value.
    pub fn enum_match_stmt<F>(&mut self, subject: IrExpr, arms_fn: F)
    where
        F: FnOnce(&mut EnumMatchStmtArms<'_>),
    {
        let Some(ResolvedType::Enum { name, variants, .. }) =
            self.types.registry().resolve(subject.as_type())
        else {
            panic!("Match subject must be an enum type")
        };
        let (enum_name, variants) = (name.clone(), variants.to_vec());

        let mut arms = EnumMatchStmtArms {
            builder: self,
            enum_name,
            variants,
            arms: Vec::new(),
        };
        arms_fn(&mut arms);
        assert_exhaustive(&arms.enum_name, &arms.variants, &arms.arms);
        let ir_arms = arms.arms;

        self.statements.push(IrStatement::Match {
            id: self.next_statement_id(),
            match_: Match::Enum {
                subject: Box::new(subject),
                arms: ir_arms,
            },
        });
    }

    /// Invoke a component by name with the given arguments.
    /// No validation is done against the invoked component's parameters.
    pub fn invoke_component(&mut self, name: &str, args: Vec<(&str, IrExpr)>) {
        let ir_args: Vec<IrArgument> = args
            .into_iter()
            .map(|(k, expr)| IrArgument {
                name: VarName::new(k).unwrap(),
                expr,
            })
            .collect();

        self.statements.push(IrStatement::ComponentInvocation {
            id: self.next_statement_id(),
            component_name: TypeName::new(name).unwrap(),
            args: ir_args,
        });
    }
}

/// Collects the arms of an [`IrBuilder::enum_match_expr`].
pub struct EnumMatchExprArms<'a> {
    builder: &'a IrBuilder,
    enum_name: TypeName,
    variants: Vec<EnumVariant>,
    arms: Vec<EnumMatchArm<IrExpr, IrVar>>,
    result_type: Option<Arc<Type>>,
}

impl EnumMatchExprArms<'_> {
    /// Add an arm for a variant without binding any fields.
    pub fn arm<F>(&mut self, variant: &str, body_fn: F)
    where
        F: FnOnce(&IrBuilder) -> IrExpr,
    {
        self.arm_bound(variant, [], body_fn);
    }

    /// Add an arm for a variant, binding the given (field_name,
    /// binding_name) pairs in the arm body's scope.
    pub fn arm_bound<'s, F>(
        &mut self,
        variant: &str,
        field_bindings: impl IntoIterator<Item = (&'s str, &'s str)>,
        body_fn: F,
    ) where
        F: FnOnce(&IrBuilder) -> IrExpr,
    {
        let (bindings, scoped_vars) = resolve_arm_bindings(
            self.builder,
            &self.enum_name,
            &self.variants,
            variant,
            field_bindings,
        );
        let body = body_fn(&self.builder.scoped(scoped_vars));
        match &self.result_type {
            Some(result_type) => assert_eq!(
                *body.as_type(),
                **result_type,
                "Match arms must all have the same type, got: {}",
                body
            ),
            None => self.result_type = Some(body.get_type()),
        }
        self.arms.push(EnumMatchArm {
            pattern: EnumPattern::Variant {
                enum_name: self.enum_name.clone(),
                variant_name: TypeName::new(variant).unwrap(),
            },
            bindings,
            body,
        });
    }
}

/// Collects the arms of an [`IrBuilder::enum_match_stmt`].
pub struct EnumMatchStmtArms<'a> {
    builder: &'a mut IrBuilder,
    enum_name: TypeName,
    variants: Vec<EnumVariant>,
    arms: Vec<EnumMatchArm<Vec<IrStatement>, IrVar>>,
}

impl EnumMatchStmtArms<'_> {
    /// Add an arm for a variant without binding any fields.
    pub fn arm<F>(&mut self, variant: &str, body_fn: F)
    where
        F: FnOnce(&mut IrBuilder),
    {
        self.arm_bound(variant, [], body_fn);
    }

    /// Add an arm for a variant, binding the given (field_name,
    /// binding_name) pairs in the arm body's scope.
    pub fn arm_bound<'s, F>(
        &mut self,
        variant: &str,
        field_bindings: impl IntoIterator<Item = (&'s str, &'s str)>,
        body_fn: F,
    ) where
        F: FnOnce(&mut IrBuilder),
    {
        let (bindings, scoped_vars) = resolve_arm_bindings(
            self.builder,
            &self.enum_name,
            &self.variants,
            variant,
            field_bindings,
        );
        let body = self.builder.in_scope(scoped_vars, body_fn);
        self.arms.push(EnumMatchArm {
            pattern: EnumPattern::Variant {
                enum_name: self.enum_name.clone(),
                variant_name: TypeName::new(variant).unwrap(),
            },
            bindings,
            body,
        });
    }
}

/// Resolve an arm's (field_name, binding_name) pairs against the variant's
/// declared fields, yielding the IR bindings and the variables to bring
/// into scope for the arm body.
fn resolve_arm_bindings<'s>(
    builder: &IrBuilder,
    enum_name: &TypeName,
    variants: &[EnumVariant],
    variant: &str,
    field_bindings: impl IntoIterator<Item = (&'s str, &'s str)>,
) -> (Vec<(FieldName, IrVar)>, Vec<(IrVar, Arc<Type>)>) {
    let variant_fields = variants
        .iter()
        .find(|v| v.name.as_str() == variant)
        .map(|v| &v.fields)
        .unwrap_or_else(|| {
            let variant_names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
            panic!(
                "Variant '{}' not found in enum '{}'. Available variants: {:?}",
                variant, enum_name, variant_names
            )
        });

    let mut bindings = Vec::new();
    let mut scoped_vars = Vec::new();
    for (field_name, binding_name) in field_bindings {
        let field_type = variant_fields
            .iter()
            .find(|(f, _, _)| f.as_str() == field_name)
            .map(|(_, t, _)| t.clone())
            .unwrap_or_else(|| {
                panic!(
                    "Field '{}' not found in variant '{}::{}'",
                    field_name, enum_name, variant
                )
            });
        let binding = builder.bind(binding_name);
        bindings.push((FieldName::new(field_name).unwrap(), binding.clone()));
        scoped_vars.push((binding, field_type));
    }
    (bindings, scoped_vars)
}

/// Panic unless every variant of the enum is covered by exactly one arm.
fn assert_exhaustive<B, V>(
    enum_name: &TypeName,
    variants: &[EnumVariant],
    arms: &[EnumMatchArm<B, V>],
) {
    for variant in variants {
        let count = arms
            .iter()
            .filter(|arm| {
                matches!(
                    &arm.pattern,
                    EnumPattern::Variant { variant_name, .. }
                        if variant_name.as_str() == variant.name.as_str()
                )
            })
            .count();
        assert!(
            count > 0,
            "Match on enum '{}' is missing an arm for variant '{}'",
            enum_name,
            variant.name
        );
        assert!(
            count == 1,
            "Match on enum '{}' has {} arms for variant '{}'",
            enum_name,
            count,
            variant.name
        );
    }
}
