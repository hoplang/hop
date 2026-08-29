use crate::document::CheapString;
use crate::expr::Type;
use crate::expr::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::expr::typing::r#type::{ComparableType, EnumVariant, EquatableType, NumericType};
use crate::expr::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::expr::typing::type_registry_builder::{TestTypes, TypeRegistryBuilder};
use crate::ir::expr_id::{ExprId, ExprIdCounter};
use crate::ir::ir_var::IrVar;
use crate::ir::pure_module::{
    PureArgument, PureExpr, PureForSource, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};
use crate::ir::var_id::VarIdCounter;
use crate::ir::writer_module::{WriterEnumDeclaration, WriterParameter, WriterRecordDeclaration};
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// Declares the record and enum types of a module under construction.
pub struct PureModuleBuilder {
    types_builder: TypeRegistryBuilder,
}

impl PureModuleBuilder {
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

    /// Freeze the declared types, enabling view and function bodies.
    pub fn freeze(self) -> PureModuleBodiesBuilder {
        PureModuleBodiesBuilder {
            types: Rc::new(self.types_builder.build()),
            expr_ids: Rc::new(RefCell::new(ExprIdCounter::new())),
            var_ids: Rc::new(RefCell::new(VarIdCounter::new())),
            pages: Vec::new(),
            functions: Vec::new(),
            callees: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn view_no_params<F>(self, name: &str, body_fn: F) -> PureModuleBodiesBuilder
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
    {
        self.freeze().view_no_params(name, body_fn)
    }

    pub fn view<'a, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        body_fn: F,
    ) -> PureModuleBodiesBuilder
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
    {
        self.freeze().view(name, params, body_fn)
    }

    pub fn function<'a, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        return_type: &str,
        body_fn: F,
    ) -> PureModuleBodiesBuilder
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
    {
        self.freeze().function(name, params, return_type, body_fn)
    }
}

impl Default for PureModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl From<PureModuleBuilder> for PureModuleBodiesBuilder {
    fn from(builder: PureModuleBuilder) -> Self {
        builder.freeze()
    }
}

/// A function's parameters and return type, keyed by name so call sites can
/// look up the callee's return type.
type FunctionSignature = (Vec<WriterParameter>, Arc<Type>);

/// Collects view and function bodies against a frozen set of types.
pub struct PureModuleBodiesBuilder {
    types: Rc<TestTypes>,
    expr_ids: Rc<RefCell<ExprIdCounter>>,
    var_ids: Rc<RefCell<VarIdCounter>>,
    pages: Vec<PurePageDeclaration>,
    functions: Vec<PureFunctionDeclaration>,
    callees: Rc<RefCell<HashMap<String, FunctionSignature>>>,
}

impl PureModuleBodiesBuilder {
    pub fn view_no_params<F>(self, name: &str, body_fn: F) -> Self
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
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
        F: FnOnce(&PureBuilder) -> PureExpr,
    {
        let (parameters, body) = self.declaration(params, Arc::new(Type::Fragment), body_fn);
        self.pages.push(PurePageDeclaration {
            name: TypeName::new(name).expect("Test view name should be valid"),
            parameters,
            body,
        });
        self
    }

    pub fn function<'a, F>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        return_type: &str,
        body_fn: F,
    ) -> Self
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
    {
        let return_type = self.types.resolve(return_type);
        let (parameters, body) = self.declaration(params, return_type.clone(), body_fn);
        self.callees
            .borrow_mut()
            .insert(name.to_string(), (parameters.clone(), return_type.clone()));
        self.functions.push(PureFunctionDeclaration {
            name: FunctionName::new(name).expect("Test function name should be valid"),
            parameters,
            return_type,
            body,
        });
        self
    }

    fn declaration<'a, F>(
        &self,
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        expected_type: Arc<Type>,
        body_fn: F,
    ) -> (Vec<WriterParameter>, PureExpr)
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
    {
        let parameters: Vec<WriterParameter> = params
            .into_iter()
            .map(|(name, typ)| {
                let id = self.var_ids.borrow_mut().next();
                WriterParameter {
                    name: VarName::new(name).unwrap(),
                    var: IrVar::new(id),
                    typ: self.types.resolve(typ),
                }
            })
            .collect();
        let vars = parameters
            .iter()
            .map(|p| (p.name.as_str().to_string(), p.var, p.typ.clone()))
            .collect();
        let builder = PureBuilder {
            var_stack: vars,
            types: self.types.clone(),
            expr_ids: self.expr_ids.clone(),
            var_ids: self.var_ids.clone(),
            callees: self.callees.clone(),
        };
        let body = body_fn(&builder);
        assert_eq!(
            *body.as_type(),
            *expected_type,
            "Declaration body must be of type {:?}, got: {}",
            expected_type,
            body
        );
        (parameters, body)
    }

    pub fn build(self) -> PureModule {
        self.build_with_registry().0
    }

    pub fn build_with_registry(self) -> (PureModule, TypeRegistry) {
        let mut record_declarations = Vec::new();
        let mut enum_declarations = Vec::new();
        for (name, resolved) in self.types.declared_types() {
            match resolved {
                ResolvedType::Record { fields, .. } => {
                    record_declarations.push(WriterRecordDeclaration {
                        name: name.clone(),
                        fields: fields.to_vec(),
                    });
                }
                ResolvedType::Enum { variants, .. } => {
                    enum_declarations.push(WriterEnumDeclaration {
                        name: name.clone(),
                        variants: variants.to_vec(),
                    });
                }
                _ => unreachable!("only records and enums can be declared"),
            }
        }
        let module = PureModule {
            pages: self.pages,
            functions: self.functions,
            records: record_declarations,
            enums: enum_declarations,
            expr_ids: *self.expr_ids.borrow(),
            var_ids: *self.var_ids.borrow(),
        };
        (module, self.types.registry().clone())
    }
}

type ScopedVar = (String, IrVar, Arc<Type>);

pub struct PureBuilder {
    var_stack: Vec<ScopedVar>,
    types: Rc<TestTypes>,
    expr_ids: Rc<RefCell<ExprIdCounter>>,
    var_ids: Rc<RefCell<VarIdCounter>>,
    callees: Rc<RefCell<HashMap<String, FunctionSignature>>>,
}

impl PureBuilder {
    fn next_expr_id(&self) -> ExprId {
        self.expr_ids.borrow_mut().next()
    }

    fn bind(&self) -> IrVar {
        IrVar::new(self.var_ids.borrow_mut().next())
    }

    fn scoped(&self, bindings: impl IntoIterator<Item = ScopedVar>) -> Self {
        let mut var_stack = self.var_stack.clone();
        var_stack.extend(bindings);
        Self {
            var_stack,
            types: self.types.clone(),
            expr_ids: self.expr_ids.clone(),
            var_ids: self.var_ids.clone(),
            callees: self.callees.clone(),
        }
    }

    /// In-scope variables, innermost last.
    pub fn vars(&self) -> &[ScopedVar] {
        &self.var_stack
    }

    /// Resolve a source-syntax type string, e.g. `Array[Int]`.
    pub fn resolve_type(&self, type_str: &str) -> Arc<Type> {
        self.types.resolve(type_str)
    }

    pub fn str(&self, s: &str) -> PureExpr {
        PureExpr::StringLiteral {
            value: CheapString::new(s.to_string()),
            id: self.next_expr_id(),
        }
    }

    pub fn int(&self, n: i32) -> PureExpr {
        PureExpr::IntLiteral {
            value: n,
            id: self.next_expr_id(),
        }
    }

    pub fn bool(&self, b: bool) -> PureExpr {
        PureExpr::BooleanLiteral {
            value: b,
            id: self.next_expr_id(),
        }
    }

    pub fn float(&self, f: f64) -> PureExpr {
        PureExpr::FloatLiteral {
            value: f,
            id: self.next_expr_id(),
        }
    }

    pub fn var(&self, name: &str) -> PureExpr {
        let (_, value, kind) = self
            .var_stack
            .iter()
            .rev()
            .find(|(var_name, _, _)| var_name == name)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Variable '{}' not found in scope. Available variables: {:?}",
                    name,
                    self.var_stack
                        .iter()
                        .map(|(v, _, _)| v.as_str())
                        .collect::<Vec<_>>()
                )
            });

        PureExpr::VariableReference {
            value,
            kind,
            id: self.next_expr_id(),
        }
    }

    pub fn eq(&self, left: PureExpr, right: PureExpr) -> PureExpr {
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
        PureExpr::Equals {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn lt(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => ComparableType::Int,
            (Type::Float, Type::Float) => ComparableType::Float,
            (l, r) => panic!(
                "Unsupported types for less-than comparison: {:?} < {:?}",
                l, r
            ),
        };
        PureExpr::LessThan {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn lte(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => ComparableType::Int,
            (Type::Float, Type::Float) => ComparableType::Float,
            (l, r) => panic!(
                "Unsupported types for less-than-or-equal comparison: {:?} <= {:?}",
                l, r
            ),
        };
        PureExpr::LessThanOrEqual {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn add(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for addition: {:?} + {:?}", l, r),
        };
        PureExpr::NumericAdd {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn sub(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for subtraction: {:?} - {:?}", l, r),
        };
        PureExpr::NumericSubtract {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn mul(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.as_type(), right.as_type()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for multiplication: {:?} * {:?}", l, r),
        };
        PureExpr::NumericMultiply {
            left: Box::new(left),
            right: Box::new(right),
            operand_types,
            id: self.next_expr_id(),
        }
    }

    pub fn not(&self, operand: PureExpr) -> PureExpr {
        assert_eq!(
            *operand.as_type(),
            Type::Bool,
            "BooleanNegation expects Bool operand, got: {}",
            operand
        );
        PureExpr::BooleanNegation {
            operand: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn neg(&self, operand: PureExpr) -> PureExpr {
        let operand_type = match operand.as_type() {
            Type::Int => NumericType::Int,
            Type::Float => NumericType::Float,
            t => panic!("Unsupported type for numeric negation: -{:?}", t),
        };
        PureExpr::NumericNegation {
            operand: Box::new(operand),
            operand_type,
            id: self.next_expr_id(),
        }
    }

    pub fn and(&self, left: PureExpr, right: PureExpr) -> PureExpr {
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
        PureExpr::BooleanLogicalAnd {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn or(&self, left: PureExpr, right: PureExpr) -> PureExpr {
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
        PureExpr::BooleanLogicalOr {
            left: Box::new(left),
            right: Box::new(right),
            id: self.next_expr_id(),
        }
    }

    pub fn array(&self, elements: Vec<PureExpr>) -> PureExpr {
        let element_type = elements
            .first()
            .map(|first| first.get_type())
            .expect("Cannot create empty array literal in test builder");
        self.array_typed(element_type, elements)
    }

    pub fn array_typed(&self, element_type: Arc<Type>, elements: Vec<PureExpr>) -> PureExpr {
        for element in &elements {
            assert_eq!(
                *element.as_type(),
                *element_type,
                "Array elements must all have the same type, got: {}",
                element
            );
        }

        PureExpr::ArrayLiteral {
            elements,
            kind: Arc::new(Type::Array(element_type)),
            id: self.next_expr_id(),
        }
    }

    pub fn int_to_string(&self, value: PureExpr) -> PureExpr {
        assert_eq!(
            *value.as_type(),
            Type::Int,
            "IntToString expects Int operand, got: {}",
            value
        );
        PureExpr::IntToString {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn float_to_int(&self, value: PureExpr) -> PureExpr {
        assert_eq!(
            *value.as_type(),
            Type::Float,
            "FloatToInt expects Float operand, got: {}",
            value
        );
        PureExpr::FloatToInt {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn int_to_float(&self, value: PureExpr) -> PureExpr {
        assert_eq!(
            *value.as_type(),
            Type::Int,
            "IntToFloat expects Int operand, got: {}",
            value
        );
        PureExpr::IntToFloat {
            value: Box::new(value),
            id: self.next_expr_id(),
        }
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, PureExpr)>) -> PureExpr {
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

        PureExpr::RecordLiteral {
            record_name: name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (FieldName::new(k).unwrap(), v))
                .collect(),
            kind: self.types.named(record_name),
            id: self.next_expr_id(),
        }
    }

    pub fn enum_variant(&self, enum_name: &str, variant_name: &str) -> PureExpr {
        self.enum_variant_with_fields(enum_name, variant_name, vec![])
    }

    pub fn enum_variant_with_fields(
        &self,
        enum_name: &str,
        variant_name: &str,
        field_values: Vec<(&str, PureExpr)>,
    ) -> PureExpr {
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

        PureExpr::EnumLiteral {
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

    pub fn some(&self, inner: PureExpr) -> PureExpr {
        let inner_type = inner.get_type();
        PureExpr::OptionLiteral {
            value: Some(Box::new(inner)),
            kind: Arc::new(Type::Option(inner_type)),
            id: self.next_expr_id(),
        }
    }

    pub fn none(&self, inner_type: &str) -> PureExpr {
        self.none_typed(self.types.resolve(inner_type))
    }

    pub fn none_typed(&self, inner_type: Arc<Type>) -> PureExpr {
        PureExpr::OptionLiteral {
            value: None,
            kind: Arc::new(Type::Option(inner_type)),
            id: self.next_expr_id(),
        }
    }

    pub fn enum_match_expr<F>(&self, subject: PureExpr, arms_fn: F) -> PureExpr
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

        PureExpr::Match {
            match_: Match::Enum {
                subject: Box::new(subject),
                arms: arms.arms,
            },
            kind,
            id: self.next_expr_id(),
        }
    }

    pub fn bool_match_expr(
        &self,
        subject: PureExpr,
        true_body: PureExpr,
        false_body: PureExpr,
    ) -> PureExpr {
        assert_eq!(*subject.as_type(), Type::Bool, "{}", subject);
        assert_eq!(
            *true_body.as_type(),
            *false_body.as_type(),
            "Match arms must all have the same type, got: {} and {}",
            true_body,
            false_body
        );
        let result_type = true_body.get_type();

        PureExpr::Match {
            match_: Match::Bool {
                subject: Box::new(subject),
                true_body: Box::new(true_body),
                false_body: Box::new(false_body),
            },
            kind: result_type,
            id: self.next_expr_id(),
        }
    }

    pub fn option_match_expr(
        &self,
        subject: PureExpr,
        some_body: PureExpr,
        none_body: PureExpr,
    ) -> PureExpr {
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

        PureExpr::Match {
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

    pub fn option_match_expr_with_binding<F>(
        &self,
        subject: PureExpr,
        binding_name: &str,
        some_body_fn: F,
        none_body: PureExpr,
    ) -> PureExpr
    where
        F: FnOnce(&Self) -> PureExpr,
    {
        let inner_type = match subject.as_type() {
            Type::Option(inner) => inner.clone(),
            _ => panic!("Match subject must be an option type, got: {}", subject),
        };

        let binding = self.bind();
        let some_body =
            some_body_fn(&self.scoped([(binding_name.to_string(), binding, inner_type)]));

        assert_eq!(
            *some_body.as_type(),
            *none_body.as_type(),
            "Match arms must all have the same type, got: {} and {}",
            some_body,
            none_body
        );
        let result_type = some_body.get_type();

        PureExpr::Match {
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

    pub fn field_access(&self, object: PureExpr, field_str: &str) -> PureExpr {
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

        PureExpr::FieldAccess {
            record: Box::new(object),
            field: field_name,
            kind: field_type,
            id: self.next_expr_id(),
        }
    }

    pub fn let_expr<F>(&self, var_name: &str, value: PureExpr, body_fn: F) -> PureExpr
    where
        F: FnOnce(&Self) -> PureExpr,
    {
        let value_type = value.get_type();

        let var = self.bind();
        let body = body_fn(&self.scoped([(var_name.to_string(), var, value_type)]));

        let kind = body.get_type();

        PureExpr::Let {
            var,
            value: Box::new(value),
            body: Box::new(body),
            kind,
            id: self.next_expr_id(),
        }
    }

    pub fn string_concat(&self, parts: Vec<PureExpr>) -> PureExpr {
        for part in &parts {
            assert_eq!(
                *part.as_type(),
                Type::String,
                "StringConcat expects String parts, got: {}",
                part
            );
        }
        PureExpr::StringConcat {
            parts,
            id: self.next_expr_id(),
        }
    }

    pub fn join(&self, args: Vec<PureExpr>) -> PureExpr {
        for arg in &args {
            assert_eq!(
                *arg.as_type(),
                Type::String,
                "join expects String arguments, got: {}",
                arg
            );
        }
        let separator = CheapString::new(" ".to_string());
        let mut parts = Vec::with_capacity((args.len() * 2).saturating_sub(1));
        for (index, arg) in args.into_iter().enumerate() {
            if index > 0 {
                parts.push(PureExpr::StringLiteral {
                    value: separator.clone(),
                    id: self.next_expr_id(),
                });
            }
            parts.push(arg);
        }
        PureExpr::StringConcat {
            parts,
            id: self.next_expr_id(),
        }
    }

    pub fn array_length(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.as_type(), Type::Array(_)),
            "ArrayLength expects Array operand, got: {}",
            operand
        );
        PureExpr::ArrayLength {
            array: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn array_is_empty(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.as_type(), Type::Array(_)),
            "ArrayIsEmpty expects Array operand, got: {}",
            operand
        );
        PureExpr::ArrayIsEmpty {
            array: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn string_is_empty(&self, operand: PureExpr) -> PureExpr {
        assert_eq!(
            *operand.as_type(),
            Type::String,
            "StringIsEmpty expects String operand, got: {}",
            operand
        );
        PureExpr::StringIsEmpty {
            string: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn option_is_some(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.as_type(), Type::Option(_)),
            "OptionIsSome expects Option operand, got: {}",
            operand
        );
        PureExpr::OptionIsSome {
            option: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    pub fn option_is_none(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.as_type(), Type::Option(_)),
            "OptionIsNone expects Option operand, got: {}",
            operand
        );
        PureExpr::OptionIsNone {
            option: Box::new(operand),
            id: self.next_expr_id(),
        }
    }

    /// A trusted, already-escaped HTML atom.
    pub fn raw(&self, content: &str) -> PureExpr {
        PureExpr::FragmentRaw {
            content: content.to_string(),
            id: self.next_expr_id(),
        }
    }

    pub fn escape(&self, expr: PureExpr) -> PureExpr {
        assert_eq!(
            *expr.as_type(),
            Type::String,
            "FragmentEscape expects String operand, got: {}",
            expr
        );
        PureExpr::FragmentEscape {
            expr: Box::new(expr),
            id: self.next_expr_id(),
        }
    }

    pub fn concat(&self, parts: Vec<PureExpr>) -> PureExpr {
        for part in &parts {
            assert_eq!(
                *part.as_type(),
                Type::Fragment,
                "FragmentConcat expects Fragment parts, got: {}",
                part
            );
        }
        PureExpr::FragmentConcat {
            parts,
            id: self.next_expr_id(),
        }
    }

    pub fn fragment_for<F>(&self, var: Option<&str>, array: PureExpr, body_fn: F) -> PureExpr
    where
        F: FnOnce(&Self) -> PureExpr,
    {
        let element_type = match array.as_type() {
            Type::Array(elem_type) => elem_type.clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        let name = var;
        let var = name.map(|_| self.bind());
        let bindings: Vec<_> = name
            .into_iter()
            .zip(var)
            .map(|(name, v)| (name.to_string(), v, element_type.clone()))
            .collect();
        let body = body_fn(&self.scoped(bindings));
        assert_eq!(
            *body.as_type(),
            Type::Fragment,
            "FragmentFor expects a Fragment body, got: {}",
            body
        );

        PureExpr::FragmentFor {
            var,
            source: Box::new(PureForSource::Array(array)),
            body: Box::new(body),
            id: self.next_expr_id(),
        }
    }

    pub fn fragment_for_range<F>(
        &self,
        var: Option<&str>,
        start: PureExpr,
        end: PureExpr,
        body_fn: F,
    ) -> PureExpr
    where
        F: FnOnce(&Self) -> PureExpr,
    {
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

        let name = var;
        let var = name.map(|_| self.bind());
        let bindings: Vec<_> = name
            .into_iter()
            .zip(var)
            .map(|(name, v)| (name.to_string(), v, Arc::new(Type::Int)))
            .collect();
        let body = body_fn(&self.scoped(bindings));
        assert_eq!(
            *body.as_type(),
            Type::Fragment,
            "FragmentFor expects a Fragment body, got: {}",
            body
        );

        PureExpr::FragmentFor {
            var,
            source: Box::new(PureForSource::RangeInclusive { start, end }),
            body: Box::new(body),
            id: self.next_expr_id(),
        }
    }

    pub fn call(&self, name: &str, args: Vec<(&str, PureExpr)>) -> PureExpr {
        let (_, return_type) = self
            .callees
            .borrow()
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("Call to undeclared function '{}'", name));

        let pure_args: Vec<PureArgument> = args
            .into_iter()
            .map(|(k, expr)| PureArgument {
                name: VarName::new(k).unwrap(),
                expr,
            })
            .collect();

        PureExpr::FunctionCall {
            function_name: FunctionName::new(name).unwrap(),
            args: pure_args,
            kind: return_type,
            id: self.next_expr_id(),
        }
    }
}

pub struct EnumMatchExprArms<'a> {
    builder: &'a PureBuilder,
    enum_name: TypeName,
    variants: Vec<EnumVariant>,
    arms: Vec<EnumMatchArm<PureExpr, IrVar>>,
    result_type: Option<Arc<Type>>,
}

impl EnumMatchExprArms<'_> {
    /// Add an arm for a variant without binding any fields.
    pub fn arm<F>(&mut self, variant: &str, body_fn: F)
    where
        F: FnOnce(&PureBuilder) -> PureExpr,
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
        F: FnOnce(&PureBuilder) -> PureExpr,
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

fn resolve_arm_bindings<'s>(
    builder: &PureBuilder,
    enum_name: &TypeName,
    variants: &[EnumVariant],
    variant: &str,
    field_bindings: impl IntoIterator<Item = (&'s str, &'s str)>,
) -> (Vec<(FieldName, IrVar)>, Vec<ScopedVar>) {
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
        let binding = builder.bind();
        bindings.push((FieldName::new(field_name).unwrap(), binding));
        scoped_vars.push((binding_name.to_string(), binding, field_type));
    }
    (bindings, scoped_vars)
}

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
