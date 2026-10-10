use crate::document::CheapString;
use crate::hop::typing::{
    ComparableType, EnumVariant, EquatableType, NumericType, ResolvedType, TestTypes, Type,
    TypeRegistry, TypeRegistryBuilder,
};
use crate::html::HtmlElementKind;
use crate::ir::binder_id::BinderId;
use crate::ir::binder_id::BinderIdCounter;
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::ir_parameter::{IrParameter, PageParameter};
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::ir::pure_module::{
    PureAttribute, PureExpr, PureForSource, PureFunctionDeclaration, PureModule,
    PurePageDeclaration,
};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

    /// Freeze the declared types, enabling page and function bodies.
    pub fn freeze<'a>(self) -> PureModuleBodiesBuilder<'a> {
        PureModuleBodiesBuilder {
            types: Rc::new(self.types_builder.build()),
            function_ids: FunctionIdCounter::new(),
            callees: HashMap::new(),
            deferred: Vec::new(),
        }
    }

    pub fn page_no_params<'a, F>(self, name: &str, body_fn: F) -> PureModuleBodiesBuilder<'a>
    where
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        self.freeze().page_no_params(name, body_fn)
    }

    pub fn page<'p, 'a, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        body_fn: F,
    ) -> PureModuleBodiesBuilder<'a>
    where
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        self.freeze().page(name, params, body_fn)
    }

    pub fn function<'p, 'a, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        return_type: &str,
        body_fn: F,
    ) -> PureModuleBodiesBuilder<'a>
    where
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        self.freeze().function(name, params, return_type, body_fn)
    }
}

impl Default for PureModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> From<PureModuleBuilder> for PureModuleBodiesBuilder<'a> {
    fn from(builder: PureModuleBuilder) -> Self {
        builder.freeze()
    }
}

/// A declared function, the names of its parameters in order, and its
/// return type.
type FunctionSignature = (IrFunction, Vec<AttributeName>, Type);

type BodyFn<'a> = Box<dyn FnOnce(&PureBuilder) -> PureExpr + 'a>;

enum DeferredDeclaration<'a> {
    Page {
        name: TypeName,
        head_fn: Option<BodyFn<'a>>,
    },
    Function {
        function: IrFunction,
        return_type: Type,
    },
}

struct Deferred<'a> {
    declaration: DeferredDeclaration<'a>,
    parameters: Vec<(AttributeName, Type)>,
    body_fn: BodyFn<'a>,
}

/// Collects page and function bodies against a frozen set of types.
pub struct PureModuleBodiesBuilder<'a> {
    types: Rc<TestTypes>,
    function_ids: FunctionIdCounter,
    callees: HashMap<String, FunctionSignature>,
    deferred: Vec<Deferred<'a>>,
}

impl<'a> PureModuleBodiesBuilder<'a> {
    pub fn page_no_params<F>(self, name: &str, body_fn: F) -> Self
    where
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        self.page(name, [], body_fn)
    }

    pub fn page<'p, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        body_fn: F,
    ) -> Self
    where
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        self.push_page(name, params, None, Box::new(body_fn))
    }

    pub fn page_with_head<'p, H, F>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        head_fn: H,
        body_fn: F,
    ) -> Self
    where
        H: FnOnce(&PureBuilder) -> PureExpr + 'a,
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        self.push_page(name, params, Some(Box::new(head_fn)), Box::new(body_fn))
    }

    fn push_page<'p>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        head_fn: Option<BodyFn<'a>>,
        body_fn: BodyFn<'a>,
    ) -> Self {
        let parameters = params
            .into_iter()
            .map(|(name, typ)| (AttributeName::parse(name).unwrap(), self.types.resolve(typ)))
            .collect();
        self.deferred.push(Deferred {
            declaration: DeferredDeclaration::Page {
                name: TypeName::parse(name).expect("Test page name should be valid"),
                head_fn,
            },
            parameters,
            body_fn,
        });
        self
    }

    pub fn function<'p, F>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        return_type: &str,
        body_fn: F,
    ) -> Self
    where
        F: FnOnce(&PureBuilder) -> PureExpr + 'a,
    {
        let function = IrFunction::new(
            self.function_ids.next(),
            FunctionName::parse(name).expect("Test function name should be valid"),
        );
        let return_type = self.types.resolve(return_type);
        let parameters: Vec<(AttributeName, Type)> = params
            .into_iter()
            .map(|(name, typ)| (AttributeName::parse(name).unwrap(), self.types.resolve(typ)))
            .collect();
        self.callees.insert(
            name.to_string(),
            (
                function.clone(),
                parameters.iter().map(|(name, _)| name.clone()).collect(),
                return_type.clone(),
            ),
        );
        self.deferred.push(Deferred {
            declaration: DeferredDeclaration::Function {
                function,
                return_type,
            },
            parameters,
            body_fn: Box::new(body_fn),
        });
        self
    }

    pub fn build(self) -> PureModule {
        self.build_with_registry().0
    }

    pub fn build_with_registry(mut self) -> (PureModule, TypeRegistry) {
        let binder_ids = Rc::new(RefCell::new(BinderIdCounter::new()));
        let callees = Rc::new(self.callees);
        // Bind the parameters afresh and build a body of the expected type.
        let build_body = |parameters: &[(AttributeName, Type)],
                          body_fn: BodyFn<'a>,
                          expected_type: &Type|
         -> (Vec<IrParameter>, PureExpr) {
            let parameters: Vec<IrParameter> = parameters
                .iter()
                .map(|(name, typ)| IrParameter {
                    name: name.clone(),
                    var: binder_ids.borrow_mut().next(),
                    typ: typ.clone(),
                })
                .collect();
            let builder = PureBuilder {
                var_stack: parameters
                    .iter()
                    .map(|p| (p.name.as_str().to_string(), p.var, p.typ.clone()))
                    .collect(),
                types: self.types.clone(),
                binder_ids: binder_ids.clone(),
                callees: callees.clone(),
            };
            let body = body_fn(&builder);
            assert_eq!(
                &body.typ(),
                expected_type,
                "Declaration body must be of type {:?}, got: {}",
                expected_type,
                body
            );
            (parameters, body)
        };
        let mut pages = Vec::new();
        let mut functions = Vec::new();
        // A page's head and body are functions of their own, numbered after
        // the declared functions and declared after them.
        let mut page_functions = Vec::new();
        for deferred in self.deferred {
            match deferred.declaration {
                DeferredDeclaration::Page { name, head_fn } => {
                    let mut declare = |body_fn: BodyFn<'a>, function: IrFunction| {
                        let (parameters, body) =
                            build_body(&deferred.parameters, body_fn, &Type::Html);
                        page_functions.push(PureFunctionDeclaration {
                            function: function.clone(),
                            parameters,
                            return_type: Type::Html,
                            body,
                        });
                        function
                    };
                    let head = head_fn.map(|head_fn| {
                        declare(head_fn, IrFunction::page_head(self.function_ids.next()))
                    });
                    let body = declare(
                        deferred.body_fn,
                        IrFunction::page_body(self.function_ids.next()),
                    );
                    pages.push(PurePageDeclaration {
                        name,
                        parameters: deferred
                            .parameters
                            .into_iter()
                            .map(|(name, typ)| PageParameter { name, typ })
                            .collect(),
                        head,
                        body,
                    });
                }
                DeferredDeclaration::Function {
                    function,
                    return_type,
                } => {
                    let (parameters, body) =
                        build_body(&deferred.parameters, deferred.body_fn, &return_type);
                    functions.push(PureFunctionDeclaration {
                        function,
                        parameters,
                        return_type,
                        body,
                    });
                }
            }
        }
        functions.extend(page_functions);
        let module = PureModule {
            pages,
            functions,
            binder_ids: *binder_ids.borrow(),
        };
        (module, self.types.registry().clone())
    }
}

type ScopedVar = (String, BinderId, Type);

pub struct PureBuilder {
    var_stack: Vec<ScopedVar>,
    types: Rc<TestTypes>,
    binder_ids: Rc<RefCell<BinderIdCounter>>,
    callees: Rc<HashMap<String, FunctionSignature>>,
}

impl PureBuilder {
    fn bind(&self) -> BinderId {
        self.binder_ids.borrow_mut().next()
    }

    fn scoped(&self, bindings: impl IntoIterator<Item = ScopedVar>) -> Self {
        let mut var_stack = self.var_stack.clone();
        var_stack.extend(bindings);
        Self {
            var_stack,
            types: self.types.clone(),
            binder_ids: self.binder_ids.clone(),
            callees: self.callees.clone(),
        }
    }

    /// In-scope variables, innermost last.
    pub fn vars(&self) -> &[ScopedVar] {
        &self.var_stack
    }

    /// Resolve a source-syntax type string, e.g. `Array[Int]`.
    pub fn resolve_type(&self, type_str: &str) -> Type {
        self.types.resolve(type_str)
    }

    pub fn str(&self, s: &str) -> PureExpr {
        PureExpr::StringLiteral {
            value: CheapString::new(s.to_string()),
        }
    }

    pub fn int(&self, n: i32) -> PureExpr {
        PureExpr::IntLiteral { value: n }
    }

    pub fn bool(&self, b: bool) -> PureExpr {
        PureExpr::BoolLiteral { value: b }
    }

    pub fn float(&self, f: f64) -> PureExpr {
        PureExpr::FloatLiteral { value: f }
    }

    pub fn var(&self, name: &str) -> PureExpr {
        let (_, value, typ) = self
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

        PureExpr::VariableReference { value, typ }
    }

    pub fn eq(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.typ(), right.typ()) {
            (Type::Bool, Type::Bool) => EquatableType::Bool,
            (Type::String, Type::String) => EquatableType::String,
            (Type::Int, Type::Int) => EquatableType::Int,
            (Type::Float, Type::Float) => EquatableType::Float,
            (l, r) => panic!(
                "Unsupported types for equality comparison: {:?} == {:?}",
                l, r
            ),
        };
        PureExpr::Binary {
            op: IrBinaryOp::Equals(operand_types),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn lt(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.typ(), right.typ()) {
            (Type::Int, Type::Int) => ComparableType::Int,
            (Type::Float, Type::Float) => ComparableType::Float,
            (l, r) => panic!(
                "Unsupported types for less-than comparison: {:?} < {:?}",
                l, r
            ),
        };
        PureExpr::Binary {
            op: IrBinaryOp::LessThan(operand_types),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn lte(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.typ(), right.typ()) {
            (Type::Int, Type::Int) => ComparableType::Int,
            (Type::Float, Type::Float) => ComparableType::Float,
            (l, r) => panic!(
                "Unsupported types for less-than-or-equal comparison: {:?} <= {:?}",
                l, r
            ),
        };
        PureExpr::Binary {
            op: IrBinaryOp::LessThanOrEqual(operand_types),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn add(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.typ(), right.typ()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for addition: {:?} + {:?}", l, r),
        };
        PureExpr::Binary {
            op: IrBinaryOp::NumericAdd(operand_types),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn sub(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.typ(), right.typ()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for subtraction: {:?} - {:?}", l, r),
        };
        PureExpr::Binary {
            op: IrBinaryOp::NumericSubtract(operand_types),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn mul(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        let operand_types = match (left.typ(), right.typ()) {
            (Type::Int, Type::Int) => NumericType::Int,
            (Type::Float, Type::Float) => NumericType::Float,
            (l, r) => panic!("Unsupported types for multiplication: {:?} * {:?}", l, r),
        };
        PureExpr::Binary {
            op: IrBinaryOp::NumericMultiply(operand_types),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn not(&self, operand: PureExpr) -> PureExpr {
        assert_eq!(
            operand.typ(),
            Type::Bool,
            "BoolNegation expects Bool operand, got: {}",
            operand
        );
        PureExpr::Unary {
            op: IrUnaryOp::BoolNegation,
            operand: Box::new(operand),
        }
    }

    pub fn neg(&self, operand: PureExpr) -> PureExpr {
        let operand_type = match operand.typ() {
            Type::Int => NumericType::Int,
            Type::Float => NumericType::Float,
            t => panic!("Unsupported type for numeric negation: -{:?}", t),
        };
        PureExpr::Unary {
            op: IrUnaryOp::NumericNegation(operand_type),
            operand: Box::new(operand),
        }
    }

    pub fn and(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        assert_eq!(
            left.typ(),
            Type::Bool,
            "BoolLogicalAnd expects Bool operands, got: {}",
            left
        );
        assert_eq!(
            right.typ(),
            Type::Bool,
            "BoolLogicalAnd expects Bool operands, got: {}",
            right
        );
        PureExpr::BoolLogicalAnd {
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn or(&self, left: PureExpr, right: PureExpr) -> PureExpr {
        assert_eq!(
            left.typ(),
            Type::Bool,
            "BoolLogicalOr expects Bool operands, got: {}",
            left
        );
        assert_eq!(
            right.typ(),
            Type::Bool,
            "BoolLogicalOr expects Bool operands, got: {}",
            right
        );
        PureExpr::BoolLogicalOr {
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn array_typed(&self, element_type: Type, elements: Vec<PureExpr>) -> PureExpr {
        for element in &elements {
            assert_eq!(
                element.typ(),
                element_type,
                "Array elements must all have the same type, got: {}",
                element
            );
        }

        PureExpr::Array {
            elements,
            typ: Type::Array(Box::new(element_type)),
        }
    }

    pub fn tuple(&self, elements: Vec<PureExpr>) -> PureExpr {
        PureExpr::Tuple {
            typ: Type::Tuple(elements.iter().map(|element| element.typ()).collect()),
            elements,
        }
    }

    pub fn tuple_index(&self, tuple: PureExpr, index: usize) -> PureExpr {
        let Type::Tuple(elements) = tuple.typ() else {
            panic!("Cannot index into non-tuple type: {}", tuple.typ());
        };
        assert!(
            index < elements.len(),
            "Index {index} is out of range for a {}-tuple",
            elements.len()
        );
        PureExpr::TupleIndex {
            typ: elements[index].clone(),
            tuple: Box::new(tuple),
            index,
        }
    }

    pub fn int_to_string(&self, value: PureExpr) -> PureExpr {
        assert_eq!(
            value.typ(),
            Type::Int,
            "IntToString expects Int operand, got: {}",
            value
        );
        PureExpr::Unary {
            op: IrUnaryOp::IntToString,
            operand: Box::new(value),
        }
    }

    pub fn float_to_int(&self, value: PureExpr) -> PureExpr {
        assert_eq!(
            value.typ(),
            Type::Float,
            "FloatToInt expects Float operand, got: {}",
            value
        );
        PureExpr::Unary {
            op: IrUnaryOp::FloatToInt,
            operand: Box::new(value),
        }
    }

    pub fn int_to_float(&self, value: PureExpr) -> PureExpr {
        assert_eq!(
            value.typ(),
            Type::Int,
            "IntToFloat expects Int operand, got: {}",
            value
        );
        PureExpr::Unary {
            op: IrUnaryOp::IntToFloat,
            operand: Box::new(value),
        }
    }

    pub fn record(&self, record_name: &str, fields: Vec<(&str, PureExpr)>) -> PureExpr {
        let name = TypeName::parse(record_name).unwrap();
        let record_fields = self.types.record_fields(record_name);

        for (field_name, value) in &fields {
            let declared_type = record_fields
                .iter()
                .find(|f| f.name.as_str() == *field_name)
                .map(|f| &f.typ)
                .unwrap_or_else(|| {
                    panic!(
                        "Field '{}' not found in record '{}'",
                        field_name, record_name
                    )
                });
            assert_eq!(
                value.typ(),
                *declared_type,
                "Field '{}' of record '{}' has mismatched type, got: {}",
                field_name,
                record_name,
                value
            );
        }

        let missing_fields: Vec<&str> = record_fields
            .iter()
            .filter(|f| !fields.iter().any(|(name, _)| *name == f.name.as_str()))
            .map(|f| f.name.as_str())
            .collect();
        assert!(
            missing_fields.is_empty(),
            "Record '{}' is missing fields: {:?}",
            record_name,
            missing_fields
        );

        PureExpr::Record {
            type_name: name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (FieldName::parse(k).unwrap(), v))
                .collect(),
            typ: self.types.named(record_name),
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
        let name = TypeName::parse(enum_name).unwrap();
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
                .find(|f| f.name.as_str() == *field_name)
                .map(|f| &f.typ)
                .unwrap_or_else(|| {
                    panic!(
                        "Field '{}' not found in variant '{}::{}'",
                        field_name, enum_name, variant_name
                    )
                });
            assert_eq!(
                value.typ(),
                *declared_type,
                "Field '{}' of variant '{}::{}' has mismatched type, got: {}",
                field_name,
                enum_name,
                variant_name,
                value
            );
        }

        let missing_fields: Vec<&str> = variant_fields
            .iter()
            .filter(|f| {
                !field_values
                    .iter()
                    .any(|(name, _)| *name == f.name.as_str())
            })
            .map(|f| f.name.as_str())
            .collect();
        assert!(
            missing_fields.is_empty(),
            "Enum variant '{}::{}' is missing fields: {:?}",
            enum_name,
            variant_name,
            missing_fields
        );

        PureExpr::Enum {
            type_name: name,
            variant_name: TypeName::parse(variant_name).unwrap(),
            fields: field_values
                .into_iter()
                .map(|(k, v)| (FieldName::parse(k).unwrap(), v))
                .collect(),
            typ: self.types.named(enum_name),
        }
    }

    pub fn some(&self, inner: PureExpr) -> PureExpr {
        let inner_type = inner.typ();
        PureExpr::Option {
            value: Some(Box::new(inner)),
            typ: Type::Option(Box::new(inner_type)),
        }
    }

    pub fn none(&self, inner_type: &str) -> PureExpr {
        self.none_typed(self.types.resolve(inner_type))
    }

    pub fn none_typed(&self, inner_type: Type) -> PureExpr {
        PureExpr::Option {
            value: None,
            typ: Type::Option(Box::new(inner_type)),
        }
    }

    pub fn enum_match_expr<F>(&self, subject: PureExpr, arms_fn: F) -> PureExpr
    where
        F: FnOnce(&mut EnumMatchExprArms<'_>),
    {
        let subject_type = subject.typ();
        let Some(ResolvedType::Enum { name, variants, .. }) =
            self.types.registry().resolve(&subject_type)
        else {
            panic!("Match subject must be an enum type")
        };
        let (type_name, variants) = (name.clone(), variants.to_vec());

        let mut arms = EnumMatchExprArms {
            builder: self,
            type_name,
            variants,
            arms: Vec::new(),
            result_type: None,
        };
        arms_fn(&mut arms);
        assert_exhaustive(&arms.type_name, &arms.variants, &arms.arms);
        let typ = arms
            .result_type
            .expect("enum_match_expr requires at least one arm");

        PureExpr::Match {
            match_: Match::Enum {
                subject: Box::new(subject),
                arms: arms.arms,
            },
            typ,
        }
    }

    pub fn bool_match_expr(
        &self,
        subject: PureExpr,
        true_body: PureExpr,
        false_body: PureExpr,
    ) -> PureExpr {
        assert_eq!(subject.typ(), Type::Bool, "{}", subject);
        assert_eq!(
            true_body.typ(),
            false_body.typ(),
            "Match arms must all have the same type, got: {} and {}",
            true_body,
            false_body
        );
        let result_type = true_body.typ();

        PureExpr::Match {
            match_: Match::Bool {
                subject: Box::new(subject),
                true_body: Box::new(true_body),
                false_body: Box::new(false_body),
            },
            typ: result_type,
        }
    }

    pub fn option_match_expr(
        &self,
        subject: PureExpr,
        some_body: PureExpr,
        none_body: PureExpr,
    ) -> PureExpr {
        assert!(
            matches!(subject.typ(), Type::Option(_)),
            "Match subject must be an option type, got: {}",
            subject
        );
        assert_eq!(
            some_body.typ(),
            none_body.typ(),
            "Match arms must all have the same type, got: {} and {}",
            some_body,
            none_body
        );
        let result_type = some_body.typ();

        PureExpr::Match {
            match_: Match::Option {
                subject: Box::new(subject),
                some_arm_binding: None,
                some_arm_body: Box::new(some_body),
                none_arm_body: Box::new(none_body),
            },
            typ: result_type,
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
        let inner_type = match subject.typ() {
            Type::Option(inner) => inner.as_ref().clone(),
            _ => panic!("Match subject must be an option type, got: {}", subject),
        };

        let binding = self.bind();
        let some_body =
            some_body_fn(&self.scoped([(binding_name.to_string(), binding, inner_type.clone())]));

        assert_eq!(
            some_body.typ(),
            none_body.typ(),
            "Match arms must all have the same type, got: {} and {}",
            some_body,
            none_body
        );
        let result_type = some_body.typ();

        PureExpr::Match {
            match_: Match::Option {
                subject: Box::new(subject),
                some_arm_binding: Some(IrBinder {
                    var: binding,
                    typ: inner_type,
                }),
                some_arm_body: Box::new(some_body),
                none_arm_body: Box::new(none_body),
            },
            typ: result_type,
        }
    }

    pub fn field_access(&self, object: PureExpr, field_str: &str) -> PureExpr {
        let field_name = FieldName::parse(field_str).unwrap();
        let object_type = object.typ();
        let field_type = match self.types.registry().resolve(&object_type) {
            Some(ResolvedType::Record {
                name: record_name,
                fields,
                ..
            }) => fields
                .iter()
                .find(|f| f.name.as_str() == field_str)
                .map(|f| f.typ.clone())
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
            typ: field_type,
        }
    }

    pub fn let_expr<F>(&self, var_name: &str, value: PureExpr, body_fn: F) -> PureExpr
    where
        F: FnOnce(&Self) -> PureExpr,
    {
        let value_type = value.typ();

        let var = self.bind();
        let body = body_fn(&self.scoped([(var_name.to_string(), var, value_type.clone())]));

        let typ = body.typ();

        PureExpr::Let {
            var: IrBinder {
                var,
                typ: value_type,
            },
            value: Box::new(value),
            body: Box::new(body),
            typ,
        }
    }

    pub fn string_concat(&self, parts: Vec<PureExpr>) -> PureExpr {
        for part in &parts {
            assert_eq!(
                part.typ(),
                Type::String,
                "StringConcat expects String parts, got: {}",
                part
            );
        }
        PureExpr::StringConcat { parts }
    }

    pub fn array_length(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.typ(), Type::Array(_)),
            "ArrayLength expects Array operand, got: {}",
            operand
        );
        PureExpr::Unary {
            op: IrUnaryOp::ArrayLength,
            operand: Box::new(operand),
        }
    }

    pub fn array_is_empty(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.typ(), Type::Array(_)),
            "ArrayIsEmpty expects Array operand, got: {}",
            operand
        );
        PureExpr::Unary {
            op: IrUnaryOp::ArrayIsEmpty,
            operand: Box::new(operand),
        }
    }

    pub fn string_is_empty(&self, operand: PureExpr) -> PureExpr {
        assert_eq!(
            operand.typ(),
            Type::String,
            "StringIsEmpty expects String operand, got: {}",
            operand
        );
        PureExpr::Unary {
            op: IrUnaryOp::StringIsEmpty,
            operand: Box::new(operand),
        }
    }

    pub fn option_is_some(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.typ(), Type::Option(_)),
            "OptionIsSome expects Option operand, got: {}",
            operand
        );
        PureExpr::Unary {
            op: IrUnaryOp::OptionIsSome,
            operand: Box::new(operand),
        }
    }

    pub fn option_is_none(&self, operand: PureExpr) -> PureExpr {
        assert!(
            matches!(operand.typ(), Type::Option(_)),
            "OptionIsNone expects Option operand, got: {}",
            operand
        );
        PureExpr::Unary {
            op: IrUnaryOp::OptionIsNone,
            operand: Box::new(operand),
        }
    }

    /// Text that renders as written. Markup text never contains `<`, `{`
    /// or `}`, so an element is built with `element`, not written out.
    pub fn text(&self, content: &str) -> PureExpr {
        assert!(
            !content.contains(['<', '{', '}']),
            "builder text() called with text containing '<', '{{' or '}}': {content:?}"
        );
        PureExpr::HtmlText {
            content: CheapString::new(content.to_string()),
        }
    }

    /// An element. A void element takes no children.
    pub fn element(
        &self,
        tag: &str,
        attributes: Vec<PureAttribute>,
        children: Vec<PureExpr>,
    ) -> PureExpr {
        let element =
            HtmlElementKind::parse(tag).unwrap_or_else(|| panic!("unknown element {tag}"));
        assert!(
            !element.is_void() || children.is_empty(),
            "void element {tag} takes no children"
        );
        let children = self.concat(children);
        PureExpr::HtmlElement {
            element,
            attributes,
            children: Box::new(children),
        }
    }

    /// An attribute with a String value.
    pub fn attr(&self, name: &str, value: PureExpr) -> PureAttribute {
        assert_eq!(
            value.typ(),
            Type::String,
            "attribute {name} expects a String value, got: {value}"
        );
        PureAttribute::Value {
            name: AttributeName::parse(name).unwrap(),
            value,
        }
    }

    /// A boolean attribute, present when the condition is true.
    pub fn presence(&self, name: &str, present: PureExpr) -> PureAttribute {
        assert_eq!(
            present.typ(),
            Type::Bool,
            "attribute {name} expects a Bool condition, got: {present}"
        );
        PureAttribute::Presence {
            name: AttributeName::parse(name).unwrap(),
            present,
        }
    }

    pub fn escape(&self, expr: PureExpr) -> PureExpr {
        assert_eq!(
            expr.typ(),
            Type::String,
            "HtmlEscape expects String operand, got: {}",
            expr
        );
        PureExpr::HtmlEscape {
            expr: Box::new(expr),
        }
    }

    pub fn concat(&self, parts: Vec<PureExpr>) -> PureExpr {
        for part in &parts {
            assert_eq!(
                part.typ(),
                Type::Html,
                "HtmlConcat expects Html parts, got: {}",
                part
            );
        }
        PureExpr::HtmlConcat { parts }
    }

    pub fn html_for<F>(&self, var: Option<&str>, array: PureExpr, body_fn: F) -> PureExpr
    where
        F: FnOnce(&Self) -> PureExpr,
    {
        let element_type = match array.typ() {
            Type::Array(elem_type) => elem_type.as_ref().clone(),
            _ => panic!("Cannot iterate over non-array type"),
        };

        let name = var;
        let var = name.map(|_| self.bind());
        let bindings: Vec<_> = name
            .into_iter()
            .zip(var)
            .map(|(name, v)| (name.to_string(), v, element_type.clone()))
            .collect();
        let var = var.map(|var| IrBinder {
            var,
            typ: element_type,
        });
        let body = body_fn(&self.scoped(bindings));
        assert_eq!(
            body.typ(),
            Type::Html,
            "HtmlFor expects an Html body, got: {}",
            body
        );

        PureExpr::HtmlFor {
            var,
            source: Box::new(PureForSource::Array(array)),
            body: Box::new(body),
        }
    }

    pub fn html_for_range<F>(
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
            start.typ(),
            Type::Int,
            "Range bounds must be Int, got: {}",
            start
        );
        assert_eq!(
            end.typ(),
            Type::Int,
            "Range bounds must be Int, got: {}",
            end
        );

        let name = var;
        let var = name.map(|_| self.bind());
        let bindings: Vec<_> = name
            .into_iter()
            .zip(var)
            .map(|(name, v)| (name.to_string(), v, Type::Int))
            .collect();
        let var = var.map(|var| IrBinder {
            var,
            typ: Type::Int,
        });
        let body = body_fn(&self.scoped(bindings));
        assert_eq!(
            body.typ(),
            Type::Html,
            "HtmlFor expects an Html body, got: {}",
            body
        );

        PureExpr::HtmlFor {
            var,
            source: Box::new(PureForSource::RangeInclusive { start, end }),
            body: Box::new(body),
        }
    }

    /// A call with its arguments given by parameter name, in any order.
    pub fn call(&self, name: &str, mut args: Vec<(&str, PureExpr)>) -> PureExpr {
        let (function, parameters, return_type) = self
            .callees
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("Call to undeclared function '{}'", name));

        let pure_args: Vec<PureExpr> = parameters
            .iter()
            .map(|param| {
                let index = args
                    .iter()
                    .position(|(k, _)| *k == param.as_str())
                    .unwrap_or_else(|| panic!("Call to '{}' has no argument '{}'", name, param));
                args.swap_remove(index).1
            })
            .collect();
        if let Some((k, _)) = args.first() {
            panic!("Call to '{}' has unknown argument '{}'", name, k);
        }

        PureExpr::Call {
            function,
            args: pure_args,
            typ: return_type,
        }
    }
}

pub struct EnumMatchExprArms<'a> {
    builder: &'a PureBuilder,
    type_name: TypeName,
    variants: Vec<EnumVariant>,
    arms: Vec<EnumMatchArm<PureExpr>>,
    result_type: Option<Type>,
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
            &self.type_name,
            &self.variants,
            variant,
            field_bindings,
        );
        let body = body_fn(&self.builder.scoped(scoped_vars));
        match &self.result_type {
            Some(result_type) => assert_eq!(
                body.typ(),
                *result_type,
                "Match arms must all have the same type, got: {}",
                body
            ),
            None => self.result_type = Some(body.typ()),
        }
        self.arms.push(EnumMatchArm {
            pattern: EnumPattern::Variant {
                type_name: self.type_name.clone(),
                variant_name: TypeName::parse(variant).unwrap(),
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
) -> (Vec<(FieldName, IrBinder)>, Vec<ScopedVar>) {
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
            .find(|f| f.name.as_str() == field_name)
            .map(|f| f.typ.clone())
            .unwrap_or_else(|| {
                panic!(
                    "Field '{}' not found in variant '{}::{}'",
                    field_name, enum_name, variant
                )
            });
        let binding = builder.bind();
        bindings.push((
            FieldName::parse(field_name).unwrap(),
            IrBinder {
                var: binding,
                typ: field_type.clone(),
            },
        ));
        scoped_vars.push((binding_name.to_string(), binding, field_type));
    }
    (bindings, scoped_vars)
}

fn assert_exhaustive<B>(enum_name: &TypeName, variants: &[EnumVariant], arms: &[EnumMatchArm<B>]) {
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
