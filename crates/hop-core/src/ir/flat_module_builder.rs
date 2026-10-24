use crate::document::CheapString;
use crate::hop::typing::{
    Constructor, DataType, Pattern, PatternField, RecordField, ResolvedType, Ty, Type, TypeRegistry,
};
use crate::html::HtmlElementKind;
use crate::ir::flat_lint;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatFunctionDeclaration, FlatModule, FlatOp};
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::ir_parameter::IrParameter;
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use std::cell::{Ref, RefCell};

pub struct FlatModuleBuilder {
    registry: TypeRegistry,
    function_ids: FunctionIdCounter,
    value_ids: RefCell<ValueIdCounter>,
    /// The type of every name bound so far, indexed by the name's number.
    value_types: RefCell<Vec<Type>>,
    functions: Vec<DeclaredFunction>,
}

impl Default for FlatModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl FlatModuleBuilder {
    pub fn new() -> Self {
        Self::with_registry(TypeRegistry::default())
    }

    pub fn with_registry(registry: TypeRegistry) -> Self {
        FlatModuleBuilder {
            registry,
            function_ids: FunctionIdCounter::new(),
            value_ids: RefCell::new(ValueIdCounter::new()),
            value_types: RefCell::new(Vec::new()),
            functions: Vec::new(),
        }
    }

    pub fn function<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare(name, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    pub fn entrypoint<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare_function(name, true, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    pub fn declare<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
    ) -> Self {
        self.declare_function(name, false, params, return_type)
    }

    fn declare_function<'p>(
        mut self,
        name: &str,
        entrypoint: bool,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
    ) -> Self {
        assert!(
            !self
                .functions
                .iter()
                .any(|declared| declared.function.name.as_str() == name)
        );
        let function =
            IrFunction::new(self.function_ids.next(), FunctionName::parse(name).unwrap());
        let parameters = params
            .into_iter()
            .map(|(name, typ)| {
                let typ = Type::from(typ);
                let var = self.binder(typ.clone());
                IrParameter {
                    name: AttributeName::parse(name).unwrap(),
                    binder: IrBinder { var, typ },
                }
            })
            .collect();
        self.functions.push(DeclaredFunction {
            function,
            entrypoint,
            parameters,
            return_type: return_type.into(),
            body: None,
        });
        self
    }

    pub fn define(
        mut self,
        name: &str,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>, &DeclaredFunction) -> ValueId,
    ) -> Self {
        let index = self
            .functions
            .iter()
            .position(|declared| declared.function.name.as_str() == name)
            .unwrap();
        let declared = &self.functions[index];
        assert!(declared.body.is_none());
        let (body, _) = FlatBlockBuilder::build(&self, declared, |t| body_fn(t, declared));
        self.functions[index].body = Some(body);
        self
    }

    fn binder(&self, typ: Type) -> ValueId {
        let name = self.value_ids.borrow_mut().next();
        let mut types = self.value_types.borrow_mut();
        assert_eq!(types.len(), name.index());
        types.push(typ);
        name
    }

    pub fn build(self) -> FlatModule {
        self.build_with_registry().0
    }

    pub fn build_with_registry(self) -> (FlatModule, TypeRegistry) {
        let registry = self.registry;
        let functions = self
            .functions
            .into_iter()
            .map(|declared| FlatFunctionDeclaration {
                function: declared.function,
                entrypoint: declared.entrypoint,
                parameters: declared.parameters,
                return_type: declared.return_type,
                body: declared.body.unwrap(),
            })
            .collect();
        let module = FlatModule {
            functions,
            value_ids: self.value_ids.into_inner(),
        };
        flat_lint::assert_well_typed(&module, &registry);
        flat_lint::assert_html_affine(&module);
        (module, registry)
    }
}

pub struct DeclaredFunction {
    pub function: IrFunction,
    pub entrypoint: bool,
    pub parameters: Vec<IrParameter>,
    pub return_type: Type,
    pub body: Option<FlatBlock>,
}

pub struct FlatBlockBuilder<'a> {
    module: &'a FlatModuleBuilder,
    function: &'a DeclaredFunction,
    bindings: RefCell<Vec<FlatBinding>>,
}

impl<'a> FlatBlockBuilder<'a> {
    fn build(
        module: &'a FlatModuleBuilder,
        function: &'a DeclaredFunction,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> (FlatBlock, Type) {
        let builder = FlatBlockBuilder {
            module,
            function,
            bindings: RefCell::new(Vec::new()),
        };
        let result = body_fn(&builder);
        let typ = builder.typ(result);
        let bindings = builder.bindings.into_inner();
        let block = FlatBlock { bindings, result };
        (block, typ)
    }

    fn block(&self, body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId) -> (FlatBlock, Type) {
        Self::build(self.module, self.function, body_fn)
    }

    fn typ(&self, name: ValueId) -> Type {
        self.module.value_types.borrow()[name.index()].clone()
    }

    /// The type of every name the module has bound so far, indexed by the
    /// name's number. The borrow must end before another name is bound.
    pub fn value_types(&self) -> Ref<'a, [Type]> {
        Ref::map(self.module.value_types.borrow(), Vec::as_slice)
    }

    fn push(&self, typ: Type, op: FlatOp) -> ValueId {
        let name = self.module.binder(typ.clone());
        self.bindings.borrow_mut().push(FlatBinding {
            binder: IrBinder { var: name, typ },
            op,
        });
        name
    }

    pub fn registry(&self) -> &'a TypeRegistry {
        &self.module.registry
    }

    pub fn functions(&self) -> &'a [DeclaredFunction] {
        &self.module.functions
    }

    pub fn str(&self, s: &str) -> ValueId {
        self.push(
            Type::STRING,
            FlatOp::Data(IrDataOp::StringLiteral(CheapString::new(s.to_string()))),
        )
    }

    pub fn int(&self, n: i32) -> ValueId {
        self.push(Type::INT, FlatOp::Data(IrDataOp::IntLiteral(n)))
    }

    pub fn bool(&self, b: bool) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(b)))
    }

    pub fn float(&self, f: f64) -> ValueId {
        self.push(Type::FLOAT, FlatOp::Data(IrDataOp::FloatLiteral(f)))
    }

    pub fn param(&self, name: &str) -> ValueId {
        self.function
            .parameters
            .iter()
            .find(|param| param.name.as_str() == name)
            .unwrap()
            .binder
            .var
    }

    pub fn eq(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::Equals(left, right)))
    }

    pub fn lt(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::LessThan(left, right)))
    }

    pub fn lte(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            Type::BOOL,
            FlatOp::Data(IrDataOp::LessThanOrEqual(left, right)),
        )
    }

    pub fn add(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            self.typ(left),
            FlatOp::Data(IrDataOp::NumericAdd(left, right)),
        )
    }

    pub fn sub(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            self.typ(left),
            FlatOp::Data(IrDataOp::NumericSubtract(left, right)),
        )
    }

    pub fn mul(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            self.typ(left),
            FlatOp::Data(IrDataOp::NumericMultiply(left, right)),
        )
    }

    pub fn not(&self, operand: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolNegation(operand)))
    }

    pub fn neg(&self, operand: ValueId) -> ValueId {
        self.push(
            self.typ(operand),
            FlatOp::Data(IrDataOp::NumericNegation(operand)),
        )
    }

    pub fn array(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let Type::Data(element_type) = self.typ(elements[0]) else {
            panic!();
        };
        self.push(
            Type::array(element_type),
            FlatOp::Data(IrDataOp::Array(elements)),
        )
    }

    pub fn empty_array(&self, element_type: Type) -> ValueId {
        let Type::Data(element_type) = element_type else {
            panic!();
        };
        self.push(
            Type::array(element_type),
            FlatOp::Data(IrDataOp::Array(Vec::new())),
        )
    }

    pub fn array_from_range(&self, start: ValueId, end: ValueId) -> ValueId {
        self.push(
            Type::array(DataType::Int),
            FlatOp::Data(IrDataOp::ArrayFromRange(start, end)),
        )
    }

    pub fn tuple(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let element_types = elements
            .iter()
            .map(|element| match self.typ(*element) {
                Type::Data(element_type) => element_type,
                Type::Html(_) => panic!(),
            })
            .collect();
        self.push(
            Type::Data(DataType::Tuple(element_types)),
            FlatOp::Data(IrDataOp::Tuple(elements)),
        )
    }

    pub fn tuple_index(&self, tuple: ValueId, index: usize) -> ValueId {
        let Type::Data(DataType::Tuple(elements)) = self.typ(tuple) else {
            panic!();
        };
        self.push(
            Type::Data(elements[index].clone()),
            FlatOp::Data(IrDataOp::TupleIndex(tuple, index)),
        )
    }

    pub fn int_to_string(&self, value: ValueId) -> ValueId {
        self.push(Type::STRING, FlatOp::Data(IrDataOp::IntToString(value)))
    }

    pub fn float_to_int(&self, value: ValueId) -> ValueId {
        self.push(Type::INT, FlatOp::Data(IrDataOp::FloatToInt(value)))
    }

    pub fn int_to_float(&self, value: ValueId) -> ValueId {
        self.push(Type::FLOAT, FlatOp::Data(IrDataOp::IntToFloat(value)))
    }

    pub fn record<'f>(
        &self,
        typ: impl Into<Type>,
        fields: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            typ.into(),
            FlatOp::Data(IrDataOp::Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (FieldName::parse(k).unwrap(), v))
                    .collect(),
            )),
        )
    }

    pub fn enum_variant_with_fields<'f>(
        &self,
        typ: impl Into<Type>,
        variant_name: &str,
        field_values: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            typ.into(),
            FlatOp::Data(IrDataOp::Enum(
                TypeName::parse(variant_name).unwrap(),
                field_values
                    .into_iter()
                    .map(|(k, v)| (FieldName::parse(k).unwrap(), v))
                    .collect(),
            )),
        )
    }

    pub fn some(&self, inner: ValueId) -> ValueId {
        let Type::Data(inner_type) = self.typ(inner) else {
            panic!();
        };
        self.push(
            Type::option(inner_type),
            FlatOp::Data(IrDataOp::OptionSome(inner)),
        )
    }

    pub fn none(&self, inner_type: Type) -> ValueId {
        let Type::Data(inner_type) = inner_type else {
            panic!();
        };
        self.push(Type::option(inner_type), FlatOp::Data(IrDataOp::OptionNone))
    }

    pub fn match_(
        &self,
        subject: ValueId,
        arms_fn: impl FnOnce(&mut MatchExprArms<'_>),
    ) -> ValueId {
        let mut arms = MatchExprArms {
            builder: self,
            subject_type: self.typ(subject),
            arms: Vec::new(),
            typ: None,
        };
        arms_fn(&mut arms);
        let MatchExprArms { arms, typ, .. } = arms;
        self.push(typ.unwrap(), FlatOp::Match(IrMatch { subject, arms }))
    }

    fn resolve_pat(&self, typ: &Type, pat: Pat, bindings: &mut ArmBindings) -> IrPattern {
        let constructor = |constructor: Constructor, args: Vec<IrPattern>| Pattern::Constructor {
            constructor,
            args,
            fields: Vec::new(),
        };
        let resolved = typ
            .as_data()
            .and_then(|data| self.module.registry.resolve(data));
        match (pat, resolved) {
            (Pat::Wildcard, _) => Pattern::Wildcard,
            (Pat::Bind(name), _) => {
                let Type::Data(data) = typ else {
                    panic!("a pattern binds data, not {typ}");
                };
                let var = self.module.binder(typ.clone());
                bindings.bindings.push((name, var));
                Pattern::Binding(IrBinder {
                    var,
                    typ: data.clone(),
                })
            }
            (Pat::True, _) => constructor(Constructor::BoolTrue, Vec::new()),
            (Pat::False, _) => constructor(Constructor::BoolFalse, Vec::new()),
            (Pat::None, _) => constructor(Constructor::OptionNone, Vec::new()),
            (Pat::Some(inner), Some(ResolvedType::Option(inner_type))) => {
                let inner = self.resolve_pat(&Type::Data(inner_type.clone()), *inner, bindings);
                constructor(Constructor::OptionSome, vec![inner])
            }
            (Pat::Tuple(elements), Some(ResolvedType::Tuple(element_types)))
                if elements.len() == element_types.len() =>
            {
                let args = elements
                    .into_iter()
                    .zip(element_types)
                    .map(|(element, element_type)| {
                        self.resolve_pat(&Type::Data(element_type.clone()), element, bindings)
                    })
                    .collect();
                constructor(Constructor::Tuple, args)
            }
            (Pat::Record(listed), Some(ResolvedType::Record { name, fields })) => {
                Pattern::Constructor {
                    constructor: Constructor::Record {
                        type_name: name.clone(),
                    },
                    args: Vec::new(),
                    fields: self.resolve_fields(fields, listed, bindings),
                }
            }
            (Pat::Variant(variant, listed), Some(ResolvedType::Enum { name, variants })) => {
                let declared = variants
                    .iter()
                    .find(|v| v.name.as_str() == variant)
                    .unwrap();
                Pattern::Constructor {
                    constructor: Constructor::EnumVariant {
                        type_name: name.clone(),
                        variant_name: declared.name.clone(),
                    },
                    args: Vec::new(),
                    fields: self.resolve_fields(&declared.fields, listed, bindings),
                }
            }
            _ => panic!(),
        }
    }

    fn resolve_fields(
        &self,
        declared: &[RecordField],
        listed: Vec<(String, Pat)>,
        bindings: &mut ArmBindings,
    ) -> Vec<PatternField<IrBinder<DataType>>> {
        let mut patterns: Vec<(String, IrPattern)> = Vec::new();
        for (field_name, field_pat) in listed {
            assert!(!patterns.iter().any(|(listed, _)| *listed == field_name));
            let field = declared
                .iter()
                .find(|field| field.name.as_str() == field_name)
                .unwrap();
            let pattern = self.resolve_pat(&Type::Data(field.typ.clone()), field_pat, bindings);
            patterns.push((field_name, pattern));
        }
        declared
            .iter()
            .enumerate()
            .map(|(index, field)| PatternField {
                name: field.name.clone(),
                index,
                pattern: match patterns
                    .iter()
                    .position(|(listed, _)| listed == field.name.as_str())
                {
                    Some(position) => patterns.swap_remove(position).1,
                    None => Pattern::Wildcard,
                },
            })
            .collect()
    }

    pub fn field_access(&self, object: ValueId, field_str: &str) -> ValueId {
        let object_type = self.typ(object);
        let Some(ResolvedType::Record { fields, .. }) = object_type
            .as_data()
            .and_then(|data| self.registry().resolve(data))
        else {
            panic!();
        };
        let field = fields
            .iter()
            .find(|f| f.name.as_str() == field_str)
            .unwrap();
        self.push(
            Type::Data(field.typ.clone()),
            FlatOp::Data(IrDataOp::FieldAccess(object, field.name.clone())),
        )
    }

    pub fn string_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push(
            Type::STRING,
            FlatOp::Data(IrDataOp::StringConcat(parts.into_iter().collect())),
        )
    }

    pub fn array_length(&self, operand: ValueId) -> ValueId {
        self.push(Type::INT, FlatOp::Data(IrDataOp::ArrayLength(operand)))
    }

    pub fn array_is_empty(&self, operand: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::ArrayIsEmpty(operand)))
    }

    pub fn string_is_empty(&self, operand: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::StringIsEmpty(operand)))
    }

    pub fn option_is_some(&self, operand: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::OptionIsSome(operand)))
    }

    pub fn option_is_none(&self, operand: ValueId) -> ValueId {
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::OptionIsNone(operand)))
    }

    pub fn text(&self, content: &str) -> ValueId {
        assert!(!content.contains(['<', '{', '}']));
        self.push(
            Type::HTML,
            FlatOp::HtmlText(CheapString::new(content.to_string())),
        )
    }

    pub fn element(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, IrAttribute)>,
        children: ValueId,
    ) -> ValueId {
        let element = HtmlElementKind::parse(tag).unwrap();
        self.push(
            Type::HTML,
            FlatOp::HtmlElement(IrHtmlElement {
                element,
                attributes: attributes.into_iter().collect(),
                children,
            }),
        )
    }

    pub fn attr_value(&self, name: &str, value: ValueId) -> (AttributeName, IrAttribute) {
        (
            AttributeName::parse(name).unwrap(),
            IrAttribute::Value(value),
        )
    }

    pub fn attr_presence(&self, name: &str, present: ValueId) -> (AttributeName, IrAttribute) {
        (
            AttributeName::parse(name).unwrap(),
            IrAttribute::Presence(present),
        )
    }

    pub fn escape(&self, expr: ValueId) -> ValueId {
        self.push(Type::HTML, FlatOp::HtmlEscape(expr))
    }

    pub fn html_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push(Type::HTML, FlatOp::HtmlConcat(parts.into_iter().collect()))
    }

    pub fn html_for<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId,
    {
        let Type::Data(DataType::Array(element_type)) = self.typ(array) else {
            panic!();
        };
        let typ = *element_type;
        let var = self.module.binder(Type::Data(typ.clone()));
        let (body, _) = self.block(|t| body_fn(t, var));
        self.push(
            Type::HTML,
            FlatOp::HtmlFor(IrFor {
                var: Some(IrBinder { var, typ }),
                array,
                body,
            }),
        )
    }

    pub fn html_for_unbound<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    {
        let (body, _) = self.block(body_fn);
        self.push(
            Type::HTML,
            FlatOp::HtmlFor(IrFor {
                var: None,
                array,
                body,
            }),
        )
    }

    pub fn call(&self, name: &str, args: impl IntoIterator<Item = ValueId>) -> ValueId {
        let callee = self
            .module
            .functions
            .iter()
            .find(|callee| callee.function.name.as_str() == name)
            .unwrap();
        self.push(
            callee.return_type.clone(),
            FlatOp::Call(callee.function.clone(), args.into_iter().collect()),
        )
    }
}

#[derive(Debug, Clone)]
pub enum Pat {
    Wildcard,
    Bind(String),
    True,
    False,
    Some(Box<Pat>),
    None,
    Tuple(Vec<Pat>),
    Record(Vec<(String, Pat)>),
    Variant(String, Vec<(String, Pat)>),
}

impl Pat {
    pub fn bind(name: &str) -> Pat {
        Pat::Bind(name.to_string())
    }

    pub fn some(inner: Pat) -> Pat {
        Pat::Some(Box::new(inner))
    }

    pub fn variant<'s>(name: &str, fields: impl IntoIterator<Item = (&'s str, Pat)>) -> Pat {
        let fields = fields
            .into_iter()
            .map(|(field, pat)| (field.to_string(), pat))
            .collect();
        Pat::Variant(name.to_string(), fields)
    }
}

pub struct MatchExprArms<'a> {
    builder: &'a FlatBlockBuilder<'a>,
    subject_type: Type,
    arms: Vec<IrMatchArm<FlatBlock>>,
    typ: Option<Type>,
}

pub struct ArmBindings {
    bindings: Vec<(String, ValueId)>,
}

impl ArmBindings {
    pub fn binding(&self, name: &str) -> ValueId {
        self.bindings
            .iter()
            .find(|(bound, _)| bound == name)
            .unwrap()
            .1
    }
}

impl MatchExprArms<'_> {
    pub fn arm(
        &mut self,
        pat: Pat,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>, &ArmBindings) -> ValueId,
    ) {
        let mut bindings = ArmBindings {
            bindings: Vec::new(),
        };
        let pattern = self
            .builder
            .resolve_pat(&self.subject_type, pat, &mut bindings);
        let (body, typ) = self.builder.block(|t| body_fn(t, &bindings));
        self.typ.get_or_insert(typ);
        self.arms.push(IrMatchArm { pattern, body });
    }
}
