use crate::document::CheapString;
use crate::hop::typing::{
    Constructor, DataType, Pattern, PatternField, RecordField, ResolvedType, TestTypes, Type,
    TypeRegistry, TypeRegistryBuilder,
};
use crate::html::HtmlElementKind;
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
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Declares the functions of a module under construction, whose record and
/// enum types are declared up front. A function's body is built when the
/// function is defined, and it can call every function declared by then, so
/// a function that calls one defined after it is declared before both are
/// defined.
pub struct FlatModuleBuilder {
    types: TestTypes,
    function_ids: FunctionIdCounter,
    value_ids: RefCell<ValueIdCounter>,
    /// The type of each name bound so far.
    value_types: RefCell<HashMap<ValueId, Type>>,
    /// The functions in the order they are declared.
    functions: Vec<DeclaredFunction>,
}

impl Default for FlatModuleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl FlatModuleBuilder {
    /// A module that declares no records or enums.
    pub fn new() -> Self {
        Self::with_types(TypeRegistryBuilder::new().build())
    }

    /// A module whose records and enums are `types`.
    pub fn with_types(types: TestTypes) -> Self {
        FlatModuleBuilder {
            types,
            function_ids: FunctionIdCounter::new(),
            value_ids: RefCell::new(ValueIdCounter::new()),
            value_types: RefCell::new(HashMap::new()),
            functions: Vec::new(),
        }
    }

    /// A function, declared and defined at once.
    pub fn function<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        return_type: &str,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare(name, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    /// A function called from outside the module, declared and defined at
    /// once.
    pub fn entrypoint<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        return_type: &str,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare_function(name, true, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    /// A function that `define` gives a body later. Every function defined
    /// before then can call it.
    pub fn declare<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        return_type: &str,
    ) -> Self {
        self.declare_function(name, false, params, return_type)
    }

    fn declare_function<'p>(
        mut self,
        name: &str,
        entrypoint: bool,
        params: impl IntoIterator<Item = (&'p str, &'p str)>,
        return_type: &str,
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
                let typ = self.types.resolve(typ);
                let var = self.binder(typ.clone());
                IrParameter {
                    name: AttributeName::parse(name).unwrap(),
                    binder: IrBinder { var, typ },
                }
            })
            .collect();
        let return_type = self.types.resolve(return_type);
        self.functions.push(DeclaredFunction {
            function,
            entrypoint,
            parameters,
            return_type,
            body: None,
        });
        self
    }

    /// Give the function declared as `name` the body `body_fn` builds from
    /// the declaration, with its parameters in scope.
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

    /// A fresh name, of type `typ`.
    fn binder(&self, typ: Type) -> ValueId {
        let name = self.value_ids.borrow_mut().next();
        self.value_types.borrow_mut().insert(name, typ);
        name
    }

    /// The module.
    pub fn build(self) -> FlatModule {
        self.build_with_registry().0
    }

    pub fn build_with_registry(self) -> (FlatModule, TypeRegistry) {
        let registry = self.types.registry().clone();
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
        assert_html_affine(&module);
        (module, registry)
    }
}

/// A declared function, with its parameters bound and its body once it is
/// defined.
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

    /// The type of `name`.
    fn typ(&self, name: ValueId) -> Type {
        self.module.value_types.borrow()[&name].clone()
    }

    fn push(&self, typ: Type, op: FlatOp) -> ValueId {
        let name = self.module.binder(typ.clone());
        self.bindings.borrow_mut().push(FlatBinding {
            binder: IrBinder { var: name, typ },
            op,
        });
        name
    }

    /// The records and enums of the module.
    pub fn registry(&self) -> &'a TypeRegistry {
        self.module.types.registry()
    }

    /// The functions declared so far, in the order they are declared.
    pub fn functions(&self) -> &'a [DeclaredFunction] {
        &self.module.functions
    }

    /// Resolve a source-syntax type string, e.g. `Array[Int]`.
    pub fn resolve_type(&self, type_str: &str) -> Type {
        self.module.types.resolve(type_str)
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

    /// The parameter `name` of the function whose body is being built.
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
        let typ = self.typ(left);
        assert!(typ == self.typ(right) && typ.as_equatable_type().is_some());
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::Equals(left, right)))
    }

    pub fn lt(&self, left: ValueId, right: ValueId) -> ValueId {
        let typ = self.typ(left);
        assert!(typ == self.typ(right) && typ.as_comparable_type().is_some());
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::LessThan(left, right)))
    }

    pub fn lte(&self, left: ValueId, right: ValueId) -> ValueId {
        let typ = self.typ(left);
        assert!(typ == self.typ(right) && typ.as_comparable_type().is_some());
        self.push(
            Type::BOOL,
            FlatOp::Data(IrDataOp::LessThanOrEqual(left, right)),
        )
    }

    pub fn add(&self, left: ValueId, right: ValueId) -> ValueId {
        let typ = self.typ(left);
        assert!(typ == self.typ(right) && typ.as_numeric_type().is_some());
        self.push(typ, FlatOp::Data(IrDataOp::NumericAdd(left, right)))
    }

    pub fn sub(&self, left: ValueId, right: ValueId) -> ValueId {
        let typ = self.typ(left);
        assert!(typ == self.typ(right) && typ.as_numeric_type().is_some());
        self.push(typ, FlatOp::Data(IrDataOp::NumericSubtract(left, right)))
    }

    pub fn mul(&self, left: ValueId, right: ValueId) -> ValueId {
        let typ = self.typ(left);
        assert!(typ == self.typ(right) && typ.as_numeric_type().is_some());
        self.push(typ, FlatOp::Data(IrDataOp::NumericMultiply(left, right)))
    }

    pub fn not(&self, operand: ValueId) -> ValueId {
        assert!(self.typ(operand) == Type::BOOL);
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::BoolNegation(operand)))
    }

    pub fn neg(&self, operand: ValueId) -> ValueId {
        let typ = self.typ(operand);
        assert!(typ.as_numeric_type().is_some());
        self.push(typ, FlatOp::Data(IrDataOp::NumericNegation(operand)))
    }

    /// An array of at least one element, of the type of its elements.
    pub fn array(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let typ = self.typ(elements[0]);
        assert!(elements.iter().all(|element| self.typ(*element) == typ));
        let Type::Data(element_type) = typ else {
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
        assert!(self.typ(start) == Type::INT && self.typ(end) == Type::INT);
        self.push(
            Type::array(DataType::Int),
            FlatOp::Data(IrDataOp::ArrayFromRange(start, end)),
        )
    }

    pub fn tuple(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let (element_types, elements) = elements
            .into_iter()
            .map(|element| match self.typ(element) {
                Type::Data(element_type) => (element_type, element),
                Type::Html => panic!(),
            })
            .unzip();
        self.push(
            Type::Data(DataType::Tuple(element_types)),
            FlatOp::Data(IrDataOp::Tuple(elements)),
        )
    }

    pub fn tuple_index(&self, tuple: ValueId, index: usize) -> ValueId {
        let Type::Data(DataType::Tuple(elements)) = self.typ(tuple) else {
            panic!();
        };
        assert!(index < elements.len());
        self.push(
            Type::Data(elements[index].clone()),
            FlatOp::Data(IrDataOp::TupleIndex(tuple, index)),
        )
    }

    pub fn int_to_string(&self, value: ValueId) -> ValueId {
        assert!(self.typ(value) == Type::INT);
        self.push(Type::STRING, FlatOp::Data(IrDataOp::IntToString(value)))
    }

    pub fn float_to_int(&self, value: ValueId) -> ValueId {
        assert!(self.typ(value) == Type::FLOAT);
        self.push(Type::INT, FlatOp::Data(IrDataOp::FloatToInt(value)))
    }

    pub fn int_to_float(&self, value: ValueId) -> ValueId {
        assert!(self.typ(value) == Type::INT);
        self.push(Type::FLOAT, FlatOp::Data(IrDataOp::IntToFloat(value)))
    }

    pub fn record<'f>(
        &self,
        record_name: &str,
        fields: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            self.module.types.named(record_name),
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
        enum_name: &str,
        variant_name: &str,
        field_values: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            self.module.types.named(enum_name),
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

    /// A match with the arms `arms_fn` adds, of the type of the first.
    pub fn match_expr(
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

    /// The pattern against a subject of `typ`, with a fresh name for each
    /// variable it binds, which is pushed to `bindings` under the name the
    /// pattern gives it.
    fn resolve_pat(&self, typ: &Type, pat: Pat, bindings: &mut ArmBindings) -> IrPattern {
        let constructor = |constructor: Constructor, args: Vec<IrPattern>| Pattern::Constructor {
            constructor,
            args,
            fields: Vec::new(),
        };
        let resolved = typ
            .as_data()
            .and_then(|data| self.module.types.registry().resolve(data));
        match (pat, resolved) {
            (Pat::Wildcard, _) => Pattern::Wildcard,
            (Pat::Bind(name), _) => {
                let var = self.module.binder(typ.clone());
                bindings.bindings.push((name, var));
                Pattern::Binding(IrBinder {
                    var,
                    typ: typ.clone(),
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

    /// The patterns of the `declared` fields, in the order of the
    /// declaration, from those a record or variant pattern lists. A field
    /// the pattern leaves out is a wildcard.
    fn resolve_fields(
        &self,
        declared: &[RecordField],
        listed: Vec<(String, Pat)>,
        bindings: &mut ArmBindings,
    ) -> Vec<PatternField<IrBinder>> {
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
        let field_name = FieldName::parse(field_str).unwrap();
        let object_type = self.typ(object);
        let field_type = match object_type
            .as_data()
            .and_then(|data| self.module.types.registry().resolve(data))
        {
            Some(ResolvedType::Record { fields, .. }) => fields
                .iter()
                .find(|f| f.name.as_str() == field_str)
                .map(|f| Type::Data(f.typ.clone()))
                .unwrap(),
            _ => panic!(),
        };

        self.push(
            field_type,
            FlatOp::Data(IrDataOp::FieldAccess(object, field_name)),
        )
    }

    pub fn string_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push(
            Type::STRING,
            FlatOp::Data(IrDataOp::StringConcat(parts.into_iter().collect())),
        )
    }

    pub fn array_length(&self, operand: ValueId) -> ValueId {
        assert!(matches!(self.typ(operand), Type::Data(DataType::Array(_))));
        self.push(Type::INT, FlatOp::Data(IrDataOp::ArrayLength(operand)))
    }

    pub fn array_is_empty(&self, operand: ValueId) -> ValueId {
        assert!(matches!(self.typ(operand), Type::Data(DataType::Array(_))));
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::ArrayIsEmpty(operand)))
    }

    pub fn string_is_empty(&self, operand: ValueId) -> ValueId {
        assert!(self.typ(operand) == Type::STRING);
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::StringIsEmpty(operand)))
    }

    pub fn option_is_some(&self, operand: ValueId) -> ValueId {
        assert!(matches!(self.typ(operand), Type::Data(DataType::Option(_))));
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::OptionIsSome(operand)))
    }

    pub fn option_is_none(&self, operand: ValueId) -> ValueId {
        assert!(matches!(self.typ(operand), Type::Data(DataType::Option(_))));
        self.push(Type::BOOL, FlatOp::Data(IrDataOp::OptionIsNone(operand)))
    }

    /// Text that renders as written. Markup text never contains `<`, `{`
    /// or `}`, so an element is built with `element`, not written out.
    pub fn text(&self, content: &str) -> ValueId {
        assert!(!content.contains(['<', '{', '}']));
        self.push(
            Type::Html,
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
            Type::Html,
            FlatOp::HtmlElement(IrHtmlElement {
                element,
                attributes: attributes.into_iter().collect(),
                children,
            }),
        )
    }

    /// An attribute with a String value.
    pub fn attr(&self, name: &str, value: ValueId) -> (AttributeName, IrAttribute) {
        (
            AttributeName::parse(name).unwrap(),
            IrAttribute::Value(value),
        )
    }

    /// A boolean attribute, present when the condition is true.
    pub fn presence(&self, name: &str, present: ValueId) -> (AttributeName, IrAttribute) {
        (
            AttributeName::parse(name).unwrap(),
            IrAttribute::Presence(present),
        )
    }

    pub fn escape(&self, expr: ValueId) -> ValueId {
        self.push(Type::Html, FlatOp::HtmlEscape(expr))
    }

    pub fn html_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push(Type::Html, FlatOp::HtmlConcat(parts.into_iter().collect()))
    }

    /// A loop over `array` with the loop variable, bound to a fresh name
    /// that the body `body_fn` builds from.
    pub fn html_for<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId,
    {
        let Type::Data(DataType::Array(element_type)) = self.typ(array) else {
            panic!();
        };
        let typ = Type::Data(*element_type);
        let var = self.module.binder(typ.clone());
        let (body, _) = self.block(|t| body_fn(t, var));
        self.push(
            Type::Html,
            FlatOp::HtmlFor(IrFor {
                var: Some(IrBinder { var, typ }),
                array,
                body,
            }),
        )
    }

    /// A loop over `array` without a loop variable, with the body `body_fn`
    /// builds.
    pub fn html_for_unbound<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    {
        assert!(matches!(self.typ(array), Type::Data(DataType::Array(_))));
        let (body, _) = self.block(body_fn);
        self.push(
            Type::Html,
            FlatOp::HtmlFor(IrFor {
                var: None,
                array,
                body,
            }),
        )
    }

    /// A call with its arguments in the order the callee declares its
    /// parameters.
    pub fn call(&self, name: &str, args: impl IntoIterator<Item = ValueId>) -> ValueId {
        let args: Vec<ValueId> = args.into_iter().collect();
        let callee = self
            .module
            .functions
            .iter()
            .find(|callee| callee.function.name.as_str() == name)
            .unwrap();
        assert!(
            args.len() == callee.parameters.len()
                && args
                    .iter()
                    .zip(&callee.parameters)
                    .all(|(arg, param)| self.typ(*arg) == param.binder.typ)
        );
        self.push(
            callee.return_type.clone(),
            FlatOp::Call(callee.function.clone(), args),
        )
    }
}

/// A pattern as a test writes it: bindings by name, types taken from the
/// subject. The fields a record or variant pattern leaves out are
/// wildcards. The arm looks its bindings up by name.
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
    /// The type of the arms' results, once there is an arm.
    typ: Option<Type>,
}

/// The variables an arm's pattern binds, by the names the pattern gives
/// them.
pub struct ArmBindings {
    bindings: Vec<(String, ValueId)>,
}

impl ArmBindings {
    /// The variable the pattern binds as `name`.
    pub fn binding(&self, name: &str) -> ValueId {
        self.bindings
            .iter()
            .find(|(bound, _)| bound == name)
            .unwrap()
            .1
    }
}

impl MatchExprArms<'_> {
    /// Add an arm, its block built with the pattern's bindings in scope,
    /// which `body_fn` looks up by name.
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

fn assert_html_affine(module: &FlatModule) {
    let mut html = HashSet::new();
    for function in &module.functions {
        html.extend(
            function
                .parameters
                .iter()
                .filter(|param| param.binder.typ == Type::Html)
                .map(|param| param.binder.var),
        );
        collect_html(&function.body, &mut html);
    }
    for function in &module.functions {
        let scope = function
            .parameters
            .iter()
            .map(|param| param.binder.var)
            .filter(|name| html.contains(name))
            .collect();
        check_block(&function.body, scope, &html, &function.function);
    }
}

fn collect_html(block: &FlatBlock, html: &mut HashSet<ValueId>) {
    for binding in &block.bindings {
        if binding.binder.typ == Type::Html {
            html.insert(binding.binder.var);
        }
        match &binding.op {
            FlatOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_html(&arm.body, html);
                }
            }
            FlatOp::HtmlFor(for_) => collect_html(&for_.body, html),
            _ => {}
        }
    }
}

fn check_block(
    block: &FlatBlock,
    mut scope: HashSet<ValueId>,
    html: &HashSet<ValueId>,
    function: &IrFunction,
) -> HashSet<ValueId> {
    for binding in &block.bindings {
        binding
            .op
            .for_each_operand(&mut |operand| read(operand, &mut scope, html, function));
        match &binding.op {
            FlatOp::Match(match_) => {
                let mut after = scope.clone();
                for arm in &match_.arms {
                    let inner = check_block(&arm.body, scope.clone(), html, function);
                    after.retain(|name| inner.contains(name));
                }
                scope = after;
            }
            FlatOp::HtmlFor(for_) => {
                check_block(&for_.body, HashSet::new(), html, function);
            }
            _ => {}
        }
        if html.contains(&binding.binder.var) {
            scope.insert(binding.binder.var);
        }
    }
    read(block.result, &mut scope, html, function);
    scope
}

fn read(
    name: ValueId,
    scope: &mut HashSet<ValueId>,
    html: &HashSet<ValueId>,
    function: &IrFunction,
) {
    if html.contains(&name) {
        assert!(
            scope.remove(&name),
            "{function} reads the Html value {name} when the path has used it or in a loop body it is bound outside of"
        );
    }
}
