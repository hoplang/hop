use crate::document::CheapString;
use crate::hop::typing::{
    ComparableType, Constructor, EquatableType, NumericType, Pattern, PatternField, Ty,
};
use crate::html::HtmlElementKind;
use crate::ir::flat_html_element::{FlatAttribute, FlatHtmlElement};
use crate::ir::flat_program::{
    FlatBinding, FlatBlock, FlatEntrypoint, FlatFor, FlatFunction, FlatLambda, FlatMatch,
    FlatMatchArm, FlatOp, FlatPattern, FlatProgram,
};
use crate::ir::flat_type::{FlatDataType, FlatType, IrModifier};
use crate::ir::flat_type_declarations::{FlatField, FlatTypeDeclarations};
use crate::ir::flat_typecheck::{self, FlatTypeError};
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::lambda_id::LambdaIdCounter;
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use std::cell::{Ref, RefCell};

pub struct FlatProgramBuilder {
    types: FlatTypeDeclarations,
    function_ids: FunctionIdCounter,
    value_ids: RefCell<ValueIdCounter>,
    value_types: RefCell<Vec<FlatType>>,
    functions: Vec<DeclaredFunction>,
    lambda_ids: RefCell<LambdaIdCounter>,
    lambdas: RefCell<Vec<FlatLambda>>,
    entrypoints: Vec<FlatEntrypoint>,
}

impl Default for FlatProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl FlatProgramBuilder {
    pub fn new() -> Self {
        Self::with_types(FlatTypeDeclarations::default())
    }

    pub fn with_types(types: FlatTypeDeclarations) -> Self {
        FlatProgramBuilder {
            types,
            function_ids: FunctionIdCounter::new(),
            value_ids: RefCell::new(ValueIdCounter::new()),
            value_types: RefCell::new(Vec::new()),
            functions: Vec::new(),
            lambda_ids: RefCell::new(LambdaIdCounter::new()),
            lambdas: RefCell::new(Vec::new()),
            entrypoints: Vec::new(),
        }
    }

    pub fn function<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare(IrModifier::Pure, name, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    pub fn render_function<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare(IrModifier::Render, name, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    pub fn mut_function<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        self.declare(IrModifier::Mut, name, params, return_type)
            .define(name, |t, _| body_fn(t))
    }

    pub fn entrypoint<'p>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        let name = FunctionName::parse(name).unwrap();
        let parameters: Vec<IrParameter<FlatDataType>> = params
            .into_iter()
            .map(|(name, typ)| {
                let typ = self.types.ty(typ);
                let var = self.binder(typ.clone());
                let FlatType::Data(typ) = typ else {
                    panic!("an entrypoint parameter must have a data type");
                };
                IrParameter {
                    name: AttributeName::parse(name).unwrap(),
                    binder: IrBinder { var, typ },
                }
            })
            .collect();
        let block_parameters: Vec<_> = parameters.iter().map(|param| param.widen()).collect();
        let body = FlatBlockBuilder::build(&self, &block_parameters, body_fn).into_block();
        self.entrypoints.push(FlatEntrypoint {
            name,
            parameters,
            body,
        });
        self
    }

    pub fn declare<'p>(
        self,
        modifier: IrModifier,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
    ) -> Self {
        let params: Vec<(&str, FlatType)> = params
            .into_iter()
            .map(|(name, typ)| (name, self.types.ty(typ)))
            .collect();
        let return_type = self.types.ty(return_type);
        self.declare_with_types(name, params, return_type, modifier)
    }

    pub fn declare_with_types<'p>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, FlatType)>,
        return_type: FlatType,
        modifier: IrModifier,
    ) -> Self {
        let function =
            IrFunction::new(self.function_ids.next(), FunctionName::parse(name).unwrap());
        let parameters = params
            .into_iter()
            .map(|(name, typ)| {
                let var = self.binder(typ.clone());
                IrParameter {
                    name: AttributeName::parse(name).unwrap(),
                    binder: IrBinder { var, typ },
                }
            })
            .collect();
        self.functions.push(DeclaredFunction {
            function,
            parameters,
            return_type,
            modifier,
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
        let body = FlatBlockBuilder::build(&self, &declared.parameters, |t| body_fn(t, declared));
        self.functions[index].body = Some(body.into_block());
        self
    }

    fn binder(&self, typ: FlatType) -> ValueId {
        let name = self.value_ids.borrow_mut().next();
        let mut types = self.value_types.borrow_mut();
        assert_eq!(types.len(), name.index());
        types.push(typ);
        name
    }

    pub fn build(self) -> Result<FlatProgram, FlatTypeError> {
        let functions = self
            .functions
            .into_iter()
            .map(|declared| FlatFunction {
                function: declared.function,
                modifier: declared.modifier,
                parameters: declared.parameters,
                return_type: declared.return_type,
                body: declared.body.unwrap(),
            })
            .collect();
        let program = FlatProgram {
            types: self.types,
            entrypoints: self.entrypoints,
            functions,
            lambdas: self.lambdas.into_inner(),
            value_ids: self.value_ids.into_inner(),
        };
        flat_typecheck::typecheck(&program)?;
        Ok(program)
    }
}

pub struct DeclaredFunction {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter<FlatType>>,
    pub return_type: FlatType,
    pub modifier: IrModifier,
    body: Option<FlatBlock>,
}

struct BuiltBlock {
    bindings: Vec<FlatBinding>,
    result: ValueId,
    typ: FlatType,
}

impl BuiltBlock {
    fn into_block(self) -> FlatBlock {
        IrBlock {
            bindings: self.bindings,
            result: self.result,
        }
    }
}

pub struct FlatBlockBuilder<'a> {
    program: &'a FlatProgramBuilder,
    parameters: &'a [IrParameter<FlatType>],
    bindings: RefCell<Vec<FlatBinding>>,
}

impl<'a> FlatBlockBuilder<'a> {
    fn build(
        program: &'a FlatProgramBuilder,
        parameters: &'a [IrParameter<FlatType>],
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> BuiltBlock {
        let builder = FlatBlockBuilder {
            program,
            parameters,
            bindings: RefCell::new(Vec::new()),
        };
        let result = body_fn(&builder);
        BuiltBlock {
            typ: builder.typ(result),
            bindings: builder.bindings.into_inner(),
            result,
        }
    }

    fn block(&self, body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId) -> BuiltBlock {
        Self::build(self.program, self.parameters, body_fn)
    }

    fn typ(&self, name: ValueId) -> FlatType {
        self.program.value_types.borrow()[name.index()].clone()
    }

    pub fn value_types(&self) -> Ref<'a, [FlatType]> {
        Ref::map(self.program.value_types.borrow(), Vec::as_slice)
    }

    fn push(&self, typ: FlatType, op: FlatOp) -> ValueId {
        let name = self.program.binder(typ.clone());
        self.bindings.borrow_mut().push(IrBinding {
            binder: IrBinder { var: name, typ },
            op,
        });
        name
    }

    fn push_html(&self, op: FlatOp) -> ValueId {
        self.push(FlatType::HTML, op)
    }

    pub fn types(&self) -> &'a FlatTypeDeclarations {
        &self.program.types
    }

    pub fn ty(&self, ty: Ty) -> FlatType {
        self.program.types.ty(ty)
    }

    pub fn functions(&self) -> &'a [DeclaredFunction] {
        &self.program.functions
    }

    pub fn str(&self, s: &str) -> ValueId {
        self.push(
            FlatType::STRING,
            FlatOp::Data(IrDataOp::StringLiteral(CheapString::new(s.to_string()))),
        )
    }

    pub fn int(&self, n: i32) -> ValueId {
        self.push(FlatType::INT, FlatOp::Data(IrDataOp::IntLiteral(n)))
    }

    pub fn bool(&self, b: bool) -> ValueId {
        self.push(FlatType::BOOL, FlatOp::Data(IrDataOp::BoolLiteral(b)))
    }

    pub fn float(&self, f: f64) -> ValueId {
        self.push(FlatType::FLOAT, FlatOp::Data(IrDataOp::FloatLiteral(f)))
    }

    pub fn param(&self, name: &str) -> ValueId {
        self.parameters
            .iter()
            .find(|param| param.name.as_str() == name)
            .unwrap()
            .binder
            .var
    }

    pub fn eq(&self, typ: EquatableType, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::Equals(typ, left, right)),
        )
    }

    pub fn lt(&self, typ: ComparableType, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::LessThan(typ, left, right)),
        )
    }

    pub fn lte(&self, typ: ComparableType, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::LessThanOrEqual(typ, left, right)),
        )
    }

    pub fn add(&self, typ: NumericType, left: ValueId, right: ValueId) -> ValueId {
        let result = match typ {
            NumericType::Int => FlatType::INT,
            NumericType::Float => FlatType::FLOAT,
        };
        self.push(result, FlatOp::Data(IrDataOp::NumericAdd(typ, left, right)))
    }

    pub fn sub(&self, typ: NumericType, left: ValueId, right: ValueId) -> ValueId {
        let result = match typ {
            NumericType::Int => FlatType::INT,
            NumericType::Float => FlatType::FLOAT,
        };
        self.push(
            result,
            FlatOp::Data(IrDataOp::NumericSubtract(typ, left, right)),
        )
    }

    pub fn mul(&self, typ: NumericType, left: ValueId, right: ValueId) -> ValueId {
        let result = match typ {
            NumericType::Int => FlatType::INT,
            NumericType::Float => FlatType::FLOAT,
        };
        self.push(
            result,
            FlatOp::Data(IrDataOp::NumericMultiply(typ, left, right)),
        )
    }

    pub fn not(&self, operand: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::BoolNegation(operand)),
        )
    }

    pub fn neg(&self, typ: NumericType, operand: ValueId) -> ValueId {
        let result = match typ {
            NumericType::Int => FlatType::INT,
            NumericType::Float => FlatType::FLOAT,
        };
        self.push(
            result,
            FlatOp::Data(IrDataOp::NumericNegation(typ, operand)),
        )
    }

    pub fn array(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let FlatType::Data(element_type) = self.typ(elements[0]) else {
            panic!();
        };
        self.push(
            FlatType::array(element_type),
            FlatOp::Data(IrDataOp::Array(elements)),
        )
    }

    pub fn empty_array(&self, element_type: FlatType) -> ValueId {
        let FlatType::Data(element_type) = element_type else {
            panic!();
        };
        self.push(
            FlatType::array(element_type),
            FlatOp::Data(IrDataOp::Array(Vec::new())),
        )
    }

    pub fn array_from_range(&self, start: ValueId, end: ValueId) -> ValueId {
        self.push(
            FlatType::array(FlatDataType::Int),
            FlatOp::Data(IrDataOp::ArrayFromRange(start, end)),
        )
    }

    pub fn tuple(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let element_types = elements
            .iter()
            .map(|element| match self.typ(*element) {
                FlatType::Data(element_type) => element_type,
                typ => panic!("a tuple holds data, not {typ}"),
            })
            .collect();
        self.push(
            FlatType::Data(FlatDataType::Tuple(element_types)),
            FlatOp::Data(IrDataOp::Tuple(elements)),
        )
    }

    pub fn int_to_string(&self, value: ValueId) -> ValueId {
        self.push(FlatType::STRING, FlatOp::Data(IrDataOp::IntToString(value)))
    }

    pub fn float_to_int(&self, value: ValueId) -> ValueId {
        self.push(FlatType::INT, FlatOp::Data(IrDataOp::FloatToInt(value)))
    }

    pub fn int_to_float(&self, value: ValueId) -> ValueId {
        self.push(FlatType::FLOAT, FlatOp::Data(IrDataOp::IntToFloat(value)))
    }

    pub fn record<'f>(
        &self,
        typ: FlatType,
        fields: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            typ,
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
        typ: FlatType,
        variant_name: &str,
        field_values: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            typ,
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
        let FlatType::Data(inner_type) = self.typ(inner) else {
            panic!();
        };
        self.push(
            FlatType::option(inner_type),
            FlatOp::Data(IrDataOp::OptionSome(inner)),
        )
    }

    pub fn none(&self, inner_type: FlatType) -> ValueId {
        let FlatType::Data(inner_type) = inner_type else {
            panic!();
        };
        self.push(
            FlatType::option(inner_type),
            FlatOp::Data(IrDataOp::OptionNone),
        )
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
        self.push(typ.unwrap(), FlatOp::Match(FlatMatch { subject, arms }))
    }

    fn resolve_pat(&self, typ: &FlatType, pat: Pat, bindings: &mut ArmBindings) -> FlatPattern {
        let constructor = |constructor: Constructor, args: Vec<FlatPattern>| Pattern::Constructor {
            constructor,
            args,
            fields: Vec::new(),
        };
        match (pat, typ.as_data()) {
            (Pat::Wildcard, _) => Pattern::Wildcard,
            (Pat::Bind(name), _) => {
                let FlatType::Data(data) = typ else {
                    panic!("a pattern binds data, not {typ}");
                };
                let var = self.program.binder(typ.clone());
                bindings.bindings.push((name, var));
                Pattern::Binding(IrBinder {
                    var,
                    typ: data.clone(),
                })
            }
            (Pat::True, _) => constructor(Constructor::BoolTrue, Vec::new()),
            (Pat::False, _) => constructor(Constructor::BoolFalse, Vec::new()),
            (Pat::None, _) => constructor(Constructor::OptionNone, Vec::new()),
            (Pat::Some(inner), Some(FlatDataType::Option(inner_type))) => {
                let inner =
                    self.resolve_pat(&FlatType::Data((**inner_type).clone()), *inner, bindings);
                constructor(Constructor::OptionSome, vec![inner])
            }
            (Pat::Tuple(elements), Some(FlatDataType::Tuple(element_types))) => {
                let args = elements
                    .into_iter()
                    .zip(element_types)
                    .map(|(element, element_type)| {
                        self.resolve_pat(&FlatType::Data(element_type.clone()), element, bindings)
                    })
                    .collect();
                constructor(Constructor::Tuple, args)
            }
            (Pat::Record(listed), Some(FlatDataType::Record(name))) => {
                let fields = &self.types().record(name).unwrap().fields;
                Pattern::Constructor {
                    constructor: Constructor::Record {
                        type_name: name.clone(),
                    },
                    args: Vec::new(),
                    fields: self.resolve_fields(fields, listed, bindings),
                }
            }
            (Pat::Variant(variant, listed), Some(FlatDataType::Enum(name))) => {
                let declared = self
                    .types()
                    .enumeration(name)
                    .unwrap()
                    .variants
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
        declared: &[FlatField],
        listed: Vec<(String, Pat)>,
        bindings: &mut ArmBindings,
    ) -> Vec<PatternField<IrBinder<FlatDataType>>> {
        listed
            .into_iter()
            .map(|(field_name, field_pat)| {
                let (index, field) = declared
                    .iter()
                    .enumerate()
                    .find(|(_, field)| field.name.as_str() == field_name)
                    .unwrap();
                PatternField {
                    name: field.name.clone(),
                    index,
                    pattern: self.resolve_pat(
                        &FlatType::Data(field.typ.clone()),
                        field_pat,
                        bindings,
                    ),
                }
            })
            .collect()
    }

    pub fn field_access(&self, object: ValueId, field_str: &str) -> ValueId {
        let FlatType::Data(FlatDataType::Record(name)) = self.typ(object) else {
            panic!();
        };
        let field = self
            .types()
            .record(&name)
            .unwrap()
            .fields
            .iter()
            .find(|f| f.name.as_str() == field_str)
            .unwrap();
        self.push(
            FlatType::Data(field.typ.clone()),
            FlatOp::Data(IrDataOp::FieldAccess(object, field.name.clone())),
        )
    }

    pub fn tuple_index(&self, tuple: ValueId, index: usize) -> ValueId {
        let FlatType::Data(FlatDataType::Tuple(elements)) = self.typ(tuple) else {
            panic!("{tuple} is not a tuple");
        };
        self.push(
            FlatType::Data(elements[index].clone()),
            FlatOp::Data(IrDataOp::TupleIndex(tuple, index)),
        )
    }

    pub fn string_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push(
            FlatType::STRING,
            FlatOp::Data(IrDataOp::StringConcat(parts.into_iter().collect())),
        )
    }

    pub fn array_length(&self, operand: ValueId) -> ValueId {
        self.push(FlatType::INT, FlatOp::Data(IrDataOp::ArrayLength(operand)))
    }

    pub fn array_is_empty(&self, operand: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::ArrayIsEmpty(operand)),
        )
    }

    pub fn array_get(&self, array: ValueId, index: ValueId) -> ValueId {
        let FlatType::Data(FlatDataType::Array(element)) = self.typ(array) else {
            panic!();
        };
        self.push(
            FlatType::option(*element),
            FlatOp::Data(IrDataOp::ArrayGet(array, index)),
        )
    }

    pub fn array_enumerate(&self, array: ValueId) -> ValueId {
        let FlatType::Data(FlatDataType::Array(element)) = self.typ(array) else {
            panic!();
        };
        self.push(
            FlatType::Data(FlatDataType::Array(Box::new(FlatDataType::Tuple(vec![
                FlatDataType::Int,
                *element,
            ])))),
            FlatOp::Data(IrDataOp::ArrayEnumerate(array)),
        )
    }

    pub fn string_is_empty(&self, operand: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::StringIsEmpty(operand)),
        )
    }

    pub fn option_is_some(&self, operand: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::OptionIsSome(operand)),
        )
    }

    pub fn option_is_none(&self, operand: ValueId) -> ValueId {
        self.push(
            FlatType::BOOL,
            FlatOp::Data(IrDataOp::OptionIsNone(operand)),
        )
    }

    pub fn text(&self, content: &str) -> ValueId {
        assert!(!content.contains(['<', '{', '}']));
        self.push_html(FlatOp::Text(CheapString::new(content.to_string())))
    }

    pub fn element(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, FlatAttribute)>,
        children: Option<ValueId>,
    ) -> ValueId {
        self.push_element(tag, attributes, children, None)
    }

    pub fn element_on_click(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, FlatAttribute)>,
        event_handler: ValueId,
        children: Option<ValueId>,
    ) -> ValueId {
        self.push_element(tag, attributes, children, Some(event_handler))
    }

    fn push_element(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, FlatAttribute)>,
        children: Option<ValueId>,
        on_click: Option<ValueId>,
    ) -> ValueId {
        self.push_html(FlatOp::Element(FlatHtmlElement {
            element: HtmlElementKind::parse(tag).unwrap(),
            attributes: attributes.into_iter().collect(),
            children,
            on_click,
        }))
    }

    pub fn cell(&self, initial_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId) -> ValueId {
        let initial = self.block(initial_fn);
        let FlatType::Data(typ) = &initial.typ else {
            panic!("a cell holds data, not {}", initial.typ);
        };
        let typ = FlatType::Data(FlatDataType::cell(typ.clone()));
        self.push(typ, FlatOp::Cell(initial.into_block()))
    }

    pub fn attr_value(&self, name: &str, value: ValueId) -> (AttributeName, FlatAttribute) {
        (
            AttributeName::parse(name).unwrap(),
            FlatAttribute::Value(value),
        )
    }

    pub fn attr_presence(&self, name: &str, present: ValueId) -> (AttributeName, FlatAttribute) {
        (
            AttributeName::parse(name).unwrap(),
            FlatAttribute::Presence(present),
        )
    }

    pub fn escape(&self, expr: ValueId) -> ValueId {
        self.push_html(FlatOp::Escape(expr))
    }

    pub fn html_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push_html(FlatOp::Concat(parts.into_iter().collect()))
    }

    pub fn html_for<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId,
    {
        let FlatType::Data(FlatDataType::Array(element_type)) = self.typ(array) else {
            panic!();
        };
        let typ = *element_type;
        let var = self.program.binder(FlatType::Data(typ.clone()));
        let body = self.block(|t| body_fn(t, var)).into_block();
        self.push_html(FlatOp::For(FlatFor {
            var: Some(IrBinder { var, typ }),
            array,
            key: None,
            body,
        }))
    }

    pub fn html_for_keyed(
        &self,
        array: ValueId,
        key_fn: impl FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId,
    ) -> KeyedFor<'_, 'a> {
        let FlatType::Data(FlatDataType::Array(element_type)) = self.typ(array) else {
            panic!();
        };
        let typ = *element_type;
        let var = self.program.binder(FlatType::Data(typ.clone()));
        let key = self.block(|t| key_fn(t, var)).into_block();
        KeyedFor {
            builder: self,
            var: IrBinder { var, typ },
            array,
            key,
        }
    }

    pub fn html_for_unbound<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    {
        let body = self.block(body_fn).into_block();
        self.push_html(FlatOp::For(FlatFor {
            var: None,
            array,
            key: None,
            body,
        }))
    }

    pub fn call(&self, name: &str, args: impl IntoIterator<Item = ValueId>) -> ValueId {
        let callee = self
            .program
            .functions
            .iter()
            .find(|callee| callee.function.name.as_str() == name)
            .unwrap();
        let args = args.into_iter().collect();
        self.push(
            callee.return_type.clone(),
            FlatOp::Call(callee.function.id, args),
        )
    }

    pub fn lambda<F>(&self, parameters: impl IntoIterator<Item = FlatType>, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, &[ValueId]) -> ValueId,
    {
        self.closure(IrModifier::Pure, parameters, body_fn)
    }

    pub fn render_lambda<F>(
        &self,
        parameters: impl IntoIterator<Item = FlatType>,
        body_fn: F,
    ) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, &[ValueId]) -> ValueId,
    {
        self.closure(IrModifier::Render, parameters, body_fn)
    }

    fn closure<F>(
        &self,
        modifier: IrModifier,
        parameters: impl IntoIterator<Item = FlatType>,
        body_fn: F,
    ) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, &[ValueId]) -> ValueId,
    {
        let parameters: Vec<IrBinder<FlatType>> = parameters
            .into_iter()
            .map(|typ| IrBinder {
                var: self.program.binder(typ.clone()),
                typ,
            })
            .collect();
        let names: Vec<ValueId> = parameters.iter().map(|param| param.var).collect();
        let body = self.block(|t| body_fn(t, &names));
        let types = parameters.iter().map(|param| param.typ.clone());
        let return_type = body.typ.clone();
        let typ = FlatType::function(modifier, types, return_type.clone());
        let id = self.program.lambda_ids.borrow_mut().next();
        let (lambda, captured) = FlatLambda::closing(
            id,
            modifier,
            parameters,
            return_type,
            body.into_block(),
            |name| {
                let typ = self.typ(name);
                IrBinder {
                    var: self.program.binder(typ.clone()),
                    typ,
                }
            },
        );
        self.program.lambdas.borrow_mut().push(lambda);
        self.push(typ, FlatOp::Lambda(id, captured))
    }

    pub fn mut_lambda<F>(
        &self,
        parameters: impl IntoIterator<Item = FlatType>,
        body_fn: F,
    ) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, &[ValueId]) -> ValueId,
    {
        self.closure(IrModifier::Mut, parameters, body_fn)
    }

    pub fn event_handler(&self, body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId) -> ValueId {
        self.mut_lambda([], |t, _| body_fn(t))
    }

    pub fn apply(&self, closure: ValueId, args: impl IntoIterator<Item = ValueId>) -> ValueId {
        let args = args.into_iter().collect();
        let FlatType::Data(FlatDataType::Function(signature)) = self.typ(closure) else {
            panic!("{closure} is not a closure");
        };
        self.push(*signature.result, FlatOp::Apply(closure, args))
    }
}

#[must_use]
pub struct KeyedFor<'b, 'a> {
    builder: &'b FlatBlockBuilder<'a>,
    var: IrBinder<FlatDataType>,
    array: ValueId,
    key: FlatBlock,
}

impl KeyedFor<'_, '_> {
    pub fn body(self, body_fn: impl FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId) -> ValueId {
        let body = self
            .builder
            .block(|t| body_fn(t, self.var.var))
            .into_block();
        self.builder.push_html(FlatOp::For(FlatFor {
            var: Some(self.var),
            array: self.array,
            key: Some(self.key),
            body,
        }))
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
    subject_type: FlatType,
    arms: Vec<FlatMatchArm>,
    typ: Option<FlatType>,
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
        let body = self.builder.block(|t| body_fn(t, &bindings));
        self.typ.get_or_insert(body.typ.clone());
        self.arms.push(FlatMatchArm {
            pattern,
            body: body.into_block(),
        });
    }
}
