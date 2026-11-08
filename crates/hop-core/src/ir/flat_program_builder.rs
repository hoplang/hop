use crate::document::CheapString;
use crate::hop::typing::{Constructor, Pattern, PatternField, Ty};
use crate::html::HtmlElementKind;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_lint::{self, FlatLintError};
use crate::ir::flat_program::{
    FlatEntrypoint, FlatFor, FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatProgram,
    FlatPureBlock, FlatPureOp,
};
use crate::ir::function_id::FunctionIdCounter;
use crate::ir::index_vec::IndexVec;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::ir_parameter::IrParameter;
use crate::ir::ir_type::{IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrType};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations};
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use std::cell::{Ref, RefCell};

pub struct FlatProgramBuilder {
    types: IrTypeDeclarations,
    function_ids: FunctionIdCounter,
    value_ids: RefCell<ValueIdCounter>,
    value_types: RefCell<Vec<IrType>>,
    functions: Vec<DeclaredFunction>,
    entrypoints: IndexVec<EntrypointId, FlatEntrypoint>,
}

impl Default for FlatProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl FlatProgramBuilder {
    pub fn new() -> Self {
        Self::with_types(IrTypeDeclarations::default())
    }

    pub fn with_types(types: IrTypeDeclarations) -> Self {
        FlatProgramBuilder {
            types,
            function_ids: FunctionIdCounter::new(),
            value_ids: RefCell::new(ValueIdCounter::new()),
            value_types: RefCell::new(Vec::new()),
            functions: Vec::new(),
            entrypoints: IndexVec::new(),
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
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId,
    ) -> Self {
        let name = FunctionName::parse(name).unwrap();
        let parameters: Vec<IrParameter<IrDataType>> = params
            .into_iter()
            .map(|(name, typ)| {
                let typ = self.types.ty(typ);
                let IrType::Data(IrFunctionDataType::Data(typ)) = typ else {
                    panic!("an entrypoint parameter must have a data type");
                };
                IrParameter {
                    name: AttributeName::parse(name).unwrap(),
                    binder: IrBinder {
                        var: self.binder(IrType::Data(IrFunctionDataType::Data(typ.clone()))),
                        typ,
                    },
                }
            })
            .collect();
        let block_parameters: Vec<_> = parameters.iter().map(|param| param.widen()).collect();
        let body = FlatBlockBuilder::build(&self, &block_parameters, body_fn).into_html();
        self.entrypoints.push(FlatEntrypoint {
            name,
            parameters,
            body,
        });
        self
    }

    pub fn declare<'p>(
        self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, Ty)>,
        return_type: Ty,
    ) -> Self {
        let params: Vec<(&str, IrType)> = params
            .into_iter()
            .map(|(name, typ)| (name, self.types.ty(typ)))
            .collect();
        let return_type = self.types.ty(return_type);
        self.declare_with_types(name, params, return_type)
    }

    pub fn declare_with_types<'p>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'p str, IrType)>,
        return_type: IrType,
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
        self.functions[index].body = Some(body);
        self
    }

    fn binder(&self, typ: IrType) -> ValueId {
        let name = self.value_ids.borrow_mut().next();
        let mut types = self.value_types.borrow_mut();
        assert_eq!(types.len(), name.index());
        types.push(typ);
        name
    }

    pub fn build(self) -> Result<FlatProgram, FlatLintError> {
        let functions = self
            .functions
            .into_iter()
            .map(|declared| match declared.return_type {
                IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => {
                    FlatFunction::Html(IrFunctionDeclaration {
                        function: declared.function,
                        parameters: declared.parameters,
                        return_type: IrHtmlType,
                        body: declared.body.unwrap().into_html(),
                    })
                }
                IrType::Data(IrFunctionDataType::Data(_) | IrFunctionDataType::Effect(_)) => {
                    let return_type = declared.return_type.as_result().unwrap();
                    let parameters = declared
                        .parameters
                        .into_iter()
                        .map(|param| {
                            param.into_function_data().unwrap_or_else(|| {
                                panic!("{} returns a value but takes Html", declared.function)
                            })
                        })
                        .collect();
                    FlatFunction::Pure(IrFunctionDeclaration {
                        function: declared.function,
                        parameters,
                        return_type,
                        body: declared.body.unwrap().into_pure(),
                    })
                }
                IrType::Data(IrFunctionDataType::Function(_))
                | IrType::Html(IrFunctionHtmlType::Function(_)) => {
                    panic!("{} returns a closure", declared.function)
                }
                IrType::Data(IrFunctionDataType::Cell(_)) => {
                    panic!("{} returns a cell", declared.function)
                }
            })
            .collect();
        let program = FlatProgram {
            types: self.types,
            entrypoints: self.entrypoints,
            functions,
            value_ids: self.value_ids.into_inner(),
        };
        flat_lint::lint(&program)?;
        Ok(program)
    }
}

pub struct DeclaredFunction {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter<IrType>>,
    pub return_type: IrType,
    body: Option<BuiltBlock>,
}

struct BuiltBlock {
    bindings: Vec<FlatHtmlBinding>,
    result: ValueId,
    typ: IrType,
}

impl BuiltBlock {
    fn into_pure(self) -> FlatPureBlock {
        IrBlock {
            bindings: (self.bindings.into_iter())
                .map(|binding| match binding {
                    FlatHtmlBinding::Pure(binding) => binding,
                    FlatHtmlBinding::Cell(..) => panic!("a Pure block binds a cell"),
                    FlatHtmlBinding::Track(..) => panic!("a Pure block tracks a cell"),
                    FlatHtmlBinding::Html(_) => panic!("a Pure block binds Html"),
                })
                .collect(),
            result: self.result,
        }
    }

    fn into_html(self) -> FlatHtmlBlock {
        IrBlock {
            bindings: self.bindings,
            result: self.result,
        }
    }
}

pub struct FlatBlockBuilder<'a> {
    program: &'a FlatProgramBuilder,
    parameters: &'a [IrParameter<IrType>],
    bindings: RefCell<Vec<FlatHtmlBinding>>,
}

impl<'a> FlatBlockBuilder<'a> {
    fn build(
        program: &'a FlatProgramBuilder,
        parameters: &'a [IrParameter<IrType>],
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

    fn typ(&self, name: ValueId) -> IrType {
        self.program.value_types.borrow()[name.index()].clone()
    }

    pub fn value_types(&self) -> Ref<'a, [IrType]> {
        Ref::map(self.program.value_types.borrow(), Vec::as_slice)
    }

    fn push(&self, typ: IrType, op: FlatPureOp) -> ValueId {
        let name = self.program.binder(typ.clone());
        let IrType::Data(typ) = typ else {
            panic!("an op of a value produces a value");
        };
        self.bindings
            .borrow_mut()
            .push(FlatHtmlBinding::Pure(IrBinding {
                binder: IrBinder { var: name, typ },
                op,
            }));
        name
    }

    fn push_html(&self, op: FlatHtmlOp) -> ValueId {
        self.push_html_typed(IrFunctionHtmlType::Html(IrHtmlType), op)
    }

    fn push_html_typed(&self, typ: IrFunctionHtmlType, op: FlatHtmlOp) -> ValueId {
        let name = self.program.binder(IrType::Html(typ.clone()));
        self.bindings
            .borrow_mut()
            .push(FlatHtmlBinding::Html(IrBinding {
                binder: IrBinder { var: name, typ },
                op,
            }));
        name
    }

    pub fn types(&self) -> &'a IrTypeDeclarations {
        &self.program.types
    }

    pub fn ty(&self, ty: Ty) -> IrType {
        self.program.types.ty(ty)
    }

    pub fn functions(&self) -> &'a [DeclaredFunction] {
        &self.program.functions
    }

    pub fn str(&self, s: &str) -> ValueId {
        self.push(
            IrType::STRING,
            FlatPureOp::Data(IrDataOp::StringLiteral(CheapString::new(s.to_string()))),
        )
    }

    pub fn int(&self, n: i32) -> ValueId {
        self.push(IrType::INT, FlatPureOp::Data(IrDataOp::IntLiteral(n)))
    }

    pub fn bool(&self, b: bool) -> ValueId {
        self.push(IrType::BOOL, FlatPureOp::Data(IrDataOp::BoolLiteral(b)))
    }

    pub fn float(&self, f: f64) -> ValueId {
        self.push(IrType::FLOAT, FlatPureOp::Data(IrDataOp::FloatLiteral(f)))
    }

    pub fn param(&self, name: &str) -> ValueId {
        self.parameters
            .iter()
            .find(|param| param.name.as_str() == name)
            .unwrap()
            .binder
            .var
    }

    pub fn eq(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::Equals(left, right)),
        )
    }

    pub fn lt(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::LessThan(left, right)),
        )
    }

    pub fn lte(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::LessThanOrEqual(left, right)),
        )
    }

    pub fn add(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            self.typ(left),
            FlatPureOp::Data(IrDataOp::NumericAdd(left, right)),
        )
    }

    pub fn sub(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            self.typ(left),
            FlatPureOp::Data(IrDataOp::NumericSubtract(left, right)),
        )
    }

    pub fn mul(&self, left: ValueId, right: ValueId) -> ValueId {
        self.push(
            self.typ(left),
            FlatPureOp::Data(IrDataOp::NumericMultiply(left, right)),
        )
    }

    pub fn not(&self, operand: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::BoolNegation(operand)),
        )
    }

    pub fn neg(&self, operand: ValueId) -> ValueId {
        self.push(
            self.typ(operand),
            FlatPureOp::Data(IrDataOp::NumericNegation(operand)),
        )
    }

    pub fn array(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let IrType::Data(IrFunctionDataType::Data(element_type)) = self.typ(elements[0]) else {
            panic!();
        };
        self.push(
            IrType::array(element_type),
            FlatPureOp::Data(IrDataOp::Array(elements)),
        )
    }

    pub fn empty_array(&self, element_type: IrType) -> ValueId {
        let IrType::Data(IrFunctionDataType::Data(element_type)) = element_type else {
            panic!();
        };
        self.push(
            IrType::array(element_type),
            FlatPureOp::Data(IrDataOp::Array(Vec::new())),
        )
    }

    pub fn array_from_range(&self, start: ValueId, end: ValueId) -> ValueId {
        self.push(
            IrType::array(IrDataType::Int),
            FlatPureOp::Data(IrDataOp::ArrayFromRange(start, end)),
        )
    }

    pub fn tuple(&self, elements: impl IntoIterator<Item = ValueId>) -> ValueId {
        let elements: Vec<ValueId> = elements.into_iter().collect();
        let element_types = elements
            .iter()
            .map(|element| match self.typ(*element) {
                IrType::Data(IrFunctionDataType::Data(element_type)) => element_type,
                typ => panic!("a tuple holds data, not {typ}"),
            })
            .collect();
        self.push(
            IrType::Data(IrFunctionDataType::Data(IrDataType::Tuple(element_types))),
            FlatPureOp::Data(IrDataOp::Tuple(elements)),
        )
    }

    pub fn tuple_index(&self, tuple: ValueId, index: usize) -> ValueId {
        let IrType::Data(IrFunctionDataType::Data(IrDataType::Tuple(elements))) = self.typ(tuple)
        else {
            panic!();
        };
        self.push(
            IrType::Data(IrFunctionDataType::Data(elements[index].clone())),
            FlatPureOp::Data(IrDataOp::TupleIndex(tuple, index)),
        )
    }

    pub fn int_to_string(&self, value: ValueId) -> ValueId {
        self.push(
            IrType::STRING,
            FlatPureOp::Data(IrDataOp::IntToString(value)),
        )
    }

    pub fn float_to_int(&self, value: ValueId) -> ValueId {
        self.push(IrType::INT, FlatPureOp::Data(IrDataOp::FloatToInt(value)))
    }

    pub fn int_to_float(&self, value: ValueId) -> ValueId {
        self.push(IrType::FLOAT, FlatPureOp::Data(IrDataOp::IntToFloat(value)))
    }

    pub fn record<'f>(
        &self,
        typ: IrType,
        fields: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            typ,
            FlatPureOp::Data(IrDataOp::Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (FieldName::parse(k).unwrap(), v))
                    .collect(),
            )),
        )
    }

    pub fn enum_variant_with_fields<'f>(
        &self,
        typ: IrType,
        variant_name: &str,
        field_values: impl IntoIterator<Item = (&'f str, ValueId)>,
    ) -> ValueId {
        self.push(
            typ,
            FlatPureOp::Data(IrDataOp::Enum(
                TypeName::parse(variant_name).unwrap(),
                field_values
                    .into_iter()
                    .map(|(k, v)| (FieldName::parse(k).unwrap(), v))
                    .collect(),
            )),
        )
    }

    pub fn some(&self, inner: ValueId) -> ValueId {
        let IrType::Data(IrFunctionDataType::Data(inner_type)) = self.typ(inner) else {
            panic!();
        };
        self.push(
            IrType::option(inner_type),
            FlatPureOp::Data(IrDataOp::OptionSome(inner)),
        )
    }

    pub fn none(&self, inner_type: IrType) -> ValueId {
        let IrType::Data(IrFunctionDataType::Data(inner_type)) = inner_type else {
            panic!();
        };
        self.push(
            IrType::option(inner_type),
            FlatPureOp::Data(IrDataOp::OptionNone),
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
        let typ = typ.unwrap();
        let arms = arms.into_iter();
        if let IrType::Html(html) = typ {
            let arms = arms
                .map(|arm| IrMatchArm {
                    pattern: arm.pattern,
                    body: arm.body.into_html(),
                })
                .collect();
            return self.push_html_typed(html, FlatHtmlOp::Match(IrMatch { subject, arms }));
        }
        let arms = arms
            .map(|arm| IrMatchArm {
                pattern: arm.pattern,
                body: arm.body.into_pure(),
            })
            .collect();
        self.push(typ, FlatPureOp::Match(IrMatch { subject, arms }))
    }

    fn resolve_pat(&self, typ: &IrType, pat: Pat, bindings: &mut ArmBindings) -> IrPattern {
        let constructor = |constructor: Constructor, args: Vec<IrPattern>| Pattern::Constructor {
            constructor,
            args,
            fields: Vec::new(),
        };
        match (pat, typ.as_data()) {
            (Pat::Wildcard, _) => Pattern::Wildcard,
            (Pat::Bind(name), _) => {
                let IrType::Data(IrFunctionDataType::Data(data)) = typ else {
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
            (Pat::Some(inner), Some(IrDataType::Option(inner_type))) => {
                let inner = self.resolve_pat(
                    &IrType::Data(IrFunctionDataType::Data((**inner_type).clone())),
                    *inner,
                    bindings,
                );
                constructor(Constructor::OptionSome, vec![inner])
            }
            (Pat::Tuple(elements), Some(IrDataType::Tuple(element_types)))
                if elements.len() == element_types.len() =>
            {
                let args = elements
                    .into_iter()
                    .zip(element_types)
                    .map(|(element, element_type)| {
                        self.resolve_pat(
                            &IrType::Data(IrFunctionDataType::Data(element_type.clone())),
                            element,
                            bindings,
                        )
                    })
                    .collect();
                constructor(Constructor::Tuple, args)
            }
            (Pat::Record(listed), Some(IrDataType::Record(name))) => {
                let fields = &self.types().record(name).unwrap().fields;
                Pattern::Constructor {
                    constructor: Constructor::Record {
                        type_name: name.clone(),
                    },
                    args: Vec::new(),
                    fields: self.resolve_fields(fields, listed, bindings),
                }
            }
            (Pat::Variant(variant, listed), Some(IrDataType::Enum(name))) => {
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
        declared: &[IrField],
        listed: Vec<(String, Pat)>,
        bindings: &mut ArmBindings,
    ) -> Vec<PatternField<IrBinder<IrDataType>>> {
        let mut patterns: Vec<(String, IrPattern)> = Vec::new();
        for (field_name, field_pat) in listed {
            assert!(!patterns.iter().any(|(listed, _)| *listed == field_name));
            let field = declared
                .iter()
                .find(|field| field.name.as_str() == field_name)
                .unwrap();
            let pattern = self.resolve_pat(
                &IrType::Data(IrFunctionDataType::Data(field.typ.clone())),
                field_pat,
                bindings,
            );
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
        let IrType::Data(IrFunctionDataType::Data(IrDataType::Record(name))) = self.typ(object)
        else {
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
            IrType::Data(IrFunctionDataType::Data(field.typ.clone())),
            FlatPureOp::Data(IrDataOp::FieldAccess(object, field.name.clone())),
        )
    }

    pub fn string_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push(
            IrType::STRING,
            FlatPureOp::Data(IrDataOp::StringConcat(parts.into_iter().collect())),
        )
    }

    pub fn array_length(&self, operand: ValueId) -> ValueId {
        self.push(
            IrType::INT,
            FlatPureOp::Data(IrDataOp::ArrayLength(operand)),
        )
    }

    pub fn array_is_empty(&self, operand: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::ArrayIsEmpty(operand)),
        )
    }

    pub fn array_get(&self, array: ValueId, index: ValueId) -> ValueId {
        let IrType::Data(IrFunctionDataType::Data(IrDataType::Array(element))) = self.typ(array)
        else {
            panic!();
        };
        self.push(
            IrType::option(*element),
            FlatPureOp::Data(IrDataOp::ArrayGet(array, index)),
        )
    }

    pub fn array_enumerate(&self, array: ValueId) -> ValueId {
        let IrType::Data(IrFunctionDataType::Data(IrDataType::Array(element))) = self.typ(array)
        else {
            panic!();
        };
        self.push(
            IrType::Data(IrFunctionDataType::Data(IrDataType::Array(Box::new(
                IrDataType::Tuple(vec![IrDataType::Int, *element]),
            )))),
            FlatPureOp::Data(IrDataOp::ArrayEnumerate(array)),
        )
    }

    pub fn string_is_empty(&self, operand: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::StringIsEmpty(operand)),
        )
    }

    pub fn option_is_some(&self, operand: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::OptionIsSome(operand)),
        )
    }

    pub fn option_is_none(&self, operand: ValueId) -> ValueId {
        self.push(
            IrType::BOOL,
            FlatPureOp::Data(IrDataOp::OptionIsNone(operand)),
        )
    }

    pub fn text(&self, content: &str) -> ValueId {
        assert!(!content.contains(['<', '{', '}']));
        self.push_html(FlatHtmlOp::Text(CheapString::new(content.to_string())))
    }

    pub fn element(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, IrAttribute)>,
        children: ValueId,
    ) -> ValueId {
        self.push_element(tag, attributes, children, None)
    }

    pub fn element_on_click(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, IrAttribute)>,
        event_handler: ValueId,
        children: ValueId,
    ) -> ValueId {
        self.push_element(tag, attributes, children, Some(event_handler))
    }

    fn push_element(
        &self,
        tag: &str,
        attributes: impl IntoIterator<Item = (AttributeName, IrAttribute)>,
        children: ValueId,
        on_click: Option<ValueId>,
    ) -> ValueId {
        self.push_html(FlatHtmlOp::Element(IrHtmlElement {
            element: HtmlElementKind::parse(tag).unwrap(),
            attributes: attributes.into_iter().collect(),
            children,
            on_click,
        }))
    }

    pub fn cell(&self, initial: ValueId) -> ValueId {
        let typ = self.typ(initial);
        let IrType::Data(IrFunctionDataType::Data(typ)) = typ else {
            panic!("a cell holds data, not {typ}");
        };
        let typ = IrFunctionDataType::Cell(typ);
        let name = self.program.binder(IrType::Data(typ.clone()));
        self.bindings
            .borrow_mut()
            .push(FlatHtmlBinding::Cell(IrBinder { var: name, typ }, initial));
        name
    }

    pub fn track(&self, cell: ValueId) -> ValueId {
        let typ = self.cell_of(cell);
        let name = self
            .program
            .binder(IrType::Data(IrFunctionDataType::Data(typ.clone())));
        self.bindings
            .borrow_mut()
            .push(FlatHtmlBinding::Track(IrBinder { var: name, typ }, cell));
        name
    }

    pub fn cell_write(&self, cell: ValueId, value: ValueId) -> ValueId {
        self.push(IrType::EFFECT, FlatPureOp::CellWrite(cell, value))
    }

    pub fn cell_read(&self, cell: ValueId) -> ValueId {
        let typ = IrType::Data(IrFunctionDataType::Effect(self.cell_of(cell)));
        self.push(typ, FlatPureOp::CellRead(cell))
    }

    fn cell_of(&self, cell: ValueId) -> IrDataType {
        let IrType::Data(IrFunctionDataType::Cell(typ)) = self.typ(cell) else {
            panic!("{cell} is not a cell");
        };
        typ
    }

    pub fn effect_pure(&self, value: ValueId) -> ValueId {
        self.push(self.effect_of(value), FlatPureOp::EffectPure(value))
    }

    pub fn effect_bind(&self, effect: ValueId, continuation: ValueId) -> ValueId {
        let IrType::Data(IrFunctionDataType::Function(signature)) = self.typ(continuation) else {
            panic!("a bind continues with a closure");
        };
        self.push(
            IrType::result(&signature.result),
            FlatPureOp::EffectBind(effect, continuation),
        )
    }

    fn effect_of(&self, value: ValueId) -> IrType {
        let IrType::Data(IrFunctionDataType::Data(typ)) = self.typ(value) else {
            panic!("an effect gives data");
        };
        IrType::Data(IrFunctionDataType::Effect(typ))
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
        self.push_html(FlatHtmlOp::Escape(expr))
    }

    pub fn html_concat(&self, parts: impl IntoIterator<Item = ValueId>) -> ValueId {
        self.push_html(FlatHtmlOp::Concat(parts.into_iter().collect()))
    }

    pub fn html_for<F>(&self, array: ValueId, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId,
    {
        let IrType::Data(IrFunctionDataType::Data(IrDataType::Array(element_type))) =
            self.typ(array)
        else {
            panic!();
        };
        let typ = *element_type;
        let var = self
            .program
            .binder(IrType::Data(IrFunctionDataType::Data(typ.clone())));
        let body = self.block(|t| body_fn(t, var)).into_html();
        self.push_html(FlatHtmlOp::For(FlatFor {
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
        let IrType::Data(IrFunctionDataType::Data(IrDataType::Array(element_type))) =
            self.typ(array)
        else {
            panic!();
        };
        let typ = *element_type;
        let var = self
            .program
            .binder(IrType::Data(IrFunctionDataType::Data(typ.clone())));
        let key = self.block(|t| key_fn(t, var)).into_pure();
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
        let body = self.block(body_fn).into_html();
        self.push_html(FlatHtmlOp::For(FlatFor {
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
        let function = callee.function.clone();
        let args = args.into_iter().collect();
        if callee.return_type == IrType::HTML {
            return self.push_html(FlatHtmlOp::Call(function, args));
        }
        self.push(callee.return_type.clone(), FlatPureOp::Call(function, args))
    }

    pub fn lambda<F>(&self, parameters: impl IntoIterator<Item = IrType>, body_fn: F) -> ValueId
    where
        F: FnOnce(&FlatBlockBuilder<'_>, &[ValueId]) -> ValueId,
    {
        let parameters: Vec<IrBinder<IrType>> = parameters
            .into_iter()
            .map(|typ| IrBinder {
                var: self.program.binder(typ.clone()),
                typ,
            })
            .collect();
        let names: Vec<ValueId> = parameters.iter().map(|param| param.var).collect();
        let body = self.block(|t| body_fn(t, &names));
        let types = parameters.iter().map(|param| param.typ.clone());
        match body.typ.clone() {
            IrType::Html(IrFunctionHtmlType::Html(IrHtmlType)) => {
                let IrType::Html(typ) = IrType::html_function(types) else {
                    unreachable!("html_function gives the type of Html")
                };
                let lambda = IrLambda {
                    parameters,
                    body: body.into_html(),
                };
                self.push_html_typed(typ, FlatHtmlOp::Lambda(lambda))
            }
            result => {
                let typ = IrType::data_function(types, result);
                let parameters = parameters
                    .into_iter()
                    .map(|param| match param.typ {
                        IrType::Data(typ) => IrBinder {
                            var: param.var,
                            typ,
                        },
                        IrType::Html(_) => unreachable!("data_function takes values"),
                    })
                    .collect();
                let lambda = IrLambda {
                    parameters,
                    body: body.into_pure(),
                };
                self.push(typ, FlatPureOp::Lambda(lambda))
            }
        }
    }

    pub fn event_handler(&self, body_fn: impl FnOnce(&FlatBlockBuilder<'_>) -> ValueId) -> ValueId {
        self.lambda([], |t, _| body_fn(t))
    }

    pub fn apply(&self, closure: ValueId, args: impl IntoIterator<Item = ValueId>) -> ValueId {
        let args = args.into_iter().collect();
        match self.typ(closure) {
            IrType::Data(IrFunctionDataType::Function(signature)) => self.push(
                IrType::result(&signature.result),
                FlatPureOp::Apply(closure, args),
            ),
            IrType::Html(IrFunctionHtmlType::Function(_)) => {
                self.push_html(FlatHtmlOp::Apply(closure, args))
            }
            typ => panic!("{closure} is a {typ}, not a closure"),
        }
    }
}

#[must_use]
pub struct KeyedFor<'b, 'a> {
    builder: &'b FlatBlockBuilder<'a>,
    var: IrBinder<IrDataType>,
    array: ValueId,
    key: FlatPureBlock,
}

impl KeyedFor<'_, '_> {
    pub fn body(self, body_fn: impl FnOnce(&FlatBlockBuilder<'_>, ValueId) -> ValueId) -> ValueId {
        let body = self.builder.block(|t| body_fn(t, self.var.var)).into_html();
        self.builder.push_html(FlatHtmlOp::For(FlatFor {
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
    subject_type: IrType,
    arms: Vec<IrMatchArm<BuiltBlock>>,
    typ: Option<IrType>,
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
        self.arms.push(IrMatchArm { pattern, body });
    }
}
