use crate::hop::typing::{
    Constructor, DataType, Pattern, RecordField, ResolvedType, Type, TypeRegistry,
};
use crate::ir::flat_module::{FlatBlock, FlatFunctionDeclaration, FlatModule, FlatOp};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_match::IrPattern;
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;

/// Panics unless every Html value is read at most once on each path and
/// never from a loop body it is bound outside of.
///
/// Sets of names are vectors of flags indexed by the name's number, so
/// that forking the scope at a match is a copy and joining it is a loop.
pub fn assert_html_affine(module: &FlatModule) {
    let mut html = vec![false; module.value_ids.count()];
    for function in &module.functions {
        for param in &function.parameters {
            if param.binder.typ == Type::Html {
                html[param.binder.var.index()] = true;
            }
        }
        collect_html(&function.body, &mut html);
    }
    for function in &module.functions {
        let mut scope = vec![false; html.len()];
        for param in &function.parameters {
            if html[param.binder.var.index()] {
                scope[param.binder.var.index()] = true;
            }
        }
        check_block(&function.body, scope, &html, &function.function);
    }
}

fn collect_html(block: &FlatBlock, html: &mut [bool]) {
    for binding in &block.bindings {
        if binding.binder.typ == Type::Html {
            html[binding.binder.var.index()] = true;
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
    mut scope: Vec<bool>,
    html: &[bool],
    function: &IrFunction,
) -> Vec<bool> {
    for binding in &block.bindings {
        binding
            .op
            .for_each_operand(&mut |operand| read(operand, &mut scope, html, function));
        match &binding.op {
            FlatOp::Match(match_) => {
                let mut after = scope.clone();
                for arm in &match_.arms {
                    let inner = check_block(&arm.body, scope.clone(), html, function);
                    for (name, live) in after.iter_mut().zip(&inner) {
                        *name = *name && *live;
                    }
                }
                scope = after;
            }
            FlatOp::HtmlFor(for_) => {
                check_block(&for_.body, vec![false; html.len()], html, function);
            }
            _ => {}
        }
        if html[binding.binder.var.index()] {
            scope[binding.binder.var.index()] = true;
        }
    }
    read(block.result, &mut scope, html, function);
    scope
}

fn read(name: ValueId, scope: &mut [bool], html: &[bool], function: &IrFunction) {
    if html[name.index()] {
        assert!(
            std::mem::replace(&mut scope[name.index()], false),
            "{function} reads the Html value {name} when the path has used it or in a loop body it is bound outside of"
        );
    }
}

/// Panics unless every binding has the type its op produces from the types
/// of its operands and every block produces the type its context expects.
/// A name is read only after it is bound.
pub fn assert_well_typed(module: &FlatModule, registry: &TypeRegistry) {
    let mut checker = Checker {
        registry,
        functions: &module.functions,
        types: vec![None; module.value_ids.count()],
    };
    for function in &module.functions {
        for param in &function.parameters {
            checker.bind(&param.binder);
        }
        checker.block(&function.body, &function.return_type, &function.function);
    }
}

/// The types are borrowed from the binders in the module and indexed by
/// the name's number, so checking neither hashes nor clones a type.
struct Checker<'a> {
    registry: &'a TypeRegistry,
    functions: &'a [FlatFunctionDeclaration],
    types: Vec<Option<&'a Type>>,
}

impl<'a> Checker<'a> {
    fn bind(&mut self, binder: &'a IrBinder) {
        assert!(
            self.types[binder.var.index()]
                .replace(&binder.typ)
                .is_none(),
            "{binder} is bound twice"
        );
    }

    fn typ(&self, name: ValueId) -> &'a Type {
        self.types[name.index()].unwrap_or_else(|| panic!("{name} is read before it is bound"))
    }

    fn data(&self, name: ValueId) -> Option<&'a DataType> {
        self.typ(name).as_data()
    }

    fn resolve<'t>(&self, typ: &'t Type) -> Option<ResolvedType<'t>>
    where
        'a: 't,
    {
        self.registry.resolve(typ.as_data()?)
    }

    fn block(&mut self, block: &'a FlatBlock, expected: &Type, function: &IrFunction) {
        for binding in &block.bindings {
            assert!(
                self.op(&binding.op, &binding.binder.typ, function),
                "{function} binds {} to an op that does not produce that type",
                binding.binder
            );
            self.bind(&binding.binder);
        }
        assert!(
            self.typ(block.result) == expected,
            "{function} has a block that produces {} where {expected} is expected",
            self.typ(block.result)
        );
    }

    /// Whether `op` produces a `declared` from the types of its operands.
    fn op(&mut self, op: &'a FlatOp, declared: &Type, function: &IrFunction) -> bool {
        match op {
            FlatOp::Data(op) => self.data_op(op, declared),
            FlatOp::Call(callee, args) => {
                let callee = self
                    .functions
                    .iter()
                    .find(|declared| declared.function.id == callee.id)
                    .unwrap_or_else(|| panic!("{function} calls the undeclared {callee}"));
                let params = &callee.parameters;
                args.len() == params.len()
                    && args
                        .iter()
                        .zip(params)
                        .all(|(arg, param)| *self.typ(*arg) == param.binder.typ)
                    && callee.return_type == *declared
            }
            FlatOp::Match(match_) => {
                let subject = self.typ(match_.subject);
                for arm in &match_.arms {
                    self.pattern(&arm.pattern, subject);
                    self.block(&arm.body, declared, function);
                }
                true
            }
            FlatOp::HtmlText(_) => *declared == Type::Html,
            FlatOp::HtmlEscape(name) => *self.typ(*name) == Type::STRING && *declared == Type::Html,
            FlatOp::HtmlConcat(parts) => {
                parts.iter().all(|part| *self.typ(*part) == Type::Html) && *declared == Type::Html
            }
            FlatOp::HtmlElement(element) => {
                let attributes = element
                    .attributes
                    .values()
                    .all(|attribute| match attribute {
                        IrAttribute::Value(name) => *self.typ(*name) == Type::STRING,
                        IrAttribute::Presence(name) => *self.typ(*name) == Type::BOOL,
                    });
                attributes && *self.typ(element.children) == Type::Html && *declared == Type::Html
            }
            FlatOp::HtmlFor(for_) => {
                let Some(DataType::Array(element)) = self.data(for_.array) else {
                    return false;
                };
                if let Some(var) = &for_.var {
                    if var.typ.as_data() != Some(element) {
                        return false;
                    }
                    self.bind(var);
                }
                self.block(&for_.body, &Type::Html, function);
                *declared == Type::Html
            }
        }
    }

    fn data_op(&self, op: &IrDataOp, declared: &Type) -> bool {
        let same = |left: ValueId, right: ValueId| {
            let typ = self.typ(left);
            (typ == self.typ(right)).then_some(typ)
        };
        let all = |names: &[ValueId], typ: Type| names.iter().all(|name| *self.typ(*name) == typ);
        let data = |name: ValueId| self.data(name);
        let declared_data = declared.as_data();
        match op {
            IrDataOp::StringLiteral(_) => *declared == Type::STRING,
            IrDataOp::IntLiteral(_) => *declared == Type::INT,
            IrDataOp::FloatLiteral(_) => *declared == Type::FLOAT,
            IrDataOp::BoolLiteral(_) => *declared == Type::BOOL,
            IrDataOp::Array(elements) => match declared_data {
                Some(DataType::Array(element)) => {
                    elements.iter().all(|name| data(*name) == Some(&**element))
                }
                _ => false,
            },
            IrDataOp::Tuple(elements) => match declared_data {
                Some(DataType::Tuple(declared)) => {
                    elements.len() == declared.len()
                        && elements
                            .iter()
                            .zip(declared)
                            .all(|(name, typ)| data(*name) == Some(typ))
                }
                _ => false,
            },
            IrDataOp::Record(fields) => match self.resolve(declared) {
                Some(ResolvedType::Record {
                    fields: declared_fields,
                    ..
                }) => self.fields(fields, declared_fields),
                _ => false,
            },
            IrDataOp::Enum(variant, fields) => match self.resolve(declared) {
                Some(ResolvedType::Enum { variants, .. }) => variants
                    .iter()
                    .find(|v| v.name == *variant)
                    .is_some_and(|variant| self.fields(fields, &variant.fields)),
                _ => false,
            },
            IrDataOp::OptionSome(inner) => match declared_data {
                Some(DataType::Option(element)) => data(*inner) == Some(&**element),
                _ => false,
            },
            IrDataOp::OptionNone => matches!(declared_data, Some(DataType::Option(_))),
            IrDataOp::StringConcat(parts) => all(parts, Type::STRING) && *declared == Type::STRING,
            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right) => same(*left, *right)
                .is_some_and(|typ| typ.as_numeric_type().is_some() && typ == declared),
            IrDataOp::Equals(left, right) => {
                same(*left, *right).is_some_and(|typ| typ.as_equatable_type().is_some())
                    && *declared == Type::BOOL
            }
            IrDataOp::LessThan(left, right) | IrDataOp::LessThanOrEqual(left, right) => {
                same(*left, *right).is_some_and(|typ| typ.as_comparable_type().is_some())
                    && *declared == Type::BOOL
            }
            IrDataOp::ArrayFromRange(start, end) => {
                all(&[*start, *end], Type::INT)
                    && matches!(declared_data, Some(DataType::Array(element)) if **element == DataType::Int)
            }
            IrDataOp::NumericNegation(operand) => {
                let typ = self.typ(*operand);
                typ.as_numeric_type().is_some() && typ == declared
            }
            IrDataOp::BoolNegation(operand) => {
                all(&[*operand], Type::BOOL) && *declared == Type::BOOL
            }
            IrDataOp::ArrayLength(operand) => {
                matches!(data(*operand), Some(DataType::Array(_))) && *declared == Type::INT
            }
            IrDataOp::ArrayIsEmpty(operand) => {
                matches!(data(*operand), Some(DataType::Array(_))) && *declared == Type::BOOL
            }
            IrDataOp::StringIsEmpty(operand) => {
                all(&[*operand], Type::STRING) && *declared == Type::BOOL
            }
            IrDataOp::OptionIsSome(operand) | IrDataOp::OptionIsNone(operand) => {
                matches!(data(*operand), Some(DataType::Option(_))) && *declared == Type::BOOL
            }
            IrDataOp::IntToString(operand) => {
                all(&[*operand], Type::INT) && *declared == Type::STRING
            }
            IrDataOp::FloatToInt(operand) => {
                all(&[*operand], Type::FLOAT) && *declared == Type::INT
            }
            IrDataOp::IntToFloat(operand) => {
                all(&[*operand], Type::INT) && *declared == Type::FLOAT
            }
            IrDataOp::FieldAccess(object, field) => match self.resolve(self.typ(*object)) {
                Some(ResolvedType::Record { fields, .. }) => fields
                    .iter()
                    .find(|f| f.name == *field)
                    .is_some_and(|field| Some(&field.typ) == declared_data),
                _ => false,
            },
            IrDataOp::TupleIndex(tuple, index) => match self.data(*tuple) {
                Some(DataType::Tuple(elements)) => elements.get(*index) == declared_data,
                _ => false,
            },
        }
    }

    fn fields(&self, given: &[(FieldName, ValueId)], declared: &[RecordField]) -> bool {
        given.len() == declared.len()
            && declared.iter().all(|field| {
                let mut values = given
                    .iter()
                    .filter(|(name, _)| *name == field.name)
                    .map(|(_, value)| *value);
                match (values.next(), values.next()) {
                    (Some(value), None) => self.typ(value).as_data() == Some(&field.typ),
                    _ => false,
                }
            })
    }

    fn pattern(&mut self, pattern: &'a IrPattern, typ: &Type) {
        match (pattern, typ) {
            (Pattern::Wildcard, _) => {}
            (Pattern::Binding(binder), _) => {
                assert!(binder.typ == *typ, "{binder} binds a {typ}");
                self.bind(binder);
            }
            (Pattern::Constructor { .. }, Type::Data(data)) => self.data_pattern(pattern, data),
            (Pattern::Constructor { .. }, Type::Html) => {
                panic!("a {typ} is matched against {}", pattern.to_doc().pretty(80))
            }
        }
    }

    fn data_pattern(&mut self, pattern: &'a IrPattern, typ: &DataType) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => {
                assert!(binder.typ.as_data() == Some(typ), "{binder} binds a {typ}");
                self.bind(binder);
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                let registry = self.registry;
                let resolved: Option<(&[DataType], &[RecordField])> =
                    match (constructor, registry.resolve(typ)) {
                        (
                            Constructor::BoolTrue | Constructor::BoolFalse,
                            Some(ResolvedType::Bool),
                        ) => Some((&[], &[])),
                        (Constructor::OptionNone, Some(ResolvedType::Option(_))) => {
                            Some((&[], &[]))
                        }
                        (Constructor::OptionSome, Some(ResolvedType::Option(inner))) => {
                            Some((std::slice::from_ref(inner), &[]))
                        }
                        (Constructor::Tuple, Some(ResolvedType::Tuple(elements))) => {
                            Some((elements, &[]))
                        }
                        (
                            Constructor::Record { type_name },
                            Some(ResolvedType::Record { name, fields }),
                        ) if type_name == name => Some((&[], fields)),
                        (
                            Constructor::EnumVariant {
                                type_name,
                                variant_name,
                            },
                            Some(ResolvedType::Enum { name, variants }),
                        ) if type_name == name => variants
                            .iter()
                            .find(|v| v.name == *variant_name)
                            .map(|variant| (&[][..], &variant.fields[..])),
                        _ => None,
                    };
                let (arg_types, declared_fields) = resolved
                    .filter(|(arg_types, _)| arg_types.len() == args.len())
                    .unwrap_or_else(|| {
                        panic!("a {typ} is matched against {}", pattern.to_doc().pretty(80))
                    });
                for (arg, arg_type) in args.iter().zip(arg_types) {
                    self.data_pattern(arg, arg_type);
                }
                for field in fields {
                    let declared = declared_fields
                        .get(field.index)
                        .filter(|declared| declared.name == field.name)
                        .unwrap_or_else(|| {
                            panic!("a {typ} is matched against {}", pattern.to_doc().pretty(80))
                        });
                    self.data_pattern(&field.pattern, &declared.typ);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::hop::typing::Ty;
    use crate::ir::flat_module_builder::FlatModuleBuilder;

    #[test]
    #[should_panic]
    fn should_reject_an_op_with_ill_typed_operands() {
        FlatModuleBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let sum = t.add(t.int(1), t.str("one"));
                t.escape(t.int_to_string(sum))
            })
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_block_of_the_wrong_type() {
        FlatModuleBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| t.int(1))
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_call_with_the_wrong_arguments() {
        FlatModuleBuilder::new()
            .function("show", [("n", Ty::Int)], Ty::Html, |t| {
                t.escape(t.int_to_string(t.param("n")))
            })
            .entrypoint("Test", [], Ty::Html, |t| t.call("show", [t.str("one")]))
            .build();
    }
}
