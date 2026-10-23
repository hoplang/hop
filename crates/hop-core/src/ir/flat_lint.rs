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
use std::collections::{HashMap, HashSet};

/// Panics unless every Html value is read at most once on each path and
/// never from a loop body it is bound outside of.
pub fn assert_html_affine(module: &FlatModule) {
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

/// Panics unless every binding has the type its op produces from the types
/// of its operands and every block produces the type its context expects.
/// A name is read only after it is bound.
pub fn assert_well_typed(module: &FlatModule, registry: &TypeRegistry) {
    let mut checker = Checker {
        registry,
        functions: &module.functions,
        types: HashMap::new(),
    };
    for function in &module.functions {
        for param in &function.parameters {
            checker.bind(&param.binder);
        }
        checker.block(&function.body, &function.return_type, &function.function);
    }
}

struct Checker<'a> {
    registry: &'a TypeRegistry,
    functions: &'a [FlatFunctionDeclaration],
    types: HashMap<ValueId, Type>,
}

impl Checker<'_> {
    fn bind(&mut self, binder: &IrBinder) {
        assert!(
            self.types.insert(binder.var, binder.typ.clone()).is_none(),
            "{binder} is bound twice"
        );
    }

    fn typ(&self, name: ValueId) -> &Type {
        self.types
            .get(&name)
            .unwrap_or_else(|| panic!("{name} is read before it is bound"))
    }

    fn data(&self, name: ValueId) -> Option<&DataType> {
        self.typ(name).as_data()
    }

    fn resolve<'t>(&'t self, typ: &'t Type) -> Option<ResolvedType<'t>> {
        self.registry.resolve(typ.as_data()?)
    }

    fn block(&mut self, block: &FlatBlock, expected: &Type, function: &IrFunction) {
        for binding in &block.bindings {
            let produced = self.op(&binding.op, &binding.binder.typ, function);
            assert!(
                produced.as_ref() == Some(&binding.binder.typ),
                "{function} binds {} to an op that produces {}",
                binding.binder,
                produced.map_or("no type".to_string(), |typ| typ.to_string())
            );
            self.bind(&binding.binder);
        }
        assert!(
            self.typ(block.result) == expected,
            "{function} has a block that produces {} where {expected} is expected",
            self.typ(block.result)
        );
    }

    fn op(&mut self, op: &FlatOp, declared: &Type, function: &IrFunction) -> Option<Type> {
        match op {
            FlatOp::Data(op) => self.data_op(op, declared),
            FlatOp::Call(callee, args) => {
                let callee = self
                    .functions
                    .iter()
                    .find(|declared| declared.function.id == callee.id)
                    .unwrap_or_else(|| panic!("{function} calls the undeclared {callee}"));
                let params = &callee.parameters;
                let typed = args.len() == params.len()
                    && args
                        .iter()
                        .zip(params)
                        .all(|(arg, param)| *self.typ(*arg) == param.binder.typ);
                typed.then(|| callee.return_type.clone())
            }
            FlatOp::Match(match_) => {
                let subject = self.typ(match_.subject).clone();
                for arm in &match_.arms {
                    self.pattern(&arm.pattern, &subject);
                    self.block(&arm.body, declared, function);
                }
                Some(declared.clone())
            }
            FlatOp::HtmlText(_) => Some(Type::Html),
            FlatOp::HtmlEscape(name) => (*self.typ(*name) == Type::STRING).then_some(Type::Html),
            FlatOp::HtmlConcat(parts) => parts
                .iter()
                .all(|part| *self.typ(*part) == Type::Html)
                .then_some(Type::Html),
            FlatOp::HtmlElement(element) => {
                let attributes = element
                    .attributes
                    .values()
                    .all(|attribute| match attribute {
                        IrAttribute::Value(name) => *self.typ(*name) == Type::STRING,
                        IrAttribute::Presence(name) => *self.typ(*name) == Type::BOOL,
                    });
                (attributes && *self.typ(element.children) == Type::Html).then_some(Type::Html)
            }
            FlatOp::HtmlFor(for_) => {
                let Some(DataType::Array(element)) = self.data(for_.array) else {
                    return None;
                };
                let element = Type::Data((**element).clone());
                if let Some(var) = &for_.var {
                    if var.typ != element {
                        return None;
                    }
                    self.bind(var);
                }
                self.block(&for_.body, &Type::Html, function);
                Some(Type::Html)
            }
        }
    }

    fn data_op(&self, op: &IrDataOp, declared: &Type) -> Option<Type> {
        let same = |left: ValueId, right: ValueId| {
            let typ = self.typ(left);
            (typ == self.typ(right)).then_some(typ)
        };
        let all = |names: &[ValueId], typ: Type| names.iter().all(|name| *self.typ(*name) == typ);
        let data = |name: ValueId| self.data(name).cloned();
        match op {
            IrDataOp::StringLiteral(_) => Some(Type::STRING),
            IrDataOp::IntLiteral(_) => Some(Type::INT),
            IrDataOp::FloatLiteral(_) => Some(Type::FLOAT),
            IrDataOp::BoolLiteral(_) => Some(Type::BOOL),
            IrDataOp::Array(elements) => match elements.first() {
                None => {
                    matches!(declared.as_data(), Some(DataType::Array(_))).then(|| declared.clone())
                }
                Some(first) => {
                    let element = data(*first)?;
                    all(elements, Type::Data(element.clone())).then(|| Type::array(element))
                }
            },
            IrDataOp::Tuple(elements) => elements
                .iter()
                .map(|element| data(*element))
                .collect::<Option<Vec<_>>>()
                .map(|elements| Type::Data(DataType::Tuple(elements))),
            IrDataOp::Record(fields) => {
                let Some(ResolvedType::Record {
                    fields: declared_fields,
                    ..
                }) = self.resolve(declared)
                else {
                    return None;
                };
                self.fields(fields, declared_fields)
                    .then(|| declared.clone())
            }
            IrDataOp::Enum(variant, fields) => {
                let Some(ResolvedType::Enum { variants, .. }) = self.resolve(declared) else {
                    return None;
                };
                let variant = variants.iter().find(|v| v.name == *variant)?;
                self.fields(fields, &variant.fields)
                    .then(|| declared.clone())
            }
            IrDataOp::OptionSome(inner) => data(*inner).map(Type::option),
            IrDataOp::OptionNone => {
                matches!(declared.as_data(), Some(DataType::Option(_))).then(|| declared.clone())
            }
            IrDataOp::StringConcat(parts) => all(parts, Type::STRING).then_some(Type::STRING),
            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right) => {
                let typ = same(*left, *right)?;
                typ.as_numeric_type().map(|_| typ.clone())
            }
            IrDataOp::Equals(left, right) => {
                same(*left, *right)?.as_equatable_type().map(|_| Type::BOOL)
            }
            IrDataOp::LessThan(left, right) | IrDataOp::LessThanOrEqual(left, right) => {
                same(*left, *right)?
                    .as_comparable_type()
                    .map(|_| Type::BOOL)
            }
            IrDataOp::ArrayFromRange(start, end) => {
                all(&[*start, *end], Type::INT).then(|| Type::array(DataType::Int))
            }
            IrDataOp::NumericNegation(operand) => {
                let typ = self.typ(*operand);
                typ.as_numeric_type().map(|_| typ.clone())
            }
            IrDataOp::BoolNegation(operand) => all(&[*operand], Type::BOOL).then_some(Type::BOOL),
            IrDataOp::ArrayLength(operand) => {
                matches!(data(*operand), Some(DataType::Array(_))).then_some(Type::INT)
            }
            IrDataOp::ArrayIsEmpty(operand) => {
                matches!(data(*operand), Some(DataType::Array(_))).then_some(Type::BOOL)
            }
            IrDataOp::StringIsEmpty(operand) => {
                all(&[*operand], Type::STRING).then_some(Type::BOOL)
            }
            IrDataOp::OptionIsSome(operand) | IrDataOp::OptionIsNone(operand) => {
                matches!(data(*operand), Some(DataType::Option(_))).then_some(Type::BOOL)
            }
            IrDataOp::IntToString(operand) => all(&[*operand], Type::INT).then_some(Type::STRING),
            IrDataOp::FloatToInt(operand) => all(&[*operand], Type::FLOAT).then_some(Type::INT),
            IrDataOp::IntToFloat(operand) => all(&[*operand], Type::INT).then_some(Type::FLOAT),
            IrDataOp::FieldAccess(object, field) => {
                let Some(ResolvedType::Record { fields, .. }) = self.resolve(self.typ(*object))
                else {
                    return None;
                };
                let field = fields.iter().find(|f| f.name == *field)?;
                Some(Type::Data(field.typ.clone()))
            }
            IrDataOp::TupleIndex(tuple, index) => {
                let Some(DataType::Tuple(elements)) = self.data(*tuple) else {
                    return None;
                };
                elements.get(*index).cloned().map(Type::Data)
            }
        }
    }

    fn fields(&self, given: &[(FieldName, ValueId)], declared: &[RecordField]) -> bool {
        given.len() == declared.len()
            && declared.iter().all(|field| {
                let values: Vec<ValueId> = given
                    .iter()
                    .filter(|(name, _)| *name == field.name)
                    .map(|(_, value)| *value)
                    .collect();
                values.len() == 1 && *self.typ(values[0]) == Type::Data(field.typ.clone())
            })
    }

    fn pattern(&mut self, pattern: &IrPattern, typ: &Type) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => {
                assert!(binder.typ == *typ, "{binder} binds a {typ}");
                self.bind(binder);
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                let mut arg_types: Vec<DataType> = Vec::new();
                let mut declared_fields: Vec<RecordField> = Vec::new();
                let matches = match (constructor, self.resolve(typ)) {
                    (Constructor::BoolTrue | Constructor::BoolFalse, Some(ResolvedType::Bool)) => {
                        true
                    }
                    (Constructor::OptionNone, Some(ResolvedType::Option(_))) => true,
                    (Constructor::OptionSome, Some(ResolvedType::Option(inner))) => {
                        arg_types.push(inner.clone());
                        true
                    }
                    (Constructor::Tuple, Some(ResolvedType::Tuple(elements))) => {
                        arg_types.extend(elements.iter().cloned());
                        true
                    }
                    (
                        Constructor::Record { type_name },
                        Some(ResolvedType::Record { name, fields }),
                    ) if type_name == name => {
                        declared_fields.extend(fields.iter().cloned());
                        true
                    }
                    (
                        Constructor::EnumVariant {
                            type_name,
                            variant_name,
                        },
                        Some(ResolvedType::Enum { name, variants }),
                    ) if type_name == name => {
                        match variants.iter().find(|v| v.name == *variant_name) {
                            Some(variant) => {
                                declared_fields.extend(variant.fields.iter().cloned());
                                true
                            }
                            None => false,
                        }
                    }
                    _ => false,
                };
                assert!(
                    matches && args.len() == arg_types.len(),
                    "a {typ} is matched against {}",
                    pattern.to_doc().pretty(80)
                );
                for (arg, arg_type) in args.iter().zip(arg_types) {
                    self.pattern(arg, &Type::Data(arg_type));
                }
                for field in fields {
                    let declared = declared_fields.get(field.index);
                    assert!(
                        declared.is_some_and(|declared| declared.name == field.name),
                        "a {typ} is matched against {}",
                        pattern.to_doc().pretty(80)
                    );
                    self.pattern(&field.pattern, &Type::Data(declared.unwrap().typ.clone()));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::flat_module_builder::FlatModuleBuilder;

    #[test]
    #[should_panic]
    fn should_reject_an_op_with_ill_typed_operands() {
        FlatModuleBuilder::new()
            .entrypoint("Test", [], "Html", |t| {
                let sum = t.add(t.int(1), t.str("one"));
                t.escape(t.int_to_string(sum))
            })
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_block_of_the_wrong_type() {
        FlatModuleBuilder::new()
            .entrypoint("Test", [], "Html", |t| t.int(1))
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_call_with_the_wrong_arguments() {
        FlatModuleBuilder::new()
            .function("show", [("n", "Int")], "Html", |t| {
                t.escape(t.int_to_string(t.param("n")))
            })
            .entrypoint("Test", [], "Html", |t| t.call("show", [t.str("one")]))
            .build();
    }
}
