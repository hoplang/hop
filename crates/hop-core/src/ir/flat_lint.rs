use std::fmt;

use crate::hop::typing::{
    Constructor, DataType, Pattern, RecordField, ResolvedType, Type, TypeRegistry, compile_arms,
};
use crate::ir::flat_module::{
    FlatDataBinding, FlatDataBlock, FlatDataOp, FlatFunctionDeclaration, FlatHtmlBlock, FlatHtmlOp,
    FlatModule,
};
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_match::{IrMatch, IrPattern};
use crate::ir::ir_parameter::IrParameter;
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;

/// Panics unless every binding has the type its op produces from the types
/// of its operands and every block produces the type its context expects.
/// A name is read only after it is bound. Every value of a match subject's
/// type matches some arm, and each arm matches some value that no arm
/// before it matches.
pub fn assert_well_typed(module: &FlatModule, registry: &TypeRegistry) {
    let mut checker = Checker {
        registry,
        functions: &module.functions,
        types: vec![None; module.value_ids.count()],
    };
    for function in &module.functions {
        match function {
            FlatFunctionDeclaration::Data(data) => {
                for param in &data.parameters {
                    checker.bind(param.binder.var, BoundType::Data(&param.binder.typ));
                }
                checker.data_block(&data.body, &data.return_type, function.function());
            }
            FlatFunctionDeclaration::Html(html) => {
                for param in &html.parameters {
                    checker.bind(param.binder.var, BoundType::of(&param.binder.typ));
                }
                checker.html_block(&html.body, function.function());
            }
        }
    }
}

/// The types are borrowed from the binders in the module and indexed by
/// the name's number, so checking neither hashes nor clones a type.
struct Checker<'a> {
    registry: &'a TypeRegistry,
    functions: &'a [FlatFunctionDeclaration],
    types: Vec<Option<BoundType<'a>>>,
}

/// The type of a name as its binder gives it, a Type for a parameter, a
/// DataType for a value and Html for Html.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BoundType<'a> {
    Data(&'a DataType),
    Html,
}

impl<'a> BoundType<'a> {
    fn of(typ: &'a Type) -> Self {
        match typ {
            Type::Data(data) => BoundType::Data(data),
            Type::Html(_) => BoundType::Html,
        }
    }

    fn is(self, typ: &Type) -> bool {
        self == BoundType::of(typ)
    }
}

impl fmt::Display for BoundType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundType::Data(data) => data.fmt(f),
            BoundType::Html => Type::HTML.fmt(f),
        }
    }
}

impl<'a> Checker<'a> {
    fn bind(&mut self, name: ValueId, typ: BoundType<'a>) {
        assert!(
            self.types[name.index()].replace(typ).is_none(),
            "{name}: {typ} is bound twice"
        );
    }

    fn typ(&self, name: ValueId) -> BoundType<'a> {
        self.types[name.index()].unwrap_or_else(|| panic!("{name} is read before it is bound"))
    }

    fn data(&self, name: ValueId) -> Option<&'a DataType> {
        match self.typ(name) {
            BoundType::Data(data) => Some(data),
            BoundType::Html => None,
        }
    }

    fn data_block(&mut self, block: &'a FlatDataBlock, expected: &DataType, function: &IrFunction) {
        self.data_bindings(&block.bindings, function);
        assert!(
            self.typ(block.result) == BoundType::Data(expected),
            "{function} has a block that produces {} where {expected} is expected",
            self.typ(block.result)
        );
    }

    fn html_block(&mut self, block: &'a FlatHtmlBlock, function: &IrFunction) {
        self.data_bindings(&block.bindings, function);
        for binding in &block.html {
            assert!(
                self.html_op(&binding.op, function),
                "{function} binds {} to an op that does not produce Html",
                binding.binder
            );
            self.bind(binding.binder.var, BoundType::Html);
        }
        assert!(
            self.typ(block.result) == BoundType::Html,
            "{function} has a block that produces {} where Html is expected",
            self.typ(block.result)
        );
    }

    fn data_bindings(&mut self, bindings: &'a [FlatDataBinding], function: &IrFunction) {
        for binding in bindings {
            assert!(
                self.flat_data_op(&binding.op, &binding.binder.typ, function),
                "{function} binds {} to an op that does not produce that type",
                binding.binder
            );
            self.bind(binding.binder.var, BoundType::Data(&binding.binder.typ));
        }
    }

    /// The declaration of the function a call in `function` calls.
    fn callee(&self, callee: &IrFunction, function: &IrFunction) -> &'a FlatFunctionDeclaration {
        self.functions
            .iter()
            .find(|declared| declared.function().id == callee.id)
            .unwrap_or_else(|| panic!("{function} calls the undeclared {callee}"))
    }

    /// Whether the arguments follow the parameters, one of its type for each.
    fn args(&self, args: &[ValueId], params: &[IrParameter<Type>]) -> bool {
        args.len() == params.len()
            && args
                .iter()
                .zip(params)
                .all(|(arg, param)| self.typ(*arg).is(&param.binder.typ))
    }

    /// Whether `op` produces a `declared` from the types of its operands.
    fn flat_data_op(
        &mut self,
        op: &'a FlatDataOp,
        declared: &DataType,
        function: &IrFunction,
    ) -> bool {
        match op {
            FlatDataOp::Data(op) => self.data_op(op, declared),
            FlatDataOp::Call(callee, args) => {
                let callee = self.callee(callee, function);
                let returns_declared = match callee {
                    FlatFunctionDeclaration::Data(callee) => callee.return_type == *declared,
                    FlatFunctionDeclaration::Html(_) => false,
                };
                returns_declared && self.args(args, &callee.parameters())
            }
            FlatDataOp::Match(match_) => {
                let subject = self.typ(match_.subject);
                for arm in &match_.arms {
                    self.pattern(&arm.pattern, subject);
                    self.data_block(&arm.body, declared, function);
                }
                self.arms(match_, function);
                true
            }
        }
    }

    /// Whether `op` produces Html from the types of its operands.
    fn html_op(&mut self, op: &'a FlatHtmlOp, function: &IrFunction) -> bool {
        match op {
            FlatHtmlOp::Call(callee, args) => {
                let callee = self.callee(callee, function);
                matches!(callee, FlatFunctionDeclaration::Html(_))
                    && self.args(args, &callee.parameters())
            }
            FlatHtmlOp::Match(match_) => {
                let subject = self.typ(match_.subject);
                for arm in &match_.arms {
                    self.pattern(&arm.pattern, subject);
                    self.html_block(&arm.body, function);
                }
                self.arms(match_, function);
                true
            }
            FlatHtmlOp::Text(_) => true,
            FlatHtmlOp::Escape(name) => self.typ(*name).is(&Type::STRING),
            FlatHtmlOp::Concat(parts) => parts.iter().all(|part| self.typ(*part).is(&Type::HTML)),
            FlatHtmlOp::Element(element) => {
                let attributes = element
                    .attributes
                    .values()
                    .all(|attribute| match attribute {
                        IrAttribute::Value(name) => self.typ(*name).is(&Type::STRING),
                        IrAttribute::Presence(name) => self.typ(*name).is(&Type::BOOL),
                    });
                attributes && self.typ(element.children).is(&Type::HTML)
            }
            FlatHtmlOp::For(for_) => {
                let Some(DataType::Array(element)) = self.data(for_.array) else {
                    return false;
                };
                if let Some(var) = &for_.var {
                    if var.typ != **element {
                        return false;
                    }
                    self.bind(var.var, BoundType::Data(&var.typ));
                }
                self.html_block(&for_.body, function);
                true
            }
        }
    }

    fn data_op(&self, op: &IrDataOp, declared: &DataType) -> bool {
        let same = |left: ValueId, right: ValueId| {
            let typ = self.data(left)?;
            (Some(typ) == self.data(right)).then_some(typ)
        };
        let all = |names: &[ValueId], typ: Type| names.iter().all(|name| self.typ(*name).is(&typ));
        let data = |name: ValueId| self.data(name);
        let declared_data = Some(declared);
        match op {
            IrDataOp::StringLiteral(_) => *declared == DataType::String,
            IrDataOp::IntLiteral(_) => *declared == DataType::Int,
            IrDataOp::FloatLiteral(_) => *declared == DataType::Float,
            IrDataOp::BoolLiteral(_) => *declared == DataType::Bool,
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
            IrDataOp::Record(fields) => match self.registry.resolve(declared) {
                Some(ResolvedType::Record {
                    fields: declared_fields,
                    ..
                }) => self.fields(fields, declared_fields),
                _ => false,
            },
            IrDataOp::Enum(variant, fields) => match self.registry.resolve(declared) {
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
            IrDataOp::StringConcat(parts) => {
                all(parts, Type::STRING) && *declared == DataType::String
            }
            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right) => same(*left, *right)
                .is_some_and(|typ| typ.as_numeric_type().is_some() && Some(typ) == declared_data),
            IrDataOp::Equals(left, right) => {
                same(*left, *right).is_some_and(|typ| typ.as_equatable_type().is_some())
                    && *declared == DataType::Bool
            }
            IrDataOp::LessThan(left, right) | IrDataOp::LessThanOrEqual(left, right) => {
                same(*left, *right).is_some_and(|typ| typ.as_comparable_type().is_some())
                    && *declared == DataType::Bool
            }
            IrDataOp::ArrayFromRange(start, end) => {
                all(&[*start, *end], Type::INT)
                    && matches!(declared_data, Some(DataType::Array(element)) if **element == DataType::Int)
            }
            IrDataOp::NumericNegation(operand) => {
                let typ = data(*operand);
                typ.and_then(DataType::as_numeric_type).is_some() && typ == declared_data
            }
            IrDataOp::BoolNegation(operand) => {
                all(&[*operand], Type::BOOL) && *declared == DataType::Bool
            }
            IrDataOp::ArrayLength(operand) => {
                matches!(data(*operand), Some(DataType::Array(_))) && *declared == DataType::Int
            }
            IrDataOp::ArrayIsEmpty(operand) => {
                matches!(data(*operand), Some(DataType::Array(_))) && *declared == DataType::Bool
            }
            IrDataOp::StringIsEmpty(operand) => {
                all(&[*operand], Type::STRING) && *declared == DataType::Bool
            }
            IrDataOp::OptionIsSome(operand) | IrDataOp::OptionIsNone(operand) => {
                matches!(data(*operand), Some(DataType::Option(_))) && *declared == DataType::Bool
            }
            IrDataOp::IntToString(operand) => {
                all(&[*operand], Type::INT) && *declared == DataType::String
            }
            IrDataOp::FloatToInt(operand) => {
                all(&[*operand], Type::FLOAT) && *declared == DataType::Int
            }
            IrDataOp::IntToFloat(operand) => {
                all(&[*operand], Type::INT) && *declared == DataType::Float
            }
            IrDataOp::FieldAccess(object, field) => {
                match data(*object).and_then(|typ| self.registry.resolve(typ)) {
                    Some(ResolvedType::Record { fields, .. }) => fields
                        .iter()
                        .find(|f| f.name == *field)
                        .is_some_and(|field| Some(&field.typ) == declared_data),
                    _ => false,
                }
            }
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
                    (Some(value), None) => self.data(value) == Some(&field.typ),
                    _ => false,
                }
            })
    }

    /// Panics unless every value of the subject's type matches some arm and
    /// each arm matches some value that no arm before it matches.
    fn arms<B>(&self, match_: &IrMatch<B>, function: &IrFunction) {
        let Some(subject) = self.data(match_.subject) else {
            return;
        };
        let patterns: Vec<IrPattern> = match_.arms.iter().map(|arm| arm.pattern.clone()).collect();
        let (_, reachable, missing) = compile_arms(self.registry, &patterns, subject.clone());
        if let Some(pattern) = missing.first() {
            panic!(
                "{function} has a match on a {subject} where no arm matches {}",
                pattern.to_doc().pretty(80)
            );
        }
        if let Some(pattern) = (0..patterns.len())
            .find(|index| !reachable.contains(index))
            .map(|index| &patterns[index])
        {
            panic!(
                "{function} has a match on a {subject} where the arms before {} match every value it matches",
                pattern.to_doc().pretty(80)
            );
        }
    }

    fn pattern(&mut self, pattern: &'a IrPattern, typ: BoundType<'a>) {
        match (pattern, typ) {
            (Pattern::Wildcard, _) => {}
            (_, BoundType::Data(data)) => self.data_pattern(pattern, data),
            (_, BoundType::Html) => {
                panic!("a {typ} is matched against {}", pattern.to_doc().pretty(80))
            }
        }
    }

    fn data_pattern(&mut self, pattern: &'a IrPattern, typ: &DataType) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => {
                assert!(binder.typ == *typ, "{binder} binds a {typ}");
                self.bind(binder.var, BoundType::Data(&binder.typ));
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
    use crate::ir::flat_module_builder::{FlatModuleBuilder, Pat};

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

    #[test]
    #[should_panic]
    fn should_reject_a_match_where_no_arm_matches_some_value() {
        FlatModuleBuilder::new()
            .entrypoint("Test", [("flag", Ty::Bool)], Ty::Html, |t| {
                t.match_(t.param("flag"), |m| {
                    m.arm(Pat::True, |t, _| t.text("yes"));
                })
            })
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_match_with_a_shadowed_arm() {
        FlatModuleBuilder::new()
            .entrypoint("Test", [("flag", Ty::Bool)], Ty::Html, |t| {
                t.match_(t.param("flag"), |m| {
                    m.arm(Pat::Wildcard, |t, _| t.text("any"));
                    m.arm(Pat::False, |t, _| t.text("no"));
                })
            })
            .build();
    }
}
