use std::fmt;

use crate::hop::typing::{Constructor, Pattern, compile_arms};
use crate::ir::flat_program::{
    FlatDataBinding, FlatDataBlock, FlatDataOp, FlatFunctionDeclaration, FlatHtmlBlock, FlatHtmlOp,
    FlatProgram,
};
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_match::{IrMatch, IrPattern};
use crate::ir::ir_parameter::IrParameter;
use crate::ir::ir_type::{IrDataType, IrType};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations};
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;

/// Panics unless every binding has the type its op produces from the types
/// of its operands and every block produces the type its context expects.
/// A name is read only after it is bound. Every value of a match subject's
/// type matches some arm, and each arm matches some value that no arm
/// before it matches.
pub fn assert_well_typed(program: &FlatProgram) {
    let mut checker = Checker {
        declarations: &program.types,
        functions: &program.functions,
        types: vec![None; program.value_ids.count()],
    };
    for function in &program.functions {
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

/// The types are borrowed from the binders in the program and indexed by
/// the name's number, so checking neither hashes nor clones a type.
struct Checker<'a> {
    declarations: &'a IrTypeDeclarations,
    functions: &'a [FlatFunctionDeclaration],
    types: Vec<Option<BoundType<'a>>>,
}

/// The type of a name as its binder gives it, an IrType for a parameter, an
/// IrDataType for a value and Html for Html.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BoundType<'a> {
    Data(&'a IrDataType),
    Html,
}

impl<'a> BoundType<'a> {
    fn of(typ: &'a IrType) -> Self {
        match typ {
            IrType::Data(data) => BoundType::Data(data),
            IrType::Html(_) => BoundType::Html,
        }
    }

    fn is(self, typ: &IrType) -> bool {
        self == BoundType::of(typ)
    }
}

impl fmt::Display for BoundType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundType::Data(data) => data.fmt(f),
            BoundType::Html => IrType::HTML.fmt(f),
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

    fn data(&self, name: ValueId) -> Option<&'a IrDataType> {
        match self.typ(name) {
            BoundType::Data(data) => Some(data),
            BoundType::Html => None,
        }
    }

    fn data_block(
        &mut self,
        block: &'a FlatDataBlock,
        expected: &IrDataType,
        function: &IrFunction,
    ) {
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
    fn args(&self, args: &[ValueId], params: &[IrParameter<IrType>]) -> bool {
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
        declared: &IrDataType,
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
            FlatHtmlOp::Escape(name) => self.typ(*name).is(&IrType::STRING),
            FlatHtmlOp::Concat(parts) => parts.iter().all(|part| self.typ(*part).is(&IrType::HTML)),
            FlatHtmlOp::Element(element) => {
                let attributes = element
                    .attributes
                    .values()
                    .all(|attribute| match attribute {
                        IrAttribute::Value(name) => self.typ(*name).is(&IrType::STRING),
                        IrAttribute::Presence(name) => self.typ(*name).is(&IrType::BOOL),
                    });
                attributes && self.typ(element.children).is(&IrType::HTML)
            }
            FlatHtmlOp::For(for_) => {
                let Some(IrDataType::Array(element)) = self.data(for_.array) else {
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

    fn data_op(&self, op: &IrDataOp, declared: &IrDataType) -> bool {
        let same = |left: ValueId, right: ValueId| {
            let typ = self.data(left)?;
            (Some(typ) == self.data(right)).then_some(typ)
        };
        let all =
            |names: &[ValueId], typ: IrType| names.iter().all(|name| self.typ(*name).is(&typ));
        let data = |name: ValueId| self.data(name);
        let declared_data = Some(declared);
        match op {
            IrDataOp::StringLiteral(_) => *declared == IrDataType::String,
            IrDataOp::IntLiteral(_) => *declared == IrDataType::Int,
            IrDataOp::FloatLiteral(_) => *declared == IrDataType::Float,
            IrDataOp::BoolLiteral(_) => *declared == IrDataType::Bool,
            IrDataOp::Array(elements) => match declared_data {
                Some(IrDataType::Array(element)) => {
                    elements.iter().all(|name| data(*name) == Some(&**element))
                }
                _ => false,
            },
            IrDataOp::Tuple(elements) => match declared_data {
                Some(IrDataType::Tuple(declared)) => {
                    elements.len() == declared.len()
                        && elements
                            .iter()
                            .zip(declared)
                            .all(|(name, typ)| data(*name) == Some(typ))
                }
                _ => false,
            },
            IrDataOp::Record(fields) => match declared {
                IrDataType::Record(name) => self
                    .declarations
                    .record(name)
                    .is_some_and(|record| self.fields(fields, &record.fields)),
                _ => false,
            },
            IrDataOp::Enum(variant, fields) => match declared {
                IrDataType::Enum(name) => self
                    .declarations
                    .enumeration(name)
                    .and_then(|enumeration| {
                        enumeration.variants.iter().find(|v| v.name == *variant)
                    })
                    .is_some_and(|variant| self.fields(fields, &variant.fields)),
                _ => false,
            },
            IrDataOp::OptionSome(inner) => match declared_data {
                Some(IrDataType::Option(element)) => data(*inner) == Some(&**element),
                _ => false,
            },
            IrDataOp::OptionNone => matches!(declared_data, Some(IrDataType::Option(_))),
            IrDataOp::StringConcat(parts) => {
                all(parts, IrType::STRING) && *declared == IrDataType::String
            }
            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right) => same(*left, *right)
                .is_some_and(|typ| typ.as_numeric_type().is_some() && Some(typ) == declared_data),
            IrDataOp::Equals(left, right) => {
                same(*left, *right).is_some_and(|typ| typ.as_equatable_type().is_some())
                    && *declared == IrDataType::Bool
            }
            IrDataOp::LessThan(left, right) | IrDataOp::LessThanOrEqual(left, right) => {
                same(*left, *right).is_some_and(|typ| typ.as_comparable_type().is_some())
                    && *declared == IrDataType::Bool
            }
            IrDataOp::ArrayFromRange(start, end) => {
                all(&[*start, *end], IrType::INT)
                    && matches!(declared_data, Some(IrDataType::Array(element)) if **element == IrDataType::Int)
            }
            IrDataOp::NumericNegation(operand) => {
                let typ = data(*operand);
                typ.and_then(IrDataType::as_numeric_type).is_some() && typ == declared_data
            }
            IrDataOp::BoolNegation(operand) => {
                all(&[*operand], IrType::BOOL) && *declared == IrDataType::Bool
            }
            IrDataOp::ArrayLength(operand) => {
                matches!(data(*operand), Some(IrDataType::Array(_))) && *declared == IrDataType::Int
            }
            IrDataOp::ArrayIsEmpty(operand) => {
                matches!(data(*operand), Some(IrDataType::Array(_)))
                    && *declared == IrDataType::Bool
            }
            IrDataOp::StringIsEmpty(operand) => {
                all(&[*operand], IrType::STRING) && *declared == IrDataType::Bool
            }
            IrDataOp::OptionIsSome(operand) | IrDataOp::OptionIsNone(operand) => {
                matches!(data(*operand), Some(IrDataType::Option(_)))
                    && *declared == IrDataType::Bool
            }
            IrDataOp::IntToString(operand) => {
                all(&[*operand], IrType::INT) && *declared == IrDataType::String
            }
            IrDataOp::FloatToInt(operand) => {
                all(&[*operand], IrType::FLOAT) && *declared == IrDataType::Int
            }
            IrDataOp::IntToFloat(operand) => {
                all(&[*operand], IrType::INT) && *declared == IrDataType::Float
            }
            IrDataOp::FieldAccess(object, field) => match data(*object) {
                Some(IrDataType::Record(name)) => self
                    .declarations
                    .record(name)
                    .and_then(|record| record.fields.iter().find(|f| f.name == *field))
                    .is_some_and(|field| Some(&field.typ) == declared_data),
                _ => false,
            },
            IrDataOp::TupleIndex(tuple, index) => match self.data(*tuple) {
                Some(IrDataType::Tuple(elements)) => elements.get(*index) == declared_data,
                _ => false,
            },
        }
    }

    fn fields(&self, given: &[(FieldName, ValueId)], declared: &[IrField]) -> bool {
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
        let (_, reachable, missing) = compile_arms(self.declarations, &patterns, subject.clone());
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

    fn data_pattern(&mut self, pattern: &'a IrPattern, typ: &IrDataType) {
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
                let declarations = self.declarations;
                let resolved: Option<(&[IrDataType], &[IrField])> = match (constructor, typ) {
                    (Constructor::BoolTrue | Constructor::BoolFalse, IrDataType::Bool) => {
                        Some((&[], &[]))
                    }
                    (Constructor::OptionNone, IrDataType::Option(_)) => Some((&[], &[])),
                    (Constructor::OptionSome, IrDataType::Option(inner)) => {
                        Some((std::slice::from_ref(&**inner), &[]))
                    }
                    (Constructor::Tuple, IrDataType::Tuple(elements)) => Some((elements, &[])),
                    (Constructor::Record { type_name }, IrDataType::Record(name))
                        if type_name == name =>
                    {
                        declarations
                            .record(name)
                            .map(|record| (&[][..], &record.fields[..]))
                    }
                    (
                        Constructor::EnumVariant {
                            type_name,
                            variant_name,
                        },
                        IrDataType::Enum(name),
                    ) if type_name == name => declarations
                        .enumeration(name)
                        .and_then(|enumeration| {
                            enumeration
                                .variants
                                .iter()
                                .find(|v| v.name == *variant_name)
                        })
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
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};

    #[test]
    #[should_panic]
    fn should_reject_an_op_with_ill_typed_operands() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let sum = t.add(t.int(1), t.str("one"));
                t.escape(t.int_to_string(sum))
            })
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_block_of_the_wrong_type() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| t.int(1))
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_call_with_the_wrong_arguments() {
        FlatProgramBuilder::new()
            .function("show", [("n", Ty::Int)], Ty::Html, |t| {
                t.escape(t.int_to_string(t.param("n")))
            })
            .entrypoint("Test", [], Ty::Html, |t| t.call("show", [t.str("one")]))
            .build();
    }

    #[test]
    #[should_panic]
    fn should_reject_a_match_where_no_arm_matches_some_value() {
        FlatProgramBuilder::new()
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
        FlatProgramBuilder::new()
            .entrypoint("Test", [("flag", Ty::Bool)], Ty::Html, |t| {
                t.match_(t.param("flag"), |m| {
                    m.arm(Pat::Wildcard, |t, _| t.text("any"));
                    m.arm(Pat::False, |t, _| t.text("no"));
                })
            })
            .build();
    }
}
