use std::fmt;

use crate::hop::typing::{Constructor, Pattern, compile_arms};
use crate::ir::flat_program::{
    FlatDataBinding, FlatDataBlock, FlatDataOp, FlatFunctionDeclaration, FlatHtmlBlock, FlatHtmlOp,
    FlatProgram,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_match::{IrMatch, IrPattern};
use crate::ir::ir_type::{
    IrDataType, IrHtmlSignature, IrHtmlType, IrType, IrValueSignature, IrValueType,
};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations};
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;

/// Panics unless every binding has the type its op produces from the types
/// of its operands and every block produces the type its context expects.
/// A name is read only after it is bound. Every value of a match subject's
/// type matches some arm, and each arm matches some value that no arm
/// before it matches. No match produces a closure, the body of a closure
/// reads no Html bound outside it, and no entrypoint takes a closure.
pub fn assert_well_typed(program: &FlatProgram) {
    let mut checker = Checker {
        declarations: &program.types,
        functions: &program.functions,
        types: vec![None; program.value_ids.count()],
        lambdas: 0,
    };
    for function in &program.functions {
        let parameters = function.parameters();
        if function.entrypoint() {
            for param in &parameters {
                assert!(
                    !matches!(
                        param.binder.typ,
                        IrType::Value(IrValueType::Function(_))
                            | IrType::Html(IrHtmlType::Function(_))
                    ),
                    "the entrypoint {} takes the closure {}",
                    function.function(),
                    param.binder
                );
            }
        }
        match function {
            FlatFunctionDeclaration::Data(data) => {
                for param in &data.parameters {
                    checker.bind(param.binder.var, BoundType::of_value(&param.binder.typ));
                }
                let expected = BoundType::Data(&data.return_type);
                checker.data_block(&data.body, expected, function.function());
            }
            FlatFunctionDeclaration::Html(html) => {
                for param in &html.parameters {
                    checker.bind(param.binder.var, BoundType::of(&param.binder.typ));
                }
                checker.html_block(&html.body, BoundType::Html, function.function());
            }
        }
    }
}

/// The types are borrowed from the binders in the program and indexed by
/// the name's number, so checking neither hashes nor clones a type.
struct Checker<'a> {
    declarations: &'a IrTypeDeclarations,
    functions: &'a [FlatFunctionDeclaration],
    // Each name with the number of closures around its binding, and
    // `lambdas` the number around the op being checked, so that a read of
    // Html from outside a closure shows.
    types: Vec<Option<(BoundType<'a>, usize)>>,
    lambdas: usize,
}

/// The type of a name as its binder gives it, an IrType for a parameter, an
/// IrValueType for a value, an IrDataType for a name a pattern or a loop
/// binds and an IrHtmlType for Html.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BoundType<'a> {
    Data(&'a IrDataType),
    ValueFunction(&'a IrValueSignature),
    Html,
    HtmlFunction(&'a IrHtmlSignature),
}

impl<'a> BoundType<'a> {
    fn of(typ: &'a IrType) -> Self {
        match typ {
            IrType::Value(value) => BoundType::of_value(value),
            IrType::Html(html) => BoundType::of_html(html),
        }
    }

    fn of_value(typ: &'a IrValueType) -> Self {
        match typ {
            IrValueType::Data(data) => BoundType::Data(data),
            IrValueType::Function(signature) => BoundType::ValueFunction(signature),
        }
    }

    fn of_html(typ: &'a IrHtmlType) -> Self {
        match typ {
            IrHtmlType::Html => BoundType::Html,
            IrHtmlType::Function(signature) => BoundType::HtmlFunction(signature),
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
            BoundType::ValueFunction(signature) => signature.fmt(f),
            BoundType::Html => IrType::HTML.fmt(f),
            BoundType::HtmlFunction(signature) => signature.fmt(f),
        }
    }
}

impl<'a> Checker<'a> {
    fn bind(&mut self, name: ValueId, typ: BoundType<'a>) {
        assert!(
            self.types[name.index()]
                .replace((typ, self.lambdas))
                .is_none(),
            "{name}: {typ} is bound twice"
        );
    }

    // Panics if the name is Html that a closure around the op reads from
    // outside it.
    fn typ(&self, name: ValueId) -> BoundType<'a> {
        let (typ, lambdas) =
            self.types[name.index()].unwrap_or_else(|| panic!("{name} is read before it is bound"));
        assert!(
            typ != BoundType::Html || lambdas == self.lambdas,
            "a closure reads the Html of {name}, bound outside it"
        );
        typ
    }

    fn data(&self, name: ValueId) -> Option<&'a IrDataType> {
        match self.typ(name) {
            BoundType::Data(data) => Some(data),
            BoundType::ValueFunction(_) | BoundType::Html | BoundType::HtmlFunction(_) => None,
        }
    }

    fn data_block(
        &mut self,
        block: &'a FlatDataBlock,
        expected: BoundType<'a>,
        function: &IrFunction,
    ) {
        self.data_bindings(&block.bindings, function);
        assert!(
            self.typ(block.result) == expected,
            "{function} has a block that produces {} where {expected} is expected",
            self.typ(block.result)
        );
    }

    fn html_block(
        &mut self,
        block: &'a FlatHtmlBlock,
        expected: BoundType<'a>,
        function: &IrFunction,
    ) {
        self.data_bindings(&block.bindings, function);
        for binding in &block.html {
            let typ = BoundType::of_html(&binding.binder.typ);
            assert!(
                self.html_op(&binding.op, typ, function),
                "{function} binds {} to an op that does not produce that type",
                binding.binder
            );
            self.bind(binding.binder.var, typ);
        }
        assert!(
            self.typ(block.result) == expected,
            "{function} has a block that produces {} where {expected} is expected",
            self.typ(block.result)
        );
    }

    fn data_bindings(&mut self, bindings: &'a [FlatDataBinding], function: &IrFunction) {
        for binding in bindings {
            let typ = BoundType::of_value(&binding.binder.typ);
            assert!(
                self.flat_data_op(&binding.op, typ, function),
                "{function} binds {} to an op that does not produce that type",
                binding.binder
            );
            self.bind(binding.binder.var, typ);
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
    fn args<'t>(
        &self,
        args: &[ValueId],
        params: impl ExactSizeIterator<Item = BoundType<'t>>,
    ) -> bool {
        args.len() == params.len()
            && args
                .iter()
                .zip(params)
                .all(|(arg, param)| self.typ(*arg) == param)
    }

    // Enters the closure, so the caller leaves it again, and binds its
    // parameters if they follow the signature.
    fn lambda_parameters<T>(
        &mut self,
        parameters: &'a [IrBinder<T>],
        signature: impl ExactSizeIterator<Item = BoundType<'a>>,
        of: impl Fn(&'a T) -> BoundType<'a>,
    ) -> bool {
        self.lambdas += 1;
        let follows = parameters.len() == signature.len()
            && (parameters.iter())
                .zip(signature)
                .all(|(param, typ)| of(&param.typ) == typ);
        if follows {
            for param in parameters {
                self.bind(param.var, of(&param.typ));
            }
        }
        follows
    }

    /// Whether `op` produces a `declared` from the types of its operands.
    fn flat_data_op(
        &mut self,
        op: &'a FlatDataOp,
        declared: BoundType<'a>,
        function: &IrFunction,
    ) -> bool {
        match op {
            FlatDataOp::Data(op) => match declared {
                BoundType::Data(declared) => self.data_op(op, declared),
                _ => false,
            },
            FlatDataOp::Call(callee, args) => match self.callee(callee, function) {
                FlatFunctionDeclaration::Data(callee) => {
                    declared == BoundType::Data(&callee.return_type)
                        && self.args(
                            args,
                            callee
                                .parameters
                                .iter()
                                .map(|p| BoundType::of_value(&p.binder.typ)),
                        )
                }
                FlatFunctionDeclaration::Html(_) => false,
            },
            FlatDataOp::Match(match_) => {
                assert!(
                    matches!(declared, BoundType::Data(_)),
                    "{function} has a match that produces the closure {declared}"
                );
                let subject = self.typ(match_.subject);
                for arm in &match_.arms {
                    self.pattern(&arm.pattern, subject);
                    self.data_block(&arm.body, declared, function);
                }
                self.arms(match_, function);
                true
            }
            FlatDataOp::Lambda(lambda) => {
                let BoundType::ValueFunction(signature) = declared else {
                    return false;
                };
                if !self.lambda_parameters(
                    &lambda.parameters,
                    signature.parameters.iter().map(BoundType::of_value),
                    BoundType::of_value,
                ) {
                    self.lambdas -= 1;
                    return false;
                }
                self.data_block(&lambda.body, BoundType::Data(&signature.result), function);
                self.lambdas -= 1;
                true
            }
            FlatDataOp::Apply(closure, args) => match self.typ(*closure) {
                BoundType::ValueFunction(signature) => {
                    declared == BoundType::Data(&signature.result)
                        && self.args(args, signature.parameters.iter().map(BoundType::of_value))
                }
                _ => false,
            },
        }
    }

    /// Whether `op` produces a `declared` from the types of its operands.
    fn html_op(
        &mut self,
        op: &'a FlatHtmlOp,
        declared: BoundType<'a>,
        function: &IrFunction,
    ) -> bool {
        let html = declared == BoundType::Html;
        match op {
            FlatHtmlOp::Call(callee, args) => match self.callee(callee, function) {
                FlatFunctionDeclaration::Html(callee) => {
                    html && self.args(
                        args,
                        callee
                            .parameters
                            .iter()
                            .map(|p| BoundType::of(&p.binder.typ)),
                    )
                }
                FlatFunctionDeclaration::Data(_) => false,
            },
            FlatHtmlOp::Match(match_) => {
                assert!(
                    html,
                    "{function} has a match that produces the closure {declared}"
                );
                let subject = self.typ(match_.subject);
                for arm in &match_.arms {
                    self.pattern(&arm.pattern, subject);
                    self.html_block(&arm.body, declared, function);
                }
                self.arms(match_, function);
                true
            }
            FlatHtmlOp::Text(_) => html,
            FlatHtmlOp::Escape(name) => html && self.typ(*name).is(&IrType::STRING),
            FlatHtmlOp::Concat(parts) => {
                html && parts.iter().all(|part| self.typ(*part).is(&IrType::HTML))
            }
            FlatHtmlOp::Element(element) => {
                let attributes = element
                    .attributes
                    .values()
                    .all(|attribute| match attribute {
                        IrAttribute::Value(name) => self.typ(*name).is(&IrType::STRING),
                        IrAttribute::Presence(name) => self.typ(*name).is(&IrType::BOOL),
                    });
                html && attributes && self.typ(element.children).is(&IrType::HTML)
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
                self.html_block(&for_.body, BoundType::Html, function);
                html
            }
            FlatHtmlOp::Lambda(lambda) => {
                let BoundType::HtmlFunction(signature) = declared else {
                    return false;
                };
                if !self.lambda_parameters(
                    &lambda.parameters,
                    signature.parameters.iter().map(BoundType::of),
                    BoundType::of,
                ) {
                    self.lambdas -= 1;
                    return false;
                }
                self.html_block(&lambda.body, BoundType::Html, function);
                self.lambdas -= 1;
                true
            }
            FlatHtmlOp::Apply(closure, args) => match self.typ(*closure) {
                BoundType::HtmlFunction(signature) => {
                    html && self.args(args, signature.parameters.iter().map(BoundType::of))
                }
                _ => false,
            },
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
            (_, BoundType::ValueFunction(_) | BoundType::Html | BoundType::HtmlFunction(_)) => {
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
    use crate::ir::ir_type::IrType;

    #[test]
    #[should_panic(expected = "a closure reads the Html of v0, bound outside it")]
    fn should_reject_a_closure_that_reads_html_bound_outside_it() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let outer = t.text("x");
                let closure = t.lambda([], |_, _| outer);
                t.apply(closure, [])
            })
            .build();
    }

    #[test]
    #[should_panic(expected = "Test@f0 has a match that produces the closure (Int) -> Int")]
    fn should_reject_a_match_that_produces_a_closure() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [("double", Ty::Bool)], Ty::Html, |t| {
                let scale = t.match_(t.param("double"), |m| {
                    m.arm(Pat::True, |t, _| {
                        t.lambda([IrType::INT], |t, params| t.add(params[0], params[0]))
                    });
                    m.arm(Pat::False, |t, _| {
                        t.lambda([IrType::INT], |_, params| params[0])
                    });
                });
                let scaled = t.apply(scale, [t.int(1)]);
                t.escape(t.int_to_string(scaled))
            })
            .build();
    }

    #[test]
    #[should_panic(expected = "the entrypoint Test@f0 takes the closure")]
    fn should_reject_an_entrypoint_that_takes_a_closure() {
        FlatProgramBuilder::new()
            .entrypoint_with_types(
                "Test",
                [("row", IrType::html_function([]))],
                IrType::HTML,
                |t| t.apply(t.param("row"), []),
            )
            .build();
    }

    #[test]
    #[should_panic(expected = "does not produce that type")]
    fn should_reject_an_application_with_the_wrong_arguments() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let double = t.lambda([IrType::INT], |t, params| t.add(params[0], params[0]));
                let doubled = t.apply(double, [t.str("one")]);
                t.escape(t.int_to_string(doubled))
            })
            .build();
    }

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
