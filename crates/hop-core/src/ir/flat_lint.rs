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

#[derive(Debug, PartialEq, Eq)]
pub enum FlatLintError {
    /// A function reads an Html value when the path has already read it,
    /// or in a loop or lambda body that the value is bound outside of.
    HtmlNotLive,
    /// A name is bound a second time.
    BoundTwice,
    /// A name is read before it is bound.
    ReadBeforeBound,
    /// A closure reads an Html value bound outside it.
    ClosureReadsOuterHtml,
    /// A binding declares a type that its op does not produce from the
    /// types of its operands.
    IllTypedBinding,
    /// A block produces a type other than the one its context expects.
    BlockOfWrongType,
    /// A call calls a function that the program does not declare.
    UndeclaredCallee,
    /// A match produces a closure.
    MatchProducesClosure,
    /// A match has a value of its subject's type that no arm matches.
    NoArmMatches,
    /// A match has an arm whose values the arms before it all match.
    UnreachableArm,
    /// A pattern is matched against a type whose values it cannot match.
    PatternMismatch,
    /// A pattern binder binds a type other than the one it is matched
    /// against.
    BinderMismatch,
}

pub fn lint(program: &FlatProgram) -> Result<(), FlatLintError> {
    check_well_typed(program)?;
    check_html_affine(program)
}

fn check_html_affine(program: &FlatProgram) -> Result<(), FlatLintError> {
    let mut html = vec![false; program.value_ids.count()];
    let html_functions = program
        .functions
        .iter()
        .filter_map(|function| match function {
            FlatFunctionDeclaration::Html(function) => Some(function),
            FlatFunctionDeclaration::Data(_) => None,
        });
    for function in html_functions.clone() {
        for param in &function.parameters {
            if param.binder.typ == IrType::HTML {
                html[param.binder.var.index()] = true;
            }
        }
        collect_html(&function.body, &mut html);
    }
    for entrypoint in &program.entrypoints {
        collect_html(&entrypoint.body, &mut html);
    }
    for function in html_functions {
        let mut scope = vec![false; html.len()];
        for param in &function.parameters {
            if html[param.binder.var.index()] {
                scope[param.binder.var.index()] = true;
            }
        }
        check_block(&function.body, scope, &html)?;
    }
    for entrypoint in &program.entrypoints {
        check_block(&entrypoint.body, vec![false; html.len()], &html)?;
    }
    Ok(())
}

fn collect_html(block: &FlatHtmlBlock, html: &mut [bool]) {
    for binding in &block.html {
        if binding.binder.typ == IrHtmlType::Html {
            html[binding.binder.var.index()] = true;
        }
        match &binding.op {
            FlatHtmlOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_html(&arm.body, html);
                }
            }
            FlatHtmlOp::For(for_) => collect_html(&for_.body, html),
            FlatHtmlOp::Lambda(lambda) => {
                for param in &lambda.parameters {
                    if param.typ == IrType::HTML {
                        html[param.var.index()] = true;
                    }
                }
                collect_html(&lambda.body, html);
            }
            _ => {}
        }
    }
}

fn check_block(
    block: &FlatHtmlBlock,
    mut scope: Vec<bool>,
    html: &[bool],
) -> Result<Vec<bool>, FlatLintError> {
    for binding in &block.html {
        let mut operands = Ok(());
        binding.op.for_each_operand(&mut |operand| {
            if operands.is_ok() {
                operands = read(operand, &mut scope, html);
            }
        });
        operands?;
        match &binding.op {
            FlatHtmlOp::Match(match_) => {
                let mut after = scope.clone();
                for arm in &match_.arms {
                    let inner = check_block(&arm.body, scope.clone(), html)?;
                    for (name, live) in after.iter_mut().zip(&inner) {
                        *name = *name && *live;
                    }
                }
                scope = after;
            }
            FlatHtmlOp::For(for_) => {
                check_block(&for_.body, vec![false; html.len()], html)?;
            }
            FlatHtmlOp::Lambda(lambda) => {
                let mut inner = vec![false; html.len()];
                for param in &lambda.parameters {
                    inner[param.var.index()] = html[param.var.index()];
                }
                check_block(&lambda.body, inner, html)?;
            }
            _ => {}
        }
        if html[binding.binder.var.index()] {
            scope[binding.binder.var.index()] = true;
        }
    }
    read(block.result, &mut scope, html)?;
    Ok(scope)
}

fn read(name: ValueId, scope: &mut [bool], html: &[bool]) -> Result<(), FlatLintError> {
    if html[name.index()] && !std::mem::replace(&mut scope[name.index()], false) {
        return Err(FlatLintError::HtmlNotLive);
    }
    Ok(())
}

fn check_well_typed(program: &FlatProgram) -> Result<(), FlatLintError> {
    let mut checker = Checker {
        declarations: &program.types,
        functions: &program.functions,
        types: vec![None; program.value_ids.count()],
        lambdas: 0,
    };
    for function in &program.functions {
        match function {
            FlatFunctionDeclaration::Data(data) => {
                for param in &data.parameters {
                    checker.bind(param.binder.var, BoundType::of_value(&param.binder.typ))?;
                }
                let expected = BoundType::Data(&data.return_type);
                checker.data_block(&data.body, expected)?;
            }
            FlatFunctionDeclaration::Html(html) => {
                for param in &html.parameters {
                    checker.bind(param.binder.var, BoundType::of(&param.binder.typ))?;
                }
                checker.html_block(&html.body, BoundType::Html)?;
            }
        }
    }
    for entrypoint in &program.entrypoints {
        for param in &entrypoint.parameters {
            checker.bind(param.binder.var, BoundType::Data(&param.binder.typ))?;
        }
        checker.html_block(&entrypoint.body, BoundType::Html)?;
    }
    Ok(())
}

fn try_all<T>(
    items: impl IntoIterator<Item = T>,
    mut f: impl FnMut(T) -> Result<bool, FlatLintError>,
) -> Result<bool, FlatLintError> {
    for item in items {
        if !f(item)? {
            return Ok(false);
        }
    }
    Ok(true)
}

struct Checker<'a> {
    declarations: &'a IrTypeDeclarations,
    functions: &'a [FlatFunctionDeclaration],
    types: Vec<Option<(BoundType<'a>, usize)>>,
    lambdas: usize,
}

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

impl<'a> Checker<'a> {
    fn bind(&mut self, name: ValueId, typ: BoundType<'a>) -> Result<(), FlatLintError> {
        match self.types[name.index()].replace((typ, self.lambdas)) {
            None => Ok(()),
            Some(_) => Err(FlatLintError::BoundTwice),
        }
    }

    fn typ(&self, name: ValueId) -> Result<BoundType<'a>, FlatLintError> {
        let (typ, lambdas) = self.types[name.index()].ok_or(FlatLintError::ReadBeforeBound)?;
        if typ == BoundType::Html && lambdas != self.lambdas {
            return Err(FlatLintError::ClosureReadsOuterHtml);
        }
        Ok(typ)
    }

    fn data(&self, name: ValueId) -> Result<Option<&'a IrDataType>, FlatLintError> {
        Ok(match self.typ(name)? {
            BoundType::Data(data) => Some(data),
            BoundType::ValueFunction(_) | BoundType::Html | BoundType::HtmlFunction(_) => None,
        })
    }

    fn data_block(
        &mut self,
        block: &'a FlatDataBlock,
        expected: BoundType<'a>,
    ) -> Result<(), FlatLintError> {
        self.data_bindings(&block.bindings)?;
        self.block_result(block.result, expected)
    }

    fn html_block(
        &mut self,
        block: &'a FlatHtmlBlock,
        expected: BoundType<'a>,
    ) -> Result<(), FlatLintError> {
        self.data_bindings(&block.bindings)?;
        for binding in &block.html {
            let declared = BoundType::of_html(&binding.binder.typ);
            let html = declared == BoundType::Html;
            let produces = match &binding.op {
                FlatHtmlOp::Call(callee, args) => match self.callee(callee)? {
                    FlatFunctionDeclaration::Html(callee) => {
                        html && self.args(
                            args,
                            callee
                                .parameters
                                .iter()
                                .map(|p| BoundType::of(&p.binder.typ)),
                        )?
                    }
                    FlatFunctionDeclaration::Data(_) => false,
                },
                FlatHtmlOp::Match(match_) => {
                    if !html {
                        return Err(FlatLintError::MatchProducesClosure);
                    }
                    let subject = self.typ(match_.subject)?;
                    for arm in &match_.arms {
                        self.pattern(&arm.pattern, subject)?;
                        self.html_block(&arm.body, declared)?;
                    }
                    self.arms(match_)?;
                    true
                }
                FlatHtmlOp::Text(_) => html,
                FlatHtmlOp::Escape(name) => html && self.typ(*name)?.is(&IrType::STRING),
                FlatHtmlOp::Concat(parts) => {
                    html && try_all(parts, |part| Ok(self.typ(*part)?.is(&IrType::HTML)))?
                }
                FlatHtmlOp::Element(element) => {
                    let attributes = try_all(element.attributes.values(), |attribute| {
                        Ok(match attribute {
                            IrAttribute::Value(name) => self.typ(*name)?.is(&IrType::STRING),
                            IrAttribute::Presence(name) => self.typ(*name)?.is(&IrType::BOOL),
                            IrAttribute::Constant(_) => true,
                        })
                    })?;
                    html && attributes && self.typ(element.children)?.is(&IrType::HTML)
                }
                FlatHtmlOp::For(for_) => {
                    let Some(IrDataType::Array(element)) = self.data(for_.array)? else {
                        return Err(FlatLintError::IllTypedBinding);
                    };
                    if let Some(var) = &for_.var {
                        if var.typ != **element {
                            return Err(FlatLintError::IllTypedBinding);
                        }
                        self.bind(var.var, BoundType::Data(&var.typ))?;
                    }
                    self.html_block(&for_.body, BoundType::Html)?;
                    html
                }
                FlatHtmlOp::Lambda(lambda) => {
                    let BoundType::HtmlFunction(signature) = declared else {
                        return Err(FlatLintError::IllTypedBinding);
                    };
                    self.lambda_parameters(
                        &lambda.parameters,
                        signature.parameters.iter().map(BoundType::of),
                        BoundType::of,
                    )?;
                    self.html_block(&lambda.body, BoundType::Html)?;
                    self.lambdas -= 1;
                    true
                }
                FlatHtmlOp::Apply(closure, args) => match self.typ(*closure)? {
                    BoundType::HtmlFunction(signature) => {
                        html && self.args(args, signature.parameters.iter().map(BoundType::of))?
                    }
                    _ => false,
                },
            };
            if !produces {
                return Err(FlatLintError::IllTypedBinding);
            }
            self.bind(binding.binder.var, declared)?;
        }
        self.block_result(block.result, expected)
    }

    fn block_result(&self, result: ValueId, expected: BoundType<'a>) -> Result<(), FlatLintError> {
        let found = self.typ(result)?;
        if found != expected {
            return Err(FlatLintError::BlockOfWrongType);
        }
        Ok(())
    }

    fn data_bindings(&mut self, bindings: &'a [FlatDataBinding]) -> Result<(), FlatLintError> {
        for binding in bindings {
            let declared = BoundType::of_value(&binding.binder.typ);
            let produces = match &binding.op {
                FlatDataOp::Data(op) => match declared {
                    BoundType::Data(declared) => self.data_op(op, declared)?,
                    _ => false,
                },
                FlatDataOp::Call(callee, args) => match self.callee(callee)? {
                    FlatFunctionDeclaration::Data(callee) => {
                        declared == BoundType::Data(&callee.return_type)
                            && self.args(
                                args,
                                callee
                                    .parameters
                                    .iter()
                                    .map(|p| BoundType::of_value(&p.binder.typ)),
                            )?
                    }
                    FlatFunctionDeclaration::Html(_) => false,
                },
                FlatDataOp::Match(match_) => {
                    if !matches!(declared, BoundType::Data(_)) {
                        return Err(FlatLintError::MatchProducesClosure);
                    }
                    let subject = self.typ(match_.subject)?;
                    for arm in &match_.arms {
                        self.pattern(&arm.pattern, subject)?;
                        self.data_block(&arm.body, declared)?;
                    }
                    self.arms(match_)?;
                    true
                }
                FlatDataOp::Lambda(lambda) => {
                    let BoundType::ValueFunction(signature) = declared else {
                        return Err(FlatLintError::IllTypedBinding);
                    };
                    self.lambda_parameters(
                        &lambda.parameters,
                        signature.parameters.iter().map(BoundType::of_value),
                        BoundType::of_value,
                    )?;
                    self.data_block(&lambda.body, BoundType::Data(&signature.result))?;
                    self.lambdas -= 1;
                    true
                }
                FlatDataOp::Apply(closure, args) => match self.typ(*closure)? {
                    BoundType::ValueFunction(signature) => {
                        declared == BoundType::Data(&signature.result)
                            && self
                                .args(args, signature.parameters.iter().map(BoundType::of_value))?
                    }
                    _ => false,
                },
            };
            if !produces {
                return Err(FlatLintError::IllTypedBinding);
            }
            self.bind(binding.binder.var, declared)?;
        }
        Ok(())
    }

    fn callee(&self, callee: &IrFunction) -> Result<&'a FlatFunctionDeclaration, FlatLintError> {
        self.functions
            .iter()
            .find(|declared| declared.function().id == callee.id)
            .ok_or(FlatLintError::UndeclaredCallee)
    }

    fn args<'t>(
        &self,
        args: &[ValueId],
        params: impl ExactSizeIterator<Item = BoundType<'t>>,
    ) -> Result<bool, FlatLintError> {
        Ok(args.len() == params.len()
            && try_all(args.iter().zip(params), |(arg, param)| {
                Ok(self.typ(*arg)? == param)
            })?)
    }

    fn lambda_parameters<T>(
        &mut self,
        parameters: &'a [IrBinder<T>],
        signature: impl ExactSizeIterator<Item = BoundType<'a>>,
        of: impl Fn(&'a T) -> BoundType<'a>,
    ) -> Result<(), FlatLintError> {
        self.lambdas += 1;
        let follows = parameters.len() == signature.len()
            && (parameters.iter())
                .zip(signature)
                .all(|(param, typ)| of(&param.typ) == typ);
        if !follows {
            return Err(FlatLintError::IllTypedBinding);
        }
        for param in parameters {
            self.bind(param.var, of(&param.typ))?;
        }
        Ok(())
    }

    fn data_op(&self, op: &IrDataOp, declared: &IrDataType) -> Result<bool, FlatLintError> {
        let same = |left: ValueId, right: ValueId| {
            let left = self.data(left)?;
            let right = self.data(right)?;
            Ok::<_, FlatLintError>(if left == right { left } else { None })
        };
        let all =
            |names: &[ValueId], typ: IrType| try_all(names, |name| Ok(self.typ(*name)?.is(&typ)));
        let declared_data = Some(declared);
        Ok(match op {
            IrDataOp::StringLiteral(_) => *declared == IrDataType::String,
            IrDataOp::IntLiteral(_) => *declared == IrDataType::Int,
            IrDataOp::FloatLiteral(_) => *declared == IrDataType::Float,
            IrDataOp::BoolLiteral(_) => *declared == IrDataType::Bool,
            IrDataOp::Array(elements) => match declared_data {
                Some(IrDataType::Array(element)) => {
                    try_all(elements, |name| Ok(self.data(*name)? == Some(&**element)))?
                }
                _ => false,
            },
            IrDataOp::Tuple(elements) => match declared_data {
                Some(IrDataType::Tuple(declared)) => {
                    elements.len() == declared.len()
                        && try_all(elements.iter().zip(declared), |(name, typ)| {
                            Ok(self.data(*name)? == Some(typ))
                        })?
                }
                _ => false,
            },
            IrDataOp::Record(fields) => match declared {
                IrDataType::Record(name) => match self.declarations.record(name) {
                    Some(record) => self.fields(fields, &record.fields)?,
                    None => false,
                },
                _ => false,
            },
            IrDataOp::Enum(variant, fields) => match declared {
                IrDataType::Enum(name) => {
                    match self.declarations.enumeration(name).and_then(|enumeration| {
                        enumeration.variants.iter().find(|v| v.name == *variant)
                    }) {
                        Some(variant) => self.fields(fields, &variant.fields)?,
                        None => false,
                    }
                }
                _ => false,
            },
            IrDataOp::OptionSome(inner) => match declared_data {
                Some(IrDataType::Option(element)) => self.data(*inner)? == Some(&**element),
                _ => false,
            },
            IrDataOp::OptionNone => matches!(declared_data, Some(IrDataType::Option(_))),
            IrDataOp::StringConcat(parts) => {
                all(parts, IrType::STRING)? && *declared == IrDataType::String
            }
            IrDataOp::NumericAdd(left, right)
            | IrDataOp::NumericSubtract(left, right)
            | IrDataOp::NumericMultiply(left, right) => same(*left, *right)?
                .is_some_and(|typ| typ.as_numeric_type().is_some() && Some(typ) == declared_data),
            IrDataOp::Equals(left, right) => {
                same(*left, *right)?.is_some_and(|typ| typ.as_equatable_type().is_some())
                    && *declared == IrDataType::Bool
            }
            IrDataOp::LessThan(left, right) | IrDataOp::LessThanOrEqual(left, right) => {
                same(*left, *right)?.is_some_and(|typ| typ.as_comparable_type().is_some())
                    && *declared == IrDataType::Bool
            }
            IrDataOp::ArrayFromRange(start, end) => {
                all(&[*start, *end], IrType::INT)?
                    && matches!(declared_data, Some(IrDataType::Array(element)) if **element == IrDataType::Int)
            }
            IrDataOp::NumericNegation(operand) => {
                let typ = self.data(*operand)?;
                typ.and_then(IrDataType::as_numeric_type).is_some() && typ == declared_data
            }
            IrDataOp::BoolNegation(operand) => {
                all(&[*operand], IrType::BOOL)? && *declared == IrDataType::Bool
            }
            IrDataOp::ArrayLength(operand) => {
                matches!(self.data(*operand)?, Some(IrDataType::Array(_)))
                    && *declared == IrDataType::Int
            }
            IrDataOp::ArrayIsEmpty(operand) => {
                matches!(self.data(*operand)?, Some(IrDataType::Array(_)))
                    && *declared == IrDataType::Bool
            }
            IrDataOp::StringIsEmpty(operand) => {
                all(&[*operand], IrType::STRING)? && *declared == IrDataType::Bool
            }
            IrDataOp::OptionIsSome(operand) | IrDataOp::OptionIsNone(operand) => {
                matches!(self.data(*operand)?, Some(IrDataType::Option(_)))
                    && *declared == IrDataType::Bool
            }
            IrDataOp::IntToString(operand) => {
                all(&[*operand], IrType::INT)? && *declared == IrDataType::String
            }
            IrDataOp::FloatToInt(operand) => {
                all(&[*operand], IrType::FLOAT)? && *declared == IrDataType::Int
            }
            IrDataOp::IntToFloat(operand) => {
                all(&[*operand], IrType::INT)? && *declared == IrDataType::Float
            }
            IrDataOp::FieldAccess(object, field) => match self.data(*object)? {
                Some(IrDataType::Record(name)) => self
                    .declarations
                    .record(name)
                    .and_then(|record| record.fields.iter().find(|f| f.name == *field))
                    .is_some_and(|field| Some(&field.typ) == declared_data),
                _ => false,
            },
            IrDataOp::TupleIndex(tuple, index) => match self.data(*tuple)? {
                Some(IrDataType::Tuple(elements)) => elements.get(*index) == declared_data,
                _ => false,
            },
        })
    }

    fn fields(
        &self,
        given: &[(FieldName, ValueId)],
        declared: &[IrField],
    ) -> Result<bool, FlatLintError> {
        Ok(given.len() == declared.len()
            && try_all(declared, |field| {
                let mut values = given
                    .iter()
                    .filter(|(name, _)| *name == field.name)
                    .map(|(_, value)| *value);
                Ok(match (values.next(), values.next()) {
                    (Some(value), None) => self.data(value)? == Some(&field.typ),
                    _ => false,
                })
            })?)
    }

    fn arms<B>(&self, match_: &IrMatch<B>) -> Result<(), FlatLintError> {
        let Some(subject) = self.data(match_.subject)? else {
            return Ok(());
        };
        let patterns: Vec<IrPattern> = match_.arms.iter().map(|arm| arm.pattern.clone()).collect();
        let (_, reachable, missing) = compile_arms(self.declarations, &patterns, subject.clone());
        if !missing.is_empty() {
            return Err(FlatLintError::NoArmMatches);
        }
        if (0..patterns.len()).any(|index| !reachable.contains(&index)) {
            return Err(FlatLintError::UnreachableArm);
        }
        Ok(())
    }

    fn pattern(&mut self, pattern: &'a IrPattern, typ: BoundType<'a>) -> Result<(), FlatLintError> {
        match (pattern, typ) {
            (Pattern::Wildcard, _) => Ok(()),
            (_, BoundType::Data(data)) => self.data_pattern(pattern, data),
            (_, BoundType::ValueFunction(_) | BoundType::Html | BoundType::HtmlFunction(_)) => {
                Err(FlatLintError::PatternMismatch)
            }
        }
    }

    fn data_pattern(
        &mut self,
        pattern: &'a IrPattern,
        typ: &IrDataType,
    ) -> Result<(), FlatLintError> {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => {
                if binder.typ != *typ {
                    return Err(FlatLintError::BinderMismatch);
                }
                self.bind(binder.var, BoundType::Data(&binder.typ))?;
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
                    .ok_or(FlatLintError::PatternMismatch)?;
                for (arg, arg_type) in args.iter().zip(arg_types) {
                    self.data_pattern(arg, arg_type)?;
                }
                for field in fields {
                    let declared = declared_fields
                        .get(field.index)
                        .filter(|declared| declared.name == field.name)
                        .ok_or(FlatLintError::PatternMismatch)?;
                    self.data_pattern(&field.pattern, &declared.typ)?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::FlatLintError;
    use crate::hop::typing::Ty;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::ir_type::IrType;

    #[test]
    fn should_reject_a_closure_that_reads_html_bound_outside_it() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let outer = t.text("x");
                let closure = t.lambda([], |_, _| outer);
                t.apply(closure, [])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ClosureReadsOuterHtml);
    }

    #[test]
    fn should_reject_a_match_that_produces_a_closure() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("double", Ty::Bool)], |t| {
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
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::MatchProducesClosure);
    }

    #[test]
    fn should_reject_an_application_with_the_wrong_arguments() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let double = t.lambda([IrType::INT], |t, params| t.add(params[0], params[0]));
                let doubled = t.apply(double, [t.str("one")]);
                t.escape(t.int_to_string(doubled))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }

    #[test]
    fn should_reject_an_op_with_ill_typed_operands() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let sum = t.add(t.int(1), t.str("one"));
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_block_of_the_wrong_type() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| t.int(1))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::BlockOfWrongType);
    }

    #[test]
    fn should_reject_a_call_with_the_wrong_arguments() {
        let error = FlatProgramBuilder::new()
            .function("show", [("n", Ty::Int)], Ty::Html, |t| {
                t.escape(t.int_to_string(t.param("n")))
            })
            .entrypoint("Test", [], |t| t.call("show", [t.str("one")]))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_match_where_no_arm_matches_some_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("flag", Ty::Bool)], |t| {
                t.match_(t.param("flag"), |m| {
                    m.arm(Pat::True, |t, _| t.text("yes"));
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::NoArmMatches);
    }

    #[test]
    fn should_reject_a_match_with_a_shadowed_arm() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("flag", Ty::Bool)], |t| {
                t.match_(t.param("flag"), |m| {
                    m.arm(Pat::Wildcard, |t, _| t.text("any"));
                    m.arm(Pat::False, |t, _| t.text("no"));
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::UnreachableArm);
    }
}
