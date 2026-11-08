use crate::hop::typing::{Constructor, Pattern, compile_arms};
use crate::ir::flat_program::{
    FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatProgram, FlatPureBlock,
    FlatPureOp,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_match::{IrMatch, IrPattern};
use crate::ir::ir_type::{
    IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrResultType, IrSignature,
    IrType,
};
use crate::ir::ir_type_declarations::{IrField, IrTypeDeclarations};
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlatLintError {
    /// A function reads an Html value when the path has already read it,
    /// or in a loop or lambda body that the value is bound outside of.
    HtmlNotLive,
    /// A name is bound a second time.
    BoundTwice,
    /// A name is read before it is bound.
    ReadBeforeBound,
    /// A name is read outside the block that binds it.
    ReadOutOfScope,
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
    /// A loop has a key that is neither an Int nor a String.
    KeyNotIntOrString,
    /// A click names a value that is not a closure from no parameters to
    /// an Effect.
    ClickNotHandler,
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
            FlatFunction::Html(function) => Some(function),
            FlatFunction::Pure(_) => None,
        });
    for function in html_functions.clone() {
        for param in &function.parameters {
            if param.binder.typ == IrType::HTML {
                html[param.binder.var.index()] = true;
            }
        }
        collect_html(&function.body, &mut html);
    }
    for entrypoint in program.entrypoints.iter() {
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
    for entrypoint in program.entrypoints.iter() {
        check_block(&entrypoint.body, vec![false; html.len()], &html)?;
    }
    Ok(())
}

fn collect_html(block: &FlatHtmlBlock, html: &mut [bool]) {
    for binding in &block.bindings {
        let FlatHtmlBinding::Html(binding) = binding else {
            continue;
        };
        if binding.binder.typ == IrFunctionHtmlType::Html(IrHtmlType) {
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
    for binding in &block.bindings {
        let FlatHtmlBinding::Html(binding) = binding else {
            continue;
        };
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
        names: vec![None; program.value_ids.count()],
        scope: Vec::new(),
        lambdas: 0,
    };
    for function in &program.functions {
        checker.scoped(|checker| match function {
            FlatFunction::Pure(pure) => {
                for param in &pure.parameters {
                    checker.bind(
                        param.binder.var,
                        BoundType::of_function_data(&param.binder.typ),
                    )?;
                }
                let expected = BoundType::of_result(&pure.return_type);
                checker.pure_block(&pure.body, expected)
            }
            FlatFunction::Html(html) => {
                for param in &html.parameters {
                    checker.bind(param.binder.var, BoundType::of(&param.binder.typ))?;
                }
                checker.html_block(&html.body, BoundType::Html)
            }
        })?;
    }
    for entrypoint in program.entrypoints.iter() {
        checker.scoped(|checker| {
            for param in &entrypoint.parameters {
                checker.bind(param.binder.var, BoundType::Data(&param.binder.typ))?;
            }
            checker.html_block(&entrypoint.body, BoundType::Html)
        })?;
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
    functions: &'a [FlatFunction],
    names: Vec<Option<Name<'a>>>,
    // The names bound in the blocks being checked, innermost last.
    scope: Vec<ValueId>,
    lambdas: usize,
}

#[derive(Debug, Clone, Copy)]
struct Name<'a> {
    typ: BoundType<'a>,
    lambdas: usize,
    in_scope: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BoundType<'a> {
    Data(&'a IrDataType),
    Effect(&'a IrDataType),
    Cell(&'a IrDataType),
    DataFunction(&'a IrSignature<IrFunctionDataType, IrResultType>),
    Html,
    HtmlFunction(&'a IrSignature<IrType, IrHtmlType>),
}

impl<'a> BoundType<'a> {
    fn of(typ: &'a IrType) -> Self {
        match typ {
            IrType::Data(data) => BoundType::of_function_data(data),
            IrType::Html(html) => BoundType::of_function_html(html),
        }
    }

    fn of_function_data(typ: &'a IrFunctionDataType) -> Self {
        match typ {
            IrFunctionDataType::Data(data) => BoundType::Data(data),
            IrFunctionDataType::Effect(data) => BoundType::Effect(data),
            IrFunctionDataType::Cell(data) => BoundType::Cell(data),
            IrFunctionDataType::Function(signature) => BoundType::DataFunction(signature),
        }
    }

    fn of_result(typ: &'a IrResultType) -> Self {
        match typ {
            IrResultType::Data(data) => BoundType::Data(data),
            IrResultType::Effect(data) => BoundType::Effect(data),
        }
    }

    fn is_result(self) -> bool {
        matches!(self, BoundType::Data(_) | BoundType::Effect(_))
    }

    fn of_function_html(typ: &'a IrFunctionHtmlType) -> Self {
        match typ {
            IrFunctionHtmlType::Html(IrHtmlType) => BoundType::Html,
            IrFunctionHtmlType::Function(signature) => BoundType::HtmlFunction(signature),
        }
    }

    fn is(self, typ: &IrType) -> bool {
        self == BoundType::of(typ)
    }
}

impl<'a> Checker<'a> {
    fn bind(&mut self, name: ValueId, typ: BoundType<'a>) -> Result<(), FlatLintError> {
        let bound = Name {
            typ,
            lambdas: self.lambdas,
            in_scope: true,
        };
        if self.names[name.index()].replace(bound).is_some() {
            return Err(FlatLintError::BoundTwice);
        }
        self.scope.push(name);
        Ok(())
    }

    fn scoped(
        &mut self,
        check: impl FnOnce(&mut Self) -> Result<(), FlatLintError>,
    ) -> Result<(), FlatLintError> {
        let outer = self.scope.len();
        check(self)?;
        for name in self.scope.drain(outer..) {
            if let Some(bound) = &mut self.names[name.index()] {
                bound.in_scope = false;
            }
        }
        Ok(())
    }

    fn typ(&self, name: ValueId) -> Result<BoundType<'a>, FlatLintError> {
        let bound = self.names[name.index()].ok_or(FlatLintError::ReadBeforeBound)?;
        if !bound.in_scope {
            return Err(FlatLintError::ReadOutOfScope);
        }
        if bound.typ == BoundType::Html && bound.lambdas != self.lambdas {
            return Err(FlatLintError::ClosureReadsOuterHtml);
        }
        Ok(bound.typ)
    }

    fn data(&self, name: ValueId) -> Result<Option<&'a IrDataType>, FlatLintError> {
        Ok(match self.typ(name)? {
            BoundType::Data(data) => Some(data),
            BoundType::Effect(_)
            | BoundType::Cell(_)
            | BoundType::DataFunction(_)
            | BoundType::Html
            | BoundType::HtmlFunction(_) => None,
        })
    }

    fn pure_block(
        &mut self,
        block: &'a FlatPureBlock,
        expected: BoundType<'a>,
    ) -> Result<(), FlatLintError> {
        for binding in &block.bindings {
            self.pure_binding(binding)?;
        }
        self.block_result(block.result, expected)
    }

    fn html_block(
        &mut self,
        block: &'a FlatHtmlBlock,
        expected: BoundType<'a>,
    ) -> Result<(), FlatLintError> {
        for binding in &block.bindings {
            match binding {
                FlatHtmlBinding::Pure(binding) => self.pure_binding(binding)?,
                FlatHtmlBinding::Cell(binder, initial) => {
                    let IrFunctionDataType::Cell(data) = &binder.typ else {
                        return Err(FlatLintError::IllTypedBinding);
                    };
                    if self.typ(*initial)? != BoundType::Data(data) {
                        return Err(FlatLintError::IllTypedBinding);
                    }
                    self.bind(binder.var, BoundType::Cell(data))?;
                }
                FlatHtmlBinding::Track(binder, cell) => {
                    if self.typ(*cell)? != BoundType::Cell(&binder.typ) {
                        return Err(FlatLintError::IllTypedBinding);
                    }
                    self.bind(binder.var, BoundType::Data(&binder.typ))?;
                }
                FlatHtmlBinding::Html(binding) => self.html_binding(binding)?,
            }
        }
        self.block_result(block.result, expected)
    }

    fn html_binding(
        &mut self,
        binding: &'a IrBinding<IrFunctionHtmlType, FlatHtmlOp>,
    ) -> Result<(), FlatLintError> {
        let declared = BoundType::of_function_html(&binding.binder.typ);
        let html = declared == BoundType::Html;
        let produces = match &binding.op {
            FlatHtmlOp::Call(callee, args) => match self.callee(callee)? {
                FlatFunction::Html(callee) => {
                    html && self.args(
                        args,
                        callee
                            .parameters
                            .iter()
                            .map(|p| BoundType::of(&p.binder.typ)),
                    )?
                }
                FlatFunction::Pure(_) => false,
            },
            FlatHtmlOp::Match(match_) => {
                if !html {
                    return Err(FlatLintError::MatchProducesClosure);
                }
                let subject = self.typ(match_.subject)?;
                for arm in &match_.arms {
                    self.scoped(|checker| {
                        checker.pattern(&arm.pattern, subject)?;
                        checker.html_block(&arm.body, declared)
                    })?;
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
                if let Some(handler) = element.on_click {
                    let BoundType::DataFunction(signature) = self.typ(handler)? else {
                        return Err(FlatLintError::ClickNotHandler);
                    };
                    if !signature.parameters.is_empty()
                        || signature.result != IrResultType::Effect(IrDataType::UNIT)
                    {
                        return Err(FlatLintError::ClickNotHandler);
                    }
                }
                html && attributes && self.typ(element.children)?.is(&IrType::HTML)
            }
            FlatHtmlOp::For(for_) => {
                let Some(IrDataType::Array(element)) = self.data(for_.array)? else {
                    return Err(FlatLintError::IllTypedBinding);
                };
                self.scoped(|checker| {
                    if let Some(var) = &for_.var {
                        if var.typ != **element {
                            return Err(FlatLintError::IllTypedBinding);
                        }
                        checker.bind(var.var, BoundType::Data(&var.typ))?;
                    }
                    if let Some(key) = &for_.key {
                        checker.scoped(|checker| {
                            for binding in &key.bindings {
                                checker.pure_binding(binding)?;
                            }
                            let typ = checker.typ(key.result)?;
                            if !typ.is(&IrType::INT) && !typ.is(&IrType::STRING) {
                                return Err(FlatLintError::KeyNotIntOrString);
                            }
                            Ok(())
                        })?;
                    }
                    checker.html_block(&for_.body, BoundType::Html)
                })?;
                html
            }
            FlatHtmlOp::Lambda(lambda) => {
                let BoundType::HtmlFunction(signature) = declared else {
                    return Err(FlatLintError::IllTypedBinding);
                };
                self.scoped(|checker| {
                    checker.lambda_parameters(
                        &lambda.parameters,
                        signature.parameters.iter().map(BoundType::of),
                        BoundType::of,
                    )?;
                    checker.html_block(&lambda.body, BoundType::Html)
                })?;
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
        self.bind(binding.binder.var, declared)
    }

    fn block_result(&self, result: ValueId, expected: BoundType<'a>) -> Result<(), FlatLintError> {
        let found = self.typ(result)?;
        if found != expected {
            return Err(FlatLintError::BlockOfWrongType);
        }
        Ok(())
    }

    fn pure_binding(
        &mut self,
        binding: &'a IrBinding<IrFunctionDataType, FlatPureOp>,
    ) -> Result<(), FlatLintError> {
        let declared = BoundType::of_function_data(&binding.binder.typ);
        let produces = match &binding.op {
            FlatPureOp::Data(op) => match declared {
                BoundType::Data(declared) => self.data_op(op, declared)?,
                _ => false,
            },
            FlatPureOp::Call(callee, args) => match self.callee(callee)? {
                FlatFunction::Pure(callee) => {
                    declared == BoundType::of_result(&callee.return_type)
                        && self.args(
                            args,
                            callee
                                .parameters
                                .iter()
                                .map(|p| BoundType::of_function_data(&p.binder.typ)),
                        )?
                }
                FlatFunction::Html(_) => false,
            },
            FlatPureOp::Match(match_) => {
                if !declared.is_result() {
                    return Err(FlatLintError::MatchProducesClosure);
                }
                let subject = self.typ(match_.subject)?;
                for arm in &match_.arms {
                    self.scoped(|checker| {
                        checker.pattern(&arm.pattern, subject)?;
                        checker.pure_block(&arm.body, declared)
                    })?;
                }
                self.arms(match_)?;
                true
            }
            FlatPureOp::CellWrite(cell, value) => match self.typ(*cell)? {
                BoundType::Cell(data) => {
                    declared == BoundType::Effect(&IrDataType::UNIT)
                        && self.typ(*value)? == BoundType::Data(data)
                }
                _ => false,
            },
            FlatPureOp::CellRead(cell) => match self.typ(*cell)? {
                BoundType::Cell(data) => declared == BoundType::Effect(data),
                _ => false,
            },
            FlatPureOp::EffectPure(value) => match self.typ(*value)? {
                BoundType::Data(data) => declared == BoundType::Effect(data),
                _ => false,
            },
            FlatPureOp::EffectBind(effect, continuation) => {
                match (self.typ(*effect)?, self.typ(*continuation)?) {
                    (BoundType::Effect(payload), BoundType::DataFunction(signature)) => {
                        signature.parameters.len() == 1
                            && signature.parameters[0] == IrFunctionDataType::Data(payload.clone())
                            && matches!(signature.result, IrResultType::Effect(_))
                            && declared == BoundType::of_result(&signature.result)
                    }
                    _ => false,
                }
            }
            FlatPureOp::Lambda(lambda) => {
                let BoundType::DataFunction(signature) = declared else {
                    return Err(FlatLintError::IllTypedBinding);
                };
                self.scoped(|checker| {
                    checker.lambda_parameters(
                        &lambda.parameters,
                        signature.parameters.iter().map(BoundType::of_function_data),
                        BoundType::of_function_data,
                    )?;
                    checker.pure_block(&lambda.body, BoundType::of_result(&signature.result))
                })?;
                self.lambdas -= 1;
                true
            }
            FlatPureOp::Apply(closure, args) => match self.typ(*closure)? {
                BoundType::DataFunction(signature) => {
                    declared == BoundType::of_result(&signature.result)
                        && self.args(
                            args,
                            signature.parameters.iter().map(BoundType::of_function_data),
                        )?
                }
                _ => false,
            },
        };
        if !produces {
            return Err(FlatLintError::IllTypedBinding);
        }
        self.bind(binding.binder.var, declared)
    }

    fn callee(&self, callee: &IrFunction) -> Result<&'a FlatFunction, FlatLintError> {
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
            IrDataOp::ArrayGet(array, index) => {
                all(&[*index], IrType::INT)?
                    && match (self.data(*array)?, declared_data) {
                        (Some(IrDataType::Array(element)), Some(IrDataType::Option(inner))) => {
                            element == inner
                        }
                        _ => false,
                    }
            }
            IrDataOp::ArrayEnumerate(operand) => match (self.data(*operand)?, declared_data) {
                (Some(IrDataType::Array(element)), Some(IrDataType::Array(pair))) => {
                    matches!(pair.as_ref(), IrDataType::Tuple(types)
                            if matches!(types.as_slice(), [IrDataType::Int, t] if t == element.as_ref()))
                }
                _ => false,
            },
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
            (
                _,
                BoundType::Effect(_)
                | BoundType::Cell(_)
                | BoundType::DataFunction(_)
                | BoundType::Html
                | BoundType::HtmlFunction(_),
            ) => Err(FlatLintError::PatternMismatch),
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
    use std::cell::Cell;

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

    #[test]
    fn should_reject_a_read_of_a_name_bound_in_an_arm_after_the_match() {
        let inner = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let picked = t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |t, _| {
                        let one = t.int(1);
                        inner.set(Some(one));
                        one
                    });
                    m.arm(Pat::False, |t, _| t.int(0));
                });
                let sum = t.add(inner.get().unwrap(), picked);
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_read_of_the_variable_of_a_loop_after_the_loop() {
        let var = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                let rows = t.html_for(t.param("items"), |t, item| {
                    var.set(Some(item));
                    t.escape(item)
                });
                let last = t.escape(var.get().unwrap());
                t.html_concat([rows, last])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_read_of_a_name_bound_in_the_key_of_a_loop_in_its_body() {
        let key = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                t.html_for_keyed(t.param("items"), |t, item| {
                    let doubled = t.add(item, item);
                    key.set(Some(doubled));
                    doubled
                })
                .body(|t, _| t.escape(t.int_to_string(key.get().unwrap())))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_read_of_a_parameter_of_a_lambda_after_the_lambda() {
        let param = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let id = t.lambda([IrType::INT], |_, params| {
                    param.set(Some(params[0]));
                    params[0]
                });
                let one = t.apply(id, [t.int(1)]);
                let sum = t.add(one, param.get().unwrap());
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_read_of_a_parameter_of_another_function() {
        let param = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .function("id", [("n", Ty::Int)], Ty::Int, |t| {
                param.set(Some(t.param("n")));
                t.param("n")
            })
            .entrypoint("Test", [], |t| {
                let sum = t.add(t.call("id", [t.int(1)]), param.get().unwrap());
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_cell_write_of_a_cell_of_an_arm_after_the_match() {
        let inner = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let picked = t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |t, _| {
                        let cell = t.cell(t.int(1));
                        inner.set(Some(cell));
                        t.text("on")
                    });
                    m.arm(Pat::False, |t, _| t.text("off"));
                });
                let reset = t.event_handler(|t| t.cell_write(inner.get().unwrap(), t.int(0)));
                let button = t.element_on_click("button", vec![], reset, t.text("x"));
                t.html_concat([picked, button])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_cell_write_of_a_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.int(0);
                let reset = t.event_handler(|t| t.cell_write(count, t.int(1)));
                t.element_on_click("button", vec![], reset, t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_cell_write_of_a_value_of_another_type() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(t.int(0));
                let flip = t.event_handler(|t| t.cell_write(count, t.bool(true)));
                t.element_on_click("button", vec![], flip, t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_bind_whose_continuation_takes_another_type() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(t.int(0));
                let read = t.cell_read(count);
                let continuation = t.lambda([IrType::STRING], |t, _| t.effect_pure(t.int(1)));
                let handler = t.event_handler(|t| t.effect_bind(read, continuation));
                t.element_on_click("button", vec![], handler, t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_click_whose_handler_is_a_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                t.element_on_click("button", vec![], t.int(1), t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_click_whose_handler_is_an_effect() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(t.int(0));
                let reset = t.cell_write(count, t.int(0));
                t.element_on_click("button", vec![], reset, t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_click_whose_handler_takes_a_parameter() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(t.int(0));
                let set = t.lambda([IrType::INT], |t, params| t.cell_write(count, params[0]));
                t.element_on_click("button", vec![], set, t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_click_whose_handler_gives_a_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.track(t.cell(t.int(0)));
                let next = t.lambda([], |t, _| t.add(count, t.int(1)));
                t.element_on_click("button", vec![], next, t.text("x"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::ClickNotHandler);
    }

    #[test]
    fn should_accept_a_cell_a_function_takes_and_writes() {
        FlatProgramBuilder::new()
            .function("Bump", [("count", Ty::cell(Ty::Int))], Ty::Html, |t| {
                let shown = t.track(t.param("count"));
                let bump =
                    t.event_handler(|t| t.cell_write(t.param("count"), t.add(shown, t.int(1))));
                t.element_on_click("button", vec![], bump, t.escape(t.int_to_string(shown)))
            })
            .entrypoint("Test", [], |t| {
                let count = t.cell(t.int(0));
                t.call("Bump", [count])
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_call_that_passes_a_value_for_a_cell() {
        let error = FlatProgramBuilder::new()
            .function("Show", [("count", Ty::cell(Ty::Int))], Ty::Html, |t| {
                t.escape(t.int_to_string(t.track(t.param("count"))))
            })
            .entrypoint("Test", [], |t| t.call("Show", [t.int(0)]))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatLintError::IllTypedBinding);
    }
}
