use crate::hop::typing::{
    ComparableType, Constructor, EquatableType, NumericType, Pattern, compile_arms,
};
use crate::ir::flat_html_element::FlatAttribute;
use crate::ir::flat_program::{
    FlatBinding, FlatBlock, FlatFunction, FlatLambda, FlatMatch, FlatOp, FlatPattern, FlatProgram,
};
use crate::ir::flat_type::{FlatDataType, FlatType, IrModifier};
use crate::ir::flat_type_declarations::{FlatField, FlatTypeDeclarations};
use crate::ir::function_id::FunctionId;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlatTypeError {
    /// An Html value is read more than once: again on a path that has
    /// already read it, or in a loop or lambda body that it is bound
    /// outside of, which would read it on every run of the body.
    HtmlReadMoreThanOnce,
    /// A name is bound a second time.
    BoundTwice,
    /// A name is read where it is not in scope: before it is bound, or
    /// outside the block that binds it.
    ReadOutOfScope,
    /// A binding declares a type that its op does not produce from the
    /// types of its operands.
    IllTypedBinding,
    /// A block produces a type other than the one its context expects.
    BlockOfWrongType,
    /// A call calls a function that the program does not declare.
    UndeclaredCallee,
    /// A binding binds a closure of a lambda that the program does not
    /// declare.
    UndeclaredLambda,
    /// A match has a subject that is not data, or is a closure.
    MatchSubjectNotData,
    /// A match has a value of its subject's type that no arm matches.
    NoArmMatches,
    /// A match has an arm whose values the arms before it all match.
    UnreachableArm,
    /// A pattern is matched against a type whose values it cannot match,
    /// or names a field more than once.
    PatternMismatch,
    /// A pattern binder binds a type other than the one it is matched
    /// against.
    BinderMismatch,
    /// A loop has a key that is neither an Int nor a String.
    KeyNotIntOrString,
    /// A click names a value that is not a closure that mutates, from no
    /// parameters to the empty tuple.
    ClickNotHandler,
    /// A void element has children.
    VoidElementWithChildren,
    /// An element that is not void has no children.
    ElementWithoutChildren,
    /// An identity is taken more than once: again on a path that has
    /// already taken it, or in a loop or lambda body that it is minted
    /// outside of, which would take it on every run of the body.
    IdTakenMoreThanOnce,
    /// A function or a lambda takes a cell or an identity, which only a
    /// cell binding or a mint binds.
    CellOrIdParameter,
    /// A block has an effect that its modifier does not allow. A cell, a
    /// mint and a loop render, a write of a cell mutates, and a call or an application
    /// has the modifier of what it calls. A read of a cell is allowed in a
    /// block that renders or mutates. A pure block allows none of these, and
    /// a block that renders or mutates allows its own effect.
    EffectNotAllowed,
    /// A function returns a cell or an identity.
    ReturnsUnreturnable,
    /// A cell holds a closure, or a value that holds one.
    CellHoldsClosure,
    /// An entrypoint takes a closure, or a value that holds one.
    EntrypointTakesClosure,
}

pub fn typecheck(program: &FlatProgram) -> Result<(), FlatTypeError> {
    let mut checker = Checker {
        declarations: &program.types,
        functions: &program.functions,
        lambdas: &program.lambdas,
        names: vec![None; program.value_ids.count()],
        scope: Vec::new(),
        live: Vec::new(),
    };
    for function in &program.functions {
        checker.live.clear();
        checker.scoped(|checker| {
            let expected = BoundType::of(&function.return_type);
            if matches!(expected, BoundType::Cell(_) | BoundType::ElementId) {
                return Err(FlatTypeError::ReturnsUnreturnable);
            }
            for param in &function.parameters {
                let typ = BoundType::of(&param.binder.typ);
                if matches!(typ, BoundType::Cell(_) | BoundType::ElementId) {
                    return Err(FlatTypeError::CellOrIdParameter);
                }
                checker.bind(param.binder.var, typ)?;
            }
            checker.block(&function.body, expected, function.modifier)
        })?;
    }
    for lambda in &program.lambdas {
        checker.live.clear();
        checker.scoped(|checker| {
            let expected = BoundType::of(&lambda.return_type);
            if matches!(expected, BoundType::Cell(_) | BoundType::ElementId) {
                return Err(FlatTypeError::ReturnsUnreturnable);
            }
            for capture in &lambda.captures {
                let typ = BoundType::of(&capture.typ);
                match typ {
                    BoundType::Html => return Err(FlatTypeError::HtmlReadMoreThanOnce),
                    BoundType::ElementId => return Err(FlatTypeError::IdTakenMoreThanOnce),
                    BoundType::Data(_) | BoundType::Cell(_) => {}
                }
                checker.bind(capture.var, typ)?;
            }
            for param in &lambda.parameters {
                let typ = BoundType::of(&param.typ);
                if matches!(typ, BoundType::Cell(_) | BoundType::ElementId) {
                    return Err(FlatTypeError::CellOrIdParameter);
                }
                checker.bind(param.var, typ)?;
            }
            checker.block(&lambda.body, expected, lambda.modifier)
        })?;
    }
    for entrypoint in &program.entrypoints {
        checker.live.clear();
        checker.scoped(|checker| {
            for param in &entrypoint.parameters {
                if program.types.holds_closure(&param.binder.typ) {
                    return Err(FlatTypeError::EntrypointTakesClosure);
                }
                checker.bind(param.binder.var, BoundType::Data(&param.binder.typ))?;
            }
            checker.block(&entrypoint.body, BoundType::Html, IrModifier::Render)
        })?;
    }
    Ok(())
}

fn try_all<T>(
    items: impl IntoIterator<Item = T>,
    mut f: impl FnMut(T) -> Result<bool, FlatTypeError>,
) -> Result<bool, FlatTypeError> {
    for item in items {
        if !f(item)? {
            return Ok(false);
        }
    }
    Ok(true)
}

struct Checker<'a> {
    declarations: &'a FlatTypeDeclarations,
    functions: &'a [FlatFunction],
    lambdas: &'a [FlatLambda],
    names: Vec<Option<Name<'a>>>,
    // The names bound in the blocks being checked, innermost last.
    scope: Vec<ValueId>,
    // The Html names and identities bound in the function, lambda or loop
    // body being checked that are not yet read on this path. A body starts
    // with none but the Html parameters of its lambda or function.
    live: Vec<ValueId>,
}

#[derive(Debug, Clone, Copy)]
struct Name<'a> {
    typ: BoundType<'a>,
    in_scope: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BoundType<'a> {
    Data(&'a FlatDataType),
    Cell(&'a FlatDataType),
    Html,
    ElementId,
}

impl<'a> BoundType<'a> {
    fn of(typ: &'a FlatType) -> Self {
        match typ {
            FlatType::Data(data) => BoundType::Data(data),
            FlatType::Html => BoundType::Html,
            FlatType::Cell(data) => BoundType::Cell(data),
            FlatType::ElementId => BoundType::ElementId,
        }
    }

    fn is(self, typ: &FlatType) -> bool {
        self == BoundType::of(typ)
    }
}

/// Whether no closure the type holds gives a cell or an identity.
fn is_well_formed(typ: &FlatDataType) -> bool {
    match typ {
        FlatDataType::String
        | FlatDataType::Bool
        | FlatDataType::Int
        | FlatDataType::Float
        | FlatDataType::Record(_)
        | FlatDataType::Enum(_) => true,
        FlatDataType::Array(element) | FlatDataType::Option(element) => is_well_formed(element),
        FlatDataType::Tuple(elements) => elements.iter().all(is_well_formed),
        FlatDataType::Function(signature) => {
            let parameters = (signature.parameters.iter()).all(|parameter| match parameter {
                FlatType::Data(data) => is_well_formed(data),
                _ => true,
            });
            let result = match &*signature.result {
                FlatType::Html => true,
                FlatType::Data(data) => is_well_formed(data),
                FlatType::Cell(_) | FlatType::ElementId => false,
            };
            parameters && result
        }
    }
}

impl<'a> Checker<'a> {
    fn bind(&mut self, name: ValueId, typ: BoundType<'a>) -> Result<(), FlatTypeError> {
        match typ {
            BoundType::Data(data) if !is_well_formed(data) => {
                return Err(FlatTypeError::IllTypedBinding);
            }
            BoundType::Cell(data) if self.declarations.holds_closure(data) => {
                return Err(FlatTypeError::CellHoldsClosure);
            }
            _ => {}
        }
        let bound = Name {
            typ,
            in_scope: true,
        };
        if self.names[name.index()].replace(bound).is_some() {
            return Err(FlatTypeError::BoundTwice);
        }
        self.scope.push(name);
        if matches!(typ, BoundType::Html | BoundType::ElementId) {
            self.live.push(name);
        }
        Ok(())
    }

    fn scoped(
        &mut self,
        check: impl FnOnce(&mut Self) -> Result<(), FlatTypeError>,
    ) -> Result<(), FlatTypeError> {
        let outer = self.scope.len();
        check(self)?;
        for name in self.scope.drain(outer..) {
            if let Some(bound) = &mut self.names[name.index()] {
                bound.in_scope = false;
            }
        }
        Ok(())
    }

    /// The type of a name the binding being checked reads. A read of Html or
    /// of an identity uses it up on this path.
    fn read(&mut self, name: ValueId) -> Result<BoundType<'a>, FlatTypeError> {
        let Some(bound) = self.names[name.index()].filter(|bound| bound.in_scope) else {
            return Err(FlatTypeError::ReadOutOfScope);
        };
        let reused = match bound.typ {
            BoundType::Html => FlatTypeError::HtmlReadMoreThanOnce,
            BoundType::ElementId => FlatTypeError::IdTakenMoreThanOnce,
            _ => return Ok(bound.typ),
        };
        let index = (self.live.iter())
            .position(|live| *live == name)
            .ok_or(reused)?;
        self.live.swap_remove(index);
        Ok(bound.typ)
    }

    fn data(&mut self, name: ValueId) -> Result<Option<&'a FlatDataType>, FlatTypeError> {
        Ok(match self.read(name)? {
            BoundType::Data(data) => Some(data),
            BoundType::Cell(_) | BoundType::Html | BoundType::ElementId => None,
        })
    }

    fn block(
        &mut self,
        block: &'a FlatBlock,
        expected: BoundType<'a>,
        modifier: IrModifier,
    ) -> Result<(), FlatTypeError> {
        for binding in &block.bindings {
            self.binding(binding, modifier)?;
        }
        if self.read(block.result)? != expected {
            return Err(FlatTypeError::BlockOfWrongType);
        }
        Ok(())
    }

    fn binding(
        &mut self,
        binding: &'a FlatBinding,
        modifier: IrModifier,
    ) -> Result<(), FlatTypeError> {
        let allowed = match binding.op {
            FlatOp::Cell(_) | FlatOp::Mint | FlatOp::For(_) => modifier == IrModifier::Render,
            FlatOp::CellRead(_) => modifier == IrModifier::Render,
            FlatOp::CellUpdate(..) => modifier == IrModifier::Mut,
            _ => true,
        };
        if !allowed {
            return Err(FlatTypeError::EffectNotAllowed);
        }
        let declared = BoundType::of(&binding.binder.typ);
        let html = declared == BoundType::Html;
        let produces = match &binding.op {
            FlatOp::Data(op) => match declared {
                BoundType::Data(declared) => self.data_op(op, declared)?,
                _ => false,
            },
            FlatOp::Call(callee, args) => {
                let callee = self.callee(*callee)?;
                if !modifier.allows(callee.modifier) {
                    return Err(FlatTypeError::EffectNotAllowed);
                }
                declared == BoundType::of(&callee.return_type)
                    && self.args(
                        args,
                        (callee.parameters.iter()).map(|p| BoundType::of(&p.binder.typ)),
                    )?
            }
            FlatOp::Match(match_) => {
                if matches!(declared, BoundType::Cell(_) | BoundType::ElementId) {
                    return Err(FlatTypeError::IllTypedBinding);
                }
                let subject = match self.data(match_.subject)? {
                    Some(FlatDataType::Function(_)) | None => {
                        return Err(FlatTypeError::MatchSubjectNotData);
                    }
                    Some(subject) => subject,
                };
                // Html and identities stay live after the match only if no
                // arm reads them.
                let before = self.live.clone();
                let mut after = before.clone();
                for arm in &match_.arms {
                    self.live = before.clone();
                    self.scoped(|checker| {
                        checker.data_pattern(&arm.pattern, subject)?;
                        checker.block(&arm.body, declared, modifier)
                    })?;
                    after.retain(|name| self.live.contains(name));
                }
                self.live = after;
                self.arms(match_, subject)?;
                true
            }
            FlatOp::Cell(initial) => match declared {
                BoundType::Cell(data) => {
                    self.scoped(|checker| {
                        checker.block(initial, BoundType::Data(data), IrModifier::Pure)
                    })?;
                    true
                }
                _ => false,
            },
            FlatOp::Mint => declared == BoundType::ElementId,
            FlatOp::CellUpdate(cell, update) => match (self.read(*cell)?, self.read(*update)?) {
                (BoundType::Cell(data), BoundType::Data(FlatDataType::Function(signature))) => {
                    let data = FlatType::Data(data.clone());
                    declared == BoundType::Data(&FlatDataType::UNIT)
                        && signature.modifier == IrModifier::Pure
                        && signature.parameters == [data.clone()]
                        && *signature.result == data
                }
                _ => false,
            },
            FlatOp::CellRead(cell) => match declared {
                BoundType::Data(data) => self.read(*cell)? == BoundType::Cell(data),
                _ => false,
            },
            FlatOp::Text(_) => html,
            FlatOp::Escape(name) => html && self.read(*name)?.is(&FlatType::STRING),
            FlatOp::Concat(parts) => {
                html && try_all(parts, |part| Ok(self.read(*part)?.is(&FlatType::HTML)))?
            }
            FlatOp::Element(element) => {
                if self.read(element.identity)? != BoundType::ElementId {
                    return Err(FlatTypeError::IllTypedBinding);
                }
                let attributes = try_all(element.attributes.values(), |attribute| {
                    Ok(match attribute {
                        FlatAttribute::Value(name) => self.read(*name)?.is(&FlatType::STRING),
                        FlatAttribute::Presence(name) => self.read(*name)?.is(&FlatType::BOOL),
                        FlatAttribute::Constant(_) => true,
                    })
                })?;
                if let Some(handler) = element.on_click {
                    let BoundType::Data(FlatDataType::Function(signature)) = self.read(handler)?
                    else {
                        return Err(FlatTypeError::ClickNotHandler);
                    };
                    if signature.modifier != IrModifier::Mut
                        || !signature.parameters.is_empty()
                        || *signature.result != FlatType::Data(FlatDataType::UNIT)
                    {
                        return Err(FlatTypeError::ClickNotHandler);
                    }
                }
                let children = match (element.element.is_void(), element.children) {
                    (true, None) => true,
                    (true, Some(_)) => return Err(FlatTypeError::VoidElementWithChildren),
                    (false, None) => return Err(FlatTypeError::ElementWithoutChildren),
                    (false, Some(children)) => self.read(children)?.is(&FlatType::HTML),
                };
                html && attributes && children
            }
            FlatOp::For(for_) => {
                let Some(FlatDataType::Array(element)) = self.data(for_.array)? else {
                    return Err(FlatTypeError::IllTypedBinding);
                };
                // The body runs once per element, so it reads no Html bound
                // outside of it.
                let outer = std::mem::take(&mut self.live);
                self.scoped(|checker| {
                    if let Some(var) = &for_.var {
                        if var.typ != **element {
                            return Err(FlatTypeError::IllTypedBinding);
                        }
                        checker.bind(var.var, BoundType::Data(&var.typ))?;
                    }
                    if let Some(key) = &for_.key {
                        checker.scoped(|checker| {
                            for binding in &key.bindings {
                                checker.binding(binding, IrModifier::Pure)?;
                            }
                            let typ = checker.read(key.result)?;
                            if !typ.is(&FlatType::INT) && !typ.is(&FlatType::STRING) {
                                return Err(FlatTypeError::KeyNotIntOrString);
                            }
                            Ok(())
                        })?;
                    }
                    checker.block(&for_.body, BoundType::Html, modifier)
                })?;
                self.live = outer;
                html
            }
            FlatOp::Lambda(id, captures) => {
                let BoundType::Data(FlatDataType::Function(signature)) = declared else {
                    return Err(FlatTypeError::IllTypedBinding);
                };
                let lambdas = self.lambdas;
                let lambda = (lambdas.iter())
                    .find(|lambda| lambda.id == *id)
                    .ok_or(FlatTypeError::UndeclaredLambda)?;
                signature.modifier == lambda.modifier
                    && *signature.result == lambda.return_type
                    && signature.parameters.len() == lambda.parameters.len()
                    && (signature.parameters.iter().zip(&lambda.parameters))
                        .all(|(typ, param)| *typ == param.typ)
                    && self.args(
                        captures,
                        lambda
                            .captures
                            .iter()
                            .map(|capture| BoundType::of(&capture.typ)),
                    )?
            }
            FlatOp::Apply(closure, args) => {
                let BoundType::Data(FlatDataType::Function(signature)) = self.read(*closure)?
                else {
                    return Err(FlatTypeError::IllTypedBinding);
                };
                if !modifier.allows(signature.modifier) {
                    return Err(FlatTypeError::EffectNotAllowed);
                }
                declared == BoundType::of(&signature.result)
                    && self.args(args, signature.parameters.iter().map(BoundType::of))?
            }
        };
        if !produces {
            return Err(FlatTypeError::IllTypedBinding);
        }
        self.bind(binding.binder.var, declared)
    }

    fn callee(&self, callee: FunctionId) -> Result<&'a FlatFunction, FlatTypeError> {
        self.functions
            .iter()
            .find(|declared| declared.function.id == callee)
            .ok_or(FlatTypeError::UndeclaredCallee)
    }

    fn args<'t>(
        &mut self,
        args: &[ValueId],
        params: impl ExactSizeIterator<Item = BoundType<'t>>,
    ) -> Result<bool, FlatTypeError> {
        Ok(args.len() == params.len()
            && try_all(args.iter().zip(params), |(arg, param)| {
                Ok(self.read(*arg)? == param)
            })?)
    }

    fn data_op(&mut self, op: &IrDataOp, declared: &FlatDataType) -> Result<bool, FlatTypeError> {
        let mut all = |names: &[ValueId], typ: FlatType| {
            try_all(names, |name| Ok(self.read(*name)?.is(&typ)))
        };
        let declared_data = Some(declared);
        Ok(match op {
            IrDataOp::StringLiteral(_) => *declared == FlatDataType::String,
            IrDataOp::IntLiteral(_) => *declared == FlatDataType::Int,
            IrDataOp::FloatLiteral(_) => *declared == FlatDataType::Float,
            IrDataOp::BoolLiteral(_) => *declared == FlatDataType::Bool,
            IrDataOp::Array(elements) => match declared_data {
                Some(FlatDataType::Array(element)) => {
                    try_all(elements, |name| Ok(self.data(*name)? == Some(&**element)))?
                }
                _ => false,
            },
            IrDataOp::Tuple(elements) => match declared_data {
                Some(FlatDataType::Tuple(declared)) => {
                    elements.len() == declared.len()
                        && try_all(elements.iter().zip(declared), |(name, typ)| {
                            Ok(self.data(*name)? == Some(typ))
                        })?
                }
                _ => false,
            },
            IrDataOp::Record(fields) => match declared {
                FlatDataType::Record(name) => match self.declarations.record(name) {
                    Some(record) => self.fields(fields, &record.fields)?,
                    None => false,
                },
                _ => false,
            },
            IrDataOp::Enum(variant, fields) => match declared {
                FlatDataType::Enum(name) => {
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
                Some(FlatDataType::Option(element)) => self.data(*inner)? == Some(&**element),
                _ => false,
            },
            IrDataOp::OptionNone => matches!(declared_data, Some(FlatDataType::Option(_))),
            IrDataOp::StringConcat(parts) => {
                all(parts, FlatType::STRING)? && *declared == FlatDataType::String
            }
            IrDataOp::NumericAdd(typ, left, right)
            | IrDataOp::NumericSubtract(typ, left, right)
            | IrDataOp::NumericMultiply(typ, left, right) => {
                let typ = match typ {
                    NumericType::Int => FlatDataType::Int,
                    NumericType::Float => FlatDataType::Float,
                };
                all(&[*left, *right], FlatType::Data(typ.clone()))? && *declared == typ
            }
            IrDataOp::Equals(typ, left, right) => {
                let typ = match typ {
                    EquatableType::String => FlatType::STRING,
                    EquatableType::Bool => FlatType::BOOL,
                    EquatableType::Int => FlatType::INT,
                    EquatableType::Float => FlatType::FLOAT,
                };
                all(&[*left, *right], typ)? && *declared == FlatDataType::Bool
            }
            IrDataOp::LessThan(typ, left, right) | IrDataOp::LessThanOrEqual(typ, left, right) => {
                let typ = match typ {
                    ComparableType::Int => FlatType::INT,
                    ComparableType::Float => FlatType::FLOAT,
                };
                all(&[*left, *right], typ)? && *declared == FlatDataType::Bool
            }
            IrDataOp::ArrayFromRange(start, end) => {
                all(&[*start, *end], FlatType::INT)?
                    && matches!(declared_data, Some(FlatDataType::Array(element)) if **element == FlatDataType::Int)
            }
            IrDataOp::NumericNegation(typ, operand) => {
                let typ = match typ {
                    NumericType::Int => FlatDataType::Int,
                    NumericType::Float => FlatDataType::Float,
                };
                all(&[*operand], FlatType::Data(typ.clone()))? && *declared == typ
            }
            IrDataOp::BoolNegation(operand) => {
                all(&[*operand], FlatType::BOOL)? && *declared == FlatDataType::Bool
            }
            IrDataOp::ArrayLength(operand) => {
                matches!(self.data(*operand)?, Some(FlatDataType::Array(_)))
                    && *declared == FlatDataType::Int
            }
            IrDataOp::ArrayIsEmpty(operand) => {
                matches!(self.data(*operand)?, Some(FlatDataType::Array(_)))
                    && *declared == FlatDataType::Bool
            }
            IrDataOp::ArrayGet(array, index) => {
                all(&[*index], FlatType::INT)?
                    && match (self.data(*array)?, declared_data) {
                        (Some(FlatDataType::Array(element)), Some(FlatDataType::Option(inner))) => {
                            element == inner
                        }
                        _ => false,
                    }
            }
            IrDataOp::ArrayEnumerate(operand) => match (self.data(*operand)?, declared_data) {
                (Some(FlatDataType::Array(element)), Some(FlatDataType::Array(pair))) => {
                    matches!(pair.as_ref(), FlatDataType::Tuple(types)
                            if matches!(types.as_slice(), [FlatDataType::Int, t] if t == element.as_ref()))
                }
                _ => false,
            },
            IrDataOp::StringIsEmpty(operand) => {
                all(&[*operand], FlatType::STRING)? && *declared == FlatDataType::Bool
            }
            IrDataOp::OptionIsSome(operand) | IrDataOp::OptionIsNone(operand) => {
                matches!(self.data(*operand)?, Some(FlatDataType::Option(_)))
                    && *declared == FlatDataType::Bool
            }
            IrDataOp::IntToString(operand) => {
                all(&[*operand], FlatType::INT)? && *declared == FlatDataType::String
            }
            IrDataOp::FloatToInt(operand) => {
                all(&[*operand], FlatType::FLOAT)? && *declared == FlatDataType::Int
            }
            IrDataOp::IntToFloat(operand) => {
                all(&[*operand], FlatType::INT)? && *declared == FlatDataType::Float
            }
            IrDataOp::FieldAccess(object, field) => match self.data(*object)? {
                Some(FlatDataType::Record(name)) => self
                    .declarations
                    .record(name)
                    .and_then(|record| record.fields.iter().find(|f| f.name == *field))
                    .is_some_and(|field| Some(&field.typ) == declared_data),
                _ => false,
            },
            IrDataOp::TupleIndex(tuple, index) => match self.data(*tuple)? {
                Some(FlatDataType::Tuple(elements)) => elements.get(*index) == declared_data,
                _ => false,
            },
        })
    }

    fn fields(
        &mut self,
        given: &[(FieldName, ValueId)],
        declared: &[FlatField],
    ) -> Result<bool, FlatTypeError> {
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

    fn arms(&self, match_: &FlatMatch, subject: &FlatDataType) -> Result<(), FlatTypeError> {
        let patterns: Vec<FlatPattern> =
            match_.arms.iter().map(|arm| arm.pattern.clone()).collect();
        let (_, reachable, missing) = compile_arms(self.declarations, &patterns, subject.clone());
        if !missing.is_empty() {
            return Err(FlatTypeError::NoArmMatches);
        }
        if (0..patterns.len()).any(|index| !reachable.contains(&index)) {
            return Err(FlatTypeError::UnreachableArm);
        }
        Ok(())
    }

    fn data_pattern(
        &mut self,
        pattern: &'a FlatPattern,
        typ: &FlatDataType,
    ) -> Result<(), FlatTypeError> {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => {
                if binder.typ != *typ {
                    return Err(FlatTypeError::BinderMismatch);
                }
                self.bind(binder.var, BoundType::Data(&binder.typ))?;
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                let declarations = self.declarations;
                let resolved: Option<(&[FlatDataType], &[FlatField])> = match (constructor, typ) {
                    (Constructor::BoolTrue | Constructor::BoolFalse, FlatDataType::Bool) => {
                        Some((&[], &[]))
                    }
                    (Constructor::OptionNone, FlatDataType::Option(_)) => Some((&[], &[])),
                    (Constructor::OptionSome, FlatDataType::Option(inner)) => {
                        Some((std::slice::from_ref(&**inner), &[]))
                    }
                    (Constructor::Tuple, FlatDataType::Tuple(elements)) => Some((elements, &[])),
                    (Constructor::Record { type_name }, FlatDataType::Record(name))
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
                        FlatDataType::Enum(name),
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
                    .ok_or(FlatTypeError::PatternMismatch)?;
                for (arg, arg_type) in args.iter().zip(arg_types) {
                    self.data_pattern(arg, arg_type)?;
                }
                for (index, field) in fields.iter().enumerate() {
                    if fields[..index]
                        .iter()
                        .any(|earlier| earlier.name == field.name)
                    {
                        return Err(FlatTypeError::PatternMismatch);
                    }
                    let declared = declared_fields
                        .get(field.index)
                        .filter(|declared| declared.name == field.name)
                        .ok_or(FlatTypeError::PatternMismatch)?;
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

    use super::FlatTypeError;
    use crate::hop::typing::{ComparableType, EquatableType, NumericType, Ty};
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_type::{FlatType, IrModifier};
    use crate::ir::flat_type_declarations_builder::FlatTypeDeclarationsBuilder;

    #[test]
    fn should_reject_a_closure_that_reads_html_bound_outside_it() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let outer = t.text("x");
                let closure = t.render_lambda([], |_, _| outer);
                t.apply(closure, [])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::HtmlReadMoreThanOnce);
    }

    #[test]
    fn should_reject_html_read_twice() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let html = t.text("x");
                t.html_concat([html, html])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::HtmlReadMoreThanOnce);
    }

    #[test]
    fn should_reject_html_read_in_a_loop_body_it_is_bound_outside_of() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                let html = t.text("x");
                t.html_for(t.param("items"), |_, _| html)
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::HtmlReadMoreThanOnce);
    }

    #[test]
    fn should_reject_html_read_after_a_match_with_an_arm_that_reads_it() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let html = t.text("x");
                let picked = t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |_, _| html);
                    m.arm(Pat::False, |t, _| t.text("y"));
                });
                t.html_concat([picked, html])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::HtmlReadMoreThanOnce);
    }

    #[test]
    fn should_accept_html_read_in_every_arm_of_a_match() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let html = t.text("x");
                t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |_, _| html);
                    m.arm(Pat::False, |t, _| t.html_concat([t.text("y"), html]));
                })
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_match_that_produces_a_closure() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [("double", Ty::Bool)], |t| {
                let scale = t.match_(t.param("double"), |m| {
                    m.arm(Pat::True, |t, _| {
                        t.lambda([FlatType::INT], |t, params| {
                            t.add(NumericType::Int, params[0], params[0])
                        })
                    });
                    m.arm(Pat::False, |t, _| {
                        t.lambda([FlatType::INT], |_, params| params[0])
                    });
                });
                let scaled = t.apply(scale, [t.int(1)]);
                t.escape(t.int_to_string(scaled))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_match_that_produces_a_closure_that_renders() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [("bold", Ty::Bool)], |t| {
                let wrap = t.match_(t.param("bold"), |m| {
                    m.arm(Pat::True, |t, _| t.render_lambda([], |t, _| t.text("b")));
                    m.arm(Pat::False, |t, _| t.render_lambda([], |t, _| t.text("i")));
                });
                t.apply(wrap, [])
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_an_application_with_the_wrong_arguments() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let double = t.lambda([FlatType::INT], |t, params| {
                    t.add(NumericType::Int, params[0], params[0])
                });
                let doubled = t.apply(double, [t.str("one")]);
                t.escape(t.int_to_string(doubled))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_an_op_with_ill_typed_operands() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let sum = t.add(NumericType::Int, t.int(1), t.str("one"));
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_an_arithmetic_op_of_a_type_its_operands_do_not_have() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let sum = t.add(NumericType::Float, t.int(1), t.int(2));
                t.escape(t.int_to_string(t.float_to_int(sum)))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_negation_of_a_type_its_operand_does_not_have() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let negated = t.neg(NumericType::Int, t.float(1.0));
                t.escape(t.int_to_string(negated))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_an_equality_of_a_type_its_operands_do_not_have() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let same = t.eq(EquatableType::Int, t.str("a"), t.str("a"));
                t.match_(same, |m| {
                    m.arm(Pat::True, |t, _| t.text("same"));
                    m.arm(Pat::False, |t, _| t.text("other"));
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_comparison_of_a_type_its_operands_do_not_have() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let less = t.lt(ComparableType::Float, t.int(1), t.int(2));
                t.match_(less, |m| {
                    m.arm(Pat::True, |t, _| t.text("less"));
                    m.arm(Pat::False, |t, _| t.text("not less"));
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_block_of_the_wrong_type() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| t.int(1))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::BlockOfWrongType);
    }

    #[test]
    fn should_reject_a_call_with_the_wrong_arguments() {
        let error = FlatProgramBuilder::new()
            .render_function("show", [("n", Ty::Int)], Ty::Html, |t| {
                t.escape(t.int_to_string(t.param("n")))
            })
            .entrypoint("Test", [], |t| t.call("show", [t.str("one")]))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
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
        assert_eq!(error, FlatTypeError::NoArmMatches);
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
        assert_eq!(error, FlatTypeError::UnreachableArm);
    }

    #[test]
    fn should_reject_a_match_on_a_cell() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                t.match_(count, |m| {
                    m.arm(Pat::Wildcard, |t, _| t.text("any"));
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::MatchSubjectNotData);
    }

    #[test]
    fn should_reject_a_match_on_a_closure() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let closure = t.lambda([], |t, _| t.int(0));
                t.match_(closure, |m| {
                    m.arm(Pat::Wildcard, |t, _| t.text("any"));
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::MatchSubjectNotData);
    }

    #[test]
    fn should_accept_a_match_that_binds_a_closure_a_tuple_holds() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let closure = t.lambda([], |t, _| t.text("x"));
                let pair = t.tuple([closure, t.some(closure)]);
                t.match_(pair, |m| {
                    m.arm(
                        Pat::Tuple(vec![Pat::bind("first"), Pat::some(Pat::bind("second"))]),
                        |t, bindings| {
                            let first = t.apply(bindings.binding("first"), []);
                            let second = t.apply(bindings.binding("second"), []);
                            t.html_concat([first, second])
                        },
                    );
                    m.arm(Pat::Wildcard, |t, _| t.text("none"));
                })
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_cell_of_a_tuple_that_holds_a_closure() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                t.cell(|t| {
                    let closure = t.lambda([], |t, _| t.int(0));
                    t.tuple([closure])
                });
                t.text("x")
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::CellHoldsClosure);
    }

    #[test]
    fn should_reject_a_cell_of_a_recursive_enum_that_holds_a_closure() {
        let types = FlatTypeDeclarationsBuilder::new()
            .enum_(
                "Tree",
                [
                    ("Leaf", vec![]),
                    (
                        "Node",
                        vec![
                            ("children", Ty::array(Ty::named("Tree"))),
                            ("show", Ty::function([], Ty::Html)),
                        ],
                    ),
                ],
            )
            .build();
        let tree = types.ty(Ty::named("Tree"));
        let error = FlatProgramBuilder::with_types(types)
            .entrypoint("Test", [], |t| {
                t.cell(|t| t.enum_variant_with_fields(tree, "Leaf", []));
                t.text("x")
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::CellHoldsClosure);
    }

    #[test]
    fn should_reject_an_entrypoint_that_takes_a_record_that_holds_a_closure() {
        let types = FlatTypeDeclarationsBuilder::new()
            .record("Column", [("cell", Ty::function([Ty::Int], Ty::Html))])
            .build();
        let error = FlatProgramBuilder::with_types(types)
            .entrypoint("Test", [("columns", Ty::array(Ty::named("Column")))], |t| {
                t.text("x")
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EntrypointTakesClosure);
    }

    #[test]
    fn should_reject_a_pattern_that_names_a_field_twice() {
        let types = FlatTypeDeclarationsBuilder::new()
            .record("User", [("name", Ty::String)])
            .build();
        let error = FlatProgramBuilder::with_types(types)
            .entrypoint("Test", [("user", Ty::named("User"))], |t| {
                t.match_(t.param("user"), |m| {
                    m.arm(
                        Pat::Record(vec![
                            ("name".into(), Pat::Wildcard),
                            ("name".into(), Pat::Wildcard),
                        ]),
                        |t, _| t.text("user"),
                    );
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::PatternMismatch);
    }

    #[test]
    fn should_reject_a_loop_whose_key_is_neither_an_int_nor_a_string() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::Bool))], |t| {
                t.html_for_keyed(t.param("items"), |_, item| item)
                    .body(|t, _| t.text("item"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::KeyNotIntOrString);
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
                let sum = t.add(NumericType::Int, inner.get().unwrap(), picked);
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ReadOutOfScope);
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
        assert_eq!(error, FlatTypeError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_read_of_a_name_bound_in_the_key_of_a_loop_in_its_body() {
        let key = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                t.html_for_keyed(t.param("items"), |t, item| {
                    let doubled = t.add(NumericType::Int, item, item);
                    key.set(Some(doubled));
                    doubled
                })
                .body(|t, _| t.escape(t.int_to_string(key.get().unwrap())))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_read_of_a_parameter_of_a_lambda_after_the_lambda() {
        let param = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let id = t.lambda([FlatType::INT], |_, params| {
                    param.set(Some(params[0]));
                    params[0]
                });
                let one = t.apply(id, [t.int(1)]);
                let sum = t.add(NumericType::Int, one, param.get().unwrap());
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ReadOutOfScope);
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
                let sum = t.add(
                    NumericType::Int,
                    t.call("id", [t.int(1)]),
                    param.get().unwrap(),
                );
                t.escape(t.int_to_string(sum))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_cell_update_of_a_cell_of_an_arm_after_the_match() {
        let inner = Cell::new(None);
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let picked = t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |t, _| {
                        let cell = t.cell(|t| t.int(1));
                        inner.set(Some(cell));
                        t.text("on")
                    });
                    m.arm(Pat::False, |t, _| t.text("off"));
                });
                let zero = t.lambda([FlatType::INT], |t, _| t.int(0));
                let reset = t.event_handler(|t| t.cell_update(inner.get().unwrap(), zero));
                let button =
                    t.element_on_click(t.mint(), "button", vec![], reset, Some(t.text("x")));
                t.html_concat([picked, button])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ReadOutOfScope);
    }

    #[test]
    fn should_reject_a_cell_update_of_a_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.int(0);
                let one = t.lambda([FlatType::INT], |t, _| t.int(1));
                let reset = t.event_handler(|t| t.cell_update(count, one));
                t.element_on_click(t.mint(), "button", vec![], reset, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_cell_update_with_a_closure_of_another_type() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let not = t.lambda([FlatType::BOOL], |t, params| t.not(params[0]));
                let flip = t.event_handler(|t| t.cell_update(count, not));
                t.element_on_click(t.mint(), "button", vec![], flip, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_cell_update_with_a_closure_that_mutates() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let same = t.mut_lambda([FlatType::INT], |_, params| params[0]);
                let keep = t.event_handler(|t| t.cell_update(count, same));
                t.element_on_click(t.mint(), "button", vec![], keep, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IllTypedBinding);
    }

    #[test]
    fn should_reject_a_cell_read_in_a_body_that_mutates() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let read = t.event_handler(|t| {
                    t.cell_read(count);
                    t.tuple([])
                });
                t.element_on_click(t.mint(), "button", vec![], read, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_cell_in_a_pure_block() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.cell_read(t.cell(|t| t.int(0))));
                t.escape(t.int_to_string(t.cell_read(count)))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_click_whose_handler_is_a_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                t.element_on_click(t.mint(), "button", vec![], t.int(1), Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_click_whose_handler_does_not_mutate() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let nothing = t.lambda([], |t, _| t.tuple([]));
                t.element_on_click(t.mint(), "button", vec![], nothing, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_click_whose_handler_takes_a_parameter() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let set = t.mut_lambda([FlatType::INT], |t, params| {
                    let value = params[0];
                    let to_value = t.lambda([FlatType::INT], |_, _| value);
                    t.cell_update(count, to_value)
                });
                t.element_on_click(t.mint(), "button", vec![], set, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_click_whose_handler_gives_a_value() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell_read(t.cell(|t| t.int(0)));
                let next = t.lambda([], |t, _| t.add(NumericType::Int, count, t.int(1)));
                t.element_on_click(t.mint(), "button", vec![], next, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ClickNotHandler);
    }

    #[test]
    fn should_reject_a_function_that_takes_a_cell() {
        let error = FlatProgramBuilder::new()
            .render_function("Show", [("count", Ty::cell(Ty::Int))], Ty::Html, |t| {
                t.escape(t.int_to_string(t.cell_read(t.param("count"))))
            })
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                t.call("Show", [count])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::CellOrIdParameter);
    }

    #[test]
    fn should_reject_a_void_element_with_children() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                t.element(t.mint(), "br", vec![], Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::VoidElementWithChildren);
    }

    #[test]
    fn should_reject_an_element_without_children() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| t.element(t.mint(), "div", vec![], None))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ElementWithoutChildren);
    }

    #[test]
    fn should_reject_an_identity_that_two_elements_take() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let id = t.mint();
                let first = t.element(id, "b", vec![], Some(t.text("x")));
                let second = t.element(id, "i", vec![], Some(t.text("y")));
                t.html_concat([first, second])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IdTakenMoreThanOnce);
    }

    #[test]
    fn should_accept_an_identity_taken_in_every_arm_of_a_match() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let id = t.mint();
                t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |t, _| {
                        t.element(id, "b", vec![], Some(t.text("x")))
                    });
                    m.arm(Pat::False, |t, _| {
                        t.element(id, "b", vec![], Some(t.text("y")))
                    });
                })
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_an_identity_taken_after_a_match_with_an_arm_that_takes_it() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let id = t.mint();
                let picked = t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |t, _| {
                        t.element(id, "b", vec![], Some(t.text("x")))
                    });
                    m.arm(Pat::False, |t, _| t.text("y"));
                });
                let after = t.element(id, "i", vec![], Some(t.text("z")));
                t.html_concat([picked, after])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IdTakenMoreThanOnce);
    }

    #[test]
    fn should_reject_an_identity_minted_outside_the_loop_body_that_takes_it() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                let id = t.mint();
                t.html_for(t.param("items"), |t, item| {
                    t.element(id, "li", vec![], Some(t.escape(item)))
                })
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IdTakenMoreThanOnce);
    }

    #[test]
    fn should_reject_an_identity_that_a_lambda_takes_from_outside_it() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let id = t.mint();
                let row =
                    t.render_lambda([], |t, _| t.element(id, "li", vec![], Some(t.text("x"))));
                t.apply(row, [])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::IdTakenMoreThanOnce);
    }

    #[test]
    fn should_reject_a_lambda_that_takes_an_identity() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let row = t.render_lambda([FlatType::ElementId], |t, params| {
                    t.element(params[0], "li", vec![], Some(t.text("x")))
                });
                t.apply(row, [t.mint()])
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::CellOrIdParameter);
    }

    #[test]
    fn should_accept_a_function_that_renders_a_value_from_a_cell() {
        FlatProgramBuilder::new()
            .render_function("counter", [("start", Ty::Int)], Ty::Int, |t| {
                let count = t.cell(|t| t.param("start"));
                t.cell_read(count)
            })
            .entrypoint("Test", [], |t| {
                let count = t.call("counter", [t.int(0)]);
                t.escape(t.int_to_string(count))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_mint_in_a_function_that_renders_a_value() {
        FlatProgramBuilder::new()
            .render_function("counter", [], Ty::Int, |t| {
                t.mint();
                t.int(0)
            })
            .entrypoint("Test", [], |t| {
                let count = t.call("counter", []);
                t.escape(t.int_to_string(count))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_cell_in_an_arm_of_a_match_of_a_value_in_a_block_that_renders() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [("on", Ty::Bool)], |t| {
                let count = t.match_(t.param("on"), |m| {
                    m.arm(Pat::True, |t, _| t.cell_read(t.cell(|t| t.int(1))));
                    m.arm(Pat::False, |t, _| t.int(0));
                });
                t.escape(t.int_to_string(count))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_call_of_a_function_that_renders_in_a_pure_block() {
        let error = FlatProgramBuilder::new()
            .render_function("counter", [], Ty::Int, |t| {
                t.cell_read(t.cell(|t| t.int(0)))
            })
            .function("twice", [], Ty::Int, |t| {
                let count = t.call("counter", []);
                t.add(NumericType::Int, count, count)
            })
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("twice", [])))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_call_of_a_function_that_renders_in_a_cell_initializer() {
        let error = FlatProgramBuilder::new()
            .render_function("counter", [], Ty::Int, |t| {
                t.cell_read(t.cell(|t| t.int(0)))
            })
            .entrypoint("Test", [], |t| {
                let count = t.cell_read(t.cell(|t| t.call("counter", [])));
                t.escape(t.int_to_string(count))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_call_of_a_function_that_renders_in_the_key_of_a_loop() {
        let error = FlatProgramBuilder::new()
            .render_function("counter", [], Ty::Int, |t| {
                t.cell_read(t.cell(|t| t.int(0)))
            })
            .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                t.html_for_keyed(t.param("items"), |t, _| t.call("counter", []))
                    .body(|t, _| t.text("item"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_call_of_a_function_that_renders_in_a_handler() {
        let error = FlatProgramBuilder::new()
            .render_function("counter", [], Ty::Int, |t| {
                t.cell_read(t.cell(|t| t.int(0)))
            })
            .entrypoint("Test", [], |t| {
                let handler = t.event_handler(|t| {
                    t.call("counter", []);
                    t.tuple([])
                });
                t.element_on_click(t.mint(), "button", vec![], handler, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_accept_a_function_that_returns_html_without_the_render_modifier() {
        FlatProgramBuilder::new()
            .function("Wrap", [("content", Ty::Html)], Ty::Html, |t| {
                t.html_concat([t.text("["), t.param("content"), t.text("]")])
            })
            .function("width", [("content", Ty::Html)], Ty::Int, |t| {
                t.call("Wrap", [t.param("content")]);
                t.int(2)
            })
            .entrypoint("Test", [], |t| {
                let pair = t.lambda([FlatType::HTML], |t, params| {
                    t.html_concat([params[0], t.text("!")])
                });
                let width = t.call("width", [t.text("a")]);
                let shown = t.apply(pair, [t.call("Wrap", [t.text("b")])]);
                t.html_concat([t.escape(t.int_to_string(width)), shown])
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_mint_in_a_function_that_returns_html_without_the_render_modifier() {
        let error = FlatProgramBuilder::new()
            .function("Wrap", [("content", Ty::Html)], Ty::Html, |t| {
                t.element(t.mint(), "b", [], Some(t.param("content")))
            })
            .entrypoint("Test", [], |t| t.call("Wrap", [t.text("a")]))
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_loop_in_a_function_without_the_render_modifier() {
        let error = FlatProgramBuilder::new()
            .function("count", [], Ty::Int, |t| {
                let numbers = t.array_from_range(t.int(0), t.int(2));
                t.html_for(numbers, |t, _| t.text("x"));
                t.int(0)
            })
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("count", [])))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_accept_html_in_a_function_that_renders_a_value() {
        FlatProgramBuilder::new()
            .render_function("count", [], Ty::Int, |t| {
                t.text("x");
                t.int(0)
            })
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("count", [])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_function_that_renders_a_value_and_takes_html() {
        FlatProgramBuilder::new()
            .render_function("count", [("content", Ty::Html)], Ty::Int, |t| t.int(0))
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("count", [t.text("x")])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_an_html_parameter_of_a_function_that_returns_a_value() {
        FlatProgramBuilder::new()
            .function("count", [("content", Ty::Html)], Ty::Int, |t| t.int(0))
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("count", [t.text("x")])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_function_that_returns_a_value_and_takes_a_closure_that_renders_html() {
        FlatProgramBuilder::new()
            .function(
                "count",
                [("row", Ty::render_function([Ty::Int], Ty::Html))],
                Ty::Int,
                |t| t.int(0),
            )
            .entrypoint("Test", [], |t| {
                let row = t.render_lambda([FlatType::INT], |t, _| t.text("x"));
                t.escape(t.int_to_string(t.call("count", [row])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_closure_that_returns_a_value_and_takes_a_closure_that_renders_html() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let row = t.render_lambda([FlatType::HTML], |_, params| params[0]);
                let wrap = FlatType::function(IrModifier::Render, [FlatType::HTML], FlatType::HTML);
                let count = t.lambda([wrap], |t, _| t.int(0));
                t.escape(t.int_to_string(t.apply(count, [row])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_closure_that_renders_html_in_a_block_that_produces_a_value() {
        FlatProgramBuilder::new()
            .function(
                "count",
                [("row", Ty::render_function([], Ty::Html))],
                Ty::Int,
                |t| t.int(0),
            )
            .entrypoint("Test", [("show", Ty::Bool)], |t| {
                let count = t.match_(t.param("show"), |m| {
                    m.arm(Pat::True, |t, _| {
                        let row = t.render_lambda([], |t, _| t.text("x"));
                        t.call("count", [row])
                    });
                    m.arm(Pat::False, |t, _| t.int(0));
                });
                t.escape(t.int_to_string(count))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_an_application_of_a_closure_that_renders_html_in_a_block_that_produces_a_value()
     {
        FlatProgramBuilder::new()
            .render_function(
                "count",
                [("row", Ty::render_function([], Ty::Html))],
                Ty::Int,
                |t| {
                    t.apply(t.param("row"), []);
                    t.int(0)
                },
            )
            .entrypoint("Test", [], |t| {
                let row = t.render_lambda([], |t, _| t.text("x"));
                t.escape(t.int_to_string(t.call("count", [row])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_an_application_of_a_closure_that_renders_a_value() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let counter = t.render_lambda([], |t, _| t.cell_read(t.cell(|t| t.int(0))));
                let count = t.apply(counter, []);
                t.escape(t.int_to_string(count))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_an_application_of_a_closure_that_renders_in_a_handler() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let counter = t.render_lambda([], |t, _| t.cell_read(t.cell(|t| t.int(0))));
                let handler = t.event_handler(|t| {
                    t.apply(counter, []);
                    t.tuple([])
                });
                t.element_on_click(t.mint(), "button", vec![], handler, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_accept_html_in_a_closure_that_renders_a_value() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.render_lambda([], |t, _| {
                    t.text("x");
                    t.int(0)
                });
                t.escape(t.int_to_string(t.apply(count, [])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_closure_that_renders_as_a_click_handler() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let handler = t.render_lambda([], |t, _| t.tuple([]));
                t.element_on_click(t.mint(), "button", vec![], handler, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ClickNotHandler);
    }

    #[test]
    fn should_accept_a_closure_that_returns_a_value_and_takes_html() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                t.lambda([FlatType::HTML], |t, _| t.int(0));
                t.text("x")
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_function_that_renders_a_closure() {
        FlatProgramBuilder::new()
            .render_function("make", [], Ty::function([], Ty::Int), |t| {
                t.lambda([], |t, _| t.int(0))
            })
            .entrypoint("Test", [], |t| t.text("x"))
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_an_update_outside_of_a_body_that_mutates() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let one = t.lambda([FlatType::INT], |t, _| t.int(1));
                t.lambda([], |t, _| t.cell_update(count, one));
                t.text("x")
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_an_application_of_a_closure_that_mutates_in_a_block_that_renders() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let one = t.lambda([FlatType::INT], |t, _| t.int(1));
                let bump = t.mut_lambda([], |t, _| t.cell_update(count, one));
                t.apply(bump, []);
                t.text("x")
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_call_of_a_function_that_mutates_in_a_pure_function() {
        let error = FlatProgramBuilder::new()
            .mut_function("seven", [], Ty::Int, |t| t.int(7))
            .function("eight", [], Ty::Int, |t| {
                t.add(NumericType::Int, t.call("seven", []), t.int(1))
            })
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("eight", [])))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_accept_a_function_that_mutates_and_returns_a_closure() {
        FlatProgramBuilder::new()
            .mut_function("nothing", [], Ty::mut_function([], Ty::tuple([])), |t| {
                t.event_handler(|t| t.tuple([]))
            })
            .entrypoint("Test", [], |t| t.text("x"))
            .build()
            .unwrap();
    }

    #[test]
    fn should_accept_a_closure_that_mutates_and_returns_a_closure() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                t.mut_lambda([], |t, _| t.event_handler(|t| t.tuple([])));
                t.text("x")
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_handler_that_mutates_and_returns_a_value_other_than_the_empty_tuple() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let handler = t.event_handler(|t| t.int(1));
                t.element_on_click(t.mint(), "button", vec![], handler, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::ClickNotHandler);
    }

    #[test]
    fn should_accept_a_closure_that_mutates_bound_in_a_pure_function() {
        FlatProgramBuilder::new()
            .function("unused", [], Ty::Int, |t| {
                t.mut_lambda([], |t, _| t.tuple([]));
                t.int(0)
            })
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("unused", [])))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn should_reject_a_cell_in_a_body_that_mutates() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let handler = t.event_handler(|t| {
                    t.cell(|t| t.int(0));
                    t.tuple([])
                });
                t.element_on_click(t.mint(), "button", vec![], handler, Some(t.text("x")))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_read_of_a_cell_in_a_cell_initializer() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let count = t.cell(|t| t.int(0));
                let step = t.cell(|t| t.cell_read(count));
                t.escape(t.int_to_string(t.cell_read(step)))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_reject_a_read_of_a_cell_in_the_key_of_a_loop() {
        let error = FlatProgramBuilder::new()
            .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                let count = t.cell(|t| t.int(0));
                t.html_for_keyed(t.param("items"), |t, _| t.cell_read(count))
                    .body(|t, _| t.text("item"))
            })
            .build()
            .unwrap_err();
        assert_eq!(error, FlatTypeError::EffectNotAllowed);
    }

    #[test]
    fn should_accept_an_index_of_an_element_of_a_tuple() {
        FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let pair = t.tuple([t.int(1), t.str("one")]);
                t.escape(t.tuple_index(pair, 1))
            })
            .build()
            .unwrap();
    }
}
