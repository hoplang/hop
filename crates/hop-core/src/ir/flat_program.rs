use std::fmt::{self, Write};

use crate::display::{Indent, join};
use crate::document::CheapString;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::index_vec::IndexVec;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::ir_type::{
    IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrResultType, IrType,
};
use crate::ir::ir_type_declarations::IrTypeDeclarations;
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::symbols::function_name::FunctionName;

#[derive(Debug, Clone)]
pub struct FlatProgram {
    pub types: IrTypeDeclarations,
    pub functions: Vec<FlatFunction>,
    pub entrypoints: IndexVec<EntrypointId, FlatEntrypoint>,
    pub value_ids: ValueIdCounter,
}

#[derive(Debug, Clone)]
pub struct FlatEntrypoint {
    pub name: FunctionName,
    pub parameters: Vec<IrParameter<IrDataType>>,
    pub body: FlatHtmlBlock,
}

#[derive(Debug, Clone)]
pub enum FlatFunction {
    Pure(IrFunctionDeclaration<IrFunctionDataType, IrResultType, FlatPureBlock>),
    Html(IrFunctionDeclaration<IrType, IrHtmlType, FlatHtmlBlock>),
}

pub type FlatPureBlock = IrBlock<IrBinding<IrFunctionDataType, FlatPureOp>>;
pub type FlatHtmlBlock = IrBlock<FlatHtmlBinding>;

/// A binding of a block that renders Html. The cells and elements a render
/// binds are retained across renders by place: the path of binding names
/// and loop iterations that reaches them.
#[derive(Debug, Clone)]
pub enum FlatHtmlBinding {
    Pure(IrBinding<IrFunctionDataType, FlatPureOp>),
    Html(IrBinding<IrFunctionHtmlType, FlatHtmlOp>),
    /// Bind a cell with an initial value, which the cell takes only when its
    /// place is new. `T -> Cell[T]`.
    Cell(IrBinder<IrFunctionDataType>, ValueId),
    /// Bind the value a cell has at this render. `Cell[T] -> T`.
    Track(IrBinder<IrDataType>, ValueId),
}

pub type FlatPureLambda = IrLambda<IrFunctionDataType, FlatPureBlock>;
pub type FlatHtmlLambda = IrLambda<IrType, FlatHtmlBlock>;

#[derive(Debug, Clone)]
pub enum FlatPureOp {
    Data(IrDataOp),
    /// Invoke a function.
    Call(IrFunction, Vec<ValueId>),
    /// Match exhaustively.
    Match(IrMatch<FlatPureBlock>),
    /// Write a cell. `(Cell[T], T) -> Effect[()]`.
    CellWrite(ValueId, ValueId),
    /// Read a cell. `Cell[T] -> Effect[T]`.
    CellRead(ValueId),
    /// Lift a value to an effect. `T -> Effect[T]`.
    EffectPure(ValueId),
    /// Run the first effect, then return the effect the second closure
    /// gives for its result. `(Effect[A], (A) -> Effect[B]) -> Effect[B]`.
    EffectBind(ValueId, ValueId),
    /// Bind a closure.
    Lambda(FlatPureLambda),
    /// Invoke a closure.
    Apply(ValueId, Vec<ValueId>),
}

#[derive(Debug, Clone)]
pub enum FlatHtmlOp {
    /// Invoke a function.
    Call(IrFunction, Vec<ValueId>),
    /// Match exhaustively.
    Match(IrMatch<FlatHtmlBlock>),
    /// Render text as written. `Html`.
    Text(CheapString),
    /// Html-escape a string. `String -> Html`.
    Escape(ValueId),
    /// Concatenate Html. `(Html, ..., Html) -> Html`.
    Concat(Vec<ValueId>),
    /// Bind a Html element, whose node its place keeps across renders.
    /// `Html -> Html`.
    Element(IrHtmlElement<ValueId>),
    /// Run a fold-map over an array, where each iteration produces a Html value.
    /// The key, when there is one, gives each element the identity its
    /// cells and elements keep across renders. `Array[T] -> Html`.
    For(FlatFor),
    /// Bind a closure.
    Lambda(FlatHtmlLambda),
    /// Invoke a closure.
    Apply(ValueId, Vec<ValueId>),
}

#[derive(Debug, Clone)]
pub struct FlatFor {
    pub var: Option<IrBinder<IrDataType>>,
    pub array: ValueId,
    pub key: Option<FlatPureBlock>,
    pub body: FlatHtmlBlock,
}

impl FlatFunction {
    pub fn function(&self) -> &IrFunction {
        match self {
            FlatFunction::Pure(function) => &function.function,
            FlatFunction::Html(function) => &function.function,
        }
    }

    pub fn parameters(&self) -> Vec<IrParameter<IrType>> {
        match self {
            FlatFunction::Pure(function) => function
                .parameters
                .iter()
                .map(|param| param.widen())
                .collect(),
            FlatFunction::Html(function) => function.parameters.clone(),
        }
    }
}

impl FlatPureOp {
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            FlatPureOp::Data(op) => op.for_each_operand(f),
            FlatPureOp::Call(_, args) => {
                for arg in args {
                    f(*arg);
                }
            }
            FlatPureOp::Match(match_) => f(match_.subject),
            FlatPureOp::CellWrite(cell, value) => {
                f(*cell);
                f(*value);
            }
            FlatPureOp::CellRead(cell) => f(*cell),
            FlatPureOp::EffectPure(value) => f(*value),
            FlatPureOp::EffectBind(effect, continuation) => {
                f(*effect);
                f(*continuation);
            }
            FlatPureOp::Lambda(_) => {}
            FlatPureOp::Apply(closure, args) => {
                f(*closure);
                for arg in args {
                    f(*arg);
                }
            }
        }
    }

    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        match self {
            FlatPureOp::Data(op) => op.for_each_operand_mut(f),
            FlatPureOp::Call(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            FlatPureOp::Match(match_) => f(&mut match_.subject),
            FlatPureOp::CellWrite(cell, value) => {
                f(cell);
                f(value);
            }
            FlatPureOp::CellRead(cell) => f(cell),
            FlatPureOp::EffectPure(value) => f(value),
            FlatPureOp::EffectBind(effect, continuation) => {
                f(effect);
                f(continuation);
            }
            FlatPureOp::Lambda(_) => {}
            FlatPureOp::Apply(closure, args) => {
                f(closure);
                for arg in args {
                    f(arg);
                }
            }
        }
    }
}

impl FlatHtmlOp {
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            FlatHtmlOp::Text(_) => {}
            FlatHtmlOp::Escape(name) => f(*name),
            FlatHtmlOp::Concat(names) => {
                for name in names {
                    f(*name);
                }
            }
            FlatHtmlOp::Element(element) => {
                element.for_each_attribute_operand(f);
                f(element.children);
                if let Some(handler) = element.on_click {
                    f(handler);
                }
            }
            FlatHtmlOp::Call(_, args) => {
                for arg in args {
                    f(*arg);
                }
            }
            FlatHtmlOp::Match(match_) => f(match_.subject),
            FlatHtmlOp::For(for_) => f(for_.array),
            FlatHtmlOp::Lambda(_) => {}
            FlatHtmlOp::Apply(closure, args) => {
                f(*closure);
                for arg in args {
                    f(*arg);
                }
            }
        }
    }

    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        match self {
            FlatHtmlOp::Text(_) => {}
            FlatHtmlOp::Escape(name) => f(name),
            FlatHtmlOp::Concat(names) => {
                for name in names {
                    f(name);
                }
            }
            FlatHtmlOp::Element(element) => {
                element.for_each_attribute_operand_mut(f);
                f(&mut element.children);
                if let Some(handler) = &mut element.on_click {
                    f(handler);
                }
            }
            FlatHtmlOp::Call(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            FlatHtmlOp::Match(match_) => f(&mut match_.subject),
            FlatHtmlOp::For(for_) => f(&mut for_.array),
            FlatHtmlOp::Lambda(_) => {}
            FlatHtmlOp::Apply(closure, args) => {
                f(closure);
                for arg in args {
                    f(arg);
                }
            }
        }
    }
}

impl fmt::Display for FlatHtmlBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatHtmlBinding::Pure(binding) => binding.fmt(f),
            FlatHtmlBinding::Cell(binder, initial) => write!(f, "let {binder} = cell({initial})"),
            FlatHtmlBinding::Track(binder, cell) => write!(f, "let {binder} = track({cell})"),
            FlatHtmlBinding::Html(binding) => binding.fmt(f),
        }
    }
}

fn write_block(block: &impl fmt::Display, w: &mut dyn Write) -> fmt::Result {
    write!(w, "\n{block}")
}

impl fmt::Display for FlatPureOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatPureOp::Data(op) => write!(f, "{op}"),
            FlatPureOp::Call(function, args) => write!(f, "{function}({})", join(args, ", ")),
            FlatPureOp::Match(match_) => match_.write(f, write_block),
            FlatPureOp::CellWrite(cell, value) => write!(f, "{cell} := {value}"),
            FlatPureOp::CellRead(cell) => write!(f, "read({cell})"),
            FlatPureOp::EffectPure(value) => write!(f, "pure({value})"),
            FlatPureOp::EffectBind(effect, continuation) => {
                write!(f, "bind({effect}, {continuation})")
            }
            FlatPureOp::Lambda(lambda) => lambda.write(f, write_block),
            FlatPureOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
        }
    }
}

impl fmt::Display for FlatHtmlOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatHtmlOp::Text(content) => write!(f, "text({:?})", content.as_str()),
            FlatHtmlOp::Concat(parts) => write!(f, "concat({})", join(parts, ", ")),
            FlatHtmlOp::Escape(string) => write!(f, "escape({string})"),
            FlatHtmlOp::Element(element) => {
                write!(f, "element({:?}, {{", element.element.as_str())?;
                for (i, (name, attribute)) in element.attributes.iter().enumerate() {
                    let sep = if i > 0 { ", " } else { "" };
                    write!(f, "{sep}{}", name.as_str())?;
                    match attribute {
                        IrAttribute::Value(value) | IrAttribute::Presence(value) => {
                            write!(f, ": {value}")?;
                        }
                        IrAttribute::Constant(Some(value)) => write!(f, ": {:?}", value.as_str())?,
                        IrAttribute::Constant(None) => {}
                    }
                }
                write!(f, "}}, {})", element.children)?;
                if let Some(handler) = element.on_click {
                    write!(f, " on_click {handler}")?;
                }
                Ok(())
            }
            FlatHtmlOp::Call(function, args) => write!(f, "{function}({})", join(args, ", ")),
            FlatHtmlOp::Match(match_) => match_.write(f, write_block),
            FlatHtmlOp::For(FlatFor {
                var,
                array,
                key,
                body,
            }) => {
                match var {
                    Some(binder) => write!(f, "for {binder} in {array}")?,
                    None => write!(f, "for _ in {array}")?,
                }
                if let Some(key) = key {
                    f.write_str(" key {")?;
                    write!(Indent::new(f), "\n{key}")?;
                    f.write_str("\n}")?;
                }
                f.write_str(" {")?;
                write!(Indent::new(f), "\n{body}")?;
                f.write_str("\n}")
            }
            FlatHtmlOp::Lambda(lambda) => lambda.write(f, write_block),
            FlatHtmlOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
        }
    }
}

impl fmt::Display for FlatFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatFunction::Pure(function) => function.write(f, write_block)?,
            FlatFunction::Html(function) => function.write(f, write_block)?,
        }
        writeln!(f)
    }
}

impl fmt::Display for FlatProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            write!(f, "{function}")?;
        }
        for (id, entrypoint) in self.entrypoints.iter_enumerated() {
            write!(
                f,
                "entrypoint {}@e{}({}) -> Html {{",
                entrypoint.name,
                id,
                join(&entrypoint.parameters, ", ")
            )?;
            write!(Indent::new(f), "\n{}", entrypoint.body)?;
            writeln!(f, "\n}}")?;
        }
        Ok(())
    }
}
