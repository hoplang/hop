use std::fmt::{self, Write};

use crate::display::{Indent, join};
use crate::document::CheapString;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::index_vec::IndexVec;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::ir_type::{IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType, IrType};
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

pub type FlatDataBlock = IrBlock<IrBinding<IrFunctionDataType, FlatDataOp>>;
pub type FlatHtmlBlock = IrBlock<FlatBinding>;

#[derive(Debug, Clone)]
pub enum FlatFunction {
    Data(IrFunctionDeclaration<IrFunctionDataType, IrDataType, FlatDataBlock>),
    Html(IrFunctionDeclaration<IrType, IrHtmlType, FlatHtmlBlock>),
}

#[derive(Debug, Clone)]
pub enum FlatBinding {
    Data(IrBinding<IrFunctionDataType, FlatDataOp>),
    Html(IrBinding<IrFunctionHtmlType, FlatHtmlOp>),
}

#[derive(Debug, Clone)]
pub enum FlatDataOp {
    Data(IrDataOp),
    /// Invokes a function.
    /// The arguments follow the function's parameters, one for each.
    Call(IrFunction, Vec<ValueId>),
    /// A match with the arms of the source. Each arm is a block that
    /// produces the match's value, and a name its pattern binds is the part
    /// of the subject it binds.
    ///
    /// Matching is exhaustive, a value must match at least one arm.
    Match(IrMatch<FlatDataBlock>),
    Lambda(IrLambda<IrFunctionDataType, FlatDataBlock>),
    /// Invokes a closure.
    /// The arguments follow the closure's parameters, one for each.
    Apply(ValueId, Vec<ValueId>),
}

#[derive(Debug, Clone)]
pub enum FlatHtmlOp {
    /// Invokes a function that returns Html and produces its result. The
    /// arguments follow the function's parameters, one for each.
    Call(IrFunction, Vec<ValueId>),
    /// A match with the arms of the source, as a match of a value has, whose
    /// arms render Html.
    Match(IrMatch<FlatHtmlBlock>),
    /// Text that renders as written, without escaping.
    Text(CheapString),
    /// Must name a String. Produces its HTML escaped form as Html.
    Escape(ValueId),
    /// N-ary mappend over Html names. Part order is output order.
    Concat(Vec<ValueId>),
    /// An element whose children are the Html a name holds.
    Element(IrHtmlElement<ValueId>),
    /// A foldMap over the array, concatenating the body's Html once per
    /// element in order.
    For(IrFor<FlatHtmlBlock>),
    Lambda(IrLambda<IrType, FlatHtmlBlock>),
    /// Invokes a closure.
    /// The arguments follow the closure's parameters, one for each.
    Apply(ValueId, Vec<ValueId>),
}

impl FlatFunction {
    pub fn function(&self) -> &IrFunction {
        match self {
            FlatFunction::Data(function) => &function.function,
            FlatFunction::Html(function) => &function.function,
        }
    }

    pub fn parameters(&self) -> Vec<IrParameter<IrType>> {
        match self {
            FlatFunction::Data(function) => function
                .parameters
                .iter()
                .map(|param| param.widen())
                .collect(),
            FlatFunction::Html(function) => function.parameters.clone(),
        }
    }
}

impl FlatDataOp {
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            FlatDataOp::Data(op) => op.for_each_operand(f),
            FlatDataOp::Call(_, args) => {
                for arg in args {
                    f(*arg);
                }
            }
            FlatDataOp::Match(match_) => f(match_.subject),
            FlatDataOp::Lambda(_) => {}
            FlatDataOp::Apply(closure, args) => {
                f(*closure);
                for arg in args {
                    f(*arg);
                }
            }
        }
    }

    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        match self {
            FlatDataOp::Data(op) => op.for_each_operand_mut(f),
            FlatDataOp::Call(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            FlatDataOp::Match(match_) => f(&mut match_.subject),
            FlatDataOp::Lambda(_) => {}
            FlatDataOp::Apply(closure, args) => {
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

impl fmt::Display for FlatBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatBinding::Data(binding) => binding.fmt(f),
            FlatBinding::Html(binding) => binding.fmt(f),
        }
    }
}

fn write_block(block: &impl fmt::Display, w: &mut dyn Write) -> fmt::Result {
    write!(w, "\n{block}")
}

impl fmt::Display for FlatDataOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatDataOp::Data(op) => write!(f, "{op}"),
            FlatDataOp::Call(function, args) => write!(f, "{function}({})", join(args, ", ")),
            FlatDataOp::Match(match_) => match_.write(f, write_block),
            FlatDataOp::Lambda(lambda) => lambda.write(f, write_block),
            FlatDataOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
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
                write!(f, "}}, {})", element.children)
            }
            FlatHtmlOp::Call(function, args) => write!(f, "{function}({})", join(args, ", ")),
            FlatHtmlOp::Match(match_) => match_.write(f, write_block),
            FlatHtmlOp::For(for_) => for_.write(f, write_block),
            FlatHtmlOp::Lambda(lambda) => lambda.write(f, write_block),
            FlatHtmlOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
        }
    }
}

impl fmt::Display for FlatFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatFunction::Data(function) => function.write(f, write_block)?,
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
