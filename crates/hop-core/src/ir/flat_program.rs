use std::fmt::{self, Write};

use crate::display::{Indent, join};
use crate::document::CheapString;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_type::{IrDataType, IrHtmlType, IrType, IrValueType};
use crate::ir::ir_type_declarations::IrTypeDeclarations;
use crate::ir::value_id::{ValueId, ValueIdCounter};

use super::ir_parameter::IrParameter;

/// A Flat program.
#[derive(Debug, Clone)]
pub struct FlatProgram {
    pub types: IrTypeDeclarations,
    pub functions: Vec<FlatFunctionDeclaration>,
    pub value_ids: ValueIdCounter,
}

/// A function that returns Html renders markup, and any other function
/// computes a value.
#[derive(Debug, Clone)]
pub enum FlatFunctionDeclaration {
    Data(FlatDataFunction),
    Html(FlatHtmlFunction),
}

/// A function whose body computes a value of its return type. No value
/// depends on Html, so its parameters take values only.
#[derive(Debug, Clone)]
pub struct FlatDataFunction {
    pub function: IrFunction,
    /// Whether the function is called from outside the program, so it is kept
    /// even when nothing in the program calls it.
    pub entrypoint: bool,
    pub parameters: Vec<IrParameter<IrValueType>>,
    pub return_type: IrDataType,
    pub body: FlatDataBlock,
}

/// A function whose body renders markup.
#[derive(Debug, Clone)]
pub struct FlatHtmlFunction {
    pub function: IrFunction,
    /// Whether the function is called from outside the program, so it is kept
    /// even when nothing in the program calls it.
    pub entrypoint: bool,
    pub parameters: Vec<IrParameter<IrType>>,
    pub body: FlatHtmlBlock,
}

impl FlatFunctionDeclaration {
    pub fn function(&self) -> &IrFunction {
        match self {
            FlatFunctionDeclaration::Data(function) => &function.function,
            FlatFunctionDeclaration::Html(function) => &function.function,
        }
    }

    pub fn entrypoint(&self) -> bool {
        match self {
            FlatFunctionDeclaration::Data(function) => function.entrypoint,
            FlatFunctionDeclaration::Html(function) => function.entrypoint,
        }
    }

    /// The parameters, those of a function that returns a value widened to
    /// IrType.
    pub fn parameters(&self) -> Vec<IrParameter<IrType>> {
        match self {
            FlatFunctionDeclaration::Data(function) => {
                function.parameters.iter().map(IrParameter::widen).collect()
            }
            FlatFunctionDeclaration::Html(function) => function.parameters.clone(),
        }
    }
}

/// A block that computes a value. A binding is in scope for every later
/// binding of the block, for the blocks nested in them, and for the result.
#[derive(Debug, Clone)]
pub struct FlatDataBlock {
    pub bindings: Vec<FlatDataBinding>,
    pub result: ValueId,
}

/// A block that renders Html. No value reads Html, so the values come
/// before the Html. A binding is in scope for every later binding of the
/// block, for the blocks nested in them, and for the result.
#[derive(Debug, Clone)]
pub struct FlatHtmlBlock {
    pub bindings: Vec<FlatDataBinding>,
    pub html: Vec<FlatHtmlBinding>,
    pub result: ValueId,
}

#[derive(Debug, Clone)]
pub struct FlatDataBinding {
    pub binder: IrBinder<IrValueType>,
    pub op: FlatDataOp,
}

#[derive(Debug, Clone)]
pub struct FlatHtmlBinding {
    pub binder: IrBinder<IrHtmlType>,
    pub op: FlatHtmlOp,
}

#[derive(Debug, Clone)]
pub enum FlatDataOp {
    Data(IrDataOp),
    /// Invokes a function that returns a value and produces its result.
    /// The arguments follow the function's parameters, one for each.
    Call(IrFunction, Vec<ValueId>),
    /// A match with the arms of the source. Each arm is a block that
    /// produces the match's value, and a name its pattern binds is the part
    /// of the subject it binds.
    ///
    /// Matching is exhaustive, a value must match at least one arm.
    Match(IrMatch<FlatDataBlock>),
    Lambda(IrLambda<IrValueType, FlatDataBlock>),
    // The arguments follow the closure's parameters, one for each.
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
    // The arguments follow the closure's parameters, one for each.
    Apply(ValueId, Vec<ValueId>),
}

impl FlatDataOp {
    /// Apply `f` to each name this op reads directly. The names read inside
    /// the blocks of a Match or a Lambda are not visited, only the subject.
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

    /// Apply `f` to each name this op reads directly, to rewrite it. Visits
    /// the same names as `for_each_operand`.
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
    /// Apply `f` to each name this op reads directly. The names read inside
    /// the blocks of a Match, a loop or a Lambda are not visited, only the
    /// subject or the array.
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

    /// Apply `f` to each name this op reads directly, to rewrite it. Visits
    /// the same names as `for_each_operand`.
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

/// Each binding and then the result on a line of its own.
impl fmt::Display for FlatDataBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for binding in &self.bindings {
            writeln!(f, "{binding}")?;
        }
        write!(f, "{}", self.result)
    }
}

/// Each binding and then the result on a line of its own.
impl fmt::Display for FlatHtmlBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for binding in &self.bindings {
            writeln!(f, "{binding}")?;
        }
        for binding in &self.html {
            writeln!(f, "{binding}")?;
        }
        write!(f, "{}", self.result)
    }
}

impl fmt::Display for FlatDataBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.binder, self.op)
    }
}

impl fmt::Display for FlatHtmlBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.binder, self.op)
    }
}

/// Writes a block on the lines after the current one.
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

impl fmt::Display for FlatFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keyword = |entrypoint| if entrypoint { "entrypoint fn" } else { "fn" };
        match self {
            FlatFunctionDeclaration::Data(function) => {
                write!(
                    f,
                    "{} {}({}) -> {} {{",
                    keyword(function.entrypoint),
                    function.function,
                    join(&function.parameters, ", "),
                    function.return_type
                )?;
                write!(Indent::new(f), "\n{}", function.body)?;
            }
            FlatFunctionDeclaration::Html(function) => {
                write!(
                    f,
                    "{} {}({}) -> Html {{",
                    keyword(function.entrypoint),
                    function.function,
                    join(&function.parameters, ", ")
                )?;
                write!(Indent::new(f), "\n{}", function.body)?;
            }
        }
        writeln!(f, "\n}}")
    }
}

impl fmt::Display for FlatProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            write!(f, "{function}")?;
        }
        Ok(())
    }
}
