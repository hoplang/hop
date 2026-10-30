use std::fmt;

use pretty::BoxDoc;

use crate::document::CheapString;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrHtmlElement;
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_type::{IrDataType, IrHtmlType, IrType};
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
    pub parameters: Vec<IrParameter<IrDataType>>,
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
    pub binder: IrBinder<IrDataType>,
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
}

impl FlatDataOp {
    /// Apply `f` to each name this op reads directly. The names read inside
    /// the blocks of a Match are not visited, only the subject.
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            FlatDataOp::Data(op) => op.for_each_operand(f),
            FlatDataOp::Call(_, args) => {
                for arg in args {
                    f(*arg);
                }
            }
            FlatDataOp::Match(match_) => f(match_.subject),
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
        }
    }
}

impl FlatHtmlOp {
    /// Apply `f` to each name this op reads directly. The names read inside
    /// the blocks of a Match or a loop are not visited, only the subject or
    /// the array.
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
        }
    }
}

impl FlatDataBlock {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.bindings
                .iter()
                .map(FlatDataBinding::to_doc)
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl FlatHtmlBlock {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.bindings
                .iter()
                .map(FlatDataBinding::to_doc)
                .chain(self.html.iter().map(FlatHtmlBinding::to_doc))
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl FlatDataBinding {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("let {} = ", self.binder)).append(self.op.to_doc())
    }
}

impl FlatHtmlBinding {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("let {} = ", self.binder)).append(self.op.to_doc())
    }
}

/// A call as the function and its arguments in parentheses.
fn call_to_doc<'a>(function: &IrFunction, args: &[ValueId]) -> BoxDoc<'a> {
    let args = args
        .iter()
        .map(|arg| arg.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    BoxDoc::text(format!("{function}({args})"))
}

impl FlatDataOp {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            FlatDataOp::Data(op) => op.to_doc(),
            FlatDataOp::Call(function, args) => call_to_doc(function, args),
            FlatDataOp::Match(match_) => match_.to_doc(|body| BoxDoc::line().append(body.to_doc())),
        }
    }
}

impl FlatHtmlOp {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            FlatHtmlOp::Text(content) => BoxDoc::text(format!("text({:?})", content.as_str())),
            FlatHtmlOp::Concat(parts) => {
                let parts = parts
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("concat({parts})"))
            }
            FlatHtmlOp::Escape(string) => BoxDoc::text(format!("escape({string})")),
            FlatHtmlOp::Element(element) => {
                BoxDoc::text(format!("element({:?}, ", element.element.as_str()))
                    .append(element.attributes_to_doc())
                    .append(BoxDoc::text(format!(", {})", element.children)))
            }
            FlatHtmlOp::Call(function, args) => call_to_doc(function, args),
            FlatHtmlOp::Match(match_) => match_.to_doc(|body| BoxDoc::line().append(body.to_doc())),
            FlatHtmlOp::For(for_) => for_.to_doc(|body| BoxDoc::line().append(body.to_doc())),
        }
    }
}

impl FlatFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let (entrypoint, return_type, body) = match self {
            FlatFunctionDeclaration::Data(function) => (
                function.entrypoint,
                function.return_type.to_string(),
                function.body.to_doc(),
            ),
            FlatFunctionDeclaration::Html(function) => (
                function.entrypoint,
                "Html".to_string(),
                function.body.to_doc(),
            ),
        };
        let parameters = self
            .parameters()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        let keyword = if entrypoint { "entrypoint fn" } else { "fn" };
        BoxDoc::text(format!(
            "{keyword} {}({parameters}) -> {return_type} {{",
            self.function()
        ))
        .append(BoxDoc::line().append(body).nest(2))
        .append(BoxDoc::line())
        .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for FlatFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
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
