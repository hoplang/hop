use std::fmt;

use pretty::BoxDoc;

use crate::document::CheapString;
use crate::hop::typing::Type;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrHtmlElement;
use crate::ir::ir_match::IrMatch;
use crate::ir::value_id::{ValueId, ValueIdCounter};

use super::ir_parameter::IrParameter;

/// A Flat module.
#[derive(Debug, Clone)]
pub struct FlatModule {
    pub functions: Vec<FlatFunctionDeclaration>,
    pub value_ids: ValueIdCounter,
}

/// A function declaration in the Flat IR.
#[derive(Debug, Clone)]
pub struct FlatFunctionDeclaration {
    pub function: IrFunction,
    /// Whether the function is called from outside the module, so it is kept
    /// even when nothing in the module calls it.
    pub entrypoint: bool,
    pub parameters: Vec<IrParameter>,
    pub return_type: Type,
    pub body: FlatBlock,
}

/// A block. A binding is in scope for every later binding of the block, for the
/// blocks nested in them, and for the result.
#[derive(Debug, Clone)]
pub struct FlatBlock {
    pub bindings: Vec<FlatBinding>,
    pub result: ValueId,
}

#[derive(Debug, Clone)]
pub struct FlatBinding {
    pub binder: IrBinder,
    pub op: FlatOp,
}

#[derive(Debug, Clone)]
pub enum FlatOp {
    Data(IrDataOp),
    /// Invokes a function and produces its result. The arguments follow
    /// the function's parameters, one for each.
    Call(IrFunction, Vec<ValueId>),
    /// A match with the arms of the source. Each arm is a block that
    /// produces the match's value, and a name its pattern binds is the part
    /// of the subject it binds.
    ///
    /// Matching is exhaustive, a value must match at least one arm.
    Match(IrMatch<FlatBlock>),
    /// Text that renders as written, without escaping.
    HtmlText(CheapString),
    /// Must name a String. Produces its HTML escaped form as Html.
    HtmlEscape(ValueId),
    /// N-ary mappend over Html names. Part order is output order.
    HtmlConcat(Vec<ValueId>),
    /// An element whose children are the Html a name holds.
    HtmlElement(IrHtmlElement<ValueId>),
    /// A foldMap over the array, concatenating the body's Html once per
    /// element in order.
    HtmlFor(IrFor<FlatBlock>),
}

impl FlatOp {
    /// Apply `f` to each name this op reads directly. The names read inside
    /// the blocks of a Match or a loop are not visited, only the subject or
    /// the array.
    pub fn for_each_operand(&self, f: &mut impl FnMut(ValueId)) {
        match self {
            FlatOp::Data(op) => op.for_each_operand(f),
            FlatOp::HtmlText(_) => {}
            FlatOp::HtmlEscape(name) => f(*name),
            FlatOp::HtmlConcat(names) => {
                for name in names {
                    f(*name);
                }
            }
            FlatOp::HtmlElement(element) => {
                element.for_each_attribute_operand(f);
                f(element.children);
            }
            FlatOp::Call(_, args) => {
                for arg in args {
                    f(*arg);
                }
            }
            FlatOp::Match(match_) => f(match_.subject),
            FlatOp::HtmlFor(for_) => f(for_.array),
        }
    }

    /// Apply `f` to each name this op reads directly, to rewrite it. Visits
    /// the same names as `for_each_operand`.
    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut ValueId)) {
        match self {
            FlatOp::Data(op) => op.for_each_operand_mut(f),
            FlatOp::HtmlText(_) => {}
            FlatOp::HtmlEscape(name) => f(name),
            FlatOp::HtmlConcat(names) => {
                for name in names {
                    f(name);
                }
            }
            FlatOp::HtmlElement(element) => {
                element.for_each_attribute_operand_mut(f);
                f(&mut element.children);
            }
            FlatOp::Call(_, args) => {
                for arg in args {
                    f(arg);
                }
            }
            FlatOp::Match(match_) => f(&mut match_.subject),
            FlatOp::HtmlFor(for_) => f(&mut for_.array),
        }
    }
}

impl FlatBlock {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.bindings
                .iter()
                .map(|binding| {
                    BoxDoc::text(format!("let {} = ", binding.binder)).append(binding.op.to_doc())
                })
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl FlatOp {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            FlatOp::Data(op) => op.to_doc(),
            FlatOp::HtmlText(content) => BoxDoc::text(format!("text({:?})", content.as_str())),
            FlatOp::HtmlConcat(parts) => {
                let parts = parts
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("concat({parts})"))
            }
            FlatOp::HtmlEscape(string) => BoxDoc::text(format!("escape({string})")),
            FlatOp::HtmlElement(element) => {
                BoxDoc::text(format!("element({:?}, ", element.element.as_str()))
                    .append(element.attributes_to_doc())
                    .append(BoxDoc::text(format!(", {})", element.children)))
            }
            FlatOp::Call(function, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("{function}({args})"))
            }
            FlatOp::Match(match_) => match_.to_doc(|body| BoxDoc::line().append(body.to_doc())),
            FlatOp::HtmlFor(for_) => for_.to_doc(|body| BoxDoc::line().append(body.to_doc())),
        }
    }
}

impl FlatFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let parameters = self
            .parameters
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        let keyword = if self.entrypoint {
            "entrypoint fn"
        } else {
            "fn"
        };
        BoxDoc::text(format!(
            "{keyword} {}({parameters}) -> {} {{",
            self.function, self.return_type
        ))
        .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
        .append(BoxDoc::line())
        .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for FlatFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for FlatModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            write!(f, "{function}")?;
        }
        Ok(())
    }
}
