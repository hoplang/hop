use std::fmt;

use pretty::BoxDoc;

use crate::document::CheapString;
use crate::hop::typing::{DataType, Type};
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::IrHtmlElement;
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// A Markup module.
#[derive(Debug)]
pub struct MarkupModule {
    pub functions: Vec<MarkupFunctionDeclaration>,
    pub value_ids: ValueIdCounter,
}

#[derive(Debug)]
pub struct MarkupFunctionDeclaration {
    pub function: IrFunction,
    pub entrypoint: bool,
    pub parameters: Vec<IrParameter>,
    pub return_type: Type,
    pub body: MarkupBody,
}

/// The body of a function that returns Html is markup, and the body of any
/// other function is a value.
#[derive(Debug)]
pub enum MarkupBody {
    Value(MarkupValueBlock),
    Markup(MarkupBlock),
}

/// A scope that renders markup.
#[derive(Debug)]
pub struct MarkupBlock {
    pub bindings: Vec<MarkupValueBinding>,
    pub markup: Vec<MarkupNode>,
}

/// A scope that computes a value.
#[derive(Debug)]
pub struct MarkupValueBlock {
    pub bindings: Vec<MarkupValueBinding>,
    pub result: ValueId,
}

/// A binding of data, which is never Html.
#[derive(Debug)]
pub struct MarkupValueBinding {
    pub name: ValueId,
    pub typ: DataType,
    pub op: MarkupValueOp,
}

#[derive(Debug)]
pub enum MarkupValueOp {
    Data(IrDataOp),
    Call(IrFunction, Vec<MarkupCallArg>),
    Match(IrMatch<MarkupValueBlock>),
}

/// An argument of a call, one for each parameter of the function: a value,
/// or the markup for an Html parameter. The markup reads the names of the
/// caller, wherever the function renders it.
#[derive(Debug)]
pub enum MarkupCallArg {
    Value(ValueId),
    Markup(Vec<MarkupNode>),
}

#[derive(Debug)]
pub enum MarkupNode {
    /// Text that renders as written, without escaping.
    Text(CheapString),
    /// Must name a String, which renders escaped.
    Escape(ValueId),
    /// An element whose children are markup. A void element has no
    /// children.
    Element(IrHtmlElement<Vec<MarkupNode>>),
    /// The markup of the first arm whose pattern matches the subject.
    Match(IrMatch<MarkupBlock>),
    /// The body's markup once per element of the array, in order.
    For(IrFor<MarkupBlock>),
    /// The markup of a function that returns Html.
    Call(IrFunction, Vec<MarkupCallArg>),
    /// The markup the caller passed for the named Html parameter. A function
    /// can render it any number of times.
    Slot(ValueId),
}

impl MarkupBlock {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let bindings = self.bindings.iter().map(MarkupValueBinding::to_doc);
        let markup = self.markup.iter().map(MarkupNode::to_doc);
        if self.bindings.is_empty() || self.markup.is_empty() {
            BoxDoc::intersperse(bindings.chain(markup), BoxDoc::line())
        } else {
            BoxDoc::intersperse(bindings, BoxDoc::line())
                .append(BoxDoc::line())
                .append(BoxDoc::line())
                .append(BoxDoc::intersperse(markup, BoxDoc::line()))
        }
    }
}

impl MarkupValueBlock {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.bindings
                .iter()
                .map(MarkupValueBinding::to_doc)
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl MarkupValueBinding {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("let {}: {} = ", self.name, self.typ)).append(self.op.to_doc())
    }
}

impl MarkupValueOp {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            MarkupValueOp::Data(op) => op.to_doc(),
            MarkupValueOp::Call(function, args) => BoxDoc::text(format!("{function}("))
                .append(BoxDoc::intersperse(
                    args.iter().map(MarkupCallArg::to_doc),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(")")),
            MarkupValueOp::Match(match_) => {
                match_.to_doc(|body| BoxDoc::line().append(body.to_doc()))
            }
        }
    }
}

impl MarkupNode {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            MarkupNode::Text(text) => BoxDoc::text(format!("text({:?})", text.as_str())),
            MarkupNode::Escape(string) => BoxDoc::text(format!("escape({string})")),
            MarkupNode::Element(element) => {
                let children = &element.children;
                let mut sections = vec![
                    BoxDoc::text(format!("{:?}", element.element.as_str())),
                    element.attributes_to_doc(),
                ];
                if !element.element.is_void() {
                    sections.push(if children.is_empty() {
                        BoxDoc::text("[]")
                    } else {
                        BoxDoc::text("[")
                            .append(
                                BoxDoc::line_()
                                    .append(BoxDoc::intersperse(
                                        children.iter().map(MarkupNode::to_doc),
                                        BoxDoc::text(",").append(BoxDoc::line()),
                                    ))
                                    .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                    .append(BoxDoc::line_())
                                    .nest(2)
                                    .group(),
                            )
                            .append(BoxDoc::text("]"))
                    });
                }
                BoxDoc::text("element(")
                    .append(
                        BoxDoc::line_()
                            .append(BoxDoc::intersperse(
                                sections,
                                BoxDoc::text(",").append(BoxDoc::line()),
                            ))
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text(")"))
            }
            MarkupNode::Match(match_) => {
                match_.to_doc(|body| BoxDoc::hardline().append(body.to_doc()))
            }
            MarkupNode::For(for_) => for_.to_doc(|body| BoxDoc::hardline().append(body.to_doc())),
            MarkupNode::Call(function, args) => BoxDoc::text(format!("{function}("))
                .append(BoxDoc::intersperse(
                    args.iter().map(MarkupCallArg::to_doc),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(")")),
            MarkupNode::Slot(binder) => BoxDoc::text(format!("slot({binder})")),
        }
    }
}

impl MarkupCallArg {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            MarkupCallArg::Value(value) => BoxDoc::text(value.to_string()),
            MarkupCallArg::Markup(markup) => {
                if markup.is_empty() {
                    BoxDoc::text("[]")
                } else {
                    BoxDoc::text("[")
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    markup.iter().map(MarkupNode::to_doc),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("]"))
                }
            }
        }
    }
}

impl MarkupFunctionDeclaration {
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
        let body = match &self.body {
            MarkupBody::Value(block) => block.to_doc(),
            MarkupBody::Markup(block) => block.to_doc(),
        };
        BoxDoc::text(format!(
            "{keyword} {}({parameters}) -> {} {{",
            self.function, self.return_type
        ))
        .append(BoxDoc::line().append(body).nest(2))
        .append(BoxDoc::line())
        .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for MarkupModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            for line in function.to_doc().pretty(60).to_string().lines() {
                writeln!(f, "{}", line.trim_end())?;
            }
        }
        Ok(())
    }
}
