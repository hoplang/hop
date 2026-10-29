use std::fmt;

use pretty::BoxDoc;

use crate::hop::typing::Type;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_switch::IrSwitch;
use crate::ir::value_id::ValueId;
use crate::symbols::type_name::TypeName;

use super::ir_parameter::IrParameter;

/// A Writer program.
///
/// The form the backends consume. A body is a sequence of statements that write
/// to the ambient buffer, with the values they need bound by lets along the
/// way. Html is written rather than held, except where a call passes it or it
/// is read more than once, and then an HtmlLiteral renders it into a buffer
/// of its own.
///
/// Every name is bound once in the program, by a parameter, a let, a loop or
/// an arm of a match, so a name identifies its binding.
#[derive(Debug)]
pub struct WriterProgram {
    pub pages: Vec<WriterPageDeclaration>,
    pub functions: Vec<WriterFunctionDeclaration>,
}

#[derive(Debug)]
pub struct WriterPageDeclaration {
    pub name: TypeName,
    pub parameters: Vec<IrParameter<Type>>,
    pub body: Vec<WriterStmt>,
}

#[derive(Debug)]
pub struct WriterFunctionDeclaration {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter<Type>>,
    pub return_type: Type,
    pub body: WriterFunctionBody,
}

#[derive(Debug)]
pub enum WriterFunctionBody {
    /// Destination passing: writes to the ambient buffer. A call site is a
    /// WriteFunction statement.
    Writes(Vec<WriterStmt>),
    /// Value returning. A call site is a Call value.
    Returns(WriterBlock),
}

/// A value binding.
#[derive(Debug)]
pub struct WriterBinding {
    pub name: ValueId,
    pub typ: Type,
    pub op: WriterOp,
}

/// Bindings followed by the name of the result: a value function body or a
/// match arm in value position.
#[derive(Debug)]
pub struct WriterBlock {
    pub bindings: Vec<WriterBinding>,
    pub result: ValueId,
}

/// A statement. Statements write to the ambient buffer, in order, or bind
/// a value for the statements after them.
#[derive(Debug)]
pub enum WriterStmt {
    Let(WriterBinding),
    /// Write a constant string, unescaped.
    Write(String),
    /// Write a String name, escaped.
    WriteString(ValueId),
    /// Write an Html name as it is.
    WriteHtml(ValueId),
    /// Invoke an Html function, which writes to the same buffer. The
    /// arguments follow the function's parameters, one for each.
    WriteFunction(IrFunction, Vec<ValueId>),
    /// Run the body once per element of the array, in order.
    HtmlFor(IrFor<Vec<WriterStmt>>),
    /// Run the arm that matches. Matching is exhaustive.
    Match(IrSwitch<Vec<WriterStmt>>),
}

#[derive(Debug)]
pub enum WriterOp {
    Data(IrDataOp),
    Call(IrFunction, Vec<ValueId>),
    /// Html as a value: the statements render into a fresh buffer.
    HtmlLiteral(Vec<WriterStmt>),
    /// A match over a value that is not Html. Each arm produces the value.
    Match(IrSwitch<WriterBlock>),
}

/// Each statement on a line of its own. The statements start with a line
/// break and the caller nests them, so an empty list prints nothing.
fn stmts_to_doc(stmts: &[WriterStmt]) -> BoxDoc<'_> {
    BoxDoc::concat(
        stmts
            .iter()
            .map(|stmt| BoxDoc::line().append(stmt.to_doc())),
    )
}

impl WriterBlock {
    /// One line per binding and one for the result.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.bindings
                .iter()
                .map(WriterBinding::to_doc)
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl WriterBinding {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("let {}: {} = ", self.name, self.typ)).append(self.op.to_doc())
    }
}

impl WriterStmt {
    /// An HtmlFor, a Match or a let holding a literal spans lines, every
    /// other statement is one line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            WriterStmt::Let(let_) => let_.to_doc(),
            WriterStmt::Write(content) => BoxDoc::text(format!("write({content:?})")),
            WriterStmt::WriteString(name) => BoxDoc::text(format!("write_string({name})")),
            WriterStmt::WriteHtml(name) => BoxDoc::text(format!("write_html({name})")),
            WriterStmt::WriteFunction(function, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("write_function {function}({args})"))
            }
            WriterStmt::HtmlFor(for_) => for_.to_doc(|body| stmts_to_doc(body)),
            WriterStmt::Match(match_) => match_.to_doc(|body| stmts_to_doc(body)),
        }
    }
}

impl WriterOp {
    /// An HtmlLiteral or a Match spans lines, every other op is one line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            WriterOp::Data(op) => op.to_doc(),
            WriterOp::Call(function, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("{function}({args})"))
            }
            WriterOp::HtmlLiteral(body) => BoxDoc::text("html {")
                .append(stmts_to_doc(body).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            WriterOp::Match(match_) => match_.to_doc(|body| BoxDoc::line().append(body.to_doc())),
        }
    }
}

impl WriterFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let parameters = self
            .parameters
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        let body = match &self.body {
            WriterFunctionBody::Writes(statements) => stmts_to_doc(statements),
            WriterFunctionBody::Returns(block) => BoxDoc::line().append(block.to_doc()),
        };
        BoxDoc::text(format!(
            "fn {}({parameters}) -> {} {{",
            self.function, self.return_type
        ))
        .append(body.nest(2))
        .append(BoxDoc::line())
        .append(BoxDoc::text("}"))
    }
}

impl WriterPageDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let parameters = self
            .parameters
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        BoxDoc::text(format!("page {}({parameters}) {{", self.name.as_str()))
            .append(stmts_to_doc(&self.body).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for WriterFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for WriterPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for WriterProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            write!(f, "{function}")?;
        }
        for page in &self.pages {
            write!(f, "{page}")?;
        }
        Ok(())
    }
}
