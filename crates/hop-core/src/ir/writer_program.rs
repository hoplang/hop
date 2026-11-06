use std::fmt::{self, Write};

use crate::display::{Indent, join};
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_switch::IrSwitch;
use crate::ir::ir_type::IrType;
use crate::ir::ir_type_declarations::IrTypeDeclarations;
use crate::ir::value_id::ValueId;
use crate::symbols::type_name::TypeName;

use super::ir_parameter::IrParameter;

/// A Writer program.
///
/// The form the backends consume. A body is a sequence of statements that write
/// to the ambient buffer, with the values they need bound by lets along the
/// way. Html is written, never held: an Html parameter takes a closure
/// without parameters that writes the Html, and the function applies it.
///
/// Every name is bound once in the program, by a parameter, a let, a loop or
/// an arm of a match, so a name identifies its binding.
#[derive(Debug)]
pub struct WriterProgram {
    pub types: IrTypeDeclarations,
    pub pages: Vec<WriterPageDeclaration>,
    pub functions: Vec<IrFunctionDeclaration<IrType, IrType, WriterFunctionBody>>,
}

#[derive(Debug)]
pub struct WriterPageDeclaration {
    pub name: TypeName,
    pub parameters: Vec<IrParameter<IrType>>,
    pub body: Vec<WriterStmt>,
}

#[derive(Debug)]
pub enum WriterFunctionBody {
    /// Destination passing: writes to the ambient buffer. A call site is a
    /// WriteFunction statement.
    Writes(Vec<WriterStmt>),
    /// Value returning. A call site is a Call value.
    Returns(WriterBlock),
}

/// Bindings followed by the name of the result: a value function body or a
/// match arm in value position.
pub type WriterBlock = IrBlock<IrBinding<IrType, WriterOp>>;

/// A statement. Statements write to the ambient buffer, in order, or bind
/// a value for the statements after them.
#[derive(Debug)]
pub enum WriterStmt {
    Let(IrBinding<IrType, WriterOp>),
    /// Write a constant string, unescaped.
    Write(String),
    /// Write a String name, escaped.
    WriteString(ValueId),
    /// Invoke an Html function, which writes to the same buffer. The
    /// arguments follow the function's parameters, one for each.
    WriteFunction(IrFunction, Vec<ValueId>),
    // Applies a closure that renders Html, which writes to the same buffer
    // as an Html function does.
    WriteApply(ValueId, Vec<ValueId>),
    /// Run the body once per element of the array, in order.
    HtmlFor(IrFor<Vec<WriterStmt>>),
    /// Run the arm that matches. Matching is exhaustive.
    Match(IrSwitch<Vec<WriterStmt>>),
}

#[derive(Debug)]
pub enum WriterOp {
    Data(IrDataOp),
    Call(IrFunction, Vec<ValueId>),
    /// A match over a value that is not Html. Each arm produces the value.
    Match(IrSwitch<WriterBlock>),
    // A closure whose body is that of a function that writes or returns,
    // as it renders Html or computes a value.
    Lambda(IrLambda<IrType, WriterFunctionBody>),
    Apply(ValueId, Vec<ValueId>),
}

/// Each statement on a line of its own, each starting with a line break,
/// so an empty list writes nothing.
fn write_stmts(stmts: &[WriterStmt], w: &mut dyn Write) -> fmt::Result {
    for stmt in stmts {
        write!(w, "\n{stmt}")?;
    }
    Ok(())
}

/// An HtmlFor, a Match or a let holding a Match or a Lambda spans lines,
/// every other statement is one line.
impl fmt::Display for WriterStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WriterStmt::Let(let_) => write!(f, "{let_}"),
            WriterStmt::Write(content) => write!(f, "write({content:?})"),
            WriterStmt::WriteString(name) => write!(f, "write_string({name})"),
            WriterStmt::WriteFunction(function, args) => {
                write!(f, "write_function {function}({})", join(args, ", "))
            }
            WriterStmt::WriteApply(closure, args) => {
                write!(f, "write_apply {closure}({})", join(args, ", "))
            }
            WriterStmt::HtmlFor(for_) => for_.write(f, |body, w| write_stmts(body, w)),
            WriterStmt::Match(match_) => match_.write(f, |body, w| write_stmts(body, w)),
        }
    }
}

/// A Match or a Lambda spans lines, every other op is one line.
impl fmt::Display for WriterOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WriterOp::Data(op) => write!(f, "{op}"),
            WriterOp::Call(function, args) => write!(f, "{function}({})", join(args, ", ")),
            WriterOp::Match(match_) => match_.write(f, |block, w| write!(w, "\n{block}")),
            WriterOp::Lambda(lambda) => lambda.write(f, WriterFunctionBody::write),
            WriterOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
        }
    }
}

impl WriterFunctionBody {
    /// Starts with a line break.
    fn write(&self, w: &mut dyn Write) -> fmt::Result {
        match self {
            WriterFunctionBody::Writes(statements) => write_stmts(statements, w),
            WriterFunctionBody::Returns(block) => write!(w, "\n{block}"),
        }
    }
}

impl fmt::Display for WriterPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "page {}({}) {{",
            self.name.as_str(),
            join(&self.parameters, ", ")
        )?;
        write_stmts(&self.body, &mut Indent::new(f))?;
        writeln!(f, "\n}}")
    }
}

impl fmt::Display for WriterProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            function.write(f, WriterFunctionBody::write)?;
            writeln!(f)?;
        }
        for page in &self.pages {
            write!(f, "{page}")?;
        }
        Ok(())
    }
}
