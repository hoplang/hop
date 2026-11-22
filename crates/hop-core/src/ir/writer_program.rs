use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_parameter::IrParameter;
use crate::ir::value_id::ValueId;
use crate::ir::writer_switch::WriterSwitch;
use crate::ir::writer_type::{WriterDataType, WriterResult, WriterType, WriterTypeDeclarations};
use crate::symbols::type_name::TypeName;

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
    pub types: WriterTypeDeclarations,
    pub pages: Vec<WriterPageDeclaration>,
    pub functions: Vec<WriterFunctionDeclaration>,
}

#[derive(Debug)]
pub struct WriterFunctionDeclaration {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter<WriterType>>,
    pub return_type: WriterResult,
    pub body: WriterFunctionBody,
}

#[derive(Debug)]
pub struct WriterPageDeclaration {
    pub name: TypeName,
    pub parameters: Vec<IrParameter<WriterType>>,
    pub body: Vec<WriterStmt>,
}

#[derive(Debug)]
pub enum WriterFunctionBody {
    Writes(Vec<WriterStmt>),
    Returns(WriterBlock),
}

pub type WriterBlock = IrBlock<IrBinding<WriterType, WriterOp>>;

#[derive(Debug)]
pub enum WriterStmt {
    Let(IrBinding<WriterType, WriterOp>),
    Write(String),
    WriteString(ValueId),
    WriteFunction(IrFunction, Vec<ValueId>),
    WriteApply(ValueId, Vec<ValueId>),
    HtmlFor(WriterFor),
    Match(WriterSwitch<Vec<WriterStmt>>),
}

#[derive(Debug)]
pub struct WriterFor {
    pub var: Option<IrBinder<WriterDataType>>,
    pub array: ValueId,
    pub body: Vec<WriterStmt>,
}

#[derive(Debug)]
pub enum WriterOp {
    Data(IrDataOp),
    Call(IrFunction, Vec<ValueId>),
    Match(WriterSwitch<WriterBlock>),
    Lambda(IrLambda<WriterType, WriterFunctionBody>),
    Apply(ValueId, Vec<ValueId>),
}

#[cfg(test)]
mod test_support {
    use super::WriterFor;
    use super::WriterFunctionBody;
    use super::WriterFunctionDeclaration;
    use super::WriterOp;
    use super::WriterPageDeclaration;
    use super::WriterProgram;
    use super::WriterStmt;
    use crate::display::{Indent, join};
    use std::fmt::{self, Write};

    fn write_stmts(stmts: &[WriterStmt], w: &mut dyn Write) -> fmt::Result {
        for stmt in stmts {
            write!(w, "\n{stmt}")?;
        }
        Ok(())
    }

    impl fmt::Display for WriterStmt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterStmt::Let(let_) => write!(f, "{let_}"),
                WriterStmt::Write(content) => write!(f, "write({content:?})"),
                WriterStmt::WriteString(name) => write!(f, "write_string({name})"),
                WriterStmt::WriteFunction(function, args) => {
                    write!(f, "write_function f{}({})", function.id, join(args, ", "))
                }
                WriterStmt::WriteApply(closure, args) => {
                    write!(f, "write_apply {closure}({})", join(args, ", "))
                }
                WriterStmt::HtmlFor(WriterFor { var, array, body }) => {
                    match var {
                        Some(binder) => write!(f, "for {binder} in {array} {{")?,
                        None => write!(f, "for _ in {array} {{")?,
                    }
                    write_stmts(body, &mut Indent::new(f))?;
                    f.write_str("\n}")
                }
                WriterStmt::Match(match_) => match_.write(f, |body, w| write_stmts(body, w)),
            }
        }
    }

    impl fmt::Display for WriterOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterOp::Data(op) => write!(f, "{op}"),
                WriterOp::Call(function, args) => {
                    write!(f, "f{}({})", function.id, join(args, ", "))
                }
                WriterOp::Match(match_) => match_.write(f, |block, w| write!(w, "\n{block}")),
                WriterOp::Lambda(lambda) => lambda.write(f, WriterFunctionBody::write),
                WriterOp::Apply(closure, args) => write!(f, "{closure}({})", join(args, ", ")),
            }
        }
    }

    impl WriterFunctionBody {
        fn write(&self, w: &mut dyn Write) -> fmt::Result {
            match self {
                WriterFunctionBody::Writes(statements) => write_stmts(statements, w),
                WriterFunctionBody::Returns(block) => write!(w, "\n{block}"),
            }
        }
    }

    impl fmt::Display for WriterFunctionDeclaration {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "fn {}(", self.function)?;
            for parameter in &self.parameters {
                write!(Indent::new(f), "\n{parameter},")?;
            }
            if !self.parameters.is_empty() {
                f.write_str("\n")?;
            }
            write!(f, ") -> {} {{", self.return_type)?;
            self.body.write(&mut Indent::new(f))?;
            f.write_str("\n}")
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
                writeln!(f, "{function}")?;
            }
            for page in &self.pages {
                write!(f, "{page}")?;
            }
            Ok(())
        }
    }
}
