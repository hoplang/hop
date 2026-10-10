use std::fmt;

use pretty::BoxDoc;

use crate::document::CheapString;
use crate::hop::typing::Type;
use crate::ir::binder_id::BinderId;
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::Match;
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

use super::ir_parameter::IrParameter;

/// A Writer module.
///
/// The form the backends consume. A body is a sequence of statements that
/// write to the ambient buffer, with the values they need bound by lets
/// along the way. A let is in scope for the rest of its block and for the
/// blocks nested in that rest. Operands are names, of a let or of a binder,
/// so a value never contains another value. Html is written rather than
/// held, except where a value needs it, and then an HtmlLiteral renders it
/// into a buffer of its own.
#[derive(Debug)]
pub struct WriterModule {
    pub pages: Vec<WriterPageDeclaration>,
    pub functions: Vec<WriterFunctionDeclaration>,
}

#[derive(Debug)]
pub struct WriterPageDeclaration {
    pub name: TypeName,
    pub parameters: Vec<IrParameter>,
    /// Statements for the assembled page.
    pub body: Vec<WriterStmt>,
}

#[derive(Debug)]
pub struct WriterFunctionDeclaration {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter>,
    pub return_type: Type,
    pub body: WriterFunctionBody,
}

#[derive(Debug)]
pub enum WriterFunctionBody {
    /// Destination passing: writes to the ambient buffer. A call site is a
    /// WriteFunction statement.
    Writes(Vec<WriterStmt>),
    /// Value returning. A call site is a Call value.
    Returns(WriterValueBlock),
}

/// A name a value or statement reads: the name of a let, or a binder,
/// which the Flat IR read through a Read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WriterName {
    Binding(ValueId),
    Binder(BinderId),
}

impl fmt::Display for WriterName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WriterName::Binding(value) => value.fmt(f),
            WriterName::Binder(binder) => binder.fmt(f),
        }
    }
}

/// The source of iteration in a For.
#[derive(Debug, Clone)]
pub enum WriterForSource {
    Array(WriterName),
    RangeInclusive { start: WriterName, end: WriterName },
}

/// A value binding.
#[derive(Debug, Clone)]
pub struct WriterLet {
    pub name: ValueId,
    pub typ: Type,
    pub op: WriterOp,
}

/// Lets followed by the name of the result: a value function body or a
/// match arm in value position.
#[derive(Debug, Clone)]
pub struct WriterValueBlock {
    pub lets: Vec<WriterLet>,
    pub result: WriterName,
}

/// A statement. Statements write to the ambient buffer, in order, or bind
/// a value for the statements after them.
#[derive(Debug, Clone)]
pub enum WriterStmt {
    Let(WriterLet),

    /// Write a constant string, unescaped.
    Write(String),

    /// Write a String name, escaped.
    WriteString(WriterName),

    /// Write an Html name as it is.
    WriteHtml(WriterName),

    /// Invoke an Html function, which writes to the same buffer. The
    /// arguments follow the function's parameters, one for each.
    WriteFunction {
        function: IrFunction,
        args: Vec<WriterName>,
    },

    /// Run the body once per element, in order. When var is None, the loop
    /// binds no variable, but still iterates.
    For {
        var: Option<IrBinder>,
        source: WriterForSource,
        body: Vec<WriterStmt>,
    },

    /// Run the arm that matches. Matching is exhaustive.
    Match(Match<WriterName, Vec<WriterStmt>>),
}

/// The computation of a let. Operands are names.
#[derive(Debug, Clone)]
pub enum WriterOp {
    StringLiteral(CheapString),
    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),

    FieldAccess {
        record: WriterName,
        field: FieldName,
    },

    TupleIndex {
        tuple: WriterName,
        index: usize,
    },

    Array(Vec<WriterName>),

    Tuple(Vec<WriterName>),

    /// The record type is the type of the let.
    Record {
        fields: Vec<(FieldName, WriterName)>,
    },

    /// The enum type is the type of the let.
    Enum {
        variant_name: TypeName,
        fields: Vec<(FieldName, WriterName)>,
    },

    Option(Option<WriterName>),

    StringConcat(Vec<WriterName>),

    Binary {
        op: IrBinaryOp,
        left: WriterName,
        right: WriterName,
    },

    Unary {
        op: IrUnaryOp,
        operand: WriterName,
    },

    /// Invoke a value returning function. The arguments follow the
    /// function's parameters, one for each.
    Call {
        function: IrFunction,
        args: Vec<WriterName>,
    },

    /// Html as a value: the statements render into a fresh buffer.
    HtmlLiteral(Vec<WriterStmt>),

    /// A match over a value that is not Html. Each arm produces the value.
    Match(Match<WriterName, WriterValueBlock>),
}

impl WriterOp {
    /// Apply `f` to each name this op reads directly. The names read
    /// inside an HtmlLiteral or the arms of a Match are not visited, only
    /// the subject of the Match.
    #[cfg(test)]
    pub fn for_each_operand(&self, f: &mut impl FnMut(WriterName)) {
        match self {
            WriterOp::StringLiteral(_)
            | WriterOp::IntLiteral(_)
            | WriterOp::FloatLiteral(_)
            | WriterOp::BoolLiteral(_)
            | WriterOp::Option(None)
            | WriterOp::HtmlLiteral(_) => {}

            WriterOp::FieldAccess { record: name, .. }
            | WriterOp::TupleIndex { tuple: name, .. }
            | WriterOp::Option(Some(name))
            | WriterOp::Unary { operand: name, .. } => f(*name),

            WriterOp::Array(names) | WriterOp::Tuple(names) | WriterOp::StringConcat(names) => {
                for name in names {
                    f(*name);
                }
            }

            WriterOp::Record { fields } | WriterOp::Enum { fields, .. } => {
                for (_, name) in fields {
                    f(*name);
                }
            }

            WriterOp::Binary { left, right, .. } => {
                f(*left);
                f(*right);
            }

            WriterOp::Call { args, .. } => {
                for arg in args {
                    f(*arg);
                }
            }

            WriterOp::Match(match_) => match match_ {
                Match::Bool { subject, .. }
                | Match::Option { subject, .. }
                | Match::Enum { subject, .. } => f(*subject),
            },
        }
    }
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

impl WriterValueBlock {
    /// One line per let and one for the result.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.lets
                .iter()
                .map(WriterLet::to_doc)
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl WriterLet {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("let {}: {} = ", self.name, self.typ)).append(self.op.to_doc())
    }
}

impl WriterStmt {
    /// A For, a Match or a let holding a literal spans lines, every other
    /// statement is one line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            WriterStmt::Let(let_) => let_.to_doc(),
            WriterStmt::Write(content) => BoxDoc::text(format!("write({content:?})")),
            WriterStmt::WriteString(name) => BoxDoc::text(format!("write_string({name})")),
            WriterStmt::WriteHtml(name) => BoxDoc::text(format!("write_html({name})")),
            WriterStmt::WriteFunction { function, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("write_function {function}({args})"))
            }
            WriterStmt::For { var, source, body } => {
                let var = match var {
                    Some(binder) => format!("{}: {}", binder.var, binder.typ),
                    None => "_".to_string(),
                };
                let source = match source {
                    WriterForSource::Array(array) => array.to_string(),
                    WriterForSource::RangeInclusive { start, end } => format!("{start}..={end}"),
                };
                BoxDoc::text(format!("for {var} in {source} {{"))
                    .append(stmts_to_doc(body).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            WriterStmt::Match(match_) => match_.to_doc(
                |subject| BoxDoc::text(subject.to_string()),
                |body| stmts_to_doc(body),
            ),
        }
    }
}

impl WriterOp {
    /// An HtmlLiteral or a Match spans lines, every other op is one
    /// line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            WriterOp::StringLiteral(value) => BoxDoc::text(format!("{:?}", value.as_str())),
            WriterOp::IntLiteral(value) => BoxDoc::text(value.to_string()),
            WriterOp::FloatLiteral(value) => BoxDoc::text(value.to_string()),
            WriterOp::BoolLiteral(value) => BoxDoc::text(value.to_string()),
            WriterOp::FieldAccess { record, field } => {
                BoxDoc::text(format!("{record}.{}", field.as_str()))
            }
            WriterOp::TupleIndex { tuple, index } => BoxDoc::text(format!("{tuple}.{index}")),
            WriterOp::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("[{elements}]"))
            }
            WriterOp::Tuple(elements) => {
                let joined = elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                if elements.len() == 1 {
                    BoxDoc::text(format!("({joined},)"))
                } else {
                    BoxDoc::text(format!("({joined})"))
                }
            }
            WriterOp::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| format!("{}: {value}", name.as_str()))
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("{{{fields}}}"))
            }
            WriterOp::Enum {
                variant_name,
                fields,
            } => {
                if fields.is_empty() {
                    BoxDoc::text(variant_name.as_str())
                } else {
                    let fields = fields
                        .iter()
                        .map(|(name, value)| format!("{}: {value}", name.as_str()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    BoxDoc::text(format!("{} {{{fields}}}", variant_name.as_str()))
                }
            }
            WriterOp::Option(Some(value)) => BoxDoc::text(format!("Some({value})")),
            WriterOp::Option(None) => BoxDoc::text("None"),
            WriterOp::StringConcat(parts) => {
                let parts = parts
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("concat({parts})"))
            }
            WriterOp::Binary { op, left, right } => BoxDoc::text(match op {
                IrBinaryOp::NumericAdd(_) => format!("{left} + {right}"),
                IrBinaryOp::NumericSubtract(_) => format!("{left} - {right}"),
                IrBinaryOp::NumericMultiply(_) => format!("{left} * {right}"),
                IrBinaryOp::Equals(_) => format!("{left} == {right}"),
                IrBinaryOp::LessThan(_) => format!("{left} < {right}"),
                IrBinaryOp::LessThanOrEqual(_) => format!("{left} <= {right}"),
            }),
            WriterOp::Unary { op, operand } => BoxDoc::text(match op {
                IrUnaryOp::NumericNegation(_) => format!("-{operand}"),
                IrUnaryOp::BoolNegation => format!("!{operand}"),
                IrUnaryOp::ArrayLength => format!("{operand}.len()"),
                IrUnaryOp::ArrayIsEmpty => format!("{operand}.is_empty()"),
                IrUnaryOp::StringIsEmpty => format!("{operand}.is_empty()"),
                IrUnaryOp::OptionIsSome => format!("{operand}.is_some()"),
                IrUnaryOp::OptionIsNone => format!("{operand}.is_none()"),
                IrUnaryOp::IntToString => format!("{operand}.to_string()"),
                IrUnaryOp::FloatToInt => format!("{operand}.to_int()"),
                IrUnaryOp::IntToFloat => format!("{operand}.to_float()"),
            }),
            WriterOp::Call { function, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("call {function}({args})"))
            }
            WriterOp::HtmlLiteral(body) => BoxDoc::text("html {")
                .append(stmts_to_doc(body).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            WriterOp::Match(match_) => match_.to_doc(
                |subject| BoxDoc::text(subject.to_string()),
                |body| BoxDoc::line().append(body.to_doc()),
            ),
        }
    }
}

impl WriterFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let parameters = self
            .parameters
            .iter()
            .map(|param| format!("{}@{}: {}", param.name.as_str(), param.var, param.typ))
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
            .map(|param| format!("{}@{}: {}", param.name.as_str(), param.var, param.typ))
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

impl fmt::Display for WriterModule {
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
