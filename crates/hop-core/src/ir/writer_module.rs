use std::fmt;

use pretty::BoxDoc;

use crate::document::CheapString;
use crate::hop::typing::{ComparableType, EquatableType, NumericType, Type};
use crate::ir::binder_id::BinderId;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::Match;
use crate::ir::var_id::VarId;
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
    pub body: Vec<Stmt>,
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
    Writes(Vec<Stmt>),
    /// Value returning. A call site is a Call value.
    Returns(ValueBlock),
}

/// A name a value or statement reads: the name of a let, or a binder,
/// which the Flat IR read through a Read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Name {
    Binding(VarId),
    Binder(BinderId),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Binding(var) => var.fmt(f),
            Name::Binder(binder) => binder.fmt(f),
        }
    }
}

/// The source of iteration in a For.
#[derive(Debug, Clone)]
pub enum ForSource {
    Array(Name),
    RangeInclusive { start: Name, end: Name },
}

/// A value binding.
#[derive(Debug, Clone)]
pub struct Let {
    pub name: VarId,
    pub typ: Type,
    pub value: Value,
}

/// Lets followed by the name of the result: a value function body or a
/// match arm in value position.
#[derive(Debug, Clone)]
pub struct ValueBlock {
    pub lets: Vec<Let>,
    pub result: Name,
}

/// A statement. Statements write to the ambient buffer, in order, or bind
/// a value for the statements after them.
#[derive(Debug, Clone)]
pub enum Stmt {
    Let(Let),

    /// Write a constant string, unescaped.
    Write(String),

    /// Write a String name, escaped.
    WriteString(Name),

    /// Write an Html name as it is.
    WriteHtml(Name),

    /// Invoke an Html function, which writes to the same buffer. The
    /// arguments follow the function's parameters, one for each.
    WriteFunction {
        function: IrFunction,
        args: Vec<Name>,
    },

    /// Run the body once per element, in order. When var is None, the loop
    /// binds no variable, but still iterates.
    For {
        var: Option<IrBinder>,
        source: ForSource,
        body: Vec<Stmt>,
    },

    /// Run the arm that matches. Matching is exhaustive.
    Match(Match<Name, Vec<Stmt>>),
}

/// The computation of a let. Operands are names.
#[derive(Debug, Clone)]
pub enum Value {
    StringLiteral(CheapString),
    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),

    FieldAccess {
        record: Name,
        field: FieldName,
    },

    TupleIndex {
        tuple: Name,
        index: usize,
    },

    Array(Vec<Name>),

    Tuple(Vec<Name>),

    /// The record type is the type of the let.
    Record {
        fields: Vec<(FieldName, Name)>,
    },

    /// The enum type is the type of the let.
    Enum {
        variant_name: TypeName,
        fields: Vec<(FieldName, Name)>,
    },

    Option(Option<Name>),

    StringConcat(Vec<Name>),

    NumericAdd {
        left: Name,
        right: Name,
        operand_types: NumericType,
    },

    NumericSubtract {
        left: Name,
        right: Name,
        operand_types: NumericType,
    },

    NumericMultiply {
        left: Name,
        right: Name,
        operand_types: NumericType,
    },

    NumericNegation {
        operand: Name,
        operand_type: NumericType,
    },

    BoolNegation(Name),

    Equals {
        left: Name,
        right: Name,
        operand_types: EquatableType,
    },

    LessThan {
        left: Name,
        right: Name,
        operand_types: ComparableType,
    },

    LessThanOrEqual {
        left: Name,
        right: Name,
        operand_types: ComparableType,
    },

    ArrayLength(Name),
    ArrayIsEmpty(Name),
    StringIsEmpty(Name),
    OptionIsSome(Name),
    OptionIsNone(Name),
    IntToString(Name),
    FloatToInt(Name),
    IntToFloat(Name),

    /// Invoke a value returning function. The arguments follow the
    /// function's parameters, one for each.
    Call {
        function: IrFunction,
        args: Vec<Name>,
    },

    /// Html as a value: the statements render into a fresh buffer.
    HtmlLiteral(Vec<Stmt>),

    /// A match over a value that is not Html. Each arm produces the value.
    Match(Match<Name, ValueBlock>),
}

impl Value {
    /// Apply `f` to each name this value reads directly. The names read
    /// inside an HtmlLiteral or the arms of a Match are not visited, only
    /// the subject of the Match.
    #[cfg(test)]
    pub fn for_each_operand(&self, f: &mut impl FnMut(Name)) {
        match self {
            Value::StringLiteral(_)
            | Value::IntLiteral(_)
            | Value::FloatLiteral(_)
            | Value::BoolLiteral(_)
            | Value::Option(None)
            | Value::HtmlLiteral(_) => {}

            Value::FieldAccess { record: name, .. }
            | Value::TupleIndex { tuple: name, .. }
            | Value::Option(Some(name))
            | Value::NumericNegation { operand: name, .. }
            | Value::BoolNegation(name)
            | Value::ArrayLength(name)
            | Value::ArrayIsEmpty(name)
            | Value::StringIsEmpty(name)
            | Value::OptionIsSome(name)
            | Value::OptionIsNone(name)
            | Value::IntToString(name)
            | Value::FloatToInt(name)
            | Value::IntToFloat(name) => f(*name),

            Value::Array(names) | Value::Tuple(names) | Value::StringConcat(names) => {
                for name in names {
                    f(*name);
                }
            }

            Value::Record { fields } | Value::Enum { fields, .. } => {
                for (_, name) in fields {
                    f(*name);
                }
            }

            Value::NumericAdd { left, right, .. }
            | Value::NumericSubtract { left, right, .. }
            | Value::NumericMultiply { left, right, .. }
            | Value::Equals { left, right, .. }
            | Value::LessThan { left, right, .. }
            | Value::LessThanOrEqual { left, right, .. } => {
                f(*left);
                f(*right);
            }

            Value::Call { args, .. } => {
                for arg in args {
                    f(*arg);
                }
            }

            Value::Match(match_) => match match_ {
                Match::Bool { subject, .. }
                | Match::Option { subject, .. }
                | Match::Enum { subject, .. } => f(**subject),
            },
        }
    }
}

/// Each statement on a line of its own. The statements start with a line
/// break and the caller nests them, so an empty list prints nothing.
fn stmts_to_doc(stmts: &[Stmt]) -> BoxDoc<'_> {
    BoxDoc::concat(
        stmts
            .iter()
            .map(|stmt| BoxDoc::line().append(stmt.to_doc())),
    )
}

impl ValueBlock {
    /// One line per let and one for the result.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.lets
                .iter()
                .map(Let::to_doc)
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl Let {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("let {}: {} = ", self.name, self.typ)).append(self.value.to_doc())
    }
}

impl Stmt {
    /// A For, a Match or a let holding a literal spans lines, every other
    /// statement is one line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Stmt::Let(let_) => let_.to_doc(),
            Stmt::Write(content) => BoxDoc::text(format!("write({content:?})")),
            Stmt::WriteString(name) => BoxDoc::text(format!("write_string({name})")),
            Stmt::WriteHtml(name) => BoxDoc::text(format!("write_html({name})")),
            Stmt::WriteFunction { function, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("write_function {function}({args})"))
            }
            Stmt::For { var, source, body } => {
                let var = match var {
                    Some(binder) => format!("{}: {}", binder.var, binder.typ),
                    None => "_".to_string(),
                };
                let source = match source {
                    ForSource::Array(array) => array.to_string(),
                    ForSource::RangeInclusive { start, end } => format!("{start}..={end}"),
                };
                BoxDoc::text(format!("for {var} in {source} {{"))
                    .append(stmts_to_doc(body).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            Stmt::Match(match_) => match_.to_doc(
                |subject| BoxDoc::text(subject.to_string()),
                |body| stmts_to_doc(body),
            ),
        }
    }
}

impl Value {
    /// An HtmlLiteral or a Match spans lines, every other value is one
    /// line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Value::StringLiteral(value) => BoxDoc::text(format!("{:?}", value.as_str())),
            Value::IntLiteral(value) => BoxDoc::text(value.to_string()),
            Value::FloatLiteral(value) => BoxDoc::text(value.to_string()),
            Value::BoolLiteral(value) => BoxDoc::text(value.to_string()),
            Value::FieldAccess { record, field } => {
                BoxDoc::text(format!("{record}.{}", field.as_str()))
            }
            Value::TupleIndex { tuple, index } => BoxDoc::text(format!("{tuple}.{index}")),
            Value::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("[{elements}]"))
            }
            Value::Tuple(elements) => {
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
            Value::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| format!("{}: {value}", name.as_str()))
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("{{{fields}}}"))
            }
            Value::Enum {
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
            Value::Option(Some(value)) => BoxDoc::text(format!("Some({value})")),
            Value::Option(None) => BoxDoc::text("None"),
            Value::StringConcat(parts) => {
                let parts = parts
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("concat({parts})"))
            }
            Value::NumericAdd { left, right, .. } => BoxDoc::text(format!("{left} + {right}")),
            Value::NumericSubtract { left, right, .. } => BoxDoc::text(format!("{left} - {right}")),
            Value::NumericMultiply { left, right, .. } => BoxDoc::text(format!("{left} * {right}")),
            Value::NumericNegation { operand, .. } => BoxDoc::text(format!("-{operand}")),
            Value::BoolNegation(operand) => BoxDoc::text(format!("!{operand}")),
            Value::Equals { left, right, .. } => BoxDoc::text(format!("{left} == {right}")),
            Value::LessThan { left, right, .. } => BoxDoc::text(format!("{left} < {right}")),
            Value::LessThanOrEqual { left, right, .. } => {
                BoxDoc::text(format!("{left} <= {right}"))
            }
            Value::ArrayLength(array) => BoxDoc::text(format!("{array}.len()")),
            Value::ArrayIsEmpty(array) => BoxDoc::text(format!("{array}.is_empty()")),
            Value::StringIsEmpty(string) => BoxDoc::text(format!("{string}.is_empty()")),
            Value::OptionIsSome(option) => BoxDoc::text(format!("{option}.is_some()")),
            Value::OptionIsNone(option) => BoxDoc::text(format!("{option}.is_none()")),
            Value::IntToString(value) => BoxDoc::text(format!("{value}.to_string()")),
            Value::FloatToInt(value) => BoxDoc::text(format!("{value}.to_int()")),
            Value::IntToFloat(value) => BoxDoc::text(format!("{value}.to_float()")),
            Value::Call { function, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("call {function}({args})"))
            }
            Value::HtmlLiteral(body) => BoxDoc::text("html {")
                .append(stmts_to_doc(body).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            Value::Match(match_) => match_.to_doc(
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
