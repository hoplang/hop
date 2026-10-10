use std::fmt;

use pretty::BoxDoc;

use crate::document::CheapString;
use crate::hop::typing::Type;
use crate::html::HtmlElementKind;
use crate::ir::binder_id::{BinderId, BinderIdCounter};
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::Match;
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::ir::var_id::{VarId, VarIdCounter};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

use super::ir_parameter::IrParameter;

/// A Flat module.
///
/// A Pure module with its expressions flattened. Every operand is the name
/// of a binding, and every computation is a binding that names its result.
/// The bindings of a block are in evaluation order, so a binding refers only
/// to names bound before it in its own block or in an enclosing block.
///
/// A binding is named by a VarId. A parameter, a loop or a match arm binds
/// a BinderId instead, and a Read op reads a binder into a binding, so a
/// binder is never an operand. Each VarId and each BinderId is bound once
/// in the module, and each is bound with its type.
#[derive(Debug)]
pub struct FlatModule {
    pub pages: Vec<FlatPageDeclaration>,
    pub functions: Vec<FlatFunctionDeclaration>,
    pub var_ids: VarIdCounter,
    pub binder_ids: BinderIdCounter,
}

/// A page declaration in the Flat IR.
#[derive(Debug)]
pub struct FlatPageDeclaration {
    pub name: TypeName,
    pub parameters: Vec<IrParameter>,
    /// Must produce Html, if present.
    pub head: Option<FlatBlock>,
    /// Must produce Html.
    pub body: FlatBlock,
}

/// A function declaration in the Flat IR.
#[derive(Debug)]
pub struct FlatFunctionDeclaration {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter>,
    pub return_type: Type,
    /// Must produce a value of type `return_type`.
    pub body: FlatBlock,
}

/// A scope. Bindings are in evaluation order, and the result is a name
/// bound here or in an enclosing block.
#[derive(Debug, Clone)]
pub struct FlatBlock {
    pub bindings: Vec<FlatBinding>,
    pub result: VarId,
}

/// One binding of a block. The name is in scope for every later binding
/// of the block, for the blocks nested in them, and for the result.
#[derive(Debug, Clone)]
pub struct FlatBinding {
    pub name: VarId,
    pub typ: Type,
    pub op: FlatOp,
}

/// The source of iteration in a HtmlFor.
#[derive(Debug, Clone)]
pub enum FlatForSource {
    Array(VarId),
    RangeInclusive { start: VarId, end: VarId },
}

/// An attribute of a HtmlElement.
#[derive(Debug, Clone)]
pub enum FlatAttribute {
    /// Must name a String.
    Value { name: AttributeName, value: VarId },
    /// Must name a Bool.
    Presence { name: AttributeName, present: VarId },
}

/// The computation of a binding. Operands are names of bindings, so an op
/// never contains another op. Only Match and HtmlFor contain blocks.
///
/// The type of a binding is on the binding, so ops that carried a `typ`
/// in Pure carry none here.
#[derive(Debug, Clone)]
pub enum FlatOp {
    /// The value of a binder in scope: a parameter, a loop variable or a
    /// match arm's variable.
    Read(BinderId),

    StringLiteral(CheapString),
    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),

    /// Text that renders as written, without escaping.
    HtmlText(CheapString),

    /// Must name a record with the field.
    FieldAccess {
        record: VarId,
        field: FieldName,
    },

    /// Must name a tuple with the index.
    TupleIndex {
        tuple: VarId,
        index: usize,
    },

    Array(Vec<VarId>),

    Tuple(Vec<VarId>),

    /// The record type is the type of the binding.
    Record {
        fields: Vec<(FieldName, VarId)>,
    },

    /// The enum type is the type of the binding. Fields are empty for a
    /// unit variant.
    Enum {
        variant_name: TypeName,
        fields: Vec<(FieldName, VarId)>,
    },

    Option(Option<VarId>),

    /// N-ary mappend over String names.
    StringConcat(Vec<VarId>),

    /// Must name two values of the op's operand type. Produces the op's
    /// result type.
    Binary {
        op: IrBinaryOp,
        left: VarId,
        right: VarId,
    },

    /// Must name a value of the op's operand type. Produces the op's
    /// result type.
    Unary {
        op: IrUnaryOp,
        operand: VarId,
    },

    /// Must name a String. Produces its HTML escaped form as Html.
    HtmlEscape(VarId),

    /// N-ary mappend over Html names. Part order is output order.
    HtmlConcat(Vec<VarId>),

    /// An element with its attributes, in the order they render, and its
    /// content. Attribute names are unique within an element.
    ///
    /// The children must name Html. A void element renders without its
    /// children and without an end tag.
    HtmlElement {
        element: HtmlElementKind,
        attributes: Vec<FlatAttribute>,
        children: VarId,
    },

    /// Invokes a function and produces its result. The arguments follow
    /// the function's parameters, one for each.
    Call {
        function: IrFunction,
        args: Vec<VarId>,
    },

    /// A match over an Enum, Bool, or Option name. Each arm is a block
    /// that produces the match's value.
    ///
    /// Matching is exhaustive, a value must match at least one arm.
    Match(Match<VarId, FlatBlock>),

    /// A foldMap over the source, concatenating the body's Html once per
    /// element in iteration order.
    ///
    /// When var is None, the loop binds no variable, but still iterates.
    HtmlFor {
        var: Option<IrBinder>,
        source: FlatForSource,
        body: FlatBlock,
    },
}

impl FlatOp {
    /// Apply `f` to each name this op reads directly. The names read inside
    /// the blocks of a Match or HtmlFor are not visited, only the subject
    /// or the source.
    pub fn for_each_operand(&self, f: &mut impl FnMut(VarId)) {
        match self {
            FlatOp::Read(_)
            | FlatOp::StringLiteral(_)
            | FlatOp::IntLiteral(_)
            | FlatOp::FloatLiteral(_)
            | FlatOp::BoolLiteral(_)
            | FlatOp::HtmlText(_)
            | FlatOp::Option(None) => {}

            FlatOp::FieldAccess { record: name, .. }
            | FlatOp::TupleIndex { tuple: name, .. }
            | FlatOp::Option(Some(name))
            | FlatOp::Unary { operand: name, .. }
            | FlatOp::HtmlEscape(name) => f(*name),

            FlatOp::Array(names)
            | FlatOp::Tuple(names)
            | FlatOp::StringConcat(names)
            | FlatOp::HtmlConcat(names) => {
                for name in names {
                    f(*name);
                }
            }

            FlatOp::Record { fields } | FlatOp::Enum { fields, .. } => {
                for (_, name) in fields {
                    f(*name);
                }
            }

            FlatOp::Binary { left, right, .. } => {
                f(*left);
                f(*right);
            }

            FlatOp::HtmlElement {
                attributes,
                children,
                ..
            } => {
                for attribute in attributes {
                    match attribute {
                        FlatAttribute::Value { value: name, .. }
                        | FlatAttribute::Presence { present: name, .. } => f(*name),
                    }
                }
                f(*children);
            }

            FlatOp::Call { args, .. } => {
                for arg in args {
                    f(*arg);
                }
            }

            FlatOp::Match(match_) => match match_ {
                Match::Bool { subject, .. }
                | Match::Option { subject, .. }
                | Match::Enum { subject, .. } => f(**subject),
            },

            FlatOp::HtmlFor { source, .. } => match source {
                FlatForSource::Array(array) => f(*array),
                FlatForSource::RangeInclusive { start, end } => {
                    f(*start);
                    f(*end);
                }
            },
        }
    }

    /// Apply `f` to each name this op reads directly, to rewrite it. Visits
    /// the same names as `for_each_operand`.
    pub fn for_each_operand_mut(&mut self, f: &mut impl FnMut(&mut VarId)) {
        match self {
            FlatOp::Read(_)
            | FlatOp::StringLiteral(_)
            | FlatOp::IntLiteral(_)
            | FlatOp::FloatLiteral(_)
            | FlatOp::BoolLiteral(_)
            | FlatOp::HtmlText(_)
            | FlatOp::Option(None) => {}

            FlatOp::FieldAccess { record: name, .. }
            | FlatOp::TupleIndex { tuple: name, .. }
            | FlatOp::Option(Some(name))
            | FlatOp::Unary { operand: name, .. }
            | FlatOp::HtmlEscape(name) => f(name),

            FlatOp::Array(names)
            | FlatOp::Tuple(names)
            | FlatOp::StringConcat(names)
            | FlatOp::HtmlConcat(names) => {
                for name in names {
                    f(name);
                }
            }

            FlatOp::Record { fields } | FlatOp::Enum { fields, .. } => {
                for (_, name) in fields {
                    f(name);
                }
            }

            FlatOp::Binary { left, right, .. } => {
                f(left);
                f(right);
            }

            FlatOp::HtmlElement {
                attributes,
                children,
                ..
            } => {
                for attribute in attributes {
                    match attribute {
                        FlatAttribute::Value { value: name, .. }
                        | FlatAttribute::Presence { present: name, .. } => f(name),
                    }
                }
                f(children);
            }

            FlatOp::Call { args, .. } => {
                for arg in args {
                    f(arg);
                }
            }

            FlatOp::Match(match_) => match match_ {
                Match::Bool { subject, .. }
                | Match::Option { subject, .. }
                | Match::Enum { subject, .. } => f(subject),
            },

            FlatOp::HtmlFor { source, .. } => match source {
                FlatForSource::Array(array) => f(array),
                FlatForSource::RangeInclusive { start, end } => {
                    f(start);
                    f(end);
                }
            },
        }
    }
}

impl FlatBlock {
    /// One line per binding and one for the result.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::intersperse(
            self.bindings
                .iter()
                .map(|binding| {
                    BoxDoc::text(format!("let {}: {} = ", binding.name, binding.typ))
                        .append(binding.op.to_doc())
                })
                .chain(std::iter::once(BoxDoc::text(self.result.to_string()))),
            BoxDoc::line(),
        )
    }
}

impl FlatOp {
    /// A Match or HtmlFor spans lines, every other op is one line.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            FlatOp::Read(binder) => BoxDoc::text(binder.to_string()),
            FlatOp::StringLiteral(value) => BoxDoc::text(format!("{:?}", value.as_str())),
            FlatOp::IntLiteral(value) => BoxDoc::text(value.to_string()),
            FlatOp::FloatLiteral(value) => BoxDoc::text(value.to_string()),
            FlatOp::BoolLiteral(value) => BoxDoc::text(value.to_string()),
            FlatOp::HtmlText(content) => BoxDoc::text(format!("text({:?})", content.as_str())),
            FlatOp::FieldAccess { record, field } => {
                BoxDoc::text(format!("{record}.{}", field.as_str()))
            }
            FlatOp::TupleIndex { tuple, index } => BoxDoc::text(format!("{tuple}.{index}")),
            FlatOp::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("[{elements}]"))
            }
            FlatOp::Tuple(elements) => {
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
            FlatOp::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| format!("{}: {value}", name.as_str()))
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("{{{fields}}}"))
            }
            FlatOp::Enum {
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
            FlatOp::Option(Some(value)) => BoxDoc::text(format!("Some({value})")),
            FlatOp::Option(None) => BoxDoc::text("None"),
            FlatOp::StringConcat(parts) | FlatOp::HtmlConcat(parts) => {
                let parts = parts
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("concat({parts})"))
            }
            FlatOp::Binary { op, left, right } => BoxDoc::text(match op {
                IrBinaryOp::NumericAdd(_) => format!("{left} + {right}"),
                IrBinaryOp::NumericSubtract(_) => format!("{left} - {right}"),
                IrBinaryOp::NumericMultiply(_) => format!("{left} * {right}"),
                IrBinaryOp::Equals(_) => format!("{left} == {right}"),
                IrBinaryOp::LessThan(_) => format!("{left} < {right}"),
                IrBinaryOp::LessThanOrEqual(_) => format!("{left} <= {right}"),
            }),
            FlatOp::Unary { op, operand } => BoxDoc::text(match op {
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
            FlatOp::HtmlEscape(string) => BoxDoc::text(format!("escape({string})")),
            FlatOp::HtmlElement {
                element,
                attributes,
                children,
            } => {
                let attributes = attributes
                    .iter()
                    .map(|attribute| match attribute {
                        FlatAttribute::Value { name, value } => {
                            format!("{}: {value}", name.as_str())
                        }
                        FlatAttribute::Presence { name, present } => {
                            format!("{}: {present}", name.as_str())
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!(
                    "html({:?}, {{{attributes}}}, {children})",
                    element.as_str()
                ))
            }
            FlatOp::Call { function, args } => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                BoxDoc::text(format!("call {function}({args})"))
            }
            FlatOp::Match(match_) => match_.to_doc(
                |subject| BoxDoc::text(subject.to_string()),
                |body| BoxDoc::line().append(body.to_doc()),
            ),
            FlatOp::HtmlFor { var, source, body } => {
                let var = match var {
                    Some(binder) => format!("{}: {}", binder.var, binder.typ),
                    None => "_".to_string(),
                };
                let source = match source {
                    FlatForSource::Array(array) => array.to_string(),
                    FlatForSource::RangeInclusive { start, end } => format!("{start}..={end}"),
                };
                BoxDoc::text(format!("for {var} in {source} {{"))
                    .append(BoxDoc::line().append(body.to_doc()).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
        }
    }
}

impl FlatFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let parameters = self
            .parameters
            .iter()
            .map(|param| format!("{}@{}: {}", param.name.as_str(), param.var, param.typ))
            .collect::<Vec<_>>()
            .join(", ");
        BoxDoc::text(format!(
            "fn {}({parameters}) -> {} {{",
            self.function, self.return_type
        ))
        .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
        .append(BoxDoc::line())
        .append(BoxDoc::text("}"))
    }
}

impl FlatPageDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let parameters = self
            .parameters
            .iter()
            .map(|param| format!("{}@{}: {}", param.name.as_str(), param.var, param.typ))
            .collect::<Vec<_>>()
            .join(", ");
        let header = BoxDoc::text(format!("page {}({parameters}) {{", self.name.as_str()));
        // A page with only a body prints the body alone. One with a head
        // prints both as the members they were declared as.
        let Some(head) = &self.head else {
            return header
                .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"));
        };
        let head = BoxDoc::text("fn head() -> Html {")
            .append(BoxDoc::line().append(head.to_doc()).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"));
        let body = BoxDoc::text("fn body() -> Html {")
            .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"));
        header
            .append(
                BoxDoc::line()
                    .append(head)
                    .append(BoxDoc::line())
                    .append(body)
                    .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for FlatFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for FlatPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for FlatModule {
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
