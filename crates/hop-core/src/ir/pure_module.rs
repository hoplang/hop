use std::fmt;

use crate::document::CheapString;
use crate::hop::typing::{NumericType, Type};
use crate::html::HtmlElementKind;
use crate::ir::binder_id::BinderId;
use crate::ir::binder_id::BinderIdCounter;
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::Match;
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use pretty::BoxDoc;

use super::ir_parameter::IrParameter;

/// A Pure module.
///
/// An expression-only, side-effect-free form of the IR.
///
/// All IDs in the module are unique across the whole module. Each binder has
/// a unique BinderId, so two binders are never the same variable: shadowing is
/// impossible and substitution is capture-free.
#[derive(Debug, Clone)]
pub struct PureModule {
    pub pages: Vec<PurePageDeclaration>,
    pub functions: Vec<PureFunctionDeclaration>,
    pub binder_ids: BinderIdCounter,
}

/// A page declaration in Pure.
#[derive(Debug, Clone)]
pub struct PurePageDeclaration {
    /// Page name
    pub name: TypeName,
    /// Parameter names with their types
    pub parameters: Vec<IrParameter>,
    /// PureIR expression for the page head. Must be of type `Html`.
    pub head: PureExpr,
    /// PureIR expression for the page body. Must be of type `Html`.
    pub body: PureExpr,
}

/// A function declaration in Pure.
#[derive(Debug, Clone)]
pub struct PureFunctionDeclaration {
    /// The function's identity, carrying its source name.
    pub function: IrFunction,
    /// Parameter names with their types
    pub parameters: Vec<IrParameter>,
    /// The function's return type. The body must be of this type.
    pub return_type: Type,
    /// PureIR expression for the function body. Must be of type `return_type`.
    pub body: PureExpr,
}

/// The source of iteration in a HtmlFor.
#[derive(Debug, Clone)]
pub enum PureForSource {
    /// Iterate over elements of an array.
    Array(PureExpr),
    /// Iterate over an inclusive integer range.
    RangeInclusive { start: PureExpr, end: PureExpr },
}

/// An attribute of a HtmlElement.
#[derive(Debug, Clone)]
pub enum PureAttribute {
    /// An attribute with a value, which renders escaped between quotes.
    ///
    /// Must hold a String.
    Value {
        name: AttributeName,
        value: PureExpr,
    },

    /// A boolean attribute, which renders without a value when present is
    /// true and does not render when it is false.
    ///
    /// Must hold a Bool.
    Presence {
        name: AttributeName,
        present: PureExpr,
    },
}

#[derive(Debug, Clone)]
pub enum PureExpr {
    /// A Let expression.
    ///
    /// The binder's type must match the value's type, and `typ` is the
    /// type of the body.
    Let {
        var: IrBinder,
        value: Box<PureExpr>,
        body: Box<PureExpr>,
        typ: Type,
    },

    /// A Match expression over an Enum, Bool, or Option.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        match_: Match<PureExpr, PureExpr>,
        typ: Type,
    },

    /// A VariableReference expression.
    ///
    /// Reads the value bound by its binder.
    ///
    /// The `typ` field must match the binder's type.
    VariableReference { value: BinderId, typ: Type },

    /// A FieldAccess expression.
    ///
    /// The expression must evaluate to a record and the field must exist on
    /// the record.
    FieldAccess {
        record: Box<PureExpr>,
        field: FieldName,
        typ: Type,
    },

    /// A StringLiteral expression.
    StringLiteral { value: CheapString },

    /// A HtmlText expression.
    ///
    /// Text that renders as written, without escaping.
    HtmlText { content: CheapString },

    /// A HtmlEscape expression.
    ///
    /// HTML-escapes a String-typed expression into Html.
    ///
    /// Must hold a String.
    HtmlEscape { expr: Box<PureExpr> },

    /// A HtmlElement expression.
    ///
    /// An element with its attributes, in the order they render, and its
    /// content. Attribute names are unique within an element.
    ///
    /// The children must be Html. A void element has no content, so it
    /// renders without its children and without an end tag.
    HtmlElement {
        element: HtmlElementKind,
        attributes: Vec<PureAttribute>,
        children: Box<PureExpr>,
    },

    /// A HtmlConcat expression.
    ///
    /// N-ary mappend over Html-typed parts.
    ///
    /// Part order is output order.
    ///
    /// Every part must be Html-typed.
    HtmlConcat { parts: Vec<PureExpr> },

    /// A HtmlFor expression.
    ///
    /// A foldMap over source, concatenating body once per element in iteration order.
    ///
    /// When var is None, the loop binds no variable, but still iterates.
    ///
    /// The type of body must be Html.
    HtmlFor {
        var: Option<IrBinder>,
        source: Box<PureForSource>,
        body: Box<PureExpr>,
    },

    /// A call expression.
    ///
    /// Invokes a function and produces its result. The arguments follow
    /// the function's parameters, one for each.
    Call {
        function: IrFunction,
        args: Vec<PureExpr>,
        typ: Type,
    },

    /// A BoolLiteral expression.
    BoolLiteral { value: bool },

    /// A FloatLiteral expression.
    FloatLiteral { value: f64 },

    /// An IntLiteral expression.
    IntLiteral { value: i32 },

    /// An array expression.
    Array { elements: Vec<PureExpr>, typ: Type },

    /// A tuple expression.
    Tuple { elements: Vec<PureExpr>, typ: Type },

    /// A TupleIndex expression.
    TupleIndex {
        tuple: Box<PureExpr>,
        index: usize,
        typ: Type,
    },

    /// A record expression.
    Record {
        type_name: TypeName,
        fields: Vec<(FieldName, PureExpr)>,
        typ: Type,
    },

    /// An enum expression.
    Enum {
        type_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, PureExpr)>,
        typ: Type,
    },

    /// An option expression.
    Option {
        value: Option<Box<PureExpr>>,
        typ: Type,
    },

    /// A StringConcat expression.
    ///
    /// N-ary mappend over String-typed parts.
    StringConcat { parts: Vec<PureExpr> },

    /// A binary operation.
    ///
    /// Must hold two expressions of the op's operand type.
    /// Returns the op's result type.
    Binary {
        op: IrBinaryOp,
        left: Box<PureExpr>,
        right: Box<PureExpr>,
    },

    /// A unary operation.
    ///
    /// Must hold an expression of the op's operand type.
    /// Returns the op's result type.
    Unary {
        op: IrUnaryOp,
        operand: Box<PureExpr>,
    },

    /// A BoolLogicalAnd expression.
    ///
    /// Must hold two Bool expressions.
    /// Returns a Bool.
    BoolLogicalAnd {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
    },

    /// A BoolLogicalOr expression.
    ///
    /// Must hold two Bool expressions.
    /// Returns a Bool.
    BoolLogicalOr {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
    },
}

impl PureExpr {
    /// The type of this expression.
    pub fn typ(&self) -> Type {
        match self {
            PureExpr::VariableReference { typ, .. }
            | PureExpr::FieldAccess { typ, .. }
            | PureExpr::Array { typ, .. }
            | PureExpr::Tuple { typ, .. }
            | PureExpr::TupleIndex { typ, .. }
            | PureExpr::Record { typ, .. }
            | PureExpr::Enum { typ, .. }
            | PureExpr::Option { typ, .. }
            | PureExpr::Match { typ, .. }
            | PureExpr::Let { typ, .. }
            | PureExpr::Call { typ, .. } => typ.clone(),

            PureExpr::FloatLiteral { .. } => Type::Float,
            PureExpr::IntLiteral { .. } => Type::Int,

            PureExpr::HtmlText { .. }
            | PureExpr::HtmlEscape { .. }
            | PureExpr::HtmlElement { .. }
            | PureExpr::HtmlConcat { .. }
            | PureExpr::HtmlFor { .. } => Type::Html,

            PureExpr::StringConcat { .. } | PureExpr::StringLiteral { .. } => Type::String,

            PureExpr::Binary { op, .. } => match op {
                IrBinaryOp::NumericAdd(operand_types)
                | IrBinaryOp::NumericSubtract(operand_types)
                | IrBinaryOp::NumericMultiply(operand_types) => match operand_types {
                    NumericType::Int => Type::Int,
                    NumericType::Float => Type::Float,
                },
                IrBinaryOp::Equals(_)
                | IrBinaryOp::LessThan(_)
                | IrBinaryOp::LessThanOrEqual(_) => Type::Bool,
            },

            PureExpr::BoolLiteral { .. }
            | PureExpr::BoolLogicalAnd { .. }
            | PureExpr::BoolLogicalOr { .. } => Type::Bool,

            PureExpr::Unary { op, .. } => match op {
                IrUnaryOp::NumericNegation(NumericType::Int) => Type::Int,
                IrUnaryOp::NumericNegation(NumericType::Float) => Type::Float,
                IrUnaryOp::BoolNegation
                | IrUnaryOp::ArrayIsEmpty
                | IrUnaryOp::StringIsEmpty
                | IrUnaryOp::OptionIsSome
                | IrUnaryOp::OptionIsNone => Type::Bool,
                IrUnaryOp::ArrayLength | IrUnaryOp::FloatToInt => Type::Int,
                IrUnaryOp::IntToString => Type::String,
                IrUnaryOp::IntToFloat => Type::Float,
            },
        }
    }

    /// Apply `f` to each direct child expression, without rebuilding.
    ///
    /// The read-only counterpart to `map_children`, and it treats binding
    /// structure the same way: binders are not distinguished from any other
    /// child, so a visitor that cares about scope must intercept `Let`,
    /// `Match` and `HtmlFor` before falling through to this.
    #[cfg(test)]
    pub fn for_each_child(&self, f: &mut impl FnMut(&PureExpr)) {
        match self {
            PureExpr::Let { value, body, .. } => {
                f(value);
                f(body);
            }

            PureExpr::Match { match_, .. } => match match_ {
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => {
                    f(subject);
                    f(true_body);
                    f(false_body);
                }
                Match::Option {
                    subject,
                    some_arm_body,
                    none_arm_body,
                    ..
                } => {
                    f(subject);
                    f(some_arm_body);
                    f(none_arm_body);
                }
                Match::Enum { subject, arms } => {
                    f(subject);
                    for arm in arms {
                        f(&arm.body);
                    }
                }
            },

            PureExpr::HtmlFor { source, body, .. } => {
                match &**source {
                    PureForSource::Array(array) => f(array),
                    PureForSource::RangeInclusive { start, end } => {
                        f(start);
                        f(end);
                    }
                }
                f(body);
            }

            PureExpr::FieldAccess { record, .. } => f(record),

            PureExpr::HtmlEscape { expr, .. } => f(expr),

            PureExpr::HtmlElement {
                attributes,
                children,
                ..
            } => {
                for attribute in attributes {
                    match attribute {
                        PureAttribute::Value { value, .. } => f(value),
                        PureAttribute::Presence { present, .. } => f(present),
                    }
                }
                f(children);
            }

            PureExpr::HtmlConcat { parts, .. } | PureExpr::StringConcat { parts, .. } => {
                for part in parts {
                    f(part);
                }
            }

            PureExpr::Call { args, .. } => {
                for arg in args {
                    f(arg);
                }
            }

            PureExpr::Array { elements, .. } | PureExpr::Tuple { elements, .. } => {
                for element in elements {
                    f(element);
                }
            }

            PureExpr::TupleIndex { tuple, .. } => f(tuple),

            PureExpr::Record { fields, .. } | PureExpr::Enum { fields, .. } => {
                for (_, value) in fields {
                    f(value);
                }
            }

            PureExpr::Option { value, .. } => {
                if let Some(value) = value {
                    f(value);
                }
            }

            PureExpr::Unary { operand, .. } => f(operand),

            PureExpr::Binary { left, right, .. }
            | PureExpr::BoolLogicalAnd { left, right, .. }
            | PureExpr::BoolLogicalOr { left, right, .. } => {
                f(left);
                f(right);
            }

            PureExpr::VariableReference { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::HtmlText { .. }
            | PureExpr::BoolLiteral { .. }
            | PureExpr::FloatLiteral { .. }
            | PureExpr::IntLiteral { .. } => {}
        }
    }
}

impl PureAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            PureAttribute::Value { name, value: expr }
            | PureAttribute::Presence {
                name,
                present: expr,
            } => BoxDoc::text(name.as_str())
                .append(BoxDoc::text(": "))
                .append(expr.to_doc()),
        }
    }
}

impl PurePageDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let header = BoxDoc::nil()
            .append("page ")
            .append(self.name.as_str())
            .append(BoxDoc::text("("))
            .append(params_to_doc(&self.parameters))
            .append(BoxDoc::text(") {"));
        // A page with only a body prints the body alone. One with a head
        // prints both as the members they were declared as.
        if matches!(&self.head, PureExpr::HtmlConcat { parts, .. } if parts.is_empty()) {
            return header
                .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"));
        }
        let head = BoxDoc::text("fn head() -> Html {")
            .append(BoxDoc::line().append(self.head.to_doc()).nest(2))
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

impl PureFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("fn ")
            .append(BoxDoc::text(self.function.to_string()))
            .append(BoxDoc::text("("))
            .append(params_to_doc(&self.parameters))
            .append(BoxDoc::text(") -> "))
            .append(self.return_type.to_doc())
            .append(BoxDoc::text(" {"))
            .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

fn params_to_doc(parameters: &[IrParameter]) -> BoxDoc<'_> {
    BoxDoc::nil()
        .append(BoxDoc::line_())
        .append(BoxDoc::intersperse(
            parameters.iter().map(|param| {
                // Both names: uses of the parameter in the body print as the
                // variable, the declaration is what callers name.
                BoxDoc::text(param.name.to_string())
                    .append(BoxDoc::text("@"))
                    .append(BoxDoc::text(param.var.to_string()))
                    .append(BoxDoc::text(": "))
                    .append(param.typ.to_doc())
            }),
            BoxDoc::text(",").append(BoxDoc::line()),
        ))
        // trailing comma if laid out on multiple lines
        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
        .append(BoxDoc::line_())
        .nest(2)
        .group()
}

impl PureExpr {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            PureExpr::VariableReference { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::FieldAccess { record, field, .. } => record
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            PureExpr::StringLiteral { value, .. } => BoxDoc::text(format!("{:?}", value.as_str())),
            PureExpr::HtmlText { content, .. } => BoxDoc::text("text(")
                .append(BoxDoc::text(format!("{:?}", content)))
                .append(")"),
            PureExpr::HtmlEscape { expr, .. } => {
                BoxDoc::text("escape(").append(expr.to_doc()).append(")")
            }
            PureExpr::HtmlElement {
                element,
                attributes,
                children,
                ..
            } => {
                let attrs = if attributes.is_empty() {
                    BoxDoc::text("[]")
                } else {
                    BoxDoc::text("[")
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    attributes.iter().map(|attribute| attribute.to_doc()),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("]"))
                };
                let mut sections = vec![
                    BoxDoc::text(format!("tag: {:?}", element.as_str())),
                    BoxDoc::text("attrs: ").append(attrs),
                ];
                if !element.is_void() {
                    sections.push(BoxDoc::text("children: ").append(children.to_doc()));
                }
                BoxDoc::text("html(")
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
            PureExpr::HtmlConcat { parts, .. } | PureExpr::StringConcat { parts, .. } => {
                if parts.is_empty() {
                    BoxDoc::text("concat()")
                } else {
                    BoxDoc::text("concat(")
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    parts.iter().map(|part| part.to_doc()),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text(")"))
                }
            }
            PureExpr::HtmlFor {
                var, source, body, ..
            } => {
                let source_doc = match source.as_ref() {
                    PureForSource::Array(array) => array.to_doc(),
                    PureForSource::RangeInclusive { start, end } => start
                        .to_doc()
                        .append(BoxDoc::text("..="))
                        .append(end.to_doc()),
                };
                let var_doc = match var {
                    Some(binder) => BoxDoc::text(binder.var.to_string())
                        .append(BoxDoc::text(": "))
                        .append(binder.typ.to_doc()),
                    None => BoxDoc::text("_"),
                };
                BoxDoc::text("for ")
                    .append(var_doc)
                    .append(BoxDoc::text(" in "))
                    .append(source_doc)
                    .append(BoxDoc::text(" {"))
                    .append(BoxDoc::line().append(body.to_doc()).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
                    .group()
            }
            PureExpr::Call { function, args, .. } => {
                let mut doc = BoxDoc::text("call ")
                    .append(BoxDoc::text(function.to_string()))
                    .append(BoxDoc::text("("));
                if !args.is_empty() {
                    doc = doc.append(BoxDoc::intersperse(
                        args.iter().map(|arg| arg.to_doc()),
                        BoxDoc::text(", "),
                    ));
                }
                doc.append(BoxDoc::text(")"))
            }
            PureExpr::BoolLiteral { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::Array { elements, .. } => {
                if elements.is_empty() {
                    BoxDoc::text("[]")
                } else {
                    BoxDoc::text("[")
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    elements.iter().map(|e| e.to_doc()),
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
            PureExpr::Tuple { elements, .. } => BoxDoc::text("(")
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            elements.iter().map(|e| e.to_doc()),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(if elements.len() == 1 {
                            BoxDoc::text(",")
                        } else {
                            BoxDoc::text(",").flat_alt(BoxDoc::nil())
                        })
                        .append(BoxDoc::line_())
                        .nest(2)
                        .group(),
                )
                .append(BoxDoc::text(")")),
            PureExpr::TupleIndex { tuple, index, .. } => tuple
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(index.to_string())),
            PureExpr::Record {
                type_name, fields, ..
            } => {
                if fields.is_empty() {
                    BoxDoc::text(type_name.as_str()).append(BoxDoc::text(" {}"))
                } else {
                    BoxDoc::text(type_name.as_str())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    fields.iter().map(|(key, value)| {
                                        BoxDoc::text(key.as_str())
                                            .append(BoxDoc::text(": "))
                                            .append(value.to_doc())
                                    }),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
            }
            PureExpr::Binary { op, left, right } => {
                let operator = match op {
                    IrBinaryOp::NumericAdd(_) => " + ",
                    IrBinaryOp::NumericSubtract(_) => " - ",
                    IrBinaryOp::NumericMultiply(_) => " * ",
                    IrBinaryOp::Equals(_) => " == ",
                    IrBinaryOp::LessThan(_) => " < ",
                    IrBinaryOp::LessThanOrEqual(_) => " <= ",
                };
                BoxDoc::nil()
                    .append(BoxDoc::text("("))
                    .append(left.to_doc())
                    .append(BoxDoc::text(operator))
                    .append(right.to_doc())
                    .append(BoxDoc::text(")"))
            }
            PureExpr::Unary { op, operand } => match op {
                IrUnaryOp::NumericNegation(_) => BoxDoc::nil()
                    .append(BoxDoc::text("("))
                    .append(BoxDoc::text("-"))
                    .append(operand.to_doc())
                    .append(BoxDoc::text(")")),
                IrUnaryOp::BoolNegation => BoxDoc::nil()
                    .append(BoxDoc::text("("))
                    .append(BoxDoc::text("!"))
                    .append(operand.to_doc())
                    .append(BoxDoc::text(")")),
                IrUnaryOp::ArrayLength => operand.to_doc().append(BoxDoc::text(".len()")),
                IrUnaryOp::ArrayIsEmpty | IrUnaryOp::StringIsEmpty => {
                    operand.to_doc().append(BoxDoc::text(".is_empty()"))
                }
                IrUnaryOp::OptionIsSome => operand.to_doc().append(BoxDoc::text(".is_some()")),
                IrUnaryOp::OptionIsNone => operand.to_doc().append(BoxDoc::text(".is_none()")),
                IrUnaryOp::IntToString => operand.to_doc().append(BoxDoc::text(".to_string()")),
                IrUnaryOp::FloatToInt => operand.to_doc().append(BoxDoc::text(".to_int()")),
                IrUnaryOp::IntToFloat => operand.to_doc().append(BoxDoc::text(".to_float()")),
            },
            PureExpr::BoolLogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::BoolLogicalOr { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" || "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::Enum {
                type_name,
                variant_name,
                fields,
                ..
            } => {
                let base = BoxDoc::text(type_name.as_str())
                    .append(BoxDoc::text("::"))
                    .append(BoxDoc::text(variant_name.as_str()));
                if fields.is_empty() {
                    base
                } else {
                    base.append(BoxDoc::text(" {"))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(name, expr)| {
                                BoxDoc::text(name.as_str())
                                    .append(BoxDoc::text(": "))
                                    .append(expr.to_doc())
                            }),
                            BoxDoc::text(", "),
                        ))
                        .append(BoxDoc::text("}"))
                }
            }
            PureExpr::Option { value, typ, .. } => {
                let inner_type = match typ {
                    Type::Option(inner) => inner.to_doc(),
                    _ => panic!("Option expression must have Option type, got {:?}", typ),
                };
                let type_prefix = BoxDoc::text("Option[")
                    .append(inner_type)
                    .append(BoxDoc::text("]::"));
                match value {
                    Some(inner) => type_prefix
                        .append(BoxDoc::text("Some("))
                        .append(inner.to_doc())
                        .append(BoxDoc::text(")")),
                    None => type_prefix.append(BoxDoc::text("None")),
                }
            }
            PureExpr::Match { match_, .. } => match_
                .to_doc(PureExpr::to_doc, |body| {
                    BoxDoc::line().append(body.to_doc())
                })
                .group(),
            PureExpr::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.var.to_string()))
                .append(BoxDoc::text(": "))
                .append(var.typ.to_doc())
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in {"))
                .append(BoxDoc::line().append(body.to_doc()).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .group(),
        }
    }
}

impl fmt::Display for PureExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for PurePageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for PureFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for PureModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            write!(f, "{}", function)?;
        }
        for page in &self.pages {
            write!(f, "{}", page)?;
        }
        Ok(())
    }
}
