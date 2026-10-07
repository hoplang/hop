use std::fmt;

use crate::document::CheapString;
use crate::hop::typing::{ComparableType, EquatableType, NumericType, Type};
use crate::ir::expr_id::{ExprId, ExprIdCounter};
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::ir_var::IrVar;
use crate::ir::var_id::VarIdCounter;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

use super::writer_module::WriterParameter;

/// A Pure module.
///
/// An expression-only, side-effect-free form of the IR.
///
/// All IDs in the module are unique across the whole module. Each binder has
/// a unique VarId, so two binders are never the same variable: shadowing is
/// impossible and substitution is capture-free.
#[derive(Debug)]
pub struct PureModule {
    pub pages: Vec<PurePageDeclaration>,
    pub functions: Vec<PureFunctionDeclaration>,
    pub expr_ids: ExprIdCounter,
    pub var_ids: VarIdCounter,
}

/// A page declaration in Pure.
#[derive(Debug)]
pub struct PurePageDeclaration {
    /// Page name
    pub name: TypeName,
    /// Parameter names with their types
    pub parameters: Vec<WriterParameter>,
    /// PureIR expression for the page head. Must be of type `Html`.
    pub head: PureExpr,
    /// PureIR expression for the page body. Must be of type `Html`.
    pub body: PureExpr,
}

/// A function declaration in Pure.
#[derive(Debug)]
pub struct PureFunctionDeclaration {
    /// The function's identity, carrying its source name.
    pub function: IrFunction,
    /// Parameter names with their types
    pub parameters: Vec<WriterParameter>,
    /// The function's return type. The body must be of this type.
    pub return_type: Type,
    /// PureIR expression for the function body. Must be of type `return_type`.
    pub body: PureExpr,
}

/// The source of iteration in a HtmlFor.
#[derive(Debug, Clone, PartialEq)]
pub enum PureForSource {
    /// Iterate over elements of an array.
    Array(PureExpr),
    /// Iterate over an inclusive integer range.
    RangeInclusive { start: PureExpr, end: PureExpr },
}

/// An argument passed to a Call.
#[derive(Debug, Clone, PartialEq)]
pub struct PureArgument {
    pub name: VarName,
    pub expr: PureExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PureExpr {
    /// A Let expression.
    Let {
        var: IrVar,
        value: Box<PureExpr>,
        body: Box<PureExpr>,
        typ: Type,
        id: ExprId,
    },

    /// A Match expression over an Enum, Bool, or Option.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        match_: Match<PureExpr, PureExpr>,
        typ: Type,
        id: ExprId,
    },

    /// A VariableReference expression.
    ///
    /// Reads the value bound by its binder.
    ///
    /// The `typ` field must match the binder's type.
    VariableReference { value: IrVar, typ: Type, id: ExprId },

    /// A FieldAccess expression.
    ///
    /// The expression must evaluate to a record and the field must exist on
    /// the record.
    FieldAccess {
        record: Box<PureExpr>,
        field: FieldName,
        typ: Type,
        id: ExprId,
    },

    /// A StringLiteral expression.
    StringLiteral { value: CheapString, id: ExprId },

    /// A HtmlRaw expression.
    ///
    /// A trusted, already-escaped HTML atom.
    HtmlRaw { content: String, id: ExprId },

    /// A HtmlEscape expression.
    ///
    /// HTML-escapes a String-typed expression into Html.
    ///
    /// Must hold a String.
    HtmlEscape { expr: Box<PureExpr>, id: ExprId },

    /// A HtmlConcat expression.
    ///
    /// N-ary mappend over Html-typed parts.
    ///
    /// Part order is output order.
    ///
    /// Every part must be Html-typed.
    HtmlConcat { parts: Vec<PureExpr>, id: ExprId },

    /// A HtmlFor expression.
    ///
    /// A foldMap over source, concatenating body once per element in iteration order.
    ///
    /// When var is None, the loop binds no variable, but still iterates.
    ///
    /// The type of body must be Html.
    HtmlFor {
        var: Option<IrVar>,
        source: Box<PureForSource>,
        body: Box<PureExpr>,
        id: ExprId,
    },

    /// A call expression.
    ///
    /// Invokes a function and produces its result.
    Call {
        function: IrFunction,
        args: Vec<PureArgument>,
        typ: Type,
        id: ExprId,
    },

    /// A BoolLiteral expression.
    BoolLiteral { value: bool, id: ExprId },

    /// A FloatLiteral expression.
    FloatLiteral { value: f64, id: ExprId },

    /// An IntLiteral expression.
    IntLiteral { value: i32, id: ExprId },

    /// An array expression.
    Array {
        elements: Vec<PureExpr>,
        typ: Type,
        id: ExprId,
    },

    /// A tuple expression.
    Tuple {
        elements: Vec<PureExpr>,
        typ: Type,
        id: ExprId,
    },

    /// A TupleIndex expression.
    TupleIndex {
        tuple: Box<PureExpr>,
        index: usize,
        typ: Type,
        id: ExprId,
    },

    /// A record expression.
    Record {
        type_name: TypeName,
        fields: Vec<(FieldName, PureExpr)>,
        typ: Type,
        id: ExprId,
    },

    /// An enum expression.
    Enum {
        type_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, PureExpr)>,
        typ: Type,
        id: ExprId,
    },

    /// An option expression.
    Option {
        value: Option<Box<PureExpr>>,
        typ: Type,
        id: ExprId,
    },

    /// A StringConcat expression.
    ///
    /// N-ary mappend over String-typed parts.
    StringConcat { parts: Vec<PureExpr>, id: ExprId },

    /// A NumericAdd expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericAdd {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// A NumericSubtract expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericSubtract {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// A NumericMultiply expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericMultiply {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// A NumericNegation expression.
    ///
    /// Must hold an expression of a NumericType.
    /// Returns the NumericType of the expression.
    NumericNegation {
        operand: Box<PureExpr>,
        operand_type: NumericType,
        id: ExprId,
    },

    /// A BoolNegation expression.
    ///
    /// Must hold a Bool expression.
    /// Returns a Bool.
    BoolNegation { operand: Box<PureExpr>, id: ExprId },

    /// A BoolLogicalAnd expression.
    ///
    /// Must hold two Bool expressions.
    /// Returns a Bool.
    BoolLogicalAnd {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        id: ExprId,
    },

    /// A BoolLogicalOr expression.
    ///
    /// Must hold two Bool expressions.
    /// Returns a Bool.
    BoolLogicalOr {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        id: ExprId,
    },

    /// An Equals expression.
    ///
    /// Must hold two values of the same EquatableType.
    /// Returns a Bool.
    Equals {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: EquatableType,
        id: ExprId,
    },

    /// A LessThan expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Bool.
    LessThan {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// A LessThanOrEqual expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Bool.
    LessThanOrEqual {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// An ArrayLength expression.
    ///
    /// Must hold an Array expression.
    /// Returns an Int.
    ArrayLength { array: Box<PureExpr>, id: ExprId },

    /// An ArrayIsEmpty expression.
    ///
    /// Must hold an Array expression.
    /// Returns a Bool.
    ArrayIsEmpty { array: Box<PureExpr>, id: ExprId },

    /// A StringIsEmpty expression.
    ///
    /// Must hold a String expression.
    /// Returns a Bool.
    StringIsEmpty { string: Box<PureExpr>, id: ExprId },

    /// An OptionIsSome expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Bool.
    OptionIsSome { option: Box<PureExpr>, id: ExprId },

    /// An OptionIsNone expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Bool.
    OptionIsNone { option: Box<PureExpr>, id: ExprId },

    /// An IntToString expression.
    ///
    /// Must hold an Int.
    /// Returns a String.
    IntToString { value: Box<PureExpr>, id: ExprId },

    /// A FloatToInt expression.
    ///
    /// Saturates at the i32 bounds and maps NaN -> 0.
    ///
    /// Must hold a Float.
    /// Returns an Int.
    FloatToInt { value: Box<PureExpr>, id: ExprId },

    /// An IntToFloat expression.
    ///
    /// Must hold an Int.
    /// Returns a Float.
    IntToFloat { value: Box<PureExpr>, id: ExprId },
}

impl PureExpr {
    /// The type of this expression.
    #[cfg(test)]
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

            PureExpr::FloatLiteral { .. } | PureExpr::IntToFloat { .. } => Type::Float,
            PureExpr::IntLiteral { .. } => Type::Int,

            PureExpr::HtmlRaw { .. }
            | PureExpr::HtmlEscape { .. }
            | PureExpr::HtmlConcat { .. }
            | PureExpr::HtmlFor { .. } => Type::Html,

            PureExpr::StringConcat { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::IntToString { .. } => Type::String,

            PureExpr::NumericAdd { operand_types, .. }
            | PureExpr::NumericSubtract { operand_types, .. }
            | PureExpr::NumericMultiply { operand_types, .. }
            | PureExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Type::Int,
                NumericType::Float => Type::Float,
            },

            PureExpr::BoolLiteral { .. }
            | PureExpr::BoolNegation { .. }
            | PureExpr::Equals { .. }
            | PureExpr::LessThan { .. }
            | PureExpr::LessThanOrEqual { .. }
            | PureExpr::BoolLogicalAnd { .. }
            | PureExpr::BoolLogicalOr { .. }
            | PureExpr::ArrayIsEmpty { .. }
            | PureExpr::StringIsEmpty { .. }
            | PureExpr::OptionIsSome { .. }
            | PureExpr::OptionIsNone { .. } => Type::Bool,

            PureExpr::ArrayLength { .. } | PureExpr::FloatToInt { .. } => Type::Int,
        }
    }

    /// The ExprId this expression carries, mutably.
    pub fn id_mut(&mut self) -> &mut ExprId {
        match self {
            PureExpr::Let { id, .. }
            | PureExpr::Match { id, .. }
            | PureExpr::VariableReference { id, .. }
            | PureExpr::FieldAccess { id, .. }
            | PureExpr::StringLiteral { id, .. }
            | PureExpr::HtmlRaw { id, .. }
            | PureExpr::HtmlEscape { id, .. }
            | PureExpr::HtmlConcat { id, .. }
            | PureExpr::HtmlFor { id, .. }
            | PureExpr::Call { id, .. }
            | PureExpr::BoolLiteral { id, .. }
            | PureExpr::FloatLiteral { id, .. }
            | PureExpr::IntLiteral { id, .. }
            | PureExpr::Array { id, .. }
            | PureExpr::Tuple { id, .. }
            | PureExpr::TupleIndex { id, .. }
            | PureExpr::Record { id, .. }
            | PureExpr::Enum { id, .. }
            | PureExpr::Option { id, .. }
            | PureExpr::StringConcat { id, .. }
            | PureExpr::NumericAdd { id, .. }
            | PureExpr::NumericSubtract { id, .. }
            | PureExpr::NumericMultiply { id, .. }
            | PureExpr::NumericNegation { id, .. }
            | PureExpr::BoolNegation { id, .. }
            | PureExpr::BoolLogicalAnd { id, .. }
            | PureExpr::BoolLogicalOr { id, .. }
            | PureExpr::Equals { id, .. }
            | PureExpr::LessThan { id, .. }
            | PureExpr::LessThanOrEqual { id, .. }
            | PureExpr::ArrayLength { id, .. }
            | PureExpr::ArrayIsEmpty { id, .. }
            | PureExpr::StringIsEmpty { id, .. }
            | PureExpr::OptionIsSome { id, .. }
            | PureExpr::OptionIsNone { id, .. }
            | PureExpr::IntToString { id, .. }
            | PureExpr::FloatToInt { id, .. }
            | PureExpr::IntToFloat { id, .. } => id,
        }
    }

    /// Apply `f` to each direct child expression, without rebuilding.
    ///
    /// The read-only counterpart to `map_children`, and it treats binding
    /// structure the same way: binders are not distinguished from any other
    /// child, so a visitor that cares about scope must intercept `Let`,
    /// `Match` and `HtmlFor` before falling through to this.
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

            PureExpr::HtmlConcat { parts, .. } | PureExpr::StringConcat { parts, .. } => {
                for part in parts {
                    f(part);
                }
            }

            PureExpr::Call { args, .. } => {
                for arg in args {
                    f(&arg.expr);
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

            PureExpr::NumericNegation { operand, .. } | PureExpr::BoolNegation { operand, .. } => {
                f(operand);
            }

            PureExpr::NumericAdd { left, right, .. }
            | PureExpr::NumericSubtract { left, right, .. }
            | PureExpr::NumericMultiply { left, right, .. }
            | PureExpr::BoolLogicalAnd { left, right, .. }
            | PureExpr::BoolLogicalOr { left, right, .. }
            | PureExpr::Equals { left, right, .. }
            | PureExpr::LessThan { left, right, .. }
            | PureExpr::LessThanOrEqual { left, right, .. } => {
                f(left);
                f(right);
            }

            PureExpr::ArrayLength { array, .. } | PureExpr::ArrayIsEmpty { array, .. } => f(array),

            PureExpr::StringIsEmpty { string, .. } => f(string),

            PureExpr::OptionIsSome { option, .. } | PureExpr::OptionIsNone { option, .. } => {
                f(option);
            }

            PureExpr::IntToString { value, .. }
            | PureExpr::FloatToInt { value, .. }
            | PureExpr::IntToFloat { value, .. } => f(value),

            PureExpr::VariableReference { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::HtmlRaw { .. }
            | PureExpr::BoolLiteral { .. }
            | PureExpr::FloatLiteral { .. }
            | PureExpr::IntLiteral { .. } => {}
        }
    }

    /// Rebuild this expression with `f` applied to each direct child
    /// expression. Does not recurse: passes drive their own recursion,
    /// typically via a catch-all arm `expr => expr.map_children(...)` for
    /// the variants they need no special handling for.
    ///
    /// Binding structure gets no special treatment: the children of `Let`,
    /// `Match` and `HtmlFor` are mapped like any others, so a pass that
    /// cares about binders or variable references must intercept those
    /// variants before falling through to this.
    pub fn map_children(self, f: &mut impl FnMut(PureExpr) -> PureExpr) -> PureExpr {
        match self {
            PureExpr::Let {
                var,
                value,
                body,
                typ,
                id,
            } => PureExpr::Let {
                var,
                value: Box::new(f(*value)),
                body: Box::new(f(*body)),
                typ,
                id,
            },

            PureExpr::Match { match_, typ, id } => {
                let match_ = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: Box::new(f(*subject)),
                        true_body: Box::new(f(*true_body)),
                        false_body: Box::new(f(*false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => Match::Option {
                        subject: Box::new(f(*subject)),
                        some_arm_binding,
                        some_arm_body: Box::new(f(*some_arm_body)),
                        none_arm_body: Box::new(f(*none_arm_body)),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: Box::new(f(*subject)),
                        arms: arms
                            .into_iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern,
                                bindings: arm.bindings,
                                body: f(arm.body),
                            })
                            .collect(),
                    },
                };
                PureExpr::Match { match_, typ, id }
            }

            PureExpr::HtmlFor {
                var,
                source,
                body,
                id,
            } => PureExpr::HtmlFor {
                var,
                source: Box::new(match *source {
                    PureForSource::Array(array) => PureForSource::Array(f(array)),
                    PureForSource::RangeInclusive { start, end } => PureForSource::RangeInclusive {
                        start: f(start),
                        end: f(end),
                    },
                }),
                body: Box::new(f(*body)),
                id,
            },

            PureExpr::FieldAccess {
                record,
                field,
                typ,
                id,
            } => PureExpr::FieldAccess {
                record: Box::new(f(*record)),
                field,
                typ,
                id,
            },

            PureExpr::HtmlEscape { expr, id } => PureExpr::HtmlEscape {
                expr: Box::new(f(*expr)),
                id,
            },

            PureExpr::HtmlConcat { parts, id } => PureExpr::HtmlConcat {
                parts: parts.into_iter().map(&mut *f).collect(),
                id,
            },

            PureExpr::Call {
                function,
                args,
                typ,
                id,
            } => PureExpr::Call {
                function,
                args: args
                    .into_iter()
                    .map(|arg| PureArgument {
                        name: arg.name,
                        expr: f(arg.expr),
                    })
                    .collect(),
                typ,
                id,
            },

            PureExpr::Array { elements, typ, id } => PureExpr::Array {
                elements: elements.into_iter().map(&mut *f).collect(),
                typ,
                id,
            },

            PureExpr::Tuple { elements, typ, id } => PureExpr::Tuple {
                elements: elements.into_iter().map(&mut *f).collect(),
                typ,
                id,
            },

            PureExpr::TupleIndex {
                tuple,
                index,
                typ,
                id,
            } => PureExpr::TupleIndex {
                tuple: Box::new(f(*tuple)),
                index,
                typ,
                id,
            },

            PureExpr::Record {
                type_name,
                fields,
                typ,
                id,
            } => PureExpr::Record {
                type_name,
                fields: fields
                    .into_iter()
                    .map(|(name, value)| (name, f(value)))
                    .collect(),
                typ,
                id,
            },

            PureExpr::Enum {
                type_name,
                variant_name,
                fields,
                typ,
                id,
            } => PureExpr::Enum {
                type_name,
                variant_name,
                fields: fields
                    .into_iter()
                    .map(|(name, value)| (name, f(value)))
                    .collect(),
                typ,
                id,
            },

            PureExpr::Option { value, typ, id } => PureExpr::Option {
                value: value.map(|v| Box::new(f(*v))),
                typ,
                id,
            },

            PureExpr::StringConcat { parts, id } => PureExpr::StringConcat {
                parts: parts.into_iter().map(&mut *f).collect(),
                id,
            },

            PureExpr::NumericAdd {
                left,
                right,
                operand_types,
                id,
            } => PureExpr::NumericAdd {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                operand_types,
                id,
            },

            PureExpr::NumericSubtract {
                left,
                right,
                operand_types,
                id,
            } => PureExpr::NumericSubtract {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                operand_types,
                id,
            },

            PureExpr::NumericMultiply {
                left,
                right,
                operand_types,
                id,
            } => PureExpr::NumericMultiply {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                operand_types,
                id,
            },

            PureExpr::NumericNegation {
                operand,
                operand_type,
                id,
            } => PureExpr::NumericNegation {
                operand: Box::new(f(*operand)),
                operand_type,
                id,
            },

            PureExpr::BoolNegation { operand, id } => PureExpr::BoolNegation {
                operand: Box::new(f(*operand)),
                id,
            },

            PureExpr::BoolLogicalAnd { left, right, id } => PureExpr::BoolLogicalAnd {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                id,
            },

            PureExpr::BoolLogicalOr { left, right, id } => PureExpr::BoolLogicalOr {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                id,
            },

            PureExpr::Equals {
                left,
                right,
                operand_types,
                id,
            } => PureExpr::Equals {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                operand_types,
                id,
            },

            PureExpr::LessThan {
                left,
                right,
                operand_types,
                id,
            } => PureExpr::LessThan {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                operand_types,
                id,
            },

            PureExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                id,
            } => PureExpr::LessThanOrEqual {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                operand_types,
                id,
            },

            PureExpr::ArrayLength { array, id } => PureExpr::ArrayLength {
                array: Box::new(f(*array)),
                id,
            },

            PureExpr::ArrayIsEmpty { array, id } => PureExpr::ArrayIsEmpty {
                array: Box::new(f(*array)),
                id,
            },

            PureExpr::StringIsEmpty { string, id } => PureExpr::StringIsEmpty {
                string: Box::new(f(*string)),
                id,
            },

            PureExpr::OptionIsSome { option, id } => PureExpr::OptionIsSome {
                option: Box::new(f(*option)),
                id,
            },

            PureExpr::OptionIsNone { option, id } => PureExpr::OptionIsNone {
                option: Box::new(f(*option)),
                id,
            },

            PureExpr::IntToString { value, id } => PureExpr::IntToString {
                value: Box::new(f(*value)),
                id,
            },

            PureExpr::FloatToInt { value, id } => PureExpr::FloatToInt {
                value: Box::new(f(*value)),
                id,
            },

            PureExpr::IntToFloat { value, id } => PureExpr::IntToFloat {
                value: Box::new(f(*value)),
                id,
            },

            PureExpr::VariableReference { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::HtmlRaw { .. }
            | PureExpr::BoolLiteral { .. }
            | PureExpr::FloatLiteral { .. }
            | PureExpr::IntLiteral { .. } => self,
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

fn params_to_doc(parameters: &[WriterParameter]) -> BoxDoc<'_> {
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
            PureExpr::HtmlRaw { content, .. } => BoxDoc::text("raw(")
                .append(BoxDoc::text(format!("{:?}", content)))
                .append(")"),
            PureExpr::HtmlEscape { expr, .. } => {
                BoxDoc::text("escape(").append(expr.to_doc()).append(")")
            }
            PureExpr::HtmlConcat { parts, .. } => {
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
                    Some(name) => BoxDoc::text(name.to_string()),
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
                        args.iter().map(|arg| {
                            BoxDoc::text(arg.name.as_str())
                                .append(BoxDoc::text(" = "))
                                .append(arg.expr.to_doc())
                        }),
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
            PureExpr::StringConcat { parts, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::intersperse(
                    parts.iter().map(|part| part.to_doc()),
                    BoxDoc::text(" + "),
                ))
                .append(BoxDoc::text(")")),
            PureExpr::NumericAdd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::NumericSubtract { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" - "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::NumericMultiply { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" * "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::NumericNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("-"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::BoolNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
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
            PureExpr::Equals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" == "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::LessThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" < "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::LessThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" <= "))
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
            PureExpr::Match { match_, .. } => {
                fn arm_to_doc<'a>(pattern: BoxDoc<'a>, body: &'a PureExpr) -> BoxDoc<'a> {
                    pattern
                        .append(BoxDoc::text(" => {"))
                        .append(BoxDoc::line().append(body.to_doc()).nest(2))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}"))
                        .group()
                }

                fn match_to_doc<'a>(subject: &'a PureExpr, arms: Vec<BoxDoc<'a>>) -> BoxDoc<'a> {
                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(arms, BoxDoc::line()))
                                .nest(2),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}"))
                        .group()
                }

                match match_ {
                    Match::Enum { subject, arms } => {
                        if arms.is_empty() {
                            BoxDoc::text("match ")
                                .append(subject.to_doc())
                                .append(BoxDoc::text(" {}"))
                        } else {
                            let arm_docs = arms
                                .iter()
                                .map(|arm| {
                                    let pattern_doc = match &arm.pattern {
                                        EnumPattern::Variant {
                                            type_name,
                                            variant_name,
                                        } => {
                                            let base = BoxDoc::text(type_name.as_str())
                                                .append(BoxDoc::text("::"))
                                                .append(BoxDoc::text(variant_name.as_str()));
                                            if arm.bindings.is_empty() {
                                                base
                                            } else {
                                                let bindings_str: Vec<String> = arm
                                                    .bindings
                                                    .iter()
                                                    .map(|(field, var)| {
                                                        format!("{}: {}", field, var)
                                                    })
                                                    .collect();
                                                base.append(BoxDoc::text(" {"))
                                                    .append(BoxDoc::text(bindings_str.join(", ")))
                                                    .append(BoxDoc::text("}"))
                                            }
                                        }
                                    };
                                    arm_to_doc(pattern_doc, &arm.body)
                                })
                                .collect();
                            match_to_doc(subject, arm_docs)
                        }
                    }
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => match_to_doc(
                        subject,
                        vec![
                            arm_to_doc(BoxDoc::text("true"), true_body),
                            arm_to_doc(BoxDoc::text("false"), false_body),
                        ],
                    ),
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let some_pattern_doc = match some_arm_binding {
                            Some(name) => BoxDoc::text("Some(")
                                .append(BoxDoc::text(name.to_string()))
                                .append(BoxDoc::text(")")),
                            None => BoxDoc::text("Some(_)"),
                        };
                        match_to_doc(
                            subject,
                            vec![
                                arm_to_doc(some_pattern_doc, some_arm_body),
                                arm_to_doc(BoxDoc::text("None"), none_arm_body),
                            ],
                        )
                    }
                }
            }
            PureExpr::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.to_string()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in {"))
                .append(BoxDoc::line().append(body.to_doc()).nest(2))
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .group(),
            PureExpr::ArrayLength { array, .. } => array.to_doc().append(BoxDoc::text(".len()")),
            PureExpr::ArrayIsEmpty { array, .. } => {
                array.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            PureExpr::StringIsEmpty { string, .. } => {
                string.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            PureExpr::OptionIsSome { option, .. } => {
                option.to_doc().append(BoxDoc::text(".is_some()"))
            }
            PureExpr::OptionIsNone { option, .. } => {
                option.to_doc().append(BoxDoc::text(".is_none()"))
            }
            PureExpr::IntToString { value, .. } => {
                value.to_doc().append(BoxDoc::text(".to_string()"))
            }
            PureExpr::FloatToInt { value, .. } => value.to_doc().append(BoxDoc::text(".to_int()")),
            PureExpr::IntToFloat { value, .. } => {
                value.to_doc().append(BoxDoc::text(".to_float()"))
            }
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
