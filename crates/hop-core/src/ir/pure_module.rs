use std::fmt;
use std::sync::Arc;

use crate::document::CheapString;
use crate::expr::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::expr::typing::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::ir::expr_id::{ExprId, ExprIdCounter};
use crate::ir::ir_var::IrVar;
use crate::ir::var_id::VarIdCounter;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

use super::writer_module::{WriterEnumDeclaration, WriterParameter, WriterRecordDeclaration};

/// A Pure module.
///
/// An expression-only, side-effect-free form of the IR.
#[derive(Debug)]
pub struct PureModule {
    pub pages: Vec<PurePageDeclaration>,
    pub functions: Vec<PureFunctionDeclaration>,
    pub records: Vec<WriterRecordDeclaration>,
    pub enums: Vec<WriterEnumDeclaration>,
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
    /// PureIR expression for the assembled page body. Must be of type `Fragment`.
    pub body: PureExpr,
}

/// A function declaration in Pure.
#[derive(Debug)]
pub struct PureFunctionDeclaration {
    /// Function name
    pub name: FunctionName,
    /// Parameter names with their types
    pub parameters: Vec<WriterParameter>,
    /// The function's return type. The body must be of this type.
    pub return_type: Arc<Type>,
    /// PureIR expression for the function body. Must be of type `return_type`.
    pub body: PureExpr,
}

/// The source of iteration in a FragmentFor.
#[derive(Debug, Clone, PartialEq)]
pub enum PureForSource {
    /// Iterate over elements of an array.
    Array(PureExpr),
    /// Iterate over an inclusive integer range.
    RangeInclusive { start: PureExpr, end: PureExpr },
}

/// An argument passed to a FunctionCall.
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
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A Match expression over an Enum, Boolean, or Option.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        match_: Match<PureExpr, PureExpr, IrVar>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A VariableReference expression.
    ///
    /// Reads the value bound by its binder.
    ///
    /// The kind field must match the binder's type.
    VariableReference {
        value: IrVar,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A FieldAccess expression.
    ///
    /// The expression must evaluate to a record and the field must exist on
    /// the record.
    FieldAccess {
        record: Box<PureExpr>,
        field: FieldName,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A StringLiteral expression.
    StringLiteral { value: CheapString, id: ExprId },

    /// A FragmentRaw expression.
    ///
    /// A trusted, already-escaped HTML atom.
    FragmentRaw { content: String, id: ExprId },

    /// A FragmentEscape expression.
    ///
    /// HTML-escapes a String-typed expression into a Fragment.
    ///
    /// Must hold a String.
    FragmentEscape { expr: Box<PureExpr>, id: ExprId },

    /// A FragmentConcat expression.
    ///
    /// N-ary mappend over Fragment-typed parts.
    ///
    /// Part order is output order.
    ///
    /// Every part must be Fragment-typed.
    FragmentConcat { parts: Vec<PureExpr>, id: ExprId },

    /// A FragmentFor expression.
    ///
    /// A foldMap over source, concatenating body once per element in iteration order.
    ///
    /// When var is None, the loop binds no variable, but still iterates.
    ///
    /// The type of body must be Fragment.
    FragmentFor {
        var: Option<IrVar>,
        source: Box<PureForSource>,
        body: Box<PureExpr>,
        id: ExprId,
    },

    /// A FunctionCall expression.
    ///
    /// Invokes a function and produces its result.
    FunctionCall {
        function_name: FunctionName,
        args: Vec<PureArgument>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A BooleanLiteral expression.
    BooleanLiteral { value: bool, id: ExprId },

    /// A FloatLiteral expression.
    FloatLiteral { value: f64, id: ExprId },

    /// An IntLiteral expression.
    IntLiteral { value: i32, id: ExprId },

    /// An ArrayLiteral expression.
    ArrayLiteral {
        elements: Vec<PureExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A RecordLiteral expression.
    RecordLiteral {
        record_name: TypeName,
        fields: Vec<(FieldName, PureExpr)>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An EnumLiteral expression.
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, PureExpr)>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An OptionLiteral expression.
    OptionLiteral {
        value: Option<Box<PureExpr>>,
        kind: Arc<Type>,
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

    /// A BooleanNegation expression.
    ///
    /// Must hold a Boolean expression.
    /// Returns a Boolean.
    BooleanNegation { operand: Box<PureExpr>, id: ExprId },

    /// A BooleanLogicalAnd expression.
    ///
    /// Must hold two Boolean expressions.
    /// Returns a Boolean.
    BooleanLogicalAnd {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        id: ExprId,
    },

    /// A BooleanLogicalOr expression.
    ///
    /// Must hold two Boolean expressions.
    /// Returns a Boolean.
    BooleanLogicalOr {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        id: ExprId,
    },

    /// An Equals expression.
    ///
    /// Must hold two values of the same EquatableType.
    /// Returns a Boolean.
    Equals {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: EquatableType,
        id: ExprId,
    },

    /// A LessThan expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Boolean.
    LessThan {
        left: Box<PureExpr>,
        right: Box<PureExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// A LessThanOrEqual expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Boolean.
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
    /// Returns a Boolean.
    ArrayIsEmpty { array: Box<PureExpr>, id: ExprId },

    /// A StringIsEmpty expression.
    ///
    /// Must hold a String expression.
    /// Returns a Boolean.
    StringIsEmpty { string: Box<PureExpr>, id: ExprId },

    /// An OptionIsSome expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Boolean.
    OptionIsSome { option: Box<PureExpr>, id: ExprId },

    /// An OptionIsNone expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Boolean.
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
    /// Get the type of this expression as an Arc
    #[cfg(test)]
    pub fn get_type(&self) -> Arc<Type> {
        match self {
            PureExpr::VariableReference { kind, .. }
            | PureExpr::FieldAccess { kind, .. }
            | PureExpr::ArrayLiteral { kind, .. }
            | PureExpr::RecordLiteral { kind, .. }
            | PureExpr::EnumLiteral { kind, .. }
            | PureExpr::OptionLiteral { kind, .. }
            | PureExpr::Match { kind, .. }
            | PureExpr::Let { kind, .. }
            | PureExpr::FunctionCall { kind, .. } => kind.clone(),

            PureExpr::FloatLiteral { .. } | PureExpr::IntToFloat { .. } => Arc::new(Type::Float),
            PureExpr::IntLiteral { .. } => Arc::new(Type::Int),

            PureExpr::FragmentRaw { .. }
            | PureExpr::FragmentEscape { .. }
            | PureExpr::FragmentConcat { .. }
            | PureExpr::FragmentFor { .. } => Arc::new(Type::Fragment),

            PureExpr::StringConcat { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::IntToString { .. } => Arc::new(Type::String),

            PureExpr::NumericAdd { operand_types, .. }
            | PureExpr::NumericSubtract { operand_types, .. }
            | PureExpr::NumericMultiply { operand_types, .. }
            | PureExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Arc::new(Type::Int),
                NumericType::Float => Arc::new(Type::Float),
            },

            PureExpr::BooleanLiteral { .. }
            | PureExpr::BooleanNegation { .. }
            | PureExpr::Equals { .. }
            | PureExpr::LessThan { .. }
            | PureExpr::LessThanOrEqual { .. }
            | PureExpr::BooleanLogicalAnd { .. }
            | PureExpr::BooleanLogicalOr { .. }
            | PureExpr::ArrayIsEmpty { .. }
            | PureExpr::StringIsEmpty { .. }
            | PureExpr::OptionIsSome { .. }
            | PureExpr::OptionIsNone { .. } => Arc::new(Type::Bool),

            PureExpr::ArrayLength { .. } | PureExpr::FloatToInt { .. } => Arc::new(Type::Int),
        }
    }

    /// Get the type of this expression
    #[cfg(test)]
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;
        static FRAGMENT_TYPE: Type = Type::Fragment;

        match self {
            PureExpr::VariableReference { kind, .. }
            | PureExpr::FieldAccess { kind, .. }
            | PureExpr::ArrayLiteral { kind, .. }
            | PureExpr::RecordLiteral { kind, .. }
            | PureExpr::EnumLiteral { kind, .. }
            | PureExpr::OptionLiteral { kind, .. }
            | PureExpr::Match { kind, .. }
            | PureExpr::Let { kind, .. }
            | PureExpr::FunctionCall { kind, .. } => kind,

            PureExpr::FloatLiteral { .. } | PureExpr::IntToFloat { .. } => &FLOAT_TYPE,
            PureExpr::IntLiteral { .. } => &INT_TYPE,

            PureExpr::FragmentRaw { .. }
            | PureExpr::FragmentEscape { .. }
            | PureExpr::FragmentConcat { .. }
            | PureExpr::FragmentFor { .. } => &FRAGMENT_TYPE,

            PureExpr::StringConcat { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::IntToString { .. } => &STRING_TYPE,

            PureExpr::NumericAdd { operand_types, .. }
            | PureExpr::NumericSubtract { operand_types, .. }
            | PureExpr::NumericMultiply { operand_types, .. }
            | PureExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            PureExpr::BooleanLiteral { .. }
            | PureExpr::BooleanNegation { .. }
            | PureExpr::Equals { .. }
            | PureExpr::LessThan { .. }
            | PureExpr::LessThanOrEqual { .. }
            | PureExpr::BooleanLogicalAnd { .. }
            | PureExpr::BooleanLogicalOr { .. }
            | PureExpr::ArrayIsEmpty { .. }
            | PureExpr::StringIsEmpty { .. }
            | PureExpr::OptionIsSome { .. }
            | PureExpr::OptionIsNone { .. } => &BOOL_TYPE,

            PureExpr::ArrayLength { .. } | PureExpr::FloatToInt { .. } => &INT_TYPE,
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
            | PureExpr::FragmentRaw { id, .. }
            | PureExpr::FragmentEscape { id, .. }
            | PureExpr::FragmentConcat { id, .. }
            | PureExpr::FragmentFor { id, .. }
            | PureExpr::FunctionCall { id, .. }
            | PureExpr::BooleanLiteral { id, .. }
            | PureExpr::FloatLiteral { id, .. }
            | PureExpr::IntLiteral { id, .. }
            | PureExpr::ArrayLiteral { id, .. }
            | PureExpr::RecordLiteral { id, .. }
            | PureExpr::EnumLiteral { id, .. }
            | PureExpr::OptionLiteral { id, .. }
            | PureExpr::StringConcat { id, .. }
            | PureExpr::NumericAdd { id, .. }
            | PureExpr::NumericSubtract { id, .. }
            | PureExpr::NumericMultiply { id, .. }
            | PureExpr::NumericNegation { id, .. }
            | PureExpr::BooleanNegation { id, .. }
            | PureExpr::BooleanLogicalAnd { id, .. }
            | PureExpr::BooleanLogicalOr { id, .. }
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
    /// `Match` and `FragmentFor` before falling through to this.
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

            PureExpr::FragmentFor { source, body, .. } => {
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

            PureExpr::FragmentEscape { expr, .. } => f(expr),

            PureExpr::FragmentConcat { parts, .. } | PureExpr::StringConcat { parts, .. } => {
                for part in parts {
                    f(part);
                }
            }

            PureExpr::FunctionCall { args, .. } => {
                for arg in args {
                    f(&arg.expr);
                }
            }

            PureExpr::ArrayLiteral { elements, .. } => {
                for element in elements {
                    f(element);
                }
            }

            PureExpr::RecordLiteral { fields, .. } | PureExpr::EnumLiteral { fields, .. } => {
                for (_, value) in fields {
                    f(value);
                }
            }

            PureExpr::OptionLiteral { value, .. } => {
                if let Some(value) = value {
                    f(value);
                }
            }

            PureExpr::NumericNegation { operand, .. }
            | PureExpr::BooleanNegation { operand, .. } => f(operand),

            PureExpr::NumericAdd { left, right, .. }
            | PureExpr::NumericSubtract { left, right, .. }
            | PureExpr::NumericMultiply { left, right, .. }
            | PureExpr::BooleanLogicalAnd { left, right, .. }
            | PureExpr::BooleanLogicalOr { left, right, .. }
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
            | PureExpr::FragmentRaw { .. }
            | PureExpr::BooleanLiteral { .. }
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
    /// `Match` and `FragmentFor` are mapped like any others, so a pass that
    /// cares about binders or variable references must intercept those
    /// variants before falling through to this.
    pub fn map_children(self, f: &mut impl FnMut(PureExpr) -> PureExpr) -> PureExpr {
        match self {
            PureExpr::Let {
                var,
                value,
                body,
                kind,
                id,
            } => PureExpr::Let {
                var,
                value: Box::new(f(*value)),
                body: Box::new(f(*body)),
                kind,
                id,
            },

            PureExpr::Match { match_, kind, id } => {
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
                PureExpr::Match { match_, kind, id }
            }

            PureExpr::FragmentFor {
                var,
                source,
                body,
                id,
            } => PureExpr::FragmentFor {
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
                kind,
                id,
            } => PureExpr::FieldAccess {
                record: Box::new(f(*record)),
                field,
                kind,
                id,
            },

            PureExpr::FragmentEscape { expr, id } => PureExpr::FragmentEscape {
                expr: Box::new(f(*expr)),
                id,
            },

            PureExpr::FragmentConcat { parts, id } => PureExpr::FragmentConcat {
                parts: parts.into_iter().map(&mut *f).collect(),
                id,
            },

            PureExpr::FunctionCall {
                function_name,
                args,
                kind,
                id,
            } => PureExpr::FunctionCall {
                function_name,
                args: args
                    .into_iter()
                    .map(|arg| PureArgument {
                        name: arg.name,
                        expr: f(arg.expr),
                    })
                    .collect(),
                kind,
                id,
            },

            PureExpr::ArrayLiteral { elements, kind, id } => PureExpr::ArrayLiteral {
                elements: elements.into_iter().map(&mut *f).collect(),
                kind,
                id,
            },

            PureExpr::RecordLiteral {
                record_name,
                fields,
                kind,
                id,
            } => PureExpr::RecordLiteral {
                record_name,
                fields: fields
                    .into_iter()
                    .map(|(name, value)| (name, f(value)))
                    .collect(),
                kind,
                id,
            },

            PureExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                kind,
                id,
            } => PureExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields: fields
                    .into_iter()
                    .map(|(name, value)| (name, f(value)))
                    .collect(),
                kind,
                id,
            },

            PureExpr::OptionLiteral { value, kind, id } => PureExpr::OptionLiteral {
                value: value.map(|v| Box::new(f(*v))),
                kind,
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

            PureExpr::BooleanNegation { operand, id } => PureExpr::BooleanNegation {
                operand: Box::new(f(*operand)),
                id,
            },

            PureExpr::BooleanLogicalAnd { left, right, id } => PureExpr::BooleanLogicalAnd {
                left: Box::new(f(*left)),
                right: Box::new(f(*right)),
                id,
            },

            PureExpr::BooleanLogicalOr { left, right, id } => PureExpr::BooleanLogicalOr {
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
            | PureExpr::FragmentRaw { .. }
            | PureExpr::BooleanLiteral { .. }
            | PureExpr::FloatLiteral { .. }
            | PureExpr::IntLiteral { .. } => self,
        }
    }
}

impl PurePageDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::nil()
            .append("page ")
            .append(self.name.as_str())
            .append(BoxDoc::text("("))
            .append(params_to_doc(&self.parameters))
            .append(BoxDoc::text(") {"))
            .append(BoxDoc::line().append(self.body.to_doc()).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

impl PureFunctionDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("fn ")
            .append(BoxDoc::text(self.name.as_str()))
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
            PureExpr::FragmentRaw { content, .. } => BoxDoc::text("raw(")
                .append(BoxDoc::text(format!("{:?}", content)))
                .append(")"),
            PureExpr::FragmentEscape { expr, .. } => {
                BoxDoc::text("escape(").append(expr.to_doc()).append(")")
            }
            PureExpr::FragmentConcat { parts, .. } => {
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
            PureExpr::FragmentFor {
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
                body.to_doc()
                    .append(BoxDoc::text(" for "))
                    .append(var_doc)
                    .append(BoxDoc::text(" in "))
                    .append(source_doc)
            }
            PureExpr::FunctionCall {
                function_name,
                args,
                ..
            } => {
                let mut doc = BoxDoc::text("call ")
                    .append(BoxDoc::text(function_name.as_str()))
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
            PureExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            PureExpr::ArrayLiteral { elements, .. } => {
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
            PureExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => {
                if fields.is_empty() {
                    BoxDoc::text(record_name.as_str()).append(BoxDoc::text(" {}"))
                } else {
                    BoxDoc::text(record_name.as_str())
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
            PureExpr::BooleanNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::BooleanLogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            PureExpr::BooleanLogicalOr { left, right, .. } => BoxDoc::nil()
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
            PureExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                ..
            } => {
                let base = BoxDoc::text(enum_name.as_str())
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
            PureExpr::OptionLiteral { value, kind, .. } => {
                let inner_type = match kind.as_ref() {
                    Type::Option(inner) => inner.to_doc(),
                    _ => panic!("OptionLiteral must have Option type, got {:?}", kind),
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
            PureExpr::Match { match_, .. } => match match_ {
                Match::Enum { subject, arms } => {
                    if arms.is_empty() {
                        BoxDoc::text("match ")
                            .append(subject.to_doc())
                            .append(BoxDoc::text(" {}"))
                    } else {
                        BoxDoc::text("match ")
                            .append(subject.to_doc())
                            .append(BoxDoc::text(" {"))
                            .append(
                                BoxDoc::line_()
                                    .append(BoxDoc::intersperse(
                                        arms.iter().map(|arm| {
                                            let pattern_doc = match &arm.pattern {
                                                EnumPattern::Variant {
                                                    enum_name,
                                                    variant_name,
                                                } => {
                                                    let base = BoxDoc::text(enum_name.as_str())
                                                        .append(BoxDoc::text("::"))
                                                        .append(BoxDoc::text(
                                                            variant_name.as_str(),
                                                        ));
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
                                                            .append(BoxDoc::text(
                                                                bindings_str.join(", "),
                                                            ))
                                                            .append(BoxDoc::text("}"))
                                                    }
                                                }
                                            };
                                            pattern_doc
                                                .append(BoxDoc::text(" => "))
                                                .append(arm.body.to_doc())
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
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => {
                    let true_arm_doc = BoxDoc::text("true")
                        .append(BoxDoc::text(" => "))
                        .append(true_body.to_doc());
                    let false_arm_doc = BoxDoc::text("false")
                        .append(BoxDoc::text(" => "))
                        .append(false_body.to_doc());

                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    [true_arm_doc, false_arm_doc],
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
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
                    let some_arm_doc = some_pattern_doc
                        .append(BoxDoc::text(" => "))
                        .append(some_arm_body.to_doc());
                    let none_arm_doc = BoxDoc::text("None")
                        .append(BoxDoc::text(" => "))
                        .append(none_arm_body.to_doc());

                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    [some_arm_doc, none_arm_doc],
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
            },
            PureExpr::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.to_string()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in "))
                .append(body.to_doc()),
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
        for enum_decl in &self.enums {
            writeln!(f, "{}", enum_decl)?;
        }
        for record_decl in &self.records {
            writeln!(f, "{}", record_decl)?;
        }
        for function in &self.functions {
            write!(f, "{}", function)?;
        }
        for page in &self.pages {
            write!(f, "{}", page)?;
        }
        Ok(())
    }
}
