use std::fmt;
use std::sync::Arc;

use crate::document::CheapString;
use crate::expr::patterns::{EnumPattern, Match};
use crate::expr::typing::r#type::{
    ComparableType, EnumVariant, EquatableType, ExamplesAnnotation, NumericType, Type,
};
use crate::ir::ir_var::IrVar;
use crate::ir::var_id::VarIdCounter;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

/// A Writer module. The lowered, statement form of the IR, consumed by
/// the evaluator and the transpilers.
///
/// All IDs in the module are unique across the whole module. Each binder has
/// a unique VarId, so two binders are never the same variable: shadowing is
/// impossible and substitution is capture-free.
#[derive(Debug)]
pub struct WriterModule {
    pub views: Vec<WriterViewDeclaration>,
    pub components: Vec<WriterComponentDeclaration>,
    pub records: Vec<WriterRecordDeclaration>,
    pub enums: Vec<WriterEnumDeclaration>,
    pub var_ids: VarIdCounter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WriterParameter {
    pub name: VarName,
    pub var: IrVar,
    pub typ: Arc<Type>,
}

impl WriterParameter {
    pub fn name(&self) -> &VarName {
        &self.name
    }
}

/// An argument passed to a component invocation in the IR.
#[derive(Debug, PartialEq)]
pub struct WriterArgument {
    pub name: VarName,
    pub expr: WriterExpr,
}

/// The source of iteration in a For loop.
#[derive(Debug, PartialEq)]
pub enum WriterForSource {
    /// Iterate over elements of an array.
    Array(WriterExpr),
    /// Iterate over an inclusive integer range.
    RangeInclusive { start: WriterExpr, end: WriterExpr },
}

#[derive(Debug)]
pub struct WriterViewDeclaration {
    /// Entrypoint name
    pub name: TypeName,
    /// Parameter names with their types
    pub parameters: Vec<WriterParameter>,
    /// IR nodes for the view body
    pub body: Vec<WriterStatement>,
}

#[derive(Debug, Clone)]
pub struct WriterRecordDeclaration {
    pub name: TypeName,
    pub fields: Vec<(FieldName, Arc<Type>, Option<ExamplesAnnotation>)>,
}

#[derive(Debug, Clone)]
pub struct WriterEnumDeclaration {
    pub name: TypeName,
    pub variants: Vec<EnumVariant>,
}

/// A component declaration in the IR.
///
/// Invokable through the ComponentInvocation statement.
#[derive(Debug)]
pub struct WriterComponentDeclaration {
    /// Component name
    pub name: TypeName,
    /// Parameter names with their types
    pub parameters: Vec<WriterParameter>,
    /// IR nodes for the component body
    pub body: Vec<WriterStatement>,
}

/// A statement in the IR.
///
/// Statements may perform one kind of effect: writing to the output stream.
/// Statement order is output order.
#[derive(Debug, PartialEq)]
pub enum WriterStatement {
    /// Write a constant string to the output stream.
    ///
    /// Write performs no escaping.
    Write { content: String },

    /// Write a String expression to the output stream.
    ///
    /// WriteString performs HTML escaping.
    ///
    /// The type of expr must be String.
    WriteString { expr: WriterExpr },

    /// Write a Fragment expression to the output stream.
    ///
    /// WriteFragment performs no escaping, a Fragment is already-escaped HTML
    /// by construction.
    ///
    /// The type of expr must be Fragment.
    WriteFragment { expr: WriterExpr },

    /// Invoke a component and write its effects to the output stream.
    ComponentInvocation {
        component_name: TypeName,
        args: Vec<WriterArgument>,
    },

    /// Loop over an array or range.
    ///
    /// When var is None, the loop binds no variable, but the loop still
    /// executes.
    For {
        var: Option<IrVar>,
        source: WriterForSource,
        body: Vec<WriterStatement>,
    },

    /// Bind a variable to the value of an expression and execute the effects
    /// of the body.
    ///
    /// The binding scopes over body only, not the statements that follow.
    Let {
        var: IrVar,
        value: WriterExpr,
        body: Vec<WriterStatement>,
    },

    /// Match on a value and execute the effects of the matched branch.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        match_: Match<WriterExpr, Vec<WriterStatement>, IrVar>,
    },
}

/// IR expression type.
///
/// Expressions produce no side effects. The statements inside a FragmentLiteral
/// write into a fresh buffer, not the enclosing output stream.
///
/// The Int type is an i32 with wrapping add/sub/mul/neg.
///
/// A FloatToString conversion is avoided since semantics would be too tricky to
/// define across backends.
#[derive(Debug, PartialEq)]
pub enum WriterExpr {
    /// A Let expression.
    Let {
        var: IrVar,
        value: Box<WriterExpr>,
        body: Box<WriterExpr>,
        kind: Arc<Type>,
    },

    /// A Match expression over an Enum, Boolean, or Option.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        match_: Match<WriterExpr, WriterExpr, IrVar>,
        kind: Arc<Type>,
    },

    /// A VariableReference expression.
    ///
    /// Reads the value bound by its binder.
    ///
    /// The kind field must match the binder's type.
    VariableReference { value: IrVar, kind: Arc<Type> },

    /// A FieldAccess expression.
    ///
    /// The expression must evaluate to a record and the field must exist on the
    /// record.
    FieldAccess {
        record: Box<WriterExpr>,
        field: FieldName,
        kind: Arc<Type>,
    },

    /// A StringLiteral expression.
    StringLiteral { value: CheapString },

    /// A FragmentLiteral expression.
    ///
    /// Produced by rendering the body into a fresh buffer.
    FragmentLiteral { body: Vec<WriterStatement> },

    /// A BooleanLiteral expression.
    BooleanLiteral { value: bool },

    /// A FloatLiteral expression.
    FloatLiteral { value: f64 },

    /// An IntLiteral expression.
    IntLiteral { value: i32 },

    /// An ArrayLiteral expression.
    ArrayLiteral {
        elements: Vec<WriterExpr>,
        kind: Arc<Type>,
    },

    /// A RecordLiteral expression.
    RecordLiteral {
        record_name: TypeName,
        fields: Vec<(FieldName, WriterExpr)>,
        kind: Arc<Type>,
    },

    /// An EnumLiteral expression.
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, WriterExpr)>,
        kind: Arc<Type>,
    },

    /// An OptionLiteral expression.
    OptionLiteral {
        value: Option<Box<WriterExpr>>,
        kind: Arc<Type>,
    },

    /// A StringConcat expression.
    ///
    /// N-ary mappend over String-typed parts.
    StringConcat { parts: Vec<WriterExpr> },

    /// A TwMerge expression, applied at the class attribute boundary.
    ///
    /// Must hold an expression of type String.
    /// Returns a String.
    TwMerge { operand: Box<WriterExpr> },

    /// A NumericAdd expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericAdd {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
        operand_types: NumericType,
    },

    /// A NumericSubtract expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericSubtract {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
        operand_types: NumericType,
    },

    /// A NumericMultiply expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericMultiply {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
        operand_types: NumericType,
    },

    /// A NumericNegation expression.
    ///
    /// Must hold an expression of a NumericType.
    /// Returns the NumericType of the expression.
    NumericNegation {
        operand: Box<WriterExpr>,
        operand_type: NumericType,
    },

    /// A BooleanNegation expression.
    ///
    /// Must hold a Boolean expression.
    /// Returns a Boolean.
    BooleanNegation { operand: Box<WriterExpr> },

    /// A BooleanLogicalAnd expression.
    ///
    /// Must hold two Boolean expressions.
    /// Returns a Boolean.
    BooleanLogicalAnd {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
    },

    /// A BooleanLogicalOr expression.
    ///
    /// Must hold two Boolean expressions.
    /// Returns a Boolean.
    BooleanLogicalOr {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
    },

    /// An Equals expression.
    ///
    /// Must hold two values of the same EquatableType.
    /// Returns a Boolean.
    Equals {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
        operand_types: EquatableType,
    },

    /// A LessThan expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Boolean.
    LessThan {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
        operand_types: ComparableType,
    },

    /// A LessThanOrEqual expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Boolean.
    LessThanOrEqual {
        left: Box<WriterExpr>,
        right: Box<WriterExpr>,
        operand_types: ComparableType,
    },

    /// An ArrayLength expression.
    ///
    /// Must hold an Array expression.
    /// Returns an Int.
    ArrayLength { array: Box<WriterExpr> },

    /// An ArrayIsEmpty expression.
    ///
    /// Must hold an Array expression.
    /// Returns a Boolean.
    ArrayIsEmpty { array: Box<WriterExpr> },

    /// A StringIsEmpty expression.
    ///
    /// Must hold a String expression.
    /// Returns a Boolean.
    StringIsEmpty { string: Box<WriterExpr> },

    /// An OptionIsSome expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Boolean.
    OptionIsSome { option: Box<WriterExpr> },

    /// An OptionIsNone expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Boolean.
    OptionIsNone { option: Box<WriterExpr> },

    /// An IntToString expression.
    ///
    /// Must hold an Int.
    /// Returns a String.
    IntToString { value: Box<WriterExpr> },

    /// A FloatToInt expression.
    ///
    /// Saturates at the i32 bounds and maps NaN -> 0.
    ///
    /// Must hold a Float.
    /// Returns an Int.
    FloatToInt { value: Box<WriterExpr> },

    /// An IntToFloat expression.
    ///
    /// Must hold an Int.
    /// Returns a Float.
    IntToFloat { value: Box<WriterExpr> },
}

impl WriterStatement {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            WriterStatement::Write { content, .. } => BoxDoc::text("write")
                .append(BoxDoc::text("("))
                .append(BoxDoc::text(format!("{:?}", content)))
                .append(BoxDoc::text(")")),
            WriterStatement::WriteString { expr, .. } => BoxDoc::text("write_string")
                .append(BoxDoc::text("("))
                .append(expr.to_doc())
                .append(BoxDoc::text(")")),
            WriterStatement::WriteFragment { expr, .. } => BoxDoc::text("write_fragment")
                .append(BoxDoc::text("("))
                .append(expr.to_doc())
                .append(BoxDoc::text(")")),
            WriterStatement::For {
                var, source, body, ..
            } => {
                let source_doc = match source {
                    WriterForSource::Array(array) => array.to_doc(),
                    WriterForSource::RangeInclusive { start, end } => start
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
                    .append(if body.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                body.iter().map(|stmt| stmt.to_doc()),
                                BoxDoc::line(),
                            ))
                            .append(BoxDoc::line())
                            .nest(2)
                    })
                    .append(BoxDoc::text("}"))
            }
            WriterStatement::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.to_string()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in {"))
                .append(if body.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            body.iter().map(|stmt| stmt.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("}")),
            WriterStatement::Match { match_, .. } => {
                fn body_to_doc(body: &[WriterStatement]) -> BoxDoc<'_> {
                    if body.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                body.iter().map(|stmt| stmt.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2)
                    }
                }

                fn arm_to_doc<'a>(pattern: BoxDoc<'a>, body: &'a [WriterStatement]) -> BoxDoc<'a> {
                    pattern
                        .append(BoxDoc::text(" => {"))
                        .append(body_to_doc(body))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}"))
                }

                match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line()
                                .append(arm_to_doc(BoxDoc::text("true"), true_body))
                                .append(BoxDoc::line())
                                .append(arm_to_doc(BoxDoc::text("false"), false_body))
                                .nest(2),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}")),
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let some_pattern = match some_arm_binding {
                            Some(var) => format!("Some({var})"),
                            None => "Some(_)".to_string(),
                        };
                        BoxDoc::text("match ")
                            .append(subject.to_doc())
                            .append(BoxDoc::text(" {"))
                            .append(
                                BoxDoc::line()
                                    .append(arm_to_doc(
                                        BoxDoc::as_string(some_pattern),
                                        some_arm_body,
                                    ))
                                    .append(BoxDoc::line())
                                    .append(arm_to_doc(BoxDoc::text("None"), none_arm_body))
                                    .nest(2),
                            )
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("}"))
                    }
                    Match::Enum { subject, arms } => {
                        let arms_doc: Vec<_> = arms
                            .iter()
                            .map(|arm| {
                                let pattern = match &arm.pattern {
                                    EnumPattern::Variant {
                                        enum_name,
                                        variant_name,
                                    } => {
                                        if arm.bindings.is_empty() {
                                            format!("{}::{}", enum_name, variant_name)
                                        } else {
                                            let bindings_str: Vec<String> = arm
                                                .bindings
                                                .iter()
                                                .map(|(field, var)| format!("{}: {}", field, var))
                                                .collect();
                                            format!(
                                                "{}::{}({})",
                                                enum_name,
                                                variant_name,
                                                bindings_str.join(", ")
                                            )
                                        }
                                    }
                                };
                                (pattern, &arm.body)
                            })
                            .collect();
                        let arms_doc = BoxDoc::intersperse(
                            arms_doc.into_iter().map(|(pattern, body)| {
                                arm_to_doc(BoxDoc::as_string(pattern), body)
                            }),
                            BoxDoc::line(),
                        );
                        BoxDoc::text("match ")
                            .append(subject.to_doc())
                            .append(BoxDoc::text(" {"))
                            .append(BoxDoc::line().append(arms_doc).nest(2))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("}"))
                    }
                }
            }
            WriterStatement::ComponentInvocation {
                component_name,
                args,
                ..
            } => {
                let mut doc = BoxDoc::text("call ")
                    .append(BoxDoc::text(component_name.as_str()))
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
                doc = doc.append(BoxDoc::text(")"));
                doc
            }
        }
    }
}

impl WriterExpr {
    /// Get the type of this expression as an Arc
    pub fn get_type(&self) -> Arc<Type> {
        match self {
            WriterExpr::VariableReference { kind, .. }
            | WriterExpr::FieldAccess { kind, .. }
            | WriterExpr::ArrayLiteral { kind, .. }
            | WriterExpr::RecordLiteral { kind, .. }
            | WriterExpr::EnumLiteral { kind, .. }
            | WriterExpr::OptionLiteral { kind, .. }
            | WriterExpr::Match { kind, .. }
            | WriterExpr::Let { kind, .. } => kind.clone(),

            WriterExpr::FloatLiteral { .. } | WriterExpr::IntToFloat { .. } => {
                Arc::new(Type::Float)
            }
            WriterExpr::IntLiteral { .. } => Arc::new(Type::Int),

            WriterExpr::FragmentLiteral { .. } => Arc::new(Type::Fragment),

            WriterExpr::StringConcat { .. }
            | WriterExpr::TwMerge { .. }
            | WriterExpr::StringLiteral { .. }
            | WriterExpr::IntToString { .. } => Arc::new(Type::String),

            WriterExpr::NumericAdd { operand_types, .. }
            | WriterExpr::NumericSubtract { operand_types, .. }
            | WriterExpr::NumericMultiply { operand_types, .. }
            | WriterExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Arc::new(Type::Int),
                NumericType::Float => Arc::new(Type::Float),
            },

            WriterExpr::BooleanLiteral { .. }
            | WriterExpr::BooleanNegation { .. }
            | WriterExpr::Equals { .. }
            | WriterExpr::LessThan { .. }
            | WriterExpr::LessThanOrEqual { .. }
            | WriterExpr::BooleanLogicalAnd { .. }
            | WriterExpr::BooleanLogicalOr { .. }
            | WriterExpr::ArrayIsEmpty { .. }
            | WriterExpr::StringIsEmpty { .. }
            | WriterExpr::OptionIsSome { .. }
            | WriterExpr::OptionIsNone { .. } => Arc::new(Type::Bool),

            WriterExpr::ArrayLength { .. } | WriterExpr::FloatToInt { .. } => Arc::new(Type::Int),
        }
    }

    /// Get the type of this expression
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;
        static FRAGMENT_TYPE: Type = Type::Fragment;

        match self {
            WriterExpr::VariableReference { kind, .. }
            | WriterExpr::FieldAccess { kind, .. }
            | WriterExpr::ArrayLiteral { kind, .. }
            | WriterExpr::RecordLiteral { kind, .. }
            | WriterExpr::EnumLiteral { kind, .. }
            | WriterExpr::OptionLiteral { kind, .. }
            | WriterExpr::Match { kind, .. }
            | WriterExpr::Let { kind, .. } => kind,

            WriterExpr::FloatLiteral { .. } | WriterExpr::IntToFloat { .. } => &FLOAT_TYPE,
            WriterExpr::IntLiteral { .. } => &INT_TYPE,

            WriterExpr::FragmentLiteral { .. } => &FRAGMENT_TYPE,

            WriterExpr::StringConcat { .. }
            | WriterExpr::TwMerge { .. }
            | WriterExpr::StringLiteral { .. }
            | WriterExpr::IntToString { .. } => &STRING_TYPE,

            WriterExpr::NumericAdd { operand_types, .. }
            | WriterExpr::NumericSubtract { operand_types, .. }
            | WriterExpr::NumericMultiply { operand_types, .. }
            | WriterExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            WriterExpr::BooleanLiteral { .. }
            | WriterExpr::BooleanNegation { .. }
            | WriterExpr::Equals { .. }
            | WriterExpr::LessThan { .. }
            | WriterExpr::LessThanOrEqual { .. }
            | WriterExpr::BooleanLogicalAnd { .. }
            | WriterExpr::BooleanLogicalOr { .. }
            | WriterExpr::ArrayIsEmpty { .. }
            | WriterExpr::StringIsEmpty { .. }
            | WriterExpr::OptionIsSome { .. }
            | WriterExpr::OptionIsNone { .. } => &BOOL_TYPE,

            WriterExpr::ArrayLength { .. } | WriterExpr::FloatToInt { .. } => &INT_TYPE,
        }
    }

    /// Pretty-print this expression
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            WriterExpr::VariableReference { value, .. } => BoxDoc::text(value.to_string()),
            WriterExpr::FieldAccess { record, field, .. } => record
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            WriterExpr::StringLiteral { value, .. } => {
                BoxDoc::text(format!("{:?}", value.as_str()))
            }

            WriterExpr::FragmentLiteral { body, .. } => BoxDoc::text("{")
                .append(if body.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            body.iter().map(|stmt| stmt.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("}")),
            WriterExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            WriterExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            WriterExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            WriterExpr::ArrayLiteral { elements, .. } => {
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
            WriterExpr::RecordLiteral {
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
            WriterExpr::StringConcat { parts, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::intersperse(
                    parts.iter().map(|part| part.to_doc()),
                    BoxDoc::text(" + "),
                ))
                .append(BoxDoc::text(")")),
            WriterExpr::NumericAdd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::NumericSubtract { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" - "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::NumericMultiply { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" * "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::BooleanNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::NumericNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("-"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::Equals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" == "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::LessThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" < "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::LessThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" <= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::BooleanLogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::BooleanLogicalOr { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" || "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::EnumLiteral {
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
            WriterExpr::OptionLiteral { value, kind, .. } => {
                // Extract inner type from Option[T] -> T
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
            WriterExpr::Match { match_, .. } => match match_ {
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
            WriterExpr::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.to_string()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in "))
                .append(body.to_doc()),
            WriterExpr::TwMerge { operand: value, .. } => BoxDoc::text("tw_merge(")
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            WriterExpr::ArrayLength { array, .. } => array.to_doc().append(BoxDoc::text(".len()")),
            WriterExpr::ArrayIsEmpty { array, .. } => {
                array.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            WriterExpr::StringIsEmpty { string, .. } => {
                string.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            WriterExpr::OptionIsSome { option, .. } => {
                option.to_doc().append(BoxDoc::text(".is_some()"))
            }
            WriterExpr::OptionIsNone { option, .. } => {
                option.to_doc().append(BoxDoc::text(".is_none()"))
            }
            WriterExpr::IntToString { value, .. } => {
                value.to_doc().append(BoxDoc::text(".to_string()"))
            }
            WriterExpr::FloatToInt { value, .. } => {
                value.to_doc().append(BoxDoc::text(".to_int()"))
            }
            WriterExpr::IntToFloat { value, .. } => {
                value.to_doc().append(BoxDoc::text(".to_float()"))
            }
        }
    }
}

impl<'a> WriterViewDeclaration {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append("view ")
            .append(self.name.as_str())
            .append(BoxDoc::text("("))
            .append(
                BoxDoc::nil()
                    // soft line break
                    .append(BoxDoc::line_())
                    .append(BoxDoc::intersperse(
                        self.parameters.iter().map(|param| {
                            // Both names: uses of the parameter in the body
                            // print as the variable, the declaration is what
                            // callers name.
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
                    // soft line break
                    .append(BoxDoc::line_())
                    .nest(2)
                    .group(),
            )
            .append(BoxDoc::text(") {"))
            .append(if self.body.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.body.iter().map(|stmt| stmt.to_doc()),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(2)
            })
            .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for WriterStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for WriterExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for WriterViewDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for WriterEnumDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum {} {{", self.name)?;
        for variant in &self.variants {
            if variant.fields.is_empty() {
                writeln!(f, "  {},", variant.name.as_str())?;
            } else {
                let fields_str: Vec<String> = variant
                    .fields
                    .iter()
                    .map(|(name, typ, _)| format!("{}: {}", name, typ))
                    .collect();
                writeln!(
                    f,
                    "  {} {{{}}},",
                    variant.name.as_str(),
                    fields_str.join(", ")
                )?;
            }
        }
        write!(f, "}}")
    }
}

impl WriterRecordDeclaration {
    fn type_name_without_module(typ: &Type) -> String {
        match typ {
            Type::Named { name, .. } => name.as_str().to_string(),
            _ => format!("{}", typ.to_doc().pretty(60)),
        }
    }
}

impl fmt::Display for WriterRecordDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "record {} {{", self.name)?;
        for (field_name, field_type, _) in &self.fields {
            writeln!(
                f,
                "  {}: {},",
                field_name.as_str(),
                Self::type_name_without_module(field_type)
            )?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for WriterModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for enum_decl in &self.enums {
            writeln!(f, "{}", enum_decl)?;
        }
        for record_decl in &self.records {
            writeln!(f, "{}", record_decl)?;
        }
        for component in &self.components {
            write!(f, "{}", component)?;
        }
        for view in &self.views {
            write!(f, "{}", view)?;
        }
        Ok(())
    }
}

impl<'a> WriterComponentDeclaration {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        let closing = ") {";
        BoxDoc::text("component ")
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line_())
                    .append(BoxDoc::intersperse(
                        self.parameters.iter().map(|param| {
                            BoxDoc::text(param.name.to_string())
                                .append(BoxDoc::text("@"))
                                .append(BoxDoc::text(param.var.to_string()))
                                .append(BoxDoc::text(": "))
                                .append(param.typ.to_doc())
                        }),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                    .append(BoxDoc::line_())
                    .nest(2)
                    .group(),
            )
            .append(BoxDoc::text(closing))
            .append(if self.body.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.body.iter().map(|stmt| stmt.to_doc()),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(2)
            })
            .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for WriterComponentDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}
