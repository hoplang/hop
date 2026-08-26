use std::fmt;
use std::sync::Arc;

use crate::document::CheapString;
use crate::expr::patterns::{EnumPattern, Match};
use crate::expr::typing::r#type::{
    ComparableType, EnumVariant, EquatableType, ExamplesAnnotation, NumericType, Type,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

/// An IR module.
///
/// All IDs in the module are unique across the whole module and a pass that
/// creates new expressions, statements or variables must mint fresh IDs from
/// the counters expr_ids, stmt_ids and var_ids.
///
/// Each binder in the IR has a unique VarId, so two binders are never the same
/// variable. Shadowing is impossible and substitution is capture-free.
#[derive(Debug)]
pub struct IrModule {
    pub views: Vec<IrViewDeclaration>,
    pub components: Vec<IrComponentDeclaration>,
    pub records: Vec<IrRecordDeclaration>,
    pub enums: Vec<IrEnumDeclaration>,
    pub expr_ids: ExprIdCounter,
    pub var_ids: VarIdCounter,
    pub stmt_ids: StatementIdCounter,
}

/// Unique identifier for each expression in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(usize);

#[derive(Debug, Clone, Copy, Default)]
pub struct ExprIdCounter(usize);

impl ExprIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> ExprId {
        let id = ExprId(self.0);
        self.0 += 1;
        id
    }
}

/// Unique identifier for each statement in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementId(usize);

#[derive(Debug, Clone, Copy, Default)]
pub struct StatementIdCounter(usize);

impl StatementIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> StatementId {
        let id = StatementId(self.0);
        self.0 += 1;
        id
    }
}

/// Identity of a bound variable in the IR.
///
/// Every binder has its own unique VarId. Equal VarIds mean the same binder.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct VarIdCounter(usize);

impl VarIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> VarId {
        let id = VarId(self.0);
        self.0 += 1;
        id
    }
}

/// A bound variable in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrVar {
    pub id: VarId,
}

impl IrVar {
    pub fn new(id: VarId) -> Self {
        Self { id }
    }
}

/// Rendered in IR dumps. Each transpiler names variables its own way, so this
/// spelling is the IR's alone.
impl fmt::Display for IrVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.id)
    }
}

/// A parameter declaration in the IR (used in views and components).
#[derive(Debug, Clone, PartialEq)]
pub struct IrParameter {
    pub name: VarName,
    pub var: IrVar,
    pub typ: Arc<Type>,
}

impl IrParameter {
    pub fn name(&self) -> &VarName {
        &self.name
    }
}

/// An argument passed to a component invocation in the IR.
#[derive(Debug, PartialEq)]
pub struct IrArgument {
    pub name: VarName,
    pub expr: IrExpr,
}

/// The source of iteration in a For loop.
#[derive(Debug, PartialEq)]
pub enum IrForSource {
    /// Iterate over elements of an array.
    Array(IrExpr),
    /// Iterate over an inclusive integer range.
    RangeInclusive { start: IrExpr, end: IrExpr },
}

#[derive(Debug)]
pub struct IrViewDeclaration {
    /// Entrypoint name
    pub name: TypeName,
    /// Parameter names with their types
    pub parameters: Vec<IrParameter>,
    /// IR nodes for the view body
    pub body: Vec<IrStatement>,
}

#[derive(Debug, Clone)]
pub struct IrRecordDeclaration {
    pub name: TypeName,
    pub fields: Vec<(FieldName, Arc<Type>, Option<ExamplesAnnotation>)>,
}

#[derive(Debug, Clone)]
pub struct IrEnumDeclaration {
    pub name: TypeName,
    pub variants: Vec<EnumVariant>,
}

/// A component declaration in the IR.
///
/// Invokable through the ComponentInvocation statement.
#[derive(Debug)]
pub struct IrComponentDeclaration {
    /// Component name
    pub name: TypeName,
    /// Parameter names with their types
    pub parameters: Vec<IrParameter>,
    /// IR nodes for the component body
    pub body: Vec<IrStatement>,
}

/// A statement in the IR.
///
/// Statements may perform one kind of effect: writing to the output stream.
/// Statement order is output order.
#[derive(Debug, PartialEq)]
pub enum IrStatement {
    /// Write a constant string to the output stream.
    ///
    /// Write performs no escaping.
    Write { id: StatementId, content: String },

    /// Write a String expression to the output stream.
    ///
    /// WriteString performs HTML escaping.
    ///
    /// The type of expr must be String.
    WriteString { id: StatementId, expr: IrExpr },

    /// Write a Fragment expression to the output stream.
    ///
    /// WriteFragment performs no escaping, a Fragment is already-escaped HTML
    /// by construction.
    ///
    /// The type of expr must be Fragment.
    WriteFragment { id: StatementId, expr: IrExpr },

    /// Invoke a component and write its effects to the output stream.
    ComponentInvocation {
        id: StatementId,
        component_name: TypeName,
        args: Vec<IrArgument>,
    },

    /// Loop over an array or range.
    ///
    /// When var is None, the loop binds no variable, but the loop still
    /// executes.
    For {
        id: StatementId,
        var: Option<IrVar>,
        source: IrForSource,
        body: Vec<IrStatement>,
    },

    /// Bind a variable to the value of an expression and execute the effects
    /// of the body.
    ///
    /// The binding scopes over body only, not the statements that follow.
    Let {
        id: StatementId,
        var: IrVar,
        value: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Match on a value and execute the effects of the matched branch.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        id: StatementId,
        match_: Match<IrExpr, Vec<IrStatement>, IrVar>,
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
pub enum IrExpr {
    /// A Let expression.
    Let {
        var: IrVar,
        value: Box<IrExpr>,
        body: Box<IrExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A Match expression over an Enum, Boolean, or Option.
    ///
    /// Matching is exhaustive, a value must match at least one branch.
    Match {
        match_: Match<IrExpr, IrExpr, IrVar>,
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
    /// The expression must evaluate to a record and the field must exist on the
    /// record.
    FieldAccess {
        record: Box<IrExpr>,
        field: FieldName,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A StringLiteral expression.
    StringLiteral { value: CheapString, id: ExprId },

    /// A FragmentLiteral expression.
    ///
    /// Produced by rendering the body into a fresh buffer.
    FragmentLiteral { body: Vec<IrStatement>, id: ExprId },

    /// A BooleanLiteral expression.
    BooleanLiteral { value: bool, id: ExprId },

    /// A FloatLiteral expression.
    FloatLiteral { value: f64, id: ExprId },

    /// An IntLiteral expression.
    IntLiteral { value: i32, id: ExprId },

    /// An ArrayLiteral expression.
    ArrayLiteral {
        elements: Vec<IrExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A RecordLiteral expression.
    RecordLiteral {
        record_name: TypeName,
        fields: Vec<(FieldName, IrExpr)>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An EnumLiteral expression.
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, IrExpr)>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An OptionLiteral expression.
    OptionLiteral {
        value: Option<Box<IrExpr>>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A StringConcat expression.
    ///
    /// Must hold two expressions of type String.
    /// Returns a String.
    StringConcat {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// A TwMerge expression, applied at the class attribute boundary.
    ///
    /// Must hold an expression of type String.
    /// Returns a String.
    TwMerge { operand: Box<IrExpr>, id: ExprId },

    /// A NumericAdd expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericAdd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// A NumericSubtract expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericSubtract {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// A NumericMultiply expression.
    ///
    /// Must hold two expressions of the same NumericType.
    /// Returns the NumericType of the expressions.
    NumericMultiply {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// A NumericNegation expression.
    ///
    /// Must hold an expression of a NumericType.
    /// Returns the NumericType of the expression.
    NumericNegation {
        operand: Box<IrExpr>,
        operand_type: NumericType,
        id: ExprId,
    },

    /// A BooleanNegation expression.
    ///
    /// Must hold a Boolean expression.
    /// Returns a Boolean.
    BooleanNegation { operand: Box<IrExpr>, id: ExprId },

    /// A BooleanLogicalAnd expression.
    ///
    /// Must hold two Boolean expressions.
    /// Returns a Boolean.
    BooleanLogicalAnd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// A BooleanLogicalOr expression.
    ///
    /// Must hold two Boolean expressions.
    /// Returns a Boolean.
    BooleanLogicalOr {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// An Equals expression.
    ///
    /// Must hold two values of the same EquatableType.
    /// Returns a Boolean.
    Equals {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: EquatableType,
        id: ExprId,
    },

    /// A LessThan expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Boolean.
    LessThan {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// A LessThanOrEqual expression.
    ///
    /// Must hold two values of the same ComparableType.
    /// Returns a Boolean.
    LessThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// An ArrayLength expression.
    ///
    /// Must hold an Array expression.
    /// Returns an Int.
    ArrayLength { array: Box<IrExpr>, id: ExprId },

    /// An ArrayIsEmpty expression.
    ///
    /// Must hold an Array expression.
    /// Returns a Boolean.
    ArrayIsEmpty { array: Box<IrExpr>, id: ExprId },

    /// A StringIsEmpty expression.
    ///
    /// Must hold a String expression.
    /// Returns a Boolean.
    StringIsEmpty { string: Box<IrExpr>, id: ExprId },

    /// An OptionIsSome expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Boolean.
    OptionIsSome { option: Box<IrExpr>, id: ExprId },

    /// An OptionIsNone expression.
    ///
    /// Must hold an Option expression.
    /// Returns a Boolean.
    OptionIsNone { option: Box<IrExpr>, id: ExprId },

    /// An IntToString expression.
    ///
    /// Must hold an Int.
    /// Returns a String.
    IntToString { value: Box<IrExpr>, id: ExprId },

    /// A FloatToInt expression.
    ///
    /// Saturates at the i32 bounds and maps NaN -> 0.
    ///
    /// Must hold a Float.
    /// Returns an Int.
    FloatToInt { value: Box<IrExpr>, id: ExprId },

    /// An IntToFloat expression.
    ///
    /// Must hold an Int.
    /// Returns a Float.
    IntToFloat { value: Box<IrExpr>, id: ExprId },
}

impl IrStatement {
    /// Traverse all expressions owned by this statement, recursively
    /// into nested sub-expressions (does not recurse into nested statement
    /// bodies, including the bodies of fragment expressions).
    pub fn traverse_exprs(&self, f: &mut impl FnMut(&IrExpr)) {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteString { expr, .. } => expr.traverse(f),
            IrStatement::WriteFragment { expr, .. } => expr.traverse(f),
            IrStatement::For { source, .. } => match source {
                IrForSource::Array(array) => array.traverse(f),
                IrForSource::RangeInclusive { start, end } => {
                    start.traverse(f);
                    end.traverse(f);
                }
            },
            IrStatement::Let { value, .. } => value.traverse(f),
            IrStatement::Match { match_, .. } => match_.subject().traverse(f),
            IrStatement::ComponentInvocation { args, .. } => {
                for arg in args {
                    arg.expr.traverse(f);
                }
            }
        }
    }

    /// Traverse all expressions owned by this statement, recursively
    /// into nested sub-expressions with mutable access
    /// (does not recurse into nested statement bodies, including the bodies
    /// of fragment expressions).
    pub fn traverse_exprs_mut(&mut self, f: &mut impl FnMut(&mut IrExpr)) {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteString { expr, .. } => expr.traverse_mut(f),
            IrStatement::WriteFragment { expr, .. } => expr.traverse_mut(f),
            IrStatement::For { source, .. } => match source {
                IrForSource::Array(array) => array.traverse_mut(f),
                IrForSource::RangeInclusive { start, end } => {
                    start.traverse_mut(f);
                    end.traverse_mut(f);
                }
            },
            IrStatement::Let { value, .. } => value.traverse_mut(f),
            IrStatement::Match { match_, .. } => match_.subject_mut().traverse_mut(f),
            IrStatement::ComponentInvocation { args, .. } => {
                for arg in args {
                    arg.expr.traverse_mut(f);
                }
            }
        }
    }

    /// Traverse this statement and all nested statements with a closure,
    /// including statements nested inside fragment expressions.
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrStatement),
    {
        f(self);
        // Statement bodies nested inside fragment expressions. Expression
        // traversal stops at a fragment's boundary, so the statements inside
        // are reached here, exactly once.
        self.traverse_exprs(&mut |e| {
            if let IrExpr::FragmentLiteral { body, .. } = e {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
        });
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteString { .. } => {}
            IrStatement::WriteFragment { .. } => {}
            IrStatement::For { body, .. } => {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
            IrStatement::Let { body, .. } => {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
            IrStatement::Match { match_, .. } => {
                for stmt in match_.bodies().into_iter().flatten() {
                    stmt.traverse(f);
                }
            }
            IrStatement::ComponentInvocation { .. } => {}
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            IrStatement::Write { content, .. } => BoxDoc::text("write")
                .append(BoxDoc::text("("))
                .append(BoxDoc::text(format!("{:?}", content)))
                .append(BoxDoc::text(")")),
            IrStatement::WriteString { expr, .. } => BoxDoc::text("write_string")
                .append(BoxDoc::text("("))
                .append(expr.to_doc())
                .append(BoxDoc::text(")")),
            IrStatement::WriteFragment { expr, .. } => BoxDoc::text("write_fragment")
                .append(BoxDoc::text("("))
                .append(expr.to_doc())
                .append(BoxDoc::text(")")),
            IrStatement::For {
                var, source, body, ..
            } => {
                let source_doc = match source {
                    IrForSource::Array(array) => array.to_doc(),
                    IrForSource::RangeInclusive { start, end } => start
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
            IrStatement::Let {
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
            IrStatement::Match { match_, .. } => {
                fn body_to_doc(body: &[IrStatement]) -> BoxDoc<'_> {
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

                fn arm_to_doc<'a>(pattern: BoxDoc<'a>, body: &'a [IrStatement]) -> BoxDoc<'a> {
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
            IrStatement::ComponentInvocation {
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

impl IrExpr {
    /// Get the id of this expression
    pub fn id(&self) -> ExprId {
        match self {
            IrExpr::VariableReference { id, .. }
            | IrExpr::FieldAccess { id, .. }
            | IrExpr::StringLiteral { id, .. }
            | IrExpr::FragmentLiteral { id, .. }
            | IrExpr::BooleanLiteral { id, .. }
            | IrExpr::FloatLiteral { id, .. }
            | IrExpr::IntLiteral { id, .. }
            | IrExpr::ArrayLiteral { id, .. }
            | IrExpr::RecordLiteral { id, .. }
            | IrExpr::EnumLiteral { id, .. }
            | IrExpr::OptionLiteral { id, .. }
            | IrExpr::Match { id, .. }
            | IrExpr::StringConcat { id, .. }
            | IrExpr::TwMerge { id, .. }
            | IrExpr::NumericAdd { id, .. }
            | IrExpr::NumericSubtract { id, .. }
            | IrExpr::NumericMultiply { id, .. }
            | IrExpr::BooleanNegation { id, .. }
            | IrExpr::NumericNegation { id, .. }
            | IrExpr::BooleanLogicalAnd { id, .. }
            | IrExpr::BooleanLogicalOr { id, .. }
            | IrExpr::Equals { id, .. }
            | IrExpr::LessThan { id, .. }
            | IrExpr::LessThanOrEqual { id, .. }
            | IrExpr::Let { id, .. }
            | IrExpr::ArrayLength { id, .. }
            | IrExpr::ArrayIsEmpty { id, .. }
            | IrExpr::StringIsEmpty { id, .. }
            | IrExpr::OptionIsSome { id, .. }
            | IrExpr::OptionIsNone { id, .. }
            | IrExpr::IntToString { id, .. }
            | IrExpr::FloatToInt { id, .. }
            | IrExpr::IntToFloat { id, .. } => *id,
        }
    }

    /// Get the type of this expression as an Arc
    pub fn get_type(&self) -> Arc<Type> {
        match self {
            IrExpr::VariableReference { kind, .. }
            | IrExpr::FieldAccess { kind, .. }
            | IrExpr::ArrayLiteral { kind, .. }
            | IrExpr::RecordLiteral { kind, .. }
            | IrExpr::EnumLiteral { kind, .. }
            | IrExpr::OptionLiteral { kind, .. }
            | IrExpr::Match { kind, .. }
            | IrExpr::Let { kind, .. } => kind.clone(),

            IrExpr::FloatLiteral { .. } | IrExpr::IntToFloat { .. } => Arc::new(Type::Float),
            IrExpr::IntLiteral { .. } => Arc::new(Type::Int),

            IrExpr::FragmentLiteral { .. } => Arc::new(Type::Fragment),

            IrExpr::StringConcat { .. }
            | IrExpr::TwMerge { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::IntToString { .. } => Arc::new(Type::String),

            IrExpr::NumericAdd { operand_types, .. }
            | IrExpr::NumericSubtract { operand_types, .. }
            | IrExpr::NumericMultiply { operand_types, .. }
            | IrExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Arc::new(Type::Int),
                NumericType::Float => Arc::new(Type::Float),
            },

            IrExpr::BooleanLiteral { .. }
            | IrExpr::BooleanNegation { .. }
            | IrExpr::Equals { .. }
            | IrExpr::LessThan { .. }
            | IrExpr::LessThanOrEqual { .. }
            | IrExpr::BooleanLogicalAnd { .. }
            | IrExpr::BooleanLogicalOr { .. }
            | IrExpr::ArrayIsEmpty { .. }
            | IrExpr::StringIsEmpty { .. }
            | IrExpr::OptionIsSome { .. }
            | IrExpr::OptionIsNone { .. } => Arc::new(Type::Bool),

            IrExpr::ArrayLength { .. } | IrExpr::FloatToInt { .. } => Arc::new(Type::Int),
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
            IrExpr::VariableReference { kind, .. }
            | IrExpr::FieldAccess { kind, .. }
            | IrExpr::ArrayLiteral { kind, .. }
            | IrExpr::RecordLiteral { kind, .. }
            | IrExpr::EnumLiteral { kind, .. }
            | IrExpr::OptionLiteral { kind, .. }
            | IrExpr::Match { kind, .. }
            | IrExpr::Let { kind, .. } => kind,

            IrExpr::FloatLiteral { .. } | IrExpr::IntToFloat { .. } => &FLOAT_TYPE,
            IrExpr::IntLiteral { .. } => &INT_TYPE,

            IrExpr::FragmentLiteral { .. } => &FRAGMENT_TYPE,

            IrExpr::StringConcat { .. }
            | IrExpr::TwMerge { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::IntToString { .. } => &STRING_TYPE,

            IrExpr::NumericAdd { operand_types, .. }
            | IrExpr::NumericSubtract { operand_types, .. }
            | IrExpr::NumericMultiply { operand_types, .. }
            | IrExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            IrExpr::BooleanLiteral { .. }
            | IrExpr::BooleanNegation { .. }
            | IrExpr::Equals { .. }
            | IrExpr::LessThan { .. }
            | IrExpr::LessThanOrEqual { .. }
            | IrExpr::BooleanLogicalAnd { .. }
            | IrExpr::BooleanLogicalOr { .. }
            | IrExpr::ArrayIsEmpty { .. }
            | IrExpr::StringIsEmpty { .. }
            | IrExpr::OptionIsSome { .. }
            | IrExpr::OptionIsNone { .. } => &BOOL_TYPE,

            IrExpr::ArrayLength { .. } | IrExpr::FloatToInt { .. } => &INT_TYPE,
        }
    }

    /// Pretty-print this expression
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            IrExpr::VariableReference { value, .. } => BoxDoc::text(value.to_string()),
            IrExpr::FieldAccess { record, field, .. } => record
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            IrExpr::StringLiteral { value, .. } => BoxDoc::text(format!("{:?}", value.as_str())),

            IrExpr::FragmentLiteral { body, .. } => BoxDoc::text("{")
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
            IrExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            IrExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            IrExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            IrExpr::ArrayLiteral { elements, .. } => {
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
            IrExpr::RecordLiteral {
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
            IrExpr::StringConcat { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::NumericAdd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::NumericSubtract { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" - "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::NumericMultiply { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" * "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::BooleanNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::NumericNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("-"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::Equals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" == "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::LessThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" < "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::LessThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" <= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::BooleanLogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::BooleanLogicalOr { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" || "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::EnumLiteral {
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
            IrExpr::OptionLiteral { value, kind, .. } => {
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
            IrExpr::Match { match_, .. } => match match_ {
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
            IrExpr::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.to_string()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in "))
                .append(body.to_doc()),
            IrExpr::TwMerge { operand: value, .. } => BoxDoc::text("tw_merge(")
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::ArrayLength { array, .. } => array.to_doc().append(BoxDoc::text(".len()")),
            IrExpr::ArrayIsEmpty { array, .. } => {
                array.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            IrExpr::StringIsEmpty { string, .. } => {
                string.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            IrExpr::OptionIsSome { option, .. } => {
                option.to_doc().append(BoxDoc::text(".is_some()"))
            }
            IrExpr::OptionIsNone { option, .. } => {
                option.to_doc().append(BoxDoc::text(".is_none()"))
            }
            IrExpr::IntToString { value, .. } => {
                value.to_doc().append(BoxDoc::text(".to_string()"))
            }
            IrExpr::FloatToInt { value, .. } => value.to_doc().append(BoxDoc::text(".to_int()")),
            IrExpr::IntToFloat { value, .. } => value.to_doc().append(BoxDoc::text(".to_float()")),
        }
    }

    /// Recursively traverses this expression and all nested expressions.
    /// Does not descend into the statement body of a fragment expression,
    /// statement-level traversals own that recursion.
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrExpr),
    {
        f(self);
        match self {
            IrExpr::FieldAccess { record, .. } => {
                record.traverse(f);
            }
            IrExpr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.traverse(f);
                }
            }
            IrExpr::RecordLiteral { fields, .. } => {
                for (_, value) in fields {
                    value.traverse(f);
                }
            }
            IrExpr::BooleanNegation { operand, .. } | IrExpr::NumericNegation { operand, .. } => {
                operand.traverse(f);
            }
            IrExpr::Equals { left, right, .. }
            | IrExpr::LessThan { left, right, .. }
            | IrExpr::LessThanOrEqual { left, right, .. }
            | IrExpr::StringConcat { left, right, .. }
            | IrExpr::NumericAdd { left, right, .. }
            | IrExpr::NumericSubtract { left, right, .. }
            | IrExpr::NumericMultiply { left, right, .. }
            | IrExpr::BooleanLogicalAnd { left, right, .. }
            | IrExpr::BooleanLogicalOr { left, right, .. } => {
                left.traverse(f);
                right.traverse(f);
            }
            IrExpr::Match { match_, .. } => {
                match_.subject().traverse(f);
                match match_ {
                    Match::Enum { arms, .. } => {
                        for arm in arms {
                            arm.body.traverse(f);
                        }
                    }
                    Match::Bool {
                        true_body,
                        false_body,
                        ..
                    } => {
                        true_body.traverse(f);
                        false_body.traverse(f);
                    }
                    Match::Option {
                        some_arm_body,
                        none_arm_body,
                        ..
                    } => {
                        some_arm_body.traverse(f);
                        none_arm_body.traverse(f);
                    }
                }
            }
            IrExpr::Let { value, body, .. } => {
                value.traverse(f);
                body.traverse(f);
            }
            IrExpr::OptionLiteral { value, .. } => {
                if let Some(inner) = value {
                    inner.traverse(f);
                }
            }
            IrExpr::EnumLiteral { fields, .. } => {
                for (_, value) in fields {
                    value.traverse(f);
                }
            }
            IrExpr::VariableReference { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::FragmentLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. } => {}
            IrExpr::TwMerge { operand: value, .. } => {
                value.traverse(f);
            }
            IrExpr::ArrayLength { array, .. } => {
                array.traverse(f);
            }
            IrExpr::ArrayIsEmpty { array, .. } => {
                array.traverse(f);
            }
            IrExpr::StringIsEmpty { string, .. } => {
                string.traverse(f);
            }
            IrExpr::OptionIsSome { option, .. } => {
                option.traverse(f);
            }
            IrExpr::OptionIsNone { option, .. } => {
                option.traverse(f);
            }
            IrExpr::IntToString { value, .. } => {
                value.traverse(f);
            }
            IrExpr::FloatToInt { value, .. } => {
                value.traverse(f);
            }
            IrExpr::IntToFloat { value, .. } => {
                value.traverse(f);
            }
        }
    }

    /// Recursively traverses this expression and all nested expressions with
    /// mutable access. Does not descend into the statement body of a fragment
    /// expression, statement-level traversals own that recursion.
    pub fn traverse_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut IrExpr),
    {
        f(self);
        match self {
            IrExpr::FieldAccess { record, .. } => {
                record.traverse_mut(f);
            }
            IrExpr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.traverse_mut(f);
                }
            }
            IrExpr::RecordLiteral { fields, .. } => {
                for (_, value) in fields {
                    value.traverse_mut(f);
                }
            }
            IrExpr::BooleanNegation { operand, .. } | IrExpr::NumericNegation { operand, .. } => {
                operand.traverse_mut(f);
            }
            IrExpr::StringConcat { left, right, .. }
            | IrExpr::NumericAdd { left, right, .. }
            | IrExpr::NumericSubtract { left, right, .. }
            | IrExpr::NumericMultiply { left, right, .. }
            | IrExpr::Equals { left, right, .. }
            | IrExpr::LessThan { left, right, .. }
            | IrExpr::LessThanOrEqual { left, right, .. }
            | IrExpr::BooleanLogicalAnd { left, right, .. }
            | IrExpr::BooleanLogicalOr { left, right, .. } => {
                left.traverse_mut(f);
                right.traverse_mut(f);
            }
            IrExpr::Match { match_, .. } => {
                match_.subject_mut().traverse_mut(f);
                match match_ {
                    Match::Enum { arms, .. } => {
                        for arm in arms {
                            arm.body.traverse_mut(f);
                        }
                    }
                    Match::Bool {
                        true_body,
                        false_body,
                        ..
                    } => {
                        true_body.traverse_mut(f);
                        false_body.traverse_mut(f);
                    }
                    Match::Option {
                        some_arm_body,
                        none_arm_body,
                        ..
                    } => {
                        some_arm_body.traverse_mut(f);
                        none_arm_body.traverse_mut(f);
                    }
                }
            }
            IrExpr::Let { value, body, .. } => {
                value.traverse_mut(f);
                body.traverse_mut(f);
            }
            IrExpr::OptionLiteral { value, .. } => {
                if let Some(inner) = value {
                    inner.traverse_mut(f);
                }
            }
            IrExpr::EnumLiteral { fields, .. } => {
                for (_, value) in fields {
                    value.traverse_mut(f);
                }
            }
            IrExpr::VariableReference { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::FragmentLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. } => {}
            IrExpr::TwMerge { operand: value, .. } => {
                value.traverse_mut(f);
            }
            IrExpr::ArrayLength { array, .. } => {
                array.traverse_mut(f);
            }
            IrExpr::ArrayIsEmpty { array, .. } => {
                array.traverse_mut(f);
            }
            IrExpr::StringIsEmpty { string, .. } => {
                string.traverse_mut(f);
            }
            IrExpr::OptionIsSome { option, .. } => {
                option.traverse_mut(f);
            }
            IrExpr::OptionIsNone { option, .. } => {
                option.traverse_mut(f);
            }
            IrExpr::IntToString { value, .. } => {
                value.traverse_mut(f);
            }
            IrExpr::FloatToInt { value, .. } => {
                value.traverse_mut(f);
            }
            IrExpr::IntToFloat { value, .. } => {
                value.traverse_mut(f);
            }
        }
    }
}

impl<'a> IrViewDeclaration {
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

/// Traverse all statement bodies recursively and apply a closure to each `Vec<IrStatement>`,
/// including the bodies of fragment expressions.
/// Children are visited before their parents (post-order / bottom-up).
pub fn traverse_statements_mut(
    statements: &mut Vec<IrStatement>,
    f: &mut impl FnMut(&mut Vec<IrStatement>),
) {
    for stmt in statements.iter_mut() {
        // Statement bodies nested inside fragment expressions. Expression
        // traversal stops at a fragment's boundary, so the bodies inside are
        // reached here, exactly once.
        stmt.traverse_exprs_mut(&mut |e| {
            if let IrExpr::FragmentLiteral { body, .. } = e {
                traverse_statements_mut(body, f);
            }
        });
        match stmt {
            IrStatement::For { body, .. } => {
                traverse_statements_mut(body, f);
            }
            IrStatement::Let { body, .. } => {
                traverse_statements_mut(body, f);
            }
            IrStatement::Match { match_, .. } => {
                for body in match_.bodies_mut() {
                    traverse_statements_mut(body, f);
                }
            }
            IrStatement::ComponentInvocation { .. } => {}
            IrStatement::Write { .. }
            | IrStatement::WriteString { .. }
            | IrStatement::WriteFragment { .. } => {}
        }
    }
    f(statements);
}

impl fmt::Display for IrStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for IrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for IrViewDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for IrEnumDeclaration {
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

impl IrRecordDeclaration {
    fn type_name_without_module(typ: &Type) -> String {
        match typ {
            Type::Named { name, .. } => name.as_str().to_string(),
            _ => format!("{}", typ.to_doc().pretty(60)),
        }
    }
}

impl fmt::Display for IrRecordDeclaration {
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

impl fmt::Display for IrModule {
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

impl<'a> IrComponentDeclaration {
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

impl fmt::Display for IrComponentDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}
