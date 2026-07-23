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

/// Unique identifier for each expression in the IR
pub type ExprId = usize;

/// Unique identifier for each statement in the IR
pub type StatementId = usize;

/// A parameter declaration in the IR (used in views and components).
#[derive(Debug, Clone, PartialEq)]
pub struct IrParameter {
    pub name: VarName,
    pub typ: Arc<Type>,
}

/// An argument passed to a component invocation in the IR.
#[derive(Debug, PartialEq)]
pub struct IrArgument {
    pub name: VarName,
    pub expr: IrExpr,
}

/// The source of iteration in a for loop - either an array or an inclusive range.
#[derive(Debug, PartialEq)]
pub enum IrForSource {
    /// Iterate over elements of an array
    Array(IrExpr),
    /// Iterate over an inclusive integer range
    RangeInclusive { start: IrExpr, end: IrExpr },
}

#[derive(Debug)]
pub struct IrModule {
    pub views: Vec<IrViewDeclaration>,
    pub components: Vec<IrComponentDeclaration>,
    pub records: Vec<IrRecordDeclaration>,
    pub enums: Vec<IrEnumDeclaration>,
}

#[derive(Debug)]
pub struct IrViewDeclaration {
    /// Entrypoint name (e.g. Index)
    pub name: TypeName,
    /// Original parameter names with their types and optional default values
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

#[derive(Debug)]
pub struct IrComponentDeclaration {
    /// Component name (e.g. TreeView)
    pub name: TypeName,
    /// Parameter names with their types and optional default values
    pub parameters: Vec<IrParameter>,
    /// IR nodes for the component body
    pub body: Vec<IrStatement>,
}

#[derive(Debug, PartialEq)]
pub enum IrStatement {
    /// Write literal string to the output stream.
    Write { id: StatementId, content: String },

    /// Write an expression to the output stream.
    ///
    /// The typechecker guarantees that the value of the expression
    /// will always be a string.
    WriteExpr {
        id: StatementId,
        expr: IrExpr,
        escape: bool,
    },

    /// Execute the body if a condition holds.
    If {
        id: StatementId,
        condition: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Loop over an array or range.
    /// When var is None, the loop variable is discarded (underscore syntax).
    For {
        id: StatementId,
        var: Option<VarName>,
        source: IrForSource,
        body: Vec<IrStatement>,
    },

    /// Bind a variable to the value of an expression.
    Let {
        id: StatementId,
        var: VarName,
        value: IrExpr,
        body: Vec<IrStatement>,
    },

    /// An irrefutable record destructure, e.g. `let { x: a, y: b } = subject in { ... }`.
    LetRecordDestructure {
        id: StatementId,
        subject: IrExpr,
        bindings: Vec<(FieldName, VarName)>,
        body: Vec<IrStatement>,
    },

    /// Render `fragment_body` into a fresh buffer and bind the result to `var`
    /// as a Fragment value, then execute `body`.
    LetFragment {
        id: StatementId,
        var: VarName,
        fragment_body: Vec<IrStatement>,
        body: Vec<IrStatement>,
    },

    /// Match on a value and execute the corresponding branch.
    Match {
        id: StatementId,
        match_: Match<IrExpr, Vec<IrStatement>>,
    },

    /// Call a component render function and write its output.
    ComponentInvocation {
        id: StatementId,
        component_name: TypeName,
        args: Vec<IrArgument>,
    },
}

/// IR expression type - a concrete expression type for the IR layer.
#[derive(Debug, PartialEq)]
pub enum IrExpr {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<IrExpr>,
        field: FieldName,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: CheapString, id: ExprId },

    /// An empty Fragment value, i.e. Fragment::empty()
    FragmentEmpty { id: ExprId },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, id: ExprId },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64, id: ExprId },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i64, id: ExprId },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<IrExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: TypeName,
        fields: Vec<(FieldName, IrExpr)>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An enum literal expression, e.g. Color::Red or Result::Ok(value: 42)
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, IrExpr)>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An option literal expression, e.g. Some(42) or None
    OptionLiteral {
        value: Option<Box<IrExpr>>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// A match expression (enum, bool, or option)
    Match {
        match_: Match<IrExpr, IrExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// Tailwind merge wrapper applied at class attribute boundary
    TwMerge { operand: Box<IrExpr>, id: ExprId },

    /// Numeric addition expression for adding numeric values
    NumericAdd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// Numeric subtraction expression for subtracting numeric values
    NumericSubtract {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// Numeric multiplication expression for multiplying numeric values
    NumericMultiply {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: NumericType,
        id: ExprId,
    },

    /// Boolean negation expression
    BooleanNegation { operand: Box<IrExpr>, id: ExprId },

    /// Numeric negation expression
    NumericNegation {
        operand: Box<IrExpr>,
        operand_type: NumericType,
        id: ExprId,
    },

    /// Boolean logical AND expression
    BooleanLogicalAnd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// Boolean logical OR expression
    BooleanLogicalOr {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// Equals expression
    Equals {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: EquatableType,
        id: ExprId,
    },

    /// Less than expression
    LessThan {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// Less than or equal expression
    LessThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: ComparableType,
        id: ExprId,
    },

    /// A let binding expression
    Let {
        var_name: VarName,
        value: Box<IrExpr>,
        body: Box<IrExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// An irrefutable record destructure, e.g. `let { x: a, y: b } = subject in body`.
    LetRecordDestructure {
        subject: Box<IrExpr>,
        bindings: Vec<(FieldName, VarName)>,
        body: Box<IrExpr>,
        kind: Arc<Type>,
        id: ExprId,
    },

    /// Array length expression, e.g. items.len()
    ArrayLength { array: Box<IrExpr>, id: ExprId },

    /// Array is empty expression, e.g. items.is_empty()
    ArrayIsEmpty { array: Box<IrExpr>, id: ExprId },

    /// String is empty expression, e.g. name.is_empty()
    StringIsEmpty { string: Box<IrExpr>, id: ExprId },

    /// Option is_some expression, e.g. maybe_value.is_some()
    OptionIsSome { option: Box<IrExpr>, id: ExprId },

    /// Option is_none expression, e.g. maybe_value.is_none()
    OptionIsNone { option: Box<IrExpr>, id: ExprId },

    /// Int to string conversion, e.g. count.to_string()
    IntToString { value: Box<IrExpr>, id: ExprId },

    /// Float to int conversion, e.g. price.to_int()
    FloatToInt { value: Box<IrExpr>, id: ExprId },

    /// Int to float conversion, e.g. count.to_float()
    IntToFloat { value: Box<IrExpr>, id: ExprId },
}

impl IrStatement {
    /// Traverse all expressions owned by this statement, recursively
    /// into nested sub-expressions (does not recurse into nested statement bodies).
    pub fn traverse_exprs(&self, f: &mut impl FnMut(&IrExpr)) {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { expr, .. } => expr.traverse(f),
            IrStatement::If { condition, .. } => condition.traverse(f),
            IrStatement::For { source, .. } => match source {
                IrForSource::Array(array) => array.traverse(f),
                IrForSource::RangeInclusive { start, end } => {
                    start.traverse(f);
                    end.traverse(f);
                }
            },
            IrStatement::Let { value, .. } => value.traverse(f),
            IrStatement::LetFragment { .. } => {}
            IrStatement::LetRecordDestructure { subject, .. } => subject.traverse(f),
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
    /// (does not recurse into nested statement bodies).
    pub fn traverse_exprs_mut(&mut self, f: &mut impl FnMut(&mut IrExpr)) {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { expr, .. } => expr.traverse_mut(f),
            IrStatement::If { condition, .. } => condition.traverse_mut(f),
            IrStatement::For { source, .. } => match source {
                IrForSource::Array(array) => array.traverse_mut(f),
                IrForSource::RangeInclusive { start, end } => {
                    start.traverse_mut(f);
                    end.traverse_mut(f);
                }
            },
            IrStatement::Let { value, .. } => value.traverse_mut(f),
            IrStatement::LetFragment { .. } => {}
            IrStatement::LetRecordDestructure { subject, .. } => subject.traverse_mut(f),
            IrStatement::Match { match_, .. } => match_.subject_mut().traverse_mut(f),
            IrStatement::ComponentInvocation { args, .. } => {
                for arg in args {
                    arg.expr.traverse_mut(f);
                }
            }
        }
    }

    /// Traverse this statement and all nested statements with a closure
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrStatement),
    {
        f(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If { body, .. } => {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
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
            IrStatement::LetFragment {
                fragment_body,
                body,
                ..
            } => {
                for stmt in fragment_body {
                    stmt.traverse(f);
                }
                for stmt in body {
                    stmt.traverse(f);
                }
            }
            IrStatement::LetRecordDestructure { body, .. } => {
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
            IrStatement::WriteExpr { expr, escape, .. } => {
                let write_fn = if *escape {
                    "write_escaped"
                } else {
                    "write_expr"
                };
                BoxDoc::text(write_fn)
                    .append(BoxDoc::text("("))
                    .append(expr.to_doc())
                    .append(BoxDoc::text(")"))
            }
            IrStatement::If {
                condition, body, ..
            } => BoxDoc::text("if ")
                .append(condition.to_doc())
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
                .append(BoxDoc::text("}")),
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
                    Some(name) => BoxDoc::text(name.as_str()),
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
                .append(BoxDoc::text(var.as_str()))
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
            IrStatement::LetFragment {
                var,
                fragment_body,
                body,
                ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" = {"))
                .append(if fragment_body.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            fragment_body.iter().map(|stmt| stmt.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("} in {"))
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
            IrStatement::LetRecordDestructure {
                subject,
                bindings,
                body,
                ..
            } => {
                let bindings_doc = BoxDoc::intersperse(
                    bindings.iter().map(|(field, var)| {
                        BoxDoc::text(field.as_str())
                            .append(BoxDoc::text(": "))
                            .append(BoxDoc::text(var.as_str()))
                    }),
                    BoxDoc::text(", "),
                );
                BoxDoc::text("let {")
                    .append(bindings_doc)
                    .append(BoxDoc::text("} = "))
                    .append(subject.to_doc())
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
                    .append(BoxDoc::text("}"))
            }
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
                            Some(var) => format!("Some({})", var.as_str()),
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
            IrExpr::Var { id, .. }
            | IrExpr::FieldAccess { id, .. }
            | IrExpr::StringLiteral { id, .. }
            | IrExpr::FragmentEmpty { id, .. }
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
            | IrExpr::LetRecordDestructure { id, .. }
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
            IrExpr::Var { kind, .. }
            | IrExpr::FieldAccess { kind, .. }
            | IrExpr::ArrayLiteral { kind, .. }
            | IrExpr::RecordLiteral { kind, .. }
            | IrExpr::EnumLiteral { kind, .. }
            | IrExpr::OptionLiteral { kind, .. }
            | IrExpr::Match { kind, .. }
            | IrExpr::Let { kind, .. }
            | IrExpr::LetRecordDestructure { kind, .. } => kind.clone(),

            IrExpr::FloatLiteral { .. } | IrExpr::IntToFloat { .. } => Arc::new(Type::Float),
            IrExpr::IntLiteral { .. } => Arc::new(Type::Int),

            IrExpr::FragmentEmpty { .. } => Arc::new(Type::Fragment),

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
            IrExpr::Var { kind, .. }
            | IrExpr::FieldAccess { kind, .. }
            | IrExpr::ArrayLiteral { kind, .. }
            | IrExpr::RecordLiteral { kind, .. }
            | IrExpr::EnumLiteral { kind, .. }
            | IrExpr::OptionLiteral { kind, .. }
            | IrExpr::Match { kind, .. }
            | IrExpr::Let { kind, .. }
            | IrExpr::LetRecordDestructure { kind, .. } => kind,

            IrExpr::FloatLiteral { .. } | IrExpr::IntToFloat { .. } => &FLOAT_TYPE,
            IrExpr::IntLiteral { .. } => &INT_TYPE,

            IrExpr::FragmentEmpty { .. } => &FRAGMENT_TYPE,

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
            IrExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            IrExpr::FieldAccess { record, field, .. } => record
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            IrExpr::StringLiteral { value, .. } => BoxDoc::text(format!("{:?}", value.as_str())),
            IrExpr::FragmentEmpty { .. } => BoxDoc::text("Fragment::empty()"),
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
                            .append(BoxDoc::text(name.as_str()))
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
                var_name,
                value,
                body,
                ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var_name.as_str()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in "))
                .append(body.to_doc()),
            IrExpr::LetRecordDestructure {
                subject,
                bindings,
                body,
                ..
            } => {
                let bindings_doc = BoxDoc::intersperse(
                    bindings.iter().map(|(field, var)| {
                        BoxDoc::text(field.as_str())
                            .append(BoxDoc::text(": "))
                            .append(BoxDoc::text(var.as_str()))
                    }),
                    BoxDoc::text(", "),
                );
                BoxDoc::text("let {")
                    .append(bindings_doc)
                    .append(BoxDoc::text("} = "))
                    .append(subject.to_doc())
                    .append(BoxDoc::text(" in "))
                    .append(body.to_doc())
            }
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

    /// Recursively traverses this expression and all nested expressions
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
            IrExpr::LetRecordDestructure { subject, body, .. } => {
                subject.traverse(f);
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
            IrExpr::Var { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::FragmentEmpty { .. }
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

    /// Recursively traverses this expression and all nested expressions with mutable access
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
            IrExpr::LetRecordDestructure { subject, body, .. } => {
                subject.traverse_mut(f);
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
            IrExpr::Var { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::FragmentEmpty { .. }
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
                            BoxDoc::text(param.name.to_string())
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

/// Traverse all statement bodies recursively and apply a closure to each `Vec<IrStatement>`.
/// Children are visited before their parents (post-order / bottom-up).
pub fn traverse_statements_mut(
    statements: &mut Vec<IrStatement>,
    f: &mut impl FnMut(&mut Vec<IrStatement>),
) {
    for stmt in statements.iter_mut() {
        match stmt {
            IrStatement::If { body, .. } => {
                traverse_statements_mut(body, f);
            }
            IrStatement::For { body, .. } => {
                traverse_statements_mut(body, f);
            }
            IrStatement::Let { body, .. } => {
                traverse_statements_mut(body, f);
            }
            IrStatement::LetFragment {
                fragment_body,
                body,
                ..
            } => {
                traverse_statements_mut(fragment_body, f);
                traverse_statements_mut(body, f);
            }
            IrStatement::LetRecordDestructure { body, .. } => {
                traverse_statements_mut(body, f);
            }
            IrStatement::Match { match_, .. } => {
                for body in match_.bodies_mut() {
                    traverse_statements_mut(body, f);
                }
            }
            IrStatement::ComponentInvocation { .. } => {}
            IrStatement::Write { .. } | IrStatement::WriteExpr { .. } => {}
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
