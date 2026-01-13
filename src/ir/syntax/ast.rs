use crate::document::CheapString;
use crate::dop::VarName;
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::semantics::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::component_name::ComponentName;
use pretty::BoxDoc;
use std::{collections::HashMap, fmt};

/// Unique identifier for each expression in the IR
pub type ExprId = u32;

/// Unique identifier for each statement in the IR
pub type StatementId = u32;

#[derive(Debug, Clone)]
pub struct IrModule {
    pub components: Vec<IrComponentDeclaration>,
    pub records: Vec<IrRecordDeclaration>,
    pub enums: Vec<IrEnumDeclaration>,
}

#[derive(Debug, Clone)]
pub struct IrComponentDeclaration {
    /// Component name (e.g. MyComponent)
    pub name: ComponentName,
    /// Original parameter names with their types (for function signature)
    pub parameters: Vec<(VarName, Type)>,
    /// IR nodes for the entrypoint body
    pub body: Vec<IrStatement>,
}

#[derive(Debug, Clone)]
pub struct IrRecordDeclaration {
    pub name: String,
    pub fields: Vec<(FieldName, Type)>,
}

#[derive(Debug, Clone)]
pub struct IrEnumDeclaration {
    pub name: String,
    /// Variants with their fields: (variant_name, fields)
    pub variants: Vec<(TypeName, Vec<(FieldName, Type)>)>,
}

#[derive(Debug, Clone, PartialEq)]
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
        else_body: Option<Vec<IrStatement>>,
    },

    /// Loop over an array.
    For {
        id: StatementId,
        var: VarName,
        array: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Bind a variable to the value of an expression.
    Let {
        id: StatementId,
        var: VarName,
        value: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Match on a value and execute the corresponding branch.
    Match {
        id: StatementId,
        match_: Match<Vec<IrStatement>>,
    },
}

/// IR expression type - a concrete expression type for the IR layer.
#[derive(Debug, Clone, PartialEq)]
pub enum IrExpr {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        kind: Type,
        id: ExprId,
    },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<IrExpr>,
        field: FieldName,
        kind: Type,
        id: ExprId,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: CheapString, id: ExprId },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, id: ExprId },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64, id: ExprId },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i64, id: ExprId },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<IrExpr>,
        kind: Type,
        id: ExprId,
    },

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: String,
        fields: Vec<(FieldName, IrExpr)>,
        kind: Type,
        id: ExprId,
    },

    /// An enum literal expression, e.g. Color::Red or Result::Ok(value: 42)
    EnumLiteral {
        enum_name: String,
        variant_name: String,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, IrExpr)>,
        kind: Type,
        id: ExprId,
    },

    /// An option literal expression, e.g. Some(42) or None
    OptionLiteral {
        value: Option<Box<IrExpr>>,
        kind: Type,
        id: ExprId,
    },

    /// A match expression (enum, bool, or option)
    Match {
        match_: Match<IrExpr>,
        kind: Type,
        id: ExprId,
    },

    /// JSON encode expression for converting values to JSON strings
    JsonEncode { value: Box<IrExpr>, id: ExprId },

    /// Environment variable lookup expression
    EnvLookup { key: Box<IrExpr>, id: ExprId },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

    /// Binary merge of CSS classes, produced by folding classes!(a, b, c, ...) left-to-right
    MergeClasses {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        id: ExprId,
    },

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
        var: VarName,
        value: Box<IrExpr>,
        body: Box<IrExpr>,
        kind: Type,
        id: ExprId,
    },
}

impl IrStatement {
    /// Get the primary expression from this statement, if any
    pub fn expr(&self) -> Option<&IrExpr> {
        match self {
            IrStatement::Write { .. } => None,
            IrStatement::WriteExpr { expr, .. } => Some(expr),
            IrStatement::If { condition, .. } => Some(condition),
            IrStatement::For { array, .. } => Some(array),
            IrStatement::Let { value, .. } => Some(value),
            IrStatement::Match { .. } => None, // subject is a variable reference, not an expression
        }
    }

    /// Get a mutable reference to the primary expression from this statement, if any
    pub fn expr_mut(&mut self) -> Option<&mut IrExpr> {
        match self {
            IrStatement::Write { .. } => None,
            IrStatement::WriteExpr { expr, .. } => Some(expr),
            IrStatement::If { condition, .. } => Some(condition),
            IrStatement::For { array, .. } => Some(array),
            IrStatement::Let { value, .. } => Some(value),
            IrStatement::Match { .. } => None, // subject is a variable reference, not an expression
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
            IrStatement::If {
                body, else_body, ..
            } => {
                for stmt in body {
                    stmt.traverse(f);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        stmt.traverse(f);
                    }
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
            IrStatement::Match { match_, .. } => {
                for stmt in match_.bodies().into_iter().flatten() {
                    stmt.traverse(f);
                }
            }
        }
    }

    /// Traverse this statement and all nested statements with a closure that receives
    /// the current scope (variables mapped to the statement that defined them)
    pub fn traverse_with_scope<'a, F>(&'a self, f: &mut F)
    where
        F: FnMut(&'a IrStatement, &HashMap<String, &'a IrStatement>),
    {
        let mut scope = HashMap::new();
        self.traverse_with_scope_impl(&mut scope, f);
    }

    fn traverse_with_scope_impl<'a, F>(
        &'a self,
        scope: &mut HashMap<String, &'a IrStatement>,
        f: &mut F,
    ) where
        F: FnMut(&'a IrStatement, &HashMap<String, &'a IrStatement>),
    {
        f(self, scope);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If {
                body, else_body, ..
            } => {
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        stmt.traverse_with_scope_impl(scope, f);
                    }
                }
            }
            IrStatement::For { var, body, .. } => {
                let prev_value = scope.insert(var.to_string(), self);
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
                if let Some(prev) = prev_value {
                    scope.insert(var.to_string(), prev);
                } else {
                    scope.remove(&var.to_string());
                }
            }
            IrStatement::Let { var, body, .. } => {
                let prev_value = scope.insert(var.to_string(), self);
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
                if let Some(prev) = prev_value {
                    scope.insert(var.to_string(), prev);
                } else {
                    scope.remove(&var.to_string());
                }
            }
            IrStatement::Match { match_, .. } => {
                // For Match::Option with a binding, we need to add it to scope
                match match_ {
                    Match::Bool {
                        true_body,
                        false_body,
                        ..
                    } => {
                        for stmt in true_body.iter() {
                            stmt.traverse_with_scope_impl(scope, f);
                        }
                        for stmt in false_body.iter() {
                            stmt.traverse_with_scope_impl(scope, f);
                        }
                    }
                    Match::Option {
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                        ..
                    } => {
                        if let Some(var) = some_arm_binding {
                            let prev_value = scope.insert(var.to_string(), self);
                            for stmt in some_arm_body.iter() {
                                stmt.traverse_with_scope_impl(scope, f);
                            }
                            if let Some(prev) = prev_value {
                                scope.insert(var.to_string(), prev);
                            } else {
                                scope.remove(&var.to_string());
                            }
                        } else {
                            for stmt in some_arm_body.iter() {
                                stmt.traverse_with_scope_impl(scope, f);
                            }
                        }
                        for stmt in none_arm_body.iter() {
                            stmt.traverse_with_scope_impl(scope, f);
                        }
                    }
                    Match::Enum { arms, .. } => {
                        for arm in arms {
                            for stmt in &arm.body {
                                stmt.traverse_with_scope_impl(scope, f);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Traverse this statement and all nested statements with a mutable closure
    pub fn traverse_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut IrStatement),
    {
        f(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If {
                body, else_body, ..
            } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        stmt.traverse_mut(f);
                    }
                }
            }
            IrStatement::For { body, .. } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
            }
            IrStatement::Let { body, .. } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
            }
            IrStatement::Match { match_, .. } => {
                for stmt in match_.bodies_mut().into_iter().flatten() {
                    stmt.traverse_mut(f);
                }
            }
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
                condition,
                body,
                else_body,
                ..
            } => {
                let mut doc = BoxDoc::text("if ")
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
                    .append(BoxDoc::text("}"));

                if let Some(else_stmts) = else_body {
                    doc = doc
                        .append(BoxDoc::text(" else {"))
                        .append(if else_stmts.is_empty() {
                            BoxDoc::nil()
                        } else {
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    else_stmts.iter().map(|stmt| stmt.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .append(BoxDoc::line())
                                .nest(2)
                        })
                        .append(BoxDoc::text("}"));
                }

                doc
            }
            IrStatement::For {
                var, array, body, ..
            } => BoxDoc::text("for ")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" in "))
                .append(array.to_doc())
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
                        .append(BoxDoc::text(subject.0.as_str()))
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
                            .append(BoxDoc::text(subject.0.as_str()))
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
                            .append(BoxDoc::text(subject.0.as_str()))
                            .append(BoxDoc::text(" {"))
                            .append(BoxDoc::line().append(arms_doc).nest(2))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("}"))
                    }
                }
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
            | IrExpr::BooleanLiteral { id, .. }
            | IrExpr::FloatLiteral { id, .. }
            | IrExpr::IntLiteral { id, .. }
            | IrExpr::ArrayLiteral { id, .. }
            | IrExpr::RecordLiteral { id, .. }
            | IrExpr::EnumLiteral { id, .. }
            | IrExpr::OptionLiteral { id, .. }
            | IrExpr::Match { id, .. }
            | IrExpr::JsonEncode { id, .. }
            | IrExpr::EnvLookup { id, .. }
            | IrExpr::StringConcat { id, .. }
            | IrExpr::MergeClasses { id, .. }
            | IrExpr::NumericAdd { id, .. }
            | IrExpr::NumericSubtract { id, .. }
            | IrExpr::NumericMultiply { id, .. }
            | IrExpr::BooleanNegation { id, .. }
            | IrExpr::BooleanLogicalAnd { id, .. }
            | IrExpr::BooleanLogicalOr { id, .. }
            | IrExpr::Equals { id, .. }
            | IrExpr::LessThan { id, .. }
            | IrExpr::LessThanOrEqual { id, .. }
            | IrExpr::Let { id, .. } => *id,
        }
    }

    /// Get the type of this expression
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;

        match self {
            IrExpr::Var { kind, .. }
            | IrExpr::FieldAccess { kind, .. }
            | IrExpr::ArrayLiteral { kind, .. }
            | IrExpr::RecordLiteral { kind, .. }
            | IrExpr::EnumLiteral { kind, .. }
            | IrExpr::OptionLiteral { kind, .. }
            | IrExpr::Match { kind, .. }
            | IrExpr::Let { kind, .. } => kind,

            IrExpr::FloatLiteral { .. } => &FLOAT_TYPE,
            IrExpr::IntLiteral { .. } => &INT_TYPE,

            IrExpr::JsonEncode { .. }
            | IrExpr::EnvLookup { .. }
            | IrExpr::StringConcat { .. }
            | IrExpr::MergeClasses { .. }
            | IrExpr::StringLiteral { .. } => &STRING_TYPE,

            IrExpr::NumericAdd { operand_types, .. }
            | IrExpr::NumericSubtract { operand_types, .. }
            | IrExpr::NumericMultiply { operand_types, .. } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            IrExpr::BooleanLiteral { .. }
            | IrExpr::BooleanNegation { .. }
            | IrExpr::Equals { .. }
            | IrExpr::LessThan { .. }
            | IrExpr::LessThanOrEqual { .. }
            | IrExpr::BooleanLogicalAnd { .. }
            | IrExpr::BooleanLogicalOr { .. } => &BOOL_TYPE,
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
            IrExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
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
                    BoxDoc::text(record_name.as_str()).append(BoxDoc::text("()"))
                } else {
                    BoxDoc::text(record_name.as_str())
                        .append(BoxDoc::text("("))
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
                        .append(BoxDoc::text(")"))
                }
            }
            IrExpr::JsonEncode { value, .. } => BoxDoc::nil()
                .append(BoxDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::EnvLookup { key, .. } => BoxDoc::nil()
                .append(BoxDoc::text("EnvLookup("))
                .append(key.to_doc())
                .append(BoxDoc::text(")")),
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
                    base.append(BoxDoc::text("("))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(name, expr)| {
                                BoxDoc::text(name.as_str())
                                    .append(BoxDoc::text(": "))
                                    .append(expr.to_doc())
                            }),
                            BoxDoc::text(", "),
                        ))
                        .append(BoxDoc::text(")"))
                }
            }
            IrExpr::OptionLiteral { value, .. } => match value {
                Some(inner) => BoxDoc::text("Some(")
                    .append(inner.to_doc())
                    .append(BoxDoc::text(")")),
                None => BoxDoc::text("None"),
            },
            IrExpr::Match { match_, .. } => match match_ {
                Match::Enum { subject, arms } => {
                    if arms.is_empty() {
                        BoxDoc::text("match ")
                            .append(BoxDoc::text(subject.0.as_str()))
                            .append(BoxDoc::text(" {}"))
                    } else {
                        BoxDoc::text("match ")
                            .append(BoxDoc::text(subject.0.as_str()))
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
                                                        base.append(BoxDoc::text("("))
                                                            .append(BoxDoc::text(
                                                                bindings_str.join(", "),
                                                            ))
                                                            .append(BoxDoc::text(")"))
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
                        .append(BoxDoc::text(subject.0.as_str()))
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
                        .append(BoxDoc::text(subject.0.as_str()))
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
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in "))
                .append(body.to_doc()),
            IrExpr::MergeClasses { left, right, .. } => BoxDoc::text("tw_merge(")
                .append(left.to_doc())
                .append(BoxDoc::text(", "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
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
            IrExpr::BooleanNegation { operand, .. } => {
                operand.traverse(f);
            }
            IrExpr::JsonEncode { value, .. } => {
                value.traverse(f);
            }
            IrExpr::EnvLookup { key, .. } => {
                key.traverse(f);
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
            IrExpr::Match { match_, .. } => match match_ {
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
            },
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
            IrExpr::Var { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. } => {}
            IrExpr::MergeClasses { left, right, .. } => {
                left.traverse(f);
                right.traverse(f);
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
            IrExpr::BooleanNegation { operand, .. } => {
                operand.traverse_mut(f);
            }
            IrExpr::JsonEncode { value, .. } => {
                value.traverse_mut(f);
            }
            IrExpr::EnvLookup { key, .. } => {
                key.traverse_mut(f);
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
            IrExpr::Match { match_, .. } => match match_ {
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
            },
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
            IrExpr::Var { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. } => {}
            IrExpr::MergeClasses { left, right, .. } => {
                left.traverse_mut(f);
                right.traverse_mut(f);
            }
        }
    }
}

impl<'a> IrComponentDeclaration {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        BoxDoc::text(self.name.as_str())
            .append(BoxDoc::text("("))
            .append(
                BoxDoc::nil()
                    // soft line break
                    .append(BoxDoc::line_())
                    .append(BoxDoc::intersperse(
                        self.parameters.iter().map(|(name, typ)| {
                            BoxDoc::text(name.to_string())
                                .append(BoxDoc::text(": "))
                                .append(typ.to_doc())
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

impl fmt::Display for IrComponentDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for IrEnumDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum {} {{", self.name)?;
        for (variant_name, fields) in self.variants.iter() {
            if fields.is_empty() {
                writeln!(f, "  {},", variant_name.as_str())?;
            } else {
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|(name, typ)| format!("{}: {}", name, typ))
                    .collect();
                writeln!(f, "  {}({}),", variant_name.as_str(), fields_str.join(", "))?;
            }
        }
        write!(f, "}}")
    }
}

impl IrRecordDeclaration {
    fn type_name_without_module(typ: &Type) -> String {
        match typ {
            Type::Record { name, .. } => name.as_str().to_string(),
            Type::Enum { name, .. } => name.as_str().to_string(),
            _ => format!("{}", typ.to_doc().pretty(60)),
        }
    }
}

impl fmt::Display for IrRecordDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "record {} {{", self.name)?;
        for (field_name, field_type) in self.fields.iter() {
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
        Ok(())
    }
}
