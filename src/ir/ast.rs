use std::{collections::HashMap, fmt};

use crate::dop::field_name::FieldName;
use crate::dop::r#type::{ComparableType, EquatableType, NumericType, Type};
use crate::dop::VarName;
use crate::hop::component_name::ComponentName;
use pretty::BoxDoc;

// This module contains the types and implementations for ASTs in
// the IR.
//
// The AST structure is:
// * IrEntrypoint -> IrStatement -> IrExpr

/// Unique identifier for each expression in the IR
pub type ExprId = u32;

/// Unique identifier for each statement in the IR
pub type StatementId = u32;

/// Information about a record declaration for transpilation.
#[derive(Debug, Clone)]
pub struct IrRecord {
    pub name: String,
    pub fields: Vec<(FieldName, Type)>,
}

/// Information about an enum declaration for transpilation.
#[derive(Debug, Clone)]
pub struct IrEnum {
    pub name: String,
    pub variants: Vec<String>,
}

/// An IR module containing entrypoints and type declarations.
#[derive(Debug, Clone)]
pub struct IrModule {
    pub entrypoints: Vec<IrEntrypoint>,
    pub records: Vec<IrRecord>,
    pub enums: Vec<IrEnum>,
}

#[derive(Debug, Clone)]
pub struct IrEntrypoint {
    /// Component name (e.g. MyComponent)
    pub name: ComponentName,
    /// Original parameter names with their types (for function signature)
    pub parameters: Vec<(VarName, Type)>,
    /// IR nodes for the entrypoint body
    pub body: Vec<IrStatement>,
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
    StringLiteral { value: String, id: ExprId },

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

    /// A record instantiation expression, e.g. User(name: "John", age: 30)
    RecordInstantiation {
        record_name: String,
        fields: Vec<(FieldName, IrExpr)>,
        kind: Type,
        id: ExprId,
    },

    /// An enum instantiation expression, e.g. Color::Red
    EnumInstantiation {
        enum_name: String,
        variant_name: String,
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

    /// Not equals expression
    NotEquals {
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

    /// Greater than expression
    GreaterThan {
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

    /// Greater than or equal expression
    GreaterThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        operand_types: ComparableType,
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
            | IrExpr::RecordInstantiation { id, .. }
            | IrExpr::EnumInstantiation { id, .. }
            | IrExpr::JsonEncode { id, .. }
            | IrExpr::EnvLookup { id, .. }
            | IrExpr::StringConcat { id, .. }
            | IrExpr::NumericAdd { id, .. }
            | IrExpr::NumericSubtract { id, .. }
            | IrExpr::NumericMultiply { id, .. }
            | IrExpr::BooleanNegation { id, .. }
            | IrExpr::BooleanLogicalAnd { id, .. }
            | IrExpr::BooleanLogicalOr { id, .. }
            | IrExpr::Equals { id, .. }
            | IrExpr::NotEquals { id, .. }
            | IrExpr::LessThan { id, .. }
            | IrExpr::GreaterThan { id, .. }
            | IrExpr::LessThanOrEqual { id, .. }
            | IrExpr::GreaterThanOrEqual { id, .. } => *id,
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
            | IrExpr::RecordInstantiation { kind, .. }
            | IrExpr::EnumInstantiation { kind, .. } => kind,

            IrExpr::FloatLiteral { .. } => &FLOAT_TYPE,
            IrExpr::IntLiteral { .. } => &INT_TYPE,

            IrExpr::JsonEncode { .. }
            | IrExpr::EnvLookup { .. }
            | IrExpr::StringConcat { .. }
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
            | IrExpr::NotEquals { .. }
            | IrExpr::LessThan { .. }
            | IrExpr::GreaterThan { .. }
            | IrExpr::LessThanOrEqual { .. }
            | IrExpr::GreaterThanOrEqual { .. }
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
            IrExpr::RecordInstantiation {
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
            IrExpr::NotEquals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" != "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::LessThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" < "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::GreaterThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" > "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::LessThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" <= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            IrExpr::GreaterThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" >= "))
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
            IrExpr::EnumInstantiation {
                enum_name,
                variant_name,
                ..
            } => BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
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
            IrExpr::RecordInstantiation { fields, .. } => {
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
            | IrExpr::NotEquals { left, right, .. }
            | IrExpr::LessThan { left, right, .. }
            | IrExpr::GreaterThan { left, right, .. }
            | IrExpr::LessThanOrEqual { left, right, .. }
            | IrExpr::GreaterThanOrEqual { left, right, .. }
            | IrExpr::StringConcat { left, right, .. }
            | IrExpr::NumericAdd { left, right, .. }
            | IrExpr::NumericSubtract { left, right, .. }
            | IrExpr::NumericMultiply { left, right, .. }
            | IrExpr::BooleanLogicalAnd { left, right, .. }
            | IrExpr::BooleanLogicalOr { left, right, .. } => {
                left.traverse(f);
                right.traverse(f);
            }
            IrExpr::Var { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. }
            | IrExpr::EnumInstantiation { .. } => {}
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
            IrExpr::RecordInstantiation { fields, .. } => {
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
            | IrExpr::NotEquals { left, right, .. }
            | IrExpr::LessThan { left, right, .. }
            | IrExpr::GreaterThan { left, right, .. }
            | IrExpr::LessThanOrEqual { left, right, .. }
            | IrExpr::GreaterThanOrEqual { left, right, .. }
            | IrExpr::BooleanLogicalAnd { left, right, .. }
            | IrExpr::BooleanLogicalOr { left, right, .. } => {
                left.traverse_mut(f);
                right.traverse_mut(f);
            }
            IrExpr::Var { .. }
            | IrExpr::StringLiteral { .. }
            | IrExpr::BooleanLiteral { .. }
            | IrExpr::FloatLiteral { .. }
            | IrExpr::IntLiteral { .. }
            | IrExpr::EnumInstantiation { .. } => {}
        }
    }
}

impl<'a> IrEntrypoint {
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

impl fmt::Display for IrEntrypoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}
