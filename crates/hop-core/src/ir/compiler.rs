use std::sync::Arc;

use crate::asset_rewriter::AssetRewriter;
use crate::document::CheapString;
use crate::document_id::DocumentId;
use crate::expr::Type;
use crate::expr::TypedExpr;
use crate::expr::patterns::{EnumMatchArm, Match};
use crate::hop::inlining::inlined_node::{InlinedNode, InlinedValue};
use crate::hop::inlining::{InlinedComponentDeclaration, InlinedViewDeclaration};
use crate::hop::typing::typed_ast::{TypedEnumDeclaration, TypedRecordDeclaration};
use crate::hop::typing::typed_node::{TypedAttributeValue, TypedLoopSource};
use crate::symbols::var_name::VarName;

use super::ir_module::{
    ExprId, ExprIdCounter, IrArgument, IrComponentDeclaration, IrEnumDeclaration, IrExpr,
    IrForSource, IrModule, IrParameter, IrRecordDeclaration, IrStatement, IrVar, IrViewDeclaration,
    StatementId, StatementIdCounter, VarId, VarIdCounter,
};

pub fn compile(
    views: Vec<InlinedViewDeclaration>,
    components: Vec<InlinedComponentDeclaration>,
    records: &[&TypedRecordDeclaration],
    enums: &[&TypedEnumDeclaration],
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
) -> IrModule {
    let mut expr_ids = ExprIdCounter::new();
    let mut stmt_ids = StatementIdCounter::new();
    let mut var_ids = VarIdCounter::new();
    let mut compiler = Compiler::new(&mut expr_ids, &mut stmt_ids, &mut var_ids, asset_rewriter);

    let views = views
        .into_iter()
        .map(|view| compiler.compile_view_decl(view))
        .collect();
    let components = components
        .into_iter()
        .map(|decl| compiler.compile_component_decl(decl))
        .collect();

    // Records and enums carry no code, so they are converted as-is. Both are
    // sorted by name since callers collect them from an unordered set of
    // modules and the IR must be deterministic.
    let mut records: Vec<IrRecordDeclaration> = records
        .iter()
        .map(|record| IrRecordDeclaration {
            name: record.name.clone(),
            fields: record.fields.clone(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    let mut enums: Vec<IrEnumDeclaration> = enums
        .iter()
        .map(|enum_decl| IrEnumDeclaration {
            name: enum_decl.name.clone(),
            variants: enum_decl.variants.clone(),
        })
        .collect();
    enums.sort_by(|a, b| a.name.cmp(&b.name));

    IrModule {
        views,
        components,
        records,
        enums,
        expr_ids,
        var_ids,
        stmt_ids,
    }
}

struct Compiler<'a> {
    expr_id_counter: &'a mut ExprIdCounter,
    statement_id_counter: &'a mut StatementIdCounter,
    var_id_counter: &'a mut VarIdCounter,
    scopes: Vec<Vec<(VarName, VarId)>>,
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
}

impl<'a> Compiler<'a> {
    fn new(
        expr_id_counter: &'a mut ExprIdCounter,
        statement_id_counter: &'a mut StatementIdCounter,
        var_id_counter: &'a mut VarIdCounter,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    ) -> Self {
        Compiler {
            expr_id_counter,
            statement_id_counter,
            var_id_counter,
            scopes: vec![Vec::new()],
            asset_rewriter,
        }
    }

    fn compile_component_decl(
        &mut self,
        decl: InlinedComponentDeclaration,
    ) -> IrComponentDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(decl.params.len());
        for param in decl.params {
            parameters.push(IrParameter {
                var: self.bind(&param.var_name),
                name: param.var_name,
                typ: param.var_type,
            });
        }

        let declaration = IrComponentDeclaration {
            name: decl.name,
            parameters,
            body: self.compile_nodes(&decl.children),
        };
        self.pop_scope();
        declaration
    }

    fn compile_view_decl(&mut self, view: InlinedViewDeclaration) -> IrViewDeclaration {
        self.push_scope();

        let mut parameters = Vec::with_capacity(view.params.len());
        for param in view.params {
            parameters.push(IrParameter {
                var: self.bind(&param.var_name),
                name: param.var_name,
                typ: param.var_type,
            });
        }

        let declaration = IrViewDeclaration {
            name: view.name,
            parameters,
            body: self.compile_nodes(&view.children),
        };
        self.pop_scope();
        declaration
    }

    fn next_var_id(&mut self) -> VarId {
        self.var_id_counter.next()
    }

    fn next_expr_id(&mut self) -> ExprId {
        self.expr_id_counter.next()
    }

    fn next_statement_id(&mut self) -> StatementId {
        self.statement_id_counter.next()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().expect("scope stack should not be empty");
    }

    fn bind(&mut self, name: &VarName) -> IrVar {
        let id = self.next_var_id();
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
            .push((name.clone(), id));
        IrVar::new(id)
    }

    fn resolve(&mut self, name: &VarName) -> IrVar {
        for scope in self.scopes.iter().rev() {
            if let Some((_, id)) = scope.iter().rev().find(|(n, _)| n == name) {
                return IrVar::new(*id);
            }
        }
        panic!("undefined variable: {name}");
    }

    fn compile_nodes(&mut self, nodes: &[InlinedNode]) -> Vec<IrStatement> {
        let mut result = Vec::new();
        for node in nodes {
            self.compile_node(node, &mut result);
        }
        result
    }

    fn compile_node(&mut self, node: &InlinedNode, output: &mut Vec<IrStatement>) {
        match node {
            InlinedNode::Text { value } => {
                output.push(IrStatement::Write {
                    id: self.next_statement_id(),
                    content: value.to_string(),
                });
            }

            InlinedNode::TextExpression { expression } => {
                if matches!(expression.as_type(), Type::Fragment) {
                    output.push(IrStatement::WriteFragment {
                        id: self.next_statement_id(),
                        expr: self.compile_expr(expression),
                    });
                } else {
                    output.push(IrStatement::WriteString {
                        id: self.next_statement_id(),
                        expr: self.compile_expr(expression),
                    });
                }
            }

            InlinedNode::Html {
                element,
                attributes,
                children,
            } => {
                output.push(IrStatement::Write {
                    id: self.next_statement_id(),
                    content: format!("<{}", element.as_str()),
                });
                for attr in attributes {
                    if let Some(val) = &attr.value {
                        self.compile_attribute(&attr.name, val, output);
                    } else {
                        // Boolean attribute
                        output.push(IrStatement::Write {
                            id: self.next_statement_id(),
                            content: format!(" {}", attr.name.as_str()),
                        });
                    }
                }
                output.push(IrStatement::Write {
                    id: self.next_statement_id(),
                    content: ">".to_string(),
                });
                if !element.is_void() {
                    for child in children {
                        self.compile_node(child, output);
                    }
                    output.push(IrStatement::Write {
                        id: self.next_statement_id(),
                        content: format!("</{}>", element.as_str()),
                    });
                }
            }

            InlinedNode::If {
                condition,
                children,
                ..
            } => {
                output.push(IrStatement::Match {
                    id: self.next_statement_id(),
                    match_: Match::Bool {
                        subject: Box::new(self.compile_expr(condition)),
                        true_body: Box::new(self.compile_nodes(children)),
                        false_body: Box::new(Vec::new()),
                    },
                });
            }

            InlinedNode::For {
                var_name,
                source,
                children,
                ..
            } => {
                let ir_source = match source {
                    TypedLoopSource::Array(array_expr) => {
                        IrForSource::Array(self.compile_expr(array_expr))
                    }
                    TypedLoopSource::RangeInclusive { start, end } => IrForSource::RangeInclusive {
                        start: self.compile_expr(start),
                        end: self.compile_expr(end),
                    },
                };
                let id = self.next_statement_id();
                self.push_scope();
                let var = var_name.as_ref().map(|name| self.bind(name));
                let body = self.compile_nodes(children);
                self.pop_scope();
                output.push(IrStatement::For {
                    id,
                    var,
                    source: ir_source,
                    body,
                });
            }

            InlinedNode::Doctype { value } => {
                output.push(IrStatement::Write {
                    id: self.next_statement_id(),
                    content: value.to_string(),
                });
            }

            InlinedNode::Let {
                var,
                value,
                children,
            } => {
                let id = self.next_statement_id();
                let value = self.compile_value(value);
                self.push_scope();
                let ir_var = self.bind(var);
                let body = self.compile_nodes(children);
                self.pop_scope();
                output.push(IrStatement::Let {
                    id,
                    var: ir_var,
                    value,
                    body,
                });
            }

            InlinedNode::Match { match_ } => {
                let compiled_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: Box::new(self.compile_expr(subject)),
                        true_body: Box::new(self.compile_nodes(true_body)),
                        false_body: Box::new(self.compile_nodes(false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let subject = Box::new(self.compile_expr(subject));
                        self.push_scope();
                        let binding = some_arm_binding.as_ref().map(|name| self.bind(name));
                        let some_body = self.compile_nodes(some_arm_body);
                        self.pop_scope();
                        let none_body = self.compile_nodes(none_arm_body);
                        Match::Option {
                            subject,
                            some_arm_binding: binding,
                            some_arm_body: Box::new(some_body),
                            none_arm_body: Box::new(none_body),
                        }
                    }
                    Match::Enum { subject, arms } => {
                        let subject = Box::new(self.compile_expr(subject));
                        let arms = arms
                            .iter()
                            .map(|arm| {
                                self.push_scope();
                                let bindings = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, name)| (field.clone(), self.bind(name)))
                                    .collect();
                                let body = self.compile_nodes(&arm.body);
                                self.pop_scope();
                                EnumMatchArm {
                                    pattern: arm.pattern.clone(),
                                    bindings,
                                    body,
                                }
                            })
                            .collect();
                        Match::Enum { subject, arms }
                    }
                };
                output.push(IrStatement::Match {
                    id: self.next_statement_id(),
                    match_: compiled_match,
                });
            }

            InlinedNode::ComponentInvocation {
                component_name,
                args,
            } => {
                let compiled_args: Vec<IrArgument> = args
                    .iter()
                    .map(|arg| IrArgument {
                        name: arg.name.clone(),
                        expr: self.compile_value(&arg.value),
                    })
                    .collect();

                output.push(IrStatement::ComponentInvocation {
                    id: self.next_statement_id(),
                    component_name: component_name.clone(),
                    args: compiled_args,
                });
            }
        }
    }

    /// Compile a binding or argument value: expressions compile as usual,
    /// fragment bodies become fragment expressions.
    fn compile_value(&mut self, value: &InlinedValue) -> IrExpr {
        match value {
            InlinedValue::Expr(expr) => self.compile_expr(expr),
            InlinedValue::Fragment(nodes) => IrExpr::FragmentLiteral {
                body: self.compile_nodes(nodes),
                id: self.next_expr_id(),
            },
        }
    }

    /// Helper to compile an attribute to IR statements
    fn compile_attribute(
        &mut self,
        name: &CheapString,
        value: &TypedAttributeValue,
        output: &mut Vec<IrStatement>,
    ) {
        match value {
            TypedAttributeValue::String(s) => {
                if name.as_str() == "class" {
                    output.push(IrStatement::Write {
                        id: self.next_statement_id(),
                        content: format!(" {}=\"", name.as_str()),
                    });
                    output.push(IrStatement::WriteString {
                        id: self.next_statement_id(),
                        expr: IrExpr::TwMerge {
                            operand: Box::new(IrExpr::StringLiteral {
                                value: s.clone(),
                                id: self.next_expr_id(),
                            }),
                            id: self.next_expr_id(),
                        },
                    });
                    output.push(IrStatement::Write {
                        id: self.next_statement_id(),
                        content: "\"".to_string(),
                    });
                } else {
                    output.push(IrStatement::Write {
                        id: self.next_statement_id(),
                        content: format!(" {}=\"{}\"", name.as_str(), s.as_str()),
                    });
                }
            }
            TypedAttributeValue::Expression(expr) => {
                debug_assert!(
                    expr.as_type() == &Type::String,
                    "Attribute expression values must evaluate to String"
                );
                // String attributes: output attribute="value"
                output.push(IrStatement::Write {
                    id: self.next_statement_id(),
                    content: format!(" {}=\"", name.as_str()),
                });
                // Wrap class attribute values in TwMerge for Tailwind class merging
                let expr = if name.as_str() == "class" {
                    IrExpr::TwMerge {
                        operand: Box::new(self.compile_expr(expr)),
                        id: self.next_expr_id(),
                    }
                } else {
                    self.compile_expr(expr)
                };
                output.push(IrStatement::WriteString {
                    id: self.next_statement_id(),
                    expr,
                });
                output.push(IrStatement::Write {
                    id: self.next_statement_id(),
                    content: "\"".to_string(),
                });
            }
        }
    }

    fn compile_expr(&mut self, expr: &TypedExpr) -> IrExpr {
        let expr_id = self.next_expr_id();

        match expr {
            TypedExpr::Var { value, kind, .. } => IrExpr::VariableReference {
                value: self.resolve(value),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::FieldAccess {
                record: object,
                field,
                kind,
                ..
            } => IrExpr::FieldAccess {
                record: Box::new(self.compile_expr(object)),
                field: field.clone(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::BooleanNegation { operand, .. } => IrExpr::BooleanNegation {
                operand: Box::new(self.compile_expr(operand)),
                id: expr_id,
            },
            TypedExpr::NumericNegation {
                operand,
                operand_type,
            } => IrExpr::NumericNegation {
                operand: Box::new(self.compile_expr(operand)),
                operand_type: operand_type.clone(),
                id: expr_id,
            },
            TypedExpr::ArrayLiteral { elements, kind, .. } => IrExpr::ArrayLiteral {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::RecordLiteral {
                record_name,
                fields,
                kind,
                ..
            } => IrExpr::RecordLiteral {
                record_name: record_name.clone(),
                fields: fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.compile_expr(v)))
                    .collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::StringLiteral { value, .. } => IrExpr::StringLiteral {
                value: CheapString::new(process_escape_sequences(value.as_str())),
                id: expr_id,
            },
            TypedExpr::Asset { path } => {
                let rewritten = match &self.asset_rewriter {
                    Some(rewriter) => {
                        rewriter.rewrite(&DocumentId::new(path.trim_start_matches('/')).unwrap())
                    }
                    None => path.to_string(),
                };
                IrExpr::StringLiteral {
                    value: CheapString::new(process_escape_sequences(&rewritten)),
                    id: expr_id,
                }
            }
            TypedExpr::BooleanLiteral { value, .. } => IrExpr::BooleanLiteral {
                value: *value,
                id: expr_id,
            },
            TypedExpr::FloatLiteral { value, .. } => IrExpr::FloatLiteral {
                value: *value,
                id: expr_id,
            },
            TypedExpr::IntLiteral { value, .. } => IrExpr::IntLiteral {
                value: *value,
                id: expr_id,
            },
            TypedExpr::StringConcat { left, right, .. } => IrExpr::StringConcat {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                id: expr_id,
            },
            TypedExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::Equals {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::NotEquals {
                left,
                right,
                operand_types,
                ..
            } => {
                // Desugar NotEquals into BooleanNegation(Equals(...))
                let equals_id = self.next_expr_id();
                IrExpr::BooleanNegation {
                    operand: Box::new(IrExpr::Equals {
                        left: Box::new(self.compile_expr(left)),
                        right: Box::new(self.compile_expr(right)),
                        operand_types: operand_types.clone(),
                        id: equals_id,
                    }),
                    id: expr_id,
                }
            }
            TypedExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::LessThan {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            // Convert a > b to b < a
            TypedExpr::GreaterThan {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::LessThan {
                left: Box::new(self.compile_expr(right)),
                right: Box::new(self.compile_expr(left)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::LessThanOrEqual {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            // Convert a >= b to b <= a
            TypedExpr::GreaterThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::LessThanOrEqual {
                left: Box::new(self.compile_expr(right)),
                right: Box::new(self.compile_expr(left)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::BooleanLogicalAnd { left, right, .. } => IrExpr::BooleanLogicalAnd {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                id: expr_id,
            },
            TypedExpr::BooleanLogicalOr { left, right, .. } => IrExpr::BooleanLogicalOr {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                id: expr_id,
            },
            TypedExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::NumericAdd {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::NumericSubtract {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => IrExpr::NumericMultiply {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                operand_types: operand_types.clone(),
                id: expr_id,
            },
            TypedExpr::EnumLiteral {
                enum_name,
                variant_name,
                fields,
                kind,
            } => IrExpr::EnumLiteral {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields
                    .iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), self.compile_expr(field_expr))
                    })
                    .collect(),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::Match { match_, kind } => {
                let compiled_match = match match_ {
                    Match::Enum { subject, arms } => {
                        let subject = Box::new(self.compile_expr(subject));
                        let arms = arms
                            .iter()
                            .map(|arm| {
                                self.push_scope();
                                let bindings = arm
                                    .bindings
                                    .iter()
                                    .map(|(field, name)| (field.clone(), self.bind(name)))
                                    .collect();
                                let body = self.compile_expr(&arm.body);
                                self.pop_scope();
                                EnumMatchArm {
                                    pattern: arm.pattern.clone(),
                                    bindings,
                                    body,
                                }
                            })
                            .collect();
                        Match::Enum { subject, arms }
                    }
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: Box::new(self.compile_expr(subject)),
                        true_body: Box::new(self.compile_expr(true_body)),
                        false_body: Box::new(self.compile_expr(false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let subject = Box::new(self.compile_expr(subject));
                        self.push_scope();
                        let binding = some_arm_binding.as_ref().map(|name| self.bind(name));
                        let some_body = self.compile_expr(some_arm_body);
                        self.pop_scope();
                        let none_body = self.compile_expr(none_arm_body);
                        Match::Option {
                            subject,
                            some_arm_binding: binding,
                            some_arm_body: Box::new(some_body),
                            none_arm_body: Box::new(none_body),
                        }
                    }
                };
                IrExpr::Match {
                    match_: compiled_match,
                    kind: kind.clone(),
                    id: expr_id,
                }
            }
            TypedExpr::OptionLiteral { value, kind } => IrExpr::OptionLiteral {
                value: value.as_ref().map(|v| Box::new(self.compile_expr(v))),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::FragmentEmpty => IrExpr::FragmentLiteral {
                body: Vec::new(),
                id: expr_id,
            },
            TypedExpr::Let {
                var,
                value,
                body,
                kind,
            } => {
                let value = Box::new(self.compile_expr(value));
                self.push_scope();
                let ir_var = self.bind(var);
                let body = Box::new(self.compile_expr(body));
                self.pop_scope();
                IrExpr::Let {
                    var: ir_var,
                    value,
                    body,
                    kind: kind.clone(),
                    id: expr_id,
                }
            }
            TypedExpr::ArrayLength { array } => IrExpr::ArrayLength {
                array: Box::new(self.compile_expr(array)),
                id: expr_id,
            },
            TypedExpr::ArrayIsEmpty { array } => IrExpr::ArrayIsEmpty {
                array: Box::new(self.compile_expr(array)),
                id: expr_id,
            },
            TypedExpr::StringIsEmpty { string } => IrExpr::StringIsEmpty {
                string: Box::new(self.compile_expr(string)),
                id: expr_id,
            },
            TypedExpr::OptionIsSome { option } => IrExpr::OptionIsSome {
                option: Box::new(self.compile_expr(option)),
                id: expr_id,
            },
            TypedExpr::OptionIsNone { option } => IrExpr::OptionIsNone {
                option: Box::new(self.compile_expr(option)),
                id: expr_id,
            },
            TypedExpr::IntToString { value } => IrExpr::IntToString {
                value: Box::new(self.compile_expr(value)),
                id: expr_id,
            },
            TypedExpr::FloatToInt { value } => IrExpr::FloatToInt {
                value: Box::new(self.compile_expr(value)),
                id: expr_id,
            },
            TypedExpr::IntToFloat { value } => IrExpr::IntToFloat {
                value: Box::new(self.compile_expr(value)),
                id: expr_id,
            },
        }
    }
}

/// Processes escape sequences in a string, converting raw escape sequences
/// like `\n` to their actual character values.
///
/// Supported escape sequences:
/// - `\n` → newline
/// - `\t` → tab
/// - `\r` → carriage return
/// - `\\` → backslash
/// - `\"` → double quote
fn process_escape_sequences(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(other) => {
                    // Invalid escape sequence - keep as-is
                    // (tokenizer already reported the error)
                    result.push('\\');
                    result.push(other);
                }
                None => {
                    // Trailing backslash - keep as-is
                    result.push('\\');
                }
            }
        } else {
            result.push(ch);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::hop::inlining::builder::{build_inlined_view, build_inlined_view_no_params};
    use expect_test::{Expect, expect};

    fn check(view: InlinedViewDeclaration, expected: Expect) {
        let before = view.to_string();
        let mut expr_ids = ExprIdCounter::new();
        let mut statement_ids = StatementIdCounter::new();
        let mut var_ids = VarIdCounter::new();
        let ir = Compiler::new(&mut expr_ids, &mut statement_ids, &mut var_ids, None)
            .compile_view_decl(view);
        let after = ir.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_compile_simple_text() {
        check(
            build_inlined_view_no_params("MainComp", |t| {
                t.text("Hello World");
            }),
            expect![[r#"
                -- before --
                view MainComp() {
                  Hello World
                }

                -- after --
                view MainComp() {
                  write("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_text_expression() {
        check(
            build_inlined_view("MainComp", [("name", Type::String)], |t| {
                t.text("Hello ");
                t.text_expr(t.var_expr("name"));
            }),
            expect![[r#"
                -- before --
                view MainComp(name: String) {
                  Hello 
                  {name}
                }

                -- after --
                view MainComp(name@v0: String) {
                  write("Hello ")
                  write_string(v0)
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_html_element() {
        check(
            build_inlined_view_no_params("MainComp", |t| {
                t.div(vec![], |t| {
                    t.text("Content");
                });
            }),
            expect![[r#"
                -- before --
                view MainComp() {
                  <div>
                    Content
                  </div>
                }

                -- after --
                view MainComp() {
                  write("<div")
                  write(">")
                  write("Content")
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_if_node() {
        check(
            build_inlined_view("MainComp", [("show", Type::Bool)], |t| {
                t.if_node(t.var_expr("show"), |t| {
                    t.div(vec![], |t| {
                        t.text("Visible");
                    });
                });
            }),
            expect![[r#"
                -- before --
                view MainComp(show: Bool) {
                  <if {show}>
                    <div>
                      Visible
                    </div>
                  </if>
                }

                -- after --
                view MainComp(show@v0: Bool) {
                  match v0 {
                    true => {
                      write("<div")
                      write(">")
                      write("Visible")
                      write("</div>")
                    }
                    false => {
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_for_node() {
        check(
            build_inlined_view(
                "MainComp",
                vec![("items", Type::Array(Arc::new(Type::String)))],
                |t| {
                    t.ul(vec![], |t| {
                        t.for_node("item", t.var_expr("items"), |t| {
                            t.li(vec![], |t| {
                                t.text_expr(t.var_expr("item"));
                            });
                        });
                    });
                },
            ),
            expect![[r#"
                -- before --
                view MainComp(items: Array[String]) {
                  <ul>
                    <for {item in items}>
                      <li>
                        {item}
                      </li>
                    </for>
                  </ul>
                }

                -- after --
                view MainComp(items@v0: Array[String]) {
                  write("<ul")
                  write(">")
                  for v1 in v0 {
                    write("<li")
                    write(">")
                    write_string(v1)
                    write("</li>")
                  }
                  write("</ul>")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_static_attributes() {
        check(
            build_inlined_view_no_params("MainComp", |t| {
                t.div(
                    vec![("class", t.attr_str("base")), ("id", t.attr_str("test"))],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view MainComp() {
                  <div class="base" id="test">
                    Content
                  </div>
                }

                -- after --
                view MainComp() {
                  write("<div")
                  write(" class=\"")
                  write_string(tw_merge("base"))
                  write("\"")
                  write(" id=\"test\"")
                  write(">")
                  write("Content")
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_dynamic_attributes() {
        check(
            build_inlined_view("MainComp", [("cls", Type::String)], |t| {
                t.div(
                    vec![
                        ("class", t.attr_str("base")),
                        ("data-value", t.attr_expr(t.var_expr("cls"))),
                    ],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view MainComp(cls: String) {
                  <div class="base" data-value={cls}>
                    Content
                  </div>
                }

                -- after --
                view MainComp(cls@v0: String) {
                  write("<div")
                  write(" class=\"")
                  write_string(tw_merge("base"))
                  write("\"")
                  write(" data-value=\"")
                  write_string(v0)
                  write("\"")
                  write(">")
                  write("Content")
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_tw_merge_both_static_and_dynamic_class_attributes() {
        check(
            build_inlined_view_no_params("MainComp", |t| {
                t.div(vec![("class", t.attr_str("p-1 p-2"))], |t| {
                    t.text("static");
                });
                t.div(
                    vec![(
                        "class",
                        t.attr_expr(TypedExpr::StringLiteral {
                            value: CheapString::new("p-1 p-2".to_string()),
                        }),
                    )],
                    |t| {
                        t.text("expression");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view MainComp() {
                  <div class="p-1 p-2">
                    static
                  </div>
                  <div class={"p-1 p-2"}>
                    expression
                  </div>
                }

                -- after --
                view MainComp() {
                  write("<div")
                  write(" class=\"")
                  write_string(tw_merge("p-1 p-2"))
                  write("\"")
                  write(">")
                  write("static")
                  write("</div>")
                  write("<div")
                  write(" class=\"")
                  write_string(tw_merge("p-1 p-2"))
                  write("\"")
                  write(">")
                  write("expression")
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_development_mode_bootstrap() {
        check(
            build_inlined_view(
                "TestComp",
                vec![("name", Type::String), ("count", Type::String)],
                |t| {
                    t.div(vec![], |t| {
                        t.text("Hello ");
                        t.text_expr(t.var_expr("name"));
                        t.text(", count: ");
                        t.text_expr(t.var_expr("count"));
                    });
                },
            ),
            expect![[r#"
                -- before --
                view TestComp(name: String, count: String) {
                  <div>
                    Hello 
                    {name}
                    , count: 
                    {count}
                  </div>
                }

                -- after --
                view TestComp(name@v0: String, count@v1: String) {
                  write("<div")
                  write(">")
                  write("Hello ")
                  write_string(v0)
                  write(", count: ")
                  write_string(v1)
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_bool_match_node() {
        check(
            build_inlined_view("TestComp", vec![("flag", Type::Bool)], |t| {
                t.bool_match_node(
                    t.var_expr("flag"),
                    |t| {
                        t.text("yes");
                    },
                    |t| {
                        t.text("no");
                    },
                );
            }),
            expect![[r#"
                -- before --
                view TestComp(flag: Bool) {
                  <match {flag}>
                    <case {true}>
                      yes
                    </case>
                    <case {false}>
                      no
                    </case>
                  </match>
                }

                -- after --
                view TestComp(flag@v0: Bool) {
                  match v0 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_inline_script() {
        check(
            build_inlined_view_no_params("MainComp", |t| {
                t.html("script", vec![], |t| {
                    t.text("alert(\"hi\")");
                });
            }),
            expect![[r#"
                -- before --
                view MainComp() {
                  <script>
                    alert("hi")
                  </script>
                }

                -- after --
                view MainComp() {
                  write("<script")
                  write(">")
                  write("alert(\"hi\")")
                  write("</script>")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_void_element() {
        check(
            build_inlined_view_no_params("MainComp", |t| {
                t.html("br", vec![], |_| {});
            }),
            expect![[r#"
                -- before --
                view MainComp() {
                  <br></br>
                }

                -- after --
                view MainComp() {
                  write("<br")
                  write(">")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_deeply_nested_string_concat() {
        // Create a deeply nested left-leaning StringConcat tree
        let depth = 100;
        let mut expr = TypedExpr::StringLiteral {
            value: CheapString::new("start".to_string()),
        };
        for i in 0..depth {
            expr = TypedExpr::StringConcat {
                left: Box::new(expr),
                right: Box::new(TypedExpr::StringLiteral {
                    value: CheapString::new(format!("{}", i)),
                }),
            };
        }

        let mut expr_ids = ExprIdCounter::new();
        let mut statement_ids = StatementIdCounter::new();
        let mut var_ids = VarIdCounter::new();
        let mut compiler = Compiler::new(&mut expr_ids, &mut statement_ids, &mut var_ids, None);
        let _result = compiler.compile_expr(&expr);
        // If we get here without stack overflow, the test passes
    }
}
