use std::sync::Arc;

use crate::asset_rewriter::AssetRewriter;
use crate::document::CheapString;
use crate::document_id::DocumentId;
use crate::dop::Type;
use crate::dop::TypedExpr;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::hop::inlining::{InlinedComponentDeclaration, InlinedViewDeclaration};
use crate::hop::inlining::inlined_node::InlinedNode;
use crate::hop::typing::typed_node::{TypedAttributeValue, TypedLoopSource};
use crate::symbols::var_name::VarName;

use super::ast::{
    ExprId, IrArgument, IrComponentDeclaration, IrExpr, IrForSource, IrParameter, IrStatement,
    IrViewDeclaration, StatementId,
};

pub struct Compiler {
    expr_id_counter: usize,
    node_id_counter: usize,
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
}

impl Compiler {
    pub fn compile_component_decl(
        decl: InlinedComponentDeclaration,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    ) -> IrComponentDeclaration {
        let mut compiler = Compiler {
            expr_id_counter: 0,
            node_id_counter: 0,
            asset_rewriter,
        };

        let parameters = decl
            .params
            .into_iter()
            .map(|param| {
                let default = param.default_value.map(|expr| compiler.compile_expr(&expr));
                IrParameter {
                    name: param.var_name,
                    typ: param.var_type,
                    default_value: default,
                }
            })
            .collect::<Vec<_>>();

        IrComponentDeclaration {
            name: decl.component_name,
            parameters,
            body: compiler.compile_nodes(&decl.children),
        }
    }

    pub fn compile(
        view: InlinedViewDeclaration,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    ) -> IrViewDeclaration {
        let mut compiler = Compiler {
            expr_id_counter: 0,
            node_id_counter: 0,
            asset_rewriter,
        };

        let parameters = view
            .params
            .into_iter()
            .map(|param| IrParameter {
                name: param.var_name,
                typ: param.var_type,
                default_value: param.default_value.map(|expr| compiler.compile_expr(&expr)),
            })
            .collect::<Vec<_>>();

        IrViewDeclaration {
            name: view.name,
            parameters,
            body: compiler.compile_nodes(&view.children),
        }
    }

    fn compile_nodes(
        &mut self,
        nodes: &[InlinedNode],
    ) -> Vec<IrStatement> {
        let mut result = Vec::new();
        for node in nodes {
            self.compile_node(node, &mut result);
        }
        result
    }

    fn compile_node(
        &mut self,
        node: &InlinedNode,
        output: &mut Vec<IrStatement>,
    ) {
        match node {
            InlinedNode::Text { value } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: value.to_string(),
                });
            }

            InlinedNode::TextExpression { expression } => {
                output.push(IrStatement::WriteExpr {
                    id: self.next_node_id(),
                    expr: self.compile_expr(expression),
                    escape: true,
                });
            }

            InlinedNode::Html {
                element,
                attributes,
                children,
            } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!("<{}", element.as_str()),
                });
                for attr in attributes {
                    if let Some(val) = &attr.value {
                        self.compile_attribute(&attr.name, val, output);
                    } else {
                        // Boolean attribute
                        output.push(IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}", attr.name.as_str()),
                        });
                    }
                }
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: ">".to_string(),
                });
                if !element.is_void() {
                    for child in children {
                        self.compile_node(child, output);
                    }
                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
                        content: format!("</{}>", element.as_str()),
                    });
                }
            }

            InlinedNode::If {
                condition,
                children,
                ..
            } => {
                output.push(IrStatement::If {
                    id: self.next_node_id(),
                    condition: self.compile_expr(condition),
                    body: self.compile_nodes(children),
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
                output.push(IrStatement::For {
                    id: self.next_node_id(),
                    var: var_name.clone(),
                    source: ir_source,
                    body: self.compile_nodes(children),
                });
            }

            InlinedNode::Doctype { value } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: value.to_string(),
                });
            }

            InlinedNode::Let {
                var,
                value,
                children,
            } => {
                output.push(IrStatement::Let {
                    id: self.next_node_id(),
                    var: var.clone(),
                    value: self.compile_expr(value),
                    body: self.compile_nodes(children),
                });
            }

            InlinedNode::LetRecordDestructure {
                subject,
                bindings,
                children,
            } => {
                output.push(IrStatement::LetRecordDestructure {
                    id: self.next_node_id(),
                    subject: self.compile_expr(subject),
                    bindings: bindings.clone(),
                    body: self.compile_nodes(children),
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
                    } => Match::Option {
                        subject: Box::new(self.compile_expr(subject)),
                        some_arm_binding: some_arm_binding.clone(),
                        some_arm_body: Box::new(self.compile_nodes(some_arm_body)),
                        none_arm_body: Box::new(self.compile_nodes(none_arm_body)),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: Box::new(self.compile_expr(subject)),
                        arms: arms
                            .iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                bindings: arm.bindings.clone(),
                                body: self.compile_nodes(&arm.body),
                            })
                            .collect(),
                    },
                };
                output.push(IrStatement::Match {
                    id: self.next_node_id(),
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
                        expr: self.compile_expr(&arg.expr),
                    })
                    .collect();

                output.push(IrStatement::ComponentInvocation {
                    id: self.next_node_id(),
                    component_name: component_name.clone(),
                    args: compiled_args,
                });
            }
        }
    }

    fn next_expr_id(&mut self) -> ExprId {
        let id = self.expr_id_counter;
        self.expr_id_counter += 1;
        id
    }

    fn next_node_id(&mut self) -> StatementId {
        let id = self.node_id_counter;
        self.node_id_counter += 1;
        id
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
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}=\"{}\"", name.as_str(), s.as_str()),
                });
            }
            TypedAttributeValue::Expression(expr) => {
                if expr.as_type() == &Type::Bool {
                    // Boolean attributes: output attribute name if true, nothing if false
                    output.push(IrStatement::If {
                        id: self.next_node_id(),
                        condition: self.compile_expr(expr),
                        body: vec![IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}", name.as_str()),
                        }],
                    });
                } else if expr.as_type() == &Type::String {
                    // String attributes: output attribute="value"
                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
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
                    output.push(IrStatement::WriteExpr {
                        id: self.next_node_id(),
                        escape: true,
                        expr,
                    });
                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
                        content: "\"".to_string(),
                    });
                } else if matches!(expr.as_type(), Type::Option(inner) if **inner == Type::String) {
                    // Option[String]: render the attribute only when Some, omit when None.
                    let binding = VarName::new("attr").unwrap();

                    // Build the same write sequence used for plain String attributes,
                    // but over the unwrapped binding variable.
                    let value_expr = IrExpr::Var {
                        value: binding.clone(),
                        kind: Arc::new(Type::String),
                        id: self.next_expr_id(),
                    };
                    // Wrap class attribute values in TwMerge for Tailwind class merging.
                    let value_expr = if name.as_str() == "class" {
                        IrExpr::TwMerge {
                            operand: Box::new(value_expr),
                            id: self.next_expr_id(),
                        }
                    } else {
                        value_expr
                    };

                    let some_arm_body = vec![
                        IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}=\"", name.as_str()),
                        },
                        IrStatement::WriteExpr {
                            id: self.next_node_id(),
                            escape: true,
                            expr: value_expr,
                        },
                        IrStatement::Write {
                            id: self.next_node_id(),
                            content: "\"".to_string(),
                        },
                    ];

                    output.push(IrStatement::Match {
                        id: self.next_node_id(),
                        match_: Match::Option {
                            subject: Box::new(self.compile_expr(expr)),
                            some_arm_binding: Some(binding),
                            some_arm_body: Box::new(some_arm_body),
                            none_arm_body: Box::new(vec![]),
                        },
                    });
                } else {
                    unreachable!(
                        "Attribute expression values must evaluate to String, Bool, or Option[String]"
                    )
                }
            }
        }
    }

    fn compile_expr(&mut self, expr: &TypedExpr) -> IrExpr {
        let expr_id = self.next_expr_id();

        match expr {
            TypedExpr::Var { value, kind, .. } => IrExpr::Var {
                value: value.clone(),
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
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: Box::new(self.compile_expr(subject)),
                        arms: arms
                            .iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                bindings: arm.bindings.clone(),
                                body: self.compile_expr(&arm.body),
                            })
                            .collect(),
                    },
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
                    } => Match::Option {
                        subject: Box::new(self.compile_expr(subject)),
                        some_arm_binding: some_arm_binding.clone(),
                        some_arm_body: Box::new(self.compile_expr(some_arm_body)),
                        none_arm_body: Box::new(self.compile_expr(none_arm_body)),
                    },
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
            TypedExpr::FragmentEmpty => {
                unreachable!(
                    "slots must be eliminated during inlining; recursive components cannot declare slots"
                );
            }
            TypedExpr::Let {
                var,
                value,
                body,
                kind,
            } => IrExpr::Let {
                var_name: var.clone(),
                value: Box::new(self.compile_expr(value)),
                body: Box::new(self.compile_expr(body)),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::LetRecordDestructure {
                subject,
                bindings,
                body,
                kind,
            } => IrExpr::LetRecordDestructure {
                subject: Box::new(self.compile_expr(subject)),
                bindings: bindings.clone(),
                body: Box::new(self.compile_expr(body)),
                kind: kind.clone(),
                id: expr_id,
            },
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
        let ir = Compiler::compile(view, None);
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
                view MainComp(name: String) {
                  write("Hello ")
                  write_escaped(name)
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
                view MainComp(show: Bool) {
                  if show {
                    write("<div")
                    write(">")
                    write("Visible")
                    write("</div>")
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
                view MainComp(items: Array[String]) {
                  write("<ul")
                  write(">")
                  for item in items {
                    write("<li")
                    write(">")
                    write_escaped(item)
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
                  write(" class=\"base\"")
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
                view MainComp(cls: String) {
                  write("<div")
                  write(" class=\"base\"")
                  write(" data-value=\"")
                  write_escaped(cls)
                  write("\"")
                  write(">")
                  write("Content")
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
                view TestComp(name: String, count: String) {
                  write("<div")
                  write(">")
                  write("Hello ")
                  write_escaped(name)
                  write(", count: ")
                  write_escaped(count)
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
                view TestComp(flag: Bool) {
                  match flag {
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
    fn should_compile_boolean_attributes() {
        check(
            build_inlined_view("TestComp", vec![("disabled", Type::Bool)], |t| {
                t.html(
                    "input",
                    vec![("disabled", t.attr_expr(t.var_expr("disabled")))],
                    |_| {},
                );
            }),
            expect![[r#"
                -- before --
                view TestComp(disabled: Bool) {
                  <input disabled={disabled}></input>
                }

                -- after --
                view TestComp(disabled: Bool) {
                  write("<input")
                  if disabled {
                    write(" disabled")
                  }
                  write(">")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_option_string_attribute() {
        check(
            build_inlined_view(
                "TestComp",
                vec![("maybe", Type::Option(Arc::new(Type::String)))],
                |t| {
                    t.html("input", vec![("data-x", t.attr_expr(t.var_expr("maybe")))], |_| {});
                },
            ),
            expect![[r#"
                -- before --
                view TestComp(maybe: Option[String]) {
                  <input data-x={maybe}></input>
                }

                -- after --
                view TestComp(maybe: Option[String]) {
                  write("<input")
                  match maybe {
                    Some(attr) => {
                      write(" data-x=\"")
                      write_escaped(attr)
                      write("\"")
                    }
                    None => {
                    }
                  }
                  write(">")
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_option_string_class_attribute() {
        check(
            build_inlined_view(
                "TestComp",
                vec![("maybe", Type::Option(Arc::new(Type::String)))],
                |t| {
                    t.html("div", vec![("class", t.attr_expr(t.var_expr("maybe")))], |_| {});
                },
            ),
            expect![[r#"
                -- before --
                view TestComp(maybe: Option[String]) {
                  <div class={maybe}></div>
                }

                -- after --
                view TestComp(maybe: Option[String]) {
                  write("<div")
                  match maybe {
                    Some(attr) => {
                      write(" class=\"")
                      write_escaped(tw_merge(attr))
                      write("\"")
                    }
                    None => {
                    }
                  }
                  write(">")
                  write("</div>")
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

        let mut compiler = Compiler {
            expr_id_counter: 0,
            node_id_counter: 0,
            asset_rewriter: None,
        };
        let _result = compiler.compile_expr(&expr);
        // If we get here without stack overflow, the test passes
    }
}
