use crate::common::is_void_element;
use crate::document::CheapString;
use crate::dop::TypedExpr;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::dop::semantics::r#type::EquatableType;
use crate::dop::{Type, VarName};
use crate::hop::semantics::typed_node::TypedLoopSource;
use crate::inlined::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
};
use std::collections::BTreeMap;

use super::ast::{ExprId, IrComponentDeclaration, IrExpr, IrForSource, IrStatement, StatementId};

pub struct Compiler {
    // Expression ID generation
    expr_id_counter: u32,

    // Node ID generation
    node_id_counter: u32,
}

impl Compiler {
    pub fn compile(entrypoint: InlinedComponentDeclaration) -> IrComponentDeclaration {
        let mut compiler = Compiler {
            expr_id_counter: 0,
            node_id_counter: 0,
        };

        // Extract parameter information by moving
        let param_info = entrypoint
            .params
            .into_iter()
            .map(|param| (param.var_name, param.var_type))
            .collect::<Vec<_>>();

        let component_name = entrypoint.component_name;
        let module_name = &entrypoint.module_name;
        let children = entrypoint.children;

        // Always generate both development and production bodies
        let dev_body = compiler.generate_development_mode_body(
            &module_name.to_path().to_string(),
            component_name.as_str(),
            &param_info,
        );
        let prod_body = compiler.compile_nodes(children, None);

        // Create condition: EnvLookup("HOP_DEV_MODE") == "enabled"
        let env_lookup_expr = IrExpr::EnvLookup {
            key: Box::new(IrExpr::StringLiteral {
                value: CheapString::new("HOP_DEV_MODE".to_string()),
                id: compiler.next_expr_id(),
            }),
            id: compiler.next_expr_id(),
        };

        let condition_expr = IrExpr::Equals {
            left: Box::new(env_lookup_expr),
            right: Box::new(IrExpr::StringLiteral {
                value: CheapString::new("enabled".to_string()),
                id: compiler.next_expr_id(),
            }),
            operand_types: EquatableType::String,
            id: compiler.next_expr_id(),
        };

        // Wrap both bodies in an If statement
        let body = vec![IrStatement::If {
            id: compiler.next_node_id(),
            condition: condition_expr,
            body: dev_body,
            else_body: Some(prod_body),
        }];

        IrComponentDeclaration {
            name: component_name,
            parameters: param_info,
            body,
        }
    }

    /// Compile without the development mode wrapper (useful for tests)
    pub fn compile_without_dev_wrapper(
        entrypoint: InlinedComponentDeclaration,
    ) -> IrComponentDeclaration {
        let mut compiler = Compiler {
            expr_id_counter: 0,
            node_id_counter: 0,
        };

        let param_info = entrypoint
            .params
            .into_iter()
            .map(|param| (param.var_name, param.var_type))
            .collect::<Vec<_>>();

        let component_name = entrypoint.component_name;
        let children = entrypoint.children;

        let body = compiler.compile_nodes(children, None);

        IrComponentDeclaration {
            name: component_name,
            parameters: param_info,
            body,
        }
    }

    fn generate_development_mode_body(
        &mut self,
        module_name: &str,
        component_name: &str,
        params: &[(VarName, Type)],
    ) -> Vec<IrStatement> {
        let mut body = Vec::new();

        // Generate the HTML bootstrap
        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "<!DOCTYPE html>\n".to_string(),
        });

        // Write module and component as separate fields for development mode
        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: format!(
                r#"<script type="application/json">{{"module": "{}", "component": "{}", "params": "#,
                module_name,
                component_name
            ),
        });

        // Create params object by manually building JSON
        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "{".to_string(),
        });

        for (i, (name, typ)) in params.iter().enumerate() {
            if i > 0 {
                body.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: ",".to_string(),
                });
            }
            // Write the key
            body.push(IrStatement::Write {
                id: self.next_node_id(),
                content: format!("\"{}\":", name.as_str()),
            });
            // Write the JSON-encoded value
            body.push(IrStatement::WriteExpr {
                id: self.next_node_id(),
                expr: IrExpr::JsonEncode {
                    value: Box::new(IrExpr::Var {
                        value: name.clone(),
                        kind: typ.clone(),
                        id: self.next_expr_id(),
                    }),
                    id: self.next_expr_id(),
                },
                escape: false,
            });
        }

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "}".to_string(),
        });

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "}</script>\n<script src=\"http://localhost:".to_string(),
        });

        // Insert the port from HOP_DEV_PORT environment variable
        body.push(IrStatement::WriteExpr {
            id: self.next_node_id(),
            expr: IrExpr::EnvLookup {
                key: Box::new(IrExpr::StringLiteral {
                    value: CheapString::new("HOP_DEV_PORT".to_string()),
                    id: self.next_expr_id(),
                }),
                id: self.next_expr_id(),
            },
            escape: false,
        });

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "/development_mode.js\"></script>".to_string(),
        });

        body
    }

    fn compile_nodes(
        &mut self,
        nodes: Vec<InlinedNode>,
        slot_content: Option<Vec<IrStatement>>,
    ) -> Vec<IrStatement> {
        let mut result = Vec::new();
        for node in nodes {
            self.compile_node(node, slot_content.as_ref(), &mut result);
        }
        result
    }

    fn compile_node(
        &mut self,
        node: InlinedNode,
        slot_content: Option<&Vec<IrStatement>>,
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
                let compiled_expr = self.compile_expr(&expression);
                let should_escape = expression.as_type() != &Type::TrustedHTML;
                output.push(IrStatement::WriteExpr {
                    id: self.next_node_id(),
                    expr: compiled_expr,
                    escape: should_escape,
                });
            }

            InlinedNode::Html {
                tag_name,
                attributes,
                children,
            } => {
                self.compile_html_node(&tag_name, attributes, children, slot_content, output);
            }

            InlinedNode::If {
                condition,
                children,
                ..
            } => {
                output.push(IrStatement::If {
                    id: self.next_node_id(),
                    condition: self.compile_expr(&condition),
                    body: self.compile_nodes(children, slot_content.cloned()),
                    else_body: None,
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
                        IrForSource::Array(self.compile_expr(&array_expr))
                    }
                    TypedLoopSource::RangeInclusive { start, end } => IrForSource::RangeInclusive {
                        start: self.compile_expr(&start),
                        end: self.compile_expr(&end),
                    },
                };
                output.push(IrStatement::For {
                    id: self.next_node_id(),
                    var: var_name,
                    source: ir_source,
                    body: self.compile_nodes(children, slot_content.cloned()),
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
                    var,
                    value: self.compile_expr(&value),
                    body: self.compile_nodes(children, slot_content.cloned()),
                });
            }

            InlinedNode::Match { match_ } => {
                let compiled_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject,
                        true_body: Box::new(self.compile_nodes(*true_body, slot_content.cloned())),
                        false_body: Box::new(
                            self.compile_nodes(*false_body, slot_content.cloned()),
                        ),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body: Box::new(
                            self.compile_nodes(*some_arm_body, slot_content.cloned()),
                        ),
                        none_arm_body: Box::new(
                            self.compile_nodes(*none_arm_body, slot_content.cloned()),
                        ),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject,
                        arms: arms
                            .into_iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern,
                                bindings: arm.bindings,
                                body: self.compile_nodes(arm.body, slot_content.cloned()),
                            })
                            .collect(),
                    },
                };
                output.push(IrStatement::Match {
                    id: self.next_node_id(),
                    match_: compiled_match,
                });
            }
        }
    }

    fn compile_html_node(
        &mut self,
        tag_name: &CheapString,
        attributes: BTreeMap<String, InlinedAttribute>,
        children: Vec<InlinedNode>,
        slot_content: Option<&Vec<IrStatement>>,
        output: &mut Vec<IrStatement>,
    ) {
        // Skip script tags without src
        if tag_name.as_str() == "script" && !attributes.contains_key("src") {
            return;
        }

        // Push opening tag
        output.push(IrStatement::Write {
            id: self.next_node_id(),
            content: format!("<{}", tag_name.as_str()),
        });

        // Push attributes
        for (name, attr) in attributes {
            if let Some(val) = attr.value {
                self.compile_attribute(name, val, output);
            } else {
                // Boolean attribute
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}", name),
                });
            }
        }

        output.push(IrStatement::Write {
            id: self.next_node_id(),
            content: ">".to_string(),
        });

        // Compile children
        if !is_void_element(tag_name.as_str()) {
            let child_nodes = self.compile_nodes(children, slot_content.cloned());
            output.extend(child_nodes);
            output.push(IrStatement::Write {
                id: self.next_node_id(),
                content: format!("</{}>", tag_name.as_str()),
            });
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
        name: String,
        value: InlinedAttributeValue,
        output: &mut Vec<IrStatement>,
    ) {
        match value {
            InlinedAttributeValue::String(s) => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}=\"{}\"", name, s),
                });
            }
            InlinedAttributeValue::Expression(expr) => {
                // Boolean attributes: output attribute name if true, nothing if false
                if expr.as_type() == &Type::Bool {
                    let compiled_expr = self.compile_expr(&expr);
                    output.push(IrStatement::If {
                        id: self.next_node_id(),
                        condition: compiled_expr,
                        body: vec![IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}", name),
                        }],
                        else_body: None,
                    });
                } else {
                    // String attributes: output attribute="value"
                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
                        content: format!(" {}=\"", name),
                    });
                    let compiled_expr = self.compile_expr(&expr);
                    let should_escape = expr.as_type() != &Type::TrustedHTML;
                    output.push(IrStatement::WriteExpr {
                        id: self.next_node_id(),
                        expr: compiled_expr,
                        escape: should_escape,
                    });
                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
                        content: "\"".to_string(),
                    });
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
                value: value.clone(),
                id: expr_id,
            },
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
                        subject: subject.clone(),
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
                        subject: subject.clone(),
                        true_body: Box::new(self.compile_expr(true_body)),
                        false_body: Box::new(self.compile_expr(false_body)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => Match::Option {
                        subject: subject.clone(),
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
            TypedExpr::Let {
                var,
                value,
                body,
                kind,
            } => IrExpr::Let {
                var: var.clone(),
                value: Box::new(self.compile_expr(value)),
                body: Box::new(self.compile_expr(body)),
                kind: kind.clone(),
                id: expr_id,
            },
            TypedExpr::MergeClasses { args } => {
                if args.is_empty() {
                    // Empty classes!() -> empty string
                    IrExpr::StringLiteral {
                        value: CheapString::new(String::new()),
                        id: expr_id,
                    }
                } else {
                    // Fold N-ary to binary right-associatively: classes!(a, b, c) -> MergeClasses(a, MergeClasses(b, c))
                    let mut iter = args.iter().rev();
                    let mut result = self.compile_expr(iter.next().unwrap());
                    for arg in iter {
                        let left = self.compile_expr(arg);
                        result = IrExpr::MergeClasses {
                            left: Box::new(left),
                            right: Box::new(result),
                            id: self.next_expr_id(),
                        };
                    }
                    result
                }
            }
            TypedExpr::ArrayLength { array } => IrExpr::ArrayLength {
                array: Box::new(self.compile_expr(array)),
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
            TypedExpr::FloatToString { value } => IrExpr::FloatToString {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inlined::builder::build_inlined;
    use expect_test::{Expect, expect};

    fn check(entrypoint: InlinedComponentDeclaration, expected: Expect) {
        let before = entrypoint.to_string();
        let ir = Compiler::compile(entrypoint);
        let after = ir.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_compile_simple_text() {
        check(
            build_inlined("MainComp", [], |t| {
                t.text("Hello World");
            }),
            expect![[r#"
                -- before --
                <MainComp>
                  Hello World
                </MainComp>

                -- after --
                MainComp() {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("Hello World")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_text_expression() {
        check(
            build_inlined("MainComp", [("name", Type::String)], |t| {
                t.text("Hello ");
                t.text_expr(t.var_expr("name"));
            }),
            expect![[r#"
                -- before --
                <MainComp {name: String}>
                  Hello 
                  {name}
                </MainComp>

                -- after --
                MainComp(name: String) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("\"name\":")
                    write_expr(JsonEncode(name))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("Hello ")
                    write_escaped(name)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_html_element() {
        check(
            build_inlined("MainComp", [], |t| {
                t.div(vec![], |t| {
                    t.text("Content");
                });
            }),
            expect![[r#"
                -- before --
                <MainComp>
                  <div>
                    Content
                  </div>
                </MainComp>

                -- after --
                MainComp() {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("<div")
                    write(">")
                    write("Content")
                    write("</div>")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_if_node() {
        check(
            build_inlined("MainComp", [("show", Type::Bool)], |t| {
                t.if_node(t.var_expr("show"), |t| {
                    t.div(vec![], |t| {
                        t.text("Visible");
                    });
                });
            }),
            expect![[r#"
                -- before --
                <MainComp {show: Bool}>
                  <if {show}>
                    <div>
                      Visible
                    </div>
                  </if>
                </MainComp>

                -- after --
                MainComp(show: Bool) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("\"show\":")
                    write_expr(JsonEncode(show))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    if show {
                      write("<div")
                      write(">")
                      write("Visible")
                      write("</div>")
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_for_node() {
        check(
            build_inlined(
                "MainComp",
                vec![("items", Type::Array(Box::new(Type::String)))],
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
                <MainComp {items: Array[String]}>
                  <ul>
                    <for {item in items}>
                      <li>
                        {item}
                      </li>
                    </for>
                  </ul>
                </MainComp>

                -- after --
                MainComp(items: Array[String]) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("\"items\":")
                    write_expr(JsonEncode(items))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
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
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_static_attributes() {
        check(
            build_inlined("MainComp", [], |t| {
                t.div(
                    vec![("class", t.attr_str("base")), ("id", t.attr_str("test"))],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            expect![[r#"
                -- before --
                <MainComp>
                  <div class="base" id="test">
                    Content
                  </div>
                </MainComp>

                -- after --
                MainComp() {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("<div")
                    write(" class=\"base\"")
                    write(" id=\"test\"")
                    write(">")
                    write("Content")
                    write("</div>")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_dynamic_attributes() {
        check(
            build_inlined("MainComp", [("cls", Type::String)], |t| {
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
                <MainComp {cls: String}>
                  <div class="base" data-value={cls}>
                    Content
                  </div>
                </MainComp>

                -- after --
                MainComp(cls: String) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{")
                    write("\"cls\":")
                    write_expr(JsonEncode(cls))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("<div")
                    write(" class=\"base\"")
                    write(" data-value=\"")
                    write_escaped(cls)
                    write("\"")
                    write(">")
                    write("Content")
                    write("</div>")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_generate_development_mode_bootstrap() {
        check(
            build_inlined(
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
                <TestComp {name: String, count: String}>
                  <div>
                    Hello 
                    {name}
                    , count: 
                    {count}
                  </div>
                </TestComp>

                -- after --
                TestComp(name: String, count: String) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"TestComp\", \"params\": ")
                    write("{")
                    write("\"name\":")
                    write_expr(JsonEncode(name))
                    write(",")
                    write("\"count\":")
                    write_expr(JsonEncode(count))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("<div")
                    write(">")
                    write("Hello ")
                    write_escaped(name)
                    write(", count: ")
                    write_escaped(count)
                    write("</div>")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_bool_match_node() {
        check(
            build_inlined("TestComp", vec![("flag", Type::Bool)], |t| {
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
                <TestComp {flag: Bool}>
                  <match {flag}>
                    <case {true}>
                      yes
                    </case>
                    <case {false}>
                      no
                    </case>
                  </match>
                </TestComp>

                -- after --
                TestComp(flag: Bool) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"TestComp\", \"params\": ")
                    write("{")
                    write("\"flag\":")
                    write_expr(JsonEncode(flag))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    match flag {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn should_compile_boolean_attributes() {
        check(
            build_inlined("TestComp", vec![("disabled", Type::Bool)], |t| {
                t.html(
                    "input",
                    vec![("disabled", t.attr_expr(t.var_expr("disabled")))],
                    |_| {},
                );
            }),
            expect![[r#"
                -- before --
                <TestComp {disabled: Bool}>
                  <input disabled={disabled} />
                </TestComp>

                -- after --
                TestComp(disabled: Bool) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"TestComp\", \"params\": ")
                    write("{")
                    write("\"disabled\":")
                    write_expr(JsonEncode(disabled))
                    write("}")
                    write("}</script>\n<script src=\"http://localhost:")
                    write_expr(EnvLookup("HOP_DEV_PORT"))
                    write("/development_mode.js\"></script>")
                  } else {
                    write("<input")
                    if disabled {
                      write(" disabled")
                    }
                    write(">")
                  }
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
        };
        let _result = compiler.compile_expr(&expr);
        // If we get here without stack overflow, the test passes
    }
}
