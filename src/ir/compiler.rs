use crate::common::is_void_element;
use crate::document::document_cursor::StringSpan;
use crate::dop::r#type::EquatableType;
use crate::dop::{PropertyName, SimpleTypedExpr, TypedExpr};
use crate::dop::{Type, VarName};
use crate::hop::component_name::ComponentName;
use crate::hop::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypoint, InlinedNode,
};
use std::collections::BTreeMap;

use super::ast::{ExprId, IrEntrypoint, IrExpr, IrStatement, StatementId};

pub struct Compiler {
    // Expression ID generation
    expr_id_counter: u32,

    // Node ID generation
    node_id_counter: u32,
}

impl Compiler {
    pub fn compile(entrypoint: InlinedEntrypoint) -> IrEntrypoint {
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

        let tag_name = entrypoint.tag_name;
        let module_name = &entrypoint.module_name;
        let children = entrypoint.children;

        // Always generate both development and production bodies
        let dev_body = compiler.generate_development_mode_body(module_name.as_str(), tag_name.as_str(), &param_info);
        let prod_body = compiler.compile_nodes(children, None);

        // Create condition: EnvLookup("HOP_DEV_MODE") == "enabled"
        let env_lookup_expr = TypedExpr::EnvLookup {
            key: Box::new(TypedExpr::StringLiteral {
                value: "HOP_DEV_MODE".to_string(),
                annotation: compiler.next_expr_id(),
            }),
            annotation: compiler.next_expr_id(),
        };

        let condition_expr = TypedExpr::Equals {
            left: Box::new(env_lookup_expr),
            right: Box::new(TypedExpr::StringLiteral {
                value: "enabled".to_string(),
                annotation: compiler.next_expr_id(),
            }),
            operand_types: EquatableType::String,
            annotation: compiler.next_expr_id(),
        };

        // Wrap both bodies in an If statement
        let body = vec![IrStatement::If {
            id: compiler.next_node_id(),
            condition: condition_expr,
            body: dev_body,
            else_body: Some(prod_body),
        }];

        IrEntrypoint {
            name: ComponentName::new(tag_name.to_string())
                .expect("Entrypoint should have valid component name"),
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

        // Create params object
        if params.is_empty() {
            body.push(IrStatement::Write {
                id: self.next_node_id(),
                content: "{}".to_string(),
            });
        } else {
            // Build object with all parameters
            let mut props = Vec::new();
            let mut object_type = BTreeMap::new();
            for (name, typ) in params {
                // VarName and PropertyName have the same validation rules (snake_case)
                // so we can safely convert between them
                let prop_name = PropertyName::new(name.as_str()).unwrap();
                object_type.insert(prop_name.clone(), typ.clone());
                props.push((
                    prop_name,
                    TypedExpr::Var {
                        value: name.clone(),
                        kind: typ.clone(),
                        annotation: self.next_expr_id(),
                    },
                ));
            }

            body.push(IrStatement::WriteExpr {
                id: self.next_node_id(),
                expr: TypedExpr::JsonEncode {
                    value: Box::new(TypedExpr::ObjectLiteral {
                        properties: props,
                        kind: Type::Object(object_type),
                        annotation: self.next_expr_id(),
                    }),
                    annotation: self.next_expr_id(),
                },
                escape: false,
            });
        }

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content:
                "}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>"
                    .to_string(),
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
                let compiled_expr = self.compile_expr(expression.clone());
                let should_escape = compiled_expr.as_type() != &Type::TrustedHTML;
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
                    condition: self.compile_expr(condition.clone()),
                    body: self.compile_nodes(children, slot_content.cloned()),
                    else_body: None,
                });
            }

            InlinedNode::For {
                var_name,
                array_expr,
                children,
                ..
            } => {
                output.push(IrStatement::For {
                    id: self.next_node_id(),
                    var: var_name,
                    array: self.compile_expr(array_expr.clone()),
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
                    value: self.compile_expr(value.clone()),
                    body: self.compile_nodes(children, slot_content.cloned()),
                });
            }
        }
    }

    fn compile_html_node(
        &mut self,
        tag_name: &StringSpan,
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
            InlinedAttributeValue::Expressions(exprs) => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}=\"", name),
                });
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        output.push(IrStatement::Write {
                            id: self.next_node_id(),
                            content: " ".to_string(),
                        });
                    }
                    let compiled_expr = self.compile_expr(expr.clone());
                    let should_escape = compiled_expr.as_type() != &Type::TrustedHTML;
                    output.push(IrStatement::WriteExpr {
                        id: self.next_node_id(),
                        expr: compiled_expr,
                        escape: should_escape,
                    });
                }
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: "\"".to_string(),
                });
            }
        }
    }

    fn compile_expr(&mut self, expr: SimpleTypedExpr) -> IrExpr {
        let expr_id = self.next_expr_id();

        match expr {
            SimpleTypedExpr::Var { value, kind, .. } => TypedExpr::Var {
                value,
                kind,
                annotation: expr_id,
            },
            SimpleTypedExpr::PropertyAccess {
                object,
                property,
                kind,
                ..
            } => TypedExpr::PropertyAccess {
                object: Box::new(self.compile_expr(*object)),
                property,
                kind,
                annotation: expr_id,
            },
            SimpleTypedExpr::Negation { operand, .. } => TypedExpr::Negation {
                operand: Box::new(self.compile_expr(*operand)),
                annotation: expr_id,
            },
            SimpleTypedExpr::ArrayLiteral { elements, kind, .. } => TypedExpr::ArrayLiteral {
                elements: elements.into_iter().map(|e| self.compile_expr(e)).collect(),
                kind,
                annotation: expr_id,
            },
            SimpleTypedExpr::ObjectLiteral {
                properties, kind, ..
            } => TypedExpr::ObjectLiteral {
                properties: properties
                    .into_iter()
                    .map(|(k, v)| (k, self.compile_expr(v)))
                    .collect(),
                kind,
                annotation: expr_id,
            },
            SimpleTypedExpr::StringLiteral { value, .. } => TypedExpr::StringLiteral {
                value,
                annotation: expr_id,
            },
            SimpleTypedExpr::BooleanLiteral { value, .. } => TypedExpr::BooleanLiteral {
                value,
                annotation: expr_id,
            },
            SimpleTypedExpr::FloatLiteral { value, .. } => TypedExpr::FloatLiteral {
                value,
                annotation: expr_id,
            },
            SimpleTypedExpr::IntLiteral { value, .. } => TypedExpr::IntLiteral {
                value,
                annotation: expr_id,
            },
            SimpleTypedExpr::JsonEncode { value, .. } => TypedExpr::JsonEncode {
                value: Box::new(self.compile_expr(*value)),
                annotation: expr_id,
            },
            SimpleTypedExpr::EnvLookup { key, .. } => TypedExpr::EnvLookup {
                key: Box::new(self.compile_expr(*key)),
                annotation: expr_id,
            },
            SimpleTypedExpr::StringConcat { left, right, .. } => TypedExpr::StringConcat {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                annotation: expr_id,
            },
            SimpleTypedExpr::Equals {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::Equals {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            SimpleTypedExpr::NotEquals {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::NotEquals {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            SimpleTypedExpr::LessThan {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::LessThan {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            SimpleTypedExpr::GreaterThan {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::GreaterThan {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            SimpleTypedExpr::LessThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::LessThanOrEqual {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            SimpleTypedExpr::GreaterThanOrEqual {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::GreaterThanOrEqual {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            TypedExpr::LogicalAnd { left, right, .. } => TypedExpr::LogicalAnd {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                annotation: expr_id,
            },
            TypedExpr::LogicalOr { left, right, .. } => TypedExpr::LogicalOr {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                annotation: expr_id,
            },
            TypedExpr::NumericAdd {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::NumericAdd {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            TypedExpr::NumericSubtract {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::NumericSubtract {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
            TypedExpr::NumericMultiply {
                left,
                right,
                operand_types,
                ..
            } => TypedExpr::NumericMultiply {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
                operand_types,
                annotation: expr_id,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::build_inlined_auto;
    use expect_test::{Expect, expect};

    fn check(entrypoint: InlinedEntrypoint, expected: Expect) {
        let before = entrypoint.to_string();
        let ir = Compiler::compile(entrypoint);
        let after = ir.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_text() {
        check(
            build_inlined_auto("MainComp", vec![], |t| {
                t.text("Hello World");
            }),
            expect![[r#"
                -- before --
                <MainComp>
                  "Hello World"
                </MainComp>

                -- after --
                MainComp() {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{}")
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
                  } else {
                    write("Hello World")
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_text_expression() {
        check(
            build_inlined_auto("MainComp", vec![("name", Type::String)], |t| {
                t.text("Hello ");
                t.text_expr(t.var_expr("name"));
            }),
            expect![[r#"
                -- before --
                <MainComp {name: String}>
                  "Hello "
                  {name}
                </MainComp>

                -- after --
                MainComp(name: String) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write_expr(JsonEncode({name: name}))
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
                  } else {
                    write("Hello ")
                    write_escaped(name)
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_html_element() {
        check(
            build_inlined_auto("MainComp", vec![], |t| {
                t.div(vec![], |t| {
                    t.text("Content");
                });
            }),
            expect![[r#"
                -- before --
                <MainComp>
                  <div>
                    "Content"
                  </div>
                </MainComp>

                -- after --
                MainComp() {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{}")
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
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
    fn test_if_node() {
        check(
            build_inlined_auto("MainComp", vec![("show", Type::Bool)], |t| {
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
                      "Visible"
                    </div>
                  </if>
                </MainComp>

                -- after --
                MainComp(show: Bool) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write_expr(JsonEncode({show: show}))
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
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
    fn test_for_node() {
        check(
            build_inlined_auto(
                "MainComp",
                vec![("items", Type::Array(Some(Box::new(Type::String))))],
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
                    write_expr(JsonEncode({items: items}))
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
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
    fn test_attributes_static() {
        check(
            build_inlined_auto("MainComp", vec![], |t| {
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
                    "Content"
                  </div>
                </MainComp>

                -- after --
                MainComp() {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write("{}")
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
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
    fn test_attributes_dynamic() {
        check(
            build_inlined_auto("MainComp", vec![("cls", Type::String)], |t| {
                t.div(
                    vec![
                        ("class", t.attr_str("base")),
                        ("data-value", t.attr_exprs(vec![t.var_expr("cls")])),
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
                    "Content"
                  </div>
                </MainComp>

                -- after --
                MainComp(cls: String) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write_expr(JsonEncode({cls: cls}))
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
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
    fn test_attributes_multiple_expressions() {
        check(
            build_inlined_auto(
                "MainComp",
                vec![
                    ("base_class", Type::String),
                    ("modifier_class", Type::String),
                    ("state_class", Type::String),
                ],
                |t| {
                    t.div(
                        vec![(
                            "class",
                            t.attr_exprs(vec![
                                t.var_expr("base_class"),
                                t.var_expr("modifier_class"),
                                t.var_expr("state_class"),
                            ]),
                        )],
                        |t| {
                            t.text("Content");
                        },
                    );
                },
            ),
            expect![[r#"
                -- before --
                <MainComp {base_class: String, modifier_class: String, state_class: String}>
                  <div class={base_class, modifier_class, state_class}>
                    "Content"
                  </div>
                </MainComp>

                -- after --
                MainComp(
                  base_class: String,
                  modifier_class: String,
                  state_class: String,
                ) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"MainComp\", \"params\": ")
                    write_expr(JsonEncode({
                      base_class: base_class,
                      modifier_class: modifier_class,
                      state_class: state_class,
                    }))
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
                  } else {
                    write("<div")
                    write(" class=\"")
                    write_escaped(base_class)
                    write(" ")
                    write_escaped(modifier_class)
                    write(" ")
                    write_escaped(state_class)
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
    fn test_development_mode() {
        check(
            build_inlined_auto(
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
                    "Hello "
                    {name}
                    ", count: "
                    {count}
                  </div>
                </TestComp>

                -- after --
                TestComp(name: String, count: String) {
                  if (EnvLookup("HOP_DEV_MODE") == "enabled") {
                    write("<!DOCTYPE html>\n")
                    write("<script type=\"application/json\">{\"module\": \"test\", \"component\": \"TestComp\", \"params\": ")
                    write_expr(JsonEncode({name: name, count: count}))
                    write("}</script>\n<script src=\"http://localhost:33861/development_mode.js\"></script>")
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
}
