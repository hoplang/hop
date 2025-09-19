use crate::common::is_void_element;
use crate::document::document_cursor::StringSpan;
use crate::dop::{AnnotatedTypedExpr, TypedExpr};
use crate::dop::{Type, VarName};
use crate::hop::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntryPoint, InlinedNode,
};
use std::collections::BTreeMap;

use super::ast::{ExprId, IrEntrypoint, IrExpr, IrStatement, StatementId};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompilationMode {
    Production,
    Development,
}

pub struct Compiler {
    compilation_mode: CompilationMode,

    // Expression ID generation
    expr_id_counter: u32,

    // Node ID generation
    node_id_counter: u32,
}

impl Compiler {
    pub fn compile(
        entrypoint: InlinedEntryPoint,
        compilation_mode: CompilationMode,
    ) -> IrEntrypoint {
        let mut compiler = Compiler {
            compilation_mode,
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
        let children = entrypoint.children;

        let body = match compiler.compilation_mode {
            CompilationMode::Production => {
                // Compile component body normally for production
                compiler.compile_nodes(children, None)
            }
            CompilationMode::Development => {
                // Generate development mode bootstrap HTML
                let component_name = tag_name.as_str();
                compiler.generate_development_mode_body(component_name, &param_info)
            }
        };

        IrEntrypoint {
            name: tag_name.to_string(),
            parameters: param_info,
            body,
        }
    }

    fn generate_development_mode_body(
        &mut self,
        component_name: &str,
        params: &[(VarName, Type)],
    ) -> Vec<IrStatement> {
        let mut body = Vec::new();

        // Generate the HTML bootstrap
        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<title>".to_string(),
        });

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: format!("{} - Development Mode", component_name),
        });

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "</title>\n</head>\n<body>\n<script id=\"hop-config\" type=\"application/json\">\n{\"entrypoint\": \"".to_string(),
        });

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: component_name.to_string(),
        });

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "\", \"params\": ".to_string(),
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
            for (name, typ) in params {
                props.push((
                    name.to_string(),
                    AnnotatedTypedExpr::Var {
                        value: name.clone(),
                        kind: typ.clone(),
                        annotation: self.next_expr_id(),
                    },
                ));
            }

            body.push(IrStatement::WriteExpr {
                id: self.next_node_id(),
                expr: AnnotatedTypedExpr::JsonEncode {
                    value: Box::new(AnnotatedTypedExpr::ObjectLiteral {
                        properties: props,
                        kind: Type::Object(BTreeMap::new()),
                        annotation: self.next_expr_id(),
                    }),
                    annotation: self.next_expr_id(),
                },
                escape: false,
            });
        }

        body.push(IrStatement::Write {
            id: self.next_node_id(),
            content: "}\n</script>\n<script type=\"module\" src=\"http://localhost:33861/dev.js\"></script>\n</body>\n</html>".to_string(),
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
                output.push(IrStatement::WriteExpr {
                    id: self.next_node_id(),
                    expr: self.compile_expr(expression.clone()),
                    escape: true,
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
        // Skip style and script tags without src
        if tag_name.as_str() == "style" {
            return;
        }
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
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}=\"", name),
                });
                output.push(IrStatement::WriteExpr {
                    id: self.next_node_id(),
                    expr: self.compile_expr(expr),
                    escape: true,
                });
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: "\"".to_string(),
                });
            }
        }
    }

    fn compile_expr(&mut self, expr: TypedExpr) -> IrExpr {
        let expr_id = self.next_expr_id();

        match expr {
            TypedExpr::Var { value, kind, .. } => AnnotatedTypedExpr::Var {
                value,
                kind,
                annotation: expr_id,
            },
            TypedExpr::PropertyAccess {
                object,
                property,
                kind,
                ..
            } => AnnotatedTypedExpr::PropertyAccess {
                object: Box::new(self.compile_expr(*object)),
                property,
                kind,
                annotation: expr_id,
            },
            TypedExpr::BinaryOp {
                left,
                operator,
                right,
                kind,
                ..
            } => AnnotatedTypedExpr::BinaryOp {
                left: Box::new(self.compile_expr(*left)),
                operator,
                right: Box::new(self.compile_expr(*right)),
                kind,
                annotation: expr_id,
            },
            TypedExpr::UnaryOp {
                operator,
                operand,
                kind,
                ..
            } => AnnotatedTypedExpr::UnaryOp {
                operator,
                operand: Box::new(self.compile_expr(*operand)),
                kind,
                annotation: expr_id,
            },
            TypedExpr::ArrayLiteral { elements, kind, .. } => AnnotatedTypedExpr::ArrayLiteral {
                elements: elements.into_iter().map(|e| self.compile_expr(e)).collect(),
                kind,
                annotation: expr_id,
            },
            TypedExpr::ObjectLiteral {
                properties, kind, ..
            } => AnnotatedTypedExpr::ObjectLiteral {
                properties: properties
                    .into_iter()
                    .map(|(k, v)| (k, self.compile_expr(v)))
                    .collect(),
                kind,
                annotation: expr_id,
            },
            TypedExpr::StringLiteral { value, .. } => AnnotatedTypedExpr::StringLiteral {
                value,
                annotation: expr_id,
            },
            TypedExpr::BooleanLiteral { value, .. } => AnnotatedTypedExpr::BooleanLiteral {
                value,
                annotation: expr_id,
            },
            TypedExpr::NumberLiteral { value, .. } => AnnotatedTypedExpr::NumberLiteral {
                value,
                annotation: expr_id,
            },
            TypedExpr::JsonEncode { value, .. } => AnnotatedTypedExpr::JsonEncode {
                value: Box::new(self.compile_expr(*value)),
                annotation: expr_id,
            },
            TypedExpr::StringConcat {
                left, right, ..
            } => AnnotatedTypedExpr::StringConcat {
                left: Box::new(self.compile_expr(*left)),
                right: Box::new(self.compile_expr(*right)),
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

    fn check(entrypoint: InlinedEntryPoint, mode: CompilationMode, expected: Expect) {
        let before = entrypoint.to_string();
        let ir = Compiler::compile(entrypoint, mode);
        let after = ir.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_text() {
        check(
            build_inlined_auto("main-comp", vec![], |t| {
                t.text("Hello World");
            }),
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp>
                  "Hello World"
                </main-comp>

                -- after --
                main-comp() {
                  write("Hello World")
                }
            "#]],
        );
    }

    #[test]
    fn test_text_expression() {
        check(
            build_inlined_auto("main-comp", vec![("name", Type::String)], |t| {
                t.text("Hello ");
                t.text_expr(t.var_expr("name"));
            }),
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp {name: string}>
                  "Hello "
                  {name}
                </main-comp>

                -- after --
                main-comp(name: string) {
                  write("Hello ")
                  write_escaped(name)
                }
            "#]],
        );
    }

    #[test]
    fn test_html_element() {
        check(
            build_inlined_auto("main-comp", vec![], |t| {
                t.div(vec![], |t| {
                    t.text("Content");
                });
            }),
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp>
                  <div>
                    "Content"
                  </div>
                </main-comp>

                -- after --
                main-comp() {
                  write("<div")
                  write(">")
                  write("Content")
                  write("</div>")
                }
            "#]],
        );
    }

    #[test]
    fn test_if_node() {
        check(
            build_inlined_auto("main-comp", vec![("show", Type::Bool)], |t| {
                t.if_node(t.var_expr("show"), |t| {
                    t.div(vec![], |t| {
                        t.text("Visible");
                    });
                });
            }),
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp {show: boolean}>
                  <if {show}>
                    <div>
                      "Visible"
                    </div>
                  </if>
                </main-comp>

                -- after --
                main-comp(show: boolean) {
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
    fn test_for_node() {
        check(
            build_inlined_auto(
                "main-comp",
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
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp {items: array[string]}>
                  <ul>
                    <for {item in items}>
                      <li>
                        {item}
                      </li>
                    </for>
                  </ul>
                </main-comp>

                -- after --
                main-comp(items: array[string]) {
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
    fn test_attributes_static() {
        check(
            build_inlined_auto("main-comp", vec![], |t| {
                t.div(
                    vec![("class", t.attr_str("base")), ("id", t.attr_str("test"))],
                    |t| {
                        t.text("Content");
                    },
                );
            }),
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp>
                  <div class="base" id="test">
                    "Content"
                  </div>
                </main-comp>

                -- after --
                main-comp() {
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
    fn test_attributes_dynamic() {
        check(
            build_inlined_auto("main-comp", vec![("cls", Type::String)], |t| {
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
            CompilationMode::Production,
            expect![[r#"
                -- before --
                <main-comp {cls: string}>
                  <div class="base" data-value={cls}>
                    "Content"
                  </div>
                </main-comp>

                -- after --
                main-comp(cls: string) {
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
    fn test_development_mode() {
        check(
            build_inlined_auto(
                "test-comp",
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
            CompilationMode::Development,
            expect![[r#"
                -- before --
                <test-comp {name: string, count: string}>
                  <div>
                    "Hello "
                    {name}
                    ", count: "
                    {count}
                  </div>
                </test-comp>

                -- after --
                test-comp(name: string, count: string) {
                  write("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<title>")
                  write("test-comp - Development Mode")
                  write("</title>\n</head>\n<body>\n<script id=\"hop-config\" type=\"application/json\">\n{\"entrypoint\": \"")
                  write("test-comp")
                  write("\", \"params\": ")
                  write_expr(JsonEncode({name: name, count: count}))
                  write("}\n</script>\n<script type=\"module\" src=\"http://localhost:33861/dev.js\"></script>\n</body>\n</html>")
                }
            "#]],
        );
    }
}
