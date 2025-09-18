use crate::common::is_void_element;
use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::Expr;
use crate::dop::expr::TypedExpr;
use crate::dop::{Type, VarName};
use crate::hop::ast::{AttributeValue, InlinedComponentDefinition, TypedAttribute};
use crate::hop::node::{InlinedNode, Node};
use crate::hop::transforms::TransformPipeline;
use std::collections::BTreeMap;

use super::ast::{ExprId, IrEntrypoint, IrExpr, IrModule, IrStatement, StatementId};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompilationMode {
    Production,
    Development,
}

pub struct Compiler {
    ir_module: IrModule,
    compilation_mode: CompilationMode,

    // Expression ID generation
    expr_id_counter: u32,

    // Node ID generation
    node_id_counter: u32,
}

impl Compiler {
    pub fn compile(
        entrypoints: Vec<InlinedComponentDefinition>,
        compilation_mode: CompilationMode,
    ) -> IrModule {
        // Clone entrypoints for transformation (keeping originals intact)
        let mut transformed_entrypoints = entrypoints;

        // Apply transformations only in production mode
        if compilation_mode == CompilationMode::Production {
            let mut pipeline = TransformPipeline::new();
            for component in &mut transformed_entrypoints {
                // Apply transformations to each component
                pipeline.run(component);
            }
        }

        let mut compiler = Compiler {
            ir_module: IrModule::new(),
            compilation_mode,
            expr_id_counter: 0,
            node_id_counter: 0,
        };

        // Compile all entrypoint components
        for component_def in &transformed_entrypoints {
            compiler.compile_entrypoint(component_def);
        }

        compiler.ir_module
    }

    fn compile_entrypoint(&mut self, component: &InlinedComponentDefinition) {
        // Extract parameter information
        let param_info = component
            .params
            .as_ref()
            .map(|(params, _)| {
                params
                    .iter()
                    .map(|param| (param.var_name.clone(), param.var_type.clone()))
                    .collect()
            })
            .unwrap_or_else(Vec::new);

        let body = match self.compilation_mode {
            CompilationMode::Production => {
                // Compile component body normally for production
                self.compile_nodes(&component.children, None)
            }
            CompilationMode::Development => {
                // Generate development mode bootstrap HTML
                let component_name = component.tag_name.as_str();
                self.generate_development_mode_body(component_name, &param_info)
            }
        };

        let entrypoint = IrEntrypoint {
            parameters: param_info,
            body,
        };

        self.ir_module
            .entry_points
            .insert(component.tag_name.as_str().to_string(), entrypoint);
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
                    Expr::Var {
                        value: name.clone(),
                        annotation: (self.next_expr_id(), typ.clone()),
                    },
                ));
            }

            body.push(IrStatement::WriteExpr {
                id: self.next_node_id(),
                expr: Expr::JsonEncode {
                    value: Box::new(Expr::ObjectLiteral {
                        properties: props,
                        annotation: (
                            self.next_expr_id(),
                            // TODO: Do we need to construct the correct type here?
                            Type::Object(BTreeMap::new()),
                        ),
                    }),
                    annotation: (self.next_expr_id(), Type::String),
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
        nodes: &[InlinedNode],
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
        node: &InlinedNode,
        slot_content: Option<&Vec<IrStatement>>,
        output: &mut Vec<IrStatement>,
    ) {
        match node {
            Node::Text { value, .. } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: value.to_string(),
                });
            }

            Node::TextExpression { expression, .. } => {
                output.push(IrStatement::WriteExpr {
                    id: self.next_node_id(),
                    expr: self.compile_expr(expression),
                    escape: true,
                });
            }

            Node::Html {
                tag_name,
                attributes,
                children,
                ..
            } => {
                self.compile_html_node(tag_name, attributes, children, slot_content, output);
            }

            Node::If {
                condition,
                children,
                ..
            } => {
                output.push(IrStatement::If {
                    id: self.next_node_id(),
                    condition: self.compile_expr(condition),
                    body: self.compile_nodes(children, slot_content.cloned()),
                });
            }

            Node::For {
                var_name,
                array_expr,
                children,
                ..
            } => {
                output.push(IrStatement::For {
                    id: self.next_node_id(),
                    var: var_name.clone(),
                    array: self.compile_expr(array_expr),
                    body: self.compile_nodes(children, slot_content.cloned()),
                });
            }

            Node::SlotDefinition { .. } => {
                if let Some(content) = slot_content {
                    output.extend_from_slice(content);
                }
            }

            Node::ComponentReference { tag_name, .. } => {
                panic!(
                    "Unexpected ComponentReference '{}' in IR compiler. Components should be inlined before compilation.",
                    tag_name.as_str()
                );
            }

            Node::Doctype { value, .. } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: value.to_string(),
                });
            }

            Node::Let {
                var,
                value,
                children,
                ..
            } => {
                output.push(IrStatement::Let {
                    id: self.next_node_id(),
                    var: var.clone(),
                    value: self.compile_expr(value),
                    body: self.compile_nodes(children, slot_content.cloned()),
                });
            }

            Node::Placeholder { .. } => {
                panic!("Found placeholder node in IR compiler")
            }
        }
    }

    fn compile_html_node(
        &mut self,
        tag_name: &DocumentRange,
        attributes: &BTreeMap<StringSpan, TypedAttribute>,
        children: &[InlinedNode],
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
            if let Some(val) = &attr.value {
                self.compile_attribute(name.as_str(), val, output);
            } else {
                // Boolean attribute
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}", name.as_str()),
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
        name: &str,
        value: &AttributeValue<TypedExpr>,
        output: &mut Vec<IrStatement>,
    ) {
        match value {
            AttributeValue::String(s) => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: format!(" {}=\"{}\"", name, s.as_str()),
                });
            }
            AttributeValue::Expression(expr) => {
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

    fn compile_expr(&mut self, expr: &TypedExpr) -> IrExpr {
        let annotation = (self.next_expr_id(), expr.annotation().clone());

        match expr {
            Expr::Var { value, .. } => Expr::Var {
                value: value.clone(),
                annotation,
            },
            Expr::PropertyAccess {
                object, property, ..
            } => Expr::PropertyAccess {
                object: Box::new(self.compile_expr(object)),
                property: property.as_str().to_string(),
                annotation,
            },
            Expr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => Expr::BinaryOp {
                left: Box::new(self.compile_expr(left)),
                operator: operator.clone(),
                right: Box::new(self.compile_expr(right)),
                annotation,
            },
            Expr::UnaryOp {
                operator, operand, ..
            } => Expr::UnaryOp {
                operator: operator.clone(),
                operand: Box::new(self.compile_expr(operand)),
                annotation,
            },
            Expr::ArrayLiteral { elements, .. } => Expr::ArrayLiteral {
                elements: elements.iter().map(|e| self.compile_expr(e)).collect(),
                annotation,
            },
            Expr::ObjectLiteral { properties, .. } => Expr::ObjectLiteral {
                properties: properties
                    .iter()
                    .map(|(k, v)| (k.as_str().to_string(), self.compile_expr(v)))
                    .collect(),
                annotation,
            },
            Expr::StringLiteral { value, .. } => Expr::StringLiteral {
                value: value.to_string(),
                annotation,
            },
            Expr::BooleanLiteral { value, .. } => Expr::BooleanLiteral {
                value: *value,
                annotation,
            },
            Expr::NumberLiteral { value, .. } => Expr::NumberLiteral {
                value: value.clone(),
                annotation,
            },
            Expr::JsonEncode { value, .. } => Expr::JsonEncode {
                value: Box::new(self.compile_expr(value)),
                annotation,
            },
            Expr::StringConcat { left, right, .. } => Expr::StringConcat {
                left: Box::new(self.compile_expr(left)),
                right: Box::new(self.compile_expr(right)),
                annotation,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error_collector::ErrorCollector;
    use crate::hop::module_name::ModuleName;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::type_checker::TypeChecker;
    use expect_test::{Expect, expect};

    fn compile_hop_to_ir(source: &str, mode: CompilationMode) -> IrModule {
        let mut errors = ErrorCollector::new();
        let module_name = ModuleName::new("test".to_string()).unwrap();
        let tokenizer = Tokenizer::new(source.to_string());
        let ast = parse(module_name.clone(), tokenizer, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Type check
        let mut typechecker = TypeChecker::default();
        typechecker.typecheck(&[&ast]);
        assert!(
            typechecker
                .type_errors
                .get(&module_name)
                .unwrap()
                .is_empty(),
            "Type errors: {:?}",
            typechecker.type_errors
        );

        // Inline entrypoint components
        let inlined_entrypoints =
            crate::hop::inliner::Inliner::inline_entrypoints(typechecker.typed_asts);

        // Compile to IR with specified mode
        Compiler::compile(inlined_entrypoints, mode)
    }

    fn check_ir(source: &str, expected: Expect) {
        let ir_module = compile_hop_to_ir(source, CompilationMode::Production);
        expected.assert_eq(&ir_module.to_string());
    }

    fn check_ir_with_mode(source: &str, mode: CompilationMode, expected: Expect) {
        let ir_module = compile_hop_to_ir(source, mode);
        expected.assert_eq(&ir_module.to_string());
    }

    #[test]
    fn test_compile_simple_text() {
        check_ir(
            &["<main-comp entrypoint>", "Hello World", "</main-comp>"].join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("Hello World")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_text_expression() {
        check_ir(
            &[
                "<main-comp entrypoint {name: string}>",
                "Hello {name}",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [name: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("Hello ")
                        WriteExpr(expr: name, escape: true)
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_html_element() {
        check_ir(
            &[
                "<main-comp entrypoint>",
                "<div>",
                "Content",
                "</div>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(">")
                        Write("Content")
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_if_statement() {
        check_ir(
            &[
                "<main-comp entrypoint {show: boolean}>",
                "<if {show}>",
                "<div>Visible</div>",
                "</if>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [show: boolean]
                      body: {
                        Write("<!DOCTYPE html>")
                        If(condition: show) {
                          Write("<div")
                          Write(">")
                          Write("Visible")
                          Write("</div>")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_for_loop() {
        check_ir(
            &[
                "<main-comp entrypoint {items: array[string]}>",
                "<for {item in items}>",
                "<li>{item}</li>",
                "</for>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [items: array[string]]
                      body: {
                        Write("<!DOCTYPE html>")
                        For(var: item, array: items) {
                          Write("<li")
                          Write(">")
                          WriteExpr(expr: item, escape: true)
                          Write("</li>")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_component_reference() {
        check_ir(
            &[
                "<card-comp {title: string}>",
                "<h2>{title}</h2>",
                "</card-comp>",
                "",
                "<main-comp entrypoint>",
                "<card-comp {title: \"Hello\"}/>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" data-hop-id=\"test/card-comp\"")
                        Write(">")
                        Let(var: title, value: "Hello") {
                          Write("<h2")
                          Write(">")
                          WriteExpr(expr: title, escape: true)
                          Write("</h2>")
                        }
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_attributes_static() {
        check_ir(
            &[
                "<main-comp entrypoint>",
                "<div class=\"base\" id=\"test\">Content</div>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" class=\"base\"")
                        Write(" id=\"test\"")
                        Write(">")
                        Write("Content")
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_compile_attributes_dynamic() {
        check_ir(
            &[
                "<main-comp entrypoint {cls: string}>",
                "<div class=\"base\" data-value={cls}>Content</div>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [cls: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" class=\"base\"")
                        Write(" data-value=\"")
                        WriteExpr(expr: cls, escape: true)
                        Write("\"")
                        Write(">")
                        Write("Content")
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_alpha_renaming_nested_scopes() {
        check_ir(
            &[
                "<main-comp entrypoint {y: string}>",
                "<for {x in [\"a\", \"b\"]}>",
                "{x}",
                "</for>",
                "{y}",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [y: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        For(var: x, array: ["a", "b"]) {
                          WriteExpr(expr: x, escape: true)
                        }
                        WriteExpr(expr: y, escape: true)
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_doctype_already_present() {
        // When DOCTYPE is already present, it should not be duplicated
        check_ir(
            &[
                "<main-comp entrypoint>",
                "<!DOCTYPE html>",
                "<html>Content</html>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<html")
                        Write(">")
                        Write("Content")
                        Write("</html>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_doctype_injection() {
        // When DOCTYPE is missing, it should be injected
        check_ir(
            &[
                "<main-comp entrypoint>",
                "<html>Content</html>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<html")
                        Write(">")
                        Write("Content")
                        Write("</html>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_void_elements() {
        check_ir(
            &[
                "<main-comp entrypoint>",
                "<img src=\"test.jpg\" alt=\"test\">",
                "<br>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<img")
                        Write(" alt=\"test\"")
                        Write(" src=\"test.jpg\"")
                        Write(">")
                        Write("<br")
                        Write(">")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_skip_style_and_script_tags() {
        check_ir(
            &[
                "<main-comp entrypoint>",
                "<div>Before</div>",
                "<style>body { color: red; }</style>",
                "<script>console.log(\"test\");</script>",
                "<div>After</div>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(">")
                        Write("Before")
                        Write("</div>")
                        Write("<div")
                        Write(">")
                        Write("After")
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_property_access_renaming() {
        check_ir(
            &[
                "<main-comp entrypoint {user: {name: string}}>",
                "Hello {user.name}",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [user: {name: string}]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("Hello ")
                        WriteExpr(expr: user.name, escape: true)
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_component_with_slot() {
        check_ir(
            &[
                "<card-comp>",
                "<div class=\"card\">",
                "<slot-default/>",
                "</div>",
                "</card-comp>",
                "",
                "<main-comp entrypoint>",
                "<card-comp>",
                "<p>Slot content</p>",
                "</card-comp>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" data-hop-id=\"test/card-comp\"")
                        Write(">")
                        Write("<div")
                        Write(" class=\"card\"")
                        Write(">")
                        Write("<p")
                        Write(">")
                        Write("Slot content")
                        Write("</p>")
                        Write("</div>")
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_components_with_parameters() {
        check_ir(
            &[
                "<inner-comp {msg: string}>",
                "<span>{msg}</span>",
                "</inner-comp>",
                "",
                "<outer-comp {text: string}>",
                "<inner-comp {msg: text}/>",
                "</outer-comp>",
                "",
                "<main-comp entrypoint>",
                "<outer-comp {text: \"Hello\"}/>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" data-hop-id=\"test/outer-comp\"")
                        Write(">")
                        Let(var: text, value: "Hello") {
                          Write("<div")
                          Write(" data-hop-id=\"test/inner-comp\"")
                          Write(">")
                          Let(var: msg, value: text) {
                            Write("<span")
                            Write(">")
                            WriteExpr(expr: msg, escape: true)
                            Write("</span>")
                          }
                          Write("</div>")
                        }
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_entrypoint_passes_parameter_to_component_with_same_name() {
        check_ir(
            &[
                "<child-comp {x: string}>",
                "<div>Value: {x}</div>",
                "</child-comp>",
                "",
                "<main-comp entrypoint {x: string}>",
                "<child-comp {x: x}/>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [x: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" data-hop-id=\"test/child-comp\"")
                        Write(">")
                        Let(var: x, value: x) {
                          Write("<div")
                          Write(">")
                          Write("Value: ")
                          WriteExpr(expr: x, escape: true)
                          Write("</div>")
                        }
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_entrypoint_passes_parameter_to_component_with_same_name_twice() {
        check_ir(
            &[
                "<child-comp {x: string}>",
                "<div>Value: {x}</div>",
                "</child-comp>",
                "",
                "<main-comp entrypoint {x: string}>",
                "<child-comp {x: x}/>",
                "<child-comp {x: x}/>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: [x: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(" data-hop-id=\"test/child-comp\"")
                        Write(">")
                        Let(var: x, value: x) {
                          Write("<div")
                          Write(">")
                          Write("Value: ")
                          WriteExpr(expr: x, escape: true)
                          Write("</div>")
                        }
                        Write("</div>")
                        Write("<div")
                        Write(" data-hop-id=\"test/child-comp\"")
                        Write(">")
                        Let(var: x, value: x) {
                          Write("<div")
                          Write(">")
                          Write("Value: ")
                          WriteExpr(expr: x, escape: true)
                          Write("</div>")
                        }
                        Write("</div>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_multiple_entrypoints() {
        // Test that IrModule properly displays multiple entrypoints
        check_ir(
            &[
                "<first-comp entrypoint {x: string}>",
                "<div>{x}</div>",
                "</first-comp>",
                "",
                "<second-comp entrypoint {y: string}>",
                "<span>Value: {y}</span>",
                "</second-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    first-comp: {
                      parameters: [x: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<div")
                        Write(">")
                        WriteExpr(expr: x, escape: true)
                        Write("</div>")
                      }
                    }
                    second-comp: {
                      parameters: [y: string]
                      body: {
                        Write("<!DOCTYPE html>")
                        Write("<span")
                        Write(">")
                        Write("Value: ")
                        WriteExpr(expr: y, escape: true)
                        Write("</span>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_variable_reuse_bug_in_sibling_scopes() {
        // This test demonstrates the bug where variables can be incorrectly reused
        // in sibling scopes after a scope is popped
        check_ir(
            &[
                "<main-comp entrypoint>",
                // First for loop introduces 'x'
                "<for {x in [\"a\", \"b\"]}>",
                "<div>{x}</div>",
                "</for>",
                // Second for loop also introduces 'x' - should be renamed!
                // But with the current bug, it might reuse 'x' since the first scope was popped
                "<for {x in [\"c\", \"d\"]}>",
                "<span>{x}</span>",
                "</for>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrModule {
                  entry_points: {
                    main-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>")
                        For(var: x, array: ["a", "b"]) {
                          Write("<div")
                          Write(">")
                          WriteExpr(expr: x, escape: true)
                          Write("</div>")
                        }
                        For(var: x, array: ["c", "d"]) {
                          Write("<span")
                          Write(">")
                          WriteExpr(expr: x, escape: true)
                          Write("</span>")
                        }
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_development_mode_compilation() {
        check_ir_with_mode(
            &[
                "<test-comp entrypoint {name: string, count: string}>",
                "<div>Hello {name}, count: {count}</div>",
                "</test-comp>",
            ]
            .join(""),
            CompilationMode::Development,
            expect![[r#"
                IrModule {
                  entry_points: {
                    test-comp: {
                      parameters: [name: string, count: string]
                      body: {
                        Write("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<title>")
                        Write("test-comp - Development Mode")
                        Write("</title>\n</head>\n<body>\n<script id=\"hop-config\" type=\"application/json\">\n{\"entrypoint\": \"")
                        Write("test-comp")
                        Write("\", \"params\": ")
                        WriteExpr(expr: JsonEncode({name: name, count: count}), escape: false)
                        Write("}\n</script>\n<script type=\"module\" src=\"http://localhost:33861/dev.js\"></script>\n</body>\n</html>")
                      }
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn test_development_mode_no_params() {
        check_ir_with_mode(
            &[
                "<simple-comp entrypoint>",
                "<div>Hello World</div>",
                "</simple-comp>",
            ]
            .join(""),
            CompilationMode::Development,
            expect![[r#"
                IrModule {
                  entry_points: {
                    simple-comp: {
                      parameters: []
                      body: {
                        Write("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n<title>")
                        Write("simple-comp - Development Mode")
                        Write("</title>\n</head>\n<body>\n<script id=\"hop-config\" type=\"application/json\">\n{\"entrypoint\": \"")
                        Write("simple-comp")
                        Write("\", \"params\": ")
                        Write("{}")
                        Write("}\n</script>\n<script type=\"module\" src=\"http://localhost:33861/dev.js\"></script>\n</body>\n</html>")
                      }
                    }
                  }
                }
            "#]],
        );
    }
}
