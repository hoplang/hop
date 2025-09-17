use crate::common::is_void_element;
use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::Type;
use crate::dop::{self, Argument, Expr};
use crate::hop::ast::{Ast, Attribute, AttributeValue, ComponentDefinition, Node};
use crate::hop::module_name::ModuleName;
use std::collections::{BTreeMap, HashMap};

use super::ast::{
    BinaryOp, ExprId, IrEntrypoint, IrExpr, IrExprValue, IrModule, IrStatement, StatementId,
    UnaryOp,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompilationMode {
    Production,
    Development,
}

pub struct Compiler<'a> {
    asts: &'a HashMap<ModuleName, Ast<Type>>,
    ir_module: IrModule,
    compilation_mode: CompilationMode,

    // Expression ID generation
    expr_id_counter: u32,

    // Node ID generation
    node_id_counter: u32,
}

impl Compiler<'_> {
    pub fn compile(
        asts: &HashMap<ModuleName, Ast<Type>>,
        compilation_mode: CompilationMode,
    ) -> IrModule {
        let mut compiler = Compiler {
            asts,
            ir_module: IrModule::new(),
            compilation_mode,
            expr_id_counter: 0,
            node_id_counter: 0,
        };

        // Compile all entrypoint components
        for ast in asts.values() {
            for component_def in ast.get_component_definitions() {
                if component_def.is_entrypoint {
                    compiler.compile_entrypoint(component_def);
                }
            }
        }

        // Apply alpha renaming as a separate pass
        let renamer = super::alpha_renaming::AlphaRenamer::new();
        renamer.rename_module(compiler.ir_module)
    }

    fn compile_entrypoint(&mut self, component: &ComponentDefinition<Type>) {
        // Extract parameter information
        let param_info = component
            .params
            .as_ref()
            .map(|(params, _)| {
                params
                    .iter()
                    .map(|param| (param.var_name.to_string(), param.var_type.clone()))
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
                self.generate_development_body(component_name, &param_info)
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

    fn generate_development_body(
        &mut self,
        component_name: &str,
        params: &[(String, Type)],
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
                    name.clone(),
                    IrExpr {
                        id: self.next_expr_id(),
                        value: IrExprValue::Var(name.clone()),
                        typ: typ.clone(),
                    },
                ));
            }

            body.push(IrStatement::WriteExpr {
                id: self.next_node_id(),
                expr: IrExpr {
                    id: self.next_expr_id(),
                    value: IrExprValue::JsonEncode {
                        value: Box::new(IrExpr {
                            id: self.next_expr_id(),
                            value: IrExprValue::ObjectLiteral(props),
                            // TODO: Do we need to construct the correct type here?
                            typ: Type::Object(BTreeMap::new()),
                        }),
                    },
                    typ: Type::String,
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
        nodes: &[Node<Type>],
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
        node: &Node<Type>,
        slot_content: Option<&Vec<IrStatement>>,
        output: &mut Vec<IrStatement>,
    ) {
        match node {
            Node::Text { range } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: range.as_str().to_string(),
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
                    var: var_name.as_str().to_string(),
                    array: self.compile_expr(array_expr),
                    body: self.compile_nodes(children, slot_content.cloned()),
                });
            }

            Node::SlotDefinition { .. } => {
                if let Some(content) = slot_content {
                    output.extend_from_slice(content);
                }
            }

            Node::ComponentReference {
                tag_name,
                definition_module,
                args,
                attributes,
                children,
                ..
            } => {
                self.compile_component_reference(
                    tag_name.as_str(),
                    definition_module
                        .as_ref()
                        .expect("Component reference should have module"),
                    args.as_ref(),
                    attributes,
                    children,
                    output,
                );
            }

            Node::Doctype { range } => {
                output.push(IrStatement::Write {
                    id: self.next_node_id(),
                    content: range.to_string(),
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
        attributes: &BTreeMap<StringSpan, Attribute<Type>>,
        children: &[Node<Type>],
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
                match val {
                    AttributeValue::String(s) => {
                        output.push(IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}=\"{}\"", name.as_str(), s.as_str()),
                        });
                    }
                    AttributeValue::Expression(expr) => {
                        output.push(IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}=\"", name.as_str()),
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

    fn compile_component_reference(
        &mut self,
        tag_name: &str,
        module: &ModuleName,
        args: Option<&(Vec<Argument<Type>>, DocumentRange)>,
        extra_attributes: &BTreeMap<StringSpan, Attribute<Type>>,
        children: &[Node<Type>],
        output: &mut Vec<IrStatement>,
    ) {
        let ast = self
            .asts
            .get(module)
            .expect("Component module should exist");
        let component = ast
            .get_component_definition(tag_name)
            .expect("Component definition should exist");

        // Determine wrapper tag
        let wrapper_tag = component
            .as_attr
            .as_ref()
            .map(|a| a.value.as_str())
            .unwrap_or("div");

        // Push opening tag with data-hop-id
        output.push(IrStatement::Write {
            id: self.next_node_id(),
            content: format!(
                "<{} data-hop-id=\"{}/{}\"",
                wrapper_tag,
                module.as_str(),
                tag_name
            ),
        });

        // Merge attributes from component definition and reference
        // Use itertools to merge the two sorted maps
        use itertools::{EitherOrBoth, Itertools};

        let merged_attrs = extra_attributes
            .iter()
            .merge_join_by(&component.attributes, |a, b| a.0.cmp(b.0))
            .collect::<Vec<_>>();

        for attr_pair in merged_attrs {
            match attr_pair {
                EitherOrBoth::Both((name, ref_attr), (_, def_attr)) if name.as_str() == "class" => {
                    // Special handling for class attribute - concatenate values
                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
                        content: " class=\"".to_string(),
                    });

                    // First add the definition's class (if any)
                    if let Some(def_val) = &def_attr.value {
                        match def_val {
                            AttributeValue::String(s) => {
                                output.push(IrStatement::Write {
                                    id: self.next_node_id(),
                                    content: s.as_str().to_string(),
                                });
                            }
                            AttributeValue::Expression(expr) => {
                                // Dynamic class from definition
                                output.push(IrStatement::WriteExpr {
                                    id: self.next_node_id(),
                                    expr: self.compile_expr(expr),
                                    escape: true,
                                });
                            }
                        }
                    }

                    // Then add the reference's class (if any)
                    if let Some(ref_val) = &ref_attr.value {
                        if def_attr.value.is_some() {
                            output.push(IrStatement::Write {
                                id: self.next_node_id(),
                                content: " ".to_string(),
                            });
                        }
                        match ref_val {
                            AttributeValue::String(s) => {
                                output.push(IrStatement::Write {
                                    id: self.next_node_id(),
                                    content: s.as_str().to_string(),
                                });
                            }
                            AttributeValue::Expression(expr) => {
                                // Dynamic class from reference
                                output.push(IrStatement::WriteExpr {
                                    id: self.next_node_id(),
                                    expr: self.compile_expr(expr),
                                    escape: true,
                                });
                            }
                        }
                    }

                    output.push(IrStatement::Write {
                        id: self.next_node_id(),
                        content: "\"".to_string(),
                    });
                }
                EitherOrBoth::Both((name, attr), (_, _))
                | EitherOrBoth::Left((name, attr))
                | EitherOrBoth::Right((name, attr)) => {
                    if let Some(val) = &attr.value {
                        match val {
                            AttributeValue::String(s) => {
                                output.push(IrStatement::Write {
                                    id: self.next_node_id(),
                                    content: format!(" {}=\"{}\"", name.as_str(), s.as_str()),
                                });
                            }
                            AttributeValue::Expression(expr) => {
                                output.push(IrStatement::Write {
                                    id: self.next_node_id(),
                                    content: format!(" {}=\"", name.as_str()),
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
                    } else {
                        // Boolean attribute
                        output.push(IrStatement::Write {
                            id: self.next_node_id(),
                            content: format!(" {}", name.as_str()),
                        });
                    }
                }
            }
        }

        output.push(IrStatement::Write {
            id: self.next_node_id(),
            content: ">".to_string(),
        });

        // Build bindings for component parameters
        let mut param_bindings = Vec::new();

        if let Some((params, _)) = &component.params {
            for param in params {
                let param_name = param.var_name.to_string();

                // Find corresponding argument value
                let value = if let Some((args, _)) = args {
                    args.iter()
                        .find(|a| a.var_name.as_str() == param_name)
                        .map(|a| self.compile_expr(&a.var_expr))
                        .unwrap_or_else(|| {
                            panic!(
                                "Missing required parameter '{}' for component '{}' in module '{}'.",
                                param_name,
                                tag_name,
                                module.as_str()
                            )
                        })
                } else {
                    panic!(
                        "No arguments provided for component '{}' in module '{}', but it requires parameter '{}'.",
                        tag_name,
                        module.as_str(),
                        param_name
                    )
                };

                param_bindings.push((param_name, value));
            }
        }

        // Compile slot content if needed
        let slot_content = if component.has_slot && !children.is_empty() {
            Some(self.compile_nodes(children, None))
        } else {
            None
        };

        // Compile component body
        let mut component_body = self.compile_nodes(&component.children, slot_content);

        // Wrap body with Let bindings (in reverse order for nesting)
        for (param_name, value) in param_bindings.into_iter().rev() {
            component_body = vec![IrStatement::Let {
                id: self.next_node_id(),
                var: param_name,
                value,
                body: component_body,
            }];
        }

        output.extend(component_body);
        output.push(IrStatement::Write {
            id: self.next_node_id(),
            content: format!("</{}>", wrapper_tag),
        });
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

    fn compile_expr(&mut self, expr: &Expr<Type>) -> IrExpr {
        let value = match expr {
            Expr::Variable { value, .. } => IrExprValue::Var(value.as_str().to_string()),
            Expr::PropertyAccess {
                object, property, ..
            } => IrExprValue::PropertyAccess {
                object: Box::new(self.compile_expr(object)),
                property: property.as_str().to_string(),
            },
            Expr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => {
                let ir_op = match operator {
                    dop::ast::BinaryOp::Equal => BinaryOp::Eq,
                };
                IrExprValue::BinaryOp {
                    left: Box::new(self.compile_expr(left)),
                    op: ir_op,
                    right: Box::new(self.compile_expr(right)),
                }
            }
            Expr::UnaryOp {
                operator, operand, ..
            } => {
                let ir_op = match operator {
                    dop::ast::UnaryOp::Not => UnaryOp::Not,
                };
                IrExprValue::UnaryOp {
                    op: ir_op,
                    operand: Box::new(self.compile_expr(operand)),
                }
            }
            Expr::ArrayLiteral { elements, .. } => {
                IrExprValue::ArrayLiteral(elements.iter().map(|e| self.compile_expr(e)).collect())
            }
            Expr::ObjectLiteral { properties, .. } => IrExprValue::ObjectLiteral(
                properties
                    .iter()
                    .map(|(k, v)| (k.as_str().to_string(), self.compile_expr(v)))
                    .collect(),
            ),
            Expr::StringLiteral { value, .. } => IrExprValue::StringLiteral(value.to_string()),
            Expr::BooleanLiteral { value, .. } => IrExprValue::BooleanLiteral(*value),
            Expr::NumberLiteral { value, .. } => {
                IrExprValue::NumberLiteral(value.as_f64().unwrap_or(0.0))
            }
        };

        IrExpr {
            id: self.next_expr_id(),
            value,
            typ: expr.get_type(),
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
    use crate::hop::typechecker::TypeChecker;
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

        // Compile to IR with specified mode using typed AST
        Compiler::compile(&typechecker.typed_asts, mode)
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
                        Write("<div data-hop-id=\"test/card-comp\"")
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
    fn test_doctype_compilation() {
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
                        Write("<div data-hop-id=\"test/card-comp\"")
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
                        Write("<div data-hop-id=\"test/outer-comp\"")
                        Write(">")
                        Let(var: text, value: "Hello") {
                          Write("<div data-hop-id=\"test/inner-comp\"")
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
                        Write("<div data-hop-id=\"test/child-comp\"")
                        Write(">")
                        Let(var: x_1, value: x) {
                          Write("<div")
                          Write(">")
                          Write("Value: ")
                          WriteExpr(expr: x_1, escape: true)
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
                        Write("<div data-hop-id=\"test/child-comp\"")
                        Write(">")
                        Let(var: x_1, value: x) {
                          Write("<div")
                          Write(">")
                          Write("Value: ")
                          WriteExpr(expr: x_1, escape: true)
                          Write("</div>")
                        }
                        Write("</div>")
                        Write("<div data-hop-id=\"test/child-comp\"")
                        Write(">")
                        Let(var: x_2, value: x) {
                          Write("<div")
                          Write(">")
                          Write("Value: ")
                          WriteExpr(expr: x_2, escape: true)
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
                        Write("<div")
                        Write(">")
                        WriteExpr(expr: x, escape: true)
                        Write("</div>")
                      }
                    }
                    second-comp: {
                      parameters: [y: string]
                      body: {
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
                        For(var: x, array: ["a", "b"]) {
                          Write("<div")
                          Write(">")
                          WriteExpr(expr: x, escape: true)
                          Write("</div>")
                        }
                        For(var: x_1, array: ["c", "d"]) {
                          Write("<span")
                          Write(">")
                          WriteExpr(expr: x_1, escape: true)
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
