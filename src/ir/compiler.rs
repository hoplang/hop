use crate::common::is_void_element;
use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::{self, Argument, Expr};
use crate::hop::ast::{Ast, Attribute, AttributeValue, ComponentDefinition, Node};
use crate::hop::module_name::ModuleName;
use crate::ir::passes::PassManager;
use crate::ir::{BinaryOp, IrEntrypoint, IrExpr, IrModule, IrNode, UnaryOp};
use std::collections::{BTreeMap, HashMap};

pub struct Compiler<'a> {
    asts: &'a HashMap<ModuleName, Ast>,
    ir_module: IrModule,

    // Alpha-renaming state
    var_counter: usize,
    scope_stack: Vec<HashMap<String, String>>, // Stack of scopes (original → renamed)
}

impl Compiler<'_> {
    pub fn compile(asts: &HashMap<ModuleName, Ast>) -> IrModule {
        let mut compiler = Compiler {
            asts,
            ir_module: IrModule::new(),
            var_counter: 0,
            scope_stack: vec![],
        };

        // Compile all entrypoint components
        for ast in asts.values() {
            for component_def in ast.get_component_definitions() {
                if component_def.is_entrypoint {
                    compiler.compile_entrypoint(ast, component_def);
                }
            }
        }

        // Run optimization passes
        let mut pass_manager = PassManager::default_optimization_pipeline();
        pass_manager.run(&mut compiler.ir_module);

        compiler.ir_module
    }

    fn compile_entrypoint(&mut self, ast: &Ast, component: &ComponentDefinition) {
        self.push_scope();

        // Extract and rename parameters
        let mut param_names = Vec::new();
        let mut renamed_params = Vec::new();

        if let Some((params, _)) = &component.params {
            for param in params {
                let original = param.var_name.to_string();
                let renamed = self.bind_var(&original);
                param_names.push(original);
                renamed_params.push(renamed);
            }
        }

        // Compile component body (already has renamed params in scope)
        let body = self.compile_nodes(&component.children, None);

        // Wrap body with parameter bindings
        // For entrypoints, parameters come from outside, so we bind them
        let body_with_bindings = self.create_param_bindings(&param_names, &renamed_params, body);

        self.pop_scope();

        let entrypoint = IrEntrypoint {
            parameters: param_names, // Original names for function signature
            body: body_with_bindings,
        };

        let name = format!("{}/{}", ast.name, component.tag_name.as_str());
        self.ir_module.entry_points.insert(name, entrypoint);
    }

    fn create_param_bindings(
        &self,
        original_params: &[String],
        renamed_params: &[String],
        body: Vec<IrNode>,
    ) -> Vec<IrNode> {
        // For entrypoints, we need to bind renamed params to original param names
        // that will be passed in from outside
        let mut result = body;
        for (original, renamed) in original_params.iter().zip(renamed_params.iter()).rev() {
            result = vec![IrNode::Let {
                var: renamed.clone(),
                value: IrExpr::Variable(original.clone()),
                body: result,
            }];
        }
        result
    }

    fn compile_nodes(&mut self, nodes: &[Node], slot_content: Option<Vec<IrNode>>) -> Vec<IrNode> {
        let mut result = Vec::new();
        for node in nodes {
            self.compile_node(node, slot_content.as_ref(), &mut result);
        }
        result
    }

    fn compile_node(
        &mut self,
        node: &Node,
        slot_content: Option<&Vec<IrNode>>,
        output: &mut Vec<IrNode>,
    ) {
        match node {
            Node::Text { range } => {
                output.push(IrNode::Write(range.as_str().to_string()));
            }

            Node::TextExpression { expression, .. } => {
                output.push(IrNode::WriteExpr {
                    expr: self.rename_expr(expression),
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
                self.push_scope();
                let body = self.compile_nodes(children, slot_content.cloned());
                let renamed_condition = self.rename_expr(condition);
                self.pop_scope();

                output.push(IrNode::If {
                    condition: renamed_condition,
                    body,
                });
            }

            Node::For {
                var_name,
                array_expr,
                children,
                ..
            } => {
                self.push_scope();
                let renamed_var = self.bind_var(var_name.as_str());
                let body = self.compile_nodes(children, slot_content.cloned());
                let renamed_array = self.rename_expr(array_expr);
                self.pop_scope();

                output.push(IrNode::For {
                    var: renamed_var,
                    array: renamed_array,
                    body,
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

            Node::Doctype { .. } => {
                output.push(IrNode::Write("<!DOCTYPE html>".to_string()));
            }

            Node::Placeholder { .. } => {
                // Skip placeholders - they represent errors
            }
        }
    }

    fn compile_html_node(
        &mut self,
        tag_name: &DocumentRange,
        attributes: &BTreeMap<StringSpan, Attribute>,
        children: &[Node],
        slot_content: Option<&Vec<IrNode>>,
        output: &mut Vec<IrNode>,
    ) {
        // Skip style and script tags without src
        if tag_name.as_str() == "style" {
            return;
        }
        if tag_name.as_str() == "script" && !attributes.contains_key("src") {
            return;
        }

        // Build opening tag with static attributes
        let mut open_tag = format!("<{}", tag_name.as_str());

        for (name, attr) in attributes {
            if let Some(val) = &attr.value {
                match val {
                    AttributeValue::String(s) => {
                        open_tag.push_str(&format!(" {}=\"{}\"", name.as_str(), s.as_str()));
                    }
                    AttributeValue::Expression(expr) => {
                        open_tag.push_str(&format!(" {}=\"", name.as_str()));
                        output.push(IrNode::Write(open_tag.clone()));
                        output.push(IrNode::WriteExpr {
                            expr: self.rename_expr(expr),
                            escape: true,
                        });
                        open_tag = String::from("\"");
                    }
                }
            } else {
                // Boolean attribute
                open_tag.push(' ');
                open_tag.push_str(name.as_str());
            }
        }

        open_tag.push('>');
        output.push(IrNode::Write(open_tag));

        // Compile children
        if !is_void_element(tag_name.as_str()) {
            let child_nodes = self.compile_nodes(children, slot_content.cloned());
            output.extend(child_nodes);
            output.push(IrNode::Write(format!("</{}>", tag_name.as_str())));
        }
    }

    fn compile_component_reference(
        &mut self,
        tag_name: &str,
        module: &ModuleName,
        args: Option<&(Vec<Argument>, DocumentRange)>,
        extra_attributes: &BTreeMap<StringSpan, Attribute>,
        children: &[Node],
        output: &mut Vec<IrNode>,
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

        // Build opening tag with data-hop-id
        let mut open_tag = format!(
            "<{} data-hop-id=\"{}/{}\"",
            wrapper_tag,
            module.as_str(),
            tag_name
        );

        // Merge attributes from component definition and reference
        // Use itertools to merge the two sorted maps
        use itertools::{EitherOrBoth, Itertools};

        let merged_attrs = extra_attributes
            .iter()
            .merge_join_by(&component.attributes, |a, b| a.0.cmp(b.0))
            .collect::<Vec<_>>();

        for attr_pair in merged_attrs {
            match attr_pair {
                EitherOrBoth::Both((name, ref_attr), (_, def_attr)) => {
                    // Attribute exists in both reference and definition
                    if name.as_str() == "class" {
                        // Special handling for class attribute - concatenate values
                        open_tag.push_str(" class=\"");

                        // First add the definition's class (if any)
                        let mut has_def_class = false;
                        if let Some(def_val) = &def_attr.value {
                            match def_val {
                                AttributeValue::String(s) => {
                                    open_tag.push_str(s.as_str());
                                    has_def_class = true;
                                }
                                AttributeValue::Expression(expr) => {
                                    // Dynamic class from definition
                                    output.push(IrNode::Write(open_tag.clone()));
                                    output.push(IrNode::WriteExpr {
                                        expr: self.rename_expr(expr),
                                        escape: true,
                                    });
                                    open_tag = String::new();
                                    has_def_class = true;
                                }
                            }
                        }

                        // Then add the reference's class (if any)
                        if let Some(ref_val) = &ref_attr.value {
                            if has_def_class {
                                open_tag.push(' ');
                            }
                            match ref_val {
                                AttributeValue::String(s) => {
                                    open_tag.push_str(s.as_str());
                                }
                                AttributeValue::Expression(expr) => {
                                    // Dynamic class from reference
                                    if !open_tag.is_empty() {
                                        output.push(IrNode::Write(open_tag.clone()));
                                    }
                                    output.push(IrNode::WriteExpr {
                                        expr: self.rename_expr(expr),
                                        escape: true,
                                    });
                                    open_tag = String::new();
                                }
                            }
                        }

                        open_tag.push('"');
                    } else {
                        // For other attributes, reference overrides definition
                        if let Some(ref_val) = &ref_attr.value {
                            match ref_val {
                                AttributeValue::String(s) => {
                                    open_tag.push_str(&format!(
                                        " {}=\"{}\"",
                                        name.as_str(),
                                        s.as_str()
                                    ));
                                }
                                AttributeValue::Expression(expr) => {
                                    open_tag.push_str(&format!(" {}=\"", name.as_str()));
                                    output.push(IrNode::Write(open_tag.clone()));
                                    output.push(IrNode::WriteExpr {
                                        expr: self.rename_expr(expr),
                                        escape: true,
                                    });
                                    open_tag = String::from("\"");
                                }
                            }
                        } else {
                            // Boolean attribute
                            open_tag.push(' ');
                            open_tag.push_str(name.as_str());
                        }
                    }
                }
                EitherOrBoth::Left((name, ref_attr)) => {
                    // Attribute only in reference
                    if let Some(val) = &ref_attr.value {
                        match val {
                            AttributeValue::String(s) => {
                                open_tag.push_str(&format!(
                                    " {}=\"{}\"",
                                    name.as_str(),
                                    s.as_str()
                                ));
                            }
                            AttributeValue::Expression(expr) => {
                                open_tag.push_str(&format!(" {}=\"", name.as_str()));
                                output.push(IrNode::Write(open_tag.clone()));
                                output.push(IrNode::WriteExpr {
                                    expr: self.rename_expr(expr),
                                    escape: true,
                                });
                                open_tag = String::from("\"");
                            }
                        }
                    } else {
                        // Boolean attribute
                        open_tag.push(' ');
                        open_tag.push_str(name.as_str());
                    }
                }
                EitherOrBoth::Right((name, def_attr)) => {
                    // Attribute only in definition
                    if let Some(val) = &def_attr.value {
                        match val {
                            AttributeValue::String(s) => {
                                open_tag.push_str(&format!(
                                    " {}=\"{}\"",
                                    name.as_str(),
                                    s.as_str()
                                ));
                            }
                            AttributeValue::Expression(expr) => {
                                open_tag.push_str(&format!(" {}=\"", name.as_str()));
                                output.push(IrNode::Write(open_tag.clone()));
                                output.push(IrNode::WriteExpr {
                                    expr: self.rename_expr(expr),
                                    escape: true,
                                });
                                open_tag = String::from("\"");
                            }
                        }
                    } else {
                        // Boolean attribute
                        open_tag.push(' ');
                        open_tag.push_str(name.as_str());
                    }
                }
            }
        }

        open_tag.push('>');
        output.push(IrNode::Write(open_tag));

        // Push new scope for component parameters
        self.push_scope();

        // Build bindings for component parameters
        let mut param_bindings = Vec::new();

        if let Some((params, _)) = &component.params {
            for param in params {
                let param_name = param.var_name.to_string();
                let renamed = self.bind_var(&param_name);

                // Find corresponding argument value
                let value = if let Some((args, _)) = args {
                    args.iter()
                        .find(|a| a.var_name.as_str() == param_name)
                        .map(|a| self.rename_expr(&a.var_expr))
                        .unwrap_or_else(|| {
                            panic!(
                                "Missing required parameter '{}' for component '{}' in module '{}'. \
                                This should have been caught by the typechecker.",
                                param_name,
                                tag_name,
                                module.as_str()
                            )
                        })
                } else {
                    panic!(
                        "No arguments provided for component '{}' in module '{}', \
                        but it requires parameter '{}'. \
                        This should have been caught by the typechecker.",
                        tag_name,
                        module.as_str(),
                        param_name
                    )
                };

                param_bindings.push((renamed, value));
            }
        }

        // Compile slot content if needed
        let slot_content = if component.has_slot && !children.is_empty() {
            Some(self.compile_nodes(children, None))
        } else {
            None
        };

        // Compile component body with parameters in scope
        let mut component_body = self.compile_nodes(&component.children, slot_content);

        self.pop_scope();

        // Wrap body with Let bindings (in reverse order for nesting)
        for (var, value) in param_bindings.into_iter().rev() {
            component_body = vec![IrNode::Let {
                var,
                value,
                body: component_body,
            }];
        }

        output.extend(component_body);
        output.push(IrNode::Write(format!("</{}>", wrapper_tag)));
    }

    // Alpha-renaming helpers
    fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn fresh_var(&mut self, name: &str) -> String {
        self.var_counter += 1;
        format!("{}_{}", name, self.var_counter)
    }

    fn bind_var(&mut self, name: &str) -> String {
        let renamed = self.fresh_var(name);
        self.scope_stack
            .last_mut()
            .expect("Scope stack should not be empty")
            .insert(name.to_string(), renamed.clone());
        renamed
    }

    fn lookup_var(&self, name: &str) -> String {
        for scope in self.scope_stack.iter().rev() {
            if let Some(renamed) = scope.get(name) {
                return renamed.clone();
            }
        }
        name.to_string() // Global or undefined
    }

    fn rename_expr(&self, expr: &Expr) -> IrExpr {
        match expr {
            Expr::Variable { value } => {
                let renamed = self.lookup_var(value.as_str());
                IrExpr::Variable(renamed)
            }
            Expr::PropertyAccess {
                object, property, ..
            } => IrExpr::PropertyAccess {
                object: Box::new(self.rename_expr(object)),
                property: property.as_str().to_string(),
            },
            Expr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => {
                let ir_op = match operator {
                    dop::ast::BinaryOp::Equal => BinaryOp::Equal,
                };
                IrExpr::BinaryOp {
                    left: Box::new(self.rename_expr(left)),
                    op: ir_op,
                    right: Box::new(self.rename_expr(right)),
                }
            }
            Expr::UnaryOp {
                operator, operand, ..
            } => {
                let ir_op = match operator {
                    dop::ast::UnaryOp::Not => UnaryOp::Not,
                };
                IrExpr::UnaryOp {
                    op: ir_op,
                    operand: Box::new(self.rename_expr(operand)),
                }
            }
            Expr::ArrayLiteral { elements, .. } => {
                IrExpr::ArrayLiteral(elements.iter().map(|e| self.rename_expr(e)).collect())
            }
            Expr::ObjectLiteral { properties, .. } => IrExpr::ObjectLiteral(
                properties
                    .iter()
                    .map(|(k, v)| (k.as_str().to_string(), self.rename_expr(v)))
                    .collect(),
            ),
            Expr::StringLiteral { value, .. } => IrExpr::StringLiteral(value.to_string()),
            Expr::BooleanLiteral { value, .. } => IrExpr::BooleanLiteral(*value),
            Expr::NumberLiteral { value, .. } => {
                IrExpr::NumberLiteral(value.as_f64().unwrap_or(0.0))
            }
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

    fn compile_hop_to_ir(source: &str) -> IrModule {
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

        // Compile to IR
        let mut asts = HashMap::new();
        asts.insert(module_name.clone(), ast);
        Compiler::compile(&asts)
    }

    fn get_entrypoint(ir_module: &IrModule) -> &IrEntrypoint {
        ir_module
            .entry_points
            .values()
            .next()
            .expect("Should have an entrypoint")
    }

    fn check_ir(source: &str, expected: Expect) {
        let ir_module = compile_hop_to_ir(source);
        let entrypoint = get_entrypoint(&ir_module);
        expected.assert_eq(&entrypoint.to_string());
    }

    #[test]
    fn test_compile_simple_text() {
        check_ir(
            &["<main-comp entrypoint>", "Hello World", "</main-comp>"].join(""),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("Hello World")
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
                IrEntrypoint {
                  parameters: ["name"]
                  body: {
                    Let(var: name_1, value: name) {
                      Write("Hello ")
                      WriteExpr(expr: name_1, escape: true)
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<div>Content</div>")
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
                IrEntrypoint {
                  parameters: ["show"]
                  body: {
                    Let(var: show_1, value: show) {
                      If(condition: show_1) {
                        Write("<div>Visible</div>")
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
                IrEntrypoint {
                  parameters: ["items"]
                  body: {
                    Let(var: items_1, value: items) {
                      For(var: item_2, array: items_1) {
                        Write("<li>")
                        WriteExpr(expr: item_2, escape: true)
                        Write("</li>")
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<div data-hop-id=\"test/card-comp\">")
                    Let(var: title_1, value: "Hello") {
                      Write("<h2>")
                      WriteExpr(expr: title_1, escape: true)
                      Write("</h2>")
                    }
                    Write("</div>")
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<div class=\"base\" id=\"test\">Content</div>")
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
                IrEntrypoint {
                  parameters: ["cls"]
                  body: {
                    Let(var: cls_1, value: cls) {
                      Write("<div class=\"base\" data-value=\"")
                      WriteExpr(expr: cls_1, escape: true)
                      Write("\">Content</div>")
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
                IrEntrypoint {
                  parameters: ["y"]
                  body: {
                    Let(var: y_1, value: y) {
                      For(var: x_2, array: ["a", "b"]) {
                        WriteExpr(expr: x_2, escape: true)
                      }
                      WriteExpr(expr: y_1, escape: true)
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
                "<!doctype html>",
                "<html>Content</html>",
                "</main-comp>",
            ]
            .join(""),
            expect![[r#"
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<!DOCTYPE html><html>Content</html>")
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<img alt=\"test\" src=\"test.jpg\"><br>")
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<div>Before</div><div>After</div>")
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
                IrEntrypoint {
                  parameters: ["user"]
                  body: {
                    Let(var: user_1, value: user) {
                      Write("Hello ")
                      WriteExpr(expr: user_1.name, escape: true)
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<div data-hop-id=\"test/card-comp\"><div class=\"card\"><p>Slot content</p></div></div>")
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
                IrEntrypoint {
                  parameters: []
                  body: {
                    Write("<div data-hop-id=\"test/outer-comp\">")
                    Let(var: text_1, value: "Hello") {
                      Write("<div data-hop-id=\"test/inner-comp\">")
                      Let(var: msg_2, value: text_1) {
                        Write("<span>")
                        WriteExpr(expr: msg_2, escape: true)
                        Write("</span>")
                      }
                      Write("</div>")
                    }
                    Write("</div>")
                  }
                }
            "#]],
        );
    }
}
