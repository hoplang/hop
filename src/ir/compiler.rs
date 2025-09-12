use crate::common::is_void_element;
use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::{Argument, Expr, VarName};
use crate::hop::ast::{Ast, Attribute, AttributeValue, ComponentDefinition, Node};
use crate::hop::module_name::ModuleName;
use crate::ir::{IrEntrypoint, IrModule, IrNode};
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
            scope_stack: vec![HashMap::new()], // Global scope
        };

        // Add globals like HOP_MODE (don't rename these)
        compiler.scope_stack[0].insert("HOP_MODE".to_string(), "HOP_MODE".to_string());

        // Compile all entrypoint components
        for ast in asts.values() {
            for component_def in ast.get_component_definitions() {
                if component_def.is_entrypoint {
                    compiler.compile_entrypoint(ast, component_def);
                }
            }
        }

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
                value: Expr::Variable {
                    value: VarName::new(DocumentRange::new(original.clone())).unwrap(),
                },
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
        let mut has_dynamic_attrs = false;

        for (name, attr) in attributes {
            if let Some(val) = &attr.value {
                match val {
                    AttributeValue::String(s) => {
                        open_tag.push_str(&format!(" {}=\"{}\"", name.as_str(), s.as_str()));
                    }
                    AttributeValue::Expression(expr) => {
                        // Output what we have so far
                        if !has_dynamic_attrs {
                            has_dynamic_attrs = true;
                        }
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

    fn rename_expr(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Variable { value } => {
                let renamed = self.lookup_var(value.as_str());
                Expr::Variable {
                    value: VarName::new(DocumentRange::new(renamed)).unwrap(),
                }
            }
            Expr::PropertyAccess {
                object,
                property,
                range,
            } => Expr::PropertyAccess {
                object: Box::new(self.rename_expr(object)),
                property: property.clone(),
                range: range.clone(),
            },
            Expr::BinaryOp {
                left,
                operator,
                right,
                range,
            } => Expr::BinaryOp {
                left: Box::new(self.rename_expr(left)),
                operator: operator.clone(),
                right: Box::new(self.rename_expr(right)),
                range: range.clone(),
            },
            Expr::UnaryOp {
                operator,
                operand,
                range,
            } => Expr::UnaryOp {
                operator: operator.clone(),
                operand: Box::new(self.rename_expr(operand)),
                range: range.clone(),
            },
            Expr::ArrayLiteral { elements, range } => Expr::ArrayLiteral {
                elements: elements.iter().map(|e| self.rename_expr(e)).collect(),
                range: range.clone(),
            },
            Expr::ObjectLiteral { properties, range } => Expr::ObjectLiteral {
                properties: properties
                    .iter()
                    .map(|(k, v)| (k.clone(), self.rename_expr(v)))
                    .collect(),
                range: range.clone(),
            },
            // Literals don't need renaming
            Expr::StringLiteral { .. }
            | Expr::BooleanLiteral { .. }
            | Expr::NumberLiteral { .. } => expr.clone(),
        }
    }
}
