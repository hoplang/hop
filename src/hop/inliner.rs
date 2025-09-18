use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::expr::TypedExpr;
use crate::dop::{Argument, Expr, Type};
use crate::hop::ast::{Ast, Attribute, AttributeValue, ComponentDefinition, Node};
use crate::hop::module_name::ModuleName;
use std::collections::{BTreeMap, HashMap};

/// The Inliner transforms ASTs by replacing ComponentReference nodes with their
/// inlined component definitions, using Let nodes for parameter binding and
/// StringConcat for class attribute merging.
pub struct Inliner<'a> {
    asts: &'a HashMap<ModuleName, Ast<TypedExpr>>,
}

impl<'a> Inliner<'a> {
    /// Create a new Inliner with the given ASTs
    pub fn new(asts: &'a HashMap<ModuleName, Ast<TypedExpr>>) -> Self {
        Inliner { asts }
    }

    /// Inline all component references in entrypoint components only
    /// Returns a vector of inlined entrypoint components
    pub fn inline_entrypoints(
        asts: HashMap<ModuleName, Ast<TypedExpr>>,
    ) -> Vec<ComponentDefinition<TypedExpr>> {
        let inliner = Inliner::new(&asts);
        let mut result = Vec::new();

        for (_, ast) in &asts {
            // Only process entrypoint components
            for component in ast.get_component_definitions() {
                if component.is_entrypoint {
                    let mut inlined_component = component.clone();
                    inlined_component.children = inliner.inline_nodes(&component.children);
                    result.push(inlined_component);
                }
            }
        }

        result
    }

    /// Recursively inline nodes in a list
    fn inline_nodes(&self, nodes: &[Node<TypedExpr>]) -> Vec<Node<TypedExpr>> {
        nodes.iter().map(|node| self.inline_node(node)).collect()
    }

    /// Inline a single node (and its children)
    fn inline_node(&self, node: &Node<TypedExpr>) -> Node<TypedExpr> {
        match node {
            Node::ComponentReference {
                tag_name,
                definition_module,
                args,
                attributes,
                children,
                range,
                ..
            } => {
                // Get the component definition
                let module = definition_module
                    .as_ref()
                    .expect("Component reference should have module");
                let ast = self
                    .asts
                    .get(module)
                    .expect("Component module should exist");
                let component = ast
                    .get_component_definition(tag_name.as_str())
                    .expect("Component definition should exist");

                // Inline the component
                self.inline_component_reference(
                    tag_name,
                    module,
                    component,
                    args.as_ref(),
                    attributes,
                    children,
                    range,
                )
            }

            Node::Html {
                tag_name,
                closing_tag_name,
                attributes,
                children,
                range,
            } => Node::Html {
                tag_name: tag_name.clone(),
                closing_tag_name: closing_tag_name.clone(),
                attributes: attributes.clone(),
                children: self.inline_nodes(children),
                range: range.clone(),
            },

            Node::If {
                condition,
                children,
                range,
            } => Node::If {
                condition: condition.clone(),
                children: self.inline_nodes(children),
                range: range.clone(),
            },

            Node::For {
                var_name,
                array_expr,
                children,
                range,
            } => Node::For {
                var_name: var_name.clone(),
                array_expr: array_expr.clone(),
                children: self.inline_nodes(children),
                range: range.clone(),
            },

            Node::Let {
                var,
                value,
                children,
                range,
            } => Node::Let {
                var: var.clone(),
                value: value.clone(),
                children: self.inline_nodes(children),
                range: range.clone(),
            },

            // Leaf nodes - return as is
            Node::Text { .. }
            | Node::TextExpression { .. }
            | Node::SlotDefinition { .. }
            | Node::Doctype { .. } => node.clone(),

            Node::Placeholder { children, range } => Node::Placeholder {
                children: self.inline_nodes(children),
                range: range.clone(),
            },
        }
    }

    /// Inline a component reference
    fn inline_component_reference(
        &self,
        tag_name: &DocumentRange,
        module: &ModuleName,
        component: &ComponentDefinition<TypedExpr>,
        args: Option<&(Vec<Argument<TypedExpr>>, DocumentRange)>,
        extra_attributes: &BTreeMap<StringSpan, Attribute<TypedExpr>>,
        slot_children: &[Node<TypedExpr>],
        range: &DocumentRange,
    ) -> Node<TypedExpr> {
        // Determine wrapper tag
        let wrapper_tag = component
            .as_attr
            .as_ref()
            .map(|a| a.value.clone())
            .unwrap_or_else(|| self.create_synthetic_range("div"));

        // Create data-hop-id attribute
        let mut attributes = BTreeMap::new();
        let data_hop_id = format!("{}/{}", module.as_str(), tag_name.as_str());
        let data_hop_id_span = DocumentRange::new("data-hop-id".to_string()).to_string_span();
        attributes.insert(
            data_hop_id_span,
            Attribute {
                name: self.create_synthetic_range("data-hop-id"),
                value: Some(AttributeValue::String(
                    self.create_synthetic_range(&data_hop_id),
                )),
                range: range.clone(),
            },
        );

        // Merge attributes from component definition and reference
        self.merge_attributes(
            &mut attributes,
            &component.attributes,
            extra_attributes,
            range,
        );

        // Process component children with slot replacement
        let inlined_children = if component.has_slot && !slot_children.is_empty() {
            self.inline_nodes_with_slot(&component.children, slot_children)
        } else {
            self.inline_nodes(&component.children)
        };

        // Build parameter bindings
        let mut body = inlined_children;
        if let Some((params, _)) = &component.params {
            // Process parameters in reverse order to create proper nesting
            for param in params.iter().rev() {
                let param_name = param.var_name.clone();

                // Find corresponding argument value
                let value = if let Some((args, _)) = args {
                    args.iter()
                        .find(|a| a.var_name.as_str() == param_name.as_str())
                        .map(|a| a.var_expr.clone())
                        .unwrap_or_else(|| {
                            panic!(
                                "Missing required parameter '{}' for component '{}' in module '{}'.",
                                param_name,
                                tag_name.as_str(),
                                module.as_str()
                            )
                        })
                } else {
                    panic!(
                        "No arguments provided for component '{}' in module '{}', but it requires parameter '{}'.",
                        tag_name.as_str(),
                        module.as_str(),
                        param_name
                    )
                };

                // Wrap the body in a Let node
                body = vec![Node::Let {
                    var: param_name,
                    value,
                    children: body,
                    range: range.clone(),
                }];
            }
        }

        // Build the wrapper node with inlined children
        Node::Html {
            tag_name: wrapper_tag.clone(),
            closing_tag_name: Some(wrapper_tag),
            attributes,
            children: body,
            range: range.clone(),
        }
    }

    /// Merge attributes from component definition and reference, handling class concatenation
    fn merge_attributes(
        &self,
        target: &mut BTreeMap<StringSpan, Attribute<TypedExpr>>,
        def_attributes: &BTreeMap<StringSpan, Attribute<TypedExpr>>,
        ref_attributes: &BTreeMap<StringSpan, Attribute<TypedExpr>>,
        range: &DocumentRange,
    ) {
        // First add component's attributes
        for (name, attr) in def_attributes {
            target.insert(name.clone(), attr.clone());
        }

        // Then override with reference's attributes, handling class specially
        for (name, ref_attr) in ref_attributes {
            if name.as_str() == "class" {
                // Special handling for class attribute - concatenation
                if let Some(def_attr) = def_attributes.get(name) {
                    // Both definition and reference have class attributes - concatenate them
                    let concatenated_value =
                        self.create_concatenated_class_value(def_attr, ref_attr, range);
                    target.insert(
                        name.clone(),
                        Attribute {
                            name: ref_attr.name.clone(),
                            value: Some(concatenated_value),
                            range: ref_attr.range.clone(),
                        },
                    );
                } else {
                    // Only reference has class attribute
                    target.insert(name.clone(), ref_attr.clone());
                }
            } else {
                // Non-class attributes: reference overrides definition
                target.insert(name.clone(), ref_attr.clone());
            }
        }
    }

    /// Create a concatenated class attribute value using StringConcat
    fn create_concatenated_class_value(
        &self,
        def_attr: &Attribute<TypedExpr>,
        ref_attr: &Attribute<TypedExpr>,
        _range: &DocumentRange,
    ) -> AttributeValue<TypedExpr> {
        let def_expr = match &def_attr.value {
            Some(AttributeValue::String(s)) => Expr::StringLiteral {
                value: s.as_str().to_string(),
                annotation: Type::String,
            },
            Some(AttributeValue::Expression(expr)) => expr.clone(),
            None => Expr::StringLiteral {
                value: "".to_string(),
                annotation: Type::String,
            },
        };

        let ref_expr = match &ref_attr.value {
            Some(AttributeValue::String(s)) => Expr::StringLiteral {
                value: s.as_str().to_string(),
                annotation: Type::String,
            },
            Some(AttributeValue::Expression(expr)) => expr.clone(),
            None => Expr::StringLiteral {
                value: "".to_string(),
                annotation: Type::String,
            },
        };

        // Create a space literal
        let space_expr = Expr::StringLiteral {
            value: " ".to_string(),
            annotation: Type::String,
        };

        // Concatenate: def_class + " " + ref_class
        let def_plus_space = Expr::StringConcat {
            left: Box::new(def_expr),
            right: Box::new(space_expr),
            annotation: Type::String,
        };

        let final_concat = Expr::StringConcat {
            left: Box::new(def_plus_space),
            right: Box::new(ref_expr),
            annotation: Type::String,
        };

        AttributeValue::Expression(final_concat)
    }

    /// Inline nodes, replacing slot definitions with the provided slot content
    fn inline_nodes_with_slot(
        &self,
        nodes: &[Node<TypedExpr>],
        slot_content: &[Node<TypedExpr>],
    ) -> Vec<Node<TypedExpr>> {
        let mut result = Vec::new();

        for node in nodes {
            match node {
                Node::SlotDefinition { .. } => {
                    // Replace slot with the provided content
                    result.extend(self.inline_nodes(slot_content));
                }
                // For other nodes, process recursively
                _ => {
                    result.push(self.inline_node_with_slot(node, slot_content));
                }
            }
        }

        result
    }

    /// Inline a single node, replacing slots with the provided content
    fn inline_node_with_slot(
        &self,
        node: &Node<TypedExpr>,
        slot_content: &[Node<TypedExpr>],
    ) -> Node<TypedExpr> {
        match node {
            Node::ComponentReference { .. } => self.inline_node(node),

            Node::Html {
                tag_name,
                closing_tag_name,
                attributes,
                children,
                range,
            } => Node::Html {
                tag_name: tag_name.clone(),
                closing_tag_name: closing_tag_name.clone(),
                attributes: attributes.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: range.clone(),
            },

            Node::If {
                condition,
                children,
                range,
            } => Node::If {
                condition: condition.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: range.clone(),
            },

            Node::For {
                var_name,
                array_expr,
                children,
                range,
            } => Node::For {
                var_name: var_name.clone(),
                array_expr: array_expr.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: range.clone(),
            },

            Node::Let {
                var,
                value,
                children,
                range,
            } => Node::Let {
                var: var.clone(),
                value: value.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: range.clone(),
            },

            Node::SlotDefinition { .. } => {
                // This case should be handled by inline_nodes_with_slot
                // but if we encounter it here, replace it
                Node::Placeholder {
                    children: self.inline_nodes(slot_content),
                    range: node.range().clone(),
                }
            }

            // Leaf nodes - return as is
            Node::Text { .. } | Node::TextExpression { .. } | Node::Doctype { .. } => node.clone(),

            Node::Placeholder { children, range } => Node::Placeholder {
                children: self.inline_nodes_with_slot(children, slot_content),
                range: range.clone(),
            },
        }
    }

    /// Helper to create synthetic DocumentRange for generated content
    fn create_synthetic_range(&self, content: &str) -> DocumentRange {
        // In a real implementation, we'd want to create proper synthetic ranges
        // For now, we'll create a placeholder range
        // This is a limitation we'd need to address for proper source mapping
        DocumentRange::new(content.to_string())
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

    fn create_typed_asts_from_sources(
        sources: Vec<(&str, &str)>,
    ) -> HashMap<ModuleName, Ast<TypedExpr>> {
        let mut errors = ErrorCollector::new();

        // Parse all sources first
        let mut untyped_asts = HashMap::new();
        for (module_name_str, source) in sources {
            let module_name = ModuleName::new(module_name_str.to_string()).unwrap();
            let tokenizer = Tokenizer::new(source.to_string());
            let ast = parse(module_name.clone(), tokenizer, &mut errors);
            untyped_asts.insert(module_name, ast);
        }

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Type check all ASTs
        let mut typechecker = TypeChecker::default();
        let untyped_asts_refs: Vec<_> = untyped_asts.values().collect();
        typechecker.typecheck(&untyped_asts_refs);

        // Check for type errors
        for (module_name, _) in &untyped_asts {
            assert!(
                typechecker.type_errors.get(module_name).unwrap().is_empty(),
                "Type errors in {}: {:?}",
                module_name.as_str(),
                typechecker.type_errors.get(module_name).unwrap()
            );
        }

        typechecker.typed_asts
    }

    fn check_inlining(sources: Vec<(&str, &str)>, expected: Expect) {
        let typed_asts = create_typed_asts_from_sources(sources);
        let inlined_entrypoints = Inliner::inline_entrypoints(typed_asts);

        // For now, just test that it doesn't crash and produces some output
        // In a real implementation, we'd want more detailed verification
        expected.assert_eq(&format!("{:#?}", inlined_entrypoints));
    }

    #[test]
    fn test_simple_component_inlining() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <card-comp {title: string}>
                        <h2>{title}</h2>
                    </card-comp>

                    <main-comp entrypoint>
                        <card-comp {title: "Hello"}/>
                    </main-comp>
                "#,
            )],
            expect![[r#"
                [
                    ComponentDefinition {
                        tag_name: "main-comp",
                        closing_tag_name: Some(
                            "main-comp",
                        ),
                        params: None,
                        as_attr: None,
                        attributes: {},
                        range: "<main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>",
                        children: [
                            Text {
                                range: "\n                        ",
                            },
                            Html {
                                tag_name: "div",
                                closing_tag_name: Some(
                                    "div",
                                ),
                                attributes: {
                                    "data-hop-id": Attribute {
                                        name: "data-hop-id",
                                        value: Some(
                                            String(
                                                "main/card-comp",
                                            ),
                                        ),
                                        range: "<card-comp {title: \"Hello\"}/>",
                                    },
                                },
                                range: "<card-comp {title: \"Hello\"}/>",
                                children: [
                                    Let {
                                        var: VarName {
                                            value: "title",
                                        },
                                        value: StringLiteral {
                                            value: "Hello",
                                            annotation: String,
                                        },
                                        children: [
                                            Text {
                                                range: "\n                        ",
                                            },
                                            Html {
                                                tag_name: "h2",
                                                closing_tag_name: Some(
                                                    "h2",
                                                ),
                                                attributes: {},
                                                range: "<h2>{title}</h2>",
                                                children: [
                                                    TextExpression {
                                                        expression: Var {
                                                            value: VarName {
                                                                value: "title",
                                                            },
                                                            annotation: String,
                                                        },
                                                        range: "{title}",
                                                    },
                                                ],
                                            },
                                            Text {
                                                range: "\n                    ",
                                            },
                                        ],
                                        range: "<card-comp {title: \"Hello\"}/>",
                                    },
                                ],
                            },
                            Text {
                                range: "\n                    ",
                            },
                        ],
                        is_entrypoint: true,
                        has_slot: false,
                    },
                ]"#]],
        );
    }

    #[test]
    fn test_component_with_slot() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <card-comp>
                        <div class="card">
                            <slot-default/>
                        </div>
                    </card-comp>

                    <main-comp entrypoint>
                        <card-comp>
                            <p>Slot content</p>
                        </card-comp>
                    </main-comp>
                "#,
            )],
            expect![[r#"
                [
                    ComponentDefinition {
                        tag_name: "main-comp",
                        closing_tag_name: Some(
                            "main-comp",
                        ),
                        params: None,
                        as_attr: None,
                        attributes: {},
                        range: "<main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>",
                        children: [
                            Text {
                                range: "\n                        ",
                            },
                            Html {
                                tag_name: "div",
                                closing_tag_name: Some(
                                    "div",
                                ),
                                attributes: {
                                    "data-hop-id": Attribute {
                                        name: "data-hop-id",
                                        value: Some(
                                            String(
                                                "main/card-comp",
                                            ),
                                        ),
                                        range: "<card-comp>\n                            <p>Slot content</p>\n                        </card-comp>",
                                    },
                                },
                                range: "<card-comp>\n                            <p>Slot content</p>\n                        </card-comp>",
                                children: [
                                    Text {
                                        range: "\n                        ",
                                    },
                                    Html {
                                        tag_name: "div",
                                        closing_tag_name: Some(
                                            "div",
                                        ),
                                        attributes: {
                                            "class": Attribute {
                                                name: "class",
                                                value: Some(
                                                    String(
                                                        "card",
                                                    ),
                                                ),
                                                range: "class=\"card\"",
                                            },
                                        },
                                        range: "<div class=\"card\">\n                            <slot-default/>\n                        </div>",
                                        children: [
                                            Text {
                                                range: "\n                            ",
                                            },
                                            Text {
                                                range: "\n                            ",
                                            },
                                            Html {
                                                tag_name: "p",
                                                closing_tag_name: Some(
                                                    "p",
                                                ),
                                                attributes: {},
                                                range: "<p>Slot content</p>",
                                                children: [
                                                    Text {
                                                        range: "Slot content",
                                                    },
                                                ],
                                            },
                                            Text {
                                                range: "\n                        ",
                                            },
                                            Text {
                                                range: "\n                        ",
                                            },
                                        ],
                                    },
                                    Text {
                                        range: "\n                    ",
                                    },
                                ],
                            },
                            Text {
                                range: "\n                    ",
                            },
                        ],
                        is_entrypoint: true,
                        has_slot: false,
                    },
                ]"#]],
        );
    }
}
