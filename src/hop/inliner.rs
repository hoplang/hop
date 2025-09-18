use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::expr::TypedExpr;
use crate::dop::{Argument, Expr, Type};
use crate::hop::ast::{Ast, Attribute, AttributeValue, ComponentDefinition, Node};
use crate::hop::module_name::ModuleName;
use std::collections::{BTreeMap, HashMap};

use super::ast::{InlinedComponentDefinition, InlinedNode, TypedNode};

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
    ) -> Vec<InlinedComponentDefinition> {
        let inliner = Inliner::new(&asts);
        let mut result = Vec::new();

        for (_, ast) in &asts {
            // Only process entrypoint components
            for component in ast.get_component_definitions() {
                if component.is_entrypoint {
                    result.push(ComponentDefinition {
                        tag_name: component.tag_name.clone(),
                        children: inliner.inline_nodes(&component.children),
                        is_entrypoint: true,
                        closing_tag_name: None,
                        params: component.params.clone(),
                        as_attr: None,
                        attributes: BTreeMap::new(),
                        has_slot: false,
                        range: (),
                    });
                }
            }
        }

        result
    }

    /// Recursively inline nodes in a list
    fn inline_nodes(&self, nodes: &[TypedNode]) -> Vec<InlinedNode> {
        nodes.iter().map(|node| self.inline_node(node)).collect()
    }

    /// Inline a single node (and its children)
    fn inline_node(&self, node: &TypedNode) -> InlinedNode {
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
                ..
            } => Node::Html {
                tag_name: tag_name.clone(),
                closing_tag_name: closing_tag_name.clone(),
                attributes: attributes.clone(),
                children: self.inline_nodes(children),
                range: (),
            },

            Node::If {
                condition,
                children,
                ..
            } => Node::If {
                condition: condition.clone(),
                children: self.inline_nodes(children),
                range: (),
            },

            Node::For {
                var_name,
                array_expr,
                children,
                ..
            } => Node::For {
                var_name: var_name.clone(),
                array_expr: array_expr.clone(),
                children: self.inline_nodes(children),
                range: (),
            },

            Node::Let {
                var,
                value,
                children,
                ..
            } => Node::Let {
                var: var.clone(),
                value: value.clone(),
                children: self.inline_nodes(children),
                range: (),
            },

            Node::Text { value, .. } => Node::Text {
                value: value.clone(),
                range: (),
            },
            Node::TextExpression { expression, .. } => Node::TextExpression {
                expression: expression.clone(),
                range: (),
            },
            Node::SlotDefinition { .. } => Node::SlotDefinition { range: () },
            Node::Doctype { value, .. } => Node::Doctype {
                value: value.clone(),
                range: (),
            },

            Node::Placeholder { children, .. } => Node::Placeholder {
                children: self.inline_nodes(children),
                range: (),
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
        slot_children: &[TypedNode],
        range: &DocumentRange,
    ) -> InlinedNode {
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
                    range: (),
                }];
            }
        }

        // Build the wrapper node with inlined children
        Node::Html {
            tag_name: wrapper_tag.clone(),
            closing_tag_name: Some(wrapper_tag),
            attributes,
            children: body,
            range: (),
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
        nodes: &[TypedNode],
        slot_content: &[TypedNode],
    ) -> Vec<InlinedNode> {
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
    fn inline_node_with_slot(&self, node: &TypedNode, slot_content: &[TypedNode]) -> InlinedNode {
        match node {
            Node::ComponentReference { .. } => self.inline_node(node),

            Node::Html {
                tag_name,
                closing_tag_name,
                attributes,
                children,
                ..
            } => Node::Html {
                tag_name: tag_name.clone(),
                closing_tag_name: closing_tag_name.clone(),
                attributes: attributes.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: (),
            },

            Node::If {
                condition,
                children,
                ..
            } => Node::If {
                condition: condition.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: (),
            },

            Node::For {
                var_name,
                array_expr,
                children,
                ..
            } => Node::For {
                var_name: var_name.clone(),
                array_expr: array_expr.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: (),
            },

            Node::Let {
                var,
                value,
                children,
                ..
            } => Node::Let {
                var: var.clone(),
                value: value.clone(),
                children: self.inline_nodes_with_slot(children, slot_content),
                range: (),
            },

            Node::SlotDefinition { .. } => {
                panic!()
            }

            // Leaf nodes - return as is
            Node::Text { value, .. } => Node::Text {
                value: value.clone(),
                range: (),
            },
            Node::TextExpression { expression, .. } => Node::TextExpression {
                expression: expression.clone(),
                range: (),
            },

            Node::Doctype { value, .. } => Node::Doctype {
                value: value.clone(),
                range: (),
            },

            Node::Placeholder { children, .. } => Node::Placeholder {
                children: self.inline_nodes_with_slot(children, slot_content),
                range: (),
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
        for module_name in untyped_asts.keys() {
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
                        tag_name: DocumentRange {
                            source: DocumentInfo {
                                text: "\n                    <card-comp {title: string}>\n                        <h2>{title}</h2>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>\n                ",
                                line_starts: [
                                    0,
                                    1,
                                    49,
                                    90,
                                    123,
                                    124,
                                    167,
                                    221,
                                    254,
                                ],
                            },
                            start: 145,
                            end: 154,
                        },
                        closing_tag_name: None,
                        params: None,
                        as_attr: None,
                        attributes: {},
                        children: [
                            Text {
                                value: "\n                        ",
                                range: (),
                            },
                            Html {
                                tag_name: DocumentRange {
                                    source: DocumentInfo {
                                        text: "div",
                                        line_starts: [
                                            0,
                                        ],
                                    },
                                    start: 0,
                                    end: 3,
                                },
                                closing_tag_name: Some(
                                    DocumentRange {
                                        source: DocumentInfo {
                                            text: "div",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 0,
                                        end: 3,
                                    },
                                ),
                                attributes: {
                                    "data-hop-id": Attribute {
                                        name: DocumentRange {
                                            source: DocumentInfo {
                                                text: "data-hop-id",
                                                line_starts: [
                                                    0,
                                                ],
                                            },
                                            start: 0,
                                            end: 11,
                                        },
                                        value: Some(
                                            String(
                                                DocumentRange {
                                                    source: DocumentInfo {
                                                        text: "main/card-comp",
                                                        line_starts: [
                                                            0,
                                                        ],
                                                    },
                                                    start: 0,
                                                    end: 14,
                                                },
                                            ),
                                        ),
                                        range: DocumentRange {
                                            source: DocumentInfo {
                                                text: "\n                    <card-comp {title: string}>\n                        <h2>{title}</h2>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>\n                ",
                                                line_starts: [
                                                    0,
                                                    1,
                                                    49,
                                                    90,
                                                    123,
                                                    124,
                                                    167,
                                                    221,
                                                    254,
                                                ],
                                            },
                                            start: 191,
                                            end: 220,
                                        },
                                    },
                                },
                                children: [
                                    Let {
                                        var: VarName {
                                            value: DocumentRange {
                                                source: DocumentInfo {
                                                    text: "\n                    <card-comp {title: string}>\n                        <h2>{title}</h2>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>\n                ",
                                                    line_starts: [
                                                        0,
                                                        1,
                                                        49,
                                                        90,
                                                        123,
                                                        124,
                                                        167,
                                                        221,
                                                        254,
                                                    ],
                                                },
                                                start: 33,
                                                end: 38,
                                            },
                                        },
                                        value: StringLiteral {
                                            value: "Hello",
                                            annotation: String,
                                        },
                                        children: [
                                            Text {
                                                value: "\n                        ",
                                                range: (),
                                            },
                                            Html {
                                                tag_name: DocumentRange {
                                                    source: DocumentInfo {
                                                        text: "\n                    <card-comp {title: string}>\n                        <h2>{title}</h2>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>\n                ",
                                                        line_starts: [
                                                            0,
                                                            1,
                                                            49,
                                                            90,
                                                            123,
                                                            124,
                                                            167,
                                                            221,
                                                            254,
                                                        ],
                                                    },
                                                    start: 74,
                                                    end: 76,
                                                },
                                                closing_tag_name: Some(
                                                    DocumentRange {
                                                        source: DocumentInfo {
                                                            text: "\n                    <card-comp {title: string}>\n                        <h2>{title}</h2>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>\n                ",
                                                            line_starts: [
                                                                0,
                                                                1,
                                                                49,
                                                                90,
                                                                123,
                                                                124,
                                                                167,
                                                                221,
                                                                254,
                                                            ],
                                                        },
                                                        start: 86,
                                                        end: 88,
                                                    },
                                                ),
                                                attributes: {},
                                                children: [
                                                    TextExpression {
                                                        expression: Var {
                                                            value: VarName {
                                                                value: DocumentRange {
                                                                    source: DocumentInfo {
                                                                        text: "\n                    <card-comp {title: string}>\n                        <h2>{title}</h2>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp {title: \"Hello\"}/>\n                    </main-comp>\n                ",
                                                                        line_starts: [
                                                                            0,
                                                                            1,
                                                                            49,
                                                                            90,
                                                                            123,
                                                                            124,
                                                                            167,
                                                                            221,
                                                                            254,
                                                                        ],
                                                                    },
                                                                    start: 78,
                                                                    end: 83,
                                                                },
                                                            },
                                                            annotation: String,
                                                        },
                                                        range: (),
                                                    },
                                                ],
                                                range: (),
                                            },
                                            Text {
                                                value: "\n                    ",
                                                range: (),
                                            },
                                        ],
                                        range: (),
                                    },
                                ],
                                range: (),
                            },
                            Text {
                                value: "\n                    ",
                                range: (),
                            },
                        ],
                        is_entrypoint: true,
                        has_slot: false,
                        range: (),
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
                        tag_name: DocumentRange {
                            source: DocumentInfo {
                                text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                line_starts: [
                                    0,
                                    1,
                                    33,
                                    76,
                                    120,
                                    151,
                                    184,
                                    185,
                                    228,
                                    264,
                                    312,
                                    349,
                                    382,
                                ],
                            },
                            start: 206,
                            end: 215,
                        },
                        closing_tag_name: None,
                        params: None,
                        as_attr: None,
                        attributes: {},
                        children: [
                            Text {
                                value: "\n                        ",
                                range: (),
                            },
                            Html {
                                tag_name: DocumentRange {
                                    source: DocumentInfo {
                                        text: "div",
                                        line_starts: [
                                            0,
                                        ],
                                    },
                                    start: 0,
                                    end: 3,
                                },
                                closing_tag_name: Some(
                                    DocumentRange {
                                        source: DocumentInfo {
                                            text: "div",
                                            line_starts: [
                                                0,
                                            ],
                                        },
                                        start: 0,
                                        end: 3,
                                    },
                                ),
                                attributes: {
                                    "data-hop-id": Attribute {
                                        name: DocumentRange {
                                            source: DocumentInfo {
                                                text: "data-hop-id",
                                                line_starts: [
                                                    0,
                                                ],
                                            },
                                            start: 0,
                                            end: 11,
                                        },
                                        value: Some(
                                            String(
                                                DocumentRange {
                                                    source: DocumentInfo {
                                                        text: "main/card-comp",
                                                        line_starts: [
                                                            0,
                                                        ],
                                                    },
                                                    start: 0,
                                                    end: 14,
                                                },
                                            ),
                                        ),
                                        range: DocumentRange {
                                            source: DocumentInfo {
                                                text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                line_starts: [
                                                    0,
                                                    1,
                                                    33,
                                                    76,
                                                    120,
                                                    151,
                                                    184,
                                                    185,
                                                    228,
                                                    264,
                                                    312,
                                                    349,
                                                    382,
                                                ],
                                            },
                                            start: 252,
                                            end: 348,
                                        },
                                    },
                                },
                                children: [
                                    Text {
                                        value: "\n                        ",
                                        range: (),
                                    },
                                    Html {
                                        tag_name: DocumentRange {
                                            source: DocumentInfo {
                                                text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                line_starts: [
                                                    0,
                                                    1,
                                                    33,
                                                    76,
                                                    120,
                                                    151,
                                                    184,
                                                    185,
                                                    228,
                                                    264,
                                                    312,
                                                    349,
                                                    382,
                                                ],
                                            },
                                            start: 58,
                                            end: 61,
                                        },
                                        closing_tag_name: Some(
                                            DocumentRange {
                                                source: DocumentInfo {
                                                    text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                    line_starts: [
                                                        0,
                                                        1,
                                                        33,
                                                        76,
                                                        120,
                                                        151,
                                                        184,
                                                        185,
                                                        228,
                                                        264,
                                                        312,
                                                        349,
                                                        382,
                                                    ],
                                                },
                                                start: 146,
                                                end: 149,
                                            },
                                        ),
                                        attributes: {
                                            "class": Attribute {
                                                name: DocumentRange {
                                                    source: DocumentInfo {
                                                        text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                        line_starts: [
                                                            0,
                                                            1,
                                                            33,
                                                            76,
                                                            120,
                                                            151,
                                                            184,
                                                            185,
                                                            228,
                                                            264,
                                                            312,
                                                            349,
                                                            382,
                                                        ],
                                                    },
                                                    start: 62,
                                                    end: 67,
                                                },
                                                value: Some(
                                                    String(
                                                        DocumentRange {
                                                            source: DocumentInfo {
                                                                text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                                line_starts: [
                                                                    0,
                                                                    1,
                                                                    33,
                                                                    76,
                                                                    120,
                                                                    151,
                                                                    184,
                                                                    185,
                                                                    228,
                                                                    264,
                                                                    312,
                                                                    349,
                                                                    382,
                                                                ],
                                                            },
                                                            start: 69,
                                                            end: 73,
                                                        },
                                                    ),
                                                ),
                                                range: DocumentRange {
                                                    source: DocumentInfo {
                                                        text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                        line_starts: [
                                                            0,
                                                            1,
                                                            33,
                                                            76,
                                                            120,
                                                            151,
                                                            184,
                                                            185,
                                                            228,
                                                            264,
                                                            312,
                                                            349,
                                                            382,
                                                        ],
                                                    },
                                                    start: 62,
                                                    end: 74,
                                                },
                                            },
                                        },
                                        children: [
                                            Text {
                                                value: "\n                            ",
                                                range: (),
                                            },
                                            Text {
                                                value: "\n                            ",
                                                range: (),
                                            },
                                            Html {
                                                tag_name: DocumentRange {
                                                    source: DocumentInfo {
                                                        text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                        line_starts: [
                                                            0,
                                                            1,
                                                            33,
                                                            76,
                                                            120,
                                                            151,
                                                            184,
                                                            185,
                                                            228,
                                                            264,
                                                            312,
                                                            349,
                                                            382,
                                                        ],
                                                    },
                                                    start: 293,
                                                    end: 294,
                                                },
                                                closing_tag_name: Some(
                                                    DocumentRange {
                                                        source: DocumentInfo {
                                                            text: "\n                    <card-comp>\n                        <div class=\"card\">\n                            <slot-default/>\n                        </div>\n                    </card-comp>\n\n                    <main-comp entrypoint>\n                        <card-comp>\n                            <p>Slot content</p>\n                        </card-comp>\n                    </main-comp>\n                ",
                                                            line_starts: [
                                                                0,
                                                                1,
                                                                33,
                                                                76,
                                                                120,
                                                                151,
                                                                184,
                                                                185,
                                                                228,
                                                                264,
                                                                312,
                                                                349,
                                                                382,
                                                            ],
                                                        },
                                                        start: 309,
                                                        end: 310,
                                                    },
                                                ),
                                                attributes: {},
                                                children: [
                                                    Text {
                                                        value: "Slot content",
                                                        range: (),
                                                    },
                                                ],
                                                range: (),
                                            },
                                            Text {
                                                value: "\n                        ",
                                                range: (),
                                            },
                                            Text {
                                                value: "\n                        ",
                                                range: (),
                                            },
                                        ],
                                        range: (),
                                    },
                                    Text {
                                        value: "\n                    ",
                                        range: (),
                                    },
                                ],
                                range: (),
                            },
                            Text {
                                value: "\n                    ",
                                range: (),
                            },
                        ],
                        is_entrypoint: true,
                        has_slot: false,
                        range: (),
                    },
                ]"#]],
        );
    }
}
