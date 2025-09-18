use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::expr::TypedExpr;
use crate::dop::parser::TypedArgument;
use crate::dop::{Expr, Type};
use crate::hop::ast::{Ast, Attribute, AttributeValue};
use crate::hop::inlined_ast::{InlinedEntryPoint, InlinedNode};
use crate::hop::module_name::ModuleName;
use std::collections::{BTreeMap, HashMap};

use crate::hop::ast::{TypedAttribute, TypedAttributeValue, TypedComponentDefinition};
use crate::hop::node::{Node, TypedNode};

/// The Inliner transforms ASTs by replacing ComponentReference nodes with their
/// inlined component definitions, using Let nodes for parameter binding and
/// StringConcat for class attribute merging.
pub struct Inliner;

impl Inliner {
    /// Inline all component references in entrypoint components only
    /// Returns a vector of inlined entrypoint components
    pub fn inline_entrypoints(asts: HashMap<ModuleName, Ast<TypedExpr>>) -> Vec<InlinedEntryPoint> {
        let mut result = Vec::new();

        for ast in asts.values() {
            // Only process entrypoint components
            for component in ast.get_component_definitions() {
                if component.is_entrypoint {
                    result.push(InlinedEntryPoint {
                        tag_name: component.tag_name.to_string_span(),
                        children: Self::inline_nodes(&component.children, None, &asts),
                        params: component.params.clone().map(|p| p.0).unwrap_or_default(),
                    });
                }
            }
        }

        result
    }

    /// Inline a component reference
    fn inline_component_reference(
        module_name: &ModuleName,
        component: &TypedComponentDefinition,
        args: &[TypedArgument],
        reference_attributes: &BTreeMap<StringSpan, TypedAttribute>,
        slot_children: &[TypedNode],
        range: &DocumentRange,
        asts: &HashMap<ModuleName, Ast<TypedExpr>>,
    ) -> InlinedNode {
        // Determine wrapper tag
        let tag_name = component
            .as_attr
            .as_ref()
            .map(|a| a.value.to_string_span())
            .unwrap_or_else(|| StringSpan::new("div".to_string()));

        // Create data-hop-id attribute
        let mut attributes = BTreeMap::new();
        let data_hop_id = format!("{}/{}", module_name.as_str(), component.tag_name.as_str());
        let data_hop_id_span = StringSpan::new("data-hop-id".to_string());
        attributes.insert(
            data_hop_id_span,
            Attribute {
                name: Self::create_synthetic_range("data-hop-id"),
                value: Some(AttributeValue::String(Self::create_synthetic_range(
                    &data_hop_id,
                ))),
                range: range.clone(),
            },
        );

        // Merge attributes from component definition and reference
        Self::merge_attributes(&mut attributes, &component.attributes, reference_attributes);

        // Process component children with slot replacement
        let slot_content = if component.has_slot && !slot_children.is_empty() {
            Some(slot_children)
        } else {
            None
        };
        let inlined_children = Self::inline_nodes(&component.children, slot_content, asts);

        // Build parameter bindings
        let mut body = inlined_children;
        if let Some((params, _)) = &component.params {
            // Process parameters in reverse order to create proper nesting
            for param in params.iter().rev() {
                let param_name = param.var_name.clone();

                // Find corresponding argument value
                let value = args
                    .iter()
                    .find(|a| a.var_name.as_str() == param_name.as_str())
                    .map(|a| a.var_expr.clone())
                    .unwrap_or_else(|| {
                        panic!(
                            "Missing required parameter '{}' for component '{}' in module '{}'.",
                            param_name,
                            component.tag_name.as_str(),
                            module_name.as_str()
                        )
                    });

                // Wrap the body in a Let node
                body = vec![InlinedNode::Let {
                    var: param_name,
                    value,
                    children: body,
                }];
            }
        }

        // Build the wrapper node with inlined children
        InlinedNode::Html {
            tag_name,
            attributes,
            children: body,
        }
    }

    /// Merge attributes from component definition and reference, handling class concatenation
    fn merge_attributes(
        target: &mut BTreeMap<StringSpan, TypedAttribute>,
        definition_attributes: &BTreeMap<StringSpan, TypedAttribute>,
        reference_attributes: &BTreeMap<StringSpan, TypedAttribute>,
    ) {
        // First add component's attributes
        for (name, attr) in definition_attributes {
            target.insert(name.clone(), attr.clone());
        }

        // Then override with reference's attributes, handling class specially
        for (name, ref_attr) in reference_attributes {
            if name.as_str() == "class" {
                // Special handling for class attribute - concatenation
                if let Some(def_attr) = definition_attributes.get(name) {
                    // Both definition and reference have class attributes - concatenate them
                    let concatenated_value =
                        Self::create_concatenated_class_value(def_attr, ref_attr);
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
        def_attr: &TypedAttribute,
        ref_attr: &TypedAttribute,
    ) -> TypedAttributeValue {
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

    /// Inline nodes, optionally replacing slot definitions with the provided slot content
    fn inline_nodes(
        nodes: &[TypedNode],
        slot_content: Option<&[TypedNode]>,
        asts: &HashMap<ModuleName, Ast<TypedExpr>>,
    ) -> Vec<InlinedNode> {
        nodes
            .iter()
            .flat_map(|node| Self::inline_node(node, slot_content, asts))
            .collect()
    }

    /// Inline a single node, optionally replacing slots with the provided content
    fn inline_node(
        node: &TypedNode,
        slot_content: Option<&[TypedNode]>,
        asts: &HashMap<ModuleName, Ast<TypedExpr>>,
    ) -> Vec<InlinedNode> {
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
                let ast = asts.get(module).expect("Component module should exist");
                let component = ast
                    .get_component_definition(tag_name.as_str())
                    .expect("Component definition should exist");

                // Inline the component
                let args_vec = args.as_ref().map(|(v, _)| v.as_slice()).unwrap_or(&[]);
                vec![Self::inline_component_reference(
                    module, component, args_vec, attributes, children, range, asts,
                )]
            }

            Node::Html {
                tag_name,
                attributes,
                children,
                ..
            } => vec![InlinedNode::Html {
                tag_name: tag_name.to_string_span(),
                attributes: attributes.clone(),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            Node::If {
                condition,
                children,
                ..
            } => vec![InlinedNode::If {
                condition: condition.clone(),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            Node::For {
                var_name,
                array_expr,
                children,
                ..
            } => vec![InlinedNode::For {
                var_name: var_name.clone(),
                array_expr: array_expr.clone(),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            Node::SlotDefinition { .. } => {
                if let Some(content) = slot_content {
                    // Replace slot with the provided content
                    Self::inline_nodes(content, None, asts)
                } else {
                    // No slot content provided, return empty vec
                    vec![]
                }
            }

            // Leaf nodes - return as is
            Node::Text { value, .. } => vec![InlinedNode::Text {
                value: value.clone(),
            }],
            Node::TextExpression { expression, .. } => vec![InlinedNode::TextExpression {
                expression: expression.clone(),
            }],

            Node::Doctype { value, .. } => vec![InlinedNode::Doctype {
                value: value.clone(),
            }],

            Node::Placeholder { .. } => panic!(),
        }
    }

    /// Helper to create synthetic DocumentRange for generated content
    fn create_synthetic_range(content: &str) -> DocumentRange {
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
                    InlinedEntryPoint {
                        tag_name: "main-comp",
                        params: [],
                        children: [
                            Text {
                                value: "\n                        ",
                            },
                            Html {
                                tag_name: "div",
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
                                            },
                                            Html {
                                                tag_name: "h2",
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
                                                    },
                                                ],
                                            },
                                            Text {
                                                value: "\n                    ",
                                            },
                                        ],
                                    },
                                ],
                            },
                            Text {
                                value: "\n                    ",
                            },
                        ],
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
                    InlinedEntryPoint {
                        tag_name: "main-comp",
                        params: [],
                        children: [
                            Text {
                                value: "\n                        ",
                            },
                            Html {
                                tag_name: "div",
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
                                    },
                                    Html {
                                        tag_name: "div",
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
                                            },
                                            Text {
                                                value: "\n                            ",
                                            },
                                            Html {
                                                tag_name: "p",
                                                attributes: {},
                                                children: [
                                                    Text {
                                                        value: "Slot content",
                                                    },
                                                ],
                                            },
                                            Text {
                                                value: "\n                        ",
                                            },
                                            Text {
                                                value: "\n                        ",
                                            },
                                        ],
                                    },
                                    Text {
                                        value: "\n                    ",
                                    },
                                ],
                            },
                            Text {
                                value: "\n                    ",
                            },
                        ],
                    },
                ]"#]],
        );
    }
}
