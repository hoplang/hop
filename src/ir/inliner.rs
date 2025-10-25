use crate::document::document_cursor::StringSpan;
use crate::dop::SimpleTypedExpr;
use crate::dop::parser::TypedArgument;
use crate::hop::ast::{Ast, AttributeValue};
use crate::hop::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypoint, InlinedNode, InlinedParameter,
};
use crate::hop::module_name::ModuleName;
use std::collections::{BTreeMap, HashMap};

use crate::hop::ast::{TypedAttribute, TypedComponentDefinition};
use crate::hop::node::{Node, TypedNode};

/// The Inliner transforms ASTs by replacing ComponentReference nodes with their
/// inlined component definitions, using Let nodes for parameter binding and
/// StringConcat for class attribute merging.
pub struct Inliner;

impl Inliner {
    /// Convert a TypedAttribute to InlinedAttribute
    fn convert_attribute(attr: &TypedAttribute) -> InlinedAttribute {
        InlinedAttribute {
            name: attr.name.as_str().to_string(),
            value: attr.value.as_ref().map(|v| match v {
                AttributeValue::Expression(expr) => InlinedAttributeValue::Expression(expr.clone()),
                AttributeValue::String(s) => InlinedAttributeValue::String(s.as_str().to_string()),
            }),
        }
    }

    /// Convert a BTreeMap of TypedAttributes to InlinedAttributes
    fn convert_attributes(
        attrs: &BTreeMap<StringSpan, TypedAttribute>,
    ) -> BTreeMap<String, InlinedAttribute> {
        attrs
            .iter()
            .map(|(key, attr)| (key.as_str().to_string(), Self::convert_attribute(attr)))
            .collect()
    }
    /// Inline all component references in entrypoint components only
    /// Returns a vector of inlined entrypoint components
    pub fn inline_entrypoints(
        asts: HashMap<ModuleName, Ast<SimpleTypedExpr>>,
    ) -> Vec<InlinedEntrypoint> {
        let mut result = Vec::new();

        for ast in asts.values() {
            // Only process entrypoint components
            for component in ast.get_component_definitions() {
                if component.is_entrypoint {
                    result.push(InlinedEntrypoint {
                        tag_name: component.tag_name.to_string_span(),
                        children: Self::inline_nodes(&component.children, None, &asts),
                        params: component
                            .params
                            .clone()
                            .map(|p| {
                                p.0.into_iter()
                                    .map(|param| InlinedParameter {
                                        var_name: param.var_name,
                                        var_type: param.var_type,
                                    })
                                    .collect()
                            })
                            .unwrap_or_default(),
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
        slot_children: &[TypedNode],
        asts: &HashMap<ModuleName, Ast<SimpleTypedExpr>>,
    ) -> Vec<InlinedNode> {
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

        // Return the inlined children without a wrapper
        body
    }

    /// Inline nodes, optionally replacing slot definitions with the provided slot content
    fn inline_nodes(
        nodes: &[TypedNode],
        slot_content: Option<&[TypedNode]>,
        asts: &HashMap<ModuleName, Ast<SimpleTypedExpr>>,
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
        asts: &HashMap<ModuleName, Ast<SimpleTypedExpr>>,
    ) -> Vec<InlinedNode> {
        match node {
            Node::ComponentReference {
                tag_name,
                definition_module,
                args,
                children,
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
                Self::inline_component_reference(
                    module, component, args_vec, children, asts,
                )
            }

            Node::Html {
                tag_name,
                attributes,
                children,
                ..
            } => vec![InlinedNode::Html {
                tag_name: tag_name.to_string_span(),
                attributes: Self::convert_attributes(attributes),
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
    ) -> HashMap<ModuleName, Ast<SimpleTypedExpr>> {
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

        // Format using Display implementation for better readability
        let output = inlined_entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_component_inlining() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <CardComp {title: String}>
                        <h2>{title}</h2>
                    </CardComp>

                    <Main entrypoint>
                        <CardComp {title: "Hello"}/>
                    </Main>
                "#,
            )],
            expect![[r#"
                <Main>
                  "\n                        "
                  <let {title = "Hello"}>
                    "\n                        "
                    <h2>
                      {title}
                    </h2>
                    "\n                    "
                  </let>
                  "\n                    "
                </Main>
            "#]],
        );
    }

    #[test]
    fn test_component_with_slot() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <CardComp>
                        <div class="card">
                            <slot-default/>
                        </div>
                    </CardComp>

                    <Main entrypoint>
                        <CardComp>
                            <p>Slot content</p>
                        </CardComp>
                    </Main>
                "#,
            )],
            expect![[r#"
                <Main>
                  "\n                        "
                  "\n                        "
                  <div class="card">
                    "\n                            "
                    "\n                            "
                    <p>
                      "Slot content"
                    </p>
                    "\n                        "
                    "\n                        "
                  </div>
                  "\n                    "
                  "\n                    "
                </Main>
            "#]],
        );
    }
}
