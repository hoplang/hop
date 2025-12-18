use crate::document::document_cursor::StringSpan;
use crate::dop::{Expr, Type};
use crate::hop::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypoint, InlinedNode, InlinedParameter,
};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use crate::hop::semantics::typed_ast::{TypedAst, TypedAttribute, TypedComponentDefinition};
use crate::hop::syntax::ast::AttributeValue;
use crate::hop::syntax::node::TypedArgument;
use crate::hop::semantics::typed_node::TypedNode;
use crate::hop::syntax::node::Node;
use anyhow::Result;
use std::collections::{BTreeMap, HashMap};

/// The Inliner transforms ASTs by replacing ComponentReference nodes with their
/// inlined component definitions, using Let nodes for parameter binding and
/// StringConcat for class attribute merging.
pub struct Inliner;

impl Inliner {
    /// Inline all component references in components listed in pages
    /// Returns a vector of inlined entrypoint components
    ///
    /// # Arguments
    /// * `asts` - Map of module name to typed AST
    /// * `pages` - List of pages to export as (ModuleName, ComponentName) tuples
    ///
    /// # Returns
    /// Result containing the list of inlined entrypoints or an error if a page doesn't exist
    pub fn inline_entrypoints(
        asts: HashMap<ModuleName, TypedAst>,
        pages: &[(ModuleName, ComponentName)],
    ) -> Result<Vec<InlinedEntrypoint>> {
        // Validate that all requested pages exist
        for (module_name, component_name) in pages {
            let component_exists = asts.get(module_name).is_some_and(|ast| {
                ast.get_component_definitions()
                    .iter()
                    .any(|c| &c.component_name == component_name)
            });

            if !component_exists {
                let available_list: Vec<_> = asts
                    .iter()
                    .flat_map(|(m, ast)| {
                        ast.get_component_definitions()
                            .iter()
                            .map(move |c| format!("  - {}/{}", m, c.component_name))
                    })
                    .collect();
                anyhow::bail!(
                    "Component '{}/{}' listed in pages but not found\nAvailable components:\n{}",
                    module_name,
                    component_name,
                    available_list.join("\n")
                );
            }
        }

        let mut result = Vec::new();

        for (module_name, ast) in &asts {
            for component in ast.get_component_definitions() {
                let included = pages
                    .iter()
                    .any(|(m, c)| m == module_name && c == &component.component_name);
                if included {
                    result.push(InlinedEntrypoint {
                        module_name: module_name.clone(),
                        component_name: component.component_name.clone(),
                        children: Self::inline_nodes(&component.children, None, &asts),
                        params: component
                            .params
                            .clone()
                            .map(|p| {
                                p.into_iter()
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

        Ok(result)
    }

    /// Convert a TypedAttribute to InlinedAttribute
    fn convert_attribute(attr: &TypedAttribute) -> InlinedAttribute {
        InlinedAttribute {
            name: attr.name.to_string(),
            value: attr.value.as_ref().map(|v| match v {
                AttributeValue::Expressions(exprs) => {
                    InlinedAttributeValue::Expressions(exprs.clone())
                }
                AttributeValue::String(s) => InlinedAttributeValue::String(s.to_string()),
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

    /// Check if a component has a children: TrustedHTML parameter
    fn component_accepts_children(component: &TypedComponentDefinition) -> bool {
        component
            .params
            .as_ref()
            .map(|params| {
                params
                    .iter()
                    .any(|p| p.var_name.as_str() == "children" && p.var_type == Type::TrustedHTML)
            })
            .unwrap_or(false)
    }

    /// Inline a component reference
    fn inline_component_reference(
        module_name: &ModuleName,
        component: &TypedComponentDefinition,
        args: &[TypedArgument],
        slot_children: &[TypedNode],
        asts: &HashMap<ModuleName, TypedAst>,
    ) -> Vec<InlinedNode> {
        // Process component children with slot replacement
        // Children are passed if the component has `children: TrustedHTML` parameter
        let slot_content =
            if Self::component_accepts_children(component) && !slot_children.is_empty() {
                Some(slot_children)
            } else {
                None
            };
        let inlined_children = Self::inline_nodes(&component.children, slot_content, asts);

        // Build parameter bindings (excluding children - handled via slot mechanism)
        let mut body = inlined_children;
        if let Some(params) = &component.params {
            // Process parameters in reverse order to create proper nesting
            // Skip the children parameter - it's handled via slot_content
            for param in params.iter().rev() {
                if param.var_name.as_str() == "children" {
                    continue;
                }

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
                            component.component_name.as_str(),
                            module_name
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

    /// Inline nodes, optionally replacing {children} expressions with the provided slot content
    fn inline_nodes(
        nodes: &[TypedNode],
        slot_content: Option<&[TypedNode]>,
        asts: &HashMap<ModuleName, TypedAst>,
    ) -> Vec<InlinedNode> {
        nodes
            .iter()
            .flat_map(|node| Self::inline_node(node, slot_content, asts))
            .collect()
    }

    /// Inline a single node, optionally replacing {children} expressions with the provided content
    fn inline_node(
        node: &TypedNode,
        slot_content: Option<&[TypedNode]>,
        asts: &HashMap<ModuleName, TypedAst>,
    ) -> Vec<InlinedNode> {
        match node {
            Node::ComponentReference {
                component_name,
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
                    .get_component_definition(component_name.as_str())
                    .expect("Component definition should exist");

                // Inline the component
                let args_vec = args.as_ref().map(|(v, _)| v.as_slice()).unwrap_or(&[]);
                Self::inline_component_reference(module, component, args_vec, children, asts)
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

            // Leaf nodes - return as is
            Node::Text { value, .. } => vec![InlinedNode::Text {
                value: value.clone(),
            }],

            // Check if this is {children} expression that should be replaced with slot content
            Node::TextExpression { expression, .. } => {
                // Check if this is a `children` variable of type TrustedHTML
                if let Expr::Var { value, kind, .. } = expression {
                    if value.as_str() == "children" && *kind == Type::TrustedHTML {
                        if let Some(content) = slot_content {
                            // Replace {children} with the provided content
                            return Self::inline_nodes(content, None, asts);
                        } else {
                            // No slot content provided, return empty vec
                            return vec![];
                        }
                    }
                }
                // Regular text expression
                vec![InlinedNode::TextExpression {
                    expression: expression.clone(),
                }]
            }

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
    use crate::hop::semantics::type_checker::TypeChecker;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parser::parse;
    use expect_test::{Expect, expect};

    fn create_typed_asts_from_sources(sources: Vec<(&str, &str)>) -> HashMap<ModuleName, TypedAst> {
        let mut errors = ErrorCollector::new();

        // Parse all sources first
        let mut untyped_asts = HashMap::new();
        for (module_name_str, source) in sources {
            let module_name = ModuleName::new(module_name_str).unwrap();
            let ast = parse(module_name.clone(), source.to_string(), &mut errors);
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
                module_name,
                typechecker.type_errors.get(module_name).unwrap()
            );
        }

        typechecker.typed_asts
    }

    fn check_inlining(sources: Vec<(&str, &str)>, pages: Vec<(&str, &str)>, expected: Expect) {
        let typed_asts = create_typed_asts_from_sources(sources);
        let pages_owned: Vec<(ModuleName, ComponentName)> = pages
            .iter()
            .map(|(m, c)| {
                (
                    ModuleName::new(m).unwrap(),
                    ComponentName::new(c.to_string()).unwrap(),
                )
            })
            .collect();
        let inlined_entrypoints =
            Inliner::inline_entrypoints(typed_asts, &pages_owned).expect("Inlining should succeed");

        // Format using Display implementation for better readability
        let output = inlined_entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        expected.assert_eq(&output);
    }

    #[test]
    fn simple_component_inlining() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <CardComp {title: String}>
                        <h2>{title}</h2>
                    </CardComp>

                    <Main>
                        <CardComp {title: "Hello"}/>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
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
    fn component_with_children() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <CardComp {children: TrustedHTML}>
                        <div class="card">
                            {children}
                        </div>
                    </CardComp>

                    <Main>
                        <CardComp>
                            <p>Slot content</p>
                        </CardComp>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
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
