use crate::document::document_cursor::StringSpan;
use crate::dop::VarName;
use crate::dop::{Type, TypedExpr};
use crate::hop::semantics::typed_ast::{TypedAst, TypedComponentDeclaration};
use crate::hop::semantics::typed_node::{TypedAttribute, TypedAttributeValue, TypedNode};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::inlined::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
    InlinedParameter,
};
use anyhow::Result;
use std::collections::{BTreeMap, HashMap};

/// The Inliner transforms ASTs by replacing ComponentReference nodes with their
/// inlined component declarations, using Let nodes for parameter binding and
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
    ) -> Result<Vec<InlinedComponentDeclaration>> {
        // Validate that all requested pages exist
        for (module_name, component_name) in pages {
            let component_exists = asts.get(module_name).is_some_and(|ast| {
                ast.get_component_declarations()
                    .iter()
                    .any(|c| &c.component_name == component_name)
            });

            if !component_exists {
                let available_list: Vec<_> = asts
                    .iter()
                    .flat_map(|(m, ast)| {
                        ast.get_component_declarations()
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
            for component in ast.get_component_declarations() {
                let included = pages
                    .iter()
                    .any(|(m, c)| m == module_name && c == &component.component_name);
                if included {
                    result.push(InlinedComponentDeclaration {
                        module_name: module_name.clone(),
                        component_name: component.component_name.clone(),
                        children: Self::inline_nodes(&component.children, None, &asts),
                        params: component
                            .params
                            .iter()
                            .cloned()
                            .map(|(var_name, var_type, default_value)| InlinedParameter {
                                var_name,
                                var_type,
                                default_value,
                            })
                            .collect(),
                    });
                }
            }
        }

        Ok(result)
    }

    /// Convert a TypedAttribute to InlinedAttribute
    fn convert_attribute(attr: &TypedAttribute) -> InlinedAttribute {
        InlinedAttribute {
            name: attr.name.clone(),
            value: attr.value.as_ref().map(|v| match v {
                TypedAttributeValue::Expressions(exprs) => {
                    InlinedAttributeValue::Expressions(exprs.clone())
                }
                TypedAttributeValue::String(s) => InlinedAttributeValue::String(s.to_string()),
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
    fn component_accepts_children(component: &TypedComponentDeclaration) -> bool {
        component.params.iter().any(|(var_name, var_type, _)| {
            var_name.as_str() == "children" && *var_type == Type::TrustedHTML
        })
    }

    /// Inline a component reference
    fn inline_component_reference(
        module_name: &ModuleName,
        component: &TypedComponentDeclaration,
        args: &[(VarName, TypedExpr)],
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
        // Process parameters in reverse order to create proper nesting
        // Skip the children parameter - it's handled via slot_content
        for (var_name, _var_type, default_value) in component.params.iter().rev() {
            if var_name.as_str() == "children" {
                continue;
            }

            let param_name = var_name.clone();

            // Find corresponding argument value, or use default if not provided
            let value = args
                .iter()
                .find(|(name, _)| name.as_str() == param_name.as_str())
                .map(|(_, expr)| expr.clone())
                .or_else(|| default_value.clone())
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
            TypedNode::ComponentReference {
                component_name,
                declaring_module,
                args,
                children,
            } => {
                // Get the component declaration
                let module = declaring_module
                    .as_ref()
                    .expect("Component reference should have module");
                let ast = asts.get(module).expect("Component module should exist");
                let component = ast
                    .get_component_declaration(component_name.as_str())
                    .expect("Component declaration should exist");

                // Inline the component
                Self::inline_component_reference(module, component, args, children, asts)
            }

            TypedNode::Html {
                tag_name,
                attributes,
                children,
            } => vec![InlinedNode::Html {
                tag_name: tag_name.clone(),
                attributes: Self::convert_attributes(attributes),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            TypedNode::If {
                condition,
                children,
            } => vec![InlinedNode::If {
                condition: condition.clone(),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            TypedNode::For {
                var_name,
                array_expr,
                children,
            } => vec![InlinedNode::For {
                var_name: var_name.clone(),
                array_expr: array_expr.clone(),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            // Leaf nodes - return as is
            TypedNode::Text { value } => vec![InlinedNode::Text {
                value: value.clone(),
            }],

            // Check if this is {children} expression that should be replaced with slot content
            TypedNode::TextExpression { expression } => {
                // Check if this is a `children` variable of type TrustedHTML
                if let TypedExpr::Var { value, kind, .. } = expression {
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

            TypedNode::Doctype { value } => vec![InlinedNode::Doctype {
                value: value.clone(),
            }],

            TypedNode::Match { match_ } => {
                let inlined_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => Match::Bool {
                        subject: subject.clone(),
                        true_body: Box::new(Self::inline_nodes(true_body, slot_content, asts)),
                        false_body: Box::new(Self::inline_nodes(false_body, slot_content, asts)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => Match::Option {
                        subject: subject.clone(),
                        some_arm_binding: some_arm_binding.clone(),
                        some_arm_body: Box::new(Self::inline_nodes(
                            some_arm_body,
                            slot_content,
                            asts,
                        )),
                        none_arm_body: Box::new(Self::inline_nodes(
                            none_arm_body,
                            slot_content,
                            asts,
                        )),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: subject.clone(),
                        arms: arms
                            .iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                body: Self::inline_nodes(&arm.body, slot_content, asts),
                            })
                            .collect(),
                    },
                };
                vec![InlinedNode::Match {
                    match_: inlined_match,
                }]
            }

            TypedNode::Let {
                var,
                var_type: _,
                value,
                children,
            } => vec![InlinedNode::Let {
                var: var.clone(),
                value: value.clone(),
                children: Self::inline_nodes(children, slot_content, asts),
            }],

            TypedNode::Placeholder => panic!(),
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

    #[test]
    fn component_with_default_parameter_uses_default() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <Greeting {name: String = "World"}>
                        <p>Hello, {name}!</p>
                    </Greeting>

                    <Main>
                        <Greeting />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  "\n                        "
                  <let {name = "World"}>
                    "\n                        "
                    <p>
                      "Hello, "
                      {name}
                      "!"
                    </p>
                    "\n                    "
                  </let>
                  "\n                    "
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_default_parameter_overridden() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <Greeting {name: String = "World"}>
                        <p>Hello, {name}!</p>
                    </Greeting>

                    <Main>
                        <Greeting {name: "Alice"} />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  "\n                        "
                  <let {name = "Alice"}>
                    "\n                        "
                    <p>
                      "Hello, "
                      {name}
                      "!"
                    </p>
                    "\n                    "
                  </let>
                  "\n                    "
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_mixed_required_and_default_parameters() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <Button {label: String, size: String = "medium"}>
                        <button class={size}>{label}</button>
                    </Button>

                    <Main>
                        <Button {label: "Click me"} />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  "\n                        "
                  <let {label = "Click me"}>
                    <let {size = "medium"}>
                      "\n                        "
                      <button class={size}>
                        {label}
                      </button>
                      "\n                    "
                    </let>
                  </let>
                  "\n                    "
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_int_default_parameter() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <Counter {count: Int = 0}>
                        <if {count == 0}>
                            <span>Zero</span>
                        </if>
                    </Counter>

                    <Main>
                        <Counter />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  "\n                        "
                  <let {count = 0}>
                    "\n                        "
                    <if {(count == 0)}>
                      "\n                            "
                      <span>
                        "Zero"
                      </span>
                      "\n                        "
                    </if>
                    "\n                    "
                  </let>
                  "\n                    "
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_bool_default_parameter() {
        check_inlining(
            vec![(
                "main",
                r#"
                    <Toggle {enabled: Bool = true}>
                        <if {enabled}>
                            <span>On</span>
                        </if>
                    </Toggle>

                    <Main>
                        <Toggle />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  "\n                        "
                  <let {enabled = true}>
                    "\n                        "
                    <if {enabled}>
                      "\n                            "
                      <span>
                        "On"
                      </span>
                      "\n                        "
                    </if>
                    "\n                    "
                  </let>
                  "\n                    "
                </Main>
            "#]],
        );
    }
}
