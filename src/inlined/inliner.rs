use crate::dop::VarName;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::dop::{Type, TypedExpr};
use crate::hop::semantics::typed_ast::{TypedAst, TypedComponentDeclaration};
use crate::hop::semantics::typed_node::{TypedAttribute, TypedAttributeValue, TypedNode};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use crate::inlined::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
    InlinedParameter,
};
use anyhow::Result;
use std::collections::{BTreeMap, HashMap, HashSet};

/// Context for inlining, carrying slot content and variable bindings.
#[derive(Debug, Clone)]
struct InlineContext<'a> {
    /// The already-inlined slot content passed to this component (if any)
    slot_content: Option<Vec<InlinedNode>>,
    /// Variables that are derived from `children` (directly or transitively)
    children_vars: HashSet<String>,
    /// Reference to all ASTs for component lookups
    asts: &'a HashMap<ModuleName, TypedAst>,
}

impl<'a> InlineContext<'a> {
    fn new(asts: &'a HashMap<ModuleName, TypedAst>) -> Self {
        Self {
            slot_content: None,
            children_vars: HashSet::new(),
            asts,
        }
    }

    fn with_slot_content(&self, content: Vec<InlinedNode>) -> Self {
        Self {
            slot_content: Some(content),
            children_vars: self.children_vars.clone(),
            asts: self.asts,
        }
    }

    /// Create a child context with an additional children-derived variable
    fn with_children_var(&self, var: &str) -> Self {
        let mut children_vars = self.children_vars.clone();
        children_vars.insert(var.to_string());
        Self {
            slot_content: self.slot_content.clone(),
            children_vars,
            asts: self.asts,
        }
    }

    /// Check if a variable holds children (directly or transitively)
    fn variable_holds_children(&self, var: &str) -> bool {
        self.children_vars.contains(var)
    }
}

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
        asts: &HashMap<ModuleName, TypedAst>,
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

        for (module_name, ast) in asts {
            for component in ast.get_component_declarations() {
                let included = pages
                    .iter()
                    .any(|(m, c)| m == module_name && c == &component.component_name);
                if included {
                    result.push(InlinedComponentDeclaration {
                        module_name: module_name.clone(),
                        component_name: component.component_name.clone(),
                        children: Self::inline_nodes(&component.children, &asts),
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
                TypedAttributeValue::Expression(expr) => {
                    InlinedAttributeValue::Expression(expr.clone())
                }
                TypedAttributeValue::String(s) => InlinedAttributeValue::String(s.to_string()),
            }),
        }
    }

    /// Convert a Vec of TypedAttributes to InlinedAttributes
    fn convert_attributes(attrs: &[TypedAttribute]) -> BTreeMap<String, InlinedAttribute> {
        attrs
            .iter()
            .map(|attr| (attr.name.clone(), Self::convert_attribute(attr)))
            .collect()
    }

    /// Get the children parameter type if present
    fn get_children_param_type(component: &TypedComponentDeclaration) -> Option<&Type> {
        component
            .params
            .iter()
            .find(|(var_name, _, _)| var_name.as_str() == "children")
            .map(|(_, var_type, _)| var_type)
    }

    /// Inline a component reference
    /// `parent_ctx` is used to inline slot_children before passing to the component
    fn inline_component_reference(
        module_name: &ModuleName,
        component: &TypedComponentDeclaration,
        args: &[(VarName, TypedExpr)],
        slot_children: &[TypedNode],
        parent_ctx: Option<&InlineContext>,
        asts: &HashMap<ModuleName, TypedAst>,
    ) -> Vec<InlinedNode> {
        // Inline slot_children in parent context (resolves any children-derived variables)
        let children_type = Self::get_children_param_type(component);
        let inlined_slot_content = if children_type.is_some() && !slot_children.is_empty() {
            let content = if let Some(ctx) = parent_ctx {
                Self::inline_nodes_with_ctx(slot_children, ctx)
            } else {
                Self::inline_nodes(slot_children, asts)
            };
            Some(content)
        } else {
            None
        };

        // Create fresh context for this component with the inlined slot content
        let mut ctx = InlineContext::new(asts);
        if let Some(content) = inlined_slot_content {
            ctx = ctx.with_slot_content(content);
        }
        match children_type {
            Some(Type::TrustedHTML | Type::Option(_)) => {
                ctx = ctx.with_children_var("children");
            }
            Some(other) => {
                panic!("Invalid children parameter type: {other}");
            }
            None => {}
        }

        let inlined_children = Self::inline_nodes_with_ctx(&component.children, &ctx);

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

    /// Inline nodes without any slot content
    fn inline_nodes(nodes: &[TypedNode], asts: &HashMap<ModuleName, TypedAst>) -> Vec<InlinedNode> {
        let ctx = InlineContext::new(asts);
        Self::inline_nodes_with_ctx(nodes, &ctx)
    }

    /// Inline nodes with the given context
    fn inline_nodes_with_ctx(nodes: &[TypedNode], ctx: &InlineContext) -> Vec<InlinedNode> {
        nodes
            .iter()
            .flat_map(|node| Self::inline_node_with_ctx(node, ctx))
            .collect()
    }

    /// Inline a single node using the context for environment-based resolution
    fn inline_node_with_ctx(node: &TypedNode, ctx: &InlineContext) -> Vec<InlinedNode> {
        match node {
            TypedNode::ComponentReference {
                component_name,
                declaring_module,
                args,
                children,
            } => {
                let module = declaring_module
                    .as_ref()
                    .expect("Component reference should have module");
                let ast = ctx.asts.get(module).expect("Component module should exist");
                let component = ast
                    .get_component_declaration(component_name.as_str())
                    .expect("Component declaration should exist");

                // Pass current context so slot_children can be inlined with children-derived vars resolved
                Self::inline_component_reference(
                    module,
                    component,
                    args,
                    children,
                    Some(ctx),
                    ctx.asts,
                )
            }

            TypedNode::Html {
                tag_name,
                attributes,
                children,
            } => vec![InlinedNode::Html {
                tag_name: tag_name.clone(),
                attributes: Self::convert_attributes(attributes),
                children: Self::inline_nodes_with_ctx(children, ctx),
            }],

            TypedNode::If {
                condition,
                children,
            } => vec![InlinedNode::If {
                condition: condition.clone(),
                children: Self::inline_nodes_with_ctx(children, ctx),
            }],

            TypedNode::For {
                var_name,
                source,
                children,
            } => vec![InlinedNode::For {
                var_name: var_name.clone(),
                source: source.clone(),
                children: Self::inline_nodes_with_ctx(children, ctx),
            }],

            // Leaf nodes - return as is
            TypedNode::Text { value } => vec![InlinedNode::Text {
                value: value.clone(),
            }],

            TypedNode::TextExpression { expression } => {
                if let TypedExpr::Var { value, kind, .. } = expression {
                    if ctx.variable_holds_children(value.as_str()) {
                        assert_eq!(
                            *kind,
                            Type::TrustedHTML,
                            "children-derived variable in TextExpression must be TrustedHTML"
                        );
                        // slot_content is already inlined, just return it
                        return ctx.slot_content.clone().expect(
                            "children-derived TrustedHTML variable should have slot content",
                        );
                    }
                }
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
                        true_body: Box::new(Self::inline_nodes_with_ctx(true_body, ctx)),
                        false_body: Box::new(Self::inline_nodes_with_ctx(false_body, ctx)),
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        if ctx.variable_holds_children(subject.0.as_str()) {
                            assert!(
                                matches!(&subject.1, Type::Option(inner) if **inner == Type::TrustedHTML),
                                "children-holding variable in Match::Option must be Option[TrustedHTML]"
                            );
                            // Statically resolve based on whether slot content was provided
                            if ctx.slot_content.is_some() {
                                // Children provided → inline Some arm with binding marked as children-derived
                                let child_ctx = if let Some(binding) = some_arm_binding {
                                    ctx.with_children_var(binding.as_str())
                                } else {
                                    ctx.clone()
                                };
                                return Self::inline_nodes_with_ctx(some_arm_body, &child_ctx);
                            } else {
                                // No children → inline None arm
                                let fresh_ctx = InlineContext::new(ctx.asts);
                                return Self::inline_nodes_with_ctx(none_arm_body, &fresh_ctx);
                            }
                        }

                        // Regular Option match
                        Match::Option {
                            subject: subject.clone(),
                            some_arm_binding: some_arm_binding.clone(),
                            some_arm_body: Box::new(Self::inline_nodes_with_ctx(
                                some_arm_body,
                                ctx,
                            )),
                            none_arm_body: Box::new(Self::inline_nodes_with_ctx(
                                none_arm_body,
                                ctx,
                            )),
                        }
                    }
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: subject.clone(),
                        arms: arms
                            .iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                bindings: arm.bindings.clone(),
                                body: Self::inline_nodes_with_ctx(&arm.body, ctx),
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
                value,
                children,
            } => {
                // Propagate children-derived status through variable aliases
                if let TypedExpr::Var {
                    value: var_value, ..
                } = value
                {
                    if ctx.variable_holds_children(var_value.as_str()) {
                        let child_ctx = ctx.with_children_var(var.as_str());
                        return Self::inline_nodes_with_ctx(children, &child_ctx);
                    }
                }

                vec![InlinedNode::Let {
                    var: var.clone(),
                    value: value.clone(),
                    children: Self::inline_nodes_with_ctx(children, ctx),
                }]
            }

            TypedNode::Placeholder => panic!("Placeholder nodes should not exist after typing"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::error_collector::ErrorCollector;
    use crate::hop::semantics::type_checker::TypeChecker;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parser::parse;
    use crate::hop::syntax::transform::whitespace_removal::remove_whitespace;
    use expect_test::{Expect, expect};

    fn create_typed_asts_from_sources(sources: Vec<(&str, &str)>) -> HashMap<ModuleName, TypedAst> {
        let mut errors = ErrorCollector::new();

        // Parse all sources first
        let mut untyped_asts = HashMap::new();
        for (module_name_str, source) in sources {
            let module_name = ModuleName::new(module_name_str).unwrap();
            let ast = parse(
                module_name.clone(),
                Document::new(source.to_string()),
                &mut errors,
            );
            let ast = remove_whitespace(ast);
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
        let inlined_entrypoints = Inliner::inline_entrypoints(&typed_asts, &pages_owned)
            .expect("Inlining should succeed");

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
                        <CardComp title="Hello"/>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <let {title = "Hello"}>
                    <h2>
                      {title}
                    </h2>
                  </let>
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
                  <div class="card">
                    <p>
                      Slot content
                    </p>
                  </div>
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
                  <let {name = "World"}>
                    <p>
                      Hello,
                      {name}
                      !
                    </p>
                  </let>
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
                        <Greeting name="Alice" />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <let {name = "Alice"}>
                    <p>
                      Hello,
                      {name}
                      !
                    </p>
                  </let>
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
                        <Button label="Click me" />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <let {label = "Click me"}>
                    <let {size = "medium"}>
                      <button class={size}>
                        {label}
                      </button>
                    </let>
                  </let>
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
                  <let {count = 0}>
                    <if {(count == 0)}>
                      <span>
                        Zero
                      </span>
                    </if>
                  </let>
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
                  <let {enabled = true}>
                    <if {enabled}>
                      <span>
                        On
                      </span>
                    </if>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_optional_children_none_provided() {
        // When no children are provided, the None arm should be inlined
        check_inlining(
            vec![(
                "main",
                r#"
                    <Separator {children: Option[TrustedHTML] = None}>
                        <li>
                            <match {children}>
                                <case {Some(c)}>
                                    {c}
                                </case>
                                <case {None}>
                                    <span>Default</span>
                                </case>
                            </match>
                        </li>
                    </Separator>

                    <Main>
                        <Separator />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <li>
                    <span>
                      Default
                    </span>
                  </li>
                </Main>
            "#]],
        );
    }

    #[test]
    fn component_with_optional_children_some_provided() {
        // When children are provided, the Some arm should be inlined with children content
        check_inlining(
            vec![(
                "main",
                r#"
                    <Separator {children: Option[TrustedHTML] = None}>
                        <li>
                            <match {children}>
                                <case {Some(c)}>
                                    {c}
                                </case>
                                <case {None}>
                                    <span>Default</span>
                                </case>
                            </match>
                        </li>
                    </Separator>

                    <Main>
                        <Separator>
                            <strong>Custom</strong>
                        </Separator>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <li>
                    <strong>
                      Custom
                    </strong>
                  </li>
                </Main>
            "#]],
        );
    }

    #[test]
    fn optional_children_with_multiple_child_elements() {
        // Multiple children elements should all be inlined in the Some arm
        check_inlining(
            vec![(
                "main",
                r#"
                    <Wrapper {children: Option[TrustedHTML] = None}>
                        <div>
                            <match {children}>
                                <case {Some(c)}>{c}</case>
                                <case {None}><span>Empty</span></case>
                            </match>
                        </div>
                    </Wrapper>

                    <Main>
                        <Wrapper>
                            <p>First</p>
                            <p>Second</p>
                        </Wrapper>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <div>
                    <p>
                      First
                    </p>
                    <p>
                      Second
                    </p>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn optional_children_without_binding() {
        // Test that optional children work even when the Some arm doesn't bind the content
        check_inlining(
            vec![(
                "main",
                r#"
                    <Indicator {children: Option[TrustedHTML] = None}>
                        <match {children}>
                            <case {Some(_)}>
                                <span>Has content</span>
                            </case>
                            <case {None}>
                                <span>Empty</span>
                            </case>
                        </match>
                    </Indicator>

                    <WithChildren>
                        <Indicator>
                            <p>Ignored</p>
                        </Indicator>
                    </WithChildren>

                    <WithoutChildren>
                        <Indicator />
                    </WithoutChildren>
                "#,
            )],
            vec![("main", "WithChildren"), ("main", "WithoutChildren")],
            expect![[r#"
                <WithChildren>
                  <span>
                    Has content
                  </span>
                </WithChildren>


                <WithoutChildren>
                  <span>
                    Empty
                  </span>
                </WithoutChildren>
            "#]],
        );
    }

    #[test]
    fn same_component_with_and_without_children() {
        // Same component used twice - once with children, once without
        check_inlining(
            vec![(
                "main",
                r#"
                    <Box {children: Option[TrustedHTML] = None}>
                        <div>
                            <match {children}>
                                <case {Some(c)}>{c}</case>
                                <case {None}><span>Empty</span></case>
                            </match>
                        </div>
                    </Box>

                    <Main>
                        <Box><p>First</p></Box>
                        <Box />
                        <Box><p>Third</p></Box>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <div>
                    <p>
                      First
                    </p>
                  </div>
                  <div>
                    <span>
                      Empty
                    </span>
                  </div>
                  <div>
                    <p>
                      Third
                    </p>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_optional_children_passthrough() {
        // Outer component passes its optional children to an inner component that also has optional children
        check_inlining(
            vec![(
                "main",
                r#"
                    <Inner {children: Option[TrustedHTML] = None}>
                        <span>
                            <match {children}>
                                <case {Some(c)}>{c}</case>
                                <case {None}>inner-default</case>
                            </match>
                        </span>
                    </Inner>

                    <Outer {children: Option[TrustedHTML] = None}>
                        <div>
                            <match {children}>
                                <case {Some(c)}>
                                    <Inner>{c}</Inner>
                                </case>
                                <case {None}>
                                    <Inner />
                                </case>
                            </match>
                        </div>
                    </Outer>

                    <WithChildren>
                        <Outer><strong>content</strong></Outer>
                    </WithChildren>

                    <WithoutChildren>
                        <Outer />
                    </WithoutChildren>
                "#,
            )],
            vec![("main", "WithChildren"), ("main", "WithoutChildren")],
            expect![[r#"
                <WithChildren>
                  <div>
                    <span>
                      <strong>
                        content
                      </strong>
                    </span>
                  </div>
                </WithChildren>


                <WithoutChildren>
                  <div>
                    <span>
                      inner-default
                    </span>
                  </div>
                </WithoutChildren>
            "#]],
        );
    }

    #[test]
    fn optional_children_used_multiple_times() {
        // Test using the bound content multiple times in the Some arm
        check_inlining(
            vec![(
                "main",
                r#"
                    <Repeat {children: Option[TrustedHTML] = None}>
                        <match {children}>
                            <case {Some(c)}>
                                <div>{c}</div>
                                <div>{c}</div>
                            </case>
                            <case {None}>
                                <span>empty</span>
                            </case>
                        </match>
                    </Repeat>

                    <Main>
                        <Repeat><strong>content</strong></Repeat>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <div>
                    <strong>
                      content
                    </strong>
                  </div>
                  <div>
                    <strong>
                      content
                    </strong>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn regular_option_match_still_works() {
        // Non-children Option matches should still work normally (not statically resolved)
        check_inlining(
            vec![(
                "main",
                r#"
                    <Card {title: Option[String] = None}>
                        <div>
                            <match {title}>
                                <case {Some(t)}><h1>{t}</h1></case>
                                <case {None}><h1>Untitled</h1></case>
                            </match>
                        </div>
                    </Card>

                    <Main>
                        <Card title={Some("Hello")} />
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <let {title = Some("Hello")}>
                    <div>
                      <let {match_subject = title}>
                        <match {match_subject}>
                          <case {Some(v0)}>
                            <let {t = v0}>
                              <h1>
                                {t}
                              </h1>
                            </let>
                          </case>
                          <case {None}>
                            <h1>
                              Untitled
                            </h1>
                          </case>
                        </match>
                      </let>
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn required_children_still_work() {
        // Required children (TrustedHTML without Option) should still work
        check_inlining(
            vec![(
                "main",
                r#"
                    <Box {children: TrustedHTML}>
                        <div class="box">{children}</div>
                    </Box>

                    <Main>
                        <Box>
                            <span>Content</span>
                        </Box>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <div class="box">
                    <span>
                      Content
                    </span>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn nested_optional_and_required_children() {
        // Component with optional children containing a component with required children
        check_inlining(
            vec![(
                "main",
                r#"
                    <Inner {children: TrustedHTML}>
                        <span>{children}</span>
                    </Inner>

                    <Outer {children: Option[TrustedHTML] = None}>
                        <div>
                            <match {children}>
                                <case {Some(c)}>{c}</case>
                                <case {None}><Inner>Default</Inner></case>
                            </match>
                        </div>
                    </Outer>

                    <Main>
                        <Outer>
                            <Inner>Custom</Inner>
                        </Outer>
                    </Main>
                "#,
            )],
            vec![("main", "Main")],
            expect![[r#"
                <Main>
                  <div>
                    <span>
                      Custom
                    </span>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn badge_and_badge_link_with_enum_element() {
        check_inlining(
            vec![(
                "main",
                r#"
                    enum BadgeElement {
                        Span,
                        Link(href: String),
                    }

                    <BadgeBase {
                        element: BadgeElement,
                        children: TrustedHTML,
                    }>
                        <let {classes: String = "inline-flex items-center"}>
                            <match {element}>
                                <case {BadgeElement::Span}>
                                    <span data-slot="badge" class={classes}>
                                        {children}
                                    </span>
                                </case>
                                <case {BadgeElement::Link(href: h)}>
                                    <a data-slot="badge" href={h} class={classes}>
                                        {children}
                                    </a>
                                </case>
                            </match>
                        </let>
                    </BadgeBase>

                    <Badge {children: TrustedHTML}>
                        <BadgeBase element={BadgeElement::Span}>
                            {children}
                        </BadgeBase>
                    </Badge>

                    <BadgeLink {href: String, children: TrustedHTML}>
                        <BadgeBase element={BadgeElement::Link(href: href)}>
                            {children}
                        </BadgeBase>
                    </BadgeLink>

                    <TestBadge>
                        <Badge>Hello</Badge>
                    </TestBadge>

                    <TestBadgeLink>
                        <BadgeLink href="/home">Click me</BadgeLink>
                    </TestBadgeLink>
                "#,
            )],
            vec![("main", "TestBadge"), ("main", "TestBadgeLink")],
            expect![[r#"
                <TestBadge>
                  <let {element = BadgeElement::Span}>
                    <let {classes = "inline-flex items-center"}>
                      <let {match_subject = element}>
                        <match {match_subject}>
                          <case {BadgeElement::Span}>
                            <span class={classes} data-slot="badge">
                              Hello
                            </span>
                          </case>
                          <case {BadgeElement::Link(href: v0)}>
                            <let {h = v0}>
                              <a class={classes} data-slot="badge" href={h}>
                                Hello
                              </a>
                            </let>
                          </case>
                        </match>
                      </let>
                    </let>
                  </let>
                </TestBadge>


                <TestBadgeLink>
                  <let {href = "/home"}>
                    <let {element = BadgeElement::Link(href: href)}>
                      <let {classes = "inline-flex items-center"}>
                        <let {match_subject = element}>
                          <match {match_subject}>
                            <case {BadgeElement::Span}>
                              <span class={classes} data-slot="badge">
                                Click me
                              </span>
                            </case>
                            <case {BadgeElement::Link(href: v0)}>
                              <let {h = v0}>
                                <a class={classes} data-slot="badge" href={h}>
                                  Click me
                                </a>
                              </let>
                            </case>
                          </match>
                        </let>
                      </let>
                    </let>
                  </let>
                </TestBadgeLink>
            "#]],
        );
    }
}
