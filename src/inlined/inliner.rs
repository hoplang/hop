use crate::dop::VarName;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::dop::{Type, TypedExpr};
use crate::hop::semantics::typed_ast::{
    TypedAst, TypedComponentDeclaration, TypedEntrypointDeclaration,
};
use crate::hop::semantics::typed_node::{TypedAttribute, TypedAttributeValue, TypedNode};
use crate::hop::symbols::module_name::ModuleName;
use crate::inlined::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypointDeclaration, InlinedNode,
    InlinedParameter,
};
use std::collections::{HashMap, HashSet};

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
    /// Inline all entrypoint declarations found in the ASTs.
    /// Returns a vector of inlined component declarations (entrypoints are treated
    /// as components in the compilation pipeline).
    ///
    /// # Arguments
    /// * `asts` - Map of module name to typed AST
    ///
    /// # Returns
    /// Vector of inlined component declarations from entrypoints
    pub fn inline_ast_entrypoints(
        asts: &HashMap<ModuleName, TypedAst>,
    ) -> Vec<InlinedEntrypointDeclaration> {
        // Collect all entrypoints from all modules, maintaining deterministic order
        let mut module_names: Vec<_> = asts.keys().collect();
        module_names.sort();

        module_names
            .into_iter()
            .flat_map(|module_name| {
                let ast = asts.get(module_name).unwrap();
                ast.get_entrypoint_declarations()
                    .iter()
                    .map(|entrypoint| Self::inline_single_entrypoint(module_name, entrypoint, asts))
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    /// Inline a single entrypoint declaration.
    pub fn inline_single_entrypoint(
        module_name: &ModuleName,
        entrypoint: &TypedEntrypointDeclaration,
        asts: &HashMap<ModuleName, TypedAst>,
    ) -> InlinedEntrypointDeclaration {
        InlinedEntrypointDeclaration {
            module_name: module_name.clone(),
            component_name: entrypoint.name.clone(),
            children: Self::inline_nodes(&entrypoint.children, asts),
            params: entrypoint
                .params
                .iter()
                .cloned()
                .map(|(var_name, var_type, default_value)| InlinedParameter {
                    var_name,
                    var_type,
                    default_value,
                })
                .collect(),
        }
    }

    /// Convert a TypedAttribute to InlinedAttribute
    fn convert_attribute(attr: &TypedAttribute) -> InlinedAttribute {
        InlinedAttribute {
            name: attr.name.clone(),
            value: attr.value.as_ref().map(|v| match v {
                TypedAttributeValue::Expression(expr) => {
                    InlinedAttributeValue::Expression(expr.clone())
                }
                TypedAttributeValue::String(s) => InlinedAttributeValue::String(s.clone()),
            }),
        }
    }

    /// Convert a Vec of TypedAttributes to InlinedAttributes
    fn convert_attributes(attrs: &[TypedAttribute]) -> Vec<InlinedAttribute> {
        attrs.iter().map(Self::convert_attribute).collect()
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

    fn check_entrypoint_inlining(sources: Vec<(&str, &str)>, expected: Expect) {
        let typed_asts = create_typed_asts_from_sources(sources);
        let inlined_entrypoints = Inliner::inline_ast_entrypoints(&typed_asts);

        let output = inlined_entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        expected.assert_eq(&output);
    }

    #[test]
    fn entrypoint_without_parameters() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    entrypoint Main() {
                        <div>Hello</div>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main() {
                  <div>
                    Hello
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_parameters() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    entrypoint Main(name: String) {
                        <div>Hello, {name}!</div>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main(name: String) {
                  <div>
                    Hello, 
                    {name}
                    !
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_default_parameter() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    entrypoint Main(name: String = "World") {
                        <div>Hello, {name}!</div>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main(name: String = "World") {
                  <div>
                    Hello, 
                    {name}
                    !
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_referencing_component() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    <Greeting {name: String}>
                        <div>Hello, {name}!</div>
                    </Greeting>

                    entrypoint Main(name: String) {
                        <Greeting name={name} />
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main(name: String) {
                  <let {name = name}>
                    <div>
                      Hello, 
                      {name}
                      !
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn multiple_entrypoints_in_same_module() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    entrypoint Home() {
                        <div>Home page</div>
                    }

                    entrypoint About() {
                        <div>About page</div>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Home() {
                  <div>
                    Home page
                  </div>
                }

                entrypoint About() {
                  <div>
                    About page
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoints_across_multiple_modules() {
        check_entrypoint_inlining(
            vec![
                (
                    "alpha",
                    r#"
                        entrypoint AlphaPage() {
                            <div>Alpha</div>
                        }
                    "#,
                ),
                (
                    "beta",
                    r#"
                        entrypoint BetaPage() {
                            <div>Beta</div>
                        }
                    "#,
                ),
            ],
            expect![[r#"
                entrypoint AlphaPage() {
                  <div>
                    Alpha
                  </div>
                }

                entrypoint BetaPage() {
                  <div>
                    Beta
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_component_using_slot_content() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    <Card {children: TrustedHTML}>
                        <div class="card">
                            {children}
                        </div>
                    </Card>

                    entrypoint Main() {
                        <Card>
                            <p>This is slot content</p>
                        </Card>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main() {
                  <div class="card">
                    <p>
                      This is slot content
                    </p>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_nested_component_inlining() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    <Inner {text: String}>
                        <span>{text}</span>
                    </Inner>

                    <Outer {label: String}>
                        <div class="outer">
                            <Inner text={label} />
                        </div>
                    </Outer>

                    entrypoint Main(title: String) {
                        <Outer label={title} />
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main(title: String) {
                  <let {label = title}>
                    <div class="outer">
                      <let {text = label}>
                        <span>
                          {text}
                        </span>
                      </let>
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_conditional_rendering() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    entrypoint Main(show_greeting: Bool, name: String) {
                        <div>
                            <if {show_greeting}>
                                <p>Hello, {name}!</p>
                            </if>
                        </div>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main(show_greeting: Bool, name: String) {
                  <div>
                    <if {show_greeting}>
                      <p>
                        Hello, 
                        {name}
                        !
                      </p>
                    </if>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_loop() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    entrypoint Main(count: Int) {
                        <ul>
                            <for {i in 1..=count}>
                                <li>Item {i.to_string()}</li>
                            </for>
                        </ul>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main(count: Int) {
                  <ul>
                    <for {i in 1..=count}>
                      <li>
                        Item 
                        {i.to_string()}
                      </li>
                    </for>
                  </ul>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_with_component_default_value_used() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    <Button {label: String = "Click me"}>
                        <button>{label}</button>
                    </Button>

                    entrypoint Main() {
                        <div>
                            <Button />
                        </div>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint Main() {
                  <div>
                    <let {label = "Click me"}>
                      <button>
                        {label}
                      </button>
                    </let>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn entrypoint_complex_page_layout() {
        check_entrypoint_inlining(
            vec![(
                "main",
                r#"
                    <Header {title: String}>
                        <header>
                            <h1>{title}</h1>
                        </header>
                    </Header>

                    <Footer>
                        <footer>
                            <p>Copyright 2024</p>
                        </footer>
                    </Footer>

                    <Layout {children: TrustedHTML}>
                        <div class="layout">
                            {children}
                        </div>
                    </Layout>

                    entrypoint HomePage(page_title: String) {
                        <Layout>
                            <Header title={page_title} />
                            <main>
                                <p>Welcome to the home page</p>
                            </main>
                            <Footer />
                        </Layout>
                    }
                "#,
            )],
            expect![[r#"
                entrypoint HomePage(page_title: String) {
                  <div class="layout">
                    <let {title = page_title}>
                      <header>
                        <h1>
                          {title}
                        </h1>
                      </header>
                    </let>
                    <main>
                      <p>
                        Welcome to the home page
                      </p>
                    </main>
                    <footer>
                      <p>
                        Copyright 2024
                      </p>
                    </footer>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn deterministic_output_order_with_multiple_modules() {
        // Test that output order is deterministic when there are multiple modules.
        // Modules should be sorted alphabetically by name.
        // This test would be flaky if the implementation used HashMap iteration order.
        // With 8 modules, there's only 1/40320 chance of accidental success.
        check_entrypoint_inlining(
            vec![
                (
                    "zeta",
                    r#"
                        entrypoint ZetaPage() {
                            <div>Zeta</div>
                        }
                    "#,
                ),
                (
                    "alpha",
                    r#"
                        entrypoint AlphaPage() {
                            <div>Alpha</div>
                        }
                    "#,
                ),
                (
                    "theta",
                    r#"
                        entrypoint ThetaPage() {
                            <div>Theta</div>
                        }
                    "#,
                ),
                (
                    "beta",
                    r#"
                        entrypoint BetaPage() {
                            <div>Beta</div>
                        }
                    "#,
                ),
                (
                    "eta",
                    r#"
                        entrypoint EtaPage() {
                            <div>Eta</div>
                        }
                    "#,
                ),
                (
                    "delta",
                    r#"
                        entrypoint DeltaPage() {
                            <div>Delta</div>
                        }
                    "#,
                ),
                (
                    "gamma",
                    r#"
                        entrypoint GammaPage() {
                            <div>Gamma</div>
                        }
                    "#,
                ),
                (
                    "epsilon",
                    r#"
                        entrypoint EpsilonPage() {
                            <div>Epsilon</div>
                        }
                    "#,
                ),
            ],
            // Output should be sorted alphabetically by module name
            expect![[r#"
                entrypoint AlphaPage() {
                  <div>
                    Alpha
                  </div>
                }

                entrypoint BetaPage() {
                  <div>
                    Beta
                  </div>
                }

                entrypoint DeltaPage() {
                  <div>
                    Delta
                  </div>
                }

                entrypoint EpsilonPage() {
                  <div>
                    Epsilon
                  </div>
                }

                entrypoint EtaPage() {
                  <div>
                    Eta
                  </div>
                }

                entrypoint GammaPage() {
                  <div>
                    Gamma
                  </div>
                }

                entrypoint ThetaPage() {
                  <div>
                    Theta
                  </div>
                }

                entrypoint ZetaPage() {
                  <div>
                    Zeta
                  </div>
                }
            "#]],
        );
    }
}
