use crate::dop::VarName;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::dop::{Type, TypedExpr};
use crate::hop::semantics::typed_ast::{
    TypedAst, TypedComponentDeclaration, TypedEntrypointDeclaration,
};
use crate::hop::semantics::typed_node::{TypedAttribute, TypedAttributeValue, TypedNode};
use crate::hop::symbols::module_id::ModuleId;
use crate::inlined::inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypointDeclaration, InlinedNode,
    InlinedParameter,
};
use std::collections::HashMap;
use std::sync::Arc;

/// The Inliner transforms ASTs by replacing ComponentReference nodes with their
/// inlined component declarations, using Let nodes for parameter binding and
/// StringConcat for class attribute merging.
pub struct Inliner;

impl Inliner {
    /// Inline all entrypoint declarations found in the ASTs.
    pub fn inline_ast_entrypoints(
        asts: &HashMap<ModuleId, TypedAst>,
    ) -> Vec<InlinedEntrypointDeclaration> {
        let mut module_ids: Vec<_> = asts.keys().collect();
        module_ids.sort();

        module_ids
            .into_iter()
            .flat_map(|module_id| {
                let ast = asts.get(module_id).unwrap();
                ast.get_entrypoint_declarations()
                    .iter()
                    .map(|entrypoint| Self::inline_single_entrypoint(module_id, entrypoint, asts))
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    /// Inline a single entrypoint declaration.
    pub fn inline_single_entrypoint(
        module_id: &ModuleId,
        entrypoint: &TypedEntrypointDeclaration,
        asts: &HashMap<ModuleId, TypedAst>,
    ) -> InlinedEntrypointDeclaration {
        let mut children = Vec::new();
        let mut children_vars = Vec::new();
        Self::inline_nodes(
            &entrypoint.children,
            asts,
            None,
            &mut children_vars,
            &mut children,
        );

        InlinedEntrypointDeclaration {
            module_id: module_id.clone(),
            component_name: entrypoint.name.clone(),
            children,
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
            .map(|(_, var_type, _)| var_type.as_ref())
    }

    /// Inline a component reference, pushing results to output
    fn inline_component_reference(
        module_id: &ModuleId,
        component: &TypedComponentDeclaration,
        args: &[(VarName, TypedExpr)],
        slot_children: &[TypedNode],
        parent_slot_content: Option<&[Arc<InlinedNode>]>,
        parent_children_vars: &mut Vec<String>,
        asts: &HashMap<ModuleId, TypedAst>,
        output: &mut Vec<Arc<InlinedNode>>,
    ) {
        // Inline slot_children in parent context
        let children_type = Self::get_children_param_type(component);
        let inlined_slot_content: Option<Vec<Arc<InlinedNode>>> =
            if children_type.is_some() && !slot_children.is_empty() {
                let mut content = Vec::new();
                Self::inline_nodes(
                    slot_children,
                    asts,
                    parent_slot_content,
                    parent_children_vars,
                    &mut content,
                );
                Some(content)
            } else {
                None
            };

        // Create fresh children_vars for this component
        let mut children_vars = Vec::new();
        match children_type {
            Some(Type::TrustedHTML | Type::Option(_)) => {
                children_vars.push("children".to_string());
            }
            Some(other) => {
                panic!("Invalid children parameter type: {other}");
            }
            None => {}
        }

        let mut inlined_children = Vec::new();
        Self::inline_nodes(
            &component.children,
            asts,
            inlined_slot_content.as_deref(),
            &mut children_vars,
            &mut inlined_children,
        );

        // Build parameter bindings (excluding children - handled via slot mechanism)
        let bindings: Vec<(VarName, TypedExpr)> = component
            .params
            .iter()
            .filter(|(var_name, _, _)| var_name.as_str() != "children")
            .map(|(var_name, _var_type, default_value)| {
                let value = args
                    .iter()
                    .find(|(name, _)| name.as_str() == var_name.as_str())
                    .map(|(_, expr)| expr.clone())
                    .or_else(|| default_value.clone())
                    .unwrap_or_else(|| {
                        panic!(
                            "Missing required parameter '{}' for component '{}' in module '{}'.",
                            var_name,
                            component.component_name.as_str(),
                            module_id
                        )
                    });
                (var_name.clone(), value)
            })
            .collect();

        if bindings.is_empty() {
            output.extend(inlined_children);
        } else {
            output.push(Arc::new(InlinedNode::Let {
                bindings,
                children: inlined_children,
            }));
        }
    }

    /// Inline nodes, pushing results to output
    fn inline_nodes(
        nodes: &[TypedNode],
        asts: &HashMap<ModuleId, TypedAst>,
        slot_content: Option<&[Arc<InlinedNode>]>,
        children_vars: &mut Vec<String>,
        output: &mut Vec<Arc<InlinedNode>>,
    ) {
        for node in nodes {
            Self::inline_node(node, asts, slot_content, children_vars, output);
        }
    }

    /// Inline a single node, pushing results to output
    fn inline_node(
        node: &TypedNode,
        asts: &HashMap<ModuleId, TypedAst>,
        slot_content: Option<&[Arc<InlinedNode>]>,
        children_vars: &mut Vec<String>,
        output: &mut Vec<Arc<InlinedNode>>,
    ) {
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
                let ast = asts.get(module).expect("Component module should exist");
                let component = ast
                    .get_component_declaration(component_name.as_str())
                    .expect("Component declaration should exist");

                Self::inline_component_reference(
                    module,
                    component,
                    args,
                    children,
                    slot_content,
                    children_vars,
                    asts,
                    output,
                );
            }

            TypedNode::Html {
                tag_name,
                attributes,
                children,
            } => {
                let mut child_output = Vec::new();
                Self::inline_nodes(
                    children,
                    asts,
                    slot_content,
                    children_vars,
                    &mut child_output,
                );
                output.push(Arc::new(InlinedNode::Html {
                    tag_name: tag_name.clone(),
                    attributes: Self::convert_attributes(attributes),
                    children: child_output,
                }));
            }

            TypedNode::If {
                condition,
                children,
            } => {
                let mut child_output = Vec::new();
                Self::inline_nodes(
                    children,
                    asts,
                    slot_content,
                    children_vars,
                    &mut child_output,
                );
                output.push(Arc::new(InlinedNode::If {
                    condition: condition.clone(),
                    children: child_output,
                }));
            }

            TypedNode::For {
                var_name,
                source,
                children,
            } => {
                let mut child_output = Vec::new();
                Self::inline_nodes(
                    children,
                    asts,
                    slot_content,
                    children_vars,
                    &mut child_output,
                );
                output.push(Arc::new(InlinedNode::For {
                    var_name: var_name.clone(),
                    source: source.clone(),
                    children: child_output,
                }));
            }

            TypedNode::Text { value } => {
                output.push(Arc::new(InlinedNode::Text {
                    value: value.clone(),
                }));
            }

            TypedNode::TextExpression { expression } => {
                if let TypedExpr::Var { value, kind, .. } = expression {
                    if children_vars.iter().any(|v| v == value.as_str()) {
                        assert_eq!(
                            **kind,
                            Type::TrustedHTML,
                            "children-derived variable in TextExpression must be TrustedHTML"
                        );
                        output.extend_from_slice(slot_content.expect(
                            "children-derived TrustedHTML variable should have slot content",
                        ));
                        return;
                    }
                }
                output.push(Arc::new(InlinedNode::TextExpression {
                    expression: expression.clone(),
                }));
            }

            TypedNode::Doctype { value } => {
                output.push(Arc::new(InlinedNode::Doctype {
                    value: value.clone(),
                }));
            }

            TypedNode::Match { match_ } => {
                let inlined_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => {
                        let mut true_output = Vec::new();
                        Self::inline_nodes(
                            true_body,
                            asts,
                            slot_content,
                            children_vars,
                            &mut true_output,
                        );
                        let mut false_output = Vec::new();
                        Self::inline_nodes(
                            false_body,
                            asts,
                            slot_content,
                            children_vars,
                            &mut false_output,
                        );
                        Match::Bool {
                            subject: subject.clone(),
                            true_body: Box::new(true_output),
                            false_body: Box::new(false_output),
                        }
                    }
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        if children_vars.iter().any(|v| v == subject.0.as_str()) {
                            assert!(
                                matches!(subject.1.as_ref(), Type::Option(inner) if **inner == Type::TrustedHTML),
                                "children-holding variable in Match::Option must be Option[TrustedHTML]"
                            );
                            // Statically resolve based on whether slot content was provided
                            if slot_content.is_some() {
                                if let Some(binding) = some_arm_binding {
                                    children_vars.push(binding.as_str().to_string());
                                    Self::inline_nodes(
                                        some_arm_body,
                                        asts,
                                        slot_content,
                                        children_vars,
                                        output,
                                    );
                                    children_vars.pop();
                                } else {
                                    Self::inline_nodes(
                                        some_arm_body,
                                        asts,
                                        slot_content,
                                        children_vars,
                                        output,
                                    );
                                }
                            } else {
                                let mut fresh_vars = Vec::new();
                                Self::inline_nodes(
                                    none_arm_body,
                                    asts,
                                    None,
                                    &mut fresh_vars,
                                    output,
                                );
                            }
                            return;
                        }

                        // Regular Option match
                        let mut some_output = Vec::new();
                        Self::inline_nodes(
                            some_arm_body,
                            asts,
                            slot_content,
                            children_vars,
                            &mut some_output,
                        );
                        let mut none_output = Vec::new();
                        Self::inline_nodes(
                            none_arm_body,
                            asts,
                            slot_content,
                            children_vars,
                            &mut none_output,
                        );
                        Match::Option {
                            subject: subject.clone(),
                            some_arm_binding: some_arm_binding.clone(),
                            some_arm_body: Box::new(some_output),
                            none_arm_body: Box::new(none_output),
                        }
                    }
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: subject.clone(),
                        arms: arms
                            .iter()
                            .map(|arm| {
                                let mut arm_output = Vec::new();
                                Self::inline_nodes(
                                    &arm.body,
                                    asts,
                                    slot_content,
                                    children_vars,
                                    &mut arm_output,
                                );
                                EnumMatchArm {
                                    pattern: arm.pattern.clone(),
                                    bindings: arm.bindings.clone(),
                                    body: arm_output,
                                }
                            })
                            .collect(),
                    },
                };
                output.push(Arc::new(InlinedNode::Match {
                    match_: inlined_match,
                }));
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
                    if children_vars.iter().any(|v| v == var_value.as_str()) {
                        children_vars.push(var.as_str().to_string());
                        Self::inline_nodes(children, asts, slot_content, children_vars, output);
                        children_vars.pop();
                        return;
                    }
                }

                let mut child_output = Vec::new();
                Self::inline_nodes(
                    children,
                    asts,
                    slot_content,
                    children_vars,
                    &mut child_output,
                );
                output.push(Arc::new(InlinedNode::Let {
                    bindings: vec![(var.clone(), value.clone())],
                    children: child_output,
                }));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::error_collector::ErrorCollector;
    use crate::hop::semantics::type_checker::typecheck;
    use crate::hop::symbols::module_id::ModuleId;
    use crate::hop::syntax::parser::parse;
    use expect_test::{Expect, expect};

    fn create_typed_asts_from_sources(sources: Vec<(&str, &str)>) -> HashMap<ModuleId, TypedAst> {
        let mut errors = ErrorCollector::new();

        let mut untyped_asts = HashMap::new();
        for (module_id_str, source) in sources {
            let module_id = ModuleId::new(module_id_str).unwrap();
            let ast = parse(
                module_id.clone(),
                Document::new(source.to_string()),
                &mut errors,
            );
            untyped_asts.insert(module_id, ast);
        }

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        let mut state = HashMap::new();
        let mut type_errors = HashMap::new();
        let mut type_annotations = HashMap::new();
        let mut typed_asts = HashMap::new();

        let untyped_asts_refs: Vec<_> = untyped_asts.values().collect();
        typecheck(
            &untyped_asts_refs,
            &mut state,
            &mut type_errors,
            &mut type_annotations,
            &mut typed_asts,
        );

        for module_id in untyped_asts.keys() {
            assert!(
                type_errors.get(module_id).unwrap().is_empty(),
                "Type errors in {}: {:?}",
                module_id,
                type_errors.get(module_id).unwrap()
            );
        }

        typed_asts
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
