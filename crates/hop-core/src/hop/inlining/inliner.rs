use super::inlined_ast::{InlinedComponentDeclaration, InlinedViewDeclaration};
use super::inlined_node::InlinedNode;
use crate::document_id::DocumentId;
use crate::expr::patterns::{EnumMatchArm, Match};
use crate::hop::typing::typed_ast::{TypedAst, TypedComponentDeclaration, TypedViewDeclaration};
use crate::hop::typing::typed_node::{TypedAttribute, TypedNode};
use crate::symbols::type_name::TypeName;
use std::collections::{HashMap, HashSet};

/// The Inliner transforms ASTs by replacing ComponentInvocation nodes with their
/// inlined component declarations, using Let nodes for parameter binding and
/// resolving `...rest` spreads into concrete attributes on the spread target.
///
/// Recursive components are detected and emitted as separate component definitions
/// with ComponentInvocation nodes at their reference sites.
pub struct Inliner;

impl Inliner {
    /// Inline all view declarations found in the ASTs.
    pub fn inline_ast_views(
        asts: &HashMap<DocumentId, TypedAst>,
        views: &[TypedViewDeclaration],
    ) -> (
        Vec<InlinedViewDeclaration>,
        Vec<InlinedComponentDeclaration>,
    ) {
        let mut state = InlinerState::new(asts);
        let inlined_views = views
            .iter()
            .map(|view| InlinedViewDeclaration {
                name: view.name.clone(),
                params: view.params.clone(),
                children: state.inline_nodes(&view.children, &[]),
            })
            .collect();
        (inlined_views, state.component_defs)
    }
}

/// Internal state for the inlining process, tracking recursive components
/// and emitted component definitions.
struct InlinerState<'a> {
    asts: &'a HashMap<DocumentId, TypedAst>,
    /// Component names whose definitions have already been emitted
    emitted_defs: HashSet<TypeName>,
    /// Collected component definitions for recursive components
    component_defs: Vec<InlinedComponentDeclaration>,
}

impl<'a> InlinerState<'a> {
    fn new(asts: &'a HashMap<DocumentId, TypedAst>) -> Self {
        Self {
            asts,
            emitted_defs: HashSet::new(),
            component_defs: Vec::new(),
        }
    }

    /// Emit a component definition for a recursive component (if not already emitted).
    fn emit_component_def(&mut self, component: &TypedComponentDeclaration) {
        if self.emitted_defs.contains(&component.component_name) {
            return;
        }
        // Mark as emitted BEFORE processing to prevent infinite recursion
        self.emitted_defs.insert(component.component_name.clone());

        // Inline the component body - self-invocations will become ComponentInvocation
        let inlined_body = self.inline_nodes(&component.children, &[]);

        self.component_defs.push(InlinedComponentDeclaration {
            component_name: component.component_name.clone(),
            params: component.params.clone(),
            children: inlined_body,
        });
    }

    /// Inline nodes, pushing results to output
    fn inline_nodes(
        &mut self,
        nodes: &[TypedNode],
        active_rest: &[TypedAttribute],
    ) -> Vec<InlinedNode> {
        let mut output = Vec::with_capacity(nodes.len());
        for node in nodes {
            self.inline_node(node, &mut output, active_rest);
        }
        output
    }

    /// Inline a single node, pushing results to output
    fn inline_node(
        &mut self,
        node: &TypedNode,
        output: &mut Vec<InlinedNode>,
        active_rest: &[TypedAttribute],
    ) {
        match node {
            TypedNode::ComponentInvocation {
                component_name,
                component_module,
                args,
                extra_attributes,
                rest_spread,
            } => {
                let component = self
                    .asts
                    .get(component_module)
                    .expect("Component module should exist")
                    .get_component_declaration(component_name.as_str())
                    .expect("Component declaration should exist");

                if component.is_recursive {
                    debug_assert!(
                        extra_attributes.is_empty() && rest_spread.is_none(),
                        "recursive components cannot carry rest spreads"
                    );
                    // Emit the component def if not yet emitted
                    self.emit_component_def(component);

                    output.push(InlinedNode::ComponentInvocation {
                        component_name: component_name.clone(),
                        args: args.clone(),
                    });
                } else {
                    let mut forwarded = extra_attributes.clone();
                    if rest_spread.is_some() {
                        forwarded.extend_from_slice(active_rest);
                    }

                    // Inline the body, then nest single-binding Lets from inside out
                    let mut children = self.inline_nodes(&component.children, &forwarded);
                    for binding in args.iter().rev() {
                        children = vec![InlinedNode::Let {
                            var: binding.name.clone(),
                            value: binding.expr.clone(),
                            children,
                        }];
                    }
                    output.extend(children);
                }
            }

            TypedNode::Html {
                element,
                attributes,
                rest_spread,
                children,
            } => {
                let mut attributes = attributes.clone();
                if rest_spread.is_some() {
                    attributes.extend_from_slice(active_rest);
                }
                output.push(InlinedNode::Html {
                    element: element.clone(),
                    attributes,
                    children: self.inline_nodes(children, active_rest),
                });
            }

            TypedNode::If {
                condition,
                children,
            } => {
                output.push(InlinedNode::If {
                    condition: condition.clone(),
                    children: self.inline_nodes(children, active_rest),
                });
            }

            TypedNode::For {
                var_name,
                source,
                children,
            } => {
                output.push(InlinedNode::For {
                    var_name: var_name.clone(),
                    source: source.clone(),
                    children: self.inline_nodes(children, active_rest),
                });
            }

            TypedNode::Text { value } => {
                output.push(InlinedNode::Text {
                    value: value.clone(),
                });
            }

            TypedNode::TextExpression { expression } => {
                output.push(InlinedNode::TextExpression {
                    expression: expression.clone(),
                });
            }

            TypedNode::Doctype { value } => {
                output.push(InlinedNode::Doctype {
                    value: value.clone(),
                });
            }

            TypedNode::Match { match_ } => {
                let inlined_match = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => {
                        let true_output = self.inline_nodes(true_body, active_rest);
                        let false_output = self.inline_nodes(false_body, active_rest);
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
                    } => Match::Option {
                        subject: subject.clone(),
                        some_arm_binding: some_arm_binding.clone(),
                        some_arm_body: Box::new(self.inline_nodes(some_arm_body, active_rest)),
                        none_arm_body: Box::new(self.inline_nodes(none_arm_body, active_rest)),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: subject.clone(),
                        arms: arms
                            .iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                bindings: arm.bindings.clone(),
                                body: self.inline_nodes(&arm.body, active_rest),
                            })
                            .collect(),
                    },
                };
                output.push(InlinedNode::Match {
                    match_: inlined_match,
                });
            }

            TypedNode::Let {
                var,
                value,
                children,
            } => {
                output.push(InlinedNode::Let {
                    var: var.clone(),
                    value: value.clone(),
                    children: self.inline_nodes(children, active_rest),
                });
            }

            TypedNode::LetFragment {
                var,
                fragment_body,
                body,
            } => {
                output.push(InlinedNode::LetFragment {
                    var: var.clone(),
                    fragment_body: self.inline_nodes(fragment_body, active_rest),
                    body: self.inline_nodes(body, active_rest),
                });
            }

            TypedNode::LetRecordDestructure {
                subject,
                bindings,
                children,
            } => {
                output.push(InlinedNode::LetRecordDestructure {
                    subject: subject.clone(),
                    bindings: bindings.clone(),
                    children: self.inline_nodes(children, active_rest),
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::hop::parsing::parser::parse;
    use crate::hop::typing::type_checker::typecheck;
    use expect_test::{Expect, expect};

    fn create_typed_asts_from_sources(sources: Vec<(&str, &str)>) -> HashMap<DocumentId, TypedAst> {
        let mut errors = Vec::new();

        let mut untyped_asts = HashMap::new();
        for (document_id_str, source) in sources {
            let document_id = DocumentId::new(document_id_str).unwrap();
            let ast = parse(
                document_id.clone(),
                Document::new(document_id.clone(), source.to_string()),
                &mut errors,
            );
            untyped_asts.insert(document_id, ast);
        }

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        let mut state = HashMap::new();
        let mut type_errors = HashMap::new();
        let mut type_annotations = HashMap::new();
        let mut definition_links = HashMap::new();
        let mut asset_references = HashMap::new();
        let mut typed_asts = HashMap::new();

        let untyped_asts_refs: Vec<_> = untyped_asts.values().collect();
        typecheck(
            &untyped_asts_refs,
            &mut state,
            &mut typed_asts,
            &mut type_errors,
            &mut type_annotations,
            &mut definition_links,
            &mut asset_references,
        );

        for document_id in untyped_asts.keys() {
            assert!(
                type_errors.get(document_id).unwrap().is_empty(),
                "Type errors in {}: {:?}",
                document_id,
                type_errors.get(document_id).unwrap()
            );
        }

        typed_asts
    }

    fn check(sources: Vec<(&str, &str)>, expected: Expect) {
        let typed_asts = create_typed_asts_from_sources(sources);

        let mut document_ids: Vec<_> = typed_asts.keys().collect();
        document_ids.sort();
        let views: Vec<_> = document_ids
            .iter()
            .flat_map(|id| typed_asts[*id].get_view_declarations())
            .cloned()
            .collect();

        let (views, component_defs) = Inliner::inline_ast_views(&typed_asts, &views);

        let mut parts: Vec<String> = Vec::new();
        for def in &component_defs {
            parts.push(format!("{}\n", def.to_doc().pretty(60)));
        }
        for ep in &views {
            parts.push(format!("{}\n", ep.to_doc().pretty(60)));
        }
        let output = parts.join("\n");

        expected.assert_eq(&output);
    }

    #[test]
    fn html_target_spread_appends_extra_attributes() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Button(label: String, ...rest) {
                        <button class="btn" ...rest>{label}</button>
                    }

                    view Main() {
                        <Button label="Hi" id="submit" type="button"/>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
                  <let {label = "Hi"}>
                    <button class="btn" id="submit" type="button">
                      {label}
                    </button>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn forwards_rest_through_nested_component() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Base(...rest) {
                        <div ...rest></div>
                    }

                    component Card(title: String, ...rest) {
                        <div>
                            <h1>{title}</h1>
                            <Base ...rest/>
                        </div>
                    }

                    view Main() {
                        <Card title="Hi" id="x"/>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
                  <let {title = "Hi"}>
                    <div>
                      <h1>
                        {title}
                      </h1>
                      <div id="x"></div>
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn spread_target_nested_in_control_flow() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Wrapper(show: Bool, ...rest) {
                        <if {show}>
                            <div ...rest></div>
                        </if>
                    }

                    view Main() {
                        <Wrapper show={true} id="x"/>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
                  <let {show = true}>
                    <if {show}>
                      <div id="x"></div>
                    </if>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn empty_rest_appends_nothing() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Box(...rest) {
                        <div ...rest></div>
                    }

                    view Main() {
                        <Box/>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
                  <div></div>
                }
            "#]],
        );
    }

    #[test]
    fn view_without_parameters() {
        check(
            vec![(
                "main.hop",
                r#"
                    view Main() {
                        <div>Hello</div>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
                  <div>
                    Hello
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn view_with_parameters() {
        check(
            vec![(
                "main.hop",
                r#"
                    view Main(name: String) {
                        <div>Hello, {name}!</div>
                    }
                "#,
            )],
            expect![[r#"
                view Main(name: String) {
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
    fn view_referencing_component() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Greeting(name: String) {
                        <div>Hello, {name}!</div>
                    }

                    view Main(name: String) {
                        <Greeting name={name} />
                    }
                "#,
            )],
            expect![[r#"
                view Main(name: String) {
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
    fn multiple_views_in_same_module() {
        check(
            vec![(
                "main.hop",
                r#"
                    view Home() {
                        <div>Home page</div>
                    }

                    view About() {
                        <div>About page</div>
                    }
                "#,
            )],
            expect![[r#"
                view Home() {
                  <div>
                    Home page
                  </div>
                }

                view About() {
                  <div>
                    About page
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn views_across_multiple_modules() {
        check(
            vec![
                (
                    "alpha.hop",
                    r#"
                        view AlphaPage() {
                            <div>Alpha</div>
                        }
                    "#,
                ),
                (
                    "beta.hop",
                    r#"
                        view BetaPage() {
                            <div>Beta</div>
                        }
                    "#,
                ),
            ],
            expect![[r#"
                view AlphaPage() {
                  <div>
                    Alpha
                  </div>
                }

                view BetaPage() {
                  <div>
                    Beta
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn view_with_component_using_child_content() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Card(children: Fragment) {
                        <div class="card">
                            {children}
                        </div>
                    }

                    view Main() {
                        <Card>
                            <p>content</p>
                        </Card>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
                  <let {
                    v_0 = {
                      <p>
                        content
                      </p>
                    }
                  }>
                    <let {children = v_0}>
                      <div class="card">
                        {children}
                      </div>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn view_with_nested_component_inlining() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Inner(text: String) {
                        <span>{text}</span>
                    }

                    component Outer(label: String) {
                        <div class="outer">
                            <Inner text={label} />
                        </div>
                    }

                    view Main(title: String) {
                        <Outer label={title} />
                    }
                "#,
            )],
            expect![[r#"
                view Main(title: String) {
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
    fn view_with_conditional_rendering() {
        check(
            vec![(
                "main.hop",
                r#"
                    view Main(show_greeting: Bool, name: String) {
                        <div>
                            <if {show_greeting}>
                                <p>Hello, {name}!</p>
                            </if>
                        </div>
                    }
                "#,
            )],
            expect![[r#"
                view Main(show_greeting: Bool, name: String) {
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
    fn view_with_loop() {
        check(
            vec![(
                "main.hop",
                r#"
                    view Main(count: Int) {
                        <ul>
                            <for {i in 1..=count}>
                                <li>Item {i.to_string()}</li>
                            </for>
                        </ul>
                    }
                "#,
            )],
            expect![[r#"
                view Main(count: Int) {
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
    fn view_with_component_default_value_used() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Button(label: String = "Click me") {
                        <button>{label}</button>
                    }

                    view Main() {
                        <div>
                            <Button />
                        </div>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
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
    fn view_complex_page_layout() {
        check(
            vec![(
                "main.hop",
                r#"
                    component Header(title: String) {
                        <header>
                            <h1>{title}</h1>
                        </header>
                    }

                    component Footer {
                        <footer>
                            <p>Copyright 2024</p>
                        </footer>
                    }

                    component Layout(children: Fragment) {
                        <div class="layout">
                            {children}
                        </div>
                    }

                    view HomePage(page_title: String) {
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
                view HomePage(page_title: String) {
                  <let {
                    v_0 = {
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
                    }
                  }>
                    <let {children = v_0}>
                      <div class="layout">
                        {children}
                      </div>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn deterministic_output_order_with_multiple_modules() {
        check(
            vec![
                (
                    "zeta.hop",
                    r#"
                        view ZetaPage() {
                            <div>Zeta</div>
                        }
                    "#,
                ),
                (
                    "alpha.hop",
                    r#"
                        view AlphaPage() {
                            <div>Alpha</div>
                        }
                    "#,
                ),
                (
                    "theta.hop",
                    r#"
                        view ThetaPage() {
                            <div>Theta</div>
                        }
                    "#,
                ),
                (
                    "beta.hop",
                    r#"
                        view BetaPage() {
                            <div>Beta</div>
                        }
                    "#,
                ),
                (
                    "eta.hop",
                    r#"
                        view EtaPage() {
                            <div>Eta</div>
                        }
                    "#,
                ),
                (
                    "delta.hop",
                    r#"
                        view DeltaPage() {
                            <div>Delta</div>
                        }
                    "#,
                ),
                (
                    "gamma.hop",
                    r#"
                        view GammaPage() {
                            <div>Gamma</div>
                        }
                    "#,
                ),
                (
                    "epsilon.hop",
                    r#"
                        view EpsilonPage() {
                            <div>Epsilon</div>
                        }
                    "#,
                ),
            ],
            expect![[r#"
                view AlphaPage() {
                  <div>
                    Alpha
                  </div>
                }

                view BetaPage() {
                  <div>
                    Beta
                  </div>
                }

                view DeltaPage() {
                  <div>
                    Delta
                  </div>
                }

                view EpsilonPage() {
                  <div>
                    Epsilon
                  </div>
                }

                view EtaPage() {
                  <div>
                    Eta
                  </div>
                }

                view GammaPage() {
                  <div>
                    Gamma
                  </div>
                }

                view ThetaPage() {
                  <div>
                    Theta
                  </div>
                }

                view ZetaPage() {
                  <div>
                    Zeta
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn recursive_component_produces_component_def_and_call() {
        check(
            vec![(
                "main.hop",
                r#"
                    record Node {
                      value: String,
                      next: Option[Node],
                    }

                    component NodeView(node: Node) {
                        <span>{node.value}</span>
                        <match {node.next}>
                            <case {Some(next)}>
                                <NodeView node={next} />
                            </case>
                            <case {None}>
                            </case>
                        </match>
                    }

                    view Test(node: Node) {
                        <NodeView node={node} />
                    }
                "#,
            )],
            expect![[r#"
                component NodeView(node: main::Node) {
                  <span>
                    {node.value}
                  </span>
                  <match {node.next}>
                    <case {Some(v_1)}>
                      <let {next = v_1}>
                        <NodeView node={next}/>
                      </let>
                    </case>
                    <case {None}></case>
                  </match>
                }

                view Test(node: main::Node) {
                  <NodeView node={node}/>
                }
            "#]],
        );
    }

    #[test]
    fn recursive_component_with_non_recursive_sibling() {
        check(
            vec![(
                "main.hop",
                r#"
                    record Node {
                      value: String,
                      next: Option[Node],
                    }

                    component Badge(text: String) {
                        <strong>{text}</strong>
                    }

                    component NodeView(node: Node) {
                        <Badge text={node.value} />
                        <match {node.next}>
                            <case {Some(next)}>
                                <NodeView node={next} />
                            </case>
                            <case {None}>
                            </case>
                        </match>
                    }

                    view Test(node: Node) {
                        <NodeView node={node} />
                    }
                "#,
            )],
            expect![[r#"
                component NodeView(node: main::Node) {
                  <let {text = node.value}>
                    <strong>
                      {text}
                    </strong>
                  </let>
                  <match {node.next}>
                    <case {Some(v_1)}>
                      <let {next = v_1}>
                        <NodeView node={next}/>
                      </let>
                    </case>
                    <case {None}></case>
                  </match>
                }

                view Test(node: main::Node) {
                  <NodeView node={node}/>
                }
            "#]],
        );
    }
}
