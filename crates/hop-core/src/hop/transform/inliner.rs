use crate::document_id::DocumentId;
use crate::dop::patterns::{EnumMatchArm, Match};
use crate::dop::Type;
use crate::hop::typing::typed_ast::{TypedAst, TypedComponentDeclaration, TypedViewDeclaration};
use crate::hop::typing::typed_node::{TypedArgument, TypedNode};
use crate::symbols::type_name::TypeName;
use std::collections::{HashMap, HashSet};

/// The Inliner transforms ASTs by replacing ComponentInvocation nodes with their
/// inlined component declarations, using Let nodes for parameter binding and
/// StringConcat for class attribute merging.
///
/// Recursive components are detected and emitted as separate component definitions
/// with ComponentInvocation nodes at their reference sites.
pub struct Inliner;

impl Inliner {
    /// Inline all view declarations found in the ASTs.
    pub fn inline_ast_views(
        asts: &HashMap<DocumentId, TypedAst>,
        views: &mut [TypedViewDeclaration],
    ) -> Vec<TypedComponentDeclaration> {
        let mut state = InlinerState::new(asts);
        for ep in views.iter_mut() {
            state.inline_view(ep);
        }
        state.component_defs
    }
}

/// Internal state for the inlining process, tracking recursive components
/// and emitted component definitions.
struct InlinerState<'a> {
    asts: &'a HashMap<DocumentId, TypedAst>,
    /// Component names that are self-referential and need function emission
    recursive_components: HashSet<TypeName>,
    /// Component names whose definitions have already been emitted
    emitted_defs: HashSet<TypeName>,
    /// Collected component definitions for recursive components
    component_defs: Vec<TypedComponentDeclaration>,
}

impl<'a> InlinerState<'a> {
    fn new(asts: &'a HashMap<DocumentId, TypedAst>) -> Self {
        let recursive_components = Self::collect_recursive_components(asts);
        Self {
            asts,
            recursive_components,
            emitted_defs: HashSet::new(),
            component_defs: Vec::new(),
        }
    }

    /// Collect component names that were marked as self-referential by the type checker.
    fn collect_recursive_components(asts: &HashMap<DocumentId, TypedAst>) -> HashSet<TypeName> {
        let mut recursive = HashSet::new();
        for ast in asts.values() {
            for component in ast.get_component_declarations() {
                if component.is_recursive {
                    recursive.insert(component.component_name.clone());
                }
            }
        }
        recursive
    }

    /// Emit a component definition for a recursive component (if not already emitted).
    fn emit_component_def(&mut self, module: &DocumentId, component_name: &TypeName) {
        if self.emitted_defs.contains(component_name) {
            return;
        }
        // Mark as emitted BEFORE processing to prevent infinite recursion
        self.emitted_defs.insert(component_name.clone());

        let component = self
            .asts
            .get(module)
            .expect("Component module should exist")
            .get_component_declaration(component_name.as_str())
            .expect("Component declaration should exist");

        // Inline the component body - self-references will become ComponentInvocation
        let inlined_body = self.inline_nodes(&component.children, None);

        self.component_defs.push(TypedComponentDeclaration {
            component_name: component.component_name.clone(),
            params: component.params.clone(),
            children: inlined_body,
            is_recursive: true,
            slot: component.slot.clone(),
        });
    }

    /// Resolve call arguments for a component, filling in default values
    /// from the component type in parameter definition order.
    fn resolve_call_args(component_type: &Type, args: &[TypedArgument]) -> Vec<TypedArgument> {
        let Type::Component {
            parameters, name, ..
        } = component_type
        else {
            unreachable!("ComponentInvocation must have Component type");
        };
        parameters
            .iter()
            .map(|(param_name, _, default_value)| {
                let value = args
                    .iter()
                    .find(|arg| arg.name.as_str() == param_name.as_str())
                    .map(|arg| arg.expr.clone())
                    .or_else(|| default_value.clone())
                    .unwrap_or_else(|| {
                        panic!(
                            "Missing required parameter '{}' for component '{}'.",
                            param_name, name
                        )
                    });
                TypedArgument {
                    name: param_name.clone(),
                    expr: value,
                }
            })
            .collect()
    }

    fn inline_view(&mut self, view: &mut TypedViewDeclaration) {
        let old_children = std::mem::take(&mut view.children);
        view.children = self.inline_nodes(&old_children, None);
    }

    /// Inline a component reference, pushing results to output
    fn inline_component_reference(
        &mut self,
        component: &TypedComponentDeclaration,
        component_type: &Type,
        args: &[TypedArgument],
        slot_children: &[TypedNode],
        parent_slot_content: Option<&[TypedNode]>,
        output: &mut Vec<TypedNode>,
    ) {
        let has_slot = component.slot.is_some();

        // Inline slot_children in parent context
        let inlined_slot_content: Option<Vec<TypedNode>> = if has_slot && !slot_children.is_empty()
        {
            let content = self.inline_nodes(slot_children, parent_slot_content);
            Some(content)
        } else if has_slot {
            Some(vec![])
        } else {
            None
        };

        let inlined_children =
            self.inline_nodes(&component.children, inlined_slot_content.as_deref());

        let bindings = Self::resolve_call_args(component_type, args);

        if bindings.is_empty() {
            output.extend(inlined_children);
        } else {
            // Nest single-binding Lets from inside out
            let mut children = inlined_children;
            for binding in bindings.into_iter().rev() {
                children = vec![TypedNode::Let {
                    var: binding.name,
                    value: binding.expr,
                    children,
                }];
            }
            output.extend(children);
        }
    }

    /// Inline nodes, pushing results to output
    fn inline_nodes(
        &mut self,
        nodes: &[TypedNode],
        slot_content: Option<&[TypedNode]>,
    ) -> Vec<TypedNode> {
        let mut output = Vec::with_capacity(nodes.len());
        for node in nodes {
            self.inline_node(node, slot_content, &mut output);
        }
        output
    }

    /// Inline a single node, pushing results to output
    fn inline_node(
        &mut self,
        node: &TypedNode,
        slot_content: Option<&[TypedNode]>,
        output: &mut Vec<TypedNode>,
    ) {
        match node {
            TypedNode::Slot => match slot_content {
                Some(content) => output.extend_from_slice(content),
                None => output.push(TypedNode::Slot),
            },
            TypedNode::ComponentInvocation {
                component_name,
                component_type,
                args,
                children,
            } => {
                let Type::Component { module, slot, .. } = component_type.as_ref() else {
                    unreachable!("ComponentInvocation must have Component type");
                };

                if self.recursive_components.contains(component_name) {
                    // Emit the component def if not yet emitted
                    self.emit_component_def(module, component_name);

                    let resolved_args = Self::resolve_call_args(component_type, args);

                    // Inline slot children in the parent context
                    let inlined_children = if slot.is_some() && !children.is_empty() {
                        self.inline_nodes(children, slot_content)
                    } else {
                        vec![]
                    };

                    output.push(TypedNode::ComponentInvocation {
                        component_name: component_name.clone(),
                        component_type: component_type.clone(),
                        args: resolved_args,
                        children: inlined_children,
                    });
                } else {
                    let component = self
                        .asts
                        .get(module)
                        .expect("Component module should exist")
                        .get_component_declaration(component_name.as_str())
                        .expect("Component declaration should exist");

                    self.inline_component_reference(
                        component,
                        component_type,
                        args,
                        children,
                        slot_content,
                        output,
                    );
                }
            }

            TypedNode::Html {
                tag_name,
                attributes,
                children,
            } => {
                output.push(TypedNode::Html {
                    tag_name: tag_name.clone(),
                    attributes: attributes.clone(),
                    children: self.inline_nodes(children, slot_content),
                });
            }

            TypedNode::If {
                condition,
                children,
            } => {
                output.push(TypedNode::If {
                    condition: condition.clone(),
                    children: self.inline_nodes(children, slot_content),
                });
            }

            TypedNode::For {
                var_name,
                source,
                children,
            } => {
                output.push(TypedNode::For {
                    var_name: var_name.clone(),
                    source: source.clone(),
                    children: self.inline_nodes(children, slot_content),
                });
            }

            TypedNode::Text { value } => {
                output.push(TypedNode::Text {
                    value: value.clone(),
                });
            }

            TypedNode::TextExpression { expression } => {
                output.push(TypedNode::TextExpression {
                    expression: expression.clone(),
                });
            }

            TypedNode::Doctype { value } => {
                output.push(TypedNode::Doctype {
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
                        let true_output = self.inline_nodes(true_body, slot_content);
                        let false_output = self.inline_nodes(false_body, slot_content);
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
                        some_arm_body: Box::new(self.inline_nodes(some_arm_body, slot_content)),
                        none_arm_body: Box::new(self.inline_nodes(none_arm_body, slot_content)),
                    },
                    Match::Enum { subject, arms } => Match::Enum {
                        subject: subject.clone(),
                        arms: arms
                            .iter()
                            .map(|arm| EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                bindings: arm.bindings.clone(),
                                body: self.inline_nodes(&arm.body, slot_content),
                            })
                            .collect(),
                    },
                };
                output.push(TypedNode::Match {
                    match_: inlined_match,
                });
            }

            TypedNode::Let {
                var,
                value,
                children,
            } => {
                output.push(TypedNode::Let {
                    var: var.clone(),
                    value: value.clone(),
                    children: self.inline_nodes(children, slot_content),
                });
            }

            TypedNode::LetRecordDestructure {
                subject,
                bindings,
                children,
            } => {
                output.push(TypedNode::LetRecordDestructure {
                    subject: subject.clone(),
                    bindings: bindings.clone(),
                    children: self.inline_nodes(children, slot_content),
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
            &mut type_errors,
            &mut type_annotations,
            &mut definition_links,
            &mut typed_asts,
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

    fn check_view_inlining(sources: Vec<(&str, &str)>, expected: Expect) {
        let typed_asts = create_typed_asts_from_sources(sources);

        let mut document_ids: Vec<_> = typed_asts.keys().collect();
        document_ids.sort();
        let mut views: Vec<_> = document_ids
            .iter()
            .flat_map(|id| typed_asts[*id].get_view_declarations())
            .cloned()
            .collect();

        let component_defs = Inliner::inline_ast_views(&typed_asts, &mut views);

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
    fn view_without_parameters() {
        check_view_inlining(
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
        check_view_inlining(
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
        check_view_inlining(
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
        check_view_inlining(
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
        check_view_inlining(
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
    fn view_with_component_using_slot_content() {
        check_view_inlining(
            vec![(
                "main.hop",
                r#"
                    component Card(slot: Slot) {
                        <div class="card">
                            {slot}
                        </div>
                    }

                    view Main() {
                        <Card>
                            <p>This is slot content</p>
                        </Card>
                    }
                "#,
            )],
            expect![[r#"
                view Main() {
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
    fn view_with_nested_component_inlining() {
        check_view_inlining(
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
        check_view_inlining(
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
        check_view_inlining(
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
        check_view_inlining(
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
        check_view_inlining(
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

                    component Layout(slot: Slot) {
                        <div class="layout">
                            {slot}
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
        check_view_inlining(
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
        check_view_inlining(
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
                <NodeView {node: main::Node}>
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
                </NodeView>

                view Test(node: main::Node) {
                  <NodeView node={node}/>
                }
            "#]],
        );
    }

    #[test]
    fn recursive_component_with_non_recursive_sibling() {
        check_view_inlining(
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
                <NodeView {node: main::Node}>
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
                </NodeView>

                view Test(node: main::Node) {
                  <NodeView node={node}/>
                }
            "#]],
        );
    }

    #[test]
    fn recursive_component_with_children_slot() {
        check_view_inlining(
            vec![(
                "main.hop",
                r#"
                    record Node {
                      value: String,
                      next: Option[Node],
                    }

                    component NodeView(node: Node, slot: Slot) {
                        <div>
                            {slot}
                            <match {node.next}>
                                <case {Some(next)}>
                                    <NodeView node={next}>
                                        <span>{node.value}</span>
                                    </NodeView>
                                </case>
                                <case {None}></case>
                            </match>
                        </div>
                    }

                    view Test(node: Node) {
                        <NodeView node={node}>
                            <p>Root content</p>
                        </NodeView>
                    }
                "#,
            )],
            expect![[r#"
                <NodeView {node: main::Node, slot: Slot}>
                  <div>
                    {slot}
                    <match {node.next}>
                      <case {Some(v_1)}>
                        <let {next = v_1}>
                          <NodeView node={next}>
                            <span>
                              {node.value}
                            </span>
                          </NodeView>
                        </let>
                      </case>
                      <case {None}></case>
                    </match>
                  </div>
                </NodeView>

                view Test(node: main::Node) {
                  <NodeView node={node}>
                    <p>
                      Root content
                    </p>
                  </NodeView>
                }
            "#]],
        );
    }
}
