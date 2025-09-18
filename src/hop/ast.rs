use std::collections::BTreeMap;

use crate::document::DocumentPosition;
use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::expr::{TypedExpr, UntypedExpr};
use crate::dop::{Argument, Parameter, VarName};
use crate::hop::module_name::ModuleName;

/// A StaticAttribute is an attribute that must
/// be known at compile time.
#[derive(Debug, Clone)]
pub struct StaticAttribute {
    pub value: DocumentRange,
}

#[derive(Debug, Clone)]
pub enum AttributeValue<T = UntypedExpr> {
    Expression(T),
    String(DocumentRange),
}

pub type TypedAttribute = Attribute<TypedExpr>;

/// An Attribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Attribute<T = UntypedExpr> {
    pub name: DocumentRange,
    pub value: Option<AttributeValue<T>>,
    pub range: DocumentRange,
}

pub type UntypedAst = Ast<UntypedExpr>;
pub type TypedAst = Ast<TypedExpr>;

#[derive(Debug, Clone)]
pub struct Ast<T> {
    pub name: ModuleName,
    imports: Vec<Import>,
    component_definitions: Vec<ComponentDefinition<T, DocumentRange>>,
}

impl<T> Ast<T> {
    pub fn new(
        name: ModuleName,
        component_definitions: Vec<ComponentDefinition<T, DocumentRange>>,
        imports: Vec<Import>,
    ) -> Self {
        Self {
            name,
            component_definitions,
            imports,
        }
    }

    pub fn get_component_definition(
        &self,
        name: &str,
    ) -> Option<&ComponentDefinition<T, DocumentRange>> {
        self.component_definitions
            .iter()
            .find(|&n| n.tag_name.as_str() == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definitions(&self) -> &[ComponentDefinition<T, DocumentRange>] {
        &self.component_definitions
    }

    /// Returns a mutable reference to all component definition nodes in the AST.
    pub fn get_component_definitions_mut(
        &mut self,
    ) -> &mut [ComponentDefinition<T, DocumentRange>] {
        &mut self.component_definitions
    }

    /// Returns a reference to all import nodes in the AST.
    pub fn get_imports(&self) -> &[Import] {
        &self.imports
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &Node<T, DocumentRange>> {
        self.component_definitions
            .iter()
            .flat_map(|n| &n.children)
            .flat_map(|n| n.iter_depth_first())
    }

    /// Finds the deepest AST node that contains the given position.
    ///
    /// Upper bound on time complexity is max(depth of tree, number of top level nodes).
    ///
    /// # Example
    ///
    /// <div>
    ///     <span>text</span>
    ///                  ^
    /// </div>
    ///
    /// returns
    ///
    /// <div>
    ///     <span>text</span>
    ///     ^^^^^^^^^^^^^^^^^
    /// </div>
    ///
    pub fn find_node_at_position(
        &self,
        position: DocumentPosition,
    ) -> Option<&Node<T, DocumentRange>> {
        for n in &self.component_definitions {
            if n.range.contains_position(position) {
                for child in &n.children {
                    if let Some(node) = child.find_node_at_position(position) {
                        return Some(node);
                    }
                }
                return None;
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub component_attr: StaticAttribute,
    pub from_attr: StaticAttribute,
    pub module_name: ModuleName,
}

impl Import {
    pub fn imported_module(&self) -> &ModuleName {
        &self.module_name
    }
    pub fn imported_component(&self) -> &DocumentRange {
        &self.component_attr.value
    }
    pub fn imports_component(&self, component_name: &str) -> bool {
        self.component_attr.value.as_str() == component_name
    }
    pub fn imports_from(&self, module_name: &ModuleName) -> bool {
        &self.module_name == module_name
    }
}

pub type TypedComponentDefinition = ComponentDefinition<TypedExpr>;

#[derive(Debug, Clone)]
pub struct ComponentDefinition<T = UntypedExpr, A = DocumentRange> {
    pub tag_name: DocumentRange,
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<Parameter>, DocumentRange)>,
    pub as_attr: Option<StaticAttribute>,
    pub attributes: BTreeMap<StringSpan, Attribute<T>>,
    pub children: Vec<Node<T, A>>,
    pub is_entrypoint: bool,
    pub has_slot: bool,
    pub range: DocumentRange,
}

impl<T> Ranged for ComponentDefinition<T> {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl<T> ComponentDefinition<T> {
    pub fn tag_name_ranges(&self) -> impl Iterator<Item = &DocumentRange> {
        self.closing_tag_name.iter().chain(Some(&self.tag_name))
    }
}

pub type UntypedNode = Node<UntypedExpr, DocumentRange>;
pub type TypedNode = Node<TypedExpr, DocumentRange>;

#[derive(Debug, Clone)]
pub enum Node<E, A> {
    /// A Text node represents text in the document.
    /// E.g. <div>hello world</div>
    ///           ^^^^^^^^^^^
    Text { value: StringSpan, range: A },

    /// A TextExpression represents an expression that occurs in a text position.
    /// E.g. <div>hello {world}</div>
    ///                 ^^^^^^^
    TextExpression { expression: E, range: A },

    /// A ComponentReference represents a reference to a component.
    /// E.g.
    /// <my-component>
    ///   <other-component/>
    ///   ^^^^^^^^^^^^^^^^^^
    /// </my-component>
    ComponentReference {
        tag_name: DocumentRange,
        definition_module: Option<ModuleName>,
        closing_tag_name: Option<DocumentRange>,
        args: Option<(Vec<Argument<E>>, DocumentRange)>,
        attributes: BTreeMap<StringSpan, Attribute<E>>,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A SlotDefinition node represents the definition of a slot, e.g.
    /// the <slot-default/> in
    ///
    /// <my-component>
    ///   <slot-default/>
    /// </my-component>
    SlotDefinition { range: DocumentRange },

    /// An If node contains content that is only evaluated when its condition
    /// expression evaluates to true.
    If {
        condition: E,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A For node contains content that is evaluated once for each item of
    /// an array.
    For {
        var_name: VarName,
        array_expr: E,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A Doctype node represents a doctype, e.g. a <!DOCTYPE html>
    Doctype {
        value: StringSpan,
        range: DocumentRange,
    },

    /// An HTML node represents a plain HTML node.
    /// E.g. <div>...</div>
    ///      ^^^^^^^^^^^^^^
    Html {
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: BTreeMap<StringSpan, Attribute<E>>,
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A Placeholder node represents a node that could not be constructed (because
    /// it is missing a required attribute or similar).
    ///
    /// We use Placeholder nodes to be able to construct the child nodes of the node that could not be
    /// constructed. This is useful for e.g. go-to-definition in the language server.
    Placeholder {
        children: Vec<Self>,
        range: DocumentRange,
    },

    /// A Let node introduces a variable binding in its children's scope.
    /// This is used during component inlining to bind component parameters.
    Let {
        var: VarName,
        value: E,
        children: Vec<Self>,
        range: DocumentRange,
    },
}

impl<T, A> Node<T, A> {
    /// Get the direct children of a node.
    pub fn children(&self) -> &[Self] {
        match self {
            Node::ComponentReference { children, .. } => children,
            Node::If { children, .. } => children,
            Node::For { children, .. } => children,
            Node::Html { children, .. } => children,
            Node::Placeholder { children, .. } => children,
            Node::Let { children, .. } => children,
            Node::SlotDefinition { .. } => &[],
            Node::Doctype { .. } => &[],
            Node::Text { .. } => &[],
            Node::TextExpression { .. } => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator<T, A> {
        DepthFirstIterator::new(self)
    }

    /// Get the range for the opening tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            Node::ComponentReference { tag_name, .. } => Some(tag_name),
            Node::Html { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    /// Get the range for the closing tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///                   ^^^
    pub fn closing_tag_name(&self) -> Option<&DocumentRange> {
        match self {
            Node::ComponentReference {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            Node::Html {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            _ => None,
        }
    }

    /// Get the name ranges for the tags of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^              ^^^
    pub fn tag_names(&self) -> impl Iterator<Item = &DocumentRange> {
        self.tag_name().into_iter().chain(self.closing_tag_name())
    }
}

impl<T> Node<T, DocumentRange> {
    pub fn find_node_at_position(&self, position: DocumentPosition) -> Option<&Self> {
        if !self.range().contains_position(position) {
            return None;
        }
        for child in self.children() {
            if let Some(node) = child.find_node_at_position(position) {
                return Some(node);
            }
        }
        Some(self)
    }
}

impl<T> Ranged for Node<T, DocumentRange> {
    fn range(&self) -> &DocumentRange {
        match self {
            Node::Text { range, .. }
            | Node::TextExpression { range, .. }
            | Node::ComponentReference { range, .. }
            | Node::SlotDefinition { range, .. }
            | Node::If { range, .. }
            | Node::For { range, .. }
            | Node::Html { range, .. }
            | Node::Placeholder { range, .. }
            | Node::Let { range, .. }
            | Node::Doctype { range, .. } => range,
        }
    }
}

pub struct DepthFirstIterator<'a, T, A> {
    stack: Vec<&'a Node<T, A>>,
}

impl<'a, T, A> DepthFirstIterator<'a, T, A> {
    fn new(root: &'a Node<T, A>) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a, T, A> Iterator for DepthFirstIterator<'a, T, A> {
    type Item = &'a Node<T, A>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;

        // Add children in reverse order so they're visited in correct order
        for child in current.children().iter().rev() {
            self.stack.push(child);
        }

        Some(current)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::document::SimpleAnnotation;
    use crate::document::extract_position::extract_position;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_find_node_at_position(input: &str, expected: Expect) {
        use crate::error_collector::ErrorCollector;

        let (source, position) = extract_position(input).expect("Position marker not found");
        let mut errors = ErrorCollector::new();
        let ast = parse(
            ModuleName::new("test".to_string()).unwrap(),
            Tokenizer::new(source),
            &mut errors,
        );

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        let found_node = ast.find_node_at_position(position);

        let output = if let Some(node) = found_node {
            let annotator = DocumentAnnotator::new().without_location();
            annotator.annotate(
                None,
                [SimpleAnnotation {
                    range: node.range().clone(),
                    message: "range".to_string(),
                }],
            )
        } else {
            "No node found at position".to_string()
        };

        expected.assert_eq(&output);
    }

    #[test]
    fn test_find_node_at_position_text_content() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>Hello World</div>
                             ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div>Hello World</div>
                  |          ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_tag_name() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>Content</div>
                     ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div>Content</div>
                  |     ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_component_reference() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <foo-bar>Content</foo-bar>
                        ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <foo-bar>Content</foo-bar>
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_if_tag() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <if {true}>
                        ^
                        <div/>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <if {true}>
                  |     ^^^^^^^^^^^
                3 |         <div/>
                  | ^^^^^^^^^^^^^^
                4 |     </if>
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_nested_content() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>
                        <span>Nested text</span>
                                    ^
                    </div>
                </main-comp>
            "},
            expect![[r#"
                range
                3 |         <span>Nested text</span>
                  |               ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_slot_definition() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <slot-default/>
                          ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <slot-default/>
                  |     ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_outside_content() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>Content</div>
                </main-comp>
                ^
            "},
            expect!["No node found at position"],
        );
    }

    #[test]
    fn test_find_node_at_position_doctype() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <!DOCTYPE html>
                     ^
                    <div>Content</div>
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <!DOCTYPE html>
                  |     ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_deeply_nested() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>
                        <if {condition}>
                            <for {item in items}>
                                <span>{item}</span>
                                        ^
                            </for>
                        </if>
                    </div>
                </main-comp>
            "},
            expect![[r#"
                range
                5 |                 <span>{item}</span>
                  |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_void_tag() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <p>Some text <br> more text</p>
                                  ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <p>Some text <br> more text</p>
                  |                  ^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_multiple_nodes_same_line() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div><span>Hello</span> <strong>World</strong></div>
                           ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |          ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_multiple_nodes_same_line_second_element() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div><span>Hello</span> <strong>World</strong></div>
                                               ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |                             ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_multiple_nodes_same_line_text_between() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div><span>Hello</span> and <strong>World</strong></div>
                                            ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> and <strong>World</strong></div>
                  |                            ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_very_deep_nesting() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>
                        <section>
                            <article>
                                <if {condition}>
                                    <for {item in items}>
                                        <header>
                                            <h1>
                                                <span>
                                                    <em>Deep {item.name} text</em>
                                                             ^
                                                </span>
                                            </h1>
                                        </header>
                                    </for>
                                </if>
                            </article>
                        </section>
                    </div>
                </main-comp>
            "},
            expect![[r#"
                range
                10 |                                     <em>Deep {item.name} text</em>
                   |                                              ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_deep_nesting_parent_element() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>
                        <section>
                            <h1>
                                <em>Deep text</em>
                                <span>
                                     ^
                                    <div></div>
                                    <em>Deep text</em>
                                </span>
                            </h1>
                        </section>
                    </div>
                </main-comp>
            "},
            expect![[r#"
                range
                 6 |                 <span>
                   |                 ^^^^^^
                 7 |                     <div></div>
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 8 |                     <em>Deep text</em>
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 9 |                 </span>
                   | ^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_inline_elements_with_expressions() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                                                                  ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                  |                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_component_with_inline_content() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div><user-card {data: user}><span>Content</span></user-card> more text</div>
                                                        ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div><user-card {data: user}><span>Content</span></user-card> more text</div>
                  |                                        ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_nested_control_structures() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <if {users}>
                        <for {user in users}>
                            <if {user.active}>
                                <for {role in user.roles}>
                                    <span>{role}</span>
                                       ^
                                </for>
                            </if>
                        </for>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                range
                 6 |                     <span>{role}</span>
                   |                     ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_self_closing_with_attributes() {
        check_find_node_at_position(
            indoc! {r#"
                <main-comp>
                    <div>
                        <input type="text" placeholder="Enter name" required />
                               ^
                        <br/>
                    </div>
                </main-comp>
            "#},
            expect![[r#"
                range
                3 |         <input type="text" placeholder="Enter name" required />
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_find_node_at_position_between_closing_and_opening_tags() {
        check_find_node_at_position(
            indoc! {"
                <main-comp>
                    <div>First</div> <div>Second</div>
                                     ^
                </main-comp>
            "},
            expect![[r#"
                range
                2 |     <div>First</div> <div>Second</div>
                  |                      ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
