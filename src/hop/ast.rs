use crate::document::DocumentPosition;
use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::Parameter;
use crate::dop::RecordDeclaration;
use crate::dop::SimpleExpr;
use crate::dop::SyntacticExpr;
use crate::dop::SyntacticType;
use crate::dop::Type;
use crate::hop::component_name::ComponentName;
use crate::hop::module_name::ModuleName;

use super::node::Node;

#[derive(Debug, Clone)]
pub enum AttributeValue<T = SyntacticExpr> {
    Expressions(Vec<T>),
    String(DocumentRange),
}

pub type TypedAttribute = Attribute<SimpleExpr>;

/// An Attribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[derive(Debug, Clone)]
pub struct Attribute<T = SyntacticExpr> {
    pub name: DocumentRange,
    pub value: Option<AttributeValue<T>>,
    pub range: DocumentRange,
}

pub type UntypedAst = Ast<SyntacticExpr, SyntacticType>;
pub type TypedAst = Ast<SimpleExpr, Type>;

#[derive(Debug, Clone)]
pub struct Ast<T, A = ()> {
    pub name: ModuleName,
    imports: Vec<Import>,
    records: Vec<Record<A>>,
    component_definitions: Vec<ComponentDefinition<T, A>>,
}

impl<T, A> Ast<T, A> {
    pub fn new(
        name: ModuleName,
        component_definitions: Vec<ComponentDefinition<T, A>>,
        imports: Vec<Import>,
        records: Vec<Record<A>>,
    ) -> Self {
        Self {
            name,
            component_definitions,
            imports,
            records,
        }
    }

    pub fn get_component_definition(&self, name: &str) -> Option<&ComponentDefinition<T, A>> {
        self.component_definitions
            .iter()
            .find(|&n| n.tag_name.as_str() == name)
    }

    /// Finds a record declaration by name.
    pub fn get_record(&self, name: &str) -> Option<&Record<A>> {
        self.records.iter().find(|&r| r.name() == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definitions(&self) -> &[ComponentDefinition<T, A>] {
        &self.component_definitions
    }

    /// Returns a reference to all import nodes in the AST.
    pub fn get_imports(&self) -> &[Import] {
        &self.imports
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[Record<A>] {
        &self.records
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &Node<T>> {
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
    pub fn find_node_at_position(&self, position: DocumentPosition) -> Option<&Node<T>> {
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
    pub component: ComponentName,
    /// The range of the component name in the source (for error reporting)
    pub component_range: DocumentRange,
    /// The full path range for error reporting (covers module::Component)
    pub path: DocumentRange,
    pub module_name: ModuleName,
}

impl Import {
    pub fn imported_module(&self) -> &ModuleName {
        &self.module_name
    }
    pub fn imported_component(&self) -> &ComponentName {
        &self.component
    }
    pub fn component_range(&self) -> &DocumentRange {
        &self.component_range
    }
    pub fn imports_component(&self, component_name: &str) -> bool {
        self.component.as_str() == component_name
    }
    pub fn imports_from(&self, module_name: &ModuleName) -> bool {
        &self.module_name == module_name
    }
}

#[derive(Debug, Clone)]
pub struct Record<A = SyntacticType> {
    pub declaration: RecordDeclaration<A>,
    pub range: DocumentRange,
}

impl<A> Record<A> {
    pub fn name(&self) -> &str {
        self.declaration.name.as_str()
    }
}

pub type TypedRecord = Record<Type>;

pub type UntypedComponentDefinition = ComponentDefinition<SyntacticExpr, SyntacticType>;
pub type TypedComponentDefinition = ComponentDefinition<SimpleExpr, Type>;

#[derive(Debug, Clone)]
pub struct ComponentDefinition<E, P = SyntacticType> {
    pub component_name: ComponentName,
    pub tag_name: DocumentRange, // Keep for source location/error reporting
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<Parameter<P>>, DocumentRange)>,
    pub children: Vec<Node<E>>,
    pub range: DocumentRange,
}

impl<E, P> Ranged for ComponentDefinition<E, P> {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl<E, P> ComponentDefinition<E, P> {
    pub fn tag_name_ranges(&self) -> impl Iterator<Item = &DocumentRange> {
        self.closing_tag_name.iter().chain(Some(&self.tag_name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::document::SimpleAnnotation;
    use crate::document::extract_position::extract_position;
    use crate::hop::parser::parse;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_find_node_at_position(input: &str, expected: Expect) {
        use crate::error_collector::ErrorCollector;

        let (source, position) = extract_position(input).expect("Position marker not found");
        let mut errors = ErrorCollector::new();
        let ast = parse(
            ModuleName::new("test").unwrap(),
            source,
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
    fn find_node_at_position_text_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>Hello World</div>
                             ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div>Hello World</div>
                  |          ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_tag_name() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>Content</div>
                     ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div>Content</div>
                  |     ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_component_reference() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <FooBar>Content</FooBar>
                        ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <FooBar>Content</FooBar>
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_if_tag() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <if {true}>
                        ^
                        <div/>
                    </if>
                </Main>
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
    fn find_node_at_position_nested_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>
                        <span>Nested text</span>
                                    ^
                    </div>
                </Main>
            "},
            expect![[r#"
                range
                3 |         <span>Nested text</span>
                  |               ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_outside_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>Content</div>
                </Main>
                ^
            "},
            expect!["No node found at position"],
        );
    }

    #[test]
    fn find_node_at_position_doctype() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <!DOCTYPE html>
                     ^
                    <div>Content</div>
                </Main>
            "},
            expect![[r#"
                range
                2 |     <!DOCTYPE html>
                  |     ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_deeply_nested() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>
                        <if {condition}>
                            <for {item in items}>
                                <span>{item}</span>
                                        ^
                            </for>
                        </if>
                    </div>
                </Main>
            "},
            expect![[r#"
                range
                5 |                 <span>{item}</span>
                  |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_void_tag() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <p>Some text <br> more text</p>
                                  ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <p>Some text <br> more text</p>
                  |                  ^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_multiple_nodes_same_line() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><span>Hello</span> <strong>World</strong></div>
                           ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |          ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_multiple_nodes_same_line_second_element() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><span>Hello</span> <strong>World</strong></div>
                                               ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |                             ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_multiple_nodes_same_line_text_between() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><span>Hello</span> and <strong>World</strong></div>
                                            ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> and <strong>World</strong></div>
                  |                            ^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_very_deep_nesting() {
        check_find_node_at_position(
            indoc! {"
                <Main>
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
                </Main>
            "},
            expect![[r#"
                range
                10 |                                     <em>Deep {item.name} text</em>
                   |                                              ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_deep_nesting_parent_element() {
        check_find_node_at_position(
            indoc! {"
                <Main>
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
                </Main>
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
    fn find_node_at_position_inline_elements_with_expressions() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                                                                  ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                  |                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_component_with_inline_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><UserCard {data: user}><span>Content</span></UserCard> more text</div>
                                                        ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><UserCard {data: user}><span>Content</span></UserCard> more text</div>
                  |                                       ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_nested_control_structures() {
        check_find_node_at_position(
            indoc! {"
                <Main>
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
                </Main>
            "},
            expect![[r#"
                range
                 6 |                     <span>{role}</span>
                   |                     ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_self_closing_with_attributes() {
        check_find_node_at_position(
            indoc! {r#"
                <Main>
                    <div>
                        <input type="text" placeholder="Enter name" required />
                               ^
                        <br/>
                    </div>
                </Main>
            "#},
            expect![[r#"
                range
                3 |         <input type="text" placeholder="Enter name" required />
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn find_node_at_position_between_closing_and_opening_tags() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>First</div> <div>Second</div>
                                     ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div>First</div> <div>Second</div>
                  |                      ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
