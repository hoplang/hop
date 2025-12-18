use std::fmt::{self, Display};

use crate::document::DocumentPosition;
use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::ParseTree;
use crate::dop::ParsedType;
use crate::dop::VarName;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::node::Node;

/// A Parameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct Parameter<T = ParsedType> {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: T,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var_name, self.var_type)
    }
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    Expressions(Vec<ParseTree>),
    String(DocumentRange),
}

/// An Attribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: DocumentRange,
    pub value: Option<AttributeValue>,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub name: ModuleName,
    imports: Vec<Import>,
    records: Vec<Record>,
    enums: Vec<Enum>,
    component_definitions: Vec<ComponentDefinition>,
}

impl Ast {
    pub fn new(
        name: ModuleName,
        component_definitions: Vec<ComponentDefinition>,
        imports: Vec<Import>,
        records: Vec<Record>,
        enums: Vec<Enum>,
    ) -> Self {
        Self {
            name,
            component_definitions,
            imports,
            records,
            enums,
        }
    }

    pub fn get_component_definition(&self, name: &str) -> Option<&ComponentDefinition> {
        self.component_definitions
            .iter()
            .find(|&n| n.tag_name.as_str() == name)
    }

    /// Finds a record declaration by name.
    pub fn get_record(&self, name: &str) -> Option<&Record> {
        self.records.iter().find(|&r| r.name() == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definitions(&self) -> &[ComponentDefinition] {
        &self.component_definitions
    }

    /// Returns a reference to all import nodes in the AST.
    pub fn get_imports(&self) -> &[Import] {
        &self.imports
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[Record] {
        &self.records
    }

    /// Returns a reference to all enum declarations in the AST.
    pub fn get_enums(&self) -> &[Enum] {
        &self.enums
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &Node> {
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
    pub fn find_node_at_position(&self, position: DocumentPosition) -> Option<&Node> {
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
    pub type_name: TypeName,
    /// The range of the type name in the source (for error reporting)
    pub type_name_range: DocumentRange,
    /// The full path range for error reporting (covers module::TypeName)
    pub path: DocumentRange,
    pub module_name: ModuleName,
}

impl Import {
    pub fn imported_module(&self) -> &ModuleName {
        &self.module_name
    }
    pub fn imported_type_name(&self) -> &TypeName {
        &self.type_name
    }
    pub fn type_name_range(&self) -> &DocumentRange {
        &self.type_name_range
    }
    pub fn imports_type(&self, type_name: &str) -> bool {
        self.type_name.as_str() == type_name
    }
    pub fn imports_from(&self, module_name: &ModuleName) -> bool {
        &self.module_name == module_name
    }
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: FieldName,
    pub field_type: ParsedType,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub fields: Vec<RecordField>,
}

impl Record {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: TypeName,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub variants: Vec<EnumVariant>,
}

impl Enum {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Debug, Clone)]
pub struct ComponentDefinition {
    pub component_name: ComponentName,
    pub tag_name: DocumentRange,
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<Parameter>, DocumentRange)>,
    pub children: Vec<Node>,
    pub range: DocumentRange,
}

impl Ranged for ComponentDefinition {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl ComponentDefinition {
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
    use crate::hop::syntax::parser::parse;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_find_node_at_position(input: &str, expected: Expect) {
        use crate::error_collector::ErrorCollector;

        let (source, position) = extract_position(input).expect("Position marker not found");
        let mut errors = ErrorCollector::new();
        let ast = parse(ModuleName::new("test").unwrap(), source, &mut errors);

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
