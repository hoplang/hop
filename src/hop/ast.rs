use std::collections::BTreeMap;

use crate::document::DocumentPosition;
use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::{DopArgument, DopExpr, DopParameter, parser::DopVarName};
use crate::hop::module_name::ModuleName;

/// A StaticAttribute is an attribute that must
/// be known at compile time.
#[derive(Debug, Clone)]
pub struct StaticAttribute {
    pub value: DocumentRange,
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    Expression(DopExpr),
    String(DocumentRange),
}

/// An Attribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: DocumentRange,
    pub value: Option<AttributeValue>,
    pub range: DocumentRange,
}

#[derive(Debug)]
pub struct HopAst {
    pub name: ModuleName,
    imports: Vec<Import>,
    component_definitions: Vec<ComponentDefinition>,
    renders: Vec<Render>,
}

impl HopAst {
    pub fn new(
        name: ModuleName,
        component_definitions: Vec<ComponentDefinition>,
        imports: Vec<Import>,
        renders: Vec<Render>,
    ) -> Self {
        Self {
            name,
            component_definitions,
            imports,
            renders,
        }
    }

    pub fn get_component_definition(&self, name: &str) -> Option<&ComponentDefinition> {
        self.component_definitions
            .iter()
            .find(|&n| n.tag_name.as_str() == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definitions(&self) -> &[ComponentDefinition] {
        &self.component_definitions
    }

    /// Returns a reference to all import nodes in the AST.
    pub fn get_imports(&self) -> &[Import] {
        &self.imports
    }

    /// Returns a reference to all render nodes in the AST.
    pub fn get_renders(&self) -> &[Render] {
        &self.renders
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    /// This includes all child nodes from both render nodes and component definitions.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &HopNode> {
        self.renders
            .iter()
            .flat_map(|n| &n.children)
            .chain(self.component_definitions.iter().flat_map(|n| &n.children))
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
    pub fn find_node_at_position(&self, position: DocumentPosition) -> Option<&HopNode> {
        for n in &self.renders {
            if n.range.contains_position(position) {
                for child in &n.children {
                    if let Some(node) = child.find_node_at_position(position) {
                        return Some(node);
                    }
                }
                return None;
            }
        }
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Render {
    pub file_attr: StaticAttribute,
    pub range: DocumentRange,
    pub children: Vec<HopNode>,
}

#[derive(Debug)]
pub struct ComponentDefinition {
    pub tag_name: DocumentRange,
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<DopParameter>, DocumentRange)>,
    pub as_attr: Option<StaticAttribute>,
    pub attributes: BTreeMap<StringSpan, Attribute>,
    pub range: DocumentRange,
    pub children: Vec<HopNode>,
    pub is_entrypoint: bool,
    pub has_slot: bool,
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

#[derive(Debug)]
pub enum HopNode {
    /// A Text node represents text in the document.
    /// E.g. <div>hello world</div>
    ///           ^^^^^^^^^^^
    Text { range: DocumentRange },

    /// A TextExpression represents an expression that occurs in a text position.
    /// E.g. <div>hello {world}</div>
    ///                 ^^^^^^^
    TextExpression {
        expression: DopExpr,
        range: DocumentRange,
    },

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
        args: Option<(Vec<DopArgument>, DocumentRange)>,
        attributes: BTreeMap<StringSpan, Attribute>,
        range: DocumentRange,
        children: Vec<HopNode>,
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
        condition: DopExpr,
        range: DocumentRange,
        children: Vec<HopNode>,
    },

    /// A For node contains content that is evaluated once for each item of
    /// an array.
    For {
        var_name: DopVarName,
        array_expr: DopExpr,
        range: DocumentRange,
        children: Vec<HopNode>,
    },

    /// A Doctype node represents a doctype, e.g. a <!DOCTYPE html>
    Doctype { range: DocumentRange },

    /// An HTML node represents a plain HTML node.
    /// E.g. <div>...</div>
    ///      ^^^^^^^^^^^^^^
    Html {
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: BTreeMap<StringSpan, Attribute>,
        range: DocumentRange,
        children: Vec<HopNode>,
    },
    XExec {
        cmd_attr: StaticAttribute,
        range: DocumentRange,
        children: Vec<HopNode>,
    },

    /// An XRaw node contains content that should be treated as a string and the contents
    /// are not parsed, typechecked nor evaluated.
    ///
    /// The children vec should always contain a single Text node.
    XRaw {
        trim: bool,
        range: DocumentRange,
        children: Vec<HopNode>,
    },

    /// An Error node represents a node that could not be constructed (because
    /// it is missing a required attribute or similar).
    ///
    /// We use Error nodes to be able to construct the child nodes of the node that could not be
    /// constructed. This is useful for e.g. go-to-definition in the language server.
    Error {
        range: DocumentRange,
        children: Vec<HopNode>,
    },
}

impl HopNode {
    /// Get the direct children of a node.
    pub fn children(&self) -> &[HopNode] {
        match self {
            HopNode::ComponentReference { children, .. } => children,
            HopNode::If { children, .. } => children,
            HopNode::For { children, .. } => children,
            HopNode::Html { children, .. } => children,
            HopNode::Error { children, .. } => children,
            HopNode::XExec { children, .. } => children,
            HopNode::XRaw { children, .. } => children,
            HopNode::SlotDefinition { .. } => &[],
            HopNode::Doctype { .. } => &[],
            HopNode::Text { .. } => &[],
            HopNode::TextExpression { .. } => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator {
        DepthFirstIterator::new(self)
    }

    pub fn find_node_at_position(&self, position: DocumentPosition) -> Option<&HopNode> {
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

    /// Get the range for the opening tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            HopNode::ComponentReference { tag_name, .. } => Some(tag_name),
            HopNode::Html { tag_name, .. } => Some(tag_name),
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
            HopNode::ComponentReference {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            HopNode::Html {
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

impl Ranged for HopNode {
    fn range(&self) -> &DocumentRange {
        match self {
            HopNode::Text { range, .. }
            | HopNode::TextExpression { range, .. }
            | HopNode::ComponentReference { range, .. }
            | HopNode::SlotDefinition { range, .. }
            | HopNode::If { range, .. }
            | HopNode::For { range, .. }
            | HopNode::Html { range, .. }
            | HopNode::XExec { range, .. }
            | HopNode::XRaw { range, .. }
            | HopNode::Error { range, .. }
            | HopNode::Doctype { range, .. } => range,
        }
    }
}

pub struct DepthFirstIterator<'a> {
    stack: Vec<&'a HopNode>,
}

impl<'a> DepthFirstIterator<'a> {
    fn new(root: &'a HopNode) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a> Iterator for DepthFirstIterator<'a> {
    type Item = &'a HopNode;

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
        let (source, position) = extract_position(input).expect("Position marker not found");
        let mut errors = Vec::new();
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
