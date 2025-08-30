use crate::common::{Position, Range, Ranged};
use crate::dop::{DopArgument, DopExpr, DopParameter, parser::DopVarName};

#[derive(Debug, Clone, PartialEq)]
pub struct HopAST {
    pub name: String,
    component_nodes: Vec<ComponentDefinitionNode>,
    import_nodes: Vec<ImportNode>,
    render_nodes: Vec<RenderNode>,
}

impl HopAST {
    /// Creates a new HopAST with the given components.
    pub fn new(
        name: String,
        component_nodes: Vec<ComponentDefinitionNode>,
        import_nodes: Vec<ImportNode>,
        render_nodes: Vec<RenderNode>,
    ) -> Self {
        Self {
            name,
            component_nodes,
            import_nodes,
            render_nodes,
        }
    }

    pub fn get_component_definition(&self, name: &str) -> Option<&ComponentDefinitionNode> {
        self.component_nodes.iter().find(|&n| n.name == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definition_nodes(&self) -> &[ComponentDefinitionNode] {
        &self.component_nodes
    }

    /// Returns a reference to all import nodes in the AST.
    pub fn get_import_nodes(&self) -> &[ImportNode] {
        &self.import_nodes
    }

    /// Returns a reference to all render nodes in the AST.
    pub fn get_render_nodes(&self) -> &[RenderNode] {
        &self.render_nodes
    }

    /// Returns an iterator over all nodes in the AST, depth-first.
    /// This includes all nodes from both render nodes and component definitions.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &HopNode> {
        self.render_nodes
            .iter()
            .flat_map(|def| &def.children)
            .chain(
                self.component_nodes
                    .iter()
                    .flat_map(|render| &render.children),
            )
            .flat_map(|child| child.iter_depth_first())
    }

    /// Finds the deepest AST node that contains the given position.
    ///
    /// # Example
    ///
    /// <div><span>text</span></div>
    ///        ^
    /// returns Some(&NativeHTML { tag_name: "span", .. })
    ///
    pub fn find_node_at_position(&self, position: Position) -> Option<&HopNode> {
        // Search through all render nodes first
        for render_node in &self.render_nodes {
            for child in &render_node.children {
                if let Some(node) = child.find_node_at_position(position) {
                    return Some(node);
                }
            }
        }

        // Then search through component definitions
        for component in &self.component_nodes {
            for child in &component.children {
                if let Some(node) = child.find_node_at_position(position) {
                    return Some(node);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
    pub value_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopExprAttribute {
    pub name: String,
    pub expression: DopExpr,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportNode {
    pub component_attr: Attribute,
    pub from_attr: Attribute,
    pub range: Range,
}

impl ImportNode {
    pub fn imports_component(&self, component_name: &str) -> bool {
        self.component_attr.value == component_name
    }

    pub fn imports_from(&self, from_path: &str) -> bool {
        self.from_attr.value == from_path
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderNode {
    pub file_attr: Attribute,
    pub range: Range,
    pub children: Vec<HopNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentDefinitionNode {
    pub name: String,
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub params: Vec<DopParameter>,
    pub as_attr: Option<Attribute>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<HopNode>,
    pub entrypoint: bool,
    pub has_slot: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HopNode {
    Doctype {
        value: String,
        range: Range,
    },
    Text {
        value: String,
        range: Range,
    },
    TextExpression {
        expression: DopExpr,
        range: Range,
    },
    ComponentReference {
        component: String,
        definition_module: Option<String>,
        opening_name_range: Range,
        closing_name_range: Option<Range>,
        args: Vec<DopArgument>,
        attributes: Vec<Attribute>,
        range: Range,
        children: Vec<HopNode>,
    },
    SlotDefinition {
        range: Range,
    },
    If {
        condition: DopExpr,
        range: Range,
        children: Vec<HopNode>,
    },
    For {
        var_name: DopVarName,
        array_expr: DopExpr,
        range: Range,
        children: Vec<HopNode>,
    },
    NativeHTML {
        tag_name: String,
        opening_name_range: Range,
        closing_name_range: Option<Range>,
        attributes: Vec<Attribute>,
        range: Range,
        children: Vec<HopNode>,
        set_attributes: Vec<DopExprAttribute>,
    },
    Error {
        range: Range,
        children: Vec<HopNode>,
    },
    XExec {
        cmd_attr: Attribute,
        range: Range,
        children: Vec<HopNode>,
    },
    XRaw {
        trim: bool,
        range: Range,
        children: Vec<HopNode>,
    },
}

impl HopNode {
    pub fn children(&self) -> &[HopNode] {
        match self {
            HopNode::ComponentReference { children, .. } => children,
            HopNode::SlotDefinition { .. } => &[],
            HopNode::If { children, .. } => children,
            HopNode::For { children, .. } => children,
            HopNode::NativeHTML { children, .. } => children,
            HopNode::Error { children, .. } => children,
            HopNode::XExec { children, .. } => children,
            HopNode::XRaw { children, .. } => children,
            // Leaf nodes with no children
            HopNode::Doctype { .. } => &[],
            HopNode::Text { .. } => &[],
            HopNode::TextExpression { .. } => &[],
        }
    }

    pub fn iter_depth_first(&self) -> DepthFirstIterator {
        DepthFirstIterator::new(self)
    }

    pub fn find_node_at_position(&self, position: Position) -> Option<&HopNode> {
        if !self.contains_position(position) {
            return None;
        }
        for child in self.children() {
            if let Some(node) = child.find_node_at_position(position) {
                return Some(node);
            }
        }
        Some(self)
    }

    pub fn opening_name_range(&self) -> Option<Range> {
        match self {
            HopNode::ComponentReference {
                opening_name_range, ..
            } => Some(*opening_name_range),
            HopNode::NativeHTML {
                opening_name_range, ..
            } => Some(*opening_name_range),
            _ => None,
        }
    }

    pub fn closing_name_range(&self) -> Option<Range> {
        match self {
            HopNode::ComponentReference {
                closing_name_range, ..
            } => *closing_name_range,
            HopNode::NativeHTML {
                closing_name_range, ..
            } => *closing_name_range,
            _ => None,
        }
    }

    pub fn tag_name(&self) -> Option<&str> {
        match self {
            HopNode::ComponentReference { component, .. } => Some(component),
            HopNode::NativeHTML { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    pub fn name_ranges(&self) -> impl Iterator<Item = Range> {
        self.opening_name_range()
            .into_iter()
            .chain(self.closing_name_range())
    }
}

impl Ranged for HopNode {
    fn range(&self) -> Range {
        match self {
            HopNode::Doctype { range, .. } => *range,
            HopNode::Text { range, .. } => *range,
            HopNode::TextExpression { range, .. } => *range,
            HopNode::ComponentReference { range, .. } => *range,
            HopNode::SlotDefinition { range, .. } => *range,
            HopNode::If { range, .. } => *range,
            HopNode::For { range, .. } => *range,
            HopNode::NativeHTML { range, .. } => *range,
            HopNode::Error { range, .. } => *range,
            HopNode::XExec { range, .. } => *range,
            HopNode::XRaw { range, .. } => *range,
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
    use crate::common::Range;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::test_utils::position_marker::extract_position;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_find_node_at_position(input: &str, expected: Expect) {
        let (source, position) = extract_position(input).expect("Position marker not found");
        let mut errors = Vec::new();
        let ast = parse("test".to_string(), Tokenizer::new(&source), &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        let found_node = ast.find_node_at_position(position);

        expected.assert_debug_eq(&found_node);
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
                Some(
                    Text {
                        value: "Hello World",
                        range: 2:10-2:21,
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "div",
                        opening_name_range: 2:6-2:9,
                        closing_name_range: Some(
                            2:19-2:22,
                        ),
                        attributes: [],
                        range: 2:5-2:23,
                        children: [
                            Text {
                                value: "Content",
                                range: 2:10-2:17,
                            },
                        ],
                        set_attributes: [],
                    },
                )
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
                Some(
                    ComponentReference {
                        component: "foo-bar",
                        definition_module: None,
                        opening_name_range: 2:6-2:13,
                        closing_name_range: Some(
                            2:23-2:30,
                        ),
                        args: [],
                        attributes: [],
                        range: 2:5-2:31,
                        children: [
                            Text {
                                value: "Content",
                                range: 2:14-2:21,
                            },
                        ],
                    },
                )
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
                Some(
                    If {
                        condition: BooleanLiteral {
                            value: true,
                            range: 2:10-2:14,
                        },
                        range: 2:5-4:10,
                        children: [
                            Text {
                                value: "\n        ",
                                range: 2:16-3:9,
                            },
                            NativeHTML {
                                tag_name: "div",
                                opening_name_range: 3:10-3:13,
                                closing_name_range: None,
                                attributes: [],
                                range: 3:9-3:15,
                                children: [],
                                set_attributes: [],
                            },
                            Text {
                                value: "\n    ",
                                range: 3:15-4:5,
                            },
                        ],
                    },
                )
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
                Some(
                    Text {
                        value: "Nested text",
                        range: 3:15-3:26,
                    },
                )
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
                Some(
                    SlotDefinition {
                        range: 2:5-2:20,
                    },
                )
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
            expect![[r#"
                None
            "#]],
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
                Some(
                    Doctype {
                        value: "",
                        range: 2:5-2:20,
                    },
                )
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
                Some(
                    TextExpression {
                        expression: Variable {
                            name: "item",
                            range: 5:24-5:28,
                        },
                        range: 5:23-5:29,
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "br",
                        opening_name_range: 2:19-2:21,
                        closing_name_range: None,
                        attributes: [],
                        range: 2:18-2:22,
                        children: [],
                        set_attributes: [],
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "span",
                        opening_name_range: 2:11-2:15,
                        closing_name_range: Some(
                            2:23-2:27,
                        ),
                        attributes: [],
                        range: 2:10-2:28,
                        children: [
                            Text {
                                value: "Hello",
                                range: 2:16-2:21,
                            },
                        ],
                        set_attributes: [],
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "strong",
                        opening_name_range: 2:30-2:36,
                        closing_name_range: Some(
                            2:44-2:50,
                        ),
                        attributes: [],
                        range: 2:29-2:51,
                        children: [
                            Text {
                                value: "World",
                                range: 2:37-2:42,
                            },
                        ],
                        set_attributes: [],
                    },
                )
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
                Some(
                    Text {
                        value: " and ",
                        range: 2:28-2:33,
                    },
                )
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
                Some(
                    TextExpression {
                        expression: PropertyAccess {
                            object: Variable {
                                name: "item",
                                range: 10:47-10:51,
                            },
                            property: "name",
                            property_range: 10:52-10:56,
                            range: 10:47-10:56,
                        },
                        range: 10:46-10:57,
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "span",
                        opening_name_range: 6:18-6:22,
                        closing_name_range: Some(
                            9:19-9:23,
                        ),
                        attributes: [],
                        range: 6:17-9:24,
                        children: [
                            Text {
                                value: "\n                    ",
                                range: 6:23-7:21,
                            },
                            NativeHTML {
                                tag_name: "div",
                                opening_name_range: 7:22-7:25,
                                closing_name_range: Some(
                                    7:28-7:31,
                                ),
                                attributes: [],
                                range: 7:21-7:32,
                                children: [],
                                set_attributes: [],
                            },
                            Text {
                                value: "\n                    ",
                                range: 7:32-8:21,
                            },
                            NativeHTML {
                                tag_name: "em",
                                opening_name_range: 8:22-8:24,
                                closing_name_range: Some(
                                    8:36-8:38,
                                ),
                                attributes: [],
                                range: 8:21-8:39,
                                children: [
                                    Text {
                                        value: "Deep text",
                                        range: 8:25-8:34,
                                    },
                                ],
                                set_attributes: [],
                            },
                            Text {
                                value: "\n                ",
                                range: 8:39-9:17,
                            },
                        ],
                        set_attributes: [],
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "strong",
                        opening_name_range: 2:48-2:54,
                        closing_name_range: Some(
                            2:69-2:75,
                        ),
                        attributes: [],
                        range: 2:47-2:76,
                        children: [
                            TextExpression {
                                expression: PropertyAccess {
                                    object: Variable {
                                        name: "site",
                                        range: 2:56-2:60,
                                    },
                                    property: "title",
                                    property_range: 2:61-2:66,
                                    range: 2:56-2:66,
                                },
                                range: 2:55-2:67,
                            },
                        ],
                        set_attributes: [],
                    },
                )
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
                Some(
                    Text {
                        value: "Content",
                        range: 2:40-2:47,
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "span",
                        opening_name_range: 6:22-6:26,
                        closing_name_range: Some(
                            6:35-6:39,
                        ),
                        attributes: [],
                        range: 6:21-6:40,
                        children: [
                            TextExpression {
                                expression: Variable {
                                    name: "role",
                                    range: 6:28-6:32,
                                },
                                range: 6:27-6:33,
                            },
                        ],
                        set_attributes: [],
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "input",
                        opening_name_range: 3:10-3:15,
                        closing_name_range: None,
                        attributes: [
                            Attribute {
                                name: "type",
                                value: "text",
                                range: 3:16-3:27,
                                value_range: 3:22-3:26,
                            },
                            Attribute {
                                name: "placeholder",
                                value: "Enter name",
                                range: 3:28-3:52,
                                value_range: 3:41-3:51,
                            },
                            Attribute {
                                name: "required",
                                value: "",
                                range: 3:53-3:61,
                                value_range: 1:1-1:1,
                            },
                        ],
                        range: 3:9-3:64,
                        children: [],
                        set_attributes: [],
                    },
                )
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
                Some(
                    NativeHTML {
                        tag_name: "div",
                        opening_name_range: 2:23-2:26,
                        closing_name_range: Some(
                            2:35-2:38,
                        ),
                        attributes: [],
                        range: 2:22-2:39,
                        children: [
                            Text {
                                value: "Second",
                                range: 2:27-2:33,
                            },
                        ],
                        set_attributes: [],
                    },
                )
            "#]],
        );
    }

    #[test]
    fn test_depth_first_iterator() {
        // Create a simple tree: div -> (p, span -> text)
        let text_node = HopNode::Text {
            value: "hello".to_string(),
            range: Range::default(),
        };

        let span_node = HopNode::NativeHTML {
            tag_name: "span".to_string(),
            opening_name_range: Range::default(),
            closing_name_range: Some(Range::default()),
            attributes: vec![],
            range: Range::default(),
            children: vec![text_node],
            set_attributes: vec![],
        };

        let p_node = HopNode::NativeHTML {
            tag_name: "p".to_string(),
            opening_name_range: Range::default(),
            closing_name_range: Some(Range::default()),
            attributes: vec![],
            range: Range::default(),
            children: vec![],
            set_attributes: vec![],
        };

        let div_node = HopNode::NativeHTML {
            tag_name: "div".to_string(),
            opening_name_range: Range::default(),
            closing_name_range: Some(Range::default()),
            attributes: vec![],
            range: Range::default(),
            children: vec![p_node, span_node],
            set_attributes: vec![],
        };

        let nodes: Vec<&HopNode> = div_node.iter_depth_first().collect();

        // Should visit: div, p, span, text
        assert_eq!(nodes.len(), 4);

        // Check the order
        assert!(matches!(nodes[0], HopNode::NativeHTML { tag_name, .. } if tag_name == "div"));
        assert!(matches!(nodes[1], HopNode::NativeHTML { tag_name, .. } if tag_name == "p"));
        assert!(matches!(nodes[2], HopNode::NativeHTML { tag_name, .. } if tag_name == "span"));
        assert!(matches!(nodes[3], HopNode::Text { value, .. } if value == "hello"));
    }
}
