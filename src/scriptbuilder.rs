use crate::common::{ComponentNode, Node};
use std::collections::HashMap;

pub struct ScriptBuilder {
    modules: HashMap<String, Vec<ComponentNode>>,
}

impl ScriptBuilder {
    pub fn new() -> Self {
        ScriptBuilder {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, module_name: String, components: Vec<ComponentNode>) {
        self.modules.insert(module_name, components);
    }

    fn get_all_descendants(&self, nodes: &[Node]) -> Vec<Node> {
        let mut result = Vec::new();
        for node in nodes {
            result.push(node.clone());
            match node {
                Node::Component(component_node) => {
                    result.extend(self.get_all_descendants(&component_node.children));
                }
                Node::NativeHTML(native_html_node) => {
                    result.extend(self.get_all_descendants(&native_html_node.children));
                }
                Node::For(for_node) => {
                    result.extend(self.get_all_descendants(&for_node.children));
                }
                Node::Cond(cond_node) => {
                    result.extend(self.get_all_descendants(&cond_node.children));
                }
                Node::Render(render_node) => {
                    result.extend(self.get_all_descendants(&render_node.children));
                }
                _ => {
                    // leaf nodes
                }
            }
        }
        result
    }

    pub fn build(&self) -> String {
        let mut concatenated_script = String::new();

        // Process all modules
        for (module_name, components) in &self.modules {
            for component in components {
                let all_nodes = self.get_all_descendants(&component.children);
                for node in all_nodes {
                    match node {
                        Node::NativeHTML(native_html_node) => {
                            if native_html_node.value != "script" {
                                continue;
                            }

                            if native_html_node.children.is_empty() {
                                continue;
                            }

                            if native_html_node.children.len() != 1 {
                                panic!("Script tag should have exactly one child");
                            }

                            let script_content = match &native_html_node.children[0] {
                                Node::Text(text_node) => &text_node.value,
                                _ => panic!("Script tag child should be a text node"),
                            };

                            let data_hop_id =
                                format!("{}/{}", module_name, component.name_attr.value);
                            let wrapped_script = format!(
                                "document.querySelectorAll('[data-hop-id=\"{}\"]').forEach((frameElement) => {{{}}});",
                                data_hop_id, script_content
                            );
                            concatenated_script.push_str(&wrapped_script);
                            concatenated_script.push('\n');
                        }
                        _ => continue,
                    }
                }
            }
        }

        concatenated_script
    }
}
