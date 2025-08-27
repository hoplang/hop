use crate::hop::ast::{ComponentDefinitionNode, HopNode};
use std::collections::HashMap;

pub struct ScriptCollector {
    modules: HashMap<String, Vec<ComponentDefinitionNode>>,
}

impl ScriptCollector {
    pub fn new() -> Self {
        ScriptCollector {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, module_name: String, components: Vec<ComponentDefinitionNode>) {
        self.modules.insert(module_name, components);
    }

    fn get_all_descendants(&self, nodes: &[HopNode]) -> Vec<HopNode> {
        let mut result = Vec::new();
        for node in nodes {
            result.push(node.clone());
            match node {
                HopNode::NativeHTML(native_html_node) => {
                    result.extend(self.get_all_descendants(&native_html_node.children));
                }
                HopNode::If(if_node) => {
                    result.extend(self.get_all_descendants(&if_node.children));
                }
                HopNode::For(for_node) => {
                    result.extend(self.get_all_descendants(&for_node.children));
                }
                HopNode::ComponentReference(render_node) => {
                    result.extend(self.get_all_descendants(&render_node.children));
                }
                HopNode::XRaw(xraw_node) => {
                    result.extend(self.get_all_descendants(&xraw_node.children));
                }
                HopNode::XLoadJson(xloadjson_node) => {
                    result.extend(self.get_all_descendants(&xloadjson_node.children));
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
                        HopNode::NativeHTML(native_html_node) => {
                            if native_html_node.tag_name != "script" {
                                continue;
                            }

                            if native_html_node.children.is_empty() {
                                continue;
                            }

                            if native_html_node.children.len() != 1 {
                                panic!("Script tag should have exactly one child");
                            }

                            let script_content = match &native_html_node.children[0] {
                                HopNode::Text(text_node) => &text_node.value,
                                _ => panic!("Script tag child should be a text node"),
                            };

                            let data_hop_id = format!("{}/{}", module_name, component.name);
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
