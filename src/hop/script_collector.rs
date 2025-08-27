use crate::hop::ast::{ComponentDefinitionNode, HopNode};

pub struct ScriptCollector {
    scripts: Vec<String>,
}

impl ScriptCollector {
    pub fn new() -> Self {
        ScriptCollector {
            scripts: Vec::new(),
        }
    }

    pub fn process_module(&mut self, module_name: &str, components: &[ComponentDefinitionNode]) {
        let mut module_script = String::new();

        for component in components {
            for child in &component.children {
                for node in child.iter_depth_first() {
                    if let HopNode::NativeHTML(native_html_node) = node {
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
                        module_script.push_str(&wrapped_script);
                        module_script.push('\n');
                    }
                }
            }
        }

        if !module_script.is_empty() {
            self.scripts.push(module_script);
        }
    }

    pub fn build(&self) -> String {
        self.scripts.join("")
    }
}
