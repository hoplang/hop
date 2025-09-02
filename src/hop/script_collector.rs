use crate::hop::ast::HopAst;
use crate::hop::ast::HopNode;

pub struct ScriptCollector {
    scripts: Vec<String>,
}

impl ScriptCollector {
    pub fn new() -> Self {
        ScriptCollector {
            scripts: Vec::new(),
        }
    }

    pub fn process_module(&mut self, ast: &HopAst) {
        let mut module_script = String::new();

        for component in ast.get_component_definitions() {
            for child in &component.children {
                for node in child.iter_depth_first() {
                    if let HopNode::Html {
                        tag_name, children, ..
                    } = node
                    {
                        if tag_name != "script" {
                            continue;
                        }

                        if children.is_empty() {
                            continue;
                        }

                        assert!(
                            children.len() == 1,
                            "Script tag should have exactly one child"
                        );

                        let script_content = match &children[0] {
                            HopNode::Text { value, .. } => value,
                            _ => panic!("Script tag child should be a text node"),
                        };

                        let data_hop_id = format!("{}/{}", ast.name, component.tag_name);
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
