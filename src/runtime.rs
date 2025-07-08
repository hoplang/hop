use crate::common::{escape_html, is_void_element, ComponentNode, Environment, Node};
use std::collections::HashMap;

/// Program represents a compiled hop program that can execute components
pub struct Program {
    component_maps: HashMap<String, HashMap<String, ComponentNode>>,
    import_maps: HashMap<String, HashMap<String, String>>,
    scripts: String,
}

impl Program {
    pub fn new(
        component_maps: HashMap<String, HashMap<String, ComponentNode>>,
        import_maps: HashMap<String, HashMap<String, String>>,
        scripts: String,
    ) -> Self {
        Program {
            component_maps,
            import_maps,
            scripts,
        }
    }

    pub fn get_scripts(&self) -> &str {
        &self.scripts
    }

    pub fn execute(
        &self,
        module_name: &str,
        component_name: &str,
        params: serde_json::Value,
    ) -> Result<String, String> {
        let component_map = self
            .component_maps
            .get(module_name)
            .ok_or_else(|| format!("Module '{}' not found", module_name))?;

        let component = component_map.get(component_name).ok_or_else(|| {
            format!(
                "Component '{}' not found in module '{}'",
                component_name, module_name
            )
        })?;

        let mut env = Environment::new();
        if let Some(params_as_attr) = &component.params_as_attr {
            env.push(params_as_attr.value.clone(), params);
        }

        let mut element_type = "div".to_string();
        if let Some(as_attr) = &component.as_attr {
            element_type = as_attr.value.clone();
        }

        let data_hop_id = format!("{}/{}", module_name, component_name);
        let mut result = format!("<{} data-hop-id=\"{}\"", element_type, data_hop_id);

        for attr in &component.attributes {
            if attr.name != "name" && attr.name != "params-as" && attr.name != "as" {
                result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
            }
        }
        result.push('>');
        result.push_str(&self.execute_nodes(&component.children, &mut env, module_name)?);
        result.push_str(&format!("</{}>", element_type));

        Ok(result)
    }

    fn execute_nodes(
        &self,
        nodes: &[Node],
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String, String> {
        let mut result = String::new();

        for node in nodes {
            match node {
                Node::Text(text_node) => {
                    result.push_str(&text_node.value);
                }
                Node::Doctype(doctype_node) => {
                    result.push_str(&format!("<!DOCTYPE {}>", doctype_node.value));
                }
                Node::NativeHTML(native_html_node) => {
                    // Skip script and style nodes
                    if native_html_node.value == "script" || native_html_node.value == "style" {
                        continue;
                    }

                    result.push_str(&format!("<{}", native_html_node.value));
                    for attr in &native_html_node.attributes {
                        if attr.name != "inner-text" {
                            result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                        }
                    }
                    result.push('>');

                    if !is_void_element(&native_html_node.value) {
                        if let Some(inner_text_attr) = &native_html_node.inner_text_attr {
                            let evaluated =
                                self.evaluate_expression(&inner_text_attr.segments, env)?;
                            result.push_str(&escape_html(evaluated.as_str().unwrap()));
                        } else {
                            result.push_str(&self.execute_nodes(
                                &native_html_node.children,
                                env,
                                current_module,
                            )?);
                        }
                        result.push_str(&format!("</{}>", native_html_node.value));
                    }
                }
                Node::For(for_node) => {
                    let array_value =
                        self.evaluate_expression(&for_node.each_attr.segments, env)?;

                    let array = array_value
                        .as_array()
                        .ok_or_else(|| "For loop expects an array".to_string())?;

                    for item in array {
                        if let Some(as_attr) = &for_node.as_attr {
                            env.push(as_attr.value.clone(), item.clone());
                        }
                        result.push_str(&self.execute_nodes(
                            &for_node.children,
                            env,
                            current_module,
                        )?);
                        if for_node.as_attr.is_some() {
                            env.pop();
                        }
                    }
                }
                Node::Cond(cond_node) => {
                    let condition_value =
                        self.evaluate_expression(&cond_node.if_attr.segments, env)?;
                    if condition_value.as_bool().unwrap_or(false) {
                        result.push_str(&self.execute_nodes(
                            &cond_node.children,
                            env,
                            current_module,
                        )?);
                    }
                }
                Node::Render(render_node) => {
                    let mut params_value = serde_json::Value::Null;
                    if let Some(params_attr) = &render_node.params_attr {
                        params_value = self.evaluate_expression(&params_attr.segments, env)?;
                    }

                    let component_name = &render_node.component_attr.value;
                    let mut target_module = current_module.to_string();

                    if let Some(current_module_import_map) = self.import_maps.get(current_module) {
                        if let Some(imported_module) = current_module_import_map.get(component_name)
                        {
                            target_module = imported_module.clone();
                        }
                    }

                    result.push_str(&self.execute(&target_module, component_name, params_value)?);
                }
                Node::Component(component_node) => {
                    // Component nodes shouldn't appear in execution, but handle children just in case
                    result.push_str(&self.execute_nodes(
                        &component_node.children,
                        env,
                        current_module,
                    )?);
                }
                Node::Import(_) => {
                    // Import nodes shouldn't appear in execution
                }
                Node::Error(error_node) => {
                    result.push_str(&self.execute_nodes(
                        &error_node.children,
                        env,
                        current_module,
                    )?);
                }
            }
        }

        Ok(result)
    }

    fn evaluate_expression(
        &self,
        expr: &[String],
        env: &Environment<serde_json::Value>,
    ) -> Result<serde_json::Value, String> {
        if expr.is_empty() {
            return Err("Empty expression".to_string());
        }

        if !env.has(&expr[0]) {
            return Err(format!("Undefined variable: {}", expr[0]));
        }

        let mut current_value = env
            .lookup(&expr[0])
            .ok_or_else(|| format!("Undefined variable: {}", expr[0]))?
            .clone();

        for segment in expr.iter().skip(1) {
            if current_value.is_null() {
                return Err("Current value is not defined".to_string());
            }

            if !current_value.is_object() {
                return Err("Cannot access property of non-object".to_string());
            }

            current_value = current_value
                .get(segment)
                .ok_or_else(|| format!("Property '{}' not found", segment))?
                .clone();
        }

        Ok(current_value)
    }
}
