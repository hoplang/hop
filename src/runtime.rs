use crate::common::{
    BinaryOp, ComponentNode, CondNode, EntrypointNode, Environment, ErrorNode, Expression, ForNode,
    NativeHTMLNode, Node, RenderNode, Type, escape_html, is_void_element,
};
use std::collections::HashMap;

/// Program represents a compiled hop program that can execute components and entrypoints
#[derive(Clone)]
pub struct Program {
    component_maps: HashMap<String, HashMap<String, ComponentNode>>,
    entrypoint_maps: HashMap<String, HashMap<String, EntrypointNode>>,
    import_maps: HashMap<String, HashMap<String, String>>,
    parameter_types: HashMap<String, HashMap<String, Type>>,
}

impl Program {
    pub fn new(
        component_maps: HashMap<String, HashMap<String, ComponentNode>>,
        entrypoint_maps: HashMap<String, HashMap<String, EntrypointNode>>,
        import_maps: HashMap<String, HashMap<String, String>>,
        parameter_types: HashMap<String, HashMap<String, Type>>,
    ) -> Self {
        Program {
            component_maps,
            entrypoint_maps,
            import_maps,
            parameter_types,
        }
    }

    /// Validate that the provided parameters match the expected JSON schema for the component
    pub fn validate(
        &self,
        module_name: &str,
        component_name: &str,
        params: &serde_json::Value,
    ) -> anyhow::Result<()> {
        let component_type = self
            .parameter_types
            .get(module_name)
            .and_then(|types| types.get(component_name))
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Component '{}' not found in module '{}'",
                    component_name,
                    module_name
                )
            })?;
        let schema = component_type.to_json_schema();

        let compiled_schema = jsonschema::JSONSchema::compile(&schema)
            .map_err(|e| anyhow::anyhow!("Failed to compile JSON schema: {}", e))?;

        if let Err(validation_errors) = compiled_schema.validate(params) {
            let error_messages: Vec<String> = validation_errors
                .map(|error| format!("{} at {}", error, error.instance_path))
                .collect();
            return Err(anyhow::anyhow!("{}", error_messages.join("\n")));
        }

        Ok(())
    }

    pub fn execute(
        &self,
        module_name: &str,
        component_name: &str,
        params: serde_json::Value,
    ) -> Result<String, String> {
        // First try to find entrypoints, then components
        if let Some(entrypoint_map) = self.entrypoint_maps.get(module_name) {
            if let Some(entrypoint) = entrypoint_map.get(component_name) {
                return self.execute_entrypoint(entrypoint, params, module_name);
            }
        }

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
        for child in &component.children {
            result.push_str(&self.evaluate_node(child, &mut env, module_name)?);
        }
        result.push_str(&format!("</{}>", element_type));

        Ok(result)
    }

    fn execute_entrypoint(
        &self,
        entrypoint: &EntrypointNode,
        params: serde_json::Value,
        module_name: &str,
    ) -> Result<String, String> {
        let mut env = Environment::new();
        if let Some(params_as_attr) = &entrypoint.params_as_attr {
            env.push(params_as_attr.value.clone(), params);
        }

        let mut result = String::new();
        for child in &entrypoint.children {
            result.push_str(&self.evaluate_entrypoint_node(child, &mut env, module_name)?);
        }

        Ok(result)
    }

    fn evaluate_entrypoint_node(
        &self,
        node: &Node,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String, String> {
        match node {
            Node::NativeHTML(NativeHTMLNode {
                tag_name,
                attributes,
                children,
                inner_text_attr,
                ..
            }) => {
                // For entrypoints, preserve script and style tags
                let mut result = format!("<{}", tag_name);
                for attr in attributes {
                    if attr.name != "inner-text" {
                        result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                    }
                }
                result.push('>');

                if !is_void_element(tag_name) {
                    if let Some(attr) = inner_text_attr {
                        let evaluated = self.evaluate_expr(&attr.expression, env)?;
                        result.push_str(&escape_html(evaluated.as_str().unwrap()));
                    } else {
                        for child in children {
                            result.push_str(&self.evaluate_entrypoint_node(
                                child,
                                env,
                                current_module,
                            )?);
                        }
                    }
                    result.push_str(&format!("</{}>", tag_name));
                }

                Ok(result)
            }
            _ => {
                // For all other node types, use the existing evaluation logic
                self.evaluate_node(node, env, current_module)
            }
        }
    }

    fn evaluate_node(
        &self,
        node: &Node,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String, String> {
        match node {
            Node::For(ForNode {
                as_attr,
                each_attr,
                children,
                ..
            }) => {
                let array_value = self.evaluate_expr(&each_attr.expression, env)?;

                let array = array_value
                    .as_array()
                    .ok_or_else(|| "For loop expects an array".to_string())?;

                let mut result = String::new();
                for item in array {
                    if let Some(attr) = as_attr {
                        env.push(attr.value.clone(), item.clone());
                    }
                    for child in children {
                        result.push_str(&self.evaluate_node(child, env, current_module)?);
                    }
                    if as_attr.is_some() {
                        env.pop();
                    }
                }

                Ok(result)
            }
            Node::Cond(CondNode {
                if_attr, children, ..
            }) => {
                let condition_value = self.evaluate_expr(&if_attr.expression, env)?;
                if condition_value.as_bool().unwrap_or(false) {
                    let mut result = String::new();
                    for child in children {
                        result.push_str(&self.evaluate_node(child, env, current_module)?);
                    }
                    Ok(result)
                } else {
                    Ok(String::new())
                }
            }
            Node::Render(RenderNode {
                component_attr,
                params_attr,
                ..
            }) => {
                let mut params_value = serde_json::Value::Null;
                if let Some(attr) = params_attr {
                    params_value = self.evaluate_expr(&attr.expression, env)?;
                }

                let component_name = &component_attr.value;
                let mut target_module = current_module.to_string();

                if let Some(current_module_import_map) = self.import_maps.get(current_module) {
                    if let Some(imported_module) = current_module_import_map.get(component_name) {
                        target_module = imported_module.clone();
                    }
                }

                self.execute(&target_module, component_name, params_value)
            }
            Node::NativeHTML(NativeHTMLNode {
                inner_text_attr,
                children,
                tag_name,
                attributes,
                ..
            }) => {
                // Skip script and style nodes
                if tag_name == "script" || tag_name == "style" {
                    return Ok(String::new());
                }

                let mut result = format!("<{}", tag_name);
                for attr in attributes {
                    if attr.name != "inner-text" {
                        result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                    }
                }
                result.push('>');

                if !is_void_element(tag_name) {
                    if let Some(attr) = inner_text_attr {
                        let evaluated = self.evaluate_expr(&attr.expression, env)?;
                        result.push_str(&escape_html(evaluated.as_str().unwrap()));
                    } else {
                        for child in children {
                            result.push_str(&self.evaluate_node(child, env, current_module)?);
                        }
                    }
                    result.push_str(&format!("</{}>", tag_name));
                }

                Ok(result)
            }
            Node::Error(ErrorNode { children, .. }) => {
                let mut result = String::new();
                for child in children {
                    result.push_str(&self.evaluate_node(child, env, current_module)?);
                }
                Ok(result)
            }
            Node::Text(text_node) => Ok(text_node.value.clone()),
            Node::Doctype(doctype_node) => Ok(format!("<!DOCTYPE {}>", doctype_node.value)),
            Node::Import(_) | Node::Component(_) | Node::Entrypoint(_) => {
                panic!("Unexpected node")
            }
        }
    }

    fn evaluate_expr(
        &self,
        expr: &Expression,
        env: &mut Environment<serde_json::Value>,
    ) -> Result<serde_json::Value, String> {
        match expr {
            Expression::Variable(name) => {
                if let Some(val) = env.lookup(name) {
                    Ok(val.clone())
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            Expression::StringLiteral(value) => Ok(serde_json::Value::String(value.clone())),
            Expression::PropertyAccess(base_expr, property) => {
                let base_value = self.evaluate_expr(base_expr, env)?;

                if base_value.is_null() {
                    return Err("Cannot access property of null value".to_string());
                }

                if !base_value.is_object() {
                    return Err("Cannot access property of non-object".to_string());
                }

                base_value
                    .get(property)
                    .ok_or_else(|| format!("Property '{}' not found", property))
                    .map(|v| v.clone())
            }
            Expression::BinaryOp(left, BinaryOp::Equal, right) => {
                let left_value = self.evaluate_expr(left, env)?;
                let right_value = self.evaluate_expr(right, env)?;

                Ok(serde_json::Value::Bool(left_value == right_value))
            }
        }
    }
}
