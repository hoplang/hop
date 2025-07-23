use crate::common::{
    BinaryOp, ComponentNode, CondNode, DefineSlotNode, Environment, ErrorNode, Expression, ForNode,
    NativeHTMLNode, Node, RenderNode, SupplySlotNode, Type, escape_html, is_void_element,
};
use std::collections::HashMap;

/// Program represents a compiled hop program that can execute components and entrypoints
#[derive(Clone)]
pub struct Program {
    component_maps: HashMap<String, HashMap<String, ComponentNode>>,
    import_maps: HashMap<String, HashMap<String, String>>,
    parameter_types: HashMap<String, HashMap<String, Type>>,
}

impl Program {
    pub fn new(
        component_maps: HashMap<String, HashMap<String, ComponentNode>>,
        import_maps: HashMap<String, HashMap<String, String>>,
        parameter_types: HashMap<String, HashMap<String, Type>>,
    ) -> Self {
        Program {
            component_maps,
            import_maps,
            parameter_types,
        }
    }

    pub fn execute_simple(
        &self,
        module_name: &str,
        component_name: &str,
        params: serde_json::Value,
    ) -> Result<String, String> {
        let empty_slots = HashMap::new();
        self.execute(module_name, component_name, params, &empty_slots)
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
        slot_content: &HashMap<String, String>,
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

        if component.entrypoint {
            // For entrypoints, don't wrap in a div, just execute children directly
            let mut result = String::new();
            for child in &component.children {
                result.push_str(&self.evaluate_node_entrypoint(child, &mut env, module_name)?);
            }
            Ok(result)
        } else {
            // For regular components, wrap in the specified element type
            let mut element_type = "div".to_string();
            if let Some(as_attr) = &component.as_attr {
                element_type = as_attr.value.clone();
            }

            let data_hop_id = format!("{}/{}", module_name, component_name);
            let mut result = format!("<{} data-hop-id=\"{}\"", element_type, data_hop_id);

            for attr in &component.attributes {
                if attr.name != "name"
                    && attr.name != "params-as"
                    && attr.name != "as"
                    && attr.name != "entrypoint"
                {
                    result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                }
            }
            result.push('>');
            for child in &component.children {
                result.push_str(&self.evaluate_node(child, slot_content, &mut env, module_name)?);
            }
            result.push_str(&format!("</{}>", element_type));

            Ok(result)
        }
    }

    fn evaluate_node(
        &self,
        node: &Node,
        slot_content: &HashMap<String, String>,
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
                        result.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
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
                        result.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
                    }
                    Ok(result)
                } else {
                    Ok(String::new())
                }
            }
            Node::Render(RenderNode {
                component_attr,
                params_attr,
                children,
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

                // Collect and evaluate supply-slot mappings
                let mut slot_content: HashMap<String, String> = HashMap::new();
                for child in children {
                    if let Node::SupplySlot(SupplySlotNode { name, children, .. }) = child {
                        let mut slot_html = String::new();
                        let empty_slots: HashMap<String, String> = HashMap::new();
                        for slot_child in children {
                            slot_html.push_str(&self.evaluate_node(
                                slot_child,
                                &empty_slots,
                                env,
                                current_module,
                            )?);
                        }
                        slot_content.insert(name.clone(), slot_html);
                    }
                }

                self.execute(&target_module, component_name, params_value, &slot_content)
            }
            Node::NativeHTML(NativeHTMLNode {
                inner_text_attr,
                children,
                tag_name,
                attributes,
                set_attributes,
                ..
            }) => {
                // Skip script and style nodes
                if tag_name == "script" || tag_name == "style" {
                    return Ok(String::new());
                }

                let mut result = format!("<{}", tag_name);
                for attr in attributes {
                    if !attr.name.starts_with("set-") {
                        result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                    }
                }

                // Evaluate and add set-* attributes
                for set_attr in set_attributes {
                    let attr_name = &set_attr.name[4..]; // Remove "set-" prefix
                    let evaluated = self.evaluate_expr(&set_attr.expression, env)?;
                    result.push_str(&format!(
                        " {}=\"{}\"",
                        attr_name,
                        escape_html(evaluated.as_str().unwrap())
                    ));
                }

                result.push('>');

                if !is_void_element(tag_name) {
                    if let Some(attr) = inner_text_attr {
                        let evaluated = self.evaluate_expr(&attr.expression, env)?;
                        result.push_str(&escape_html(evaluated.as_str().unwrap()));
                    } else {
                        for child in children {
                            result.push_str(&self.evaluate_node(
                                child,
                                slot_content,
                                env,
                                current_module,
                            )?);
                        }
                    }
                    result.push_str(&format!("</{}>", tag_name));
                }

                Ok(result)
            }
            Node::Error(ErrorNode { children, .. }) => {
                let mut result = String::new();
                for child in children {
                    result.push_str(&self.evaluate_node(
                        child,
                        slot_content,
                        env,
                        current_module,
                    )?);
                }
                Ok(result)
            }
            Node::Text(text_node) => Ok(text_node.value.clone()),
            Node::Doctype(doctype_node) => Ok(format!("<!DOCTYPE {}>", doctype_node.value)),
            Node::DefineSlot(DefineSlotNode { name, children, .. }) => {
                // Check if we have supply-slot content for this slot
                if let Some(supplied_html) = slot_content.get(name) {
                    // Use the pre-evaluated supplied content
                    Ok(supplied_html.clone())
                } else {
                    // Render the default content
                    let mut result = String::new();
                    for child in children {
                        result.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
                    }
                    Ok(result)
                }
            }
            Node::SupplySlot(SupplySlotNode { children, .. }) => {
                let mut result = String::new();
                for child in children {
                    result.push_str(&self.evaluate_node(
                        child,
                        slot_content,
                        env,
                        current_module,
                    )?);
                }
                Ok(result)
            }
            Node::Import(_) | Node::Component(_) => {
                panic!("Unexpected node")
            }
        }
    }

    fn evaluate_node_entrypoint(
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
                set_attributes,
                ..
            }) => {
                // For entrypoints, preserve script and style tags
                let mut result = format!("<{}", tag_name);
                for attr in attributes {
                    if !attr.name.starts_with("set-") {
                        result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                    }
                }

                // Evaluate and add set-* attributes
                for set_attr in set_attributes {
                    let attr_name = &set_attr.name[4..]; // Remove "set-" prefix
                    let evaluated = self.evaluate_expr(&set_attr.expression, env)?;
                    result.push_str(&format!(
                        " {}=\"{}\"",
                        attr_name,
                        escape_html(evaluated.as_str().unwrap())
                    ));
                }

                result.push('>');

                if !is_void_element(tag_name) {
                    if let Some(attr) = inner_text_attr {
                        let evaluated = self.evaluate_expr(&attr.expression, env)?;
                        result.push_str(&escape_html(evaluated.as_str().unwrap()));
                    } else {
                        for child in children {
                            result.push_str(&self.evaluate_node_entrypoint(
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
                // For all other node types, use the existing evaluation logic (no slots in entrypoints)
                let empty_slots: HashMap<String, String> = HashMap::new();
                self.evaluate_node(node, &empty_slots, env, current_module)
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
                    .cloned()
            }
            Expression::BinaryOp(left, BinaryOp::Equal, right) => {
                let left_value = self.evaluate_expr(left, env)?;
                let right_value = self.evaluate_expr(right, env)?;

                Ok(serde_json::Value::Bool(left_value == right_value))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Position, Range, Token, TokenKind};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;
    use crate::typechecker::typecheck;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    fn compile_modules(modules_source: Vec<(String, String)>) -> Result<Program, String> {
        let mut component_maps = HashMap::new();
        let mut import_maps = HashMap::new();
        let mut module_parameter_types: HashMap<String, HashMap<String, Type>> = HashMap::new();

        // Parse and typecheck modules in order
        for (module_name, source_code) in &modules_source {
            let mut errors = Vec::new();
            let tokens = tokenize(source_code, &mut errors);
            let module = parse(tokens, &mut errors);

            let mut import_types = HashMap::new();
            for n in &module.imports {
                let from_module = &n.from_attr.value;
                let component_name = &n.component_attr.value;
                if let Some(types) = module_parameter_types.get(from_module) {
                    if let Some(component_type) = types.get(component_name) {
                        import_types.insert(component_name.clone(), component_type.clone());
                    }
                }
            }

            let type_info = typecheck(&module, &import_types, &mut errors);
            if !errors.is_empty() {
                return Err(format!("Errors in {}: {:?}", module_name, errors));
            }

            let mut component_map = HashMap::new();
            let mut import_map = HashMap::new();

            for n in &module.components {
                component_map.insert(n.name.clone(), n.clone());
            }

            for n in &module.imports {
                import_map.insert(n.component_attr.value.clone(), n.from_attr.value.clone());
            }

            component_maps.insert(module_name.clone(), component_map);
            import_maps.insert(module_name.clone(), import_map);
            module_parameter_types.insert(module_name.clone(), type_info.parameter_types);
        }

        Ok(Program::new(
            component_maps,
            import_maps,
            module_parameter_types,
        ))
    }

    fn normalize_tokens(tokens: Vec<Token>) -> Vec<Token> {
        tokens
            .into_iter()
            .map(|t| {
                let normalized_value = if t.kind == TokenKind::Text && t.value.trim().is_empty() {
                    " ".to_string()
                } else {
                    t.value
                };

                // Normalize and sort attributes
                let mut normalized_attrs = t.attributes;
                normalized_attrs.iter_mut().for_each(|attr| {
                    attr.range = Range::new(Position::new(0, 0), Position::new(0, 0));
                });
                normalized_attrs.sort_by(|a, b| a.name.cmp(&b.name));

                Token::new(
                    t.kind,
                    normalized_value,
                    normalized_attrs,
                    Range::new(Position::new(0, 0), Position::new(0, 0)),
                )
            })
            .collect()
    }

    #[test]
    fn test_runtime() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/runtime");
        let entries = fs::read_dir(d).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let data_json = archive.get("data.json").unwrap().content.trim();
            let expected_output = archive.get("output.html").unwrap().content.trim();

            // Collect all modules from .hop files in order
            let mut modules_source = Vec::new();
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let module_name = file.name.trim_end_matches(".hop");
                    modules_source.push((module_name.to_string(), file.content.trim().to_string()));
                }
            }

            let program = compile_modules(modules_source).unwrap_or_else(|e| {
                panic!("Compilation failed for {}: {}", file_name, e);
            });

            println!("{}", file_name);

            let data: serde_json::Value = serde_json::from_str(data_json).unwrap_or_else(|e| {
                panic!("Failed to parse JSON data for {}: {}", file_name, e);
            });

            let actual_output = program
                .execute_simple("main", "main-comp", data)
                .unwrap_or_else(|e| {
                    panic!("Execution failed for {}: {}", file_name, e);
                });

            // Normalize whitespace by tokenizing both outputs and comparing tokens
            let mut errors = Vec::new();
            let expected_tokens = normalize_tokens(tokenize(expected_output, &mut errors));
            let actual_tokens = normalize_tokens(tokenize(actual_output.trim(), &mut errors));
            assert!(errors.is_empty());

            assert_eq!(
                actual_tokens, expected_tokens,
                "Mismatch in file: {}",
                file_name
            );
        }
    }
}
