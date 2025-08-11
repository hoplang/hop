use crate::common::{
    BinaryOp, ComponentDefinitionNode, ComponentReferenceNode, Environment, ErrorNode, Expression,
    ForeachNode, IfNode, NativeHTMLNode, Node, RenderNode, SlotDefinitionNode, SlotReferenceNode, Type,
    XExecNode, escape_html, is_void_element,
};
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

/// Program represents a compiled hop program that can execute components and entrypoints
#[derive(Clone)]
pub struct Program {
    component_maps: HashMap<String, HashMap<String, ComponentDefinitionNode>>,
    import_maps: HashMap<String, HashMap<String, String>>,
    parameter_types: HashMap<String, HashMap<String, Type>>,
    render_nodes: HashMap<String, Vec<RenderNode>>,
    scripts: String,
}

impl Program {
    pub fn new(
        component_maps: HashMap<String, HashMap<String, ComponentDefinitionNode>>,
        import_maps: HashMap<String, HashMap<String, String>>,
        parameter_types: HashMap<String, HashMap<String, Type>>,
        render_maps: HashMap<String, Vec<RenderNode>>,
        scripts: String,
    ) -> Self {
        Program {
            component_maps,
            import_maps,
            parameter_types,
            render_nodes: render_maps,
            scripts,
        }
    }

    pub fn get_render_nodes(&self) -> &HashMap<String, Vec<RenderNode>> {
        &self.render_nodes
    }

    /// Get the collected scripts from the program
    pub fn get_scripts(&self) -> &str {
        &self.scripts
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
            env.push(params_as_attr.var_name.value.clone(), params);
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

    pub fn evaluate_node(
        &self,
        node: &Node,
        slot_content: &HashMap<String, String>,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String, String> {
        match node {
            Node::If(IfNode {
                condition,
                children,
                ..
            }) => {
                let condition_value = self.evaluate_expr(condition, env)?;
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
            Node::ComponentReference(ComponentReferenceNode {
                component,
                params_attr,
                children,
                ..
            }) => {
                let mut params_value = serde_json::Value::Null;
                if let Some(attr) = params_attr {
                    params_value = self.evaluate_expr(&attr.expression, env)?;
                }

                let component_name = component;
                let mut target_module = current_module.to_string();

                if let Some(current_module_import_map) = self.import_maps.get(current_module) {
                    if let Some(imported_module) = current_module_import_map.get(component_name) {
                        target_module = imported_module.clone();
                    }
                }

                // Check if target component has only a default slot
                let target_component = self
                    .component_maps
                    .get(&target_module)
                    .and_then(|module_map| module_map.get(component_name));

                let has_only_default_slot = target_component
                    .map(|comp| {
                        comp.slots.len() == 1 && comp.slots.contains(&"default".to_string())
                    })
                    .unwrap_or(false);

                // Collect and evaluate supply-slot mappings
                let mut new_slot_content: HashMap<String, String> = HashMap::new();
                let mut non_slot_children = Vec::new();

                for child in children {
                    if let Node::SlotReference(SlotReferenceNode { name, children, .. }) = child {
                        let mut slot_html = String::new();
                        for slot_child in children {
                            slot_html.push_str(&self.evaluate_node(
                                slot_child,
                                slot_content,
                                env,
                                current_module,
                            )?);
                        }
                        new_slot_content.insert(name.clone(), slot_html);
                    } else if has_only_default_slot {
                        // For components with only default slot, collect non-slot children
                        non_slot_children.push(child);
                    }
                }

                // If component has only default slot and we have non-slot children,
                // pass them as default slot content
                if has_only_default_slot
                    && !non_slot_children.is_empty()
                    && !new_slot_content.contains_key("default")
                {
                    let mut default_html = String::new();
                    for child in non_slot_children {
                        default_html.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
                    }
                    new_slot_content.insert("default".to_string(), default_html);
                }

                self.execute(
                    &target_module,
                    component_name,
                    params_value,
                    &new_slot_content,
                )
            }
            Node::NativeHTML(NativeHTMLNode {
                inner_text_attr,
                children,
                tag_name,
                attributes,
                set_attributes,
                ..
            }) => {
                // Skip style nodes
                if tag_name == "style" {
                    return Ok(String::new());
                }

                // Skip script nodes without a src attribute
                if tag_name == "script" && !attributes.iter().any(|e| e.name == "src") {
                    return Ok(String::new());
                }

                // For hop-x-raw tags, just render the inner content without the tags
                if tag_name == "hop-x-raw" {
                    let mut result = String::new();
                    for child in children {
                        result.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
                    }
                    return Ok(result);
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
            Node::SlotDefinition(SlotDefinitionNode { name, children, .. }) => {
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
            Node::SlotReference(SlotReferenceNode { children, .. }) => {
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
            Node::XExec(XExecNode {
                cmd_attr, children, ..
            }) => {
                // Collect child content as stdin
                let mut stdin_content = String::new();
                for child in children {
                    stdin_content.push_str(&self.evaluate_node(
                        child,
                        slot_content,
                        env,
                        current_module,
                    )?);
                }

                // Execute the command with stdin
                let command = &cmd_attr.value;
                self.execute_command(command, &stdin_content)
            }
            Node::Foreach(ForeachNode { var_name, array_expr, children, .. }) => {
                let array_value = self.evaluate_expr(array_expr, env)?;
                
                let array = array_value
                    .as_array()
                    .ok_or_else(|| "Foreach loop expects an array".to_string())?;

                let mut result = String::new();
                for item in array {
                    env.push(var_name.value.clone(), item.clone());
                    for child in children {
                        result.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
                    }
                    env.pop();
                }

                Ok(result)
            }
        }
    }

    pub fn evaluate_node_entrypoint(
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
            Expression::LoopGenerator(_, _) => {
                Err("Loop generators can only be used within foreach expressions".to_string())
            }
        }
    }

    fn execute_command(&self, command: &str, stdin_content: &str) -> Result<String, String> {
        // Parse the command and arguments
        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return Err("Empty command".to_string());
        }

        let cmd = parts[0];
        let args = &parts[1..];

        // Execute the command
        let mut child = Command::new(cmd)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("Failed to execute command '{}': {}", command, e))?;

        // Write stdin content to the child process
        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(stdin_content.as_bytes())
                .map_err(|e| format!("Failed to write to stdin: {}", e))?;
        }

        // Wait for the command to complete and get output
        let output = child
            .wait_with_output()
            .map_err(|e| format!("Failed to read command output: {}", e))?;

        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout).to_string())
        } else {
            Err(format!(
                "Command '{}' failed with exit code {}: {}",
                command,
                output.status.code().unwrap_or(-1),
                String::from_utf8_lossy(&output.stderr)
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Position, Range, Token, TokenKind};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;
    use crate::typechecker::{TypeResult, typecheck};
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    fn compile_modules(modules_source: Vec<(String, String)>) -> Result<Program, String> {
        let mut component_maps = HashMap::new();
        let mut import_maps = HashMap::new();
        let mut module_parameter_types: HashMap<String, HashMap<String, Type>> = HashMap::new();
        let mut module_type_results: HashMap<String, TypeResult> = HashMap::new();

        // Parse and typecheck modules in order
        for (module_name, source_code) in &modules_source {
            let mut errors = Vec::new();
            let tokens = tokenize(source_code, &mut errors);
            let module = parse(module_name.clone(), tokens, &mut errors);

            let type_info = typecheck(&module, &module_type_results, &mut errors);
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
            // Extract parameter types from component info for backward compatibility
            let mut parameter_types = HashMap::new();
            for (name, info) in &type_info.component_info {
                parameter_types.insert(name.clone(), info.parameter_type.clone());
            }
            module_parameter_types.insert(module_name.clone(), parameter_types);
            module_type_results.insert(module_name.clone(), type_info);
        }

        Ok(Program::new(
            component_maps,
            import_maps,
            module_parameter_types,
            HashMap::new(),
            String::new(),
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
        d.push("test_data/runtime.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let data_json = archive
                .get("data.json")
                .expect("Missing 'data.json' section in test case")
                .content
                .trim();
            let expected_output = archive
                .get("output.html")
                .expect("Missing 'output.html' section in test case")
                .content
                .trim();

            println!("Test case {} (line {})", case_num + 1, line_number);

            // Collect all modules from .hop files in order
            let mut modules_source = Vec::new();
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let module_name = file.name.trim_end_matches(".hop");
                    modules_source.push((module_name.to_string(), file.content.trim().to_string()));
                }
            }

            let program = compile_modules(modules_source).unwrap_or_else(|e| {
                panic!(
                    "Compilation failed for test case {} (line {}): {}",
                    case_num + 1,
                    line_number,
                    e
                );
            });

            let data: serde_json::Value = serde_json::from_str(data_json).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse JSON data for test case {} (line {}): {}",
                    case_num + 1,
                    line_number,
                    e
                );
            });

            let actual_output = program
                .execute_simple("main", "main-comp", data)
                .unwrap_or_else(|e| {
                    panic!(
                        "Execution failed for test case {} (line {}): {}",
                        case_num + 1,
                        line_number,
                        e
                    );
                });

            // Normalize whitespace by tokenizing both outputs and comparing tokens
            let mut errors = Vec::new();
            let expected_tokens = normalize_tokens(tokenize(expected_output, &mut errors));
            let actual_tokens = normalize_tokens(tokenize(actual_output.trim(), &mut errors));
            assert!(errors.is_empty());

            assert_eq!(
                actual_tokens,
                expected_tokens,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            if line == "## BEGIN" {
                assert!(
                    !in_case,
                    "Found '## BEGIN' at line {} while already inside a test case",
                    line_number
                );
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(
                    in_case,
                    "Found '## END' at line {} without matching '## BEGIN'",
                    line_number
                );
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(
            !in_case,
            "Reached end of file while inside a test case (missing '## END')"
        );

        test_cases
    }
}
