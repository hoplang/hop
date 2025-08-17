use crate::common::{
    ComponentDefinitionNode, ComponentReferenceNode, Environment, ErrorNode, ForNode, HopMode,
    IfNode, NativeHTMLNode, Node, RenderNode, SlotDefinitionNode, SlotReferenceNode, XExecNode,
    XRawNode, escape_html, is_void_element,
};
use crate::dop;
use crate::dop::{DopType, evaluate_expr};
use crate::parser::Module;
use crate::typechecker::TypeResult;
use anyhow::Result;
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

/// Program represents a compiled hop program that can execute components and entrypoints
#[derive(Clone)]
pub struct Program {
    component_maps: HashMap<String, HashMap<String, ComponentDefinitionNode>>,
    import_maps: HashMap<String, HashMap<String, String>>,
    parameter_types: HashMap<String, HashMap<String, DopType>>,
    render_nodes: HashMap<String, Vec<RenderNode>>,
    scripts: String,
    hop_mode: HopMode,
}

impl Program {
    pub fn new(
        modules: HashMap<String, Module>,
        type_results: HashMap<String, TypeResult>,
        scripts: String,
        hop_mode: HopMode,
    ) -> Self {
        let mut component_maps = HashMap::new();
        let mut import_maps = HashMap::new();
        let mut parameter_types = HashMap::new();
        let mut render_nodes = HashMap::new();

        for (module_name, module) in modules {
            // Build component map
            let mut component_map = HashMap::new();
            for comp_node in &module.components {
                component_map.insert(comp_node.name.clone(), comp_node.clone());
            }

            // Build import map
            let mut import_map = HashMap::new();
            for import_node in &module.imports {
                import_map.insert(
                    import_node.component_attr.value.clone(),
                    import_node.from_attr.value.clone(),
                );
            }

            // Store render nodes if any
            if !module.renders.is_empty() {
                render_nodes.insert(module_name.clone(), module.renders.clone());
            }

            // Extract parameter types from type results
            if let Some(type_info) = type_results.get(&module_name) {
                let mut module_parameter_types = HashMap::new();
                for (name, info) in &type_info.component_info {
                    module_parameter_types.insert(name.clone(), info.parameter_type.clone());
                }
                parameter_types.insert(module_name.clone(), module_parameter_types);
            }

            component_maps.insert(module_name.clone(), component_map);
            import_maps.insert(module_name.clone(), import_map);
        }

        Program {
            component_maps,
            import_maps,
            parameter_types,
            render_nodes,
            scripts,
            hop_mode,
        }
    }

    /// Get the collected scripts from the program
    pub fn get_scripts(&self) -> &str {
        &self.scripts
    }

    /// Get the component maps for inspection
    pub fn get_component_maps(&self) -> &HashMap<String, HashMap<String, ComponentDefinitionNode>> {
        &self.component_maps
    }

    /// Get the parameter types for inspection
    pub fn get_parameter_types(&self) -> &HashMap<String, HashMap<String, DopType>> {
        &self.parameter_types
    }

    /// Get all file_attr values from render nodes across all modules
    /// I.e. files specified in <render file="index.html">
    pub fn get_render_file_paths(&self) -> Vec<String> {
        self.render_nodes
            .values()
            .flatten()
            .map(|render_node| render_node.file_attr.value.clone())
            .collect()
    }

    /// Render the content for a specific file path
    pub fn render_file(&self, file_path: &str) -> Result<String> {
        // Find the render node with the matching file_attr.value
        for render_nodes in self.render_nodes.values() {
            for node in render_nodes {
                if node.file_attr.value == file_path {
                    let mut env = self.init_environment();
                    let mut content = String::new();
                    for child in &node.children {
                        let rendered = self.evaluate_node_entrypoint(child, &mut env, "build")?;
                        content.push_str(&rendered);
                    }
                    return Ok(content);
                }
            }
        }
        Err(anyhow::anyhow!(
            "File path '{}' not found in render nodes",
            file_path
        ))
    }

    /// Initialize an environment with global variables like HOP_MODE
    fn init_environment(&self) -> Environment<serde_json::Value> {
        let mut env = Environment::new();
        env.push(
            "HOP_MODE".to_string(),
            serde_json::Value::String(self.hop_mode.as_str().to_string()),
        );
        env
    }

    pub fn execute_simple(
        &self,
        module_name: &str,
        component_name: &str,
        params: serde_json::Value,
    ) -> Result<String> {
        let empty_slots = HashMap::new();
        self.execute(module_name, component_name, params, &empty_slots)
    }

    pub fn execute_preview(
        &self,
        module_name: &str,
        component_name: &str,
        params: serde_json::Value,
    ) -> Result<String> {
        let component_map = self
            .component_maps
            .get(module_name)
            .ok_or_else(|| anyhow::anyhow!("Module '{}' not found", module_name))?;

        let component = component_map
            .get(component_name)
            .ok_or_else(|| anyhow::anyhow!("Component '{}' not found", component_name))?;

        // Use preview content if available, otherwise fall back to regular content
        let content_to_render = match &component.preview {
            Some(preview_nodes) => preview_nodes,
            None => &component.children,
        };

        let mut result = String::new();
        let mut env = self.init_environment();

        // Set up environment with parameters if the component has params
        if let Some(params_as_attr) = &component.params_as_attr {
            env.push(params_as_attr.var_name.value.clone(), params);
        }

        // Render each node in the preview content
        let empty_slots = HashMap::new();
        for node in content_to_render {
            let rendered = self.evaluate_node(node, &empty_slots, &mut env, module_name)?;
            result.push_str(&rendered);
        }

        Ok(result)
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
    ) -> Result<String> {
        let component_map = self
            .component_maps
            .get(module_name)
            .ok_or_else(|| anyhow::anyhow!("Module '{}' not found", module_name))?;

        let component = component_map.get(component_name).ok_or_else(|| {
            anyhow::anyhow!(
                "Component '{}' not found in module '{}'",
                component_name,
                module_name
            )
        })?;

        let mut env = self.init_environment();
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
    ) -> Result<String> {
        match node {
            Node::If(IfNode {
                condition,
                children,
                ..
            }) => {
                let condition_value = dop::evaluate_expr(condition, env)?;
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
                    params_value = evaluate_expr(&attr.expression, env)?;
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

                let mut result = format!("<{}", tag_name);
                for attr in attributes {
                    if !attr.name.starts_with("set-") {
                        result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                    }
                }

                // Evaluate and add set-* attributes
                for set_attr in set_attributes {
                    let attr_name = &set_attr.name[4..]; // Remove "set-" prefix
                    let evaluated = evaluate_expr(&set_attr.expression, env)?;
                    result.push_str(&format!(
                        " {}=\"{}\"",
                        attr_name,
                        escape_html(evaluated.as_str().unwrap())
                    ));
                }

                result.push('>');

                if !is_void_element(tag_name) {
                    for child in children {
                        result.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
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
            Node::TextExpression(text_expr_node) => {
                let result = evaluate_expr(&text_expr_node.expression, env)?;
                Ok(escape_html(result.as_str().unwrap_or("")))
            }
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
            Node::XRaw(XRawNode { trim, children, .. }) => {
                // For hop-x-raw nodes, just render the inner content without the tags
                let mut result = String::new();
                for child in children {
                    result.push_str(&self.evaluate_node(
                        child,
                        slot_content,
                        env,
                        current_module,
                    )?);
                }

                if *trim {
                    Ok(Self::trim_raw_string(&result))
                } else {
                    Ok(result)
                }
            }
            Node::For(ForNode {
                var_name,
                array_expr,
                children,
                ..
            }) => {
                let array_value = evaluate_expr(array_expr, env)?;

                let array = array_value
                    .as_array()
                    .ok_or_else(|| anyhow::anyhow!("For loop expects an array"))?;

                let mut result = String::new();
                for item in array {
                    env.push(var_name.0.value.clone(), item.clone());
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

    fn evaluate_node_entrypoint(
        &self,
        node: &Node,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String> {
        match node {
            Node::NativeHTML(NativeHTMLNode {
                tag_name,
                attributes,
                children,
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
                    let evaluated = evaluate_expr(&set_attr.expression, env)?;
                    result.push_str(&format!(
                        " {}=\"{}\"",
                        attr_name,
                        escape_html(evaluated.as_str().unwrap())
                    ));
                }

                result.push('>');

                if !is_void_element(tag_name) {
                    for child in children {
                        result.push_str(&self.evaluate_node_entrypoint(
                            child,
                            env,
                            current_module,
                        )?);
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

    /// execute_command is used by the experimental <hop-x-exec> command
    /// which allows an external program to be executed from the context of a
    /// hop program.
    fn execute_command(&self, command: &str, stdin_content: &str) -> Result<String> {
        // Parse the command and arguments
        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return Err(anyhow::anyhow!("Empty command"));
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
            .map_err(|e| anyhow::anyhow!("Failed to execute command '{}': {}", command, e))?;

        // Write stdin content to the child process
        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(stdin_content.as_bytes())
                .map_err(|e| anyhow::anyhow!("Failed to write to stdin: {}", e))?;
        }

        // Wait for the command to complete and get output
        let output = child
            .wait_with_output()
            .map_err(|e| anyhow::anyhow!("Failed to read command output: {}", e))?;

        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout).to_string())
        } else {
            Err(anyhow::anyhow!(
                "Command '{}' failed with exit code {}: {}",
                command,
                output.status.code().unwrap_or(-1),
                String::from_utf8_lossy(&output.stderr)
            ))
        }
    }

    fn trim_raw_string(input: &str) -> String {
        let lines: Vec<&str> = input.lines().collect();
        if lines.is_empty() {
            return String::new();
        }

        // Find the first line with non-whitespace content
        let first_content_line_index = lines.iter().position(|line| !line.trim().is_empty());

        let first_content_line_index = match first_content_line_index {
            Some(index) => index,
            None => return String::new(), // All lines are whitespace-only
        };

        // Find the last line with non-whitespace content
        let last_content_line_index = lines
            .iter()
            .rposition(|line| !line.trim().is_empty())
            .unwrap();

        // Get the lines from first content to last content
        let content_lines = &lines[first_content_line_index..=last_content_line_index];

        if content_lines.is_empty() {
            return String::new();
        }

        // Determine the common leading whitespace from the first content line
        let first_line = content_lines[0];
        let leading_whitespace_count = first_line.len()
            - first_line
                .trim_start_matches(|c: char| c.is_whitespace())
                .len();

        // Remove the common leading whitespace from all lines
        let mut result_lines = Vec::new();
        for line in content_lines {
            if line.trim().is_empty() {
                // For whitespace-only lines, just add an empty line
                result_lines.push("");
            } else if line.len() >= leading_whitespace_count
                && line
                    .chars()
                    .take(leading_whitespace_count)
                    .all(|c| c.is_whitespace())
            {
                // Remove the common leading whitespace
                result_lines.push(&line[leading_whitespace_count..]);
            } else {
                // Line has less whitespace than expected, just trim what we can
                result_lines.push(line.trim_start_matches(|c: char| c.is_whitespace()));
            }
        }

        result_lines.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Range;
    use crate::parser::parse;
    use crate::tokenizer::Token;
    use crate::tokenizer::Tokenizer;
    use crate::typechecker::{TypeResult, typecheck};
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    fn compile_modules(modules_source: Vec<(String, String)>) -> Result<Program> {
        let mut parsed_modules = HashMap::new();
        let mut module_type_results: HashMap<String, TypeResult> = HashMap::new();

        // Parse and typecheck modules in order
        for (module_name, source_code) in &modules_source {
            let mut errors = Vec::new();
            let tokenizer = Tokenizer::new(source_code);
            let module = parse(module_name.clone(), tokenizer, &mut errors);

            let type_info = typecheck(&module, &module_type_results, &mut errors);
            if !errors.is_empty() {
                return Err(anyhow::anyhow!("Errors in {}: {:?}", module_name, errors));
            }

            parsed_modules.insert(module_name.clone(), module);
            module_type_results.insert(module_name.clone(), type_info);
        }

        Ok(Program::new(
            parsed_modules,
            module_type_results,
            String::new(),
            HopMode::Build, // Default to build mode for tests
        ))
    }

    fn normalize_tokens(tokenizer: Tokenizer) -> Vec<Token> {
        tokenizer
            .into_iter()
            .map(|r| r.unwrap())
            .map(|(t, _)| match t {
                Token::Text { value } => {
                    let normalized_value = if value.trim().is_empty() {
                        " ".to_string()
                    } else {
                        value
                    };
                    Token::Text {
                        value: normalized_value,
                    }
                }
                Token::StartTag {
                    attributes,
                    expression,
                    self_closing,
                    value,
                    ..
                } => {
                    let mut normalized_attrs = attributes;
                    normalized_attrs.iter_mut().for_each(|attr| {
                        attr.range = Range::default();
                    });
                    normalized_attrs.sort_by(|a, b| a.name.cmp(&b.name));
                    Token::StartTag {
                        attributes: normalized_attrs,
                        expression,
                        value,
                        self_closing,
                    }
                }
                _ => t,
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
            let expected_tokens = normalize_tokens(Tokenizer::new(expected_output));
            let actual_tokens = normalize_tokens(Tokenizer::new(actual_output.trim()));

            assert_eq!(
                actual_tokens,
                expected_tokens,
                "Mismatch in test case {} (line {}), left = actual, right = expected",
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
