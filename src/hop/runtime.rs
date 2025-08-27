use crate::common::{escape_html, is_void_element};
use crate::dop;
use crate::dop::{DopType, evaluate_expr, load_json_file};
use crate::hop::ast::{
    ComponentDefinitionNode, ComponentReferenceNode, ErrorNode, ForNode, HopNode, IfNode,
    NativeHTMLNode, RenderNode, SlotDefinitionNode, SlotReferenceNode, XExecNode, XLoadJsonNode,
    XRawNode,
};
use crate::hop::environment::Environment;
use crate::hop::parser::Module;
use crate::hop::typechecker::TypeResult;
use anyhow::Result;
use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::process::{Command, Stdio};

/// HopMode influences the runtime value of the global variable HOP_MODE which
/// will be set to 'build' when running `hop build` and 'dev' when running
/// `hop dev`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HopMode {
    Build,
    Dev,
}

impl HopMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            HopMode::Build => "build",
            HopMode::Dev => "dev",
        }
    }
}

/// Program represents a compiled hop program that can execute components and entrypoints
#[derive(Clone)]
pub struct Program {
    component_maps: HashMap<String, HashMap<String, ComponentDefinitionNode>>,
    import_maps: HashMap<String, HashMap<String, String>>,
    parameter_types: HashMap<String, HashMap<String, BTreeMap<String, DopType>>>,
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
                    module_parameter_types.insert(name.clone(), info.parameter_types.clone());
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
    pub fn get_parameter_types(
        &self,
    ) -> &HashMap<String, HashMap<String, BTreeMap<String, DopType>>> {
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
                    let mut env = Self::init_environment(self.hop_mode);
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
    fn init_environment(hop_mode: HopMode) -> Environment<serde_json::Value> {
        let mut env = Environment::new();
        env.push(
            "HOP_MODE".to_string(),
            serde_json::Value::String(hop_mode.as_str().to_string()),
        );
        env
    }

    pub fn execute_simple(
        &self,
        module_name: &str,
        component_name: &str,
        params: std::collections::BTreeMap<String, serde_json::Value>,
    ) -> Result<String> {
        let empty_slots = HashMap::new();
        self.evaluate_component(module_name, component_name, params, &empty_slots, None)
    }

    pub fn execute_preview(&self, module_name: &str, component_name: &str) -> Result<String> {
        let component_map = self
            .component_maps
            .get(module_name)
            .ok_or_else(|| anyhow::anyhow!("Module '{}' not found", module_name))?;

        let component = component_map
            .get(component_name)
            .ok_or_else(|| anyhow::anyhow!("Component '{}' not found", component_name))?;

        let preview = component
            .preview
            .clone()
            .ok_or_else(|| anyhow::anyhow!("No preview for component '{}'", component_name))?;

        let mut result = String::new();
        let mut env = Self::init_environment(self.hop_mode);

        // Render each node in the preview content
        for node in preview {
            let rendered = self.evaluate_node(&node, &HashMap::new(), &mut env, module_name)?;
            result.push_str(&rendered);
        }

        Ok(result)
    }

    pub fn evaluate_component(
        &self,
        module_name: &str,
        component_name: &str,
        params: std::collections::BTreeMap<String, serde_json::Value>,
        slot_content: &HashMap<String, String>,
        additional_class: Option<&str>,
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

        let mut env = Self::init_environment(self.hop_mode);
        // Set up environment with all parameters and their corresponding values
        for param in &component.params {
            let param_name = &param.var_name.value;
            let value = params
                .get(param_name)
                .cloned()
                .unwrap_or_else(|| panic!("Missing parameter {param_name}"));
            env.push(param.var_name.value.clone(), value);
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

            let mut added_class = false;

            for attr in &component.attributes {
                if attr.name != "name"
                    && attr.name != "params-as"
                    && attr.name != "as"
                    && attr.name != "entrypoint"
                {
                    if attr.name == "class" {
                        added_class = true;
                        match additional_class {
                            None => result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value)),
                            Some(cls) => result
                                .push_str(&format!(" {}=\"{} {}\"", attr.name, attr.value, cls)),
                        }
                    } else {
                        result.push_str(&format!(" {}=\"{}\"", attr.name, attr.value));
                    }
                }
            }

            // If component doesn't have a class attribute but the reference does, add it
            if let (Some(cls), false) = (additional_class, added_class) {
                result.push_str(&format!(" class=\"{}\"", cls))
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
        node: &HopNode,
        slot_content: &HashMap<String, String>,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String> {
        match node {
            HopNode::If(IfNode {
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
            HopNode::ComponentReference(ComponentReferenceNode {
                component,
                args,
                attributes,
                children,
                ..
            }) => {
                let mut arg_values = std::collections::BTreeMap::new();
                for arg in args {
                    arg_values.insert(arg.name.clone(), evaluate_expr(&arg.expression, env)?);
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
                    if let HopNode::SlotReference(SlotReferenceNode { name, children, .. }) = child
                    {
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

                // Extract class attribute from component reference
                let additional_class = attributes
                    .iter()
                    .find(|attr| attr.name == "class")
                    .map(|attr| attr.value.as_str());

                self.evaluate_component(
                    &target_module,
                    component_name,
                    arg_values,
                    &new_slot_content,
                    additional_class,
                )
            }
            HopNode::NativeHTML(NativeHTMLNode {
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
            HopNode::Error(ErrorNode { children, .. }) => {
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
            HopNode::Text(text_node) => Ok(text_node.value.clone()),
            HopNode::TextExpression(text_expr_node) => {
                let result = evaluate_expr(&text_expr_node.expression, env)?;
                Ok(escape_html(result.as_str().unwrap_or("")))
            }
            HopNode::Doctype(_) => Ok("<!DOCTYPE html>".to_string()),
            HopNode::SlotDefinition(SlotDefinitionNode { name, children, .. }) => {
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
            HopNode::SlotReference(SlotReferenceNode { children, .. }) => {
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
            HopNode::XExec(XExecNode {
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
            HopNode::XRaw(XRawNode { trim, children, .. }) => {
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
            HopNode::XLoadJson(XLoadJsonNode {
                file_attr,
                as_attr,
                children,
                ..
            }) => {
                // Load JSON data from file
                let file_path = &file_attr.value;
                let var_name = &as_attr.value;

                let json_value = match load_json_file(file_path) {
                    Ok(value) => value,
                    Err(err) => {
                        return Err(anyhow::anyhow!(
                            "Failed to load JSON file '{}': {}",
                            file_path,
                            err
                        ));
                    }
                };

                // Push the JSON data variable into scope
                env.push(var_name.clone(), json_value);

                // Evaluate children with the JSON data in scope
                let mut result = String::new();
                for child in children {
                    result.push_str(&self.evaluate_node(
                        child,
                        slot_content,
                        env,
                        current_module,
                    )?);
                }

                // Pop the JSON variable from scope
                env.pop();

                Ok(result)
            }
            HopNode::For(ForNode {
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

    fn evaluate_node_entrypoint(
        &self,
        node: &HopNode,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String> {
        match node {
            HopNode::NativeHTML(NativeHTMLNode {
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
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::typechecker::{TypeResult, typecheck};
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn check(archive_str: &str, data_json: &str, expected: Expect) {
        let archive = Archive::from(archive_str);

        // Collect all modules from .hop files in order
        let mut modules_source = Vec::new();
        for file in archive.iter() {
            if file.name.ends_with(".hop") {
                let module_name = file.name.trim_end_matches(".hop");
                modules_source.push((module_name.to_string(), file.content.trim().to_string()));
            }
        }

        // Parse and typecheck modules
        let mut parsed_modules = HashMap::new();
        let mut module_type_results: HashMap<String, TypeResult> = HashMap::new();

        for (module_name, source_code) in &modules_source {
            let mut errors = Vec::new();
            let tokenizer = Tokenizer::new(source_code);
            let module = parse(module_name.clone(), tokenizer, &mut errors);

            let type_info = typecheck(&module, &module_type_results, &mut errors);
            if !errors.is_empty() {
                panic!("Errors in {}: {:?}", module_name, errors);
            }

            parsed_modules.insert(module_name.clone(), module);
            module_type_results.insert(module_name.clone(), type_info);
        }

        let program = Program::new(
            parsed_modules,
            module_type_results,
            String::new(),
            HopMode::Build,
        );

        let data: serde_json::Value =
            serde_json::from_str(data_json).expect("Failed to parse JSON data");

        // Convert JSON object fields to individual named parameters
        let mut params = std::collections::BTreeMap::new();
        if let serde_json::Value::Object(obj) = data {
            for (key, value) in obj {
                params.insert(key, value);
            }
        }

        let actual_output = program
            .execute_simple("main", "main-comp", params)
            .expect("Execution failed");

        // Normalize output by removing lines that contain only whitespace
        let normalized_output = actual_output
            .lines()
            .filter(|line| !line.trim().is_empty())
            .collect::<Vec<_>>()
            .join("\n")
            + "\n";

        expected.assert_eq(&normalized_output);
    }

    #[test]
    fn test_basic_component() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp class="p-2">
                    <div>hello</div>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp" class="p-2">
                    <div>hello</div>
                </div>
            "#]],
        );
    }

    /// Expressions should be safe against XSS.
    #[test]
    fn test_xss_protection() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {p: string}>
                    <div>{p}</div>
                </main-comp>
            "#},
            r#"{"p": "<script>alert(1);</script>"}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>&lt;script&gt;alert(1);&lt;/script&gt;</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_conditional_rendering() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {
                    user: {name: string},
                    admin: {name: string},
                    status: string,
                    expected_status: string,
                }>
                  <if {user.name == admin.name}>
                    <div>Is Admin</div>
                  </if>
                  <if {status == expected_status}>
                    <div>Status OK</div>
                  </if>
                </main-comp>
            "#},
            r#"{
              "user": {"name": "alice"},
              "admin": {"name": "alice"},
              "status": "active",
              "expected_status": "active"
            }"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Is Admin</div>
                    <div>Status OK</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_for_loop_with_conditionals() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: array[{show: boolean, data: string}]}>
                    <for {item in items}>
                        <if {item.show}>
                            <div>{item.data}</div>
                        </if>
                    </for>
                </main-comp>
            "#},
            r#"{"items": [
            	{"show": true, "data": "foo"},
            	{"show": false, "data": "bar"},
            	{"show": true, "data": "baz"}
            ]}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                            <div>foo</div>
                            <div>baz</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_for_loop_with_type_conditionals() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: array[{name: string, type: string}]}>
                  <for {item in items}>
                    <div>
                      <span>{item.name}</span>
                      <if {item.type == 'admin'}>
                        <strong> [Admin]</strong>
                      </if>
                      <if {item.type == 'user'}>
                        <em> [User]</em>
                      </if>
                    </div>
                  </for>
                </main-comp>
            "#},
            r#"{"items": [
              {
                "name": "Alice",
                "type": "admin"
              },
              {
                "name": "Bob", 
                "type": "user"
              },
              {
                "name": "Carol",
                "type": "admin"
              }
            ]}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>
                      <span>Alice</span>
                        <strong> [Admin]</strong>
                    </div>
                    <div>
                      <span>Bob</span>
                        <em> [User]</em>
                    </div>
                    <div>
                      <span>Carol</span>
                        <strong> [Admin]</strong>
                    </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_component_imports() {
        check(
            indoc! {r#"
                -- test.hop --
                <button-comp as="button">
                	<span>button</span>
                </button-comp>
                -- main.hop --
                <import from="test" component="button-comp">

                <main-comp>
                  <button-comp></button-comp>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                  <button data-hop-id="test/button-comp">
                	<span>button</span>
                </button>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {p: string}>
                	<div>{p}</div>
                </main-comp>
            "#},
            r#"{"p": "foo bar"}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	<div>foo bar</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_simple_for_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {users: array[{name: string}]}>
                  <for {user in users}>
                    <div>{user.name}</div>
                  </for>
                </main-comp>
            "#},
            r#"{
              "users": [
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"}
              ]
            }"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Alice</div>
                    <div>Bob</div>
                    <div>Charlie</div>
                </div>
            "#]],
        );
    }

    /// Test HOP_MODE global variable is available and displays correctly.
    #[test]
    fn test_hop_mode_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div>Mode: {HOP_MODE}</div>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                  <div>Mode: build</div>
                </div>
            "#]],
        );
    }

    /// Test HOP_MODE can be used in conditions.
    #[test]
    fn test_hop_mode_condition() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <if {HOP_MODE == 'build'}>
                    <div>Build mode active</div>
                  </if>
                  <if {HOP_MODE == 'dev'}>
                    <div>Dev mode active</div>
                  </if>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Build mode active</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_nested_slots() {
        check(
            indoc! {r#"
                -- main.hop --
                <foo-comp>
                    <div class="foo">
                        <h1><slot-title /></h1>
                        <p><slot-content /></p>
                    </div>
                </foo-comp>

                <bar-comp>
                    <foo-comp>
                        <with-title><slot-title/></with-title>
                        <with-content><slot-content/></with-content>
                    </foo-comp>
                </bar-comp>

                <main-comp entrypoint>
                    <bar-comp>
                        <with-title>Bar Title</with-title>
                        <with-content>Bar Content</with-content>
                    </bar-comp>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                    <div data-hop-id="main/bar-comp">
                    <div data-hop-id="main/foo-comp">
                    <div class="foo">
                        <h1>Bar Title</h1>
                        <p>Bar Content</p>
                    </div>
                </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_set_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {
                    profile_url: string,
                    name: string,
                }>
                  <a set-href="profile_url" set-title="name">Click here</a>
                </main-comp>
            "#},
            r#"{"profile_url": "https://example.com/user/123", "name": "John Doe"}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                  <a href="https://example.com/user/123" title="John Doe">Click here</a>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_slot_with_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-card>
                    <div class="card">
                        <slot-content></slot-content>
                    </div>
                </main-card>

                <main-comp entrypoint {title: string, message: string}>
                    <main-card>
                        <with-content>
                            <h1>{title}</h1>
                            <p>{message}</p>
                        </with-content>
                    </main-card>
                </main-comp>
            "#},
            r#"{
                "title": "Hello World",
                "message": "This text comes from outside params"
            }"#,
            expect![[r#"
                    <div data-hop-id="main/main-card">
                    <div class="card">
                            <h1>Hello World</h1>
                            <p>This text comes from outside params</p>
                    </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_default_slot() {
        check(
            indoc! {r#"
                -- main.hop --
                <my-button>
                	<slot-default>Default Button</slot-default>
                </my-button>

                <main-comp entrypoint>
                	<my-button>Click me!</my-button>
                	<my-button><with-default>Custom Button</with-default></my-button>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                	<div data-hop-id="main/my-button">
                	Click me!
                </div>
                	<div data-hop-id="main/my-button">
                	Custom Button
                </div>
            "#]],
        );
    }

    #[test]
    fn test_complex_layout() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-layout>
                    <div class="page">
                        <header>
                            <slot-title></slot-title>
                        </header>
                        <main>
                            <slot-content></slot-content>
                        </main>
                    </div>
                </main-layout>

                <main-comp entrypoint>
                    <main-layout>
                        <with-title>My Custom Title</with-title>
                        <with-content>
                            <p>This is custom content</p>
                            <p>With multiple paragraphs</p>
                        </with-content>
                    </main-layout>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                    <div data-hop-id="main/main-layout">
                    <div class="page">
                        <header>
                            My Custom Title
                        </header>
                        <main>
                            <p>This is custom content</p>
                            <p>With multiple paragraphs</p>
                        </main>
                    </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_deeply_nested_slots() {
        check(
            indoc! {r#"
                -- main.hop --
                <page-layout>
                    <div class="page">
                        <slot-content></slot-content>
                    </div>
                </page-layout>

                <modal-wrapper>
                    <div class="modal-container">
                        <slot-modal></slot-modal>
                    </div>
                </modal-wrapper>

                <modal-comp>
                    <div class="modal">
                        <div class="modal-header">
                            <slot-header></slot-header>
                        </div>
                        <div class="modal-body">
                            <slot-body></slot-body>
                        </div>
                    </div>
                </modal-comp>

                <main-comp entrypoint {page_title: string}>
                    <page-layout>
                        <with-content>
                            <h1>{page_title}</h1>
                            <modal-wrapper>
                                <with-modal>
                                    <modal-comp>
                                        <with-header>
                						  	<span>header</span>
                                        </with-header>
                                        <with-body>
                                            <span>body</span>
                                        </with-body>
                                    </modal-comp>
                                </with-modal>
                            </modal-wrapper>
                        </with-content>
                    </page-layout>
                </main-comp>
            "#},
            r#"{
                "page_title": "Welcome Page"
            }"#,
            expect![[r#"
                    <div data-hop-id="main/page-layout">
                    <div class="page">
                            <h1>Welcome Page</h1>
                            <div data-hop-id="main/modal-wrapper">
                    <div class="modal-container">
                                    <div data-hop-id="main/modal-comp">
                    <div class="modal">
                        <div class="modal-header">
                						  	<span>header</span>
                        </div>
                        <div class="modal-body">
                                            <span>body</span>
                        </div>
                    </div>
                </div>
                    </div>
                </div>
                    </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_complex_user_management() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-card>
                    <div class="card">
                        <slot-header></slot-header>
                        <div class="body">
                            <slot-content></slot-content>
                        </div>
                        <slot-footer></slot-footer>
                    </div>
                </main-card>

                <main-comp entrypoint {
                    title: string,
                    active_count: string,
                    users: array[{name: string, active: boolean, admin: boolean}],
                }>
                    <main-card>
                        <with-header>
                            <h1>{title}</h1>
                        </with-header>
                        <with-content>
                            <for {user in users}>
                                <if {user.active}>
                                    <div class="user-item">
                                        <span>{user.name}</span>
                                        <if {user.admin}>
                                            <strong> (Admin)</strong>
                                        </if>
                                    </div>
                                </if>
                            </for>
                        </with-content>
                        <with-footer>
                            <p>Total active users: <span>{active_count}</span></p>
                        </with-footer>
                    </main-card>
                </main-comp>
            "#},
            r#"{
                "title": "User Management",
                "active_count": "3",
                "users": [
                    {"name": "Alice", "active": true, "admin": true},
                    {"name": "Bob", "active": false, "admin": false},
                    {"name": "Charlie", "active": true, "admin": false},
                    {"name": "Diana", "active": true, "admin": true},
                    {"name": "Eve", "active": false, "admin": false}
                ]
            }"#,
            expect![[r#"
                    <div data-hop-id="main/main-card">
                    <div class="card">
                            <h1>User Management</h1>
                        <div class="body">
                                    <div class="user-item">
                                        <span>Alice</span>
                                            <strong> (Admin)</strong>
                                    </div>
                                    <div class="user-item">
                                        <span>Charlie</span>
                                    </div>
                                    <div class="user-item">
                                        <span>Diana</span>
                                            <strong> (Admin)</strong>
                                    </div>
                        </div>
                            <p>Total active users: <span>3</span></p>
                    </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_simple_wrapper() {
        check(
            indoc! {r#"
                -- main.hop --
                <wrapper-comp>
                    <div>
                        <slot-content></slot-content>
                    </div>
                </wrapper-comp>

                <main-comp entrypoint>
                    <wrapper-comp>
                        <with-content>Custom</with-content>
                    </wrapper-comp>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                    <div data-hop-id="main/wrapper-comp">
                    <div>
                        Custom
                    </div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_multiple_conditions() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {role: string, status: string}>
                  <if {role == 'admin'}>
                    <div>Admin Access</div>
                  </if>
                  <if {status == 'active'}>
                    <div>Active User</div>
                  </if>
                  <if {status == 'inactive'}>
                    <div>Inactive User</div>
                  </if>
                </main-comp>
            "#},
            r#"{
              "role": "admin",
              "status": "active"
            }"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Admin Access</div>
                    <div>Active User</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_x_raw_simple() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <hop-x-raw>foo bar</hop-x-raw>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    foo bar
                </div>
            "#]],
        );
    }

    #[test]
    fn test_x_raw_html() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<hop-x-raw>
                		<div>some html</div>
                		<p>more content</p>
                	</hop-x-raw>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                		<div>some html</div>
                		<p>more content</p>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_x_raw_trimmed() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<hop-x-raw trim>  trimmed  </hop-x-raw>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	trimmed  
                </div>
            "#]],
        );
    }

    #[test]
    fn test_x_raw_not_trimmed() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<hop-x-raw>  not trimmed  </hop-x-raw>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	  not trimmed  
                </div>
            "#]],
        );
    }

    #[test]
    fn test_object_property_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: {isActive: boolean, role: string}}>
                  <if {user.isActive}>
                    <div>Welcome active user!</div>
                  </if>
                  <if {user.role == 'admin'}>
                    <div>Admin panel access</div>
                  </if>
                </main-comp>
            "#},
            r#"{"user": {
              "isActive": true,
              "role": "admin"
            }}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Welcome active user!</div>
                    <div>Admin panel access</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_nested_conditions() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {config: {enabled: boolean, debug: boolean}}>
                  <if {config.enabled}>
                    <div>Feature is enabled</div>
                    <if {config.debug}>
                      <div>Debug mode on</div>
                    </if>
                  </if>
                </main-comp>
            "#},
            r#"{"config": {
              "enabled": true,
              "debug": false
            }}"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Feature is enabled</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_false_condition() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {visible: boolean}>
                  <if {visible}>
                    <div>Content is visible</div>
                  </if>
                </main-comp>
            "#},
            r#"{
              "visible": false
            }"#,
            expect![[r#"
                <div data-hop-id="main/main-comp">
                </div>
            "#]],
        );
    }

    /// Test class merging when component reference has class attribute and
    /// component definition has class attribute.
    #[test]
    fn test_class_merging_both_have_class() {
        check(
            indoc! {r#"
                -- main.hop --
                <my-button class="bg-red-500">Click me</my-button>

                <main-comp>
                	<my-button class="px-4 py-2 rounded"/>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	<div data-hop-id="main/my-button" class="bg-red-500 px-4 py-2 rounded">Click me</div>
                </div>
            "#]],
        );
    }

    /// Test class merging when component reference has class attribute but
    /// component definition doesn't have class attribute.
    #[test]
    fn test_class_merging_reference_only() {
        check(
            indoc! {r#"
                -- main.hop --
                <my-button>Click me</my-button>

                <main-comp>
                	<my-button class="px-4 py-2 rounded"/>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	<div data-hop-id="main/my-button" class="px-4 py-2 rounded">Click me</div>
                </div>
            "#]],
        );
    }

    /// Test class merging when component definition has class attribute but
    /// component reference doesn't.
    #[test]
    fn test_class_merging_definition_only() {
        check(
            indoc! {r#"
                -- main.hop --
                <my-button class="px-4 py-2">Click me</my-button>

                <main-comp>
                	<my-button/>
                </main-comp>
            "#},
            "{}",
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	<div data-hop-id="main/my-button" class="px-4 py-2">Click me</div>
                </div>
            "#]],
        );
    }
}
