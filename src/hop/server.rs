use crate::common::{Position, Range, RangeError, Ranged, escape_html, is_void_element};
use crate::dop::{self, evaluate_expr, load_json_file};
use crate::hop::ast::HopAST;
use crate::hop::environment::Environment;
use crate::hop::parser::parse;
use crate::hop::runtime::HopMode;
use crate::hop::script_collector::ScriptCollector;
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::TopoSorter;
use crate::hop::typechecker::{DefinitionLink, TypeAnnotation, typecheck};
use crate::tui::source_annotator::Annotated;
use anyhow::Result;
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

use super::ast::HopNode;
use super::typechecker::ModuleTypeInformation;

#[derive(Debug, Clone, PartialEq)]
pub struct HoverInfo {
    pub type_str: String,
    pub range: Range,
}

impl Ranged for HoverInfo {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for HoverInfo {
    fn message(&self) -> String {
        self.type_str.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionLocation {
    pub module: String,
    pub range: Range,
}

impl Ranged for DefinitionLocation {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for DefinitionLocation {
    fn message(&self) -> String {
        "Definition".to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub message: String,
    pub range: Range,
}

impl Ranged for Diagnostic {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for Diagnostic {
    fn message(&self) -> String {
        self.message.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenameLocation {
    pub module: String,
    pub range: Range,
}

impl Ranged for RenameLocation {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for RenameLocation {
    fn message(&self) -> String {
        "Rename".to_string()
    }
}

impl PartialOrd for RenameLocation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RenameLocation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // First compare by module name
        match self.module.cmp(&other.module) {
            std::cmp::Ordering::Equal => {
                // Then by range start
                match self.range.start.cmp(&other.range.start) {
                    std::cmp::Ordering::Equal => {
                        // Finally by range end
                        self.range.end.cmp(&other.range.end)
                    }
                    other => other,
                }
            }
            other => other,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenameableSymbol {
    pub current_name: String,
    pub range: Range,
}

impl Ranged for RenameableSymbol {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for RenameableSymbol {
    fn message(&self) -> String {
        format!("Renameable symbol: {}", self.current_name)
    }
}

pub struct Server {
    asts: HashMap<String, HopAST>,
    type_information: HashMap<String, ModuleTypeInformation>,
    type_annotations: HashMap<String, Vec<TypeAnnotation>>,
    definition_links: HashMap<String, Vec<DefinitionLink>>,
    parse_errors: HashMap<String, Vec<RangeError>>,
    type_errors: HashMap<String, Vec<RangeError>>,
    source_code: HashMap<String, String>,
    topo_sorter: TopoSorter,
    hop_mode: HopMode,
}

impl Default for Server {
    fn default() -> Self {
        Self::new(HopMode::Dev)
    }
}

impl Server {
    pub fn new(hop_mode: HopMode) -> Self {
        Self {
            asts: HashMap::new(),
            type_information: HashMap::new(),
            type_annotations: HashMap::new(),
            definition_links: HashMap::new(),
            parse_errors: HashMap::new(),
            type_errors: HashMap::new(),
            source_code: HashMap::new(),
            topo_sorter: TopoSorter::default(),
            hop_mode,
        }
    }

    pub fn has_module(&self, module_name: &str) -> bool {
        self.asts.contains_key(module_name)
    }

    fn typecheck_module(&mut self, module_name: &str) {
        let ast = match self.asts.get(module_name) {
            Some(module) => module,
            None => return,
        };

        let type_errors = self.type_errors.entry(module_name.to_string()).or_default();
        let type_annotations = self
            .type_annotations
            .entry(module_name.to_string())
            .or_default();
        let definition_links = self
            .definition_links
            .entry(module_name.to_string())
            .or_default();

        type_errors.clear();
        type_annotations.clear();
        definition_links.clear();

        let component_type_info = typecheck(
            ast,
            &self.type_information,
            type_errors,
            type_annotations,
            definition_links,
        );
        self.type_information
            .insert(module_name.to_string(), component_type_info);
    }

    fn parse_module(&mut self, module_name: &str, source_code: &str) {
        let parse_errors = self
            .parse_errors
            .entry(module_name.to_string())
            .or_default();
        parse_errors.clear();

        let tokenizer = Tokenizer::new(source_code);
        let module = parse(module_name.to_string(), tokenizer, parse_errors);

        self.asts.insert(module_name.to_string(), module);
        self.source_code
            .insert(module_name.to_string(), source_code.to_string());
    }

    pub fn update_module(&mut self, module_name: String, source_code: &str) -> Vec<String> {
        self.parse_module(module_name.as_str(), source_code);

        self.topo_sorter.add_node(module_name.clone());

        let ast = match self.asts.get(&module_name) {
            Some(module) => module,
            None => unreachable!(),
        };

        self.topo_sorter.clear_dependencies(&module_name);
        for import_node in ast.get_import_nodes() {
            self.topo_sorter
                .add_dependency(&module_name, &import_node.from_attr.value);
        }

        let dependent_modules = match self.topo_sorter.sort_subgraph(&module_name) {
            Ok(nodes) => nodes,
            Err(_) => vec![module_name],
        };

        for module_name in &dependent_modules {
            self.typecheck_module(module_name)
        }

        dependent_modules
    }

    pub fn get_hover_info(&self, module_name: &str, position: Position) -> Option<HoverInfo> {
        self.type_annotations
            .get(module_name)?
            .iter()
            .find(|a| a.contains_position(position))
            .map(|annotation| HoverInfo {
                type_str: format!("`{}`: `{}`", annotation.name, annotation.typ),
                range: annotation.range,
            })
    }

    pub fn get_definition_location(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<DefinitionLocation> {
        self.definition_links
            .get(module_name)?
            .iter()
            .find(|link| link.contains_position(position))
            .map(|link| DefinitionLocation {
                module: link.definition_module.clone(),
                range: link.definition_range,
            })
    }

    pub fn get_rename_locations(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<Vec<RenameLocation>> {
        let ast = self.asts.get(module_name)?;
        for component_node in ast.get_component_definition_nodes() {
            let is_on_tag_name = component_node
                .opening_name_range
                .contains_position(position)
                || component_node
                    .closing_name_range
                    .is_some_and(|range| range.contains_position(position));

            if is_on_tag_name {
                return Some(
                    self.collect_component_rename_locations(&component_node.name, module_name),
                );
            }
        }

        let node = ast.find_node_at_position(position)?;

        let is_on_tag_name = node.name_ranges().any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            HopNode::ComponentReference {
                component,
                definition_module: definition_location,
                ..
            } => definition_location.as_ref().map(|target_module| {
                self.collect_component_rename_locations(component, target_module)
            }),
            n @ HopNode::NativeHTML { .. } => Some(
                n.name_ranges()
                    .map(|range| RenameLocation {
                        module: module_name.to_string(),
                        range,
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    /// Returns information about a renameable symbol at the given position.
    /// Checks if the position is on a component name (reference or definition)
    /// and returns the symbol's current name and range if found.
    pub fn get_renameable_symbol(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<RenameableSymbol> {
        let ast = self.asts.get(module_name)?;

        // Check if we're on a component definition
        for component_node in ast.get_component_definition_nodes() {
            if component_node
                .opening_name_range
                .contains_position(position)
            {
                return Some(RenameableSymbol {
                    current_name: component_node.name.clone(),
                    range: component_node.opening_name_range,
                });
            }
            if let Some(closing_range) = component_node.closing_name_range {
                if closing_range.contains_position(position) {
                    return Some(RenameableSymbol {
                        current_name: component_node.name.clone(),
                        range: closing_range,
                    });
                }
            }
        }

        let node = ast.find_node_at_position(position)?;

        let tag_name = node.tag_name()?;

        node.name_ranges()
            .find(|r| r.contains_position(position))
            .map(|range| RenameableSymbol {
                current_name: tag_name.to_string(),
                range,
            })
    }

    /// Collects all locations where a component should be renamed, including:
    /// - The component definition (opening and closing tags)
    /// - All references to the component (opening and closing tags)
    /// - All import statements that import the component
    fn collect_component_rename_locations(
        &self,
        component_name: &str,
        definition_module: &str,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        if let Some(ast) = self.asts.get(definition_module) {
            if let Some(component_node) = ast
                .get_component_definition_nodes()
                .iter()
                .find(|node| node.name == component_name)
            {
                // Add the definition's opening tag name
                locations.push(RenameLocation {
                    module: definition_module.to_string(),
                    range: component_node.opening_name_range,
                });

                // Add the definition's closing tag name if it exists
                if let Some(range) = component_node.closing_name_range {
                    locations.push(RenameLocation {
                        module: definition_module.to_string(),
                        range,
                    });
                }
            }
        }

        for (module_name, ast) in &self.asts {
            // Find all import statements that import this component
            locations.extend(
                ast.get_import_nodes()
                    .iter()
                    .filter(|n| {
                        n.imports_component(component_name) && n.imports_from(definition_module)
                    })
                    .map(|n| RenameLocation {
                        module: module_name.clone(),
                        range: n.component_attr.value_range,
                    }),
            );

            // Find all component references that references this component
            locations.extend(
                ast.iter_all_nodes()
                    .filter(|node| {
                        if let HopNode::ComponentReference {
                            component,
                            definition_module: reference_definition_module,
                            ..
                        } = node
                        {
                            return reference_definition_module
                                .as_ref()
                                .is_some_and(|d| d == definition_module)
                                && component == component_name;
                        }
                        false
                    })
                    .flat_map(|node| node.name_ranges())
                    .map(|range| RenameLocation {
                        module: module_name.clone(),
                        range,
                    }),
            );
        }

        locations
    }

    pub fn get_error_diagnostics(&self, module_name: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let mut found_parse_errors = false;

        for error in self.parse_errors.get(module_name).into_iter().flatten() {
            diagnostics.push(Diagnostic {
                message: error.message.clone(),
                range: error.range,
            });
            found_parse_errors = true;
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            for error in self.type_errors.get(module_name).into_iter().flatten() {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    range: error.range,
                });
            }
        }

        diagnostics
    }

    pub fn get_parse_errors(&self, module_name: &str) -> Vec<RangeError> {
        self.parse_errors
            .get(module_name)
            .cloned()
            .unwrap_or_default()
    }

    pub fn get_type_errors(&self, module_name: &str) -> Vec<RangeError> {
        self.type_errors
            .get(module_name)
            .cloned()
            .unwrap_or_default()
    }

    pub fn get_source_code(&self, module_name: &str) -> Option<String> {
        self.source_code.get(module_name).cloned()
    }

    // Execution methods from Program

    pub fn get_scripts(&self) -> String {
        let mut script_collector = ScriptCollector::new();
        for ast in self.asts.values() {
            script_collector.process_module(ast);
        }
        script_collector.build()
    }

    /// Get all file_attr values from render nodes across all modules
    /// I.e. files specified in <render file="index.html">
    pub fn get_render_file_paths(&self) -> Vec<String> {
        let mut result = Vec::new();
        for ast in self.asts.values() {
            for node in ast.get_render_nodes() {
                result.push(node.file_attr.value.clone())
            }
        }
        result
    }

    /// Render the content for a specific file path
    pub fn render_file(&self, file_path: &str) -> Result<String> {
        // Find the render node with the matching file_attr.value
        for ast in self.asts.values() {
            for node in ast.get_render_nodes() {
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

    pub fn evaluate_component(
        &self,
        module_name: &str,
        component_name: &str,
        args: Vec<serde_json::Value>,
        slot_content: Option<&str>,
        additional_classes: Option<&str>,
    ) -> Result<String> {
        let ast = self
            .asts
            .get(module_name)
            .ok_or_else(|| anyhow::anyhow!("Module '{}' not found", module_name))?;

        let component = ast
            .get_component_definition(component_name)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Component '{}' not found in module '{}'",
                    component_name,
                    module_name
                )
            })?;

        let mut env = Self::init_environment(self.hop_mode);

        // Set up environment with all parameters and their corresponding values
        for (idx, param) in component.params.iter().enumerate() {
            env.push(param.var_name.value.clone(), args[idx].clone());
            // TODO: Check that lengths are correct and use zip
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
            let mut element_type = "div";
            if let Some(as_attr) = &component.as_attr {
                element_type = &as_attr.value;
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
                        match additional_classes {
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
            if let (Some(cls), false) = (additional_classes, added_class) {
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
        slot_content: Option<&str>,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> anyhow::Result<String> {
        match node {
            HopNode::If {
                condition,
                children,
                ..
            } => match dop::evaluate_expr(condition, env)?.as_bool() {
                Some(cond) => {
                    let mut result = String::new();
                    if cond {
                        for child in children {
                            result.push_str(&self.evaluate_node(
                                child,
                                slot_content,
                                env,
                                current_module,
                            )?);
                        }
                    }
                    Ok(result)
                }
                None => anyhow::bail!("Could not evaluate expression to boolean"),
            },

            HopNode::For {
                var_name,
                array_expr,
                children,
                ..
            } => {
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

            HopNode::ComponentReference {
                component,
                args,
                attributes,
                definition_module,
                children,
                ..
            } => {
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(evaluate_expr(&arg.expression, env)?);
                }

                let component_name = component;

                let target_module = definition_module
                    .as_ref()
                    .expect("Could not find definition module for component reference");

                let target_component = self
                    .asts
                    .get(target_module)
                    .and_then(|ast| ast.get_component_definition(component_name))
                    .expect("Could not find target component for component reference");

                // Check if target component has a default slot
                let has_default_slot = target_component.has_slot;

                // Collect slot content if component has a slot and there are children
                let slot_html = if has_default_slot && !children.is_empty() {
                    let mut default_html = String::new();
                    for child in children {
                        default_html.push_str(&self.evaluate_node(
                            child,
                            slot_content,
                            env,
                            current_module,
                        )?);
                    }
                    Some(default_html)
                } else {
                    None
                };

                // Extract class attribute from component reference
                let additional_classes = attributes
                    .iter()
                    .find(|attr| attr.name == "class")
                    .map(|attr| attr.value.as_str());

                self.evaluate_component(
                    target_module,
                    component_name,
                    arg_values,
                    slot_html.as_deref(),
                    additional_classes,
                )
            }

            HopNode::SlotDefinition { .. } => {
                // Use the supplied slot content if available, otherwise return empty
                Ok(slot_content.unwrap_or_default().to_string())
            }

            HopNode::NativeHTML {
                children,
                tag_name,
                attributes,
                set_attributes,
                ..
            } => {
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

            HopNode::Error { .. } => Ok(String::new()),

            HopNode::Text { value, .. } => Ok(value.clone()),

            HopNode::TextExpression { expression, .. } => {
                match evaluate_expr(expression, env)?.as_str() {
                    Some(s) => Ok(escape_html(s)),
                    None => anyhow::bail!("Could not evaluate expression to string"),
                }
            }

            HopNode::Doctype { .. } => Ok("<!DOCTYPE html>".to_string()),

            HopNode::XExec {
                cmd_attr, children, ..
            } => {
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
                Self::execute_command(command, &stdin_content)
            }

            HopNode::XRaw { trim, children, .. } => {
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

            HopNode::XLoadJson {
                file_attr,
                as_attr,
                children,
                ..
            } => {
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
        }
    }

    fn evaluate_node_entrypoint(
        &self,
        node: &HopNode,
        env: &mut Environment<serde_json::Value>,
        current_module: &str,
    ) -> Result<String> {
        match node {
            HopNode::NativeHTML {
                tag_name,
                attributes,
                children,
                set_attributes,
                ..
            } => {
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
                self.evaluate_node(node, None, env, current_module)
            }
        }
    }

    /// execute_command is used by the experimental <hop-x-exec> command
    /// which allows an external program to be executed from the context of a
    /// hop program.
    fn execute_command(command: &str, stdin_content: &str) -> Result<String> {
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

    /// trim_raw_string is used by the experimental <hop-x-raw> node
    /// and allows indentation to be trimmed.
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
    use crate::test_utils::archive::extract_markers_from_archive;
    use crate::tui::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn server_from_archive(archive: &Archive) -> Server {
        let mut server = Server::default();
        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            server.update_module(module_name, &file.content);
        }
        server
    }

    fn check_rename_locations(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));
        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module = marker.filename.replace(".hop", "");

        let mut locs = server_from_archive(&archive)
            .get_rename_locations(&module, marker.position)
            .expect("Expected locations to be defined");

        locs.sort();

        let mut output = Vec::new();

        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");

            let module_locs: Vec<RenameLocation> = locs
                .iter()
                .filter(|l| l.module == module_name)
                .cloned()
                .collect();

            if !module_locs.is_empty() {
                output.push(
                    SourceAnnotator::new()
                        .with_filename(&file.name)
                        .with_location()
                        .add_annotations(&file.content, &module_locs),
                );
            }
        }

        expected.assert_eq(&output.join("\n"));
    }

    fn check_definition_location(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));

        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module = marker.filename.replace(".hop", "");

        let loc = server_from_archive(&archive)
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == loc.module)
            .expect("File not found in archive");

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .add_annotations(&file.content, &[loc]);

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let archive = Archive::from(input);
        let diagnostics = server_from_archive(&archive).get_error_diagnostics(module);

        if diagnostics.is_empty() {
            panic!("Expected diagnostics to be non-empty");
        }

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module)
            .expect("File not found in archive");

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .add_annotations(&file.content, &diagnostics);

        expected.assert_eq(&output);
    }

    fn check_renameable_symbol(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));
        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module = marker.filename.replace(".hop", "");

        let symbol = server_from_archive(&archive)
            .get_renameable_symbol(&module, marker.position)
            .expect("Expected symbol to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module)
            .expect("Could not find file in archive");

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .add_annotations(&file.content, &[symbol]);

        expected.assert_eq(&output);
    }

    fn check_hover_info(archive: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(archive));

        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module = marker.filename.replace(".hop", "");

        let hover_info = server_from_archive(&archive)
            .get_hover_info(&module, marker.position)
            .expect("Expected hover info to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module)
            .expect("Could not find file in archive");

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .add_annotations(&file.content, &[hover_info]);

        expected.assert_eq(&output);
    }

    #[test]
    fn test_get_definition_from_component_reference() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="hop/components" />

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_definition_from_component_reference_closing() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="hop/components" />

                <main-comp>
                  <hello-world>

                  </hello-world>
                     ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_component_reference() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="components" />

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^

                Rename
                  --> components.hop (line 3, col 3)
                3 | </hello-world>
                  |   ^^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | <import component="hello-world" from="components" />
                  |                    ^^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <hello-world />
                  |    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_component_reference_same_component() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^

                Rename
                  --> main.hop (line 3, col 3)
                3 | </hello-world>
                  |   ^^^^^^^^^^^

                Rename
                  --> main.hop (line 6, col 4)
                6 |   <hello-world />
                  |    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_component_definition() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                 ^
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="components" />

                <main-comp>
                  <hello-world />
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^

                Rename
                  --> components.hop (line 3, col 3)
                3 | </hello-world>
                  |   ^^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | <import component="hello-world" from="components" />
                  |                    ^^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <hello-world />
                  |    ^^^^^^^^^^^
            "#]],
        );
    }

    // Make sure that when we rename a component in a module that has
    // the same name as a module in some other component, the module in
    // the other component is left unchanged.
    #[test]
    fn test_get_rename_locations_main_comp_scoped() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                <main-comp>
                 ^
                  <hello-world />
                </main-comp>

                -- main.hop --
                <import component="hello-world" from="components" />

                <main-comp>
                  <hello-world />
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 5, col 2)
                5 | <main-comp>
                  |  ^^^^^^^^^

                Rename
                  --> components.hop (line 7, col 3)
                7 | </main-comp>
                  |   ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_renameable_symbol() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                <hello-world>
                 ^
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            expect![[r#"
                Renameable symbol: hello-world
                  --> main.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_hover_info() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: {name: string}}>
                              ^
                  <h1>Hello {user.name}</h1>
                </main-comp>
            "#},
            expect![[r#"
                `user`: `{name: string}`
                  --> main.hop (line 1, col 13)
                1 | <main-comp {user: {name: string}}>
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_native_html_opening_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <div>
                     ^
                        <span>Content</span>
                    </div>
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 4, col 7)
                4 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_native_html_closing_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <div>
                        <span>Content</span>
                    </div>
                       ^
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 4, col 7)
                4 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_self_closing_html_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <br />
                     ^
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <br />
                  |      ^^
            "#]],
        );
    }

    #[test]
    fn test_get_renameable_symbol_native_html() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <div>Content</div>
                     ^
                </main-comp>
            "#},
            expect![[r#"
                Renameable symbol: div
                  --> main.hop (line 2, col 6)
                2 |     <div>Content</div>
                  |      ^^^
            "#]],
        );
    }

    #[test]
    fn test_get_error_diagnostics_parse_errors() {
        check_error_diagnostics(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div>
                  <span>unclosed span
                </main-comp>
            "#},
            "main",
            expect![[r#"
                Unclosed <span>
                  --> main.hop (line 3, col 3)
                3 |   <span>unclosed span
                  |   ^^^^^^

                Unclosed <div>
                  --> main.hop (line 2, col 3)
                2 |   <div>
                  |   ^^^^^
            "#]],
        );
    }

    // Even when there's parse errors we should be able to rename.
    #[test]
    fn test_rename_with_parse_errors() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  ^
                  <div>
                  <span>unclosed span
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 2)
                1 | <main-comp>
                  |  ^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 3)
                4 | </main-comp>
                  |   ^^^^^^^^^
            "#]],
        );
    }
}
