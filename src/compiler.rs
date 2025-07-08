use crate::common::{
    escape_html, format_range_errors, is_void_element, ComponentNode, Environment, Node,
};
use crate::parser::parse;
use crate::scriptbuilder::ScriptBuilder;
use crate::tokenizer::tokenize;
use crate::toposorter::TopoSorter;
use crate::typechecker::typecheck;
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

/// Compiler compiles hop modules into a Program that can execute components
pub struct Compiler {
    modules: HashMap<String, String>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, name: String, module: String) {
        self.modules.insert(name, module);
    }

    pub fn compile(&self) -> Result<Program, String> {
        let mut component_maps = HashMap::new();
        let mut import_maps = HashMap::new();
        let mut module_components = HashMap::new();
        let mut module_imports = HashMap::new();
        let mut module_parameter_types: HashMap<String, HashMap<String, crate::common::Type>> =
            HashMap::new();
        let mut script_builder = ScriptBuilder::new();
        let mut module_sorter = TopoSorter::new();

        // Parse all modules
        for (module_name, source_code) in &self.modules {
            let result = parse(tokenize(source_code.to_string()));
            if !result.errors.is_empty() {
                return Err(format_range_errors(
                    &format!("Parse errors in module {}", module_name),
                    &result.errors,
                ));
            }

            module_components.insert(module_name.clone(), result.components.clone());
            module_imports.insert(module_name.clone(), result.imports.clone());
            script_builder.add_module(module_name.clone(), result.components);

            module_sorter.add_node(module_name.clone());
            for import_node in &result.imports {
                module_sorter.add_dependency(module_name, &import_node.from_attr.value);
            }
        }

        // Sort modules topologically
        let sort_result = module_sorter.sort();
        if let Some(error) = sort_result.error {
            return Err(format!(
                "Circular module dependencies: {}",
                error.cycle.join(" -> ")
            ));
        }
        let sorted_modules = sort_result.nodes;

        // Typecheck modules in topological order
        for module_name in sorted_modules {
            let components = module_components.get(&module_name).unwrap();
            let imports = module_imports.get(&module_name).unwrap();
            let mut import_types = HashMap::new();

            for import_node in imports {
                let from_module = &import_node.from_attr.value;
                let component_name = &import_node.component_attr.value;

                if let Some(from_module_types) = module_parameter_types.get(from_module) {
                    if let Some(component_type) = from_module_types.get(component_name) {
                        import_types.insert(component_name.clone(), component_type.clone());
                    } else {
                        return Err(format!(
                            "Component {} not found in module {}",
                            component_name, from_module
                        ));
                    }
                } else {
                    return Err(format!("Module {} not found", from_module));
                }
            }

            let type_info = typecheck(components, &import_types);
            if !type_info.errors.is_empty() {
                return Err(format_range_errors(
                    &format!("Type errors in module {}", module_name),
                    &type_info.errors,
                ));
            }

            let mut component_map = HashMap::new();
            let mut import_map = HashMap::new();

            for comp_node in components {
                component_map.insert(comp_node.name_attr.value.clone(), comp_node.clone());
            }

            for import_node in imports {
                import_map.insert(
                    import_node.component_attr.value.clone(),
                    import_node.from_attr.value.clone(),
                );
            }

            component_maps.insert(module_name.clone(), component_map);
            import_maps.insert(module_name.clone(), import_map);
            module_parameter_types.insert(module_name.clone(), type_info.parameter_types);
        }

        Ok(Program::new(
            component_maps,
            import_maps,
            script_builder.build(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Position, Range, Token, TokenKind};
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::Path;

    fn normalize_tokens(tokens: Vec<Token>) -> Vec<Token> {
        tokens
            .into_iter()
            .map(|t| {
                if t.kind == TokenKind::Text && t.value.trim().is_empty() {
                    Token::new(
                        t.kind,
                        " ".to_string(),
                        t.attributes,
                        Range::new(Position::new(0, 0), Position::new(0, 0)),
                    )
                } else {
                    Token::new(
                        t.kind,
                        t.value,
                        t.attributes,
                        Range::new(Position::new(0, 0), Position::new(0, 0)),
                    )
                }
            })
            .collect()
    }

    #[test]
    fn test_compiler() {
        let entries = fs::read_dir(Path::new("test_data/compiler")).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let data_json = archive.get("data.json").unwrap().content.trim();
            let expected_output = archive.get("output.html").unwrap().content.trim();

            let mut compiler = Compiler::new();

            // Add all .hop files as modules
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let module_name = file.name.trim_end_matches(".hop");
                    compiler.add_module(module_name.to_string(), file.content.trim().to_string());
                }
            }

            let program = compiler.compile().unwrap_or_else(|e| {
                panic!("Compilation failed for {}: {}", file_name, e);
            });

            println!("{}", file_name);

            let data: serde_json::Value = serde_json::from_str(data_json).unwrap_or_else(|e| {
                panic!("Failed to parse JSON data for {}: {}", file_name, e);
            });

            let actual_output = program.execute("main", "main", data).unwrap_or_else(|e| {
                panic!("Execution failed for {}: {}", file_name, e);
            });

            // Normalize whitespace by tokenizing both outputs and comparing tokens
            let expected_tokens =
                normalize_tokens(crate::tokenizer::tokenize(expected_output.to_string()));
            let actual_tokens = normalize_tokens(crate::tokenizer::tokenize(actual_output.clone()));

            assert_eq!(
                actual_tokens, expected_tokens,
                "Mismatch in file: {}",
                file_name
            );
        }
    }
}
