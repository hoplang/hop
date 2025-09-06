use crate::common::{escape_html, is_void_element};
use crate::dop::{self, evaluate_expr};
use crate::hop::ast::{HopAst, HopNode};
use crate::hop::environment::Environment;
use anyhow::Result;
use std::collections::HashMap;
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

/// Render the content for a specific file path
pub fn render_file(
    asts: &HashMap<String, HopAst>,
    hop_mode: HopMode,
    file_path: &str,
    output: &mut String,
) -> Result<()> {
    let render = asts
        .values()
        .flat_map(|ast| ast.get_renders())
        .find(|r| r.file_attr.value.as_str() == file_path);
    match render {
        Some(node) => {
            let mut env = init_environment(hop_mode);
            for child in &node.children {
                evaluate_node_entrypoint(asts, hop_mode, child, &mut env, "build", output)?;
            }
            Ok(())
        }
        None => Err(anyhow::anyhow!(
            "File path '{}' not found in render nodes",
            file_path
        )),
    }
}

// Evaluate a component definition of a specific name in a specific module.
pub fn evaluate_component(
    asts: &HashMap<String, HopAst>,
    hop_mode: HopMode,
    module_name: &str,
    component_name: &str,
    args: HashMap<String, serde_json::Value>,
    slot_content: Option<&str>,
    additional_classes: Option<&str>,
    output: &mut String,
) -> Result<()> {
    let ast = asts
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

    let mut env = init_environment(hop_mode);

    // Set up environment with all parameters and their corresponding values
    if let Some((params, _)) = &component.params {
        for (_, param) in params.iter() {
            if let Some(value) = args.get(param.var_name.value.as_str()) {
                let _ = env.push(param.var_name.value.to_string(), value.clone());
            }
        }
    }

    if component.is_entrypoint {
        // For entrypoints, don't wrap in a div, just execute children directly
        for child in &component.children {
            evaluate_node_entrypoint(asts, hop_mode, child, &mut env, module_name, output)?;
        }
        Ok(())
    } else {
        let tag_name = match &component.as_attr {
            Some(as_attr) => as_attr.value.as_str(),
            _ => "div",
        };

        output.push('<');
        output.push_str(tag_name);
        output.push_str(" data-hop-id=\"");
        output.push_str(module_name);
        output.push('/');
        output.push_str(component_name);
        output.push('"');

        for (name, attr) in &component.attributes {
            if name == "class" {
                match additional_classes {
                    None => {
                        output.push(' ');
                        output.push_str(name);
                        if let Some(val) = &attr.value {
                            output.push_str("=\"");
                            output.push_str(val.as_str());
                            output.push('"');
                        }
                    }
                    Some(cls) => {
                        output.push(' ');
                        output.push_str(name);
                        output.push_str("=\"");
                        if let Some(val) = &attr.value {
                            output.push_str(val.as_str());
                        }
                        output.push(' ');
                        output.push_str(cls);
                        output.push('"');
                    }
                }
            } else {
                output.push(' ');
                output.push_str(name);
                if let Some(val) = &attr.value {
                    output.push_str("=\"");
                    output.push_str(val.as_str());
                    output.push('"');
                }
            }
        }

        // If component doesn't have a class attribute but the reference does, add it
        if !component.attributes.contains_key("class") {
            if let Some(cls) = additional_classes {
                output.push_str(" class=\"");
                output.push_str(cls);
                output.push('"');
            }
        }
        output.push('>');
        for child in &component.children {
            evaluate_node(
                asts,
                hop_mode,
                child,
                slot_content,
                &mut env,
                module_name,
                output,
            )?;
        }
        output.push_str("</");
        output.push_str(tag_name);
        output.push('>');

        Ok(())
    }
}

/// Initialize an environment with global variables like HOP_MODE
fn init_environment(hop_mode: HopMode) -> Environment<serde_json::Value> {
    let mut env = Environment::new();
    let _ = env.push(
        "HOP_MODE".to_string(),
        serde_json::Value::String(hop_mode.as_str().to_string()),
    );
    env
}

fn evaluate_node(
    asts: &HashMap<String, HopAst>,
    hop_mode: HopMode,
    node: &HopNode,
    slot_content: Option<&str>,
    env: &mut Environment<serde_json::Value>,
    current_module: &str,
    output: &mut String,
) -> anyhow::Result<()> {
    match node {
        HopNode::If {
            condition,
            children,
            ..
        } => match dop::evaluate_expr(condition, env)?.as_bool() {
            Some(cond) => {
                if cond {
                    for child in children {
                        evaluate_node(
                            asts,
                            hop_mode,
                            child,
                            slot_content,
                            env,
                            current_module,
                            output,
                        )?;
                    }
                }
                Ok(())
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

            for item in array {
                _ = env.push(var_name.value.to_string(), item.clone());
                for child in children {
                    evaluate_node(
                        asts,
                        hop_mode,
                        child,
                        slot_content,
                        env,
                        current_module,
                        output,
                    )?;
                }
                _ = env.pop();
            }

            Ok(())
        }

        HopNode::ComponentReference {
            tag_name,
            args,
            attributes,
            definition_module,
            children,
            ..
        } => {
            let target_module = definition_module
                .as_ref()
                .expect("Could not find definition module for component reference");

            let target_component = asts
                .get(target_module)
                .and_then(|ast| ast.get_component_definition(tag_name.as_str()))
                .expect("Could not find target component for component reference");

            let mut arg_values = HashMap::new();
            match args {
                None => {}
                Some((args, _)) => {
                    for arg in args.values() {
                        let value = evaluate_expr(&arg.expression, env)?;
                        arg_values.insert(arg.var_name.value.to_string(), value);
                    }
                }
            }

            // Collect slot content if component has a slot and there are children
            let slot_html = if target_component.has_slot && !children.is_empty() {
                let mut default_html = String::new();
                for child in children {
                    evaluate_node(
                        asts,
                        hop_mode,
                        child,
                        slot_content,
                        env,
                        current_module,
                        &mut default_html,
                    )?;
                }
                Some(default_html)
            } else {
                None
            };

            // Extract class attribute from component reference
            let additional_classes = attributes
                .get("class")
                .and_then(|attr| attr.value.clone())
                .map(|val| val.to_string());

            evaluate_component(
                asts,
                hop_mode,
                target_module,
                tag_name.as_str(),
                arg_values,
                slot_html.as_deref(),
                additional_classes.as_deref(),
                output,
            )
        }

        HopNode::SlotDefinition { .. } => {
            // Use the supplied slot content if available, otherwise return empty
            if let Some(content) = slot_content {
                output.push_str(content);
            }
            Ok(())
        }

        HopNode::Html {
            children,
            tag_name,
            attributes,
            set_attributes,
            ..
        } => {
            // Skip style nodes
            if tag_name.as_str() == "style" {
                return Ok(());
            }

            // Skip script nodes without a src attribute
            if tag_name.as_str() == "script" && !attributes.contains_key("src") {
                return Ok(());
            }

            output.push('<');
            output.push_str(tag_name.as_str());
            for (name, attr) in attributes {
                if !name.starts_with("set-") {
                    output.push(' ');
                    output.push_str(name);
                    if let Some(val) = &attr.value {
                        output.push_str("=\"");
                        output.push_str(val.as_str());
                        output.push('"');
                    }
                }
            }

            // Evaluate and add set-* attributes
            for set_attr in set_attributes {
                let attr_name = &set_attr.name[4..]; // Remove "set-" prefix
                let evaluated = evaluate_expr(&set_attr.expression, env)?;
                output.push(' ');
                output.push_str(attr_name);
                output.push_str("=\"");
                output.push_str(&escape_html(evaluated.as_str().unwrap()));
                output.push('"');
            }

            output.push('>');

            if !is_void_element(tag_name.as_str()) {
                for child in children {
                    evaluate_node(
                        asts,
                        hop_mode,
                        child,
                        slot_content,
                        env,
                        current_module,
                        output,
                    )?;
                }
                output.push_str("</");
                output.push_str(tag_name.as_str());
                output.push('>');
            }

            Ok(())
        }

        HopNode::Error { .. } => Ok(()),

        HopNode::Text { value, .. } => {
            output.push_str(value.as_str());
            Ok(())
        }

        HopNode::TextExpression { expression, .. } => {
            match evaluate_expr(expression, env)?.as_str() {
                Some(s) => {
                    output.push_str(&escape_html(s));
                    Ok(())
                }
                None => anyhow::bail!("Could not evaluate expression to string"),
            }
        }

        HopNode::Doctype { .. } => {
            output.push_str("<!DOCTYPE html>");
            Ok(())
        }

        HopNode::XExec {
            cmd_attr, children, ..
        } => {
            // Collect child content as stdin
            let mut stdin_content = String::new();
            for child in children {
                evaluate_node(
                    asts,
                    hop_mode,
                    child,
                    slot_content,
                    env,
                    current_module,
                    &mut stdin_content,
                )?;
            }

            // Execute the command with stdin
            let command = &cmd_attr.value;
            let result = execute_command(command.as_str(), &stdin_content)?;
            output.push_str(&result);
            Ok(())
        }

        HopNode::XRaw { trim, children, .. } => {
            // For hop-x-raw nodes, just render the inner content without the tags
            if *trim {
                let mut temp = String::new();
                for child in children {
                    evaluate_node(
                        asts,
                        hop_mode,
                        child,
                        slot_content,
                        env,
                        current_module,
                        &mut temp,
                    )?;
                }
                output.push_str(&trim_raw_string(&temp));
            } else {
                for child in children {
                    evaluate_node(
                        asts,
                        hop_mode,
                        child,
                        slot_content,
                        env,
                        current_module,
                        output,
                    )?;
                }
            }
            Ok(())
        }
    }
}

fn evaluate_node_entrypoint(
    asts: &HashMap<String, HopAst>,
    hop_mode: HopMode,
    node: &HopNode,
    env: &mut Environment<serde_json::Value>,
    current_module: &str,
    output: &mut String,
) -> Result<()> {
    match node {
        HopNode::Html {
            tag_name,
            attributes,
            children,
            set_attributes,
            ..
        } => {
            // For entrypoints, preserve script and style tags
            output.push('<');
            output.push_str(tag_name.as_str());
            for (name, attr) in attributes {
                if !name.starts_with("set-") {
                    output.push(' ');
                    output.push_str(name);
                    if let Some(val) = &attr.value {
                        output.push_str("=\"");
                        output.push_str(val.as_str());
                        output.push('"');
                    }
                }
            }

            // Evaluate and add set-* attributes
            for set_attr in set_attributes {
                let attr_name = &set_attr.name[4..]; // Remove "set-" prefix
                let evaluated = evaluate_expr(&set_attr.expression, env)?;
                output.push(' ');
                output.push_str(attr_name);
                output.push_str("=\"");
                output.push_str(&escape_html(evaluated.as_str().unwrap()));
                output.push('"');
            }

            output.push('>');

            if !is_void_element(tag_name.as_str()) {
                for child in children {
                    evaluate_node_entrypoint(asts, hop_mode, child, env, current_module, output)?;
                }
                output.push_str("</");
                output.push_str(tag_name.as_str());
                output.push('>');
            }

            Ok(())
        }
        _ => {
            // For all other node types, use the existing evaluation logic (no slots in entrypoints)
            evaluate_node(asts, hop_mode, node, None, env, current_module, output)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::{ast::HopAst, typechecker::TypeChecker};
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use serde_json::json;
    use simple_txtar::Archive;
    use std::collections::HashMap;

    fn asts_from_archive(archive: &Archive) -> HashMap<String, HopAst> {
        let mut asts = HashMap::new();

        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            let mut errors = Vec::new();
            let tokenizer = Tokenizer::new(&file.content);
            let ast = parse(module_name.clone(), tokenizer, &mut errors);

            if !errors.is_empty() {
                panic!("Parse errors in {}: {:?}", module_name, errors);
            }

            asts.insert(module_name, ast);
        }

        let mut typechecker = TypeChecker::default();

        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            let ast = asts.get(&module_name).unwrap();

            typechecker.typecheck(&[ast]);

            if !typechecker.type_errors.get(&ast.name).unwrap().is_empty() {
                panic!(
                    "Type errors in {}: {:?}",
                    module_name,
                    typechecker.type_errors.get(&ast.name).unwrap()
                );
            }
        }

        asts
    }

    fn check(archive_str: &str, data: serde_json::Value, expected: Expect) {
        let asts = asts_from_archive(&Archive::from(archive_str));

        let args: HashMap<String, serde_json::Value> = data
            .as_object()
            .expect("Expected JSON object with parameter names as keys")
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        let mut actual_output = String::new();
        evaluate_component(
            &asts,
            HopMode::Dev,
            "main",
            "main-comp",
            args,
            None,
            None,
            &mut actual_output,
        )
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
            json!({}),
            expect![[r#"
                <div data-hop-id="main/main-comp" class="p-2">
                    <div>hello</div>
                </div>
            "#]],
        );
    }

    /// Using {} should print the string into the HTML.
    #[test]
    fn test_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {p: string}>
                	<div>{p}</div>
                </main-comp>
            "#},
            json!({
                "p": "foo bar",
            }),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	<div>foo bar</div>
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
            json!({
                "p": "<script>alert(1);</script>"
            }),
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
                }>
                  <if {user.name == admin.name}>
                    <div>Is Admin</div>
                  </if>
                </main-comp>
            "#},
            json!({
              "user": {"name": "alice"},
              "admin": {"name": "alice"},
            }),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Is Admin</div>
                </div>
            "#]],
        );
    }

    #[test]
    fn test_for_loop_with_conditional() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {
                    items: array[{show: boolean, data: string}]
                }>
                    <for {item in items}>
                        <if {item.show}>
                            <div>{item.data}</div>
                        </if>
                    </for>
                </main-comp>
            "#},
            json!({
                "items": [
                    {"show": true, "data": "foo"},
                    {"show": false, "data": "bar"},
                    {"show": true, "data": "baz"}
                ]
            }),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                            <div>foo</div>
                            <div>baz</div>
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
            json!({}),
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
            json!({
              "users": [
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"},
              ]
            }),
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
            json!({}),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                  <div>Mode: dev</div>
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
            json!({}),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div>Dev mode active</div>
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
                        <slot-default />
                    </div>
                </foo-comp>

                <bar-comp>
                    <foo-comp>
                        <slot-default/>
                    </foo-comp>
                </bar-comp>

                <main-comp entrypoint>
                    <bar-comp>
                        <h1>Bar Title</h1>
                        <p>Bar Content</p>
                    </bar-comp>
                </main-comp>
            "#},
            json!({}),
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
            json!({
                "profile_url": "https://example.com/user/123",
                "name": "John Doe",
            }),
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
                        <slot-default/>
                    </div>
                </main-card>

                <main-comp entrypoint {title: string, message: string}>
                    <main-card>
                        <h1>{title}</h1>
                        <p>{message}</p>
                    </main-card>
                </main-comp>
            "#},
            json!({
                "title": "Hello World",
                "message": "This text comes from outside params"
            }),
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
    fn test_x_raw_simple() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <hop-x-raw>foo bar</hop-x-raw>
                </main-comp>
            "#},
            json!({}),
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
                		<main-comp>some html</main-comp>
                	</hop-x-raw>
                </main-comp>
            "#},
            json!({}),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                		<main-comp>some html</main-comp>
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
            json!({}),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	trimmed  
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
            json!({}),
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
            json!({}),
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
            json!({}),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                	<div data-hop-id="main/my-button" class="px-4 py-2">Click me</div>
                </div>
            "#]],
        );
    }

    /// Test set-id attribute functionality
    #[test]
    fn test_set_id_attribute() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {id: string}>
                    <div set-id="id">my div</div>
                </main-comp>
            "#},
            json!({
                "id": "my-unique-id"
            }),
            expect![[r#"
                <div data-hop-id="main/main-comp">
                    <div id="my-unique-id">my div</div>
                </div>
            "#]],
        );
    }
}
