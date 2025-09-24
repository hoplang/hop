use super::{GoTranspiler, JsTranspiler, LanguageMode, PythonTranspiler, Transpiler};
use crate::dop::r#type::Type;
use crate::ir::ast::IrEntrypoint;
use crate::ir::test_utils::build_ir_auto;
use std::collections::BTreeMap;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug)]
struct TestCase {
    entrypoint: IrEntrypoint,
    expected_output: &'static str,
}

impl TestCase {
    fn new(entrypoint: IrEntrypoint, expected_output: &'static str) -> Self {
        Self {
            entrypoint,
            expected_output,
        }
    }
}

#[derive(Debug)]
struct TypeCheckTestCase {
    entrypoints: Vec<IrEntrypoint>,
}

impl TypeCheckTestCase {
    fn new(entrypoints: Vec<IrEntrypoint>) -> Self {
        Self { entrypoints }
    }
}

fn execute_javascript(code: &str, is_typescript: bool) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let extension = if is_typescript { "ts" } else { "js" };
    let module_file = temp_dir.path().join(format!("module.{}", extension));
    let runner_file = temp_dir.path().join(format!("runner.{}", extension));

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let runner_code = format!(
        r#"
import module from './module.{}';
console.log(module.test());
"#,
        extension
    );

    fs::write(&runner_file, runner_code)
        .map_err(|e| format!("Failed to write runner file: {}", e))?;

    let output = Command::new("bun")
        .arg("run")
        .arg(&runner_file)
        .output()
        .map_err(|e| format!("Failed to execute Bun: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Bun execution failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn execute_python(code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let module_file = temp_dir.path().join("module.py");
    let runner_file = temp_dir.path().join("runner.py");

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let runner_code = r#"
from module import test
print(test(), end='')
"#;

    fs::write(&runner_file, runner_code)
        .map_err(|e| format!("Failed to write runner file: {}", e))?;

    let output = Command::new("python3")
        .arg(&runner_file)
        .current_dir(&temp_dir)
        .output()
        .map_err(|e| format!("Failed to execute Python: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Python execution failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn execute_go(code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let main_file = temp_dir.path().join("main.go");

    let main_code = r#"package main

import (
    "fmt"
    "testmod/components"
)

func main() {
    result := components.Test()
    fmt.Print(result)
}
"#;

    fs::write(&main_file, main_code).map_err(|e| format!("Failed to write main.go: {}", e))?;

    let go_mod = r#"module testmod

go 1.24
"#;
    fs::write(temp_dir.path().join("go.mod"), go_mod)
        .map_err(|e| format!("Failed to write go.mod: {}", e))?;

    let components_dir = temp_dir.path().join("components");
    fs::create_dir(&components_dir)
        .map_err(|e| format!("Failed to create components dir: {}", e))?;
    fs::write(components_dir.join("test.go"), code)
        .map_err(|e| format!("Failed to write components/test.go: {}", e))?;

    let output = Command::new("go")
        .arg("run")
        .arg("main.go")
        .current_dir(&temp_dir)
        .output()
        .map_err(|e| format!("Failed to execute Go: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Go execution failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn run_integration_test(test_case: TestCase) -> Result<(), String> {
    let entrypoints = vec![test_case.entrypoint];

    let js_transpiler = JsTranspiler::new(LanguageMode::JavaScript);
    let js_code = js_transpiler.transpile_module(&entrypoints);

    let ts_transpiler = JsTranspiler::new(LanguageMode::TypeScript);
    let ts_code = ts_transpiler.transpile_module(&entrypoints);

    let go_transpiler = GoTranspiler::new();
    let go_code = go_transpiler.transpile_module(&entrypoints);

    let python_transpiler = PythonTranspiler::new();
    let python_code = python_transpiler.transpile_module(&entrypoints);

    let js_output = execute_javascript(&js_code, false)
        .map_err(|e| format!("JavaScript execution failed: {}", e))?;

    let ts_output = execute_javascript(&ts_code, true)
        .map_err(|e| format!("TypeScript execution failed: {}", e))?;

    let go_output = execute_go(&go_code).map_err(|e| format!("Go execution failed: {}", e))?;

    let python_output =
        execute_python(&python_code).map_err(|e| format!("Python execution failed: {}", e))?;

    if js_output != test_case.expected_output {
        return Err(format!(
            "JavaScript output mismatch:\nExpected: {}\nGot: {}",
            test_case.expected_output, js_output
        ));
    }

    if ts_output != test_case.expected_output {
        return Err(format!(
            "TypeScript output mismatch:\nExpected: {}\nGot: {}",
            test_case.expected_output, ts_output
        ));
    }

    if go_output != test_case.expected_output {
        return Err(format!(
            "Go output mismatch:\nExpected: {}\nGot: {}",
            test_case.expected_output, go_output
        ));
    }

    if python_output != test_case.expected_output {
        return Err(format!(
            "Python output mismatch:\nExpected: {}\nGot: {}",
            test_case.expected_output, python_output
        ));
    }

    Ok(())
}

fn typecheck_javascript(code: &str, is_typescript: bool) -> Result<(), String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let extension = if is_typescript { "ts" } else { "js" };
    let module_file = temp_dir.path().join(format!("module.{}", extension));

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let mut args = vec!["x", "tsc", "--noEmit", "--target", "ES2020"];

    if is_typescript {
        args.push("--strict");
    } else {
        args.push("--allowJs");
        args.push("--checkJs");
    }

    let file_path = module_file
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?;
    args.push(file_path);

    let output = Command::new("bun")
        .args(&args)
        .output()
        .map_err(|e| format!("Failed to execute TypeScript compiler: {}", e))?;

    if !output.status.success() {
        let lang = if is_typescript {
            "TypeScript"
        } else {
            "JavaScript"
        };
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!(
            "{} type checking failed:\nSTDERR:\n{}\nSTDOUT:\n{}",
            lang, stderr, stdout
        ));
    }

    Ok(())
}

fn typecheck_python(code: &str) -> Result<(), String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let module_file = temp_dir.path().join("module.py");

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let output = Command::new("python3")
        .arg("-m")
        .arg("mypy")
        .arg("--strict")
        .arg("--no-error-summary")
        .arg(&module_file)
        .output()
        .map_err(|e| format!("Failed to execute mypy: {}", e))?;

    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "Python type checking failed:\nSTDOUT:\n{}\nSTDERR:\n{}",
            stdout, stderr
        ));
    }

    Ok(())
}

fn typecheck_go(code: &str) -> Result<(), String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;

    let go_mod = "module test\n\ngo 1.24\n";
    fs::write(temp_dir.path().join("go.mod"), go_mod)
        .map_err(|e| format!("Failed to write go.mod: {}", e))?;

    let package_dir = temp_dir.path().join("components");
    fs::create_dir(&package_dir).map_err(|e| format!("Failed to create package dir: {}", e))?;

    let module_file = package_dir.join("components.go");
    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let output = Command::new("go")
        .arg("build")
        .arg("./components")
        .current_dir(&temp_dir)
        .output()
        .map_err(|e| format!("Failed to execute Go compiler: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Go compilation failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(())
}

fn run_type_check_test(test_case: TypeCheckTestCase) -> Result<(), String> {
    let js_transpiler = JsTranspiler::new(LanguageMode::JavaScript);
    let js_code = js_transpiler.transpile_module(&test_case.entrypoints);
    typecheck_javascript(&js_code, false)?;

    let ts_transpiler = JsTranspiler::new(LanguageMode::TypeScript);
    let ts_code = ts_transpiler.transpile_module(&test_case.entrypoints);
    typecheck_javascript(&ts_code, true)?;

    let go_transpiler = GoTranspiler::new();
    let go_code = go_transpiler.transpile_module(&test_case.entrypoints);
    typecheck_go(&go_code)?;

    let python_transpiler = PythonTranspiler::new();
    let python_code = python_transpiler.transpile_module(&test_case.entrypoints);
    typecheck_python(&python_code)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_simple_html() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.write("<h1>Hello, World!</h1>");
            }),
            "<h1>Hello, World!</h1>",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_with_let_binding() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("name", t.str("Alice"), |t| {
                    t.write("Hello, ");
                    t.write_expr(t.var("name"), false);
                    t.write("!");
                });
            }),
            "Hello, Alice!",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_conditional() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("show", t.bool(true), |t| {
                    t.if_stmt(t.var("show"), |t| {
                        t.write("Visible");
                    });
                    t.if_stmt(t.not(t.var("show")), |t| {
                        t.write("Hidden");
                    });
                });
            }),
            "Visible",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_for_loop() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.for_loop(
                    "item",
                    t.array(vec![t.str("a"), t.str("b"), t.str("c")]),
                    |t| {
                        t.write_expr(t.var("item"), false);
                        t.write(",");
                    },
                );
            }),
            "a,b,c,",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_html_escaping() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("text", t.str("<div>Hello & world</div>"), |t| {
                    t.write_expr_escaped(t.var("text"));
                });
            }),
            "&lt;div&gt;Hello &amp; world&lt;/div&gt;",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_object_property_access() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt(
                    "user",
                    t.object(vec![("name", t.str("Bob")), ("age", t.str("25"))]),
                    |t| {
                        t.write_expr(t.prop_access(t.var("user"), "name"), false);
                        t.write(" is ");
                        t.write_expr(t.prop_access(t.var("user"), "age"), false);
                        t.write(" years old");
                    },
                );
            }),
            "Bob is 25 years old",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_let_binding() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("message", t.str("Hello from let"), |t| {
                    t.write_expr(t.var("message"), false);
                });
            }),
            "Hello from let",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_string_concatenation() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.let_stmt("first", t.str("Hello"), |t| {
                    t.let_stmt("second", t.str(" World"), |t| {
                        t.write_expr(t.string_concat(t.var("first"), t.var("second")), false);
                    });
                });
            }),
            "Hello World",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_json_encode() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.write_expr(
                    t.json_encode(t.array(vec![t.str("Hello"), t.str("World")])),
                    false,
                );
            }),
            r#"["Hello","World"]"#,
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_complex_nested_structure() {
        let test_case = TestCase::new(
            build_ir_auto("test", vec![], |t| {
                t.for_loop("item", t.array(vec![t.str("A"), t.str("B")]), |t| {
                    t.let_stmt("prefix", t.str("["), |t| {
                        t.write_expr(t.var("prefix"), false);
                        t.write_expr(t.var("item"), false);
                        t.write("]");
                    });
                });
            }),
            "[A][B]",
        );

        run_integration_test(test_case).expect("Integration test failed");
    }

    #[test]
    #[ignore]
    fn test_typecheck_deeply_nested_params() {
        // Create deeply nested parameter structure
        let parameters = vec![(
            "config",
            Type::Object({
                let mut config = BTreeMap::new();
                config.insert("api_key".to_string(), Type::String);
                config.insert(
                    "database".to_string(),
                    Type::Object({
                        let mut db = BTreeMap::new();
                        db.insert("host".to_string(), Type::String);
                        db.insert("port".to_string(), Type::Number);
                        db.insert(
                            "credentials".to_string(),
                            Type::Object({
                                let mut creds = BTreeMap::new();
                                creds.insert("username".to_string(), Type::String);
                                creds.insert("password".to_string(), Type::String);
                                creds
                            }),
                        );
                        db
                    }),
                );
                config.insert(
                    "features".to_string(),
                    Type::Array(Some(Box::new(Type::Object({
                        let mut feature = BTreeMap::new();
                        feature.insert("name".to_string(), Type::String);
                        feature.insert("enabled".to_string(), Type::Bool);
                        feature.insert(
                            "settings".to_string(),
                            Type::Object({
                                let mut settings = BTreeMap::new();
                                settings.insert("level".to_string(), Type::String);
                                settings.insert("timeout".to_string(), Type::Number);
                                settings
                            }),
                        );
                        feature
                    })))),
                );
                config
            }),
        )];

        let test_case =
            TypeCheckTestCase::new(vec![build_ir_auto("test-deep-config", parameters, |t| {
                t.write("<div>API Key: ");
                t.write_expr_escaped(t.prop_access(t.var("config"), "api_key"));
                t.write("</div>\n");
                t.write("<div>DB Host: ");
                t.write_expr_escaped(
                    t.prop_access(t.prop_access(t.var("config"), "database"), "host"),
                );
                t.write("</div>\n");
            })]);

        run_type_check_test(test_case).expect("Type check test failed");
    }

    #[test]
    #[ignore]
    fn test_typecheck_array_of_objects_param() {
        let parameters = vec![(
            "users",
            Type::Array(Some(Box::new(Type::Object({
                let mut user = BTreeMap::new();
                user.insert("name".to_string(), Type::String);
                user.insert("email".to_string(), Type::String);
                user.insert(
                    "profile".to_string(),
                    Type::Object({
                        let mut profile = BTreeMap::new();
                        profile.insert("bio".to_string(), Type::String);
                        profile.insert("age".to_string(), Type::Number);
                        profile
                    }),
                );
                user
            })))),
        )];

        let test_case =
            TypeCheckTestCase::new(vec![build_ir_auto("test-users", parameters, |t| {
                t.for_loop("user", t.var("users"), |t| {
                    t.write("<div>");
                    t.write_expr_escaped(t.prop_access(t.var("user"), "name"));
                    t.write(" - ");
                    t.write_expr_escaped(
                        t.prop_access(t.prop_access(t.var("user"), "profile"), "bio"),
                    );
                    t.write("</div>\n");
                });
            })]);

        run_type_check_test(test_case).expect("Type check test failed");
    }
}
