use super::{GoTranspiler, JsTranspiler, LanguageMode, Transpiler};
use crate::ir::ast::IrEntrypoint;
use crate::ir::test_utils::build_ir_auto;
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

fn execute_javascript(code: &str, is_typescript: bool) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let extension = if is_typescript { "ts" } else { "js" };
    let module_file = temp_dir.path().join(format!("module.{}", extension));
    let runner_file = temp_dir.path().join(format!("runner.{}", extension));

    // Write the generated module code
    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    // Create a runner that imports and executes the function
    // Always calls test() since all our tests use "test" as the entrypoint name
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

fn execute_go(code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let main_file = temp_dir.path().join("main.go");

    // Create a main.go file that imports and calls our function
    // Always calls Test() since all our tests use "test" as the entrypoint name
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

    // Create go.mod file
    let go_mod = r#"module testmod

go 1.24
"#;
    fs::write(temp_dir.path().join("go.mod"), go_mod)
        .map_err(|e| format!("Failed to write go.mod: {}", e))?;

    // Create components subdirectory and put our code there
    let components_dir = temp_dir.path().join("components");
    fs::create_dir(&components_dir)
        .map_err(|e| format!("Failed to create components dir: {}", e))?;
    fs::write(components_dir.join("test.go"), code)
        .map_err(|e| format!("Failed to write components/test.go: {}", e))?;

    // Run the Go code
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

    // Transpile to JavaScript
    let js_transpiler = JsTranspiler::new(LanguageMode::JavaScript);
    let js_code = js_transpiler.transpile_module(&entrypoints);

    // Transpile to TypeScript
    let ts_transpiler = JsTranspiler::new(LanguageMode::TypeScript);
    let ts_code = ts_transpiler.transpile_module(&entrypoints);

    // Transpile to Go
    let go_transpiler = GoTranspiler::new();
    let go_code = go_transpiler.transpile_module(&entrypoints);

    // Execute JavaScript version
    let js_output = execute_javascript(&js_code, false)
        .map_err(|e| format!("JavaScript execution failed: {}", e))?;

    // Execute TypeScript version
    let ts_output = execute_javascript(&ts_code, true)
        .map_err(|e| format!("TypeScript execution failed: {}", e))?;

    // Execute Go version
    let go_output = execute_go(&go_code).map_err(|e| format!("Go execution failed: {}", e))?;

    // Verify outputs match expected
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

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore] // Mark as ignored by default since it requires Node.js and Go runtime
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
                t.let_stmt("text", t.str("<script>alert('xss')</script>"), |t| {
                    t.write_expr_escaped(t.var("text"));
                });
            }),
            "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;",
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
                    t.json_encode(t.object(vec![("name", t.str("Test")), ("value", t.num(42.0))])),
                    false,
                );
            }),
            r#"{"name":"Test","value":42}"#,
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
}
