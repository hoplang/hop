use super::{GoTranspiler, PythonTranspiler, Transpiler, TsTranspiler};
use crate::document::Document;
use crate::hop::program::Program;
use crate::hop::syntax::format;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use expect_test::Expect;
use std::collections::HashMap;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn execute_typescript(code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let module_file = temp_dir.path().join("module.ts");
    let runner_file = temp_dir.path().join("runner.ts");

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let runner_code = r#"
import module from './module.ts';
console.log(module.test());
"#;

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
    "strings"
    "testmod/components"
)

func main() {
    var buf strings.Builder
    components.Test(&buf)
    fmt.Print(buf.String())
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

fn typecheck_typescript(code: &str) -> Result<(), String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let module_file = temp_dir.path().join("module.ts");

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let file_path = module_file
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?;

    let output = Command::new("bun")
        .args([
            "x", "tsc", "--noEmit", "--target", "ES2020", "--strict", file_path,
        ])
        .output()
        .map_err(|e| format!("Failed to execute TypeScript compiler: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!(
            "TypeScript type checking failed:\nSTDERR:\n{}\nSTDOUT:\n{}",
            stderr, stdout
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

fn check(hop_source: &str, expected_output: &str, expected: Expect) {
    // Parse hop source code
    let module_name = ModuleName::new("test").unwrap();
    let mut modules = HashMap::new();
    modules.insert(module_name.clone(), Document::new(hop_source.to_string()));

    let program = Program::new(modules);

    // Verify input is properly formatted
    let parsed_ast = program.get_parsed_ast(&module_name).expect("Failed to get parsed AST");
    let formatted = format(parsed_ast.clone());
    assert_eq!(
        formatted.trim(),
        hop_source.trim(),
        "Test input is not properly formatted. Update the test input (right) to match the formatted output (left)."
    );

    // Check for errors and report them for easier debugging
    let parse_errors = program.get_parse_errors();
    for (module, errors) in parse_errors.iter() {
        for error in errors.iter() {
            eprintln!("Parse Error in {:?}: {:?}", module, error);
        }
    }
    let type_errors = program.get_type_errors();
    for (module, errors) in type_errors.iter() {
        for error in errors.iter() {
            eprintln!("Type Error in {:?}: {:?}", module, error);
        }
    }

    let typed_asts = program.get_typed_modules();

    // Compile to IR (skip HTML structure injection and dev mode wrapper for simpler test output)
    let pages = vec![(module_name, ComponentName::new("Test".to_string()).unwrap())];
    let options = OrchestrateOptions {
        skip_html_structure: true,
        skip_dev_mode_wrapper: true,
    };
    let module = orchestrate(typed_asts, None, &pages, options).expect("Orchestration failed");

    let ir_output = module.to_string();

    let mut output = format!(
        "-- ir --\n{}-- expected output --\n{}\n",
        ir_output, expected_output
    );

    let ts_transpiler = TsTranspiler::new();
    let ts_code = ts_transpiler.transpile_module(&module);
    typecheck_typescript(&ts_code).expect("TypeScript typecheck failed");
    let ts_output = execute_typescript(&ts_code).expect("TypeScript execution failed");
    assert_eq!(ts_output, expected_output, "TypeScript output mismatch");
    output.push_str("-- ts --\nOK\n");

    let go_transpiler = GoTranspiler::new("components".to_string());
    let go_code = go_transpiler.transpile_module(&module);
    typecheck_go(&go_code).expect("Go typecheck failed");
    let go_output = execute_go(&go_code).expect("Go execution failed");
    assert_eq!(go_output, expected_output, "Go output mismatch");
    output.push_str("-- go --\nOK\n");

    let python_transpiler = PythonTranspiler::new();
    let python_code = python_transpiler.transpile_module(&module);
    typecheck_python(&python_code).expect("Python typecheck failed");
    let python_output = execute_python(&python_code).expect("Python execution failed");
    assert_eq!(python_output, expected_output, "Python output mismatch");
    output.push_str("-- python --\nOK\n");

    expected.assert_eq(&output);
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;
    use indoc::indoc;

    #[test]
    #[ignore]
    fn option_match_returning_options() {
        check(
            indoc! {r#"
                <Test>
                  <let {inner: Option[String] = Some("hello")}>
                    <let {
                      mapped: Option[String] = match inner {
                        Some(x) => Some(x),
                        None => None,
                      },
                    }>
                      <match {mapped}>
                        <case {Some(result)}>
                          mapped:{result}
                        </case>
                        <case {None}>
                          was-none
                        </case>
                      </match>
                    </let>
                  </let>
                </Test>
            "#},
            "mapped:hello",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[String]::Some("hello") in {
                    match match_subject {
                      Some(_) => {
                        write("mapped:hello")
                      }
                      None => {
                        write("was-none")
                      }
                    }
                  }
                }
                -- expected output --
                mapped:hello
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_as_some_value() {
        check(
            indoc! {r#"
                <Test>
                  <let {inner_opt: Option[String] = Some("inner")}>
                    <let {
                      outer: Option[String] = Some(
                        match inner_opt {Some(x) => x, None => "default"}
                      ),
                    }>
                      <match {outer}>
                        <case {Some(val)}>
                          {val}
                        </case>
                        <case {None}>
                          none
                        </case>
                      </match>
                    </let>
                  </let>
                </Test>
            "#},
            "inner",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[String]::Some("inner") in {
                    match match_subject {
                      Some(_) => {
                        write("inner")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                inner
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_expr() {
        check(
            indoc! {r#"
                <Test>
                  <let {flag: Bool = true}>
                    {match flag {true => "yes", false => "no"}}
                  </let>
                  <let {other: Bool = false}>
                    {match other {true => "YES", false => "NO"}}
                  </let>
                </Test>
            "#},
            "yesNO",
            expect![[r#"
                -- ir --
                Test() {
                  write("yesNO")
                }
                -- expected output --
                yesNO
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal_inline_match_expr() {
        check(
            indoc! {r#"
                <Test>
                  <let {opt1: Option[String] = Some("hi")}>
                    {match opt1 {Some(_) => "some", None => "none"}}
                  </let>
                  ,
                  <let {opt2: Option[String] = None}>
                    {match opt2 {Some(_) => "SOME", None => "NONE"}}
                  </let>
                </Test>
            "#},
            "some,NONE",
            expect![[r#"
                -- ir --
                Test() {
                  write("some,NONE")
                }
                -- expected output --
                some,NONE
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_bool_match_expr() {
        check(
            indoc! {r#"
                <Test>
                  <let {outer: Bool = true}>
                    <let {inner: Bool = false}>
                      {match outer {
                        true => match inner {true => "TT", false => "TF"},
                        false => "F",
                      }}
                    </let>
                  </let>
                  ,
                  <let {outer2: Bool = false}>
                    <let {inner2: Bool = true}>
                      {match outer2 {
                        true => match inner2 {true => "TT", false => "TF"},
                        false => "F",
                      }}
                    </let>
                  </let>
                </Test>
            "#},
            "TF,F",
            expect![[r#"
                -- ir --
                Test() {
                  write("TF,F")
                }
                -- expected output --
                TF,F
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_equality_in_let_expr() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Test>
                  <let {color: Color = Color::Green}>
                    <let {is_red: Bool = color == Color::Red}>
                      {match is_red {true => "eq", false => "not eq"}}
                    </let>
                  </let>
                </Test>
            "#},
            "not eq",
            expect![[r#"
                -- ir --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  write("not eq")
                }
                -- expected output --
                not eq
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_float_simple() {
        check(
            indoc! {r#"
                <Test>
                  <let {count: Int = 42}>
                    <let {result: Float = count.to_float() + 0.5}>
                      {result.to_string()}
                    </let>
                  </let>
                </Test>
            "#},
            "42.5",
            expect![[r#"
                -- ir --
                Test() {
                  let count = 42 in {
                    let result = (count.to_float() + 0.5) in {
                      write_escaped(result.to_string())
                    }
                  }
                }
                -- expected output --
                42.5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_float_in_addition() {
        check(
            indoc! {r#"
                <Test>
                  <let {count: Int = 5}>
                    <let {rate: Float = 0.5}>
                      <let {result: Float = count.to_float() + rate}>
                        {result.to_string()}
                      </let>
                    </let>
                  </let>
                </Test>
            "#},
            "5.5",
            expect![[r#"
                -- ir --
                Test() {
                  let count = 5 in {
                    let rate = 0.5 in {
                      let result = (count.to_float() + rate) in {
                        write_escaped(result.to_string())
                      }
                    }
                  }
                }
                -- expected output --
                5.5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_negative() {
        check(
            indoc! {r#"
                <Test>
                  <let {num: Int = -123}>
                    {num.to_string()}
                  </let>
                </Test>
            "#},
            "-123",
            expect![[r#"
                -- ir --
                Test() {
                  let num = (-123) in {
                    write_escaped(num.to_string())
                  }
                }
                -- expected output --
                -123
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_negative() {
        check(
            indoc! {r#"
                <Test>
                  <let {temp: Float = -2.9}>
                    {temp.to_int().to_string()}
                  </let>
                </Test>
            "#},
            "-2",
            expect![[r#"
                -- ir --
                Test() {
                  let temp = (-2.9) in {
                    write_escaped(temp.to_int().to_string())
                  }
                }
                -- expected output --
                -2
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn simple_html() {
        check(
            indoc! {r#"
                <Test>
                  <h1>
                    Hello, World!
                  </h1>
                </Test>
            "#},
            "<h1>Hello, World!</h1>",
            expect![[r#"
                -- ir --
                Test() {
                  write("<h1>Hello, World!</h1>")
                }
                -- expected output --
                <h1>Hello, World!</h1>
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn with_let_binding() {
        check(
            indoc! {r#"
                <Test>
                  <let {name: String = "Alice"}>
                    Hello, {name}!
                  </let>
                </Test>
            "#},
            "Hello, Alice!",
            expect![[r#"
                -- ir --
                Test() {
                  write("Hello, Alice!")
                }
                -- expected output --
                Hello, Alice!
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn conditional() {
        check(
            indoc! {r#"
                <Test>
                  <let {show: Bool = true}>
                    <if {show}>
                      Visible
                    </if>
                    <if {!show}>
                      Hidden
                    </if>
                  </let>
                </Test>
            "#},
            "Visible",
            expect![[r#"
                -- ir --
                Test() {
                  write("Visible")
                }
                -- expected output --
                Visible
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn if_else() {
        check(
            indoc! {r#"
                <Test>
                  <let {show: Bool = true}>
                    <match {show}>
                      <case {true}>
                        True branch
                      </case>
                      <case {false}>
                        False branch
                      </case>
                    </match>
                  </let>
                  <let {hide: Bool = false}>
                    <match {hide}>
                      <case {true}>
                        Should not appear
                      </case>
                      <case {false}>
                        False branch
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "True branchFalse branch",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = true in {
                    match match_subject {
                      true => {
                        write("True branch")
                      }
                      false => {
                        write("False branch")
                      }
                    }
                  }
                  let match_subject_1 = false in {
                    match match_subject_1 {
                      true => {
                        write("Should not appear")
                      }
                      false => {
                        write("False branch")
                      }
                    }
                  }
                }
                -- expected output --
                True branchFalse branch
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop() {
        check(
            indoc! {r#"
                <Test>
                  <for {item in ["a", "b", "c"]}>
                    {item},
                  </for>
                </Test>
            "#},
            "a,b,c,",
            expect![[r#"
                -- ir --
                Test() {
                  for item in ["a", "b", "c"] {
                    write_escaped(item)
                    write(",")
                  }
                }
                -- expected output --
                a,b,c,
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range() {
        check(
            indoc! {r#"
                <Test>
                  <for {i in 1..=3}>
                    {i.to_string()},
                  </for>
                </Test>
            "#},
            "1,2,3,",
            expect![[r#"
                -- ir --
                Test() {
                  for i in 1..=3 {
                    write_escaped(i.to_string())
                    write(",")
                  }
                }
                -- expected output --
                1,2,3,
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range_zero_to_five() {
        check(
            indoc! {r#"
                <Test>
                  <for {x in 0..=5}>
                    {x.to_string()}
                  </for>
                </Test>
            "#},
            "012345",
            expect![[r#"
                -- ir --
                Test() {
                  for x in 0..=5 {
                    write_escaped(x.to_string())
                  }
                }
                -- expected output --
                012345
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range_nested() {
        check(
            indoc! {r#"
                <Test>
                  <for {i in 1..=2}>
                    <for {j in 1..=2}>
                      ({i.to_string()},{j.to_string()})
                    </for>
                  </for>
                </Test>
            "#},
            "(1,1)(1,2)(2,1)(2,2)",
            expect![[r#"
                -- ir --
                Test() {
                  for i in 1..=2 {
                    for j in 1..=2 {
                      write("(")
                      write_escaped(i.to_string())
                      write(",")
                      write_escaped(j.to_string())
                      write(")")
                    }
                  }
                }
                -- expected output --
                (1,1)(1,2)(2,1)(2,2)
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn html_escaping() {
        check(
            indoc! {r#"
                <Test>
                  <let {text: String = "<div>Hello & world</div>"}>
                    {text}
                  </let>
                </Test>
            "#},
            "&lt;div&gt;Hello &amp; world&lt;/div&gt;",
            expect![[r#"
                -- ir --
                Test() {
                  write("&lt;div&gt;Hello &amp; world&lt;/div&gt;")
                }
                -- expected output --
                &lt;div&gt;Hello &amp; world&lt;/div&gt;
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn let_binding() {
        check(
            indoc! {r#"
                <Test>
                  <let {message: String = "Hello from let"}>
                    {message}
                  </let>
                </Test>
            "#},
            "Hello from let",
            expect![[r#"
                -- ir --
                Test() {
                  write("Hello from let")
                }
                -- expected output --
                Hello from let
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concatenation() {
        check(
            indoc! {r#"
                <Test>
                  <let {first: String = "Hello"}>
                    <let {second: String = " World"}>
                      {first + second}
                    </let>
                  </let>
                </Test>
            "#},
            "Hello World",
            expect![[r#"
                -- ir --
                Test() {
                  write("Hello World")
                }
                -- expected output --
                Hello World
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn complex_nested_structure() {
        check(
            indoc! {r#"
                <Test>
                  <for {item in ["A", "B"]}>
                    <let {prefix: String = "["}>
                      {prefix}{item}]
                    </let>
                  </for>
                </Test>
            "#},
            "[A][B]",
            expect![[r#"
                -- ir --
                Test() {
                  for item in ["A", "B"] {
                    write("[")
                    write_escaped(item)
                    write("]")
                  }
                }
                -- expected output --
                [A][B]
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concat_equality() {
        check(
            indoc! {r#"
                <Test>
                  <if {"foo" + "bar" == "foobar"}>
                    equals
                  </if>
                </Test>
            "#},
            "equals",
            expect![[r#"
                -- ir --
                Test() {
                  write("equals")
                }
                -- expected output --
                equals
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_comparison() {
        check(
            indoc! {r#"
                <Test>
                  <if {3 < 5}>
                    3 &lt; 5
                  </if>
                  <if {10 < 2}>
                    10 &lt; 2
                  </if>
                </Test>
            "#},
            "3 &lt; 5",
            expect![[r#"
                -- ir --
                Test() {
                  if (3 < 5) {
                    write("3 &lt; 5")
                  }
                  if (10 < 2) {
                    write("10 &lt; 2")
                  }
                }
                -- expected output --
                3 &lt; 5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_float_comparison() {
        check(
            indoc! {r#"
                <Test>
                  <if {1.5 < 2.5}>
                    1.5 &lt; 2.5
                  </if>
                  <if {3.0 < 1.0}>
                    3.0 &lt; 1.0
                  </if>
                </Test>
            "#},
            "1.5 &lt; 2.5",
            expect![[r#"
                -- ir --
                Test() {
                  if (1.5 < 2.5) {
                    write("1.5 &lt; 2.5")
                  }
                  if (3 < 1) {
                    write("3.0 &lt; 1.0")
                  }
                }
                -- expected output --
                1.5 &lt; 2.5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_equality_comparison() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Test>
                  <let {color: Color = Color::Red}>
                    <match {color == Color::Red}>
                      <case {true}>
                        equal
                      </case>
                      <case {false}>
                        not equal
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "equal",
            expect![[r#"
                -- ir --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let match_subject = true in {
                    match match_subject {
                      true => {
                        write("equal")
                      }
                      false => {
                        write("not equal")
                      }
                    }
                  }
                }
                -- expected output --
                equal
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_equality_different_variants() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Test>
                  <let {color: Color = Color::Red}>
                    <match {color == Color::Green}>
                      <case {true}>
                        equal
                      </case>
                      <case {false}>
                        not equal
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "not equal",
            expect![[r#"
                -- ir --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let match_subject = false in {
                    match match_subject {
                      true => {
                        write("equal")
                      }
                      false => {
                        write("not equal")
                      }
                    }
                  }
                }
                -- expected output --
                not equal
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_true() {
        check(
            indoc! {r#"
                <Test>
                  <let {flag: Bool = true}>
                    <match {flag}>
                      <case {true}>
                        yes
                      </case>
                      <case {false}>
                        no
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "yes",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = true in {
                    match match_subject {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                }
                -- expected output --
                yes
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_false() {
        check(
            indoc! {r#"
                <Test>
                  <let {flag: Bool = false}>
                    <match {flag}>
                      <case {true}>
                        yes
                      </case>
                      <case {false}>
                        no
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "no",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = false in {
                    match match_subject {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                }
                -- expected output --
                no
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn field_access() {
        check(
            indoc! {r#"
                record Person {
                  name: String,
                  age: Int,
                }

                <Test>
                  <let {person: Person = Person(name: "Alice", age: 30)}>
                    {person.name}
                    <if {person.age == 30}>
                      :30
                    </if>
                  </let>
                </Test>
            "#},
            "Alice:30",
            expect![[r#"
                -- ir --
                record Person {
                  name: String,
                  age: Int,
                }
                Test() {
                  let person = Person(name: "Alice", age: 30) in {
                    write_escaped(person.name)
                    if (person.age == 30) {
                      write(":30")
                    }
                  }
                }
                -- expected output --
                Alice:30
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn record_literal() {
        check(
            indoc! {r#"
                record Item {
                  label: String,
                  count: Int,
                  active: Bool,
                }

                <Test>
                  <let {
                    item: Item = Item(
                      label: "widget",
                      count: 5,
                      active: true,
                    ),
                  }>
                    {item.label}
                    <if {item.count == 5}>
                      ,5
                    </if>
                    ,
                    <if {item.active}>
                      active
                    </if>
                  </let>
                </Test>
            "#},
            "widget,5,active",
            expect![[r#"
                -- ir --
                record Item {
                  label: String,
                  count: Int,
                  active: Bool,
                }
                Test() {
                  let item = Item(
                    label: "widget",
                    count: 5,
                    active: true,
                  ) in {
                    write_escaped(item.label)
                    if (item.count == 5) {
                      write(",5")
                    }
                    write(",")
                    if item.active {
                      write("active")
                    }
                  }
                }
                -- expected output --
                widget,5,active
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_record() {
        check(
            indoc! {r#"
                record Address {
                  city: String,
                  zip: String,
                }

                record Person {
                  name: String,
                  address: Address,
                }

                <Test>
                  <let {
                    person: Person = Person(
                      name: "Alice",
                      address: Address(city: "Paris", zip: "75001"),
                    ),
                  }>
                    {person.name},{person.address.city}
                  </let>
                </Test>
            "#},
            "Alice,Paris",
            expect![[r#"
                -- ir --
                record Address {
                  city: String,
                  zip: String,
                }
                record Person {
                  name: String,
                  address: Address,
                }
                Test() {
                  let person = Person(
                    name: "Alice",
                    address: Address(city: "Paris", zip: "75001"),
                  ) in {
                    write_escaped(person.name)
                    write(",")
                    write_escaped(person.address.city)
                  }
                }
                -- expected output --
                Alice,Paris
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn record_with_enum_field() {
        check(
            indoc! {r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                record User {
                  name: String,
                  status: Status,
                }

                <Test>
                  <let {
                    user: User = User(
                      name: "Alice",
                      status: Status::Active,
                    ),
                  }>
                    {user.name}:
                    <if {user.status == Status::Active}>
                      active
                    </if>
                  </let>
                </Test>
            "#},
            "Alice:active",
            expect![[r#"
                -- ir --
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }
                record User {
                  name: String,
                  status: Status,
                }
                Test() {
                  let user = User(
                    name: "Alice",
                    status: Status::Active,
                  ) in {
                    write_escaped(user.name)
                    write(":")
                    if (user.status == Status::Active) {
                      write("active")
                    }
                  }
                }
                -- expected output --
                Alice:active
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_add() {
        check(
            indoc! {r#"
                <Test>
                  <let {a: Int = 3}>
                    <let {b: Int = 7}>
                      <if {a + b == 10}>
                        correct
                      </if>
                    </let>
                  </let>
                </Test>
            "#},
            "correct",
            expect![[r#"
                -- ir --
                Test() {
                  let a = 3 in {
                    let b = 7 in {
                      if ((a + b) == 10) {
                        write("correct")
                      }
                    }
                  }
                }
                -- expected output --
                correct
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_subtract() {
        check(
            indoc! {r#"
                <Test>
                  <let {a: Int = 10}>
                    <let {b: Int = 3}>
                      <if {a - b == 7}>
                        correct
                      </if>
                    </let>
                  </let>
                </Test>
            "#},
            "correct",
            expect![[r#"
                -- ir --
                Test() {
                  let a = 10 in {
                    let b = 3 in {
                      if ((a - b) == 7) {
                        write("correct")
                      }
                    }
                  }
                }
                -- expected output --
                correct
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_multiply() {
        check(
            indoc! {r#"
                <Test>
                  <let {a: Int = 4}>
                    <let {b: Int = 5}>
                      <if {a * b == 20}>
                        correct
                      </if>
                    </let>
                  </let>
                </Test>
            "#},
            "correct",
            expect![[r#"
                -- ir --
                Test() {
                  let a = 4 in {
                    let b = 5 in {
                      if ((a * b) == 20) {
                        write("correct")
                      }
                    }
                  }
                }
                -- expected output --
                correct
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boolean_logical_and() {
        check(
            indoc! {r#"
                <Test>
                  <let {a: Bool = true}>
                    <let {b: Bool = true}>
                      <if {a && b}>
                        TT
                      </if>
                    </let>
                  </let>
                  <let {c: Bool = true}>
                    <let {d: Bool = false}>
                      <if {c && d}>
                        TF
                      </if>
                    </let>
                  </let>
                </Test>
            "#},
            "TT",
            expect![[r#"
                -- ir --
                Test() {
                  write("TT")
                }
                -- expected output --
                TT
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boolean_logical_or() {
        check(
            indoc! {r#"
                <Test>
                  <let {a: Bool = false}>
                    <let {b: Bool = true}>
                      <if {a || b}>
                        FT
                      </if>
                    </let>
                  </let>
                  <let {c: Bool = false}>
                    <let {d: Bool = false}>
                      <if {c || d}>
                        FF
                      </if>
                    </let>
                  </let>
                </Test>
            "#},
            "FT",
            expect![[r#"
                -- ir --
                Test() {
                  write("FT")
                }
                -- expected output --
                FT
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_or_equal() {
        check(
            indoc! {r#"
                <Test>
                  <if {3 <= 5}>
                    A
                  </if>
                  <if {5 <= 5}>
                    B
                  </if>
                  <if {7 <= 5}>
                    C
                  </if>
                </Test>
            "#},
            "AB",
            expect![[r#"
                -- ir --
                Test() {
                  if (3 <= 5) {
                    write("A")
                  }
                  if (5 <= 5) {
                    write("B")
                  }
                  if (7 <= 5) {
                    write("C")
                  }
                }
                -- expected output --
                AB
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal() {
        check(
            indoc! {r#"
                <Test>
                  <let {some_val: Option[String] = Some("hello")}>
                    <match {some_val}>
                      <case {Some(val)}>
                        Some:{val}
                      </case>
                      <case {None}>
                        None
                      </case>
                    </match>
                  </let>
                  <let {none_val: Option[String] = None}>
                    <match {none_val}>
                      <case {Some(val)}>
                        Some:{val}
                      </case>
                      <case {None}>
                        ,None
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "Some:hello,None",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[String]::Some("hello") in {
                    match match_subject {
                      Some(v0) => {
                        let val = v0 in {
                          write("Some:")
                          write_escaped(val)
                        }
                      }
                      None => {
                        write("None")
                      }
                    }
                  }
                  let match_subject_1 = Option[String]::None in {
                    match match_subject_1 {
                      Some(v0_2) => {
                        let val_3 = v0_2 in {
                          write("Some:")
                          write_escaped(val_3)
                        }
                      }
                      None => {
                        write(",None")
                      }
                    }
                  }
                }
                -- expected output --
                Some:hello,None
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal_inline_match() {
        check(
            indoc! {r#"
                <Test>
                  <let {opt1: Option[String] = Some("world")}>
                    <match {opt1}>
                      <case {Some(val)}>
                        Got:{val}
                      </case>
                      <case {None}>
                        Empty
                      </case>
                    </match>
                  </let>
                  ,
                  <let {opt2: Option[String] = None}>
                    <match {opt2}>
                      <case {Some(val)}>
                        Got:{val}
                      </case>
                      <case {None}>
                        Empty
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "Got:world,Empty",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[String]::Some("world") in {
                    match match_subject {
                      Some(v0) => {
                        let val = v0 in {
                          write("Got:")
                          write_escaped(val)
                        }
                      }
                      None => {
                        write("Empty")
                      }
                    }
                  }
                  write(",")
                  let match_subject_1 = Option[String]::None in {
                    match match_subject_1 {
                      Some(v0_2) => {
                        let val_3 = v0_2 in {
                          write("Got:")
                          write_escaped(val_3)
                        }
                      }
                      None => {
                        write("Empty")
                      }
                    }
                  }
                }
                -- expected output --
                Got:world,Empty
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_wildcard_pattern() {
        check(
            indoc! {r#"
                <Test>
                  <let {opt1: Option[String] = Some("hello")}>
                    <match {opt1}>
                      <case {Some(_)}>
                        is-some
                      </case>
                      <case {None}>
                        is-none
                      </case>
                    </match>
                  </let>
                  ,
                  <let {opt2: Option[String] = None}>
                    <match {opt2}>
                      <case {Some(_)}>
                        IS-SOME
                      </case>
                      <case {None}>
                        IS-NONE
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "is-some,IS-NONE",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[String]::Some("hello") in {
                    match match_subject {
                      Some(_) => {
                        write("is-some")
                      }
                      None => {
                        write("is-none")
                      }
                    }
                  }
                  write(",")
                  let match_subject_1 = Option[String]::None in {
                    match match_subject_1 {
                      Some(_) => {
                        write("IS-SOME")
                      }
                      None => {
                        write("IS-NONE")
                      }
                    }
                  }
                }
                -- expected output --
                is-some,IS-NONE
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_nested_constant_folding() {
        check(
            indoc! {r#"
                <Test>
                  <let {inner_opt: Option[String] = Some("inner")}>
                    <let {
                      outer: Option[String] = Some(
                        match inner_opt {Some(x) => x, None => "default"}
                      ),
                    }>
                      <match {outer}>
                        <case {Some(val)}>
                          {val}
                        </case>
                        <case {None}>
                          none
                        </case>
                      </match>
                    </let>
                  </let>
                </Test>
            "#},
            "inner",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[String]::Some("inner") in {
                    match match_subject {
                      Some(_) => {
                        write("inner")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                inner
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_array_for_loop() {
        check(
            indoc! {r#"
                <Test>
                  <for {item in [Some("a"), None, Some("b")]}>
                    <match {item}>
                      <case {Some(val)}>
                        [{val}]
                      </case>
                      <case {None}>
                        [_]
                      </case>
                    </match>
                  </for>
                </Test>
            "#},
            "[a][_][b]",
            expect![[r#"
                -- ir --
                Test() {
                  for item in [
                    Option[String]::Some("a"),
                    Option[String]::None,
                    Option[String]::Some("b"),
                  ] {
                    let match_subject = item in {
                      match match_subject {
                        Some(v0) => {
                          let val = v0 in {
                            write("[")
                            write_escaped(val)
                            write("]")
                          }
                        }
                        None => {
                          write("[_]")
                        }
                      }
                    }
                  }
                }
                -- expected output --
                [a][_][b]
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_expression() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Test>
                  <let {color: Color = Color::Green}>
                    <match {color}>
                      <case {Color::Red}>
                        red
                      </case>
                      <case {Color::Green}>
                        green
                      </case>
                      <case {Color::Blue}>
                        blue
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "green",
            expect![[r#"
                -- ir --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let match_subject = Color::Green in {
                    match match_subject {
                      Color::Red => {
                        write("red")
                      }
                      Color::Green => {
                        write("green")
                      }
                      Color::Blue => {
                        write("blue")
                      }
                    }
                  }
                }
                -- expected output --
                green
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_statement() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Test>
                  <let {color: Color = Color::Blue}>
                    <match {color}>
                      <case {Color::Red}>
                        red
                      </case>
                      <case {Color::Green}>
                        green
                      </case>
                      <case {Color::Blue}>
                        blue
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "blue",
            expect![[r#"
                -- ir --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let match_subject = Color::Blue in {
                    match match_subject {
                      Color::Red => {
                        write("red")
                      }
                      Color::Green => {
                        write("green")
                      }
                      Color::Blue => {
                        write("blue")
                      }
                    }
                  }
                }
                -- expected output --
                blue
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_with_fields_literal() {
        check(
            indoc! {r#"
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }

                <Test>
                  <let {ok: Result = Result::Ok(value: "success")}>
                    <match {ok}>
                      <case {Result::Ok(value: v)}>
                        created:{v}
                      </case>
                      <case {Result::Err(message: m)}>
                        error:{m}
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "created:success",
            expect![[r#"
                -- ir --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let match_subject = Result::Ok(value: "success") in {
                    match match_subject {
                      Result::Ok(value: v0) => {
                        let v = v0 in {
                          write("created:")
                          write_escaped(v)
                        }
                      }
                      Result::Err(message: v1) => {
                        let m = v1 in {
                          write("error:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                created:success
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_with_field_bindings() {
        check(
            indoc! {r#"
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }

                <Test>
                  <let {result: Result = Result::Ok(value: "hello")}>
                    <match {result}>
                      <case {Result::Ok(value: v)}>
                        Ok:{v}
                      </case>
                      <case {Result::Err(message: m)}>
                        Err:{m}
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "Ok:hello",
            expect![[r#"
                -- ir --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let match_subject = Result::Ok(value: "hello") in {
                    match match_subject {
                      Result::Ok(value: v0) => {
                        let v = v0 in {
                          write("Ok:")
                          write_escaped(v)
                        }
                      }
                      Result::Err(message: v1) => {
                        let m = v1 in {
                          write("Err:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                Ok:hello
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_err_variant_with_bindings() {
        check(
            indoc! {r#"
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }

                <Test>
                  <let {
                    result: Result = Result::Err(message: "something went wrong"),
                  }>
                    <match {result}>
                      <case {Result::Ok(value: v)}>
                        Ok:{v}
                      </case>
                      <case {Result::Err(message: m)}>
                        Err:{m}
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "Err:something went wrong",
            expect![[r#"
                -- ir --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let match_subject = Result::Err(message: "something went wrong") in {
                    match match_subject {
                      Result::Ok(value: v0) => {
                        let v = v0 in {
                          write("Ok:")
                          write_escaped(v)
                        }
                      }
                      Result::Err(message: v1) => {
                        let m = v1 in {
                          write("Err:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                Err:something went wrong
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_with_multiple_fields() {
        check(
            indoc! {r#"
                enum Response {
                  Success(code: String, body: String),
                  Error(reason: String),
                }

                <Test>
                  <let {
                    resp: Response = Response::Success(code: "200", body: "OK"),
                  }>
                    <match {resp}>
                      <case {Response::Success(code: c, body: b)}>
                        {c}:{b}
                      </case>
                      <case {Response::Error(reason: r)}>
                        Error:{r}
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "200:OK",
            expect![[r#"
                -- ir --
                enum Response {
                  Success(code: String, body: String),
                  Error(reason: String),
                }
                Test() {
                  let match_subject = Response::Success(code: "200", body: "OK") in {
                    match match_subject {
                      Response::Success(code: v0, body: v1) => {
                        let c = v0 in {
                          let b = v1 in {
                            write_escaped(c)
                            write(":")
                            write_escaped(b)
                          }
                        }
                      }
                      Response::Error(reason: v2) => {
                        let r = v2 in {
                          write("Error:")
                          write_escaped(r)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                200:OK
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_simple() {
        check(
            indoc! {r#"
                <Test>
                  <let {items: Array[String] = ["a", "b", "c"]}>
                    {items.len().to_string()}
                  </let>
                </Test>
            "#},
            "3",
            expect![[r#"
                -- ir --
                Test() {
                  let items = ["a", "b", "c"] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- expected output --
                3
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_empty() {
        check(
            indoc! {r#"
                <Test>
                  <let {items: Array[String] = []}>
                    {items.len().to_string()}
                  </let>
                </Test>
            "#},
            "0",
            expect![[r#"
                -- ir --
                Test() {
                  let items = [] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- expected output --
                0
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_in_comparison() {
        check(
            indoc! {r#"
                <Test>
                  <let {items: Array[String] = ["x", "y"]}>
                    <if {items.len() == 2}>
                      has two
                    </if>
                  </let>
                </Test>
            "#},
            "has two",
            expect![[r#"
                -- ir --
                Test() {
                  let items = ["x", "y"] in {
                    if (items.len() == 2) {
                      write("has two")
                    }
                  }
                }
                -- expected output --
                has two
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_less_than() {
        check(
            indoc! {r#"
                <Test>
                  <let {items: Array[String] = ["a"]}>
                    <if {items.len() < 5}>
                      less than 5
                    </if>
                  </let>
                </Test>
            "#},
            "less than 5",
            expect![[r#"
                -- ir --
                Test() {
                  let items = ["a"] in {
                    if (items.len() < 5) {
                      write("less than 5")
                    }
                  }
                }
                -- expected output --
                less than 5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_int_array() {
        check(
            indoc! {r#"
                <Test>
                  <let {numbers: Array[Int] = [1, 2, 3, 4, 5]}>
                    {numbers.len().to_string()}
                  </let>
                </Test>
            "#},
            "5",
            expect![[r#"
                -- ir --
                Test() {
                  let numbers = [1, 2, 3, 4, 5] in {
                    write_escaped(numbers.len().to_string())
                  }
                }
                -- expected output --
                5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_simple() {
        check(
            indoc! {r#"
                <Test>
                  <let {count: Int = 42}>
                    {count.to_string()}
                  </let>
                </Test>
            "#},
            "42",
            expect![[r#"
                -- ir --
                Test() {
                  let count = 42 in {
                    write_escaped(count.to_string())
                  }
                }
                -- expected output --
                42
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_zero() {
        check(
            indoc! {r#"
                <Test>
                  <let {num: Int = 0}>
                    {num.to_string()}
                  </let>
                </Test>
            "#},
            "0",
            expect![[r#"
                -- ir --
                Test() {
                  let num = 0 in {
                    write_escaped(num.to_string())
                  }
                }
                -- expected output --
                0
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_concat() {
        check(
            indoc! {r#"
                <Test>
                  <let {count: Int = 5}>
                    {"Count: " + count.to_string()}
                  </let>
                </Test>
            "#},
            "Count: 5",
            expect![[r#"
                -- ir --
                Test() {
                  let count = 5 in {
                    write_escaped(("Count: " + count.to_string()))
                  }
                }
                -- expected output --
                Count: 5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_simple() {
        check(
            indoc! {r#"
                <Test>
                  <let {price: Float = 3.7}>
                    {price.to_int().to_string()}
                  </let>
                </Test>
            "#},
            "3",
            expect![[r#"
                -- ir --
                Test() {
                  let price = 3.7 in {
                    write_escaped(price.to_int().to_string())
                  }
                }
                -- expected output --
                3
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_whole_number() {
        check(
            indoc! {r#"
                <Test>
                  <let {val: Float = 5.0}>
                    {val.to_int().to_string()}
                  </let>
                </Test>
            "#},
            "5",
            expect![[r#"
                -- ir --
                Test() {
                  let val = 5 in {
                    write_escaped(val.to_int().to_string())
                  }
                }
                -- expected output --
                5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_string_simple() {
        check(
            indoc! {r#"
                <Test>
                  <let {price: Float = 19.99}>
                    {price.to_string()}
                  </let>
                </Test>
            "#},
            "19.99",
            expect![[r#"
                -- ir --
                Test() {
                  let price = 19.99 in {
                    write_escaped(price.to_string())
                  }
                }
                -- expected output --
                19.99
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_string_concat() {
        check(
            indoc! {r#"
                <Test>
                  <let {price: Float = 9.99}>
                    {"$" + price.to_string()}
                  </let>
                </Test>
            "#},
            "$9.99",
            expect![[r#"
                -- ir --
                Test() {
                  let price = 9.99 in {
                    write_escaped(("$" + price.to_string()))
                  }
                }
                -- expected output --
                $9.99
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_range() {
        check(
            indoc! {r#"
                <Test>
                  <for {_ in 0..=2}>
                    x
                  </for>
                </Test>
            "#},
            "xxx",
            expect![[r#"
                -- ir --
                Test() {
                  for _ in 0..=2 {
                    write("x")
                  }
                }
                -- expected output --
                xxx
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_array() {
        check(
            indoc! {r#"
                <Test>
                  <let {items: Array[String] = ["a", "b", "c"]}>
                    <for {_ in items}>
                      *
                    </for>
                  </let>
                </Test>
            "#},
            "***",
            expect![[r#"
                -- ir --
                Test() {
                  let items = ["a", "b", "c"] in {
                    for _ in items {
                      write("*")
                    }
                  }
                }
                -- expected output --
                ***
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_nested() {
        check(
            indoc! {r#"
                <Test>
                  <for {_ in 0..=1}>
                    <for {_ in 0..=2}>
                      .
                    </for>
                  </for>
                </Test>
            "#},
            "......",
            expect![[r#"
                -- ir --
                Test() {
                  for _ in 0..=1 {
                    for _ in 0..=2 {
                      write(".")
                    }
                  }
                }
                -- expected output --
                ......
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_mixed_with_named() {
        check(
            indoc! {r#"
                <Test>
                  <for {i in 1..=2}>
                    <for {_ in 0..=1}>
                      {i.to_string()}
                    </for>
                  </for>
                </Test>
            "#},
            "1122",
            expect![[r#"
                -- ir --
                Test() {
                  for i in 1..=2 {
                    for _ in 0..=1 {
                      write_escaped(i.to_string())
                    }
                  }
                }
                -- expected output --
                1122
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_float_literal() {
        check(
            indoc! {r#"
                <Test>
                  {3.14.to_string()}
                </Test>
            "#},
            "3.14",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped(3.14.to_string())
                }
                -- expected output --
                3.14
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_array_literal() {
        check(
            indoc! {r#"
                <Test>
                  {[1, 2, 3].len().to_string()}
                </Test>
            "#},
            "3",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped([1, 2, 3].len().to_string())
                }
                -- expected output --
                3
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_parenthesized_expression() {
        check(
            indoc! {r#"
                <Test>
                  {(1 + 2).to_string()}
                </Test>
            "#},
            "3",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped((1 + 2).to_string())
                }
                -- expected output --
                3
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_literal_to_string() {
        check(
            indoc! {r#"
                <Test>
                  {42.to_string()}
                </Test>
            "#},
            "42",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped(42.to_string())
                }
                -- expected output --
                42
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn negated_int_to_string() {
        check(
            indoc! {r#"
                <Test>
                  {(-42).to_string()}
                </Test>
            "#},
            "-42",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped((-42).to_string())
                }
                -- expected output --
                -42
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_literal_to_string() {
        check(
            indoc! {r#"
                <Test>
                  {5.0.to_string()}
                </Test>
            "#},
            "5",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped(5.to_string())
                }
                -- expected output --
                5
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn negated_float_to_string() {
        check(
            indoc! {r#"
                <Test>
                  {(-3.14).to_string()}
                </Test>
            "#},
            "-3.14",
            expect![[r#"
                -- ir --
                Test() {
                  write_escaped((-3.14).to_string())
                }
                -- expected output --
                -3.14
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_option_from_hop_syntax() {
        check(
            indoc! {r#"
                <Test>
                  <let {
                    nested1: Option[Option[String]] = Some(Some("deep")),
                  }>
                    <match {nested1}>
                      <case {Some(Some(x))}>
                        {x}
                      </case>
                      <case {Some(None)}>
                        some-none
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                  ,
                  <let {nested2: Option[Option[String]] = Some(None)}>
                    <match {nested2}>
                      <case {Some(Some(x))}>
                        {x}
                      </case>
                      <case {Some(None)}>
                        some-none
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                  ,
                  <let {nested3: Option[Option[String]] = None}>
                    <match {nested3}>
                      <case {Some(Some(x))}>
                        {x}
                      </case>
                      <case {Some(None)}>
                        some-none
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                </Test>
            "#},
            "deep,some-none,none",
            expect![[r#"
                -- ir --
                Test() {
                  let match_subject = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match match_subject {
                      Some(v0) => {
                        match v0 {
                          Some(v1) => {
                            let x = v1 in {
                              write_escaped(x)
                            }
                          }
                          None => {
                            write("some-none")
                          }
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                  write(",")
                  let match_subject_1 = Option[Option[String]]::Some(Option[String]::None) in {
                    match match_subject_1 {
                      Some(v0_2) => {
                        match v0_2 {
                          Some(v1_3) => {
                            let x_4 = v1_3 in {
                              write_escaped(x_4)
                            }
                          }
                          None => {
                            write("some-none")
                          }
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                  write(",")
                  let match_subject_5 = Option[Option[String]]::None in {
                    match match_subject_5 {
                      Some(v0_6) => {
                        match v0_6 {
                          Some(v1_7) => {
                            let x_8 = v1_7 in {
                              write_escaped(x_8)
                            }
                          }
                          None => {
                            write("some-none")
                          }
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                deep,some-none,none
                -- ts --
                OK
                -- go --
                OK
                -- python --
                OK
            "#]],
        );
    }
}
