use super::transpile::{GoTranspiler, PythonTranspiler, Transpiler, TsTranspiler};
use crate::document::Document;
use crate::hop::program::Program;
use crate::hop::syntax::format;
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
import { Test } from './module.ts';
console.log(Test());
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

    // Compile to IR without optimization
    let unoptimized_options = OrchestrateOptions {
        skip_html_structure: true,
        skip_dev_mode_wrapper: true,
        skip_optimization: true,
    };
    let unoptimized_module = orchestrate(typed_asts, None, unoptimized_options);

    // Compile to IR with optimization
    let optimized_options = OrchestrateOptions {
        skip_html_structure: true,
        skip_dev_mode_wrapper: true,
        skip_optimization: false,
    };
    let optimized_module = orchestrate(typed_asts, None, optimized_options);

    let unoptimized_ir = unoptimized_module.to_string();
    let optimized_ir = optimized_module.to_string();

    let mut output = format!(
        "-- ir (unoptimized) --\n{}-- ir (optimized) --\n{}-- expected output --\n{}\n",
        unoptimized_ir, optimized_ir, expected_output
    );

    // Test unoptimized version
    let ts_transpiler = TsTranspiler::new();
    let ts_code = ts_transpiler.transpile_module(&unoptimized_module);
    if let Err(e) = typecheck_typescript(&ts_code) {
        panic!("TypeScript typecheck failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, unoptimized_ir, ts_code);
    }
    let ts_output = match execute_typescript(&ts_code) {
        Ok(out) => out,
        Err(e) => panic!("TypeScript execution failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, unoptimized_ir, ts_code),
    };
    assert_eq!(ts_output, expected_output, "TypeScript output mismatch (unoptimized)\n\nIR:\n{}\nGenerated code:\n{}", unoptimized_ir, ts_code);
    output.push_str("-- ts (unoptimized) --\nOK\n");

    let go_transpiler = GoTranspiler::new("components".to_string());
    let go_code = go_transpiler.transpile_module(&unoptimized_module);
    if let Err(e) = typecheck_go(&go_code) {
        panic!("Go typecheck failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, unoptimized_ir, go_code);
    }
    let go_output = match execute_go(&go_code) {
        Ok(out) => out,
        Err(e) => panic!("Go execution failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, unoptimized_ir, go_code),
    };
    assert_eq!(go_output, expected_output, "Go output mismatch (unoptimized)\n\nIR:\n{}\nGenerated code:\n{}", unoptimized_ir, go_code);
    output.push_str("-- go (unoptimized) --\nOK\n");

    let python_transpiler = PythonTranspiler::new();
    let python_code = python_transpiler.transpile_module(&unoptimized_module);
    if let Err(e) = typecheck_python(&python_code) {
        panic!("Python typecheck failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, unoptimized_ir, python_code);
    }
    let python_output = match execute_python(&python_code) {
        Ok(out) => out,
        Err(e) => panic!("Python execution failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, unoptimized_ir, python_code),
    };
    assert_eq!(python_output, expected_output, "Python output mismatch (unoptimized)\n\nIR:\n{}\nGenerated code:\n{}", unoptimized_ir, python_code);
    output.push_str("-- python (unoptimized) --\nOK\n");

    // Test optimized version
    let ts_code = ts_transpiler.transpile_module(&optimized_module);
    if let Err(e) = typecheck_typescript(&ts_code) {
        panic!("TypeScript typecheck failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, optimized_ir, ts_code);
    }
    let ts_output = match execute_typescript(&ts_code) {
        Ok(out) => out,
        Err(e) => panic!("TypeScript execution failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, optimized_ir, ts_code),
    };
    assert_eq!(ts_output, expected_output, "TypeScript output mismatch (optimized)\n\nIR:\n{}\nGenerated code:\n{}", optimized_ir, ts_code);
    output.push_str("-- ts (optimized) --\nOK\n");

    let go_code = go_transpiler.transpile_module(&optimized_module);
    if let Err(e) = typecheck_go(&go_code) {
        panic!("Go typecheck failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, optimized_ir, go_code);
    }
    let go_output = match execute_go(&go_code) {
        Ok(out) => out,
        Err(e) => panic!("Go execution failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, optimized_ir, go_code),
    };
    assert_eq!(go_output, expected_output, "Go output mismatch (optimized)\n\nIR:\n{}\nGenerated code:\n{}", optimized_ir, go_code);
    output.push_str("-- go (optimized) --\nOK\n");

    let python_code = python_transpiler.transpile_module(&optimized_module);
    if let Err(e) = typecheck_python(&python_code) {
        panic!("Python typecheck failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, optimized_ir, python_code);
    }
    let python_output = match execute_python(&python_code) {
        Ok(out) => out,
        Err(e) => panic!("Python execution failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}", e, optimized_ir, python_code),
    };
    assert_eq!(python_output, expected_output, "Python output mismatch (optimized)\n\nIR:\n{}\nGenerated code:\n{}", optimized_ir, python_code);
    output.push_str("-- python (optimized) --\nOK\n");

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
                entrypoint Test() {
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
                }
            "#},
            "mapped:hello",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let inner = Option[String]::Some("hello") in {
                    let mapped = match inner {
                      Some(v0) => let x = v0 in Option[String]::Some(x),
                      None => Option[String]::None,
                    } in {
                      let match_subject = mapped in {
                        match match_subject {
                          Some(v0) => {
                            let result = v0 in {
                              write("mapped:")
                              write_escaped(result)
                            }
                          }
                          None => {
                            write("was-none")
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_as_some_value() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "inner",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let inner_opt = Option[String]::Some("inner") in {
                    let outer = Option[String]::Some(match inner_opt {
                      Some(v0) => let x = v0 in x,
                      None => "default",
                    }) in {
                      let match_subject = outer in {
                        match match_subject {
                          Some(v0) => {
                            let val = v0 in {
                              write_escaped(val)
                            }
                          }
                          None => {
                            write("none")
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_expr() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {flag: Bool = true}>
                    {match flag {true => "yes", false => "no"}}
                  </let>
                  <let {other: Bool = false}>
                    {match other {true => "YES", false => "NO"}}
                  </let>
                }
            "#},
            "yesNO",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let flag = true in {
                    write_escaped(match flag {true => "yes", false => "no"})
                  }
                  let other = false in {
                    write_escaped(match other {
                      true => "YES",
                      false => "NO",
                    })
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("yesNO")
                }
                -- expected output --
                yesNO
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal_inline_match_expr() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {opt1: Option[String] = Some("hi")}>
                    {match opt1 {Some(_) => "some", None => "none"}}
                  </let>
                  ,
                  <let {opt2: Option[String] = None}>
                    {match opt2 {Some(_) => "SOME", None => "NONE"}}
                  </let>
                }
            "#},
            "some,NONE",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let opt1 = Option[String]::Some("hi") in {
                    write_escaped(match opt1 {
                      Some(_) => "some",
                      None => "none",
                    })
                  }
                  write(",")
                  let opt2 = Option[String]::None in {
                    write_escaped(match opt2 {
                      Some(_) => "SOME",
                      None => "NONE",
                    })
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("some,NONE")
                }
                -- expected output --
                some,NONE
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_bool_match_expr() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "TF,F",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let outer = true in {
                    let inner = false in {
                      write_escaped(match outer {
                        true => match inner {true => "TT", false => "TF"},
                        false => "F",
                      })
                    }
                  }
                  write(",")
                  let outer2 = false in {
                    let inner2 = true in {
                      write_escaped(match outer2 {
                        true => match inner2 {true => "TT", false => "TF"},
                        false => "F",
                      })
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("TF,F")
                }
                -- expected output --
                TF,F
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
                  <let {color: Color = Color::Green}>
                    <let {is_red: Bool = color == Color::Red}>
                      {match is_red {true => "eq", false => "not eq"}}
                    </let>
                  </let>
                }
            "#},
            "not eq",
            expect![[r#"
                -- ir (unoptimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let color = Color::Green in {
                    let is_red = (color == Color::Red) in {
                      write_escaped(match is_red {
                        true => "eq",
                        false => "not eq",
                      })
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_float_simple() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {count: Int = 42}>
                    <let {result: Float = count.to_float() + 0.5}>
                      {result.to_string()}
                    </let>
                  </let>
                }
            "#},
            "42.5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let count = 42 in {
                    let result = (count.to_float() + 0.5) in {
                      write_escaped(result.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let count = 42 in {
                    let result = (count.to_float() + 0.5) in {
                      write_escaped(result.to_string())
                    }
                  }
                }
                -- expected output --
                42.5
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_negative() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {num: Int = -123}>
                    {num.to_string()}
                  </let>
                }
            "#},
            "-123",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let num = (-123) in {
                    write_escaped(num.to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let num = (-123) in {
                    write_escaped(num.to_string())
                  }
                }
                -- expected output --
                -123
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_negative() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {temp: Float = -2.9}>
                    {temp.to_int().to_string()}
                  </let>
                }
            "#},
            "-2",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let temp = (-2.9) in {
                    write_escaped(temp.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let temp = (-2.9) in {
                    write_escaped(temp.to_int().to_string())
                  }
                }
                -- expected output --
                -2
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn simple_html() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <h1>
                    Hello, World!
                  </h1>
                }
            "#},
            "<h1>Hello, World!</h1>",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  write("<h1")
                  write(">")
                  write("Hello, World!")
                  write("</h1>")
                }
                -- ir (optimized) --
                Test() {
                  write("<h1>Hello, World!</h1>")
                }
                -- expected output --
                <h1>Hello, World!</h1>
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn with_let_binding() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {name: String = "Alice"}>
                    Hello, {name}!
                  </let>
                }
            "#},
            "Hello, Alice!",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let name = "Alice" in {
                    write("Hello, ")
                    write_escaped(name)
                    write("!")
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("Hello, Alice!")
                }
                -- expected output --
                Hello, Alice!
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn conditional() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {show: Bool = true}>
                    <if {show}>
                      Visible
                    </if>
                    <if {!show}>
                      Hidden
                    </if>
                  </let>
                }
            "#},
            "Visible",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let show = true in {
                    if show {
                      write("Visible")
                    }
                    if (!show) {
                      write("Hidden")
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("Visible")
                }
                -- expected output --
                Visible
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {item in ["a", "b", "c"]}>
                    {item},
                  </for>
                }
            "#},
            "a,b,c,",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for item in ["a", "b", "c"] {
                    write_escaped(item)
                    write(",")
                  }
                }
                -- ir (optimized) --
                Test() {
                  for item in ["a", "b", "c"] {
                    write_escaped(item)
                    write(",")
                  }
                }
                -- expected output --
                a,b,c,
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {i in 1..=3}>
                    {i.to_string()},
                  </for>
                }
            "#},
            "1,2,3,",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for i in 1..=3 {
                    write_escaped(i.to_string())
                    write(",")
                  }
                }
                -- ir (optimized) --
                Test() {
                  for i in 1..=3 {
                    write_escaped(i.to_string())
                    write(",")
                  }
                }
                -- expected output --
                1,2,3,
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range_zero_to_five() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {x in 0..=5}>
                    {x.to_string()}
                  </for>
                }
            "#},
            "012345",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for x in 0..=5 {
                    write_escaped(x.to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  for x in 0..=5 {
                    write_escaped(x.to_string())
                  }
                }
                -- expected output --
                012345
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range_nested() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {i in 1..=2}>
                    <for {j in 1..=2}>
                      ({i.to_string()},{j.to_string()})
                    </for>
                  </for>
                }
            "#},
            "(1,1)(1,2)(2,1)(2,2)",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn html_escaping() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {text: String = "<div>Hello & world</div>"}>
                    {text}
                  </let>
                }
            "#},
            "&lt;div&gt;Hello &amp; world&lt;/div&gt;",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let text = "<div>Hello & world</div>" in {
                    write_escaped(text)
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("&lt;div&gt;Hello &amp; world&lt;/div&gt;")
                }
                -- expected output --
                &lt;div&gt;Hello &amp; world&lt;/div&gt;
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn let_binding() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {message: String = "Hello from let"}>
                    {message}
                  </let>
                }
            "#},
            "Hello from let",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let message = "Hello from let" in {
                    write_escaped(message)
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("Hello from let")
                }
                -- expected output --
                Hello from let
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concatenation() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {first: String = "Hello"}>
                    <let {second: String = " World"}>
                      {first + second}
                    </let>
                  </let>
                }
            "#},
            "Hello World",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let first = "Hello" in {
                    let second = " World" in {
                      write_escaped((first + second))
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("Hello World")
                }
                -- expected output --
                Hello World
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn complex_nested_structure() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {item in ["A", "B"]}>
                    <let {prefix: String = "["}>
                      {prefix}{item}]
                    </let>
                  </for>
                }
            "#},
            "[A][B]",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for item in ["A", "B"] {
                    let prefix = "[" in {
                      write_escaped(prefix)
                      write_escaped(item)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  for item in ["A", "B"] {
                    write("[")
                    write_escaped(item)
                    write("]")
                  }
                }
                -- expected output --
                [A][B]
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concat_equality() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <if {"foo" + "bar" == "foobar"}>
                    equals
                  </if>
                }
            "#},
            "equals",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  if (("foo" + "bar") == "foobar") {
                    write("equals")
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("equals")
                }
                -- expected output --
                equals
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_comparison() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <if {3 < 5}>
                    3 &lt; 5
                  </if>
                  <if {10 < 2}>
                    10 &lt; 2
                  </if>
                }
            "#},
            "3 &lt; 5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  if (3 < 5) {
                    write("3 &lt; 5")
                  }
                  if (10 < 2) {
                    write("10 &lt; 2")
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_float_comparison() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <if {1.5 < 2.5}>
                    1.5 &lt; 2.5
                  </if>
                  <if {3.0 < 1.0}>
                    3.0 &lt; 1.0
                  </if>
                }
            "#},
            "1.5 &lt; 2.5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  if (1.5 < 2.5) {
                    write("1.5 &lt; 2.5")
                  }
                  if (3 < 1) {
                    write("3.0 &lt; 1.0")
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
                  <let {color: Color = Color::Red}>
                    <if {color == Color::Red}>
                      equal
                    </if>
                  </let>
                }
            "#},
            "equal",
            expect![[r#"
                -- ir (unoptimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let color = Color::Red in {
                    if (color == Color::Red) {
                      write("equal")
                    }
                  }
                }
                -- ir (optimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  write("equal")
                }
                -- expected output --
                equal
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
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
                }
            "#},
            "not equal",
            expect![[r#"
                -- ir (unoptimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let color = Color::Red in {
                    let match_subject = (color == Color::Green) in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_true() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let flag = true in {
                    let match_subject = flag in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_false() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "no",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let flag = false in {
                    let match_subject = flag in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
                  <let {person: Person = Person(name: "Alice", age: 30)}>
                    {person.name}
                    <if {person.age == 30}>
                      :30
                    </if>
                  </let>
                }
            "#},
            "Alice:30",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
                  <let {
                    person: Person = Person(
                      name: "Alice",
                      address: Address(city: "Paris", zip: "75001"),
                    ),
                  }>
                    {person.name},{person.address.city}
                  </let>
                }
            "#},
            "Alice,Paris",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
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
                }
            "#},
            "Alice:active",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_add() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {a: Int = 3}>
                    <let {b: Int = 7}>
                      <if {a + b == 10}>
                        correct
                      </if>
                    </let>
                  </let>
                }
            "#},
            "correct",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let a = 3 in {
                    let b = 7 in {
                      if ((a + b) == 10) {
                        write("correct")
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_subtract() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {a: Int = 10}>
                    <let {b: Int = 3}>
                      <if {a - b == 7}>
                        correct
                      </if>
                    </let>
                  </let>
                }
            "#},
            "correct",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let a = 10 in {
                    let b = 3 in {
                      if ((a - b) == 7) {
                        write("correct")
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_multiply() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {a: Int = 4}>
                    <let {b: Int = 5}>
                      <if {a * b == 20}>
                        correct
                      </if>
                    </let>
                  </let>
                }
            "#},
            "correct",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let a = 4 in {
                    let b = 5 in {
                      if ((a * b) == 20) {
                        write("correct")
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boolean_logical_and() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "TT",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let a = true in {
                    let b = true in {
                      if (a && b) {
                        write("TT")
                      }
                    }
                  }
                  let c = true in {
                    let d = false in {
                      if (c && d) {
                        write("TF")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("TT")
                }
                -- expected output --
                TT
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boolean_logical_or() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "FT",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let a = false in {
                    let b = true in {
                      if (a || b) {
                        write("FT")
                      }
                    }
                  }
                  let c = false in {
                    let d = false in {
                      if (c || d) {
                        write("FF")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("FT")
                }
                -- expected output --
                FT
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_or_equal() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <if {3 <= 5}>
                    A
                  </if>
                  <if {5 <= 5}>
                    B
                  </if>
                  <if {7 <= 5}>
                    C
                  </if>
                }
            "#},
            "AB",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {some_val: Option[String] = Some("hello")}>
                    <match {some_val}>
                      <case {Some(val)}>
                        {val}
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "hello",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let some_val = Option[String]::Some("hello") in {
                    let match_subject = some_val in {
                      match match_subject {
                        Some(v0) => {
                          let val = v0 in {
                            write_escaped(val)
                          }
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[String]::Some("hello") in {
                    match match_subject {
                      Some(v0) => {
                        let val = v0 in {
                          write_escaped(val)
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                hello
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_wildcard_pattern() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {opt: Option[String] = Some("hello")}>
                    <match {opt}>
                      <case {Some(_)}>
                        some
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "some",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let opt = Option[String]::Some("hello") in {
                    let match_subject = opt in {
                      match match_subject {
                        Some(_) => {
                          write("some")
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[String]::Some("hello") in {
                    match match_subject {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                some
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_nested_constant_folding() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "inner",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let inner_opt = Option[String]::Some("inner") in {
                    let outer = Option[String]::Some(match inner_opt {
                      Some(v0) => let x = v0 in x,
                      None => "default",
                    }) in {
                      let match_subject = outer in {
                        match match_subject {
                          Some(v0) => {
                            let val = v0 in {
                              write_escaped(val)
                            }
                          }
                          None => {
                            write("none")
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_array_for_loop() {
        check(
            indoc! {r#"
                entrypoint Test() {
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
                }
            "#},
            "[a][_][b]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_expr() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                entrypoint Test() {
                  <let {color: Color = Color::Green}>
                    {match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}
                  </let>
                }
            "#},
            "green",
            expect![[r#"
                -- ir (unoptimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let color = Color::Green in {
                    write_escaped(match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    })
                  }
                }
                -- ir (optimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  write("green")
                }
                -- expected output --
                green
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
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
                }
            "#},
            "blue",
            expect![[r#"
                -- ir (unoptimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let color = Color::Blue in {
                    let match_subject = color in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
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
                }
            "#},
            "Ok:hello",
            expect![[r#"
                -- ir (unoptimized) --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let result = Result::Ok(value: "hello") in {
                    let match_subject = result in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
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
                }
            "#},
            "Err:something went wrong",
            expect![[r#"
                -- ir (unoptimized) --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let result = Result::Err(message: "something went wrong") in {
                    let match_subject = result in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
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

                entrypoint Test() {
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
                }
            "#},
            "200:OK",
            expect![[r#"
                -- ir (unoptimized) --
                enum Response {
                  Success(code: String, body: String),
                  Error(reason: String),
                }
                Test() {
                  let resp = Response::Success(code: "200", body: "OK") in {
                    let match_subject = resp in {
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
                }
                -- ir (optimized) --
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
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_simple() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {items: Array[String] = ["a", "b", "c"]}>
                    {items.len().to_string()}
                  </let>
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let items = ["a", "b", "c"] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let items = ["a", "b", "c"] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- expected output --
                3
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_empty() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {items: Array[String] = []}>
                    {items.len().to_string()}
                  </let>
                }
            "#},
            "0",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let items = [] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let items = [] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- expected output --
                0
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_in_comparison() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {items: Array[String] = ["x", "y"]}>
                    <if {items.len() == 2}>
                      has two
                    </if>
                  </let>
                }
            "#},
            "has two",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let items = ["x", "y"] in {
                    if (items.len() == 2) {
                      write("has two")
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let items = ["x", "y"] in {
                    if (items.len() == 2) {
                      write("has two")
                    }
                  }
                }
                -- expected output --
                has two
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_less_than() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {items: Array[String] = ["a"]}>
                    <if {items.len() < 5}>
                      less than 5
                    </if>
                  </let>
                }
            "#},
            "less than 5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let items = ["a"] in {
                    if (items.len() < 5) {
                      write("less than 5")
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let items = ["a"] in {
                    if (items.len() < 5) {
                      write("less than 5")
                    }
                  }
                }
                -- expected output --
                less than 5
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_int_array() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {numbers: Array[Int] = [1, 2, 3, 4, 5]}>
                    {numbers.len().to_string()}
                  </let>
                }
            "#},
            "5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let numbers = [1, 2, 3, 4, 5] in {
                    write_escaped(numbers.len().to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let numbers = [1, 2, 3, 4, 5] in {
                    write_escaped(numbers.len().to_string())
                  }
                }
                -- expected output --
                5
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_simple() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {count: Int = 42}>
                    {count.to_string()}
                  </let>
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let count = 42 in {
                    write_escaped(count.to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let count = 42 in {
                    write_escaped(count.to_string())
                  }
                }
                -- expected output --
                42
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_zero() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {num: Int = 0}>
                    {num.to_string()}
                  </let>
                }
            "#},
            "0",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let num = 0 in {
                    write_escaped(num.to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let num = 0 in {
                    write_escaped(num.to_string())
                  }
                }
                -- expected output --
                0
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_concat() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {count: Int = 5}>
                    {"Count: " + count.to_string()}
                  </let>
                }
            "#},
            "Count: 5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let count = 5 in {
                    write_escaped(("Count: " + count.to_string()))
                  }
                }
                -- ir (optimized) --
                Test() {
                  let count = 5 in {
                    write_escaped(("Count: " + count.to_string()))
                  }
                }
                -- expected output --
                Count: 5
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_simple() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {price: Float = 3.7}>
                    {price.to_int().to_string()}
                  </let>
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let price = 3.7 in {
                    write_escaped(price.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let price = 3.7 in {
                    write_escaped(price.to_int().to_string())
                  }
                }
                -- expected output --
                3
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_whole_number() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {val: Float = 5.0}>
                    {val.to_int().to_string()}
                  </let>
                }
            "#},
            "5",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let val = 5 in {
                    write_escaped(val.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let val = 5 in {
                    write_escaped(val.to_int().to_string())
                  }
                }
                -- expected output --
                5
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_string_simple() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {price: Float = 19.99}>
                    {price.to_string()}
                  </let>
                }
            "#},
            "19.99",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let price = 19.99 in {
                    write_escaped(price.to_string())
                  }
                }
                -- ir (optimized) --
                Test() {
                  let price = 19.99 in {
                    write_escaped(price.to_string())
                  }
                }
                -- expected output --
                19.99
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_string_concat() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {price: Float = 9.99}>
                    {"$" + price.to_string()}
                  </let>
                }
            "#},
            "$9.99",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let price = 9.99 in {
                    write_escaped(("$" + price.to_string()))
                  }
                }
                -- ir (optimized) --
                Test() {
                  let price = 9.99 in {
                    write_escaped(("$" + price.to_string()))
                  }
                }
                -- expected output --
                $9.99
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_range() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {_ in 0..=2}>
                    x
                  </for>
                }
            "#},
            "xxx",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for _ in 0..=2 {
                    write("x")
                  }
                }
                -- ir (optimized) --
                Test() {
                  for _ in 0..=2 {
                    write("x")
                  }
                }
                -- expected output --
                xxx
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_array() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {items: Array[String] = ["a", "b", "c"]}>
                    <for {_ in items}>
                      *
                    </for>
                  </let>
                }
            "#},
            "***",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let items = ["a", "b", "c"] in {
                    for _ in items {
                      write("*")
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let items = ["a", "b", "c"] in {
                    for _ in items {
                      write("*")
                    }
                  }
                }
                -- expected output --
                ***
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_nested() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {_ in 0..=1}>
                    <for {_ in 0..=2}>
                      .
                    </for>
                  </for>
                }
            "#},
            "......",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for _ in 0..=1 {
                    for _ in 0..=2 {
                      write(".")
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  for _ in 0..=1 {
                    for _ in 0..=2 {
                      write(".")
                    }
                  }
                }
                -- expected output --
                ......
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_mixed_with_named() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <for {i in 1..=2}>
                    <for {_ in 0..=1}>
                      {i.to_string()}
                    </for>
                  </for>
                }
            "#},
            "1122",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  for i in 1..=2 {
                    for _ in 0..=1 {
                      write_escaped(i.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  for i in 1..=2 {
                    for _ in 0..=1 {
                      write_escaped(i.to_string())
                    }
                  }
                }
                -- expected output --
                1122
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_float_literal() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  {3.14.to_string()}
                }
            "#},
            "3.14",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  write_escaped(3.14.to_string())
                }
                -- ir (optimized) --
                Test() {
                  write_escaped(3.14.to_string())
                }
                -- expected output --
                3.14
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_array_literal() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  {[1, 2, 3].len().to_string()}
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  write_escaped([1, 2, 3].len().to_string())
                }
                -- ir (optimized) --
                Test() {
                  write_escaped([1, 2, 3].len().to_string())
                }
                -- expected output --
                3
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_parenthesized_expression() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  {(1 + 2).to_string()}
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  write_escaped((1 + 2).to_string())
                }
                -- ir (optimized) --
                Test() {
                  write_escaped((1 + 2).to_string())
                }
                -- expected output --
                3
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_literal_to_string() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  {42.to_string()}
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  write_escaped(42.to_string())
                }
                -- ir (optimized) --
                Test() {
                  write_escaped(42.to_string())
                }
                -- expected output --
                42
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn negated_float_to_string() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  {(-3.14).to_string()}
                }
            "#},
            "-3.14",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  write_escaped((-3.14).to_string())
                }
                -- ir (optimized) --
                Test() {
                  write_escaped((-3.14).to_string())
                }
                -- expected output --
                -3.14
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_option_match() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {
                    nested: Option[Option[String]] = Some(Some("deep")),
                  }>
                    <match {nested}>
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
                }
            "#},
            "deep",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    let match_subject = nested in {
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
                  }
                }
                -- ir (optimized) --
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
                }
                -- expected output --
                deep
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_some_input() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {opt: Option[String] = Some("x")}>
                    <match {opt}>
                      <case {Some(_)}>
                        some
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "some",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let opt = Option[String]::Some("x") in {
                    let match_subject = opt in {
                      match match_subject {
                        Some(_) => {
                          write("some")
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[String]::Some("x") in {
                    match match_subject {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                some
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_none_input() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {opt: Option[String] = None}>
                    <match {opt}>
                      <case {Some(_)}>
                        some
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "none",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let opt = Option[String]::None in {
                    let match_subject = opt in {
                      match match_subject {
                        Some(_) => {
                          write("some")
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[String]::None in {
                    match match_subject {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                none
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_expr_some_input() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {opt: Option[String] = Some("x")}>
                    {match opt {Some(_) => "some", None => "none"}}
                  </let>
                }
            "#},
            "some",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let opt = Option[String]::Some("x") in {
                    write_escaped(match opt {
                      Some(_) => "some",
                      None => "none",
                    })
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("some")
                }
                -- expected output --
                some
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_expr_none_input() {
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {opt: Option[String] = None}>
                    {match opt {Some(_) => "some", None => "none"}}
                  </let>
                }
            "#},
            "none",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let opt = Option[String]::None in {
                    write_escaped(match opt {
                      Some(_) => "some",
                      None => "none",
                    })
                  }
                }
                -- ir (optimized) --
                Test() {
                  write("none")
                }
                -- expected output --
                none
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_option_wildcard_inner() {
        // Test Some(Some(_)) pattern - inner value discarded
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {nested: Option[Option[String]] = Some(Some("x"))}>
                    <match {nested}>
                      <case {Some(Some(_))}>
                        some-some
                      </case>
                      <case {Some(None)}>
                        some-none
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "some-some",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    let match_subject = nested in {
                      match match_subject {
                        Some(v0) => {
                          match v0 {
                            Some(_) => {
                              write("some-some")
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
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match match_subject {
                      Some(v0) => {
                        match v0 {
                          Some(_) => {
                            write("some-some")
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
                some-some
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_option_wildcard_outer() {
        // Test Some(_) pattern on Option[Option[String]] - entire inner option discarded
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {nested: Option[Option[String]] = Some(Some("x"))}>
                    <match {nested}>
                      <case {Some(_)}>
                        some
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "some",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    let match_subject = nested in {
                      match match_subject {
                        Some(_) => {
                          write("some")
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match match_subject {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                some
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_wildcard_binding_ok() {
        // Test Result::Ok(value: _) - wildcard binding for enum field
        check(
            indoc! {r#"
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }

                entrypoint Test() {
                  <let {result: Result = Result::Ok(value: "hello")}>
                    <match {result}>
                      <case {Result::Ok(value: _)}>
                        ok
                      </case>
                      <case {Result::Err(message: _)}>
                        err
                      </case>
                    </match>
                  </let>
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let result = Result::Ok(value: "hello") in {
                    let match_subject = result in {
                      match match_subject {
                        Result::Ok => {
                          write("ok")
                        }
                        Result::Err => {
                          write("err")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let match_subject = Result::Ok(value: "hello") in {
                    match match_subject {
                      Result::Ok => {
                        write("ok")
                      }
                      Result::Err => {
                        write("err")
                      }
                    }
                  }
                }
                -- expected output --
                ok
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_wildcard_binding_err() {
        // Test Result::Err(message: _) - wildcard binding for enum field
        check(
            indoc! {r#"
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }

                entrypoint Test() {
                  <let {result: Result = Result::Err(message: "failed")}>
                    <match {result}>
                      <case {Result::Ok(value: _)}>
                        ok
                      </case>
                      <case {Result::Err(message: _)}>
                        err
                      </case>
                    </match>
                  </let>
                }
            "#},
            "err",
            expect![[r#"
                -- ir (unoptimized) --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let result = Result::Err(message: "failed") in {
                    let match_subject = result in {
                      match match_subject {
                        Result::Ok => {
                          write("ok")
                        }
                        Result::Err => {
                          write("err")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let match_subject = Result::Err(message: "failed") in {
                    match match_subject {
                      Result::Ok => {
                        write("ok")
                      }
                      Result::Err => {
                        write("err")
                      }
                    }
                  }
                }
                -- expected output --
                err
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn record_wildcard_binding() {
        // Test record pattern with wildcard binding - Person(name: _, age: a)
        check(
            indoc! {r#"
                record Person {
                  name: String,
                  age: Int,
                }

                entrypoint Test() {
                  <let {person: Person = Person(name: "Alice", age: 30)}>
                    <match {person}>
                      <case {Person(name: _, age: a)}>
                        age:{a.to_string()}
                      </case>
                    </match>
                  </let>
                }
            "#},
            "age:30",
            expect![[r#"
                -- ir (unoptimized) --
                record Person {
                  name: String,
                  age: Int,
                }
                Test() {
                  let person = Person(name: "Alice", age: 30) in {
                    let match_subject = person in {
                      let v1 = match_subject.age in {
                        let a = v1 in {
                          write("age:")
                          write_escaped(a.to_string())
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Person {
                  name: String,
                  age: Int,
                }
                Test() {
                  let person = Person(name: "Alice", age: 30) in {
                    let match_subject = person in {
                      let v1 = match_subject.age in {
                        let a = v1 in {
                          write("age:")
                          write_escaped(a.to_string())
                        }
                      }
                    }
                  }
                }
                -- expected output --
                age:30
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn triple_nested_option_wildcard() {
        // Test Some(Some(Some(_))) pattern - triple nested with innermost wildcard
        check(
            indoc! {r#"
                entrypoint Test() {
                  <let {
                    deep: Option[Option[Option[String]]] = Some(
                      Some(Some("value"))
                    ),
                  }>
                    <match {deep}>
                      <case {Some(Some(Some(_)))}>
                        triple-some
                      </case>
                      <case {Some(Some(None))}>
                        double-some-none
                      </case>
                      <case {Some(None)}>
                        single-some-none
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "triple-some",
            expect![[r#"
                -- ir (unoptimized) --
                Test() {
                  let deep = Option[Option[Option[String]]]::Some(Option[Option[String]]::Some(Option[String]::Some("value"))) in {
                    let match_subject = deep in {
                      match match_subject {
                        Some(v0) => {
                          match v0 {
                            Some(v1) => {
                              match v1 {
                                Some(_) => {
                                  write("triple-some")
                                }
                                None => {
                                  write("double-some-none")
                                }
                              }
                            }
                            None => {
                              write("single-some-none")
                            }
                          }
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                Test() {
                  let match_subject = Option[Option[Option[String]]]::Some(Option[Option[String]]::Some(Option[String]::Some("value"))) in {
                    match match_subject {
                      Some(v0) => {
                        match v0 {
                          Some(v1) => {
                            match v1 {
                              Some(_) => {
                                write("triple-some")
                              }
                              None => {
                                write("double-some-none")
                              }
                            }
                          }
                          None => {
                            write("single-some-none")
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
                triple-some
                -- ts (unoptimized) --
                OK
                -- go (unoptimized) --
                OK
                -- python (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- go (optimized) --
                OK
                -- python (optimized) --
                OK
            "#]],
        );
    }
}
