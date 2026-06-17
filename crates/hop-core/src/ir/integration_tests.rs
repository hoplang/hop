use super::semantics::evaluator::evaluate_view;
use super::transpile::{RustTranspiler, Transpiler, TsTranspiler};
use crate::asset_rewriter::AssetRewriter;
use crate::document::Document;
use crate::document_id::DocumentId;
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use crate::program::Program;
use expect_test::Expect;
use std::collections::HashMap;
use std::fs;
use std::process::Command;
use std::sync::Arc;
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

fn typecheck_typescript(code: &str) -> Result<(), String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
    let module_file = temp_dir.path().join("module.ts");

    fs::write(&module_file, code).map_err(|e| format!("Failed to write module file: {}", e))?;

    let file_path = module_file
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?;

    let output = Command::new("tsgo")
        .args(["--noEmit", "--target", "ES2020", "--strict", file_path])
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

fn execute_rust(code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;

    // Create wrapper that calls the Test struct's render method
    let main_code = format!(
        r#"{}

fn main() {{
    print!("{{}}", Test {{}}.render());
}}
"#,
        code
    );

    let main_rs = temp_dir.path().join("main.rs");
    fs::write(&main_rs, main_code).map_err(|e| format!("Failed to write main.rs: {}", e))?;

    // Compile with rustc
    let binary_path = temp_dir.path().join("hoptest");
    let compile_output = Command::new("rustc")
        .arg("--edition=2021")
        .args(["-C", "debuginfo=0"])
        .arg(&main_rs)
        .arg("-o")
        .arg(&binary_path)
        .output()
        .map_err(|e| format!("Failed to compile Rust: {}", e))?;

    if !compile_output.status.success() {
        return Err(format!(
            "Rust compilation failed:\n{}",
            String::from_utf8_lossy(&compile_output.stderr)
        ));
    }

    // Execute
    let output = Command::new(&binary_path)
        .output()
        .map_err(|e| format!("Failed to execute Rust binary: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Rust execution failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn typecheck_rust(code: &str) -> Result<(), String> {
    let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;

    // Add #![allow(dead_code)] to suppress warnings
    let code_with_attrs = format!("#![allow(dead_code)]\n{}", code);
    let lib_rs = temp_dir.path().join("lib.rs");
    fs::write(&lib_rs, code_with_attrs).map_err(|e| format!("Failed to write lib.rs: {}", e))?;

    // Type check with rustc (emit metadata only, no codegen)
    let output = Command::new("rustc")
        .arg("--edition=2021")
        .arg("--crate-type=lib")
        .arg("--emit=metadata")
        .arg("-o")
        .arg(temp_dir.path().join("libhoptest.rmeta"))
        .arg(&lib_rs)
        .output()
        .map_err(|e| format!("Failed to execute rustc: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Rust type checking failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(())
}

fn execute_evaluator(module: &super::IrModule) -> Result<String, String> {
    let view = module
        .views
        .iter()
        .find(|c| c.name.as_str() == "Test")
        .ok_or_else(|| "Test view not found".to_string())?;

    evaluate_view(view, HashMap::new(), &module.components)
        .map_err(|e| format!("Evaluator failed: {}", e))
}

fn check(hop_source: &str, expected_output: &str, expected: Expect) {
    check_with_asset_rewriter(hop_source, None, expected_output, expected);
}

fn check_with_asset_rewriter(
    hop_source: &str,
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    expected_output: &str,
    expected: Expect,
) {
    // Parse hop source code
    let document_id = DocumentId::new("test.hop").unwrap();
    let mut program = Program::default();
    let document = Document::new(document_id.clone(), hop_source.to_string());
    program.update_module(&document_id, document);

    // Check for parse errors
    let parse_errors = program.get_parse_errors();
    let has_parse_errors = parse_errors.values().any(|e| !e.is_empty());
    if has_parse_errors {
        for (module, errors) in parse_errors.iter() {
            for error in errors.iter() {
                eprintln!("Parse Error in {:?}: {:?}", module, error);
            }
        }
        panic!("Parse errors found");
    }

    // Verify input is properly formatted
    let formatted = program
        .get_formatted_module(&document_id)
        .expect("Failed to format module");
    assert_eq!(
        formatted.trim(),
        hop_source.trim(),
        "Test input is not properly formatted. Update the test input (right) to match the formatted output (left)."
    );

    // Check for type errors
    let type_errors = program.get_type_errors();
    let has_type_errors = type_errors.values().any(|e| !e.is_empty());
    if has_type_errors {
        for (module, errors) in type_errors.iter() {
            for error in errors.iter() {
                eprintln!("Type Error in {:?}: {:?}", module, error);
            }
        }
        panic!("Type errors found");
    }

    let typed_asts = program.get_typed_modules().clone();

    // Compile to IR without optimization
    let unoptimized_options = OrchestrateOptions {
        skip_html_structure: true,
        skip_optimization: true,
        skip_inlining: true,
        asset_rewriter: asset_rewriter.clone(),
        ..Default::default()
    };
    let unoptimized_module = orchestrate(&typed_asts, unoptimized_options);

    // Compile to IR with optimization
    let optimized_options = OrchestrateOptions {
        skip_html_structure: true,
        skip_optimization: false,
        asset_rewriter,
        ..Default::default()
    };
    let optimized_module = orchestrate(&typed_asts, optimized_options);

    let unoptimized_ir = unoptimized_module.to_string();
    let optimized_ir = optimized_module.to_string();

    let mut output = format!(
        "-- ir (unoptimized) --\n{}-- ir (optimized) --\n{}-- expected output --\n{}\n",
        unoptimized_ir, optimized_ir, expected_output
    );

    // Test evaluator on unoptimized IR
    let eval_output = match execute_evaluator(&unoptimized_module) {
        Ok(out) => out,
        Err(e) => panic!(
            "Evaluator failed (unoptimized):\n{}\n\nIR:\n{}",
            e, unoptimized_ir
        ),
    };
    assert_eq!(
        eval_output, expected_output,
        "Evaluator output mismatch (unoptimized)\n\nIR:\n{}",
        unoptimized_ir
    );
    output.push_str("-- eval (unoptimized) --\nOK\n");

    // Test evaluator on optimized IR
    let eval_output = match execute_evaluator(&optimized_module) {
        Ok(out) => out,
        Err(e) => panic!(
            "Evaluator failed (optimized):\n{}\n\nIR:\n{}",
            e, optimized_ir
        ),
    };
    assert_eq!(
        eval_output, expected_output,
        "Evaluator output mismatch (optimized)\n\nIR:\n{}",
        optimized_ir
    );
    output.push_str("-- eval (optimized) --\nOK\n");

    // Test unoptimized version
    let mut ts_transpiler = TsTranspiler::new();
    let ts_code = ts_transpiler.transpile_module(&unoptimized_module);
    if let Err(e) = typecheck_typescript(&ts_code) {
        panic!(
            "TypeScript typecheck failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, unoptimized_ir, ts_code
        );
    }
    let ts_output = match execute_typescript(&ts_code) {
        Ok(out) => out,
        Err(e) => panic!(
            "TypeScript execution failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, unoptimized_ir, ts_code
        ),
    };
    assert_eq!(
        ts_output, expected_output,
        "TypeScript output mismatch (unoptimized)\n\nIR:\n{}\nGenerated code:\n{}",
        unoptimized_ir, ts_code
    );
    output.push_str("-- ts (unoptimized) --\nOK\n");

    let mut rust_transpiler = RustTranspiler::new();
    let rust_code = rust_transpiler.transpile_module(&unoptimized_module);
    if let Err(e) = typecheck_rust(&rust_code) {
        panic!(
            "Rust typecheck failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, unoptimized_ir, rust_code
        );
    }
    let rust_output = match execute_rust(&rust_code) {
        Ok(out) => out,
        Err(e) => panic!(
            "Rust execution failed (unoptimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, unoptimized_ir, rust_code
        ),
    };
    assert_eq!(
        rust_output, expected_output,
        "Rust output mismatch (unoptimized)\n\nIR:\n{}\nGenerated code:\n{}",
        unoptimized_ir, rust_code
    );
    output.push_str("-- rust (unoptimized) --\nOK\n");

    // Test optimized version
    let ts_code = ts_transpiler.transpile_module(&optimized_module);
    if let Err(e) = typecheck_typescript(&ts_code) {
        panic!(
            "TypeScript typecheck failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, optimized_ir, ts_code
        );
    }
    let ts_output = match execute_typescript(&ts_code) {
        Ok(out) => out,
        Err(e) => panic!(
            "TypeScript execution failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, optimized_ir, ts_code
        ),
    };
    assert_eq!(
        ts_output, expected_output,
        "TypeScript output mismatch (optimized)\n\nIR:\n{}\nGenerated code:\n{}",
        optimized_ir, ts_code
    );
    output.push_str("-- ts (optimized) --\nOK\n");

    let rust_code = rust_transpiler.transpile_module(&optimized_module);
    if let Err(e) = typecheck_rust(&rust_code) {
        panic!(
            "Rust typecheck failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, optimized_ir, rust_code
        );
    }
    let rust_output = match execute_rust(&rust_code) {
        Ok(out) => out,
        Err(e) => panic!(
            "Rust execution failed (optimized):\n{}\n\nIR:\n{}\nGenerated code:\n{}",
            e, optimized_ir, rust_code
        ),
    };
    assert_eq!(
        rust_output, expected_output,
        "Rust output mismatch (optimized)\n\nIR:\n{}\nGenerated code:\n{}",
        optimized_ir, rust_code
    );
    output.push_str("-- rust (optimized) --\nOK\n");

    expected.assert_eq(&output);
}

#[cfg(test)]
mod tests {
    use crate::asset_rewriter::{PrefixingAssetRewriter, ReplacingAssetRewriter};

    use super::*;
    use expect_test::expect;
    use indoc::indoc;

    #[test]
    #[ignore]
    fn option_match_returning_options() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let inner = Option[String]::Some("hello") in {
                    let mapped = match inner {
                      Some(v_0) => let x = v_0 in Option[String]::Some(x),
                      None => Option[String]::None,
                    } in {
                      match mapped {
                        Some(v_1) => {
                          let result = v_1 in {
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
                -- ir (optimized) --
                view Test() {
                  let mapped = Option[String]::Some("hello") in {
                    match mapped {
                      Some(v_1) => {
                        let result = v_1 in {
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
                -- expected output --
                mapped:hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_as_some_value() {
        check(
            indoc! {r#"
                view Test {
                  <let {inner_opt: Option[String] = Some("inner")}>
                    <let {
                      outer: Option[String] = Some(
                        match inner_opt {Some(x) => x, None => "default"}
                      ),
                    }>
                      <match {outer}>
                        <case {Some(s)}>
                          {s}
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
                view Test() {
                  let inner_opt = Option[String]::Some("inner") in {
                    let outer = Option[String]::Some(match inner_opt {
                      Some(v_0) => let x = v_0 in x,
                      None => "default",
                    }) in {
                      match outer {
                        Some(v_1) => {
                          let s = v_1 in {
                            write_escaped(s)
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
                view Test() {
                  let outer = Option[String]::Some("inner") in {
                    match outer {
                      Some(v_1) => {
                        let s = v_1 in {
                          write_escaped(s)
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                inner
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_expr() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
                  write("yesNO")
                }
                -- expected output --
                yesNO
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_expr_with_binary_subject() {
        check(
            indoc! {r#"
                view Test {
                  <let {path: String = ""}>
                    <let {git_ref: String = "main"}>
                      {match path == "" {
                        true => git_ref,
                        _ => git_ref + " - " + path,
                      }}
                    </let>
                  </let>
                }
            "#},
            "main",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let path = "" in {
                    let git_ref = "main" in {
                      write_escaped(let v_0 = (path == "") in match v_0 {
                        true => git_ref,
                        false => ((git_ref + " - ") + path),
                      })
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("main")
                }
                -- expected output --
                main
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal_inline_match_expr() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
                  write("some,NONE")
                }
                -- expected output --
                some,NONE
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_bool_match_expr() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
                  write("TF,F")
                }
                -- expected output --
                TF,F
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_negative() {
        check(
            indoc! {r#"
                view Test {
                  <let {num: Int = -123}>
                    {num.to_string()}
                  </let>
                }
            "#},
            "-123",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let num = (-123) in {
                    write_escaped(num.to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let num = (-123) in {
                    write_escaped(num.to_string())
                  }
                }
                -- expected output --
                -123
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_negative() {
        check(
            indoc! {r#"
                view Test {
                  <let {temp: Float = -2.9}>
                    {temp.to_int().to_string()}
                  </let>
                }
            "#},
            "-2",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let temp = (-2.9) in {
                    write_escaped(temp.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let temp = (-2.9) in {
                    write_escaped(temp.to_int().to_string())
                  }
                }
                -- expected output --
                -2
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn simple_html() {
        check(
            indoc! {r#"
                view Test {
                  <h1>
                    Hello, World!
                  </h1>
                }
            "#},
            "<h1>Hello, World!</h1>",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("<h1")
                  write(">")
                  write("Hello, World!")
                  write("</h1>")
                }
                -- ir (optimized) --
                view Test() {
                  write("<h1>Hello, World!</h1>")
                }
                -- expected output --
                <h1>Hello, World!</h1>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn html_comment() {
        check(
            indoc! {r#"
                view Test {
                  <!-- This is a comment -->
                  <h1>
                    Hello, World!
                  </h1>
                  <!-- Another comment -->
                }
            "#},
            "<h1>Hello, World!</h1>",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("<h1")
                  write(">")
                  write("Hello, World!")
                  write("</h1>")
                }
                -- ir (optimized) --
                view Test() {
                  write("<h1>Hello, World!</h1>")
                }
                -- expected output --
                <h1>Hello, World!</h1>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn with_let_binding() {
        check(
            indoc! {r#"
                view Test {
                  <let {name: String = "Alice"}>
                    Hello, {name}!
                  </let>
                }
            "#},
            "Hello, Alice!",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let name = "Alice" in {
                    write("Hello, ")
                    write_escaped(name)
                    write("!")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("Hello, Alice!")
                }
                -- expected output --
                Hello, Alice!
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn conditional() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
                  write("Visible")
                }
                -- expected output --
                Visible
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop() {
        check(
            indoc! {r#"
                view Test {
                  <for {item in ["a", "b", "c"]}>
                    {item},
                  </for>
                }
            "#},
            "a,b,c,",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  for item in ["a", "b", "c"] {
                    write_escaped(item)
                    write(",")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for item in ["a", "b", "c"] {
                    write_escaped(item)
                    write(",")
                  }
                }
                -- expected output --
                a,b,c,
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range() {
        check(
            indoc! {r#"
                view Test {
                  <for {i in 1..=3}>
                    {i.to_string()},
                  </for>
                }
            "#},
            "1,2,3,",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  for i in 1..=3 {
                    write_escaped(i.to_string())
                    write(",")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for i in 1..=3 {
                    write_escaped(i.to_string())
                    write(",")
                  }
                }
                -- expected output --
                1,2,3,
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range_zero_to_five() {
        check(
            indoc! {r#"
                view Test {
                  <for {x in 0..=5}>
                    {x.to_string()}
                  </for>
                }
            "#},
            "012345",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  for x in 0..=5 {
                    write_escaped(x.to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for x in 0..=5 {
                    write_escaped(x.to_string())
                  }
                }
                -- expected output --
                012345
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_range_nested() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn html_escaping() {
        check(
            indoc! {r#"
                view Test {
                  <let {text: String = "<div>Hello & world</div>"}>
                    {text}
                  </let>
                }
            "#},
            "&lt;div&gt;Hello &amp; world&lt;/div&gt;",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let text = "<div>Hello & world</div>" in {
                    write_escaped(text)
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("&lt;div&gt;Hello &amp; world&lt;/div&gt;")
                }
                -- expected output --
                &lt;div&gt;Hello &amp; world&lt;/div&gt;
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn let_binding() {
        check(
            indoc! {r#"
                view Test {
                  <let {message: String = "Hello from let"}>
                    {message}
                  </let>
                }
            "#},
            "Hello from let",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let message = "Hello from let" in {
                    write_escaped(message)
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("Hello from let")
                }
                -- expected output --
                Hello from let
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concatenation() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let first = "Hello" in {
                    let second = " World" in {
                      write_escaped((first + second))
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("Hello World")
                }
                -- expected output --
                Hello World
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn complex_nested_structure() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  for item in ["A", "B"] {
                    let prefix = "[" in {
                      write_escaped(prefix)
                      write_escaped(item)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for item in ["A", "B"] {
                    write("[")
                    write_escaped(item)
                    write("]")
                  }
                }
                -- expected output --
                [A][B]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concat_equality() {
        check(
            indoc! {r#"
                view Test {
                  <if {"foo" + "bar" == "foobar"}>
                    equals
                  </if>
                }
            "#},
            "equals",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  if (("foo" + "bar") == "foobar") {
                    write("equals")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("equals")
                }
                -- expected output --
                equals
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_comparison() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  if (3 < 5) {
                    write("3 &lt; 5")
                  }
                  if (10 < 2) {
                    write("10 &lt; 2")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  if (3 < 5) {
                    write("3 &lt; 5")
                  }
                  if (10 < 2) {
                    write("10 &lt; 2")
                  }
                }
                -- expected output --
                3 &lt; 5
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_float_comparison() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  if (1.5 < 2.5) {
                    write("1.5 &lt; 2.5")
                  }
                  if (3 < 1) {
                    write("3.0 &lt; 1.0")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  if (1.5 < 2.5) {
                    write("1.5 &lt; 2.5")
                  }
                  if (3 < 1) {
                    write("3.0 &lt; 1.0")
                  }
                }
                -- expected output --
                1.5 &lt; 2.5
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_true() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let flag = true in {
                    match flag {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let flag = true in {
                    match flag {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_false() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let flag = false in {
                    match flag {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let flag = false in {
                    match flag {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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

                view Test {
                  <let {person: Person = Person {name: "Alice", age: 30}}>
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
                view Test() {
                  let person = Person {name: "Alice", age: 30} in {
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
                view Test() {
                  let person = Person {name: "Alice", age: 30} in {
                    write_escaped(person.name)
                    if (person.age == 30) {
                      write(":30")
                    }
                  }
                }
                -- expected output --
                Alice:30
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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

                view Test {
                  <let {
                    person: Person = Person {
                      name: "Alice",
                      address: Address {city: "Paris", zip: "75001"},
                    },
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
                view Test() {
                  let person = Person {
                    name: "Alice",
                    address: Address {city: "Paris", zip: "75001"},
                  } in {
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
                view Test() {
                  let person = Person {
                    name: "Alice",
                    address: Address {city: "Paris", zip: "75001"},
                  } in {
                    write_escaped(person.name)
                    write(",")
                    write_escaped(person.address.city)
                  }
                }
                -- expected output --
                Alice,Paris
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_add() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let a = 3 in {
                    let b = 7 in {
                      if ((a + b) == 10) {
                        write("correct")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_subtract() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let a = 10 in {
                    let b = 3 in {
                      if ((a - b) == 7) {
                        write("correct")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn numeric_multiply() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let a = 4 in {
                    let b = 5 in {
                      if ((a * b) == 20) {
                        write("correct")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boolean_logical_and() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
                  write("TT")
                }
                -- expected output --
                TT
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boolean_logical_or() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
                  write("FT")
                }
                -- expected output --
                FT
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn less_than_or_equal() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
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
                view Test() {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_literal() {
        check(
            indoc! {r#"
                view Test {
                  <let {some_val: Option[String] = Some("hello")}>
                    <match {some_val}>
                      <case {Some(s)}>
                        {s}
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
                view Test() {
                  let some_val = Option[String]::Some("hello") in {
                    match some_val {
                      Some(v_0) => {
                        let s = v_0 in {
                          write_escaped(s)
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let some_val = Option[String]::Some("hello") in {
                    match some_val {
                      Some(v_0) => {
                        let s = v_0 in {
                          write_escaped(s)
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_wildcard_pattern() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let opt = Option[String]::Some("hello") in {
                    match opt {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_match_nested_constant_folding() {
        check(
            indoc! {r#"
                view Test {
                  <let {inner_opt: Option[String] = Some("inner")}>
                    <let {
                      outer: Option[String] = Some(
                        match inner_opt {Some(x) => x, None => "default"}
                      ),
                    }>
                      <match {outer}>
                        <case {Some(s)}>
                          {s}
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
                view Test() {
                  let inner_opt = Option[String]::Some("inner") in {
                    let outer = Option[String]::Some(match inner_opt {
                      Some(v_0) => let x = v_0 in x,
                      None => "default",
                    }) in {
                      match outer {
                        Some(v_1) => {
                          let s = v_1 in {
                            write_escaped(s)
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
                view Test() {
                  let outer = Option[String]::Some("inner") in {
                    match outer {
                      Some(v_1) => {
                        let s = v_1 in {
                          write_escaped(s)
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- expected output --
                inner
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_array_for_loop() {
        check(
            indoc! {r#"
                view Test {
                  <for {item in [Some("a"), None, Some("b")]}>
                    <match {item}>
                      <case {Some(s)}>
                        [{s}]
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
                view Test() {
                  for item in [
                    Option[String]::Some("a"),
                    Option[String]::None,
                    Option[String]::Some("b"),
                  ] {
                    match item {
                      Some(v_0) => {
                        let s = v_0 in {
                          write("[")
                          write_escaped(s)
                          write("]")
                        }
                      }
                      None => {
                        write("[_]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for item in [
                    Option[String]::Some("a"),
                    Option[String]::None,
                    Option[String]::Some("b"),
                  ] {
                    match item {
                      Some(v_0) => {
                        let s = v_0 in {
                          write("[")
                          write_escaped(s)
                          write("]")
                        }
                      }
                      None => {
                        write("[_]")
                      }
                    }
                  }
                }
                -- expected output --
                [a][_][b]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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

                view Test {
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
                view Test() {
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
                view Test() {
                  write("green")
                }
                -- expected output --
                green
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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

                view Test {
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
                view Test() {
                  let color = Color::Blue in {
                    match color {
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
                -- ir (optimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                view Test() {
                  let color = Color::Blue in {
                    match color {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_with_field_bindings() {
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: Outcome = Outcome::Success {value: "hello"},
                  }>
                    <match {result}>
                      <case {Outcome::Success {value: v}}>
                        Ok:{v}
                      </case>
                      <case {Outcome::Failure {message: m}}>
                        Err:{m}
                      </case>
                    </match>
                  </let>
                }
            "#},
            "Ok:hello",
            expect![[r#"
                -- ir (unoptimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Success {value: "hello"} in {
                    match result {
                      Outcome::Success(value: v_0) => {
                        let v = v_0 in {
                          write("Ok:")
                          write_escaped(v)
                        }
                      }
                      Outcome::Failure(message: v_1) => {
                        let m = v_1 in {
                          write("Err:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Success {value: "hello"} in {
                    match result {
                      Outcome::Success(value: v_0) => {
                        let v = v_0 in {
                          write("Ok:")
                          write_escaped(v)
                        }
                      }
                      Outcome::Failure(message: v_1) => {
                        let m = v_1 in {
                          write("Err:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                Ok:hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_with_fields_match_on_expression_subject() {
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: String = match Outcome::Success {value: "hi"} {
                      Outcome::Success {value: v} => v,
                      Outcome::Failure {message: m} => m,
                    },
                  }>
                    got:{result}
                  </let>
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = let v_0 = Outcome::Success {value: "hi"} in match v_0 {
                    Outcome::Success {value: v_1} => let v = v_1 in v,
                    Outcome::Failure {message: v_2} => let m = v_2 in m,
                  } in {
                    write("got:")
                    write_escaped(result)
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  write("got:hi")
                }
                -- expected output --
                got:hi
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_in_component_prop() {
        check(
            indoc! {r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Badge(color: Color) {
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
                }

                view Test {
                  <Badge color={Color::Green}/>
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
                component Badge(color: test::Color) {
                  match color {
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
                view Test() {
                  call Badge(color = Color::Green)
                }
                -- ir (optimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                view Test() {
                  let color = Color::Green in {
                    match color {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_err_variant_with_bindings() {
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: Outcome = Outcome::Failure {
                      message: "something went wrong",
                    },
                  }>
                    <match {result}>
                      <case {Outcome::Success {value: v}}>
                        Ok:{v}
                      </case>
                      <case {Outcome::Failure {message: m}}>
                        Err:{m}
                      </case>
                    </match>
                  </let>
                }
            "#},
            "Err:something went wrong",
            expect![[r#"
                -- ir (unoptimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Failure {message: "something went wrong"} in {
                    match result {
                      Outcome::Success(value: v_0) => {
                        let v = v_0 in {
                          write("Ok:")
                          write_escaped(v)
                        }
                      }
                      Outcome::Failure(message: v_1) => {
                        let m = v_1 in {
                          write("Err:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Failure {message: "something went wrong"} in {
                    match result {
                      Outcome::Success(value: v_0) => {
                        let v = v_0 in {
                          write("Ok:")
                          write_escaped(v)
                        }
                      }
                      Outcome::Failure(message: v_1) => {
                        let m = v_1 in {
                          write("Err:")
                          write_escaped(m)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                Err:something went wrong
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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
                  Win {
                    code: String,
                    body: String,
                  },
                  Lose {
                    reason: String,
                  },
                }

                view Test {
                  <let {
                    resp: Response = Response::Win {
                      code: "200",
                      body: "OK",
                    },
                  }>
                    <match {resp}>
                      <case {Response::Win {code: c, body: b}}>
                        {c}:{b}
                      </case>
                      <case {Response::Lose {reason: r}}>
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
                  Win {code: String, body: String},
                  Lose {reason: String},
                }
                view Test() {
                  let resp = Response::Win {code: "200", body: "OK"} in {
                    match resp {
                      Response::Win(code: v_0, body: v_1) => {
                        let c = v_0 in {
                          let b = v_1 in {
                            write_escaped(c)
                            write(":")
                            write_escaped(b)
                          }
                        }
                      }
                      Response::Lose(reason: v_2) => {
                        let r = v_2 in {
                          write("Error:")
                          write_escaped(r)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Response {
                  Win {code: String, body: String},
                  Lose {reason: String},
                }
                view Test() {
                  let resp = Response::Win {code: "200", body: "OK"} in {
                    match resp {
                      Response::Win(code: v_0, body: v_1) => {
                        let c = v_0 in {
                          let b = v_1 in {
                            write_escaped(c)
                            write(":")
                            write_escaped(b)
                          }
                        }
                      }
                      Response::Lose(reason: v_2) => {
                        let r = v_2 in {
                          write("Error:")
                          write_escaped(r)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                200:OK
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_with_shorthand_field_destructuring() {
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: Outcome = Outcome::Success {value: "hello"},
                  }>
                    <match {result}>
                      <case {Outcome::Success {value}}>
                        Ok:{value}
                      </case>
                      <case {Outcome::Failure {message}}>
                        Err:{message}
                      </case>
                    </match>
                  </let>
                }
            "#},
            "Ok:hello",
            expect![[r#"
                -- ir (unoptimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Success {value: "hello"} in {
                    match result {
                      Outcome::Success(value: v_0) => {
                        let value = v_0 in {
                          write("Ok:")
                          write_escaped(value)
                        }
                      }
                      Outcome::Failure(message: v_1) => {
                        let message = v_1 in {
                          write("Err:")
                          write_escaped(message)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Success {value: "hello"} in {
                    match result {
                      Outcome::Success(value: v_0) => {
                        let value = v_0 in {
                          write("Ok:")
                          write_escaped(value)
                        }
                      }
                      Outcome::Failure(message: v_1) => {
                        let message = v_1 in {
                          write("Err:")
                          write_escaped(message)
                        }
                      }
                    }
                  }
                }
                -- expected output --
                Ok:hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_simple() {
        check(
            indoc! {r#"
                view Test {
                  <let {items: Array[String] = ["a", "b", "c"]}>
                    {items.len().to_string()}
                  </let>
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let items = ["a", "b", "c"] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = ["a", "b", "c"] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- expected output --
                3
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_empty() {
        check(
            indoc! {r#"
                view Test {
                  <let {items: Array[String] = []}>
                    {items.len().to_string()}
                  </let>
                }
            "#},
            "0",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let items = [] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = [] in {
                    write_escaped(items.len().to_string())
                  }
                }
                -- expected output --
                0
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_in_comparison() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let items = ["x", "y"] in {
                    if (items.len() == 2) {
                      write("has two")
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = ["x", "y"] in {
                    if (items.len() == 2) {
                      write("has two")
                    }
                  }
                }
                -- expected output --
                has two
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_less_than() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let items = ["a"] in {
                    if (items.len() < 5) {
                      write("less than 5")
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = ["a"] in {
                    if (items.len() < 5) {
                      write("less than 5")
                    }
                  }
                }
                -- expected output --
                less than 5
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_length_int_array() {
        check(
            indoc! {r#"
                view Test {
                  <let {numbers: Array[Int] = [1, 2, 3, 4, 5]}>
                    {numbers.len().to_string()}
                  </let>
                }
            "#},
            "5",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let numbers = [1, 2, 3, 4, 5] in {
                    write_escaped(numbers.len().to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let numbers = [1, 2, 3, 4, 5] in {
                    write_escaped(numbers.len().to_string())
                  }
                }
                -- expected output --
                5
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_is_empty_true() {
        check(
            indoc! {r#"
                view Test {
                  <let {items: Array[String] = []}>
                    <match {items.is_empty()}>
                      <case {true}>
                        empty
                      </case>
                      <case {false}>
                        not empty
                      </case>
                    </match>
                  </let>
                }
            "#},
            "empty",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let items = [] in {
                    let v_0 = items.is_empty() in {
                      match v_0 {
                        true => {
                          write("empty")
                        }
                        false => {
                          write("not empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = [] in {
                    let v_0 = items.is_empty() in {
                      match v_0 {
                        true => {
                          write("empty")
                        }
                        false => {
                          write("not empty")
                        }
                      }
                    }
                  }
                }
                -- expected output --
                empty
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_is_empty_false() {
        check(
            indoc! {r#"
                view Test {
                  <let {items: Array[String] = ["a", "b"]}>
                    <match {items.is_empty()}>
                      <case {true}>
                        empty
                      </case>
                      <case {false}>
                        not empty
                      </case>
                    </match>
                  </let>
                }
            "#},
            "not empty",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let items = ["a", "b"] in {
                    let v_0 = items.is_empty() in {
                      match v_0 {
                        true => {
                          write("empty")
                        }
                        false => {
                          write("not empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = ["a", "b"] in {
                    let v_0 = items.is_empty() in {
                      match v_0 {
                        true => {
                          write("empty")
                        }
                        false => {
                          write("not empty")
                        }
                      }
                    }
                  }
                }
                -- expected output --
                not empty
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn array_is_empty_int_array() {
        check(
            indoc! {r#"
                view Test {
                  <let {numbers: Array[Int] = [1, 2, 3]}>
                    <match {numbers.is_empty()}>
                      <case {true}>
                        no numbers
                      </case>
                      <case {false}>
                        has numbers
                      </case>
                    </match>
                  </let>
                }
            "#},
            "has numbers",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let numbers = [1, 2, 3] in {
                    let v_0 = numbers.is_empty() in {
                      match v_0 {
                        true => {
                          write("no numbers")
                        }
                        false => {
                          write("has numbers")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let numbers = [1, 2, 3] in {
                    let v_0 = numbers.is_empty() in {
                      match v_0 {
                        true => {
                          write("no numbers")
                        }
                        false => {
                          write("has numbers")
                        }
                      }
                    }
                  }
                }
                -- expected output --
                has numbers
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_simple() {
        check(
            indoc! {r#"
                view Test {
                  <let {count: Int = 42}>
                    {count.to_string()}
                  </let>
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let count = 42 in {
                    write_escaped(count.to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let count = 42 in {
                    write_escaped(count.to_string())
                  }
                }
                -- expected output --
                42
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_zero() {
        check(
            indoc! {r#"
                view Test {
                  <let {num: Int = 0}>
                    {num.to_string()}
                  </let>
                }
            "#},
            "0",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let num = 0 in {
                    write_escaped(num.to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let num = 0 in {
                    write_escaped(num.to_string())
                  }
                }
                -- expected output --
                0
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_concat() {
        check(
            indoc! {r#"
                view Test {
                  <let {count: Int = 5}>
                    {"Count: " + count.to_string()}
                  </let>
                }
            "#},
            "Count: 5",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let count = 5 in {
                    write_escaped(("Count: " + count.to_string()))
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let count = 5 in {
                    write_escaped(("Count: " + count.to_string()))
                  }
                }
                -- expected output --
                Count: 5
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_simple() {
        check(
            indoc! {r#"
                view Test {
                  <let {price: Float = 3.7}>
                    {price.to_int().to_string()}
                  </let>
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let price = 3.7 in {
                    write_escaped(price.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let price = 3.7 in {
                    write_escaped(price.to_int().to_string())
                  }
                }
                -- expected output --
                3
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn float_to_int_whole_number() {
        check(
            indoc! {r#"
                view Test {
                  <let {num: Float = 5.0}>
                    {num.to_int().to_string()}
                  </let>
                }
            "#},
            "5",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let num = 5 in {
                    write_escaped(num.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let num = 5 in {
                    write_escaped(num.to_int().to_string())
                  }
                }
                -- expected output --
                5
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_range() {
        check(
            indoc! {r#"
                view Test {
                  <for {_ in 0..=2}>
                    x
                  </for>
                }
            "#},
            "xxx",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  for _ in 0..=2 {
                    write("x")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for _ in 0..=2 {
                    write("x")
                  }
                }
                -- expected output --
                xxx
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_array() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let items = ["a", "b", "c"] in {
                    for _ in items {
                      write("*")
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let items = ["a", "b", "c"] in {
                    for _ in items {
                      write("*")
                    }
                  }
                }
                -- expected output --
                ***
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_nested() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  for _ in 0..=1 {
                    for _ in 0..=2 {
                      write(".")
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for _ in 0..=1 {
                    for _ in 0..=2 {
                      write(".")
                    }
                  }
                }
                -- expected output --
                ......
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_with_underscore_mixed_with_named() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  for i in 1..=2 {
                    for _ in 0..=1 {
                      write_escaped(i.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  for i in 1..=2 {
                    for _ in 0..=1 {
                      write_escaped(i.to_string())
                    }
                  }
                }
                -- expected output --
                1122
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_array_literal() {
        check(
            indoc! {r#"
                view Test {
                  {[1, 2, 3].len().to_string()}
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write_escaped([1, 2, 3].len().to_string())
                }
                -- ir (optimized) --
                view Test() {
                  write_escaped([1, 2, 3].len().to_string())
                }
                -- expected output --
                3
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn method_call_on_parenthesized_expression() {
        check(
            indoc! {r#"
                view Test {
                  {(1 + 2).to_string()}
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write_escaped((1 + 2).to_string())
                }
                -- ir (optimized) --
                view Test() {
                  write_escaped((1 + 2).to_string())
                }
                -- expected output --
                3
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_literal_to_string() {
        check(
            indoc! {r#"
                view Test {
                  {42.to_string()}
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write_escaped(42.to_string())
                }
                -- ir (optimized) --
                view Test() {
                  write_escaped(42.to_string())
                }
                -- expected output --
                42
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_option_match() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match nested {
                      Some(v_0) => {
                        match v_0 {
                          Some(v_1) => {
                            let x = v_1 in {
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
                -- ir (optimized) --
                view Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match nested {
                      Some(v_0) => {
                        match v_0 {
                          Some(v_1) => {
                            let x = v_1 in {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_some_input() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let opt = Option[String]::Some("x") in {
                    match opt {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let opt = Option[String]::Some("x") in {
                    match opt {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_none_input() {
        check(
            indoc! {r#"
                view Test {
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
                view Test() {
                  let opt = Option[String]::None in {
                    match opt {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let opt = Option[String]::None in {
                    match opt {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_expr_some_input() {
        check(
            indoc! {r#"
                view Test {
                  <let {opt: Option[String] = Some("x")}>
                    {match opt {Some(_) => "some", None => "none"}}
                  </let>
                }
            "#},
            "some",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let opt = Option[String]::Some("x") in {
                    write_escaped(match opt {
                      Some(_) => "some",
                      None => "none",
                    })
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("some")
                }
                -- expected output --
                some
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_wildcard_match_expr_none_input() {
        check(
            indoc! {r#"
                view Test {
                  <let {opt: Option[String] = None}>
                    {match opt {Some(_) => "some", None => "none"}}
                  </let>
                }
            "#},
            "none",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let opt = Option[String]::None in {
                    write_escaped(match opt {
                      Some(_) => "some",
                      None => "none",
                    })
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("none")
                }
                -- expected output --
                none
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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
                view Test {
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
                view Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match nested {
                      Some(v_0) => {
                        match v_0 {
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
                -- ir (optimized) --
                view Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match nested {
                      Some(v_0) => {
                        match v_0 {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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
                view Test {
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
                view Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match nested {
                      Some(_) => {
                        write("some")
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let nested = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match nested {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_wildcard_binding_ok() {
        // Test Outcome::Success(value: _) - wildcard binding for enum field
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: Outcome = Outcome::Success {value: "hello"},
                  }>
                    <match {result}>
                      <case {Outcome::Success {value: _}}>
                        ok
                      </case>
                      <case {Outcome::Failure {message: _}}>
                        err
                      </case>
                    </match>
                  </let>
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Success {value: "hello"} in {
                    match result {
                      Outcome::Success => {
                        write("ok")
                      }
                      Outcome::Failure => {
                        write("err")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Success {value: "hello"} in {
                    match result {
                      Outcome::Success => {
                        write("ok")
                      }
                      Outcome::Failure => {
                        write("err")
                      }
                    }
                  }
                }
                -- expected output --
                ok
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_wildcard_binding_err() {
        // Test Outcome::Failure(message: _) - wildcard binding for enum field
        check(
            indoc! {r#"
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: Outcome = Outcome::Failure {message: "failed"},
                  }>
                    <match {result}>
                      <case {Outcome::Success {value: _}}>
                        ok
                      </case>
                      <case {Outcome::Failure {message: _}}>
                        err
                      </case>
                    </match>
                  </let>
                }
            "#},
            "err",
            expect![[r#"
                -- ir (unoptimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Failure {message: "failed"} in {
                    match result {
                      Outcome::Success => {
                        write("ok")
                      }
                      Outcome::Failure => {
                        write("err")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outcome::Failure {message: "failed"} in {
                    match result {
                      Outcome::Success => {
                        write("ok")
                      }
                      Outcome::Failure => {
                        write("err")
                      }
                    }
                  }
                }
                -- expected output --
                err
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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

                view Test {
                  <let {person: Person = Person {name: "Alice", age: 30}}>
                    <match {person}>
                      <case {Person {name: _, age: a}}>
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
                view Test() {
                  let person = Person {name: "Alice", age: 30} in {
                    let v_1 = person.age in {
                      let a = v_1 in {
                        write("age:")
                        write_escaped(a.to_string())
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Person {
                  name: String,
                  age: Int,
                }
                view Test() {
                  let person = Person {name: "Alice", age: 30} in {
                    let v_1 = person.age in {
                      let a = v_1 in {
                        write("age:")
                        write_escaped(a.to_string())
                      }
                    }
                  }
                }
                -- expected output --
                age:30
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
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
                view Test {
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
                view Test() {
                  let deep = Option[Option[Option[String]]]::Some(Option[Option[String]]::Some(Option[String]::Some("value"))) in {
                    match deep {
                      Some(v_0) => {
                        match v_0 {
                          Some(v_1) => {
                            match v_1 {
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
                -- ir (optimized) --
                view Test() {
                  let deep = Option[Option[Option[String]]]::Some(Option[Option[String]]::Some(Option[String]::Some("value"))) in {
                    match deep {
                      Some(v_0) => {
                        match v_0 {
                          Some(v_1) => {
                            match v_1 {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_enum_wildcard() {
        // Test nested enum matching with wildcard - Outer::Success(value: Inner::Success(value: _))
        check(
            indoc! {r#"
                enum Inner {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                enum Outer {
                  Success {
                    value: Inner,
                  },
                  Failure {
                    message: String,
                  },
                }

                view Test {
                  <let {
                    result: Outer = Outer::Success {
                      value: Inner::Success {value: "deep"},
                    },
                  }>
                    <match {result}>
                      <case {Outer::Success {
                        value: Inner::Success {value: _},
                      }}>
                        ok-ok
                      </case>
                      <case {Outer::Success {
                        value: Inner::Failure {message: _},
                      }}>
                        ok-err
                      </case>
                      <case {Outer::Failure {message: _}}>
                        err
                      </case>
                    </match>
                  </let>
                }
            "#},
            "ok-ok",
            expect![[r#"
                -- ir (unoptimized) --
                enum Inner {
                  Success {value: String},
                  Failure {message: String},
                }
                enum Outer {
                  Success {value: test::Inner},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outer::Success {value: Inner::Success {value: "deep"}} in {
                    match result {
                      Outer::Success(value: v_0) => {
                        match v_0 {
                          Inner::Success => {
                            write("ok-ok")
                          }
                          Inner::Failure => {
                            write("ok-err")
                          }
                        }
                      }
                      Outer::Failure => {
                        write("err")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Inner {
                  Success {value: String},
                  Failure {message: String},
                }
                enum Outer {
                  Success {value: test::Inner},
                  Failure {message: String},
                }
                view Test() {
                  let result = Outer::Success {value: Inner::Success {value: "deep"}} in {
                    match result {
                      Outer::Success(value: v_0) => {
                        match v_0 {
                          Inner::Success => {
                            write("ok-ok")
                          }
                          Inner::Failure => {
                            write("ok-err")
                          }
                        }
                      }
                      Outer::Failure => {
                        write("err")
                      }
                    }
                  }
                }
                -- expected output --
                ok-ok
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_partial_wildcard_true() {
        // Test bool match with one explicit case and wildcard - match b {true => "t", _ => "f"}
        check(
            indoc! {r#"
                view Test {
                  <let {b: Bool = true}>
                    {match b {true => "t", _ => "f"}}
                  </let>
                }
            "#},
            "t",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let b = true in {
                    write_escaped(match b {true => "t", false => "f"})
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("t")
                }
                -- expected output --
                t
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn bool_match_partial_wildcard_false() {
        // Test bool match with wildcard matching false
        check(
            indoc! {r#"
                view Test {
                  <let {b: Bool = false}>
                    {match b {true => "t", _ => "f"}}
                  </let>
                }
            "#},
            "f",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let b = false in {
                    write_escaped(match b {true => "t", false => "f"})
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("f")
                }
                -- expected output --
                f
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_match_statements_with_literal_subjects() {
        check(
            indoc! {r#"
                view Test {
                  <match {Some("outer")}>
                    <case {Some(x)}>
                      <match {Some("inner")}>
                        <case {Some(y)}>
                          {x}:{y}
                        </case>
                        <case {None}>
                          inner-none
                        </case>
                      </match>
                    </case>
                    <case {None}>
                      outer-none
                    </case>
                  </match>
                }
            "#},
            "outer:inner",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let v_0 = Option[String]::Some("outer") in {
                    match v_0 {
                      Some(v_1) => {
                        let x = v_1 in {
                          let v_2 = Option[String]::Some("inner") in {
                            match v_2 {
                              Some(v_3) => {
                                let y = v_3 in {
                                  write_escaped(x)
                                  write(":")
                                  write_escaped(y)
                                }
                              }
                              None => {
                                write("inner-none")
                              }
                            }
                          }
                        }
                      }
                      None => {
                        write("outer-none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let v_0 = Option[String]::Some("outer") in {
                    match v_0 {
                      Some(v_1) => {
                        let x = v_1 in {
                          let v_2 = Option[String]::Some("inner") in {
                            match v_2 {
                              Some(v_3) => {
                                let y = v_3 in {
                                  write_escaped(x)
                                  write(":")
                                  write_escaped(y)
                                }
                              }
                              None => {
                                write("inner-none")
                              }
                            }
                          }
                        }
                      }
                      None => {
                        write("outer-none")
                      }
                    }
                  }
                }
                -- expected output --
                outer:inner
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_match_statements_with_variable_subjects() {
        check(
            indoc! {r#"
                view Test {
                  <let {
                    outer: Option[Option[String]] = Some(Some("hello")),
                  }>
                    <match {outer}>
                      <case {Some(inner)}>
                        <match {inner}>
                          <case {Some(value)}>
                            value:{value}
                          </case>
                          <case {None}>
                            inner-none
                          </case>
                        </match>
                      </case>
                      <case {None}>
                        outer-none
                      </case>
                    </match>
                  </let>
                }
            "#},
            "value:hello",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let outer = Option[Option[String]]::Some(Option[String]::Some("hello")) in {
                    match outer {
                      Some(v_0) => {
                        let inner = v_0 in {
                          match inner {
                            Some(v_1) => {
                              let value = v_1 in {
                                write("value:")
                                write_escaped(value)
                              }
                            }
                            None => {
                              write("inner-none")
                            }
                          }
                        }
                      }
                      None => {
                        write("outer-none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let outer = Option[Option[String]]::Some(Option[String]::Some("hello")) in {
                    match outer {
                      Some(v_0) => {
                        let inner = v_0 in {
                          match inner {
                            Some(v_1) => {
                              let value = v_1 in {
                                write("value:")
                                write_escaped(value)
                              }
                            }
                            None => {
                              write("inner-none")
                            }
                          }
                        }
                      }
                      None => {
                        write("outer-none")
                      }
                    }
                  }
                }
                -- expected output --
                value:hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn join_macro_merges_css_classes() {
        check(
            indoc! {r#"
                view Test {
                  <div class={
                    join!(
                      "foo",
                      "bar",
                      "baz",
                    )
                  }>
                  </div>
                }
            "#},
            r#"<div class="foo bar baz"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("<div")
                  write(" class=\"")
                  write_escaped(tw_merge((("foo" + " ") + (("bar" + " ") + "baz"))))
                  write("\"")
                  write(">")
                  write("</div>")
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"foo bar baz\"></div>")
                }
                -- expected output --
                <div class="foo bar baz"></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn reserved_keyword_as_variable_name_typescript() {
        check(
            indoc! {r#"
                view Test {
                  <let {delete: String = "removed"}>
                    {delete}
                  </let>
                }
            "#},
            "removed",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let delete_1 = "removed" in {
                    write_escaped(delete_1)
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("removed")
                }
                -- expected output --
                removed
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn reserved_keyword_class_as_variable_name() {
        check(
            indoc! {r#"
                view Test {
                  <let {class: String = "my-class"}>
                    <div class={class}>
                    </div>
                  </let>
                }
            "#},
            r#"<div class="my-class"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let class_1 = "my-class" in {
                    write("<div")
                    write(" class=\"")
                    write_escaped(tw_merge(class_1))
                    write("\"")
                    write(">")
                    write("</div>")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"my-class\"></div>")
                }
                -- expected output --
                <div class="my-class"></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn reserved_keyword_switch_as_variable_name() {
        check(
            indoc! {r#"
                view Test {
                  <let {switch: String = "on"}>
                    <span>
                      {switch}
                    </span>
                  </let>
                }
            "#},
            r#"<span>on</span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let switch_1 = "on" in {
                    write("<span")
                    write(">")
                    write_escaped(switch_1)
                    write("</span>")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<span>on</span>")
                }
                -- expected output --
                <span>on</span>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn unreserved_keyword_type_as_variable_name() {
        check(
            indoc! {r#"
                view Test {
                  <let {type: String = "button"}>
                    <input type={type}>
                  </let>
                }
            "#},
            r#"<input type="button">"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let type_1 = "button" in {
                    write("<input")
                    write(" type=\"")
                    write_escaped(type_1)
                    write("\"")
                    write(">")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<input type=\"button\">")
                }
                -- expected output --
                <input type="button">
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn unreserved_keyword_for_as_attribute_name() {
        check(
            indoc! {r#"
                view Test {
                  <label for="email">
                    Email
                  </label>
                }
            "#},
            r#"<label for="email">Email</label>"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("<label")
                  write(" for=\"email\"")
                  write(">")
                  write("Email")
                  write("</label>")
                }
                -- ir (optimized) --
                view Test() {
                  write("<label for=\"email\">Email</label>")
                }
                -- expected output --
                <label for="email">Email</label>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn escape_sequences() {
        check(
            indoc! {r#"
                view Test {
                  {"\""}
                  {"\\"}
                  {"foo\nbar"}
                  {"foo\tbar"}
                  {"C:\\Users\\name"}
                }
            "#},
            "&quot; \\ foo\nbar foo\tbar C:\\Users\\name",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write_escaped("\"")
                  write(" ")
                  write_escaped("\\")
                  write(" ")
                  write_escaped("foo\nbar")
                  write(" ")
                  write_escaped("foo\tbar")
                  write(" ")
                  write_escaped("C:\\Users\\name")
                }
                -- ir (optimized) --
                view Test() {
                  write("&quot; \\ foo\nbar foo\tbar C:\\Users\\name")
                }
                -- expected output --
                &quot; \ foo
                bar foo	bar C:\Users\name
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_record_field_in_let_binding() {
        check(
            indoc! {r#"
                record Item {
                  name: String,
                  value: String,
                }

                view Test {
                  <let {
                    items: Array[Item] = [
                      Item {name: "a", value: "1"},
                      Item {name: "b", value: "2"},
                    ],
                  }>
                    <for {item in items}>
                      <let {n: String = item.name}>
                        [{n}]
                      </let>
                    </for>
                  </let>
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
                record Item {
                  name: String,
                  value: String,
                }
                view Test() {
                  let items = [
                    Item {name: "a", value: "1"},
                    Item {name: "b", value: "2"},
                  ] in {
                    for item in items {
                      let n = item.name in {
                        write("[")
                        write_escaped(n)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Item {
                  name: String,
                  value: String,
                }
                view Test() {
                  let items = [
                    Item {name: "a", value: "1"},
                    Item {name: "b", value: "2"},
                  ] in {
                    for item in items {
                      let n = item.name in {
                        write("[")
                        write_escaped(n)
                        write("]")
                      }
                    }
                  }
                }
                -- expected output --
                [a][b]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_nested_record_field_in_let_binding() {
        check(
            indoc! {r#"
                record Address {
                  city: String,
                }

                record Person {
                  name: String,
                  address: Address,
                }

                view Test {
                  <let {
                    people: Array[Person] = [
                      Person {
                        name: "alice",
                        address: Address {city: "paris"},
                      },
                      Person {
                        name: "bob",
                        address: Address {city: "london"},
                      },
                    ],
                  }>
                    <for {person in people}>
                      <let {city: String = person.address.city}>
                        [{city}]
                      </let>
                    </for>
                  </let>
                }
            "#},
            "[paris][london]",
            expect![[r#"
                -- ir (unoptimized) --
                record Address {
                  city: String,
                }
                record Person {
                  name: String,
                  address: Address,
                }
                view Test() {
                  let people = [
                    Person {
                      name: "alice",
                      address: Address {city: "paris"},
                    },
                    Person {name: "bob", address: Address {city: "london"}},
                  ] in {
                    for person in people {
                      let city = person.address.city in {
                        write("[")
                        write_escaped(city)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Address {
                  city: String,
                }
                record Person {
                  name: String,
                  address: Address,
                }
                view Test() {
                  let people = [
                    Person {
                      name: "alice",
                      address: Address {city: "paris"},
                    },
                    Person {name: "bob", address: Address {city: "london"}},
                  ] in {
                    for person in people {
                      let city = person.address.city in {
                        write("[")
                        write_escaped(city)
                        write("]")
                      }
                    }
                  }
                }
                -- expected output --
                [paris][london]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_record_field_in_record_literal() {
        check(
            indoc! {r#"
                record Source {
                  name: String,
                  value: String,
                }

                record Target {
                  label: String,
                }

                view Test {
                  <let {
                    sources: Array[Source] = [
                      Source {name: "a", value: "1"},
                      Source {name: "b", value: "2"},
                    ],
                  }>
                    <for {src in sources}>
                      <let {target: Target = Target {label: src.name}}>
                        [{target.label}]
                      </let>
                    </for>
                  </let>
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
                record Source {
                  name: String,
                  value: String,
                }
                record Target {
                  label: String,
                }
                view Test() {
                  let sources = [
                    Source {name: "a", value: "1"},
                    Source {name: "b", value: "2"},
                  ] in {
                    for src in sources {
                      let target = Target {label: src.name} in {
                        write("[")
                        write_escaped(target.label)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Source {
                  name: String,
                  value: String,
                }
                record Target {
                  label: String,
                }
                view Test() {
                  let sources = [
                    Source {name: "a", value: "1"},
                    Source {name: "b", value: "2"},
                  ] in {
                    for src in sources {
                      let target = Target {label: src.name} in {
                        write("[")
                        write_escaped(target.label)
                        write("]")
                      }
                    }
                  }
                }
                -- expected output --
                [a][b]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_record_field_in_option_construction() {
        check(
            indoc! {r#"
                record Item {
                  name: String,
                }

                view Test {
                  <let {
                    items: Array[Item] = [
                      Item {name: "a"},
                      Item {name: "b"},
                    ],
                  }>
                    <for {item in items}>
                      <let {opt: Option[String] = Some(item.name)}>
                        <match {opt}>
                          <case {Some(s)}>
                            [{s}]
                          </case>
                          <case {None}>
                            [-]
                          </case>
                        </match>
                      </let>
                    </for>
                  </let>
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
                record Item {
                  name: String,
                }
                view Test() {
                  let items = [Item {name: "a"}, Item {name: "b"}] in {
                    for item in items {
                      let opt = Option[String]::Some(item.name) in {
                        match opt {
                          Some(v_0) => {
                            let s = v_0 in {
                              write("[")
                              write_escaped(s)
                              write("]")
                            }
                          }
                          None => {
                            write("[-]")
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Item {
                  name: String,
                }
                view Test() {
                  let items = [Item {name: "a"}, Item {name: "b"}] in {
                    for item in items {
                      let opt = Option[String]::Some(item.name) in {
                        match opt {
                          Some(v_0) => {
                            let s = v_0 in {
                              write("[")
                              write_escaped(s)
                              write("]")
                            }
                          }
                          None => {
                            write("[-]")
                          }
                        }
                      }
                    }
                  }
                }
                -- expected output --
                [a][b]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concat_in_let_binding() {
        check(
            indoc! {r#"
                view Test {
                  <let {a: String = "hello"}>
                    <let {b: String = "world"}>
                      <let {c: String = a + " " + b}>
                        [{c}]
                      </let>
                    </let>
                  </let>
                }
            "#},
            "[hello world]",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let a = "hello" in {
                    let b = "world" in {
                      let c = ((a + " ") + b) in {
                        write("[")
                        write_escaped(c)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("[hello world]")
                }
                -- expected output --
                [hello world]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_concat_in_record_field() {
        check(
            indoc! {r#"
                record Greeting {
                  message: String,
                }

                view Test {
                  <let {
                    g: Greeting = Greeting {message: "hello" + " world"},
                  }>
                    {g.message}
                  </let>
                }
            "#},
            "hello world",
            expect![[r#"
                -- ir (unoptimized) --
                record Greeting {
                  message: String,
                }
                view Test() {
                  let g = Greeting {message: ("hello" + " world")} in {
                    write_escaped(g.message)
                  }
                }
                -- ir (optimized) --
                record Greeting {
                  message: String,
                }
                view Test() {
                  let g = Greeting {message: "hello world"} in {
                    write_escaped(g.message)
                  }
                }
                -- expected output --
                hello world
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_in_let_binding() {
        check(
            indoc! {r#"
                view Test {
                  <let {n: Int = 42}>
                    <let {s: String = n.to_string()}>
                      [{s}]
                    </let>
                  </let>
                }
            "#},
            "[42]",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let n = 42 in {
                    let s = n.to_string() in {
                      write("[")
                      write_escaped(s)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let n = 42 in {
                    let s = n.to_string() in {
                      write("[")
                      write_escaped(s)
                      write("]")
                    }
                  }
                }
                -- expected output --
                [42]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn record_with_array_field() {
        check(
            indoc! {r#"
                record Container {
                  items: Array[String],
                }

                view Test {
                  <let {c: Container = Container {items: ["a", "b"]}}>
                    <for {item in c.items}>
                      [{item}]
                    </for>
                  </let>
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
                record Container {
                  items: Array[String],
                }
                view Test() {
                  let c = Container {items: ["a", "b"]} in {
                    for item in c.items {
                      write("[")
                      write_escaped(item)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                record Container {
                  items: Array[String],
                }
                view Test() {
                  let c = Container {items: ["a", "b"]} in {
                    for item in c.items {
                      write("[")
                      write_escaped(item)
                      write("]")
                    }
                  }
                }
                -- expected output --
                [a][b]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn int_to_string_in_record_field() {
        check(
            indoc! {r#"
                record Label {
                  text: String,
                }

                view Test {
                  <let {l: Label = Label {text: 42.to_string()}}>
                    [{l.text}]
                  </let>
                }
            "#},
            "[42]",
            expect![[r#"
                -- ir (unoptimized) --
                record Label {
                  text: String,
                }
                view Test() {
                  let l = Label {text: 42.to_string()} in {
                    write("[")
                    write_escaped(l.text)
                    write("]")
                  }
                }
                -- ir (optimized) --
                record Label {
                  text: String,
                }
                view Test() {
                  let l = Label {text: 42.to_string()} in {
                    write("[")
                    write_escaped(l.text)
                    write("]")
                  }
                }
                -- expected output --
                [42]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_record_with_array() {
        check(
            indoc! {r#"
                record Inner {
                  values: Array[String],
                }

                record Outer {
                  inner: Inner,
                }

                view Test {
                  <let {
                    o: Outer = Outer {inner: Inner {values: ["x", "y"]}},
                  }>
                    <for {v in o.inner.values}>
                      [{v}]
                    </for>
                  </let>
                }
            "#},
            "[x][y]",
            expect![[r#"
                -- ir (unoptimized) --
                record Inner {
                  values: Array[String],
                }
                record Outer {
                  inner: Inner,
                }
                view Test() {
                  let o = Outer {inner: Inner {values: ["x", "y"]}} in {
                    for v in o.inner.values {
                      write("[")
                      write_escaped(v)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                record Inner {
                  values: Array[String],
                }
                record Outer {
                  inner: Inner,
                }
                view Test() {
                  let o = Outer {inner: Inner {values: ["x", "y"]}} in {
                    for v in o.inner.values {
                      write("[")
                      write_escaped(v)
                      write("]")
                    }
                  }
                }
                -- expected output --
                [x][y]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn move_field_into_record_literal() {
        check(
            indoc! {r#"
                record Foo {
                  a: String,
                }

                view Test {
                  <let {x: Foo = Foo {a: "hello"}, y: Foo = Foo {a: x.a}}>
                    [{x.a}][{y.a}]
                  </let>
                }
            "#},
            "[hello][hello]",
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  a: String,
                }
                view Test() {
                  let x = Foo {a: "hello"} in {
                    let y = Foo {a: x.a} in {
                      write("[")
                      write_escaped(x.a)
                      write("][")
                      write_escaped(y.a)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                record Foo {
                  a: String,
                }
                view Test() {
                  let x = Foo {a: "hello"} in {
                    let y = Foo {a: x.a} in {
                      write("[")
                      write_escaped(x.a)
                      write("][")
                      write_escaped(y.a)
                      write("]")
                    }
                  }
                }
                -- expected output --
                [hello][hello]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn match_expr_field_access_reused() {
        check(
            indoc! {r#"
                record Foo {
                  a: String,
                }

                view Test {
                  <let {x: Foo = Foo {a: "hello"}, b: Bool = true}>
                    <let {
                      result: String = match b {
                        true => x.a,
                        false => "default",
                      },
                    }>
                      [{result}][{x.a}]
                    </let>
                  </let>
                }
            "#},
            "[hello][hello]",
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  a: String,
                }
                view Test() {
                  let x = Foo {a: "hello"} in {
                    let b = true in {
                      let result = match b {
                        true => x.a,
                        false => "default",
                      } in {
                        write("[")
                        write_escaped(result)
                        write("][")
                        write_escaped(x.a)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Foo {
                  a: String,
                }
                view Test() {
                  let x = Foo {a: "hello"} in {
                    let b = true in {
                      let result = match b {
                        true => x.a,
                        false => "default",
                      } in {
                        write("[")
                        write_escaped(result)
                        write("][")
                        write_escaped(x.a)
                        write("]")
                      }
                    }
                  }
                }
                -- expected output --
                [hello][hello]
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn self_referential_record_with_array() {
        check(
            indoc! {r#"
                record TreeNode {
                  value: String,
                  children: Array[TreeNode],
                }

                view Test {
                  <let {
                    leaf: TreeNode = TreeNode {value: "leaf", children: []},
                  }>
                    {leaf.value}
                  </let>
                }
            "#},
            "leaf",
            expect![[r#"
                -- ir (unoptimized) --
                record TreeNode {
                  value: String,
                  children: Array[test::TreeNode],
                }
                view Test() {
                  let leaf = TreeNode {value: "leaf", children: []} in {
                    write_escaped(leaf.value)
                  }
                }
                -- ir (optimized) --
                record TreeNode {
                  value: String,
                  children: Array[test::TreeNode],
                }
                view Test() {
                  let leaf = TreeNode {value: "leaf", children: []} in {
                    write_escaped(leaf.value)
                  }
                }
                -- expected output --
                leaf
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn self_referential_record_with_option() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                view Test {
                  <let {node: Node = Node {value: "first", next: None}}>
                    {node.value}
                  </let>
                }
            "#},
            "first",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                view Test() {
                  let node = Node {
                    value: "first",
                    next: Option[test::Node]::None,
                  } in {
                    write_escaped(node.value)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                view Test() {
                  let node = Node {
                    value: "first",
                    next: Option[test::Node]::None,
                  } in {
                    write_escaped(node.value)
                  }
                }
                -- expected output --
                first
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn self_referential_enum() {
        check(
            indoc! {r#"
                enum Expr {
                  Literal {
                    value: String,
                  },
                  Neg {
                    inner: Expr,
                  },
                }

                view Test {
                  <let {e: Expr = Expr::Literal {value: "42"}}>
                    <match {e}>
                      <case {Expr::Literal {value: v}}>
                        {v}
                      </case>
                      <case {Expr::Neg {inner: _}}>
                        neg
                      </case>
                    </match>
                  </let>
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                enum Expr {
                  Literal {value: String},
                  Neg {inner: test::Expr},
                }
                view Test() {
                  let e = Expr::Literal {value: "42"} in {
                    match e {
                      Expr::Literal(value: v_0) => {
                        let v = v_0 in {
                          write_escaped(v)
                        }
                      }
                      Expr::Neg => {
                        write("neg")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Expr {
                  Literal {value: String},
                  Neg {inner: test::Expr},
                }
                view Test() {
                  let e = Expr::Literal {value: "42"} in {
                    match e {
                      Expr::Literal(value: v_0) => {
                        let v = v_0 in {
                          write_escaped(v)
                        }
                      }
                      Expr::Neg => {
                        write("neg")
                      }
                    }
                  }
                }
                -- expected output --
                42
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_self_referential_enum() {
        check(
            indoc! {r#"
                enum Expr {
                  Literal {
                    value: String,
                  },
                  Neg {
                    inner: Expr,
                  },
                }

                view Test {
                  <let {
                    e: Expr = Expr::Neg {
                      inner: Expr::Literal {value: "42"},
                    },
                  }>
                    <match {e}>
                      <case {Expr::Literal {value: v}}>
                        lit:{v}
                      </case>
                      <case {Expr::Neg {inner: _}}>
                        neg
                      </case>
                    </match>
                  </let>
                }
            "#},
            "neg",
            expect![[r#"
                -- ir (unoptimized) --
                enum Expr {
                  Literal {value: String},
                  Neg {inner: test::Expr},
                }
                view Test() {
                  let e = Expr::Neg {inner: Expr::Literal {value: "42"}} in {
                    match e {
                      Expr::Literal(value: v_0) => {
                        let v = v_0 in {
                          write("lit:")
                          write_escaped(v)
                        }
                      }
                      Expr::Neg => {
                        write("neg")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Expr {
                  Literal {value: String},
                  Neg {inner: test::Expr},
                }
                view Test() {
                  let e = Expr::Neg {inner: Expr::Literal {value: "42"}} in {
                    match e {
                      Expr::Literal(value: v_0) => {
                        let v = v_0 in {
                          write("lit:")
                          write_escaped(v)
                        }
                      }
                      Expr::Neg => {
                        write("neg")
                      }
                    }
                  }
                }
                -- expected output --
                neg
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn simple_component_call() {
        check(
            indoc! {r#"
                component Greeting(name: String) {
                  Hello, {name}!
                }

                view Test {
                  <Greeting name="World"/>
                }
            "#},
            "Hello, World!",
            expect![[r#"
                -- ir (unoptimized) --
                component Greeting(name: String) {
                  write("Hello, ")
                  write_escaped(name)
                  write("!")
                }
                view Test() {
                  call Greeting(name = "World")
                }
                -- ir (optimized) --
                view Test() {
                  write("Hello, World!")
                }
                -- expected output --
                Hello, World!
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_children() {
        check(
            indoc! {r#"
                component Card(
                  title: String,
                  slot: Slot,
                ) {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {slot}
                  </div>
                }

                view Test {
                  <Card title="Hello">
                    <p>
                      world
                    </p>
                  </Card>
                }
            "#},
            r#"<div class="card"><h2>Hello</h2><p>world</p></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(title: String) [children] {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_escaped(title)
                  write("</h2>")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "Hello") {
                    write("<p")
                    write(">")
                    write("world")
                    write("</p>")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"card\"><h2>Hello</h2><p>world</p></div>")
                }
                -- expected output --
                <div class="card"><h2>Hello</h2><p>world</p></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_children_forwarded_to_another_component() {
        check(
            indoc! {r#"
                component Inner(slot: Slot) {
                  <div class="inner">
                    {slot}
                  </div>
                }

                component Outer(slot: Slot) {
                  <div class="outer">
                    <Inner>
                      {slot}
                    </Inner>
                  </div>
                }

                view Test {
                  <Outer>
                    <p>
                      hello
                    </p>
                  </Outer>
                }
            "#},
            r#"<div class="outer"><div class="inner"><p>hello</p></div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Inner() [children] {
                  write("<div")
                  write(" class=\"inner\"")
                  write(">")
                  write_expr(slot)
                  write("</div>")
                }
                component Outer() [children] {
                  write("<div")
                  write(" class=\"outer\"")
                  write(">")
                  call Inner() {
                    write_expr(slot)
                  }
                  write("</div>")
                }
                view Test() {
                  call Outer() {
                    write("<p")
                    write(">")
                    write("hello")
                    write("</p>")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"outer\"><div class=\"inner\"><p>hello</p></div>")
                  write("</div>")
                }
                -- expected output --
                <div class="outer"><div class="inner"><p>hello</p></div></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_children_with_component_calls() {
        check(
            indoc! {r#"
                component Header(title: String) {
                  <header>
                    <h1>
                      {title}
                    </h1>
                  </header>
                }

                component Footer {
                  <footer>
                    <p>
                      Copyright 2024
                    </p>
                  </footer>
                }

                component Layout(slot: Slot) {
                  <div class="layout">
                    {slot}
                  </div>
                }

                view Test {
                  <Layout>
                    <Header title="Welcome"/>
                    <main>
                      <p>
                        Hello world
                      </p>
                    </main>
                    <Footer/>
                  </Layout>
                }
            "#},
            r#"<div class="layout"><header><h1>Welcome</h1></header><main><p>Hello world</p></main><footer><p>Copyright 2024</p></footer></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Header(title: String) {
                  write("<header")
                  write(">")
                  write("<h1")
                  write(">")
                  write_escaped(title)
                  write("</h1>")
                  write("</header>")
                }
                component Footer() {
                  write("<footer")
                  write(">")
                  write("<p")
                  write(">")
                  write("Copyright 2024")
                  write("</p>")
                  write("</footer>")
                }
                component Layout() [children] {
                  write("<div")
                  write(" class=\"layout\"")
                  write(">")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  call Layout() {
                    call Header(title = "Welcome")
                    write("<main")
                    write(">")
                    write("<p")
                    write(">")
                    write("Hello world")
                    write("</p>")
                    write("</main>")
                    call Footer()
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"layout\"><header><h1>Welcome</h1></header><main>")
                  write("<p>Hello world</p></main><footer><p>Copyright 2024</p>")
                  write("</footer></div>")
                }
                -- expected output --
                <div class="layout"><header><h1>Welcome</h1></header><main><p>Hello world</p></main><footer><p>Copyright 2024</p></footer></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_children_aliased_through_let() {
        check(
            indoc! {r#"
                component Wrapper(slot: Slot) {
                  <let {content: Slot = slot}>
                    <div class="wrapper">
                      {content}
                    </div>
                  </let>
                }

                view Test {
                  <Wrapper>
                    <span>
                      hello
                    </span>
                  </Wrapper>
                }
            "#},
            r#"<div class="wrapper"><span>hello</span></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Wrapper() [children] {
                  let content = slot in {
                    write("<div")
                    write(" class=\"wrapper\"")
                    write(">")
                    write_expr(content)
                    write("</div>")
                  }
                }
                view Test() {
                  call Wrapper() {
                    write("<span")
                    write(">")
                    write("hello")
                    write("</span>")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"wrapper\"><span>hello</span></div>")
                }
                -- expected output --
                <div class="wrapper"><span>hello</span></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_children_used_twice() {
        check(
            indoc! {r#"
                component Repeat(slot: Slot) {
                  <div class="first">
                    {slot}
                  </div>
                  <div class="second">
                    {slot}
                  </div>
                }

                view Test {
                  <Repeat>
                    <span>
                      hi
                    </span>
                  </Repeat>
                }
            "#},
            r#"<div class="first"><span>hi</span></div><div class="second"><span>hi</span></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Repeat() [children] {
                  write("<div")
                  write(" class=\"first\"")
                  write(">")
                  write_expr(slot)
                  write("</div>")
                  write("<div")
                  write(" class=\"second\"")
                  write(">")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  call Repeat() {
                    write("<span")
                    write(">")
                    write("hi")
                    write("</span>")
                  }
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"first\"><span>hi</span></div><div class=\"second\"")
                  write("><span>hi</span></div>")
                }
                -- expected output --
                <div class="first"><span>hi</span></div><div class="second"><span>hi</span></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn recursive_component_with_non_recursive_sibling() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                component Badge(text: String) {
                  <strong>
                    {text}
                  </strong>
                }

                component NodeView(node: Node) {
                  <Badge text={node.value}/>
                  <match {node.next}>
                    <case {Some(next)}>
                      <NodeView node={next}/>
                    </case>
                    <case {None}>
                    </case>
                  </match>
                }

                view Test {
                  <let {
                    list: Node = Node {
                      value: "a",
                      next: Some(Node {value: "b", next: None}),
                    },
                  }>
                    <NodeView node={list}/>
                  </let>
                }
            "#},
            "<strong>a</strong><strong>b</strong>",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                component Badge(text: String) {
                  write("<strong")
                  write(">")
                  write_escaped(text)
                  write("</strong>")
                }
                component NodeView(node: test::Node) {
                  call Badge(text = node.value)
                  let v_0 = node.next in {
                    match v_0 {
                      Some(v_1) => {
                        let next = v_1 in {
                          call NodeView(node = next)
                        }
                      }
                      None => {
                      }
                    }
                  }
                }
                view Test() {
                  let list = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    call NodeView(node = list)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                component NodeView(node: test::Node) {
                  let text = node.value in {
                    write("<strong>")
                    write_escaped(text)
                    write("</strong>")
                  }
                  let v_0 = node.next in {
                    match v_0 {
                      Some(v_1) => {
                        let next = v_1 in {
                          call NodeView(node = next)
                        }
                      }
                      None => {
                      }
                    }
                  }
                }
                view Test() {
                  let list = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    call NodeView(node = list)
                  }
                }
                -- expected output --
                <strong>a</strong><strong>b</strong>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn recursive_component_with_linked_list() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                component NodeView(node: Node) {
                  <span>
                    {node.value}
                  </span>
                  <match {node.next}>
                    <case {Some(next)}>
                      <NodeView node={next}/>
                    </case>
                    <case {None}>
                    </case>
                  </match>
                }

                view Test {
                  <let {
                    list: Node = Node {
                      value: "a",
                      next: Some(
                        Node {
                          value: "b",
                          next: Some(Node {value: "c", next: None}),
                        }
                      ),
                    },
                  }>
                    <NodeView node={list}/>
                  </let>
                }
            "#},
            "<span>a</span><span>b</span><span>c</span>",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                component NodeView(node: test::Node) {
                  write("<span")
                  write(">")
                  write_escaped(node.value)
                  write("</span>")
                  let v_0 = node.next in {
                    match v_0 {
                      Some(v_1) => {
                        let next = v_1 in {
                          call NodeView(node = next)
                        }
                      }
                      None => {
                      }
                    }
                  }
                }
                view Test() {
                  let list = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::Some(Node {
                        value: "c",
                        next: Option[test::Node]::None,
                      }),
                    }),
                  } in {
                    call NodeView(node = list)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                component NodeView(node: test::Node) {
                  write("<span>")
                  write_escaped(node.value)
                  write("</span>")
                  let v_0 = node.next in {
                    match v_0 {
                      Some(v_1) => {
                        let next = v_1 in {
                          call NodeView(node = next)
                        }
                      }
                      None => {
                      }
                    }
                  }
                }
                view Test() {
                  let list = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::Some(Node {
                        value: "c",
                        next: Option[test::Node]::None,
                      }),
                    }),
                  } in {
                    call NodeView(node = list)
                  }
                }
                -- expected output --
                <span>a</span><span>b</span><span>c</span>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn recursive_component_with_children() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                component NodeView(
                  node: Node,
                  slot: Slot,
                ) {
                  <div>
                    {slot}
                    <match {node.next}>
                      <case {Some(next)}>
                        <NodeView node={next}>
                          <span>
                            {node.value}
                          </span>
                        </NodeView>
                      </case>
                      <case {None}>
                      </case>
                    </match>
                  </div>
                }

                view Test {
                  <let {
                    root: Node = Node {
                      value: "a",
                      next: Some(Node {value: "b", next: None}),
                    },
                  }>
                    <NodeView node={root}>
                      <p>
                        root
                      </p>
                    </NodeView>
                  </let>
                }
            "#},
            "<div><p>root</p><div><span>a</span></div></div>",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                component NodeView(node: test::Node) [children] {
                  write("<div")
                  write(">")
                  write_expr(slot)
                  let v_0 = node.next in {
                    match v_0 {
                      Some(v_1) => {
                        let next = v_1 in {
                          call NodeView(node = next) {
                            write("<span")
                            write(">")
                            write_escaped(node.value)
                            write("</span>")
                          }
                        }
                      }
                      None => {
                      }
                    }
                  }
                  write("</div>")
                }
                view Test() {
                  let root = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    call NodeView(node = root) {
                      write("<p")
                      write(">")
                      write("root")
                      write("</p>")
                    }
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                component NodeView(node: test::Node) [children] {
                  write("<div>")
                  write_expr(slot)
                  let v_0 = node.next in {
                    match v_0 {
                      Some(v_1) => {
                        let next = v_1 in {
                          call NodeView(node = next) {
                            write("<span>")
                            write_escaped(node.value)
                            write("</span>")
                          }
                        }
                      }
                      None => {
                      }
                    }
                  }
                  write("</div>")
                }
                view Test() {
                  let root = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    call NodeView(node = root) {
                      write("<p>root</p>")
                    }
                  }
                }
                -- expected output --
                <div><p>root</p><div><span>a</span></div></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_default_parameter() {
        check(
            indoc! {r#"
                component Card(title: String = "New card") {
                  <div>
                    {title}
                  </div>
                }

                view Test {
                  <Card/>
                }
            "#},
            r#"<div>New card</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(title: String = "New card") {
                  write("<div")
                  write(">")
                  write_escaped(title)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "New card")
                }
                -- ir (optimized) --
                view Test() {
                  write("<div>New card</div>")
                }
                -- expected output --
                <div>New card</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_default_parameter_overridden() {
        check(
            indoc! {r#"
                component Card(title: String = "New card") {
                  <div>
                    {title}
                  </div>
                }

                view Test {
                  <Card title="Custom title"/>
                }
            "#},
            r#"<div>Custom title</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(title: String = "New card") {
                  write("<div")
                  write(">")
                  write_escaped(title)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "Custom title")
                }
                -- ir (optimized) --
                view Test() {
                  write("<div>Custom title</div>")
                }
                -- expected output --
                <div>Custom title</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_mixed_default_and_required_parameters() {
        check(
            indoc! {r#"
                component Card(
                  title: String,
                  subtitle: String = "No subtitle",
                ) {
                  <div>
                    {title} - {subtitle}
                  </div>
                }

                view Test {
                  <Card title="Hello"/>
                }
            "#},
            r#"<div>Hello - No subtitle</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(
                  title: String,
                  subtitle: String = "No subtitle",
                ) {
                  write("<div")
                  write(">")
                  write_escaped(title)
                  write(" - ")
                  write_escaped(subtitle)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "Hello", subtitle = "No subtitle")
                }
                -- ir (optimized) --
                view Test() {
                  write("<div>Hello - No subtitle</div>")
                }
                -- expected output --
                <div>Hello - No subtitle</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_mixed_default_and_required_parameters_all_provided() {
        check(
            indoc! {r#"
                component Card(
                  title: String,
                  subtitle: String = "No subtitle",
                ) {
                  <div>
                    {title} - {subtitle}
                  </div>
                }

                view Test {
                  <Card title="Hello" subtitle="World"/>
                }
            "#},
            r#"<div>Hello - World</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(
                  title: String,
                  subtitle: String = "No subtitle",
                ) {
                  write("<div")
                  write(">")
                  write_escaped(title)
                  write(" - ")
                  write_escaped(subtitle)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "Hello", subtitle = "World")
                }
                -- ir (optimized) --
                view Test() {
                  write("<div>Hello - World</div>")
                }
                -- expected output --
                <div>Hello - World</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_multiple_default_parameters() {
        check(
            indoc! {r#"
                component Card(
                  title: String = "Default",
                  subtitle: String = "Sub",
                  footer: String = "End",
                ) {
                  <div>
                    {title} - {subtitle} - {footer}
                  </div>
                }

                view Test {
                  <Card subtitle="Custom"/>
                }
            "#},
            r#"<div>Default - Custom - End</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(
                  title: String = "Default",
                  subtitle: String = "Sub",
                  footer: String = "End",
                ) {
                  write("<div")
                  write(">")
                  write_escaped(title)
                  write(" - ")
                  write_escaped(subtitle)
                  write(" - ")
                  write_escaped(footer)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "Default", subtitle = "Custom", footer = "End")
                }
                -- ir (optimized) --
                view Test() {
                  write("<div>Default - Custom - End</div>")
                }
                -- expected output --
                <div>Default - Custom - End</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_optional_children() {
        check(
            indoc! {r#"
                component Card(
                  title: String,
                  slot: Slot = Slot::Empty,
                ) {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {slot}
                  </div>
                }

                view Test {
                  <Card title="Hello"/>
                }
            "#},
            r#"<div class="card"><h2>Hello</h2></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(title: String) [children] {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_escaped(title)
                  write("</h2>")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "Hello", slot = Slot::Empty)
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"card\"><h2>Hello</h2></div>")
                }
                -- expected output --
                <div class="card"><h2>Hello</h2></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn component_with_optional_children_called_with_and_without_argument() {
        check(
            indoc! {r#"
                component Card(
                  title: String,
                  slot: Slot = Slot::Empty,
                ) {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {slot}
                  </div>
                }

                view Test {
                  <Card title="With">
                    <p>
                      body
                    </p>
                  </Card>
                  <Card title="Without"/>
                }
            "#},
            r#"<div class="card"><h2>With</h2><p>body</p></div><div class="card"><h2>Without</h2></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card(title: String) [children] {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_escaped(title)
                  write("</h2>")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  call Card(title = "With") {
                    write("<p")
                    write(">")
                    write("body")
                    write("</p>")
                  }
                  call Card(title = "Without", slot = Slot::Empty)
                }
                -- ir (optimized) --
                view Test() {
                  write("<div class=\"card\"><h2>With</h2><p>body</p></div><div")
                  write(" class=\"card\"><h2>Without</h2></div>")
                }
                -- expected output --
                <div class="card"><h2>With</h2><p>body</p></div><div class="card"><h2>Without</h2></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn slot_empty_in_let_binding() {
        check(
            indoc! {r#"
                component Card(slot: Slot) {
                  <div class="card">
                    {slot}
                  </div>
                }

                view Test {
                  <let {content: Slot = Slot::Empty}>
                    <Card>
                      {content}
                    </Card>
                  </let>
                }
            "#},
            r#"<div class="card"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card() [children] {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  let content = Slot::Empty in {
                    call Card() {
                      write_expr(content)
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let content = Slot::Empty in {
                    write("<div class=\"card\">")
                    write_expr(content)
                    write("</div>")
                  }
                }
                -- expected output --
                <div class="card"></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn slot_empty_copied_between_let_bindings() {
        check(
            indoc! {r#"
                component Card(slot: Slot) {
                  <div class="card">
                    {slot}
                  </div>
                }

                view Test {
                  <let {a: Slot = Slot::Empty}>
                    <let {b: Slot = a}>
                      <Card>
                        {b}
                      </Card>
                    </let>
                  </let>
                }
            "#},
            r#"<div class="card"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                component Card() [children] {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write_expr(slot)
                  write("</div>")
                }
                view Test() {
                  let a = Slot::Empty in {
                    let b = a in {
                      call Card() {
                        write_expr(b)
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let a = Slot::Empty in {
                    let b = a in {
                      write("<div class=\"card\">")
                      write_expr(b)
                      write("</div>")
                    }
                  }
                }
                -- expected output --
                <div class="card"></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_is_empty_true() {
        check(
            indoc! {r#"
                view Test {
                  <let {name: String = ""}>
                    <match {name.is_empty()}>
                      <case {true}>
                        empty
                      </case>
                      <case {false}>
                        not empty
                      </case>
                    </match>
                  </let>
                }
            "#},
            "empty",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let name = "" in {
                    let v_0 = name.is_empty() in {
                      match v_0 {
                        true => {
                          write("empty")
                        }
                        false => {
                          write("not empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let v_0 = "".is_empty() in {
                    match v_0 {
                      true => {
                        write("empty")
                      }
                      false => {
                        write("not empty")
                      }
                    }
                  }
                }
                -- expected output --
                empty
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn string_is_empty_false() {
        check(
            indoc! {r#"
                view Test {
                  <let {name: String = "hello"}>
                    <match {name.is_empty()}>
                      <case {true}>
                        empty
                      </case>
                      <case {false}>
                        not empty
                      </case>
                    </match>
                  </let>
                }
            "#},
            "not empty",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  let name = "hello" in {
                    let v_0 = name.is_empty() in {
                      match v_0 {
                        true => {
                          write("empty")
                        }
                        false => {
                          write("not empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                view Test() {
                  let v_0 = "hello".is_empty() in {
                    match v_0 {
                      true => {
                        write("empty")
                      }
                      false => {
                        write("not empty")
                      }
                    }
                  }
                }
                -- expected output --
                not empty
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_is_some_true() {
        check(
            indoc! {r#"
                view Test {
                  <let {value: Option[String] = Some("hello")}>
                    <match {value.is_some()}>
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
                view Test() {
                  let value = Option[String]::Some("hello") in {
                    let v_0 = value.is_some() in {
                      match v_0 {
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
                view Test() {
                  let v_0 = Option[String]::Some("hello").is_some() in {
                    match v_0 {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_is_some_false() {
        check(
            indoc! {r#"
                view Test {
                  <let {value: Option[String] = None}>
                    <match {value.is_some()}>
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
                view Test() {
                  let value = Option[String]::None in {
                    let v_0 = value.is_some() in {
                      match v_0 {
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
                view Test() {
                  let v_0 = Option[String]::None.is_some() in {
                    match v_0 {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_is_none_true() {
        check(
            indoc! {r#"
                view Test {
                  <let {value: Option[String] = None}>
                    <match {value.is_none()}>
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
                view Test() {
                  let value = Option[String]::None in {
                    let v_0 = value.is_none() in {
                      match v_0 {
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
                view Test() {
                  let v_0 = Option[String]::None.is_none() in {
                    match v_0 {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_is_none_false() {
        check(
            indoc! {r#"
                view Test {
                  <let {value: Option[String] = Some("hello")}>
                    <match {value.is_none()}>
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
                view Test() {
                  let value = Option[String]::Some("hello") in {
                    let v_0 = value.is_none() in {
                      match v_0 {
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
                view Test() {
                  let v_0 = Option[String]::Some("hello").is_none() in {
                    match v_0 {
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
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn top_level_text() {
        check(
            indoc! {r#"
                view Test {
                  hello world
                }
            "#},
            "hello world",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("hello world")
                }
                -- ir (optimized) --
                view Test() {
                  write("hello world")
                }
                -- expected output --
                hello world
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn top_level_multiline_text() {
        check(
            indoc! {r#"
                view Test {
                  hello
                  world
                }
            "#},
            "hello world",
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("hello")
                  write(" ")
                  write("world")
                }
                -- ir (optimized) --
                view Test() {
                  write("hello world")
                }
                -- expected output --
                hello world
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_bool_field_destructured_in_component() {
        check(
            indoc! {r#"
                enum Item {
                  Todo {
                    label: String,
                    done: Bool,
                  },
                }

                component RenderItem(item: Item) {
                  <match {item}>
                    <case {Item::Todo {label: l, done: d}}>
                      <if {d}>
                        [x]
                      </if>
                      <if {!d}>
                        [ ]
                      </if>
                      {l}
                    </case>
                  </match>
                }

                view Test {
                  <RenderItem item={
                    Item::Todo {label: "Buy milk", done: true}
                  }/>
                  ,
                  <RenderItem item={
                    Item::Todo {label: "Walk dog", done: false}
                  }/>
                }
            "#},
            "[x]Buy milk,[ ]Walk dog",
            expect![[r#"
                -- ir (unoptimized) --
                enum Item {
                  Todo {label: String, done: Bool},
                }
                component RenderItem(item: test::Item) {
                  match item {
                    Item::Todo(label: v_0, done: v_1) => {
                      let l = v_0 in {
                        let d = v_1 in {
                          if d {
                            write("[x]")
                          }
                          if (!d) {
                            write("[ ]")
                          }
                          write_escaped(l)
                        }
                      }
                    }
                  }
                }
                view Test() {
                  call RenderItem(item = Item::Todo {label: "Buy milk", done: true})
                  write(",")
                  call RenderItem(item = Item::Todo {label: "Walk dog", done: false})
                }
                -- ir (optimized) --
                enum Item {
                  Todo {label: String, done: Bool},
                }
                view Test() {
                  let item = Item::Todo {label: "Buy milk", done: true} in {
                    match item {
                      Item::Todo(label: v_0, done: v_1) => {
                        let l = v_0 in {
                          let d = v_1 in {
                            if d {
                              write("[x]")
                            }
                            if (!d) {
                              write("[ ]")
                            }
                            write_escaped(l)
                          }
                        }
                      }
                    }
                  }
                  write(",")
                  let item_1 = Item::Todo {label: "Walk dog", done: false} in {
                    match item_1 {
                      Item::Todo(label: v_0_2, done: v_1_3) => {
                        let l_4 = v_0_2 in {
                          let d_5 = v_1_3 in {
                            if d_5 {
                              write("[x]")
                            }
                            if (!d_5) {
                              write("[ ]")
                            }
                            write_escaped(l_4)
                          }
                        }
                      }
                    }
                  }
                }
                -- expected output --
                [x]Buy milk,[ ]Walk dog
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_int_field_compared_in_component() {
        check(
            indoc! {r#"
                enum TimeAgo {
                  MinutesAgo {
                    count: Int,
                  },
                  HoursAgo {
                    count: Int,
                  },
                }

                component Render(time: TimeAgo) {
                  <match {time}>
                    <case {TimeAgo::MinutesAgo {count: c}}>
                      {match c == 1 {
                        true => "1 minute ago",
                        false => c.to_string() + " minutes ago",
                      }}
                    </case>
                    <case {TimeAgo::HoursAgo {count: c}}>
                      {match c == 1 {
                        true => "1 hour ago",
                        false => c.to_string() + " hours ago",
                      }}
                    </case>
                  </match>
                }

                view Test {
                  <Render time={TimeAgo::MinutesAgo {count: 1}}/>
                  ,
                  <Render time={TimeAgo::MinutesAgo {count: 5}}/>
                  ,
                  <Render time={TimeAgo::HoursAgo {count: 1}}/>
                }
            "#},
            "1 minute ago,5 minutes ago,1 hour ago",
            expect![[r#"
                -- ir (unoptimized) --
                enum TimeAgo {
                  MinutesAgo {count: Int},
                  HoursAgo {count: Int},
                }
                component Render(time: test::TimeAgo) {
                  match time {
                    TimeAgo::MinutesAgo(count: v_0) => {
                      let c = v_0 in {
                        write_escaped(let v_2 = (c == 1) in match v_2 {
                          true => "1 minute ago",
                          false => (c.to_string() + " minutes ago"),
                        })
                      }
                    }
                    TimeAgo::HoursAgo(count: v_1) => {
                      let c_1 = v_1 in {
                        write_escaped(let v_3 = (c_1 == 1) in match v_3 {
                          true => "1 hour ago",
                          false => (c_1.to_string() + " hours ago"),
                        })
                      }
                    }
                  }
                }
                view Test() {
                  call Render(time = TimeAgo::MinutesAgo {count: 1})
                  write(",")
                  call Render(time = TimeAgo::MinutesAgo {count: 5})
                  write(",")
                  call Render(time = TimeAgo::HoursAgo {count: 1})
                }
                -- ir (optimized) --
                enum TimeAgo {
                  MinutesAgo {count: Int},
                  HoursAgo {count: Int},
                }
                view Test() {
                  let time = TimeAgo::MinutesAgo {count: 1} in {
                    match time {
                      TimeAgo::MinutesAgo(count: v_0) => {
                        let c = v_0 in {
                          write_escaped(let v_2 = (c == 1) in match v_2 {
                            true => "1 minute ago",
                            false => (c.to_string() + " minutes ago"),
                          })
                        }
                      }
                      TimeAgo::HoursAgo(count: v_1) => {
                        let c_1 = v_1 in {
                          write_escaped(let v_3 = (c_1 == 1) in match v_3 {
                            true => "1 hour ago",
                            false => (c_1.to_string() + " hours ago"),
                          })
                        }
                      }
                    }
                  }
                  write(",")
                  let time_2 = TimeAgo::MinutesAgo {count: 5} in {
                    match time_2 {
                      TimeAgo::MinutesAgo(count: v_0_3) => {
                        let c_4 = v_0_3 in {
                          write_escaped(let v_2_5 = (c_4 == 1) in match v_2_5 {
                            true => "1 minute ago",
                            false => (c_4.to_string() + " minutes ago"),
                          })
                        }
                      }
                      TimeAgo::HoursAgo(count: v_1_6) => {
                        let c_7 = v_1_6 in {
                          write_escaped(let v_3_8 = (c_7 == 1) in match v_3_8 {
                            true => "1 hour ago",
                            false => (c_7.to_string() + " hours ago"),
                          })
                        }
                      }
                    }
                  }
                  write(",")
                  let time_9 = TimeAgo::HoursAgo {count: 1} in {
                    match time_9 {
                      TimeAgo::MinutesAgo(count: v_0_10) => {
                        let c_11 = v_0_10 in {
                          write_escaped(let v_2_12 = (c_11 == 1) in match v_2_12 {
                            true => "1 minute ago",
                            false => (c_11.to_string() + " minutes ago"),
                          })
                        }
                      }
                      TimeAgo::HoursAgo(count: v_1_13) => {
                        let c_14 = v_1_13 in {
                          write_escaped(let v_3_15 = (c_14 == 1) in match v_3_15 {
                            true => "1 hour ago",
                            false => (c_14.to_string() + " hours ago"),
                          })
                        }
                      }
                    }
                  }
                }
                -- expected output --
                1 minute ago,5 minutes ago,1 hour ago
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_wildcard_field_in_component() {
        check(
            indoc! {r#"
                enum CodeBlock {
                  Snippet {
                    language: String,
                    code: String,
                  },
                }

                component RenderCode(block: CodeBlock) {
                  <match {block}>
                    <case {CodeBlock::Snippet {language: _, code: c}}>
                      <code>
                        {c}
                      </code>
                    </case>
                  </match>
                }

                view Test {
                  <RenderCode block={
                    CodeBlock::Snippet {language: "rust", code: "fn main()"}
                  }/>
                }
            "#},
            "<code>fn main()</code>",
            expect![[r#"
                -- ir (unoptimized) --
                enum CodeBlock {
                  Snippet {language: String, code: String},
                }
                component RenderCode(block: test::CodeBlock) {
                  match block {
                    CodeBlock::Snippet(code: v_1) => {
                      let c = v_1 in {
                        write("<code")
                        write(">")
                        write_escaped(c)
                        write("</code>")
                      }
                    }
                  }
                }
                view Test() {
                  call RenderCode(block = CodeBlock::Snippet {language: "rust", code: "fn main()"})
                }
                -- ir (optimized) --
                enum CodeBlock {
                  Snippet {language: String, code: String},
                }
                view Test() {
                  let block = CodeBlock::Snippet {language: "rust", code: "fn main()"} in {
                    match block {
                      CodeBlock::Snippet(code: v_1) => {
                        let c = v_1 in {
                          write("<code>")
                          write_escaped(c)
                          write("</code>")
                        }
                      }
                    }
                  }
                }
                -- expected output --
                <code>fn main()</code>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_field_named_type_in_component() {
        check(
            indoc! {r#"
                enum ButtonElement {
                  Link {
                    href: String,
                  },
                  Button {
                    disabled: Bool,
                    type: String,
                  },
                }

                component Render(el: ButtonElement) {
                  <match {el}>
                    <case {ButtonElement::Link {href: h}}>
                      <a href={h}>
                        link
                      </a>
                    </case>
                    <case {ButtonElement::Button {disabled: _, type: t}}>
                      <button type={t}>
                        btn
                      </button>
                    </case>
                  </match>
                }

                view Test {
                  <Render el={
                    ButtonElement::Button {disabled: false, type: "submit"}
                  }/>
                }
            "#},
            r#"<button type="submit">btn</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                enum ButtonElement {
                  Link {href: String},
                  Button {disabled: Bool, type: String},
                }
                component Render(el: test::ButtonElement) {
                  match el {
                    ButtonElement::Link(href: v_0) => {
                      let h = v_0 in {
                        write("<a")
                        write(" href=\"")
                        write_escaped(h)
                        write("\"")
                        write(">")
                        write("link")
                        write("</a>")
                      }
                    }
                    ButtonElement::Button(type: v_2) => {
                      let t = v_2 in {
                        write("<button")
                        write(" type=\"")
                        write_escaped(t)
                        write("\"")
                        write(">")
                        write("btn")
                        write("</button>")
                      }
                    }
                  }
                }
                view Test() {
                  call Render(el = ButtonElement::Button {disabled: false, type: "submit"})
                }
                -- ir (optimized) --
                enum ButtonElement {
                  Link {href: String},
                  Button {disabled: Bool, type: String},
                }
                view Test() {
                  let el = ButtonElement::Button {disabled: false, type: "submit"} in {
                    match el {
                      ButtonElement::Link(href: v_0) => {
                        let h = v_0 in {
                          write("<a href=\"")
                          write_escaped(h)
                          write("\">link</a>")
                        }
                      }
                      ButtonElement::Button(type: v_2) => {
                        let t = v_2 in {
                          write("<button type=\"")
                          write_escaped(t)
                          write("\">btn</button>")
                        }
                      }
                    }
                  }
                }
                -- expected output --
                <button type="submit">btn</button>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_record_used_in_inline_match_and_match_tag() {
        check(
            indoc! {r#"
                record Target {
                  id: String,
                  title: String,
                }

                view Test {
                  <let {
                    target: Option[Target] = Some(
                      Target {id: "1", title: "hello"}
                    ),
                  }>
                    <let {
                      items: Array[Option[String]] = [
                        match target {
                          Some(t) => Some(t.title),
                          None => None,
                        },
                      ],
                    }>
                      <for {item in items}>
                        <match {item}>
                          <case {Some(s)}>
                            [{s}]
                          </case>
                          <case {None}>
                          </case>
                        </match>
                      </for>
                      <match {target}>
                        <case {Some(t)}>
                          {t.title}
                        </case>
                        <case {None}>
                        </case>
                      </match>
                    </let>
                  </let>
                }
            "#},
            "[hello]hello",
            expect![[r#"
                -- ir (unoptimized) --
                record Target {
                  id: String,
                  title: String,
                }
                view Test() {
                  let target = Option[test::Target]::Some(Target {
                    id: "1",
                    title: "hello",
                  }) in {
                    let items = [
                      match target {
                        Some(v_0) => let t = v_0 in Option[String]::Some(t.title),
                        None => Option[String]::None,
                      },
                    ] in {
                      for item in items {
                        match item {
                          Some(v_1) => {
                            let s = v_1 in {
                              write("[")
                              write_escaped(s)
                              write("]")
                            }
                          }
                          None => {
                          }
                        }
                      }
                      match target {
                        Some(v_2) => {
                          let t_1 = v_2 in {
                            write_escaped(t_1.title)
                          }
                        }
                        None => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Target {
                  id: String,
                  title: String,
                }
                view Test() {
                  let target = Option[test::Target]::Some(Target {
                    id: "1",
                    title: "hello",
                  }) in {
                    let items = [
                      match target {
                        Some(v_0) => let t = v_0 in Option[String]::Some(t.title),
                        None => Option[String]::None,
                      },
                    ] in {
                      for item in items {
                        match item {
                          Some(v_1) => {
                            let s = v_1 in {
                              write("[")
                              write_escaped(s)
                              write("]")
                            }
                          }
                          None => {
                          }
                        }
                      }
                      match target {
                        Some(v_2) => {
                          let t_1 = v_2 in {
                            write_escaped(t_1.title)
                          }
                        }
                        None => {
                        }
                      }
                    }
                  }
                }
                -- expected output --
                [hello]hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn asset_macro_in_dev_rewrites_to_hop_assets() {
        check_with_asset_rewriter(
            indoc! {r#"
                view Test {
                  <img src={asset!("/logo.svg")}>
                }
            "#},
            Some(Arc::new(PrefixingAssetRewriter::new(
                "/hop_assets".to_string(),
            ))),
            r#"<img src="/hop_assets/logo.svg">"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("<img")
                  write(" src=\"")
                  write_escaped("/hop_assets/logo.svg")
                  write("\"")
                  write(">")
                }
                -- ir (optimized) --
                view Test() {
                  write("<img src=\"/hop_assets/logo.svg\">")
                }
                -- expected output --
                <img src="/hop_assets/logo.svg">
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn asset_macro_in_prod_with_prefix() {
        check_with_asset_rewriter(
            indoc! {r#"
                view Test {
                  <img src={asset!("/logo.svg")}>
                }
            "#},
            Some(Arc::new(ReplacingAssetRewriter::new(HashMap::from([(
                DocumentId::new("logo.svg").unwrap(),
                "/static/v1/logo-a1b2c3d4.svg".to_string(),
            )])))),
            r#"<img src="/static/v1/logo-a1b2c3d4.svg">"#,
            expect![[r#"
                -- ir (unoptimized) --
                view Test() {
                  write("<img")
                  write(" src=\"")
                  write_escaped("/static/v1/logo-a1b2c3d4.svg")
                  write("\"")
                  write(">")
                }
                -- ir (optimized) --
                view Test() {
                  write("<img src=\"/static/v1/logo-a1b2c3d4.svg\">")
                }
                -- expected output --
                <img src="/static/v1/logo-a1b2c3d4.svg">
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }
}
