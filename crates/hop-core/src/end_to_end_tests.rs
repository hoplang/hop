use crate::asset_rewriter::AssetRewriter;
use crate::document::Document;
use crate::document_id::DocumentId;
use crate::ir::lower_pure;
use crate::ir::pure_module::PureModule;
use crate::ir::runtime::evaluator;
use crate::ir::transpile::{RustTranspiler, Transpiler, TsTranspiler};
use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
use crate::program::Program;
use crate::symbols::type_name::TypeName;
use expect_test::Expect;
use indoc::formatdoc;
use indoc::indoc;
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

    let runner_code = indoc! {r#"
      import { Test } from './module.ts';
      console.log(Test());
    "#};

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

    let main_code = formatdoc! {r#"
        {code}
        
        fn main() {{
            print!("{{}}", Test {{}}.render());
        }}
    "#};

    let main_rs = temp_dir.path().join("main.rs");
    fs::write(&main_rs, main_code).map_err(|e| format!("Failed to write main.rs: {}", e))?;

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

fn execute_evaluator(module: &PureModule) -> Result<String, String> {
    let page_name = TypeName::new("Test").unwrap();
    evaluator::evaluate_page(module, &page_name, HashMap::new())
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
        for (module, errors) in parse_errors {
            for error in errors {
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
        for (module, errors) in type_errors {
            for error in errors {
                eprintln!("Type Error in {:?}: {:?}", module, error);
            }
        }
        panic!("Type errors found");
    }

    let typed_asts = program.get_typed_modules().clone();
    let registry = program.type_registry();

    // Compile to IR without optimization
    let unoptimized_options = OrchestrateOptions {
        skip_html_structure: true,
        skip_optimization: true,
        asset_rewriter: asset_rewriter.clone(),
        ..Default::default()
    };
    let unoptimized_pure = orchestrate_pure(&typed_asts, unoptimized_options);

    // Compile to IR with optimization
    let optimized_options = OrchestrateOptions {
        skip_html_structure: true,
        skip_optimization: false,
        asset_rewriter,
        ..Default::default()
    };
    let optimized_pure = orchestrate_pure(&typed_asts, optimized_options);

    // Evaluate the Pure modules before lowering consumes them.
    let unoptimized_eval = execute_evaluator(&unoptimized_pure);
    let optimized_eval = execute_evaluator(&optimized_pure);

    let unoptimized_module = lower_pure(unoptimized_pure);
    let optimized_module = lower_pure(optimized_pure);

    let unoptimized_ir = unoptimized_module.to_string();
    let optimized_ir = optimized_module.to_string();

    let mut output = format!(
        "-- ir (unoptimized) --\n{}-- ir (optimized) --\n{}-- expected output --\n{}\n",
        unoptimized_ir, optimized_ir, expected_output
    );

    // Test evaluator on the unoptimized Pure module
    let eval_output = match unoptimized_eval {
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

    // Test evaluator on the optimized Pure module
    let eval_output = match optimized_eval {
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
    let ts_code = ts_transpiler.transpile_module(&unoptimized_module, registry);
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
    let rust_code = rust_transpiler.transpile_module(&unoptimized_module, registry);
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
    let ts_code = ts_transpiler.transpile_module(&optimized_module, registry);
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

    let rust_code = rust_transpiler.transpile_module(&optimized_module, registry);
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
    use crate::ir::pure_module_generator::random_module_with_test_view;
    use expect_test::expect;
    use indoc::indoc;

    #[test]
    #[ignore]
    fn fuzz_transpile_ts_renders_identically() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module_with_test_view(u);
            let pure = module.to_string();
            let page_name = TypeName::new("Test").unwrap();
            let expected = evaluator::evaluate_page(&module, &page_name, HashMap::new())
                .unwrap_or_else(|e| panic!("Evaluator failed:\n{e}\n\nPure:\n{pure}"))
                .trim()
                .to_string();
            let module = lower_pure(module);
            let ir = module.to_string();
            let ts_code = TsTranspiler::new().transpile_module(&module, &registry);
            if let Err(e) = typecheck_typescript(&ts_code) {
                panic!(
                    "TypeScript typecheck failed:\n{e}\n\nPure:\n{pure}\nIR:\n{ir}\nCode:\n{ts_code}"
                );
            }
            let ts_output = execute_typescript(&ts_code).unwrap_or_else(|e| {
                panic!("TypeScript failed:\n{e}\n\nPure:\n{pure}\nIR:\n{ir}\nCode:\n{ts_code}")
            });
            assert_eq!(
                expected, ts_output,
                "evaluator and TypeScript disagree\n\nPure:\n{pure}\nIR:\n{ir}\nCode:\n{ts_code}"
            );
            Ok(())
        });
    }

    #[test]
    #[ignore]
    fn fuzz_transpile_rust_renders_identically() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module_with_test_view(u);
            let pure = module.to_string();
            let page_name = TypeName::new("Test").unwrap();
            let expected = evaluator::evaluate_page(&module, &page_name, HashMap::new())
                .unwrap_or_else(|e| panic!("Evaluator failed:\n{e}\n\nPure:\n{pure}"))
                .trim()
                .to_string();
            let module = lower_pure(module);
            let ir = module.to_string();
            let rust_code = RustTranspiler::new().transpile_module(&module, &registry);
            let rust_output = execute_rust(&rust_code).unwrap_or_else(|e| {
                panic!("Rust failed:\n{e}\n\nPure:\n{pure}\nIR:\n{ir}\nCode:\n{rust_code}")
            });
            assert_eq!(
                expected, rust_output,
                "evaluator and Rust disagree\n\nPure:\n{pure}\nIR:\n{ir}\nCode:\n{rust_code}"
            );
            Ok(())
        });
    }

    #[test]
    #[ignore]
    fn bool_binding_from_record_pattern_used_in_logical_operator() {
        check(
            indoc! {r#"
                record Flag {
                  value: Bool,
                }

                view Test {
                  <for {f in [Flag {value: true}]}>
                    <match {f}>
                      <case {Flag {value: b}}>
                        <if {b || false}>
                          yes
                        </if>
                      </case>
                    </match>
                  </for>
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        match (v2 || false) {
                          true => {
                            write("yes")
                          }
                          false => {
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        match (v2 || false) {
                          true => {
                            write("yes")
                          }
                          false => {
                          }
                        }
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
    fn int_binding_from_record_pattern_compared_with_literal() {
        check(
            indoc! {r#"
                record Count {
                  n: Int,
                }

                view Test {
                  <for {c in [Count {n: 57}]}>
                    <match {c}>
                      <case {Count {n: v}}>
                        <if {v == 57}>
                          eq
                        </if>
                      </case>
                    </match>
                  </for>
                }
            "#},
            "eq",
            expect![[r#"
                -- ir (unoptimized) --
                record Count {
                  n: Int,
                }
                page Test() {
                  for v0 in [Count {n: 57}] {
                    let v1 = v0.n in {
                      let v2 = v1 in {
                        match (v2 == 57) {
                          true => {
                            write("eq")
                          }
                          false => {
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Count {
                  n: Int,
                }
                page Test() {
                  for v0 in [Count {n: 57}] {
                    let v1 = v0.n in {
                      let v2 = v1 in {
                        match (v2 == 57) {
                          true => {
                            write("eq")
                          }
                          false => {
                          }
                        }
                      }
                    }
                  }
                }
                -- expected output --
                eq
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
    fn bool_binding_from_record_pattern_as_match_expr_subject() {
        check(
            indoc! {r#"
                record Flag {
                  value: Bool,
                }

                view Test {
                  <for {f in [Flag {value: true}]}>
                    <match {f}>
                      <case {Flag {value: b}}>
                        {match b {true => "yes", false => "no"}}
                      </case>
                    </match>
                  </for>
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        write_string(match v2 {
                          true => { "yes" }
                          false => { "no" }
                        })
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        write_string(match v2 {
                          true => { "yes" }
                          false => { "no" }
                        })
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
    fn bool_binding_from_record_pattern_as_match_statement_subject() {
        check(
            indoc! {r#"
                record Flag {
                  value: Bool,
                }

                view Test {
                  <for {f in [Flag {value: true}]}>
                    <match {f}>
                      <case {Flag {value: b}}>
                        <match {b}>
                          <case {true}>
                            yes
                          </case>
                          <case {false}>
                            no
                          </case>
                        </match>
                      </case>
                    </match>
                  </for>
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        match v2 {
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
                }
                -- ir (optimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        match v2 {
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
    fn bool_binding_from_record_pattern_as_if_condition() {
        check(
            indoc! {r#"
                record Flag {
                  value: Bool,
                }

                view Test {
                  <for {f in [Flag {value: true}]}>
                    <match {f}>
                      <case {Flag {value: b}}>
                        <if {b}>
                          yes
                        </if>
                      </case>
                    </match>
                  </for>
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        match v2 {
                          true => {
                            write("yes")
                          }
                          false => {
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Flag {
                  value: Bool,
                }
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        match v2 {
                          true => {
                            write("yes")
                          }
                          false => {
                          }
                        }
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
    fn rest_spread_forwards_attribute_to_html() {
        check(
            indoc! {r#"
                component Button(
                  label: String,
                  ...rest,
                ) {
                  <button class="btn" ...rest>
                    {label}
                  </button>
                }

                view Test {
                  <Button label="Hi" id="submit"/>
                }
            "#},
            r#"<button class="btn" id="submit">Hi</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button(label@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<button")
                  write(" class=\"btn\"")
                  write_fragment(v1)
                  write(">")
                  write_string(v0)
                  write("</button>")
                }
                page Test() {
                  call Button(label = "Hi", rest = {
                    write(" id=\"submit\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<button class=\"btn\" id=\"submit\">Hi</button>")
                }
                -- expected output --
                <button class="btn" id="submit">Hi</button>
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
    fn rest_chains_past_a_call_cycle() {
        // First and Second call each other, so they share a call cycle, but
        // the rests run straight down to Leaf's div. Both pick up `title`.
        check(
            indoc! {r#"
                component Leaf(title: String = "d") {
                  <div>
                    {title}
                  </div>
                }

                component First(
                  n: Int,
                  ...rest,
                ) {
                  <Second n={n} ...rest/>
                }

                component Second(
                  n: Int,
                  ...rest,
                ) {
                  <Leaf ...rest/>
                  <if {0 < n}>
                    <First n={n - 1}/>
                  </if>
                }

                view Test {
                  <First n={1} title="x"/>
                }
            "#},
            r#"<div>x</div><div>d</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn First(
                  n@v0: Int,
                  title@v1: String,
                  rest@v2: Fragment,
                ) -> Fragment {
                  call Second(n = v0, title = v1, rest = {
                    write_fragment(v2)
                  })
                }
                fn Leaf(title@v3: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v3)
                  write("</div>")
                }
                fn Second(
                  n@v4: Int,
                  title@v5: String,
                  rest@v6: Fragment,
                ) -> Fragment {
                  call Leaf(title = v5)
                  match (0 < v4) {
                    true => {
                      call First(n = (v4 - 1), title = "d", rest = {})
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call First(n = 1, title = "x", rest = {})
                }
                -- ir (optimized) --
                fn First(
                  n@v0: Int,
                  title@v1: String,
                  rest@v2: Fragment,
                ) -> Fragment {
                  call Second(n = v0, title = v1, rest = {
                    write_fragment(v2)
                  })
                }
                fn Second(
                  n@v4: Int,
                  title@v5: String,
                  rest@v6: Fragment,
                ) -> Fragment {
                  write("<div>")
                  write_string(v5)
                  write("</div>")
                  match (0 < v4) {
                    true => {
                      call First(n = (v4 - 1), title = "d", rest = {})
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call First(n = 1, title = "x", rest = {})
                }
                -- expected output --
                <div>x</div><div>d</div>
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
    fn rest_chains_through_a_component_to_an_element() {
        check(
            indoc! {r#"
                component Base(...rest) {
                  <div ...rest>
                  </div>
                }

                component Card(
                  title: String,
                  ...rest,
                ) {
                  <section>
                    <h1>
                      {title}
                    </h1>
                    <Base ...rest/>
                  </section>
                }

                view Test {
                  <Card title="Hi" id="x" data-k="v"/>
                }
            "#},
            r#"<section><h1>Hi</h1><div id="x" data-k="v"></div></section>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Base(rest@v0: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v0)
                  write(">")
                  write("</div>")
                }
                fn Card(title@v1: String, rest@v2: Fragment) -> Fragment {
                  write("<section")
                  write(">")
                  write("<h1")
                  write(">")
                  write_string(v1)
                  write("</h1>")
                  call Base(rest = {
                    write_fragment(v2)
                  })
                  write("</section>")
                }
                page Test() {
                  call Card(title = "Hi", rest = {
                    write(" id=\"x\"")
                    write(" data-k=\"v\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<section><h1>Hi</h1><div id=\"x\" data-k=\"v\"></div></section>")
                }
                -- expected output --
                <section><h1>Hi</h1><div id="x" data-k="v"></div></section>
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
    fn rest_reaches_a_spread_target_nested_in_control_flow() {
        check(
            indoc! {r#"
                component Wrapper(
                  show: Bool,
                  ...rest,
                ) {
                  <if {show}>
                    <div ...rest>
                    </div>
                  </if>
                }

                view Test {
                  <Wrapper show={true} id="x"/>
                }
            "#},
            r#"<div id="x"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper(show@v0: Bool, rest@v1: Fragment) -> Fragment {
                  match v0 {
                    true => {
                      write("<div")
                      write_fragment(v1)
                      write(">")
                      write("</div>")
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Wrapper(show = true, rest = {
                    write(" id=\"x\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div id=\"x\"></div>")
                }
                -- expected output --
                <div id="x"></div>
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
    fn valueless_attribute_travels_through_rest() {
        check(
            indoc! {r#"
                component Button(...rest) {
                  <button ...rest>
                  </button>
                }

                view Test {
                  <Button disabled/>
                }
            "#},
            r#"<button disabled></button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button(rest@v0: Fragment) -> Fragment {
                  write("<button")
                  write_fragment(v0)
                  write(">")
                  write("</button>")
                }
                page Test() {
                  call Button(rest = {
                    write(" disabled")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<button disabled></button>")
                }
                -- expected output --
                <button disabled></button>
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
    fn rest_reaches_a_spread_target_nested_in_match() {
        check(
            indoc! {r#"
                component Wrapper(
                  show: Bool,
                  ...rest,
                ) {
                  <match {show}>
                    <case {true}>
                      <div ...rest>
                      </div>
                    </case>
                    <case {false}>
                    </case>
                  </match>
                }

                view Test {
                  <Wrapper show={true} id="x"/>
                }
            "#},
            r#"<div id="x"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper(show@v0: Bool, rest@v1: Fragment) -> Fragment {
                  match v0 {
                    true => {
                      write("<div")
                      write_fragment(v1)
                      write(">")
                      write("</div>")
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Wrapper(show = true, rest = {
                    write(" id=\"x\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div id=\"x\"></div>")
                }
                -- expected output --
                <div id="x"></div>
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
    fn rest_escapes_attribute_values() {
        check(
            indoc! {r#"
                component Panel(...rest) {
                  <div ...rest>
                  </div>
                }

                view Test {
                  <Panel title={"a'b<c&d"}/>
                }
            "#},
            r#"<div title="a&#39;b&lt;c&amp;d"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Panel(rest@v0: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v0)
                  write(">")
                  write("</div>")
                }
                page Test() {
                  call Panel(rest = {
                    write(" title=\"")
                    write_string("a'b<c&d")
                    write("\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div title=\"a&#39;b&lt;c&amp;d\"></div>")
                }
                -- expected output --
                <div title="a&#39;b&lt;c&amp;d"></div>
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
    fn rest_reaches_a_void_element() {
        check(
            indoc! {r#"
                component Icon(...rest) {
                  <img ...rest>
                }

                view Test {
                  <Icon src="a.png" alt="a"/>
                }
            "#},
            r#"<img src="a.png" alt="a">"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Icon(rest@v0: Fragment) -> Fragment {
                  write("<img")
                  write_fragment(v0)
                  write(">")
                }
                page Test() {
                  call Icon(rest = {
                    write(" src=\"a.png\"")
                    write(" alt=\"a\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<img src=\"a.png\" alt=\"a\">")
                }
                -- expected output --
                <img src="a.png" alt="a">
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
    fn empty_rest_contributes_no_attributes() {
        check(
            indoc! {r#"
                component A(...rest) {
                  <div ...rest>
                  </div>
                }

                view Test {
                  <A/>
                }
            "#},
            r#"<div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A(rest@v0: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v0)
                  write(">")
                  write("</div>")
                }
                page Test() {
                  call A(rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div></div>")
                }
                -- expected output --
                <div></div>
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
    fn accepts_extra_attrs_when_rest_reaches_html() {
        check(
            indoc! {r#"
                component Button(
                  class: String,
                  children: Fragment,
                  ...rest,
                ) {
                  <button class={class} ...rest>
                    {children}
                  </button>
                }

                view Test {
                  <Button class="p-2" data-foo="bar">
                    Hi
                  </Button>
                }
            "#},
            r#"<button class="p-2" data-foo="bar">Hi</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button(
                  class@v0: String,
                  children@v1: Fragment,
                  rest@v2: Fragment,
                ) -> Fragment {
                  write("<button")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_fragment(v2)
                  write(">")
                  write_fragment(v1)
                  write("</button>")
                }
                page Test() {
                  call Button(class = "p-2", children = {
                    write("Hi")
                  }, rest = {
                    write(" data-foo=\"bar\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<button class=\"p-2\" data-foo=\"bar\">Hi</button>")
                }
                -- expected output --
                <button class="p-2" data-foo="bar">Hi</button>
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
    fn accepts_forwarded_attr_not_set_on_element() {
        check(
            indoc! {r#"
                component Button(
                  children: Fragment,
                  ...rest,
                ) {
                  <button class="builtin" ...rest>
                    {children}
                  </button>
                }

                view Test {
                  <Button data-x="y">
                    Hi
                  </Button>
                }
            "#},
            r#"<button class="builtin" data-x="y">Hi</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button(
                  children@v0: Fragment,
                  rest@v1: Fragment,
                ) -> Fragment {
                  write("<button")
                  write(" class=\"builtin\"")
                  write_fragment(v1)
                  write(">")
                  write_fragment(v0)
                  write("</button>")
                }
                page Test() {
                  call Button(children = {
                    write("Hi")
                  }, rest = {
                    write(" data-x=\"y\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<button class=\"builtin\" data-x=\"y\">Hi</button>")
                }
                -- expected output --
                <button class="builtin" data-x="y">Hi</button>
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
    fn accepts_svg_attributes_on_forwarded_svg() {
        check(
            indoc! {r#"
                component Svg(...rest) {
                  <svg ...rest>
                  </svg>
                }

                view Test {
                  <Svg viewBox="0 0 100 100"/>
                }
            "#},
            r#"<svg viewBox="0 0 100 100"></svg>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Svg(rest@v0: Fragment) -> Fragment {
                  write("<svg")
                  write_fragment(v0)
                  write(">")
                  write("</svg>")
                }
                page Test() {
                  call Svg(rest = {
                    write(" viewBox=\"0 0 100 100\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<svg viewBox=\"0 0 100 100\"></svg>")
                }
                -- expected output --
                <svg viewBox="0 0 100 100"></svg>
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
    fn accepts_required_arg_forwarded_through_rest() {
        check(
            indoc! {r#"
                component Card(title: String) {
                  <div>
                    {title}
                  </div>
                }

                component Wrapper(...rest) {
                  <Card ...rest/>
                }

                view Test {
                  <Wrapper title="hi"/>
                }
            "#},
            r#"<div>hi</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card(title@v0: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                fn Wrapper(
                  title@v1: String,
                  rest@v2: Fragment,
                ) -> Fragment {
                  call Card(title = v1)
                }
                page Test() {
                  call Wrapper(title = "hi", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>hi</div>")
                }
                -- expected output --
                <div>hi</div>
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
    fn accepts_explicit_arg_supplied_alongside_rest_spread() {
        check(
            indoc! {r#"
                component Card(title: String) {
                  <div>
                    {title}
                  </div>
                }

                component Wrapper(...rest) {
                  <Card title="explicit" ...rest/>
                }

                view Test {
                  <Wrapper/>
                }
            "#},
            r#"<div>explicit</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card(title@v0: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                fn Wrapper(rest@v1: Fragment) -> Fragment {
                  call Card(title = "explicit")
                }
                page Test() {
                  call Wrapper(rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>explicit</div>")
                }
                -- expected output --
                <div>explicit</div>
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
    fn accepts_record_param_forwarded_through_rest() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                }

                component Card(user: User) {
                  <div>
                    {user.name}
                  </div>
                }

                component Wrapper(...rest) {
                  <Card ...rest/>
                }

                view Test {
                  <let {user: User = User {name: "Ada"}}>
                    <Wrapper user={user}/>
                  </let>
                }
            "#},
            r#"<div>Ada</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                record User {
                  name: String,
                }
                fn Card(user@v1: test::User) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v1.name)
                  write("</div>")
                }
                fn Wrapper(
                  user@v2: test::User,
                  rest@v3: Fragment,
                ) -> Fragment {
                  call Card(user = v2)
                }
                page Test() {
                  let v0 = User {name: "Ada"} in {
                    call Wrapper(user = v0, rest = {})
                  }
                }
                -- ir (optimized) --
                record User {
                  name: String,
                }
                page Test() {
                  write("<div>Ada</div>")
                }
                -- expected output --
                <div>Ada</div>
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
    fn accepts_user_variable_with_underscore_without_collisions() {
        check(
            indoc! {r#"
                record Flag {
                  value: String,
                }

                view Test {
                  <let {v_1: String = "outer"}>
                    <for {f in [Flag {value: "x"}]}>
                      <match {f}>
                        <case {Flag {value: b}}>
                          {v_1}
                          {b}
                        </case>
                      </match>
                    </for>
                  </let>
                }
            "#},
            r#"outer x"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Flag {
                  value: String,
                }
                page Test() {
                  let v0 = "outer" in {
                    for v1 in [Flag {value: "x"}] {
                      let v2 = v1.value in {
                        let v3 = v2 in {
                          write_string(v0)
                          write(" ")
                          write_string(v3)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Flag {
                  value: String,
                }
                page Test() {
                  for v1 in [Flag {value: "x"}] {
                    let v2 = v1.value in {
                      let v3 = v2 in {
                        write("outer ")
                        write_string(v3)
                      }
                    }
                  }
                }
                -- expected output --
                outer x
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
    fn accepts_required_args_forwarded_transitively() {
        check(
            indoc! {r#"
                component Card(title: String) {
                  <div>
                    {title}
                  </div>
                }

                component Bar(
                  name: String,
                  ...rest,
                ) {
                  <div>
                    {name}
                    <Card ...rest/>
                  </div>
                }

                component Baz(...rest) {
                  <Bar ...rest/>
                }

                view Test {
                  <Baz name="n" title="t"/>
                }
            "#},
            r#"<div>n<div>t</div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Bar(
                  name@v0: String,
                  title@v1: String,
                  rest@v2: Fragment,
                ) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  call Card(title = v1)
                  write("</div>")
                }
                fn Baz(
                  name@v3: String,
                  title@v4: String,
                  rest@v5: Fragment,
                ) -> Fragment {
                  call Bar(name = v3, title = v4, rest = {
                    write_fragment(v5)
                  })
                }
                fn Card(title@v6: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v6)
                  write("</div>")
                }
                page Test() {
                  call Baz(name = "n", title = "t", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>n<div>t</div></div>")
                }
                -- expected output --
                <div>n<div>t</div></div>
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
    fn accepts_int_param_forwarded_through_rest() {
        check(
            indoc! {r#"
                component Card(count: Int) {
                  <if {count > 0}>
                    <div>
                      positive
                    </div>
                  </if>
                }

                component Wrapper(...rest) {
                  <Card ...rest/>
                }

                view Test {
                  <Wrapper count={3}/>
                }
            "#},
            r#"<div>positive</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card(count@v0: Int) -> Fragment {
                  match (0 < v0) {
                    true => {
                      write("<div")
                      write(">")
                      write("positive")
                      write("</div>")
                    }
                    false => {
                    }
                  }
                }
                fn Wrapper(count@v1: Int, rest@v2: Fragment) -> Fragment {
                  call Card(count = v1)
                }
                page Test() {
                  call Wrapper(count = 3, rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>positive</div>")
                }
                -- expected output --
                <div>positive</div>
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
    fn accepts_typed_field_and_open_html_tail_in_one_forward() {
        check(
            indoc! {r#"
                component A(
                  count: Int,
                  ...rest,
                ) {
                  <div ...rest>
                    <if {count > 0}>
                      positive
                    </if>
                  </div>
                }

                component B(...rest) {
                  <A ...rest/>
                }

                view Test {
                  <B count={3} data-foo="bar"/>
                }
            "#},
            r#"<div data-foo="bar">positive</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A(count@v0: Int, rest@v1: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v1)
                  write(">")
                  match (0 < v0) {
                    true => {
                      write("positive")
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                fn B(count@v2: Int, rest@v3: Fragment) -> Fragment {
                  call A(count = v2, rest = {
                    write_fragment(v3)
                  })
                }
                page Test() {
                  call B(count = 3, rest = {
                    write(" data-foo=\"bar\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div data-foo=\"bar\">positive</div>")
                }
                -- expected output --
                <div data-foo="bar">positive</div>
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
    fn accepts_children_forwarded_transitively_through_rest() {
        check(
            indoc! {r#"
                component Foo(children: Fragment) {
                  <div>
                    {children}
                  </div>
                }

                component Bar(...rest) {
                  <Foo ...rest/>
                }

                component Baz(...rest) {
                  <Bar ...rest/>
                }

                view Test {
                  <Baz>
                    deep
                  </Baz>
                }
            "#},
            r#"<div>deep</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Bar(
                  children@v0: Fragment,
                  rest@v1: Fragment,
                ) -> Fragment {
                  call Foo(children = v0)
                }
                fn Baz(
                  children@v2: Fragment,
                  rest@v3: Fragment,
                ) -> Fragment {
                  call Bar(children = v2, rest = {
                    write_fragment(v3)
                  })
                }
                fn Foo(children@v4: Fragment) -> Fragment {
                  write("<div")
                  write(">")
                  write_fragment(v4)
                  write("</div>")
                }
                page Test() {
                  call Baz(children = {
                    write("deep")
                  }, rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>deep</div>")
                }
                -- expected output --
                <div>deep</div>
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
    fn accepts_param_reserved_out_of_rest_when_callee_has_default() {
        check(
            indoc! {r#"
                component Inner(
                  class: String = "x",
                  ...rest,
                ) {
                  <span class={class} ...rest>
                  </span>
                }

                component Outer(
                  class: String,
                  ...rest,
                ) {
                  <div class={class}>
                    <Inner ...rest/>
                  </div>
                }

                view Test {
                  <Outer class="x"/>
                }
            "#},
            r#"<div class="x"><span class="x"></span></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner(class@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<span")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_fragment(v1)
                  write(">")
                  write("</span>")
                }
                fn Outer(class@v2: String, rest@v3: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"")
                  write_string(v2)
                  write("\"")
                  write(">")
                  call Inner(class = "x", rest = {
                    write_fragment(v3)
                  })
                  write("</div>")
                }
                page Test() {
                  call Outer(class = "x", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div class=\"x\"><span class=\"x\"></span></div>")
                }
                -- expected output --
                <div class="x"><span class="x"></span></div>
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
    fn accepts_intercept_and_merge_wrapper() {
        check(
            indoc! {r#"
                component Foo(
                  children: Fragment,
                  class: String,
                  ...rest,
                ) {
                  <div class={class} ...rest>
                    {children}
                  </div>
                }

                component Button(
                  children: Fragment,
                  class: String = "",
                  ...rest,
                ) {
                  <Foo class={class} ...rest>
                    {children}
                  </Foo>
                }

                view Test {
                  <Button class="primary">
                    click
                  </Button>
                }
            "#},
            r#"<div class="primary">click</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button(
                  children@v0: Fragment,
                  class@v1: String,
                  rest@v2: Fragment,
                ) -> Fragment {
                  call Foo(children = {
                    write_fragment(v0)
                  }, class = v1, rest = {
                    write_fragment(v2)
                  })
                }
                fn Foo(
                  children@v3: Fragment,
                  class@v4: String,
                  rest@v5: Fragment,
                ) -> Fragment {
                  write("<div")
                  write(" class=\"")
                  write_string(v4)
                  write("\"")
                  write_fragment(v5)
                  write(">")
                  write_fragment(v3)
                  write("</div>")
                }
                page Test() {
                  call Button(children = {
                    write("click")
                  }, class = "primary", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div class=\"primary\">click</div>")
                }
                -- expected output --
                <div class="primary">click</div>
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
    fn accepts_optional_param_forwarded_and_overridden_through_rest() {
        check(
            indoc! {r#"
                component Inner(
                  class: String = "x",
                  ...rest,
                ) {
                  <span class={class} ...rest>
                  </span>
                }

                component Wrapper(...rest) {
                  <Inner ...rest/>
                }

                view Test {
                  <Wrapper class="y"/>
                }
            "#},
            r#"<span class="y"></span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner(class@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<span")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_fragment(v1)
                  write(">")
                  write("</span>")
                }
                fn Wrapper(
                  class@v2: String,
                  rest@v3: Fragment,
                ) -> Fragment {
                  call Inner(class = v2, rest = {
                    write_fragment(v3)
                  })
                }
                page Test() {
                  call Wrapper(class = "y", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<span class=\"y\"></span>")
                }
                -- expected output --
                <span class="y"></span>
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
    fn accepts_optional_default_chain_with_caller_value() {
        check(
            indoc! {r#"
                component A(
                  class: String = "",
                  ...rest,
                ) {
                  <div class={class} ...rest>
                  </div>
                }

                component B(
                  class: String = "",
                  ...rest,
                ) {
                  <A class={class} ...rest/>
                }

                view Test {
                  <B class="main"/>
                }
            "#},
            r#"<div class="main"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A(class@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_fragment(v1)
                  write(">")
                  write("</div>")
                }
                fn B(class@v2: String, rest@v3: Fragment) -> Fragment {
                  call A(class = v2, rest = {
                    write_fragment(v3)
                  })
                }
                page Test() {
                  call B(class = "main", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div class=\"main\"></div>")
                }
                -- expected output --
                <div class="main"></div>
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
    fn accepts_optional_default_chain_uses_outer_default() {
        check(
            indoc! {r#"
                component A(
                  class: String = "a",
                  ...rest,
                ) {
                  <div class={class} ...rest>
                  </div>
                }

                component B(
                  class: String = "b",
                  ...rest,
                ) {
                  <A class={class} ...rest/>
                }

                view Test {
                  <B/>
                }
            "#},
            r#"<div class="b"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A(class@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_fragment(v1)
                  write(">")
                  write("</div>")
                }
                fn B(class@v2: String, rest@v3: Fragment) -> Fragment {
                  call A(class = v2, rest = {
                    write_fragment(v3)
                  })
                }
                page Test() {
                  call B(class = "b", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div class=\"b\"></div>")
                }
                -- expected output --
                <div class="b"></div>
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
    fn accepts_forwarded_optional_default_through_rest() {
        check(
            indoc! {r#"
                component A(
                  label: String = "x",
                  ...rest,
                ) {
                  <span ...rest>
                    {label}
                  </span>
                }

                component B(...rest) {
                  <A ...rest/>
                }

                view Test {
                  <B/>
                }
            "#},
            r#"<span>x</span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A(label@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<span")
                  write_fragment(v1)
                  write(">")
                  write_string(v0)
                  write("</span>")
                }
                fn B(label@v2: String, rest@v3: Fragment) -> Fragment {
                  call A(label = v2, rest = {
                    write_fragment(v3)
                  })
                }
                page Test() {
                  call B(label = "x", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<span>x</span>")
                }
                -- expected output --
                <span>x</span>
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
    fn accepts_forwarded_default_materialized_once_in_chain() {
        check(
            indoc! {r#"
                component Leaf(
                  label: String = "x",
                  ...rest,
                ) {
                  <span ...rest>
                    {label}
                  </span>
                }

                component Mid(...rest) {
                  <Leaf ...rest/>
                }

                component Top(...rest) {
                  <Mid ...rest/>
                }

                view Test {
                  <Top/>
                }
            "#},
            r#"<span>x</span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Leaf(label@v0: String, rest@v1: Fragment) -> Fragment {
                  write("<span")
                  write_fragment(v1)
                  write(">")
                  write_string(v0)
                  write("</span>")
                }
                fn Mid(label@v2: String, rest@v3: Fragment) -> Fragment {
                  call Leaf(label = v2, rest = {
                    write_fragment(v3)
                  })
                }
                fn Top(label@v4: String, rest@v5: Fragment) -> Fragment {
                  call Mid(label = v4, rest = {
                    write_fragment(v5)
                  })
                }
                page Test() {
                  call Top(label = "x", rest = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<span>x</span>")
                }
                -- expected output --
                <span>x</span>
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
    fn accepts_forwarded_attr_distinct_from_pinned() {
        check(
            indoc! {r#"
                component Inner(...rest) {
                  <span ...rest>
                  </span>
                }

                component Wrapper(...rest) {
                  <Inner title="a" ...rest/>
                }

                view Test {
                  <Wrapper lang="en"/>
                }
            "#},
            r#"<span title="a" lang="en"></span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner(rest@v0: Fragment) -> Fragment {
                  write("<span")
                  write_fragment(v0)
                  write(">")
                  write("</span>")
                }
                fn Wrapper(rest@v1: Fragment) -> Fragment {
                  call Inner(rest = {
                    write(" title=\"a\"")
                    write_fragment(v1)
                  })
                }
                page Test() {
                  call Wrapper(rest = {
                    write(" lang=\"en\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<span title=\"a\" lang=\"en\"></span>")
                }
                -- expected output --
                <span title="a" lang="en"></span>
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
    fn accepts_param_named_like_html_attr_alongside_tail_attr() {
        check(
            indoc! {r#"
                component A(
                  tabindex: Int,
                  ...rest,
                ) {
                  <div ...rest>
                    <if {tabindex > 0}>
                      focusable
                    </if>
                  </div>
                }

                component B(...rest) {
                  <A ...rest/>
                }

                view Test {
                  <B tabindex={2} data-x="y"/>
                }
            "#},
            r#"<div data-x="y">focusable</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A(tabindex@v0: Int, rest@v1: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v1)
                  write(">")
                  match (0 < v0) {
                    true => {
                      write("focusable")
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                fn B(tabindex@v2: Int, rest@v3: Fragment) -> Fragment {
                  call A(tabindex = v2, rest = {
                    write_fragment(v3)
                  })
                }
                page Test() {
                  call B(tabindex = 2, rest = {
                    write(" data-x=\"y\"")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div data-x=\"y\">focusable</div>")
                }
                -- expected output --
                <div data-x="y">focusable</div>
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
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    let v3 = match v0 {
                      Some(v1) => {
                        let v2 = v1 in { Option[String]::Some(v2) }
                      }
                      None => { Option[String]::None }
                    } in {
                      match v3 {
                        Some(v4) => {
                          let v5 = v4 in {
                            write("mapped:")
                            write_string(v5)
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
                page Test() {
                  write("mapped:hello")
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
    fn record_match_on_expression_subject() {
        check(
            indoc! {r#"
                record Point {
                  x: String,
                  y: String,
                }

                view Test {
                  <let {
                    result: String = match Point {x: "hi", y: "bye"} {
                      Point {x: a, y: _} => a,
                    },
                  }>
                    got:{result}
                  </let>
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                record Point {
                  x: String,
                  y: String,
                }
                page Test() {
                  let v3 = let v0 = Point {x: "hi", y: "bye"} in {
                    let v1 = v0.x in { let v2 = v1 in { v2 } }
                  } in {
                    write("got:")
                    write_string(v3)
                  }
                }
                -- ir (optimized) --
                record Point {
                  x: String,
                  y: String,
                }
                page Test() {
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
    fn bind_all_match_on_expression_subject() {
        check(
            indoc! {r#"
                record Point {
                  x: String,
                  y: String,
                }

                view Test {
                  <let {
                    result: String = match Point {x: "hi", y: "bye"} {
                      p => p.x,
                    },
                  }>
                    got:{result}
                  </let>
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                record Point {
                  x: String,
                  y: String,
                }
                page Test() {
                  let v1 = let v0 = Point {x: "hi", y: "bye"} in {
                    v0.x
                  } in {
                    write("got:")
                    write_string(v1)
                  }
                }
                -- ir (optimized) --
                record Point {
                  x: String,
                  y: String,
                }
                page Test() {
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
    fn match_on_expression_subject() {
        check(
            indoc! {r#"
                view Test {
                  <match {Some("hi")}>
                    <case {Some(x)}>
                      got:{x}
                    </case>
                    <case {None}>
                      none
                    </case>
                  </match>
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  match Option[String]::Some("hi") {
                    Some(v0) => {
                      let v1 = v0 in {
                        write("got:")
                        write_string(v1)
                      }
                    }
                    None => {
                      write("none")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = Option[String]::Some("inner") in {
                    let v3 = Option[String]::Some(match v0 {
                      Some(v1) => { let v2 = v1 in { v2 } }
                      None => { "default" }
                    }) in {
                      match v3 {
                        Some(v4) => {
                          let v5 = v4 in {
                            write_string(v5)
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
                page Test() {
                  write("inner")
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
    fn component_binding_duplicated_by_inlining_at_two_call_sites() {
        check(
            indoc! {r#"
                component Tag(text: String) {
                  <let {label: String = text}>
                    [{label}]
                  </let>
                }

                view Test {
                  <Tag text="a"/>
                  <Tag text="b"/>
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
                fn Tag(text@v0: String) -> Fragment {
                  let v1 = v0 in {
                    write("[")
                    write_string(v1)
                    write("]")
                  }
                }
                page Test() {
                  call Tag(text = "a")
                  call Tag(text = "b")
                }
                -- ir (optimized) --
                page Test() {
                  write("[a][b]")
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
    fn component_arguments_are_bound_simultaneously() {
        check(
            indoc! {r#"
                component Swap(
                  a: String,
                  b: String,
                ) {
                  <p>
                    {a}
                  </p>
                  <p>
                    {b}
                  </p>
                }

                view Test {
                  <let {a: String = "A"}>
                    <let {b: String = "B"}>
                      <Swap a={b} b={a}/>
                    </let>
                  </let>
                }
            "#},
            "<p>B</p><p>A</p>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Swap(a@v2: String, b@v3: String) -> Fragment {
                  write("<p")
                  write(">")
                  write_string(v2)
                  write("</p>")
                  write("<p")
                  write(">")
                  write_string(v3)
                  write("</p>")
                }
                page Test() {
                  let v0 = "A" in {
                    let v1 = "B" in {
                      call Swap(a = v1, b = v0)
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("<p>B</p><p>A</p>")
                }
                -- expected output --
                <p>B</p><p>A</p>
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
    fn rest_spread_attributes_are_evaluated_in_caller_scope() {
        check(
            indoc! {r#"
                component Rows(
                  items: Array[String],
                  ...rest,
                ) {
                  <for {item in items}>
                    <div ...rest>
                      {item}
                    </div>
                  </for>
                }

                view Test {
                  <let {item: String = "outer"}>
                    <Rows items={["a", "b"]} id={item}/>
                  </let>
                }
            "#},
            r#"<div id="outer">a</div><div id="outer">b</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Rows(
                  items@v1: Array[String],
                  rest@v2: Fragment,
                ) -> Fragment {
                  for v3 in v1 {
                    write("<div")
                    write_fragment(v2)
                    write(">")
                    write_string(v3)
                    write("</div>")
                  }
                }
                page Test() {
                  let v0 = "outer" in {
                    call Rows(items = ["a", "b"], rest = {
                      write(" id=\"")
                      write_string(v0)
                      write("\"")
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v5 = {
                    write(" id=\"outer\"")
                  } in {
                    for v6 in ["a", "b"] {
                      write("<div")
                      write_fragment(v5)
                      write(">")
                      write_string(v6)
                      write("</div>")
                    }
                  }
                }
                -- expected output --
                <div id="outer">a</div><div id="outer">b</div>
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
                page Test() {
                  let v0 = true in {
                    write_string(match v0 {
                      true => { "yes" }
                      false => { "no" }
                    })
                  }
                  let v1 = false in {
                    write_string(match v1 {
                      true => { "YES" }
                      false => { "NO" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "" in {
                    let v1 = "main" in {
                      write_string(match (v0 == "") {
                        true => { v1 }
                        false => { ((v1 + " - ") + v0) }
                      })
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = Option[String]::Some("hi") in {
                    write_string(match v0 {
                      Some(_) => { "some" }
                      None => { "none" }
                    })
                  }
                  write(",")
                  let v1 = Option[String]::None in {
                    write_string(match v1 {
                      Some(_) => { "SOME" }
                      None => { "NONE" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = true in {
                    let v1 = false in {
                      write_string(match v0 {
                        true => {
                          match v1 { true => { "TT" } false => { "TF" } }
                        }
                        false => { "F" }
                      })
                    }
                  }
                  write(",")
                  let v2 = false in {
                    let v3 = true in {
                      write_string(match v2 {
                        true => {
                          match v3 { true => { "TT" } false => { "TF" } }
                        }
                        false => { "F" }
                      })
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = (-123) in {
                    write_string(v0.to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("-123")
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
                page Test() {
                  let v0 = (-2.9) in {
                    write_string(v0.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("-2")
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
                page Test() {
                  write("<h1")
                  write(">")
                  write("Hello, World!")
                  write("</h1>")
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  write("<h1")
                  write(">")
                  write("Hello, World!")
                  write("</h1>")
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "Alice" in {
                    write("Hello, ")
                    write_string(v0)
                    write("!")
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = true in {
                    match v0 {
                      true => {
                        write("Visible")
                      }
                      false => {
                      }
                    }
                    match (!v0) {
                      true => {
                        write("Hidden")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  for v0 in ["a", "b", "c"] {
                    write_string(v0)
                    write(",")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b", "c"] {
                    write_string(v0)
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
    fn for_loop_over_bool_array_with_if() {
        check(
            indoc! {r#"
                view Test {
                  <for {v in [true]}>
                    <if {v}>
                      x
                    </if>
                  </for>
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [true] {
                    match v0 {
                      true => {
                        write("x")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [true] {
                    match v0 {
                      true => {
                        write("x")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- expected output --
                x
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
                page Test() {
                  for v0 in 1..=3 {
                    write_string(v0.to_string())
                    write(",")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in 1..=3 {
                    write_string(v0.to_string())
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
                page Test() {
                  for v0 in 0..=5 {
                    write_string(v0.to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in 0..=5 {
                    write_string(v0.to_string())
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
                page Test() {
                  for v0 in 1..=2 {
                    for v1 in 1..=2 {
                      write("(")
                      write_string(v0.to_string())
                      write(",")
                      write_string(v1.to_string())
                      write(")")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in 1..=2 {
                    for v1 in 1..=2 {
                      write("(")
                      write_string(v0.to_string())
                      write(",")
                      write_string(v1.to_string())
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
                page Test() {
                  let v0 = "<div>Hello & world</div>" in {
                    write_string(v0)
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "Hello from let" in {
                    write_string(v0)
                  }
                }
                -- ir (optimized) --
                page Test() {
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
    fn string_concat_folds_constants_around_a_dynamic_part() {
        check(
            indoc! {r#"
                view Test {
                  <for {name in ["a", "b"]}>
                    <span class={
                      join!(
                        name,
                        "px-2",
                        "py-1",
                      )
                    }>
                      {name + "!" + "?"}
                    </span>
                  </for>
                }
            "#},
            "<span class=\"a px-2 py-1\">a!?</span><span class=\"b px-2 py-1\">b!?</span>",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    write("<span")
                    write(" class=\"")
                    write_string((v0 + " " + "px-2" + " " + "py-1"))
                    write("\"")
                    write(">")
                    write_string(((v0 + "!") + "?"))
                    write("</span>")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    write("<span class=\"")
                    write_string(v0)
                    write(" px-2 py-1\">")
                    write_string(v0)
                    write("!?</span>")
                  }
                }
                -- expected output --
                <span class="a px-2 py-1">a!?</span><span class="b px-2 py-1">b!?</span>
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
                page Test() {
                  let v0 = "Hello" in {
                    let v1 = " World" in {
                      write_string((v0 + v1))
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  for v0 in ["A", "B"] {
                    let v1 = "[" in {
                      write_string(v1)
                      write_string(v0)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["A", "B"] {
                    write("[")
                    write_string(v0)
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
                page Test() {
                  match (("foo" + "bar") == "foobar") {
                    true => {
                      write("equals")
                    }
                    false => {
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  match (3 < 5) {
                    true => {
                      write("3 &lt; 5")
                    }
                    false => {
                    }
                  }
                  match (10 < 2) {
                    true => {
                      write("10 &lt; 2")
                    }
                    false => {
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("3 &lt; 5")
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
                page Test() {
                  match (1.5 < 2.5) {
                    true => {
                      write("1.5 &lt; 2.5")
                    }
                    false => {
                    }
                  }
                  match (3 < 1) {
                    true => {
                      write("3.0 &lt; 1.0")
                    }
                    false => {
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("1.5 &lt; 2.5")
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
                page Test() {
                  let v0 = true in {
                    match v0 {
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
                page Test() {
                  write("yes")
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
                page Test() {
                  let v0 = false in {
                    match v0 {
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
                page Test() {
                  write("no")
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
                page Test() {
                  let v0 = Person {name: "Alice", age: 30} in {
                    write_string(v0.name)
                    match (v0.age == 30) {
                      true => {
                        write(":30")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Person {
                  name: String,
                  age: Int,
                }
                page Test() {
                  write("Alice:30")
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
    fn record_literal_with_fields_out_of_declaration_order() {
        check(
            indoc! {r#"
                record Pair {
                  first: String,
                  second: String,
                }

                view Test {
                  <let {pair: Pair = Pair {second: "b", first: "a"}}>
                    {pair.first}-{pair.second}
                  </let>
                }
            "#},
            "a-b",
            expect![[r#"
                -- ir (unoptimized) --
                record Pair {
                  first: String,
                  second: String,
                }
                page Test() {
                  let v0 = Pair {second: "b", first: "a"} in {
                    write_string(v0.first)
                    write("-")
                    write_string(v0.second)
                  }
                }
                -- ir (optimized) --
                record Pair {
                  first: String,
                  second: String,
                }
                page Test() {
                  write("a-b")
                }
                -- expected output --
                a-b
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
    fn enum_literal_with_fields_out_of_declaration_order() {
        check(
            indoc! {r#"
                enum Shape {
                  Rect {
                    width: String,
                    height: String,
                  },
                }

                view Test {
                  <let {shape = Shape::Rect {height: "b", width: "a"}}>
                    <match {shape}>
                      <case {Shape::Rect {width: w, height: h}}>
                        {w}-{h}
                      </case>
                    </match>
                  </let>
                }
            "#},
            "a-b",
            expect![[r#"
                -- ir (unoptimized) --
                enum Shape {
                  Rect {width: String, height: String},
                }
                page Test() {
                  let v0 = Shape::Rect {height: "b", width: "a"} in {
                    match v0 {
                      Shape::Rect(width: v1, height: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            write_string(v3)
                            write("-")
                            write_string(v4)
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Shape {
                  Rect {width: String, height: String},
                }
                page Test() {
                  write("a-b")
                }
                -- expected output --
                a-b
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
                page Test() {
                  let v0 = Person {
                    name: "Alice",
                    address: Address {city: "Paris", zip: "75001"},
                  } in {
                    write_string(v0.name)
                    write(",")
                    write_string(v0.address.city)
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
                page Test() {
                  write("Alice,Paris")
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
                page Test() {
                  let v0 = 3 in {
                    let v1 = 7 in {
                      match ((v0 + v1) == 10) {
                        true => {
                          write("correct")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("correct")
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
                page Test() {
                  let v0 = 10 in {
                    let v1 = 3 in {
                      match ((v0 - v1) == 7) {
                        true => {
                          write("correct")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("correct")
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
                page Test() {
                  let v0 = 4 in {
                    let v1 = 5 in {
                      match ((v0 * v1) == 20) {
                        true => {
                          write("correct")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("correct")
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
                page Test() {
                  let v0 = true in {
                    let v1 = true in {
                      match (v0 && v1) {
                        true => {
                          write("TT")
                        }
                        false => {
                        }
                      }
                    }
                  }
                  let v2 = true in {
                    let v3 = false in {
                      match (v2 && v3) {
                        true => {
                          write("TF")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = false in {
                    let v1 = true in {
                      match (v0 || v1) {
                        true => {
                          write("FT")
                        }
                        false => {
                        }
                      }
                    }
                  }
                  let v2 = false in {
                    let v3 = false in {
                      match (v2 || v3) {
                        true => {
                          write("FF")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  match (3 <= 5) {
                    true => {
                      write("A")
                    }
                    false => {
                    }
                  }
                  match (5 <= 5) {
                    true => {
                      write("B")
                    }
                    false => {
                    }
                  }
                  match (7 <= 5) {
                    true => {
                      write("C")
                    }
                    false => {
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("AB")
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
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
                          write_string(v2)
                        }
                      }
                      None => {
                        write("none")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("hello")
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
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0 {
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
                page Test() {
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
                page Test() {
                  let v0 = Option[String]::Some("inner") in {
                    let v3 = Option[String]::Some(match v0 {
                      Some(v1) => { let v2 = v1 in { v2 } }
                      None => { "default" }
                    }) in {
                      match v3 {
                        Some(v4) => {
                          let v5 = v4 in {
                            write_string(v5)
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
                page Test() {
                  write("inner")
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
                page Test() {
                  for v0 in [
                    Option[String]::Some("a"),
                    Option[String]::None,
                    Option[String]::Some("b"),
                  ] {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
                          write("[")
                          write_string(v2)
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
                page Test() {
                  for v0 in [
                    Option[String]::Some("a"),
                    Option[String]::None,
                    Option[String]::Some("b"),
                  ] {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
                          write("[")
                          write_string(v2)
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
                page Test() {
                  let v0 = Color::Green in {
                    write_string(match v0 {
                      Color::Red => { "red" }
                      Color::Green => { "green" }
                      Color::Blue => { "blue" }
                    })
                  }
                }
                -- ir (optimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                page Test() {
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
                page Test() {
                  let v0 = Color::Blue in {
                    match v0 {
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
                page Test() {
                  write("blue")
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
                page Test() {
                  let v0 = Outcome::Success {value: "hello"} in {
                    match v0 {
                      Outcome::Success(value: v1) => {
                        let v2 = v1 in {
                          write("Ok:")
                          write_string(v2)
                        }
                      }
                      Outcome::Failure(message: v3) => {
                        let v4 = v3 in {
                          write("Err:")
                          write_string(v4)
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
                page Test() {
                  write("Ok:hello")
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
    fn enum_with_field_named_tag() {
        check(
            indoc! {r#"
                enum Item {
                  Tagged {
                    tag: String,
                  },
                  Plain,
                }

                view Test {
                  <let {item: Item = Item::Tagged {tag: "news"}}>
                    <match {item}>
                      <case {Item::Tagged {tag: t}}>
                        tag:{t}
                      </case>
                      <case {Item::Plain}>
                        plain
                      </case>
                    </match>
                  </let>
                }
            "#},
            "tag:news",
            expect![[r#"
                -- ir (unoptimized) --
                enum Item {
                  Tagged {tag: String},
                  Plain,
                }
                page Test() {
                  let v0 = Item::Tagged {tag: "news"} in {
                    match v0 {
                      Item::Tagged(tag: v1) => {
                        let v2 = v1 in {
                          write("tag:")
                          write_string(v2)
                        }
                      }
                      Item::Plain => {
                        write("plain")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Item {
                  Tagged {tag: String},
                  Plain,
                }
                page Test() {
                  write("tag:news")
                }
                -- expected output --
                tag:news
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
                page Test() {
                  let v4 = match Outcome::Success {value: "hi"} {
                    Outcome::Success {value: v0} => {
                      let v1 = v0 in { v1 }
                    }
                    Outcome::Failure {message: v2} => {
                      let v3 = v2 in { v3 }
                    }
                  } in {
                    write("got:")
                    write_string(v4)
                  }
                }
                -- ir (optimized) --
                enum Outcome {
                  Success {value: String},
                  Failure {message: String},
                }
                page Test() {
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
                fn Badge(color@v0: test::Color) -> Fragment {
                  match v0 {
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
                page Test() {
                  call Badge(color = Color::Green)
                }
                -- ir (optimized) --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                page Test() {
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
                page Test() {
                  let v0 = Outcome::Failure {message: "something went wrong"} in {
                    match v0 {
                      Outcome::Success(value: v1) => {
                        let v2 = v1 in {
                          write("Ok:")
                          write_string(v2)
                        }
                      }
                      Outcome::Failure(message: v3) => {
                        let v4 = v3 in {
                          write("Err:")
                          write_string(v4)
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
                page Test() {
                  write("Err:something went wrong")
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
                page Test() {
                  let v0 = Response::Win {code: "200", body: "OK"} in {
                    match v0 {
                      Response::Win(code: v1, body: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            write_string(v3)
                            write(":")
                            write_string(v4)
                          }
                        }
                      }
                      Response::Lose(reason: v5) => {
                        let v6 = v5 in {
                          write("Error:")
                          write_string(v6)
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
                page Test() {
                  write("200:OK")
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
                page Test() {
                  let v0 = Outcome::Success {value: "hello"} in {
                    match v0 {
                      Outcome::Success(value: v1) => {
                        let v2 = v1 in {
                          write("Ok:")
                          write_string(v2)
                        }
                      }
                      Outcome::Failure(message: v3) => {
                        let v4 = v3 in {
                          write("Err:")
                          write_string(v4)
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
                page Test() {
                  write("Ok:hello")
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
                page Test() {
                  let v0 = ["a", "b", "c"] in {
                    write_string(v0.len().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("3")
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
                page Test() {
                  let v0 = [] in {
                    write_string(v0.len().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("0")
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
                page Test() {
                  let v0 = ["x", "y"] in {
                    match (v0.len() == 2) {
                      true => {
                        write("has two")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("has two")
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
                page Test() {
                  let v0 = ["a"] in {
                    match (v0.len() < 5) {
                      true => {
                        write("less than 5")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("less than 5")
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
                page Test() {
                  let v0 = [1, 2, 3, 4, 5] in {
                    write_string(v0.len().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("5")
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
                page Test() {
                  let v0 = [] in {
                    match v0.is_empty() {
                      true => {
                        write("empty")
                      }
                      false => {
                        write("not empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("empty")
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
                page Test() {
                  let v0 = ["a", "b"] in {
                    match v0.is_empty() {
                      true => {
                        write("empty")
                      }
                      false => {
                        write("not empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("not empty")
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
                page Test() {
                  let v0 = [1, 2, 3] in {
                    match v0.is_empty() {
                      true => {
                        write("no numbers")
                      }
                      false => {
                        write("has numbers")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("has numbers")
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
                page Test() {
                  let v0 = 42 in {
                    write_string(v0.to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("42")
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
                page Test() {
                  let v0 = 0 in {
                    write_string(v0.to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("0")
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
                page Test() {
                  let v0 = 5 in {
                    write_string(("Count: " + v0.to_string()))
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("Count: 5")
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
                page Test() {
                  let v0 = 3.7 in {
                    write_string(v0.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("3")
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
                page Test() {
                  let v0 = 5 in {
                    write_string(v0.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("5")
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
                page Test() {
                  for _ in 0..=2 {
                    write("x")
                  }
                }
                -- ir (optimized) --
                page Test() {
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
    fn for_loop_variable_left_unused_by_optimization_becomes_underscore() {
        check(
            indoc! {r#"
                view Test {
                  <for {x in ["a", "b"]}>
                    <if {false}>
                      {x}
                    </if>
                    y
                  </for>
                }
            "#},
            "yy",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    match false {
                      true => {
                        write_string(v0)
                      }
                      false => {
                      }
                    }
                    write("y")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for _ in ["a", "b"] {
                    write("y")
                  }
                }
                -- expected output --
                yy
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
                page Test() {
                  let v0 = ["a", "b", "c"] in {
                    for _ in v0 {
                      write("*")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for _ in ["a", "b", "c"] {
                    write("*")
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
                page Test() {
                  for _ in 0..=1 {
                    for _ in 0..=2 {
                      write(".")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  for v0 in 1..=2 {
                    for _ in 0..=1 {
                      write_string(v0.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in 1..=2 {
                    for _ in 0..=1 {
                      write_string(v0.to_string())
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
                page Test() {
                  write_string([1, 2, 3].len().to_string())
                }
                -- ir (optimized) --
                page Test() {
                  write("3")
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
                page Test() {
                  write_string((1 + 2).to_string())
                }
                -- ir (optimized) --
                page Test() {
                  write("3")
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
                page Test() {
                  write_string(42.to_string())
                }
                -- ir (optimized) --
                page Test() {
                  write("42")
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
                page Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("deep")) in {
                    match v0 {
                      Some(v1) => {
                        match v1 {
                          Some(v2) => {
                            let v3 = v2 in {
                              write_string(v3)
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
                page Test() {
                  write("deep")
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
                page Test() {
                  let v0 = Option[String]::Some("x") in {
                    match v0 {
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
                page Test() {
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
                page Test() {
                  let v0 = Option[String]::None in {
                    match v0 {
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
                page Test() {
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
                page Test() {
                  let v0 = Option[String]::Some("x") in {
                    write_string(match v0 {
                      Some(_) => { "some" }
                      None => { "none" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = Option[String]::None in {
                    write_string(match v0 {
                      Some(_) => { "some" }
                      None => { "none" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match v0 {
                      Some(v1) => {
                        match v1 {
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
                page Test() {
                  write("some-some")
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
                page Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("x")) in {
                    match v0 {
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
                page Test() {
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
                page Test() {
                  let v0 = Outcome::Success {value: "hello"} in {
                    match v0 {
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
                page Test() {
                  write("ok")
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
                page Test() {
                  let v0 = Outcome::Failure {message: "failed"} in {
                    match v0 {
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
                page Test() {
                  write("err")
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
                page Test() {
                  let v0 = Person {name: "Alice", age: 30} in {
                    let v1 = v0.age in {
                      let v2 = v1 in {
                        write("age:")
                        write_string(v2.to_string())
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Person {
                  name: String,
                  age: Int,
                }
                page Test() {
                  write("age:30")
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
                page Test() {
                  let v0 = Option[Option[Option[String]]]::Some(Option[Option[String]]::Some(Option[String]::Some("value"))) in {
                    match v0 {
                      Some(v1) => {
                        match v1 {
                          Some(v2) => {
                            match v2 {
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
                page Test() {
                  write("triple-some")
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
                page Test() {
                  let v0 = Outer::Success {value: Inner::Success {value: "deep"}} in {
                    match v0 {
                      Outer::Success(value: v1) => {
                        match v1 {
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
                page Test() {
                  write("ok-ok")
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
                page Test() {
                  let v0 = true in {
                    write_string(match v0 {
                      true => { "t" }
                      false => { "f" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = false in {
                    write_string(match v0 {
                      true => { "t" }
                      false => { "f" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  match Option[String]::Some("outer") {
                    Some(v0) => {
                      let v1 = v0 in {
                        match Option[String]::Some("inner") {
                          Some(v2) => {
                            let v3 = v2 in {
                              write_string(v1)
                              write(":")
                              write_string(v3)
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
                -- ir (optimized) --
                page Test() {
                  write("outer:inner")
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
                page Test() {
                  let v0 = Option[Option[String]]::Some(Option[String]::Some("hello")) in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
                          match v2 {
                            Some(v3) => {
                              let v4 = v3 in {
                                write("value:")
                                write_string(v4)
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
                page Test() {
                  write("value:hello")
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
                page Test() {
                  write("<div")
                  write(" class=\"")
                  write_string(("foo" + " " + "bar" + " " + "baz"))
                  write("\"")
                  write(">")
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "removed" in {
                    write_string(v0)
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "my-class" in {
                    write("<div")
                    write(" class=\"")
                    write_string(v0)
                    write("\"")
                    write(">")
                    write("</div>")
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "on" in {
                    write("<span")
                    write(">")
                    write_string(v0)
                    write("</span>")
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = "button" in {
                    write("<input")
                    write(" type=\"")
                    write_string(v0)
                    write("\"")
                    write(">")
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  write("<label")
                  write(" for=\"email\"")
                  write(">")
                  write("Email")
                  write("</label>")
                }
                -- ir (optimized) --
                page Test() {
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
    fn view_parameter_named_typescript_reserved_keyword() {
        check(
            indoc! {r#"
                view Test {
                  ok
                }

                view Other(delete: String) {
                  {delete}
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("ok")
                }
                page Other(delete@v0: String) {
                  write_string(v0)
                }
                -- ir (optimized) --
                page Test() {
                  write("ok")
                }
                page Other(delete@v0: String) {
                  write_string(v0)
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
    fn view_parameter_named_rust_keyword() {
        check(
            indoc! {r#"
                view Test {
                  ok
                }

                view Other(type: String) {
                  {type}
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("ok")
                }
                page Other(type@v0: String) {
                  write_string(v0)
                }
                -- ir (optimized) --
                page Test() {
                  write("ok")
                }
                page Other(type@v0: String) {
                  write_string(v0)
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
    fn recursive_component_parameter_named_typescript_reserved_keyword() {
        check(
            indoc! {r#"
                component Countdown(delete: Int) {
                  {delete.to_string()}
                  <if {0 < delete}>
                    <Countdown delete={delete - 1}/>
                  </if>
                }

                view Test {
                  <Countdown delete={3}/>
                }
            "#},
            "3210",
            expect![[r#"
                -- ir (unoptimized) --
                fn Countdown(delete@v0: Int) -> Fragment {
                  write_string(v0.to_string())
                  match (0 < v0) {
                    true => {
                      call Countdown(delete = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Countdown(delete = 3)
                }
                -- ir (optimized) --
                fn Countdown(delete@v0: Int) -> Fragment {
                  write_string(v0.to_string())
                  match (0 < v0) {
                    true => {
                      call Countdown(delete = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Countdown(delete = 3)
                }
                -- expected output --
                3210
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
    fn recursive_component_parameter_named_rust_keyword() {
        check(
            indoc! {r#"
                component Countdown(type: Int) {
                  {type.to_string()}
                  <if {0 < type}>
                    <Countdown type={type - 1}/>
                  </if>
                }

                view Test {
                  <Countdown type={3}/>
                }
            "#},
            "3210",
            expect![[r#"
                -- ir (unoptimized) --
                fn Countdown(type@v0: Int) -> Fragment {
                  write_string(v0.to_string())
                  match (0 < v0) {
                    true => {
                      call Countdown(type = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Countdown(type = 3)
                }
                -- ir (optimized) --
                fn Countdown(type@v0: Int) -> Fragment {
                  write_string(v0.to_string())
                  match (0 < v0) {
                    true => {
                      call Countdown(type = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Countdown(type = 3)
                }
                -- expected output --
                3210
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
                page Test() {
                  write_string("\"")
                  write(" ")
                  write_string("\\")
                  write(" ")
                  write_string("foo\nbar")
                  write(" ")
                  write_string("foo\tbar")
                  write(" ")
                  write_string("C:\\Users\\name")
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = [
                    Item {name: "a", value: "1"},
                    Item {name: "b", value: "2"},
                  ] in {
                    for v1 in v0 {
                      let v2 = v1.name in {
                        write("[")
                        write_string(v2)
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
                page Test() {
                  for v1 in [
                    Item {name: "a", value: "1"},
                    Item {name: "b", value: "2"},
                  ] {
                    let v2 = v1.name in {
                      write("[")
                      write_string(v2)
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
                page Test() {
                  let v0 = [
                    Person {
                      name: "alice",
                      address: Address {city: "paris"},
                    },
                    Person {name: "bob", address: Address {city: "london"}},
                  ] in {
                    for v1 in v0 {
                      let v2 = v1.address.city in {
                        write("[")
                        write_string(v2)
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
                page Test() {
                  for v1 in [
                    Person {
                      name: "alice",
                      address: Address {city: "paris"},
                    },
                    Person {name: "bob", address: Address {city: "london"}},
                  ] {
                    let v2 = v1.address.city in {
                      write("[")
                      write_string(v2)
                      write("]")
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
                page Test() {
                  let v0 = [
                    Source {name: "a", value: "1"},
                    Source {name: "b", value: "2"},
                  ] in {
                    for v1 in v0 {
                      let v2 = Target {label: v1.name} in {
                        write("[")
                        write_string(v2.label)
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
                page Test() {
                  for v1 in [
                    Source {name: "a", value: "1"},
                    Source {name: "b", value: "2"},
                  ] {
                    let v2 = Target {label: v1.name} in {
                      write("[")
                      write_string(v2.label)
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
                page Test() {
                  let v0 = [Item {name: "a"}, Item {name: "b"}] in {
                    for v1 in v0 {
                      let v2 = Option[String]::Some(v1.name) in {
                        match v2 {
                          Some(v3) => {
                            let v4 = v3 in {
                              write("[")
                              write_string(v4)
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
                page Test() {
                  for v1 in [Item {name: "a"}, Item {name: "b"}] {
                    let v2 = Option[String]::Some(v1.name) in {
                      match v2 {
                        Some(v3) => {
                          let v4 = v3 in {
                            write("[")
                            write_string(v4)
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
                page Test() {
                  let v0 = "hello" in {
                    let v1 = "world" in {
                      let v2 = ((v0 + " ") + v1) in {
                        write("[")
                        write_string(v2)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  let v0 = Greeting {message: ("hello" + " world")} in {
                    write_string(v0.message)
                  }
                }
                -- ir (optimized) --
                record Greeting {
                  message: String,
                }
                page Test() {
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
                page Test() {
                  let v0 = 42 in {
                    let v1 = v0.to_string() in {
                      write("[")
                      write_string(v1)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("[42]")
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
                page Test() {
                  let v0 = Container {items: ["a", "b"]} in {
                    for v1 in v0.items {
                      write("[")
                      write_string(v1)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                record Container {
                  items: Array[String],
                }
                page Test() {
                  for v1 in ["a", "b"] {
                    write("[")
                    write_string(v1)
                    write("]")
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
                page Test() {
                  let v0 = Label {text: 42.to_string()} in {
                    write("[")
                    write_string(v0.text)
                    write("]")
                  }
                }
                -- ir (optimized) --
                record Label {
                  text: String,
                }
                page Test() {
                  write("[42]")
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
                page Test() {
                  let v0 = Outer {inner: Inner {values: ["x", "y"]}} in {
                    for v1 in v0.inner.values {
                      write("[")
                      write_string(v1)
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
                page Test() {
                  for v1 in ["x", "y"] {
                    write("[")
                    write_string(v1)
                    write("]")
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
                page Test() {
                  let v0 = Foo {a: "hello"} in {
                    let v1 = Foo {a: v0.a} in {
                      write("[")
                      write_string(v0.a)
                      write("][")
                      write_string(v1.a)
                      write("]")
                    }
                  }
                }
                -- ir (optimized) --
                record Foo {
                  a: String,
                }
                page Test() {
                  write("[hello][hello]")
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
                page Test() {
                  let v0 = Foo {a: "hello"} in {
                    let v1 = true in {
                      let v2 = match v1 {
                        true => { v0.a }
                        false => { "default" }
                      } in {
                        write("[")
                        write_string(v2)
                        write("][")
                        write_string(v0.a)
                        write("]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Foo {
                  a: String,
                }
                page Test() {
                  write("[hello][hello]")
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
                page Test() {
                  let v0 = TreeNode {value: "leaf", children: []} in {
                    write_string(v0.value)
                  }
                }
                -- ir (optimized) --
                record TreeNode {
                  value: String,
                  children: Array[test::TreeNode],
                }
                page Test() {
                  write("leaf")
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
                page Test() {
                  let v0 = Node {
                    value: "first",
                    next: Option[test::Node]::None,
                  } in {
                    write_string(v0.value)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  write("first")
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
                page Test() {
                  let v0 = Expr::Literal {value: "42"} in {
                    match v0 {
                      Expr::Literal(value: v1) => {
                        let v2 = v1 in {
                          write_string(v2)
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
                page Test() {
                  write("42")
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
                page Test() {
                  let v0 = Expr::Neg {inner: Expr::Literal {value: "42"}} in {
                    match v0 {
                      Expr::Literal(value: v1) => {
                        let v2 = v1 in {
                          write("lit:")
                          write_string(v2)
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
                page Test() {
                  write("neg")
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
    fn mutually_recursive_records() {
        check(
            indoc! {r#"
                record Folder {
                  name: String,
                  parent: Option[File],
                }

                record File {
                  owner: Option[Folder],
                }

                view Test {
                  <let {f: Folder = Folder {name: "root", parent: None}}>
                    {f.name}
                  </let>
                }
            "#},
            "root",
            expect![[r#"
                -- ir (unoptimized) --
                record File {
                  owner: Option[test::Folder],
                }
                record Folder {
                  name: String,
                  parent: Option[test::File],
                }
                page Test() {
                  let v0 = Folder {
                    name: "root",
                    parent: Option[test::File]::None,
                  } in {
                    write_string(v0.name)
                  }
                }
                -- ir (optimized) --
                record File {
                  owner: Option[test::Folder],
                }
                record Folder {
                  name: String,
                  parent: Option[test::File],
                }
                page Test() {
                  write("root")
                }
                -- expected output --
                root
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
    fn three_type_recursion_cycle() {
        check(
            indoc! {r#"
                enum Expr {
                  Literal {
                    value: String,
                  },
                  Wrapped {
                    inner: Option[Node],
                  },
                }

                record Node {
                  next: Option[Leaf],
                }

                record Leaf {
                  back: Option[Expr],
                }

                view Test {
                  <let {leaf: Leaf = Leaf {back: None}}>
                    <match {leaf.back}>
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
                enum Expr {
                  Literal {value: String},
                  Wrapped {inner: Option[test::Node]},
                }
                record Leaf {
                  back: Option[test::Expr],
                }
                record Node {
                  next: Option[test::Leaf],
                }
                page Test() {
                  let v0 = Leaf {back: Option[test::Expr]::None} in {
                    match v0.back {
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
                enum Expr {
                  Literal {value: String},
                  Wrapped {inner: Option[test::Node]},
                }
                record Leaf {
                  back: Option[test::Expr],
                }
                record Node {
                  next: Option[test::Leaf],
                }
                page Test() {
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
    fn recursive_field_from_variable() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                view Test {
                  <let {tail: Option[Node] = None}>
                    <let {head: Node = Node {value: "head", next: tail}}>
                      {head.value}
                    </let>
                  </let>
                }
            "#},
            "head",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  let v0 = Option[test::Node]::None in {
                    let v1 = Node {value: "head", next: v0} in {
                      write_string(v1.value)
                    }
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  write("head")
                }
                -- expected output --
                head
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
    fn recursive_field_from_match_arms() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                view Test {
                  <let {leaf: Node = Node {value: "leaf", next: None}}>
                    <let {
                      head: Node = Node {
                        value: "head",
                        next: match true {
                          true => Some(leaf),
                          false => None,
                        },
                      },
                    }>
                      {head.value}
                    </let>
                  </let>
                }
            "#},
            "head",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  let v0 = Node {
                    value: "leaf",
                    next: Option[test::Node]::None,
                  } in {
                    let v1 = Node {
                      value: "head",
                      next: match true {
                        true => { Option[test::Node]::Some(v0) }
                        false => { Option[test::Node]::None }
                      },
                    } in {
                      write_string(v1.value)
                    }
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  write("head")
                }
                -- expected output --
                head
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
    fn nested_option_recursive_field() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Option[Node]],
                }

                view Test {
                  <let {n: Node = Node {value: "node", next: None}}>
                    {n.value}
                  </let>
                }
            "#},
            "node",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[Option[test::Node]],
                }
                page Test() {
                  let v0 = Node {
                    value: "node",
                    next: Option[Option[test::Node]]::None,
                  } in {
                    write_string(v0.value)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[Option[test::Node]],
                }
                page Test() {
                  write("node")
                }
                -- expected output --
                node
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
    fn reading_a_nested_option_boxed_field() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Option[Node]],
                }

                view Test {
                  <let {
                    n: Node = Node {
                      value: "head",
                      next: Some(Some(Node {value: "tail", next: None})),
                    },
                  }>
                    <match {n.next}>
                      <case {Some(inner)}>
                        <match {inner}>
                          <case {Some(m)}>
                            {m.value}
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
            "tail",
            expect![[r#"
                -- ir (unoptimized) --
                record Node {
                  value: String,
                  next: Option[Option[test::Node]],
                }
                page Test() {
                  let v0 = Node {
                    value: "head",
                    next: Option[Option[test::Node]]::Some(Option[test::Node]::Some(Node {
                      value: "tail",
                      next: Option[Option[test::Node]]::None,
                    })),
                  } in {
                    match v0.next {
                      Some(v1) => {
                        let v2 = v1 in {
                          match v2 {
                            Some(v3) => {
                              let v4 = v3 in {
                                write_string(v4.value)
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
                record Node {
                  value: String,
                  next: Option[Option[test::Node]],
                }
                page Test() {
                  write("tail")
                }
                -- expected output --
                tail
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
    fn reading_a_boxed_field() {
        check(
            indoc! {r#"
                record Node {
                  value: String,
                  next: Option[Node],
                }

                record Holder {
                  held: Option[Node],
                }

                view Test {
                  <let {n: Node = Node {value: "node", next: None}}>
                    <let {h: Holder = Holder {held: n.next}}>
                      <match {h.held}>
                        <case {Some(_)}>
                          some
                        </case>
                        <case {None}>
                          {n.value}
                        </case>
                      </match>
                    </let>
                  </let>
                }
            "#},
            "node",
            expect![[r#"
                -- ir (unoptimized) --
                record Holder {
                  held: Option[test::Node],
                }
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  let v0 = Node {
                    value: "node",
                    next: Option[test::Node]::None,
                  } in {
                    let v1 = Holder {held: v0.next} in {
                      match v1.held {
                        Some(_) => {
                          write("some")
                        }
                        None => {
                          write_string(v0.value)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Holder {
                  held: Option[test::Node],
                }
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                page Test() {
                  write("node")
                }
                -- expected output --
                node
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
    fn reading_a_directly_boxed_field() {
        check(
            indoc! {r#"
                record A {
                  b: B,
                }

                record B {
                  name: String,
                  a: Option[A],
                }

                view Test {
                  <let {x: A = A {b: B {name: "b", a: None}}}>
                    {x.b.name}
                    <match {x.b.a}>
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
            "bnone",
            expect![[r#"
                -- ir (unoptimized) --
                record A {
                  b: B,
                }
                record B {
                  name: String,
                  a: Option[test::A],
                }
                page Test() {
                  let v0 = A {
                    b: B {name: "b", a: Option[test::A]::None},
                  } in {
                    write_string(v0.b.name)
                    match v0.b.a {
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
                record A {
                  b: B,
                }
                record B {
                  name: String,
                  a: Option[test::A],
                }
                page Test() {
                  write("bnone")
                }
                -- expected output --
                bnone
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
    fn matching_a_boxed_enum_field() {
        check(
            indoc! {r#"
                enum Tree {
                  Node {
                    label: String,
                    left: Tree,
                    right: Option[Tree],
                  },
                  Leaf,
                }

                record Step {
                  t: Tree,
                  rest: Option[Tree],
                }

                view Test {
                  <let {
                    tree: Tree = Tree::Node {
                      label: "a",
                      left: Tree::Leaf,
                      right: None,
                    },
                  }>
                    <match {tree}>
                      <case {Tree::Node {label: l, left: lt, right: r}}>
                        <let {s: Step = Step {t: lt, rest: r}}>
                          {l}
                          <match {s.rest}>
                            <case {Some(_)}>
                              some
                            </case>
                            <case {None}>
                              none
                            </case>
                          </match>
                        </let>
                      </case>
                      <case {Tree::Leaf}>
                        empty
                      </case>
                    </match>
                  </let>
                }
            "#},
            "anone",
            expect![[r#"
                -- ir (unoptimized) --
                enum Tree {
                  Node {label: String, left: test::Tree, right: Option[test::Tree]},
                  Leaf,
                }
                record Step {
                  t: Tree,
                  rest: Option[test::Tree],
                }
                page Test() {
                  let v0 = Tree::Node {label: "a", left: Tree::Leaf, right: Option[test::Tree]::None} in {
                    match v0 {
                      Tree::Node(label: v1, left: v2, right: v3) => {
                        let v4 = v1 in {
                          let v5 = v2 in {
                            let v6 = v3 in {
                              let v7 = Step {t: v5, rest: v6} in {
                                write_string(v4)
                                match v7.rest {
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
                        }
                      }
                      Tree::Leaf => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Tree {
                  Node {label: String, left: test::Tree, right: Option[test::Tree]},
                  Leaf,
                }
                record Step {
                  t: Tree,
                  rest: Option[test::Tree],
                }
                page Test() {
                  write("anone")
                }
                -- expected output --
                anone
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
    fn matching_a_non_boxed_option_enum_field() {
        check(
            indoc! {r#"
                enum Contact {
                  Email {
                    address: String,
                    label: Option[String],
                  },
                  Anonymous,
                }

                view Test {
                  <let {
                    c: Contact = Contact::Email {
                      address: "a@b.c",
                      label: Some("work"),
                    },
                  }>
                    <match {c}>
                      <case {Contact::Email {address: a, label: l}}>
                        {a}
                        <match {l}>
                          <case {Some(s)}>
                            {s}
                          </case>
                          <case {None}>
                            no-label
                          </case>
                        </match>
                      </case>
                      <case {Contact::Anonymous}>
                        anon
                      </case>
                    </match>
                  </let>
                }
            "#},
            "a@b.cwork",
            expect![[r#"
                -- ir (unoptimized) --
                enum Contact {
                  Email {address: String, label: Option[String]},
                  Anonymous,
                }
                page Test() {
                  let v0 = Contact::Email {address: "a@b.c", label: Option[String]::Some("work")} in {
                    match v0 {
                      Contact::Email(address: v1, label: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            write_string(v3)
                            match v4 {
                              Some(v5) => {
                                let v6 = v5 in {
                                  write_string(v6)
                                }
                              }
                              None => {
                                write("no-label")
                              }
                            }
                          }
                        }
                      }
                      Contact::Anonymous => {
                        write("anon")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum Contact {
                  Email {address: String, label: Option[String]},
                  Anonymous,
                }
                page Test() {
                  write("a@b.cwork")
                }
                -- expected output --
                a@b.cwork
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
                fn Greeting(name@v0: String) -> Fragment {
                  write("Hello, ")
                  write_string(v0)
                  write("!")
                }
                page Test() {
                  call Greeting(name = "World")
                }
                -- ir (optimized) --
                page Test() {
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
                  children: Fragment,
                ) {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {children}
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
                fn Card(
                  title@v0: String,
                  children@v1: Fragment,
                ) -> Fragment {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_string(v0)
                  write("</h2>")
                  write_fragment(v1)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "Hello", children = {
                    write("<p")
                    write(">")
                    write("world")
                    write("</p>")
                  })
                }
                -- ir (optimized) --
                page Test() {
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
                component Inner(children: Fragment) {
                  <div class="inner">
                    {children}
                  </div>
                }

                component Outer(children: Fragment) {
                  <div class="outer">
                    <Inner>
                      {children}
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
                fn Inner(children@v0: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"inner\"")
                  write(">")
                  write_fragment(v0)
                  write("</div>")
                }
                fn Outer(children@v1: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"outer\"")
                  write(">")
                  call Inner(children = {
                    write_fragment(v1)
                  })
                  write("</div>")
                }
                page Test() {
                  call Outer(children = {
                    write("<p")
                    write(">")
                    write("hello")
                    write("</p>")
                  })
                }
                -- ir (optimized) --
                page Test() {
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

                component Layout(children: Fragment) {
                  <div class="layout">
                    {children}
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
                fn Footer() -> Fragment {
                  write("<footer")
                  write(">")
                  write("<p")
                  write(">")
                  write("Copyright 2024")
                  write("</p>")
                  write("</footer>")
                }
                fn Header(title@v0: String) -> Fragment {
                  write("<header")
                  write(">")
                  write("<h1")
                  write(">")
                  write_string(v0)
                  write("</h1>")
                  write("</header>")
                }
                fn Layout(children@v1: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"layout\"")
                  write(">")
                  write_fragment(v1)
                  write("</div>")
                }
                page Test() {
                  call Layout(children = {
                    call Header(title = "Welcome")
                    write("<main")
                    write(">")
                    write("<p")
                    write(">")
                    write("Hello world")
                    write("</p>")
                    write("</main>")
                    call Footer()
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div class=\"layout\"><header><h1>Welcome</h1></header>")
                  write("<main><p>Hello world</p></main>")
                  write("<footer><p>Copyright 2024</p></footer></div>")
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
    fn component_children_used_twice() {
        check(
            indoc! {r#"
                component Repeat(children: Fragment) {
                  <div class="first">
                    {children}
                  </div>
                  <div class="second">
                    {children}
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
                fn Repeat(children@v0: Fragment) -> Fragment {
                  write("<div")
                  write(" class=\"first\"")
                  write(">")
                  write_fragment(v0)
                  write("</div>")
                  write("<div")
                  write(" class=\"second\"")
                  write(">")
                  write_fragment(v0)
                  write("</div>")
                }
                page Test() {
                  call Repeat(children = {
                    write("<span")
                    write(">")
                    write("hi")
                    write("</span>")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  let v1 = {
                    write("<span>hi</span>")
                  } in {
                    write("<div class=\"first\">")
                    write_fragment(v1)
                    write("</div><div class=\"second\">")
                    write_fragment(v1)
                    write("</div>")
                  }
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
                fn Badge(text@v1: String) -> Fragment {
                  write("<strong")
                  write(">")
                  write_string(v1)
                  write("</strong>")
                }
                fn NodeView(node@v2: test::Node) -> Fragment {
                  call Badge(text = v2.value)
                  match v2.next {
                    Some(v3) => {
                      let v4 = v3 in {
                        call NodeView(node = v4)
                      }
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v0 = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::None,
                    }),
                  } in {
                    call NodeView(node = v0)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                fn NodeView(node@v2: test::Node) -> Fragment {
                  write("<strong>")
                  write_string(v2.value)
                  write("</strong>")
                  match v2.next {
                    Some(v3) => {
                      let v4 = v3 in {
                        call NodeView(node = v4)
                      }
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  call NodeView(node = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::None,
                    }),
                  })
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
                fn NodeView(node@v1: test::Node) -> Fragment {
                  write("<span")
                  write(">")
                  write_string(v1.value)
                  write("</span>")
                  match v1.next {
                    Some(v2) => {
                      let v3 = v2 in {
                        call NodeView(node = v3)
                      }
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v0 = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::Some(Node {
                        value: "c",
                        next: Option[test::Node]::None,
                      }),
                    }),
                  } in {
                    call NodeView(node = v0)
                  }
                }
                -- ir (optimized) --
                record Node {
                  value: String,
                  next: Option[test::Node],
                }
                fn NodeView(node@v1: test::Node) -> Fragment {
                  write("<span>")
                  write_string(v1.value)
                  write("</span>")
                  match v1.next {
                    Some(v2) => {
                      let v3 = v2 in {
                        call NodeView(node = v3)
                      }
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  call NodeView(node = Node {
                    value: "a",
                    next: Option[test::Node]::Some(Node {
                      value: "b",
                      next: Option[test::Node]::Some(Node {
                        value: "c",
                        next: Option[test::Node]::None,
                      }),
                    }),
                  })
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
                fn Card(title@v0: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "New card")
                }
                -- ir (optimized) --
                page Test() {
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
                fn Card(title@v0: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "Custom title")
                }
                -- ir (optimized) --
                page Test() {
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
                fn Card(title@v0: String, subtitle@v1: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write(" - ")
                  write_string(v1)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "Hello", subtitle = "No subtitle")
                }
                -- ir (optimized) --
                page Test() {
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
                fn Card(title@v0: String, subtitle@v1: String) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write(" - ")
                  write_string(v1)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "Hello", subtitle = "World")
                }
                -- ir (optimized) --
                page Test() {
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
                fn Card(
                  title@v0: String,
                  subtitle@v1: String,
                  footer@v2: String,
                ) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write(" - ")
                  write_string(v1)
                  write(" - ")
                  write_string(v2)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "Default", subtitle = "Custom", footer = "End")
                }
                -- ir (optimized) --
                page Test() {
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
                  children: Fragment = Fragment::empty(),
                ) {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {children}
                  </div>
                }

                view Test {
                  <Card title="Hello"/>
                }
            "#},
            r#"<div class="card"><h2>Hello</h2></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card(
                  title@v0: String,
                  children@v1: Fragment,
                ) -> Fragment {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_string(v0)
                  write("</h2>")
                  write_fragment(v1)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "Hello", children = {})
                }
                -- ir (optimized) --
                page Test() {
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
                  children: Fragment = Fragment::empty(),
                ) {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {children}
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
                fn Card(
                  title@v0: String,
                  children@v1: Fragment,
                ) -> Fragment {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_string(v0)
                  write("</h2>")
                  write_fragment(v1)
                  write("</div>")
                }
                page Test() {
                  call Card(title = "With", children = {
                    write("<p")
                    write(">")
                    write("body")
                    write("</p>")
                  })
                  call Card(title = "Without", children = {})
                }
                -- ir (optimized) --
                page Test() {
                  write("<div class=\"card\"><h2>With</h2><p>body</p></div>")
                  write("<div class=\"card\"><h2>Without</h2></div>")
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
                page Test() {
                  let v0 = "" in {
                    match v0.is_empty() {
                      true => {
                        write("empty")
                      }
                      false => {
                        write("not empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("empty")
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
                page Test() {
                  let v0 = "hello" in {
                    match v0.is_empty() {
                      true => {
                        write("empty")
                      }
                      false => {
                        write("not empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("not empty")
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
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0.is_some() {
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
                page Test() {
                  write("yes")
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
                page Test() {
                  let v0 = Option[String]::None in {
                    match v0.is_some() {
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
                page Test() {
                  write("no")
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
                page Test() {
                  let v0 = Option[String]::None in {
                    match v0.is_none() {
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
                page Test() {
                  write("yes")
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
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    match v0.is_none() {
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
                page Test() {
                  write("no")
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
    fn option_is_none_as_comparison_operand() {
        check(
            indoc! {r#"
                view Test {
                  <let {o: Option[Bool] = None}>
                    <if {true == o.is_none()}>
                      x
                    </if>
                  </let>
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[Bool]::None in {
                    match (true == v0.is_none()) {
                      true => {
                        write("x")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("x")
                }
                -- expected output --
                x
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
    fn string_is_empty_as_comparison_operand() {
        check(
            indoc! {r#"
                view Test {
                  <if {"a".is_empty() == "b".is_empty()}>
                    x
                  </if>
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  match ("a".is_empty() == "b".is_empty()) {
                    true => {
                      write("x")
                    }
                    false => {
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("x")
                }
                -- expected output --
                x
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
                page Test() {
                  write("hello world")
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  write("hello")
                  write(" ")
                  write("world")
                }
                -- ir (optimized) --
                page Test() {
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
                fn RenderItem(item@v0: test::Item) -> Fragment {
                  match v0 {
                    Item::Todo(label: v1, done: v2) => {
                      let v3 = v1 in {
                        let v4 = v2 in {
                          match v4 {
                            true => {
                              write("[x]")
                            }
                            false => {
                            }
                          }
                          match (!v4) {
                            true => {
                              write("[ ]")
                            }
                            false => {
                            }
                          }
                          write_string(v3)
                        }
                      }
                    }
                  }
                }
                page Test() {
                  call RenderItem(item = Item::Todo {label: "Buy milk", done: true})
                  write(",")
                  call RenderItem(item = Item::Todo {label: "Walk dog", done: false})
                }
                -- ir (optimized) --
                enum Item {
                  Todo {label: String, done: Bool},
                }
                page Test() {
                  write("[x]Buy milk,[ ]Walk dog")
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
                fn Render(time@v0: test::TimeAgo) -> Fragment {
                  match v0 {
                    TimeAgo::MinutesAgo(count: v1) => {
                      let v2 = v1 in {
                        write_string(match (v2 == 1) {
                          true => { "1 minute ago" }
                          false => { (v2.to_string() + " minutes ago") }
                        })
                      }
                    }
                    TimeAgo::HoursAgo(count: v3) => {
                      let v4 = v3 in {
                        write_string(match (v4 == 1) {
                          true => { "1 hour ago" }
                          false => { (v4.to_string() + " hours ago") }
                        })
                      }
                    }
                  }
                }
                page Test() {
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
                page Test() {
                  write("1 minute ago,5 minutes ago,1 hour ago")
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
                fn RenderCode(block@v0: test::CodeBlock) -> Fragment {
                  match v0 {
                    CodeBlock::Snippet(code: v1) => {
                      let v2 = v1 in {
                        write("<code")
                        write(">")
                        write_string(v2)
                        write("</code>")
                      }
                    }
                  }
                }
                page Test() {
                  call RenderCode(block = CodeBlock::Snippet {language: "rust", code: "fn main()"})
                }
                -- ir (optimized) --
                enum CodeBlock {
                  Snippet {language: String, code: String},
                }
                page Test() {
                  write("<code>fn main()</code>")
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
                fn Render(el@v0: test::ButtonElement) -> Fragment {
                  match v0 {
                    ButtonElement::Link(href: v1) => {
                      let v2 = v1 in {
                        write("<a")
                        write(" href=\"")
                        write_string(v2)
                        write("\"")
                        write(">")
                        write("link")
                        write("</a>")
                      }
                    }
                    ButtonElement::Button(type: v3) => {
                      let v4 = v3 in {
                        write("<button")
                        write(" type=\"")
                        write_string(v4)
                        write("\"")
                        write(">")
                        write("btn")
                        write("</button>")
                      }
                    }
                  }
                }
                page Test() {
                  call Render(el = ButtonElement::Button {disabled: false, type: "submit"})
                }
                -- ir (optimized) --
                enum ButtonElement {
                  Link {href: String},
                  Button {disabled: Bool, type: String},
                }
                page Test() {
                  write("<button type=\"submit\">btn</button>")
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
                page Test() {
                  let v0 = Option[test::Target]::Some(Target {
                    id: "1",
                    title: "hello",
                  }) in {
                    let v3 = [
                      match v0 {
                        Some(v1) => {
                          let v2 = v1 in { Option[String]::Some(v2.title) }
                        }
                        None => { Option[String]::None }
                      },
                    ] in {
                      for v4 in v3 {
                        match v4 {
                          Some(v5) => {
                            let v6 = v5 in {
                              write("[")
                              write_string(v6)
                              write("]")
                            }
                          }
                          None => {
                          }
                        }
                      }
                      match v0 {
                        Some(v7) => {
                          let v8 = v7 in {
                            write_string(v8.title)
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
                page Test() {
                  for v4 in [Option[String]::Some("hello")] {
                    match v4 {
                      Some(v5) => {
                        let v6 = v5 in {
                          write("[")
                          write_string(v6)
                          write("]")
                        }
                      }
                      None => {
                      }
                    }
                  }
                  write("hello")
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
                page Test() {
                  write("<img")
                  write(" src=\"")
                  write_string("/hop_assets/logo.svg")
                  write("\"")
                  write(">")
                }
                -- ir (optimized) --
                page Test() {
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
                page Test() {
                  write("<img")
                  write(" src=\"")
                  write_string("/static/v1/logo-a1b2c3d4.svg")
                  write("\"")
                  write(">")
                }
                -- ir (optimized) --
                page Test() {
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

    #[test]
    #[ignore]
    fn recursive_component_with_children_renders() {
        check(
            indoc! {r#"
                component Nest(
                  depth: Int,
                  children: Fragment,
                ) {
                  <match {depth > 0}>
                    <case {true}>
                      <div>
                        <Nest depth={depth - 1}>
                          {children}
                        </Nest>
                      </div>
                    </case>
                    <case {false}>
                      {children}
                    </case>
                  </match>
                }

                view Test {
                  <Nest depth={2}>
                    <b>
                      x
                    </b>
                  </Nest>
                }
            "#},
            "<div><div><b>x</b></div></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Nest(depth@v0: Int, children@v1: Fragment) -> Fragment {
                  match (0 < v0) {
                    true => {
                      write("<div")
                      write(">")
                      call Nest(depth = (v0 - 1), children = {
                        write_fragment(v1)
                      })
                      write("</div>")
                    }
                    false => {
                      write_fragment(v1)
                    }
                  }
                }
                page Test() {
                  call Nest(depth = 2, children = {
                    write("<b")
                    write(">")
                    write("x")
                    write("</b>")
                  })
                }
                -- ir (optimized) --
                fn Nest(depth@v0: Int, children@v1: Fragment) -> Fragment {
                  match (0 < v0) {
                    true => {
                      write("<div>")
                      call Nest(depth = (v0 - 1), children = {
                        write_fragment(v1)
                      })
                      write("</div>")
                    }
                    false => {
                      write_fragment(v1)
                    }
                  }
                }
                page Test() {
                  call Nest(depth = 2, children = {
                    write("<b>x</b>")
                  })
                }
                -- expected output --
                <div><div><b>x</b></div></div>
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
    fn children_can_be_bound_to_a_variable() {
        check(
            indoc! {r#"
                component Foo(children: Fragment) {
                  <let {x = children}>
                    <div>
                      {x}
                    </div>
                  </let>
                }

                view Test {
                  <Foo>
                    <b>
                      hi
                    </b>
                  </Foo>
                }
            "#},
            "<div><b>hi</b></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Foo(children@v0: Fragment) -> Fragment {
                  let v1 = v0 in {
                    write("<div")
                    write(">")
                    write_fragment(v1)
                    write("</div>")
                  }
                }
                page Test() {
                  call Foo(children = {
                    write("<b")
                    write(">")
                    write("hi")
                    write("</b>")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  let v3 = {
                    write("<b>hi</b>")
                  } in {
                    write("<div>")
                    write_fragment(v3)
                    write("</div>")
                  }
                }
                -- expected output --
                <div><b>hi</b></div>
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
    fn nested_children_forwarding() {
        check(
            indoc! {r#"
                component Inner(children: Fragment) {
                  <em>
                    {children}
                  </em>
                }

                component Outer(children: Fragment) {
                  <section>
                    <Inner>
                      {children}
                    </Inner>
                  </section>
                }

                view Test {
                  <Outer>
                    z
                  </Outer>
                }
            "#},
            "<section><em>z</em></section>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner(children@v0: Fragment) -> Fragment {
                  write("<em")
                  write(">")
                  write_fragment(v0)
                  write("</em>")
                }
                fn Outer(children@v1: Fragment) -> Fragment {
                  write("<section")
                  write(">")
                  call Inner(children = {
                    write_fragment(v1)
                  })
                  write("</section>")
                }
                page Test() {
                  call Outer(children = {
                    write("z")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<section><em>z</em></section>")
                }
                -- expected output --
                <section><em>z</em></section>
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
    fn recursive_component_carries_a_rest() {
        check(
            indoc! {r#"
                component Nest(
                  n: Int,
                  ...rest,
                ) {
                  <div ...rest>
                    <if {0 < n}>
                      <Nest n={n - 1}/>
                    </if>
                  </div>
                }

                view Test {
                  <Nest n={2} id="root"/>
                }
            "#},
            r#"<div id="root"><div><div></div></div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Nest(n@v0: Int, rest@v1: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v1)
                  write(">")
                  match (0 < v0) {
                    true => {
                      call Nest(n = (v0 - 1), rest = {})
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  call Nest(n = 2, rest = {
                    write(" id=\"root\"")
                  })
                }
                -- ir (optimized) --
                fn Nest(n@v0: Int, rest@v1: Fragment) -> Fragment {
                  write("<div")
                  write_fragment(v1)
                  write(">")
                  match (0 < v0) {
                    true => {
                      call Nest(n = (v0 - 1), rest = {})
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  call Nest(n = 2, rest = {
                    write(" id=\"root\"")
                  })
                }
                -- expected output --
                <div id="root"><div><div></div></div></div>
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
    fn recursive_component_with_int_param() {
        check(
            indoc! {r#"
                component Countdown(n: Int) {
                  {n.to_string()}
                  <if {0 < n}>
                    <Countdown n={n - 1}/>
                  </if>
                }

                view Test {
                  <Countdown n={3}/>
                }
            "#},
            "3210",
            expect![[r#"
                -- ir (unoptimized) --
                fn Countdown(n@v0: Int) -> Fragment {
                  write_string(v0.to_string())
                  match (0 < v0) {
                    true => {
                      call Countdown(n = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Countdown(n = 3)
                }
                -- ir (optimized) --
                fn Countdown(n@v0: Int) -> Fragment {
                  write_string(v0.to_string())
                  match (0 < v0) {
                    true => {
                      call Countdown(n = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Countdown(n = 3)
                }
                -- expected output --
                3210
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
    fn recursive_component_with_option_param() {
        check(
            indoc! {r#"
                component Loop(
                  n: Int,
                  label: Option[String],
                ) {
                  <match {label}>
                    <case {Some(text)}>
                      {text}
                    </case>
                    <case {None}>
                      x
                    </case>
                  </match>
                  <if {0 < n}>
                    <Loop n={n - 1} label={label}/>
                  </if>
                }

                view Test {
                  <Loop n={2} label={Some("a")}/>
                }
            "#},
            "aaa",
            expect![[r#"
                -- ir (unoptimized) --
                fn Loop(n@v0: Int, label@v1: Option[String]) -> Fragment {
                  match v1 {
                    Some(v2) => {
                      let v3 = v2 in {
                        write_string(v3)
                      }
                    }
                    None => {
                      write("x")
                    }
                  }
                  match (0 < v0) {
                    true => {
                      call Loop(n = (v0 - 1), label = v1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Loop(n = 2, label = Option[String]::Some("a"))
                }
                -- ir (optimized) --
                fn Loop(n@v0: Int, label@v1: Option[String]) -> Fragment {
                  match v1 {
                    Some(v2) => {
                      let v3 = v2 in {
                        write_string(v3)
                      }
                    }
                    None => {
                      write("x")
                    }
                  }
                  match (0 < v0) {
                    true => {
                      call Loop(n = (v0 - 1), label = v1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Loop(n = 2, label = Option[String]::Some("a"))
                }
                -- expected output --
                aaa
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
    fn recursive_component_with_option_arg_used_twice() {
        check(
            indoc! {r#"
                component C(x: Option[String]) {
                  <if {x.is_none()}>
                    <C x={x}/>
                  </if>
                }

                view Test {
                  <let {o: Option[String] = Some("a")}>
                    <C x={o}/>
                    <C x={o}/>
                  </let>
                }
            "#},
            "",
            expect![[r#"
                -- ir (unoptimized) --
                fn C(x@v1: Option[String]) -> Fragment {
                  match v1.is_none() {
                    true => {
                      call C(x = v1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v0 = Option[String]::Some("a") in {
                    call C(x = v0)
                    call C(x = v0)
                  }
                }
                -- ir (optimized) --
                fn C(x@v1: Option[String]) -> Fragment {
                  match v1.is_none() {
                    true => {
                      call C(x = v1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call C(x = Option[String]::Some("a"))
                  call C(x = Option[String]::Some("a"))
                }
                -- expected output --

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
    fn mutually_recursive_components() {
        check(
            indoc! {r#"
                component Even(n: Int) {
                  <if {n == 0}>
                    even
                  </if>
                  <if {0 < n}>
                    <Odd n={n - 1}/>
                  </if>
                }

                component Odd(n: Int) {
                  <if {n == 0}>
                    odd
                  </if>
                  <if {0 < n}>
                    <Even n={n - 1}/>
                  </if>
                }

                view Test {
                  <Even n={4}/>
                }
            "#},
            "even",
            expect![[r#"
                -- ir (unoptimized) --
                fn Even(n@v0: Int) -> Fragment {
                  match (v0 == 0) {
                    true => {
                      write("even")
                    }
                    false => {
                    }
                  }
                  match (0 < v0) {
                    true => {
                      call Odd(n = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                fn Odd(n@v1: Int) -> Fragment {
                  match (v1 == 0) {
                    true => {
                      write("odd")
                    }
                    false => {
                    }
                  }
                  match (0 < v1) {
                    true => {
                      call Even(n = (v1 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Even(n = 4)
                }
                -- ir (optimized) --
                fn Even(n@v0: Int) -> Fragment {
                  match (v0 == 0) {
                    true => {
                      write("even")
                    }
                    false => {
                    }
                  }
                  match (0 < v0) {
                    true => {
                      call Odd(n = (v0 - 1))
                    }
                    false => {
                    }
                  }
                }
                fn Odd(n@v1: Int) -> Fragment {
                  match (v1 == 0) {
                    true => {
                      write("odd")
                    }
                    false => {
                    }
                  }
                  match (0 < v1) {
                    true => {
                      call Even(n = (v1 - 1))
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Even(n = 4)
                }
                -- expected output --
                even
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
    fn field_access_on_record_literal_in_if_condition() {
        check(
            indoc! {r#"
                record R {
                  f: Bool,
                }

                view Test {
                  <if {R {f: true}.f}>
                    x
                  </if>
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                record R {
                  f: Bool,
                }
                page Test() {
                  match R {f: true}.f {
                    true => {
                      write("x")
                    }
                    false => {
                    }
                  }
                }
                -- ir (optimized) --
                record R {
                  f: Bool,
                }
                page Test() {
                  write("x")
                }
                -- expected output --
                x
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
    fn recursive_component_with_empty_array_arg() {
        check(
            indoc! {r#"
                component C(p: Array[String]) {
                  <for {_ in p}>
                    <C p={[]}/>
                  </for>
                }

                view Test {
                  <C p={["a"]}/>
                }
            "#},
            "",
            expect![[r#"
                -- ir (unoptimized) --
                fn C(p@v0: Array[String]) -> Fragment {
                  for _ in v0 {
                    call C(p = [])
                  }
                }
                page Test() {
                  call C(p = ["a"])
                }
                -- ir (optimized) --
                fn C(p@v0: Array[String]) -> Fragment {
                  for _ in v0 {
                    call C(p = [])
                  }
                }
                page Test() {
                  call C(p = ["a"])
                }
                -- expected output --

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
    fn field_access_on_record_literal_from_arg_in_if_condition() {
        check(
            indoc! {r#"
                record R {
                  f: Array[String],
                }

                component C(p: Array[String]) {
                  <if {R {f: p}.f.is_empty()}>
                    <C p={[]}/>
                  </if>
                }

                view Test {
                  <C p={["a"]}/>
                }
            "#},
            "",
            expect![[r#"
                -- ir (unoptimized) --
                record R {
                  f: Array[String],
                }
                fn C(p@v0: Array[String]) -> Fragment {
                  match R {f: v0}.f.is_empty() {
                    true => {
                      call C(p = [])
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call C(p = ["a"])
                }
                -- ir (optimized) --
                record R {
                  f: Array[String],
                }
                fn C(p@v0: Array[String]) -> Fragment {
                  match v0.is_empty() {
                    true => {
                      call C(p = [])
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call C(p = ["a"])
                }
                -- expected output --

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
    fn option_bool_match_in_component() {
        check(
            indoc! {r#"
                pub component OptBool(checked: Option[Bool]) {
                  <match {checked}>
                    <case {Some(true)}>
                      <span>
                        yes
                      </span>
                    </case>
                    <case {Some(false)}>
                      <span>
                        no
                      </span>
                    </case>
                    <case {None}>
                    </case>
                  </match>
                }

                view Test {
                  <OptBool checked={Some(true)}/>
                }
            "#},
            "<span>yes</span>",
            expect![[r#"
                -- ir (unoptimized) --
                fn OptBool(checked@v0: Option[Bool]) -> Fragment {
                  match v0 {
                    Some(v1) => {
                      match v1 {
                        true => {
                          write("<span")
                          write(">")
                          write("yes")
                          write("</span>")
                        }
                        false => {
                          write("<span")
                          write(">")
                          write("no")
                          write("</span>")
                        }
                      }
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  call OptBool(checked = Option[Bool]::Some(true))
                }
                -- ir (optimized) --
                page Test() {
                  write("<span>yes</span>")
                }
                -- expected output --
                <span>yes</span>
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
    fn nested_option_bool_literal_patterns() {
        check(
            indoc! {r#"
                view Test {
                  <let {x: Option[Option[Bool]] = Some(Some(true))}>
                    <match {x}>
                      <case {Some(Some(true))}>
                        tt
                      </case>
                      <case {Some(Some(false))}>
                        tf
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
            "tt",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[Option[Bool]]::Some(Option[Bool]::Some(true)) in {
                    match v0 {
                      Some(v1) => {
                        match v1 {
                          Some(v2) => {
                            match v2 {
                              true => {
                                write("tt")
                              }
                              false => {
                                write("tf")
                              }
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
                page Test() {
                  write("tt")
                }
                -- expected output --
                tt
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
    fn for_loop_int_comparison() {
        check(
            indoc! {r#"
                view Test {
                  <for {n in [1, 2, 3]}>
                    <if {n > 1}>
                      {n.to_string()}
                    </if>
                  </for>
                }
            "#},
            "23",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [1, 2, 3] {
                    match (1 < v0) {
                      true => {
                        write_string(v0.to_string())
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [1, 2, 3] {
                    match (1 < v0) {
                      true => {
                        write_string(v0.to_string())
                      }
                      false => {
                      }
                    }
                  }
                }
                -- expected output --
                23
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
    fn for_loop_string_equality() {
        check(
            indoc! {r#"
                view Test {
                  <for {s in ["a", "b"]}>
                    <if {s == "a"}>
                      {s}
                    </if>
                  </for>
                }
            "#},
            "a",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    match (v0 == "a") {
                      true => {
                        write_string(v0)
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    match (v0 == "a") {
                      true => {
                        write_string(v0)
                      }
                      false => {
                      }
                    }
                  }
                }
                -- expected output --
                a
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
    fn for_loop_float_comparison() {
        check(
            indoc! {r#"
                view Test {
                  <for {f in [1.5, 2.5]}>
                    <if {f > 2.0}>
                      big
                    </if>
                  </for>
                }
            "#},
            "big",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [1.5, 2.5] {
                    match (2 < v0) {
                      true => {
                        write("big")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [1.5, 2.5] {
                    match (2 < v0) {
                      true => {
                        write("big")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- expected output --
                big
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
    fn for_loop_bool_logical_and() {
        check(
            indoc! {r#"
                view Test {
                  <for {flag in [true, false]}>
                    <if {flag && true}>
                      x
                    </if>
                  </for>
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [true, false] {
                    match (v0 && true) {
                      true => {
                        write("x")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [true, false] {
                    match (v0 && true) {
                      true => {
                        write("x")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- expected output --
                x
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
    fn for_loop_string_used_by_value_and_by_ref() {
        check(
            indoc! {r#"
                pub component Show(label: String) {
                  <span>
                    {label}
                  </span>
                }

                view Test {
                  <for {s in ["a", "b"]}>
                    <if {s == "a"}>
                      <Show label={s}/>
                    </if>
                  </for>
                }
            "#},
            "<span>a</span>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Show(label@v1: String) -> Fragment {
                  write("<span")
                  write(">")
                  write_string(v1)
                  write("</span>")
                }
                page Test() {
                  for v0 in ["a", "b"] {
                    match (v0 == "a") {
                      true => {
                        call Show(label = v0)
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    match (v0 == "a") {
                      true => {
                        write("<span>")
                        write_string(v0)
                        write("</span>")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- expected output --
                <span>a</span>
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
    fn record_field_named_class() {
        check(
            indoc! {r#"
                record Foo {
                  class: String,
                }

                view Test {
                  <let {foo: Foo = Foo {class: "a"}}>
                    <div>
                      {foo.class}
                    </div>
                  </let>
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  class: String,
                }
                page Test() {
                  let v0 = Foo {class: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.class)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
                record Foo {
                  class: String,
                }
                page Test() {
                  write("<div>a</div>")
                }
                -- expected output --
                <div>a</div>
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
    fn record_field_named_function() {
        check(
            indoc! {r#"
                record Foo {
                  function: String,
                }

                view Test {
                  <let {f: Foo = Foo {function: "a"}}>
                    <div>
                      {f.function}
                    </div>
                  </let>
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  function: String,
                }
                page Test() {
                  let v0 = Foo {function: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.function)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
                record Foo {
                  function: String,
                }
                page Test() {
                  write("<div>a</div>")
                }
                -- expected output --
                <div>a</div>
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
    fn record_field_named_protected() {
        check(
            indoc! {r#"
                record Foo {
                  protected: String,
                }

                view Test {
                  <let {f: Foo = Foo {protected: "a"}}>
                    <div>
                      {f.protected}
                    </div>
                  </let>
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  protected: String,
                }
                page Test() {
                  let v0 = Foo {protected: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.protected)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
                record Foo {
                  protected: String,
                }
                page Test() {
                  write("<div>a</div>")
                }
                -- expected output --
                <div>a</div>
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
    fn record_field_named_eval() {
        check(
            indoc! {r#"
                record Foo {
                  eval: String,
                }

                view Test {
                  <let {f: Foo = Foo {eval: "a"}}>
                    <div>
                      {f.eval}
                    </div>
                  </let>
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  eval: String,
                }
                page Test() {
                  let v0 = Foo {eval: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.eval)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
                record Foo {
                  eval: String,
                }
                page Test() {
                  write("<div>a</div>")
                }
                -- expected output --
                <div>a</div>
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
    fn enum_payload_field_named_class() {
        check(
            indoc! {r#"
                enum E {
                  A {
                    class: String,
                  },
                }

                view Test {
                  <let {e: E = E::A {class: "a"}}>
                    <match {e}>
                      <case {E::A {class: v}}>
                        <div>
                          {v}
                        </div>
                      </case>
                    </match>
                  </let>
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                enum E {
                  A {class: String},
                }
                page Test() {
                  let v0 = E::A {class: "a"} in {
                    match v0 {
                      E::A(class: v1) => {
                        let v2 = v1 in {
                          write("<div")
                          write(">")
                          write_string(v2)
                          write("</div>")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                enum E {
                  A {class: String},
                }
                page Test() {
                  write("<div>a</div>")
                }
                -- expected output --
                <div>a</div>
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
    fn record_named_math_does_not_shadow_the_js_math_global() {
        check(
            indoc! {r#"
                record Math {
                  x: Int,
                }

                view Test {
                  <let {m: Math = Math {x: 4}}>
                    <let {b: Int = 5}>
                      {(m.x * b).to_string()}
                    </let>
                  </let>
                }
            "#},
            r#"20"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Math {
                  x: Int,
                }
                page Test() {
                  let v0 = Math {x: 4} in {
                    let v1 = 5 in {
                      write_string((v0.x * v1).to_string())
                    }
                  }
                }
                -- ir (optimized) --
                record Math {
                  x: Int,
                }
                page Test() {
                  write("20")
                }
                -- expected output --
                20
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
    fn record_named_number_does_not_shadow_the_js_number_global() {
        check(
            indoc! {r#"
                record Number {
                  x: Float,
                }

                view Test {
                  <let {n: Number = Number {x: 3.7}}>
                    {n.x.to_int().to_string()}
                  </let>
                }
            "#},
            r#"3"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Number {
                  x: Float,
                }
                page Test() {
                  let v0 = Number {x: 3.7} in {
                    write_string(v0.x.to_int().to_string())
                  }
                }
                -- ir (optimized) --
                record Number {
                  x: Float,
                }
                page Test() {
                  write("3")
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
    fn record_spread_fills_fields_from_subject() {
        check(
            indoc! {r#"
                record State {
                  query: String,
                  num: Int,
                }

                view Test {
                  <let {base = State {query: "a", num: 1}}>
                    <let {next = State {...base, num: 2}}>
                      {next.query}
                      {next.num.to_string()}
                    </let>
                  </let>
                }
            "#},
            r#"a 2"#,
            expect![[r#"
                -- ir (unoptimized) --
                record State {
                  query: String,
                  num: Int,
                }
                page Test() {
                  let v0 = State {query: "a", num: 1} in {
                    let v1 = State {query: v0.query, num: 2} in {
                      write_string(v1.query)
                      write(" ")
                      write_string(v1.num.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                record State {
                  query: String,
                  num: Int,
                }
                page Test() {
                  write("a 2")
                }
                -- expected output --
                a 2
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
    fn record_spread_with_all_fields_overridden() {
        check(
            indoc! {r#"
                record State {
                  query: String,
                  num: Int,
                }

                view Test {
                  <let {base = State {query: "a", num: 1}}>
                    <let {next = State {...base, query: "b", num: 2}}>
                      {next.query}
                      {next.num.to_string()}
                    </let>
                  </let>
                }
            "#},
            r#"b 2"#,
            expect![[r#"
                -- ir (unoptimized) --
                record State {
                  query: String,
                  num: Int,
                }
                page Test() {
                  let v0 = State {query: "a", num: 1} in {
                    let v1 = State {query: "b", num: 2} in {
                      write_string(v1.query)
                      write(" ")
                      write_string(v1.num.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                record State {
                  query: String,
                  num: Int,
                }
                page Test() {
                  write("b 2")
                }
                -- expected output --
                b 2
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
    fn record_spread_field_access_folds_through_literal() {
        check(
            indoc! {r#"
                record State {
                  query: String,
                  num: Int,
                }

                view Test {
                  <for {s in [State {query: "a", num: 7}]}>
                    {State {...s, query: "x"}.query}
                    {State {...s, query: "x"}.num.to_string()}
                  </for>
                }
            "#},
            r#"x 7"#,
            expect![[r#"
                -- ir (unoptimized) --
                record State {
                  query: String,
                  num: Int,
                }
                page Test() {
                  for v0 in [State {query: "a", num: 7}] {
                    write_string(State {query: "x", num: v0.num}.query)
                    write(" ")
                    write_string(State {
                      query: "x",
                      num: v0.num,
                    }.num.to_string())
                  }
                }
                -- ir (optimized) --
                record State {
                  query: String,
                  num: Int,
                }
                page Test() {
                  for v0 in [State {query: "a", num: 7}] {
                    write("x ")
                    write_string(v0.num.to_string())
                  }
                }
                -- expected output --
                x 7
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
    fn record_spread_in_match_arm_passed_as_component_prop() {
        check(
            indoc! {r#"
                record Item {
                  label: String,
                  selected: Bool,
                }

                component Row(item: Item) {
                  <div>
                    {item.label}
                  </div>
                }

                view Test {
                  <for {item in [Item {label: "a", selected: false}]}>
                    <match {item.selected}>
                      <case {true}>
                        <Row item={Item {...item, label: "on"}}/>
                      </case>
                      <case {false}>
                        <Row item={Item {...item, label: "off"}}/>
                      </case>
                    </match>
                  </for>
                }
            "#},
            r#"<div>off</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Item {
                  label: String,
                  selected: Bool,
                }
                fn Row(item@v1: test::Item) -> Fragment {
                  write("<div")
                  write(">")
                  write_string(v1.label)
                  write("</div>")
                }
                page Test() {
                  for v0 in [Item {label: "a", selected: false}] {
                    match v0.selected {
                      true => {
                        call Row(item = Item {
                          label: "on",
                          selected: v0.selected,
                        })
                      }
                      false => {
                        call Row(item = Item {
                          label: "off",
                          selected: v0.selected,
                        })
                      }
                    }
                  }
                }
                -- ir (optimized) --
                record Item {
                  label: String,
                  selected: Bool,
                }
                page Test() {
                  for v0 in [Item {label: "a", selected: false}] {
                    match v0.selected {
                      true => {
                        write("<div>on</div>")
                      }
                      false => {
                        write("<div>off</div>")
                      }
                    }
                  }
                }
                -- expected output --
                <div>off</div>
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
    fn record_spread_nested_update() {
        check(
            indoc! {r#"
                record Settings {
                  theme: String,
                  compact: Bool,
                }

                record State {
                  query: String,
                  settings: Settings,
                }

                component Dark(s: State) {
                  <let {t = Settings {...s.settings, theme: "dark"}}>
                    <let {next = State {...s, settings: t}}>
                      {next.query}
                      {next.settings.theme}
                    </let>
                  </let>
                }

                view Test {
                  <let {s = Settings {theme: "light", compact: true}}>
                    <Dark s={State {query: "q", settings: s}}/>
                  </let>
                }
            "#},
            r#"q dark"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Settings {
                  theme: String,
                  compact: Bool,
                }
                record State {
                  query: String,
                  settings: Settings,
                }
                fn Dark(s@v1: test::State) -> Fragment {
                  let v3 = let v2 = v1.settings in {
                    Settings {theme: "dark", compact: v2.compact}
                  } in {
                    let v4 = State {query: v1.query, settings: v3} in {
                      write_string(v4.query)
                      write(" ")
                      write_string(v4.settings.theme)
                    }
                  }
                }
                page Test() {
                  let v0 = Settings {theme: "light", compact: true} in {
                    call Dark(s = State {query: "q", settings: v0})
                  }
                }
                -- ir (optimized) --
                record Settings {
                  theme: String,
                  compact: Bool,
                }
                record State {
                  query: String,
                  settings: Settings,
                }
                page Test() {
                  write("q dark")
                }
                -- expected output --
                q dark
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
    fn record_spread_of_record_literal_subject() {
        check(
            indoc! {r#"
                record Foo {
                  x: String,
                  y: String,
                }

                view Test {
                  {Foo {...Foo {x: "bar", y: "baz"}, y: "foo"}.x}
                  {Foo {...Foo {x: "bar", y: "baz"}, y: "foo"}.y}
                }
            "#},
            r#"bar foo"#,
            expect![[r#"
                -- ir (unoptimized) --
                record Foo {
                  x: String,
                  y: String,
                }
                page Test() {
                  write_string(let v0 = Foo {x: "bar", y: "baz"} in {
                    Foo {x: v0.x, y: "foo"}
                  }.x)
                  write(" ")
                  write_string(let v1 = Foo {x: "bar", y: "baz"} in {
                    Foo {x: v1.x, y: "foo"}
                  }.y)
                }
                -- ir (optimized) --
                record Foo {
                  x: String,
                  y: String,
                }
                page Test() {
                  write("bar foo")
                }
                -- expected output --
                bar foo
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
    fn function_called_in_range_bound_and_interpolation() {
        check(
            indoc! {r#"
                fn foo(x: Int) -> Int {
                  x + 10
                }

                component Wrapper {
                  <div>
                    <for {x in 0..=foo(-7)}>
                      {x.to_string()},
                    </for>
                    {foo(10).to_string()}
                  </div>
                }

                view Test {
                  <Wrapper/>
                }
            "#},
            "<div>0,1,2,3,20</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper() -> Fragment {
                  write("<div")
                  write(">")
                  for v0 in 0..=call foo(x = (-7)) {
                    write_string(v0.to_string())
                    write(",")
                  }
                  write_string(call foo(x = 10).to_string())
                  write("</div>")
                }
                fn foo(x@v1: Int) -> Int {
                  (v1 + 10)
                }
                page Test() {
                  call Wrapper()
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>")
                  for v4 in 0..=3 {
                    write_string(v4.to_string())
                    write(",")
                  }
                  write("20</div>")
                }
                -- expected output --
                <div>0,1,2,3,20</div>
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
