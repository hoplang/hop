use crate::asset_rewriter::AssetRewriter;
use crate::document::Document;
use crate::document_annotator::DocumentAnnotator;
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
use txtar::Archive;

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
    let page_name = TypeName::parse("Test").unwrap();
    evaluator::evaluate_page(module, &page_name, HashMap::new())
        .map_err(|e| format!("Evaluator failed: {}", e))
}

fn check(archive: &str, expected_output: &str, expected: Expect) {
    check_with_asset_rewriter(archive, None, expected_output, expected);
}

fn check_with_asset_rewriter(
    archive: &str,
    asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    expected_output: &str,
    expected: Expect,
) {
    let archive = Archive::from(archive);
    let mut program = Program::default();
    let mut modules = 0;
    for file in archive.iter() {
        assert!(
            file.name.ends_with(".hop"),
            "expected a .hop module, got '{}'",
            file.name
        );
        let document_id = DocumentId::new(&file.name).unwrap();
        let document = Document::new(document_id.clone(), file.content.clone());
        program.update_module(&document_id, document);
        modules += 1;
    }
    assert!(modules > 0, "archive declares no modules");

    let diagnostics = program.diagnostics();
    if !diagnostics.is_empty() {
        let rendered = DocumentAnnotator::new()
            .with_severity_label()
            .with_lines_before(1)
            .annotate(diagnostics)
            .render();
        eprintln!("{}", rendered);
        panic!("Diagnostics found");
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
            let page_name = TypeName::parse("Test").unwrap();
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
            let page_name = TypeName::parse("Test").unwrap();
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
                -- main.hop --
                record Flag {
                  value: Bool,
                }

                page Test() {
                  fn body() -> Html {
                    for f in [Flag {value: true}] {
                      match f {
                        Flag {value: b} => {
                          match b || false {
                            true => <>yes</>,
                            false => <></>,
                          }
                        },
                      }
                    }
                  }
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v2 = v1 in {
                        let v3 = (v2 || false) in {
                          match v3 {
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
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      let v3 = (v1 || false) in {
                        match v3 {
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
    fn let_statements_in_body_arm_and_interpolation() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let first = "foo";
                    let title: Option[String] = Some("bar");
                    let prefix = match title {
                      Some(t) => {
                        let spaced = t + " ";
                        spaced
                      },
                      None => "",
                    };
                    <p>{ let name = prefix + first; name }</p>
                  }
                }
            "#},
            "<p>bar foo</p>",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "foo" in {
                    let v1 = Option[String]::Some("bar") in {
                      let v5 = match v1 {
                        Some(v2) => {
                          let v3 = v2 in { let v4 = (v3 + " ") in { v4 } }
                        }
                        None => { "" }
                      } in {
                        write("<p")
                        write(">")
                        write_string(let v6 = (v5 + v0) in { v6 })
                        write("</p>")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("<p>bar foo</p>")
                }
                -- expected output --
                <p>bar foo</p>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn let_in_interpolation_with_markup_tail() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <ul>
                      {
                        let label = "Item";
                        let count = 2;
                        <li class={ let base = "row"; base + "-" + "odd" }>{label}: {count.to_string()}</li>
                      }
                    </ul>
                  }
                }
            "#},
            "<ul><li class=\"row-odd\">Item: 2</li></ul>",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("<ul")
                  write(">")
                  let v0 = "Item" in {
                    let v1 = 2 in {
                      write("<li")
                      write(" class=\"")
                      write_string(let v2 = "row" in {
                        ((v2 + "-") + "odd")
                      })
                      write("\"")
                      write(">")
                      write_string(v0)
                      write(": ")
                      write_string(v1.to_string())
                      write("</li>")
                    }
                  }
                  write("</ul>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<ul><li class=\"row-odd\">Item: 2</li></ul>")
                }
                -- expected output --
                <ul><li class="row-odd">Item: 2</li></ul>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                record Count {
                  n: Int,
                }

                page Test() {
                  fn body() -> Html {
                    for c in [Count {n: 57}] {
                      match c {
                        Count {n: v} => {
                          match v == 57 {
                            true => <>eq</>,
                            false => <></>,
                          }
                        },
                      }
                    }
                  }
                }
            "#},
            "eq",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [Count {n: 57}] {
                    let v1 = v0.n in {
                      let v2 = v1 in {
                        let v3 = (v2 == 57) in {
                          match v3 {
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
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Count {n: 57}] {
                    let v1 = v0.n in {
                      let v3 = (v1 == 57) in {
                        match v3 {
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
                -- main.hop --
                record Flag {
                  value: Bool,
                }

                page Test() {
                  fn body() -> Html {
                    for f in [Flag {value: true}] {
                      match f {
                        Flag {value: b} => <>{match b {true => "yes", false => "no"}}</>,
                      }
                    }
                  }
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
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
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v1 = v0.value in {
                      write_string(match v1 {
                        true => { "yes" }
                        false => { "no" }
                      })
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
                -- main.hop --
                fn Button(
                  label: String,
                  ...rest,
                ) -> Html {
                  <button class="btn" ...rest>
                    {label}
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <Button label="Hi" id="submit"/>
                  }
                }
            "#},
            r#"<button class="btn" id="submit">Hi</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f0(label@v0: String, rest@v1: Html) -> Html {
                  write("<button")
                  write(" class=\"btn\"")
                  write_html(v1)
                  write(">")
                  write_string(v0)
                  write("</button>")
                }
                page Test() {
                  call Button@f0(label = "Hi", rest = {
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
                -- main.hop --
                fn Leaf(title: String = "d") -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn First(
                  n: Int,
                  ...rest,
                ) -> Html {
                  <Second n={n} ...rest/>
                }

                fn Second(
                  n: Int,
                  ...rest,
                ) -> Html {
                  <>
                    <Leaf ...rest/>
                    {match 0 < n {
                      true => <First n={n - 1}/>,
                      false => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <First n={1} title="x"/>
                  }
                }
            "#},
            r#"<div>x</div><div>d</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn First@f0(
                  n@v0: Int,
                  title@v1: String,
                  rest@v2: Html,
                ) -> Html {
                  call Second@f2(n = v0, title = v1, rest = {
                    write_html(v2)
                  })
                }
                fn Leaf@f1(title@v3: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v3)
                  write("</div>")
                }
                fn Second@f2(
                  n@v4: Int,
                  title@v5: String,
                  rest@v6: Html,
                ) -> Html {
                  call Leaf@f1(title = v5)
                  let v7 = (0 < v4) in {
                    match v7 {
                      true => {
                        call First@f0(n = (v4 - 1), title = "d", rest = {})
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call First@f0(n = 1, title = "x", rest = {})
                }
                -- ir (optimized) --
                fn First@f0(
                  n@v0: Int,
                  title@v1: String,
                  rest@v2: Html,
                ) -> Html {
                  call Second@f2(n = v0, title = v1, rest = {
                    write_html(v2)
                  })
                }
                fn Second@f2(
                  n@v4: Int,
                  title@v5: String,
                  rest@v6: Html,
                ) -> Html {
                  write("<div>")
                  write_string(v5)
                  write("</div>")
                  let v7 = (0 < v4) in {
                    match v7 {
                      true => {
                        call First@f0(n = (v4 - 1), title = "d", rest = {})
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call First@f0(n = 1, title = "x", rest = {})
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
    fn rest_chains_through_a_function_to_an_element() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Base(...rest) -> Html {
                  <div ...rest>
                  </div>
                }

                fn Card(
                  title: String,
                  ...rest,
                ) -> Html {
                  <section>
                    <h1>
                      {title}
                    </h1>
                    <Base ...rest/>
                  </section>
                }

                page Test() {
                  fn body() -> Html {
                    <Card title="Hi" id="x" data-k="v"/>
                  }
                }
            "#},
            r#"<section><h1>Hi</h1><div id="x" data-k="v"></div></section>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Base@f0(rest@v0: Html) -> Html {
                  write("<div")
                  write_html(v0)
                  write(">")
                  write("</div>")
                }
                fn Card@f1(title@v1: String, rest@v2: Html) -> Html {
                  write("<section")
                  write(">")
                  write("<h1")
                  write(">")
                  write_string(v1)
                  write("</h1>")
                  call Base@f0(rest = {
                    write_html(v2)
                  })
                  write("</section>")
                }
                page Test() {
                  call Card@f1(title = "Hi", rest = {
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
                -- main.hop --
                fn Wrapper(
                  show: Bool,
                  ...rest,
                ) -> Html {
                  match show {
                    true => {
                      <div ...rest>
                      </div>
                    },
                    false => <></>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper show={true} id="x"/>
                  }
                }
            "#},
            r#"<div id="x"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper@f0(show@v0: Bool, rest@v1: Html) -> Html {
                  match v0 {
                    true => {
                      write("<div")
                      write_html(v1)
                      write(">")
                      write("</div>")
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Wrapper@f0(show = true, rest = {
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
                -- main.hop --
                fn Button(...rest) -> Html {
                  <button ...rest>
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <Button disabled/>
                  }
                }
            "#},
            r#"<button disabled></button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f0(rest@v0: Html) -> Html {
                  write("<button")
                  write_html(v0)
                  write(">")
                  write("</button>")
                }
                page Test() {
                  call Button@f0(rest = {
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
                -- main.hop --
                fn Wrapper(
                  show: Bool,
                  ...rest,
                ) -> Html {
                  match show {
                    true => {
                      <div ...rest>
                      </div>
                    },
                    false => <></>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper show={true} id="x"/>
                  }
                }
            "#},
            r#"<div id="x"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper@f0(show@v0: Bool, rest@v1: Html) -> Html {
                  match v0 {
                    true => {
                      write("<div")
                      write_html(v1)
                      write(">")
                      write("</div>")
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  call Wrapper@f0(show = true, rest = {
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
                -- main.hop --
                fn Panel(...rest) -> Html {
                  <div ...rest>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Panel title={"a'b<c&d"}/>
                  }
                }
            "#},
            r#"<div title="a&#39;b&lt;c&amp;d"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Panel@f0(rest@v0: Html) -> Html {
                  write("<div")
                  write_html(v0)
                  write(">")
                  write("</div>")
                }
                page Test() {
                  call Panel@f0(rest = {
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
                -- main.hop --
                fn Icon(...rest) -> Html {
                  <img ...rest>
                }

                page Test() {
                  fn body() -> Html {
                    <Icon src="a.png" alt="a"/>
                  }
                }
            "#},
            r#"<img src="a.png" alt="a">"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Icon@f0(rest@v0: Html) -> Html {
                  write("<img")
                  write_html(v0)
                  write(">")
                }
                page Test() {
                  call Icon@f0(rest = {
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
                -- main.hop --
                fn A(...rest) -> Html {
                  <div ...rest>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <A/>
                  }
                }
            "#},
            r#"<div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0(rest@v0: Html) -> Html {
                  write("<div")
                  write_html(v0)
                  write(">")
                  write("</div>")
                }
                page Test() {
                  call A@f0(rest = {})
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
                -- main.hop --
                fn Button(
                  class: String,
                  children: Html,
                  ...rest,
                ) -> Html {
                  <button class={class} ...rest>
                    {children}
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <Button class="p-2" data-foo="bar">
                      Hi
                    </Button>
                  }
                }
            "#},
            r#"<button class="p-2" data-foo="bar">Hi</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f0(
                  class@v0: String,
                  children@v1: Html,
                  rest@v2: Html,
                ) -> Html {
                  write("<button")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_html(v2)
                  write(">")
                  write_html(v1)
                  write("</button>")
                }
                page Test() {
                  call Button@f0(class = "p-2", children = {
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
                -- main.hop --
                fn Button(
                  children: Html,
                  ...rest,
                ) -> Html {
                  <button class="builtin" ...rest>
                    {children}
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <Button data-x="y">
                      Hi
                    </Button>
                  }
                }
            "#},
            r#"<button class="builtin" data-x="y">Hi</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f0(children@v0: Html, rest@v1: Html) -> Html {
                  write("<button")
                  write(" class=\"builtin\"")
                  write_html(v1)
                  write(">")
                  write_html(v0)
                  write("</button>")
                }
                page Test() {
                  call Button@f0(children = {
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
                -- main.hop --
                fn Svg(...rest) -> Html {
                  <svg ...rest>
                  </svg>
                }

                page Test() {
                  fn body() -> Html {
                    <Svg viewBox="0 0 100 100"/>
                  }
                }
            "#},
            r#"<svg viewBox="0 0 100 100"></svg>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Svg@f0(rest@v0: Html) -> Html {
                  write("<svg")
                  write_html(v0)
                  write(">")
                  write("</svg>")
                }
                page Test() {
                  call Svg@f0(rest = {
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
                -- main.hop --
                fn Card(title: String) -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  <Card ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper title="hi"/>
                  }
                }
            "#},
            r#"<div>hi</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                fn Wrapper@f1(title@v1: String, rest@v2: Html) -> Html {
                  call Card@f0(title = v1)
                }
                page Test() {
                  call Wrapper@f1(title = "hi", rest = {})
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
                -- main.hop --
                fn Card(title: String) -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  <Card title="explicit" ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper/>
                  }
                }
            "#},
            r#"<div>explicit</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                fn Wrapper@f1(rest@v1: Html) -> Html {
                  call Card@f0(title = "explicit")
                }
                page Test() {
                  call Wrapper@f1(rest = {})
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
                -- main.hop --
                record User {
                  name: String,
                }

                fn Card(user: User) -> Html {
                  <div>
                    {user.name}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  <Card ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    let user: User = User {name: "Ada"};
                    <Wrapper user={user}/>
                  }
                }
            "#},
            r#"<div>Ada</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(user@v1: main::User) -> Html {
                  write("<div")
                  write(">")
                  write_string(v1.name)
                  write("</div>")
                }
                fn Wrapper@f1(user@v2: main::User, rest@v3: Html) -> Html {
                  call Card@f0(user = v2)
                }
                page Test() {
                  let v0 = User {name: "Ada"} in {
                    call Wrapper@f1(user = v0, rest = {})
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Flag {
                  value: String,
                }

                page Test() {
                  fn body() -> Html {
                    let v_1: String = "outer";
                    for f in [Flag {value: "x"}] {
                      match f {
                        Flag {value: b} => {
                          <>
                            {v_1}
                            {b}
                          </>
                        },
                      }
                    }
                  }
                }
            "#},
            r#"outerx"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "outer" in {
                    for v1 in [Flag {value: "x"}] {
                      let v2 = v1.value in {
                        let v3 = v2 in {
                          write_string(v0)
                          write_string(v3)
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v1 in [Flag {value: "x"}] {
                    let v2 = v1.value in {
                      write("outer")
                      write_string(v2)
                    }
                  }
                }
                -- expected output --
                outerx
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                fn Card(title: String) -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn Bar(
                  name: String,
                  ...rest,
                ) -> Html {
                  <div>
                    {name}
                    <Card ...rest/>
                  </div>
                }

                fn Baz(...rest) -> Html {
                  <Bar ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Baz name="n" title="t"/>
                  }
                }
            "#},
            r#"<div>n<div>t</div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Bar@f0(
                  name@v0: String,
                  title@v1: String,
                  rest@v2: Html,
                ) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  call Card@f2(title = v1)
                  write("</div>")
                }
                fn Baz@f1(
                  name@v3: String,
                  title@v4: String,
                  rest@v5: Html,
                ) -> Html {
                  call Bar@f0(name = v3, title = v4, rest = {
                    write_html(v5)
                  })
                }
                fn Card@f2(title@v6: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v6)
                  write("</div>")
                }
                page Test() {
                  call Baz@f1(name = "n", title = "t", rest = {})
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
                -- main.hop --
                fn Card(count: Int) -> Html {
                  match count > 0 {
                    true => {
                      <div>
                        positive
                      </div>
                    },
                    false => <></>,
                  }
                }

                fn Wrapper(...rest) -> Html {
                  <Card ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper count={3}/>
                  }
                }
            "#},
            r#"<div>positive</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(count@v0: Int) -> Html {
                  let v1 = (0 < v0) in {
                    match v1 {
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
                }
                fn Wrapper@f1(count@v2: Int, rest@v3: Html) -> Html {
                  call Card@f0(count = v2)
                }
                page Test() {
                  call Wrapper@f1(count = 3, rest = {})
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
                -- main.hop --
                fn A(
                  count: Int,
                  ...rest,
                ) -> Html {
                  <div ...rest>
                    {match count > 0 {
                      true => <>positive</>,
                      false => <></>,
                    }}
                  </div>
                }

                fn B(...rest) -> Html {
                  <A ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <B count={3} data-foo="bar"/>
                  }
                }
            "#},
            r#"<div data-foo="bar">positive</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0(count@v0: Int, rest@v1: Html) -> Html {
                  write("<div")
                  write_html(v1)
                  write(">")
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        write("positive")
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                fn B@f1(count@v3: Int, rest@v4: Html) -> Html {
                  call A@f0(count = v3, rest = {
                    write_html(v4)
                  })
                }
                page Test() {
                  call B@f1(count = 3, rest = {
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
                -- main.hop --
                fn Foo(children: Html) -> Html {
                  <div>
                    {children}
                  </div>
                }

                fn Bar(...rest) -> Html {
                  <Foo ...rest/>
                }

                fn Baz(...rest) -> Html {
                  <Bar ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Baz>
                      deep
                    </Baz>
                  }
                }
            "#},
            r#"<div>deep</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Bar@f0(children@v0: Html, rest@v1: Html) -> Html {
                  call Foo@f2(children = v0)
                }
                fn Baz@f1(children@v2: Html, rest@v3: Html) -> Html {
                  call Bar@f0(children = v2, rest = {
                    write_html(v3)
                  })
                }
                fn Foo@f2(children@v4: Html) -> Html {
                  write("<div")
                  write(">")
                  write_html(v4)
                  write("</div>")
                }
                page Test() {
                  call Baz@f1(children = {
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
                -- main.hop --
                fn Inner(
                  class: String = "x",
                  ...rest,
                ) -> Html {
                  <span class={class} ...rest>
                  </span>
                }

                fn Outer(
                  class: String,
                  ...rest,
                ) -> Html {
                  <div class={class}>
                    <Inner ...rest/>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Outer class="x"/>
                  }
                }
            "#},
            r#"<div class="x"><span class="x"></span></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner@f0(class@v0: String, rest@v1: Html) -> Html {
                  write("<span")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_html(v1)
                  write(">")
                  write("</span>")
                }
                fn Outer@f1(class@v2: String, rest@v3: Html) -> Html {
                  write("<div")
                  write(" class=\"")
                  write_string(v2)
                  write("\"")
                  write(">")
                  call Inner@f0(class = "x", rest = {
                    write_html(v3)
                  })
                  write("</div>")
                }
                page Test() {
                  call Outer@f1(class = "x", rest = {})
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
                -- main.hop --
                fn Foo(
                  children: Html,
                  class: String,
                  ...rest,
                ) -> Html {
                  <div class={class} ...rest>
                    {children}
                  </div>
                }

                fn Button(
                  children: Html,
                  class: String = "",
                  ...rest,
                ) -> Html {
                  <Foo class={class} ...rest>
                    {children}
                  </Foo>
                }

                page Test() {
                  fn body() -> Html {
                    <Button class="primary">
                      click
                    </Button>
                  }
                }
            "#},
            r#"<div class="primary">click</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f0(
                  children@v0: Html,
                  class@v1: String,
                  rest@v2: Html,
                ) -> Html {
                  call Foo@f1(children = {
                    write_html(v0)
                  }, class = v1, rest = {
                    write_html(v2)
                  })
                }
                fn Foo@f1(
                  children@v3: Html,
                  class@v4: String,
                  rest@v5: Html,
                ) -> Html {
                  write("<div")
                  write(" class=\"")
                  write_string(v4)
                  write("\"")
                  write_html(v5)
                  write(">")
                  write_html(v3)
                  write("</div>")
                }
                page Test() {
                  call Button@f0(children = {
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
                -- main.hop --
                fn Inner(
                  class: String = "x",
                  ...rest,
                ) -> Html {
                  <span class={class} ...rest>
                  </span>
                }

                fn Wrapper(...rest) -> Html {
                  <Inner ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper class="y"/>
                  }
                }
            "#},
            r#"<span class="y"></span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner@f0(class@v0: String, rest@v1: Html) -> Html {
                  write("<span")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_html(v1)
                  write(">")
                  write("</span>")
                }
                fn Wrapper@f1(class@v2: String, rest@v3: Html) -> Html {
                  call Inner@f0(class = v2, rest = {
                    write_html(v3)
                  })
                }
                page Test() {
                  call Wrapper@f1(class = "y", rest = {})
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
                -- main.hop --
                fn A(
                  class: String = "",
                  ...rest,
                ) -> Html {
                  <div class={class} ...rest>
                  </div>
                }

                fn B(
                  class: String = "",
                  ...rest,
                ) -> Html {
                  <A class={class} ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <B class="main"/>
                  }
                }
            "#},
            r#"<div class="main"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0(class@v0: String, rest@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_html(v1)
                  write(">")
                  write("</div>")
                }
                fn B@f1(class@v2: String, rest@v3: Html) -> Html {
                  call A@f0(class = v2, rest = {
                    write_html(v3)
                  })
                }
                page Test() {
                  call B@f1(class = "main", rest = {})
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
                -- main.hop --
                fn A(
                  class: String = "a",
                  ...rest,
                ) -> Html {
                  <div class={class} ...rest>
                  </div>
                }

                fn B(
                  class: String = "b",
                  ...rest,
                ) -> Html {
                  <A class={class} ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <B/>
                  }
                }
            "#},
            r#"<div class="b"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0(class@v0: String, rest@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"")
                  write_string(v0)
                  write("\"")
                  write_html(v1)
                  write(">")
                  write("</div>")
                }
                fn B@f1(class@v2: String, rest@v3: Html) -> Html {
                  call A@f0(class = v2, rest = {
                    write_html(v3)
                  })
                }
                page Test() {
                  call B@f1(class = "b", rest = {})
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
                -- main.hop --
                fn A(
                  label: String = "x",
                  ...rest,
                ) -> Html {
                  <span ...rest>
                    {label}
                  </span>
                }

                fn B(...rest) -> Html {
                  <A ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <B/>
                  }
                }
            "#},
            r#"<span>x</span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0(label@v0: String, rest@v1: Html) -> Html {
                  write("<span")
                  write_html(v1)
                  write(">")
                  write_string(v0)
                  write("</span>")
                }
                fn B@f1(label@v2: String, rest@v3: Html) -> Html {
                  call A@f0(label = v2, rest = {
                    write_html(v3)
                  })
                }
                page Test() {
                  call B@f1(label = "x", rest = {})
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
                -- main.hop --
                fn Leaf(
                  label: String = "x",
                  ...rest,
                ) -> Html {
                  <span ...rest>
                    {label}
                  </span>
                }

                fn Mid(...rest) -> Html {
                  <Leaf ...rest/>
                }

                fn Top(...rest) -> Html {
                  <Mid ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Top/>
                  }
                }
            "#},
            r#"<span>x</span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Leaf@f0(label@v0: String, rest@v1: Html) -> Html {
                  write("<span")
                  write_html(v1)
                  write(">")
                  write_string(v0)
                  write("</span>")
                }
                fn Mid@f1(label@v2: String, rest@v3: Html) -> Html {
                  call Leaf@f0(label = v2, rest = {
                    write_html(v3)
                  })
                }
                fn Top@f2(label@v4: String, rest@v5: Html) -> Html {
                  call Mid@f1(label = v4, rest = {
                    write_html(v5)
                  })
                }
                page Test() {
                  call Top@f2(label = "x", rest = {})
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
                -- main.hop --
                fn Inner(...rest) -> Html {
                  <span ...rest>
                  </span>
                }

                fn Wrapper(...rest) -> Html {
                  <Inner title="a" ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper lang="en"/>
                  }
                }
            "#},
            r#"<span title="a" lang="en"></span>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner@f0(rest@v0: Html) -> Html {
                  write("<span")
                  write_html(v0)
                  write(">")
                  write("</span>")
                }
                fn Wrapper@f1(rest@v1: Html) -> Html {
                  call Inner@f0(rest = {
                    write(" title=\"a\"")
                    write_html(v1)
                  })
                }
                page Test() {
                  call Wrapper@f1(rest = {
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
                -- main.hop --
                fn A(
                  tabindex: Int,
                  ...rest,
                ) -> Html {
                  <div ...rest>
                    {match tabindex > 0 {
                      true => <>focusable</>,
                      false => <></>,
                    }}
                  </div>
                }

                fn B(...rest) -> Html {
                  <A ...rest/>
                }

                page Test() {
                  fn body() -> Html {
                    <B tabindex={2} data-x="y"/>
                  }
                }
            "#},
            r#"<div data-x="y">focusable</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0(tabindex@v0: Int, rest@v1: Html) -> Html {
                  write("<div")
                  write_html(v1)
                  write(">")
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        write("focusable")
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                fn B@f1(tabindex@v3: Int, rest@v4: Html) -> Html {
                  call A@f0(tabindex = v3, rest = {
                    write_html(v4)
                  })
                }
                page Test() {
                  call B@f1(tabindex = 2, rest = {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let inner: Option[String] = Some("hello");
                    let mapped: Option[String] = match inner {
                      Some(x) => Some(x),
                      None => None,
                    };
                    match mapped {
                      Some(result) => {
                        <>
                          mapped:
                          {result}
                        </>
                      },
                      None => <>was-none</>,
                    }
                  }
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
                -- main.hop --
                record Point {
                  x: String,
                  y: String,
                }

                page Test() {
                  fn body() -> Html {
                    let result: String = match (Point {x: "hi", y: "bye"}) {
                      Point {x: a, y: _} => a,
                    };
                    <>
                      got:
                      {result}
                    </>
                  }
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v3 = let v0 = Point {x: "hi", y: "bye"} in {
                    let v1 = v0.x in { let v2 = v1 in { v2 } }
                  } in {
                    write("got:")
                    write_string(v3)
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
    fn bind_all_match_on_expression_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                record Point {
                  x: String,
                  y: String,
                }

                page Test() {
                  fn body() -> Html {
                    let result: String = match (Point {x: "hi", y: "bye"}) {
                      p => p.x,
                    };
                    <>
                      got:
                      {result}
                    </>
                  }
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v2 = let v0 = Point {x: "hi", y: "bye"} in {
                    let v1 = v0 in { v1.x }
                  } in {
                    write("got:")
                    write_string(v2)
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
    fn match_on_expression_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    match Some("hi") {
                      Some(x) => {
                        <>
                          got:
                          {x}
                        </>
                      },
                      None => <>none</>,
                    }
                  }
                }
            "#},
            "got:hi",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::Some("hi") in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
                          write("got:")
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let inner_opt: Option[String] = Some("inner");
                    let outer: Option[String] = Some(
                      match inner_opt {Some(x) => x, None => "default"}
                    );
                    match outer {
                      Some(s) => <>{s}</>,
                      None => <>none</>,
                    }
                  }
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
    fn function_binding_duplicated_by_inlining_at_two_call_sites() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Tag(text: String) -> Html {
                  let label: String = text;
                  <div>
                    {label}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Tag text="a"/>
                      <Tag text="b"/>
                    </>
                  }
                }
            "#},
            "<div>a</div><div>b</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Tag@f0(text@v0: String) -> Html {
                  let v1 = v0 in {
                    write("<div")
                    write(">")
                    write_string(v1)
                    write("</div>")
                  }
                }
                page Test() {
                  call Tag@f0(text = "a")
                  call Tag@f0(text = "b")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>a</div><div>b</div>")
                }
                -- expected output --
                <div>a</div><div>b</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn function_arguments_are_bound_simultaneously() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Swap(
                  a: String,
                  b: String,
                ) -> Html {
                  <p>
                    {a} {b}
                  </p>
                }

                page Test() {
                  fn body() -> Html {
                    let a: String = "A";
                    let b: String = "B";
                    <Swap a={b} b={a}/>
                  }
                }
            "#},
            "<p>B A</p>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Swap@f0(a@v2: String, b@v3: String) -> Html {
                  write("<p")
                  write(">")
                  write_string(v2)
                  write(" ")
                  write_string(v3)
                  write("</p>")
                }
                page Test() {
                  let v0 = "A" in {
                    let v1 = "B" in {
                      call Swap@f0(a = v1, b = v0)
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("<p>B A</p>")
                }
                -- expected output --
                <p>B A</p>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                fn Rows(
                  items: Array[String],
                  ...rest,
                ) -> Html {
                  for item in items {
                    <div ...rest>
                      {item}
                    </div>
                  }
                }

                page Test() {
                  fn body() -> Html {
                    let item: String = "outer";
                    <Rows items={["a", "b"]} id={item}/>
                  }
                }
            "#},
            r#"<div id="outer">a</div><div id="outer">b</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Rows@f0(items@v1: Array[String], rest@v2: Html) -> Html {
                  for v3 in v1 {
                    write("<div")
                    write_html(v2)
                    write(">")
                    write_string(v3)
                    write("</div>")
                  }
                }
                page Test() {
                  let v0 = "outer" in {
                    call Rows@f0(items = ["a", "b"], rest = {
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
                      write_html(v5)
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {
                        let flag: Bool = true;
                        <>{match flag {true => "yes", false => "no"}}</>
                      }
                      {
                        let other: Bool = false;
                        <>{match other {true => "YES", false => "NO"}}</>
                      }
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let path: String = "";
                    let git_ref: String = "main";
                    <>{match path == "" {
                      true => git_ref,
                      _ => git_ref + " - " + path,
                    }}</>
                  }
                }
            "#},
            "main",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "" in {
                    let v1 = "main" in {
                      write_string(let v2 = (v0 == "") in {
                        match v2 {
                          true => { v1 }
                          false => { ((v1 + " - ") + v0) }
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {
                        let opt1: Option[String] = Some("hi");
                        <>{match opt1 {Some(_) => "some", None => "none"}}</>
                      }
                      ,
                      {
                        let opt2: Option[String] = None;
                        <>{match opt2 {Some(_) => "SOME", None => "NONE"}}</>
                      }
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {
                        let outer: Bool = true;
                        let inner: Bool = false;
                        <>{match outer {
                          true => match inner {true => "TT", false => "TF"},
                          false => "F",
                        }}</>
                      }
                      ,
                      {
                        let outer2: Bool = false;
                        let inner2: Bool = true;
                        <>{match outer2 {
                          true => match inner2 {true => "TT", false => "TF"},
                          false => "F",
                        }}</>
                      }
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let num: Int = -123;
                    <>{num.to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let temp: Float = -2.9;
                    <>{temp.to_int().to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <h1>
                      Hello, World!
                    </h1>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      <!-- This is a comment -->
                      <h1>
                        Hello, World!
                      </h1>
                      <!-- Another comment -->
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name: String = "Alice";
                    <>
                      Hello,
                      {" "}
                      {name}
                      !
                    </>
                  }
                }
            "#},
            "Hello, Alice!",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "Alice" in {
                    write("Hello,")
                    write_string(" ")
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let show: Bool = true;
                    <>
                      {match show {
                        true => <>Visible</>,
                        false => <></>,
                      }}
                      {match !show {
                        true => <>Hidden</>,
                        false => <></>,
                      }}
                    </>
                  }
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
                    let v1 = (!v0) in {
                      match v1 {
                        true => {
                          write("Hidden")
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for item in ["a", "b", "c"] {
                      <>
                        {item}
                        ,
                      </>
                    }
                  }
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
    fn for_expression_over_array() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for item in ["a", "b", "c"] {
                      <>{item},</>
                    }
                  }
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
    fn for_expression_over_range() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for i in 1..=3 {
                      <>{i.to_string()},</>
                    }
                  }
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
    fn for_expression_with_underscore() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = ["a", "b", "c"];
                    for _ in items {
                      <>*</>
                    }
                  }
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
    fn for_expression_with_let_in_body() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for item in ["a", "b", "c"] {
                      let shout = item + "!";
                      <>{shout}</>
                    }
                  }
                }
            "#},
            "a!b!c!",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in ["a", "b", "c"] {
                    let v1 = (v0 + "!") in {
                      write_string(v1)
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b", "c"] {
                    let v1 = (v0 + "!") in {
                      write_string(v1)
                    }
                  }
                }
                -- expected output --
                a!b!c!
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn for_loop_over_bool_array_with_bool_match() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for v in [true] {
                      match v {
                        true => <>x</>,
                        false => <></>,
                      }
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for i in 1..=3 {
                      <>
                        {i.to_string()}
                        ,
                      </>
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for x in 0..=5 {
                      <>{x.to_string()}</>
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for i in 1..=2 {
                      for j in 1..=2 {
                        <>
                          (
                          {i.to_string()}
                          ,
                          {j.to_string()}
                          )
                        </>
                      }
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let text: String = "<div>Hello & world</div>";
                    <>{text}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let message: String = "Hello from let";
                    <>{message}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for name in ["a", "b"] {
                      <span class={
                        join!(
                          name,
                          "px-2",
                          "py-1",
                        )
                      }>
                        {name + "!" + "?"}
                      </span>
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let first: String = "Hello";
                    let second: String = " World";
                    <>{first + second}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for item in ["A", "B"] {
                      let prefix: String = "[";
                      <>
                        {prefix}
                        {item}
                        ]
                      </>
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    match "foo" + "bar" == "foobar" {
                      true => <>equals</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "equals",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = (("foo" + "bar") == "foobar") in {
                    match v0 {
                      true => {
                        write("equals")
                      }
                      false => {
                      }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {match 3 < 5 {
                        true => <>3 &lt; 5</>,
                        false => <></>,
                      }}
                      {match 10 < 2 {
                        true => <>10 &lt; 2</>,
                        false => <></>,
                      }}
                    </>
                  }
                }
            "#},
            "3 &lt; 5",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = (3 < 5) in {
                    match v0 {
                      true => {
                        write("3 &lt; 5")
                      }
                      false => {
                      }
                    }
                  }
                  let v1 = (10 < 2) in {
                    match v1 {
                      true => {
                        write("10 &lt; 2")
                      }
                      false => {
                      }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    match 1.5 < 2.5 {
                      true => <>1.5 &lt; 2.5</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "1.5 &lt; 2.5",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = (1.5 < 2.5) in {
                    match v0 {
                      true => {
                        write("1.5 &lt; 2.5")
                      }
                      false => {
                      }
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
    fn field_access() {
        check(
            indoc! {r#"
                -- main.hop --
                record Person {
                  name: String,
                  age: Int,
                }

                page Test() {
                  fn body() -> Html {
                    let person: Person = Person {name: "Alice", age: 30};
                    <>
                      {person.name}
                      {match person.age == 30 {
                        true => <>:30</>,
                        false => <></>,
                      }}
                    </>
                  }
                }
            "#},
            "Alice:30",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Person {name: "Alice", age: 30} in {
                    write_string(v0.name)
                    let v1 = (v0.age == 30) in {
                      match v1 {
                        true => {
                          write(":30")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Pair {
                  first: String,
                  second: String,
                }

                page Test() {
                  fn body() -> Html {
                    let pair: Pair = Pair {second: "b", first: "a"};
                    <>
                      {pair.first}
                      -
                      {pair.second}
                    </>
                  }
                }
            "#},
            "a-b",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Pair {second: "b", first: "a"} in {
                    write_string(v0.first)
                    write("-")
                    write_string(v0.second)
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                enum Shape {
                  Rect {
                    width: String,
                    height: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let shape = Shape::Rect {height: "b", width: "a"};
                    match shape {
                      Shape::Rect {width: w, height: h} => {
                        <>
                          {w}
                          -
                          {h}
                        </>
                      },
                    }
                  }
                }
            "#},
            "a-b",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Address {
                  city: String,
                  zip: String,
                }

                record Person {
                  name: String,
                  address: Address,
                }

                page Test() {
                  fn body() -> Html {
                    let person: Person = Person {
                      name: "Alice",
                      address: Address {city: "Paris", zip: "75001"},
                    };
                    <>
                      {person.name}
                      ,
                      {person.address.city}
                    </>
                  }
                }
            "#},
            "Alice,Paris",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let a: Int = 3;
                    let b: Int = 7;
                    match a + b == 10 {
                      true => <>correct</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "correct",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 3 in {
                    let v1 = 7 in {
                      let v2 = ((v0 + v1) == 10) in {
                        match v2 {
                          true => {
                            write("correct")
                          }
                          false => {
                          }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let a: Int = 10;
                    let b: Int = 3;
                    match a - b == 7 {
                      true => <>correct</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "correct",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 10 in {
                    let v1 = 3 in {
                      let v2 = ((v0 - v1) == 7) in {
                        match v2 {
                          true => {
                            write("correct")
                          }
                          false => {
                          }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let a: Int = 4;
                    let b: Int = 5;
                    match a * b == 20 {
                      true => <>correct</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "correct",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 4 in {
                    let v1 = 5 in {
                      let v2 = ((v0 * v1) == 20) in {
                        match v2 {
                          true => {
                            write("correct")
                          }
                          false => {
                          }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let a: Bool = true;
                    let b: Bool = true;
                    match a && b {
                      true => <>TT</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "TT",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = true in {
                    let v1 = true in {
                      let v2 = (v0 && v1) in {
                        match v2 {
                          true => {
                            write("TT")
                          }
                          false => {
                          }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let a: Bool = false;
                    let b: Bool = true;
                    match a || b {
                      true => <>FT</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "FT",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = false in {
                    let v1 = true in {
                      let v2 = (v0 || v1) in {
                        match v2 {
                          true => {
                            write("FT")
                          }
                          false => {
                          }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {match 3 <= 5 {
                        true => <>A</>,
                        false => <></>,
                      }}
                      {match 5 <= 5 {
                        true => <>B</>,
                        false => <></>,
                      }}
                      {match 7 <= 5 {
                        true => <>C</>,
                        false => <></>,
                      }}
                    </>
                  }
                }
            "#},
            "AB",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = (3 <= 5) in {
                    match v0 {
                      true => {
                        write("A")
                      }
                      false => {
                      }
                    }
                  }
                  let v1 = (5 <= 5) in {
                    match v1 {
                      true => {
                        write("B")
                      }
                      false => {
                      }
                    }
                  }
                  let v2 = (7 <= 5) in {
                    match v2 {
                      true => {
                        write("C")
                      }
                      false => {
                      }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let some_val: Option[String] = Some("hello");
                    match some_val {
                      Some(s) => <>{s}</>,
                      None => <>none</>,
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let opt: Option[String] = Some("hello");
                    match opt {
                      Some(_) => <>some</>,
                      None => <>none</>,
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let inner_opt: Option[String] = Some("inner");
                    let outer: Option[String] = Some(
                      match inner_opt {Some(x) => x, None => "default"}
                    );
                    match outer {
                      Some(s) => <>{s}</>,
                      None => <>none</>,
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for item in [Some("a"), None, Some("b")] {
                      match item {
                        Some(s) => <>{format!("[{}]", s)}</>,
                        None => <>[_]</>,
                      }
                    }
                  }
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
                          write_string(("[" + v2 + "]"))
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
                        write("[")
                        write_string(v1)
                        write("]")
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
    fn enum_without_variants() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Empty {}

                page Test() {
                  fn body() -> Html {
                    <>hi</>
                  }
                }
            "#},
            "hi",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("hi")
                }
                -- ir (optimized) --
                page Test() {
                  write("hi")
                }
                -- expected output --
                hi
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                page Test() {
                  fn body() -> Html {
                    let color: Color = Color::Green;
                    <>{match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}</>
                  }
                }
            "#},
            "green",
            expect![[r#"
                -- ir (unoptimized) --
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
    fn enum_match_with_field_bindings() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let result: Outcome = Outcome::Success {value: "hello"};
                    match result {
                      Outcome::Success {value: v} => <>{format!("Ok: {}", v)}</>,
                      Outcome::Failure {message: m} => <>{format!("Err: {}", m)}</>,
                    }
                  }
                }
            "#},
            "Ok: hello",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Outcome::Success {value: "hello"} in {
                    match v0 {
                      Outcome::Success(value: v1) => {
                        let v2 = v1 in {
                          write_string(("Ok: " + v2))
                        }
                      }
                      Outcome::Failure(message: v3) => {
                        let v4 = v3 in {
                          write_string(("Err: " + v4))
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("Ok: hello")
                }
                -- expected output --
                Ok: hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                enum Item {
                  Tagged {
                    tag: String,
                  },
                  Plain,
                }

                page Test() {
                  fn body() -> Html {
                    let item: Item = Item::Tagged {tag: "news"};
                    match item {
                      Item::Tagged {tag: t} => <>{format!("tag: {}", t)}</>,
                      Item::Plain => <>plain</>,
                    }
                  }
                }
            "#},
            "tag: news",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Item::Tagged {tag: "news"} in {
                    match v0 {
                      Item::Tagged(tag: v1) => {
                        let v2 = v1 in {
                          write_string(("tag: " + v2))
                        }
                      }
                      Item::Plain => {
                        write("plain")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("tag: news")
                }
                -- expected output --
                tag: news
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let result: String = match (Outcome::Success {value: "hi"}) {
                      Outcome::Success {value: v} => v,
                      Outcome::Failure {message: m} => m,
                    };
                    <>{format!("Got: {}", result)}</>
                  }
                }
            "#},
            "Got: hi",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v5 = let v0 = Outcome::Success {value: "hi"} in {
                    match v0 {
                      Outcome::Success {value: v1} => {
                        let v2 = v1 in { v2 }
                      }
                      Outcome::Failure {message: v3} => {
                        let v4 = v3 in { v4 }
                      }
                    }
                  } in {
                    write_string(("Got: " + v5))
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("Got: hi")
                }
                -- expected output --
                Got: hi
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_in_function_prop() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                fn Badge(color: Color) -> Html {
                  match color {
                    Color::Red => <>red</>,
                    Color::Green => <>green</>,
                    Color::Blue => <>blue</>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Badge color={Color::Green}/>
                  }
                }
            "#},
            "green",
            expect![[r#"
                -- ir (unoptimized) --
                fn Badge@f0(color@v0: main::Color) -> Html {
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
                  call Badge@f0(color = Color::Green)
                }
                -- ir (optimized) --
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
                -- main.hop --
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let result: Outcome = Outcome::Failure {
                      message: "something went wrong",
                    };
                    match result {
                      Outcome::Success {value: v} => <>{format!("Ok: {}", v)}</>,
                      Outcome::Failure {message: m} => <>{format!("Err: {}", m)}</>,
                    }
                  }
                }
            "#},
            "Err: something went wrong",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Outcome::Failure {message: "something went wrong"} in {
                    match v0 {
                      Outcome::Success(value: v1) => {
                        let v2 = v1 in {
                          write_string(("Ok: " + v2))
                        }
                      }
                      Outcome::Failure(message: v3) => {
                        let v4 = v3 in {
                          write_string(("Err: " + v4))
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("Err: something went wrong")
                }
                -- expected output --
                Err: something went wrong
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                enum Response {
                  Success {
                    code: String,
                    body: String,
                  },
                  Failure {
                    reason: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let resp = Response::Success {
                      code: "200",
                      body: "OK",
                    };
                    match resp {
                      Response::Success {code: c, body: b} => <>{format!("{} {}", c, b)}</>,
                      Response::Failure {reason: r} => <>{format!("Error: {}", r)}</>,
                    }
                  }
                }
            "#},
            "200 OK",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Response::Success {code: "200", body: "OK"} in {
                    match v0 {
                      Response::Success(code: v1, body: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            write_string((v3 + " " + v4))
                          }
                        }
                      }
                      Response::Failure(reason: v5) => {
                        let v6 = v5 in {
                          write_string(("Error: " + v6))
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("200 OK")
                }
                -- expected output --
                200 OK
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let result: Outcome = Outcome::Success {value: "hello"};
                    match result {
                      Outcome::Success {value} => <>{format!("Ok: {}", value)}</>,
                      Outcome::Failure {message} => <>{format!("Err: {}", message)}</>,
                    }
                  }
                }
            "#},
            "Ok: hello",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Outcome::Success {value: "hello"} in {
                    match v0 {
                      Outcome::Success(value: v1) => {
                        let v2 = v1 in {
                          write_string(("Ok: " + v2))
                        }
                      }
                      Outcome::Failure(message: v3) => {
                        let v4 = v3 in {
                          write_string(("Err: " + v4))
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("Ok: hello")
                }
                -- expected output --
                Ok: hello
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = ["a", "b", "c"];
                    <>{items.len().to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = [];
                    <>{items.len().to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = ["x", "y"];
                    match items.len() == 2 {
                      true => <>has two</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "has two",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = ["x", "y"] in {
                    let v1 = (v0.len() == 2) in {
                      match v1 {
                        true => {
                          write("has two")
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = ["a"];
                    match items.len() < 5 {
                      true => <>less than 5</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "less than 5",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = ["a"] in {
                    let v1 = (v0.len() < 5) in {
                      match v1 {
                        true => {
                          write("less than 5")
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let numbers: Array[Int] = [1, 2, 3, 4, 5];
                    <>{numbers.len().to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = [];
                    match items.is_empty() {
                      true => <>empty</>,
                      false => <>not empty</>,
                    }
                  }
                }
            "#},
            "empty",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = [] in {
                    let v1 = v0.is_empty() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = ["a", "b"];
                    match items.is_empty() {
                      true => <>empty</>,
                      false => <>not empty</>,
                    }
                  }
                }
            "#},
            "not empty",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = ["a", "b"] in {
                    let v1 = v0.is_empty() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let numbers: Array[Int] = [1, 2, 3];
                    match numbers.is_empty() {
                      true => <>no numbers</>,
                      false => <>has numbers</>,
                    }
                  }
                }
            "#},
            "has numbers",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = [1, 2, 3] in {
                    let v1 = v0.is_empty() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let count: Int = 42;
                    <>{count.to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let num: Int = 0;
                    <>{num.to_string()}</>
                  }
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
    fn float_to_int_simple() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let price: Float = 3.7;
                    <>{price.to_int().to_string()}</>
                  }
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
    fn int_to_float_on_a_loop_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for i in [3] {
                      <>{i.to_float().to_int().to_string()}</>
                    }
                  }
                }
            "#},
            "3",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [3] {
                    write_string(v0.to_float().to_int().to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [3] {
                    write_string(v0.to_float().to_int().to_string())
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let num: Float = 5.0;
                    <>{num.to_int().to_string()}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for _ in 0..=2 {
                      <>x</>
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for x in ["a", "b"] {
                      <>
                        {match false {
                          true => <>{x}</>,
                          false => <></>,
                        }}
                        y
                      </>
                    }
                  }
                }
            "#},
            "yy",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    let v1 = false in {
                      match v1 {
                        true => {
                          write_string(v0)
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let items: Array[String] = ["a", "b", "c"];
                    for _ in items {
                      <>*</>
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for _ in 0..=1 {
                      for _ in 0..=2 {
                        <>.</>
                      }
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for i in 1..=2 {
                      for _ in 0..=1 {
                        <>{i.to_string()}</>
                      }
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {[1, 2, 3].len().to_string()}
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {(1 + 2).to_string()}
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      {42.to_string()}
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let nested: Option[Option[String]] = Some(Some("deep"));
                    match nested {
                      Some(Some(x)) => <>{x}</>,
                      Some(None) => <>some-none</>,
                      None => <>none</>,
                    }
                  }
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
    fn option_wildcard_match_expr_some_input() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let opt: Option[String] = Some("x");
                    <>{match opt {Some(_) => "some", None => "none"}}</>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let opt: Option[String] = None;
                    <>{match opt {Some(_) => "some", None => "none"}}</>
                  }
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
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let nested = Some(Some("x"));
                    match nested {
                      Some(Some(_)) => <>some-some</>,
                      Some(None) => <>some-none</>,
                      None => <>none</>,
                    }
                  }
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
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let nested = Some(Some("x"));
                    match nested {
                      Some(_) => <>some</>,
                      None => <>none</>,
                    }
                  }
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
        check(
            indoc! {r#"
                -- main.hop --
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let result = Outcome::Success {value: "Hello"};
                    match result {
                      Outcome::Success {value: _} => <>ok</>,
                      Outcome::Failure {message: _} => <>err</>,
                    }
                  }
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Outcome::Success {value: "Hello"} in {
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
        check(
            indoc! {r#"
                -- main.hop --
                enum Outcome {
                  Success {
                    value: String,
                  },
                  Failure {
                    message: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let result = Outcome::Failure {message: "failed"};
                    match result {
                      Outcome::Success {value: _} => <>ok</>,
                      Outcome::Failure {message: _} => <>err</>,
                    }
                  }
                }
            "#},
            "err",
            expect![[r#"
                -- ir (unoptimized) --
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
        check(
            indoc! {r#"
                -- main.hop --
                record Person {
                  name: String,
                  age: Int,
                }

                page Test() {
                  fn body() -> Html {
                    let person = Person {name: "Alice", age: 30};
                    match person {
                      Person {name: _, age: a} => <>{format!("age: {}", a)}</>,
                    }
                  }
                }
            "#},
            "age: 30",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Person {name: "Alice", age: 30} in {
                    let v1 = v0.age in {
                      let v2 = v1 in {
                        write_string(("age: " + v2.to_string()))
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("age: 30")
                }
                -- expected output --
                age: 30
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let deep = Some(Some(Some("value")));
                    match deep {
                      Some(Some(Some(_))) => <>sss</>,
                      Some(Some(None)) => <>ssn</>,
                      Some(None) => <>sn</>,
                      None => <>n</>,
                    }
                  }
                }
            "#},
            "sss",
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
                                write("sss")
                              }
                              None => {
                                write("ssn")
                              }
                            }
                          }
                          None => {
                            write("sn")
                          }
                        }
                      }
                      None => {
                        write("n")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("sss")
                }
                -- expected output --
                sss
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
        check(
            indoc! {r#"
                -- main.hop --
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

                page Test() {
                  fn body() -> Html {
                    let result = Outer::Success {
                      value: Inner::Success {value: "deep"},
                    };
                    match result {
                      Outer::Success {value: Inner::Success {value: _}} => <>ok-ok</>,
                      Outer::Success {value: Inner::Failure {message: _}} => <>ok-err</>,
                      Outer::Failure {message: _} => <>err</>,
                    }
                  }
                }
            "#},
            "ok-ok",
            expect![[r#"
                -- ir (unoptimized) --
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
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let b = true;
                    match b {
                      true => <>t</>,
                      _ => <>f</>,
                    }
                  }
                }
            "#},
            "t",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = true in {
                    match v0 {
                      true => {
                        write("t")
                      }
                      false => {
                        write("f")
                      }
                    }
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
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let b = false;
                    match b {
                      true => <>t</>,
                      _ => <>f</>,
                    }
                  }
                }
            "#},
            "f",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = false in {
                    match v0 {
                      true => {
                        write("t")
                      }
                      false => {
                        write("f")
                      }
                    }
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
    fn nested_match_with_literal_subjects() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    match Some("outer") {
                      Some(x) => {
                        match Some("inner") {
                          Some(y) => <>{x}:{y}</>,
                          None => <>inner-none</>,
                        }
                      },
                      None => <>outer-none</>,
                    }
                  }
                }
            "#},
            "outer:inner",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::Some("outer") in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v1 in {
                          let v3 = Option[String]::Some("inner") in {
                            match v3 {
                              Some(v4) => {
                                let v5 = v4 in {
                                  write_string(v2)
                                  write(":")
                                  write_string(v5)
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
    fn nested_match_with_variable_subjects() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let outer = Some(Some("hello"));
                    match outer {
                      Some(inner) => {
                        match inner {
                          Some(value) => <>value:{value}</>,
                          None => <>inner-none</>,
                        }
                      },
                      None => <>outer-none</>,
                    }
                  }
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
    fn join_macro_concatenates_css_classes() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <div class={
                      join!(
                        "foo",
                        "bar",
                        "baz",
                      )
                    }>
                    </div>
                  }
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
    fn format_macro_interpolates_strings_and_ints() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name = "hop";
                    let count = 3;
                    <>{format!("a: {}, b: {}", name, count)}</>
                  }
                }
            "#},
            "a: hop, b: 3",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "hop" in {
                    let v1 = 3 in {
                      write_string(("a: " + v0 + ", b: " + v1.to_string()))
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("a: hop, b: 3")
                }
                -- expected output --
                a: hop, b: 3
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn format_macro_escapes_braces() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name = "c";
                    <>{format!("a{{b{}d}}e", name)}</>
                  }
                }
            "#},
            "a{bcd}e",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "c" in {
                    write_string(("a{" + "b" + v0 + "d}" + "e"))
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("a{bcd}e")
                }
                -- expected output --
                a{bcd}e
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn delete_as_variable_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let delete = "removed";
                    <>{delete}</>
                  }
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
    fn class_as_variable_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let class = "my-class";
                    <div class={class}></div>
                  }
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
    fn switch_as_variable_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let switch = "on";
                    <span>
                      {switch}
                    </span>
                  }
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
    fn type_as_variable_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let type = "button";
                    <input type={type}>
                  }
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
    fn for_as_attribute_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <label for="email">
                      Email
                    </label>
                  }
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
    fn delete_as_page_parameter_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      ok
                    </>
                  }
                }

                page Other(delete: String) {
                  fn body() -> Html {
                    <>
                      {delete}
                    </>
                  }
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
    fn type_as_page_parameter_name() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      ok
                    </>
                  }
                }

                page Other(type: String) {
                  fn body() -> Html {
                    <>
                      {type}
                    </>
                  }
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
    fn delete_as_recursive_function_parameter_name() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Countdown(delete: Int) -> Html {
                  <>
                    {delete.to_string()}
                    {match 0 < delete {
                      true => <Countdown delete={delete - 1}/>,
                      false => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <Countdown delete={3}/>
                  }
                }
            "#},
            "3210",
            expect![[r#"
                -- ir (unoptimized) --
                fn Countdown@f0(delete@v0: Int) -> Html {
                  write_string(v0.to_string())
                  let v1 = (0 < v0) in {
                    match v1 {
                      true => {
                        call Countdown@f0(delete = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Countdown@f0(delete = 3)
                }
                -- ir (optimized) --
                fn Countdown@f0(delete@v0: Int) -> Html {
                  write_string(v0.to_string())
                  let v1 = (0 < v0) in {
                    match v1 {
                      true => {
                        call Countdown@f0(delete = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Countdown@f0(delete = 3)
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
    fn type_as_recursive_function_parameter_name() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Countdown(type: Int) -> Html {
                  <>
                    {type.to_string()}
                    {match 0 < type {
                      true => <Countdown type={type - 1}/>,
                      false => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <Countdown type={3}/>
                  }
                }
            "#},
            "3210",
            expect![[r#"
                -- ir (unoptimized) --
                fn Countdown@f0(type@v0: Int) -> Html {
                  write_string(v0.to_string())
                  let v1 = (0 < v0) in {
                    match v1 {
                      true => {
                        call Countdown@f0(type = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Countdown@f0(type = 3)
                }
                -- ir (optimized) --
                fn Countdown@f0(type@v0: Int) -> Html {
                  write_string(v0.to_string())
                  let v1 = (0 < v0) in {
                    match v1 {
                      true => {
                        call Countdown@f0(type = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Countdown@f0(type = 3)
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for s in ["\"", "\\", "foo\nbar", "foo\tbar", "C:\\Users\\name"] {
                      <>{s}</>
                    }
                  }
                }
            "#},
            "&quot;\\foo\nbarfoo\tbarC:\\Users\\name",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    "\"",
                    "\\",
                    "foo\nbar",
                    "foo\tbar",
                    "C:\\Users\\name",
                  ] {
                    write_string(v0)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    "\"",
                    "\\",
                    "foo\nbar",
                    "foo\tbar",
                    "C:\\Users\\name",
                  ] {
                    write_string(v0)
                  }
                }
                -- expected output --
                &quot;\foo
                barfoo	barC:\Users\name
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                record Item {
                  name: String,
                  value: String,
                }

                page Test() {
                  fn body() -> Html {
                    let items = [
                      Item {name: "a", value: "1"},
                      Item {name: "b", value: "2"},
                    ];
                    for item in items {
                      let n: String = item.name;
                      <>
                        [{n}]
                      </>
                    }
                  }
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Address {
                  city: String,
                }

                record Person {
                  name: String,
                  address: Address,
                }

                page Test() {
                  fn body() -> Html {
                    let people = [
                      Person {
                        name: "alice",
                        address: Address {city: "paris"},
                      },
                      Person {
                        name: "bob",
                        address: Address {city: "london"},
                      },
                    ];
                    for person in people {
                      let city: String = person.address.city;
                      <>
                        [{city}]
                      </>
                    }
                  }
                }
            "#},
            "[paris][london]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Source {
                  name: String,
                  value: String,
                }

                record Target {
                  label: String,
                }

                page Test() {
                  fn body() -> Html {
                    let sources = [
                        Source {name: "a", value: "1"},
                        Source {name: "b", value: "2"},
                    ];
                    for src in sources {
                      let target = Target {label: src.name};
                      <>
                        [{target.label}]
                      </>
                    }
                  }
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Item {
                  name: String,
                }

                page Test() {
                  fn body() -> Html {
                    let items = [
                        Item {name: "a"},
                        Item {name: "b"},
                    ];
                    for item in items {
                      let opt = Some(item.name);
                      match opt {
                        Some(s) => {
                          <>
                            [{s}]
                          </>
                        },
                        None => <>[-]</>,
                      }
                    }
                  }
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
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
                page Test() {
                  for v1 in [Item {name: "a"}, Item {name: "b"}] {
                    let v2 = Option[String]::Some(v1.name) in {
                      match v2 {
                        Some(v3) => {
                          write("[")
                          write_string(v3)
                          write("]")
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let a = "hello";
                    let b = "world";
                    let c = a + " " + b;
                    <>{c}</>
                  }
                }
            "#},
            "hello world",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "hello" in {
                    let v1 = "world" in {
                      let v2 = ((v0 + " ") + v1) in {
                        write_string(v2)
                      }
                    }
                  }
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
    fn string_concat_in_record_field() {
        check(
            indoc! {r#"
                -- main.hop --
                record Greeting {
                  message: String,
                }

                page Test() {
                  fn body() -> Html {
                    let g = Greeting {message: "hello" + " " + "world"};
                    <>{g.message}</>
                  }
                }
            "#},
            "hello world",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Greeting {
                    message: (("hello" + " ") + "world"),
                  } in {
                    write_string(v0.message)
                  }
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
    fn int_to_string_in_let_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let n = 42;
                    let s = n.to_string();
                    <>{s}</>
                  }
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 42 in {
                    let v1 = v0.to_string() in {
                      write_string(v1)
                    }
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
    fn record_with_array_field() {
        check(
            indoc! {r#"
                -- main.hop --
                record Container {
                  items: Array[String],
                }

                page Test() {
                  fn body() -> Html {
                    let c = Container {items: ["a", "b"]};
                    for item in c.items {
                      <>
                        [{item}]
                      </>
                    }
                  }
                }
            "#},
            "[a][b]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Label {
                  text: String,
                }

                page Test() {
                  fn body() -> Html {
                    let l = Label {text: 42.to_string()};
                    <>{l.text}</>
                  }
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Label {text: 42.to_string()} in {
                    write_string(v0.text)
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
    fn nested_record_with_array() {
        check(
            indoc! {r#"
                -- main.hop --
                record Inner {
                  values: Array[String],
                }

                record Outer {
                  inner: Inner,
                }

                page Test() {
                  fn body() -> Html {
                    let o = Outer {inner: Inner {values: ["x", "y"]}};
                    for v in o.inner.values {
                      <>
                        [{v}]
                      </>
                    }
                  }
                }
            "#},
            "[x][y]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Foo {
                  a: String,
                }

                page Test() {
                  fn body() -> Html {
                    let x = Foo {a: "hello"};
                    let y = Foo {a: x.a};
                    <>[{x.a}][{y.a}]</>
                  }
                }
            "#},
            "[hello][hello]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Foo {
                  a: String,
                }

                page Test() {
                  fn body() -> Html {
                    let x = Foo {a: "hello"};
                    let b = true;
                    let result = match b {
                      true => x.a,
                      false => "default",
                    };
                    <>[{result}][{x.a}]</>
                  }
                }
            "#},
            "[hello][hello]",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record TreeNode {
                  value: String,
                  children: Array[TreeNode],
                }

                page Test() {
                  fn body() -> Html {
                    let leaf = TreeNode {value: "leaf", children: []};
                    <>{leaf.value}</>
                  }
                }
            "#},
            "leaf",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = TreeNode {value: "leaf", children: []} in {
                    write_string(v0.value)
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Node],
                }

                page Test() {
                  fn body() -> Html {
                    let node = Node {value: "first", next: None};
                    <>{node.value}</>
                  }
                }
            "#},
            "first",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Node {
                    value: "first",
                    next: Option[main::Node]::None,
                  } in {
                    write_string(v0.value)
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                enum Expr {
                  Literal {
                    value: String,
                  },
                  Neg {
                    inner: Expr,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let e = Expr::Literal {value: "42"};
                    match e {
                      Expr::Literal {value: v} => <>{v}</>,
                      Expr::Neg {inner: _} => <>neg</>,
                    }
                  }
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
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
    fn match_on_recursive_enum_field_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Expr {
                  Literal { value: String },
                  Neg { inner: Expr },
                }

                page Test() {
                  fn body() -> Html {
                    for e in [Expr::Neg {inner: Expr::Literal {value: "42"}}] {
                      match e {
                        Expr::Neg {inner: i} =>
                          match i {
                            Expr::Literal {value: v} => <>{v}</>,
                            Expr::Neg {inner: _} => <>nested</>,
                          },
                        Expr::Literal {value: _} => <>lit</>,
                      }
                    }
                  }
                }
            "#},
            "42",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    Expr::Neg {inner: Expr::Literal {value: "42"}},
                  ] {
                    match v0 {
                      Expr::Literal => {
                        write("lit")
                      }
                      Expr::Neg(inner: v1) => {
                        let v2 = v1 in {
                          match v2 {
                            Expr::Literal(value: v3) => {
                              let v4 = v3 in {
                                write_string(v4)
                              }
                            }
                            Expr::Neg => {
                              write("nested")
                            }
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Expr::Neg {inner: Expr::Literal {value: "42"}},
                  ] {
                    match v0 {
                      Expr::Literal => {
                        write("lit")
                      }
                      Expr::Neg(inner: v1) => {
                        match v1 {
                          Expr::Literal(value: v3) => {
                            write_string(v3)
                          }
                          Expr::Neg => {
                            write("nested")
                          }
                        }
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
                -- main.hop --
                enum Expr {
                  Literal {
                    value: String,
                  },
                  Neg {
                    inner: Expr,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let e = Expr::Neg {
                      inner: Expr::Literal {value: "42"}
                    };
                    match e {
                      Expr::Literal {value: v} => <>lit:{v}</>,
                      Expr::Neg {inner: _} => <>neg</>,
                    }
                  }
                }
            "#},
            "neg",
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Folder {
                  name: String,
                  parent: Option[File],
                }

                record File {
                  owner: Option[Folder],
                }

                page Test() {
                  fn body() -> Html {
                    let f = Folder {name: "root", parent: None};
                    <>{f.name}</>
                  }
                }
            "#},
            "root",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Folder {
                    name: "root",
                    parent: Option[main::File]::None,
                  } in {
                    write_string(v0.name)
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
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

                page Test() {
                  fn body() -> Html {
                    let leaf = Leaf {back: None};
                    match leaf.back {
                      Some(_) => <>some</>,
                      None => <>none</>,
                    }
                  }
                }
            "#},
            "none",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Leaf {back: Option[main::Expr]::None} in {
                    let v1 = v0.back in {
                      match v1 {
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
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Node],
                }

                page Test() {
                  fn body() -> Html {
                    let tail: Option[Node] = None;
                    let head = Node {value: "head", next: tail};
                    <>{head.value}</>
                  }
                }
            "#},
            "head",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[main::Node]::None in {
                    let v1 = Node {value: "head", next: v0} in {
                      write_string(v1.value)
                    }
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Node],
                }

                page Test() {
                  fn body() -> Html {
                    let leaf = Node {value: "leaf", next: None};
                    let head = Node {
                      value: "head",
                      next: match true {
                        true => Some(leaf),
                        false => None,
                      },
                    };
                    <>{head.value}</>
                  }
                }
            "#},
            "head",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Node {
                    value: "leaf",
                    next: Option[main::Node]::None,
                  } in {
                    let v2 = Node {
                      value: "head",
                      next: let v1 = true in {
                        match v1 {
                          true => { Option[main::Node]::Some(v0) }
                          false => { Option[main::Node]::None }
                        }
                      },
                    } in {
                      write_string(v2.value)
                    }
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Option[Node]],
                }

                page Test() {
                  fn body() -> Html {
                    let n = Node {value: "node", next: None};
                    <>{n.value}</>
                  }
                }
            "#},
            "node",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Node {
                    value: "node",
                    next: Option[Option[main::Node]]::None,
                  } in {
                    write_string(v0.value)
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Option[Node]],
                }

                page Test() {
                  fn body() -> Html {
                    let n = Node {
                      value: "head",
                      next: Some(Some(Node {value: "tail", next: None})),
                    };
                    match n.next {
                      Some(inner) => {
                        match inner {
                          Some(m) => <>{m.value}</>,
                          None => <>inner-none</>,
                        }
                      },
                      None => <>outer-none</>,
                    }
                  }
                }
            "#},
            "tail",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Node {
                    value: "head",
                    next: Option[Option[main::Node]]::Some(Option[main::Node]::Some(Node {
                      value: "tail",
                      next: Option[Option[main::Node]]::None,
                    })),
                  } in {
                    let v1 = v0.next in {
                      match v1 {
                        Some(v2) => {
                          let v3 = v2 in {
                            match v3 {
                              Some(v4) => {
                                let v5 = v4 in {
                                  write_string(v5.value)
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
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Node],
                }

                record Holder {
                  held: Option[Node],
                }

                page Test() {
                  fn body() -> Html {
                    let n = Node {value: "node", next: None};
                    let h = Holder {held: n.next};
                    match h.held {
                      Some(_) => <>some</>,
                      None => <>{n.value}</>,
                    }
                  }
                }
            "#},
            "node",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Node {
                    value: "node",
                    next: Option[main::Node]::None,
                  } in {
                    let v1 = Holder {held: v0.next} in {
                      let v2 = v1.held in {
                        match v2 {
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
                }
                -- ir (optimized) --
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
                -- main.hop --
                record A {
                  b: B,
                }

                record B {
                  name: String,
                  a: Option[A],
                }

                page Test() {
                  fn body() -> Html {
                    let x = A {b: B {name: "b", a: None}};
                    <>
                      {x.b.name}
                      {match x.b.a {
                        Some(_) => <>some</>,
                        None => <>none</>,
                      }}
                    </>
                  }
                }
            "#},
            "bnone",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = A {
                    b: B {name: "b", a: Option[main::A]::None},
                  } in {
                    write_string(v0.b.name)
                    let v1 = v0.b.a in {
                      match v1 {
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
                -- main.hop --
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

                page Test() {
                  fn body() -> Html {
                    let tree = Tree::Node {
                      label: "a",
                      left: Tree::Leaf,
                      right: None,
                    };
                    match tree {
                      Tree::Node {label: l, left: lt, right: r} => {
                        let s: Step = Step {t: lt, rest: r};
                        <>
                          {l}
                          {match s.rest {
                            Some(_) => <>some</>,
                            None => <>none</>,
                          }}
                        </>
                      },
                      Tree::Leaf => <>empty</>,
                    }
                  }
                }
            "#},
            "anone",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Tree::Node {label: "a", left: Tree::Leaf, right: Option[main::Tree]::None} in {
                    match v0 {
                      Tree::Node(label: v1, left: v2, right: v3) => {
                        let v4 = v1 in {
                          let v5 = v2 in {
                            let v6 = v3 in {
                              let v7 = Step {t: v5, rest: v6} in {
                                write_string(v4)
                                let v8 = v7.rest in {
                                  match v8 {
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
                      }
                      Tree::Leaf => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                enum Contact {
                  Email {
                    address: String,
                    label: Option[String],
                  },
                  Anonymous,
                }

                page Test() {
                  fn body() -> Html {
                    let c = Contact::Email {
                      address: "a@b.c",
                      label: Some("work"),
                    };
                    match c {
                      Contact::Email {address: a, label: l} => {
                        <>
                          {a}
                          {match l {
                            Some(s) => <>{s}</>,
                            None => <>no-label</>,
                          }}
                        </>
                      },
                      Contact::Anonymous => <>anon</>,
                    }
                  }
                }
            "#},
            "a@b.cwork",
            expect![[r#"
                -- ir (unoptimized) --
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
    fn rebuilding_an_enum_from_a_boxed_option_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Tree {
                  Node {
                    label: String,
                    kid: Option[Tree],
                  },
                  Leaf,
                }

                page Test() {
                  fn body() -> Html {
                    for t in [Tree::Node {label: "a", kid: None}] {
                      match t {
                        Tree::Node {label: l, kid: k} => {
                          match (Tree::Node {label: "b", kid: k}) {
                            Tree::Node {label: l2, kid: k2} => <>
                              {l}
                              {l2}
                              {match k2 {
                                Some(_) => <>s</>,
                                None => <>n</>,
                              }}
                            </>,
                            Tree::Leaf => <>x</>,
                          }
                        },
                        Tree::Leaf => <>empty</>,
                      }
                    }
                  }
                }
            "#},
            "abn",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[main::Tree]::None},
                  ] {
                    match v0 {
                      Tree::Node(label: v1, kid: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            let v5 = Tree::Node {label: "b", kid: v4} in {
                              match v5 {
                                Tree::Node(label: v6, kid: v7) => {
                                  let v8 = v6 in {
                                    let v9 = v7 in {
                                      write_string(v3)
                                      write_string(v8)
                                      match v9 {
                                        Some(_) => {
                                          write("s")
                                        }
                                        None => {
                                          write("n")
                                        }
                                      }
                                    }
                                  }
                                }
                                Tree::Leaf => {
                                  write("x")
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
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[main::Tree]::None},
                  ] {
                    match v0 {
                      Tree::Node(label: v1, kid: v2) => {
                        let v5 = Tree::Node {label: "b", kid: v2} in {
                          match v5 {
                            Tree::Node(label: v6, kid: v7) => {
                              write_string(v1)
                              write_string(v6)
                              match v7 {
                                Some(_) => {
                                  write("s")
                                }
                                None => {
                                  write("n")
                                }
                              }
                            }
                            Tree::Leaf => {
                              write("x")
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
                -- expected output --
                abn
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn rebuilding_an_enum_from_a_directly_boxed_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Tree {
                  Node {
                    label: String,
                    kid: Tree,
                  },
                  Leaf,
                }

                page Test() {
                  fn body() -> Html {
                    for t in [Tree::Node {label: "a", kid: Tree::Leaf}] {
                      match t {
                        Tree::Node {label: l, kid: k} => {
                          match (Tree::Node {label: "b", kid: k}) {
                            Tree::Node {label: l2, kid: _} => <>
                              {l}
                              {l2}
                            </>,
                            Tree::Leaf => <>x</>,
                          }
                        },
                        Tree::Leaf => <>empty</>,
                      }
                    }
                  }
                }
            "#},
            "ab",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [Tree::Node {label: "a", kid: Tree::Leaf}] {
                    match v0 {
                      Tree::Node(label: v1, kid: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            let v5 = Tree::Node {label: "b", kid: v4} in {
                              match v5 {
                                Tree::Node(label: v6) => {
                                  let v7 = v6 in {
                                    write_string(v3)
                                    write_string(v7)
                                  }
                                }
                                Tree::Leaf => {
                                  write("x")
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
                page Test() {
                  for v0 in [Tree::Node {label: "a", kid: Tree::Leaf}] {
                    match v0 {
                      Tree::Node(label: v1, kid: v2) => {
                        let v5 = Tree::Node {label: "b", kid: v2} in {
                          match v5 {
                            Tree::Node(label: v6) => {
                              write_string(v1)
                              write_string(v6)
                            }
                            Tree::Leaf => {
                              write("x")
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
                -- expected output --
                ab
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn rebuilding_an_enum_from_a_nested_option_boxed_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Tree {
                  Node {
                    label: String,
                    kid: Option[Option[Tree]],
                  },
                  Leaf,
                }

                page Test() {
                  fn body() -> Html {
                    for t in [Tree::Node {label: "a", kid: None}] {
                      match t {
                        Tree::Node {label: l, kid: k} => {
                          match (Tree::Node {label: "b", kid: k}) {
                            Tree::Node {label: l2, kid: _} => <>
                              {l}
                              {l2}
                            </>,
                            Tree::Leaf => <>x</>,
                          }
                        },
                        Tree::Leaf => <>empty</>,
                      }
                    }
                  }
                }
            "#},
            "ab",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Option[main::Tree]]::None},
                  ] {
                    match v0 {
                      Tree::Node(label: v1, kid: v2) => {
                        let v3 = v1 in {
                          let v4 = v2 in {
                            let v5 = Tree::Node {label: "b", kid: v4} in {
                              match v5 {
                                Tree::Node(label: v6) => {
                                  let v7 = v6 in {
                                    write_string(v3)
                                    write_string(v7)
                                  }
                                }
                                Tree::Leaf => {
                                  write("x")
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
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Option[main::Tree]]::None},
                  ] {
                    match v0 {
                      Tree::Node(label: v1, kid: v2) => {
                        let v5 = Tree::Node {label: "b", kid: v2} in {
                          match v5 {
                            Tree::Node(label: v6) => {
                              write_string(v1)
                              write_string(v6)
                            }
                            Tree::Leaf => {
                              write("x")
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
                -- expected output --
                ab
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn rebuilding_an_enum_from_a_boxed_record_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                record Holder {
                  tag: String,
                  e: Option[Wrap],
                }

                enum Wrap {
                  Full {
                    h: Option[Holder],
                  },
                  Empty,
                }

                page Test() {
                  fn body() -> Html {
                    for w in [Wrap::Full {h: None}] {
                      match w {
                        Wrap::Full {h: hh} => {
                          match (Wrap::Full {h: hh}) {
                            Wrap::Full {h: h2} => <>
                              {match h2 {
                                Some(x) => <>{x.tag}</>,
                                None => <>re</>,
                              }}
                            </>,
                            Wrap::Empty => <>x</>,
                          }
                        },
                        Wrap::Empty => <>empty</>,
                      }
                    }
                  }
                }
            "#},
            "re",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [Wrap::Full {h: Option[main::Holder]::None}] {
                    match v0 {
                      Wrap::Full(h: v1) => {
                        let v2 = v1 in {
                          let v3 = Wrap::Full {h: v2} in {
                            match v3 {
                              Wrap::Full(h: v4) => {
                                let v5 = v4 in {
                                  match v5 {
                                    Some(v6) => {
                                      let v7 = v6 in {
                                        write_string(v7.tag)
                                      }
                                    }
                                    None => {
                                      write("re")
                                    }
                                  }
                                }
                              }
                              Wrap::Empty => {
                                write("x")
                              }
                            }
                          }
                        }
                      }
                      Wrap::Empty => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Wrap::Full {h: Option[main::Holder]::None}] {
                    match v0 {
                      Wrap::Full(h: v1) => {
                        let v3 = Wrap::Full {h: v1} in {
                          match v3 {
                            Wrap::Full(h: v4) => {
                              match v4 {
                                Some(v6) => {
                                  write_string(v6.tag)
                                }
                                None => {
                                  write("re")
                                }
                              }
                            }
                            Wrap::Empty => {
                              write("x")
                            }
                          }
                        }
                      }
                      Wrap::Empty => {
                        write("empty")
                      }
                    }
                  }
                }
                -- expected output --
                re
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn boxed_binding_returned_from_a_match_arm() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Tree {
                  Node {
                    label: String,
                    kid: Option[Tree],
                  },
                  Leaf,
                }

                fn pick(t: Tree) -> Option[Tree] {
                  match t {
                    Tree::Node {label: _, kid: k} => k,
                    Tree::Leaf => None,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    for t in [Tree::Node {label: "a", kid: None}] {
                      match pick(t) {
                        Some(_) => <>some</>,
                        None => <>none</>,
                      }
                    }
                  }
                }
            "#},
            "none",
            expect![[r#"
                -- ir (unoptimized) --
                fn pick@f0(t@v2: main::Tree) -> Option[main::Tree] {
                  match v2 {
                    Tree::Node {kid: v3} => { let v4 = v3 in { v4 } }
                    Tree::Leaf => { Option[main::Tree]::None }
                  }
                }
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[main::Tree]::None},
                  ] {
                    let v1 = call pick@f0(t = v0) in {
                      match v1 {
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
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[main::Tree]::None},
                  ] {
                    let v1 = match v0 {
                      Tree::Node {kid: v6} => { v6 }
                      Tree::Leaf => { Option[main::Tree]::None }
                    } in {
                      match v1 {
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
    fn boxed_binding_passed_as_a_function_argument() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Tree {
                  Node {
                    label: String,
                    kid: Option[Tree],
                  },
                  Leaf,
                }

                fn depth(t: Option[Tree]) -> Int {
                  match t {
                    Some(_) => 1,
                    None => 0,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    for t in [Tree::Node {label: "a", kid: None}] {
                      match t {
                        Tree::Node {label: _, kid: k} => <>{depth(k).to_string()}</>,
                        Tree::Leaf => <>empty</>,
                      }
                    }
                  }
                }
            "#},
            "0",
            expect![[r#"
                -- ir (unoptimized) --
                fn depth@f0(t@v3: Option[main::Tree]) -> Int {
                  match v3 { Some(_) => { 1 } None => { 0 } }
                }
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[main::Tree]::None},
                  ] {
                    match v0 {
                      Tree::Node(kid: v1) => {
                        let v2 = v1 in {
                          write_string(call depth@f0(t = v2).to_string())
                        }
                      }
                      Tree::Leaf => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[main::Tree]::None},
                  ] {
                    match v0 {
                      Tree::Node(kid: v1) => {
                        write_string(match v1 {
                          Some(_) => { 1 }
                          None => { 0 }
                        }.to_string())
                      }
                      Tree::Leaf => {
                        write("empty")
                      }
                    }
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
    fn simple_function_call() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Greeting(name: String) -> Html {
                  <>Hello, {name}!</>
                }

                page Test() {
                  fn body() -> Html {
                    <Greeting name="World"/>
                  }
                }
            "#},
            "Hello, World!",
            expect![[r#"
                -- ir (unoptimized) --
                fn Greeting@f0(name@v0: String) -> Html {
                  write("Hello, ")
                  write_string(v0)
                  write("!")
                }
                page Test() {
                  call Greeting@f0(name = "World")
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
    fn function_with_children() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  children: Html,
                ) -> Html {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {children}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card title="Hello">
                      <p>
                        world
                      </p>
                    </Card>
                  }
                }
            "#},
            r#"<div class="card"><h2>Hello</h2><p>world</p></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String, children@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_string(v0)
                  write("</h2>")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "Hello", children = {
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
    fn function_children_forwarded_to_another_function() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Inner(children: Html) -> Html {
                  <div class="inner">
                    {children}
                  </div>
                }

                fn Outer(children: Html) -> Html {
                  <div class="outer">
                    <Inner>
                      {children}
                    </Inner>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Outer>
                      <p>
                        hello
                      </p>
                    </Outer>
                  }
                }
            "#},
            r#"<div class="outer"><div class="inner"><p>hello</p></div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner@f0(children@v0: Html) -> Html {
                  write("<div")
                  write(" class=\"inner\"")
                  write(">")
                  write_html(v0)
                  write("</div>")
                }
                fn Outer@f1(children@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"outer\"")
                  write(">")
                  call Inner@f0(children = {
                    write_html(v1)
                  })
                  write("</div>")
                }
                page Test() {
                  call Outer@f1(children = {
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
    fn function_children_with_function_calls() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Header(title: String) -> Html {
                  <header>
                    <h1>
                      {title}
                    </h1>
                  </header>
                }

                fn Footer() -> Html {
                  <footer>
                    <p>
                      Copyright 2024
                    </p>
                  </footer>
                }

                fn Layout(children: Html) -> Html {
                  <div class="layout">
                    {children}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
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
                }
            "#},
            r#"<div class="layout"><header><h1>Welcome</h1></header><main><p>Hello world</p></main><footer><p>Copyright 2024</p></footer></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Footer@f0() -> Html {
                  write("<footer")
                  write(">")
                  write("<p")
                  write(">")
                  write("Copyright 2024")
                  write("</p>")
                  write("</footer>")
                }
                fn Header@f1(title@v0: String) -> Html {
                  write("<header")
                  write(">")
                  write("<h1")
                  write(">")
                  write_string(v0)
                  write("</h1>")
                  write("</header>")
                }
                fn Layout@f2(children@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"layout\"")
                  write(">")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Layout@f2(children = {
                    call Header@f1(title = "Welcome")
                    write("<main")
                    write(">")
                    write("<p")
                    write(">")
                    write("Hello world")
                    write("</p>")
                    write("</main>")
                    call Footer@f0()
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
    fn function_children_used_twice() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Repeat(children: Html) -> Html {
                  <>
                    <div class="first">
                      {children}
                    </div>
                    <div class="second">
                      {children}
                    </div>
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <Repeat>
                      <span>
                        hi
                      </span>
                    </Repeat>
                  }
                }
            "#},
            r#"<div class="first"><span>hi</span></div><div class="second"><span>hi</span></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Repeat@f0(children@v0: Html) -> Html {
                  write("<div")
                  write(" class=\"first\"")
                  write(">")
                  write_html(v0)
                  write("</div>")
                  write("<div")
                  write(" class=\"second\"")
                  write(">")
                  write_html(v0)
                  write("</div>")
                }
                page Test() {
                  call Repeat@f0(children = {
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
                    write_html(v1)
                    write("</div><div class=\"second\">")
                    write_html(v1)
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
    fn recursive_function_with_non_recursive_sibling() {
        check(
            indoc! {r#"
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Node],
                }

                fn Badge(text: String) -> Html {
                  <strong>
                    {text}
                  </strong>
                }

                fn NodeView(node: Node) -> Html {
                  <>
                    <Badge text={node.value}/>
                    {match node.next {
                      Some(next) => {
                        <NodeView node={next}/>
                      },
                      None => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    let list = Node {
                      value: "a",
                      next: Some(Node {value: "b", next: None}),
                    };
                    <NodeView node={list}/>
                  }
                }
            "#},
            "<strong>a</strong><strong>b</strong>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Badge@f0(text@v1: String) -> Html {
                  write("<strong")
                  write(">")
                  write_string(v1)
                  write("</strong>")
                }
                fn NodeView@f1(node@v2: main::Node) -> Html {
                  call Badge@f0(text = v2.value)
                  let v3 = v2.next in {
                    match v3 {
                      Some(v4) => {
                        let v5 = v4 in {
                          call NodeView@f1(node = v5)
                        }
                      }
                      None => {
                      }
                    }
                  }
                }
                page Test() {
                  let v0 = Node {
                    value: "a",
                    next: Option[main::Node]::Some(Node {
                      value: "b",
                      next: Option[main::Node]::None,
                    }),
                  } in {
                    call NodeView@f1(node = v0)
                  }
                }
                -- ir (optimized) --
                fn NodeView@f1(node@v2: main::Node) -> Html {
                  write("<strong>")
                  write_string(v2.value)
                  write("</strong>")
                  let v3 = v2.next in {
                    match v3 {
                      Some(v4) => {
                        call NodeView@f1(node = v4)
                      }
                      None => {
                      }
                    }
                  }
                }
                page Test() {
                  call NodeView@f1(node = Node {
                    value: "a",
                    next: Option[main::Node]::Some(Node {
                      value: "b",
                      next: Option[main::Node]::None,
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
    fn recursive_function_with_linked_list() {
        check(
            indoc! {r#"
                -- main.hop --
                record Node {
                  value: String,
                  next: Option[Node],
                }

                fn NodeView(node: Node) -> Html {
                  <>
                    <span>
                      {node.value}
                    </span>
                    {match node.next {
                      Some(next) => {
                        <NodeView node={next}/>
                      },
                      None => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    let list = Node {
                      value: "a",
                      next: Some(
                        Node {
                          value: "b",
                          next: Some(Node {value: "c", next: None}),
                        }
                      ),
                    };
                    <NodeView node={list}/>
                  }
                }
            "#},
            "<span>a</span><span>b</span><span>c</span>",
            expect![[r#"
                -- ir (unoptimized) --
                fn NodeView@f0(node@v1: main::Node) -> Html {
                  write("<span")
                  write(">")
                  write_string(v1.value)
                  write("</span>")
                  let v2 = v1.next in {
                    match v2 {
                      Some(v3) => {
                        let v4 = v3 in {
                          call NodeView@f0(node = v4)
                        }
                      }
                      None => {
                      }
                    }
                  }
                }
                page Test() {
                  let v0 = Node {
                    value: "a",
                    next: Option[main::Node]::Some(Node {
                      value: "b",
                      next: Option[main::Node]::Some(Node {
                        value: "c",
                        next: Option[main::Node]::None,
                      }),
                    }),
                  } in {
                    call NodeView@f0(node = v0)
                  }
                }
                -- ir (optimized) --
                fn NodeView@f0(node@v1: main::Node) -> Html {
                  write("<span>")
                  write_string(v1.value)
                  write("</span>")
                  let v2 = v1.next in {
                    match v2 {
                      Some(v3) => {
                        call NodeView@f0(node = v3)
                      }
                      None => {
                      }
                    }
                  }
                }
                page Test() {
                  call NodeView@f0(node = Node {
                    value: "a",
                    next: Option[main::Node]::Some(Node {
                      value: "b",
                      next: Option[main::Node]::Some(Node {
                        value: "c",
                        next: Option[main::Node]::None,
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
    fn function_with_default_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title: String = "New card") -> Html {
                  <div>
                    {title}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card/>
                  }
                }
            "#},
            r#"<div>New card</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "New card")
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
    fn function_with_default_parameter_overridden() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title: String = "New card") -> Html {
                  <div>
                    {title}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card title="Custom title"/>
                  }
                }
            "#},
            r#"<div>Custom title</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "Custom title")
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
    fn function_with_mixed_default_and_required_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  subtitle: String = "No subtitle",
                ) -> Html {
                  <div>
                    {title} - {subtitle}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card title="Hello"/>
                  }
                }
            "#},
            r#"<div>Hello - No subtitle</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String, subtitle@v1: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write(" - ")
                  write_string(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "Hello", subtitle = "No subtitle")
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
    fn function_with_mixed_default_and_required_parameters_all_provided() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  subtitle: String = "No subtitle",
                ) -> Html {
                  <div>
                    {title} - {subtitle}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card title="Hello" subtitle="World"/>
                  }
                }
            "#},
            r#"<div>Hello - World</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String, subtitle@v1: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write(" - ")
                  write_string(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "Hello", subtitle = "World")
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
    fn function_with_multiple_default_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String = "Default",
                  subtitle: String = "Sub",
                  footer: String = "End",
                ) -> Html {
                  <div>
                    {title} - {subtitle} - {footer}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card subtitle="Custom"/>
                  }
                }
            "#},
            r#"<div>Default - Custom - End</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(
                  title@v0: String,
                  subtitle@v1: String,
                  footer@v2: String,
                ) -> Html {
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
                  call Card@f0(title = "Default", subtitle = "Custom", footer = "End")
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
    fn function_with_optional_children() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  children: Html = <></>,
                ) -> Html {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {children}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card title="Hello"/>
                  }
                }
            "#},
            r#"<div class="card"><h2>Hello</h2></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String, children@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_string(v0)
                  write("</h2>")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "Hello", children = {})
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
    fn function_with_optional_children_called_with_and_without_argument() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  children: Html = <></>,
                ) -> Html {
                  <div class="card">
                    <h2>
                      {title}
                    </h2>
                    {children}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Card title="With">
                        <p>
                          body
                        </p>
                      </Card>
                      <Card title="Without"/>
                    </>
                  }
                }
            "#},
            r#"<div class="card"><h2>With</h2><p>body</p></div><div class="card"><h2>Without</h2></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(title@v0: String, children@v1: Html) -> Html {
                  write("<div")
                  write(" class=\"card\"")
                  write(">")
                  write("<h2")
                  write(">")
                  write_string(v0)
                  write("</h2>")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "With", children = {
                    write("<p")
                    write(">")
                    write("body")
                    write("</p>")
                  })
                  call Card@f0(title = "Without", children = {})
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name = "";
                    match name.is_empty() {
                      true => <>empty</>,
                      false => <>not empty</>,
                    }
                  }
                }
            "#},
            "empty",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "" in {
                    let v1 = v0.is_empty() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name = "hello";
                    match name.is_empty() {
                      true => <>empty</>,
                      false => <>not empty</>,
                    }
                  }
                }
            "#},
            "not empty",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "hello" in {
                    let v1 = v0.is_empty() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let value = Some("hello");
                    match value.is_some() {
                      true => <>yes</>,
                      false => <>no</>,
                    }
                  }
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    let v1 = v0.is_some() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let value: Option[String] = None;
                    match value.is_some() {
                      true => <>yes</>,
                      false => <>no</>,
                    }
                  }
                }
            "#},
            "no",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::None in {
                    let v1 = v0.is_some() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let value: Option[String] = None;
                    match value.is_none() {
                      true => <>yes</>,
                      false => <>no</>,
                    }
                  }
                }
            "#},
            "yes",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::None in {
                    let v1 = v0.is_none() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let value = Some("hello");
                    match value.is_none() {
                      true => <>yes</>,
                      false => <>no</>,
                    }
                  }
                }
            "#},
            "no",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::Some("hello") in {
                    let v1 = v0.is_none() in {
                      match v1 {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let o: Option[Bool] = None;
                    match true == o.is_none() {
                      true => <>x</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[Bool]::None in {
                    let v1 = (true == v0.is_none()) in {
                      match v1 {
                        true => {
                          write("x")
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    match "a".is_empty() == "b".is_empty() {
                      true => <>x</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = ("a".is_empty() == "b".is_empty()) in {
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      hello world
                    </>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      hello
                      world
                    </>
                  }
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
    fn enum_bool_field_destructured_in_function() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Item {
                  Todo {
                    label: String,
                    done: Bool,
                  },
                }

                fn RenderItem(item: Item) -> Html {
                  match item {
                    Item::Todo {label: l, done: d} => {
                      <>
                        {match d {
                          true => <>[x]</>,
                          false => <></>,
                        }}
                        {match !d {
                          true => <>[ ]</>,
                          false => <></>,
                        }}
                        {l}
                      </>
                    }
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <RenderItem item={
                        Item::Todo {label: "Buy milk", done: true}
                      }/>
                      ,
                      <RenderItem item={
                        Item::Todo {label: "Walk dog", done: false}
                      }/>
                    </>
                  }
                }
            "#},
            "[x]Buy milk,[ ]Walk dog",
            expect![[r#"
                -- ir (unoptimized) --
                fn RenderItem@f0(item@v0: main::Item) -> Html {
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
                          let v5 = (!v4) in {
                            match v5 {
                              true => {
                                write("[ ]")
                              }
                              false => {
                              }
                            }
                          }
                          write_string(v3)
                        }
                      }
                    }
                  }
                }
                page Test() {
                  call RenderItem@f0(item = Item::Todo {label: "Buy milk", done: true})
                  write(",")
                  call RenderItem@f0(item = Item::Todo {label: "Walk dog", done: false})
                }
                -- ir (optimized) --
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
    fn enum_int_field_compared_in_function() {
        check(
            indoc! {r#"
                -- main.hop --
                enum TimeAgo {
                  MinutesAgo {
                    count: Int,
                  },
                  HoursAgo {
                    count: Int,
                  },
                }

                fn Render(time: TimeAgo) -> Html {
                  match time {
                    TimeAgo::MinutesAgo {count: c} => {
                      match c == 1 {
                        true => <>1 minute ago</>,
                        false => <>{c.to_string() + " minutes ago"}</>,
                      }
                    },
                    TimeAgo::HoursAgo {count: c} => {
                      match c == 1 {
                        true => <>1 hour ago</>,
                        false => <>{c.to_string() + " hours ago"}</>,
                      }
                    }
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Render time={TimeAgo::MinutesAgo {count: 1}}/>
                      ,
                      <Render time={TimeAgo::MinutesAgo {count: 5}}/>
                      ,
                      <Render time={TimeAgo::HoursAgo {count: 1}}/>
                    </>
                  }
                }
            "#},
            "1 minute ago,5 minutes ago,1 hour ago",
            expect![[r#"
                -- ir (unoptimized) --
                fn Render@f0(time@v0: main::TimeAgo) -> Html {
                  match v0 {
                    TimeAgo::MinutesAgo(count: v1) => {
                      let v2 = v1 in {
                        let v3 = (v2 == 1) in {
                          match v3 {
                            true => {
                              write("1 minute ago")
                            }
                            false => {
                              write_string((v2.to_string() + " minutes ago"))
                            }
                          }
                        }
                      }
                    }
                    TimeAgo::HoursAgo(count: v4) => {
                      let v5 = v4 in {
                        let v6 = (v5 == 1) in {
                          match v6 {
                            true => {
                              write("1 hour ago")
                            }
                            false => {
                              write_string((v5.to_string() + " hours ago"))
                            }
                          }
                        }
                      }
                    }
                  }
                }
                page Test() {
                  call Render@f0(time = TimeAgo::MinutesAgo {count: 1})
                  write(",")
                  call Render@f0(time = TimeAgo::MinutesAgo {count: 5})
                  write(",")
                  call Render@f0(time = TimeAgo::HoursAgo {count: 1})
                }
                -- ir (optimized) --
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
    fn enum_wildcard_field_in_function() {
        check(
            indoc! {r#"
                -- main.hop --
                enum CodeBlock {
                  Snippet {
                    language: String,
                    code: String,
                  },
                }

                fn RenderCode(block: CodeBlock) -> Html {
                  match block {
                    CodeBlock::Snippet {language: _, code: c} => {
                      <code>{c}</code>
                    }
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <RenderCode block={
                      CodeBlock::Snippet {language: "rust", code: "fn main()"}
                    }/>
                  }
                }
            "#},
            "<code>fn main()</code>",
            expect![[r#"
                -- ir (unoptimized) --
                fn RenderCode@f0(block@v0: main::CodeBlock) -> Html {
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
                  call RenderCode@f0(block = CodeBlock::Snippet {language: "rust", code: "fn main()"})
                }
                -- ir (optimized) --
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
    fn enum_field_named_type_in_function() {
        check(
            indoc! {r#"
                -- main.hop --
                enum ButtonElement {
                  Link {
                    href: String,
                  },
                  Button {
                    disabled: Bool,
                    type: String,
                  },
                }

                fn Render(el: ButtonElement) -> Html {
                  match el {
                    ButtonElement::Link {href: h} => {
                      <a href={h}>
                        link
                      </a>
                    },
                    ButtonElement::Button {disabled: _, type: t} => {
                      <button type={t}>
                        btn
                      </button>
                    }
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Render el={
                      ButtonElement::Button {disabled: false, type: "submit"}
                    }/>
                  }
                }
            "#},
            r#"<button type="submit">btn</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Render@f0(el@v0: main::ButtonElement) -> Html {
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
                  call Render@f0(el = ButtonElement::Button {disabled: false, type: "submit"})
                }
                -- ir (optimized) --
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
    fn option_record_used_in_inline_match_and_match_expr() {
        check(
            indoc! {r#"
                -- main.hop --
                record Target {
                  id: String,
                  title: String,
                }

                page Test() {
                  fn body() -> Html {
                    let target: Option[Target] = Some(
                      Target {id: "1", title: "hello"}
                    );
                    let items: Array[Option[String]] = [
                      match target {
                        Some(t) => Some(t.title),
                        None => None,
                      },
                    ];
                    <>
                      {for item in items {
                        match item {
                          Some(s) => {
                            <>
                              [
                              {s}
                              ]
                            </>
                          },
                          None => <></>,
                        }
                      }}
                      {match target {
                        Some(t) => {
                          <>
                            {t.title}
                          </>
                        },
                        None => <></>,
                      }}
                    </>
                  }
                }
            "#},
            "[hello]hello",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[main::Target]::Some(Target {
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
                page Test() {
                  for v4 in [Option[String]::Some("hello")] {
                    match v4 {
                      Some(v5) => {
                        write("[")
                        write_string(v5)
                        write("]")
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <img src={asset!("/logo.svg")}>
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <img src={asset!("/logo.svg")}>
                  }
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
    fn recursive_function_with_children_renders() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Nest(
                  depth: Int,
                  children: Html,
                ) -> Html {
                  match depth > 0 {
                    true => {
                      <div>
                        <Nest depth={depth - 1}>
                          {children}
                        </Nest>
                      </div>
                    },
                    false => children,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Nest depth={2}>
                      <b>
                        x
                      </b>
                    </Nest>
                  }
                }
            "#},
            "<div><div><b>x</b></div></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Nest@f0(depth@v0: Int, children@v1: Html) -> Html {
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        write("<div")
                        write(">")
                        call Nest@f0(depth = (v0 - 1), children = {
                          write_html(v1)
                        })
                        write("</div>")
                      }
                      false => {
                        write_html(v1)
                      }
                    }
                  }
                }
                page Test() {
                  call Nest@f0(depth = 2, children = {
                    write("<b")
                    write(">")
                    write("x")
                    write("</b>")
                  })
                }
                -- ir (optimized) --
                fn Nest@f0(depth@v0: Int, children@v1: Html) -> Html {
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        write("<div>")
                        call Nest@f0(depth = (v0 - 1), children = {
                          write_html(v1)
                        })
                        write("</div>")
                      }
                      false => {
                        write_html(v1)
                      }
                    }
                  }
                }
                page Test() {
                  call Nest@f0(depth = 2, children = {
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
                -- main.hop --
                fn Foo(children: Html) -> Html {
                  let x = children;
                  <div>
                    {x}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Foo>
                      <b>
                        hi
                      </b>
                    </Foo>
                  }
                }
            "#},
            "<div><b>hi</b></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Foo@f0(children@v0: Html) -> Html {
                  let v1 = v0 in {
                    write("<div")
                    write(">")
                    write_html(v1)
                    write("</div>")
                  }
                }
                page Test() {
                  call Foo@f0(children = {
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
                    write_html(v3)
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
                -- main.hop --
                fn Inner(children: Html) -> Html {
                  <em>
                    {children}
                  </em>
                }

                fn Outer(children: Html) -> Html {
                  <section>
                    <Inner>
                      {children}
                    </Inner>
                  </section>
                }

                page Test() {
                  fn body() -> Html {
                    <Outer>
                      z
                    </Outer>
                  }
                }
            "#},
            "<section><em>z</em></section>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Inner@f0(children@v0: Html) -> Html {
                  write("<em")
                  write(">")
                  write_html(v0)
                  write("</em>")
                }
                fn Outer@f1(children@v1: Html) -> Html {
                  write("<section")
                  write(">")
                  call Inner@f0(children = {
                    write_html(v1)
                  })
                  write("</section>")
                }
                page Test() {
                  call Outer@f1(children = {
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
    fn recursive_function_carries_a_rest() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Nest(
                  n: Int,
                  ...rest,
                ) -> Html {
                  <div ...rest>
                    {match 0 < n {
                      true => <Nest n={n - 1}/>,
                      false => <></>,
                    }}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Nest n={2} id="root"/>
                  }
                }
            "#},
            r#"<div id="root"><div><div></div></div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Nest@f0(n@v0: Int, rest@v1: Html) -> Html {
                  write("<div")
                  write_html(v1)
                  write(">")
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        call Nest@f0(n = (v0 - 1), rest = {})
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  call Nest@f0(n = 2, rest = {
                    write(" id=\"root\"")
                  })
                }
                -- ir (optimized) --
                fn Nest@f0(n@v0: Int, rest@v1: Html) -> Html {
                  write("<div")
                  write_html(v1)
                  write(">")
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        call Nest@f0(n = (v0 - 1), rest = {})
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  call Nest@f0(n = 2, rest = {
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
    fn recursive_function_with_int_param() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Countdown(n: Int) -> Html {
                  <>
                    {n.to_string()}
                    {match 0 < n {
                      true => <Countdown n={n - 1}/>,
                      false => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <Countdown n={3}/>
                  }
                }
            "#},
            "3210",
            expect![[r#"
                -- ir (unoptimized) --
                fn Countdown@f0(n@v0: Int) -> Html {
                  write_string(v0.to_string())
                  let v1 = (0 < v0) in {
                    match v1 {
                      true => {
                        call Countdown@f0(n = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Countdown@f0(n = 3)
                }
                -- ir (optimized) --
                fn Countdown@f0(n@v0: Int) -> Html {
                  write_string(v0.to_string())
                  let v1 = (0 < v0) in {
                    match v1 {
                      true => {
                        call Countdown@f0(n = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Countdown@f0(n = 3)
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
    fn recursive_function_with_option_param() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Loop(
                  n: Int,
                  label: Option[String],
                ) -> Html {
                  <>
                    {match label {
                      Some(text) => <>{text}</>,
                      None => <>x</>,
                    }}
                    {match 0 < n {
                      true => <Loop n={n - 1} label={label}/>,
                      false => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <Loop n={2} label={Some("a")}/>
                  }
                }
            "#},
            "aaa",
            expect![[r#"
                -- ir (unoptimized) --
                fn Loop@f0(n@v0: Int, label@v1: Option[String]) -> Html {
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
                  let v4 = (0 < v0) in {
                    match v4 {
                      true => {
                        call Loop@f0(n = (v0 - 1), label = v1)
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Loop@f0(n = 2, label = Option[String]::Some("a"))
                }
                -- ir (optimized) --
                fn Loop@f0(n@v0: Int, label@v1: Option[String]) -> Html {
                  match v1 {
                    Some(v2) => {
                      write_string(v2)
                    }
                    None => {
                      write("x")
                    }
                  }
                  let v4 = (0 < v0) in {
                    match v4 {
                      true => {
                        call Loop@f0(n = (v0 - 1), label = v1)
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Loop@f0(n = 2, label = Option[String]::Some("a"))
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
    fn recursive_function_with_option_arg_used_twice() {
        check(
            indoc! {r#"
                -- main.hop --
                fn C(x: Option[String]) -> Html {
                  match x.is_none() {
                    true => <C x={x}/>,
                    false => <></>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    let o: Option[String] = Some("a");
                    <>
                      <C x={o}/>
                      <C x={o}/>
                    </>
                  }
                }
            "#},
            "",
            expect![[r#"
                -- ir (unoptimized) --
                fn C@f0(x@v1: Option[String]) -> Html {
                  let v2 = v1.is_none() in {
                    match v2 {
                      true => {
                        call C@f0(x = v1)
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  let v0 = Option[String]::Some("a") in {
                    call C@f0(x = v0)
                    call C@f0(x = v0)
                  }
                }
                -- ir (optimized) --
                fn C@f0(x@v1: Option[String]) -> Html {
                  let v2 = v1.is_none() in {
                    match v2 {
                      true => {
                        call C@f0(x = v1)
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call C@f0(x = Option[String]::Some("a"))
                  call C@f0(x = Option[String]::Some("a"))
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
    fn mutually_recursive_functions() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Even(n: Int) -> Html {
                  <>
                    {match n == 0 {
                      true => <>even</>,
                      false => <></>,
                    }}
                    {match 0 < n {
                      true => <Odd n={n - 1}/>,
                      false => <></>,
                    }}
                  </>
                }

                fn Odd(n: Int) -> Html {
                  <>
                    {match n == 0 {
                      true => <>odd</>,
                      false => <></>,
                    }}
                    {match 0 < n {
                      true => <Even n={n - 1}/>,
                      false => <></>,
                    }}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    <Even n={4}/>
                  }
                }
            "#},
            "even",
            expect![[r#"
                -- ir (unoptimized) --
                fn Even@f0(n@v0: Int) -> Html {
                  let v1 = (v0 == 0) in {
                    match v1 {
                      true => {
                        write("even")
                      }
                      false => {
                      }
                    }
                  }
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        call Odd@f1(n = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                fn Odd@f1(n@v3: Int) -> Html {
                  let v4 = (v3 == 0) in {
                    match v4 {
                      true => {
                        write("odd")
                      }
                      false => {
                      }
                    }
                  }
                  let v5 = (0 < v3) in {
                    match v5 {
                      true => {
                        call Even@f0(n = (v3 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Even@f0(n = 4)
                }
                -- ir (optimized) --
                fn Even@f0(n@v0: Int) -> Html {
                  let v1 = (v0 == 0) in {
                    match v1 {
                      true => {
                        write("even")
                      }
                      false => {
                      }
                    }
                  }
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        call Odd@f1(n = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                fn Odd@f1(n@v3: Int) -> Html {
                  let v4 = (v3 == 0) in {
                    match v4 {
                      true => {
                        write("odd")
                      }
                      false => {
                      }
                    }
                  }
                  let v5 = (0 < v3) in {
                    match v5 {
                      true => {
                        call Even@f0(n = (v3 - 1))
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Even@f0(n = 4)
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
    fn field_access_on_record_literal_as_match_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                record R {
                  f: Bool,
                }

                page Test() {
                  fn body() -> Html {
                    match (R {f: true}.f) {
                      true => <>x</>,
                      false => <></>,
                    }
                  }
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = R {f: true}.f in {
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
    fn recursive_function_with_empty_array_arg() {
        check(
            indoc! {r#"
                -- main.hop --
                fn C(p: Array[String]) -> Html {
                  for _ in p {
                    <C p={[]}/>
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <C p={["a"]}/>
                  }
                }
            "#},
            "",
            expect![[r#"
                -- ir (unoptimized) --
                fn C@f0(p@v0: Array[String]) -> Html {
                  for _ in v0 {
                    call C@f0(p = [])
                  }
                }
                page Test() {
                  call C@f0(p = ["a"])
                }
                -- ir (optimized) --
                fn C@f0(p@v0: Array[String]) -> Html {
                  for _ in v0 {
                    call C@f0(p = [])
                  }
                }
                page Test() {
                  call C@f0(p = ["a"])
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
    fn field_access_on_record_literal_from_arg_as_match_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                record R {
                  f: Array[String],
                }

                fn C(p: Array[String]) -> Html {
                  match (R {f: p}.f.is_empty()) {
                    true => <C p={[]}/>,
                    false => <></>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <C p={["a"]}/>
                  }
                }
            "#},
            "",
            expect![[r#"
                -- ir (unoptimized) --
                fn C@f0(p@v0: Array[String]) -> Html {
                  let v1 = R {f: v0}.f.is_empty() in {
                    match v1 {
                      true => {
                        call C@f0(p = [])
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call C@f0(p = ["a"])
                }
                -- ir (optimized) --
                fn C@f0(p@v0: Array[String]) -> Html {
                  let v1 = v0.is_empty() in {
                    match v1 {
                      true => {
                        call C@f0(p = [])
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call C@f0(p = ["a"])
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
    fn option_bool_match_in_function() {
        check(
            indoc! {r#"
                -- main.hop --
                pub fn OptBool(checked: Option[Bool]) -> Html {
                  match checked {
                    Some(true) => {
                      <span>
                        yes
                      </span>
                    },
                    Some(false) => {
                      <span>
                        no
                      </span>
                    },
                    None => <></>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <OptBool checked={Some(true)}/>
                  }
                }
            "#},
            "<span>yes</span>",
            expect![[r#"
                -- ir (unoptimized) --
                fn OptBool@f0(checked@v0: Option[Bool]) -> Html {
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
                  call OptBool@f0(checked = Option[Bool]::Some(true))
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let x: Option[Option[Bool]] = Some(Some(true));
                    match x {
                      Some(Some(true)) => <>tt</>,
                      Some(Some(false)) => <>tf</>,
                      Some(None) => <>some-none</>,
                      None => <>none</>,
                    }
                  }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for n in [1, 2, 3] {
                      match n > 1 {
                        true => <>{n.to_string()}</>,
                        false => <></>,
                      }
                    }
                  }
                }
            "#},
            "23",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [1, 2, 3] {
                    let v1 = (1 < v0) in {
                      match v1 {
                        true => {
                          write_string(v0.to_string())
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [1, 2, 3] {
                    let v1 = (1 < v0) in {
                      match v1 {
                        true => {
                          write_string(v0.to_string())
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for s in ["a", "b"] {
                      match s == "a" {
                        true => <>{s}</>,
                        false => <></>,
                      }
                    }
                  }
                }
            "#},
            "a",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    let v1 = (v0 == "a") in {
                      match v1 {
                        true => {
                          write_string(v0)
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    let v1 = (v0 == "a") in {
                      match v1 {
                        true => {
                          write_string(v0)
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for f in [1.5, 2.5] {
                      match f > 2.0 {
                        true => <>big</>,
                        false => <></>,
                      }
                    }
                  }
                }
            "#},
            "big",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [1.5, 2.5] {
                    let v1 = (2 < v0) in {
                      match v1 {
                        true => {
                          write("big")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [1.5, 2.5] {
                    let v1 = (2 < v0) in {
                      match v1 {
                        true => {
                          write("big")
                        }
                        false => {
                        }
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
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for flag in [true, false] {
                      match flag && true {
                        true => <>x</>,
                        false => <></>,
                      }
                    }
                  }
                }
            "#},
            "x",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [true, false] {
                    let v1 = (v0 && true) in {
                      match v1 {
                        true => {
                          write("x")
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [true, false] {
                    let v1 = (v0 && true) in {
                      match v1 {
                        true => {
                          write("x")
                        }
                        false => {
                        }
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
                -- main.hop --
                pub fn Show(label: String) -> Html {
                  <span>
                    {label}
                  </span>
                }

                page Test() {
                  fn body() -> Html {
                    for s in ["a", "b"] {
                      match s == "a" {
                        true => <Show label={s}/>,
                        false => <></>,
                      }
                    }
                  }
                }
            "#},
            "<span>a</span>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Show@f0(label@v2: String) -> Html {
                  write("<span")
                  write(">")
                  write_string(v2)
                  write("</span>")
                }
                page Test() {
                  for v0 in ["a", "b"] {
                    let v1 = (v0 == "a") in {
                      match v1 {
                        true => {
                          call Show@f0(label = v0)
                        }
                        false => {
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in ["a", "b"] {
                    let v1 = (v0 == "a") in {
                      match v1 {
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
                -- main.hop --
                record Foo {
                  class: String,
                }

                page Test() {
                  fn body() -> Html {
                    let foo: Foo = Foo {class: "a"};
                    <div>
                      {foo.class}
                    </div>
                  }
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Foo {class: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.class)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Foo {
                  function: String,
                }

                page Test() {
                  fn body() -> Html {
                    let f: Foo = Foo {function: "a"};
                    <div>
                      {f.function}
                    </div>
                  }
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Foo {function: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.function)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Foo {
                  protected: String,
                }

                page Test() {
                  fn body() -> Html {
                    let f: Foo = Foo {protected: "a"};
                    <div>
                      {f.protected}
                    </div>
                  }
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Foo {protected: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.protected)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Foo {
                  eval: String,
                }

                page Test() {
                  fn body() -> Html {
                    let f: Foo = Foo {eval: "a"};
                    <div>
                      {f.eval}
                    </div>
                  }
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Foo {eval: "a"} in {
                    write("<div")
                    write(">")
                    write_string(v0.eval)
                    write("</div>")
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                enum E {
                  A {
                    class: String,
                  },
                }

                page Test() {
                  fn body() -> Html {
                    let e: E = E::A {class: "a"};
                    match e {
                      E::A {class: v} => {
                        <div>
                          {v}
                        </div>
                      },
                    }
                  }
                }
            "#},
            r#"<div>a</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
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
                -- main.hop --
                record Math {
                  x: Int,
                }

                page Test() {
                  fn body() -> Html {
                    let m: Math = Math {x: 4};
                    let b: Int = 5;
                    <>{(m.x * b).to_string()}</>
                  }
                }
            "#},
            r#"20"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Math {x: 4} in {
                    let v1 = 5 in {
                      write_string((v0.x * v1).to_string())
                    }
                  }
                }
                -- ir (optimized) --
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
                -- main.hop --
                record Number {
                  x: Float,
                }

                page Test() {
                  fn body() -> Html {
                    let n: Number = Number {x: 3.7};
                    <>{n.x.to_int().to_string()}</>
                  }
                }
            "#},
            r#"3"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Number {x: 3.7} in {
                    write_string(v0.x.to_int().to_string())
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
    fn record_spread_fills_fields_from_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                record State {
                  query: String,
                  num: Int,
                }

                page Test() {
                  fn body() -> Html {
                    let base = State {query: "a", num: 1};
                    let next = State {...base, num: 2};
                    <>
                      {next.query}
                      {next.num.to_string()}
                    </>
                  }
                }
            "#},
            r#"a2"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = State {query: "a", num: 1} in {
                    let v1 = State {query: v0.query, num: 2} in {
                      write_string(v1.query)
                      write_string(v1.num.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("a2")
                }
                -- expected output --
                a2
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                record State {
                  query: String,
                  num: Int,
                }

                page Test() {
                  fn body() -> Html {
                    let base = State {query: "a", num: 1};
                    let next = State {...base, query: "b", num: 2};
                    <>
                      {next.query}
                      {next.num.to_string()}
                    </>
                  }
                }
            "#},
            r#"b2"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = State {query: "a", num: 1} in {
                    let v1 = State {query: "b", num: 2} in {
                      write_string(v1.query)
                      write_string(v1.num.to_string())
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("b2")
                }
                -- expected output --
                b2
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                record State {
                  query: String,
                  num: Int,
                }

                page Test() {
                  fn body() -> Html {
                    for s in [State {query: "a", num: 7}] {
                      <>
                        {State {...s, query: "x"}.query}
                        {State {...s, query: "x"}.num.to_string()}
                      </>
                    }
                  }
                }
            "#},
            r#"x7"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [State {query: "a", num: 7}] {
                    write_string(State {query: "x", num: v0.num}.query)
                    write_string(State {
                      query: "x",
                      num: v0.num,
                    }.num.to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [State {query: "a", num: 7}] {
                    write("x")
                    write_string(v0.num.to_string())
                  }
                }
                -- expected output --
                x7
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn record_spread_in_match_arm_passed_as_function_prop() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {
                  label: String,
                  selected: Bool,
                }

                fn Row(item: Item) -> Html {
                  <div>
                    {item.label}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    for item in [Item {label: "a", selected: false}] {
                      match item.selected {
                        true => {
                          <Row item={Item {...item, label: "on"}}/>
                        },
                        false => {
                          <Row item={Item {...item, label: "off"}}/>
                        },
                      }
                    }
                  }
                }
            "#},
            r#"<div>off</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Row@f0(item@v2: main::Item) -> Html {
                  write("<div")
                  write(">")
                  write_string(v2.label)
                  write("</div>")
                }
                page Test() {
                  for v0 in [Item {label: "a", selected: false}] {
                    let v1 = v0.selected in {
                      match v1 {
                        true => {
                          call Row@f0(item = Item {
                            label: "on",
                            selected: v0.selected,
                          })
                        }
                        false => {
                          call Row@f0(item = Item {
                            label: "off",
                            selected: v0.selected,
                          })
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Item {label: "a", selected: false}] {
                    let v1 = v0.selected in {
                      match v1 {
                        true => {
                          write("<div>on</div>")
                        }
                        false => {
                          write("<div>off</div>")
                        }
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
                -- main.hop --
                record Settings {
                  theme: String,
                  compact: Bool,
                }

                record State {
                  query: String,
                  settings: Settings,
                }

                fn Dark(s: State) -> Html {
                  let t = Settings {...s.settings, theme: "dark"};
                  let next = State {...s, settings: t};
                  <>
                    {next.query}
                    {next.settings.theme}
                  </>
                }

                page Test() {
                  fn body() -> Html {
                    let s = Settings {theme: "light", compact: true};
                    <Dark s={State {query: "q", settings: s}}/>
                  }
                }
            "#},
            r#"qdark"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Dark@f0(s@v1: main::State) -> Html {
                  let v3 = let v2 = v1.settings in {
                    Settings {theme: "dark", compact: v2.compact}
                  } in {
                    let v4 = State {query: v1.query, settings: v3} in {
                      write_string(v4.query)
                      write_string(v4.settings.theme)
                    }
                  }
                }
                page Test() {
                  let v0 = Settings {theme: "light", compact: true} in {
                    call Dark@f0(s = State {query: "q", settings: v0})
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("qdark")
                }
                -- expected output --
                qdark
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                record Foo {
                  x: String,
                  y: String,
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      {Foo {...Foo {x: "bar", y: "baz"}, y: "foo"}.x}
                    </>
                  }
                }
            "#},
            r#"bar"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write_string(let v0 = Foo {x: "bar", y: "baz"} in {
                    Foo {x: v0.x, y: "foo"}
                  }.x)
                }
                -- ir (optimized) --
                page Test() {
                  write("bar")
                }
                -- expected output --
                bar
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn function_imported_from_another_module() {
        check(
            indoc! {r#"
                -- main.hop --
                import other::label

                page Test() {
                  fn body() -> Html {
                    <div>{label(prefix: "a")}</div>
                  }
                }
                -- other.hop --
                pub fn label(prefix: String, count: Int = 1) -> String {
                  prefix + count.to_string()
                }
            "#},
            "<div>a1</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn label@f0(prefix@v0: String, count@v1: Int) -> String {
                  (v0 + v1.to_string())
                }
                page Test() {
                  write("<div")
                  write(">")
                  write_string(call label@f0(prefix = "a", count = 1))
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>a1</div>")
                }
                -- expected output --
                <div>a1</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn function_with_omitted_default_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                fn label(prefix: String = "x", count: Int = 1) -> String {
                  prefix + count.to_string()
                }

                page Test() {
                  fn body() -> Html {
                    <div>{label()}{label(count: 2)}{label("y")}</div>
                  }
                }

                page Other(prefix: String) {
                  fn body() -> Html {
                    <div>{label(prefix: prefix)}</div>
                  }
                }
            "#},
            "<div>x1x2y1</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn label@f0(prefix@v1: String, count@v2: Int) -> String {
                  (v1 + v2.to_string())
                }
                page Test() {
                  write("<div")
                  write(">")
                  write_string(call label@f0(prefix = "x", count = 1))
                  write_string(call label@f0(prefix = "x", count = 2))
                  write_string(call label@f0(prefix = "y", count = 1))
                  write("</div>")
                }
                page Other(prefix@v0: String) {
                  write("<div")
                  write(">")
                  write_string(call label@f0(prefix = v0, count = 1))
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>x1x2y1</div>")
                }
                page Other(prefix@v0: String) {
                  write("<div>")
                  write_string(v0)
                  write("1</div>")
                }
                -- expected output --
                <div>x1x2y1</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                -- main.hop --
                fn foo(x: Int) -> Int {
                  x + 10
                }

                fn Wrapper() -> Html {
                  <div>
                    {for x in 0..=foo(-7) {
                      <>
                        {x.to_string()}
                        ,
                      </>
                    }}
                    {foo(10).to_string()}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper/>
                  }
                }
            "#},
            "<div>0,1,2,3,20</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper@f0() -> Html {
                  write("<div")
                  write(">")
                  for v0 in 0..=call foo@f1(x = (-7)) {
                    write_string(v0.to_string())
                    write(",")
                  }
                  write_string(call foo@f1(x = 10).to_string())
                  write("</div>")
                }
                fn foo@f1(x@v1: Int) -> Int {
                  (v1 + 10)
                }
                page Test() {
                  call Wrapper@f0()
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

    #[test]
    #[ignore]
    fn markup_as_a_function_body() {
        check(
            indoc! {r#"
                -- main.hop --
                fn card(label: String) -> Html {
                  <div>{label}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <>{card("hello")}</>
                  }
                }
            "#},
            "<div>hello</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn card@f0(label@v0: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                page Test() {
                  call card@f0(label = "hello")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>hello</div>")
                }
                -- expected output --
                <div>hello</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn markup_written_in_an_interpolation() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <div>{<span>hello</span>}</div>
                  }
                }
            "#},
            "<div><span>hello</span></div>",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("<div")
                  write(">")
                  write("<span")
                  write(">")
                  write("hello")
                  write("</span>")
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div><span>hello</span></div>")
                }
                -- expected output --
                <div><span>hello</span></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn markup_passed_as_a_function_argument() {
        check(
            indoc! {r#"
                -- main.hop --
                fn wrap(children: Html) -> Html {
                  <div>{children}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <>{wrap(<span>hello</span>)}</>
                  }
                }
            "#},
            "<div><span>hello</span></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn wrap@f0(children@v0: Html) -> Html {
                  write("<div")
                  write(">")
                  write_html(v0)
                  write("</div>")
                }
                page Test() {
                  call wrap@f0(children = {
                    write("<span")
                    write(">")
                    write("hello")
                    write("</span>")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div><span>hello</span></div>")
                }
                -- expected output --
                <div><span>hello</span></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn markup_passed_as_a_function_attribute() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(slot: Html) -> Html {
                  <div>{slot}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card slot={<span>hello</span>}/>
                  }
                }
            "#},
            "<div><span>hello</span></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0(slot@v0: Html) -> Html {
                  write("<div")
                  write(">")
                  write_html(v0)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(slot = {
                    write("<span")
                    write(">")
                    write("hello")
                    write("</span>")
                  })
                }
                -- ir (optimized) --
                page Test() {
                  write("<div><span>hello</span></div>")
                }
                -- expected output --
                <div><span>hello</span></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn markup_in_the_arms_of_a_match_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                fn badge(on: Bool) -> Html {
                  match on {true => <b>yes</b>, false => <i>no</i>}
                }

                page Test() {
                  fn body() -> Html {
                    <div>{badge(true)}{badge(false)}</div>
                  }
                }
            "#},
            "<div><b>yes</b><i>no</i></div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn badge@f0(on@v0: Bool) -> Html {
                  match v0 {
                    true => {
                      write("<b")
                      write(">")
                      write("yes")
                      write("</b>")
                    }
                    false => {
                      write("<i")
                      write(">")
                      write("no")
                      write("</i>")
                    }
                  }
                }
                page Test() {
                  write("<div")
                  write(">")
                  call badge@f0(on = true)
                  call badge@f0(on = false)
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div><b>yes</b><i>no</i></div>")
                }
                -- expected output --
                <div><b>yes</b><i>no</i></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn a_call_as_a_function_body() {
        check(
            indoc! {r#"
                -- main.hop --
                fn card(label: String) -> Html {
                  <div>{label}</div>
                }

                fn Outer() -> Html {
                  card("hello")
                }

                page Test() {
                  fn body() -> Html {
                    <Outer/>
                  }
                }
            "#},
            "<div>hello</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Outer@f0() -> Html {
                  call card@f1(label = "hello")
                }
                fn card@f1(label@v0: String) -> Html {
                  write("<div")
                  write(">")
                  write_string(v0)
                  write("</div>")
                }
                page Test() {
                  call Outer@f0()
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>hello</div>")
                }
                -- expected output --
                <div>hello</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn catch_all_after_constructor_on_expression_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Shape {
                  Circle,
                  Square,
                }

                fn mk() -> Shape {
                  Shape::Square
                }

                fn f() -> Int {
                  match mk() {
                    Shape::Circle => 1,
                    other => match other {
                      Shape::Square => 2,
                      Shape::Circle => 3,
                    },
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <div>{f().to_string()}</div>
                  }
                }
            "#},
            "<div>2</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn f@f0() -> Int {
                  let v0 = call mk@f1() in {
                    match v0 {
                      Shape::Circle => { 1 }
                      Shape::Square => {
                        let v1 = v0 in {
                          match v1 {
                            Shape::Circle => { 3 }
                            Shape::Square => { 2 }
                          }
                        }
                      }
                    }
                  }
                }
                fn mk@f1() -> main::Shape {
                  Shape::Square
                }
                page Test() {
                  write("<div")
                  write(">")
                  write_string(call f@f0().to_string())
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>2</div>")
                }
                -- expected output --
                <div>2</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn catch_all_after_constructor_in_match_on_expression_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Shape {
                  Circle,
                  Square,
                }

                fn mk() -> Shape {
                  Shape::Square
                }

                page Test() {
                  fn body() -> Html {
                    match mk() {
                      Shape::Circle => <>circle</>,
                      other => {
                        match other {
                          Shape::Square => <>square</>,
                          Shape::Circle => <>never</>,
                        }
                      },
                    }
                  }
                }
            "#},
            "square",
            expect![[r#"
                -- ir (unoptimized) --
                fn mk@f0() -> main::Shape {
                  Shape::Square
                }
                page Test() {
                  let v0 = call mk@f0() in {
                    match v0 {
                      Shape::Circle => {
                        write("circle")
                      }
                      Shape::Square => {
                        let v1 = v0 in {
                          match v1 {
                            Shape::Circle => {
                              write("never")
                            }
                            Shape::Square => {
                              write("square")
                            }
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("square")
                }
                -- expected output --
                square
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn catch_all_after_constructor_on_option_expression_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                fn mk() -> Option[String] {
                  Some("hi")
                }

                fn f() -> String {
                  match mk() {
                    None => "none",
                    other => match other {
                      Some(x) => x,
                      None => "never",
                    },
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <div>{f()}</div>
                  }
                }
            "#},
            "<div>hi</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn f@f0() -> String {
                  let v0 = call mk@f1() in {
                    match v0 {
                      Some(v1) => {
                        let v2 = v0 in {
                          match v2 {
                            Some(v3) => { let v4 = v3 in { v4 } }
                            None => { "never" }
                          }
                        }
                      }
                      None => { "none" }
                    }
                  }
                }
                fn mk@f1() -> Option[String] {
                  Option[String]::Some("hi")
                }
                page Test() {
                  write("<div")
                  write(">")
                  write_string(call f@f0())
                  write("</div>")
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
    fn same_named_functions_in_different_modules() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card() -> Html {
                  <span>A</span>
                }

                page Test() {
                  fn body() -> Html {
                    <Card />
                  }
                }
                -- other.hop --
                fn Card() -> Html {
                  <span>B</span>
                }

                page Other() {
                  fn body() -> Html {
                    <Card />
                  }
                }
            "#},
            "<span>A</span>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f0() -> Html {
                  write("<span")
                  write(">")
                  write("A")
                  write("</span>")
                }
                fn Card@f1() -> Html {
                  write("<span")
                  write(">")
                  write("B")
                  write("</span>")
                }
                page Test() {
                  call Card@f0()
                }
                page Other() {
                  call Card@f1()
                }
                -- ir (optimized) --
                page Test() {
                  write("<span>A</span>")
                }
                page Other() {
                  write("<span>B</span>")
                }
                -- expected output --
                <span>A</span>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn functions_whose_names_differ_only_by_case() {
        check(
            indoc! {r#"
                -- main.hop --
                fn nav_bar(x: Int) -> Int {
                  x
                }

                fn NavBar() -> Html {
                  <b>nav</b>
                }

                page Test() {
                  fn body() -> Html {
                    <div><NavBar />{nav_bar(1).to_string()}</div>
                  }
                }
            "#},
            "<div><b>nav</b>1</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn NavBar@f0() -> Html {
                  write("<b")
                  write(">")
                  write("nav")
                  write("</b>")
                }
                fn nav_bar@f1(x@v0: Int) -> Int {
                  v0
                }
                page Test() {
                  write("<div")
                  write(">")
                  call NavBar@f0()
                  write_string(call nav_bar@f1(x = 1).to_string())
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div><b>nav</b>1</div>")
                }
                -- expected output --
                <div><b>nav</b>1</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn named_call_arguments_written_out_of_order() {
        check(
            indoc! {r#"
                -- main.hop --
                fn label(prefix: String, count: Int) -> String {
                  prefix + count.to_string()
                }

                page Test() {
                  fn body() -> Html {
                    <div>{label(count: 2, prefix: "n")}</div>
                  }
                }
            "#},
            "<div>n2</div>",
            expect![[r#"
                -- ir (unoptimized) --
                fn label@f0(prefix@v0: String, count@v1: Int) -> String {
                  (v0 + v1.to_string())
                }
                page Test() {
                  write("<div")
                  write(">")
                  write_string(call label@f0(prefix = "n", count = 2))
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>n2</div>")
                }
                -- expected output --
                <div>n2</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
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
