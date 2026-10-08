use crate::asset_path_rewriter::AssetPathRewriter;
use crate::document::Document;
use crate::document_annotator::DocumentAnnotator;
use crate::ir::lower_pure;
use crate::ir::pure_module::PureModule;
use crate::ir::runtime::evaluator;
use crate::ir::transpile::{RustTranspiler, Transpiler, TsTranspiler};
use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
use crate::program::Program;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::root_relative_file_path::RootRelativeFilePath;
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
    evaluator::evaluate_page(module, &page_name, HashMap::new(), None)
        .map_err(|e| format!("Evaluator failed: {}", e))
}

fn check(archive: &str, expected_output: &str, expected: Expect) {
    check_with_asset_path_rewriter(archive, None, expected_output, expected);
}

fn check_with_asset_path_rewriter(
    archive: &str,
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    expected_output: &str,
    expected: Expect,
) {
    let archive = Archive::from(archive);
    let mut program = Program::new();
    let mut modules = 0;
    for file in archive.iter() {
        assert!(
            file.name.ends_with(".hop"),
            "expected a .hop module, got '{}'",
            file.name
        );
        let document_id = RootContainedFilePath::new(&file.name).unwrap();
        let document = Document::new(document_id.clone(), file.content.clone());
        program.update_hop_document(&document_id, document);
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

    let typed_modules = program.typed_modules().clone();
    let registry = program.type_registry();

    // Compile to IR without optimization
    let unoptimized_options = OrchestrateOptions {
        skip_optimization: true,
        asset_path_rewriter: asset_path_rewriter.clone(),
        ..Default::default()
    };
    let unoptimized_pure = orchestrate_pure(&typed_modules, unoptimized_options);

    // Compile to IR with optimization
    let optimized_options = OrchestrateOptions {
        skip_optimization: false,
        asset_path_rewriter,
        ..Default::default()
    };
    let optimized_pure = orchestrate_pure(&typed_modules, optimized_options);

    // Evaluate the Pure modules before lowering consumes them.
    let unoptimized_eval = execute_evaluator(&unoptimized_pure);
    let optimized_eval = execute_evaluator(&optimized_pure);

    let unoptimized_module = lower_pure(unoptimized_pure, None);
    let optimized_module = lower_pure(optimized_pure, None);

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
            let expected = evaluator::evaluate_page(&module, &page_name, HashMap::new(), None)
                .unwrap_or_else(|e| panic!("Evaluator failed:\n{e}\n\nPure:\n{pure}"))
                .trim()
                .to_string();
            let module = lower_pure(module, None);
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
            let expected = evaluator::evaluate_page(&module, &page_name, HashMap::new(), None)
                .unwrap_or_else(|e| panic!("Evaluator failed:\n{e}\n\nPure:\n{pure}"))
                .trim()
                .to_string();
            let module = lower_pure(module, None);
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
                    let v1 = v0 in {
                      let v2 = v1.value in {
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
                    let v2 = v0.value in {
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
    fn logical_operators_short_circuit() {
        check(
            indoc! {r#"
                -- main.hop --
                fn spin() -> Bool {
                  spin()
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      {match false && spin() {
                        true => <>yes</>,
                        false => <>no</>,
                      }}
                      {match true || spin() {
                        true => <>yes</>,
                        false => <>no</>,
                      }}
                    </>
                  }
                }
            "#},
            "noyes",
            expect![[r#"
                -- ir (unoptimized) --
                fn spin@f0() -> Bool {
                  call spin@f0()
                }
                page Test() {
                  let v0 = (false && call spin@f0()) in {
                    match v0 {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                  let v1 = (true || call spin@f0()) in {
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
                -- ir (optimized) --
                fn spin@f0() -> Bool {
                  call spin@f0()
                }
                page Test() {
                  let v0 = (false && call spin@f0()) in {
                    match v0 {
                      true => {
                        write("yes")
                      }
                      false => {
                        write("no")
                      }
                    }
                  }
                  let v1 = (true || call spin@f0()) in {
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
                -- expected output --
                noyes
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                      let v5 = let v2 = v1 in {
                        match v2 {
                          Some(v3) => { let v4 = (v3 + " ") in { v4 } }
                          None => { "" }
                        }
                      } in {
                        write("<p>")
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
    fn tuple_literal_as_match_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Badge(admin: Bool, name: Option[String]) -> Html {
                  match (admin, name) {
                    (true, Some(n)) => <p>admin {n}</p>,
                    (true, None) => <p>admin</p>,
                    (false, _) => <p>guest</p>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Badge admin={true} name={Some("ada")}/>
                      <Badge admin={true} name={None}/>
                      <Badge admin={false} name={Some("bob")}/>
                    </>
                  }
                }
            "#},
            "<p>admin ada</p><p>admin</p><p>guest</p>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Badge@f0(
                  admin@v0: Bool,
                  name@v1: Option[String],
                ) -> Html {
                  let v2 = (v0, v1) in {
                    let v3 = v2.0 in {
                      let v4 = v2.1 in {
                        match v3 {
                          true => {
                            match v4 {
                              Some(v5) => {
                                write("<p>admin ")
                                write_string(v5)
                                write("</p>")
                              }
                              None => {
                                write("<p>admin</p>")
                              }
                            }
                          }
                          false => {
                            write("<p>guest</p>")
                          }
                        }
                      }
                    }
                  }
                }
                page Test() {
                  call Badge@f0(admin = true, name = Option[String]::Some("ada"))
                  call Badge@f0(admin = true, name = Option[String]::None)
                  call Badge@f0(admin = false, name = Option[String]::Some("bob"))
                }
                -- ir (optimized) --
                page Test() {
                  write("<p>admin ada</p><p>admin</p><p>guest</p>")
                }
                -- expected output --
                <p>admin ada</p><p>admin</p><p>guest</p>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn nested_tuple_literal_argument_destructured_by_match() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Row(cell: ((String, Int), (Bool,))) -> Html {
                  match cell {
                    ((label, count), (true,)) => <p>{label}: {count.to_string()}</p>,
                    ((label, _), (false,)) => <p>{label}</p>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Row cell={(("apples", 3), (true,))}/>
                      <Row cell={(("pears", 0), (false,))}/>
                    </>
                  }
                }
            "#},
            "<p>apples: 3</p><p>pears</p>",
            expect![[r#"
                -- ir (unoptimized) --
                fn Row@f0(cell@v0: ((String, Int), (Bool,))) -> Html {
                  let v1 = v0 in {
                    let v2 = v1.0 in {
                      let v3 = v1.1 in {
                        let v4 = v3.0 in {
                          match v4 {
                            true => {
                              let v5 = v2.0 in {
                                let v6 = v2.1 in {
                                  write("<p>")
                                  write_string(v5)
                                  write(": ")
                                  write_string(v6.to_string())
                                  write("</p>")
                                }
                              }
                            }
                            false => {
                              let v7 = v2.0 in {
                                write("<p>")
                                write_string(v7)
                                write("</p>")
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
                page Test() {
                  call Row@f0(cell = (("apples", 3), (true,)))
                  call Row@f0(cell = (("pears", 0), (false,)))
                }
                -- ir (optimized) --
                page Test() {
                  write("<p>apples: 3</p><p>pears</p>")
                }
                -- expected output --
                <p>apples: 3</p><p>pears</p>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                  write("<ul>")
                  let v0 = "Item" in {
                    let v1 = 2 in {
                      write("<li class=\"")
                      write_string(let v2 = "row" in {
                        ((v2 + "-") + "odd")
                      })
                      write("\">")
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
                    let v1 = v0 in {
                      let v2 = v1.n in {
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
                    let v2 = v0.n in {
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
                    let v1 = v0 in {
                      let v2 = v1.value in {
                        write_string(let v3 = v2 in {
                          match v3 { true => { "yes" } false => { "no" } }
                        })
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Flag {value: true}] {
                    let v2 = v0.value in {
                      write_string(match v2 {
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
                fn Button@f0(label@v0: String, id@v1: String) -> Html {
                  write("<button class=\"btn\" id=\"")
                  write_string(v1)
                  write("\">")
                  write_string(v0)
                  write("</button>")
                }
                page Test() {
                  call Button@f0(label = "Hi", id = "submit")
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
                fn Leaf(title?: String = "d") -> Html {
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
                fn First@f0(n@v0: Int, title@v1: String) -> Html {
                  call Second@f1(n = v0, title = v1)
                }
                fn Leaf@f2(title@v5: String) -> Html {
                  write("<div>")
                  write_string(v5)
                  write("</div>")
                }
                fn Second@f1(n@v2: Int, title@v3: String) -> Html {
                  call Leaf@f2(title = v3)
                  let v4 = (0 < v2) in {
                    match v4 {
                      true => {
                        call First@f0(n = (v2 - 1), title = "d")
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call First@f0(n = 1, title = "x")
                }
                -- ir (optimized) --
                fn First@f0(n@v0: Int, title@v1: String) -> Html {
                  call Second@f1(n = v0, title = v1)
                }
                fn Second@f1(n@v2: Int, title@v3: String) -> Html {
                  write("<div>")
                  write_string(v3)
                  write("</div>")
                  let v4 = (0 < v2) in {
                    match v4 {
                      true => {
                        call First@f0(n = (v2 - 1), title = "d")
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call First@f0(n = 1, title = "x")
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
                fn Base@f1(id@v3: String, data-k@v4: String) -> Html {
                  write("<div id=\"")
                  write_string(v3)
                  write("\" data-k=\"")
                  write_string(v4)
                  write("\"></div>")
                }
                fn Card@f0(
                  title@v0: String,
                  id@v1: String,
                  data-k@v2: String,
                ) -> Html {
                  write("<section><h1>")
                  write_string(v0)
                  write("</h1>")
                  call Base@f1(id = v1, data-k = v2)
                  write("</section>")
                }
                page Test() {
                  call Card@f0(title = "Hi", id = "x", data-k = "v")
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
                fn Wrapper@f0(show@v0: Bool, id@v1: String) -> Html {
                  let v2 = v0 in {
                    match v2 {
                      true => {
                        write("<div id=\"")
                        write_string(v1)
                        write("\"></div>")
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Wrapper@f0(show = true, id = "x")
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
    fn bool_attribute_is_present_when_true_and_absent_when_false() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Field(required: Bool) -> Html {
                  <input required={required}/>
                }

                fn Button(...rest) -> Html {
                  <button ...rest>
                  </button>
                }

                page Test() {
                  fn body() -> Html {
                    <>
                      <Field required={true}/>
                      <Field required={false}/>
                      <Button disabled={true}/>
                      <Button disabled={false}/>
                    </>
                  }
                }
            "#},
            r#"<input required><input><button disabled></button><button></button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f1(disabled@v1: Bool) -> Html {
                  write("<button")
                  match v1 {
                    true => {
                      write(" disabled")
                    }
                    false => {
                    }
                  }
                  write("></button>")
                }
                fn Field@f0(required@v0: Bool) -> Html {
                  write("<input")
                  match v0 {
                    true => {
                      write(" required")
                    }
                    false => {
                    }
                  }
                  write(">")
                }
                page Test() {
                  call Field@f0(required = true)
                  call Field@f0(required = false)
                  call Button@f1(disabled = true)
                  call Button@f1(disabled = false)
                }
                -- ir (optimized) --
                page Test() {
                  write("<input required><input><button disabled></button>")
                  write("<button></button>")
                }
                -- expected output --
                <input required><input><button disabled></button><button></button>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                fn Wrapper@f0(show@v0: Bool, id@v1: String) -> Html {
                  let v2 = v0 in {
                    match v2 {
                      true => {
                        write("<div id=\"")
                        write_string(v1)
                        write("\"></div>")
                      }
                      false => {
                      }
                    }
                  }
                }
                page Test() {
                  call Wrapper@f0(show = true, id = "x")
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
            r#"<div title="a'b&lt;c&amp;d"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Panel@f0(title@v0: String) -> Html {
                  write("<div title=\"")
                  write_string(v0)
                  write("\"></div>")
                }
                page Test() {
                  call Panel@f0(title = "a'b<c&d")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div title=\"a'b&lt;c&amp;d\"></div>")
                }
                -- expected output --
                <div title="a'b&lt;c&amp;d"></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn quoted_attribute_values_are_escaped() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <>
                      <span title="Tom &amp; Jerry" data-x="x<y"></span>
                      <input pattern="\\d+" title="say \"hi\""/>
                    </>
                  }
                }
            "#},
            r#"<span title="Tom &amp;amp; Jerry" data-x="x&lt;y"></span><input pattern="\d+" title="say &quot;hi&quot;">"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("<span title=\"Tom &amp;amp; Jerry\" data-x=\"x&lt;y\"></span>")
                  write("<input pattern=\"\\d+\" title=\"say &quot;hi&quot;\">")
                }
                -- ir (optimized) --
                page Test() {
                  write("<span title=\"Tom &amp;amp; Jerry\" data-x=\"x&lt;y\"></span>")
                  write("<input pattern=\"\\d+\" title=\"say &quot;hi&quot;\">")
                }
                -- expected output --
                <span title="Tom &amp;amp; Jerry" data-x="x&lt;y"></span><input pattern="\d+" title="say &quot;hi&quot;">
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn rest_escapes_quoted_attribute_values() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Panel(...rest) -> Html {
                  <div ...rest>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Panel title="a &amp; b"/>
                  }
                }
            "#},
            r#"<div title="a &amp;amp; b"></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Panel@f0(title@v0: String) -> Html {
                  write("<div title=\"")
                  write_string(v0)
                  write("\"></div>")
                }
                page Test() {
                  call Panel@f0(title = "a &amp; b")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div title=\"a &amp;amp; b\"></div>")
                }
                -- expected output --
                <div title="a &amp;amp; b"></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                  <img ...rest/>
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
                fn Icon@f0(src@v0: String, alt@v1: String) -> Html {
                  write("<img src=\"")
                  write_string(v0)
                  write("\" alt=\"")
                  write_string(v1)
                  write("\">")
                }
                page Test() {
                  call Icon@f0(src = "a.png", alt = "a")
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
                fn A@f0() -> Html {
                  write("<div></div>")
                }
                page Test() {
                  call A@f0()
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
    fn call_expression_passes_empty_rest() {
        check(
            indoc! {r#"
                -- main.hop --
                fn A(...rest) -> Html {
                  <div ...rest>
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    {A()}
                  }
                }
            "#},
            r#"<div></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn A@f0() -> Html {
                  write("<div></div>")
                }
                page Test() {
                  call A@f0()
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
    fn call_expression_forwards_rest_attributes_and_spread() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Button(
                  kind: String,
                  ...rest,
                ) -> Html {
                  <button class={kind} ...rest>
                    {kind}
                  </button>
                }

                fn Secondary(...rest) -> Html {
                  Button(kind: "secondary", ...rest)
                }

                page Test() {
                  fn body() -> Html {
                    Secondary(id: "save", "aria-label": "Save")
                  }
                }
            "#},
            r#"<button class="secondary" id="save" aria-label="Save">secondary</button>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Button@f1(
                  kind@v2: String,
                  id@v3: String,
                  aria-label@v4: String,
                ) -> Html {
                  write("<button class=\"")
                  write_string(v2)
                  write("\" id=\"")
                  write_string(v3)
                  write("\" aria-label=\"")
                  write_string(v4)
                  write("\">")
                  write_string(v2)
                  write("</button>")
                }
                fn Secondary@f0(
                  id@v0: String,
                  aria-label@v1: String,
                ) -> Html {
                  call Button@f1(kind = "secondary", id = v0, aria-label = v1)
                }
                page Test() {
                  call Secondary@f0(id = "save", aria-label = "Save")
                }
                -- ir (optimized) --
                page Test() {
                  write("<button class=\"secondary\" id=\"save\" aria-label=\"Save\">")
                  write("secondary</button>")
                }
                -- expected output --
                <button class="secondary" id="save" aria-label="Save">secondary</button>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                  data-foo@v2: String,
                ) -> Html {
                  write("<button class=\"")
                  write_string(v0)
                  write("\" data-foo=\"")
                  write_string(v2)
                  write("\">")
                  write_html(v1)
                  write("</button>")
                }
                page Test() {
                  call Button@f0(class = "p-2", children = {
                    write("Hi")
                  }, data-foo = "bar")
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
                fn Button@f0(children@v0: Html, data-x@v1: String) -> Html {
                  write("<button class=\"builtin\" data-x=\"")
                  write_string(v1)
                  write("\">")
                  write_html(v0)
                  write("</button>")
                }
                page Test() {
                  call Button@f0(children = {
                    write("Hi")
                  }, data-x = "y")
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
    fn renders_self_closing_svg_element_with_end_tag() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <svg>
                      <path d="M0 0"/>
                    </svg>
                  }
                }
            "#},
            r#"<svg><path d="M0 0"></path></svg>"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("<svg><path d=\"M0 0\"></path></svg>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<svg><path d=\"M0 0\"></path></svg>")
                }
                -- expected output --
                <svg><path d="M0 0"></path></svg>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                fn Svg@f0(viewBox@v0: String) -> Html {
                  write("<svg viewBox=\"")
                  write_string(v0)
                  write("\"></svg>")
                }
                page Test() {
                  call Svg@f0(viewBox = "0 0 100 100")
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
                fn Card@f1(title@v1: String) -> Html {
                  write("<div>")
                  write_string(v1)
                  write("</div>")
                }
                fn Wrapper@f0(title@v0: String) -> Html {
                  call Card@f1(title = v0)
                }
                page Test() {
                  call Wrapper@f0(title = "hi")
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
                fn Card@f1(title@v0: String) -> Html {
                  write("<div>")
                  write_string(v0)
                  write("</div>")
                }
                fn Wrapper@f0() -> Html {
                  call Card@f1(title = "explicit")
                }
                page Test() {
                  call Wrapper@f0()
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
                fn Card@f1(user@v2: User) -> Html {
                  write("<div>")
                  write_string(v2.name)
                  write("</div>")
                }
                fn Wrapper@f0(user@v1: User) -> Html {
                  call Card@f1(user = v1)
                }
                page Test() {
                  let v0 = User {name: "Ada"} in {
                    call Wrapper@f0(user = v0)
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
                      let v2 = v1 in {
                        let v3 = v2.value in {
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
                    let v3 = v1.value in {
                      write("outer")
                      write_string(v3)
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
                fn Bar@f1(name@v2: String, title@v3: String) -> Html {
                  write("<div>")
                  write_string(v2)
                  call Card@f2(title = v3)
                  write("</div>")
                }
                fn Baz@f0(name@v0: String, title@v1: String) -> Html {
                  call Bar@f1(name = v0, title = v1)
                }
                fn Card@f2(title@v4: String) -> Html {
                  write("<div>")
                  write_string(v4)
                  write("</div>")
                }
                page Test() {
                  call Baz@f0(name = "n", title = "t")
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
                fn Card@f1(count@v1: Int) -> Html {
                  let v2 = (0 < v1) in {
                    match v2 {
                      true => {
                        write("<div>positive</div>")
                      }
                      false => {
                      }
                    }
                  }
                }
                fn Wrapper@f0(count@v0: Int) -> Html {
                  call Card@f1(count = v0)
                }
                page Test() {
                  call Wrapper@f0(count = 3)
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
                fn A@f1(count@v2: Int, data-foo@v3: String) -> Html {
                  write("<div data-foo=\"")
                  write_string(v3)
                  write("\">")
                  let v4 = (0 < v2) in {
                    match v4 {
                      true => {
                        write("positive")
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                fn B@f0(count@v0: Int, data-foo@v1: String) -> Html {
                  call A@f1(count = v0, data-foo = v1)
                }
                page Test() {
                  call B@f0(count = 3, data-foo = "bar")
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
                fn Bar@f1(children@v1: Html) -> Html {
                  call Foo@f2(children = v1)
                }
                fn Baz@f0(children@v0: Html) -> Html {
                  call Bar@f1(children = v0)
                }
                fn Foo@f2(children@v2: Html) -> Html {
                  write("<div>")
                  write_html(v2)
                  write("</div>")
                }
                page Test() {
                  call Baz@f0(children = {
                    write("deep")
                  })
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
    fn accepts_param_reserved_out_of_rest_when_callee_param_is_optional() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Inner(
                  class?: String = "x",
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
                fn Inner@f1(class@v1: String) -> Html {
                  write("<span class=\"")
                  write_string(v1)
                  write("\"></span>")
                }
                fn Outer@f0(class@v0: String) -> Html {
                  write("<div class=\"")
                  write_string(v0)
                  write("\">")
                  call Inner@f1(class = "x")
                  write("</div>")
                }
                page Test() {
                  call Outer@f0(class = "x")
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
                  class?: String = "",
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
                fn Button@f0(children@v0: Html, class@v1: String) -> Html {
                  call Foo@f1(children = {
                    write_html(v0)
                  }, class = v1)
                }
                fn Foo@f1(children@v2: Html, class@v3: String) -> Html {
                  write("<div class=\"")
                  write_string(v3)
                  write("\">")
                  write_html(v2)
                  write("</div>")
                }
                page Test() {
                  call Button@f0(children = {
                    write("click")
                  }, class = "primary")
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
                  class?: String = "x",
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
                fn Inner@f1(class@v1: String) -> Html {
                  write("<span class=\"")
                  write_string(v1)
                  write("\"></span>")
                }
                fn Wrapper@f0(class@v0: String) -> Html {
                  call Inner@f1(class = v0)
                }
                page Test() {
                  call Wrapper@f0(class = "y")
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
    fn accepts_optional_param_chain_with_caller_value() {
        check(
            indoc! {r#"
                -- main.hop --
                fn A(
                  class?: String = "",
                  ...rest,
                ) -> Html {
                  <div class={class} ...rest>
                  </div>
                }

                fn B(
                  class?: String = "",
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
                fn A@f1(class@v1: String) -> Html {
                  write("<div class=\"")
                  write_string(v1)
                  write("\"></div>")
                }
                fn B@f0(class@v0: String) -> Html {
                  call A@f1(class = v0)
                }
                page Test() {
                  call B@f0(class = "main")
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
    fn accepts_optional_param_chain_uses_outer_fallback() {
        check(
            indoc! {r#"
                -- main.hop --
                fn A(
                  class?: String = "a",
                  ...rest,
                ) -> Html {
                  <div class={class} ...rest>
                  </div>
                }

                fn B(
                  class?: String = "b",
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
                fn A@f1(class@v1: String) -> Html {
                  write("<div class=\"")
                  write_string(v1)
                  write("\"></div>")
                }
                fn B@f0(class@v0: String) -> Html {
                  call A@f1(class = v0)
                }
                page Test() {
                  call B@f0(class = "b")
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
    fn accepts_forwarded_optional_param_through_rest() {
        check(
            indoc! {r#"
                -- main.hop --
                fn A(
                  label?: String = "x",
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
                fn A@f1(label@v1: String) -> Html {
                  write("<span>")
                  write_string(v1)
                  write("</span>")
                }
                fn B@f0(label@v0: String) -> Html {
                  call A@f1(label = v0)
                }
                page Test() {
                  call B@f0(label = "x")
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
    fn accepts_forwarded_fallback_materialized_once_in_chain() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Leaf(
                  label?: String = "x",
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
                fn Leaf@f2(label@v2: String) -> Html {
                  write("<span>")
                  write_string(v2)
                  write("</span>")
                }
                fn Mid@f1(label@v1: String) -> Html {
                  call Leaf@f2(label = v1)
                }
                fn Top@f0(label@v0: String) -> Html {
                  call Mid@f1(label = v0)
                }
                page Test() {
                  call Top@f0(label = "x")
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
                fn Inner@f1(title@v1: String, lang@v2: String) -> Html {
                  write("<span title=\"")
                  write_string(v1)
                  write("\" lang=\"")
                  write_string(v2)
                  write("\"></span>")
                }
                fn Wrapper@f0(lang@v0: String) -> Html {
                  call Inner@f1(title = "a", lang = v0)
                }
                page Test() {
                  call Wrapper@f0(lang = "en")
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
    fn forwarded_param_is_not_captured_by_let() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title: String) -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  let title = "local";
                  <section>
                    {title}
                    <Card ...rest/>
                  </section>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper title="hi"/>
                  }
                }
            "#},
            r#"<section>local<div>hi</div></section>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f1(title@v2: String) -> Html {
                  write("<div>")
                  write_string(v2)
                  write("</div>")
                }
                fn Wrapper@f0(title@v0: String) -> Html {
                  let v1 = "local" in {
                    write("<section>")
                    write_string(v1)
                    call Card@f1(title = v0)
                    write("</section>")
                  }
                }
                page Test() {
                  call Wrapper@f0(title = "hi")
                }
                -- ir (optimized) --
                page Test() {
                  write("<section>local<div>hi</div></section>")
                }
                -- expected output --
                <section>local<div>hi</div></section>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn forwarded_param_is_not_captured_by_for_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title: String) -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  for title in ["a", "b"] {
                    <p>
                      {title}
                      <Card ...rest/>
                    </p>
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper title="hi"/>
                  }
                }
            "#},
            r#"<p>a<div>hi</div></p><p>b<div>hi</div></p>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f1(title@v2: String) -> Html {
                  write("<div>")
                  write_string(v2)
                  write("</div>")
                }
                fn Wrapper@f0(title@v0: String) -> Html {
                  for v1 in ["a", "b"] {
                    write("<p>")
                    write_string(v1)
                    call Card@f1(title = v0)
                    write("</p>")
                  }
                }
                page Test() {
                  call Wrapper@f0(title = "hi")
                }
                -- ir (optimized) --
                page Test() {
                  for v5 in ["a", "b"] {
                    write("<p>")
                    write_string(v5)
                    write("<div>hi</div></p>")
                  }
                }
                -- expected output --
                <p>a<div>hi</div></p><p>b<div>hi</div></p>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn forwarded_param_is_not_captured_by_match_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title: String) -> Html {
                  <div>
                    {title}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  match Some("m") {
                    Some(title) => {
                      <p>
                        {title}
                        <Card ...rest/>
                      </p>
                    },
                    None => <></>,
                  }
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper title="hi"/>
                  }
                }
            "#},
            r#"<p>m<div>hi</div></p>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f1(title@v3: String) -> Html {
                  write("<div>")
                  write_string(v3)
                  write("</div>")
                }
                fn Wrapper@f0(title@v0: String) -> Html {
                  let v1 = Option[String]::Some("m") in {
                    match v1 {
                      Some(v2) => {
                        write("<p>")
                        write_string(v2)
                        call Card@f1(title = v0)
                        write("</p>")
                      }
                      None => {
                      }
                    }
                  }
                }
                page Test() {
                  call Wrapper@f0(title = "hi")
                }
                -- ir (optimized) --
                page Test() {
                  write("<p>m<div>hi</div></p>")
                }
                -- expected output --
                <p>m<div>hi</div></p>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn rest_spread_on_element_is_not_captured_by_let() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Wrapper(...rest) -> Html {
                  let rest = <b>x</b>;
                  <div ...rest>
                    {rest}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper id="hi"/>
                  }
                }
            "#},
            r#"<div id="hi"><b>x</b></div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Wrapper@f0(id@v0: String) -> Html {
                  let v1 = {
                    write("<b>x</b>")
                  } in {
                    write("<div id=\"")
                    write_string(v0)
                    write("\">")
                    write_html(v1)
                    write("</div>")
                  }
                }
                page Test() {
                  call Wrapper@f0(id = "hi")
                }
                -- ir (optimized) --
                page Test() {
                  let v3 = {
                    write("<b>x</b>")
                  } in {
                    write("<div id=\"hi\">")
                    write_html(v3)
                    write("</div>")
                  }
                }
                -- expected output --
                <div id="hi"><b>x</b></div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn rest_spread_into_function_is_not_captured_by_let() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  ...rest,
                ) -> Html {
                  <div ...rest>
                    {title}
                  </div>
                }

                fn Wrapper(...rest) -> Html {
                  let rest = "local";
                  <section>
                    {rest}
                    <Card title="t" ...rest/>
                  </section>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrapper id="hi"/>
                  }
                }
            "#},
            r#"<section>local<div id="hi">t</div></section>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Card@f1(title@v2: String, id@v3: String) -> Html {
                  write("<div id=\"")
                  write_string(v3)
                  write("\">")
                  write_string(v2)
                  write("</div>")
                }
                fn Wrapper@f0(id@v0: String) -> Html {
                  let v1 = "local" in {
                    write("<section>")
                    write_string(v1)
                    call Card@f1(title = "t", id = v0)
                    write("</section>")
                  }
                }
                page Test() {
                  call Wrapper@f0(id = "hi")
                }
                -- ir (optimized) --
                page Test() {
                  write("<section>local<div id=\"hi\">t</div></section>")
                }
                -- expected output --
                <section>local<div id="hi">t</div></section>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                fn A@f1(tabindex@v2: Int, data-x@v3: String) -> Html {
                  write("<div data-x=\"")
                  write_string(v3)
                  write("\">")
                  let v4 = (0 < v2) in {
                    match v4 {
                      true => {
                        write("focusable")
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                fn B@f0(tabindex@v0: Int, data-x@v1: String) -> Html {
                  call A@f1(tabindex = v0, data-x = v1)
                }
                page Test() {
                  call B@f0(tabindex = 2, data-x = "y")
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
                    let v3 = let v1 = v0 in {
                      match v1 {
                        Some(v2) => { Option[String]::Some(v2) }
                        None => { Option[String]::None }
                      }
                    } in {
                      let v4 = v3 in {
                        match v4 {
                          Some(v5) => {
                            write("mapped:")
                            write_string(v5)
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
                  let v2 = let v0 = Point {x: "hi", y: "bye"} in {
                    let v1 = v0.x in { v1 }
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
                  let v1 = let v0 = Point {x: "hi", y: "bye"} in {
                    v0.x
                  } in {
                    write("got:")
                    write_string(v1)
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
                        write("got:")
                        write_string(v1)
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
                    let v3 = Option[String]::Some(let v1 = v0 in {
                      match v1 { Some(v2) => { v2 } None => { "default" } }
                    }) in {
                      let v4 = v3 in {
                        match v4 {
                          Some(v5) => {
                            write_string(v5)
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
                    write("<div>")
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
                  write("<p>")
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
                fn Rows@f0(items@v1: Array[String], id@v2: String) -> Html {
                  for v3 in v1 {
                    write("<div id=\"")
                    write_string(v2)
                    write("\">")
                    write_string(v3)
                    write("</div>")
                  }
                }
                page Test() {
                  let v0 = "outer" in {
                    call Rows@f0(items = ["a", "b"], id = v0)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v6 in ["a", "b"] {
                    write("<div id=\"outer\">")
                    write_string(v6)
                    write("</div>")
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
                    write_string(let v1 = v0 in {
                      match v1 { true => { "yes" } false => { "no" } }
                    })
                  }
                  let v2 = false in {
                    write_string(let v3 = v2 in {
                      match v3 { true => { "YES" } false => { "NO" } }
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
                    write_string(let v1 = v0 in {
                      match v1 { Some(_) => { "some" } None => { "none" } }
                    })
                  }
                  write(",")
                  let v2 = Option[String]::None in {
                    write_string(let v3 = v2 in {
                      match v3 { Some(_) => { "SOME" } None => { "NONE" } }
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
                      write_string(let v2 = v0 in {
                        match v2 {
                          true => {
                            let v3 = v1 in {
                              match v3 {
                                true => { "TT" }
                                false => { "TF" }
                              }
                            }
                          }
                          false => { "F" }
                        }
                      })
                    }
                  }
                  write(",")
                  let v4 = false in {
                    let v5 = true in {
                      write_string(let v6 = v4 in {
                        match v6 {
                          true => {
                            let v7 = v5 in {
                              match v7 {
                                true => { "TT" }
                                false => { "TF" }
                              }
                            }
                          }
                          false => { "F" }
                        }
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
                  let v0 = -123 in {
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
    fn nan_comparisons() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let e10 = 10000000000.0;
                    let e100 = e10 * e10 * e10 * e10 * e10 * e10 * e10 * e10 * e10 * e10;
                    let inf = e100 * e100 * e100 * e100;
                    let nan = inf * 0.0;
                    match (
                      nan == nan,
                      nan != nan,
                      nan < nan,
                      nan <= nan,
                      nan > nan,
                      nan >= nan,
                      nan < inf,
                      -inf < nan,
                      nan.to_int() == 0,
                    ) {
                      (false, true, false, false, false, false, false, false, true) => <>ok</>,
                      _ => <>wrong</>,
                    }
                  }
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 10000000000 in {
                    let v1 = (((((((((v0 * v0) * v0) * v0) * v0) * v0) * v0) * v0) * v0) * v0) in {
                      let v2 = (((v1 * v1) * v1) * v1) in {
                        let v3 = (v2 * 0) in {
                          let v4 = (
                            (v3 == v3),
                            (!(v3 == v3)),
                            (v3 < v3),
                            (v3 <= v3),
                            (v3 < v3),
                            (v3 <= v3),
                            (v3 < v2),
                            ((-v2) < v3),
                            (v3.to_int() == 0),
                          ) in {
                            let v5 = v4.0 in {
                              let v6 = v4.1 in {
                                let v7 = v4.2 in {
                                  let v8 = v4.3 in {
                                    let v9 = v4.4 in {
                                      let v10 = v4.5 in {
                                        let v11 = v4.6 in {
                                          let v12 = v4.7 in {
                                            let v13 = v4.8 in {
                                              match v13 {
                                                true => {
                                                  match v12 {
                                                    true => {
                                                      write("wrong")
                                                    }
                                                    false => {
                                                      match v11 {
                                                        true => {
                                                          write("wrong")
                                                        }
                                                        false => {
                                                          match v10 {
                                                            true => {
                                                              write("wrong")
                                                            }
                                                            false => {
                                                              match v9 {
                                                                true => {
                                                                  write("wrong")
                                                                }
                                                                false => {
                                                                  match v8 {
                                                                    true => {
                                                                      write("wrong")
                                                                    }
                                                                    false => {
                                                                      match v7 {
                                                                        true => {
                                                                          write("wrong")
                                                                        }
                                                                        false => {
                                                                          match v6 {
                                                                            true => {
                                                                              match v5 {
                                                                                true => {
                                                                                  write("wrong")
                                                                                }
                                                                                false => {
                                                                                  write("ok")
                                                                                }
                                                                              }
                                                                            }
                                                                            false => {
                                                                              write("wrong")
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                                false => {
                                                  write("wrong")
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
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
    fn int_arithmetic_wraps_at_bounds() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let max = 2147483647;
                    let min = -2147483648;
                    match (
                      max + 1 == min,
                      min - 1 == max,
                      -min == min,
                      max * 2 == -2,
                      max * max == 1,
                      min * -1 == min,
                    ) {
                      (true, true, true, true, true, true) => <>ok</>,
                      _ => <>wrong</>,
                    }
                  }
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 2147483647 in {
                    let v1 = -2147483648 in {
                      let v2 = (
                        ((v0 + 1) == v1),
                        ((v1 - 1) == v0),
                        ((-v1) == v1),
                        ((v0 * 2) == -2),
                        ((v0 * v0) == 1),
                        ((v1 * -1) == v1),
                      ) in {
                        let v3 = v2.0 in {
                          let v4 = v2.1 in {
                            let v5 = v2.2 in {
                              let v6 = v2.3 in {
                                let v7 = v2.4 in {
                                  let v8 = v2.5 in {
                                    match v8 {
                                      true => {
                                        match v7 {
                                          true => {
                                            match v6 {
                                              true => {
                                                match v5 {
                                                  true => {
                                                    match v4 {
                                                      true => {
                                                        match v3 {
                                                          true => {
                                                            write("ok")
                                                          }
                                                          false => {
                                                            write("wrong")
                                                          }
                                                        }
                                                      }
                                                      false => {
                                                        write("wrong")
                                                      }
                                                    }
                                                  }
                                                  false => {
                                                    write("wrong")
                                                  }
                                                }
                                              }
                                              false => {
                                                write("wrong")
                                              }
                                            }
                                          }
                                          false => {
                                            write("wrong")
                                          }
                                        }
                                      }
                                      false => {
                                        write("wrong")
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
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
    fn float_to_int() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let e10 = 10000000000.0;
                    let e100 = e10 * e10 * e10 * e10 * e10 * e10 * e10 * e10 * e10 * e10;
                    let inf = e100 * e100 * e100 * e100;
                    let whole = 5.0;
                    let positive = 3.7;
                    let negative = -2.9;
                    let above = 2147483648.0;
                    let top = 2147483647.9;
                    let bottom = -2147483648.9;
                    let below = -2147483649.0;
                    let half = -0.5;
                    match (
                      whole.to_int() == 5,
                      positive.to_int() == 3,
                      negative.to_int() == -2,
                      inf.to_int() == 2147483647,
                      (-inf).to_int() == -2147483648,
                      above.to_int() == 2147483647,
                      top.to_int() == 2147483647,
                      bottom.to_int() == -2147483648,
                      below.to_int() == -2147483648,
                      half.to_int().to_string() == "0",
                    ) {
                      (true, true, true, true, true, true, true, true, true, true) => <>ok</>,
                      _ => <>wrong</>,
                    }
                  }
                }
            "#},
            "ok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 10000000000 in {
                    let v1 = (((((((((v0 * v0) * v0) * v0) * v0) * v0) * v0) * v0) * v0) * v0) in {
                      let v2 = (((v1 * v1) * v1) * v1) in {
                        let v3 = 5 in {
                          let v4 = 3.7 in {
                            let v5 = -2.9 in {
                              let v6 = 2147483648 in {
                                let v7 = 2147483647.9 in {
                                  let v8 = -2147483648.9 in {
                                    let v9 = -2147483649 in {
                                      let v10 = -0.5 in {
                                        let v11 = (
                                          (v3.to_int() == 5),
                                          (v4.to_int() == 3),
                                          (v5.to_int() == -2),
                                          (v2.to_int() == 2147483647),
                                          ((-v2).to_int() == -2147483648),
                                          (v6.to_int() == 2147483647),
                                          (v7.to_int() == 2147483647),
                                          (v8.to_int() == -2147483648),
                                          (v9.to_int() == -2147483648),
                                          (v10.to_int().to_string() == "0"),
                                        ) in {
                                          let v12 = v11.0 in {
                                            let v13 = v11.1 in {
                                              let v14 = v11.2 in {
                                                let v15 = v11.3 in {
                                                  let v16 = v11.4 in {
                                                    let v17 = v11.5 in {
                                                      let v18 = v11.6 in {
                                                        let v19 = v11.7 in {
                                                          let v20 = v11.8 in {
                                                            let v21 = v11.9 in {
                                                              match v21 {
                                                                true => {
                                                                  match v20 {
                                                                    true => {
                                                                      match v19 {
                                                                        true => {
                                                                          match v18 {
                                                                            true => {
                                                                              match v17 {
                                                                                true => {
                                                                                  match v16 {
                                                                                    true => {
                                                                                      match v15 {
                                                                                        true => {
                                                                                          match v14 {
                                                                                            true => {
                                                                                              match v13 {
                                                                                                true => {
                                                                                                  match v12 {
                                                                                                    true => {
                                                                                                      write("ok")
                                                                                                    }
                                                                                                    false => {
                                                                                                      write("wrong")
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                                false => {
                                                                                                  write("wrong")
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                            false => {
                                                                                              write("wrong")
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                        false => {
                                                                                          write("wrong")
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    false => {
                                                                                      write("wrong")
                                                                                    }
                                                                                  }
                                                                                }
                                                                                false => {
                                                                                  write("wrong")
                                                                                }
                                                                              }
                                                                            }
                                                                            false => {
                                                                              write("wrong")
                                                                            }
                                                                          }
                                                                        }
                                                                        false => {
                                                                          write("wrong")
                                                                        }
                                                                      }
                                                                    }
                                                                    false => {
                                                                      write("wrong")
                                                                    }
                                                                  }
                                                                }
                                                                false => {
                                                                  write("wrong")
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
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
    fn special_numeric_literals() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let inf = 200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0;
                    let neg_inf = -200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0;
                    let min = -2147483648;
                    let nan = inf * 0.0;
                    <>
                      {match (
                        0.0 < inf,
                        inf * 2.0 == inf,
                        neg_inf < 0.0,
                        neg_inf * 2.0 == neg_inf,
                        min - 1 == 2147483647,
                      ) {
                        (true, true, true, true, true) => <>ok</>,
                        _ => <>wrong</>,
                      }}
                      {for i in 0..=1 {
                        let x = i.to_float();
                        match nan + x != nan + x {
                          true => <>ok</>,
                          false => <>wrong</>,
                        }
                      }}
                    </>
                  }
                }
            "#},
            "okokok",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = inf in {
                    let v1 = -inf in {
                      let v2 = -2147483648 in {
                        let v3 = (v0 * 0) in {
                          let v4 = (
                            (0 < v0),
                            ((v0 * 2) == v0),
                            (v1 < 0),
                            ((v1 * 2) == v1),
                            ((v2 - 1) == 2147483647),
                          ) in {
                            let v5 = v4.0 in {
                              let v6 = v4.1 in {
                                let v7 = v4.2 in {
                                  let v8 = v4.3 in {
                                    let v9 = v4.4 in {
                                      match v9 {
                                        true => {
                                          match v8 {
                                            true => {
                                              match v7 {
                                                true => {
                                                  match v6 {
                                                    true => {
                                                      match v5 {
                                                        true => {
                                                          write("ok")
                                                        }
                                                        false => {
                                                          write("wrong")
                                                        }
                                                      }
                                                    }
                                                    false => {
                                                      write("wrong")
                                                    }
                                                  }
                                                }
                                                false => {
                                                  write("wrong")
                                                }
                                              }
                                            }
                                            false => {
                                              write("wrong")
                                            }
                                          }
                                        }
                                        false => {
                                          write("wrong")
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          for v10 in 0..=1 {
                            let v11 = v10.to_float() in {
                              let v12 = (!((v3 + v11) == (v3 + v11))) in {
                                match v12 {
                                  true => {
                                    write("ok")
                                  }
                                  false => {
                                    write("wrong")
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("ok")
                  for v10 in 0..=1 {
                    let v11 = v10.to_float() in {
                      let v12 = (!((NaN + v11) == (NaN + v11))) in {
                        match v12 {
                          true => {
                            write("ok")
                          }
                          false => {
                            write("wrong")
                          }
                        }
                      }
                    }
                  }
                }
                -- expected output --
                okokok
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn range_loops_at_bounds() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let max = 2147483647;
                    let min = -2147483648;
                    <>
                      {for i in max - 1..=max {
                        <>
                          {i.to_string()}
                          ,
                        </>
                      }}
                      {for i in min..=min + 1 {
                        <>
                          {i.to_string()}
                          ,
                        </>
                      }}
                      {for _ in 3..=1 {
                        <>wrong</>
                      }}
                      {for _ in max..=min {
                        <>wrong</>
                      }}
                    </>
                  }
                }
            "#},
            "2147483646,2147483647,-2147483648,-2147483647,",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = 2147483647 in {
                    let v1 = -2147483648 in {
                      for v2 in (v0 - 1)..=v0 {
                        write_string(v2.to_string())
                        write(",")
                      }
                      for v3 in v1..=(v1 + 1) {
                        write_string(v3.to_string())
                        write(",")
                      }
                      for _ in 3..=1 {
                        write("wrong")
                      }
                      for _ in v0..=v1 {
                        write("wrong")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v2 in 2147483646..=2147483647 {
                    write_string(v2.to_string())
                    write(",")
                  }
                  for v3 in -2147483648..=-2147483647 {
                    write_string(v3.to_string())
                    write(",")
                  }
                  for _ in 3..=1 {
                    write("wrong")
                  }
                  for _ in 2147483647..=-2147483648 {
                    write("wrong")
                  }
                }
                -- expected output --
                2147483646,2147483647,-2147483648,-2147483647,
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                  write("<h1>Hello, World!</h1>")
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
                  write("<h1>Hello, World!</h1>")
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
                    let v1 = v0 in {
                      match v1 {
                        true => {
                          write("Visible")
                        }
                        false => {
                        }
                      }
                    }
                    let v2 = (!v0) in {
                      match v2 {
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
                    let v1 = v0 in {
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
                    write("<span class=\"")
                    write_string(v0)
                    write(" px-2 py-1\">")
                    write_string(v0)
                    write("!?</span>")
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
                      write_string(v0)
                      write_string(v1)
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
                    let v1 = v0 in {
                      match v1 {
                        Shape::Rect(width: v2, height: v3) => {
                          write_string(v2)
                          write("-")
                          write_string(v3)
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
    fn bool_logical_and() {
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
    fn bool_logical_or() {
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          write_string(v2)
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
                    let v1 = v0 in {
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
    fn option_match_test_and_wildcard_in_some() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    for x in [Some(true), Some(false), None] {
                      match x {
                        Some(true) => <>a</>,
                        Some(_) => <>b</>,
                        None => <>c</>,
                      }
                    }
                  }
                }
            "#},
            "abc",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    Option[Bool]::Some(true),
                    Option[Bool]::Some(false),
                    Option[Bool]::None,
                  ] {
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          match v2 {
                            true => {
                              write("a")
                            }
                            false => {
                              write("b")
                            }
                          }
                        }
                        None => {
                          write("c")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Option[Bool]::Some(true),
                    Option[Bool]::Some(false),
                    Option[Bool]::None,
                  ] {
                    match v0 {
                      Some(v2) => {
                        match v2 {
                          true => {
                            write("a")
                          }
                          false => {
                            write("b")
                          }
                        }
                      }
                      None => {
                        write("c")
                      }
                    }
                  }
                }
                -- expected output --
                abc
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn record_match_test_and_wildcard_in_field() {
        check(
            indoc! {r#"
                -- main.hop --
                record Foo {
                  a: Bool,
                  b: Option[String],
                }

                page Test() {
                  fn body() -> Html {
                    for x in [
                      Foo {a: true, b: Some("a")},
                      Foo {a: true, b: None},
                      Foo {a: false, b: Some("x")},
                    ] {
                      match x {
                        Foo {a: true, b: Some(n)} => <>{n}</>,
                        Foo {a: true, b: None} => <>b</>,
                        Foo {a: false, b: _} => <>c</>,
                      }
                    }
                  }
                }
            "#},
            "abc",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    Foo {a: true, b: Option[String]::Some("a")},
                    Foo {a: true, b: Option[String]::None},
                    Foo {a: false, b: Option[String]::Some("x")},
                  ] {
                    let v1 = v0 in {
                      let v2 = v1.a in {
                        let v3 = v1.b in {
                          match v2 {
                            true => {
                              match v3 {
                                Some(v4) => {
                                  write_string(v4)
                                }
                                None => {
                                  write("b")
                                }
                              }
                            }
                            false => {
                              write("c")
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
                    Foo {a: true, b: Option[String]::Some("a")},
                    Foo {a: true, b: Option[String]::None},
                    Foo {a: false, b: Option[String]::Some("x")},
                  ] {
                    let v2 = v0.a in {
                      let v3 = v0.b in {
                        match v2 {
                          true => {
                            match v3 {
                              Some(v4) => {
                                write_string(v4)
                              }
                              None => {
                                write("b")
                              }
                            }
                          }
                          false => {
                            write("c")
                          }
                        }
                      }
                    }
                  }
                }
                -- expected output --
                abc
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn enum_match_test_and_wildcard_in_field() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Status {
                  Active {
                    admin: Bool,
                  },
                  Inactive,
                }

                page Test() {
                  fn body() -> Html {
                    for x in [
                      Status::Active {admin: true},
                      Status::Active {admin: false},
                      Status::Inactive,
                    ] {
                      match x {
                        Status::Active {admin: true} => <>a</>,
                        Status::Active {admin: _} => <>b</>,
                        Status::Inactive => <>c</>,
                      }
                    }
                  }
                }
            "#},
            "abc",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  for v0 in [
                    Status::Active {admin: true},
                    Status::Active {admin: false},
                    Status::Inactive,
                  ] {
                    let v1 = v0 in {
                      match v1 {
                        Status::Active(admin: v2) => {
                          match v2 {
                            true => {
                              write("a")
                            }
                            false => {
                              write("b")
                            }
                          }
                        }
                        Status::Inactive => {
                          write("c")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Status::Active {admin: true},
                    Status::Active {admin: false},
                    Status::Inactive,
                  ] {
                    match v0 {
                      Status::Active(admin: v2) => {
                        match v2 {
                          true => {
                            write("a")
                          }
                          false => {
                            write("b")
                          }
                        }
                      }
                      Status::Inactive => {
                        write("c")
                      }
                    }
                  }
                }
                -- expected output --
                abc
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                    let v3 = Option[String]::Some(let v1 = v0 in {
                      match v1 { Some(v2) => { v2 } None => { "default" } }
                    }) in {
                      let v4 = v3 in {
                        match v4 {
                          Some(v5) => {
                            write_string(v5)
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          write("[")
                          write_string(v2)
                          write("]")
                        }
                        None => {
                          write("[_]")
                        }
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
                      Some(v2) => {
                        write("[")
                        write_string(v2)
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
                    write_string(let v1 = v0 in {
                      match v1 {
                        Color::Red => { "red" }
                        Color::Green => { "green" }
                        Color::Blue => { "blue" }
                      }
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
                    let v1 = v0 in {
                      match v1 {
                        Outcome::Success(value: v2) => {
                          write("Ok: ")
                          write_string(v2)
                        }
                        Outcome::Failure(message: v3) => {
                          write("Err: ")
                          write_string(v3)
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
                    let v1 = v0 in {
                      match v1 {
                        Item::Tagged(tag: v2) => {
                          write("tag: ")
                          write_string(v2)
                        }
                        Item::Plain => {
                          write("plain")
                        }
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
                  let v3 = let v0 = Outcome::Success {value: "hi"} in {
                    match v0 {
                      Outcome::Success {value: v1} => { v1 }
                      Outcome::Failure {message: v2} => { v2 }
                    }
                  } in {
                    write("Got: ")
                    write_string(v3)
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
                fn Badge@f0(color@v0: Color) -> Html {
                  let v1 = v0 in {
                    match v1 {
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
                    let v1 = v0 in {
                      match v1 {
                        Outcome::Success(value: v2) => {
                          write("Ok: ")
                          write_string(v2)
                        }
                        Outcome::Failure(message: v3) => {
                          write("Err: ")
                          write_string(v3)
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
                    let v1 = v0 in {
                      match v1 {
                        Response::Success(code: v2, body: v3) => {
                          write_string(v2)
                          write(" ")
                          write_string(v3)
                        }
                        Response::Failure(reason: v4) => {
                          write("Error: ")
                          write_string(v4)
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
                    let v1 = v0 in {
                      match v1 {
                        Outcome::Success(value: v2) => {
                          write("Ok: ")
                          write_string(v2)
                        }
                        Outcome::Failure(message: v3) => {
                          write("Err: ")
                          write_string(v3)
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          match v2 {
                            Some(v3) => {
                              write_string(v3)
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
                    write_string(let v1 = v0 in {
                      match v1 { Some(_) => { "some" } None => { "none" } }
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
                    write_string(let v1 = v0 in {
                      match v1 { Some(_) => { "some" } None => { "none" } }
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          match v2 {
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
                    let v1 = v0 in {
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
                    let v1 = v0 in {
                      match v1 {
                        Outcome::Success => {
                          write("ok")
                        }
                        Outcome::Failure => {
                          write("err")
                        }
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
                    let v1 = v0 in {
                      match v1 {
                        Outcome::Success => {
                          write("ok")
                        }
                        Outcome::Failure => {
                          write("err")
                        }
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
                    let v1 = v0 in {
                      let v2 = v1.age in {
                        write("age: ")
                        write_string(v2.to_string())
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          match v2 {
                            Some(v3) => {
                              match v3 {
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
                    let v1 = v0 in {
                      match v1 {
                        Outer::Success(value: v2) => {
                          match v2 {
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
                    let v1 = v0 in {
                      match v1 {
                        true => {
                          write("t")
                        }
                        false => {
                          write("f")
                        }
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
                    let v1 = v0 in {
                      match v1 {
                        true => {
                          write("t")
                        }
                        false => {
                          write("f")
                        }
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
                        let v2 = Option[String]::Some("inner") in {
                          match v2 {
                            Some(v3) => {
                              write_string(v1)
                              write(":")
                              write_string(v3)
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          let v3 = v2 in {
                            match v3 {
                              Some(v4) => {
                                write("value:")
                                write_string(v4)
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
                  write("<div class=\"foo bar baz\"></div>")
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
                      write("a: ")
                      write_string(v0)
                      write(", b: ")
                      write_string(v1.to_string())
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
                    write("a{b")
                    write_string(v0)
                    write("d}e")
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
    fn format_macro_resolves_escape_sequences() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name = "c";
                    <>{format!("a\"b\n{}\\d", name)}</>
                  }
                }
            "#},
            "a&quot;b\nc\\d",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "c" in {
                    write("a&quot;b\n")
                    write_string(v0)
                    write("\\d")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("a&quot;b\nc\\d")
                }
                -- expected output --
                a&quot;b
                c\d
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                    write("<div class=\"")
                    write_string(v0)
                    write("\"></div>")
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
                    write("<span>")
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
                    <input type={type}/>
                  }
                }
            "#},
            r#"<input type="button">"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = "button" in {
                    write("<input type=\"")
                    write_string(v0)
                    write("\">")
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
                  write("<label for=\"email\">Email</label>")
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
                        let v3 = v2 in {
                          match v3 {
                            Some(v4) => {
                              write("[")
                              write_string(v4)
                              write("]")
                            }
                            None => {
                              write("[-]")
                            }
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
                        Some(v4) => {
                          write("[")
                          write_string(v4)
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
                      let v3 = let v2 = v1 in {
                        match v2 { true => { v0.a } false => { "default" } }
                      } in {
                        write("[")
                        write_string(v3)
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
                    next: Option[Node]::None,
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
                    let v1 = v0 in {
                      match v1 {
                        Expr::Literal(value: v2) => {
                          write_string(v2)
                        }
                        Expr::Neg => {
                          write("neg")
                        }
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
                    let v1 = v0 in {
                      match v1 {
                        Expr::Literal => {
                          write("lit")
                        }
                        Expr::Neg(inner: v2) => {
                          let v3 = v2 in {
                            match v3 {
                              Expr::Literal(value: v4) => {
                                write_string(v4)
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
                      Expr::Neg(inner: v2) => {
                        match v2 {
                          Expr::Literal(value: v4) => {
                            write_string(v4)
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
                    let v1 = v0 in {
                      match v1 {
                        Expr::Literal(value: v2) => {
                          write("lit:")
                          write_string(v2)
                        }
                        Expr::Neg => {
                          write("neg")
                        }
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
                    parent: Option[File]::None,
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
                  let v0 = Leaf {back: Option[Expr]::None} in {
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
                  let v0 = Option[Node]::None in {
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
                    next: Option[Node]::None,
                  } in {
                    let v2 = Node {
                      value: "head",
                      next: let v1 = true in {
                        match v1 {
                          true => { Option[Node]::Some(v0) }
                          false => { Option[Node]::None }
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
                    next: Option[Option[Node]]::None,
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
                    next: Option[Option[Node]]::Some(Option[Node]::Some(Node {
                      value: "tail",
                      next: Option[Option[Node]]::None,
                    })),
                  } in {
                    let v1 = v0.next in {
                      match v1 {
                        Some(v2) => {
                          let v3 = v2 in {
                            match v3 {
                              Some(v4) => {
                                write_string(v4.value)
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
                    next: Option[Node]::None,
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
                  let v0 = A {b: B {name: "b", a: Option[A]::None}} in {
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
                  let v0 = Tree::Node {label: "a", left: Tree::Leaf, right: Option[Tree]::None} in {
                    let v1 = v0 in {
                      match v1 {
                        Tree::Node(label: v2, left: v3, right: v4) => {
                          let v5 = Step {t: v3, rest: v4} in {
                            write_string(v2)
                            let v6 = v5.rest in {
                              match v6 {
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
                        Tree::Leaf => {
                          write("empty")
                        }
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
                    let v1 = v0 in {
                      match v1 {
                        Contact::Email(address: v2, label: v3) => {
                          write_string(v2)
                          let v4 = v3 in {
                            match v4 {
                              Some(v5) => {
                                write_string(v5)
                              }
                              None => {
                                write("no-label")
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
                    Tree::Node {label: "a", kid: Option[Tree]::None},
                  ] {
                    let v1 = v0 in {
                      match v1 {
                        Tree::Node(label: v2, kid: v3) => {
                          let v4 = Tree::Node {label: "b", kid: v3} in {
                            match v4 {
                              Tree::Node(label: v5, kid: v6) => {
                                write_string(v2)
                                write_string(v5)
                                let v7 = v6 in {
                                  match v7 {
                                    Some(_) => {
                                      write("s")
                                    }
                                    None => {
                                      write("n")
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
                        Tree::Leaf => {
                          write("empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Tree]::None},
                  ] {
                    match v0 {
                      Tree::Node(label: v2, kid: v3) => {
                        let v4 = Tree::Node {label: "b", kid: v3} in {
                          match v4 {
                            Tree::Node(label: v5, kid: v6) => {
                              write_string(v2)
                              write_string(v5)
                              match v6 {
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
                    let v1 = v0 in {
                      match v1 {
                        Tree::Node(label: v2, kid: v3) => {
                          let v4 = Tree::Node {label: "b", kid: v3} in {
                            match v4 {
                              Tree::Node(label: v5) => {
                                write_string(v2)
                                write_string(v5)
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
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Tree::Node {label: "a", kid: Tree::Leaf}] {
                    match v0 {
                      Tree::Node(label: v2, kid: v3) => {
                        let v4 = Tree::Node {label: "b", kid: v3} in {
                          match v4 {
                            Tree::Node(label: v5) => {
                              write_string(v2)
                              write_string(v5)
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
                    Tree::Node {label: "a", kid: Option[Option[Tree]]::None},
                  ] {
                    let v1 = v0 in {
                      match v1 {
                        Tree::Node(label: v2, kid: v3) => {
                          let v4 = Tree::Node {label: "b", kid: v3} in {
                            match v4 {
                              Tree::Node(label: v5) => {
                                write_string(v2)
                                write_string(v5)
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
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Option[Tree]]::None},
                  ] {
                    match v0 {
                      Tree::Node(label: v2, kid: v3) => {
                        let v4 = Tree::Node {label: "b", kid: v3} in {
                          match v4 {
                            Tree::Node(label: v5) => {
                              write_string(v2)
                              write_string(v5)
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
                  for v0 in [Wrap::Full {h: Option[Holder]::None}] {
                    let v1 = v0 in {
                      match v1 {
                        Wrap::Full(h: v2) => {
                          let v3 = Wrap::Full {h: v2} in {
                            match v3 {
                              Wrap::Full(h: v4) => {
                                let v5 = v4 in {
                                  match v5 {
                                    Some(v6) => {
                                      write_string(v6.tag)
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
                        Wrap::Empty => {
                          write("empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [Wrap::Full {h: Option[Holder]::None}] {
                    match v0 {
                      Wrap::Full(h: v2) => {
                        let v3 = Wrap::Full {h: v2} in {
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
                fn pick@f0(t@v2: Tree) -> Option[Tree] {
                  let v3 = v2 in {
                    match v3 {
                      Tree::Node {kid: v4} => { v4 }
                      Tree::Leaf => { Option[Tree]::None }
                    }
                  }
                }
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Tree]::None},
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
                    Tree::Node {label: "a", kid: Option[Tree]::None},
                  ] {
                    let v1 = match v0 {
                      Tree::Node {kid: v7} => { v7 }
                      Tree::Leaf => { Option[Tree]::None }
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
                fn depth@f0(t@v3: Option[Tree]) -> Int {
                  let v4 = v3 in {
                    match v4 { Some(_) => { 1 } None => { 0 } }
                  }
                }
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Tree]::None},
                  ] {
                    let v1 = v0 in {
                      match v1 {
                        Tree::Node(kid: v2) => {
                          write_string(call depth@f0(t = v2).to_string())
                        }
                        Tree::Leaf => {
                          write("empty")
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [
                    Tree::Node {label: "a", kid: Option[Tree]::None},
                  ] {
                    match v0 {
                      Tree::Node(kid: v2) => {
                        write_string(match v2 {
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
                  write("<div class=\"card\"><h2>")
                  write_string(v0)
                  write("</h2>")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "Hello", children = {
                    write("<p>world</p>")
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
                fn Inner@f1(children@v1: Html) -> Html {
                  write("<div class=\"inner\">")
                  write_html(v1)
                  write("</div>")
                }
                fn Outer@f0(children@v0: Html) -> Html {
                  write("<div class=\"outer\">")
                  call Inner@f1(children = {
                    write_html(v0)
                  })
                  write("</div>")
                }
                page Test() {
                  call Outer@f0(children = {
                    write("<p>hello</p>")
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
                fn Footer@f1() -> Html {
                  write("<footer><p>Copyright 2024</p></footer>")
                }
                fn Header@f0(title@v0: String) -> Html {
                  write("<header><h1>")
                  write_string(v0)
                  write("</h1></header>")
                }
                fn Layout@f2(children@v1: Html) -> Html {
                  write("<div class=\"layout\">")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Layout@f2(children = {
                    call Header@f0(title = "Welcome")
                    write("<main><p>Hello world</p></main>")
                    call Footer@f1()
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
                  write("<div class=\"first\">")
                  write_html(v0)
                  write("</div><div class=\"second\">")
                  write_html(v0)
                  write("</div>")
                }
                page Test() {
                  call Repeat@f0(children = {
                    write("<span>hi</span>")
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
                fn Badge@f1(text@v4: String) -> Html {
                  write("<strong>")
                  write_string(v4)
                  write("</strong>")
                }
                fn NodeView@f0(node@v1: Node) -> Html {
                  call Badge@f1(text = v1.value)
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
                  let v0 = Node {
                    value: "a",
                    next: Option[Node]::Some(Node {
                      value: "b",
                      next: Option[Node]::None,
                    }),
                  } in {
                    call NodeView@f0(node = v0)
                  }
                }
                -- ir (optimized) --
                fn NodeView@f0(node@v1: Node) -> Html {
                  write("<strong>")
                  write_string(v1.value)
                  write("</strong>")
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
                    next: Option[Node]::Some(Node {
                      value: "b",
                      next: Option[Node]::None,
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
                fn NodeView@f0(node@v1: Node) -> Html {
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
                  let v0 = Node {
                    value: "a",
                    next: Option[Node]::Some(Node {
                      value: "b",
                      next: Option[Node]::Some(Node {
                        value: "c",
                        next: Option[Node]::None,
                      }),
                    }),
                  } in {
                    call NodeView@f0(node = v0)
                  }
                }
                -- ir (optimized) --
                fn NodeView@f0(node@v1: Node) -> Html {
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
                    next: Option[Node]::Some(Node {
                      value: "b",
                      next: Option[Node]::Some(Node {
                        value: "c",
                        next: Option[Node]::None,
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
    fn function_with_optional_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title?: String = "New card") -> Html {
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
                  write("<div>")
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
    fn function_with_negative_fallback_values() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Offset(dx?: Int = -1, scale?: Float = -2.5) -> Html {
                  <div>
                    {(dx * 3).to_string()} {(scale * 2.0).to_int().to_string()}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Offset/>
                  }
                }
            "#},
            r#"<div>-3 -5</div>"#,
            expect![[r#"
                -- ir (unoptimized) --
                fn Offset@f0(dx@v0: Int, scale@v1: Float) -> Html {
                  write("<div>")
                  write_string((v0 * 3).to_string())
                  write(" ")
                  write_string((v1 * 2).to_int().to_string())
                  write("</div>")
                }
                page Test() {
                  call Offset@f0(dx = -1, scale = -2.5)
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>-3 -5</div>")
                }
                -- expected output --
                <div>-3 -5</div>
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn function_with_optional_parameter_overridden() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(title?: String = "New card") -> Html {
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
                  write("<div>")
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
    fn function_with_mixed_optional_and_required_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  subtitle?: String = "No subtitle",
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
                  write("<div>")
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
    fn function_with_mixed_optional_and_required_parameters_all_provided() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title: String,
                  subtitle?: String = "No subtitle",
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
                  write("<div>")
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
    fn function_with_multiple_optional_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Card(
                  title?: String = "Default",
                  subtitle?: String = "Sub",
                  footer?: String = "End",
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
                  write("<div>")
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
                  children?: Html = <></>,
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
                  write("<div class=\"card\"><h2>")
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
                  children?: Html = <></>,
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
                  write("<div class=\"card\"><h2>")
                  write_string(v0)
                  write("</h2>")
                  write_html(v1)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(title = "With", children = {
                    write("<p>body</p>")
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
    fn option_unwrap_or_some() {
        check(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    let name = Some("Alice");
                    <>{name.unwrap_or("anonymous")}</>
                  }
                }
            "#},
            "Alice",
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  let v0 = Option[String]::Some("Alice") in {
                    write_string(match v0 {
                      Some(v1) => { v1 }
                      None => { "anonymous" }
                    })
                  }
                }
                -- ir (optimized) --
                page Test() {
                  write("Alice")
                }
                -- expected output --
                Alice
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
                -- ts (optimized) --
                OK
                -- rust (optimized) --
                OK
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn option_unwrap_or_none() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Greeting(name: Option[String]) -> Html {
                  <>{name.unwrap_or("anonymous")}</>
                }

                page Test() {
                  fn body() -> Html {
                    <Greeting name={None} />
                  }
                }
            "#},
            "anonymous",
            expect![[r#"
                -- ir (unoptimized) --
                fn Greeting@f0(name@v0: Option[String]) -> Html {
                  write_string(match v0 {
                    Some(v1) => { v1 }
                    None => { "anonymous" }
                  })
                }
                page Test() {
                  call Greeting@f0(name = Option[String]::None)
                }
                -- ir (optimized) --
                page Test() {
                  write("anonymous")
                }
                -- expected output --
                anonymous
                -- eval (unoptimized) --
                OK
                -- eval (optimized) --
                OK
                -- ts (unoptimized) --
                OK
                -- rust (unoptimized) --
                OK
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
                fn RenderItem@f0(item@v0: Item) -> Html {
                  let v1 = v0 in {
                    match v1 {
                      Item::Todo(label: v2, done: v3) => {
                        let v4 = v3 in {
                          match v4 {
                            true => {
                              write("[x]")
                            }
                            false => {
                            }
                          }
                        }
                        let v5 = (!v3) in {
                          match v5 {
                            true => {
                              write("[ ]")
                            }
                            false => {
                            }
                          }
                        }
                        write_string(v2)
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
                fn Render@f0(time@v0: TimeAgo) -> Html {
                  let v1 = v0 in {
                    match v1 {
                      TimeAgo::MinutesAgo(count: v2) => {
                        let v3 = (v2 == 1) in {
                          match v3 {
                            true => {
                              write("1 minute ago")
                            }
                            false => {
                              write_string(v2.to_string())
                              write(" minutes ago")
                            }
                          }
                        }
                      }
                      TimeAgo::HoursAgo(count: v4) => {
                        let v5 = (v4 == 1) in {
                          match v5 {
                            true => {
                              write("1 hour ago")
                            }
                            false => {
                              write_string(v4.to_string())
                              write(" hours ago")
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
                fn RenderCode@f0(block@v0: CodeBlock) -> Html {
                  let v1 = v0 in {
                    match v1 {
                      CodeBlock::Snippet(code: v2) => {
                        write("<code>")
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
                fn Render@f0(el@v0: ButtonElement) -> Html {
                  let v1 = v0 in {
                    match v1 {
                      ButtonElement::Link(href: v2) => {
                        write("<a href=\"")
                        write_string(v2)
                        write("\">link</a>")
                      }
                      ButtonElement::Button(type: v3) => {
                        write("<button type=\"")
                        write_string(v3)
                        write("\">btn</button>")
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
                  let v0 = Option[Target]::Some(Target {
                    id: "1",
                    title: "hello",
                  }) in {
                    let v3 = [
                      let v1 = v0 in {
                        match v1 {
                          Some(v2) => { Option[String]::Some(v2.title) }
                          None => { Option[String]::None }
                        }
                      },
                    ] in {
                      for v4 in v3 {
                        let v5 = v4 in {
                          match v5 {
                            Some(v6) => {
                              write("[")
                              write_string(v6)
                              write("]")
                            }
                            None => {
                            }
                          }
                        }
                      }
                      let v7 = v0 in {
                        match v7 {
                          Some(v8) => {
                            write_string(v8.title)
                          }
                          None => {
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v4 in [Option[String]::Some("hello")] {
                    match v4 {
                      Some(v6) => {
                        write("[")
                        write_string(v6)
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
        check_with_asset_path_rewriter(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <img src={asset!("/logo.svg")}/>
                  }
                }
            "#},
            Some(Arc::new(|asset_path: &RootRelativeFilePath| {
                format!("/hop_assets/{}", asset_path.as_str())
            })),
            r#"<img src="/hop_assets/logo.svg">"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("<img src=\"/hop_assets/logo.svg\">")
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
        check_with_asset_path_rewriter(
            indoc! {r#"
                -- main.hop --
                page Test() {
                  fn body() -> Html {
                    <img src={asset!("/logo.svg")}/>
                  }
                }
            "#},
            Some(Arc::new(|asset_path: &RootRelativeFilePath| {
                assert_eq!(asset_path.as_str(), "logo.svg");
                "/static/v1/logo-a1b2c3d4.svg".to_string()
            })),
            r#"<img src="/static/v1/logo-a1b2c3d4.svg">"#,
            expect![[r#"
                -- ir (unoptimized) --
                page Test() {
                  write("<img src=\"/static/v1/logo-a1b2c3d4.svg\">")
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
                -- ir (optimized) --
                fn Nest@f0(depth@v0: Int, children@v1: Html) -> Html {
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        write("<div>")
                        call Nest@f0(depth = (v0 - 1), children = v1)
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
                    write("<div>")
                    write_html(v1)
                    write("</div>")
                  }
                }
                page Test() {
                  call Foo@f0(children = {
                    write("<b>hi</b>")
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
                fn Inner@f1(children@v1: Html) -> Html {
                  write("<em>")
                  write_html(v1)
                  write("</em>")
                }
                fn Outer@f0(children@v0: Html) -> Html {
                  write("<section>")
                  call Inner@f1(children = {
                    write_html(v0)
                  })
                  write("</section>")
                }
                page Test() {
                  call Outer@f0(children = {
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
                fn Nest@f0(n@v0: Int, id@v1: String) -> Html {
                  write("<div id=\"")
                  write_string(v1)
                  write("\">")
                  let v2 = (0 < v0) in {
                    match v2 {
                      true => {
                        call Nest@f1(n = (v0 - 1))
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                fn Nest@f1(n@v3: Int) -> Html {
                  write("<div>")
                  let v4 = (0 < v3) in {
                    match v4 {
                      true => {
                        call Nest@f1(n = (v3 - 1))
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  call Nest@f0(n = 2, id = "root")
                }
                -- ir (optimized) --
                fn Nest@f1(n@v3: Int) -> Html {
                  write("<div>")
                  let v4 = (0 < v3) in {
                    match v4 {
                      true => {
                        call Nest@f1(n = (v3 - 1))
                      }
                      false => {
                      }
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  write("<div id=\"root\">")
                  call Nest@f1(n = 1)
                  write("</div>")
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
                  let v2 = v1 in {
                    match v2 {
                      Some(v3) => {
                        write_string(v3)
                      }
                      None => {
                        write("x")
                      }
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
                    Some(v3) => {
                      write_string(v3)
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
                  let v1 = v0 in {
                    match v1 {
                      Some(v2) => {
                        match v2 {
                          true => {
                            write("<span>yes</span>")
                          }
                          false => {
                            write("<span>no</span>")
                          }
                        }
                      }
                      None => {
                      }
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
                    let v1 = v0 in {
                      match v1 {
                        Some(v2) => {
                          match v2 {
                            Some(v3) => {
                              match v3 {
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
                  write("<span>")
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
                    write("<div>")
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
                    write("<div>")
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
                    write("<div>")
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
                    write("<div>")
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
                    let v1 = v0 in {
                      match v1 {
                        E::A(class: v2) => {
                          write("<div>")
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
                    let v2 = let v1 = v0 in {
                      State {query: v1.query, num: 2}
                    } in {
                      write_string(v2.query)
                      write_string(v2.num.to_string())
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
                    let v2 = let v1 = v0 in {
                      State {query: "b", num: 2}
                    } in {
                      write_string(v2.query)
                      write_string(v2.num.to_string())
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
                    write_string(let v1 = v0 in {
                      State {query: "x", num: v1.num}
                    }.query)
                    write_string(let v2 = v0 in {
                      State {query: "x", num: v2.num}
                    }.num.to_string())
                  }
                }
                -- ir (optimized) --
                page Test() {
                  for v0 in [State {query: "a", num: 7}] {
                    write_string(State {query: "x", num: v0.num}.query)
                    write_string(State {
                      query: "x",
                      num: v0.num,
                    }.num.to_string())
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
                fn Row@f0(item@v4: Item) -> Html {
                  write("<div>")
                  write_string(v4.label)
                  write("</div>")
                }
                page Test() {
                  for v0 in [Item {label: "a", selected: false}] {
                    let v1 = v0.selected in {
                      match v1 {
                        true => {
                          call Row@f0(item = let v2 = v0 in {
                            Item {label: "on", selected: v2.selected}
                          })
                        }
                        false => {
                          call Row@f0(item = let v3 = v0 in {
                            Item {label: "off", selected: v3.selected}
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
                          write("<div>")
                          write_string(Item {
                            label: "on",
                            selected: v0.selected,
                          }.label)
                          write("</div>")
                        }
                        false => {
                          write("<div>")
                          write_string(Item {
                            label: "off",
                            selected: v0.selected,
                          }.label)
                          write("</div>")
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
                fn Dark@f0(s@v1: State) -> Html {
                  let v3 = let v2 = v1.settings in {
                    Settings {theme: "dark", compact: v2.compact}
                  } in {
                    let v5 = let v4 = v1 in {
                      State {query: v4.query, settings: v3}
                    } in {
                      write_string(v5.query)
                      write_string(v5.settings.theme)
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
                pub fn label(prefix: String, count?: Int = 1) -> String {
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
                  write("<div>")
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
    fn function_with_omitted_optional_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                fn label(prefix?: String = "x", count?: Int = 1) -> String {
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
                  write("<div>")
                  write_string(call label@f0(prefix = "x", count = 1))
                  write_string(call label@f0(prefix = "x", count = 2))
                  write_string(call label@f0(prefix = "y", count = 1))
                  write("</div>")
                }
                page Other(prefix@v0: String) {
                  write("<div>")
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
                  write("<div>")
                  for v0 in 0..=call foo@f1(x = -7) {
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
                  write("<div>")
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
                  write("<div><span>hello</span></div>")
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
                  write("<div>")
                  write_html(v0)
                  write("</div>")
                }
                page Test() {
                  call wrap@f0(children = {
                    write("<span>hello</span>")
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
                  write("<div>")
                  write_html(v0)
                  write("</div>")
                }
                page Test() {
                  call Card@f0(slot = {
                    write("<span>hello</span>")
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
                  let v1 = v0 in {
                    match v1 {
                      true => {
                        write("<b>yes</b>")
                      }
                      false => {
                        write("<i>no</i>")
                      }
                    }
                  }
                }
                page Test() {
                  write("<div>")
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
                  write("<div>")
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
                fn mk@f1() -> Shape {
                  Shape::Square
                }
                page Test() {
                  write("<div>")
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
                fn mk@f0() -> Shape {
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
                      Some(_) => {
                        let v1 = v0 in {
                          match v1 {
                            Some(v2) => { v2 }
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
                  write("<div>")
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
                  write("<span>A</span>")
                }
                fn Card@f1() -> Html {
                  write("<span>B</span>")
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
                  write("<b>nav</b>")
                }
                fn nav_bar@f1(x@v0: Int) -> Int {
                  v0
                }
                page Test() {
                  write("<div>")
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
                  write("<div>")
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
