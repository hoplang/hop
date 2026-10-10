use crate::asset_path_rewriter::AssetPathRewriter;
use crate::document::Document;
use crate::document_annotator::DocumentAnnotator;
use crate::ir::flat_module::FlatModule;
use crate::ir::ir_page::IrPage;
use crate::ir::runtime::flat_evaluator;
use crate::ir::transpile::{RustTranspiler, Transpiler, TsTranspiler};
use crate::ir::{flat_to_writer, optimize_flat, pure_to_flat};
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

fn execute_evaluator(module: &FlatModule, pages: &[IrPage]) -> Result<String, String> {
    let page_name = TypeName::parse("Test").unwrap();
    flat_evaluator::evaluate_page(module, pages, &page_name, HashMap::new(), None)
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

    let (pure, pages) = orchestrate_pure(
        &typed_modules,
        OrchestrateOptions {
            asset_path_rewriter,
            ..Default::default()
        },
    );
    let unoptimized_flat = pure_to_flat(pure.clone());
    let optimized_flat = optimize_flat(pure_to_flat(pure));

    // Evaluate the Flat modules before lowering consumes them.
    let unoptimized_eval = execute_evaluator(&unoptimized_flat, &pages);
    let optimized_eval = execute_evaluator(&optimized_flat, &pages);

    let unoptimized_module = flat_to_writer(unoptimized_flat, &pages, None);
    let optimized_module = flat_to_writer(optimized_flat, &pages, None);

    let unoptimized_ir = unoptimized_module.to_string();
    let optimized_ir = optimized_module.to_string();

    let mut output = format!(
        "-- ir (unoptimized) --\n{}-- ir (optimized) --\n{}-- expected output --\n{}\n",
        unoptimized_ir, optimized_ir, expected_output
    );

    // Test evaluator on the unoptimized Flat module
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

    // Test evaluator on the optimized Flat module
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
    use crate::ir::runtime::EvalError;
    use expect_test::expect;
    use indoc::indoc;

    #[test]
    #[ignore]
    fn fuzz_transpile_ts_renders_identically() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module_with_test_view(u);
            let pure = module.to_string();
            let pages = IrPage::for_entries(&module);
            let page_name = TypeName::parse("Test").unwrap();
            let module = pure_to_flat(module);
            let expected = match flat_evaluator::evaluate_page(
                &module,
                &pages,
                &page_name,
                HashMap::new(),
                None,
            ) {
                Ok(output) => output.trim().to_string(),
                Err(EvalError::RecursionLimit { .. }) => return Ok(()),
                Err(e) => panic!("Evaluator failed:\n{e}\n\nPure:\n{pure}"),
            };
            let module = flat_to_writer(optimize_flat(module), &pages, None);
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
            let pages = IrPage::for_entries(&module);
            let page_name = TypeName::parse("Test").unwrap();
            let module = pure_to_flat(module);
            let expected = match flat_evaluator::evaluate_page(
                &module,
                &pages,
                &page_name,
                HashMap::new(),
                None,
            ) {
                Ok(output) => output.trim().to_string(),
                Err(EvalError::RecursionLimit { .. }) => return Ok(()),
                Err(e) => panic!("Evaluator failed:\n{e}\n\nPure:\n{pure}"),
            };
            let module = flat_to_writer(optimize_flat(module), &pages, None);
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
                  let v0: Bool = true
                  let v1: Flag = {value: v0}
                  let v2: Array[Flag] = [v1]
                  for b0: Flag in v2 {
                    let v4: Bool = b0.value
                    let v6: Bool = match v4 {
                      true => {
                        v4
                      }
                      false => {
                        let v5: Bool = false
                        v5
                      }
                    }
                    match v6 {
                      true => {
                        write("yes")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Bool = true
                  let v1: Flag = {value: v0}
                  let v2: Array[Flag] = [v1]
                  for b0: Flag in v2 {
                    let v4: Bool = b0.value
                    let v6: Bool = match v4 {
                      true => {
                        v4
                      }
                      false => {
                        let v5: Bool = false
                        v5
                      }
                    }
                    match v6 {
                      true => {
                        write("yes")
                      }
                      false => {
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
                  let v0: Bool = call spin@f0()
                  v0
                }
                page Test() {
                  let v1: Bool = false
                  let v3: Bool = match v1 {
                    true => {
                      let v2: Bool = call spin@f0()
                      v2
                    }
                    false => {
                      v1
                    }
                  }
                  let v9: Bool = true
                  let v11: Bool = match v9 {
                    true => {
                      v9
                    }
                    false => {
                      let v10: Bool = call spin@f0()
                      v10
                    }
                  }
                  match v3 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
                    }
                  }
                  match v11 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
                    }
                  }
                }
                -- ir (optimized) --
                fn spin@f0() -> Bool {
                  let v0: Bool = call spin@f0()
                  v0
                }
                page Test() {
                  write("noyes")
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
                  let v1: String = "bar"
                  let v2: Option[String] = Some(v1)
                  let v7: String = match v2 {
                    Some(b3: String) => {
                      let v4: String = " "
                      let v5: String = concat(b3, v4)
                      v5
                    }
                    None => {
                      let v6: String = ""
                      v6
                    }
                  }
                  write("<p>")
                  write_string(v7)
                  write("foo</p>")
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
                fn Badge@f0(admin@b0: Bool, name@b1: Option[String]) -> Html {
                  let v2: (Bool, Option[String]) = (b0, b1)
                  let v3: Bool = v2.0
                  let v4: Option[String] = v2.1
                  match v3 {
                    true => {
                      match v4 {
                        Some(b5: String) => {
                          write("<p>admin ")
                          write_string(b5)
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
                page Test() {
                  let v18: Bool = true
                  let v19: String = "ada"
                  let v20: Option[String] = Some(v19)
                  let v22: Bool = true
                  let v23: Option[String] = None
                  let v25: Bool = false
                  let v26: String = "bob"
                  let v27: Option[String] = Some(v26)
                  write_function Badge@f0(v18, v20)
                  write_function Badge@f0(v22, v23)
                  write_function Badge@f0(v25, v27)
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
                fn Row@f0(cell@b0: ((String, Int), (Bool,))) -> Html {
                  let v1: (String, Int) = b0.0
                  let v2: (Bool,) = b0.1
                  let v3: Bool = v2.0
                  match v3 {
                    true => {
                      let v4: String = v1.0
                      let v5: Int = v1.1
                      let v8: String = v5.to_string()
                      write("<p>")
                      write_string(v4)
                      write(": ")
                      write_string(v8)
                      write("</p>")
                    }
                    false => {
                      let v12: String = v1.0
                      write("<p>")
                      write_string(v12)
                      write("</p>")
                    }
                  }
                }
                page Test() {
                  let v17: String = "apples"
                  let v18: Int = 3
                  let v19: (String, Int) = (v17, v18)
                  let v20: Bool = true
                  let v21: (Bool,) = (v20,)
                  let v22: ((String, Int), (Bool,)) = (v19, v21)
                  let v24: String = "pears"
                  let v25: Int = 0
                  let v26: (String, Int) = (v24, v25)
                  let v27: Bool = false
                  let v28: (Bool,) = (v27,)
                  let v29: ((String, Int), (Bool,)) = (v26, v28)
                  write_function Row@f0(v22)
                  write_function Row@f0(v29)
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
                  let v1: Int = 2
                  let v9: String = v1.to_string()
                  write("<ul><li class=\"row-odd\">Item: ")
                  write_string(v9)
                  write("</li></ul>")
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
                  let v0: Int = 57
                  let v1: Count = {n: v0}
                  let v2: Array[Count] = [v1]
                  for b0: Count in v2 {
                    let v4: Int = b0.n
                    let v5: Int = 57
                    let v6: Bool = v4 == v5
                    match v6 {
                      true => {
                        write("eq")
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 57
                  let v1: Count = {n: v0}
                  let v2: Array[Count] = [v1]
                  for b0: Count in v2 {
                    let v4: Int = b0.n
                    let v5: Int = 57
                    let v6: Bool = v4 == v5
                    match v6 {
                      true => {
                        write("eq")
                      }
                      false => {
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
                  let v0: Bool = true
                  let v1: Flag = {value: v0}
                  let v2: Array[Flag] = [v1]
                  for b0: Flag in v2 {
                    let v4: Bool = b0.value
                    let v7: String = match v4 {
                      true => {
                        let v5: String = "yes"
                        v5
                      }
                      false => {
                        let v6: String = "no"
                        v6
                      }
                    }
                    write_string(v7)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Bool = true
                  let v1: Flag = {value: v0}
                  let v2: Array[Flag] = [v1]
                  for b0: Flag in v2 {
                    let v4: Bool = b0.value
                    let v7: String = match v4 {
                      true => {
                        let v5: String = "yes"
                        v5
                      }
                      false => {
                        let v6: String = "no"
                        v6
                      }
                    }
                    write_string(v7)
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
                fn Button@f0(label@b0: String, id@b1: String) -> Html {
                  write("<button class=\"btn\" id=\"")
                  write_string(b1)
                  write("\">")
                  write_string(b0)
                  write("</button>")
                }
                page Test() {
                  let v6: String = "Hi"
                  let v7: String = "submit"
                  write_function Button@f0(v6, v7)
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
                fn First@f0(n@b0: Int, title@b1: String) -> Html {
                  write_function Second@f1(b0, b1)
                }
                fn Leaf@f2(title@b5: String) -> Html {
                  write("<div>")
                  write_string(b5)
                  write("</div>")
                }
                fn Second@f1(n@b2: Int, title@b3: String) -> Html {
                  let v9: Int = 0
                  let v11: Bool = v9 < b2
                  write_function Leaf@f2(b3)
                  match v11 {
                    true => {
                      let v13: Int = 1
                      let v14: Int = b2 - v13
                      let v15: String = "d"
                      write_function First@f0(v14, v15)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v20: Int = 1
                  let v21: String = "x"
                  write_function First@f0(v20, v21)
                }
                -- ir (optimized) --
                fn First@f0(n@b0: Int, title@b1: String) -> Html {
                  write_function Second@f1(b0, b1)
                }
                fn Second@f1(n@b2: Int, title@b3: String) -> Html {
                  let v9: Int = 0
                  let v11: Bool = v9 < b2
                  write("<div>")
                  write_string(b3)
                  write("</div>")
                  match v11 {
                    true => {
                      let v13: Int = 1
                      let v14: Int = b2 - v13
                      let v15: String = "d"
                      write_function First@f0(v14, v15)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v20: Int = 1
                  let v21: String = "x"
                  write_function First@f0(v20, v21)
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
                fn Base@f1(id@b3: String, data-k@b4: String) -> Html {
                  write("<div id=\"")
                  write_string(b3)
                  write("\" data-k=\"")
                  write_string(b4)
                  write("\"></div>")
                }
                fn Card@f0(title@b0: String, id@b1: String, data-k@b2: String) -> Html {
                  write("<section><h1>")
                  write_string(b0)
                  write("</h1>")
                  write_function Base@f1(b1, b2)
                  write("</section>")
                }
                page Test() {
                  let v13: String = "Hi"
                  let v14: String = "x"
                  let v15: String = "v"
                  write_function Card@f0(v13, v14, v15)
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
                fn Wrapper@f0(show@b0: Bool, id@b1: String) -> Html {
                  match b0 {
                    true => {
                      write("<div id=\"")
                      write_string(b1)
                      write("\"></div>")
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v6: Bool = true
                  let v7: String = "x"
                  write_function Wrapper@f0(v6, v7)
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
                fn Button@f1(disabled@b1: Bool) -> Html {
                  write("<button")
                  match b1 {
                    true => {
                      write(" disabled")
                    }
                    false => {
                    }
                  }
                  write("></button>")
                }
                fn Field@f0(required@b0: Bool) -> Html {
                  write("<input")
                  match b0 {
                    true => {
                      write(" required")
                    }
                    false => {
                    }
                  }
                  write(">")
                }
                page Test() {
                  let v6: Bool = true
                  let v8: Bool = false
                  let v10: Bool = true
                  let v12: Bool = false
                  write_function Field@f0(v6)
                  write_function Field@f0(v8)
                  write_function Button@f1(v10)
                  write_function Button@f1(v12)
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
                fn Wrapper@f0(show@b0: Bool, id@b1: String) -> Html {
                  match b0 {
                    true => {
                      write("<div id=\"")
                      write_string(b1)
                      write("\"></div>")
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v6: Bool = true
                  let v7: String = "x"
                  write_function Wrapper@f0(v6, v7)
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
                fn Panel@f0(title@b0: String) -> Html {
                  write("<div title=\"")
                  write_string(b0)
                  write("\"></div>")
                }
                page Test() {
                  let v3: String = "a'b<c&d"
                  write_function Panel@f0(v3)
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
                fn Panel@f0(title@b0: String) -> Html {
                  write("<div title=\"")
                  write_string(b0)
                  write("\"></div>")
                }
                page Test() {
                  let v3: String = "a &amp; b"
                  write_function Panel@f0(v3)
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
                fn Icon@f0(src@b0: String, alt@b1: String) -> Html {
                  write("<img src=\"")
                  write_string(b0)
                  write("\" alt=\"")
                  write_string(b1)
                  write("\">")
                }
                page Test() {
                  let v4: String = "a.png"
                  let v5: String = "a"
                  write_function Icon@f0(v4, v5)
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
                  write_function A@f0()
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
                  write_function A@f0()
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
                fn Button@f1(kind@b2: String, id@b3: String, aria-label@b4: String) -> Html {
                  write("<button class=\"")
                  write_string(b2)
                  write("\" id=\"")
                  write_string(b3)
                  write("\" aria-label=\"")
                  write_string(b4)
                  write("\">")
                  write_string(b2)
                  write("</button>")
                }
                fn Secondary@f0(id@b0: String, aria-label@b1: String) -> Html {
                  let v7: String = "secondary"
                  write_function Button@f1(v7, b0, b1)
                }
                page Test() {
                  let v11: String = "save"
                  let v12: String = "Save"
                  write_function Secondary@f0(v11, v12)
                }
                -- ir (optimized) --
                page Test() {
                  let v17: String = "secondary"
                  write("<button class=\"")
                  write_string(v17)
                  write("\" id=\"save\" aria-label=\"Save\">")
                  write_string(v17)
                  write("</button>")
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
                fn Button@f0(class@b0: String, children@b1: Html, data-foo@b2: String) -> Html {
                  write("<button class=\"")
                  write_string(b0)
                  write("\" data-foo=\"")
                  write_string(b2)
                  write("\">")
                  write_html(b1)
                  write("</button>")
                }
                page Test() {
                  let v5: String = "p-2"
                  let v7: Html = html {
                    write("Hi")
                  }
                  let v8: String = "bar"
                  write_function Button@f0(v5, v7, v8)
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
                fn Button@f0(children@b0: Html, data-x@b1: String) -> Html {
                  write("<button class=\"builtin\" data-x=\"")
                  write_string(b1)
                  write("\">")
                  write_html(b0)
                  write("</button>")
                }
                page Test() {
                  let v6: Html = html {
                    write("Hi")
                  }
                  let v7: String = "y"
                  write_function Button@f0(v6, v7)
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
                fn Svg@f0(viewBox@b0: String) -> Html {
                  write("<svg viewBox=\"")
                  write_string(b0)
                  write("\"></svg>")
                }
                page Test() {
                  let v3: String = "0 0 100 100"
                  write_function Svg@f0(v3)
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
                fn Card@f1(title@b1: String) -> Html {
                  write("<div>")
                  write_string(b1)
                  write("</div>")
                }
                fn Wrapper@f0(title@b0: String) -> Html {
                  write_function Card@f1(b0)
                }
                page Test() {
                  let v6: String = "hi"
                  write_function Wrapper@f0(v6)
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
                fn Card@f1(title@b0: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write("</div>")
                }
                fn Wrapper@f0() -> Html {
                  let v4: String = "explicit"
                  write_function Card@f1(v4)
                }
                page Test() {
                  write_function Wrapper@f0()
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
                fn Card@f1(user@b2: User) -> Html {
                  let v1: String = b2.name
                  write("<div>")
                  write_string(v1)
                  write("</div>")
                }
                fn Wrapper@f0(user@b1: User) -> Html {
                  write_function Card@f1(b1)
                }
                page Test() {
                  let v7: String = "Ada"
                  let v8: User = {name: v7}
                  write_function Wrapper@f0(v8)
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
                  let v1: String = "x"
                  let v2: Flag = {value: v1}
                  let v3: Array[Flag] = [v2]
                  for b1: Flag in v3 {
                    let v5: String = b1.value
                    write("outer")
                    write_string(v5)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v1: String = "x"
                  let v2: Flag = {value: v1}
                  let v3: Array[Flag] = [v2]
                  for b1: Flag in v3 {
                    let v5: String = b1.value
                    write("outer")
                    write_string(v5)
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
                fn Bar@f1(name@b2: String, title@b3: String) -> Html {
                  write("<div>")
                  write_string(b2)
                  write_function Card@f2(b3)
                  write("</div>")
                }
                fn Baz@f0(name@b0: String, title@b1: String) -> Html {
                  write_function Bar@f1(b0, b1)
                }
                fn Card@f2(title@b4: String) -> Html {
                  write("<div>")
                  write_string(b4)
                  write("</div>")
                }
                page Test() {
                  let v13: String = "n"
                  let v14: String = "t"
                  write_function Baz@f0(v13, v14)
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
                fn Card@f1(count@b1: Int) -> Html {
                  let v0: Int = 0
                  let v2: Bool = v0 < b1
                  match v2 {
                    true => {
                      write("<div>positive</div>")
                    }
                    false => {
                    }
                  }
                }
                fn Wrapper@f0(count@b0: Int) -> Html {
                  write_function Card@f1(b0)
                }
                page Test() {
                  let v10: Int = 3
                  write_function Wrapper@f0(v10)
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
                fn A@f1(count@b2: Int, data-foo@b3: String) -> Html {
                  let v1: Int = 0
                  let v3: Bool = v1 < b2
                  write("<div data-foo=\"")
                  write_string(b3)
                  write("\">")
                  match v3 {
                    true => {
                      write("positive")
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                fn B@f0(count@b0: Int, data-foo@b1: String) -> Html {
                  write_function A@f1(b0, b1)
                }
                page Test() {
                  let v13: Int = 3
                  let v14: String = "bar"
                  write_function B@f0(v13, v14)
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
                fn Bar@f1(children@b1: Html) -> Html {
                  write_function Foo@f2(b1)
                }
                fn Baz@f0(children@b0: Html) -> Html {
                  write_function Bar@f1(b0)
                }
                fn Foo@f2(children@b2: Html) -> Html {
                  write("<div>")
                  write_html(b2)
                  write("</div>")
                }
                page Test() {
                  let v8: Html = html {
                    write("deep")
                  }
                  write_function Baz@f0(v8)
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
                fn Inner@f1(class@b1: String) -> Html {
                  write("<span class=\"")
                  write_string(b1)
                  write("\"></span>")
                }
                fn Outer@f0(class@b0: String) -> Html {
                  let v4: String = "x"
                  write("<div class=\"")
                  write_string(b0)
                  write("\">")
                  write_function Inner@f1(v4)
                  write("</div>")
                }
                page Test() {
                  let v8: String = "x"
                  write_function Outer@f0(v8)
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
                fn Button@f0(children@b0: Html, class@b1: String) -> Html {
                  let v1: Html = html {
                    write_html(b0)
                  }
                  write_function Foo@f1(v1, b1)
                }
                fn Foo@f1(children@b2: Html, class@b3: String) -> Html {
                  write("<div class=\"")
                  write_string(b3)
                  write("\">")
                  write_html(b2)
                  write("</div>")
                }
                page Test() {
                  let v9: Html = html {
                    write("click")
                  }
                  let v10: String = "primary"
                  write_function Button@f0(v9, v10)
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
                fn Inner@f1(class@b1: String) -> Html {
                  write("<span class=\"")
                  write_string(b1)
                  write("\"></span>")
                }
                fn Wrapper@f0(class@b0: String) -> Html {
                  write_function Inner@f1(b0)
                }
                page Test() {
                  let v5: String = "y"
                  write_function Wrapper@f0(v5)
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
                fn A@f1(class@b1: String) -> Html {
                  write("<div class=\"")
                  write_string(b1)
                  write("\"></div>")
                }
                fn B@f0(class@b0: String) -> Html {
                  write_function A@f1(b0)
                }
                page Test() {
                  let v5: String = "main"
                  write_function B@f0(v5)
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
                fn A@f1(class@b1: String) -> Html {
                  write("<div class=\"")
                  write_string(b1)
                  write("\"></div>")
                }
                fn B@f0(class@b0: String) -> Html {
                  write_function A@f1(b0)
                }
                page Test() {
                  let v5: String = "b"
                  write_function B@f0(v5)
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
                fn A@f1(label@b1: String) -> Html {
                  write("<span>")
                  write_string(b1)
                  write("</span>")
                }
                fn B@f0(label@b0: String) -> Html {
                  write_function A@f1(b0)
                }
                page Test() {
                  let v6: String = "x"
                  write_function B@f0(v6)
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
                fn Leaf@f2(label@b2: String) -> Html {
                  write("<span>")
                  write_string(b2)
                  write("</span>")
                }
                fn Mid@f1(label@b1: String) -> Html {
                  write_function Leaf@f2(b1)
                }
                fn Top@f0(label@b0: String) -> Html {
                  write_function Mid@f1(b0)
                }
                page Test() {
                  let v8: String = "x"
                  write_function Top@f0(v8)
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
                fn Inner@f1(title@b1: String, lang@b2: String) -> Html {
                  write("<span title=\"")
                  write_string(b1)
                  write("\" lang=\"")
                  write_string(b2)
                  write("\"></span>")
                }
                fn Wrapper@f0(lang@b0: String) -> Html {
                  let v4: String = "a"
                  write_function Inner@f1(v4, b0)
                }
                page Test() {
                  let v7: String = "en"
                  write_function Wrapper@f0(v7)
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
                fn Card@f1(title@b2: String) -> Html {
                  write("<div>")
                  write_string(b2)
                  write("</div>")
                }
                fn Wrapper@f0(title@b0: String) -> Html {
                  write("<section>local")
                  write_function Card@f1(b0)
                  write("</section>")
                }
                page Test() {
                  let v10: String = "hi"
                  write_function Wrapper@f0(v10)
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
                fn Card@f1(title@b2: String) -> Html {
                  write("<div>")
                  write_string(b2)
                  write("</div>")
                }
                fn Wrapper@f0(title@b0: String) -> Html {
                  let v4: String = "a"
                  let v5: String = "b"
                  let v6: Array[String] = [v4, v5]
                  for b1: String in v6 {
                    write("<p>")
                    write_string(b1)
                    write_function Card@f1(b0)
                    write("</p>")
                  }
                }
                page Test() {
                  let v14: String = "hi"
                  write_function Wrapper@f0(v14)
                }
                -- ir (optimized) --
                page Test() {
                  let v19: String = "a"
                  let v20: String = "b"
                  let v21: Array[String] = [v19, v20]
                  for b3: String in v21 {
                    write("<p>")
                    write_string(b3)
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
                fn Card@f1(title@b3: String) -> Html {
                  write("<div>")
                  write_string(b3)
                  write("</div>")
                }
                fn Wrapper@f0(title@b0: String) -> Html {
                  let v4: String = "m"
                  let v5: Option[String] = Some(v4)
                  match v5 {
                    Some(b2: String) => {
                      write("<p>")
                      write_string(b2)
                      write_function Card@f1(b0)
                      write("</p>")
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v14: String = "hi"
                  write_function Wrapper@f0(v14)
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
                fn Wrapper@f0(id@b0: String) -> Html {
                  write("<div id=\"")
                  write_string(b0)
                  write("\"><b>x</b></div>")
                }
                page Test() {
                  let v6: String = "hi"
                  write_function Wrapper@f0(v6)
                }
                -- ir (optimized) --
                page Test() {
                  write("<div id=\"hi\"><b>x</b></div>")
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
                fn Card@f1(title@b2: String, id@b3: String) -> Html {
                  write("<div id=\"")
                  write_string(b3)
                  write("\">")
                  write_string(b2)
                  write("</div>")
                }
                fn Wrapper@f0(id@b0: String) -> Html {
                  let v7: String = "t"
                  write("<section>local")
                  write_function Card@f1(v7, b0)
                  write("</section>")
                }
                page Test() {
                  let v12: String = "hi"
                  write_function Wrapper@f0(v12)
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
                fn A@f1(tabindex@b2: Int, data-x@b3: String) -> Html {
                  let v1: Int = 0
                  let v3: Bool = v1 < b2
                  write("<div data-x=\"")
                  write_string(b3)
                  write("\">")
                  match v3 {
                    true => {
                      write("focusable")
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                fn B@f0(tabindex@b0: Int, data-x@b1: String) -> Html {
                  write_function A@f1(b0, b1)
                }
                page Test() {
                  let v13: Int = 2
                  let v14: String = "y"
                  write_function B@f0(v13, v14)
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
                  let v0: String = "hello"
                  let v1: Option[String] = Some(v0)
                  let v5: Option[String] = match v1 {
                    Some(b2: String) => {
                      let v3: Option[String] = Some(b2)
                      v3
                    }
                    None => {
                      let v4: Option[String] = None
                      v4
                    }
                  }
                  match v5 {
                    Some(b5: String) => {
                      write("mapped:")
                      write_string(b5)
                    }
                    None => {
                      write("was-none")
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
                  let v0: String = "hi"
                  let v1: String = "bye"
                  let v2: Point = {x: v0, y: v1}
                  let v3: String = v2.x
                  write("got:")
                  write_string(v3)
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
                  let v0: String = "hi"
                  let v1: String = "bye"
                  let v2: Point = {x: v0, y: v1}
                  let v3: String = v2.x
                  write("got:")
                  write_string(v3)
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
                  let v0: String = "hi"
                  let v1: Option[String] = Some(v0)
                  match v1 {
                    Some(b1: String) => {
                      write("got:")
                      write_string(b1)
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
                  let v0: String = "inner"
                  let v1: Option[String] = Some(v0)
                  let v4: String = match v1 {
                    Some(b2: String) => {
                      b2
                    }
                    None => {
                      let v3: String = "default"
                      v3
                    }
                  }
                  let v5: Option[String] = Some(v4)
                  match v5 {
                    Some(b5: String) => {
                      write_string(b5)
                    }
                    None => {
                      write("none")
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
                fn Tag@f0(text@b0: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write("</div>")
                }
                page Test() {
                  let v4: String = "a"
                  let v6: String = "b"
                  write_function Tag@f0(v4)
                  write_function Tag@f0(v6)
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
                fn Swap@f0(a@b2: String, b@b3: String) -> Html {
                  write("<p>")
                  write_string(b2)
                  write(" ")
                  write_string(b3)
                  write("</p>")
                }
                page Test() {
                  let v7: String = "A"
                  let v8: String = "B"
                  write_function Swap@f0(v8, v7)
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
                fn Rows@f0(items@b1: Array[String], id@b2: String) -> Html {
                  for b3: String in b1 {
                    write("<div id=\"")
                    write_string(b2)
                    write("\">")
                    write_string(b3)
                    write("</div>")
                  }
                }
                page Test() {
                  let v7: String = "outer"
                  let v8: String = "a"
                  let v9: String = "b"
                  let v10: Array[String] = [v8, v9]
                  write_function Rows@f0(v10, v7)
                }
                -- ir (optimized) --
                page Test() {
                  let v8: String = "a"
                  let v9: String = "b"
                  let v10: Array[String] = [v8, v9]
                  for b4: String in v10 {
                    write("<div id=\"outer\">")
                    write_string(b4)
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
                  let v0: Bool = true
                  let v3: String = match v0 {
                    true => {
                      let v1: String = "yes"
                      v1
                    }
                    false => {
                      let v2: String = "no"
                      v2
                    }
                  }
                  let v6: Bool = false
                  let v9: String = match v6 {
                    true => {
                      let v7: String = "YES"
                      v7
                    }
                    false => {
                      let v8: String = "NO"
                      v8
                    }
                  }
                  write_string(v3)
                  write_string(v9)
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
                  let v0: String = ""
                  let v1: String = "main"
                  let v2: String = ""
                  let v3: Bool = v0 == v2
                  let v7: String = match v3 {
                    true => {
                      v1
                    }
                    false => {
                      let v4: String = " - "
                      let v5: String = concat(v1, v4)
                      let v6: String = concat(v5, v0)
                      v6
                    }
                  }
                  write_string(v7)
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
                  let v0: String = "hi"
                  let v1: Option[String] = Some(v0)
                  let v4: String = match v1 {
                    Some(_) => {
                      let v2: String = "some"
                      v2
                    }
                    None => {
                      let v3: String = "none"
                      v3
                    }
                  }
                  let v8: Option[String] = None
                  let v11: String = match v8 {
                    Some(_) => {
                      let v9: String = "SOME"
                      v9
                    }
                    None => {
                      let v10: String = "NONE"
                      v10
                    }
                  }
                  write_string(v4)
                  write(",")
                  write_string(v11)
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
                  let v0: Bool = true
                  let v1: Bool = false
                  let v6: String = match v0 {
                    true => {
                      let v4: String = match v1 {
                        true => {
                          let v2: String = "TT"
                          v2
                        }
                        false => {
                          let v3: String = "TF"
                          v3
                        }
                      }
                      v4
                    }
                    false => {
                      let v5: String = "F"
                      v5
                    }
                  }
                  let v10: Bool = false
                  let v11: Bool = true
                  let v16: String = match v10 {
                    true => {
                      let v14: String = match v11 {
                        true => {
                          let v12: String = "TT"
                          v12
                        }
                        false => {
                          let v13: String = "TF"
                          v13
                        }
                      }
                      v14
                    }
                    false => {
                      let v15: String = "F"
                      v15
                    }
                  }
                  write_string(v6)
                  write(",")
                  write_string(v16)
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
                  let v0: Int = -123
                  let v1: String = v0.to_string()
                  write_string(v1)
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
                  let v0: Float = 10000000000
                  let v1: Float = v0 * v0
                  let v2: Float = v1 * v0
                  let v3: Float = v2 * v0
                  let v4: Float = v3 * v0
                  let v5: Float = v4 * v0
                  let v6: Float = v5 * v0
                  let v7: Float = v6 * v0
                  let v8: Float = v7 * v0
                  let v9: Float = v8 * v0
                  let v10: Float = v9 * v9
                  let v11: Float = v10 * v9
                  let v12: Float = v11 * v9
                  let v13: Float = 0
                  let v14: Float = v12 * v13
                  let v15: Bool = v14 == v14
                  let v16: Bool = v14 == v14
                  let v17: Bool = !v16
                  let v18: Bool = v14 < v14
                  let v19: Bool = v14 <= v14
                  let v20: Bool = v14 < v14
                  let v21: Bool = v14 <= v14
                  let v22: Bool = v14 < v12
                  let v23: Float = -v12
                  let v24: Bool = v23 < v14
                  let v25: Int = v14.to_int()
                  let v26: Int = 0
                  let v27: Bool = v25 == v26
                  let v28: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) = (v15, v17, v18, v19, v20, v21, v22, v24, v27)
                  let v29: Bool = v28.0
                  let v30: Bool = v28.1
                  let v31: Bool = v28.2
                  let v32: Bool = v28.3
                  let v33: Bool = v28.4
                  let v34: Bool = v28.5
                  let v35: Bool = v28.6
                  let v36: Bool = v28.7
                  let v37: Bool = v28.8
                  match v37 {
                    true => {
                      match v36 {
                        true => {
                          write("wrong")
                        }
                        false => {
                          match v35 {
                            true => {
                              write("wrong")
                            }
                            false => {
                              match v34 {
                                true => {
                                  write("wrong")
                                }
                                false => {
                                  match v33 {
                                    true => {
                                      write("wrong")
                                    }
                                    false => {
                                      match v32 {
                                        true => {
                                          write("wrong")
                                        }
                                        false => {
                                          match v31 {
                                            true => {
                                              write("wrong")
                                            }
                                            false => {
                                              match v30 {
                                                true => {
                                                  match v29 {
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
                  let v0: Int = 2147483647
                  let v1: Int = -2147483648
                  let v2: Int = 1
                  let v3: Int = v0 + v2
                  let v4: Bool = v3 == v1
                  let v5: Int = 1
                  let v6: Int = v1 - v5
                  let v7: Bool = v6 == v0
                  let v8: Int = -v1
                  let v9: Bool = v8 == v1
                  let v10: Int = 2
                  let v11: Int = v0 * v10
                  let v12: Int = -2
                  let v13: Bool = v11 == v12
                  let v14: Int = v0 * v0
                  let v15: Int = 1
                  let v16: Bool = v14 == v15
                  let v17: Int = -1
                  let v18: Int = v1 * v17
                  let v19: Bool = v18 == v1
                  let v20: (Bool, Bool, Bool, Bool, Bool, Bool) = (v4, v7, v9, v13, v16, v19)
                  let v21: Bool = v20.0
                  let v22: Bool = v20.1
                  let v23: Bool = v20.2
                  let v24: Bool = v20.3
                  let v25: Bool = v20.4
                  let v26: Bool = v20.5
                  match v26 {
                    true => {
                      match v25 {
                        true => {
                          match v24 {
                            true => {
                              match v23 {
                                true => {
                                  match v22 {
                                    true => {
                                      match v21 {
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
                  let v0: Float = 10000000000
                  let v1: Float = v0 * v0
                  let v2: Float = v1 * v0
                  let v3: Float = v2 * v0
                  let v4: Float = v3 * v0
                  let v5: Float = v4 * v0
                  let v6: Float = v5 * v0
                  let v7: Float = v6 * v0
                  let v8: Float = v7 * v0
                  let v9: Float = v8 * v0
                  let v10: Float = v9 * v9
                  let v11: Float = v10 * v9
                  let v12: Float = v11 * v9
                  let v13: Float = 5
                  let v14: Float = 3.7
                  let v15: Float = -2.9
                  let v16: Float = 2147483648
                  let v17: Float = 2147483647.9
                  let v18: Float = -2147483648.9
                  let v19: Float = -2147483649
                  let v20: Float = -0.5
                  let v21: Int = v13.to_int()
                  let v22: Int = 5
                  let v23: Bool = v21 == v22
                  let v24: Int = v14.to_int()
                  let v25: Int = 3
                  let v26: Bool = v24 == v25
                  let v27: Int = v15.to_int()
                  let v28: Int = -2
                  let v29: Bool = v27 == v28
                  let v30: Int = v12.to_int()
                  let v31: Int = 2147483647
                  let v32: Bool = v30 == v31
                  let v33: Float = -v12
                  let v34: Int = v33.to_int()
                  let v35: Int = -2147483648
                  let v36: Bool = v34 == v35
                  let v37: Int = v16.to_int()
                  let v38: Int = 2147483647
                  let v39: Bool = v37 == v38
                  let v40: Int = v17.to_int()
                  let v41: Int = 2147483647
                  let v42: Bool = v40 == v41
                  let v43: Int = v18.to_int()
                  let v44: Int = -2147483648
                  let v45: Bool = v43 == v44
                  let v46: Int = v19.to_int()
                  let v47: Int = -2147483648
                  let v48: Bool = v46 == v47
                  let v49: Int = v20.to_int()
                  let v50: String = v49.to_string()
                  let v51: String = "0"
                  let v52: Bool = v50 == v51
                  let v53: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) = (v23, v26, v29, v32, v36, v39, v42, v45, v48, v52)
                  let v54: Bool = v53.0
                  let v55: Bool = v53.1
                  let v56: Bool = v53.2
                  let v57: Bool = v53.3
                  let v58: Bool = v53.4
                  let v59: Bool = v53.5
                  let v60: Bool = v53.6
                  let v61: Bool = v53.7
                  let v62: Bool = v53.8
                  let v63: Bool = v53.9
                  match v63 {
                    true => {
                      match v62 {
                        true => {
                          match v61 {
                            true => {
                              match v60 {
                                true => {
                                  match v59 {
                                    true => {
                                      match v58 {
                                        true => {
                                          match v57 {
                                            true => {
                                              match v56 {
                                                true => {
                                                  match v55 {
                                                    true => {
                                                      match v54 {
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
                  let v0: Float = inf
                  let v1: Float = -inf
                  let v2: Int = -2147483648
                  let v3: Float = 0
                  let v4: Float = v0 * v3
                  let v5: Float = 0
                  let v6: Bool = v5 < v0
                  let v7: Float = 2
                  let v8: Float = v0 * v7
                  let v9: Bool = v8 == v0
                  let v10: Float = 0
                  let v11: Bool = v1 < v10
                  let v12: Float = 2
                  let v13: Float = v1 * v12
                  let v14: Bool = v13 == v1
                  let v15: Int = 1
                  let v16: Int = v2 - v15
                  let v17: Int = 2147483647
                  let v18: Bool = v16 == v17
                  let v19: (Bool, Bool, Bool, Bool, Bool) = (v6, v9, v11, v14, v18)
                  let v20: Bool = v19.0
                  let v21: Bool = v19.1
                  let v22: Bool = v19.2
                  let v23: Bool = v19.3
                  let v24: Bool = v19.4
                  let v42: Int = 0
                  let v43: Int = 1
                  match v24 {
                    true => {
                      match v23 {
                        true => {
                          match v22 {
                            true => {
                              match v21 {
                                true => {
                                  match v20 {
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
                  for b10: Int in v42..=v43 {
                    let v45: Float = b10.to_float()
                    let v46: Float = v4 + v45
                    let v47: Float = v4 + v45
                    let v48: Bool = v46 == v47
                    let v49: Bool = !v48
                    match v49 {
                      true => {
                        write("ok")
                      }
                      false => {
                        write("wrong")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v4: Float = NaN
                  let v42: Int = 0
                  let v43: Int = 1
                  write("ok")
                  for b10: Int in v42..=v43 {
                    let v45: Float = b10.to_float()
                    let v46: Float = v4 + v45
                    let v47: Float = v4 + v45
                    let v48: Bool = v46 == v47
                    let v49: Bool = !v48
                    match v49 {
                      true => {
                        write("ok")
                      }
                      false => {
                        write("wrong")
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
                  let v0: Int = 2147483647
                  let v1: Int = -2147483648
                  let v2: Int = 1
                  let v3: Int = v0 - v2
                  let v10: Int = 1
                  let v11: Int = v1 + v10
                  let v18: Int = 3
                  let v19: Int = 1
                  for b2: Int in v3..=v0 {
                    let v5: String = b2.to_string()
                    write_string(v5)
                    write(",")
                  }
                  for b3: Int in v1..=v11 {
                    let v13: String = b3.to_string()
                    write_string(v13)
                    write(",")
                  }
                  for _ in v18..=v19 {
                    write("wrong")
                  }
                  for _ in v0..=v1 {
                    write("wrong")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 2147483647
                  let v1: Int = -2147483648
                  let v3: Int = 2147483646
                  let v11: Int = -2147483647
                  let v18: Int = 3
                  let v19: Int = 1
                  for b2: Int in v3..=v0 {
                    let v5: String = b2.to_string()
                    write_string(v5)
                    write(",")
                  }
                  for b3: Int in v1..=v11 {
                    let v13: String = b3.to_string()
                    write_string(v13)
                    write(",")
                  }
                  for _ in v18..=v19 {
                    write("wrong")
                  }
                  for _ in v0..=v1 {
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
                  write("Hello, Alice!")
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
                  let v0: Bool = true
                  let v5: Bool = !v0
                  match v0 {
                    true => {
                      write("Visible")
                    }
                    false => {
                    }
                  }
                  match v5 {
                    true => {
                      write("Hidden")
                    }
                    false => {
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for b0: String in v3 {
                    write_string(b0)
                    write(",")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for b0: String in v3 {
                    write_string(b0)
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for b0: String in v3 {
                    write_string(b0)
                    write(",")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for b0: String in v3 {
                    write_string(b0)
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
                  let v0: Int = 1
                  let v1: Int = 3
                  for b0: Int in v0..=v1 {
                    let v3: String = b0.to_string()
                    write_string(v3)
                    write(",")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 1
                  let v1: Int = 3
                  for b0: Int in v0..=v1 {
                    let v3: String = b0.to_string()
                    write_string(v3)
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for _ in v3 {
                    write("*")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for _ in v3 {
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for b0: String in v3 {
                    write_string(b0)
                    write("!")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for b0: String in v3 {
                    write_string(b0)
                    write("!")
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
                  let v0: Bool = true
                  let v1: Array[Bool] = [v0]
                  for b0: Bool in v1 {
                    match b0 {
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
                  let v0: Bool = true
                  let v1: Array[Bool] = [v0]
                  for b0: Bool in v1 {
                    match b0 {
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
                  let v0: Int = 1
                  let v1: Int = 3
                  for b0: Int in v0..=v1 {
                    let v3: String = b0.to_string()
                    write_string(v3)
                    write(",")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 1
                  let v1: Int = 3
                  for b0: Int in v0..=v1 {
                    let v3: String = b0.to_string()
                    write_string(v3)
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
                  let v0: Int = 0
                  let v1: Int = 5
                  for b0: Int in v0..=v1 {
                    let v3: String = b0.to_string()
                    write_string(v3)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 0
                  let v1: Int = 5
                  for b0: Int in v0..=v1 {
                    let v3: String = b0.to_string()
                    write_string(v3)
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
                  let v0: Int = 1
                  let v1: Int = 2
                  for b0: Int in v0..=v1 {
                    let v2: Int = 1
                    let v3: Int = 2
                    for b1: Int in v2..=v3 {
                      let v6: String = b0.to_string()
                      let v10: String = b1.to_string()
                      write("(")
                      write_string(v6)
                      write(",")
                      write_string(v10)
                      write(")")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 1
                  let v1: Int = 2
                  for b0: Int in v0..=v1 {
                    let v2: Int = 1
                    let v3: Int = 2
                    for b1: Int in v2..=v3 {
                      let v6: String = b0.to_string()
                      let v10: String = b1.to_string()
                      write("(")
                      write_string(v6)
                      write(",")
                      write_string(v10)
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
                  write("&lt;div&gt;Hello &amp; world&lt;/div&gt;")
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
                  write("Hello from let")
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    write("<span class=\"")
                    write_string(b0)
                    write(" px-2 py-1\">")
                    write_string(b0)
                    write("!?</span>")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    write("<span class=\"")
                    write_string(b0)
                    write(" px-2 py-1\">")
                    write_string(b0)
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
                  write("Hello World")
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
                  let v0: String = "A"
                  let v1: String = "B"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    write("[")
                    write_string(b0)
                    write("]")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "A"
                  let v1: String = "B"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    write("[")
                    write_string(b0)
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
                  let v0: String = "foo"
                  let v1: String = "bar"
                  let v2: String = concat(v0, v1)
                  let v3: String = "foobar"
                  let v4: Bool = v2 == v3
                  match v4 {
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
                  let v0: Int = 3
                  let v1: Int = 5
                  let v2: Bool = v0 < v1
                  let v7: Int = 10
                  let v8: Int = 2
                  let v9: Bool = v7 < v8
                  match v2 {
                    true => {
                      write("3 &lt; 5")
                    }
                    false => {
                    }
                  }
                  match v9 {
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
                  let v0: Float = 1.5
                  let v1: Float = 2.5
                  let v2: Bool = v0 < v1
                  match v2 {
                    true => {
                      write("1.5 &lt; 2.5")
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
                  let v0: String = "Alice"
                  let v1: Int = 30
                  let v2: Person = {name: v0, age: v1}
                  let v3: String = v2.name
                  let v5: Int = v2.age
                  let v6: Int = 30
                  let v7: Bool = v5 == v6
                  write_string(v3)
                  match v7 {
                    true => {
                      write(":30")
                    }
                    false => {
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
                  let v0: String = "b"
                  let v1: String = "a"
                  let v2: Pair = {second: v0, first: v1}
                  let v3: String = v2.first
                  let v6: String = v2.second
                  write_string(v3)
                  write("-")
                  write_string(v6)
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
                  let v0: String = "b"
                  let v1: String = "a"
                  let v2: Shape = Rect {height: v0, width: v1}
                  match v2 {
                    Shape::Rect {width@b2: String, height@b3: String} => {
                      write_string(b2)
                      write("-")
                      write_string(b3)
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
                  let v0: String = "Alice"
                  let v1: String = "Paris"
                  let v2: String = "75001"
                  let v3: Address = {city: v1, zip: v2}
                  let v4: Person = {name: v0, address: v3}
                  let v5: String = v4.name
                  let v8: Address = v4.address
                  let v9: String = v8.city
                  write_string(v5)
                  write(",")
                  write_string(v9)
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
                  let v0: Int = 3
                  let v1: Int = 7
                  let v2: Int = v0 + v1
                  let v3: Int = 10
                  let v4: Bool = v2 == v3
                  match v4 {
                    true => {
                      write("correct")
                    }
                    false => {
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
                  let v0: Int = 10
                  let v1: Int = 3
                  let v2: Int = v0 - v1
                  let v3: Int = 7
                  let v4: Bool = v2 == v3
                  match v4 {
                    true => {
                      write("correct")
                    }
                    false => {
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
                  let v0: Int = 4
                  let v1: Int = 5
                  let v2: Int = v0 * v1
                  let v3: Int = 20
                  let v4: Bool = v2 == v3
                  match v4 {
                    true => {
                      write("correct")
                    }
                    false => {
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
                  let v0: Bool = true
                  let v1: Bool = true
                  let v2: Bool = match v0 {
                    true => {
                      v1
                    }
                    false => {
                      v0
                    }
                  }
                  match v2 {
                    true => {
                      write("TT")
                    }
                    false => {
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
                  let v0: Bool = false
                  let v1: Bool = true
                  let v2: Bool = match v0 {
                    true => {
                      v0
                    }
                    false => {
                      v1
                    }
                  }
                  match v2 {
                    true => {
                      write("FT")
                    }
                    false => {
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
                  let v0: Int = 3
                  let v1: Int = 5
                  let v2: Bool = v0 <= v1
                  let v7: Int = 5
                  let v8: Int = 5
                  let v9: Bool = v7 <= v8
                  let v14: Int = 7
                  let v15: Int = 5
                  let v16: Bool = v14 <= v15
                  match v2 {
                    true => {
                      write("A")
                    }
                    false => {
                    }
                  }
                  match v9 {
                    true => {
                      write("B")
                    }
                    false => {
                    }
                  }
                  match v16 {
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
                  let v0: String = "hello"
                  let v1: Option[String] = Some(v0)
                  match v1 {
                    Some(b2: String) => {
                      write_string(b2)
                    }
                    None => {
                      write("none")
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
                  let v0: String = "hello"
                  let v1: Option[String] = Some(v0)
                  match v1 {
                    Some(_) => {
                      write("some")
                    }
                    None => {
                      write("none")
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
                  let v0: Bool = true
                  let v1: Option[Bool] = Some(v0)
                  let v2: Bool = false
                  let v3: Option[Bool] = Some(v2)
                  let v4: Option[Bool] = None
                  let v5: Array[Option[Bool]] = [v1, v3, v4]
                  for b0: Option[Bool] in v5 {
                    match b0 {
                      Some(b2: Bool) => {
                        match b2 {
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
                -- ir (optimized) --
                page Test() {
                  let v0: Bool = true
                  let v1: Option[Bool] = Some(v0)
                  let v2: Bool = false
                  let v3: Option[Bool] = Some(v2)
                  let v4: Option[Bool] = None
                  let v5: Array[Option[Bool]] = [v1, v3, v4]
                  for b0: Option[Bool] in v5 {
                    match b0 {
                      Some(b2: Bool) => {
                        match b2 {
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
                  let v0: Bool = true
                  let v1: String = "a"
                  let v2: Option[String] = Some(v1)
                  let v3: Foo = {a: v0, b: v2}
                  let v4: Bool = true
                  let v5: Option[String] = None
                  let v6: Foo = {a: v4, b: v5}
                  let v7: Bool = false
                  let v8: String = "x"
                  let v9: Option[String] = Some(v8)
                  let v10: Foo = {a: v7, b: v9}
                  let v11: Array[Foo] = [v3, v6, v10]
                  for b0: Foo in v11 {
                    let v13: Bool = b0.a
                    let v14: Option[String] = b0.b
                    match v13 {
                      true => {
                        match v14 {
                          Some(b4: String) => {
                            write_string(b4)
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
                -- ir (optimized) --
                page Test() {
                  let v0: Bool = true
                  let v1: String = "a"
                  let v2: Option[String] = Some(v1)
                  let v3: Foo = {a: v0, b: v2}
                  let v4: Bool = true
                  let v5: Option[String] = None
                  let v6: Foo = {a: v4, b: v5}
                  let v7: Bool = false
                  let v8: String = "x"
                  let v9: Option[String] = Some(v8)
                  let v10: Foo = {a: v7, b: v9}
                  let v11: Array[Foo] = [v3, v6, v10]
                  for b0: Foo in v11 {
                    let v13: Bool = b0.a
                    let v14: Option[String] = b0.b
                    match v13 {
                      true => {
                        match v14 {
                          Some(b4: String) => {
                            write_string(b4)
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
                  let v0: Bool = true
                  let v1: Status = Active {admin: v0}
                  let v2: Bool = false
                  let v3: Status = Active {admin: v2}
                  let v4: Status = Inactive
                  let v5: Array[Status] = [v1, v3, v4]
                  for b0: Status in v5 {
                    match b0 {
                      Status::Active {admin@b2: Bool} => {
                        match b2 {
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
                -- ir (optimized) --
                page Test() {
                  let v0: Bool = true
                  let v1: Status = Active {admin: v0}
                  let v2: Bool = false
                  let v3: Status = Active {admin: v2}
                  let v4: Status = Inactive
                  let v5: Array[Status] = [v1, v3, v4]
                  for b0: Status in v5 {
                    match b0 {
                      Status::Active {admin@b2: Bool} => {
                        match b2 {
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
                  let v0: String = "inner"
                  let v1: Option[String] = Some(v0)
                  let v4: String = match v1 {
                    Some(b2: String) => {
                      b2
                    }
                    None => {
                      let v3: String = "default"
                      v3
                    }
                  }
                  let v5: Option[String] = Some(v4)
                  match v5 {
                    Some(b5: String) => {
                      write_string(b5)
                    }
                    None => {
                      write("none")
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
                  let v0: String = "a"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[String] = None
                  let v3: String = "b"
                  let v4: Option[String] = Some(v3)
                  let v5: Array[Option[String]] = [v1, v2, v4]
                  for b0: Option[String] in v5 {
                    match b0 {
                      Some(b2: String) => {
                        write("[")
                        write_string(b2)
                        write("]")
                      }
                      None => {
                        write("[_]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[String] = None
                  let v3: String = "b"
                  let v4: Option[String] = Some(v3)
                  let v5: Array[Option[String]] = [v1, v2, v4]
                  for b0: Option[String] in v5 {
                    match b0 {
                      Some(b2: String) => {
                        write("[")
                        write_string(b2)
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
                  let v0: Color = Green
                  let v4: String = match v0 {
                    Color::Red => {
                      let v1: String = "red"
                      v1
                    }
                    Color::Green => {
                      let v2: String = "green"
                      v2
                    }
                    Color::Blue => {
                      let v3: String = "blue"
                      v3
                    }
                  }
                  write_string(v4)
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
                  let v0: String = "hello"
                  let v1: Outcome = Success {value: v0}
                  match v1 {
                    Outcome::Success {value@b2: String} => {
                      write("Ok: ")
                      write_string(b2)
                    }
                    Outcome::Failure {message@b3: String} => {
                      write("Err: ")
                      write_string(b3)
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
                  let v0: String = "news"
                  let v1: Item = Tagged {tag: v0}
                  match v1 {
                    Item::Tagged {tag@b2: String} => {
                      write("tag: ")
                      write_string(b2)
                    }
                    Item::Plain => {
                      write("plain")
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
                  let v0: String = "hi"
                  let v1: Outcome = Success {value: v0}
                  let v4: String = match v1 {
                    Outcome::Success {value@b1: String} => {
                      b1
                    }
                    Outcome::Failure {message@b2: String} => {
                      b2
                    }
                  }
                  write("Got: ")
                  write_string(v4)
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
                fn Badge@f0(color@b0: Color) -> Html {
                  match b0 {
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
                  let v8: Color = Green
                  write_function Badge@f0(v8)
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
                  let v0: String = "something went wrong"
                  let v1: Outcome = Failure {message: v0}
                  match v1 {
                    Outcome::Success {value@b2: String} => {
                      write("Ok: ")
                      write_string(b2)
                    }
                    Outcome::Failure {message@b3: String} => {
                      write("Err: ")
                      write_string(b3)
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
                  let v0: String = "200"
                  let v1: String = "OK"
                  let v2: Response = Success {code: v0, body: v1}
                  match v2 {
                    Response::Success {code@b2: String, body@b3: String} => {
                      write_string(b2)
                      write(" ")
                      write_string(b3)
                    }
                    Response::Failure {reason@b4: String} => {
                      write("Error: ")
                      write_string(b4)
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
                  let v0: String = "hello"
                  let v1: Outcome = Success {value: v0}
                  match v1 {
                    Outcome::Success {value@b2: String} => {
                      write("Ok: ")
                      write_string(b2)
                    }
                    Outcome::Failure {message@b3: String} => {
                      write("Err: ")
                      write_string(b3)
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  let v4: Int = v3.len()
                  let v5: String = v4.to_string()
                  write_string(v5)
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
                  let v0: Array[String] = []
                  let v1: Int = v0.len()
                  let v2: String = v1.to_string()
                  write_string(v2)
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
                  let v0: String = "x"
                  let v1: String = "y"
                  let v2: Array[String] = [v0, v1]
                  let v3: Int = v2.len()
                  let v4: Int = 2
                  let v5: Bool = v3 == v4
                  match v5 {
                    true => {
                      write("has two")
                    }
                    false => {
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
                  let v0: String = "a"
                  let v1: Array[String] = [v0]
                  let v2: Int = v1.len()
                  let v3: Int = 5
                  let v4: Bool = v2 < v3
                  match v4 {
                    true => {
                      write("less than 5")
                    }
                    false => {
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
                  let v0: Int = 1
                  let v1: Int = 2
                  let v2: Int = 3
                  let v3: Int = 4
                  let v4: Int = 5
                  let v5: Array[Int] = [v0, v1, v2, v3, v4]
                  let v6: Int = v5.len()
                  let v7: String = v6.to_string()
                  write_string(v7)
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
                  let v0: Array[String] = []
                  let v1: Bool = v0.is_empty()
                  match v1 {
                    true => {
                      write("empty")
                    }
                    false => {
                      write("not empty")
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  let v3: Bool = v2.is_empty()
                  match v3 {
                    true => {
                      write("empty")
                    }
                    false => {
                      write("not empty")
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
                  let v0: Int = 1
                  let v1: Int = 2
                  let v2: Int = 3
                  let v3: Array[Int] = [v0, v1, v2]
                  let v4: Bool = v3.is_empty()
                  match v4 {
                    true => {
                      write("no numbers")
                    }
                    false => {
                      write("has numbers")
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
                  let v0: Int = 42
                  let v1: String = v0.to_string()
                  write_string(v1)
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
                  let v0: Int = 0
                  let v1: String = v0.to_string()
                  write_string(v1)
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
                  let v0: Int = 3
                  let v1: Array[Int] = [v0]
                  for b0: Int in v1 {
                    let v3: Float = b0.to_float()
                    let v4: Int = v3.to_int()
                    let v5: String = v4.to_string()
                    write_string(v5)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 3
                  let v1: Array[Int] = [v0]
                  for b0: Int in v1 {
                    let v3: Float = b0.to_float()
                    let v4: Int = v3.to_int()
                    let v5: String = v4.to_string()
                    write_string(v5)
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
                  let v0: Int = 0
                  let v1: Int = 2
                  for _ in v0..=v1 {
                    write("x")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 0
                  let v1: Int = 2
                  for _ in v0..=v1 {
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    let v3: Bool = false
                    match v3 {
                      true => {
                        write_string(b0)
                      }
                      false => {
                      }
                    }
                    write("y")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for _ in v2 {
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for _ in v3 {
                    write("*")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: String = "c"
                  let v3: Array[String] = [v0, v1, v2]
                  for _ in v3 {
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
                  let v0: Int = 0
                  let v1: Int = 1
                  for _ in v0..=v1 {
                    let v2: Int = 0
                    let v3: Int = 2
                    for _ in v2..=v3 {
                      write(".")
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 0
                  let v1: Int = 1
                  for _ in v0..=v1 {
                    let v2: Int = 0
                    let v3: Int = 2
                    for _ in v2..=v3 {
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
                  let v0: Int = 1
                  let v1: Int = 2
                  for b0: Int in v0..=v1 {
                    let v2: Int = 0
                    let v3: Int = 1
                    for _ in v2..=v3 {
                      let v5: String = b0.to_string()
                      write_string(v5)
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 1
                  let v1: Int = 2
                  for b0: Int in v0..=v1 {
                    let v2: Int = 0
                    let v3: Int = 1
                    for _ in v2..=v3 {
                      let v5: String = b0.to_string()
                      write_string(v5)
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
                  let v0: Int = 1
                  let v1: Int = 2
                  let v2: Int = 3
                  let v3: Array[Int] = [v0, v1, v2]
                  let v4: Int = v3.len()
                  let v5: String = v4.to_string()
                  write_string(v5)
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
                  let v0: Int = 1
                  let v1: Int = 2
                  let v2: Int = v0 + v1
                  let v3: String = v2.to_string()
                  write_string(v3)
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
                  let v0: Int = 42
                  let v1: String = v0.to_string()
                  write_string(v1)
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
                  let v0: String = "deep"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[Option[String]] = Some(v1)
                  match v2 {
                    Some(b2: Option[String]) => {
                      match b2 {
                        Some(b3: String) => {
                          write_string(b3)
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
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v4: String = match v1 {
                    Some(_) => {
                      let v2: String = "some"
                      v2
                    }
                    None => {
                      let v3: String = "none"
                      v3
                    }
                  }
                  write_string(v4)
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
                  let v0: Option[String] = None
                  let v3: String = match v0 {
                    Some(_) => {
                      let v1: String = "some"
                      v1
                    }
                    None => {
                      let v2: String = "none"
                      v2
                    }
                  }
                  write_string(v3)
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
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[Option[String]] = Some(v1)
                  match v2 {
                    Some(b2: Option[String]) => {
                      match b2 {
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
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[Option[String]] = Some(v1)
                  match v2 {
                    Some(_) => {
                      write("some")
                    }
                    None => {
                      write("none")
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
                  let v0: String = "Hello"
                  let v1: Outcome = Success {value: v0}
                  match v1 {
                    Outcome::Success => {
                      write("ok")
                    }
                    Outcome::Failure => {
                      write("err")
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
                  let v0: String = "failed"
                  let v1: Outcome = Failure {message: v0}
                  match v1 {
                    Outcome::Success => {
                      write("ok")
                    }
                    Outcome::Failure => {
                      write("err")
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
                  let v0: String = "Alice"
                  let v1: Int = 30
                  let v2: Person = {name: v0, age: v1}
                  let v3: Int = v2.age
                  let v5: String = v3.to_string()
                  write("age: ")
                  write_string(v5)
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
                  let v0: String = "value"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[Option[String]] = Some(v1)
                  let v3: Option[Option[Option[String]]] = Some(v2)
                  match v3 {
                    Some(b2: Option[Option[String]]) => {
                      match b2 {
                        Some(b3: Option[String]) => {
                          match b3 {
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
                  let v0: String = "deep"
                  let v1: Inner = Success {value: v0}
                  let v2: Outer = Success {value: v1}
                  match v2 {
                    Outer::Success {value@b2: Inner} => {
                      match b2 {
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
                  let v0: Bool = true
                  match v0 {
                    true => {
                      write("t")
                    }
                    false => {
                      write("f")
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
                  let v0: Bool = false
                  match v0 {
                    true => {
                      write("t")
                    }
                    false => {
                      write("f")
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
                  let v0: String = "outer"
                  let v1: Option[String] = Some(v0)
                  match v1 {
                    Some(b1: String) => {
                      let v2: String = "inner"
                      let v3: Option[String] = Some(v2)
                      match v3 {
                        Some(b3: String) => {
                          write_string(b1)
                          write(":")
                          write_string(b3)
                        }
                        None => {
                          write("inner-none")
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
                  let v0: String = "hello"
                  let v1: Option[String] = Some(v0)
                  let v2: Option[Option[String]] = Some(v1)
                  match v2 {
                    Some(b2: Option[String]) => {
                      match b2 {
                        Some(b4: String) => {
                          write("value:")
                          write_string(b4)
                        }
                        None => {
                          write("inner-none")
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
                  let v1: Int = 3
                  let v4: String = v1.to_string()
                  write("a: hop, b: ")
                  write_string(v4)
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
                  write("a{bcd}e")
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
                  write("a&quot;b\nc\\d")
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
                  write("removed")
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
                  write("<div class=\"my-class\"></div>")
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
                  write("<span>on</span>")
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
                  write("<input type=\"button\">")
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
                page Other(delete@b0: String) {
                  write_string(b0)
                }
                -- ir (optimized) --
                page Test() {
                  write("ok")
                }
                page Other(delete@b0: String) {
                  write_string(b0)
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
                page Other(type@b0: String) {
                  write_string(b0)
                }
                -- ir (optimized) --
                page Test() {
                  write("ok")
                }
                page Other(type@b0: String) {
                  write_string(b0)
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
                fn Countdown@f0(delete@b0: Int) -> Html {
                  let v1: String = b0.to_string()
                  let v3: Int = 0
                  let v5: Bool = v3 < b0
                  write_string(v1)
                  match v5 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = b0 - v7
                      write_function Countdown@f0(v8)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v13: Int = 3
                  write_function Countdown@f0(v13)
                }
                -- ir (optimized) --
                fn Countdown@f0(delete@b0: Int) -> Html {
                  let v1: String = b0.to_string()
                  let v3: Int = 0
                  let v5: Bool = v3 < b0
                  write_string(v1)
                  match v5 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = b0 - v7
                      write_function Countdown@f0(v8)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v13: Int = 3
                  write_function Countdown@f0(v13)
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
                fn Countdown@f0(type@b0: Int) -> Html {
                  let v1: String = b0.to_string()
                  let v3: Int = 0
                  let v5: Bool = v3 < b0
                  write_string(v1)
                  match v5 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = b0 - v7
                      write_function Countdown@f0(v8)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v13: Int = 3
                  write_function Countdown@f0(v13)
                }
                -- ir (optimized) --
                fn Countdown@f0(type@b0: Int) -> Html {
                  let v1: String = b0.to_string()
                  let v3: Int = 0
                  let v5: Bool = v3 < b0
                  write_string(v1)
                  match v5 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = b0 - v7
                      write_function Countdown@f0(v8)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v13: Int = 3
                  write_function Countdown@f0(v13)
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
                  let v0: String = "\""
                  let v1: String = "\\"
                  let v2: String = "foo\nbar"
                  let v3: String = "foo\tbar"
                  let v4: String = "C:\\Users\\name"
                  let v5: Array[String] = [v0, v1, v2, v3, v4]
                  for b0: String in v5 {
                    write_string(b0)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "\""
                  let v1: String = "\\"
                  let v2: String = "foo\nbar"
                  let v3: String = "foo\tbar"
                  let v4: String = "C:\\Users\\name"
                  let v5: Array[String] = [v0, v1, v2, v3, v4]
                  for b0: String in v5 {
                    write_string(b0)
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
                  let v0: String = "a"
                  let v1: String = "1"
                  let v2: Item = {name: v0, value: v1}
                  let v3: String = "b"
                  let v4: String = "2"
                  let v5: Item = {name: v3, value: v4}
                  let v6: Array[Item] = [v2, v5]
                  for b1: Item in v6 {
                    let v8: String = b1.name
                    write("[")
                    write_string(v8)
                    write("]")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "1"
                  let v2: Item = {name: v0, value: v1}
                  let v3: String = "b"
                  let v4: String = "2"
                  let v5: Item = {name: v3, value: v4}
                  let v6: Array[Item] = [v2, v5]
                  for b1: Item in v6 {
                    let v8: String = b1.name
                    write("[")
                    write_string(v8)
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
                  let v0: String = "alice"
                  let v1: String = "paris"
                  let v2: Address = {city: v1}
                  let v3: Person = {name: v0, address: v2}
                  let v4: String = "bob"
                  let v5: String = "london"
                  let v6: Address = {city: v5}
                  let v7: Person = {name: v4, address: v6}
                  let v8: Array[Person] = [v3, v7]
                  for b1: Person in v8 {
                    let v10: Address = b1.address
                    let v11: String = v10.city
                    write("[")
                    write_string(v11)
                    write("]")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "alice"
                  let v1: String = "paris"
                  let v2: Address = {city: v1}
                  let v3: Person = {name: v0, address: v2}
                  let v4: String = "bob"
                  let v5: String = "london"
                  let v6: Address = {city: v5}
                  let v7: Person = {name: v4, address: v6}
                  let v8: Array[Person] = [v3, v7]
                  for b1: Person in v8 {
                    let v10: Address = b1.address
                    let v11: String = v10.city
                    write("[")
                    write_string(v11)
                    write("]")
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
                  let v0: String = "a"
                  let v1: String = "1"
                  let v2: Source = {name: v0, value: v1}
                  let v3: String = "b"
                  let v4: String = "2"
                  let v5: Source = {name: v3, value: v4}
                  let v6: Array[Source] = [v2, v5]
                  for b1: Source in v6 {
                    let v8: String = b1.name
                    let v9: Target = {label: v8}
                    let v11: String = v9.label
                    write("[")
                    write_string(v11)
                    write("]")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "1"
                  let v2: Source = {name: v0, value: v1}
                  let v3: String = "b"
                  let v4: String = "2"
                  let v5: Source = {name: v3, value: v4}
                  let v6: Array[Source] = [v2, v5]
                  for b1: Source in v6 {
                    let v8: String = b1.name
                    write("[")
                    write_string(v8)
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
                  let v0: String = "a"
                  let v1: Item = {name: v0}
                  let v2: String = "b"
                  let v3: Item = {name: v2}
                  let v4: Array[Item] = [v1, v3]
                  for b1: Item in v4 {
                    let v6: String = b1.name
                    let v7: Option[String] = Some(v6)
                    match v7 {
                      Some(b4: String) => {
                        write("[")
                        write_string(b4)
                        write("]")
                      }
                      None => {
                        write("[-]")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: Item = {name: v0}
                  let v2: String = "b"
                  let v3: Item = {name: v2}
                  let v4: Array[Item] = [v1, v3]
                  for b1: Item in v4 {
                    let v6: String = b1.name
                    write("[")
                    write_string(v6)
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
                  let v0: String = "hello"
                  let v1: String = " "
                  let v2: String = concat(v0, v1)
                  let v3: String = "world"
                  let v4: String = concat(v2, v3)
                  let v5: Greeting = {message: v4}
                  let v6: String = v5.message
                  write_string(v6)
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
                  let v0: Int = 42
                  let v1: String = v0.to_string()
                  write_string(v1)
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  let v3: Container = {items: v2}
                  let v4: Array[String] = v3.items
                  for b1: String in v4 {
                    write("[")
                    write_string(b1)
                    write("]")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for b1: String in v2 {
                    write("[")
                    write_string(b1)
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
                  let v0: Int = 42
                  let v1: String = v0.to_string()
                  let v2: Label = {text: v1}
                  let v3: String = v2.text
                  write_string(v3)
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
                  let v0: String = "x"
                  let v1: String = "y"
                  let v2: Array[String] = [v0, v1]
                  let v3: Inner = {values: v2}
                  let v4: Outer = {inner: v3}
                  let v5: Inner = v4.inner
                  let v6: Array[String] = v5.values
                  for b1: String in v6 {
                    write("[")
                    write_string(b1)
                    write("]")
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "x"
                  let v1: String = "y"
                  let v2: Array[String] = [v0, v1]
                  for b1: String in v2 {
                    write("[")
                    write_string(b1)
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
                  let v0: String = "hello"
                  let v1: Foo = {a: v0}
                  let v2: String = v1.a
                  let v3: Foo = {a: v2}
                  let v5: String = v1.a
                  let v8: String = v3.a
                  write("[")
                  write_string(v5)
                  write("][")
                  write_string(v8)
                  write("]")
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "hello"
                  write("[")
                  write_string(v0)
                  write("][")
                  write_string(v0)
                  write("]")
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
                  let v0: String = "hello"
                  let v1: Foo = {a: v0}
                  let v2: Bool = true
                  let v5: String = match v2 {
                    true => {
                      let v3: String = v1.a
                      v3
                    }
                    false => {
                      let v4: String = "default"
                      v4
                    }
                  }
                  let v9: String = v1.a
                  write("[")
                  write_string(v5)
                  write("][")
                  write_string(v9)
                  write("]")
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "hello"
                  write("[")
                  write_string(v0)
                  write("][")
                  write_string(v0)
                  write("]")
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
                  let v0: String = "leaf"
                  let v1: Array[TreeNode] = []
                  let v2: TreeNode = {value: v0, children: v1}
                  let v3: String = v2.value
                  write_string(v3)
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
                  let v0: String = "first"
                  let v1: Option[Node] = None
                  let v2: Node = {value: v0, next: v1}
                  let v3: String = v2.value
                  write_string(v3)
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
                  let v0: String = "42"
                  let v1: Expr = Literal {value: v0}
                  match v1 {
                    Expr::Literal {value@b2: String} => {
                      write_string(b2)
                    }
                    Expr::Neg => {
                      write("neg")
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
                  let v0: String = "42"
                  let v1: Expr = Literal {value: v0}
                  let v2: Expr = Neg {inner: v1}
                  let v3: Array[Expr] = [v2]
                  for b0: Expr in v3 {
                    match b0 {
                      Expr::Literal => {
                        write("lit")
                      }
                      Expr::Neg {inner@b2: Expr} => {
                        match b2 {
                          Expr::Literal {value@b4: String} => {
                            write_string(b4)
                          }
                          Expr::Neg => {
                            write("nested")
                          }
                        }
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "42"
                  let v1: Expr = Literal {value: v0}
                  let v2: Expr = Neg {inner: v1}
                  let v3: Array[Expr] = [v2]
                  for b0: Expr in v3 {
                    match b0 {
                      Expr::Literal => {
                        write("lit")
                      }
                      Expr::Neg {inner@b2: Expr} => {
                        match b2 {
                          Expr::Literal {value@b4: String} => {
                            write_string(b4)
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
                  let v0: String = "42"
                  let v1: Expr = Literal {value: v0}
                  let v2: Expr = Neg {inner: v1}
                  match v2 {
                    Expr::Literal {value@b2: String} => {
                      write("lit:")
                      write_string(b2)
                    }
                    Expr::Neg => {
                      write("neg")
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
                  let v0: String = "root"
                  let v1: Option[File] = None
                  let v2: Folder = {name: v0, parent: v1}
                  let v3: String = v2.name
                  write_string(v3)
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
                  let v0: Option[Expr] = None
                  let v1: Leaf = {back: v0}
                  let v2: Option[Expr] = v1.back
                  match v2 {
                    Some(_) => {
                      write("some")
                    }
                    None => {
                      write("none")
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
                  let v0: Option[Node] = None
                  let v1: String = "head"
                  let v2: Node = {value: v1, next: v0}
                  let v3: String = v2.value
                  write_string(v3)
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
                  let v0: String = "leaf"
                  let v1: Option[Node] = None
                  let v2: Node = {value: v0, next: v1}
                  let v3: String = "head"
                  let v4: Bool = true
                  let v7: Option[Node] = match v4 {
                    true => {
                      let v5: Option[Node] = Some(v2)
                      v5
                    }
                    false => {
                      let v6: Option[Node] = None
                      v6
                    }
                  }
                  let v8: Node = {value: v3, next: v7}
                  let v9: String = v8.value
                  write_string(v9)
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
                  let v0: String = "node"
                  let v1: Option[Option[Node]] = None
                  let v2: Node = {value: v0, next: v1}
                  let v3: String = v2.value
                  write_string(v3)
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
                  let v0: String = "head"
                  let v1: String = "tail"
                  let v2: Option[Option[Node]] = None
                  let v3: Node = {value: v1, next: v2}
                  let v4: Option[Node] = Some(v3)
                  let v5: Option[Option[Node]] = Some(v4)
                  let v6: Node = {value: v0, next: v5}
                  let v7: Option[Option[Node]] = v6.next
                  match v7 {
                    Some(b2: Option[Node]) => {
                      match b2 {
                        Some(b4: Node) => {
                          let v10: String = b4.value
                          write_string(v10)
                        }
                        None => {
                          write("inner-none")
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
                  let v0: String = "node"
                  let v1: Option[Node] = None
                  let v2: Node = {value: v0, next: v1}
                  let v3: Option[Node] = v2.next
                  let v4: Holder = {held: v3}
                  let v5: Option[Node] = v4.held
                  match v5 {
                    Some(_) => {
                      write("some")
                    }
                    None => {
                      let v8: String = v2.value
                      write_string(v8)
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
                  let v0: String = "b"
                  let v1: Option[A] = None
                  let v2: B = {name: v0, a: v1}
                  let v3: A = {b: v2}
                  let v4: B = v3.b
                  let v5: String = v4.name
                  let v7: B = v3.b
                  let v8: Option[A] = v7.a
                  write_string(v5)
                  match v8 {
                    Some(_) => {
                      write("some")
                    }
                    None => {
                      write("none")
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
                  let v0: String = "a"
                  let v1: Tree = Leaf
                  let v2: Option[Tree] = None
                  let v3: Tree = Node {label: v0, left: v1, right: v2}
                  match v3 {
                    Tree::Node {label@b2: String, left@b3: Tree, right@b4: Option[Tree]} => {
                      let v6: Step = {t: b3, rest: b4}
                      let v9: Option[Tree] = v6.rest
                      write_string(b2)
                      match v9 {
                        Some(_) => {
                          write("some")
                        }
                        None => {
                          write("none")
                        }
                      }
                    }
                    Tree::Leaf => {
                      write("empty")
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
                  let v0: String = "a@b.c"
                  let v1: String = "work"
                  let v2: Option[String] = Some(v1)
                  let v3: Contact = Email {address: v0, label: v2}
                  match v3 {
                    Contact::Email {address@b2: String, label@b3: Option[String]} => {
                      write_string(b2)
                      match b3 {
                        Some(b5: String) => {
                          write_string(b5)
                        }
                        None => {
                          write("no-label")
                        }
                      }
                    }
                    Contact::Anonymous => {
                      write("anon")
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
                  let v0: String = "a"
                  let v1: Option[Tree] = None
                  let v2: Tree = Node {label: v0, kid: v1}
                  let v3: Array[Tree] = [v2]
                  for b0: Tree in v3 {
                    match b0 {
                      Tree::Node {label@b2: String, kid@b3: Option[Tree]} => {
                        let v5: String = "b"
                        let v7: Tree = Node {label: v5, kid: b3}
                        match v7 {
                          Tree::Node {label@b5: String, kid@b6: Option[Tree]} => {
                            write_string(b2)
                            write_string(b5)
                            match b6 {
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
                      Tree::Leaf => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: Option[Tree] = None
                  let v2: Tree = Node {label: v0, kid: v1}
                  let v3: Array[Tree] = [v2]
                  for b0: Tree in v3 {
                    match b0 {
                      Tree::Node {label@b2: String, kid@b3: Option[Tree]} => {
                        write_string(b2)
                        write("b")
                        match b3 {
                          Some(_) => {
                            write("s")
                          }
                          None => {
                            write("n")
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
                  let v0: String = "a"
                  let v1: Tree = Leaf
                  let v2: Tree = Node {label: v0, kid: v1}
                  let v3: Array[Tree] = [v2]
                  for b0: Tree in v3 {
                    match b0 {
                      Tree::Node {label@b2: String, kid@b3: Tree} => {
                        let v5: String = "b"
                        let v7: Tree = Node {label: v5, kid: b3}
                        match v7 {
                          Tree::Node {label@b5: String} => {
                            write_string(b2)
                            write_string(b5)
                          }
                          Tree::Leaf => {
                            write("x")
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
                  let v0: String = "a"
                  let v1: Tree = Leaf
                  let v2: Tree = Node {label: v0, kid: v1}
                  let v3: Array[Tree] = [v2]
                  for b0: Tree in v3 {
                    match b0 {
                      Tree::Node {label@b2: String} => {
                        write_string(b2)
                        write("b")
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
                  let v0: String = "a"
                  let v1: Option[Option[Tree]] = None
                  let v2: Tree = Node {label: v0, kid: v1}
                  let v3: Array[Tree] = [v2]
                  for b0: Tree in v3 {
                    match b0 {
                      Tree::Node {label@b2: String, kid@b3: Option[Option[Tree]]} => {
                        let v5: String = "b"
                        let v7: Tree = Node {label: v5, kid: b3}
                        match v7 {
                          Tree::Node {label@b5: String} => {
                            write_string(b2)
                            write_string(b5)
                          }
                          Tree::Leaf => {
                            write("x")
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
                  let v0: String = "a"
                  let v1: Option[Option[Tree]] = None
                  let v2: Tree = Node {label: v0, kid: v1}
                  let v3: Array[Tree] = [v2]
                  for b0: Tree in v3 {
                    match b0 {
                      Tree::Node {label@b2: String} => {
                        write_string(b2)
                        write("b")
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
                  let v0: Option[Holder] = None
                  let v1: Wrap = Full {h: v0}
                  let v2: Array[Wrap] = [v1]
                  for b0: Wrap in v2 {
                    match b0 {
                      Wrap::Full {h@b2: Option[Holder]} => {
                        let v5: Wrap = Full {h: b2}
                        match v5 {
                          Wrap::Full {h@b4: Option[Holder]} => {
                            match b4 {
                              Some(b6: Holder) => {
                                let v8: String = b6.tag
                                write_string(v8)
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
                      Wrap::Empty => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Option[Holder] = None
                  let v1: Wrap = Full {h: v0}
                  let v2: Array[Wrap] = [v1]
                  for b0: Wrap in v2 {
                    match b0 {
                      Wrap::Full {h@b2: Option[Holder]} => {
                        match b2 {
                          Some(b6: Holder) => {
                            let v8: String = b6.tag
                            write_string(v8)
                          }
                          None => {
                            write("re")
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
                fn pick@f0(t@b2: Tree) -> Option[Tree] {
                  let v3: Option[Tree] = match b2 {
                    Tree::Node {kid@b4: Option[Tree]} => {
                      b4
                    }
                    Tree::Leaf => {
                      let v2: Option[Tree] = None
                      v2
                    }
                  }
                  v3
                }
                page Test() {
                  let v4: String = "a"
                  let v5: Option[Tree] = None
                  let v6: Tree = Node {label: v4, kid: v5}
                  let v7: Array[Tree] = [v6]
                  for b0: Tree in v7 {
                    let v9: Option[Tree] = call pick@f0(b0)
                    match v9 {
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
                  let v4: String = "a"
                  let v5: Option[Tree] = None
                  let v6: Tree = Node {label: v4, kid: v5}
                  let v7: Array[Tree] = [v6]
                  for b0: Tree in v7 {
                    let v18: Option[Tree] = match b0 {
                      Tree::Node {kid@b5: Option[Tree]} => {
                        b5
                      }
                      Tree::Leaf => {
                        let v17: Option[Tree] = None
                        v17
                      }
                    }
                    match v18 {
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
                fn depth@f0(t@b3: Option[Tree]) -> Int {
                  let v3: Int = match b3 {
                    Some(_) => {
                      let v1: Int = 1
                      v1
                    }
                    None => {
                      let v2: Int = 0
                      v2
                    }
                  }
                  v3
                }
                page Test() {
                  let v4: String = "a"
                  let v5: Option[Tree] = None
                  let v6: Tree = Node {label: v4, kid: v5}
                  let v7: Array[Tree] = [v6]
                  for b0: Tree in v7 {
                    match b0 {
                      Tree::Node {kid@b2: Option[Tree]} => {
                        let v10: Int = call depth@f0(b2)
                        let v11: String = v10.to_string()
                        write_string(v11)
                      }
                      Tree::Leaf => {
                        write("empty")
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v4: String = "a"
                  let v5: Option[Tree] = None
                  let v6: Tree = Node {label: v4, kid: v5}
                  let v7: Array[Tree] = [v6]
                  for b0: Tree in v7 {
                    match b0 {
                      Tree::Node {kid@b2: Option[Tree]} => {
                        let v20: Int = match b2 {
                          Some(_) => {
                            let v18: Int = 1
                            v18
                          }
                          None => {
                            let v19: Int = 0
                            v19
                          }
                        }
                        let v11: String = v20.to_string()
                        write_string(v11)
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
                fn Greeting@f0(name@b0: String) -> Html {
                  write("Hello, ")
                  write_string(b0)
                  write("!")
                }
                page Test() {
                  let v5: String = "World"
                  write_function Greeting@f0(v5)
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
                fn Card@f0(title@b0: String, children@b1: Html) -> Html {
                  write("<div class=\"card\"><h2>")
                  write_string(b0)
                  write("</h2>")
                  write_html(b1)
                  write("</div>")
                }
                page Test() {
                  let v8: String = "Hello"
                  let v12: Html = html {
                    write("<p>world</p>")
                  }
                  write_function Card@f0(v8, v12)
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
                fn Inner@f1(children@b1: Html) -> Html {
                  write("<div class=\"inner\">")
                  write_html(b1)
                  write("</div>")
                }
                fn Outer@f0(children@b0: Html) -> Html {
                  let v6: Html = html {
                    write_html(b0)
                  }
                  write("<div class=\"outer\">")
                  write_function Inner@f1(v6)
                  write("</div>")
                }
                page Test() {
                  let v13: Html = html {
                    write("<p>hello</p>")
                  }
                  write_function Outer@f0(v13)
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
                fn Header@f0(title@b0: String) -> Html {
                  write("<header><h1>")
                  write_string(b0)
                  write("</h1></header>")
                }
                fn Layout@f2(children@b1: Html) -> Html {
                  write("<div class=\"layout\">")
                  write_html(b1)
                  write("</div>")
                }
                page Test() {
                  let v15: String = "Welcome"
                  let v23: Html = html {
                    write_function Header@f0(v15)
                    write("<main><p>Hello world</p></main>")
                    write_function Footer@f1()
                  }
                  write_function Layout@f2(v23)
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
                fn Repeat@f0(children@b0: Html) -> Html {
                  write("<div class=\"first\">")
                  write_html(b0)
                  write("</div><div class=\"second\">")
                  write_html(b0)
                  write("</div>")
                }
                page Test() {
                  let v12: Html = html {
                    write("<span>hi</span>")
                  }
                  write_function Repeat@f0(v12)
                }
                -- ir (optimized) --
                page Test() {
                  let v11: Html = html {
                    write("<span>hi</span>")
                  }
                  write("<div class=\"first\">")
                  write_html(v11)
                  write("</div><div class=\"second\">")
                  write_html(v11)
                  write("</div>")
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
                fn Badge@f1(text@b4: String) -> Html {
                  write("<strong>")
                  write_string(b4)
                  write("</strong>")
                }
                fn NodeView@f0(node@b1: Node) -> Html {
                  let v5: String = b1.value
                  let v8: Option[Node] = b1.next
                  write_function Badge@f1(v5)
                  match v8 {
                    Some(b3: Node) => {
                      write_function NodeView@f0(b3)
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v14: String = "a"
                  let v15: String = "b"
                  let v16: Option[Node] = None
                  let v17: Node = {value: v15, next: v16}
                  let v18: Option[Node] = Some(v17)
                  let v19: Node = {value: v14, next: v18}
                  write_function NodeView@f0(v19)
                }
                -- ir (optimized) --
                fn NodeView@f0(node@b1: Node) -> Html {
                  let v5: String = b1.value
                  let v8: Option[Node] = b1.next
                  write("<strong>")
                  write_string(v5)
                  write("</strong>")
                  match v8 {
                    Some(b3: Node) => {
                      write_function NodeView@f0(b3)
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v14: String = "a"
                  let v15: String = "b"
                  let v16: Option[Node] = None
                  let v17: Node = {value: v15, next: v16}
                  let v18: Option[Node] = Some(v17)
                  let v19: Node = {value: v14, next: v18}
                  write_function NodeView@f0(v19)
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
                fn NodeView@f0(node@b1: Node) -> Html {
                  let v1: String = b1.value
                  let v6: Option[Node] = b1.next
                  write("<span>")
                  write_string(v1)
                  write("</span>")
                  match v6 {
                    Some(b3: Node) => {
                      write_function NodeView@f0(b3)
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v12: String = "a"
                  let v13: String = "b"
                  let v14: String = "c"
                  let v15: Option[Node] = None
                  let v16: Node = {value: v14, next: v15}
                  let v17: Option[Node] = Some(v16)
                  let v18: Node = {value: v13, next: v17}
                  let v19: Option[Node] = Some(v18)
                  let v20: Node = {value: v12, next: v19}
                  write_function NodeView@f0(v20)
                }
                -- ir (optimized) --
                fn NodeView@f0(node@b1: Node) -> Html {
                  let v1: String = b1.value
                  let v6: Option[Node] = b1.next
                  write("<span>")
                  write_string(v1)
                  write("</span>")
                  match v6 {
                    Some(b3: Node) => {
                      write_function NodeView@f0(b3)
                    }
                    None => {
                    }
                  }
                }
                page Test() {
                  let v12: String = "a"
                  let v13: String = "b"
                  let v14: String = "c"
                  let v15: Option[Node] = None
                  let v16: Node = {value: v14, next: v15}
                  let v17: Option[Node] = Some(v16)
                  let v18: Node = {value: v13, next: v17}
                  let v19: Option[Node] = Some(v18)
                  let v20: Node = {value: v12, next: v19}
                  write_function NodeView@f0(v20)
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
                fn Card@f0(title@b0: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write("</div>")
                }
                page Test() {
                  let v4: String = "New card"
                  write_function Card@f0(v4)
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
                fn Offset@f0(dx@b0: Int, scale@b1: Float) -> Html {
                  let v1: Int = 3
                  let v2: Int = b0 * v1
                  let v3: String = v2.to_string()
                  let v7: Float = 2
                  let v8: Float = b1 * v7
                  let v9: Int = v8.to_int()
                  let v10: String = v9.to_string()
                  write("<div>")
                  write_string(v3)
                  write(" ")
                  write_string(v10)
                  write("</div>")
                }
                page Test() {
                  let v14: Int = -1
                  let v15: Float = -2.5
                  write_function Offset@f0(v14, v15)
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
                fn Card@f0(title@b0: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write("</div>")
                }
                page Test() {
                  let v4: String = "Custom title"
                  write_function Card@f0(v4)
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
                fn Card@f0(title@b0: String, subtitle@b1: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write(" - ")
                  write_string(b1)
                  write("</div>")
                }
                page Test() {
                  let v7: String = "Hello"
                  let v8: String = "No subtitle"
                  write_function Card@f0(v7, v8)
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
                fn Card@f0(title@b0: String, subtitle@b1: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write(" - ")
                  write_string(b1)
                  write("</div>")
                }
                page Test() {
                  let v7: String = "Hello"
                  let v8: String = "World"
                  write_function Card@f0(v7, v8)
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
                fn Card@f0(title@b0: String, subtitle@b1: String, footer@b2: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write(" - ")
                  write_string(b1)
                  write(" - ")
                  write_string(b2)
                  write("</div>")
                }
                page Test() {
                  let v10: String = "Default"
                  let v11: String = "Custom"
                  let v12: String = "End"
                  write_function Card@f0(v10, v11, v12)
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
                fn Card@f0(title@b0: String, children@b1: Html) -> Html {
                  write("<div class=\"card\"><h2>")
                  write_string(b0)
                  write("</h2>")
                  write_html(b1)
                  write("</div>")
                }
                page Test() {
                  let v8: String = "Hello"
                  let v9: Html = html {
                  }
                  write_function Card@f0(v8, v9)
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
                fn Card@f0(title@b0: String, children@b1: Html) -> Html {
                  write("<div class=\"card\"><h2>")
                  write_string(b0)
                  write("</h2>")
                  write_html(b1)
                  write("</div>")
                }
                page Test() {
                  let v8: String = "With"
                  let v12: Html = html {
                    write("<p>body</p>")
                  }
                  let v14: String = "Without"
                  let v15: Html = html {
                  }
                  write_function Card@f0(v8, v12)
                  write_function Card@f0(v14, v15)
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
                  let v0: String = ""
                  let v1: Bool = v0.is_empty()
                  match v1 {
                    true => {
                      write("empty")
                    }
                    false => {
                      write("not empty")
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
                  let v0: String = "hello"
                  let v1: Bool = v0.is_empty()
                  match v1 {
                    true => {
                      write("empty")
                    }
                    false => {
                      write("not empty")
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
                  let v0: String = "hello"
                  let v1: Option[String] = Some(v0)
                  let v2: Bool = v1.is_some()
                  match v2 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
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
                  let v0: Option[String] = None
                  let v1: Bool = v0.is_some()
                  match v1 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
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
                  let v0: Option[String] = None
                  let v1: Bool = v0.is_none()
                  match v1 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
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
                  let v0: String = "hello"
                  let v1: Option[String] = Some(v0)
                  let v2: Bool = v1.is_none()
                  match v2 {
                    true => {
                      write("yes")
                    }
                    false => {
                      write("no")
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
                  let v0: Option[Bool] = None
                  let v1: Bool = true
                  let v2: Bool = v0.is_none()
                  let v3: Bool = v1 == v2
                  match v3 {
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
                  let v0: String = "Alice"
                  let v1: Option[String] = Some(v0)
                  let v4: String = match v1 {
                    Some(b1: String) => {
                      b1
                    }
                    None => {
                      let v3: String = "anonymous"
                      v3
                    }
                  }
                  write_string(v4)
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
                fn Greeting@f0(name@b0: Option[String]) -> Html {
                  let v3: String = match b0 {
                    Some(b1: String) => {
                      b1
                    }
                    None => {
                      let v2: String = "anonymous"
                      v2
                    }
                  }
                  write_string(v3)
                }
                page Test() {
                  let v6: Option[String] = None
                  write_function Greeting@f0(v6)
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
                  let v0: String = "a"
                  let v1: Bool = v0.is_empty()
                  let v2: String = "b"
                  let v3: Bool = v2.is_empty()
                  let v4: Bool = v1 == v3
                  match v4 {
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
                fn RenderItem@f0(item@b0: Item) -> Html {
                  match b0 {
                    Item::Todo {label@b2: String, done@b3: Bool} => {
                      let v7: Bool = !b3
                      match b3 {
                        true => {
                          write("[x]")
                        }
                        false => {
                        }
                      }
                      match v7 {
                        true => {
                          write("[ ]")
                        }
                        false => {
                        }
                      }
                      write_string(b2)
                    }
                  }
                }
                page Test() {
                  let v16: String = "Buy milk"
                  let v17: Bool = true
                  let v18: Item = Todo {label: v16, done: v17}
                  let v21: String = "Walk dog"
                  let v22: Bool = false
                  let v23: Item = Todo {label: v21, done: v22}
                  write_function RenderItem@f0(v18)
                  write(",")
                  write_function RenderItem@f0(v23)
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
                fn Render@f0(time@b0: TimeAgo) -> Html {
                  match b0 {
                    TimeAgo::MinutesAgo {count@b2: Int} => {
                      let v2: Int = 1
                      let v3: Bool = b2 == v2
                      match v3 {
                        true => {
                          write("1 minute ago")
                        }
                        false => {
                          let v7: String = b2.to_string()
                          write_string(v7)
                          write(" minutes ago")
                        }
                      }
                    }
                    TimeAgo::HoursAgo {count@b4: Int} => {
                      let v14: Int = 1
                      let v15: Bool = b4 == v14
                      match v15 {
                        true => {
                          write("1 hour ago")
                        }
                        false => {
                          let v19: String = b4.to_string()
                          write_string(v19)
                          write(" hours ago")
                        }
                      }
                    }
                  }
                }
                page Test() {
                  let v26: Int = 1
                  let v27: TimeAgo = MinutesAgo {count: v26}
                  let v30: Int = 5
                  let v31: TimeAgo = MinutesAgo {count: v30}
                  let v34: Int = 1
                  let v35: TimeAgo = HoursAgo {count: v34}
                  write_function Render@f0(v27)
                  write(",")
                  write_function Render@f0(v31)
                  write(",")
                  write_function Render@f0(v35)
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
                fn RenderCode@f0(block@b0: CodeBlock) -> Html {
                  match b0 {
                    CodeBlock::Snippet {code@b2: String} => {
                      write("<code>")
                      write_string(b2)
                      write("</code>")
                    }
                  }
                }
                page Test() {
                  let v6: String = "rust"
                  let v7: String = "fn main()"
                  let v8: CodeBlock = Snippet {language: v6, code: v7}
                  write_function RenderCode@f0(v8)
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
                fn Render@f0(el@b0: ButtonElement) -> Html {
                  match b0 {
                    ButtonElement::Link {href@b2: String} => {
                      write("<a href=\"")
                      write_string(b2)
                      write("\">link</a>")
                    }
                    ButtonElement::Button {type@b3: String} => {
                      write("<button type=\"")
                      write_string(b3)
                      write("\">btn</button>")
                    }
                  }
                }
                page Test() {
                  let v10: Bool = false
                  let v11: String = "submit"
                  let v12: ButtonElement = Button {disabled: v10, type: v11}
                  write_function Render@f0(v12)
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
                  let v0: String = "1"
                  let v1: String = "hello"
                  let v2: Target = {id: v0, title: v1}
                  let v3: Option[Target] = Some(v2)
                  let v8: Option[String] = match v3 {
                    Some(b2: Target) => {
                      let v5: String = b2.title
                      let v6: Option[String] = Some(v5)
                      v6
                    }
                    None => {
                      let v7: Option[String] = None
                      v7
                    }
                  }
                  let v9: Array[Option[String]] = [v8]
                  for b4: Option[String] in v9 {
                    match b4 {
                      Some(b6: String) => {
                        write("[")
                        write_string(b6)
                        write("]")
                      }
                      None => {
                      }
                    }
                  }
                  match v3 {
                    Some(b8: Target) => {
                      let v20: String = b8.title
                      write_string(v20)
                    }
                    None => {
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v1: String = "hello"
                  let v6: Option[String] = Some(v1)
                  let v9: Array[Option[String]] = [v6]
                  for b4: Option[String] in v9 {
                    match b4 {
                      Some(b6: String) => {
                        write("[")
                        write_string(b6)
                        write("]")
                      }
                      None => {
                      }
                    }
                  }
                  write_string(v1)
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
                fn Nest@f0(depth@b0: Int, children@b1: Html) -> Html {
                  let v0: Int = 0
                  let v2: Bool = v0 < b0
                  match v2 {
                    true => {
                      let v4: Int = 1
                      let v5: Int = b0 - v4
                      let v7: Html = html {
                        write_html(b1)
                      }
                      write("<div>")
                      write_function Nest@f0(v5, v7)
                      write("</div>")
                    }
                    false => {
                      write_html(b1)
                    }
                  }
                }
                page Test() {
                  let v13: Int = 2
                  let v17: Html = html {
                    write("<b>x</b>")
                  }
                  write_function Nest@f0(v13, v17)
                }
                -- ir (optimized) --
                fn Nest@f0(depth@b0: Int, children@b1: Html) -> Html {
                  let v0: Int = 0
                  let v2: Bool = v0 < b0
                  match v2 {
                    true => {
                      let v4: Int = 1
                      let v5: Int = b0 - v4
                      write("<div>")
                      write_function Nest@f0(v5, b1)
                      write("</div>")
                    }
                    false => {
                      write_html(b1)
                    }
                  }
                }
                page Test() {
                  let v13: Int = 2
                  let v16: Html = html {
                    write("<b>x</b>")
                  }
                  write_function Nest@f0(v13, v16)
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
                fn Foo@f0(children@b0: Html) -> Html {
                  write("<div>")
                  write_html(b0)
                  write("</div>")
                }
                page Test() {
                  let v6: Html = html {
                    write("<b>hi</b>")
                  }
                  write_function Foo@f0(v6)
                }
                -- ir (optimized) --
                page Test() {
                  write("<div><b>hi</b></div>")
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
                fn Inner@f1(children@b1: Html) -> Html {
                  write("<em>")
                  write_html(b1)
                  write("</em>")
                }
                fn Outer@f0(children@b0: Html) -> Html {
                  let v4: Html = html {
                    write_html(b0)
                  }
                  write("<section>")
                  write_function Inner@f1(v4)
                  write("</section>")
                }
                page Test() {
                  let v9: Html = html {
                    write("z")
                  }
                  write_function Outer@f0(v9)
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
                fn Nest@f0(n@b0: Int, id@b1: String) -> Html {
                  let v1: Int = 0
                  let v3: Bool = v1 < b0
                  write("<div id=\"")
                  write_string(b1)
                  write("\">")
                  match v3 {
                    true => {
                      let v5: Int = 1
                      let v6: Int = b0 - v5
                      write_function Nest@f1(v6)
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                fn Nest@f1(n@b3: Int) -> Html {
                  let v12: Int = 0
                  let v14: Bool = v12 < b3
                  write("<div>")
                  match v14 {
                    true => {
                      let v16: Int = 1
                      let v17: Int = b3 - v16
                      write_function Nest@f1(v17)
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  let v23: Int = 2
                  let v24: String = "root"
                  write_function Nest@f0(v23, v24)
                }
                -- ir (optimized) --
                fn Nest@f1(n@b3: Int) -> Html {
                  let v12: Int = 0
                  let v14: Bool = v12 < b3
                  write("<div>")
                  match v14 {
                    true => {
                      let v16: Int = 1
                      let v17: Int = b3 - v16
                      write_function Nest@f1(v17)
                    }
                    false => {
                    }
                  }
                  write("</div>")
                }
                page Test() {
                  let v29: Int = 1
                  write("<div id=\"root\">")
                  write_function Nest@f1(v29)
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
                fn Countdown@f0(n@b0: Int) -> Html {
                  let v1: String = b0.to_string()
                  let v3: Int = 0
                  let v5: Bool = v3 < b0
                  write_string(v1)
                  match v5 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = b0 - v7
                      write_function Countdown@f0(v8)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v13: Int = 3
                  write_function Countdown@f0(v13)
                }
                -- ir (optimized) --
                fn Countdown@f0(n@b0: Int) -> Html {
                  let v1: String = b0.to_string()
                  let v3: Int = 0
                  let v5: Bool = v3 < b0
                  write_string(v1)
                  match v5 {
                    true => {
                      let v7: Int = 1
                      let v8: Int = b0 - v7
                      write_function Countdown@f0(v8)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v13: Int = 3
                  write_function Countdown@f0(v13)
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
                fn Loop@f0(n@b0: Int, label@b1: Option[String]) -> Html {
                  let v7: Int = 0
                  let v9: Bool = v7 < b0
                  match b1 {
                    Some(b3: String) => {
                      write_string(b3)
                    }
                    None => {
                      write("x")
                    }
                  }
                  match v9 {
                    true => {
                      let v11: Int = 1
                      let v12: Int = b0 - v11
                      write_function Loop@f0(v12, b1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v18: Int = 2
                  let v19: String = "a"
                  let v20: Option[String] = Some(v19)
                  write_function Loop@f0(v18, v20)
                }
                -- ir (optimized) --
                fn Loop@f0(n@b0: Int, label@b1: Option[String]) -> Html {
                  let v7: Int = 0
                  let v9: Bool = v7 < b0
                  match b1 {
                    Some(b3: String) => {
                      write_string(b3)
                    }
                    None => {
                      write("x")
                    }
                  }
                  match v9 {
                    true => {
                      let v11: Int = 1
                      let v12: Int = b0 - v11
                      write_function Loop@f0(v12, b1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v18: Int = 2
                  let v19: String = "a"
                  let v20: Option[String] = Some(v19)
                  write_function Loop@f0(v18, v20)
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
                fn C@f0(x@b1: Option[String]) -> Html {
                  let v1: Bool = b1.is_none()
                  match v1 {
                    true => {
                      write_function C@f0(b1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v6: String = "a"
                  let v7: Option[String] = Some(v6)
                  write_function C@f0(v7)
                  write_function C@f0(v7)
                }
                -- ir (optimized) --
                fn C@f0(x@b1: Option[String]) -> Html {
                  let v1: Bool = b1.is_none()
                  match v1 {
                    true => {
                      write_function C@f0(b1)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v6: String = "a"
                  let v7: Option[String] = Some(v6)
                  write_function C@f0(v7)
                  write_function C@f0(v7)
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
                fn Even@f0(n@b0: Int) -> Html {
                  let v1: Int = 0
                  let v2: Bool = b0 == v1
                  let v7: Int = 0
                  let v9: Bool = v7 < b0
                  match v2 {
                    true => {
                      write("even")
                    }
                    false => {
                    }
                  }
                  match v9 {
                    true => {
                      let v11: Int = 1
                      let v12: Int = b0 - v11
                      write_function Odd@f1(v12)
                    }
                    false => {
                    }
                  }
                }
                fn Odd@f1(n@b3: Int) -> Html {
                  let v18: Int = 0
                  let v19: Bool = b3 == v18
                  let v24: Int = 0
                  let v26: Bool = v24 < b3
                  match v19 {
                    true => {
                      write("odd")
                    }
                    false => {
                    }
                  }
                  match v26 {
                    true => {
                      let v28: Int = 1
                      let v29: Int = b3 - v28
                      write_function Even@f0(v29)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v34: Int = 4
                  write_function Even@f0(v34)
                }
                -- ir (optimized) --
                fn Even@f0(n@b0: Int) -> Html {
                  let v1: Int = 0
                  let v2: Bool = b0 == v1
                  let v7: Int = 0
                  let v9: Bool = v7 < b0
                  match v2 {
                    true => {
                      write("even")
                    }
                    false => {
                    }
                  }
                  match v9 {
                    true => {
                      let v11: Int = 1
                      let v12: Int = b0 - v11
                      write_function Odd@f1(v12)
                    }
                    false => {
                    }
                  }
                }
                fn Odd@f1(n@b3: Int) -> Html {
                  let v18: Int = 0
                  let v19: Bool = b3 == v18
                  let v24: Int = 0
                  let v26: Bool = v24 < b3
                  match v19 {
                    true => {
                      write("odd")
                    }
                    false => {
                    }
                  }
                  match v26 {
                    true => {
                      let v28: Int = 1
                      let v29: Int = b3 - v28
                      write_function Even@f0(v29)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v34: Int = 4
                  write_function Even@f0(v34)
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
                  let v0: Bool = true
                  let v1: R = {f: v0}
                  let v2: Bool = v1.f
                  match v2 {
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
                fn C@f0(p@b0: Array[String]) -> Html {
                  for _ in b0 {
                    let v1: Array[String] = []
                    write_function C@f0(v1)
                  }
                }
                page Test() {
                  let v4: String = "a"
                  let v5: Array[String] = [v4]
                  write_function C@f0(v5)
                }
                -- ir (optimized) --
                fn C@f0(p@b0: Array[String]) -> Html {
                  for _ in b0 {
                    let v1: Array[String] = []
                    write_function C@f0(v1)
                  }
                }
                page Test() {
                  let v4: String = "a"
                  let v5: Array[String] = [v4]
                  write_function C@f0(v5)
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
                fn C@f0(p@b0: Array[String]) -> Html {
                  let v1: R = {f: b0}
                  let v2: Array[String] = v1.f
                  let v3: Bool = v2.is_empty()
                  match v3 {
                    true => {
                      let v4: Array[String] = []
                      write_function C@f0(v4)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v8: String = "a"
                  let v9: Array[String] = [v8]
                  write_function C@f0(v9)
                }
                -- ir (optimized) --
                fn C@f0(p@b0: Array[String]) -> Html {
                  let v3: Bool = b0.is_empty()
                  match v3 {
                    true => {
                      let v4: Array[String] = []
                      write_function C@f0(v4)
                    }
                    false => {
                    }
                  }
                }
                page Test() {
                  let v8: String = "a"
                  let v9: Array[String] = [v8]
                  write_function C@f0(v9)
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
                fn OptBool@f0(checked@b0: Option[Bool]) -> Html {
                  match b0 {
                    Some(b2: Bool) => {
                      match b2 {
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
                page Test() {
                  let v11: Bool = true
                  let v12: Option[Bool] = Some(v11)
                  write_function OptBool@f0(v12)
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
                  let v0: Bool = true
                  let v1: Option[Bool] = Some(v0)
                  let v2: Option[Option[Bool]] = Some(v1)
                  match v2 {
                    Some(b2: Option[Bool]) => {
                      match b2 {
                        Some(b3: Bool) => {
                          match b3 {
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
                  let v0: Int = 1
                  let v1: Int = 2
                  let v2: Int = 3
                  let v3: Array[Int] = [v0, v1, v2]
                  for b0: Int in v3 {
                    let v4: Int = 1
                    let v6: Bool = v4 < b0
                    match v6 {
                      true => {
                        let v8: String = b0.to_string()
                        write_string(v8)
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: Int = 1
                  let v1: Int = 2
                  let v2: Int = 3
                  let v3: Array[Int] = [v0, v1, v2]
                  for b0: Int in v3 {
                    let v4: Int = 1
                    let v6: Bool = v4 < b0
                    match v6 {
                      true => {
                        let v8: String = b0.to_string()
                        write_string(v8)
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
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    let v4: String = "a"
                    let v5: Bool = b0 == v4
                    match v5 {
                      true => {
                        write_string(b0)
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: String = "b"
                  let v2: Array[String] = [v0, v1]
                  for b0: String in v2 {
                    let v4: String = "a"
                    let v5: Bool = b0 == v4
                    match v5 {
                      true => {
                        write_string(b0)
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
                  let v0: Float = 1.5
                  let v1: Float = 2.5
                  let v2: Array[Float] = [v0, v1]
                  for b0: Float in v2 {
                    let v3: Float = 2
                    let v5: Bool = v3 < b0
                    match v5 {
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
                  let v0: Float = 1.5
                  let v1: Float = 2.5
                  let v2: Array[Float] = [v0, v1]
                  for b0: Float in v2 {
                    let v3: Float = 2
                    let v5: Bool = v3 < b0
                    match v5 {
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
                  let v0: Bool = true
                  let v1: Bool = false
                  let v2: Array[Bool] = [v0, v1]
                  for b0: Bool in v2 {
                    let v5: Bool = match b0 {
                      true => {
                        let v4: Bool = true
                        v4
                      }
                      false => {
                        b0
                      }
                    }
                    match v5 {
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
                  let v0: Bool = true
                  let v1: Bool = false
                  let v2: Array[Bool] = [v0, v1]
                  for b0: Bool in v2 {
                    let v5: Bool = match b0 {
                      true => {
                        let v4: Bool = true
                        v4
                      }
                      false => {
                        b0
                      }
                    }
                    match v5 {
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
                fn Show@f0(label@b2: String) -> Html {
                  write("<span>")
                  write_string(b2)
                  write("</span>")
                }
                page Test() {
                  let v4: String = "a"
                  let v5: String = "b"
                  let v6: Array[String] = [v4, v5]
                  for b0: String in v6 {
                    let v8: String = "a"
                    let v9: Bool = b0 == v8
                    match v9 {
                      true => {
                        write_function Show@f0(b0)
                      }
                      false => {
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v4: String = "a"
                  let v5: String = "b"
                  let v6: Array[String] = [v4, v5]
                  for b0: String in v6 {
                    let v8: String = "a"
                    let v9: Bool = b0 == v8
                    match v9 {
                      true => {
                        write("<span>")
                        write_string(b0)
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
                  let v0: String = "a"
                  let v1: Foo = {class: v0}
                  let v2: String = v1.class
                  write("<div>")
                  write_string(v2)
                  write("</div>")
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
                  let v0: String = "a"
                  let v1: Foo = {function: v0}
                  let v2: String = v1.function
                  write("<div>")
                  write_string(v2)
                  write("</div>")
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
                  let v0: String = "a"
                  let v1: Foo = {protected: v0}
                  let v2: String = v1.protected
                  write("<div>")
                  write_string(v2)
                  write("</div>")
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
                  let v0: String = "a"
                  let v1: Foo = {eval: v0}
                  let v2: String = v1.eval
                  write("<div>")
                  write_string(v2)
                  write("</div>")
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
                  let v0: String = "a"
                  let v1: E = A {class: v0}
                  match v1 {
                    E::A {class@b2: String} => {
                      write("<div>")
                      write_string(b2)
                      write("</div>")
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
                  let v0: Int = 4
                  let v1: Math = {x: v0}
                  let v2: Int = 5
                  let v3: Int = v1.x
                  let v4: Int = v3 * v2
                  let v5: String = v4.to_string()
                  write_string(v5)
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
                  let v0: Float = 3.7
                  let v1: Number = {x: v0}
                  let v2: Float = v1.x
                  let v3: Int = v2.to_int()
                  let v4: String = v3.to_string()
                  write_string(v4)
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
                  let v0: String = "a"
                  let v1: Int = 1
                  let v2: State = {query: v0, num: v1}
                  let v3: String = v2.query
                  let v4: Int = 2
                  let v5: State = {query: v3, num: v4}
                  let v6: String = v5.query
                  let v8: Int = v5.num
                  let v9: String = v8.to_string()
                  write_string(v6)
                  write_string(v9)
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
                  let v0: String = "a"
                  let v1: Int = 1
                  let v2: State = {query: v0, num: v1}
                  let v3: String = "b"
                  let v4: Int = 2
                  let v5: State = {query: v3, num: v4}
                  let v6: String = v5.query
                  let v8: Int = v5.num
                  let v9: String = v8.to_string()
                  write_string(v6)
                  write_string(v9)
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
                  let v0: String = "a"
                  let v1: Int = 7
                  let v2: State = {query: v0, num: v1}
                  let v3: Array[State] = [v2]
                  for b0: State in v3 {
                    let v5: String = "x"
                    let v6: Int = b0.num
                    let v7: State = {query: v5, num: v6}
                    let v8: String = v7.query
                    let v11: String = "x"
                    let v12: Int = b0.num
                    let v13: State = {query: v11, num: v12}
                    let v14: Int = v13.num
                    let v15: String = v14.to_string()
                    write_string(v8)
                    write_string(v15)
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v0: String = "a"
                  let v1: Int = 7
                  let v2: State = {query: v0, num: v1}
                  let v3: Array[State] = [v2]
                  for b0: State in v3 {
                    let v12: Int = b0.num
                    let v15: String = v12.to_string()
                    write("x")
                    write_string(v15)
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
                fn Row@f0(item@b4: Item) -> Html {
                  let v1: String = b4.label
                  write("<div>")
                  write_string(v1)
                  write("</div>")
                }
                page Test() {
                  let v5: String = "a"
                  let v6: Bool = false
                  let v7: Item = {label: v5, selected: v6}
                  let v8: Array[Item] = [v7]
                  for b0: Item in v8 {
                    let v10: Bool = b0.selected
                    match v10 {
                      true => {
                        let v12: String = "on"
                        let v13: Bool = b0.selected
                        let v14: Item = {label: v12, selected: v13}
                        write_function Row@f0(v14)
                      }
                      false => {
                        let v17: String = "off"
                        let v18: Bool = b0.selected
                        let v19: Item = {label: v17, selected: v18}
                        write_function Row@f0(v19)
                      }
                    }
                  }
                }
                -- ir (optimized) --
                page Test() {
                  let v5: String = "a"
                  let v6: Bool = false
                  let v7: Item = {label: v5, selected: v6}
                  let v8: Array[Item] = [v7]
                  for b0: Item in v8 {
                    let v10: Bool = b0.selected
                    match v10 {
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
                fn Dark@f0(s@b1: State) -> Html {
                  let v1: Settings = b1.settings
                  let v2: String = "dark"
                  let v3: Bool = v1.compact
                  let v4: Settings = {theme: v2, compact: v3}
                  let v6: String = b1.query
                  let v7: State = {query: v6, settings: v4}
                  let v8: String = v7.query
                  let v10: Settings = v7.settings
                  let v11: String = v10.theme
                  write_string(v8)
                  write_string(v11)
                }
                page Test() {
                  let v14: String = "light"
                  let v15: Bool = true
                  let v16: Settings = {theme: v14, compact: v15}
                  let v17: String = "q"
                  let v18: State = {query: v17, settings: v16}
                  write_function Dark@f0(v18)
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
                  let v0: String = "bar"
                  let v1: String = "baz"
                  let v2: Foo = {x: v0, y: v1}
                  let v3: String = v2.x
                  let v4: String = "foo"
                  let v5: Foo = {x: v3, y: v4}
                  let v6: String = v5.x
                  write_string(v6)
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
                fn label@f0(prefix@b0: String, count@b1: Int) -> String {
                  let v2: String = b1.to_string()
                  let v3: String = concat(b0, v2)
                  v3
                }
                page Test() {
                  let v4: String = "a"
                  let v5: Int = 1
                  let v6: String = call label@f0(v4, v5)
                  write("<div>")
                  write_string(v6)
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
                fn label@f0(prefix@b1: String, count@b2: Int) -> String {
                  let v2: String = b2.to_string()
                  let v3: String = concat(b1, v2)
                  v3
                }
                page Test() {
                  let v4: String = "x"
                  let v5: Int = 1
                  let v6: String = call label@f0(v4, v5)
                  let v8: String = "x"
                  let v9: Int = 2
                  let v10: String = call label@f0(v8, v9)
                  let v12: String = "y"
                  let v13: Int = 1
                  let v14: String = call label@f0(v12, v13)
                  write("<div>")
                  write_string(v6)
                  write_string(v10)
                  write_string(v14)
                  write("</div>")
                }
                page Other(prefix@b0: String) {
                  let v19: Int = 1
                  let v20: String = call label@f0(b0, v19)
                  write("<div>")
                  write_string(v20)
                  write("</div>")
                }
                -- ir (optimized) --
                page Test() {
                  write("<div>x1x2y1</div>")
                }
                page Other(prefix@b0: String) {
                  write("<div>")
                  write_string(b0)
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
                  let v0: Int = 0
                  let v1: Int = -7
                  let v2: Int = call foo@f1(v1)
                  let v9: Int = 10
                  let v10: Int = call foo@f1(v9)
                  let v11: String = v10.to_string()
                  write("<div>")
                  for b0: Int in v0..=v2 {
                    let v4: String = b0.to_string()
                    write_string(v4)
                    write(",")
                  }
                  write_string(v11)
                  write("</div>")
                }
                fn foo@f1(x@b1: Int) -> Int {
                  let v16: Int = 10
                  let v17: Int = b1 + v16
                  v17
                }
                page Test() {
                  write_function Wrapper@f0()
                }
                -- ir (optimized) --
                page Test() {
                  let v23: Int = 0
                  let v26: Int = 3
                  write("<div>")
                  for b2: Int in v23..=v26 {
                    let v28: String = b2.to_string()
                    write_string(v28)
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
                fn card@f0(label@b0: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write("</div>")
                }
                page Test() {
                  let v4: String = "hello"
                  write_function card@f0(v4)
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
                fn wrap@f0(children@b0: Html) -> Html {
                  write("<div>")
                  write_html(b0)
                  write("</div>")
                }
                page Test() {
                  let v5: Html = html {
                    write("<span>hello</span>")
                  }
                  write_function wrap@f0(v5)
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
                fn Card@f0(slot@b0: Html) -> Html {
                  write("<div>")
                  write_html(b0)
                  write("</div>")
                }
                page Test() {
                  let v5: Html = html {
                    write("<span>hello</span>")
                  }
                  write_function Card@f0(v5)
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
                fn badge@f0(on@b0: Bool) -> Html {
                  match b0 {
                    true => {
                      write("<b>yes</b>")
                    }
                    false => {
                      write("<i>no</i>")
                    }
                  }
                }
                page Test() {
                  let v8: Bool = true
                  let v10: Bool = false
                  write("<div>")
                  write_function badge@f0(v8)
                  write_function badge@f0(v10)
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
                  let v0: String = "hello"
                  write_function card@f1(v0)
                }
                fn card@f1(label@b0: String) -> Html {
                  write("<div>")
                  write_string(b0)
                  write("</div>")
                }
                page Test() {
                  write_function Outer@f0()
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
                  let v0: Shape = call mk@f1()
                  let v5: Int = match v0 {
                    Shape::Circle => {
                      let v1: Int = 1
                      v1
                    }
                    Shape::Square => {
                      let v4: Int = match v0 {
                        Shape::Circle => {
                          let v2: Int = 3
                          v2
                        }
                        Shape::Square => {
                          let v3: Int = 2
                          v3
                        }
                      }
                      v4
                    }
                  }
                  v5
                }
                fn mk@f1() -> Shape {
                  let v6: Shape = Square
                  v6
                }
                page Test() {
                  let v7: Int = call f@f0()
                  let v8: String = v7.to_string()
                  write("<div>")
                  write_string(v8)
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
                  let v0: Shape = Square
                  v0
                }
                page Test() {
                  let v1: Shape = call mk@f0()
                  match v1 {
                    Shape::Circle => {
                      write("circle")
                    }
                    Shape::Square => {
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
                  let v0: Option[String] = call mk@f1()
                  let v5: String = match v0 {
                    Some(_) => {
                      let v3: String = match v0 {
                        Some(b2: String) => {
                          b2
                        }
                        None => {
                          let v2: String = "never"
                          v2
                        }
                      }
                      v3
                    }
                    None => {
                      let v4: String = "none"
                      v4
                    }
                  }
                  v5
                }
                fn mk@f1() -> Option[String] {
                  let v6: String = "hi"
                  let v7: Option[String] = Some(v6)
                  v7
                }
                page Test() {
                  let v8: String = call f@f0()
                  write("<div>")
                  write_string(v8)
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
                  write_function Card@f0()
                }
                page Other() {
                  write_function Card@f1()
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
                fn nav_bar@f1(x@b0: Int) -> Int {
                  b0
                }
                page Test() {
                  let v5: Int = 1
                  let v6: Int = call nav_bar@f1(v5)
                  let v7: String = v6.to_string()
                  write("<div>")
                  write_function NavBar@f0()
                  write_string(v7)
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
                fn label@f0(prefix@b0: String, count@b1: Int) -> String {
                  let v2: String = b1.to_string()
                  let v3: String = concat(b0, v2)
                  v3
                }
                page Test() {
                  let v4: String = "n"
                  let v5: Int = 2
                  let v6: String = call label@f0(v4, v5)
                  write("<div>")
                  write_string(v6)
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
