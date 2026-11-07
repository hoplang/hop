use crate::asset_path_rewriter::AssetPathRewriter;
use crate::config::TargetLanguage;
use crate::document::Document;
use crate::document_annotator::DocumentAnnotator;
use crate::ir::flat_program::FlatProgram;
use crate::ir::flat_program_generator::random_program_with_test_view;
use crate::ir::ir_page::IrPage;
use crate::ir::runtime::EvalError;
use crate::ir::runtime::flat_evaluator;
use crate::ir::{flat_to_writer, optimize_flat, transpile};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::root_relative_file_path::RootRelativeFilePath;
use crate::session::Session;
use crate::symbols::type_name::TypeName;
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

    let output = Command::new("tsc")
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

fn execute_evaluator(flat: &FlatProgram, pages: &[IrPage]) -> Result<String, String> {
    let page_name = TypeName::parse("Test").unwrap();
    flat_evaluator::evaluate_page(flat, pages, &page_name, HashMap::new(), None)
        .map_err(|e| format!("Evaluator failed: {}", e))
}

fn check(archive: &str, expected_output: &str) {
    check_with_asset_path_rewriter(archive, None, expected_output);
}

fn check_with_asset_path_rewriter(
    archive: &str,
    asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    expected_output: &str,
) {
    let archive = Archive::from(archive);
    let mut session = Session::new();
    let mut modules = 0;
    for file in archive.iter() {
        assert!(
            file.name.ends_with(".hop"),
            "expected a .hop module, got '{}'",
            file.name
        );
        let document_id = RootContainedFilePath::new(&file.name).unwrap();
        let document = Document::new(document_id.clone(), file.content.clone());
        session.update_hop_document(&document_id, document);
        modules += 1;
    }
    assert!(modules > 0, "archive declares no modules");

    let diagnostics = session.diagnostics();
    if !diagnostics.is_empty() {
        let rendered = DocumentAnnotator::new()
            .with_severity_label()
            .with_lines_before(1)
            .annotate(diagnostics)
            .render();
        eprintln!("{}", rendered);
        panic!("Diagnostics found");
    }

    let (unoptimized_flat, pages) = session.compile_flat(None, asset_path_rewriter);
    let optimized_flat = optimize_flat(unoptimized_flat.clone());

    // Evaluate the Flat programs before lowering consumes them.
    let unoptimized_eval = execute_evaluator(&unoptimized_flat, &pages);
    let optimized_eval = execute_evaluator(&optimized_flat, &pages);

    let unoptimized_program = flat_to_writer(unoptimized_flat, &pages, None);
    let optimized_program = flat_to_writer(optimized_flat, &pages, None);

    let unoptimized_ir = unoptimized_program.to_string();
    let optimized_ir = optimized_program.to_string();

    // Test evaluator on the unoptimized Flat program
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

    // Test evaluator on the optimized Flat program
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

    // Test unoptimized version
    let ts_code = transpile(TargetLanguage::Typescript, &unoptimized_program);
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

    let rust_code = transpile(TargetLanguage::Rust, &unoptimized_program);
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

    // Test optimized version
    let ts_code = transpile(TargetLanguage::Typescript, &optimized_program);
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

    let rust_code = transpile(TargetLanguage::Rust, &optimized_program);
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
}

#[test]
#[ignore]
fn fuzz_transpile_ts_renders_identically() {
    arbtest::arbtest(|u| {
        let program = random_program_with_test_view(u);
        let flat = program.to_string();
        let pages = IrPage::for_entrypoints(&program);
        let page_name = TypeName::parse("Test").unwrap();
        let expected =
            match flat_evaluator::evaluate_page(&program, &pages, &page_name, HashMap::new(), None)
            {
                Ok(output) => output.trim().to_string(),
                Err(
                    EvalError::CallDepthLimit { .. }
                    | EvalError::StringLengthLimit { .. }
                    | EvalError::HtmlSizeLimit { .. }
                    | EvalError::OpCountLimit { .. },
                ) => return Ok(()),
                Err(e) => panic!("Evaluator failed:\n{e}\n\nFlat:\n{flat}"),
            };
        let program = flat_to_writer(optimize_flat(program), &pages, None);
        let ir = program.to_string();
        let ts_code = transpile(TargetLanguage::Typescript, &program);
        if let Err(e) = typecheck_typescript(&ts_code) {
            panic!(
                "TypeScript typecheck failed:\n{e}\n\nFlat:\n{flat}\nIR:\n{ir}\nCode:\n{ts_code}"
            );
        }
        let ts_output = execute_typescript(&ts_code).unwrap_or_else(|e| {
            panic!("TypeScript failed:\n{e}\n\nFlat:\n{flat}\nIR:\n{ir}\nCode:\n{ts_code}")
        });
        assert_eq!(
            expected, ts_output,
            "evaluator and TypeScript disagree\n\nFlat:\n{flat}\nIR:\n{ir}\nCode:\n{ts_code}"
        );
        Ok(())
    });
}

#[test]
#[ignore]
fn fuzz_transpile_rust_renders_identically() {
    arbtest::arbtest(|u| {
        let program = random_program_with_test_view(u);
        let flat = program.to_string();
        let pages = IrPage::for_entrypoints(&program);
        let page_name = TypeName::parse("Test").unwrap();
        let expected =
            match flat_evaluator::evaluate_page(&program, &pages, &page_name, HashMap::new(), None)
            {
                Ok(output) => output.trim().to_string(),
                Err(
                    EvalError::CallDepthLimit { .. }
                    | EvalError::StringLengthLimit { .. }
                    | EvalError::HtmlSizeLimit { .. }
                    | EvalError::OpCountLimit { .. },
                ) => return Ok(()),
                Err(e) => panic!("Evaluator failed:\n{e}\n\nFlat:\n{flat}"),
            };
        let program = flat_to_writer(optimize_flat(program), &pages, None);
        let ir = program.to_string();
        let rust_code = transpile(TargetLanguage::Rust, &program);
        let rust_output = execute_rust(&rust_code).unwrap_or_else(|e| {
            panic!("Rust failed:\n{e}\n\nFlat:\n{flat}\nIR:\n{ir}\nCode:\n{rust_code}")
        });
        assert_eq!(
            expected, rust_output,
            "evaluator and Rust disagree\n\nFlat:\n{flat}\nIR:\n{ir}\nCode:\n{rust_code}"
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
    );
}

#[test]
#[ignore]
fn rest_named_after_a_parameter_it_forwards() {
    check(
        indoc! {r#"
            -- main.hop --
            fn Card(title: String, ...rest) -> Html {
              <div ...rest>
                {title}
              </div>
            }

            fn Panel(...title) -> Html {
              <Card ...title/>
            }

            page Test() {
              fn body() -> Html {
                <Panel title="Home" id="p"/>
              }
            }
        "#},
        r#"<div id="p">Home</div>"#,
    );
}

#[test]
#[ignore]
fn rest_named_after_the_only_parameter_it_forwards() {
    check(
        indoc! {r#"
            -- main.hop --
            fn Card(title: String) -> Html {
              <div>
                {title}
              </div>
            }

            fn Panel(...title) -> Html {
              <Card ...title/>
            }

            page Test() {
              fn body() -> Html {
                <Panel title="Home"/>
              }
            }
        "#},
        r#"<div>Home</div>"#,
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
    );
}

#[test]
#[ignore]
fn array_get_in_bounds() {
    check(
        indoc! {r#"
            -- main.hop --
            page Test() {
              fn body() -> Html {
                let items: Array[String] = ["a", "b", "c"];
                <>{items.get(1).unwrap_or("none")}</>
              }
            }
        "#},
        "b",
    );
}

#[test]
#[ignore]
fn array_get_out_of_bounds() {
    check(
        indoc! {r#"
            -- main.hop --
            page Test() {
              fn body() -> Html {
                let items: Array[String] = ["a", "b"];
                <>
                  {for i in -1..=2 {
                    match items.get(i) {
                      Some(item) => <>{item}</>,
                      None => <>-</>,
                    }
                  }}
                </>
              }
            }
        "#},
        "-ab-",
    );
}

#[test]
#[ignore]
fn array_enumerate_in_for_loop() {
    check(
        indoc! {r#"
            -- main.hop --
            page Test() {
              fn body() -> Html {
                let items: Array[String] = ["a", "b", "c"];
                <>
                  {for pair in items.enumerate() {
                    match pair {
                      (i, item) => <>{i.to_string()}={item};</>,
                    }
                  }}
                </>
              }
            }
        "#},
        "0=a;1=b;2=c;",
    );
}

#[test]
#[ignore]
fn array_enumerate_empty() {
    check(
        indoc! {r#"
            -- main.hop --
            page Test() {
              fn body() -> Html {
                let items: Array[String] = [];
                <>{items.enumerate().len().to_string()}</>
              }
            }
        "#},
        "0",
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
    );
}

#[test]
#[ignore]
fn html_bound_before_a_match_renders_in_the_arm_that_reads_it() {
    check(
        indoc! {r#"
            -- main.hop --
            page Test() {
              fn body() -> Html {
                for flag in [true, false] {
                  let mark = <b>yes</b>;
                  match flag {
                    true => mark,
                    false => <i>no</i>,
                  }
                }
              }
            }
        "#},
        "<b>yes</b><i>no</i>",
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
    );
}
#[test]
#[ignore]
fn lambda_renders_each_row_with_a_captured_value() {
    check(
        indoc! {r#"
            -- main.hop --
            fn Rows(labels: Array[String], row: fn(String, Html) -> Html) -> Html {
              <ul>
                {for label in labels {
                  row(label, <i>{label}</i>)
                }}
              </ul>
            }

            page Test() {
              fn body() -> Html {
                let suffix = "!";
                <Rows labels={["a", "b"]} row={fn(label, icon) {
                  <li>{icon}{label}{suffix}</li>
                }}/>
              }
            }
        "#},
        "<ul><li><i>a</i>a!</li><li><i>b</i>b!</li></ul>",
    );
}

#[test]
#[ignore]
fn lambda_passed_through_a_recursive_function() {
    check(
        indoc! {r#"
            -- main.hop --
            fn sum(n: Int, weight: fn(Int) -> Int) -> Int {
              match n <= 0 {
                true => 0,
                false => weight(n) + sum(n - 1, weight),
              }
            }

            fn Countdown(n: Int, item: fn(Int) -> Html) -> Html {
              match n <= 0 {
                true => <></>,
                false => <>{item(n)}<Countdown n={n - 1} item={item}/></>,
              }
            }

            page Test() {
              fn body() -> Html {
                let scale = 10;
                <>
                  {sum(3, fn(n) { n * scale }).to_string()}
                  <Countdown n={3} item={fn(n) { <b>{n.to_string()}</b> }}/>
                </>
              }
            }
        "#},
        "60<b>3</b><b>2</b><b>1</b>",
    );
}

#[test]
#[ignore]
fn lambda_through_a_rest_specialization() {
    check(
        indoc! {r#"
            -- main.hop --
            fn Item(label: String, mark: fn(String) -> Html, ...rest) -> Html {
              <li ...rest>{mark(label)}</li>
            }

            fn Bullet(...rest) -> Html {
              <Item ...rest/>
            }

            page Test() {
              fn body() -> Html {
                <ul>
                  <Bullet label="x" class="a" mark={fn(text) { <em>{text}</em> }}/>
                  <Bullet label="y" mark={fn(text) { <>{text}</> }}/>
                </ul>
              }
            }
        "#},
        r#"<ul><li class="a"><em>x</em></li><li>y</li></ul>"#,
    );
}

#[test]
#[ignore]
fn lambda_bound_by_let_and_applied_in_a_loop() {
    check(
        indoc! {r#"
            -- main.hop --
            page Test() {
              fn body() -> Html {
                let wrap = fn(content: Html, n: Int) -> Html {
                  <p class={n.to_string()}>{content}</p>
                };
                <div>
                  {for n in 1..=2 {
                    wrap(<b>{n.to_string()}</b>, n * 2)
                  }}
                </div>
              }
            }
        "#},
        r#"<div><p class="2"><b>1</b></p><p class="4"><b>2</b></p></div>"#,
    );
}
