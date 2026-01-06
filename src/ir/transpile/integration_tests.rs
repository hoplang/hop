use super::{GoTranspiler, PythonTranspiler, Transpiler, TsTranspiler};
use crate::ir::ast::{IrComponentDeclaration, IrEnumDeclaration, IrModule, IrRecordDeclaration};
use crate::ir::syntax::builder::{build_ir, build_ir_with_enums};
use expect_test::Expect;
use std::collections::HashSet;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Language {
    TypeScript,
    Go,
    Python,
}

#[derive(Debug)]
struct TestCase {
    entrypoint: IrComponentDeclaration,
    expected_output: &'static str,
    enums: Vec<IrEnumDeclaration>,
    records: Vec<IrRecordDeclaration>,
    languages: HashSet<Language>,
}

impl TestCase {
    fn new(entrypoint: IrComponentDeclaration, expected_output: &'static str) -> Self {
        Self {
            entrypoint,
            expected_output,
            enums: vec![],
            records: vec![],
            languages: HashSet::from([Language::TypeScript, Language::Go, Language::Python]),
        }
    }

    fn with_enums(mut self, enums: Vec<IrEnumDeclaration>) -> Self {
        self.enums = enums;
        self
    }

    fn with_records(mut self, records: Vec<(&str, Vec<(&str, crate::dop::Type)>)>) -> Self {
        use crate::dop::symbols::field_name::FieldName;
        self.records = records
            .into_iter()
            .map(|(name, fields)| IrRecordDeclaration {
                name: name.to_string(),
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (FieldName::new(k).unwrap(), v))
                    .collect(),
            })
            .collect();
        self
    }
}

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

fn run_integration_test(test_case: &TestCase) -> String {
    let module = IrModule {
        components: vec![test_case.entrypoint.clone()],
        records: test_case.records.clone(),
        enums: test_case.enums.clone(),
    };

    let input = test_case.entrypoint.to_string();
    let mut output = format!(
        "-- input --\n{}-- expected output --\n{}\n",
        input, test_case.expected_output
    );

    if test_case.languages.contains(&Language::TypeScript) {
        let ts_transpiler = TsTranspiler::new();
        let ts_code = ts_transpiler.transpile_module(&module);
        typecheck_typescript(&ts_code).expect("TypeScript typecheck failed");
        let ts_output = execute_typescript(&ts_code).expect("TypeScript execution failed");
        assert_eq!(
            ts_output, test_case.expected_output,
            "TypeScript output mismatch"
        );
        output.push_str("-- ts --\nOK\n");
    }

    if test_case.languages.contains(&Language::Go) {
        let go_transpiler = GoTranspiler::new("components".to_string());
        let go_code = go_transpiler.transpile_module(&module);
        typecheck_go(&go_code).expect("Go typecheck failed");
        let go_output = execute_go(&go_code).expect("Go execution failed");
        assert_eq!(go_output, test_case.expected_output, "Go output mismatch");
        output.push_str("-- go --\nOK\n");
    }

    if test_case.languages.contains(&Language::Python) {
        let python_transpiler = PythonTranspiler::new();
        let python_code = python_transpiler.transpile_module(&module);
        typecheck_python(&python_code).expect("Python typecheck failed");
        let python_output = execute_python(&python_code).expect("Python execution failed");
        assert_eq!(
            python_output, test_case.expected_output,
            "Python output mismatch"
        );
        output.push_str("-- python --\nOK\n");
    }

    output
}

fn check(test_case: TestCase, expected: Expect) {
    let output = run_integration_test(&test_case);
    expected.assert_eq(&output);
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

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    #[ignore]
    fn simple_html() {
        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.write("<h1>Hello, World!</h1>");
                }),
                "<h1>Hello, World!</h1>",
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("name", t.str("Alice"), |t| {
                        t.write("Hello, ");
                        t.write_expr(t.var("name"), false);
                        t.write("!");
                    });
                }),
                "Hello, Alice!",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let name = "Alice" in {
                    write("Hello, ")
                    write_expr(name)
                    write("!")
                  }
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
            TestCase::new(
                build_ir("Test", [], |t| {
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
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("show", t.bool(true), |t| {
                        t.if_else_stmt(
                            t.var("show"),
                            |t| {
                                t.write("True branch");
                            },
                            |t| {
                                t.write("False branch");
                            },
                        );
                    });
                    t.let_stmt("hide", t.bool(false), |t| {
                        t.if_else_stmt(
                            t.var("hide"),
                            |t| {
                                t.write("Should not appear");
                            },
                            |t| {
                                t.write("False branch");
                            },
                        );
                    });
                }),
                "True branchFalse branch",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let show = true in {
                    if show {
                      write("True branch")
                    } else {
                      write("False branch")
                    }
                  }
                  let hide = false in {
                    if hide {
                      write("Should not appear")
                    } else {
                      write("False branch")
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
            TestCase::new(
                build_ir("Test", [], |t| {
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
            ),
            expect![[r#"
                -- input --
                Test() {
                  for item in ["a", "b", "c"] {
                    write_expr(item)
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
    fn html_escaping() {
        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("text", t.str("<div>Hello & world</div>"), |t| {
                        t.write_expr_escaped(t.var("text"));
                    });
                }),
                "&lt;div&gt;Hello &amp; world&lt;/div&gt;",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let text = "<div>Hello & world</div>" in {
                    write_escaped(text)
                  }
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("message", t.str("Hello from let"), |t| {
                        t.write_expr(t.var("message"), false);
                    });
                }),
                "Hello from let",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let message = "Hello from let" in {
                    write_expr(message)
                  }
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("first", t.str("Hello"), |t| {
                        t.let_stmt("second", t.str(" World"), |t| {
                            t.write_expr(t.string_concat(t.var("first"), t.var("second")), false);
                        });
                    });
                }),
                "Hello World",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let first = "Hello" in {
                    let second = " World" in {
                      write_expr((first + second))
                    }
                  }
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
    fn json_encode() {
        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.write_expr(
                        t.json_encode(t.array(vec![t.str("Hello"), t.str("World")])),
                        false,
                    );
                }),
                r#"["Hello","World"]"#,
            ),
            expect![[r#"
                -- input --
                Test() {
                  write_expr(JsonEncode(["Hello", "World"]))
                }
                -- expected output --
                ["Hello","World"]
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.for_loop("item", t.array(vec![t.str("A"), t.str("B")]), |t| {
                        t.let_stmt("prefix", t.str("["), |t| {
                            t.write_expr(t.var("prefix"), false);
                            t.write_expr(t.var("item"), false);
                            t.write("]");
                        });
                    });
                }),
                "[A][B]",
            ),
            expect![[r#"
                -- input --
                Test() {
                  for item in ["A", "B"] {
                    let prefix = "[" in {
                      write_expr(prefix)
                      write_expr(item)
                      write("]")
                    }
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.if_stmt(
                        t.eq(t.string_concat(t.str("foo"), t.str("bar")), t.str("foobar")),
                        |t| {
                            t.write("equals");
                        },
                    );
                }),
                "equals",
            ),
            expect![[r#"
                -- input --
                Test() {
                  if (("foo" + "bar") == "foobar") {
                    write("equals")
                  }
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.if_stmt(t.less_than(t.int(3), t.int(5)), |t| {
                        t.write("3 < 5");
                    });
                    t.if_stmt(t.less_than(t.int(10), t.int(2)), |t| {
                        t.write("10 < 2");
                    });
                }),
                "3 < 5",
            ),
            expect![[r#"
                -- input --
                Test() {
                  if (3 < 5) {
                    write("3 < 5")
                  }
                  if (10 < 2) {
                    write("10 < 2")
                  }
                }
                -- expected output --
                3 < 5
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.if_stmt(t.less_than(t.float(1.5), t.float(2.5)), |t| {
                        t.write("1.5 < 2.5");
                    });
                    t.if_stmt(t.less_than(t.float(3.0), t.float(1.0)), |t| {
                        t.write("3.0 < 1.0");
                    });
                }),
                "1.5 < 2.5",
            ),
            expect![[r#"
                -- input --
                Test() {
                  if (1.5 < 2.5) {
                    write("1.5 < 2.5")
                  }
                  if (3 < 1) {
                    write("3.0 < 1.0")
                  }
                }
                -- expected output --
                1.5 < 2.5
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
        use crate::dop::symbols::type_name::TypeName;

        let enums = vec![("Color", vec!["Red", "Green", "Blue"])];

        let enum_declarations = vec![IrEnumDeclaration {
            name: "Color".to_string(),
            variants: vec![
                TypeName::new("Red").unwrap(),
                TypeName::new("Green").unwrap(),
                TypeName::new("Blue").unwrap(),
            ],
        }];

        check(
            TestCase::new(
                build_ir_with_enums("Test", [], enums.clone(), |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                        t.if_else_stmt(
                            t.eq(t.var("color"), t.enum_variant("Color", "Red")),
                            |t| {
                                t.write("equal");
                            },
                            |t| {
                                t.write("not equal");
                            },
                        );
                    });
                }),
                "equal",
            )
            .with_enums(enum_declarations),
            expect![[r#"
                -- input --
                Test() {
                  let color = Color::Red in {
                    if (color == Color::Red) {
                      write("equal")
                    } else {
                      write("not equal")
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
        use crate::dop::symbols::type_name::TypeName;

        let enums = vec![("Color", vec!["Red", "Green", "Blue"])];

        let enum_declarations = vec![IrEnumDeclaration {
            name: "Color".to_string(),
            variants: vec![
                TypeName::new("Red").unwrap(),
                TypeName::new("Green").unwrap(),
                TypeName::new("Blue").unwrap(),
            ],
        }];

        check(
            TestCase::new(
                build_ir_with_enums("Test", [], enums.clone(), |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Red"), |t| {
                        t.if_else_stmt(
                            t.eq(t.var("color"), t.enum_variant("Color", "Green")),
                            |t| {
                                t.write("equal");
                            },
                            |t| {
                                t.write("not equal");
                            },
                        );
                    });
                }),
                "not equal",
            )
            .with_enums(enum_declarations),
            expect![[r#"
                -- input --
                Test() {
                  let color = Color::Red in {
                    if (color == Color::Green) {
                      write("equal")
                    } else {
                      write("not equal")
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("flag", t.bool(true), |t| {
                        t.bool_match_stmt(
                            t.var("flag"),
                            |t| {
                                t.write("yes");
                            },
                            |t| {
                                t.write("no");
                            },
                        );
                    });
                }),
                "yes",
            ),
            expect![[r#"
                -- input --
                Test() {
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("flag", t.bool(false), |t| {
                        t.bool_match_stmt(
                            t.var("flag"),
                            |t| {
                                t.write("yes");
                            },
                            |t| {
                                t.write("no");
                            },
                        );
                    });
                }),
                "no",
            ),
            expect![[r#"
                -- input --
                Test() {
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
        use crate::dop::Type;
        use crate::ir::syntax::builder::build_ir_with_records;

        let records = vec![("Person", vec![("name", Type::String), ("age", Type::Int)])];

        check(
            TestCase::new(
                build_ir_with_records("Test", [], records.clone(), |t| {
                    t.let_stmt(
                        "person",
                        t.record("Person", vec![("name", t.str("Alice")), ("age", t.int(30))]),
                        |t| {
                            let person = t.var("person");
                            let name = t.field_access(person, "name");
                            t.write_expr(name, false);
                            let person = t.var("person");
                            let age = t.field_access(person, "age");
                            t.if_stmt(t.eq(age, t.int(30)), |t| {
                                t.write(":30");
                            });
                        },
                    );
                }),
                "Alice:30",
            )
            .with_records(records),
            expect![[r#"
                -- input --
                Test() {
                  let person = Person(name: "Alice", age: 30) in {
                    write_expr(person.name)
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
        use crate::dop::Type;
        use crate::ir::syntax::builder::build_ir_with_records;

        let records = vec![(
            "Item",
            vec![
                ("label", Type::String),
                ("count", Type::Int),
                ("active", Type::Bool),
            ],
        )];

        check(
            TestCase::new(
                build_ir_with_records("Test", [], records.clone(), |t| {
                    t.let_stmt(
                        "item",
                        t.record(
                            "Item",
                            vec![
                                ("label", t.str("widget")),
                                ("count", t.int(5)),
                                ("active", t.bool(true)),
                            ],
                        ),
                        |t| {
                            let item = t.var("item");
                            let label = t.field_access(item, "label");
                            t.write_expr(label, false);
                            let item = t.var("item");
                            let count = t.field_access(item, "count");
                            t.if_stmt(t.eq(count, t.int(5)), |t| {
                                t.write(",5");
                            });
                            t.write(",");
                            let item = t.var("item");
                            let active = t.field_access(item, "active");
                            t.if_stmt(active, |t| {
                                t.write("active");
                            });
                        },
                    );
                }),
                "widget,5,active",
            )
            .with_records(records),
            expect![[r#"
                -- input --
                Test() {
                  let item = Item(
                    label: "widget",
                    count: 5,
                    active: true,
                  ) in {
                    write_expr(item.label)
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
    fn numeric_add() {
        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("a", t.int(3), |t| {
                        t.let_stmt("b", t.int(7), |t| {
                            let sum = t.add(t.var("a"), t.var("b"));
                            t.if_stmt(t.eq(sum, t.int(10)), |t| {
                                t.write("correct");
                            });
                        });
                    });
                }),
                "correct",
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("a", t.int(10), |t| {
                        t.let_stmt("b", t.int(3), |t| {
                            let diff = t.subtract(t.var("a"), t.var("b"));
                            t.if_stmt(t.eq(diff, t.int(7)), |t| {
                                t.write("correct");
                            });
                        });
                    });
                }),
                "correct",
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("a", t.int(4), |t| {
                        t.let_stmt("b", t.int(5), |t| {
                            let product = t.multiply(t.var("a"), t.var("b"));
                            t.if_stmt(t.eq(product, t.int(20)), |t| {
                                t.write("correct");
                            });
                        });
                    });
                }),
                "correct",
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("a", t.bool(true), |t| {
                        t.let_stmt("b", t.bool(true), |t| {
                            t.if_stmt(t.and(t.var("a"), t.var("b")), |t| {
                                t.write("TT");
                            });
                        });
                    });
                    t.let_stmt("c", t.bool(true), |t| {
                        t.let_stmt("d", t.bool(false), |t| {
                            t.if_stmt(t.and(t.var("c"), t.var("d")), |t| {
                                t.write("TF");
                            });
                        });
                    });
                }),
                "TT",
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("a", t.bool(false), |t| {
                        t.let_stmt("b", t.bool(true), |t| {
                            t.if_stmt(t.or(t.var("a"), t.var("b")), |t| {
                                t.write("FT");
                            });
                        });
                    });
                    t.let_stmt("c", t.bool(false), |t| {
                        t.let_stmt("d", t.bool(false), |t| {
                            t.if_stmt(t.or(t.var("c"), t.var("d")), |t| {
                                t.write("FF");
                            });
                        });
                    });
                }),
                "FT",
            ),
            expect![[r#"
                -- input --
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
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.if_stmt(t.less_than_or_equal(t.int(3), t.int(5)), |t| {
                        t.write("A");
                    });
                    t.if_stmt(t.less_than_or_equal(t.int(5), t.int(5)), |t| {
                        t.write("B");
                    });
                    t.if_stmt(t.less_than_or_equal(t.int(7), t.int(5)), |t| {
                        t.write("C");
                    });
                }),
                "AB",
            ),
            expect![[r#"
                -- input --
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
    fn let_expression() {
        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    let result = t.let_expr("x", t.str("Hello"), |t| {
                        t.string_concat(t.var("x"), t.str(" World"))
                    });
                    t.write_expr(result, false);
                }),
                "Hello World",
            ),
            expect![[r#"
                -- input --
                Test() {
                  write_expr(let x = "Hello" in (x + " World"))
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
    fn bool_match_expr() {
        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("flag", t.bool(true), |t| {
                        let result =
                            t.bool_match_expr(t.var("flag"), t.str("yes"), t.str("no"));
                        t.write_expr(result, false);
                    });
                    t.let_stmt("other", t.bool(false), |t| {
                        let result =
                            t.bool_match_expr(t.var("other"), t.str("YES"), t.str("NO"));
                        t.write_expr(result, false);
                    });
                }),
                "yesNO",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let flag = true in {
                    write_expr(match flag {true => "yes", false => "no"})
                  }
                  let other = false in {
                    write_expr(match other {true => "YES", false => "NO"})
                  }
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
    fn option_literal() {
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    t.let_stmt("some_val", t.some(t.str("hello")), |t| {
                        t.option_match_stmt(
                            t.var("some_val"),
                            Some("val"),
                            |t| {
                                t.write("Some:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write("None");
                            },
                        );
                    });
                    t.let_stmt("none_val", t.none(Type::String), |t| {
                        t.option_match_stmt(
                            t.var("none_val"),
                            Some("val"),
                            |t| {
                                t.write("Some:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write(",None");
                            },
                        );
                    });
                }),
                "Some:hello,None",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let some_val = Some("hello") in {
                    match some_val {
                      Some(val) => {
                        write("Some:")
                        write_expr(val)
                      }
                      None => {
                        write("None")
                      }
                    }
                  }
                  let none_val = None in {
                    match none_val {
                      Some(val) => {
                        write("Some:")
                        write_expr(val)
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
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    // Match on Some literal via let binding
                    t.let_stmt("opt1", t.some(t.str("world")), |t| {
                        t.option_match_stmt(
                            t.var("opt1"),
                            Some("val"),
                            |t| {
                                t.write("Got:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write("Empty");
                            },
                        );
                    });
                    t.write(",");
                    // Match on None literal via let binding
                    t.let_stmt("opt2", t.none(Type::String), |t| {
                        t.option_match_stmt(
                            t.var("opt2"),
                            Some("val"),
                            |t| {
                                t.write("Got:");
                                t.write_expr(t.var("val"), false);
                            },
                            |t| {
                                t.write("Empty");
                            },
                        );
                    });
                }),
                "Got:world,Empty",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let opt1 = Some("world") in {
                    match opt1 {
                      Some(val) => {
                        write("Got:")
                        write_expr(val)
                      }
                      None => {
                        write("Empty")
                      }
                    }
                  }
                  write(",")
                  let opt2 = None in {
                    match opt2 {
                      Some(val) => {
                        write("Got:")
                        write_expr(val)
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
    fn option_literal_inline_match_expr() {
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    // Match expression on Some variable
                    t.let_stmt("opt1", t.some(t.str("hi")), |t| {
                        let result =
                            t.option_match_expr(t.var("opt1"), t.str("some"), t.str("none"));
                        t.write_expr(result, false);
                    });
                    t.write(",");
                    // Match expression on None variable
                    t.let_stmt("opt2", t.none(Type::String), |t| {
                        let result2 =
                            t.option_match_expr(t.var("opt2"), t.str("SOME"), t.str("NONE"));
                        t.write_expr(result2, false);
                    });
                }),
                "some,NONE",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let opt1 = Some("hi") in {
                    write_expr(match opt1 {
                      Some(_) => "some",
                      None => "none",
                    })
                  }
                  write(",")
                  let opt2 = None in {
                    write_expr(match opt2 {
                      Some(_) => "SOME",
                      None => "NONE",
                    })
                  }
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
    fn option_match_returning_options() {
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    // match inner { Some(x) => Some(x), None => None }
                    // Then match on the result
                    t.let_stmt("inner", t.some(t.str("hello")), |t| {
                        t.let_stmt(
                            "mapped",
                            t.option_match_expr_with_binding(
                                t.var("inner"),
                                "x",
                                Type::String,
                                |t| t.some(t.var("x")), // Return Some(x)
                                t.none(Type::String),   // Return None
                            ),
                            |t| {
                                t.option_match_stmt(
                                    t.var("mapped"),
                                    Some("result"),
                                    |t| {
                                        t.write("mapped:");
                                        t.write_expr(t.var("result"), false);
                                    },
                                    |t| {
                                        t.write("was-none");
                                    },
                                );
                            },
                        );
                    });
                }),
                "mapped:hello",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let inner = Some("hello") in {
                    let mapped = match inner {
                      Some(x) => Some(x),
                      None => None,
                    } in {
                      match mapped {
                        Some(result) => {
                          write("mapped:")
                          write_expr(result)
                        }
                        None => {
                          write("was-none")
                        }
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
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    // Some(match innerOpt { Some(x) => x, None => "default" })
                    t.let_stmt("inner_opt", t.some(t.str("inner")), |t| {
                        let match_result = t.option_match_expr_with_binding(
                            t.var("inner_opt"),
                            "x",
                            Type::String,
                            |t| t.var("x"),
                            t.str("default"),
                        );
                        t.let_stmt("outer", t.some(match_result), |t| {
                            t.option_match_stmt(
                                t.var("outer"),
                                Some("val"),
                                |t| {
                                    t.write_expr(t.var("val"), false);
                                },
                                |t| {
                                    t.write("none");
                                },
                            );
                        });
                    });
                }),
                "inner",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let inner_opt = Some("inner") in {
                    let outer = Some(match inner_opt {
                      Some(x) => x,
                      None => "default",
                    }) in {
                      match outer {
                        Some(val) => {
                          write_expr(val)
                        }
                        None => {
                          write("none")
                        }
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
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    let items = t.array(vec![
                        t.some(t.str("a")),
                        t.none(Type::String),
                        t.some(t.str("b")),
                    ]);
                    t.for_loop("item", items, |t| {
                        t.option_match_stmt(
                            t.var("item"),
                            Some("val"),
                            |t| {
                                t.write("[");
                                t.write_expr(t.var("val"), false);
                                t.write("]");
                            },
                            |t| {
                                t.write("[_]");
                            },
                        );
                    });
                }),
                "[a][_][b]",
            ),
            expect![[r#"
                -- input --
                Test() {
                  for item in [Some("a"), None, Some("b")] {
                    match item {
                      Some(val) => {
                        write("[")
                        write_expr(val)
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
    fn nested_option_literal_match() {
        use crate::dop::Type;

        check(
            TestCase::new(
                build_ir("Test", [], |t| {
                    // Some(Some("deep"))
                    let nested_some = t.some(t.some(t.str("deep")));
                    t.let_stmt("nested1", nested_some, |t| {
                        let result = t.option_match_expr_with_binding(
                            t.var("nested1"),
                            "outer",
                            Type::Option(Box::new(Type::String)),
                            |t| {
                                t.option_match_expr_with_binding(
                                    t.var("outer"),
                                    "inner",
                                    Type::String,
                                    |t| t.var("inner"),
                                    t.str("inner-none"),
                                )
                            },
                            t.str("outer-none"),
                        );
                        t.write_expr(result, false);
                        t.write(",");

                        // Some(None)
                        let some_none = t.some(t.none(Type::String));
                        t.let_stmt("nested2", some_none, |t| {
                            let result2 = t.option_match_expr_with_binding(
                                t.var("nested2"),
                                "outer",
                                Type::Option(Box::new(Type::String)),
                                |t| {
                                    t.option_match_expr_with_binding(
                                        t.var("outer"),
                                        "inner",
                                        Type::String,
                                        |t| t.var("inner"),
                                        t.str("inner-none"),
                                    )
                                },
                                t.str("outer-none"),
                            );
                            t.write_expr(result2, false);
                            t.write(",");

                            // None (outer)
                            let outer_none = t.none(Type::Option(Box::new(Type::String)));
                            t.let_stmt("nested3", outer_none, |t| {
                                let result3 = t.option_match_expr_with_binding(
                                    t.var("nested3"),
                                    "outer",
                                    Type::Option(Box::new(Type::String)),
                                    |t| {
                                        t.option_match_expr_with_binding(
                                            t.var("outer"),
                                            "inner",
                                            Type::String,
                                            |t| t.var("inner"),
                                            t.str("inner-none"),
                                        )
                                    },
                                    t.str("outer-none"),
                                );
                                t.write_expr(result3, false);
                            });
                        });
                    });
                }),
                "deep,inner-none,outer-none",
            ),
            expect![[r#"
                -- input --
                Test() {
                  let nested1 = Some(Some("deep")) in {
                    write_expr(match nested1 {
                      Some(outer) => match outer {
                        Some(inner) => inner,
                        None => "inner-none",
                      },
                      None => "outer-none",
                    })
                    write(",")
                    let nested2 = Some(None) in {
                      write_expr(match nested2 {
                        Some(outer) => match outer {
                          Some(inner) => inner,
                          None => "inner-none",
                        },
                        None => "outer-none",
                      })
                      write(",")
                      let nested3 = None in {
                        write_expr(match nested3 {
                          Some(outer) => match outer {
                            Some(inner) => inner,
                            None => "inner-none",
                          },
                          None => "outer-none",
                        })
                      }
                    }
                  }
                }
                -- expected output --
                deep,inner-none,outer-none
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
