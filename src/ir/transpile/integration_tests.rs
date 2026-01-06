use super::{GoTranspiler, PythonTranspiler, Transpiler, TsTranspiler};
use crate::ir::ast::{IrComponentDeclaration, IrEnumDeclaration, IrModule, IrRecordDeclaration};
use crate::ir::syntax::builder::{build_ir, build_ir_with_enums};
use expect_test::Expect;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug)]
struct TestCase {
    entrypoint: IrComponentDeclaration,
    expected_output: &'static str,
    enums: Vec<IrEnumDeclaration>,
    records: Vec<IrRecordDeclaration>,
}

impl TestCase {
    fn new(entrypoint: IrComponentDeclaration, expected_output: &'static str) -> Self {
        Self {
            entrypoint,
            expected_output,
            enums: vec![],
            records: vec![],
        }
    }

    fn with_enums(mut self, enums: Vec<IrEnumDeclaration>) -> Self {
        self.enums = enums;
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

    let ts_transpiler = TsTranspiler::new();
    let ts_code = ts_transpiler.transpile_module(&module);

    let go_transpiler = GoTranspiler::new("components".to_string());
    let go_code = go_transpiler.transpile_module(&module);

    let python_transpiler = PythonTranspiler::new();
    let python_code = python_transpiler.transpile_module(&module);

    // Run typecheckers first, then execute
    typecheck_typescript(&ts_code).expect("TypeScript typecheck failed");
    let ts_output = execute_typescript(&ts_code).expect("TypeScript execution failed");
    assert_eq!(
        ts_output, test_case.expected_output,
        "TypeScript output mismatch"
    );

    typecheck_go(&go_code).expect("Go typecheck failed");
    let go_output = execute_go(&go_code).expect("Go execution failed");
    assert_eq!(go_output, test_case.expected_output, "Go output mismatch");

    typecheck_python(&python_code).expect("Python typecheck failed");
    let python_output = execute_python(&python_code).expect("Python execution failed");
    assert_eq!(
        python_output, test_case.expected_output,
        "Python output mismatch"
    );

    let input = test_case.entrypoint.to_string();

    format!(
        "-- input --\n{}-- expected output --\n{}\n-- ts --\nOK\n-- go --\nOK\n-- python --\nOK\n",
        input, test_case.expected_output
    )
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
}
