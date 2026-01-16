use super::{GoTranspiler, PythonTranspiler, Transpiler, TsTranspiler};
use crate::ir::ast::IrModule;
use crate::ir::syntax::builder::{IrBuilder, IrModuleBuilder};
use expect_test::Expect;
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

fn check(module: IrModule, expected_output: &str, expected: Expect) {
    let input = module.to_string();

    let mut output = format!(
        "-- input --\n{}-- expected output --\n{}\n",
        input, expected_output
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

    #[test]
    #[ignore]
    fn simple_html() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.write("<h1>Hello, World!</h1>");
                })
                .build(),
            "<h1>Hello, World!</h1>",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("name", t.str("Alice"), |t| {
                        t.write("Hello, ");
                        t.write_expr(t.var("name"), false);
                        t.write("!");
                    });
                })
                .build(),
            "Hello, Alice!",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("show", t.bool(true), |t| {
                        t.if_stmt(t.var("show"), |t| {
                            t.write("Visible");
                        });
                        t.if_stmt(t.not(t.var("show")), |t| {
                            t.write("Hidden");
                        });
                    });
                })
                .build(),
            "Visible",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "True branchFalse branch",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_loop(
                        "item",
                        t.array(vec![t.str("a"), t.str("b"), t.str("c")]),
                        |t| {
                            t.write_expr(t.var("item"), false);
                            t.write(",");
                        },
                    );
                })
                .build(),
            "a,b,c,",
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
    fn for_loop_with_range() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_range("i", t.int(1), t.int(3), |t| {
                        t.write_expr(t.var("i"), false);
                        t.write(",");
                    });
                })
                .build(),
            "1,2,3,",
            expect![[r#"
                -- input --
                Test() {
                  for i in 1..=3 {
                    write_expr(i)
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_range("x", t.int(0), t.int(5), |t| {
                        t.write_expr(t.var("x"), false);
                    });
                })
                .build(),
            "012345",
            expect![[r#"
                -- input --
                Test() {
                  for x in 0..=5 {
                    write_expr(x)
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_range("i", t.int(1), t.int(2), |t| {
                        t.for_range("j", t.int(1), t.int(2), |t| {
                            t.write("(");
                            t.write_expr(t.var("i"), false);
                            t.write(",");
                            t.write_expr(t.var("j"), false);
                            t.write(")");
                        });
                    });
                })
                .build(),
            "(1,1)(1,2)(2,1)(2,2)",
            expect![[r#"
                -- input --
                Test() {
                  for i in 1..=2 {
                    for j in 1..=2 {
                      write("(")
                      write_expr(i)
                      write(",")
                      write_expr(j)
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("text", t.str("<div>Hello & world</div>"), |t| {
                        t.write_expr_escaped(t.var("text"));
                    });
                })
                .build(),
            "&lt;div&gt;Hello &amp; world&lt;/div&gt;",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("message", t.str("Hello from let"), |t| {
                        t.write_expr(t.var("message"), false);
                    });
                })
                .build(),
            "Hello from let",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("first", t.str("Hello"), |t| {
                        t.let_stmt("second", t.str(" World"), |t| {
                            t.write_expr(t.string_concat(t.var("first"), t.var("second")), false);
                        });
                    });
                })
                .build(),
            "Hello World",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.write_expr(
                        t.json_encode(t.array(vec![t.str("Hello"), t.str("World")])),
                        false,
                    );
                })
                .build(),
            r#"["Hello","World"]"#,
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_loop("item", t.array(vec![t.str("A"), t.str("B")]), |t| {
                        t.let_stmt("prefix", t.str("["), |t| {
                            t.write_expr(t.var("prefix"), false);
                            t.write_expr(t.var("item"), false);
                            t.write("]");
                        });
                    });
                })
                .build(),
            "[A][B]",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.if_stmt(
                        t.eq(t.string_concat(t.str("foo"), t.str("bar")), t.str("foobar")),
                        |t| {
                            t.write("equals");
                        },
                    );
                })
                .build(),
            "equals",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.if_stmt(t.less_than(t.int(3), t.int(5)), |t| {
                        t.write("3 < 5");
                    });
                    t.if_stmt(t.less_than(t.int(10), t.int(2)), |t| {
                        t.write("10 < 2");
                    });
                })
                .build(),
            "3 < 5",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.if_stmt(t.less_than(t.float(1.5), t.float(2.5)), |t| {
                        t.write("1.5 < 2.5");
                    });
                    t.if_stmt(t.less_than(t.float(3.0), t.float(1.0)), |t| {
                        t.write("3.0 < 1.0");
                    });
                })
                .build(),
            "1.5 < 2.5",
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
        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("Test", [], |t| {
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
                })
                .build(),
            "equal",
            expect![[r#"
                -- input --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
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
        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("Test", [], |t| {
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
                })
                .build(),
            "not equal",
            expect![[r#"
                -- input --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "yes",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "no",
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

        check(
            IrModuleBuilder::new()
                .record("Person", |r| {
                    r.field("name", Type::String);
                    r.field("age", Type::Int);
                })
                .component("Test", [], |t| {
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
                })
                .build(),
            "Alice:30",
            expect![[r#"
                -- input --
                record Person {
                  name: String,
                  age: Int,
                }
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

        check(
            IrModuleBuilder::new()
                .record("Item", |r| {
                    r.field("label", Type::String);
                    r.field("count", Type::Int);
                    r.field("active", Type::Bool);
                })
                .component("Test", [], |t| {
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
                })
                .build(),
            "widget,5,active",
            expect![[r#"
                -- input --
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
    fn nested_record() {
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .record("Address", |r| {
                    r.field("city", Type::String);
                    r.field("zip", Type::String);
                })
                .record("Person", |r| {
                    r.field("name", Type::String);
                    r.field("address", r.record_type("Address"));
                })
                .component("Test", [], |t| {
                    t.let_stmt(
                        "person",
                        t.record(
                            "Person",
                            vec![
                                ("name", t.str("Alice")),
                                (
                                    "address",
                                    t.record(
                                        "Address",
                                        vec![("city", t.str("Paris")), ("zip", t.str("75001"))],
                                    ),
                                ),
                            ],
                        ),
                        |t| {
                            let person = t.var("person");
                            let name = t.field_access(person, "name");
                            t.write_expr(name, false);
                            t.write(",");
                            let person = t.var("person");
                            let address = t.field_access(person, "address");
                            let city = t.field_access(address, "city");
                            t.write_expr(city, false);
                        },
                    );
                })
                .build(),
            "Alice,Paris",
            expect![[r#"
                -- input --
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
                    write_expr(person.name)
                    write(",")
                    write_expr(person.address.city)
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
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .enum_decl("Status", ["Active", "Inactive", "Pending"])
                .record("User", |r| {
                    r.field("name", Type::String);
                    r.field("status", r.enum_type("Status"));
                })
                .component("Test", [], |t| {
                    t.let_stmt(
                        "user",
                        t.record(
                            "User",
                            vec![
                                ("name", t.str("Alice")),
                                ("status", t.enum_variant("Status", "Active")),
                            ],
                        ),
                        |t| {
                            t.write_expr(t.field_access(t.var("user"), "name"), false);
                            t.write(":");
                            let status = t.field_access(t.var("user"), "status");
                            t.if_stmt(t.eq(status, t.enum_variant("Status", "Active")), |t| {
                                t.write("active");
                            });
                        },
                    );
                })
                .build(),
            "Alice:active",
            expect![[r#"
                -- input --
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
                    write_expr(user.name)
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("a", t.int(3), |t| {
                        t.let_stmt("b", t.int(7), |t| {
                            let sum = t.add(t.var("a"), t.var("b"));
                            t.if_stmt(t.eq(sum, t.int(10)), |t| {
                                t.write("correct");
                            });
                        });
                    });
                })
                .build(),
            "correct",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("a", t.int(10), |t| {
                        t.let_stmt("b", t.int(3), |t| {
                            let diff = t.subtract(t.var("a"), t.var("b"));
                            t.if_stmt(t.eq(diff, t.int(7)), |t| {
                                t.write("correct");
                            });
                        });
                    });
                })
                .build(),
            "correct",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("a", t.int(4), |t| {
                        t.let_stmt("b", t.int(5), |t| {
                            let product = t.multiply(t.var("a"), t.var("b"));
                            t.if_stmt(t.eq(product, t.int(20)), |t| {
                                t.write("correct");
                            });
                        });
                    });
                })
                .build(),
            "correct",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "TT",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "FT",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.if_stmt(t.less_than_or_equal(t.int(3), t.int(5)), |t| {
                        t.write("A");
                    });
                    t.if_stmt(t.less_than_or_equal(t.int(5), t.int(5)), |t| {
                        t.write("B");
                    });
                    t.if_stmt(t.less_than_or_equal(t.int(7), t.int(5)), |t| {
                        t.write("C");
                    });
                })
                .build(),
            "AB",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    let result = t.let_expr("x", t.str("Hello"), |t| {
                        t.string_concat(t.var("x"), t.str(" World"))
                    });
                    t.write_expr(result, false);
                })
                .build(),
            "Hello World",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("flag", t.bool(true), |t| {
                        let result = t.bool_match_expr(t.var("flag"), t.str("yes"), t.str("no"));
                        t.write_expr(result, false);
                    });
                    t.let_stmt("other", t.bool(false), |t| {
                        let result = t.bool_match_expr(t.var("other"), t.str("YES"), t.str("NO"));
                        t.write_expr(result, false);
                    });
                })
                .build(),
            "yesNO",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "Some:hello,None",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "Got:world,Empty",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "some,NONE",
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
    fn option_match_stmt_without_binding() {
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    // Match statement on Some without extracting the value
                    t.let_stmt("opt1", t.some(t.str("hello")), |t| {
                        t.option_match_stmt(
                            t.var("opt1"),
                            None, // No binding
                            |t| {
                                t.write("is-some");
                            },
                            |t| {
                                t.write("is-none");
                            },
                        );
                    });
                    t.write(",");
                    // Match statement on None without extracting the value
                    t.let_stmt("opt2", t.none(Type::String), |t| {
                        t.option_match_stmt(
                            t.var("opt2"),
                            None, // No binding
                            |t| {
                                t.write("IS-SOME");
                            },
                            |t| {
                                t.write("IS-NONE");
                            },
                        );
                    });
                })
                .build(),
            "is-some,IS-NONE",
            expect![[r#"
                -- input --
                Test() {
                  let opt1 = Some("hello") in {
                    match opt1 {
                      Some(_) => {
                        write("is-some")
                      }
                      None => {
                        write("is-none")
                      }
                    }
                  }
                  write(",")
                  let opt2 = None in {
                    match opt2 {
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
    fn option_match_returning_options() {
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "mapped:hello",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "inner",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "[a][_][b]",
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
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
                })
                .build(),
            "deep,inner-none,outer-none",
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

    #[test]
    #[ignore]
    fn nested_bool_match_expr() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("outer", t.bool(true), |t| {
                        t.let_stmt("inner", t.bool(false), |t| {
                            let result = t.bool_match_expr(
                                t.var("outer"),
                                t.bool_match_expr(t.var("inner"), t.str("TT"), t.str("TF")),
                                t.str("F"),
                            );
                            t.write_expr(result, false);
                        });
                    });
                    t.write(",");
                    t.let_stmt("outer2", t.bool(false), |t| {
                        t.let_stmt("inner2", t.bool(true), |t| {
                            let result = t.bool_match_expr(
                                t.var("outer2"),
                                t.bool_match_expr(t.var("inner2"), t.str("TT"), t.str("TF")),
                                t.str("F"),
                            );
                            t.write_expr(result, false);
                        });
                    });
                })
                .build(),
            "TF,F",
            expect![[r#"
                -- input --
                Test() {
                  let outer = true in {
                    let inner = false in {
                      write_expr(match outer {
                        true => match inner {true => "TT", false => "TF"},
                        false => "F",
                      })
                    }
                  }
                  write(",")
                  let outer2 = false in {
                    let inner2 = true in {
                      write_expr(match outer2 {
                        true => match inner2 {true => "TT", false => "TF"},
                        false => "F",
                      })
                    }
                  }
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
    fn enum_match_expression() {
        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("Test", [], |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Green"), |t| {
                        let result = t.match_expr(
                            t.var("color"),
                            vec![
                                ("Red", t.str("red")),
                                ("Green", t.str("green")),
                                ("Blue", t.str("blue")),
                            ],
                        );
                        t.write_expr(result, false);
                    });
                })
                .build(),
            "green",
            expect![[r#"
                -- input --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  let color = Color::Green in {
                    write_expr(match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    })
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
    fn enum_equality_in_let_expr() {
        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("Test", [], |t| {
                    // write_expr(let color = Color::Green in let is_red = color == Color::Red in if is_red then "eq" else "not eq")
                    let color_expr = t.enum_variant("Color", "Green");
                    let result = t.let_expr("color", color_expr, |t| {
                        t.let_expr(
                            "is_red",
                            t.eq(t.var("color"), t.enum_variant("Color", "Red")),
                            |t| t.bool_match_expr(t.var("is_red"), t.str("eq"), t.str("not eq")),
                        )
                    });
                    t.write_expr(result, false);
                })
                .build(),
            "not eq",
            expect![[r#"
                -- input --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
                  write_expr(let color = Color::Green in let is_red = (color == Color::Red) in match is_red {
                    true => "eq",
                    false => "not eq",
                  })
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
    fn enum_match_statement() {
        check(
            IrModuleBuilder::new()
                .enum_decl("Color", ["Red", "Green", "Blue"])
                .component("Test", [], |t| {
                    t.let_stmt("color", t.enum_variant("Color", "Blue"), |t| {
                        t.enum_match_stmt(
                            t.var("color"),
                            vec![
                                ("Red", Box::new(|t: &mut IrBuilder| t.write("red"))),
                                ("Green", Box::new(|t: &mut IrBuilder| t.write("green"))),
                                ("Blue", Box::new(|t: &mut IrBuilder| t.write("blue"))),
                            ],
                        );
                    });
                })
                .build(),
            "blue",
            expect![[r#"
                -- input --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
                Test() {
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
        use crate::dop::Type;
        use crate::ir::syntax::builder::IrBuilder;

        check(
            IrModuleBuilder::new()
                .enum_with_fields("Result", |e| {
                    e.variant_with_fields("Ok", vec![("value", Type::String)]);
                    e.variant_with_fields("Err", vec![("message", Type::String)]);
                })
                .component("Test", [], |t| {
                    t.let_stmt(
                        "ok",
                        t.enum_variant_with_fields(
                            "Result",
                            "Ok",
                            vec![("value", t.str("success"))],
                        ),
                        |t| {
                            t.enum_match_stmt_with_bindings(
                                t.var("ok"),
                                vec![
                                    (
                                        "Ok",
                                        vec![("value", "v")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("created:");
                                            t.write_expr(t.var("v"), false);
                                        }),
                                    ),
                                    (
                                        "Err",
                                        vec![("message", "m")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("error:");
                                            t.write_expr(t.var("m"), false);
                                        }),
                                    ),
                                ],
                            );
                        },
                    );
                })
                .build(),
            "created:success",
            expect![[r#"
                -- input --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let ok = Result::Ok(value: "success") in {
                    match ok {
                      Result::Ok(value: v) => {
                        write("created:")
                        write_expr(v)
                      }
                      Result::Err(message: m) => {
                        write("error:")
                        write_expr(m)
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
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .enum_with_fields("Result", |e| {
                    e.variant_with_fields("Ok", vec![("value", Type::String)]);
                    e.variant_with_fields("Err", vec![("message", Type::String)]);
                })
                .component("Test", [], |t| {
                    t.let_stmt(
                        "result",
                        t.enum_variant_with_fields("Result", "Ok", vec![("value", t.str("hello"))]),
                        |t| {
                            t.enum_match_stmt_with_bindings(
                                t.var("result"),
                                vec![
                                    (
                                        "Ok",
                                        vec![("value", "v")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("Ok:");
                                            t.write_expr(t.var("v"), false);
                                        }),
                                    ),
                                    (
                                        "Err",
                                        vec![("message", "m")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("Err:");
                                            t.write_expr(t.var("m"), false);
                                        }),
                                    ),
                                ],
                            );
                        },
                    );
                })
                .build(),
            "Ok:hello",
            expect![[r#"
                -- input --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let result = Result::Ok(value: "hello") in {
                    match result {
                      Result::Ok(value: v) => {
                        write("Ok:")
                        write_expr(v)
                      }
                      Result::Err(message: m) => {
                        write("Err:")
                        write_expr(m)
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
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .enum_with_fields("Result", |e| {
                    e.variant_with_fields("Ok", vec![("value", Type::String)]);
                    e.variant_with_fields("Err", vec![("message", Type::String)]);
                })
                .component("Test", [], |t| {
                    t.let_stmt(
                        "result",
                        t.enum_variant_with_fields(
                            "Result",
                            "Err",
                            vec![("message", t.str("something went wrong"))],
                        ),
                        |t| {
                            t.enum_match_stmt_with_bindings(
                                t.var("result"),
                                vec![
                                    (
                                        "Ok",
                                        vec![("value", "v")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("Ok:");
                                            t.write_expr(t.var("v"), false);
                                        }),
                                    ),
                                    (
                                        "Err",
                                        vec![("message", "m")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("Err:");
                                            t.write_expr(t.var("m"), false);
                                        }),
                                    ),
                                ],
                            );
                        },
                    );
                })
                .build(),
            "Err:something went wrong",
            expect![[r#"
                -- input --
                enum Result {
                  Ok(value: String),
                  Err(message: String),
                }
                Test() {
                  let result = Result::Err(message: "something went wrong") in {
                    match result {
                      Result::Ok(value: v) => {
                        write("Ok:")
                        write_expr(v)
                      }
                      Result::Err(message: m) => {
                        write("Err:")
                        write_expr(m)
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
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .enum_with_fields("Response", |e| {
                    e.variant_with_fields(
                        "Success",
                        vec![("code", Type::String), ("body", Type::String)],
                    );
                    e.variant_with_fields("Error", vec![("reason", Type::String)]);
                })
                .component("Test", [], |t| {
                    t.let_stmt(
                        "resp",
                        t.enum_variant_with_fields(
                            "Response",
                            "Success",
                            vec![("code", t.str("200")), ("body", t.str("OK"))],
                        ),
                        |t| {
                            t.enum_match_stmt_with_bindings(
                                t.var("resp"),
                                vec![
                                    (
                                        "Success",
                                        vec![("code", "c"), ("body", "b")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write_expr(t.var("c"), false);
                                            t.write(":");
                                            t.write_expr(t.var("b"), false);
                                        }),
                                    ),
                                    (
                                        "Error",
                                        vec![("reason", "r")],
                                        Box::new(|t: &mut IrBuilder| {
                                            t.write("Error:");
                                            t.write_expr(t.var("r"), false);
                                        }),
                                    ),
                                ],
                            );
                        },
                    );
                })
                .build(),
            "200:OK",
            expect![[r#"
                -- input --
                enum Response {
                  Success(code: String, body: String),
                  Error(reason: String),
                }
                Test() {
                  let resp = Response::Success(code: "200", body: "OK") in {
                    match resp {
                      Response::Success(code: c, body: b) => {
                        write_expr(c)
                        write(":")
                        write_expr(b)
                      }
                      Response::Error(reason: r) => {
                        write("Error:")
                        write_expr(r)
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("items", t.array(vec![t.str("a"), t.str("b"), t.str("c")]), |t| {
                        let len = t.array_length(t.var("items"));
                        t.write_expr(len, false);
                    });
                })
                .build(),
            "3",
            expect![[r#"
                -- input --
                Test() {
                  let items = ["a", "b", "c"] in {
                    write_expr(items.len())
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
        use crate::dop::Type;

        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("items", t.typed_array(Type::String, vec![]), |t| {
                        let len = t.array_length(t.var("items"));
                        t.write_expr(len, false);
                    });
                })
                .build(),
            "0",
            expect![[r#"
                -- input --
                Test() {
                  let items = [] in {
                    write_expr(items.len())
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("items", t.array(vec![t.str("x"), t.str("y")]), |t| {
                        let len = t.array_length(t.var("items"));
                        t.if_stmt(t.eq(len, t.int(2)), |t| {
                            t.write("has two");
                        });
                    });
                })
                .build(),
            "has two",
            expect![[r#"
                -- input --
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("items", t.array(vec![t.str("a")]), |t| {
                        let len = t.array_length(t.var("items"));
                        t.if_stmt(t.less_than(len, t.int(5)), |t| {
                            t.write("less than 5");
                        });
                    });
                })
                .build(),
            "less than 5",
            expect![[r#"
                -- input --
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("numbers", t.array(vec![t.int(1), t.int(2), t.int(3), t.int(4), t.int(5)]), |t| {
                        let len = t.array_length(t.var("numbers"));
                        t.write_expr(len, false);
                    });
                })
                .build(),
            "5",
            expect![[r#"
                -- input --
                Test() {
                  let numbers = [1, 2, 3, 4, 5] in {
                    write_expr(numbers.len())
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("count", t.int(42), |t| {
                        let str_count = t.int_to_string(t.var("count"));
                        t.write_expr(str_count, false);
                    });
                })
                .build(),
            "42",
            expect![[r#"
                -- input --
                Test() {
                  let count = 42 in {
                    write_expr(count.to_string())
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("num", t.int(0), |t| {
                        let str_num = t.int_to_string(t.var("num"));
                        t.write_expr(str_num, false);
                    });
                })
                .build(),
            "0",
            expect![[r#"
                -- input --
                Test() {
                  let num = 0 in {
                    write_expr(num.to_string())
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
    fn int_to_string_negative() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("num", t.int(-123), |t| {
                        let str_num = t.int_to_string(t.var("num"));
                        t.write_expr(str_num, false);
                    });
                })
                .build(),
            "-123",
            expect![[r#"
                -- input --
                Test() {
                  let num = -123 in {
                    write_expr(num.to_string())
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
    fn int_to_string_concat() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("count", t.int(5), |t| {
                        let str_count = t.int_to_string(t.var("count"));
                        let message = t.string_concat(t.str("Count: "), str_count);
                        t.write_expr(message, false);
                    });
                })
                .build(),
            "Count: 5",
            expect![[r#"
                -- input --
                Test() {
                  let count = 5 in {
                    write_expr(("Count: " + count.to_string()))
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("price", t.float(3.7), |t| {
                        let int_price = t.float_to_int(t.var("price"));
                        t.write_expr(int_price, false);
                    });
                })
                .build(),
            "3",
            expect![[r#"
                -- input --
                Test() {
                  let price = 3.7 in {
                    write_expr(price.to_int())
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
    fn float_to_int_negative() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("temp", t.float(-2.9), |t| {
                        let int_temp = t.float_to_int(t.var("temp"));
                        t.write_expr(int_temp, false);
                    });
                })
                .build(),
            "-2",
            expect![[r#"
                -- input --
                Test() {
                  let temp = -2.9 in {
                    write_expr(temp.to_int())
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
    fn float_to_int_whole_number() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("val", t.float(5.0), |t| {
                        let int_val = t.float_to_int(t.var("val"));
                        t.write_expr(int_val, false);
                    });
                })
                .build(),
            "5",
            expect![[r#"
                -- input --
                Test() {
                  let val = 5 in {
                    write_expr(val.to_int())
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("price", t.float(19.99), |t| {
                        let str_price = t.float_to_string(t.var("price"));
                        t.write_expr(str_price, false);
                    });
                })
                .build(),
            "19.99",
            expect![[r#"
                -- input --
                Test() {
                  let price = 19.99 in {
                    write_expr(price.to_string())
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("price", t.float(9.99), |t| {
                        let str_price = t.float_to_string(t.var("price"));
                        let message = t.string_concat(t.str("$"), str_price);
                        t.write_expr(message, false);
                    });
                })
                .build(),
            "$9.99",
            expect![[r#"
                -- input --
                Test() {
                  let price = 9.99 in {
                    write_expr(("$" + price.to_string()))
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
    fn int_to_float_simple() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("count", t.int(42), |t| {
                        let float_count = t.int_to_float(t.var("count"));
                        t.write_expr(float_count, false);
                    });
                })
                .build(),
            "42",
            expect![[r#"
                -- input --
                Test() {
                  let count = 42 in {
                    write_expr(count.to_float())
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
    fn int_to_float_in_addition() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("count", t.int(5), |t| {
                        t.let_stmt("rate", t.float(0.5), |t| {
                            let float_count = t.int_to_float(t.var("count"));
                            let result = t.add(float_count, t.var("rate"));
                            t.write_expr(result, false);
                        });
                    });
                })
                .build(),
            "5.5",
            expect![[r#"
                -- input --
                Test() {
                  let count = 5 in {
                    let rate = 0.5 in {
                      write_expr((count.to_float() + rate))
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
    fn for_loop_with_underscore_range() {
        check(
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_range_discarded(t.int(0), t.int(2), |t| {
                        t.write("x");
                    });
                })
                .build(),
            "xxx",
            expect![[r#"
                -- input --
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.let_stmt("items", t.array(vec![t.str("a"), t.str("b"), t.str("c")]), |t| {
                        t.for_array_discarded(t.var("items"), |t| {
                            t.write("*");
                        });
                    });
                })
                .build(),
            "***",
            expect![[r#"
                -- input --
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_range_discarded(t.int(0), t.int(1), |t| {
                        t.for_range_discarded(t.int(0), t.int(2), |t| {
                            t.write(".");
                        });
                    });
                })
                .build(),
            "......",
            expect![[r#"
                -- input --
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
            IrModuleBuilder::new()
                .component("Test", [], |t| {
                    t.for_range("i", t.int(1), t.int(2), |t| {
                        t.for_range_discarded(t.int(0), t.int(1), |t| {
                            t.write_expr(t.var("i"), false);
                        });
                    });
                })
                .build(),
            "1122",
            expect![[r#"
                -- input --
                Test() {
                  for i in 1..=2 {
                    for _ in 0..=1 {
                      write_expr(i)
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
}
