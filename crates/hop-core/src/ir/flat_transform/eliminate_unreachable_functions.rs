use std::collections::BTreeSet;

use crate::ir::flat_program::{FlatFunctionDeclaration, FlatProgram};
use crate::ir::function_id::FunctionId;

use super::callees::{collect_data_callees, collect_html_callees};

/// Drop the functions no entrypoint can reach.
pub fn eliminate_unreachable_functions(program: FlatProgram) -> FlatProgram {
    let mut reached: BTreeSet<FunctionId> = (program.functions.iter())
        .filter(|function| function.entrypoint())
        .map(|function| function.function().id)
        .collect();
    let mut pending: Vec<FunctionId> = reached.iter().copied().collect();
    while let Some(id) = pending.pop() {
        let mut callees = BTreeSet::new();
        match (program.functions.iter())
            .find(|function| function.function().id == id)
            .unwrap()
        {
            FlatFunctionDeclaration::Data(function) => {
                collect_data_callees(&function.body, &mut callees);
            }
            FlatFunctionDeclaration::Html(function) => {
                collect_html_callees(&function.body, &mut callees);
            }
        }
        for callee in callees {
            if reached.insert(callee) {
                pending.push(callee);
            }
        }
    }
    FlatProgram {
        functions: (program.functions.into_iter())
            .filter(|function| reached.contains(&function.function().id))
            .collect(),
        ..program
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;
    use crate::hop::typing::Ty;
    use crate::ir::flat_program_builder::FlatProgramBuilder;
    use crate::ir::ir_type::IrType;

    fn check(program: FlatProgram, expected: Expect) {
        let before = program.to_string();
        let after = eliminate_unreachable_functions(program).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_drop_a_recursive_function_no_entrypoint_calls() {
        check(
            FlatProgramBuilder::new()
                .declare("ping", [("n", Ty::Int)], Ty::Int)
                .declare("pong", [("n", Ty::Int)], Ty::Int)
                .define("ping", |t, _| t.call("pong", [t.param("n")]))
                .define("pong", |t, _| t.call("ping", [t.param("n")]))
                .entrypoint("Test", [], Ty::Html, |t| t.text("shown"))
                .build(),
            expect![[r#"
                -- before --
                fn ping@f0(n@v0: Int) -> Int {
                  let v2: Int = pong@f1(v0)
                  v2
                }
                fn pong@f1(n@v1: Int) -> Int {
                  let v3: Int = ping@f0(v1)
                  v3
                }
                entrypoint fn Test@f2() -> Html {
                  let v4: Html = text("shown")
                  v4
                }

                -- after --
                entrypoint fn Test@f2() -> Html {
                  let v4: Html = text("shown")
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_the_callees_of_a_loop_and_of_a_lambda_nothing_applies() {
        check(
            FlatProgramBuilder::new()
                .function("row", [("n", Ty::Int)], Ty::Html, |t| {
                    t.escape(t.int_to_string(t.param("n")))
                })
                .function("label", [], Ty::Html, |t| t.text("label"))
                .function("unused", [], Ty::Html, |t| t.text("unused"))
                .entrypoint("Test", [("items", Ty::array(Ty::Int))], Ty::Html, |t| {
                    t.lambda([], |t, _| t.call("label", []));
                    t.html_for(t.param("items"), |t, item| t.call("row", [item]))
                })
                .build(),
            expect![[r#"
                -- before --
                fn row@f0(n@v0: Int) -> Html {
                  let v1: String = v0.to_string()
                  let v2: Html = escape(v1)
                  v2
                }
                fn label@f1() -> Html {
                  let v3: Html = text("label")
                  v3
                }
                fn unused@f2() -> Html {
                  let v4: Html = text("unused")
                  v4
                }
                entrypoint fn Test@f3(items@v5: Array[Int]) -> Html {
                  let v7: () -> Html = fn() {
                    let v6: Html = label@f1()
                    v6
                  }
                  let v10: Html = for v8: Int in v5 {
                    let v9: Html = row@f0(v8)
                    v9
                  }
                  v10
                }

                -- after --
                fn row@f0(n@v0: Int) -> Html {
                  let v1: String = v0.to_string()
                  let v2: Html = escape(v1)
                  v2
                }
                fn label@f1() -> Html {
                  let v3: Html = text("label")
                  v3
                }
                entrypoint fn Test@f3(items@v5: Array[Int]) -> Html {
                  let v7: () -> Html = fn() {
                    let v6: Html = label@f1()
                    v6
                  }
                  let v10: Html = for v8: Int in v5 {
                    let v9: Html = row@f0(v8)
                    v9
                  }
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_every_entrypoint_and_what_each_calls() {
        check(
            FlatProgramBuilder::new()
                .function("double", [("n", Ty::Int)], Ty::Int, |t| {
                    t.mul(t.param("n"), t.int(2))
                })
                .entrypoint_with_types("Count", [("n", IrType::INT)], IrType::INT, |t| {
                    t.call("double", [t.param("n")])
                })
                .entrypoint("Test", [], Ty::Html, |t| t.text("shown"))
                .build(),
            expect![[r#"
                -- before --
                fn double@f0(n@v0: Int) -> Int {
                  let v1: Int = 2
                  let v2: Int = mul(v0, v1)
                  v2
                }
                entrypoint fn Count@f1(n@v3: Int) -> Int {
                  let v4: Int = double@f0(v3)
                  v4
                }
                entrypoint fn Test@f2() -> Html {
                  let v5: Html = text("shown")
                  v5
                }

                -- after --
                fn double@f0(n@v0: Int) -> Int {
                  let v1: Int = 2
                  let v2: Int = mul(v0, v1)
                  v2
                }
                entrypoint fn Count@f1(n@v3: Int) -> Int {
                  let v4: Int = double@f0(v3)
                  v4
                }
                entrypoint fn Test@f2() -> Html {
                  let v5: Html = text("shown")
                  v5
                }
            "#]],
        );
    }
}
