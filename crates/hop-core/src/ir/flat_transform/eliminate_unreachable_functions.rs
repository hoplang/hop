use std::collections::BTreeSet;

use crate::ir::flat_program::{FlatFunction, FlatProgram};
use crate::ir::function_id::FunctionId;

use super::callees::{collect_html_callees, collect_pure_callees};

pub fn eliminate_unreachable_functions(program: FlatProgram) -> FlatProgram {
    let mut reached = BTreeSet::new();
    for entrypoint in program.entrypoints.iter() {
        collect_html_callees(&entrypoint.body, &mut reached);
    }
    let mut pending: Vec<FunctionId> = reached.iter().copied().collect();
    while let Some(id) = pending.pop() {
        let mut callees = BTreeSet::new();
        match (program.functions.iter())
            .find(|function| function.function().id == id)
            .unwrap()
        {
            FlatFunction::Pure(function) => {
                collect_pure_callees(&function.body, &mut callees);
            }
            FlatFunction::Html(function) => {
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
    use crate::ir::flat_lint;
    use crate::ir::flat_program_builder::FlatProgramBuilder;
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_programs_evaluate_identically_after_elimination() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entrypoint_args = random_entrypoint_args(&program, &mut rng);

            let before_program = program.clone();
            let before: Vec<Option<String>> = entrypoint_args
                .iter()
                .map(|(entrypoint, args)| {
                    match evaluate_entrypoint(&program, *entrypoint, args.clone())
                        .map(Value::into_markup)
                    {
                        Ok(output) => Some(output),
                        Err(
                            EvalError::CallDepthLimit { .. }
                            | EvalError::OpCountLimit { .. }
                            | EvalError::HtmlSizeLimit { .. }
                            | EvalError::StringLengthLimit { .. },
                        ) => None,
                        Err(error) => panic!("{error}"),
                    }
                })
                .collect();

            let program = eliminate_unreachable_functions(program);
            flat_lint::lint(&program).unwrap();

            for ((entrypoint, args), before) in entrypoint_args.iter().zip(&before) {
                // An entrypoint that hit a limit has no output to preserve.
                let Some(before) = before else {
                    continue;
                };
                let after = evaluate_entrypoint(&program, *entrypoint, args.clone())
                    .unwrap()
                    .into_markup();
                assert_eq!(
                    before, &after,
                    "entrypoint {entrypoint}\n-- before --\n{before_program}\n-- after --\n{program}"
                );
            }
            Ok(())
        });
    }

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
                .entrypoint("Test", [], |t| t.text("shown"))
                .build()
                .unwrap(),
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
                entrypoint Test@e0() -> Html {
                  let v4: Html = text("shown")
                  v4
                }

                -- after --
                entrypoint Test@e0() -> Html {
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
                .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                    t.lambda([], |t, _| t.call("label", []));
                    t.html_for(t.param("items"), |t, item| t.call("row", [item]))
                })
                .build()
                .unwrap(),
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
                entrypoint Test@e0(items@v5: Array[Int]) -> Html {
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
                entrypoint Test@e0(items@v5: Array[Int]) -> Html {
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
    fn should_keep_the_callee_of_a_loop_key() {
        check(
            FlatProgramBuilder::new()
                .function("key", [("n", Ty::Int)], Ty::Int, |t| t.param("n"))
                .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                    t.html_for_keyed(t.param("items"), |t, item| t.call("key", [item]))
                        .body(|t, _| t.text("row"))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn key@f0(n@v0: Int) -> Int {
                  v0
                }
                entrypoint Test@e0(items@v1: Array[Int]) -> Html {
                  let v5: Html = for v2: Int in v1 key {
                    let v3: Int = key@f0(v2)
                    v3
                  } {
                    let v4: Html = text("row")
                    v4
                  }
                  v5
                }

                -- after --
                fn key@f0(n@v0: Int) -> Int {
                  v0
                }
                entrypoint Test@e0(items@v1: Array[Int]) -> Html {
                  let v5: Html = for v2: Int in v1 key {
                    let v3: Int = key@f0(v2)
                    v3
                  } {
                    let v4: Html = text("row")
                    v4
                  }
                  v5
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
                .entrypoint("Count", [("n", Ty::Int)], |t| {
                    t.escape(t.int_to_string(t.call("double", [t.param("n")])))
                })
                .entrypoint("Test", [], |t| t.text("shown"))
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn double@f0(n@v0: Int) -> Int {
                  let v1: Int = 2
                  let v2: Int = mul(v0, v1)
                  v2
                }
                entrypoint Count@e0(n@v3: Int) -> Html {
                  let v4: Int = double@f0(v3)
                  let v5: String = v4.to_string()
                  let v6: Html = escape(v5)
                  v6
                }
                entrypoint Test@e1() -> Html {
                  let v7: Html = text("shown")
                  v7
                }

                -- after --
                fn double@f0(n@v0: Int) -> Int {
                  let v1: Int = 2
                  let v2: Int = mul(v0, v1)
                  v2
                }
                entrypoint Count@e0(n@v3: Int) -> Html {
                  let v4: Int = double@f0(v3)
                  let v5: String = v4.to_string()
                  let v6: Html = escape(v5)
                  v6
                }
                entrypoint Test@e1() -> Html {
                  let v7: Html = text("shown")
                  v7
                }
            "#]],
        );
    }
}
