use crate::ir::flat_program::FlatProgram;
use crate::ir::flat_transform;

/// Optimize the program.
pub fn optimize_flat(program: FlatProgram) -> FlatProgram {
    let program = flat_transform::inline_function_calls(program);
    let program = flat_transform::perform_partial_evaluation(program);
    let program = flat_transform::eliminate_dead_bindings(program);
    let program = flat_transform::eliminate_unreachable_functions(program);
    program
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::typing::{NumericType, Ty};
    use crate::ir::entrypoint_id::EntrypointId;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::flat_typecheck;
    use crate::ir::ir_type::IrType;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::clicks::clicks;
    use crate::ir::runtime::flat_evaluator::invoke_entrypoint;
    use crate::ir::runtime::labels::Labels;
    use crate::ir::runtime::state::State;
    use crate::ir::runtime::value::{Html, HtmlNode, Value};
    use crate::symbols::attribute_name::AttributeName;
    use expect_test::{Expect, expect};
    use std::collections::HashMap;

    #[test]
    fn fuzz_random_programs_evaluate_identically_after_optimization() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            let entrypoint_args = random_entrypoint_args(&program, u)?;
            // The elements clicked on each entrypoint, one after each render,
            // each an index into the clicks of that render, wrapped around.
            let mut clicked = Vec::with_capacity(entrypoint_args.len());
            for _ in &entrypoint_args {
                let count = u.int_in_range(0..=4)?;
                clicked.push(
                    (0..count)
                        .map(|_| u.arbitrary())
                        .collect::<Result<Vec<usize>, _>>()?,
                );
            }

            let optimized = optimize_flat(program.clone());
            flat_typecheck::typecheck(&optimized).unwrap();

            for ((entrypoint, args), clicked) in entrypoint_args.iter().zip(&clicked) {
                let before = observe(&program, *entrypoint, args, clicked);
                let after = observe(&optimized, *entrypoint, args, clicked);
                // What follows a render or a click that hit a limit before
                // has nothing to preserve.
                assert!(
                    after.len() >= before.len() && before == after[..before.len()],
                    "entrypoint {entrypoint}, clicked {clicked:?}\n\
                     -- before --\n{program}\n{before:#?}\n\
                     -- after --\n{optimized}\n{after:#?}"
                );
            }
            Ok(())
        });
    }

    /// What a page shows over an invocation and a click after each render:
    /// the markup of each render, how many of its elements have a click, and
    /// the tag and label of each element it shows, so that an element keeps
    /// its label exactly when it keeps its node. Ends at the first
    /// invocation or click that hits a limit of the evaluator.
    fn observe(
        program: &FlatProgram,
        entrypoint: EntrypointId,
        args: &HashMap<AttributeName, Value<'static>>,
        clicked: &[usize],
    ) -> Vec<String> {
        fn shown(html: &Html<'_>, labels: &mut Labels, out: &mut Vec<String>) {
            match html.node() {
                HtmlNode::Element {
                    element,
                    children,
                    identity,
                    ..
                } => {
                    out.push(format!("{}@e{}", element.as_str(), labels.label(*identity)));
                    shown(children, labels, out);
                }
                HtmlNode::Concat(parts) => {
                    for part in parts {
                        shown(part, labels, out);
                    }
                }
                HtmlNode::Text(_) | HtmlNode::Escape(_) => {}
            }
        }
        let is_limit = |error: &EvalError| {
            matches!(
                error,
                EvalError::CallDepthLimit { .. }
                    | EvalError::OpCountLimit { .. }
                    | EvalError::HtmlSizeLimit { .. }
                    | EvalError::StringLengthLimit { .. }
                    // A cell keeps a value from a click, so a range between
                    // two cells may exceed the array length limit.
                    | EvalError::ArrayLengthLimit { .. }
            )
        };
        let mut out = Vec::new();
        let mut state = State::default();
        let mut labels = Labels::default();
        for step in 0..=clicked.len() {
            let (value, next_state) =
                match invoke_entrypoint(program, entrypoint, args.clone(), &state) {
                    Ok(invoked) => invoked,
                    Err(error) if is_limit(&error) => break,
                    Err(error) => panic!("{error}"),
                };
            let html = value.unwrap_html();
            let targets = clicks(&program.functions, &html);
            let mut markup = String::new();
            html.write(&mut markup);
            let mut elements = Vec::new();
            shown(&html, &mut labels, &mut elements);
            out.push(format!(
                "{markup}\n{} clicks, shown: {}",
                targets.len(),
                elements.join(" ")
            ));
            let Some(choice) = clicked.get(step) else {
                break;
            };
            if targets.is_empty() {
                break;
            }
            let effect = match &targets[choice % targets.len()].1 {
                Ok(effect) => effect,
                Err(error) if is_limit(error) => break,
                Err(error) => panic!("{error}"),
            };
            state = next_state;
            match state.run(effect, &program.functions) {
                Ok(()) => {}
                Err(error) if is_limit(&error) => break,
                Err(error) => panic!("{error}"),
            }
        }
        out
    }

    fn check(program: FlatProgram, expected: Expect) {
        let before = program.to_string();
        let after = optimize_flat(program).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_render_the_body_of_a_closure_a_called_function_applies_in_place() {
        check(
            FlatProgramBuilder::new()
                .function(
                    "Rows",
                    [
                        ("items", Ty::array(Ty::String)),
                        ("row", Ty::function([Ty::String], Ty::Html)),
                    ],
                    Ty::Html,
                    |t| {
                        let rows =
                            t.html_for(t.param("items"), |t, item| t.apply(t.param("row"), [item]));
                        t.element("ul", [], Some(rows))
                    },
                )
                .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                    let row = t.lambda([IrType::STRING], |t, params| {
                        t.element("li", [], Some(t.escape(params[0])))
                    });
                    t.call("Rows", [t.param("items"), row])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn Rows@f0(items@v0: Array[String], row@v1: (String) -> Html) -> Html {
                  let v4: Html = for v2: String in v0 {
                    let v3: Html = v1(v2)
                    v3
                  }
                  let v5: Html = element("ul", {}, v4)
                  v5
                }
                entrypoint Test@e0(items@v6: Array[String]) -> Html {
                  let v10: (String) -> Html = fn(v7: String) {
                    let v8: Html = escape(v7)
                    let v9: Html = element("li", {}, v8)
                    v9
                  }
                  let v11: Html = Rows@f0(v6, v10)
                  v11
                }

                -- after --
                entrypoint Test@e0(items@v6: Array[String]) -> Html {
                  let v14: Html = for v12: String in v6 {
                    let v16: Html = escape(v12)
                    let v17: Html = element("li", {}, v16)
                    v17
                  }
                  let v15: Html = element("ul", {}, v14)
                  v15
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_only_the_computations_an_entrypoint_renders() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Items", [("items", Ty::array(Ty::String))], |t| {
                    let _unused = t.str("value");
                    t.element(
                        "ul",
                        [],
                        Some(t.html_for(t.param("items"), |t, item| {
                            t.element("li", vec![], Some(t.escape(item)))
                        })),
                    )
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Items@e0(items@v0: Array[String]) -> Html {
                  let v1: String = "value"
                  let v5: Html = for v2: String in v0 {
                    let v3: Html = escape(v2)
                    let v4: Html = element("li", {}, v3)
                    v4
                  }
                  let v6: Html = element("ul", {}, v5)
                  v6
                }

                -- after --
                entrypoint Items@e0(items@v0: Array[String]) -> Html {
                  let v5: Html = for v2: String in v0 {
                    let v3: Html = escape(v2)
                    let v4: Html = element("li", {}, v3)
                    v4
                  }
                  let v6: Html = element("ul", {}, v5)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_select_an_arm_and_drop_what_the_match_read() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let flag = t.bool(true);
                    t.html_concat([t.match_(flag, |m| {
                        m.arm(Pat::True, |t, _| t.html_concat(vec![t.text("yes")]));
                        m.arm(Pat::False, |t, _| t.html_concat(vec![]));
                    })])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Bool = true
                  let v4: Html = match v0 {
                    true => {
                      let v1: Html = text("yes")
                      let v2: Html = concat(v1)
                      v2
                    }
                    false => {
                      let v3: Html = concat()
                      v3
                    }
                  }
                  let v5: Html = concat(v4)
                  v5
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v1: Html = text("yes")
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_a_call_and_fold_what_its_argument_makes_constant() {
        check(
            FlatProgramBuilder::new()
                .function("double", [("x", Ty::Int)], Ty::Int, |t| {
                    t.add(NumericType::Int, t.param("x"), t.param("x"))
                })
                .entrypoint("Test", [], |t| {
                    t.escape(t.int_to_string(t.call("double", [t.int(21)])))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn double@f0(x@v0: Int) -> Int {
                  let v1: Int = add[Int](v0, v0)
                  v1
                }
                entrypoint Test@e0() -> Html {
                  let v2: Int = 21
                  let v3: Int = double@f0(v2)
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v5: Html = text("42")
                  v5
                }
            "#]],
        );
    }
}
