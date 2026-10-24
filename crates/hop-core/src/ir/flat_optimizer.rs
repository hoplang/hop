use crate::ir::flat_module::FlatModule;
use crate::ir::flat_transform;

/// Optimize the module: inline every call that can be, then in every body
/// evaluate what is constant and drop the bindings nothing reads.
pub fn optimize_flat(module: FlatModule) -> FlatModule {
    let module = flat_transform::inline_function_calls(module);
    let module = flat_transform::perform_partial_evaluation(module);
    flat_transform::eliminate_dead_bindings(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::typing::Ty;
    use crate::ir::flat_lint;
    use crate::ir::flat_module_builder::{FlatModuleBuilder, Pat};
    use crate::ir::flat_module_generator::{random_entrypoint_args, random_module};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_optimization() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entrypoint_args = random_entrypoint_args(&module, &mut rng, &registry);

            let before_module = module.clone();
            let before: Vec<Option<String>> = entrypoint_args
                .iter()
                .map(|(function, args)| {
                    match evaluate_entrypoint(&module, function, args.clone())
                        .map(Value::into_markup)
                    {
                        Ok(output) => Some(output),
                        Err(EvalError::RecursionLimit { .. }) => None,
                        Err(error) => panic!("{error}"),
                    }
                })
                .collect();

            let module = optimize_flat(module);
            flat_lint::assert_well_typed(&module, &registry);

            for ((function, args), before) in entrypoint_args.iter().zip(&before) {
                // An entrypoint that hit the call depth limit has no output to
                // preserve, and the passes may drop the diverging call.
                let Some(before) = before else {
                    continue;
                };
                let after = evaluate_entrypoint(&module, function, args.clone())
                    .unwrap()
                    .into_markup();
                assert_eq!(
                    before, &after,
                    "entrypoint {function}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    fn check(module: FlatModule, expected: Expect) {
        let before = module.to_string();
        let after = optimize_flat(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_leave_only_the_computations_an_entrypoint_renders() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Items", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    let _unused = t.str("value");
                    t.element(
                        "ul",
                        [],
                        t.html_for(t.param("items"), |t, item| {
                            t.element("li", vec![], t.escape(item))
                        }),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Items@f0(items@v0: Array[String]) -> Html {
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
                entrypoint fn Items@f0(items@v0: Array[String]) -> Html {
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
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    let flag = t.bool(true);
                    t.html_concat([t.match_(flag, |m| {
                        m.arm(Pat::True, |t, _| t.html_concat(vec![t.text("yes")]));
                        m.arm(Pat::False, |t, _| t.html_concat(vec![]));
                    })])
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
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
                entrypoint fn Test@f0() -> Html {
                  let v1: Html = text("yes")
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_a_call_and_fold_what_its_argument_makes_constant() {
        check(
            FlatModuleBuilder::new()
                .function("double", [("x", Ty::Int)], Ty::Int, |t| {
                    t.add(t.param("x"), t.param("x"))
                })
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.escape(t.int_to_string(t.call("double", [t.int(21)])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn double@f0(x@v0: Int) -> Int {
                  let v1: Int = v0 + v0
                  v1
                }
                entrypoint fn Test@f1() -> Html {
                  let v2: Int = 21
                  let v3: Int = double@f0(v2)
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint fn Test@f1() -> Html {
                  let v4: String = "42"
                  let v5: Html = escape(v4)
                  v5
                }
            "#]],
        );
    }
}
