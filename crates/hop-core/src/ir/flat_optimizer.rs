use crate::ir::flat_module::{FlatFunctionDeclaration, FlatModule, FlatPageDeclaration};
use crate::ir::flat_transform;

/// Optimize the module: inline every call that can be, then in every body
/// evaluate what is constant and drop the bindings nothing reads.
pub fn optimize_flat(module: FlatModule) -> FlatModule {
    let module = flat_transform::inline_function_calls(module);
    let mut var_ids = module.var_ids;
    let pages = module
        .pages
        .into_iter()
        .map(|page| FlatPageDeclaration {
            name: page.name,
            parameters: page.parameters,
            head: flat_transform::eliminate_dead_bindings(
                flat_transform::perform_partial_evaluation(page.head, &mut var_ids),
            ),
            body: flat_transform::eliminate_dead_bindings(
                flat_transform::perform_partial_evaluation(page.body, &mut var_ids),
            ),
        })
        .collect();
    let functions = module
        .functions
        .into_iter()
        .map(|function| FlatFunctionDeclaration {
            function: function.function,
            parameters: function.parameters,
            return_type: function.return_type,
            body: flat_transform::eliminate_dead_bindings(
                flat_transform::perform_partial_evaluation(function.body, &mut var_ids),
            ),
        })
        .collect();
    FlatModule {
        pages,
        functions,
        var_ids,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::pure_to_flat::pure_to_flat;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_page;
    use crate::ir::runtime::random::random_value;
    use crate::ir::runtime::value::Value;
    use crate::symbols::attribute_name::AttributeName;
    use crate::symbols::type_name::TypeName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_optimization() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let module = pure_to_flat(module);

            let page_args: Vec<(TypeName, HashMap<AttributeName, Value>)> = module
                .pages
                .iter()
                .map(|page| {
                    let args = page
                        .parameters
                        .iter()
                        .map(|p| {
                            (
                                p.name().clone(),
                                random_value(&mut rng, &p.typ, None, &registry),
                            )
                        })
                        .collect();
                    (page.name.clone(), args)
                })
                .collect();

            let before_module = module.to_string();
            let before: Vec<Option<String>> = page_args
                .iter()
                .map(|(page_name, args)| {
                    match evaluate_page(&module, page_name, args.clone(), None) {
                        Ok(output) => Some(output),
                        Err(EvalError::RecursionLimit { .. }) => None,
                        Err(error) => panic!("{error}"),
                    }
                })
                .collect();

            let module = optimize_flat(module);

            for ((page_name, args), before) in page_args.iter().zip(&before) {
                // A page that hit the call depth limit has no output to
                // preserve, and the passes may drop the diverging call.
                let Some(before) = before else {
                    continue;
                };
                let after = evaluate_page(&module, page_name, args.clone(), None).unwrap();
                assert_eq!(
                    before, &after,
                    "page {page_name}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    fn check(module: PureModule, expected: Expect) {
        let module = pure_to_flat(module);
        let before = module.to_string();
        let after = optimize_flat(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_leave_only_the_computations_a_page_renders() {
        check(
            PureModuleBuilder::new()
                .page("Items", [("items", "Array[String]")], |t| {
                    t.let_expr("unused", t.str("value"), |t| {
                        t.element(
                            "ul",
                            vec![],
                            vec![t.html_for(Some("item"), t.var("items"), |t| {
                                t.element("li", vec![], vec![t.escape(t.var("item"))])
                            })],
                        )
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                page Items(items@v0: Array[String]) {
                  let v4: String = "value"
                  let v8: Html = for v2: String in v0 {
                    let v5: Html = escape(v2)
                    let v6: Html = concat(v5)
                    let v7: Html = html(tag: "li", attrs: [], children: v6)
                    v7
                  }
                  let v9: Html = concat(v8)
                  let v10: Html = html(tag: "ul", attrs: [], children: v9)
                  v10
                }

                -- after --
                page Items(items@v0: Array[String]) {
                  let v8: Html = for v2: String in v0 {
                    let v5: Html = escape(v2)
                    let v7: Html = html(tag: "li", attrs: [], children: v5)
                    v7
                  }
                  let v10: Html = html(tag: "ul", attrs: [], children: v8)
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_select_an_arm_and_drop_what_the_match_read() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    t.let_expr("flag", t.bool(true), |t| {
                        t.concat(vec![t.bool_match_expr(
                            t.var("flag"),
                            t.concat(vec![t.text("yes")]),
                            t.concat(vec![]),
                        )])
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  let v2: Bool = true
                  let v6: Html = match v2 {
                    true => {
                      let v3: Html = text("yes")
                      let v4: Html = concat(v3)
                      v4
                    }
                    false => {
                      let v5: Html = concat()
                      v5
                    }
                  }
                  let v7: Html = concat(v6)
                  v7
                }

                -- after --
                page Test() {
                  let v3: Html = text("yes")
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_a_call_and_fold_what_its_argument_makes_constant() {
        check(
            PureModuleBuilder::new()
                .function("double", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.var("x"))
                })
                .page_no_params("Test", |t| {
                    t.escape(t.int_to_string(t.call("double", vec![("x", t.int(21))])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn double@f0(x@v0: Int) -> Int {
                  let v6: Int = v0 + v0
                  v6
                }
                page Test() {
                  let v2: Int = 21
                  let v3: Int = call double@f0(v2)
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                page Test() {
                  let v4: String = "42"
                  let v5: Html = escape(v4)
                  v5
                }
            "#]],
        );
    }
}
