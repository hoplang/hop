use std::collections::HashSet;

use crate::hop::typing::Pattern;
use crate::ir::flat_module::{
    FlatDataBinding, FlatDataBlock, FlatDataFunction, FlatDataOp, FlatFunctionDeclaration,
    FlatHtmlBinding, FlatHtmlBlock, FlatHtmlFunction, FlatHtmlOp, FlatModule,
};
use crate::ir::ir_for::IrFor;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::value_id::ValueId;

/// A pass that removes the bindings nothing reads from every body.
///
/// Every op is pure, so a binding whose name no later binding, nested
/// block or result reads computes nothing anyone sees, and goes away with
/// the blocks nested in it. A loop variable or binding of a pattern that
/// nothing reads any more goes away too, and becomes `_`.
pub fn eliminate_dead_bindings(module: FlatModule) -> FlatModule {
    let functions = module
        .functions
        .into_iter()
        .map(|function| match function {
            FlatFunctionDeclaration::Data(function) => {
                FlatFunctionDeclaration::Data(FlatDataFunction {
                    body: eliminate_data(function.body, &mut HashSet::new()),
                    ..function
                })
            }
            FlatFunctionDeclaration::Html(function) => {
                FlatFunctionDeclaration::Html(FlatHtmlFunction {
                    body: eliminate_html(function.body, &mut HashSet::new()),
                    ..function
                })
            }
        })
        .collect();
    FlatModule {
        functions,
        value_ids: module.value_ids,
    }
}

/// Walk the bindings backwards, keeping a binding when its name is live
/// and making what it reads live in turn. `live` holds the names read by
/// what comes after, and is shared with nested blocks, since names are
/// unique. A loop variable or binding of a pattern stays when the block it
/// scopes over, walked first, reads it.
fn eliminate_data(block: FlatDataBlock, live: &mut HashSet<ValueId>) -> FlatDataBlock {
    live.insert(block.result);
    FlatDataBlock {
        bindings: eliminate_data_bindings(block.bindings, live),
        result: block.result,
    }
}

/// Walk the Html backwards and then the values, as `eliminate_data` walks
/// the bindings of a value.
fn eliminate_html(block: FlatHtmlBlock, live: &mut HashSet<ValueId>) -> FlatHtmlBlock {
    live.insert(block.result);
    let html = eliminate_html_bindings(block.html, live);
    FlatHtmlBlock {
        bindings: eliminate_data_bindings(block.bindings, live),
        html,
        result: block.result,
    }
}

fn eliminate_data_bindings(
    bindings: Vec<FlatDataBinding>,
    live: &mut HashSet<ValueId>,
) -> Vec<FlatDataBinding> {
    let mut kept = Vec::with_capacity(bindings.len());
    for binding in bindings.into_iter().rev() {
        if !live.contains(&binding.binder.var) {
            continue;
        }
        let FlatDataBinding { binder, op } = binding;
        let op = match op {
            FlatDataOp::Match(match_) => {
                FlatDataOp::Match(eliminate_match(match_, live, eliminate_data))
            }
            op => op,
        };
        op.for_each_operand(&mut |operand| {
            live.insert(operand);
        });
        kept.push(FlatDataBinding { binder, op });
    }
    kept.reverse();
    kept
}

fn eliminate_html_bindings(
    bindings: Vec<FlatHtmlBinding>,
    live: &mut HashSet<ValueId>,
) -> Vec<FlatHtmlBinding> {
    let mut kept = Vec::with_capacity(bindings.len());
    for binding in bindings.into_iter().rev() {
        if !live.contains(&binding.binder.var) {
            continue;
        }
        let FlatHtmlBinding { binder, op } = binding;
        let op = match op {
            FlatHtmlOp::Match(match_) => {
                FlatHtmlOp::Match(eliminate_match(match_, live, eliminate_html))
            }
            FlatHtmlOp::For(IrFor { var, array, body }) => {
                let body = eliminate_html(body, live);
                let var = var.filter(|binder| live.contains(&binder.var));
                FlatHtmlOp::For(IrFor { var, array, body })
            }
            op => op,
        };
        op.for_each_operand(&mut |operand| {
            live.insert(operand);
        });
        kept.push(FlatHtmlBinding { binder, op });
    }
    kept.reverse();
    kept
}

/// Eliminate in each arm, and make a binding of its pattern a wildcard
/// when the arm does not read it.
fn eliminate_match<B>(
    match_: IrMatch<B>,
    live: &mut HashSet<ValueId>,
    eliminate_block: impl Fn(B, &mut HashSet<ValueId>) -> B,
) -> IrMatch<B> {
    IrMatch {
        subject: match_.subject,
        arms: match_
            .arms
            .into_iter()
            .map(|arm| {
                let body = eliminate_block(arm.body, live);
                let mut pattern = arm.pattern;
                pattern.for_each_binding_mut(&mut |binding| {
                    if let Pattern::Binding(binder) = binding
                        && !live.contains(&binder.var)
                    {
                        *binding = Pattern::Wildcard;
                    }
                });
                IrMatchArm { pattern, body }
            })
            .collect(),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::hop::typing::{Ty, TypeRegistryBuilder};
    use crate::ir::flat_module::FlatModule;
    use crate::ir::flat_module_builder::{FlatModuleBuilder, Pat};
    use crate::ir::flat_module_generator::{random_entrypoint_args, random_module};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_elimination() {
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
                        Err(EvalError::CallDepthLimit { .. } | EvalError::OpCountLimit { .. }) => {
                            None
                        }
                        Err(error) => panic!("{error}"),
                    }
                })
                .collect();

            let module = eliminate_dead_bindings(module);

            for ((function, args), before) in entrypoint_args.iter().zip(&before) {
                // An entrypoint that hit the call depth limit or the op count limit
                // has no output to preserve, and the pass may drop the diverging call.
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
        let after = eliminate_dead_bindings(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_drop_a_value_nothing_reads() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    let _unused = t.str("value");
                    t.text("Hello")
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: String = "value"
                  let v1: Html = text("Hello")
                  v1
                }

                -- after --
                entrypoint fn Test@f0() -> Html {
                  let v1: Html = text("Hello")
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_a_dead_match_with_its_arms() {
        check(
            FlatModuleBuilder::new()
                .function("f", [], Ty::Int, |t| t.call("f", []))
                .entrypoint("Test", [("flag", Ty::Bool)], Ty::Html, |t| {
                    let _unused = t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.call("f", []));
                        m.arm(Pat::False, |t, _| t.int(0));
                    });
                    t.text("Hello")
                })
                .build(),
            expect![[r#"
                -- before --
                fn f@f0() -> Int {
                  let v0: Int = f@f0()
                  v0
                }
                entrypoint fn Test@f1(flag@v1: Bool) -> Html {
                  let v4: Int = match v1 {
                    true => {
                      let v2: Int = f@f0()
                      v2
                    }
                    false => {
                      let v3: Int = 0
                      v3
                    }
                  }
                  let v5: Html = text("Hello")
                  v5
                }

                -- after --
                fn f@f0() -> Int {
                  let v0: Int = f@f0()
                  v0
                }
                entrypoint fn Test@f1(flag@v1: Bool) -> Html {
                  let v5: Html = text("Hello")
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_unbind_an_unused_loop_variable() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Test", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    t.html_for(t.param("items"), |t, _| t.text("."))
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(items@v0: Array[String]) -> Html {
                  let v3: Html = for v1: String in v0 {
                    let v2: Html = text(".")
                    v2
                  }
                  v3
                }

                -- after --
                entrypoint fn Test@f0(items@v0: Array[String]) -> Html {
                  let v3: Html = for _ in v0 {
                    let v2: Html = text(".")
                    v2
                  }
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_unbind_an_unused_option_binding() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Test", [("name", Ty::option(Ty::String))], Ty::Html, |t| {
                    t.match_(t.param("name"), |m| {
                        m.arm(Pat::some(Pat::bind("n")), |t, _| t.text("some"));
                        m.arm(Pat::None, |t, _| t.text("none"));
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(name@v0: Option[String]) -> Html {
                  let v4: Html = match v0 {
                    Some(v1: String) => {
                      let v2: Html = text("some")
                      v2
                    }
                    None => {
                      let v3: Html = text("none")
                      v3
                    }
                  }
                  v4
                }

                -- after --
                entrypoint fn Test@f0(name@v0: Option[String]) -> Html {
                  let v4: Html = match v0 {
                    Some(_) => {
                      let v2: Html = text("some")
                      v2
                    }
                    None => {
                      let v3: Html = text("none")
                      v3
                    }
                  }
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_unused_enum_arm_bindings_and_keep_used_ones() {
        let types = TypeRegistryBuilder::new()
            .enum_(
                "Shape",
                [
                    ("Dot", vec![]),
                    ("Rect", vec![("width", Ty::Int), ("height", Ty::Int)]),
                ],
            )
            .build();
        check(
            FlatModuleBuilder::with_registry(types)
                .function("width", [("shape", Ty::named("Shape"))], Ty::Int, |t| {
                    t.match_(t.param("shape"), |m| {
                        m.arm(Pat::variant("Dot", []), |t, _| t.int(0));
                        m.arm(
                            Pat::variant(
                                "Rect",
                                [("width", Pat::bind("w")), ("height", Pat::bind("h"))],
                            ),
                            |_, arm| arm.binding("w"),
                        );
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                fn width@f0(shape@v0: Shape) -> Int {
                  let v4: Int = match v0 {
                    Shape::Dot => {
                      let v1: Int = 0
                      v1
                    }
                    Shape::Rect {width@v2: Int, height@v3: Int} => {
                      v2
                    }
                  }
                  v4
                }

                -- after --
                fn width@f0(shape@v0: Shape) -> Int {
                  let v4: Int = match v0 {
                    Shape::Dot => {
                      let v1: Int = 0
                      v1
                    }
                    Shape::Rect {width@v2: Int, height: _} => {
                      v2
                    }
                  }
                  v4
                }
            "#]],
        );
    }
}
