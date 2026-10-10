use std::collections::HashSet;

use crate::ir::binder_id::BinderId;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatOp};
use crate::ir::ir_match::{EnumMatchArm, Match};
use crate::ir::var_id::VarId;

/// A pass that removes the bindings nothing reads.
///
/// Every op is pure, so a binding whose name no later binding, nested
/// block or result reads computes nothing anyone sees, and goes away with
/// the blocks nested in it. A binder that no remaining Read reads goes away
/// too: an unused loop variable or option binding becomes `_`, and unused
/// enum arm bindings leave their arm.
pub fn eliminate_dead_bindings(block: FlatBlock) -> FlatBlock {
    let mut live = Live {
        names: HashSet::new(),
        binders: HashSet::new(),
    };
    eliminate(block, &mut live)
}

/// What the bindings after the current one read: the bindings they name,
/// and the binders they Read.
struct Live {
    names: HashSet<VarId>,
    binders: HashSet<BinderId>,
}

/// Walk the bindings backwards, keeping a binding when its name is live
/// and making what it reads live in turn. `live` holds what is read by
/// what comes after, and is shared with nested blocks, since names are
/// unique. A binder stays when the block it scopes over, walked first,
/// reads it.
fn eliminate(block: FlatBlock, live: &mut Live) -> FlatBlock {
    live.names.insert(block.result);
    let mut kept = Vec::with_capacity(block.bindings.len());
    for binding in block.bindings.into_iter().rev() {
        if !live.names.contains(&binding.name) {
            continue;
        }
        let FlatBinding { name, typ, op } = binding;
        let op = match op {
            FlatOp::Match(match_) => FlatOp::Match(match match_ {
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => Match::Bool {
                    subject,
                    true_body: Box::new(eliminate(*true_body, live)),
                    false_body: Box::new(eliminate(*false_body, live)),
                },
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => {
                    let some_arm_body = Box::new(eliminate(*some_arm_body, live));
                    let some_arm_binding =
                        some_arm_binding.filter(|binder| live.binders.contains(&binder.var));
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body: Box::new(eliminate(*none_arm_body, live)),
                    }
                }
                Match::Enum { subject, arms } => Match::Enum {
                    subject,
                    arms: arms
                        .into_iter()
                        .map(|arm| {
                            let body = eliminate(arm.body, live);
                            let bindings = arm
                                .bindings
                                .into_iter()
                                .filter(|(_, binder)| live.binders.contains(&binder.var))
                                .collect();
                            EnumMatchArm {
                                pattern: arm.pattern,
                                bindings,
                                body,
                            }
                        })
                        .collect(),
                },
            }),
            FlatOp::HtmlFor { var, source, body } => {
                let body = eliminate(body, live);
                let var = var.filter(|binder| live.binders.contains(&binder.var));
                FlatOp::HtmlFor { var, source, body }
            }
            FlatOp::Read(binder) => {
                live.binders.insert(binder);
                FlatOp::Read(binder)
            }
            op => op,
        };
        op.for_each_operand(&mut |operand| {
            live.names.insert(operand);
        });
        kept.push(FlatBinding { name, typ, op });
    }
    kept.reverse();
    FlatBlock {
        bindings: kept,
        result: block.result,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::flat_module::{FlatFunctionDeclaration, FlatModule, FlatPageDeclaration};
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

    fn run(module: FlatModule) -> FlatModule {
        FlatModule {
            var_ids: module.var_ids,
            binder_ids: module.binder_ids,
            pages: module
                .pages
                .into_iter()
                .map(|page| FlatPageDeclaration {
                    name: page.name,
                    parameters: page.parameters,
                    head: eliminate_dead_bindings(page.head),
                    body: eliminate_dead_bindings(page.body),
                })
                .collect(),
            functions: module
                .functions
                .into_iter()
                .map(|function| FlatFunctionDeclaration {
                    function: function.function,
                    parameters: function.parameters,
                    return_type: function.return_type,
                    body: eliminate_dead_bindings(function.body),
                })
                .collect(),
        }
    }

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_elimination() {
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

            let module = run(module);

            for ((page_name, args), before) in page_args.iter().zip(&before) {
                // A page that hit the call depth limit has no output to
                // preserve, and the pass may drop the diverging call.
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
        let after = run(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_drop_a_value_nothing_reads() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    t.let_expr("unused", t.str("value"), |t| t.text("Hello"))
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  let v1: String = "value"
                  let v2: Html = text("Hello")
                  v2
                }

                -- after --
                page Test() {
                  let v2: Html = text("Hello")
                  v2
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_a_dead_match_with_its_arms() {
        check(
            PureModuleBuilder::new()
                .function("f", [], "Int", |t| t.call("f", vec![]))
                .page("Test", [("flag", "Bool")], |t| {
                    t.let_expr(
                        "n",
                        t.bool_match_expr(t.var("flag"), t.call("f", vec![]), t.int(0)),
                        |t| t.text("Hello"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                fn f@f0() -> Int {
                  let v6: Int = call f@f0()
                  v6
                }
                page Test(flag@b0: Bool) {
                  let v1: Bool = b0
                  let v4: Int = match v1 {
                    true => {
                      let v2: Int = call f@f0()
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
                  let v6: Int = call f@f0()
                  v6
                }
                page Test(flag@b0: Bool) {
                  let v5: Html = text("Hello")
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_unbind_an_unused_loop_variable() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("items", "Array[String]")], |t| {
                    t.html_for(Some("item"), t.var("items"), |t| t.text("."))
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(items@b0: Array[String]) {
                  let v1: Array[String] = b0
                  let v3: Html = for b1: String in v1 {
                    let v2: Html = text(".")
                    v2
                  }
                  v3
                }

                -- after --
                page Test(items@b0: Array[String]) {
                  let v1: Array[String] = b0
                  let v3: Html = for _ in v1 {
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
            PureModuleBuilder::new()
                .page("Test", [("name", "Option[String]")], |t| {
                    t.option_match_expr_with_binding(
                        t.var("name"),
                        "n",
                        |t| t.text("some"),
                        t.text("none"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(name@b0: Option[String]) {
                  let v1: Option[String] = b0
                  let v4: Html = match v1 {
                    Some(b1: String) => {
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
                page Test(name@b0: Option[String]) {
                  let v1: Option[String] = b0
                  let v4: Html = match v1 {
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
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Shape",
                    [
                        ("Dot", vec![]),
                        ("Rect", vec![("width", "Int"), ("height", "Int")]),
                    ],
                )
                .function("width", [("shape", "Shape")], "Int", |t| {
                    t.enum_match_expr(t.var("shape"), |arms| {
                        arms.arm("Dot", |t| t.int(0));
                        arms.arm_bound("Rect", [("width", "w"), ("height", "h")], |t| t.var("w"));
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                fn width@f0(shape@b0: Shape) -> Int {
                  let v0: Shape = b0
                  let v3: Int = match v0 {
                    Shape::Dot => {
                      let v1: Int = 0
                      v1
                    }
                    Shape::Rect {width@b1: Int, height@b2: Int} => {
                      let v2: Int = b1
                      v2
                    }
                  }
                  v3
                }

                -- after --
                fn width@f0(shape@b0: Shape) -> Int {
                  let v0: Shape = b0
                  let v3: Int = match v0 {
                    Shape::Dot => {
                      let v1: Int = 0
                      v1
                    }
                    Shape::Rect {width@b1: Int} => {
                      let v2: Int = b1
                      v2
                    }
                  }
                  v3
                }
            "#]],
        );
    }
}
