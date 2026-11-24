use std::collections::HashSet;

use crate::hop::typing::Pattern;
use crate::ir::flat_program::{
    FlatBinding, FlatBlock, FlatEntrypoint, FlatFor, FlatFunction, FlatLambda, FlatMatch,
    FlatMatchArm, FlatOp, FlatProgram,
};
use crate::ir::flat_type::IrModifier;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::value_id::ValueId;

/// A pass that removes the bindings nothing reads from every body.
///
/// Every op outside of a body that mutates is pure, so a binding there
/// whose name no later binding, nested block or result reads computes
/// nothing anyone sees, and goes away with the blocks nested in it. A body
/// that mutates keeps every binding. A cell nothing reads or updates has a value
/// nobody sees, and an element whose Html nothing reads is never shown, so
/// the state of its node is never seen. A click reads its handler, a cell
/// update the cell and the closure it updates it with, and a handler nothing applies
/// goes away. A loop variable or binding of a
/// pattern that nothing reads any more goes away too, and becomes `_`.
pub fn eliminate_dead_bindings(program: FlatProgram) -> FlatProgram {
    let functions = program
        .functions
        .into_iter()
        .map(|function| FlatFunction {
            body: eliminate_block(
                function.body,
                &mut HashSet::new(),
                function.modifier == IrModifier::Mut,
            ),
            ..function
        })
        .collect();
    FlatProgram {
        entrypoints: program
            .entrypoints
            .into_iter()
            .map(|entrypoint| FlatEntrypoint {
                body: eliminate_block(entrypoint.body, &mut HashSet::new(), false),
                ..entrypoint
            })
            .collect(),
        lambdas: program
            .lambdas
            .into_iter()
            .map(|lambda| FlatLambda {
                body: eliminate_block(
                    lambda.body,
                    &mut HashSet::new(),
                    lambda.modifier == IrModifier::Mut,
                ),
                ..lambda
            })
            .collect(),
        types: program.types,
        functions,
        value_ids: program.value_ids,
    }
}

fn eliminate_block(block: FlatBlock, live: &mut HashSet<ValueId>, keep: bool) -> FlatBlock {
    live.insert(block.result);
    let mut kept = Vec::with_capacity(block.bindings.len());
    for binding in block.bindings.into_iter().rev() {
        kept.extend(eliminate_binding(binding, live, keep));
    }
    kept.reverse();
    IrBlock {
        bindings: kept,
        result: block.result,
    }
}

fn eliminate_binding(
    binding: FlatBinding,
    live: &mut HashSet<ValueId>,
    keep: bool,
) -> Option<FlatBinding> {
    if !keep && !live.contains(&binding.binder.var) {
        return None;
    }
    let IrBinding { binder, op } = binding;
    let op = match op {
        FlatOp::Match(match_) => FlatOp::Match(eliminate_match(match_, live, keep)),
        FlatOp::For(FlatFor {
            var,
            array,
            key,
            body,
        }) => {
            let body = eliminate_block(body, live, keep);
            let key = key.map(|key| eliminate_block(key, live, keep));
            let var = var.filter(|binder| live.contains(&binder.var));
            FlatOp::For(FlatFor {
                var,
                array,
                key,
                body,
            })
        }
        FlatOp::Cell(initial) => FlatOp::Cell(eliminate_block(initial, live, keep)),
        op => op,
    };
    op.for_each_operand(&mut |operand| {
        live.insert(operand);
    });
    Some(IrBinding { binder, op })
}

fn eliminate_match(match_: FlatMatch, live: &mut HashSet<ValueId>, keep: bool) -> FlatMatch {
    FlatMatch {
        subject: match_.subject,
        arms: match_
            .arms
            .into_iter()
            .map(|arm| {
                let body = eliminate_block(arm.body, live, keep);
                let mut pattern = arm.pattern;
                pattern.for_each_binding_mut(&mut |binding| {
                    if let Pattern::Binding(binder) = binding
                        && !live.contains(&binder.var)
                    {
                        *binding = Pattern::Wildcard;
                    }
                });
                FlatMatchArm { pattern, body }
            })
            .collect(),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::hop::typing::{NumericType, Ty};
    use crate::ir::flat_program::FlatProgram;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::flat_type::FlatType;
    use crate::ir::flat_type_declarations_builder::FlatTypeDeclarationsBuilder;
    use crate::ir::flat_typecheck;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use expect_test::{Expect, expect};

    #[test]
    fn fuzz_random_programs_evaluate_identically_after_elimination() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            let entrypoint_args = random_entrypoint_args(&program, u)?;

            let before_program = program.clone();
            let before: Vec<Option<String>> = entrypoint_args
                .iter()
                .map(|(entrypoint, args)| {
                    match evaluate_entrypoint(&program, *entrypoint, args.clone()) {
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

            let program = eliminate_dead_bindings(program);
            flat_typecheck::typecheck(&program).unwrap();

            for ((entrypoint, args), before) in entrypoint_args.iter().zip(&before) {
                // An entrypoint that hit a limit has no output to preserve.
                let Some(before) = before else {
                    continue;
                };
                let after = evaluate_entrypoint(&program, *entrypoint, args.clone()).unwrap();
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
        let after = eliminate_dead_bindings(program).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_keep_a_cell_that_a_click_updates_and_the_closure_it_updates_it_with() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let n = t.cell(|t| t.int(0));
                    let one = t.lambda([FlatType::INT], |t, _| t.int(1));
                    let set = t.event_handler(|t| t.cell_update(n, one));
                    t.element_on_click(t.mint(), "button", [], set, Some(t.text("x")))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                lambda l0[](
                  v2: Int,
                ) -> Int {
                  let v3: Int = 1
                  v3
                }
                mut lambda l1[
                  v6: Cell[Int],
                  v7: (Int) -> Int,
                ]() -> Tuple[] {
                  let v5: Tuple[] = cell_update(v6, v7)
                  v5
                }
                entrypoint Test@e0() -> Html {
                  let v1: Cell[Int] = cell {
                    let v0: Int = 0
                    v0
                  }
                  let v4: (Int) -> Int = l0[]
                  let v8: mut () -> Tuple[] = l1[v1, v4]
                  let v9: ElementId = mint()
                  let v10: Html = text("x")
                  let v11: Html = element(v9, "button", {}, v10) on_click v8
                  v11
                }

                -- after --
                lambda l0[](
                  v2: Int,
                ) -> Int {
                  let v3: Int = 1
                  v3
                }
                mut lambda l1[
                  v6: Cell[Int],
                  v7: (Int) -> Int,
                ]() -> Tuple[] {
                  let v5: Tuple[] = cell_update(v6, v7)
                  v5
                }
                entrypoint Test@e0() -> Html {
                  let v1: Cell[Int] = cell {
                    let v0: Int = 0
                    v0
                  }
                  let v4: (Int) -> Int = l0[]
                  let v8: mut () -> Tuple[] = l1[v1, v4]
                  let v9: ElementId = mint()
                  let v10: Html = text("x")
                  let v11: Html = element(v9, "button", {}, v10) on_click v8
                  v11
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_a_closure_that_mutates_and_nothing_applies_and_keep_the_cell_a_click_updates() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let n = t.cell(|t| t.int(0));
                    let unseen = t.cell(|t| t.int(0));
                    let _idle = t.mut_lambda([], |t, _| {
                        let one = t.lambda([FlatType::INT], |t, _| t.int(1));
                        t.cell_update(unseen, one)
                    });
                    let bump = t.event_handler(|t| {
                        let incr = t.lambda([FlatType::INT], |t, params| {
                            t.add(NumericType::Int, params[0], t.int(1))
                        });
                        t.cell_update(n, incr)
                    });
                    t.element_on_click(t.mint(), "button", [], bump, Some(t.text("x")))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                lambda l0[](
                  v4: Int,
                ) -> Int {
                  let v5: Int = 1
                  v5
                }
                mut lambda l1[
                  v8: Cell[Int],
                ]() -> Tuple[] {
                  let v6: (Int) -> Int = l0[]
                  let v7: Tuple[] = cell_update(v8, v6)
                  v7
                }
                lambda l2[](
                  v10: Int,
                ) -> Int {
                  let v11: Int = 1
                  let v12: Int = int_add(v10, v11)
                  v12
                }
                mut lambda l3[
                  v15: Cell[Int],
                ]() -> Tuple[] {
                  let v13: (Int) -> Int = l2[]
                  let v14: Tuple[] = cell_update(v15, v13)
                  v14
                }
                entrypoint Test@e0() -> Html {
                  let v1: Cell[Int] = cell {
                    let v0: Int = 0
                    v0
                  }
                  let v3: Cell[Int] = cell {
                    let v2: Int = 0
                    v2
                  }
                  let v9: mut () -> Tuple[] = l1[v3]
                  let v16: mut () -> Tuple[] = l3[v1]
                  let v17: ElementId = mint()
                  let v18: Html = text("x")
                  let v19: Html = element(v17, "button", {}, v18) on_click v16
                  v19
                }

                -- after --
                lambda l0[](
                  v4: Int,
                ) -> Int {
                  let v5: Int = 1
                  v5
                }
                mut lambda l1[
                  v8: Cell[Int],
                ]() -> Tuple[] {
                  let v6: (Int) -> Int = l0[]
                  let v7: Tuple[] = cell_update(v8, v6)
                  v7
                }
                lambda l2[](
                  v10: Int,
                ) -> Int {
                  let v11: Int = 1
                  let v12: Int = int_add(v10, v11)
                  v12
                }
                mut lambda l3[
                  v15: Cell[Int],
                ]() -> Tuple[] {
                  let v13: (Int) -> Int = l2[]
                  let v14: Tuple[] = cell_update(v15, v13)
                  v14
                }
                entrypoint Test@e0() -> Html {
                  let v1: Cell[Int] = cell {
                    let v0: Int = 0
                    v0
                  }
                  let v16: mut () -> Tuple[] = l3[v1]
                  let v17: ElementId = mint()
                  let v18: Html = text("x")
                  let v19: Html = element(v17, "button", {}, v18) on_click v16
                  v19
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_a_value_nothing_reads() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let _unused = t.str("value");
                    t.text("Hello")
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "value"
                  let v1: Html = text("Hello")
                  v1
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v1: Html = text("Hello")
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_a_dead_match_with_its_arms() {
        check(
            FlatProgramBuilder::new()
                .function("f", [], Ty::Int, |t| t.call("f", []))
                .entrypoint("Test", [("flag", Ty::Bool)], |t| {
                    let _unused = t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.call("f", []));
                        m.arm(Pat::False, |t, _| t.int(0));
                    });
                    t.text("Hello")
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn f@f0() -> Int {
                  let v0: Int = f0()
                  v0
                }
                entrypoint Test@e0(
                  flag@v1: Bool,
                ) -> Html {
                  let v4: Int = match v1 {
                    true => {
                      let v2: Int = f0()
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
                  let v0: Int = f0()
                  v0
                }
                entrypoint Test@e0(
                  flag@v1: Bool,
                ) -> Html {
                  let v5: Html = text("Hello")
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_unbind_an_unused_loop_variable() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                    t.html_for(t.param("items"), |t, _| t.text("."))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(
                  items@v0: Array[String],
                ) -> Html {
                  let v3: Html = for v1: String in v0 {
                    let v2: Html = text(".")
                    v2
                  }
                  v3
                }

                -- after --
                entrypoint Test@e0(
                  items@v0: Array[String],
                ) -> Html {
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
            FlatProgramBuilder::new()
                .entrypoint("Test", [("name", Ty::option(Ty::String))], |t| {
                    t.match_(t.param("name"), |m| {
                        m.arm(Pat::some(Pat::bind("n")), |t, _| t.text("some"));
                        m.arm(Pat::None, |t, _| t.text("none"));
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(
                  name@v0: Option[String],
                ) -> Html {
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
                entrypoint Test@e0(
                  name@v0: Option[String],
                ) -> Html {
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
        let types = FlatTypeDeclarationsBuilder::new()
            .enum_(
                "Shape",
                [
                    ("Dot", vec![]),
                    ("Rect", vec![("width", Ty::Int), ("height", Ty::Int)]),
                ],
            )
            .build();
        check(
            FlatProgramBuilder::with_types(types)
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
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn width@f0(
                  shape@v0: Shape,
                ) -> Int {
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
                fn width@f0(
                  shape@v0: Shape,
                ) -> Int {
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
