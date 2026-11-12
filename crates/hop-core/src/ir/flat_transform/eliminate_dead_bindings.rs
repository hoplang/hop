use std::collections::HashSet;

use crate::hop::typing::Pattern;
use crate::ir::flat_program::{
    FlatEntrypoint, FlatFor, FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatProgram,
    FlatPureBlock, FlatPureOp,
};
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::ir_type::{IrFunctionDataType, IrFunctionHtmlType};
use crate::ir::value_id::ValueId;

/// A pass that removes the bindings nothing reads from every body.
///
/// Every op is pure, so a binding whose name no later binding, nested
/// block or result reads computes nothing anyone sees, and goes away with
/// the blocks nested in it. A cell nothing reads or writes has a value
/// nobody sees, and an element whose Html nothing reads is never shown, so
/// the state of its node is never seen. A click reads its handler, a cell
/// write the cell and the value it writes, and an effect or a handler
/// nothing performs goes away. A loop variable or binding of a
/// pattern that nothing reads any more goes away too, and becomes `_`.
pub fn eliminate_dead_bindings(program: FlatProgram) -> FlatProgram {
    let functions = program
        .functions
        .into_iter()
        .map(|function| match function {
            FlatFunction::Pure(function) => FlatFunction::Pure(IrFunctionDeclaration {
                body: eliminate_pure(function.body, &mut HashSet::new()),
                ..function
            }),
            FlatFunction::Html(function) => FlatFunction::Html(IrFunctionDeclaration {
                body: eliminate_html(function.body, &mut HashSet::new()),
                ..function
            }),
        })
        .collect();
    FlatProgram {
        entrypoints: program
            .entrypoints
            .into_iter()
            .map(|entrypoint| FlatEntrypoint {
                body: eliminate_html(entrypoint.body, &mut HashSet::new()),
                ..entrypoint
            })
            .collect(),
        types: program.types,
        functions,
        value_ids: program.value_ids,
    }
}

fn eliminate_pure(block: FlatPureBlock, live: &mut HashSet<ValueId>) -> FlatPureBlock {
    live.insert(block.result);
    let mut kept = Vec::with_capacity(block.bindings.len());
    for binding in block.bindings.into_iter().rev() {
        kept.extend(eliminate_pure_binding(binding, live));
    }
    kept.reverse();
    IrBlock {
        bindings: kept,
        result: block.result,
    }
}

fn eliminate_html(block: FlatHtmlBlock, live: &mut HashSet<ValueId>) -> FlatHtmlBlock {
    live.insert(block.result);
    let mut kept = Vec::with_capacity(block.bindings.len());
    for binding in block.bindings.into_iter().rev() {
        let binding = match binding {
            FlatHtmlBinding::Pure(binding) => {
                eliminate_pure_binding(binding, live).map(FlatHtmlBinding::Pure)
            }
            FlatHtmlBinding::Cell(binder, initial) => {
                if !live.contains(&binder.var) {
                    continue;
                }
                live.insert(initial);
                Some(FlatHtmlBinding::Cell(binder, initial))
            }
            FlatHtmlBinding::Track(binder, cell) => {
                if !live.contains(&binder.var) {
                    continue;
                }
                live.insert(cell);
                Some(FlatHtmlBinding::Track(binder, cell))
            }
            FlatHtmlBinding::Html(binding) => {
                eliminate_html_binding(binding, live).map(FlatHtmlBinding::Html)
            }
        };
        kept.extend(binding);
    }
    kept.reverse();
    IrBlock {
        bindings: kept,
        result: block.result,
    }
}

fn eliminate_pure_binding(
    binding: IrBinding<IrFunctionDataType, FlatPureOp>,
    live: &mut HashSet<ValueId>,
) -> Option<IrBinding<IrFunctionDataType, FlatPureOp>> {
    if !live.contains(&binding.binder.var) {
        return None;
    }
    let IrBinding { binder, op } = binding;
    let op = match op {
        FlatPureOp::Match(match_) => {
            FlatPureOp::Match(eliminate_match(match_, live, eliminate_pure))
        }
        FlatPureOp::Lambda(IrLambda { parameters, body }) => FlatPureOp::Lambda(IrLambda {
            parameters,
            body: eliminate_pure(body, live),
        }),
        op => op,
    };
    op.for_each_operand(&mut |operand| {
        live.insert(operand);
    });
    Some(IrBinding { binder, op })
}

fn eliminate_html_binding(
    binding: IrBinding<IrFunctionHtmlType, FlatHtmlOp>,
    live: &mut HashSet<ValueId>,
) -> Option<IrBinding<IrFunctionHtmlType, FlatHtmlOp>> {
    if !live.contains(&binding.binder.var) {
        return None;
    }
    let IrBinding { binder, op } = binding;
    let op = match op {
        FlatHtmlOp::Match(match_) => {
            FlatHtmlOp::Match(eliminate_match(match_, live, eliminate_html))
        }
        FlatHtmlOp::For(FlatFor {
            var,
            array,
            key,
            body,
        }) => {
            let body = eliminate_html(body, live);
            let key = key.map(|key| eliminate_pure(key, live));
            let var = var.filter(|binder| live.contains(&binder.var));
            FlatHtmlOp::For(FlatFor {
                var,
                array,
                key,
                body,
            })
        }
        FlatHtmlOp::Lambda(IrLambda { parameters, body }) => FlatHtmlOp::Lambda(IrLambda {
            parameters,
            body: eliminate_html(body, live),
        }),
        op => op,
    };
    op.for_each_operand(&mut |operand| {
        live.insert(operand);
    });
    Some(IrBinding { binder, op })
}

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
    use crate::hop::typing::{NumericType, Ty};
    use crate::ir::flat_program::FlatProgram;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::flat_typecheck;
    use crate::ir::ir_type_declarations_builder::IrTypeDeclarationsBuilder;
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
    fn should_keep_a_cell_that_a_click_writes_and_nothing_reads() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let n = t.cell(t.int(0));
                    let set = t.event_handler(|t| t.cell_write(n, t.int(1)));
                    t.element_on_click("button", [], set, Some(t.text("x")))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Int = 0
                  let v1: Cell[Int] = cell(v0)
                  let v4: () -> Effect[Tuple[]] = fn() {
                    let v2: Int = 1
                    let v3: Effect[Tuple[]] = v1 := v2
                    v3
                  }
                  let v5: Html = text("x")
                  let v6: Html = element("button", {}, v5) on_click v4
                  v6
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: Int = 0
                  let v1: Cell[Int] = cell(v0)
                  let v4: () -> Effect[Tuple[]] = fn() {
                    let v2: Int = 1
                    let v3: Effect[Tuple[]] = v1 := v2
                    v3
                  }
                  let v5: Html = text("x")
                  let v6: Html = element("button", {}, v5) on_click v4
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_an_effect_nothing_performs_and_keep_the_cell_a_click_writes() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let n = t.cell(t.int(0));
                    let unseen = t.cell(t.int(0));
                    let _idle = t.cell_write(unseen, t.int(1));
                    let bump = t.cell_write(n, t.add(NumericType::Int, t.track(n), t.int(1)));
                    let bump = t.event_handler(|_| bump);
                    t.element_on_click("button", [], bump, Some(t.text("x")))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Int = 0
                  let v1: Cell[Int] = cell(v0)
                  let v2: Int = 0
                  let v3: Cell[Int] = cell(v2)
                  let v4: Int = 1
                  let v5: Effect[Tuple[]] = v3 := v4
                  let v6: Int = track(v1)
                  let v7: Int = 1
                  let v8: Int = add[Int](v6, v7)
                  let v9: Effect[Tuple[]] = v1 := v8
                  let v10: () -> Effect[Tuple[]] = fn() {
                    v9
                  }
                  let v11: Html = text("x")
                  let v12: Html = element("button", {}, v11) on_click v10
                  v12
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: Int = 0
                  let v1: Cell[Int] = cell(v0)
                  let v6: Int = track(v1)
                  let v7: Int = 1
                  let v8: Int = add[Int](v6, v7)
                  let v9: Effect[Tuple[]] = v1 := v8
                  let v10: () -> Effect[Tuple[]] = fn() {
                    v9
                  }
                  let v11: Html = text("x")
                  let v12: Html = element("button", {}, v11) on_click v10
                  v12
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
                  let v0: Int = f@f0()
                  v0
                }
                entrypoint Test@e0(flag@v1: Bool) -> Html {
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
                entrypoint Test@e0(flag@v1: Bool) -> Html {
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
                entrypoint Test@e0(items@v0: Array[String]) -> Html {
                  let v3: Html = for v1: String in v0 {
                    let v2: Html = text(".")
                    v2
                  }
                  v3
                }

                -- after --
                entrypoint Test@e0(items@v0: Array[String]) -> Html {
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
                entrypoint Test@e0(name@v0: Option[String]) -> Html {
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
                entrypoint Test@e0(name@v0: Option[String]) -> Html {
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
        let types = IrTypeDeclarationsBuilder::new()
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
