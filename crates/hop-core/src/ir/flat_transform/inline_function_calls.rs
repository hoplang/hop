use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dependency_graph::DependencyGraph;
use crate::ir::flat_program::{
    FlatBinding, FlatBlock, FlatEntrypoint, FlatFor, FlatFunction, FlatOp, FlatProgram,
};
use crate::ir::flat_transform::callees::collect_callees;
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::function_id::FunctionId;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// A pass that replaces a call to a non-recursive function with the
/// callee's body, and drops the function once no call to it is left. Only
/// the functions in a call cycle and the entrypoints remain.
///
/// A call's arguments are names, computed before the call, so a parameter
/// is bound by reading the argument's name in its place. The body is copied
/// with fresh names, its bindings join the block the call sits in, and
/// every read of the call's name becomes a read of the body's result. A
/// call is transparent for state, and each copy has names of its own, so a
/// cell or an element of the copy is identified from one invocation to the
/// next exactly as the one of the callee at the call was.
pub fn inline_function_calls(program: FlatProgram) -> FlatProgram {
    let FlatProgram {
        types,
        functions,
        entrypoints,
        mut value_ids,
    } = program;

    let mut graph: DependencyGraph<FunctionId> = DependencyGraph::new();
    for function in &functions {
        let mut callees = BTreeSet::new();
        collect_callees(&function.body, &mut callees);
        graph.set_dependencies(function.function.id, callees);
    }

    let sccs = graph.sorted_sccs();
    let recursive: HashSet<FunctionId> = sccs
        .iter()
        .filter(|scc| scc.len() > 1 || scc.iter().any(|name| graph.depends_on(name, name)))
        .flatten()
        .cloned()
        .collect();

    // Declaration order is part of the program's identity, so keep it.
    let order: Vec<FunctionId> = functions.iter().map(|f| f.function.id).collect();
    let mut decls: HashMap<FunctionId, FlatFunction> =
        functions.into_iter().map(|f| (f.function.id, f)).collect();

    // The name each inlined call's result is read through. One map serves
    // every body, since names are unique in the program.
    let mut renames = HashMap::new();

    // sorted_sccs puts dependencies before dependents, which for a call graph
    // means every callee is inlined before the callers that copy it.
    for id in sccs.into_iter().flatten() {
        // Taking the declaration out while its own body is inlined keeps the
        // map borrow-free, and means a self-call finds nothing to inline.
        let Some(decl) = decls.remove(&id) else {
            continue;
        };
        let mut inliner = Inliner {
            decls: &decls,
            recursive: &recursive,
            value_ids: &mut value_ids,
            renames: &mut renames,
        };
        let decl = FlatFunction {
            body: inliner.block(decl.body),
            ..decl
        };
        decls.insert(id, decl);
    }

    let entrypoints = entrypoints
        .into_iter()
        .map(|entrypoint| {
            let mut inliner = Inliner {
                decls: &decls,
                recursive: &recursive,
                value_ids: &mut value_ids,
                renames: &mut renames,
            };
            FlatEntrypoint {
                body: inliner.block(entrypoint.body),
                ..entrypoint
            }
        })
        .collect();

    // Every call to a function outside a cycle was replaced by its body, so
    // nothing calls such a function any more. Entrypoints are kept separately.
    let functions = order
        .into_iter()
        .map(|id| decls.remove(&id).expect("each function is declared once"))
        .filter(|decl| recursive.contains(&decl.function.id))
        .collect();

    FlatProgram {
        types,
        functions,
        entrypoints,
        value_ids,
    }
}

/// Inlines the calls of the blocks of one body, and rewrites their reads
/// through `renames`, extended with the result of every call it inlines.
struct Inliner<'a> {
    decls: &'a HashMap<FunctionId, FlatFunction>,
    recursive: &'a HashSet<FunctionId>,
    value_ids: &'a mut ValueIdCounter,
    renames: &'a mut HashMap<ValueId, ValueId>,
}

impl<'a> Inliner<'a> {
    fn rename(&self, name: ValueId) -> ValueId {
        self.renames.get(&name).copied().unwrap_or(name)
    }

    /// The declaration of the callee when the call is inlined, which it is
    /// unless the callee is recursive or is the function being inlined.
    fn inlined(&self, function: &IrFunction) -> Option<&'a FlatFunction> {
        let decls = self.decls;
        decls
            .get(&function.id)
            .filter(|_| !self.recursive.contains(&function.id))
    }

    /// A freshener that reads each argument of the call for the parameter
    /// it is passed to.
    fn freshener(&mut self, decl: &FlatFunction, args: Vec<ValueId>) -> Freshener<'_> {
        assert_eq!(
            args.len(),
            decl.parameters.len(),
            "call to {} supplies one argument per parameter",
            decl.function
        );
        let arguments = decl
            .parameters
            .iter()
            .zip(args)
            .map(|(param, arg)| (param.binder.var, arg))
            .collect();
        Freshener::new(self.value_ids, arguments)
    }

    /// The bindings of an inlined call take its place in the block.
    fn block(&mut self, block: FlatBlock) -> FlatBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.binding(binding, &mut bindings);
        }
        IrBlock {
            bindings,
            result: self.rename(block.result),
        }
    }

    fn binding(&mut self, binding: FlatBinding, out: &mut Vec<FlatBinding>) {
        let IrBinding { binder, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.rename(*operand));
        let op = match op {
            FlatOp::Call(function, args) => match self.inlined(&function) {
                Some(decl) => {
                    let body = self.freshener(decl, args).freshen_block(&decl.body);
                    out.extend(body.bindings);
                    // The result is a fresh name or an argument, and neither
                    // is ever renamed, so no read goes through two renames.
                    self.renames.insert(binder.var, body.result);
                    return;
                }
                None => FlatOp::Call(function, args),
            },
            FlatOp::Match(match_) => FlatOp::Match(IrMatch {
                subject: match_.subject,
                arms: match_
                    .arms
                    .into_iter()
                    .map(|arm| IrMatchArm {
                        pattern: arm.pattern,
                        body: self.block(arm.body),
                    })
                    .collect(),
            }),
            FlatOp::For(FlatFor {
                var,
                array,
                key,
                body,
            }) => FlatOp::For(FlatFor {
                var,
                array,
                key: key.map(|key| self.block(key)),
                body: self.block(body),
            }),
            FlatOp::Lambda(IrLambda { parameters, body }) => FlatOp::Lambda(IrLambda {
                parameters,
                body: self.block(body),
            }),
            FlatOp::Cell(initial) => FlatOp::Cell(self.block(initial)),
            op => op,
        };
        out.push(IrBinding { binder, op });
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::hop::typing::{ComparableType, NumericType, Ty};
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::flat_type::FlatType;
    use crate::ir::flat_type::IrModifier;
    use crate::ir::flat_typecheck;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use expect_test::{Expect, expect};

    #[test]
    fn fuzz_random_programs_evaluate_identically_after_inlining() {
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

            let program = inline_function_calls(program);
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
        let after = inline_function_calls(program).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_inline_a_call_that_passes_a_closure_and_copy_the_lambdas_of_the_callee() {
        check(
            FlatProgramBuilder::new()
                .function(
                    "Twice",
                    [("f", Ty::function([Ty::Int], Ty::Int)), ("x", Ty::Int)],
                    Ty::Int,
                    |t| {
                        let f = t.param("f");
                        let again = t.lambda([FlatType::INT], |t, params| t.apply(f, [params[0]]));
                        t.apply(again, [t.apply(f, [t.param("x")])])
                    },
                )
                .entrypoint("Test", [("n", Ty::Int)], |t| {
                    let n = t.param("n");
                    let add = t.lambda([FlatType::INT], |t, params| {
                        t.add(NumericType::Int, params[0], n)
                    });
                    let twice = t.call("Twice", [add, t.int(1)]);
                    t.escape(t.int_to_string(twice))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn Twice@f0(
                  f@v0: (Int) -> Int,
                  x@v1: Int,
                ) -> Int {
                  let v4: (Int) -> Int = fn(
                    v2: Int,
                  ) {
                    let v3: Int = v0(v2)
                    v3
                  }
                  let v5: Int = v0(v1)
                  let v6: Int = v4(v5)
                  v6
                }
                entrypoint Test@e0(
                  n@v7: Int,
                ) -> Html {
                  let v10: (Int) -> Int = fn(
                    v8: Int,
                  ) {
                    let v9: Int = int_add(v8, v7)
                    v9
                  }
                  let v11: Int = 1
                  let v12: Int = f0(v10, v11)
                  let v13: String = int_to_string(v12)
                  let v14: Html = escape(v13)
                  v14
                }

                -- after --
                entrypoint Test@e0(
                  n@v7: Int,
                ) -> Html {
                  let v10: (Int) -> Int = fn(
                    v8: Int,
                  ) {
                    let v9: Int = int_add(v8, v7)
                    v9
                  }
                  let v11: Int = 1
                  let v17: (Int) -> Int = fn(
                    v15: Int,
                  ) {
                    let v16: Int = v10(v15)
                    v16
                  }
                  let v18: Int = v10(v11)
                  let v19: Int = v17(v18)
                  let v13: String = int_to_string(v19)
                  let v14: Html = escape(v13)
                  v14
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_a_call_and_read_the_argument_where_the_parameter_was_read() {
        check(
            FlatProgramBuilder::new()
                .function("double", [("x", Ty::Int)], Ty::Int, |t| {
                    t.add(NumericType::Int, t.param("x"), t.param("x"))
                })
                .entrypoint("Test", [("n", Ty::Int)], |t| {
                    t.escape(t.int_to_string(t.call("double", [t.param("n")])))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn double@f0(
                  x@v0: Int,
                ) -> Int {
                  let v1: Int = int_add(v0, v0)
                  v1
                }
                entrypoint Test@e0(
                  n@v2: Int,
                ) -> Html {
                  let v3: Int = f0(v2)
                  let v4: String = int_to_string(v3)
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint Test@e0(
                  n@v2: Int,
                ) -> Html {
                  let v6: Int = int_add(v2, v2)
                  let v4: String = int_to_string(v6)
                  let v5: Html = escape(v4)
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_callees_before_their_callers() {
        check(
            FlatProgramBuilder::new()
                .function("inner", [("x", Ty::Int)], Ty::Int, |t| {
                    t.add(NumericType::Int, t.param("x"), t.int(1))
                })
                .function("outer", [("y", Ty::Int)], Ty::Int, |t| {
                    t.mul(NumericType::Int, t.call("inner", [t.param("y")]), t.int(2))
                })
                .entrypoint("Test", [("n", Ty::Int)], |t| {
                    t.escape(t.int_to_string(t.call("outer", [t.param("n")])))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn inner@f0(
                  x@v0: Int,
                ) -> Int {
                  let v1: Int = 1
                  let v2: Int = int_add(v0, v1)
                  v2
                }
                fn outer@f1(
                  y@v3: Int,
                ) -> Int {
                  let v4: Int = f0(v3)
                  let v5: Int = 2
                  let v6: Int = int_mul(v4, v5)
                  v6
                }
                entrypoint Test@e0(
                  n@v7: Int,
                ) -> Html {
                  let v8: Int = f1(v7)
                  let v9: String = int_to_string(v8)
                  let v10: Html = escape(v9)
                  v10
                }

                -- after --
                entrypoint Test@e0(
                  n@v7: Int,
                ) -> Html {
                  let v13: Int = 1
                  let v14: Int = int_add(v7, v13)
                  let v15: Int = 2
                  let v16: Int = int_mul(v14, v15)
                  let v9: String = int_to_string(v16)
                  let v10: Html = escape(v9)
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_give_each_copy_of_a_loop_fresh_names() {
        check(
            FlatProgramBuilder::new()
                .render_function("list", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    t.html_for(t.param("items"), |t, item| {
                        t.element(t.mint(), "li", [], Some(t.escape(item)))
                    })
                })
                .entrypoint(
                    "Test",
                    [("a", Ty::array(Ty::String)), ("b", Ty::array(Ty::String))],
                    |t| {
                        t.html_concat([
                            t.call("list", vec![t.param("a")]),
                            t.call("list", vec![t.param("b")]),
                        ])
                    },
                )
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                render fn list@f0(
                  items@v0: Array[String],
                ) -> Html {
                  let v5: Html = for v1: String in v0 {
                    let v2: ElementId = mint()
                    let v3: Html = escape(v1)
                    let v4: Html = element(v2, "li", {}, v3)
                    v4
                  }
                  v5
                }
                entrypoint Test@e0(
                  a@v6: Array[String],
                  b@v7: Array[String],
                ) -> Html {
                  let v8: Html = f0(v6)
                  let v9: Html = f0(v7)
                  let v10: Html = concat(v8, v9)
                  v10
                }

                -- after --
                entrypoint Test@e0(
                  a@v6: Array[String],
                  b@v7: Array[String],
                ) -> Html {
                  let v15: Html = for v11: String in v6 {
                    let v12: ElementId = mint()
                    let v13: Html = escape(v11)
                    let v14: Html = element(v12, "li", {}, v13)
                    v14
                  }
                  let v20: Html = for v16: String in v7 {
                    let v17: ElementId = mint()
                    let v18: Html = escape(v16)
                    let v19: Html = element(v17, "li", {}, v18)
                    v19
                  }
                  let v10: Html = concat(v15, v20)
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_give_each_copy_of_a_cell_a_fresh_cell_that_its_writes_write() {
        check(
            FlatProgramBuilder::new()
                .render_function("counter", [], Ty::Html, |t| {
                    let n = t.cell(|t| t.int(0));
                    let count = t.cell_read(n);
                    let bump = t.event_handler(|t| {
                        t.cell_write(n, t.add(NumericType::Int, count, t.int(1)))
                    });
                    t.element_on_click(
                        t.mint(),
                        "button",
                        [],
                        bump,
                        Some(t.escape(t.int_to_string(count))),
                    )
                })
                .entrypoint("Test", [], |t| {
                    t.html_concat([t.call("counter", []), t.call("counter", [])])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                render fn counter@f0() -> Html {
                  let v1: Cell[Int] = cell {
                    let v0: Int = 0
                    v0
                  }
                  let v2: Int = cell_read(v1)
                  let v6: mut () -> Tuple[] = fn() {
                    let v3: Int = 1
                    let v4: Int = int_add(v2, v3)
                    let v5: Tuple[] = cell_write(v1, v4)
                    v5
                  }
                  let v7: ElementId = mint()
                  let v8: String = int_to_string(v2)
                  let v9: Html = escape(v8)
                  let v10: Html = element(v7, "button", {}, v9) on_click v6
                  v10
                }
                entrypoint Test@e0() -> Html {
                  let v11: Html = f0()
                  let v12: Html = f0()
                  let v13: Html = concat(v11, v12)
                  v13
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v15: Cell[Int] = cell {
                    let v14: Int = 0
                    v14
                  }
                  let v16: Int = cell_read(v15)
                  let v20: mut () -> Tuple[] = fn() {
                    let v17: Int = 1
                    let v18: Int = int_add(v16, v17)
                    let v19: Tuple[] = cell_write(v15, v18)
                    v19
                  }
                  let v21: ElementId = mint()
                  let v22: String = int_to_string(v16)
                  let v23: Html = escape(v22)
                  let v24: Html = element(v21, "button", {}, v23) on_click v20
                  let v26: Cell[Int] = cell {
                    let v25: Int = 0
                    v25
                  }
                  let v27: Int = cell_read(v26)
                  let v31: mut () -> Tuple[] = fn() {
                    let v28: Int = 1
                    let v29: Int = int_add(v27, v28)
                    let v30: Tuple[] = cell_write(v26, v29)
                    v30
                  }
                  let v32: ElementId = mint()
                  let v33: String = int_to_string(v27)
                  let v34: Html = escape(v33)
                  let v35: Html = element(v32, "button", {}, v34) on_click v31
                  let v13: Html = concat(v24, v35)
                  v13
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_a_function_that_mutates_into_a_handler() {
        check(
            FlatProgramBuilder::new()
                .mut_function(
                    "both",
                    [
                        ("first", Ty::mut_function([], Ty::tuple([]))),
                        ("second", Ty::mut_function([], Ty::tuple([]))),
                    ],
                    Ty::tuple([]),
                    |t| {
                        t.apply(t.param("first"), []);
                        t.apply(t.param("second"), [])
                    },
                )
                .entrypoint("Test", [], |t| {
                    let count = t.cell(|t| t.int(0));
                    let on = t.cell(|t| t.bool(false));
                    let n = t.cell_read(count);
                    let is_on = t.cell_read(on);
                    let both = t.event_handler(|t| {
                        let bump = t.mut_lambda([], |t, _| {
                            t.cell_write(count, t.add(NumericType::Int, n, t.int(1)))
                        });
                        let flip = t.mut_lambda([], |t, _| t.cell_write(on, t.not(is_on)));
                        t.call("both", [bump, flip])
                    });
                    t.element_on_click(
                        t.mint(),
                        "button",
                        [],
                        both,
                        Some(t.escape(t.int_to_string(n))),
                    )
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                mut fn both@f0(
                  first@v0: mut () -> Tuple[],
                  second@v1: mut () -> Tuple[],
                ) -> Tuple[] {
                  let v2: Tuple[] = v0()
                  let v3: Tuple[] = v1()
                  v3
                }
                entrypoint Test@e0() -> Html {
                  let v5: Cell[Int] = cell {
                    let v4: Int = 0
                    v4
                  }
                  let v7: Cell[Bool] = cell {
                    let v6: Bool = false
                    v6
                  }
                  let v8: Int = cell_read(v5)
                  let v9: Bool = cell_read(v7)
                  let v18: mut () -> Tuple[] = fn() {
                    let v13: mut () -> Tuple[] = fn() {
                      let v10: Int = 1
                      let v11: Int = int_add(v8, v10)
                      let v12: Tuple[] = cell_write(v5, v11)
                      v12
                    }
                    let v16: mut () -> Tuple[] = fn() {
                      let v14: Bool = bool_not(v9)
                      let v15: Tuple[] = cell_write(v7, v14)
                      v15
                    }
                    let v17: Tuple[] = f0(v13, v16)
                    v17
                  }
                  let v19: ElementId = mint()
                  let v20: String = int_to_string(v8)
                  let v21: Html = escape(v20)
                  let v22: Html = element(v19, "button", {}, v21) on_click v18
                  v22
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v5: Cell[Int] = cell {
                    let v4: Int = 0
                    v4
                  }
                  let v7: Cell[Bool] = cell {
                    let v6: Bool = false
                    v6
                  }
                  let v8: Int = cell_read(v5)
                  let v9: Bool = cell_read(v7)
                  let v18: mut () -> Tuple[] = fn() {
                    let v13: mut () -> Tuple[] = fn() {
                      let v10: Int = 1
                      let v11: Int = int_add(v8, v10)
                      let v12: Tuple[] = cell_write(v5, v11)
                      v12
                    }
                    let v16: mut () -> Tuple[] = fn() {
                      let v14: Bool = bool_not(v9)
                      let v15: Tuple[] = cell_write(v7, v14)
                      v15
                    }
                    let v23: Tuple[] = v13()
                    let v24: Tuple[] = v16()
                    v24
                  }
                  let v19: ElementId = mint()
                  let v20: String = int_to_string(v8)
                  let v21: Html = escape(v20)
                  let v22: Html = element(v19, "button", {}, v21) on_click v18
                  v22
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_a_self_recursive_function_alone() {
        check(
            FlatProgramBuilder::new()
                .render_function("count", [("n", Ty::Int)], Ty::Html, |t| {
                    t.match_(t.lt(ComparableType::Int, t.param("n"), t.int(1)), |m| {
                        m.arm(Pat::True, |t, _| t.text("."));
                        m.arm(Pat::False, |t, _| {
                            t.call("count", [t.sub(NumericType::Int, t.param("n"), t.int(1))])
                        });
                    })
                })
                .entrypoint("Test", [], |t| t.call("count", [t.int(3)]))
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                render fn count@f0(
                  n@v0: Int,
                ) -> Html {
                  let v1: Int = 1
                  let v2: Bool = int_lt(v0, v1)
                  let v7: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = int_sub(v0, v4)
                      let v6: Html = f0(v5)
                      v6
                    }
                  }
                  v7
                }
                entrypoint Test@e0() -> Html {
                  let v8: Int = 3
                  let v9: Html = f0(v8)
                  v9
                }

                -- after --
                render fn count@f0(
                  n@v0: Int,
                ) -> Html {
                  let v1: Int = 1
                  let v2: Bool = int_lt(v0, v1)
                  let v7: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = int_sub(v0, v4)
                      let v6: Html = f0(v5)
                      v6
                    }
                  }
                  v7
                }
                entrypoint Test@e0() -> Html {
                  let v8: Int = 3
                  let v9: Html = f0(v8)
                  v9
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_mutually_recursive_functions_alone() {
        check(
            FlatProgramBuilder::new()
                .declare(IrModifier::Render, "ping", [("n", Ty::Int)], Ty::Html)
                .declare(IrModifier::Render, "pong", [("n", Ty::Int)], Ty::Html)
                .define("ping", |t, _| {
                    t.match_(t.lt(ComparableType::Int, t.param("n"), t.int(1)), |m| {
                        m.arm(Pat::True, |t, _| t.text("."));
                        m.arm(Pat::False, |t, _| {
                            t.call("pong", [t.sub(NumericType::Int, t.param("n"), t.int(1))])
                        });
                    })
                })
                .define("pong", |t, _| t.call("ping", [t.param("n")]))
                .entrypoint("Test", [], |t| t.call("ping", [t.int(2)]))
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                render fn ping@f0(
                  n@v0: Int,
                ) -> Html {
                  let v2: Int = 1
                  let v3: Bool = int_lt(v0, v2)
                  let v8: Html = match v3 {
                    true => {
                      let v4: Html = text(".")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = int_sub(v0, v5)
                      let v7: Html = f1(v6)
                      v7
                    }
                  }
                  v8
                }
                render fn pong@f1(
                  n@v1: Int,
                ) -> Html {
                  let v9: Html = f0(v1)
                  v9
                }
                entrypoint Test@e0() -> Html {
                  let v10: Int = 2
                  let v11: Html = f0(v10)
                  v11
                }

                -- after --
                render fn ping@f0(
                  n@v0: Int,
                ) -> Html {
                  let v2: Int = 1
                  let v3: Bool = int_lt(v0, v2)
                  let v8: Html = match v3 {
                    true => {
                      let v4: Html = text(".")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = int_sub(v0, v5)
                      let v7: Html = f1(v6)
                      v7
                    }
                  }
                  v8
                }
                render fn pong@f1(
                  n@v1: Int,
                ) -> Html {
                  let v9: Html = f0(v1)
                  v9
                }
                entrypoint Test@e0() -> Html {
                  let v10: Int = 2
                  let v11: Html = f0(v10)
                  v11
                }
            "#]],
        );
    }
}
