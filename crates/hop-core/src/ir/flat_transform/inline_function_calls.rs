use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dependency_graph::DependencyGraph;
use crate::ir::flat_program::{
    FlatEntrypoint, FlatFor, FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatProgram,
    FlatPureBlock, FlatPureOp,
};
use crate::ir::flat_transform::callees::{collect_html_callees, collect_pure_callees};
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::function_id::FunctionId;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::ir_type::{IrFunctionDataType, IrFunctionHtmlType};
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
        match function {
            FlatFunction::Pure(function) => {
                collect_pure_callees(&function.body, &mut callees);
            }
            FlatFunction::Html(function) => {
                collect_html_callees(&function.body, &mut callees);
            }
        }
        graph.set_dependencies(function.function().id, callees);
    }

    let sccs = graph.sorted_sccs();
    let recursive: HashSet<FunctionId> = sccs
        .iter()
        .filter(|scc| scc.len() > 1 || scc.iter().any(|name| graph.depends_on(name, name)))
        .flatten()
        .cloned()
        .collect();

    // Declaration order is part of the program's identity, so keep it.
    let order: Vec<FunctionId> = functions.iter().map(|f| f.function().id).collect();
    let mut decls: HashMap<FunctionId, FlatFunction> = functions
        .into_iter()
        .map(|f| (f.function().id, f))
        .collect();

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
        let decl = match decl {
            FlatFunction::Pure(function) => FlatFunction::Pure(IrFunctionDeclaration {
                body: inliner.pure_block(function.body),
                ..function
            }),
            FlatFunction::Html(function) => FlatFunction::Html(IrFunctionDeclaration {
                body: inliner.html_block(function.body),
                ..function
            }),
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
                body: inliner.html_block(entrypoint.body),
                ..entrypoint
            }
        })
        .collect();

    // Every call to a function outside a cycle was replaced by its body, so
    // nothing calls such a function any more. Entrypoints are kept separately.
    let functions = order
        .into_iter()
        .map(|id| decls.remove(&id).expect("each function is declared once"))
        .filter(|decl| recursive.contains(&decl.function().id))
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
            decl.parameters().len(),
            "call to {} supplies one argument per parameter",
            decl.function()
        );
        let arguments = decl
            .parameters()
            .iter()
            .zip(args)
            .map(|(param, arg)| (param.binder.var, arg))
            .collect();
        Freshener::new(self.value_ids, arguments)
    }

    fn pure_block(&mut self, block: FlatPureBlock) -> FlatPureBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.pure_binding(binding, &mut bindings);
        }
        IrBlock {
            bindings,
            result: self.rename(block.result),
        }
    }

    /// The bindings of an inlined call take its place in the block.
    fn html_block(&mut self, block: FlatHtmlBlock) -> FlatHtmlBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        let mut pure = Vec::new();
        for binding in block.bindings {
            match binding {
                FlatHtmlBinding::Pure(binding) => {
                    self.pure_binding(binding, &mut pure);
                    bindings.extend(pure.drain(..).map(FlatHtmlBinding::Pure));
                }
                FlatHtmlBinding::Cell(binder, initial) => {
                    bindings.push(FlatHtmlBinding::Cell(binder, self.rename(initial)));
                }
                FlatHtmlBinding::Track(binder, cell) => {
                    bindings.push(FlatHtmlBinding::Track(binder, self.rename(cell)));
                }
                FlatHtmlBinding::Html(binding) => self.html_binding(binding, &mut bindings),
            }
        }
        IrBlock {
            bindings,
            result: self.rename(block.result),
        }
    }

    fn match_<B>(
        &mut self,
        match_: IrMatch<B>,
        inline_block: impl Fn(&mut Self, B) -> B,
    ) -> IrMatch<B> {
        IrMatch {
            subject: match_.subject,
            arms: match_
                .arms
                .into_iter()
                .map(|arm| IrMatchArm {
                    pattern: arm.pattern,
                    body: inline_block(self, arm.body),
                })
                .collect(),
        }
    }

    fn pure_binding(
        &mut self,
        binding: IrBinding<IrFunctionDataType, FlatPureOp>,
        out: &mut Vec<IrBinding<IrFunctionDataType, FlatPureOp>>,
    ) {
        let IrBinding { binder, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.rename(*operand));
        let op = match op {
            FlatPureOp::Call(function, args) => match self.inlined(&function) {
                Some(decl) => {
                    let FlatFunction::Pure(callee) = decl else {
                        panic!("{function} is called for a value but returns Html");
                    };
                    let body = self.freshener(decl, args).freshen_pure_block(&callee.body);
                    out.extend(body.bindings);
                    // The result is a fresh name or an argument, and neither
                    // is ever renamed, so no read goes through two renames.
                    self.renames.insert(binder.var, body.result);
                    return;
                }
                None => FlatPureOp::Call(function, args),
            },
            FlatPureOp::Match(match_) => FlatPureOp::Match(self.match_(match_, Self::pure_block)),
            FlatPureOp::Lambda(IrLambda { parameters, body }) => FlatPureOp::Lambda(IrLambda {
                parameters,
                body: self.pure_block(body),
            }),
            op => op,
        };
        out.push(IrBinding { binder, op });
    }

    fn html_binding(
        &mut self,
        binding: IrBinding<IrFunctionHtmlType, FlatHtmlOp>,
        out: &mut Vec<FlatHtmlBinding>,
    ) {
        let IrBinding { binder, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.rename(*operand));
        let op = match op {
            FlatHtmlOp::Call(function, args) => match self.inlined(&function) {
                Some(decl) => {
                    let FlatFunction::Html(callee) = decl else {
                        panic!("{function} is called for Html but returns a value");
                    };
                    let body = self.freshener(decl, args).freshen_html_block(&callee.body);
                    out.extend(body.bindings);
                    // The result is a fresh name or an argument, and neither
                    // is ever renamed, so no read goes through two renames.
                    self.renames.insert(binder.var, body.result);
                    return;
                }
                None => FlatHtmlOp::Call(function, args),
            },
            FlatHtmlOp::Match(match_) => FlatHtmlOp::Match(self.match_(match_, Self::html_block)),
            FlatHtmlOp::For(FlatFor {
                var,
                array,
                key,
                body,
            }) => FlatHtmlOp::For(FlatFor {
                var,
                array,
                key: key.map(|key| self.pure_block(key)),
                body: self.html_block(body),
            }),
            FlatHtmlOp::Lambda(IrLambda { parameters, body }) => FlatHtmlOp::Lambda(IrLambda {
                parameters,
                body: self.html_block(body),
            }),
            op => op,
        };
        out.push(FlatHtmlBinding::Html(IrBinding { binder, op }));
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::hop::typing::{ComparableType, NumericType, Ty};
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::flat_typecheck;
    use crate::ir::ir_type::{IrDataType, IrFunctionDataType, IrType};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
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

            let program = inline_function_calls(program);
            flat_typecheck::typecheck(&program).unwrap();

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
                        let again = t.lambda([IrType::INT], |t, params| t.apply(f, [params[0]]));
                        t.apply(again, [t.apply(f, [t.param("x")])])
                    },
                )
                .entrypoint("Test", [("n", Ty::Int)], |t| {
                    let n = t.param("n");
                    let add = t.lambda([IrType::INT], |t, params| {
                        t.add(NumericType::Int, params[0], n)
                    });
                    let twice = t.call("Twice", [add, t.int(1)]);
                    t.escape(t.int_to_string(twice))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn Twice@f0(f@v0: (Int) -> Int, x@v1: Int) -> Int {
                  let v4: (Int) -> Int = fn(v2: Int) {
                    let v3: Int = v0(v2)
                    v3
                  }
                  let v5: Int = v0(v1)
                  let v6: Int = v4(v5)
                  v6
                }
                entrypoint Test@e0(n@v7: Int) -> Html {
                  let v10: (Int) -> Int = fn(v8: Int) {
                    let v9: Int = add[Int](v8, v7)
                    v9
                  }
                  let v11: Int = 1
                  let v12: Int = Twice@f0(v10, v11)
                  let v13: String = v12.to_string()
                  let v14: Html = escape(v13)
                  v14
                }

                -- after --
                entrypoint Test@e0(n@v7: Int) -> Html {
                  let v10: (Int) -> Int = fn(v8: Int) {
                    let v9: Int = add[Int](v8, v7)
                    v9
                  }
                  let v11: Int = 1
                  let v17: (Int) -> Int = fn(v15: Int) {
                    let v16: Int = v10(v15)
                    v16
                  }
                  let v18: Int = v10(v11)
                  let v19: Int = v17(v18)
                  let v13: String = v19.to_string()
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
                fn double@f0(x@v0: Int) -> Int {
                  let v1: Int = add[Int](v0, v0)
                  v1
                }
                entrypoint Test@e0(n@v2: Int) -> Html {
                  let v3: Int = double@f0(v2)
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint Test@e0(n@v2: Int) -> Html {
                  let v6: Int = add[Int](v2, v2)
                  let v4: String = v6.to_string()
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
                fn inner@f0(x@v0: Int) -> Int {
                  let v1: Int = 1
                  let v2: Int = add[Int](v0, v1)
                  v2
                }
                fn outer@f1(y@v3: Int) -> Int {
                  let v4: Int = inner@f0(v3)
                  let v5: Int = 2
                  let v6: Int = mul[Int](v4, v5)
                  v6
                }
                entrypoint Test@e0(n@v7: Int) -> Html {
                  let v8: Int = outer@f1(v7)
                  let v9: String = v8.to_string()
                  let v10: Html = escape(v9)
                  v10
                }

                -- after --
                entrypoint Test@e0(n@v7: Int) -> Html {
                  let v13: Int = 1
                  let v14: Int = add[Int](v7, v13)
                  let v15: Int = 2
                  let v16: Int = mul[Int](v14, v15)
                  let v9: String = v16.to_string()
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
                .function("list", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    t.html_for(t.param("items"), |t, item| {
                        t.element("li", [], Some(t.escape(item)))
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
                fn list@f0(items@v0: Array[String]) -> Html {
                  let v4: Html = for v1: String in v0 {
                    let v2: Html = escape(v1)
                    let v3: Html = element("li", {}, v2)
                    v3
                  }
                  v4
                }
                entrypoint Test@e0(a@v5: Array[String], b@v6: Array[String]) -> Html {
                  let v7: Html = list@f0(v5)
                  let v8: Html = list@f0(v6)
                  let v9: Html = concat(v7, v8)
                  v9
                }

                -- after --
                entrypoint Test@e0(a@v5: Array[String], b@v6: Array[String]) -> Html {
                  let v13: Html = for v10: String in v5 {
                    let v11: Html = escape(v10)
                    let v12: Html = element("li", {}, v11)
                    v12
                  }
                  let v17: Html = for v14: String in v6 {
                    let v15: Html = escape(v14)
                    let v16: Html = element("li", {}, v15)
                    v16
                  }
                  let v9: Html = concat(v13, v17)
                  v9
                }
            "#]],
        );
    }

    #[test]
    fn should_give_each_copy_of_a_cell_a_fresh_cell_that_its_writes_write() {
        check(
            FlatProgramBuilder::new()
                .function("counter", [], Ty::Html, |t| {
                    let n = t.cell(t.int(0));
                    let count = t.track(n);
                    let bump = t.event_handler(|t| {
                        t.cell_write(n, t.add(NumericType::Int, count, t.int(1)))
                    });
                    t.element_on_click("button", [], bump, Some(t.escape(t.int_to_string(count))))
                })
                .entrypoint("Test", [], |t| {
                    t.html_concat([t.call("counter", []), t.call("counter", [])])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn counter@f0() -> Html {
                  let v0: Int = 0
                  let v1: Cell[Int] = cell(v0)
                  let v2: Int = track(v1)
                  let v6: () -> Effect[Tuple[]] = fn() {
                    let v3: Int = 1
                    let v4: Int = add[Int](v2, v3)
                    let v5: Effect[Tuple[]] = v1 := v4
                    v5
                  }
                  let v7: String = v2.to_string()
                  let v8: Html = escape(v7)
                  let v9: Html = element("button", {}, v8) on_click v6
                  v9
                }
                entrypoint Test@e0() -> Html {
                  let v10: Html = counter@f0()
                  let v11: Html = counter@f0()
                  let v12: Html = concat(v10, v11)
                  v12
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v13: Int = 0
                  let v14: Cell[Int] = cell(v13)
                  let v15: Int = track(v14)
                  let v19: () -> Effect[Tuple[]] = fn() {
                    let v16: Int = 1
                    let v17: Int = add[Int](v15, v16)
                    let v18: Effect[Tuple[]] = v14 := v17
                    v18
                  }
                  let v20: String = v15.to_string()
                  let v21: Html = escape(v20)
                  let v22: Html = element("button", {}, v21) on_click v19
                  let v23: Int = 0
                  let v24: Cell[Int] = cell(v23)
                  let v25: Int = track(v24)
                  let v29: () -> Effect[Tuple[]] = fn() {
                    let v26: Int = 1
                    let v27: Int = add[Int](v25, v26)
                    let v28: Effect[Tuple[]] = v24 := v27
                    v28
                  }
                  let v30: String = v25.to_string()
                  let v31: Html = escape(v30)
                  let v32: Html = element("button", {}, v31) on_click v29
                  let v12: Html = concat(v22, v32)
                  v12
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_a_function_that_gives_an_effect_into_a_handler() {
        check(
            FlatProgramBuilder::new()
                .function(
                    "both",
                    [
                        ("first", Ty::effect(Ty::tuple([]))),
                        ("second", Ty::effect(Ty::tuple([]))),
                    ],
                    Ty::effect(Ty::tuple([])),
                    |t| {
                        let unit = IrType::Data(IrFunctionDataType::Data(IrDataType::UNIT));
                        let second = t.lambda([unit], |t, _| t.param("second"));
                        t.effect_bind(t.param("first"), second)
                    },
                )
                .entrypoint("Test", [], |t| {
                    let count = t.cell(t.int(0));
                    let on = t.cell(t.bool(false));
                    let n = t.track(count);
                    let is_on = t.track(on);
                    let both = t.event_handler(|t| {
                        let bump = t.cell_write(count, t.add(NumericType::Int, n, t.int(1)));
                        let flip = t.cell_write(on, t.not(is_on));
                        t.call("both", [bump, flip])
                    });
                    t.element_on_click("button", [], both, Some(t.escape(t.int_to_string(n))))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn both@f0(first@v0: Effect[Tuple[]], second@v1: Effect[Tuple[]]) -> Effect[Tuple[]] {
                  let v3: (Tuple[]) -> Effect[Tuple[]] = fn(v2: Tuple[]) {
                    v1
                  }
                  let v4: Effect[Tuple[]] = bind(v0, v3)
                  v4
                }
                entrypoint Test@e0() -> Html {
                  let v5: Int = 0
                  let v6: Cell[Int] = cell(v5)
                  let v7: Bool = false
                  let v8: Cell[Bool] = cell(v7)
                  let v9: Int = track(v6)
                  let v10: Bool = track(v8)
                  let v17: () -> Effect[Tuple[]] = fn() {
                    let v11: Int = 1
                    let v12: Int = add[Int](v9, v11)
                    let v13: Effect[Tuple[]] = v6 := v12
                    let v14: Bool = not(v10)
                    let v15: Effect[Tuple[]] = v8 := v14
                    let v16: Effect[Tuple[]] = both@f0(v13, v15)
                    v16
                  }
                  let v18: String = v9.to_string()
                  let v19: Html = escape(v18)
                  let v20: Html = element("button", {}, v19) on_click v17
                  v20
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v5: Int = 0
                  let v6: Cell[Int] = cell(v5)
                  let v7: Bool = false
                  let v8: Cell[Bool] = cell(v7)
                  let v9: Int = track(v6)
                  let v10: Bool = track(v8)
                  let v17: () -> Effect[Tuple[]] = fn() {
                    let v11: Int = 1
                    let v12: Int = add[Int](v9, v11)
                    let v13: Effect[Tuple[]] = v6 := v12
                    let v14: Bool = not(v10)
                    let v15: Effect[Tuple[]] = v8 := v14
                    let v22: (Tuple[]) -> Effect[Tuple[]] = fn(v21: Tuple[]) {
                      v15
                    }
                    let v23: Effect[Tuple[]] = bind(v13, v22)
                    v23
                  }
                  let v18: String = v9.to_string()
                  let v19: Html = escape(v18)
                  let v20: Html = element("button", {}, v19) on_click v17
                  v20
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_a_self_recursive_function_alone() {
        check(
            FlatProgramBuilder::new()
                .function("count", [("n", Ty::Int)], Ty::Html, |t| {
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
                fn count@f0(n@v0: Int) -> Html {
                  let v1: Int = 1
                  let v2: Bool = lt[Int](v0, v1)
                  let v7: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = sub[Int](v0, v4)
                      let v6: Html = count@f0(v5)
                      v6
                    }
                  }
                  v7
                }
                entrypoint Test@e0() -> Html {
                  let v8: Int = 3
                  let v9: Html = count@f0(v8)
                  v9
                }

                -- after --
                fn count@f0(n@v0: Int) -> Html {
                  let v1: Int = 1
                  let v2: Bool = lt[Int](v0, v1)
                  let v7: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = sub[Int](v0, v4)
                      let v6: Html = count@f0(v5)
                      v6
                    }
                  }
                  v7
                }
                entrypoint Test@e0() -> Html {
                  let v8: Int = 3
                  let v9: Html = count@f0(v8)
                  v9
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_mutually_recursive_functions_alone() {
        check(
            FlatProgramBuilder::new()
                .declare("ping", [("n", Ty::Int)], Ty::Html)
                .declare("pong", [("n", Ty::Int)], Ty::Html)
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
                fn ping@f0(n@v0: Int) -> Html {
                  let v2: Int = 1
                  let v3: Bool = lt[Int](v0, v2)
                  let v8: Html = match v3 {
                    true => {
                      let v4: Html = text(".")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = sub[Int](v0, v5)
                      let v7: Html = pong@f1(v6)
                      v7
                    }
                  }
                  v8
                }
                fn pong@f1(n@v1: Int) -> Html {
                  let v9: Html = ping@f0(v1)
                  v9
                }
                entrypoint Test@e0() -> Html {
                  let v10: Int = 2
                  let v11: Html = ping@f0(v10)
                  v11
                }

                -- after --
                fn ping@f0(n@v0: Int) -> Html {
                  let v2: Int = 1
                  let v3: Bool = lt[Int](v0, v2)
                  let v8: Html = match v3 {
                    true => {
                      let v4: Html = text(".")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = sub[Int](v0, v5)
                      let v7: Html = pong@f1(v6)
                      v7
                    }
                  }
                  v8
                }
                fn pong@f1(n@v1: Int) -> Html {
                  let v9: Html = ping@f0(v1)
                  v9
                }
                entrypoint Test@e0() -> Html {
                  let v10: Int = 2
                  let v11: Html = ping@f0(v10)
                  v11
                }
            "#]],
        );
    }

    #[test]
    fn should_pass_a_cell_to_the_body_of_a_callee() {
        check(
            FlatProgramBuilder::new()
                .function("Bump", [("count", Ty::cell(Ty::Int))], Ty::Html, |t| {
                    let shown = t.track(t.param("count"));
                    let bump = t.event_handler(|t| {
                        t.cell_write(t.param("count"), t.add(NumericType::Int, shown, t.int(1)))
                    });
                    t.element_on_click("button", [], bump, Some(t.escape(t.int_to_string(shown))))
                })
                .entrypoint("Test", [], |t| {
                    let count = t.cell(t.int(0));
                    t.call("Bump", [count])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn Bump@f0(count@v0: Cell[Int]) -> Html {
                  let v1: Int = track(v0)
                  let v5: () -> Effect[Tuple[]] = fn() {
                    let v2: Int = 1
                    let v3: Int = add[Int](v1, v2)
                    let v4: Effect[Tuple[]] = v0 := v3
                    v4
                  }
                  let v6: String = v1.to_string()
                  let v7: Html = escape(v6)
                  let v8: Html = element("button", {}, v7) on_click v5
                  v8
                }
                entrypoint Test@e0() -> Html {
                  let v9: Int = 0
                  let v10: Cell[Int] = cell(v9)
                  let v11: Html = Bump@f0(v10)
                  v11
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v9: Int = 0
                  let v10: Cell[Int] = cell(v9)
                  let v12: Int = track(v10)
                  let v16: () -> Effect[Tuple[]] = fn() {
                    let v13: Int = 1
                    let v14: Int = add[Int](v12, v13)
                    let v15: Effect[Tuple[]] = v10 := v14
                    v15
                  }
                  let v17: String = v12.to_string()
                  let v18: Html = escape(v17)
                  let v19: Html = element("button", {}, v18) on_click v16
                  v19
                }
            "#]],
        );
    }
}
