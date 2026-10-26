use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dependency_graph::DependencyGraph;
use crate::ir::flat_module::{
    FlatDataBinding, FlatDataBlock, FlatDataFunction, FlatDataOp, FlatFunctionDeclaration,
    FlatHtmlBinding, FlatHtmlBlock, FlatHtmlFunction, FlatHtmlOp, FlatModule,
};
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::function_id::FunctionId;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// A pass that replaces a call to a non-recursive function with the
/// callee's body, and drops the function once no call to it is left. Only
/// the functions in a call cycle and the entrypoint functions remain.
///
/// A call's arguments are names, computed before the call, so a parameter
/// is bound by reading the argument's name in its place. The body is copied
/// with fresh names, its bindings join the block the call sits in, and
/// every read of the call's name becomes a read of the body's result.
pub fn inline_function_calls(module: FlatModule) -> FlatModule {
    let FlatModule {
        functions,
        mut value_ids,
    } = module;

    let mut graph: DependencyGraph<FunctionId> = DependencyGraph::new();
    for function in &functions {
        let mut callees = BTreeSet::new();
        match function {
            FlatFunctionDeclaration::Data(function) => {
                collect_data_callees(&function.body, &mut callees);
            }
            FlatFunctionDeclaration::Html(function) => {
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

    // Declaration order is part of the module's identity, so keep it.
    let order: Vec<FunctionId> = functions.iter().map(|f| f.function().id).collect();
    let mut decls: HashMap<FunctionId, FlatFunctionDeclaration> = functions
        .into_iter()
        .map(|f| (f.function().id, f))
        .collect();

    // The name each inlined call's result is read through. One map serves
    // every body, since names are unique in the module.
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
            FlatFunctionDeclaration::Data(function) => {
                FlatFunctionDeclaration::Data(FlatDataFunction {
                    body: inliner.data_block(function.body),
                    ..function
                })
            }
            FlatFunctionDeclaration::Html(function) => {
                FlatFunctionDeclaration::Html(FlatHtmlFunction {
                    body: inliner.html_block(function.body),
                    ..function
                })
            }
        };
        decls.insert(id, decl);
    }

    // Every call to a function outside a cycle was replaced by its body, so
    // nothing calls such a function any more. An entrypoint function is called
    // from outside the module, so it stays.
    let functions = order
        .into_iter()
        .map(|id| decls.remove(&id).expect("each function is declared once"))
        .filter(|decl| {
            let entrypoint = match decl {
                FlatFunctionDeclaration::Data(function) => function.entrypoint,
                FlatFunctionDeclaration::Html(function) => function.entrypoint,
            };
            entrypoint || recursive.contains(&decl.function().id)
        })
        .collect();

    FlatModule {
        functions,
        value_ids,
    }
}

fn collect_data_callees(block: &FlatDataBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        collect_data_binding_callees(binding, out);
    }
}

fn collect_html_callees(block: &FlatHtmlBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        collect_data_binding_callees(binding, out);
    }
    for binding in &block.html {
        match &binding.op {
            FlatHtmlOp::Call(function, ..) => {
                out.insert(function.id);
            }
            FlatHtmlOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_html_callees(&arm.body, out);
                }
            }
            FlatHtmlOp::For(IrFor { body, .. }) => {
                collect_html_callees(body, out);
            }
            _ => {}
        }
    }
}

fn collect_data_binding_callees(binding: &FlatDataBinding, out: &mut BTreeSet<FunctionId>) {
    match &binding.op {
        FlatDataOp::Call(function, ..) => {
            out.insert(function.id);
        }
        FlatDataOp::Match(match_) => {
            for arm in &match_.arms {
                collect_data_callees(&arm.body, out);
            }
        }
        FlatDataOp::Data(_) => {}
    }
}

/// Inlines the calls of the blocks of one body, and rewrites their reads
/// through `renames`, extended with the result of every call it inlines.
struct Inliner<'a> {
    decls: &'a HashMap<FunctionId, FlatFunctionDeclaration>,
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
    fn inlined(&self, function: &IrFunction) -> Option<&'a FlatFunctionDeclaration> {
        let decls = self.decls;
        decls
            .get(&function.id)
            .filter(|_| !self.recursive.contains(&function.id))
    }

    /// A freshener that reads each argument of the call for the parameter
    /// it is passed to.
    fn freshener(&mut self, decl: &FlatFunctionDeclaration, args: Vec<ValueId>) -> Freshener<'_> {
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

    fn data_block(&mut self, block: FlatDataBlock) -> FlatDataBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.data_binding(binding, &mut bindings);
        }
        FlatDataBlock {
            bindings,
            result: self.rename(block.result),
        }
    }

    /// The values of an inlined Html call join the values of the block, so
    /// they still come before its Html.
    fn html_block(&mut self, block: FlatHtmlBlock) -> FlatHtmlBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.data_binding(binding, &mut bindings);
        }
        let mut html = Vec::with_capacity(block.html.len());
        for binding in block.html {
            self.html_binding(binding, &mut bindings, &mut html);
        }
        FlatHtmlBlock {
            bindings,
            html,
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

    fn data_binding(&mut self, binding: FlatDataBinding, out: &mut Vec<FlatDataBinding>) {
        let FlatDataBinding { binder, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.rename(*operand));
        let op = match op {
            FlatDataOp::Call(function, args) => match self.inlined(&function) {
                Some(decl) => {
                    let FlatFunctionDeclaration::Data(callee) = decl else {
                        panic!("{function} is called for a value but returns Html");
                    };
                    let body = self.freshener(decl, args).freshen_data_block(&callee.body);
                    out.extend(body.bindings);
                    // The result is a fresh name or an argument, and neither
                    // is ever renamed, so no read goes through two renames.
                    self.renames.insert(binder.var, body.result);
                    return;
                }
                None => FlatDataOp::Call(function, args),
            },
            FlatDataOp::Match(match_) => FlatDataOp::Match(self.match_(match_, Self::data_block)),
            op => op,
        };
        out.push(FlatDataBinding { binder, op });
    }

    fn html_binding(
        &mut self,
        binding: FlatHtmlBinding,
        bindings: &mut Vec<FlatDataBinding>,
        html: &mut Vec<FlatHtmlBinding>,
    ) {
        let FlatHtmlBinding { binder, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.rename(*operand));
        let op = match op {
            FlatHtmlOp::Call(function, args) => match self.inlined(&function) {
                Some(decl) => {
                    let FlatFunctionDeclaration::Html(callee) = decl else {
                        panic!("{function} is called for Html but returns a value");
                    };
                    let body = self.freshener(decl, args).freshen_html_block(&callee.body);
                    bindings.extend(body.bindings);
                    html.extend(body.html);
                    // The result is a fresh name or an argument, and neither
                    // is ever renamed, so no read goes through two renames.
                    self.renames.insert(binder.var, body.result);
                    return;
                }
                None => FlatHtmlOp::Call(function, args),
            },
            FlatHtmlOp::Match(match_) => FlatHtmlOp::Match(self.match_(match_, Self::html_block)),
            FlatHtmlOp::For(IrFor { var, array, body }) => FlatHtmlOp::For(IrFor {
                var,
                array,
                body: self.html_block(body),
            }),
            op => op,
        };
        html.push(FlatHtmlBinding { binder, op });
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::hop::typing::Ty;
    use crate::ir::flat_module_builder::{FlatModuleBuilder, Pat};
    use crate::ir::flat_module_generator::{random_entrypoint_args, random_module};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_inlining() {
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

            let module = inline_function_calls(module);

            for ((function, args), before) in entrypoint_args.iter().zip(&before) {
                // An entrypoint that hit the call depth limit or the op count limit
                // has no output to preserve, and inlining removes frames from the count.
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
        let after = inline_function_calls(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_inline_a_call_and_read_the_argument_where_the_parameter_was_read() {
        check(
            FlatModuleBuilder::new()
                .function("double", [("x", Ty::Int)], Ty::Int, |t| {
                    t.add(t.param("x"), t.param("x"))
                })
                .entrypoint("Test", [("n", Ty::Int)], Ty::Html, |t| {
                    t.escape(t.int_to_string(t.call("double", [t.param("n")])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn double@f0(x@v0: Int) -> Int {
                  let v1: Int = v0 + v0
                  v1
                }
                entrypoint fn Test@f1(n@v2: Int) -> Html {
                  let v3: Int = double@f0(v2)
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint fn Test@f1(n@v2: Int) -> Html {
                  let v6: Int = v2 + v2
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
            FlatModuleBuilder::new()
                .function("inner", [("x", Ty::Int)], Ty::Int, |t| {
                    t.add(t.param("x"), t.int(1))
                })
                .function("outer", [("y", Ty::Int)], Ty::Int, |t| {
                    t.mul(t.call("inner", [t.param("y")]), t.int(2))
                })
                .entrypoint("Test", [("n", Ty::Int)], Ty::Html, |t| {
                    t.escape(t.int_to_string(t.call("outer", [t.param("n")])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn inner@f0(x@v0: Int) -> Int {
                  let v1: Int = 1
                  let v2: Int = v0 + v1
                  v2
                }
                fn outer@f1(y@v3: Int) -> Int {
                  let v4: Int = inner@f0(v3)
                  let v5: Int = 2
                  let v6: Int = v4 * v5
                  v6
                }
                entrypoint fn Test@f2(n@v7: Int) -> Html {
                  let v8: Int = outer@f1(v7)
                  let v9: String = v8.to_string()
                  let v10: Html = escape(v9)
                  v10
                }

                -- after --
                entrypoint fn Test@f2(n@v7: Int) -> Html {
                  let v13: Int = 1
                  let v14: Int = v7 + v13
                  let v15: Int = 2
                  let v16: Int = v14 * v15
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
            FlatModuleBuilder::new()
                .function("list", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    t.html_for(t.param("items"), |t, item| {
                        t.element("li", [], t.escape(item))
                    })
                })
                .entrypoint(
                    "Test",
                    [("a", Ty::array(Ty::String)), ("b", Ty::array(Ty::String))],
                    Ty::Html,
                    |t| {
                        t.html_concat([
                            t.call("list", vec![t.param("a")]),
                            t.call("list", vec![t.param("b")]),
                        ])
                    },
                )
                .build(),
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
                entrypoint fn Test@f1(a@v5: Array[String], b@v6: Array[String]) -> Html {
                  let v7: Html = list@f0(v5)
                  let v8: Html = list@f0(v6)
                  let v9: Html = concat(v7, v8)
                  v9
                }

                -- after --
                entrypoint fn Test@f1(a@v5: Array[String], b@v6: Array[String]) -> Html {
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
    fn should_leave_a_self_recursive_function_alone() {
        check(
            FlatModuleBuilder::new()
                .function("count", [("n", Ty::Int)], Ty::Html, |t| {
                    t.match_(t.lt(t.param("n"), t.int(1)), |m| {
                        m.arm(Pat::True, |t, _| t.text("."));
                        m.arm(Pat::False, |t, _| {
                            t.call("count", [t.sub(t.param("n"), t.int(1))])
                        });
                    })
                })
                .entrypoint("Test", [], Ty::Html, |t| t.call("count", [t.int(3)]))
                .build(),
            expect![[r#"
                -- before --
                fn count@f0(n@v0: Int) -> Html {
                  let v1: Int = 1
                  let v2: Bool = v0 < v1
                  let v7: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = v0 - v4
                      let v6: Html = count@f0(v5)
                      v6
                    }
                  }
                  v7
                }
                entrypoint fn Test@f1() -> Html {
                  let v8: Int = 3
                  let v9: Html = count@f0(v8)
                  v9
                }

                -- after --
                fn count@f0(n@v0: Int) -> Html {
                  let v1: Int = 1
                  let v2: Bool = v0 < v1
                  let v7: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = v0 - v4
                      let v6: Html = count@f0(v5)
                      v6
                    }
                  }
                  v7
                }
                entrypoint fn Test@f1() -> Html {
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
            FlatModuleBuilder::new()
                .declare("ping", [("n", Ty::Int)], Ty::Html)
                .declare("pong", [("n", Ty::Int)], Ty::Html)
                .define("ping", |t, _| {
                    t.match_(t.lt(t.param("n"), t.int(1)), |m| {
                        m.arm(Pat::True, |t, _| t.text("."));
                        m.arm(Pat::False, |t, _| {
                            t.call("pong", [t.sub(t.param("n"), t.int(1))])
                        });
                    })
                })
                .define("pong", |t, _| t.call("ping", [t.param("n")]))
                .entrypoint("Test", [], Ty::Html, |t| t.call("ping", [t.int(2)]))
                .build(),
            expect![[r#"
                -- before --
                fn ping@f0(n@v0: Int) -> Html {
                  let v2: Int = 1
                  let v3: Bool = v0 < v2
                  let v8: Html = match v3 {
                    true => {
                      let v4: Html = text(".")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = v0 - v5
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
                entrypoint fn Test@f2() -> Html {
                  let v10: Int = 2
                  let v11: Html = ping@f0(v10)
                  v11
                }

                -- after --
                fn ping@f0(n@v0: Int) -> Html {
                  let v2: Int = 1
                  let v3: Bool = v0 < v2
                  let v8: Html = match v3 {
                    true => {
                      let v4: Html = text(".")
                      v4
                    }
                    false => {
                      let v5: Int = 1
                      let v6: Int = v0 - v5
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
                entrypoint fn Test@f2() -> Html {
                  let v10: Int = 2
                  let v11: Html = ping@f0(v10)
                  v11
                }
            "#]],
        );
    }
}
