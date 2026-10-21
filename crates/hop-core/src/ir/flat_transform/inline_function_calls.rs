use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dependency_graph::DependencyGraph;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatFunctionDeclaration, FlatModule, FlatOp};
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::function_id::FunctionId;
use crate::ir::ir_for::IrFor;
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

    // Declaration order is part of the module's identity, so keep it.
    let order: Vec<FunctionId> = functions.iter().map(|f| f.function.id).collect();
    let mut decls: HashMap<FunctionId, FlatFunctionDeclaration> =
        functions.into_iter().map(|f| (f.function.id, f)).collect();

    // The name each inlined call's result is read through. One map serves
    // every body, since names are unique in the module.
    let mut renames = HashMap::new();

    // sorted_sccs puts dependencies before dependents, which for a call graph
    // means every callee is inlined before the callers that copy it.
    for id in sccs.into_iter().flatten() {
        // Taking the declaration out while its own body is inlined keeps the
        // map borrow-free, and means a self-call finds nothing to inline.
        let Some(mut decl) = decls.remove(&id) else {
            continue;
        };
        decl.body = inline_block(decl.body, &decls, &recursive, &mut value_ids, &mut renames);
        decls.insert(id, decl);
    }

    // Every call to a function outside a cycle was replaced by its body, so
    // nothing calls such a function any more. An entrypoint function is called
    // from outside the module, so it stays.
    let functions = order
        .into_iter()
        .map(|id| decls.remove(&id).expect("each function is declared once"))
        .filter(|decl| decl.entrypoint || recursive.contains(&decl.function.id))
        .collect();

    FlatModule {
        functions,
        value_ids,
    }
}

fn collect_callees(block: &FlatBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        match &binding.op {
            FlatOp::Call(function, ..) => {
                out.insert(function.id);
            }
            FlatOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_callees(&arm.body, out);
                }
            }
            FlatOp::HtmlFor(IrFor { body, .. }) => {
                collect_callees(body, out);
            }
            _ => {}
        }
    }
}

/// Inline the calls of the block, and rewrite its reads through `renames`,
/// extended with the result of every call it inlines.
fn inline_block(
    block: FlatBlock,
    decls: &HashMap<FunctionId, FlatFunctionDeclaration>,
    recursive: &HashSet<FunctionId>,
    value_ids: &mut ValueIdCounter,
    renames: &mut HashMap<ValueId, ValueId>,
) -> FlatBlock {
    let mut bindings = Vec::with_capacity(block.bindings.len());
    for binding in block.bindings {
        let FlatBinding { binder, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| {
            if let Some(renamed) = renames.get(operand) {
                *operand = *renamed;
            }
        });
        let op = match op {
            FlatOp::Call(function, args)
                if decls.contains_key(&function.id) && !recursive.contains(&function.id) =>
            {
                let decl = &decls[&function.id];
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
                let body = Freshener::new(value_ids, arguments).freshen_block(&decl.body);
                bindings.extend(body.bindings);
                // The result is a fresh name or an argument, and neither is
                // ever renamed, so no read goes through two renames.
                renames.insert(binder.var, body.result);
                continue;
            }
            FlatOp::Match(IrMatch { subject, arms }) => FlatOp::Match(IrMatch {
                subject,
                arms: arms
                    .into_iter()
                    .map(|arm| IrMatchArm {
                        pattern: arm.pattern,
                        body: inline_block(arm.body, decls, recursive, value_ids, renames),
                    })
                    .collect(),
            }),
            FlatOp::HtmlFor(IrFor { var, array, body }) => FlatOp::HtmlFor(IrFor {
                var,
                array,
                body: inline_block(body, decls, recursive, value_ids, renames),
            }),
            op => op,
        };
        bindings.push(FlatBinding { binder, op });
    }
    FlatBlock {
        bindings,
        result: renames.get(&block.result).copied().unwrap_or(block.result),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
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

            let before_module = module.to_string();
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

            let module = inline_function_calls(module);

            for ((function, args), before) in entrypoint_args.iter().zip(&before) {
                // An entrypoint that hit the call depth limit has no output to
                // preserve, and inlining removes frames from the count.
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
                .function("double", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.var("x"))
                })
                .entrypoint("Test", [("n", "Int")], "Html", |t| {
                    t.escape(t.int_to_string(t.call("double", [t.var("n")])))
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
                .function("inner", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.int(1))
                })
                .function("outer", [("y", "Int")], "Int", |t| {
                    t.mul(t.call("inner", [t.var("y")]), t.int(2))
                })
                .entrypoint("Test", [("n", "Int")], "Html", |t| {
                    t.escape(t.int_to_string(t.call("outer", [t.var("n")])))
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
                .function("list", [("items", "Array[String]")], "Html", |t| {
                    t.html_for(Some("item"), t.var("items"), |t| {
                        t.element("li", [], t.escape(t.var("item")))
                    })
                })
                .entrypoint(
                    "Test",
                    [("a", "Array[String]"), ("b", "Array[String]")],
                    "Html",
                    |t| {
                        t.html_concat([
                            t.call("list", vec![t.var("a")]),
                            t.call("list", vec![t.var("b")]),
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
                .function("count", [("n", "Int")], "Html", |t| {
                    t.match_expr(t.lt(t.var("n"), t.int(1)), |m| {
                        m.arm(Pat::True, |t| t.text("."));
                        m.arm(Pat::False, |t| {
                            t.call("count", [t.sub(t.var("n"), t.int(1))])
                        });
                    })
                })
                .entrypoint("Test", [], "Html", |t| t.call("count", [t.int(3)]))
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
                .declare("ping", [("n", "Int")], "Html")
                .declare("pong", [("n", "Int")], "Html")
                .define("ping", |t, _| {
                    t.match_expr(t.lt(t.var("n"), t.int(1)), |m| {
                        m.arm(Pat::True, |t| t.text("."));
                        m.arm(Pat::False, |t| {
                            t.call("pong", [t.sub(t.var("n"), t.int(1))])
                        });
                    })
                })
                .define("pong", |t, _| t.call("ping", [t.var("n")]))
                .entrypoint("Test", [], "Html", |t| t.call("ping", [t.int(2)]))
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
