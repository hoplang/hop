use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dependency_graph::DependencyGraph;
use crate::ir::binder_id::{BinderId, BinderIdCounter};
use crate::ir::flat_module::{
    FlatBinding, FlatBlock, FlatForSource, FlatFunctionDeclaration, FlatModule, FlatOp,
};
use crate::ir::function_id::FunctionId;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_match::{EnumMatchArm, Match};
use crate::ir::var_id::{VarId, VarIdCounter};

/// A pass that replaces a call to a non-recursive function with the
/// callee's body, and drops the function once no call to it is left. Only
/// the functions in a call cycle and the entry functions remain.
///
/// A call's arguments are names, computed before the call, so a parameter
/// is bound by dropping every Read of it and reading the argument's name in
/// its place. The body is copied with fresh names and binders, its bindings
/// join the block the call sits in, and every read of the call's name
/// becomes a read of the body's result.
pub fn inline_function_calls(module: FlatModule) -> FlatModule {
    let FlatModule {
        functions,
        mut var_ids,
        mut binder_ids,
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
        decl.body = inline_block(
            decl.body,
            &decls,
            &recursive,
            &mut var_ids,
            &mut binder_ids,
            &mut renames,
        );
        decls.insert(id, decl);
    }

    // Every call to a function outside a cycle was replaced by its body, so
    // nothing calls such a function any more. An entry function is called
    // from outside the module, so it stays.
    let functions = order
        .into_iter()
        .map(|id| decls.remove(&id).expect("each function is declared once"))
        .filter(|decl| decl.entry || recursive.contains(&decl.function.id))
        .collect();

    FlatModule {
        functions,
        var_ids,
        binder_ids,
    }
}

fn collect_callees(block: &FlatBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        match &binding.op {
            FlatOp::Call { function, .. } => {
                out.insert(function.id);
            }
            FlatOp::Match(Match::Bool {
                true_body,
                false_body,
                ..
            }) => {
                collect_callees(true_body, out);
                collect_callees(false_body, out);
            }
            FlatOp::Match(Match::Option {
                some_arm_body,
                none_arm_body,
                ..
            }) => {
                collect_callees(some_arm_body, out);
                collect_callees(none_arm_body, out);
            }
            FlatOp::Match(Match::Enum { arms, .. }) => {
                for arm in arms {
                    collect_callees(&arm.body, out);
                }
            }
            FlatOp::HtmlFor { body, .. } => collect_callees(body, out),
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
    var_ids: &mut VarIdCounter,
    binder_ids: &mut BinderIdCounter,
    renames: &mut HashMap<VarId, VarId>,
) -> FlatBlock {
    let mut bindings = Vec::with_capacity(block.bindings.len());
    for binding in block.bindings {
        let FlatBinding { name, typ, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| {
            if let Some(renamed) = renames.get(operand) {
                *operand = *renamed;
            }
        });
        let op = match op {
            FlatOp::Call { function, args }
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
                    .map(|(param, arg)| (param.var, arg))
                    .collect();
                let mut freshener = Freshener {
                    var_ids,
                    binder_ids,
                    arguments,
                    names: HashMap::new(),
                    binders: HashMap::new(),
                };
                let body = freshener.freshen_block(&decl.body);
                bindings.extend(body.bindings);
                // The result is a fresh name or an argument, and neither is
                // ever renamed, so no read goes through two renames.
                renames.insert(name, body.result);
                continue;
            }
            FlatOp::Match(match_) => FlatOp::Match(match match_ {
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => Match::Bool {
                    subject,
                    true_body: inline_block(
                        true_body, decls, recursive, var_ids, binder_ids, renames,
                    ),
                    false_body: inline_block(
                        false_body, decls, recursive, var_ids, binder_ids, renames,
                    ),
                },
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body: inline_block(
                        some_arm_body,
                        decls,
                        recursive,
                        var_ids,
                        binder_ids,
                        renames,
                    ),
                    none_arm_body: inline_block(
                        none_arm_body,
                        decls,
                        recursive,
                        var_ids,
                        binder_ids,
                        renames,
                    ),
                },
                Match::Enum { subject, arms } => Match::Enum {
                    subject,
                    arms: arms
                        .into_iter()
                        .map(|arm| EnumMatchArm {
                            pattern: arm.pattern,
                            bindings: arm.bindings,
                            body: inline_block(
                                arm.body, decls, recursive, var_ids, binder_ids, renames,
                            ),
                        })
                        .collect(),
                },
            }),
            FlatOp::HtmlFor { var, source, body } => FlatOp::HtmlFor {
                var,
                source,
                body: inline_block(body, decls, recursive, var_ids, binder_ids, renames),
            },
            op => op,
        };
        bindings.push(FlatBinding { name, typ, op });
    }
    FlatBlock {
        bindings,
        result: renames.get(&block.result).copied().unwrap_or(block.result),
    }
}

/// Copies a callee body for one call, with fresh names and binders, so the
/// copy shares nothing with the declaration or any other copy.
struct Freshener<'a> {
    var_ids: &'a mut VarIdCounter,
    binder_ids: &'a mut BinderIdCounter,
    /// The argument for each parameter.
    arguments: HashMap<BinderId, VarId>,
    /// The name in the copy of every binding in scope so far: the argument
    /// for a Read of a parameter, and a fresh name for any other binding.
    names: HashMap<VarId, VarId>,
    /// The fresh binder in the copy of every loop or arm binder in scope so
    /// far.
    binders: HashMap<BinderId, BinderId>,
}

impl Freshener<'_> {
    fn name(&self, name: VarId) -> VarId {
        self.names.get(&name).copied().unwrap_or_else(|| {
            unreachable!("a callee body binds every name it reads, and {name} is not bound")
        })
    }

    fn fresh_binder(&mut self, binder: &IrBinder) -> IrBinder {
        let var = self.binder_ids.next();
        self.binders.insert(binder.var, var);
        IrBinder {
            var,
            typ: binder.typ.clone(),
        }
    }

    fn freshen_block(&mut self, block: &FlatBlock) -> FlatBlock {
        let bindings = block
            .bindings
            .iter()
            .filter_map(|binding| self.freshen_binding(binding))
            .collect();
        FlatBlock {
            bindings,
            result: self.name(block.result),
        }
    }

    /// The copy of a binding, or nothing for a Read of a parameter, whose
    /// name stands for the argument in the copy.
    fn freshen_binding(&mut self, binding: &FlatBinding) -> Option<FlatBinding> {
        let op = match &binding.op {
            FlatOp::Read(binder) => {
                if let Some(argument) = self.arguments.get(binder) {
                    self.names.insert(binding.name, *argument);
                    return None;
                }
                FlatOp::Read(self.binders.get(binder).copied().unwrap_or_else(|| {
                    unreachable!(
                        "a callee body binds every binder it reads, and {binder} is not bound"
                    )
                }))
            }
            FlatOp::Match(match_) => FlatOp::Match(match match_ {
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => Match::Bool {
                    subject: self.name(*subject),
                    true_body: self.freshen_block(true_body),
                    false_body: self.freshen_block(false_body),
                },
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => {
                    let subject = self.name(*subject);
                    let some_arm_binding = some_arm_binding
                        .as_ref()
                        .map(|binder| self.fresh_binder(binder));
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body: self.freshen_block(some_arm_body),
                        none_arm_body: self.freshen_block(none_arm_body),
                    }
                }
                Match::Enum { subject, arms } => Match::Enum {
                    subject: self.name(*subject),
                    arms: arms
                        .iter()
                        .map(|arm| {
                            let bindings = arm
                                .bindings
                                .iter()
                                .map(|(field, binder)| (field.clone(), self.fresh_binder(binder)))
                                .collect();
                            EnumMatchArm {
                                pattern: arm.pattern.clone(),
                                bindings,
                                body: self.freshen_block(&arm.body),
                            }
                        })
                        .collect(),
                },
            }),
            FlatOp::HtmlFor { var, source, body } => {
                let source = match source {
                    FlatForSource::Array(array) => FlatForSource::Array(self.name(*array)),
                    FlatForSource::RangeInclusive { start, end } => FlatForSource::RangeInclusive {
                        start: self.name(*start),
                        end: self.name(*end),
                    },
                };
                let var = var.as_ref().map(|binder| self.fresh_binder(binder));
                FlatOp::HtmlFor {
                    var,
                    source,
                    body: self.freshen_block(body),
                }
            }
            op => {
                let mut op = op.clone();
                op.for_each_operand_mut(&mut |operand| *operand = self.name(*operand));
                op
            }
        };
        let name = self.var_ids.next();
        self.names.insert(binding.name, name);
        Some(FlatBinding {
            name,
            typ: binding.typ.clone(),
            op,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::{random_entry_args, random_module};
    use crate::ir::pure_to_flat::pure_to_flat;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entry;
    use crate::ir::runtime::value::Value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_inlining() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entry_args = random_entry_args(&module, &mut rng, &registry);
            let module = pure_to_flat(module);

            let before_module = module.to_string();
            let before: Vec<Option<String>> = entry_args
                .iter()
                .map(|(function, args)| {
                    match evaluate_entry(&module, function, args.clone()).map(Value::into_markup) {
                        Ok(output) => Some(output),
                        Err(EvalError::RecursionLimit { .. }) => None,
                        Err(error) => panic!("{error}"),
                    }
                })
                .collect();

            let module = inline_function_calls(module);

            for ((function, args), before) in entry_args.iter().zip(&before) {
                // An entry that hit the call depth limit has no output to
                // preserve, and inlining removes frames from the count.
                let Some(before) = before else {
                    continue;
                };
                let after = evaluate_entry(&module, function, args.clone())
                    .unwrap()
                    .into_markup();
                assert_eq!(
                    before, &after,
                    "entry {function}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    fn check(module: PureModule, expected: Expect) {
        let module = pure_to_flat(module);
        let before = module.to_string();
        let after = inline_function_calls(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_inline_a_call_and_read_the_argument_where_the_parameter_was_read() {
        check(
            PureModuleBuilder::new()
                .function("double", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.var("x"))
                })
                .entry("Test", [("n", "Int")], "Html", |t| {
                    t.escape(t.int_to_string(t.call("double", vec![("x", t.var("n"))])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn double@f0(x@b0: Int) -> Int {
                  let v0: Int = b0
                  let v1: Int = b0
                  let v2: Int = v0 + v1
                  v2
                }
                entry fn Test@f1(n@b1: Int) -> Html {
                  let v3: Int = b1
                  let v4: Int = call double@f0(v3)
                  let v5: String = v4.to_string()
                  let v6: Html = escape(v5)
                  v6
                }

                -- after --
                entry fn Test@f1(n@b1: Int) -> Html {
                  let v3: Int = b1
                  let v7: Int = v3 + v3
                  let v5: String = v7.to_string()
                  let v6: Html = escape(v5)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_callees_before_their_callers() {
        check(
            PureModuleBuilder::new()
                .function("inner", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.int(1))
                })
                .function("outer", [("y", "Int")], "Int", |t| {
                    t.mul(t.call("inner", vec![("x", t.var("y"))]), t.int(2))
                })
                .entry("Test", [("n", "Int")], "Html", |t| {
                    t.escape(t.int_to_string(t.call("outer", vec![("y", t.var("n"))])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn inner@f0(x@b0: Int) -> Int {
                  let v0: Int = b0
                  let v1: Int = 1
                  let v2: Int = v0 + v1
                  v2
                }
                fn outer@f1(y@b1: Int) -> Int {
                  let v3: Int = b1
                  let v4: Int = call inner@f0(v3)
                  let v5: Int = 2
                  let v6: Int = v4 * v5
                  v6
                }
                entry fn Test@f2(n@b2: Int) -> Html {
                  let v7: Int = b2
                  let v8: Int = call outer@f1(v7)
                  let v9: String = v8.to_string()
                  let v10: Html = escape(v9)
                  v10
                }

                -- after --
                entry fn Test@f2(n@b2: Int) -> Html {
                  let v7: Int = b2
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
    fn should_give_each_copy_of_a_loop_fresh_binders() {
        check(
            PureModuleBuilder::new()
                .function("list", [("items", "Array[String]")], "Html", |t| {
                    t.html_for(Some("item"), t.var("items"), |t| {
                        t.element("li", vec![], vec![t.escape(t.var("item"))])
                    })
                })
                .entry(
                    "Test",
                    [("a", "Array[String]"), ("b", "Array[String]")],
                    "Html",
                    |t| {
                        t.concat(vec![
                            t.call("list", vec![("items", t.var("a"))]),
                            t.call("list", vec![("items", t.var("b"))]),
                        ])
                    },
                )
                .build(),
            expect![[r#"
                -- before --
                fn list@f0(items@b0: Array[String]) -> Html {
                  let v0: Array[String] = b0
                  let v5: Html = for b1: String in v0 {
                    let v1: String = b1
                    let v2: Html = escape(v1)
                    let v3: Html = concat(v2)
                    let v4: Html = html("li", {}, v3)
                    v4
                  }
                  v5
                }
                entry fn Test@f1(a@b2: Array[String], b@b3: Array[String]) -> Html {
                  let v6: Array[String] = b2
                  let v7: Html = call list@f0(v6)
                  let v8: Array[String] = b3
                  let v9: Html = call list@f0(v8)
                  let v10: Html = concat(v7, v9)
                  v10
                }

                -- after --
                entry fn Test@f1(a@b2: Array[String], b@b3: Array[String]) -> Html {
                  let v6: Array[String] = b2
                  let v15: Html = for b4: String in v6 {
                    let v11: String = b4
                    let v12: Html = escape(v11)
                    let v13: Html = concat(v12)
                    let v14: Html = html("li", {}, v13)
                    v14
                  }
                  let v8: Array[String] = b3
                  let v20: Html = for b5: String in v8 {
                    let v16: String = b5
                    let v17: Html = escape(v16)
                    let v18: Html = concat(v17)
                    let v19: Html = html("li", {}, v18)
                    v19
                  }
                  let v10: Html = concat(v15, v20)
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_a_self_recursive_function_alone() {
        check(
            PureModuleBuilder::new()
                .function("count", [("n", "Int")], "Html", |t| {
                    t.bool_match_expr(
                        t.lt(t.var("n"), t.int(1)),
                        t.text("."),
                        t.call("count", vec![("n", t.sub(t.var("n"), t.int(1)))]),
                    )
                })
                .entry("Test", [], "Html", |t| {
                    t.call("count", vec![("n", t.int(3))])
                })
                .build(),
            expect![[r#"
                -- before --
                fn count@f0(n@b0: Int) -> Html {
                  let v0: Int = b0
                  let v1: Int = 1
                  let v2: Bool = v0 < v1
                  let v8: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = b0
                      let v5: Int = 1
                      let v6: Int = v4 - v5
                      let v7: Html = call count@f0(v6)
                      v7
                    }
                  }
                  v8
                }
                entry fn Test@f1() -> Html {
                  let v9: Int = 3
                  let v10: Html = call count@f0(v9)
                  v10
                }

                -- after --
                fn count@f0(n@b0: Int) -> Html {
                  let v0: Int = b0
                  let v1: Int = 1
                  let v2: Bool = v0 < v1
                  let v8: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = b0
                      let v5: Int = 1
                      let v6: Int = v4 - v5
                      let v7: Html = call count@f0(v6)
                      v7
                    }
                  }
                  v8
                }
                entry fn Test@f1() -> Html {
                  let v9: Int = 3
                  let v10: Html = call count@f0(v9)
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_leave_mutually_recursive_functions_alone() {
        check(
            PureModuleBuilder::new()
                .function("ping", [("n", "Int")], "Html", |t| {
                    t.bool_match_expr(
                        t.lt(t.var("n"), t.int(1)),
                        t.text("."),
                        t.call("pong", vec![("n", t.sub(t.var("n"), t.int(1)))]),
                    )
                })
                .function("pong", [("n", "Int")], "Html", |t| {
                    t.call("ping", vec![("n", t.var("n"))])
                })
                .entry("Test", [], "Html", |t| {
                    t.call("ping", vec![("n", t.int(2))])
                })
                .build(),
            expect![[r#"
                -- before --
                fn ping@f0(n@b0: Int) -> Html {
                  let v0: Int = b0
                  let v1: Int = 1
                  let v2: Bool = v0 < v1
                  let v8: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = b0
                      let v5: Int = 1
                      let v6: Int = v4 - v5
                      let v7: Html = call pong@f1(v6)
                      v7
                    }
                  }
                  v8
                }
                fn pong@f1(n@b1: Int) -> Html {
                  let v9: Int = b1
                  let v10: Html = call ping@f0(v9)
                  v10
                }
                entry fn Test@f2() -> Html {
                  let v11: Int = 2
                  let v12: Html = call ping@f0(v11)
                  v12
                }

                -- after --
                fn ping@f0(n@b0: Int) -> Html {
                  let v0: Int = b0
                  let v1: Int = 1
                  let v2: Bool = v0 < v1
                  let v8: Html = match v2 {
                    true => {
                      let v3: Html = text(".")
                      v3
                    }
                    false => {
                      let v4: Int = b0
                      let v5: Int = 1
                      let v6: Int = v4 - v5
                      let v7: Html = call pong@f1(v6)
                      v7
                    }
                  }
                  v8
                }
                fn pong@f1(n@b1: Int) -> Html {
                  let v9: Int = b1
                  let v10: Html = call ping@f0(v9)
                  v10
                }
                entry fn Test@f2() -> Html {
                  let v11: Int = 2
                  let v12: Html = call ping@f0(v11)
                  v12
                }
            "#]],
        );
    }
}
