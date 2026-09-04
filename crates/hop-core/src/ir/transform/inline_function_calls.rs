use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dependency_graph::DependencyGraph;
use crate::hop::patterns::Match;
use crate::ir::expr_id::ExprIdCounter;
use crate::ir::ir_var::IrVar;
use crate::ir::pure_module::{
    PureArgument, PureExpr, PureForSource, PureFunctionDeclaration, PureModule, PurePageDeclaration,
};
use crate::ir::var_id::{VarId, VarIdCounter};
use crate::symbols::function_name::FunctionName;

/// A pass that replaces a call to a non-recursive function with the callee's
/// body.
pub fn inline_function_calls(module: PureModule) -> PureModule {
    let PureModule {
        pages,
        functions,
        records,
        enums,
        mut expr_ids,
        mut var_ids,
    } = module;

    let mut graph: DependencyGraph<FunctionName> = DependencyGraph::new();
    for function in &functions {
        let mut callees = BTreeSet::new();
        collect_callees(&function.body, &mut callees);
        graph.set_dependencies(function.name.clone(), callees);
    }

    let sccs = graph.sorted_sccs();
    let recursive: HashSet<FunctionName> = sccs
        .iter()
        .filter(|scc| scc.len() > 1 || scc.iter().any(|name| graph.depends_on(name, name)))
        .flatten()
        .cloned()
        .collect();

    // Declaration order is part of the module's identity, so keep it.
    let order: Vec<FunctionName> = functions.iter().map(|f| f.name.clone()).collect();
    let mut decls: HashMap<FunctionName, PureFunctionDeclaration> =
        functions.into_iter().map(|f| (f.name.clone(), f)).collect();

    // sorted_sccs puts dependencies before dependents, which for a call graph
    // means every callee is inlined before the callers that copy it.
    for name in sccs.into_iter().flatten() {
        // Taking the declaration out while its own body is inlined keeps the
        // map borrow-free, and means a self-call finds nothing to inline.
        let Some(mut decl) = decls.remove(&name) else {
            continue;
        };
        decl.body = inline(decl.body, &decls, &recursive, &mut expr_ids, &mut var_ids);
        decls.insert(name, decl);
    }

    let pages = pages
        .into_iter()
        .map(|page| PurePageDeclaration {
            body: inline(page.body, &decls, &recursive, &mut expr_ids, &mut var_ids),
            ..page
        })
        .collect();

    let functions = order
        .into_iter()
        .map(|name| decls.remove(&name).expect("each function is declared once"))
        .collect();

    PureModule {
        pages,
        functions,
        records,
        enums,
        expr_ids,
        var_ids,
    }
}

fn collect_callees(expr: &PureExpr, out: &mut BTreeSet<FunctionName>) {
    if let PureExpr::FunctionCall { function_name, .. } = expr {
        out.insert(function_name.clone());
    }
    expr.for_each_child(&mut |child| collect_callees(child, out));
}

fn inline(
    expr: PureExpr,
    decls: &HashMap<FunctionName, PureFunctionDeclaration>,
    recursive: &HashSet<FunctionName>,
    expr_ids: &mut ExprIdCounter,
    var_ids: &mut VarIdCounter,
) -> PureExpr {
    match expr {
        PureExpr::FunctionCall {
            function_name,
            args,
            kind,
            id,
        } => {
            let args: Vec<PureArgument> = args
                .into_iter()
                .map(|arg| PureArgument {
                    name: arg.name,
                    expr: inline(arg.expr, decls, recursive, expr_ids, var_ids),
                })
                .collect();

            let callee = match decls.get(&function_name) {
                Some(decl) if !recursive.contains(&function_name) => decl,
                _ => {
                    return PureExpr::FunctionCall {
                        function_name,
                        args,
                        kind,
                        id,
                    };
                }
            };

            match instantiate(callee, args, expr_ids, var_ids) {
                Ok(body) => body,
                Err(args) => PureExpr::FunctionCall {
                    function_name,
                    args,
                    kind,
                    id,
                },
            }
        }

        expr => expr.map_children(&mut |child| inline(child, decls, recursive, expr_ids, var_ids)),
    }
}

/// Build a copy of the callee's body with `args` bound to its parameters.
///
/// Hands the arguments back when the call does not supply every parameter.
fn instantiate(
    decl: &PureFunctionDeclaration,
    mut args: Vec<PureArgument>,
    expr_ids: &mut ExprIdCounter,
    var_ids: &mut VarIdCounter,
) -> Result<PureExpr, Vec<PureArgument>> {
    if decl
        .parameters
        .iter()
        .any(|param| !args.iter().any(|arg| arg.name == param.name))
    {
        return Err(args);
    }

    // Freshen the body, parameters included, so this copy shares no binder
    // with any other.
    let mut renames: HashMap<VarId, IrVar> = decl
        .parameters
        .iter()
        .map(|param| (param.var.id, IrVar::new(var_ids.next())))
        .collect();
    let mut body = freshen(decl.body.clone(), &mut renames, expr_ids, var_ids);

    let mut reads = HashMap::new();
    count_reads(&body, 0, &mut reads);

    for param in decl.parameters.iter().rev() {
        let index = args
            .iter()
            .position(|arg| arg.name == param.name)
            .expect("every parameter has an argument, checked above");
        let value = args.remove(index).expr;
        let var = renames[&param.var.id];
        let linear = matches!(reads.get(&var.id), Some(&(1, 0)));
        body = if linear {
            replace_once(body, var.id, value)
        } else if is_trivial(&value) {
            replace_all(body, var.id, &value, expr_ids)
        } else {
            PureExpr::Let {
                var,
                value: Box::new(value),
                kind: decl.return_type.clone(),
                body: Box::new(body),
                id: expr_ids.next(),
            }
        };
    }

    Ok(body)
}

/// An expression that costs nothing to evaluate and so can be duplicated, or
/// dropped, freely.
fn is_trivial(expr: &PureExpr) -> bool {
    matches!(
        expr,
        PureExpr::VariableReference { .. }
            | PureExpr::StringLiteral { .. }
            | PureExpr::IntLiteral { .. }
            | PureExpr::FloatLiteral { .. }
            | PureExpr::BooleanLiteral { .. }
            | PureExpr::FragmentRaw { .. }
    )
}

/// Give every binder in `expr` a fresh VarId and every node a fresh ExprId,
/// rewriting variable references through `renames`.
fn freshen(
    expr: PureExpr,
    renames: &mut HashMap<VarId, IrVar>,
    expr_ids: &mut ExprIdCounter,
    var_ids: &mut VarIdCounter,
) -> PureExpr {
    match expr {
        PureExpr::Let {
            var,
            value,
            body,
            kind,
            id: _,
        } => {
            // The value is outside the binding, so freshen it before the
            // rename for `var` is in scope.
            let value = Box::new(freshen(*value, renames, expr_ids, var_ids));
            let fresh = IrVar::new(var_ids.next());
            renames.insert(var.id, fresh);
            PureExpr::Let {
                var: fresh,
                value,
                body: Box::new(freshen(*body, renames, expr_ids, var_ids)),
                kind,
                id: expr_ids.next(),
            }
        }

        PureExpr::VariableReference { value, kind, id: _ } => PureExpr::VariableReference {
            value: renames.get(&value.id).copied().unwrap_or(value),
            kind,
            id: expr_ids.next(),
        },

        // Match arms and loops bind too, and map_children does not surface
        // their binders, so rename those before descending.
        mut expr => {
            for var in binders_mut(&mut expr) {
                let fresh = IrVar::new(var_ids.next());
                renames.insert(var.id, fresh);
                *var = fresh;
            }
            let mut expr =
                expr.map_children(&mut |child| freshen(child, renames, expr_ids, var_ids));
            *expr.id_mut() = expr_ids.next();
            expr
        }
    }
}

/// The binders introduced by this node itself, other than `Let`.
fn binders_mut(expr: &mut PureExpr) -> Vec<&mut IrVar> {
    match expr {
        PureExpr::Match { match_, .. } => match match_ {
            Match::Option {
                some_arm_binding, ..
            } => some_arm_binding.iter_mut().collect(),
            Match::Enum { arms, .. } => arms
                .iter_mut()
                .flat_map(|arm| arm.bindings.iter_mut().map(|(_, var)| var))
                .collect(),
            Match::Bool { .. } => Vec::new(),
        },
        PureExpr::FragmentFor { var, .. } => var.iter_mut().collect(),
        _ => Vec::new(),
    }
}

/// Move `value` into the one read of `var`.
fn replace_once(expr: PureExpr, var: VarId, value: PureExpr) -> PureExpr {
    fn go(expr: PureExpr, var: VarId, value: &mut Option<PureExpr>) -> PureExpr {
        match expr {
            PureExpr::VariableReference { value: v, kind, id } if v.id == var => {
                match value.take() {
                    Some(replacement) => replacement,
                    None => PureExpr::VariableReference { value: v, kind, id },
                }
            }
            expr => expr.map_children(&mut |child| go(child, var, value)),
        }
    }
    let mut value = Some(value);
    go(expr, var, &mut value)
}

/// Copy `value` into every read of `var`. Only for values cheap enough to
/// duplicate: each copy is a whole new expression and gets its own ExprId.
fn replace_all(
    expr: PureExpr,
    var: VarId,
    value: &PureExpr,
    expr_ids: &mut ExprIdCounter,
) -> PureExpr {
    match expr {
        PureExpr::VariableReference { value: v, .. } if v.id == var => {
            let mut copy = value.clone();
            *copy.id_mut() = expr_ids.next();
            copy
        }
        expr => expr.map_children(&mut |child| replace_all(child, var, value, expr_ids)),
    }
}

/// Count how often each variable is read, and the shallowest loop nesting any
/// of its reads sits at.
fn count_reads(expr: &PureExpr, depth: usize, out: &mut HashMap<VarId, (usize, usize)>) {
    match expr {
        PureExpr::VariableReference { value, .. } => {
            let entry = out.entry(value.id).or_insert((0, depth));
            entry.0 += 1;
            entry.1 = entry.1.min(depth);
        }

        // A read in the body runs once per iteration, so an argument
        // substituted there would be evaluated more than once. The source is
        // evaluated once, and stays at this depth.
        PureExpr::FragmentFor { source, body, .. } => {
            match &**source {
                PureForSource::Array(array) => count_reads(array, depth, out),
                PureForSource::RangeInclusive { start, end } => {
                    count_reads(start, depth, out);
                    count_reads(end, depth, out);
                }
            }
            count_reads(body, depth + 1, out);
            return;
        }

        _ => {}
    }
    expr.for_each_child(&mut |child| count_reads(child, depth, out));
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::runtime::evaluator::evaluate_page;
    use crate::ir::runtime::random::random_value;
    use crate::ir::runtime::value::Value;
    use crate::symbols::type_name::TypeName;
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};
    use std::sync::Arc;

    use crate::hop::typing::r#type::Type;

    fn assert_every_read_is_bound(module: &PureModule) {
        fn check(expr: &PureExpr, bound: &mut HashSet<VarId>, module: &PureModule) {
            match expr {
                PureExpr::VariableReference { value, .. } => {
                    assert!(
                        bound.contains(&value.id),
                        "unbound read of v{} in\n{module}",
                        value.id
                    );
                }
                PureExpr::Let {
                    var, value, body, ..
                } => {
                    check(value, bound, module);
                    bound.insert(var.id);
                    check(body, bound, module);
                    return;
                }
                _ => {
                    for var in binders(expr) {
                        bound.insert(var.id);
                    }
                }
            }
            expr.for_each_child(&mut |child| check(child, bound, module));
        }

        /// The binders a node introduces, other than `Let`.
        fn binders(expr: &PureExpr) -> Vec<IrVar> {
            match expr {
                PureExpr::Match { match_, .. } => match match_ {
                    Match::Option {
                        some_arm_binding, ..
                    } => some_arm_binding.iter().copied().collect(),
                    Match::Enum { arms, .. } => arms
                        .iter()
                        .flat_map(|arm| arm.bindings.iter().map(|(_, var)| *var))
                        .collect(),
                    Match::Bool { .. } => Vec::new(),
                },
                PureExpr::FragmentFor { var, .. } => var.iter().copied().collect(),
                _ => Vec::new(),
            }
        }

        for page in &module.pages {
            let mut bound: HashSet<VarId> = page.parameters.iter().map(|p| p.var.id).collect();
            check(&page.body, &mut bound, module);
        }
        for function in &module.functions {
            let mut bound: HashSet<VarId> = function.parameters.iter().map(|p| p.var.id).collect();
            check(&function.body, &mut bound, module);
        }
    }

    #[test]
    fn fuzz_random_pure_modules_evaluate_identically_after_inlining() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = StdRng::seed_from_u64(u.arbitrary()?);

            let page_args: Vec<(TypeName, HashMap<VarName, Value>)> = module
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
            let before: Vec<String> = page_args
                .iter()
                .map(|(page_name, args)| evaluate_page(&module, page_name, args.clone()).unwrap())
                .collect();

            let module = inline_function_calls(module);
            assert_every_read_is_bound(&module);

            for ((page_name, args), before_output) in page_args.iter().zip(&before) {
                let after_output = evaluate_page(&module, page_name, args.clone()).unwrap();
                assert_eq!(
                    before_output, &after_output,
                    "page {page_name}\n-- before --\n{before_module}\n-- after --\n{module}"
                );
            }
            Ok(())
        });
    }

    fn check(module: PureModule, expected: Expect) {
        let before = module.to_string();
        let module = inline_function_calls(module);
        assert_every_read_is_bound(&module);
        let after = module.to_string();
        expected.assert_eq(&format!("-- before --\n{}\n-- after --\n{}", before, after));
    }

    #[test]
    fn should_inline_a_call_and_substitute_its_argument() {
        check(
            PureModuleBuilder::new()
                .function("Badge", [("label", "String")], "Fragment", |t| {
                    t.concat(vec![t.raw("<b>"), t.escape(t.var("label")), t.raw("</b>")])
                })
                .view("Main", [("title", "String")], |t| {
                    t.call("Badge", vec![("label", t.var("title"))])
                })
                .build(),
            expect![[r#"
                -- before --
                fn Badge(label@v0: String) -> Fragment {
                  concat(raw("<b>"), escape(v0), raw("</b>"))
                }
                page Main(title@v1: String) {
                  call Badge(label = v1)
                }

                -- after --
                fn Badge(label@v0: String) -> Fragment {
                  concat(raw("<b>"), escape(v0), raw("</b>"))
                }
                page Main(title@v1: String) {
                  concat(raw("<b>"), escape(v1), raw("</b>"))
                }
            "#]],
        );
    }

    #[test]
    fn should_bind_an_argument_read_more_than_once() {
        check(
            PureModuleBuilder::new()
                .function("Twice", [("body", "Fragment")], "Fragment", |t| {
                    t.concat(vec![t.var("body"), t.var("body")])
                })
                .view("Main", [("name", "String")], |t| {
                    t.call("Twice", vec![("body", t.escape(t.var("name")))])
                })
                .build(),
            expect![[r#"
                -- before --
                fn Twice(body@v0: Fragment) -> Fragment {
                  concat(v0, v0)
                }
                page Main(name@v1: String) {
                  call Twice(body = escape(v1))
                }

                -- after --
                fn Twice(body@v0: Fragment) -> Fragment {
                  concat(v0, v0)
                }
                page Main(name@v1: String) {
                  let v2 = escape(v1) in { concat(v2, v2) }
                }
            "#]],
        );
    }

    #[test]
    fn should_bind_an_argument_read_inside_a_loop() {
        check(
            PureModuleBuilder::new()
                .function("Repeat", [("body", "Fragment")], "Fragment", |t| {
                    t.fragment_for(None, t.array(vec![t.str("a"), t.str("b")]), |t| {
                        t.var("body")
                    })
                })
                .view("Main", [("name", "String")], |t| {
                    t.call("Repeat", vec![("body", t.escape(t.var("name")))])
                })
                .build(),
            expect![[r#"
                -- before --
                fn Repeat(body@v0: Fragment) -> Fragment {
                  for _ in ["a", "b"] { v0 }
                }
                page Main(name@v1: String) {
                  call Repeat(body = escape(v1))
                }

                -- after --
                fn Repeat(body@v0: Fragment) -> Fragment {
                  for _ in ["a", "b"] { v0 }
                }
                page Main(name@v1: String) {
                  let v2 = escape(v1) in { for _ in ["a", "b"] { v2 } }
                }
            "#]],
        );
    }

    #[test]
    fn should_inline_callees_before_their_callers() {
        check(
            PureModuleBuilder::new()
                .function("Inner", [("x", "String")], "Fragment", |t| {
                    t.concat(vec![t.raw("["), t.escape(t.var("x")), t.raw("]")])
                })
                .function("Outer", [("x", "String")], "Fragment", |t| {
                    t.concat(vec![
                        t.raw("<i>"),
                        t.call("Inner", vec![("x", t.var("x"))]),
                        t.raw("</i>"),
                    ])
                })
                .view("Main", [("name", "String")], |t| {
                    t.call("Outer", vec![("x", t.var("name"))])
                })
                .build(),
            expect![[r#"
                -- before --
                fn Inner(x@v0: String) -> Fragment {
                  concat(raw("["), escape(v0), raw("]"))
                }
                fn Outer(x@v1: String) -> Fragment {
                  concat(raw("<i>"), call Inner(x = v1), raw("</i>"))
                }
                page Main(name@v2: String) {
                  call Outer(x = v2)
                }

                -- after --
                fn Inner(x@v0: String) -> Fragment {
                  concat(raw("["), escape(v0), raw("]"))
                }
                fn Outer(x@v1: String) -> Fragment {
                  concat(
                    raw("<i>"),
                    concat(raw("["), escape(v1), raw("]")),
                    raw("</i>"),
                  )
                }
                page Main(name@v2: String) {
                  concat(
                    raw("<i>"),
                    concat(raw("["), escape(v2), raw("]")),
                    raw("</i>"),
                  )
                }
            "#]],
        );
    }

    /// Rewrite `caller`'s body to `raw(marker)` followed by a call to
    /// `callee`, forwarding the caller's own first parameter.
    ///
    /// The builder only lets a body call an already-declared function, so a
    /// cycle has to be closed after the fact.
    fn patch_to_call(module: &mut PureModule, caller: &str, callee: &str, marker: &str) {
        let mut expr_ids = module.expr_ids;
        let decl = module
            .functions
            .iter_mut()
            .find(|f| f.name.as_str() == caller)
            .unwrap_or_else(|| panic!("{caller} is declared"));
        let param = decl.parameters[0].clone();
        let name = param.name.clone();
        decl.body = PureExpr::FragmentConcat {
            parts: vec![
                PureExpr::FragmentRaw {
                    content: marker.to_string(),
                    id: expr_ids.next(),
                },
                PureExpr::FunctionCall {
                    function_name: FunctionName::new(callee).unwrap(),
                    args: vec![PureArgument {
                        name,
                        expr: PureExpr::VariableReference {
                            value: param.var,
                            kind: param.typ,
                            id: expr_ids.next(),
                        },
                    }],
                    kind: Arc::new(Type::Fragment),
                    id: expr_ids.next(),
                },
            ],
            id: expr_ids.next(),
        };
        module.expr_ids = expr_ids;
    }

    #[test]
    fn should_leave_a_self_recursive_function_alone() {
        let mut module = PureModuleBuilder::new()
            .function("Loop", [("n", "Int")], "Fragment", |t| t.raw("placeholder"))
            .view_no_params("Main", |t| t.call("Loop", vec![("n", t.int(3))]))
            .build();
        patch_to_call(&mut module, "Loop", "Loop", "<li>");
        check(
            module,
            expect![[r#"
            -- before --
            fn Loop(n@v0: Int) -> Fragment {
              concat(raw("<li>"), call Loop(n = v0))
            }
            page Main() {
              call Loop(n = 3)
            }

            -- after --
            fn Loop(n@v0: Int) -> Fragment {
              concat(raw("<li>"), call Loop(n = v0))
            }
            page Main() {
              call Loop(n = 3)
            }
        "#]],
        );
    }

    #[test]
    fn should_leave_mutually_recursive_functions_alone() {
        let mut module = PureModuleBuilder::new()
            .function("Ping", [("n", "Int")], "Fragment", |t| t.raw("placeholder"))
            .function("Pong", [("n", "Int")], "Fragment", |t| {
                t.call("Ping", vec![("n", t.var("n"))])
            })
            .view_no_params("Main", |t| t.call("Pong", vec![("n", t.int(3))]))
            .build();
        patch_to_call(&mut module, "Ping", "Pong", "<ping>");
        check(
            module,
            expect![[r#"
            -- before --
            fn Ping(n@v0: Int) -> Fragment {
              concat(raw("<ping>"), call Pong(n = v0))
            }
            fn Pong(n@v1: Int) -> Fragment {
              call Ping(n = v1)
            }
            page Main() {
              call Pong(n = 3)
            }

            -- after --
            fn Ping(n@v0: Int) -> Fragment {
              concat(raw("<ping>"), call Pong(n = v0))
            }
            fn Pong(n@v1: Int) -> Fragment {
              call Ping(n = v1)
            }
            page Main() {
              call Pong(n = 3)
            }
        "#]],
        );
    }
}
