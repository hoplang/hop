use std::collections::HashMap;

use crate::ir::ir_var::IrVar;
use crate::ir::pure_module::PureExpr;
use crate::ir::var_id::VarId;

/// A pass that propagates the variable of a let binding that is an alias,
/// e.g. `let x = y;`
///
/// The emptied `let` is left for `eliminate_unused_variable_declarations`.
pub fn propagate_variable_names(expr: PureExpr) -> PureExpr {
    let mut renames = HashMap::new();
    rename(expr, &mut renames)
}

fn rename(expr: PureExpr, renames: &mut HashMap<VarId, IrVar>) -> PureExpr {
    match expr {
        PureExpr::Let {
            var,
            value,
            body,
            typ,
            id,
        } => {
            let value = rename(*value, renames);
            if let PureExpr::VariableReference { value: source, .. } = &value {
                renames.insert(var.id, *source);
            }
            PureExpr::Let {
                var,
                value: Box::new(value),
                body: Box::new(rename(*body, renames)),
                typ,
                id,
            }
        }

        PureExpr::VariableReference { value, typ, id } => PureExpr::VariableReference {
            value: renames.get(&value.id).copied().unwrap_or(value),
            typ,
            id,
        },

        expr => expr.map_children(&mut |child| rename(child, renames)),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::pure_module::{PureFunctionDeclaration, PureModule, PurePageDeclaration};
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::runtime::evaluator::evaluate_page;
    use crate::ir::runtime::random::random_value;
    use crate::ir::runtime::value::Value;
    use crate::symbols::type_name::TypeName;
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};

    #[test]
    fn fuzz_random_pure_modules_evaluate_identically_after_propagation() {
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

            let before: Vec<String> = page_args
                .iter()
                .map(|(page_name, args)| evaluate_page(&module, page_name, args.clone()).unwrap())
                .collect();

            let module = run(module);

            let after: Vec<String> = page_args
                .iter()
                .map(|(page_name, args)| evaluate_page(&module, page_name, args.clone()).unwrap())
                .collect();

            assert_eq!(before, after);
            Ok(())
        });
    }

    fn run(module: PureModule) -> PureModule {
        PureModule {
            pages: module
                .pages
                .into_iter()
                .map(|page| PurePageDeclaration {
                    name: page.name,
                    parameters: page.parameters,
                    body: propagate_variable_names(page.body),
                })
                .collect(),
            functions: module
                .functions
                .into_iter()
                .map(|function| PureFunctionDeclaration {
                    function: function.function,
                    parameters: function.parameters,
                    return_type: function.return_type,
                    body: propagate_variable_names(function.body),
                })
                .collect(),
            expr_ids: module.expr_ids,
            var_ids: module.var_ids,
        }
    }

    fn check(module: PureModule, expected: Expect) {
        let before = module.to_string();
        let module = run(module);
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_propagate_the_original_name_of_an_alias_let() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("title", "String")], |t| {
                    t.let_expr("alias", t.var("title"), |t| t.escape(t.var("alias")))
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(title@v0: String) {
                  let v1 = v0 in { escape(v1) }
                }

                -- after --
                page Test(title@v0: String) {
                  let v1 = v0 in { escape(v0) }
                }
            "#]],
        );
    }

    #[test]
    fn should_collapse_a_chain_of_renamings_in_one_pass() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("title", "String")], |t| {
                    t.let_expr("first", t.var("title"), |t| {
                        t.let_expr("second", t.var("first"), |t| t.escape(t.var("second")))
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(title@v0: String) {
                  let v1 = v0 in { let v2 = v1 in { escape(v2) } }
                }

                -- after --
                page Test(title@v0: String) {
                  let v1 = v0 in { let v2 = v0 in { escape(v0) } }
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_into_a_nested_binder() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("titles", "Array[String]")], |t| {
                    t.html_for(Some("title"), t.var("titles"), |t| {
                        t.let_expr("alias", t.var("title"), |t| t.escape(t.var("alias")))
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(titles@v0: Array[String]) {
                  for v1 in v0 { let v2 = v1 in { escape(v2) } }
                }

                -- after --
                page Test(titles@v0: Array[String]) {
                  for v1 in v0 { let v2 = v1 in { escape(v1) } }
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_a_let_whose_value_is_not_a_variable() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("title", "String")], |t| {
                    t.let_expr(
                        "shouted",
                        t.string_concat(vec![t.var("title"), t.str("!")]),
                        |t| t.escape(t.var("shouted")),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(title@v0: String) {
                  let v1 = (v0 + "!") in { escape(v1) }
                }

                -- after --
                page Test(title@v0: String) {
                  let v1 = (v0 + "!") in { escape(v1) }
                }
            "#]],
        );
    }
}
