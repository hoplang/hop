use crate::html::write_escaped_html;
use crate::ir::expr_id::{ExprId, ExprIdCounter};
use crate::ir::pure_module::PureExpr;

/// A pass that normalizes fragment structure.
///
/// - A FragmentEscape of a constant string becomes a FragmentRaw with the
///   escaping applied at compile time.
/// - A FragmentEscape distributes over StringConcat: escaping is a monoid
///   homomorphism, escape(a + b) = escape(a) <> escape(b), so every constant
///   part can be escaped at compile time however deeply it nests.
/// - Nested FragmentConcats are flattened.
/// - Adjacent FragmentRaws are merged while the combined length stays below
///   the limit.
pub fn normalize_fragments(expr: PureExpr, expr_ids: &mut ExprIdCounter, limit: usize) -> PureExpr {
    transform(expr, expr_ids, limit)
}

fn transform(expr: PureExpr, expr_ids: &mut ExprIdCounter, limit: usize) -> PureExpr {
    match expr {
        PureExpr::FragmentEscape { expr: inner, id } => {
            let inner = transform(*inner, expr_ids, limit);
            push_escape(inner, id, expr_ids, limit)
        }

        PureExpr::FragmentConcat { parts, id } => PureExpr::FragmentConcat {
            parts: flatten_and_merge(
                parts
                    .into_iter()
                    .map(|part| transform(part, expr_ids, limit)),
                limit,
            ),
            id,
        },

        expr => expr.map_children(&mut |child| transform(child, expr_ids, limit)),
    }
}

/// Push an escape down into an already-normalized String-typed expression.
fn push_escape(
    inner: PureExpr,
    id: ExprId,
    expr_ids: &mut ExprIdCounter,
    limit: usize,
) -> PureExpr {
    match inner {
        PureExpr::StringLiteral { value, .. } => {
            let mut content = String::new();
            write_escaped_html(value.as_str(), &mut content);
            PureExpr::FragmentRaw { content, id }
        }
        PureExpr::StringConcat { parts, .. } => {
            let escaped: Vec<PureExpr> = parts
                .into_iter()
                .map(|part| {
                    let part_id = expr_ids.next();
                    push_escape(part, part_id, expr_ids, limit)
                })
                .collect();
            PureExpr::FragmentConcat {
                parts: flatten_and_merge(escaped, limit),
                id,
            }
        }
        inner => PureExpr::FragmentEscape {
            expr: Box::new(inner),
            id,
        },
    }
}

/// Flatten already-normalized parts one level and greedily merge adjacent
/// FragmentRaws whose combined length stays below the limit. A merged raw keeps
/// the id of its first chunk.
fn flatten_and_merge(parts: impl IntoIterator<Item = PureExpr>, limit: usize) -> Vec<PureExpr> {
    let mut merged: Vec<PureExpr> = Vec::new();
    for part in parts {
        let subparts = match part {
            PureExpr::FragmentConcat { parts, .. } => parts,
            part => vec![part],
        };
        for part in subparts {
            match (merged.last_mut(), part) {
                (
                    Some(PureExpr::FragmentRaw {
                        content: accumulated,
                        ..
                    }),
                    PureExpr::FragmentRaw { content, .. },
                ) if accumulated.len() + content.len() < limit => {
                    accumulated.push_str(&content);
                }
                (_, part) => merged.push(part),
            }
        }
    }
    merged
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ir::pure_module::{PureComponentDeclaration, PureModule, PureViewDeclaration};
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::runtime::evaluator::evaluate_view;
    use crate::ir::runtime::random::random_value;
    use crate::ir::runtime::value::Value;
    use crate::symbols::type_name::TypeName;
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::StdRng};

    #[test]
    fn fuzz_random_pure_modules_evaluate_identically_after_normalization() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = StdRng::seed_from_u64(u.arbitrary()?);

            let view_args: Vec<(TypeName, HashMap<VarName, Value>)> = module
                .views
                .iter()
                .map(|view| {
                    let args = view
                        .parameters
                        .iter()
                        .map(|p| {
                            (
                                p.name().clone(),
                                random_value(&mut rng, &p.typ, None, &registry),
                            )
                        })
                        .collect();
                    (view.name.clone(), args)
                })
                .collect();

            let before: Vec<String> = view_args
                .iter()
                .map(|(view_name, args)| evaluate_view(&module, view_name, args.clone()).unwrap())
                .collect();

            let module = run(module, 60);

            let after: Vec<String> = view_args
                .iter()
                .map(|(view_name, args)| evaluate_view(&module, view_name, args.clone()).unwrap())
                .collect();

            assert_eq!(before, after);
            Ok(())
        });
    }

    /// Apply the pass to every view and component body.
    fn run(module: PureModule, limit: usize) -> PureModule {
        let mut expr_ids = module.expr_ids;
        let views = module
            .views
            .into_iter()
            .map(|view| PureViewDeclaration {
                name: view.name,
                parameters: view.parameters,
                body: normalize_fragments(view.body, &mut expr_ids, limit),
            })
            .collect();
        let components = module
            .components
            .into_iter()
            .map(|component| PureComponentDeclaration {
                name: component.name,
                parameters: component.parameters,
                body: normalize_fragments(component.body, &mut expr_ids, limit),
            })
            .collect();
        PureModule {
            views,
            components,
            records: module.records,
            enums: module.enums,
            expr_ids,
            var_ids: module.var_ids,
        }
    }

    fn check(module: PureModule, limit: usize, expected: Expect) {
        let before = module.to_string();
        let module = run(module, limit);
        let after = module.to_string();
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_escape_constant_string_at_compile_time() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| t.concat(vec![t.escape(t.str("<b> & \"q\""))]))
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test() {
                  concat(escape("<b> & \"q\""))
                }

                -- after --
                view Test() {
                  concat(raw("&lt;b&gt; &amp; &quot;q&quot;"))
                }
            "#]],
        );
    }

    #[test]
    fn should_merge_adjacent_raws() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.raw("<div>"), t.raw("Hello"), t.raw("</div>")])
                })
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test() {
                  concat(raw("<div>"), raw("Hello"), raw("</div>"))
                }

                -- after --
                view Test() {
                  concat(raw("<div>Hello</div>"))
                }
            "#]],
        );
    }

    #[test]
    fn should_respect_merge_limit() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.raw("aaaa"), t.raw("bbbb"), t.raw("cc")])
                })
                .build(),
            8,
            expect![[r#"
                -- before --
                view Test() {
                  concat(raw("aaaa"), raw("bbbb"), raw("cc"))
                }

                -- after --
                view Test() {
                  concat(raw("aaaa"), raw("bbbbcc"))
                }
            "#]],
        );
    }

    #[test]
    fn should_flatten_nested_concats() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![
                        t.raw("a"),
                        t.concat(vec![t.raw("b"), t.concat(vec![t.raw("c")])]),
                    ])
                })
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test() {
                  concat(raw("a"), concat(raw("b"), concat(raw("c"))))
                }

                -- after --
                view Test() {
                  concat(raw("abc"))
                }
            "#]],
        );
    }

    #[test]
    fn should_split_escape_of_concat_and_escape_constant_halves() {
        check(
            PureModuleBuilder::new()
                .view("Test", [("name", "String")], |t| {
                    t.concat(vec![
                        t.escape(t.string_concat(vec![t.str("Hi <"), t.var("name")])),
                    ])
                })
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test(name@v0: String) {
                  concat(escape(("Hi <" + v0)))
                }

                -- after --
                view Test(name@v0: String) {
                  concat(raw("Hi &lt;"), escape(v0))
                }
            "#]],
        );
    }

    #[test]
    fn should_collapse_nested_constant_string_concat_to_one_raw() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.escape(t.string_concat(vec![
                        t.string_concat(vec![t.str("a<"), t.str("b>")]),
                        t.str("c&"),
                    ]))])
                })
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test() {
                  concat(escape((("a<" + "b>") + "c&")))
                }

                -- after --
                view Test() {
                  concat(raw("a&lt;b&gt;c&amp;"))
                }
            "#]],
        );
    }

    #[test]
    fn should_merge_split_escape_with_neighboring_raws() {
        check(
            PureModuleBuilder::new()
                .view("Test", [("name", "String")], |t| {
                    t.concat(vec![
                        t.raw("<p>"),
                        t.escape(t.string_concat(vec![t.str("Hi, "), t.var("name")])),
                        t.raw("</p>"),
                    ])
                })
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test(name@v0: String) {
                  concat(raw("<p>"), escape(("Hi, " + v0)), raw("</p>"))
                }

                -- after --
                view Test(name@v0: String) {
                  concat(raw("<p>Hi, "), escape(v0), raw("</p>"))
                }
            "#]],
        );
    }

    #[test]
    fn should_normalize_inside_for_loop_bodies() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.fragment_for(
                        Some("item"),
                        t.array(vec![t.str("a")]),
                        |t| t.concat(vec![t.raw("<li>"), t.escape(t.var("item")), t.raw("</li>")]),
                    )])
                })
                .build(),
            usize::MAX,
            expect![[r#"
                -- before --
                view Test() {
                  concat(
                    concat(
                      raw("<li>"),
                      escape(v0),
                      raw("</li>"),
                    ) for v0 in ["a"],
                  )
                }

                -- after --
                view Test() {
                  concat(
                    concat(
                      raw("<li>"),
                      escape(v0),
                      raw("</li>"),
                    ) for v0 in ["a"],
                  )
                }
            "#]],
        );
    }
}
