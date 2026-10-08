use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::ir::document_shell::DocumentShell;
use crate::ir::ir_match::{EnumPattern, Match};
use crate::ir::pure_module::{
    PureAttribute, PureExpr, PureForSource, PureFunctionDeclaration, PureModule,
};
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::html_node::{HtmlAttribute, HtmlNode, write_html};
use crate::ir::runtime::value::Value;
use crate::ir::runtime::variable_env::VariableEnv;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::type_name::TypeName;
use std::collections::HashMap;

pub fn evaluate_page(
    module: &PureModule,
    page_name: &TypeName,
    mut args: HashMap<AttributeName, Value>,
    shell: Option<&DocumentShell>,
) -> Result<String, EvalError> {
    let page = module
        .pages
        .iter()
        .find(|page| &page.name == page_name)
        .ok_or_else(|| EvalError::PageNotFound {
            page: page_name.clone(),
        })?;

    let mut env = VariableEnv::new();

    for param in &page.parameters {
        if let Some(value) = args.remove(param.name()) {
            env.insert(param.var.id, value);
        } else {
            return Err(EvalError::MissingParameter {
                page: page.name.clone(),
                param: param.name().clone(),
            });
        }
    }

    let head = evaluate_expr(&page.head, &mut env, &module.functions).unwrap_html();
    let body = evaluate_expr(&page.body, &mut env, &module.functions).unwrap_html();

    let mut html = String::new();
    match shell {
        Some(shell) => {
            html.push_str(shell.before_head);
            write_html(&head, &mut html);
            html.push_str(&shell.after_head);
            write_html(&body, &mut html);
            html.push_str(shell.after_body);
        }
        None => {
            write_html(&head, &mut html);
            write_html(&body, &mut html);
        }
    }
    Ok(html)
}

fn evaluate_expr(
    expr: &PureExpr,
    env: &mut VariableEnv,
    function_decls: &[PureFunctionDeclaration],
) -> Value {
    match expr {
        PureExpr::Let {
            var, value, body, ..
        } => {
            let value = evaluate_expr(value, env, function_decls);
            env.insert(var.id, value);
            let result = evaluate_expr(body, env, function_decls);
            env.remove(&var.id);
            result
        }

        PureExpr::Match {
            match_: Match::Enum { subject, arms },
            ..
        } => {
            let (variant_name, fields) = evaluate_expr(subject, env, function_decls).unwrap_enum();
            let mut matching_arms = arms.iter().filter(|arm| {
                let EnumPattern::Variant {
                    variant_name: pattern_variant,
                    ..
                } = &arm.pattern;
                variant_name == *pattern_variant
            });
            let arm = matching_arms
                .next()
                .unwrap_or_else(|| panic!("No matching arm found for variant '{}'", variant_name));
            assert!(
                matching_arms.next().is_none(),
                "Multiple matching arms found for variant '{}'",
                variant_name
            );
            for (field_name, var) in &arm.bindings {
                let field = fields.get(field_name).unwrap_or_else(|| {
                    panic!(
                        "Field '{}' not found in enum variant '{}'",
                        field_name, variant_name
                    )
                });
                env.insert(var.id, field.clone());
            }
            let result = evaluate_expr(&arm.body, env, function_decls);
            for (_, var) in &arm.bindings {
                env.remove(&var.id);
            }
            result
        }

        PureExpr::Match {
            match_:
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                },
            ..
        } => {
            if evaluate_expr(subject, env, function_decls).unwrap_bool() {
                evaluate_expr(true_body, env, function_decls)
            } else {
                evaluate_expr(false_body, env, function_decls)
            }
        }

        PureExpr::Match {
            match_:
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                },
            ..
        } => match evaluate_expr(subject, env, function_decls).unwrap_option() {
            Some(inner) => {
                if let Some(var) = some_arm_binding {
                    env.insert(var.id, *inner);
                }
                let result = evaluate_expr(some_arm_body, env, function_decls);
                if let Some(var) = some_arm_binding {
                    env.remove(&var.id);
                }
                result
            }
            None => evaluate_expr(none_arm_body, env, function_decls),
        },

        PureExpr::VariableReference { value, .. } => env.get(&value.id).clone(),

        PureExpr::FieldAccess { record, field, .. } => {
            let mut record = evaluate_expr(record, env, function_decls).unwrap_record();
            record
                .remove(field)
                .unwrap_or_else(|| panic!("Field '{}' not found in record", field))
        }

        PureExpr::StringLiteral { value, .. } => Value::String(value.to_string()),

        PureExpr::HtmlText { content, .. } => {
            Value::Html(vec![HtmlNode::Text(content.to_string())])
        }

        PureExpr::HtmlEscape { expr, .. } => {
            let text = evaluate_expr(expr, env, function_decls).unwrap_string();
            Value::Html(vec![HtmlNode::Escape(text)])
        }

        PureExpr::HtmlElement {
            element,
            attributes,
            children,
            ..
        } => {
            let mut rendered = Vec::new();
            for attribute in attributes {
                match attribute {
                    PureAttribute::Value { name, value } => {
                        let value = evaluate_expr(value, env, function_decls).unwrap_string();
                        rendered.push(HtmlAttribute {
                            name: name.clone(),
                            value: Some(value),
                        });
                    }
                    PureAttribute::Presence { name, present } => {
                        if evaluate_expr(present, env, function_decls).unwrap_bool() {
                            rendered.push(HtmlAttribute {
                                name: name.clone(),
                                value: None,
                            });
                        }
                    }
                }
            }
            let children = if element.is_void() {
                Vec::new()
            } else {
                evaluate_expr(children, env, function_decls).unwrap_html()
            };
            Value::Html(vec![HtmlNode::Element {
                element: element.clone(),
                attributes: rendered,
                children,
            }])
        }

        PureExpr::HtmlConcat { parts, .. } => {
            let mut nodes = Vec::new();
            for part in parts {
                nodes.extend(evaluate_expr(part, env, function_decls).unwrap_html());
            }
            Value::Html(nodes)
        }

        PureExpr::HtmlFor {
            var, source, body, ..
        } => {
            let items = match source.as_ref() {
                PureForSource::Array(array) => {
                    evaluate_expr(array, env, function_decls).unwrap_array()
                }
                PureForSource::RangeInclusive { start, end } => {
                    let start = evaluate_expr(start, env, function_decls).unwrap_int();
                    let end = evaluate_expr(end, env, function_decls).unwrap_int();
                    (start..=end).map(Value::Int).collect()
                }
            };
            let mut nodes = Vec::new();
            for item in items {
                if let Some(var) = var {
                    env.insert(var.id, item);
                }
                nodes.extend(evaluate_expr(body, env, function_decls).unwrap_html());
                if let Some(var) = var {
                    env.remove(&var.id);
                }
            }
            Value::Html(nodes)
        }

        PureExpr::Call { function, args, .. } => {
            let func = function_decls
                .iter()
                .find(|decl| decl.function.id == function.id)
                .unwrap_or_else(|| panic!("Function '{}' not found in module", function));
            for (index, arg) in args.iter().enumerate() {
                assert!(
                    func.parameters.iter().any(|p| p.name() == &arg.name),
                    "Unknown argument '{}' for function '{}'",
                    arg.name,
                    function
                );
                assert!(
                    !args[..index].iter().any(|earlier| earlier.name == arg.name),
                    "Duplicate argument '{}' for function '{}'",
                    arg.name,
                    function
                );
            }
            let mut callee_env = VariableEnv::new();
            for param in &func.parameters {
                if let Some(arg) = args.iter().find(|arg| &arg.name == param.name()) {
                    let value = evaluate_expr(&arg.expr, env, function_decls);
                    callee_env.insert(param.var.id, value);
                } else {
                    panic!(
                        "Missing required parameter '{}' for function '{}'",
                        param.name(),
                        function
                    );
                }
            }
            evaluate_expr(&func.body, &mut callee_env, function_decls)
        }

        PureExpr::BoolLiteral { value, .. } => Value::Bool(*value),

        PureExpr::FloatLiteral { value, .. } => Value::Float(*value),

        PureExpr::IntLiteral { value, .. } => Value::Int(*value),

        PureExpr::Array { elements, .. } => {
            let mut array = Vec::new();
            for element in elements {
                array.push(evaluate_expr(element, env, function_decls));
            }
            Value::Array(array)
        }

        PureExpr::Tuple { elements, .. } => {
            let mut tuple = Vec::new();
            for element in elements {
                tuple.push(evaluate_expr(element, env, function_decls));
            }
            Value::Tuple(tuple)
        }

        PureExpr::TupleIndex { tuple, index, .. } => {
            let tuple = evaluate_expr(tuple, env, function_decls).unwrap_tuple();
            tuple
                .into_iter()
                .nth(*index)
                .unwrap_or_else(|| panic!("Index {} is out of range for the tuple", index))
        }

        PureExpr::Record { fields, .. } => {
            let mut record = HashMap::new();
            for (field_name, field) in fields {
                let field = evaluate_expr(field, env, function_decls);
                record.insert(field_name.clone(), field);
            }
            Value::Record(record)
        }

        PureExpr::Enum {
            variant_name,
            fields,
            ..
        } => {
            let mut field_values = HashMap::new();
            for (field_name, field) in fields {
                let field = evaluate_expr(field, env, function_decls);
                field_values.insert(field_name.clone(), field);
            }
            Value::Enum {
                variant_name: variant_name.clone(),
                fields: field_values,
            }
        }

        PureExpr::Option { value, .. } => Value::Option(
            value
                .as_ref()
                .map(|inner| Box::new(evaluate_expr(inner, env, function_decls))),
        ),

        PureExpr::StringConcat { parts, .. } => {
            let mut result = String::new();
            for part in parts {
                result.push_str(&evaluate_expr(part, env, function_decls).unwrap_string());
            }
            Value::String(result)
        }

        PureExpr::NumericAdd {
            left,
            right,
            operand_types,
            ..
        } => {
            let left = evaluate_expr(left, env, function_decls);
            let right = evaluate_expr(right, env, function_decls);
            match operand_types {
                NumericType::Int => Value::Int(left.unwrap_int().wrapping_add(right.unwrap_int())),
                NumericType::Float => Value::Float(left.unwrap_float() + right.unwrap_float()),
            }
        }

        PureExpr::NumericSubtract {
            left,
            right,
            operand_types,
            ..
        } => {
            let left = evaluate_expr(left, env, function_decls);
            let right = evaluate_expr(right, env, function_decls);
            match operand_types {
                NumericType::Int => Value::Int(left.unwrap_int().wrapping_sub(right.unwrap_int())),
                NumericType::Float => Value::Float(left.unwrap_float() - right.unwrap_float()),
            }
        }

        PureExpr::NumericMultiply {
            left,
            right,
            operand_types,
            ..
        } => {
            let left = evaluate_expr(left, env, function_decls);
            let right = evaluate_expr(right, env, function_decls);
            match operand_types {
                NumericType::Int => Value::Int(left.unwrap_int().wrapping_mul(right.unwrap_int())),
                NumericType::Float => Value::Float(left.unwrap_float() * right.unwrap_float()),
            }
        }

        PureExpr::NumericNegation {
            operand,
            operand_type,
            ..
        } => {
            let operand = evaluate_expr(operand, env, function_decls);
            match operand_type {
                NumericType::Int => Value::Int(operand.unwrap_int().wrapping_neg()),
                NumericType::Float => Value::Float(-operand.unwrap_float()),
            }
        }

        PureExpr::BoolNegation { operand, .. } => {
            let operand = evaluate_expr(operand, env, function_decls).unwrap_bool();
            Value::Bool(!operand)
        }

        PureExpr::BoolLogicalAnd { left, right, .. } => {
            if evaluate_expr(left, env, function_decls).unwrap_bool() {
                let right = evaluate_expr(right, env, function_decls).unwrap_bool();
                Value::Bool(right)
            } else {
                Value::Bool(false)
            }
        }

        PureExpr::BoolLogicalOr { left, right, .. } => {
            if evaluate_expr(left, env, function_decls).unwrap_bool() {
                Value::Bool(true)
            } else {
                let right = evaluate_expr(right, env, function_decls).unwrap_bool();
                Value::Bool(right)
            }
        }

        PureExpr::Equals {
            left,
            right,
            operand_types,
            ..
        } => {
            let left = evaluate_expr(left, env, function_decls);
            let right = evaluate_expr(right, env, function_decls);
            match operand_types {
                EquatableType::Bool => Value::Bool(left.unwrap_bool() == right.unwrap_bool()),
                EquatableType::String => Value::Bool(left.unwrap_string() == right.unwrap_string()),
                EquatableType::Int => Value::Bool(left.unwrap_int() == right.unwrap_int()),
                EquatableType::Float => Value::Bool(left.unwrap_float() == right.unwrap_float()),
            }
        }

        PureExpr::LessThan {
            left,
            right,
            operand_types,
            ..
        } => {
            let left = evaluate_expr(left, env, function_decls);
            let right = evaluate_expr(right, env, function_decls);
            match operand_types {
                ComparableType::Int => Value::Bool(left.unwrap_int() < right.unwrap_int()),
                ComparableType::Float => Value::Bool(left.unwrap_float() < right.unwrap_float()),
            }
        }

        PureExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => {
            let left = evaluate_expr(left, env, function_decls);
            let right = evaluate_expr(right, env, function_decls);
            match operand_types {
                ComparableType::Int => Value::Bool(left.unwrap_int() <= right.unwrap_int()),
                ComparableType::Float => Value::Bool(left.unwrap_float() <= right.unwrap_float()),
            }
        }

        PureExpr::ArrayLength { array, .. } => {
            let array = evaluate_expr(array, env, function_decls).unwrap_array();
            Value::Int(array.len() as i32)
        }

        PureExpr::ArrayIsEmpty { array, .. } => {
            let array = evaluate_expr(array, env, function_decls).unwrap_array();
            Value::Bool(array.is_empty())
        }

        PureExpr::StringIsEmpty { string, .. } => {
            let string = evaluate_expr(string, env, function_decls).unwrap_string();
            Value::Bool(string.is_empty())
        }

        PureExpr::OptionIsSome { option, .. } => {
            let option = evaluate_expr(option, env, function_decls).unwrap_option();
            Value::Bool(option.is_some())
        }

        PureExpr::OptionIsNone { option, .. } => {
            let option = evaluate_expr(option, env, function_decls).unwrap_option();
            Value::Bool(option.is_none())
        }

        PureExpr::IntToString { value, .. } => {
            let value = evaluate_expr(value, env, function_decls).unwrap_int();
            Value::String(value.to_string())
        }

        PureExpr::FloatToInt { value, .. } => {
            let value = evaluate_expr(value, env, function_decls).unwrap_float();
            Value::Int(value as i32)
        }

        PureExpr::IntToFloat { value, .. } => {
            let value = evaluate_expr(value, env, function_decls).unwrap_int();
            Value::Float(value as f64)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::runtime::random::random_value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_without_panicking() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            for page in &module.pages {
                let args: HashMap<AttributeName, Value> = page
                    .parameters
                    .iter()
                    .map(|p| {
                        (
                            p.name().clone(),
                            random_value(&mut rng, &p.typ, None, &registry),
                        )
                    })
                    .collect();
                evaluate_page(&module, &page.name, args, None).unwrap();
            }
            Ok(())
        });
    }

    fn check(module: PureModule, args: Vec<(&str, Value)>, expected: Expect) {
        let before = module.to_string();
        let args_map: HashMap<AttributeName, Value> = args
            .into_iter()
            .map(|(k, v)| (AttributeName::parse(k).unwrap(), v))
            .collect();
        let page_name = module.pages[0].name.clone();
        let after =
            evaluate_page(&module, &page_name, args_map, None).expect("Evaluation should succeed");

        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_hold_an_empty_tuple_in_a_record_field() {
        check(
            PureModuleBuilder::new()
                .record("Holder", [("nothing", "()")])
                .page_no_params("Test", |t| {
                    let held = t.record("Holder", vec![("nothing", t.tuple(vec![]))]);
                    t.escape(t.int_to_string(t.array_length(
                        t.array_typed(t.resolve_type("()"), vec![t.field_access(held, "nothing")]),
                    )))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  escape([Holder {nothing: ()}.nothing].len().to_string())
                }

                -- after --
                1
            "#]],
        );
    }

    #[test]
    fn should_read_an_element_back_out_of_a_tuple() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    let pair = t.tuple(vec![t.int(1), t.str("two")]);
                    t.escape(t.tuple_index(pair, 1))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  escape((1, "two").1)
                }

                -- after --
                two
            "#]],
        );
    }

    #[test]
    fn should_read_an_element_back_out_of_a_one_tuple() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    let only = t.tuple(vec![t.str("alone")]);
                    t.escape(t.tuple_index(only, 0))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  escape(("alone",).0)
                }

                -- after --
                alone
            "#]],
        );
    }

    #[test]
    fn should_index_a_nested_tuple() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    let inner = t.tuple(vec![t.str("deep"), t.int(2)]);
                    let outer = t.tuple(vec![t.int(1), inner]);
                    t.escape(t.tuple_index(t.tuple_index(outer, 1), 0))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  escape((1, ("deep", 2)).1.0)
                }

                -- after --
                deep
            "#]],
        );
    }

    #[test]
    fn should_wrap_int_addition_at_i32_boundary() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    let sum = t.add(t.int(2147483647), t.int(1));
                    t.escape(t.int_to_string(sum))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  escape((2147483647 + 1).to_string())
                }

                -- after --
                -2147483648
            "#]],
        );
    }

    #[test]
    fn should_evaluate_an_element() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Test", |t| {
                    t.element("div", vec![], vec![t.text("Hello World")])
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  html(
                    tag: "div",
                    attrs: [],
                    children: concat(text("Hello World")),
                  )
                }

                -- after --
                <div>Hello World</div>
            "#]],
        );
    }

    #[test]
    fn should_escape_attribute_values_and_settle_boolean_attributes() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("cls", "String"), ("flag", "Bool")], |t| {
                    t.element(
                        "input",
                        vec![
                            t.attr("class", t.var("cls")),
                            t.presence("disabled", t.var("flag")),
                            t.presence("checked", t.bool(false)),
                        ],
                        vec![],
                    )
                })
                .build(),
            vec![
                ("cls", Value::String("a\"b".to_string())),
                ("flag", Value::Bool(true)),
            ],
            expect![[r#"
                -- before --
                page Test(cls@v0: String, flag@v1: Bool) {
                  html(
                    tag: "input",
                    attrs: [class: v0, disabled: v1, checked: false],
                  )
                }

                -- after --
                <input class="a&quot;b" disabled>
            "#]],
        );
    }

    #[test]
    fn should_escape_html_in_expressions() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("content", "String")], |t| {
                    t.escape(t.var("content"))
                })
                .build(),
            vec![(
                "content",
                Value::String("<script>alert('xss')</script>".to_string()),
            )],
            expect![[r#"
                -- before --
                page Test(content@v0: String) {
                  escape(v0)
                }

                -- after --
                &lt;script&gt;alert('xss')&lt;/script&gt;
            "#]],
        );
    }

    #[test]
    fn should_render_if_body_when_condition_is_true() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("show", "Bool")], |t| {
                    t.bool_match_expr(
                        t.var("show"),
                        t.element("div", vec![], vec![t.text("Visible")]),
                        t.concat(vec![]),
                    )
                })
                .build(),
            vec![("show", Value::Bool(true))],
            expect![[r#"
                -- before --
                page Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      html(
                        tag: "div",
                        attrs: [],
                        children: concat(text("Visible")),
                      )
                    }
                    false => { concat() }
                  }
                }

                -- after --
                <div>Visible</div>
            "#]],
        );
    }

    #[test]
    fn should_skip_if_body_when_condition_is_false() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("show", "Bool")], |t| {
                    t.bool_match_expr(
                        t.var("show"),
                        t.element("div", vec![], vec![t.text("Hidden")]),
                        t.concat(vec![]),
                    )
                })
                .build(),
            vec![("show", Value::Bool(false))],
            expect![[r#"
                -- before --
                page Test(show@v0: Bool) {
                  match v0 {
                    true => {
                      html(
                        tag: "div",
                        attrs: [],
                        children: concat(text("Hidden")),
                      )
                    }
                    false => { concat() }
                  }
                }

                -- after --

            "#]],
        );
    }

    #[test]
    fn should_iterate_over_array_in_for_loop() {
        check(
            PureModuleBuilder::new()
                .page("Test", [("items", "Array[String]")], |t| {
                    t.html_for(Some("item"), t.var("items"), |t| {
                        t.concat(vec![
                            t.element("li", vec![], vec![t.escape(t.var("item"))]),
                            t.text("\n"),
                        ])
                    })
                })
                .build(),
            vec![(
                "items",
                Value::Array(vec![
                    Value::String("Apple".to_string()),
                    Value::String("Banana".to_string()),
                    Value::String("Cherry".to_string()),
                ]),
            )],
            expect![[r#"
                -- before --
                page Test(items@v0: Array[String]) {
                  for v1 in v0 {
                    concat(
                      html(
                        tag: "li",
                        attrs: [],
                        children: concat(escape(v1)),
                      ),
                      text("\n"),
                    )
                  }
                }

                -- after --
                <li>Apple</li>
                <li>Banana</li>
                <li>Cherry</li>

            "#]],
        );
    }

    #[test]
    fn should_error_when_required_param_not_provided() {
        let module = PureModuleBuilder::new()
            .page("Test", [("name", "String")], |t| t.escape(t.var("name")))
            .build();

        let page_name = TypeName::parse("Test").unwrap();
        let result = evaluate_page(&module, &page_name, HashMap::new(), None);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Missing required parameter"));
        assert!(err.to_string().contains("name"));
    }
}
