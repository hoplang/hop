use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumPattern, Match};
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::ir::pure_module::{
    PureAttribute, PureExpr, PureForSource, PureFunctionDeclaration, PureModule,
};
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::html_node::{HtmlAttribute, HtmlNode};
use crate::ir::runtime::value::Value;
use crate::ir::runtime::variable_env::VariableEnv;
use crate::symbols::attribute_name::AttributeName;
use std::collections::HashMap;

use crate::ir::runtime::flat_evaluator::MAX_CALL_DEPTH;

/// Evaluate a function called from outside the module, on the arguments
/// named by its parameters.
pub fn evaluate_entry(
    module: &PureModule,
    function: &IrFunction,
    mut args: HashMap<AttributeName, Value>,
) -> Result<Value, EvalError> {
    let decl = module
        .functions
        .iter()
        .find(|decl| decl.function.id == function.id)
        .ok_or_else(|| EvalError::FunctionNotFound {
            function: function.clone(),
        })?;
    let mut values = Vec::with_capacity(decl.parameters.len());
    for param in &decl.parameters {
        let value = args
            .remove(param.name())
            .ok_or_else(|| EvalError::MissingParameter {
                function: function.clone(),
                param: param.name().clone(),
            })?;
        values.push(value);
    }
    evaluate_function(&module.functions, function, values, 0)
}

/// Evaluate a function on its arguments, one for each parameter in the
/// order they are declared, with `depth` function frames already active. A
/// call from outside any function has depth zero.
pub fn evaluate_function(
    function_decls: &[PureFunctionDeclaration],
    function: &IrFunction,
    args: Vec<Value>,
    depth: usize,
) -> Result<Value, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::RecursionLimit {
            function: function.clone(),
            limit: MAX_CALL_DEPTH,
        });
    }
    let decl = function_decls
        .iter()
        .find(|decl| decl.function.id == function.id)
        .ok_or_else(|| EvalError::FunctionNotFound {
            function: function.clone(),
        })?;
    if args.len() != decl.parameters.len() {
        return Err(EvalError::ArgumentCount {
            function: function.clone(),
            expected: decl.parameters.len(),
            found: args.len(),
        });
    }
    let mut env = VariableEnv::new();
    for (param, value) in decl.parameters.iter().zip(args) {
        env.insert(param.var, value);
    }
    evaluate_expr(&decl.body, &mut env, function_decls, depth + 1)
}

fn evaluate_expr(
    expr: &PureExpr,
    env: &mut VariableEnv,
    function_decls: &[PureFunctionDeclaration],
    depth: usize,
) -> Result<Value, EvalError> {
    match expr {
        PureExpr::Let {
            var, value, body, ..
        } => {
            let value = evaluate_expr(value, env, function_decls, depth)?;
            env.insert(var.var, value);
            let result = evaluate_expr(body, env, function_decls, depth);
            env.remove(&var.var);
            result
        }

        PureExpr::Match { match_, .. } => match &**match_ {
            Match::Enum { subject, arms } => {
                let (variant_name, fields) =
                    evaluate_expr(subject, env, function_decls, depth)?.unwrap_enum();
                let mut matching_arms = arms.iter().filter(|arm| {
                    let EnumPattern::Variant {
                        variant_name: pattern_variant,
                        ..
                    } = &arm.pattern;
                    variant_name == *pattern_variant
                });
                let arm = matching_arms.next().unwrap_or_else(|| {
                    panic!("No matching arm found for variant '{}'", variant_name)
                });
                assert!(
                    matching_arms.next().is_none(),
                    "Multiple matching arms found for variant '{}'",
                    variant_name
                );
                for (field_name, binder) in &arm.bindings {
                    let (_, field) = fields
                        .iter()
                        .find(|(name, _)| name == field_name)
                        .unwrap_or_else(|| {
                            panic!(
                                "Field '{}' not found in enum variant '{}'",
                                field_name, variant_name
                            )
                        });
                    env.insert(binder.var, field.clone());
                }
                let result = evaluate_expr(&arm.body, env, function_decls, depth);
                for (_, binder) in &arm.bindings {
                    env.remove(&binder.var);
                }
                result
            }

            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                if evaluate_expr(subject, env, function_decls, depth)?.unwrap_bool() {
                    evaluate_expr(true_body, env, function_decls, depth)
                } else {
                    evaluate_expr(false_body, env, function_decls, depth)
                }
            }

            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => match evaluate_expr(subject, env, function_decls, depth)?.unwrap_option() {
                Some(inner) => {
                    if let Some(binder) = some_arm_binding {
                        env.insert(binder.var, *inner);
                    }
                    let result = evaluate_expr(some_arm_body, env, function_decls, depth);
                    if let Some(binder) = some_arm_binding {
                        env.remove(&binder.var);
                    }
                    result
                }
                None => evaluate_expr(none_arm_body, env, function_decls, depth),
            },
        },

        PureExpr::VariableReference { value, .. } => Ok(env.get(value).clone()),

        PureExpr::FieldAccess { record, field, .. } => {
            let record = evaluate_expr(record, env, function_decls, depth)?.unwrap_record();
            let (_, value) = record
                .into_iter()
                .find(|(name, _)| name == field)
                .unwrap_or_else(|| panic!("Field '{}' not found in record", field));
            Ok(value)
        }

        PureExpr::StringLiteral { value, .. } => Ok(Value::String(value.to_string())),

        PureExpr::HtmlText { content, .. } => {
            Ok(Value::Html(vec![HtmlNode::Text(content.to_string())]))
        }

        PureExpr::HtmlEscape { expr, .. } => {
            let text = evaluate_expr(expr, env, function_decls, depth)?.unwrap_string();
            Ok(Value::Html(vec![HtmlNode::Escape(text)]))
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
                        let value =
                            evaluate_expr(value, env, function_decls, depth)?.unwrap_string();
                        rendered.push(HtmlAttribute {
                            name: name.clone(),
                            value: Some(value),
                        });
                    }
                    PureAttribute::Presence { name, present } => {
                        if evaluate_expr(present, env, function_decls, depth)?.unwrap_bool() {
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
                evaluate_expr(children, env, function_decls, depth)?.unwrap_html()
            };
            Ok(Value::Html(vec![HtmlNode::Element {
                element: element.clone(),
                attributes: rendered,
                children,
            }]))
        }

        PureExpr::HtmlConcat { parts, .. } => {
            let mut nodes = Vec::new();
            for part in parts {
                nodes.extend(evaluate_expr(part, env, function_decls, depth)?.unwrap_html());
            }
            Ok(Value::Html(nodes))
        }

        PureExpr::HtmlFor {
            var, source, body, ..
        } => {
            let items = match source.as_ref() {
                PureForSource::Array(array) => {
                    evaluate_expr(array, env, function_decls, depth)?.unwrap_array()
                }
                PureForSource::RangeInclusive { start, end } => {
                    let start = evaluate_expr(start, env, function_decls, depth)?.unwrap_int();
                    let end = evaluate_expr(end, env, function_decls, depth)?.unwrap_int();
                    (start..=end).map(Value::Int).collect()
                }
            };
            let mut nodes = Vec::new();
            for item in items {
                if let Some(binder) = var {
                    env.insert(binder.var, item);
                }
                nodes.extend(evaluate_expr(body, env, function_decls, depth)?.unwrap_html());
                if let Some(binder) = var {
                    env.remove(&binder.var);
                }
            }
            Ok(Value::Html(nodes))
        }

        PureExpr::Call { function, args, .. } => {
            let mut values = Vec::with_capacity(args.len());
            for arg in args {
                values.push(evaluate_expr(arg, env, function_decls, depth)?);
            }
            evaluate_function(function_decls, function, values, depth)
        }

        PureExpr::BoolLiteral { value, .. } => Ok(Value::Bool(*value)),

        PureExpr::FloatLiteral { value, .. } => Ok(Value::Float(*value)),

        PureExpr::IntLiteral { value, .. } => Ok(Value::Int(*value)),

        PureExpr::Array { elements, .. } => {
            let mut array = Vec::new();
            for element in elements {
                array.push(evaluate_expr(element, env, function_decls, depth)?);
            }
            Ok(Value::Array(array))
        }

        PureExpr::Tuple { elements, .. } => {
            let mut tuple = Vec::new();
            for element in elements {
                tuple.push(evaluate_expr(element, env, function_decls, depth)?);
            }
            Ok(Value::Tuple(tuple))
        }

        PureExpr::TupleIndex { tuple, index, .. } => {
            let tuple = evaluate_expr(tuple, env, function_decls, depth)?.unwrap_tuple();
            Ok(tuple
                .into_iter()
                .nth(*index)
                .unwrap_or_else(|| panic!("Index {} is out of range for the tuple", index)))
        }

        PureExpr::Record { fields, .. } => {
            let mut record = Vec::new();
            for (field_name, field) in fields {
                let field = evaluate_expr(field, env, function_decls, depth)?;
                record.push((field_name.clone(), field));
            }
            Ok(Value::Record(record))
        }

        PureExpr::Enum {
            variant_name,
            fields,
            ..
        } => {
            let mut field_values = Vec::new();
            for (field_name, field) in fields {
                let field = evaluate_expr(field, env, function_decls, depth)?;
                field_values.push((field_name.clone(), field));
            }
            Ok(Value::Enum {
                variant_name: variant_name.clone(),
                fields: field_values,
            })
        }

        PureExpr::Option { value, .. } => Ok(Value::Option(match value {
            Some(inner) => Some(Box::new(evaluate_expr(inner, env, function_decls, depth)?)),
            None => None,
        })),

        PureExpr::StringConcat { parts, .. } => {
            let mut result = String::new();
            for part in parts {
                result.push_str(&evaluate_expr(part, env, function_decls, depth)?.unwrap_string());
            }
            Ok(Value::String(result))
        }

        PureExpr::Binary { op, left, right } => {
            let left = evaluate_expr(left, env, function_decls, depth)?;
            let right = evaluate_expr(right, env, function_decls, depth)?;
            Ok(match op {
                IrBinaryOp::NumericAdd(NumericType::Int) => {
                    Value::Int(left.unwrap_int().wrapping_add(right.unwrap_int()))
                }
                IrBinaryOp::NumericAdd(NumericType::Float) => {
                    Value::Float(left.unwrap_float() + right.unwrap_float())
                }
                IrBinaryOp::NumericSubtract(NumericType::Int) => {
                    Value::Int(left.unwrap_int().wrapping_sub(right.unwrap_int()))
                }
                IrBinaryOp::NumericSubtract(NumericType::Float) => {
                    Value::Float(left.unwrap_float() - right.unwrap_float())
                }
                IrBinaryOp::NumericMultiply(NumericType::Int) => {
                    Value::Int(left.unwrap_int().wrapping_mul(right.unwrap_int()))
                }
                IrBinaryOp::NumericMultiply(NumericType::Float) => {
                    Value::Float(left.unwrap_float() * right.unwrap_float())
                }
                IrBinaryOp::Equals(EquatableType::Bool) => {
                    Value::Bool(left.unwrap_bool() == right.unwrap_bool())
                }
                IrBinaryOp::Equals(EquatableType::String) => {
                    Value::Bool(left.unwrap_string() == right.unwrap_string())
                }
                IrBinaryOp::Equals(EquatableType::Int) => {
                    Value::Bool(left.unwrap_int() == right.unwrap_int())
                }
                IrBinaryOp::Equals(EquatableType::Float) => {
                    Value::Bool(left.unwrap_float() == right.unwrap_float())
                }
                IrBinaryOp::LessThan(ComparableType::Int) => {
                    Value::Bool(left.unwrap_int() < right.unwrap_int())
                }
                IrBinaryOp::LessThan(ComparableType::Float) => {
                    Value::Bool(left.unwrap_float() < right.unwrap_float())
                }
                IrBinaryOp::LessThanOrEqual(ComparableType::Int) => {
                    Value::Bool(left.unwrap_int() <= right.unwrap_int())
                }
                IrBinaryOp::LessThanOrEqual(ComparableType::Float) => {
                    Value::Bool(left.unwrap_float() <= right.unwrap_float())
                }
            })
        }

        PureExpr::Unary { op, operand } => {
            let operand = evaluate_expr(operand, env, function_decls, depth)?;
            Ok(match op {
                IrUnaryOp::NumericNegation(NumericType::Int) => {
                    Value::Int(operand.unwrap_int().wrapping_neg())
                }
                IrUnaryOp::NumericNegation(NumericType::Float) => {
                    Value::Float(-operand.unwrap_float())
                }
                IrUnaryOp::BoolNegation => Value::Bool(!operand.unwrap_bool()),
                IrUnaryOp::ArrayLength => Value::Int(operand.unwrap_array().len() as i32),
                IrUnaryOp::ArrayIsEmpty => Value::Bool(operand.unwrap_array().is_empty()),
                IrUnaryOp::StringIsEmpty => Value::Bool(operand.unwrap_string().is_empty()),
                IrUnaryOp::OptionIsSome => Value::Bool(operand.unwrap_option().is_some()),
                IrUnaryOp::OptionIsNone => Value::Bool(operand.unwrap_option().is_none()),
                IrUnaryOp::IntToString => Value::String(operand.unwrap_int().to_string()),
                IrUnaryOp::FloatToInt => Value::Int(operand.unwrap_float() as i32),
                IrUnaryOp::IntToFloat => Value::Float(operand.unwrap_int() as f64),
            })
        }

        PureExpr::BoolLogicalAnd { left, right, .. } => {
            if evaluate_expr(left, env, function_decls, depth)?.unwrap_bool() {
                let right = evaluate_expr(right, env, function_decls, depth)?.unwrap_bool();
                Ok(Value::Bool(right))
            } else {
                Ok(Value::Bool(false))
            }
        }

        PureExpr::BoolLogicalOr { left, right, .. } => {
            if evaluate_expr(left, env, function_decls, depth)?.unwrap_bool() {
                Ok(Value::Bool(true))
            } else {
                let right = evaluate_expr(right, env, function_decls, depth)?.unwrap_bool();
                Ok(Value::Bool(right))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::{random_entry_args, random_module};
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_without_panicking() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            for (function, args) in random_entry_args(&module, &mut rng, &registry) {
                match evaluate_entry(&module, &function, args) {
                    Ok(_) | Err(EvalError::RecursionLimit { .. }) => {}
                    Err(error) => panic!("{error}"),
                }
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
        let entry = module
            .functions
            .iter()
            .find(|decl| decl.entry)
            .expect("the module has an entry function")
            .function
            .clone();
        let after = evaluate_entry(&module, &entry, args_map)
            .expect("Evaluation should succeed")
            .into_markup();

        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_hold_an_empty_tuple_in_a_record_field() {
        check(
            PureModuleBuilder::new()
                .record("Holder", [("nothing", "()")])
                .entry("Test", [], "Html", |t| {
                    let held = t.record("Holder", vec![("nothing", t.tuple(vec![]))]);
                    t.escape(t.int_to_string(t.array_length(
                        t.array_typed(t.resolve_type("()"), vec![t.field_access(held, "nothing")]),
                    )))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
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
                .entry("Test", [], "Html", |t| {
                    let pair = t.tuple(vec![t.int(1), t.str("two")]);
                    t.escape(t.tuple_index(pair, 1))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
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
                .entry("Test", [], "Html", |t| {
                    let only = t.tuple(vec![t.str("alone")]);
                    t.escape(t.tuple_index(only, 0))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
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
                .entry("Test", [], "Html", |t| {
                    let inner = t.tuple(vec![t.str("deep"), t.int(2)]);
                    let outer = t.tuple(vec![t.int(1), inner]);
                    t.escape(t.tuple_index(t.tuple_index(outer, 1), 0))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
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
                .entry("Test", [], "Html", |t| {
                    let sum = t.add(t.int(2147483647), t.int(1));
                    t.escape(t.int_to_string(sum))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
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
                .entry("Test", [], "Html", |t| {
                    t.element("div", vec![], vec![t.text("Hello World")])
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
                  html("div", {}, concat(text("Hello World")))
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
                .entry("Test", [("cls", "String"), ("flag", "Bool")], "Html", |t| {
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
                entry fn Test@f0(cls@b0: String, flag@b1: Bool) -> Html {
                  html("input", {class: b0, disabled: b1, checked: false})
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
                .entry("Test", [("content", "String")], "Html", |t| {
                    t.escape(t.var("content"))
                })
                .build(),
            vec![(
                "content",
                Value::String("<script>alert('xss')</script>".to_string()),
            )],
            expect![[r#"
                -- before --
                entry fn Test@f0(content@b0: String) -> Html {
                  escape(b0)
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
                .entry("Test", [("show", "Bool")], "Html", |t| {
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
                entry fn Test@f0(show@b0: Bool) -> Html {
                  match b0 {
                    true => {
                      html("div", {}, concat(text("Visible")))
                    }
                    false => {
                      concat()
                    }
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
                .entry("Test", [("show", "Bool")], "Html", |t| {
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
                entry fn Test@f0(show@b0: Bool) -> Html {
                  match b0 {
                    true => {
                      html("div", {}, concat(text("Hidden")))
                    }
                    false => {
                      concat()
                    }
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
                .entry("Test", [("items", "Array[String]")], "Html", |t| {
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
                entry fn Test@f0(items@b0: Array[String]) -> Html {
                  for b1: String in b0 {
                    concat(html("li", {}, concat(escape(b1))), text("\n"))
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
    fn should_evaluate_a_function_on_its_own() {
        let module = PureModuleBuilder::new()
            .function("Double", [("n", "Int")], "Int", |t| {
                t.add(t.var("n"), t.var("n"))
            })
            .build();
        let args = vec![Value::Int(21)];
        let result = evaluate_function(&module.functions, &module.functions[0].function, args, 0);
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn should_error_when_function_is_given_too_many_arguments() {
        let module = PureModuleBuilder::new()
            .function("Double", [("n", "Int")], "Int", |t| {
                t.add(t.var("n"), t.var("n"))
            })
            .build();
        let args = vec![Value::Int(21), Value::Int(1)];
        let result = evaluate_function(&module.functions, &module.functions[0].function, args, 0);
        assert_eq!(
            result.unwrap_err().to_string(),
            "Function 'Double@f0' takes 1 arguments but was given 2"
        );
    }

    #[test]
    fn should_evaluate_a_recursive_function() {
        check(
            PureModuleBuilder::new()
                .function("Sum", [("n", "Int")], "Int", |t| {
                    t.bool_match_expr(
                        t.lte(t.var("n"), t.int(0)),
                        t.int(0),
                        t.add(
                            t.var("n"),
                            t.call("Sum", vec![("n", t.sub(t.var("n"), t.int(1)))]),
                        ),
                    )
                })
                .entry("Test", [], "Html", |t| {
                    t.escape(t.int_to_string(t.call("Sum", vec![("n", t.int(10))])))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                fn Sum@f0(n@b0: Int) -> Int {
                  match (b0 <= 0) {
                    true => {
                      0
                    }
                    false => {
                      (b0 + call Sum@f0((b0 - 1)))
                    }
                  }
                }
                entry fn Test@f1() -> Html {
                  escape(call Sum@f0(10).to_string())
                }

                -- after --
                55
            "#]],
        );
    }

    #[test]
    fn should_error_when_a_function_never_stops_calling_itself() {
        let module = PureModuleBuilder::new()
            .function("Loop", [("n", "Int")], "Int", |t| {
                t.call("Loop", vec![("n", t.add(t.var("n"), t.int(1)))])
            })
            .entry("Test", [], "Html", |t| {
                t.escape(t.int_to_string(t.call("Loop", vec![("n", t.int(0))])))
            })
            .build();
        let result = evaluate_entry(&module, &module.functions[1].function, HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Function 'Loop@f0' exceeded the call depth limit of 12"
        );
    }

    #[test]
    fn should_error_when_required_param_not_provided() {
        let module = PureModuleBuilder::new()
            .entry("Test", [("name", "String")], "Html", |t| {
                t.escape(t.var("name"))
            })
            .build();
        let result = evaluate_entry(&module, &module.functions[0].function, HashMap::new());
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Missing required parameter"));
        assert!(err.to_string().contains("name"));
    }
}
