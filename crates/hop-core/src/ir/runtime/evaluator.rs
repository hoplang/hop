use crate::ir::pure_module::PureExpr;
use crate::ir::runtime::value::Value;
use crate::ir::var_id::VarId;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use crate::{
    expr::typing::r#type::{ComparableType, EquatableType, NumericType},
    html::write_escaped_html,
};
use std::collections::HashMap;
use tailwind_merge::tw_merge;
use thiserror::Error;

use crate::expr::patterns::{EnumPattern, Match};
use crate::ir::pure_module::{PureForSource, PureFunctionDeclaration, PureModule};

pub fn evaluate_page(
    module: &PureModule,
    page_name: &TypeName,
    args: HashMap<VarName, Value>,
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
        if let Some(value) = args.get(param.name()) {
            env.insert(param.var.id, value.clone());
        } else {
            return Err(EvalError::MissingParameter {
                page: page.name.clone(),
                param: param.name().clone(),
            });
        }
    }

    let value = evaluate_expr(&page.body, &mut env, &module.functions)?;
    let Value::String(html) = value else {
        panic!("Page body must evaluate to a Fragment");
    };

    Ok(html)
}

/// Errors the evaluator can produce.
#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Page '{page}' not found in module")]
    PageNotFound { page: TypeName },
    #[error("Missing required parameter '{param}' for page '{page}'")]
    MissingParameter { page: TypeName, param: VarName },
}

/// Variable environment for the evaluator.
struct VariableEnv {
    map: HashMap<VarId, Value>,
}

impl VariableEnv {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    /// Insert a binding into the environment.
    ///
    /// Panics if the binding already exists in the environment.
    fn insert(&mut self, key: VarId, value: Value) {
        assert_eq!(self.map.insert(key, value), None);
    }

    /// Remove a binding from the environment.
    ///
    /// Panics if the binding does not exist in the environment.
    fn remove(&mut self, key: &VarId) {
        assert!(self.map.remove(key).is_some());
    }

    /// Get a value from the environment.
    ///
    /// Panics if the binding does not exist in the environment.
    fn get(&self, key: &VarId) -> &Value {
        self.map.get(key).unwrap()
    }
}

fn evaluate_expr(
    expr: &PureExpr,
    env: &mut VariableEnv,
    function_decls: &[PureFunctionDeclaration],
) -> Result<Value, EvalError> {
    match expr {
        PureExpr::VariableReference { value: var, .. } => Ok(env.get(&var.id).clone()),
        PureExpr::FieldAccess {
            record: object,
            field,
            ..
        } => {
            let obj_value = evaluate_expr(object, env, function_decls)?;
            if let Some(rec) = obj_value.as_record() {
                Ok(rec
                    .get(field)
                    .cloned()
                    .unwrap_or_else(|| panic!("Field '{}' not found in record", field)))
            } else {
                panic!("Expected record for field access")
            }
        }
        PureExpr::StringLiteral { value: s, .. } => Ok(Value::String(s.to_string())),

        PureExpr::FragmentRaw { content, .. } => Ok(Value::String(content.clone())),

        PureExpr::FragmentEscape { expr, .. } => {
            let value = evaluate_expr(expr, env, function_decls)?;
            let Value::String(s) = value else {
                panic!("FragmentEscape requires a string value");
            };
            let mut escaped = String::new();
            write_escaped_html(&s, &mut escaped);
            Ok(Value::String(escaped))
        }

        PureExpr::FragmentConcat { parts, .. } => {
            let mut result = String::new();
            for part in parts {
                let value = evaluate_expr(part, env, function_decls)?;
                let Value::String(s) = value else {
                    panic!("FragmentConcat requires Fragment parts");
                };
                result.push_str(&s);
            }
            Ok(Value::String(result))
        }

        PureExpr::FragmentFor {
            var, source, body, ..
        } => {
            let mut result = String::new();
            match source.as_ref() {
                PureForSource::Array(array) => {
                    let array_value = evaluate_expr(array, env, function_decls)?;
                    let items = array_value
                        .as_array()
                        .cloned()
                        .expect("Expected array value");

                    for item in items {
                        if let Some(var) = var {
                            env.insert(var.id, item);
                        }
                        let value = evaluate_expr(body, env, function_decls)?;
                        let Value::String(s) = value else {
                            panic!("FragmentFor requires a Fragment body");
                        };
                        result.push_str(&s);
                        if let Some(var) = var {
                            env.remove(&var.id);
                        }
                    }
                }
                PureForSource::RangeInclusive { start, end } => {
                    let start_value = evaluate_expr(start, env, function_decls)?;
                    let end_value = evaluate_expr(end, env, function_decls)?;
                    let start_int = start_value.as_i32().expect("Expected integer value");
                    let end_int = end_value.as_i32().expect("Expected integer value");

                    for i in start_int..=end_int {
                        if let Some(var) = var {
                            env.insert(var.id, Value::Int(i));
                        }
                        let value = evaluate_expr(body, env, function_decls)?;
                        let Value::String(s) = value else {
                            panic!("FragmentFor requires a Fragment body");
                        };
                        result.push_str(&s);
                        if let Some(var) = var {
                            env.remove(&var.id);
                        }
                    }
                }
            }
            Ok(Value::String(result))
        }

        PureExpr::FunctionCall {
            function_name,
            args,
            ..
        } => {
            let func = function_decls
                .iter()
                .find(|c| c.name.as_str() == function_name.as_str())
                .unwrap_or_else(|| panic!("Undefined function: {}", function_name.as_str()));

            for arg in args {
                assert!(
                    func.parameters
                        .iter()
                        .any(|p| p.name().as_str() == arg.name.as_str()),
                    "Unknown argument '{}' for function '{}'",
                    arg.name.as_str(),
                    function_name.as_str()
                );
            }
            assert_eq!(
                args.len(),
                func.parameters.len(),
                "Duplicate argument for function '{}'",
                function_name.as_str()
            );

            let mut callee_env = VariableEnv::new();
            for param in &func.parameters {
                if let Some(arg) = args
                    .iter()
                    .find(|arg| arg.name.as_str() == param.name().as_str())
                {
                    let value = evaluate_expr(&arg.expr, env, function_decls)?;
                    callee_env.insert(param.var.id, value);
                } else {
                    panic!(
                        "Missing required parameter '{}' for function '{}'",
                        param.name(),
                        function_name.as_str()
                    );
                }
            }

            evaluate_expr(&func.body, &mut callee_env, function_decls)
        }

        PureExpr::BooleanLiteral { value: b, .. } => Ok(Value::Bool(*b)),
        PureExpr::FloatLiteral { value: f, .. } => Ok(Value::Float(*f)),
        PureExpr::IntLiteral { value: i, .. } => Ok(Value::Int(*i)),
        PureExpr::ArrayLiteral { elements, .. } => {
            let mut array = Vec::new();
            for elem in elements {
                array.push(evaluate_expr(elem, env, function_decls)?);
            }
            Ok(Value::Array(array))
        }
        PureExpr::RecordLiteral { fields, .. } => {
            let mut rec = HashMap::new();
            for (key, value) in fields {
                rec.insert(key.clone(), evaluate_expr(value, env, function_decls)?);
            }
            Ok(Value::Record(rec))
        }
        PureExpr::StringConcat { parts, .. } => {
            let mut result = String::new();
            for part in parts {
                match evaluate_expr(part, env, function_decls)? {
                    Value::String(part) => result.push_str(&part),
                    _ => panic!("String concatenation requires String parts"),
                }
            }
            Ok(Value::String(result))
        }
        PureExpr::BooleanNegation { operand, .. } => {
            let val = evaluate_expr(operand, env, function_decls)?;
            let bool_val = val.as_bool().expect("Expected boolean value");
            Ok(Value::Bool(!bool_val))
        }
        PureExpr::NumericNegation {
            operand,
            operand_type,
            ..
        } => {
            let val = evaluate_expr(operand, env, function_decls)?;
            match operand_type {
                NumericType::Int => {
                    let int_val = val.as_i32().expect("Expected integer value");
                    Ok(Value::Int(int_val.wrapping_neg()))
                }
                NumericType::Float => {
                    let float_val = val.as_f64().expect("Expected float value");
                    Ok(Value::Float(-float_val))
                }
            }
        }
        PureExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Bool,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;
            let left_bool = left_val.as_bool().expect("Expected boolean value");
            let right_bool = right_val.as_bool().expect("Expected boolean value");
            Ok(Value::Bool(left_bool == right_bool))
        }
        PureExpr::Equals {
            left,
            right,
            operand_types: EquatableType::String,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;
            let left_str = left_val.as_str().expect("Expected string value");
            let right_str = right_val.as_str().expect("Expected string value");
            Ok(Value::Bool(left_str == right_str))
        }
        PureExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Int,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;
            let left_int = left_val.as_i32().expect("Expected integer value");
            let right_int = right_val.as_i32().expect("Expected integer value");
            Ok(Value::Bool(left_int == right_int))
        }
        PureExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Float,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;
            let left_float = left_val.as_f64().expect("Expected float value");
            let right_float = right_val.as_f64().expect("Expected float value");
            Ok(Value::Bool(left_float == right_float))
        }
        PureExpr::LessThan {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i32().expect("Expected integer value");
                    let right_int = right_val.as_i32().expect("Expected integer value");
                    left_int < right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().expect("Expected float value");
                    let right_float = right_val.as_f64().expect("Expected float value");
                    left_float < right_float
                }
            };
            Ok(Value::Bool(result))
        }

        PureExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i32().expect("Expected integer value");
                    let right_int = right_val.as_i32().expect("Expected integer value");
                    left_int <= right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().expect("Expected float value");
                    let right_float = right_val.as_f64().expect("Expected float value");
                    left_float <= right_float
                }
            };
            Ok(Value::Bool(result))
        }

        PureExpr::BooleanLogicalAnd { left, right, .. } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;
            let left_bool = left_val.as_bool().expect("Expected boolean value");
            let right_bool = right_val.as_bool().expect("Expected boolean value");
            Ok(Value::Bool(left_bool && right_bool))
        }

        PureExpr::BooleanLogicalOr { left, right, .. } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;
            let left_bool = left_val.as_bool().expect("Expected boolean value");
            let right_bool = right_val.as_bool().expect("Expected boolean value");
            Ok(Value::Bool(left_bool || right_bool))
        }

        PureExpr::NumericAdd {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i32().expect("Expected integer value");
                    let right_int = right_val.as_i32().expect("Expected integer value");
                    Ok(Value::Int(left_int.wrapping_add(right_int)))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().expect("Expected float value");
                    let right_float = right_val.as_f64().expect("Expected float value");
                    Ok(Value::Float(left_float + right_float))
                }
            }
        }

        PureExpr::NumericSubtract {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i32().expect("Expected integer value");
                    let right_int = right_val.as_i32().expect("Expected integer value");
                    Ok(Value::Int(left_int.wrapping_sub(right_int)))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().expect("Expected float value");
                    let right_float = right_val.as_f64().expect("Expected float value");
                    Ok(Value::Float(left_float - right_float))
                }
            }
        }

        PureExpr::NumericMultiply {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env, function_decls)?;
            let right_val = evaluate_expr(right, env, function_decls)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i32().expect("Expected integer value");
                    let right_int = right_val.as_i32().expect("Expected integer value");
                    Ok(Value::Int(left_int.wrapping_mul(right_int)))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().expect("Expected float value");
                    let right_float = right_val.as_f64().expect("Expected float value");
                    Ok(Value::Float(left_float * right_float))
                }
            }
        }
        PureExpr::EnumLiteral {
            variant_name,
            fields,
            ..
        } => {
            let mut field_values = HashMap::new();
            for (field_name, field_expr) in fields {
                let field_val = evaluate_expr(field_expr, env, function_decls)?;
                field_values.insert(field_name.clone(), field_val);
            }
            Ok(Value::Enum {
                variant_name: variant_name.clone(),
                fields: field_values,
            })
        }
        PureExpr::OptionLiteral { value, .. } => match value {
            Some(inner) => Ok(Value::Some(Box::new(evaluate_expr(
                inner,
                env,
                function_decls,
            )?))),
            None => Ok(Value::None),
        },
        PureExpr::Match { match_, .. } => match match_ {
            Match::Enum { subject, arms } => {
                let subject_val = evaluate_expr(subject, env, function_decls)?;

                let Value::Enum {
                    variant_name,
                    fields,
                } = &subject_val
                else {
                    panic!("Expected Enum value in match expression");
                };

                for arm in arms {
                    let EnumPattern::Variant {
                        variant_name: pattern_variant,
                        ..
                    } = &arm.pattern;
                    if variant_name == pattern_variant {
                        // Bind fields to variables
                        for (field_name, var_name) in &arm.bindings {
                            let field_val = fields.get(field_name).unwrap_or_else(|| {
                                panic!(
                                    "Field '{}' not found in enum variant '{}'",
                                    field_name, variant_name
                                )
                            });
                            env.insert(var_name.id, field_val.clone());
                        }
                        let result = evaluate_expr(&arm.body, env, function_decls);
                        for (_, var_name) in &arm.bindings {
                            env.remove(&var_name.id);
                        }
                        return result;
                    }
                }

                panic!("No matching arm found for variant '{}'", variant_name)
            }
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject_val = evaluate_expr(subject, env, function_decls)?;
                let subject_bool = subject_val
                    .as_bool()
                    .expect("Match subject must evaluate to a boolean");

                if subject_bool {
                    evaluate_expr(true_body, env, function_decls)
                } else {
                    evaluate_expr(false_body, env, function_decls)
                }
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let subject_val = evaluate_expr(subject, env, function_decls)?;

                match subject_val {
                    Value::Some(inner) => {
                        if let Some(var_name) = some_arm_binding {
                            env.insert(var_name.id, *inner);
                            let result = evaluate_expr(some_arm_body, env, function_decls);
                            env.remove(&var_name.id);
                            result
                        } else {
                            evaluate_expr(some_arm_body, env, function_decls)
                        }
                    }
                    Value::None => evaluate_expr(none_arm_body, env, function_decls),
                    _ => panic!("Expected Option value in match expression"),
                }
            }
        },
        PureExpr::Let {
            var, value, body, ..
        } => {
            let val = evaluate_expr(value, env, function_decls)?;
            env.insert(var.id, val);
            let result = evaluate_expr(body, env, function_decls)?;
            env.remove(&var.id);
            Ok(result)
        }
        PureExpr::TwMerge { operand, .. } => {
            let val = evaluate_expr(operand, env, function_decls)?;
            match val {
                Value::String(s) => Ok(Value::String(tw_merge(&s))),
                _ => panic!("TwMerge requires a string argument"),
            }
        }
        PureExpr::ArrayLength { array, .. } => {
            let array_val = evaluate_expr(array, env, function_decls)?;
            match array_val {
                Value::Array(arr) => Ok(Value::Int(arr.len() as i32)),
                _ => panic!("ArrayLength requires an array argument"),
            }
        }
        PureExpr::ArrayIsEmpty { array, .. } => {
            let array_val = evaluate_expr(array, env, function_decls)?;
            match array_val {
                Value::Array(arr) => Ok(Value::Bool(arr.is_empty())),
                _ => panic!("ArrayIsEmpty requires an array argument"),
            }
        }
        PureExpr::StringIsEmpty { string, .. } => {
            let string_val = evaluate_expr(string, env, function_decls)?;
            match string_val {
                Value::String(s) => Ok(Value::Bool(s.is_empty())),
                _ => panic!("StringIsEmpty requires a string argument"),
            }
        }
        PureExpr::OptionIsSome { option, .. } => {
            let option_val = evaluate_expr(option, env, function_decls)?;
            match option_val {
                Value::Some(_) => Ok(Value::Bool(true)),
                Value::None => Ok(Value::Bool(false)),
                _ => panic!("OptionIsSome requires an Option argument"),
            }
        }
        PureExpr::OptionIsNone { option, .. } => {
            let option_val = evaluate_expr(option, env, function_decls)?;
            match option_val {
                Value::Some(_) => Ok(Value::Bool(false)),
                Value::None => Ok(Value::Bool(true)),
                _ => panic!("OptionIsNone requires an Option argument"),
            }
        }
        PureExpr::IntToString { value, .. } => {
            let int_val = evaluate_expr(value, env, function_decls)?;
            match int_val {
                Value::Int(n) => Ok(Value::String(n.to_string())),
                _ => panic!("IntToString requires an integer argument"),
            }
        }
        PureExpr::FloatToInt { value, .. } => {
            let float_val = evaluate_expr(value, env, function_decls)?;
            match float_val {
                Value::Float(f) => Ok(Value::Int(f as i32)),
                _ => panic!("FloatToInt requires a float argument"),
            }
        }
        PureExpr::IntToFloat { value, .. } => {
            let int_val = evaluate_expr(value, env, function_decls)?;
            match int_val {
                Value::Int(i) => Ok(Value::Float(i as f64)),
                _ => panic!("IntToFloat requires an integer argument"),
            }
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
    use rand::{SeedableRng, rngs::StdRng};

    #[test]
    fn fuzz_random_modules_evaluate_without_panicking() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = StdRng::seed_from_u64(u.arbitrary()?);
            for page in &module.pages {
                let args: HashMap<VarName, Value> = page
                    .parameters
                    .iter()
                    .map(|p| {
                        (
                            p.name().clone(),
                            random_value(&mut rng, &p.typ, None, &registry),
                        )
                    })
                    .collect();
                evaluate_page(&module, &page.name, args).unwrap();
            }
            Ok(())
        });
    }

    fn check(module: PureModule, args: Vec<(&str, Value)>, expected: Expect) {
        let before = module.to_string();
        let args_map: HashMap<VarName, Value> = args
            .into_iter()
            .map(|(k, v)| (VarName::new(k).unwrap(), v))
            .collect();
        let page_name = module.pages[0].name.clone();
        let after =
            evaluate_page(&module, &page_name, args_map).expect("Evaluation should succeed");

        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_wrap_int_addition_at_i32_boundary() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
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
    fn should_evaluate_simple_raw() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| t.raw("<div>Hello World</div>"))
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  raw("<div>Hello World</div>")
                }

                -- after --
                <div>Hello World</div>
            "#]],
        );
    }

    #[test]
    fn should_escape_html_in_expressions() {
        check(
            PureModuleBuilder::new()
                .view("Test", [("content", "String")], |t| {
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
                &lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;
            "#]],
        );
    }

    #[test]
    fn should_render_if_body_when_condition_is_true() {
        check(
            PureModuleBuilder::new()
                .view("Test", [("show", "Bool")], |t| {
                    t.bool_match_expr(t.var("show"), t.raw("<div>Visible</div>"), t.concat(vec![]))
                })
                .build(),
            vec![("show", Value::Bool(true))],
            expect![[r#"
                -- before --
                page Test(show@v0: Bool) {
                  match v0 {
                    true => raw("<div>Visible</div>"),
                    false => concat(),
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
                .view("Test", [("show", "Bool")], |t| {
                    t.bool_match_expr(t.var("show"), t.raw("<div>Hidden</div>"), t.concat(vec![]))
                })
                .build(),
            vec![("show", Value::Bool(false))],
            expect![[r#"
                -- before --
                page Test(show@v0: Bool) {
                  match v0 {
                    true => raw("<div>Hidden</div>"),
                    false => concat(),
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
                .view("Test", [("items", "Array[String]")], |t| {
                    t.fragment_for(Some("item"), t.var("items"), |t| {
                        t.concat(vec![
                            t.raw("<li>"),
                            t.escape(t.var("item")),
                            t.raw("</li>\n"),
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
                  concat(
                    raw("<li>"),
                    escape(v1),
                    raw("</li>\n"),
                  ) for v1 in v0
                }

                -- after --
                <li>Apple</li>
                <li>Banana</li>
                <li>Cherry</li>

            "#]],
        );
    }

    #[test]
    fn let_binds_a_value_then_uses_it() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("v_0", t.raw("<b>hi</b>"), |t| t.var("v_0"))
                })
                .build(),
            vec![],
            expect![[r#"
                -- before --
                page Test() {
                  let v0 = raw("<b>hi</b>") in v0
                }

                -- after --
                <b>hi</b>
            "#]],
        );
    }

    #[test]
    fn should_error_when_required_param_not_provided() {
        let module = PureModuleBuilder::new()
            .view("Test", [("name", "String")], |t| t.escape(t.var("name")))
            .build();

        // Call without providing the required argument
        let page_name = TypeName::new("Test").unwrap();
        let result = evaluate_page(&module, &page_name, HashMap::new());
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Missing required parameter"));
        assert!(err.to_string().contains("name"));
    }

    #[test]
    fn function_args_are_evaluated_in_caller_env_not_shadowed_by_earlier_params() {
        // p1 = p0 refers to the caller's p0 (42), not the function's own
        // p0 (999) that gets bound first.
        check(
            PureModuleBuilder::new()
                .function("C", [("p0", "Int"), ("p1", "Int")], "Fragment", |t| {
                    t.escape(t.int_to_string(t.var("p1")))
                })
                .view("Test", [("p0", "Int")], |t| {
                    t.call("C", vec![("p0", t.int(999)), ("p1", t.var("p0"))])
                })
                .build(),
            vec![("p0", Value::Int(42))],
            expect![[r#"
                -- before --
                fn C(p0@v0: Int, p1@v1: Int) -> Fragment {
                  escape(v1.to_string())
                }
                page Test(p0@v2: Int) {
                  call C(p0 = 999, p1 = v2)
                }

                -- after --
                42
            "#]],
        );
    }
}
