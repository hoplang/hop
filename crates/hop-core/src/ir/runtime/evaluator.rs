use crate::ir::IrExpr;
use crate::ir::runtime::value::Value;
use crate::symbols::var_name::VarName;
use crate::{
    expr::typing::r#type::{ComparableType, EquatableType, NumericType},
    html::write_escaped_html,
};
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use tailwind_merge::tw_merge;

use crate::expr::patterns::{EnumPattern, Match};
use crate::ir::syntax::ast::{IrComponentDeclaration, IrForSource, IrStatement, IrViewDeclaration};

/// Stack-based environment for the evaluator.
struct Env {
    stack: Vec<(VarName, Value)>,
}

impl Env {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }
    fn push(&mut self, key: VarName, value: Value) {
        self.stack.push((key, value));
    }
    fn pop(&mut self) {
        self.stack.pop();
    }
    fn lookup(&self, key: &str) -> Option<&Value> {
        self.stack
            .iter()
            .rev()
            .find(|(k, _)| k.as_str() == key)
            .map(|(_, v)| v)
    }
}

/// Evaluate an IR view with the given arguments
pub fn evaluate_view(
    view: &IrViewDeclaration,
    args: HashMap<String, Value>,
    component_defs: &[IrComponentDeclaration],
) -> Result<String> {
    let mut env = Env::new();

    for param in &view.parameters {
        if let Some(value) = args.get(param.name.as_str()) {
            env.push(param.name.clone(), value.clone());
        } else {
            return Err(anyhow!(
                "Missing required parameter '{}' for view '{}'",
                param.name,
                view.name
            ));
        }
    }

    // Execute body
    let mut output = String::new();
    eval_statements(&view.body, &mut env, &mut output, component_defs)?;

    Ok(output)
}

/// Evaluate a slice of IR statements
fn eval_statements(
    statements: &[IrStatement],
    env: &mut Env,
    output: &mut String,
    component_defs: &[IrComponentDeclaration],
) -> Result<()> {
    for statement in statements {
        eval_statement(statement, env, output, component_defs)?;
    }
    Ok(())
}

/// Evaluate a single IR node
fn eval_statement(
    node: &IrStatement,
    env: &mut Env,
    output: &mut String,
    component_defs: &[IrComponentDeclaration],
) -> Result<()> {
    match node {
        IrStatement::Write { id: _, content } => {
            output.push_str(content);
            Ok(())
        }

        IrStatement::WriteExpr {
            id: _,
            expr,
            escape,
        } => {
            let value = evaluate_expr(expr, env)?;
            let s = value.to_output_string();
            if *escape {
                write_escaped_html(&s, output);
            } else {
                output.push_str(&s);
            }
            Ok(())
        }

        IrStatement::If {
            id: _,
            condition,
            body,
        } => {
            let cond_value = evaluate_expr(condition, env)?;
            if cond_value.as_bool().unwrap_or(false) {
                eval_statements(body, env, output, component_defs)?;
            }
            Ok(())
        }

        IrStatement::For {
            id: _,
            var,
            source,
            body,
        } => {
            match source {
                IrForSource::Array(array) => {
                    let array_value = evaluate_expr(array, env)?;
                    let items = array_value.as_array().cloned().unwrap_or_default();

                    for item in items {
                        if let Some(var) = var {
                            env.push(var.clone(), item);
                        }
                        eval_statements(body, env, output, component_defs)?;
                        if var.is_some() {
                            env.pop();
                        }
                    }
                }
                IrForSource::RangeInclusive { start, end } => {
                    let start_value = evaluate_expr(start, env)?;
                    let end_value = evaluate_expr(end, env)?;
                    let start_int = start_value.as_i64().unwrap_or(0);
                    let end_int = end_value.as_i64().unwrap_or(0);

                    for i in start_int..=end_int {
                        if let Some(var) = var {
                            env.push(var.clone(), Value::Int(i));
                        }
                        eval_statements(body, env, output, component_defs)?;
                        if var.is_some() {
                            env.pop();
                        }
                    }
                }
            }
            Ok(())
        }

        IrStatement::Let {
            id: _,
            var,
            value,
            body,
        } => {
            let val = evaluate_expr(value, env)?;
            env.push(var.clone(), val);
            eval_statements(body, env, output, component_defs)?;
            env.pop();
            Ok(())
        }

        IrStatement::LetFragment {
            id: _,
            var,
            fragment_body,
            body,
        } => {
            let mut captured = String::new();
            eval_statements(fragment_body, env, &mut captured, component_defs)?;
            env.push(var.clone(), Value::String(captured));
            eval_statements(body, env, output, component_defs)?;
            env.pop();
            Ok(())
        }

        IrStatement::LetRecordDestructure {
            id: _,
            subject,
            bindings,
            body,
        } => {
            let subject_value = evaluate_expr(subject, env)?;
            let rec = subject_value
                .as_record()
                .ok_or_else(|| anyhow!("Expected record value in destructure"))?;
            let mut pushed = 0;
            for (field, var) in bindings {
                if let Some(field_val) = rec.get(field) {
                    env.push(var.clone(), field_val.clone());
                    pushed += 1;
                }
            }
            eval_statements(body, env, output, component_defs)?;
            for _ in 0..pushed {
                env.pop();
            }
            Ok(())
        }

        IrStatement::Match { id: _, match_ } => match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject_value = evaluate_expr(subject, env)?;
                if subject_value.as_bool().unwrap_or(false) {
                    eval_statements(true_body, env, output, component_defs)?;
                } else {
                    eval_statements(false_body, env, output, component_defs)?;
                }
                Ok(())
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let subject_value = evaluate_expr(subject, env)?;

                match subject_value {
                    Value::Some(inner) => {
                        if let Some(var) = some_arm_binding {
                            env.push(var.clone(), *inner);
                            eval_statements(some_arm_body, env, output, component_defs)?;
                            env.pop();
                        } else {
                            eval_statements(some_arm_body, env, output, component_defs)?;
                        }
                    }
                    Value::None => {
                        eval_statements(none_arm_body, env, output, component_defs)?;
                    }
                    _ => return Err(anyhow!("Expected Option value in match")),
                }
                Ok(())
            }
            Match::Enum { subject, arms } => {
                let subject_value = evaluate_expr(subject, env)?;

                let Value::Enum {
                    variant_name,
                    fields,
                } = &subject_value
                else {
                    return Err(anyhow!("Expected Enum value in match"));
                };

                for arm in arms {
                    let EnumPattern::Variant {
                        variant_name: pattern_variant,
                        ..
                    } = &arm.pattern;
                    if variant_name == pattern_variant {
                        // Bind fields to variables
                        let bindings_count = arm.bindings.len();
                        for (field_name, var_name) in &arm.bindings {
                            if let Some(field_val) = fields.get(field_name) {
                                env.push(var_name.clone(), field_val.clone());
                            }
                        }
                        eval_statements(&arm.body, env, output, component_defs)?;
                        for _ in 0..bindings_count {
                            env.pop();
                        }
                        return Ok(());
                    }
                }
                Err(anyhow!(
                    "No matching arm for enum variant: {}",
                    variant_name
                ))
            }
        },

        IrStatement::ComponentInvocation {
            component_name,
            args,
            ..
        } => {
            let component_def = component_defs
                .iter()
                .find(|c| c.name.as_str() == component_name.as_str())
                .ok_or_else(|| anyhow!("Undefined component: {}", component_name.as_str()))?;

            // Evaluate all argument expressions in the caller's env first,
            // so an earlier-bound parameter can't shadow a caller variable
            // that a later argument expression refers to. Only after every
            // value is computed do we bind them onto the env.
            let bind_count = component_def.parameters.len();
            let mut values = Vec::with_capacity(bind_count);
            for param in &component_def.parameters {
                if let Some(arg) = args
                    .iter()
                    .find(|arg| arg.name.as_str() == param.name.as_str())
                {
                    let value = evaluate_expr(&arg.expr, env)?;
                    values.push((param.name.clone(), value));
                } else {
                    return Err(anyhow!(
                        "Missing required parameter '{}' for component '{}'",
                        param.name,
                        component_name.as_str()
                    ));
                }
            }
            for (name, value) in values {
                env.push(name, value);
            }

            eval_statements(&component_def.body, env, output, component_defs)?;

            for _ in 0..bind_count {
                env.pop();
            }
            Ok(())
        }
    }
}

fn evaluate_expr(expr: &IrExpr, env: &mut Env) -> Result<Value> {
    match expr {
        IrExpr::Var { value: name, .. } => env
            .lookup(name.as_str())
            .cloned()
            .ok_or_else(|| anyhow!("Undefined variable: {}", name)),
        IrExpr::FieldAccess {
            record: object,
            field,
            ..
        } => {
            let obj_value = evaluate_expr(object, env)?;
            if let Some(rec) = obj_value.as_record() {
                Ok(rec.get(field).cloned().unwrap_or(Value::None))
            } else {
                Err(anyhow!("Expected record for field access"))
            }
        }
        IrExpr::StringLiteral { value: s, .. } => Ok(Value::String(s.to_string())),
        IrExpr::FragmentEmpty { .. } => Ok(Value::String(String::new())),
        IrExpr::BooleanLiteral { value: b, .. } => Ok(Value::Bool(*b)),
        IrExpr::FloatLiteral { value: f, .. } => Ok(Value::Float(*f)),
        IrExpr::IntLiteral { value: i, .. } => Ok(Value::Int(*i)),
        IrExpr::ArrayLiteral { elements, .. } => {
            let mut array = Vec::new();
            for elem in elements {
                array.push(evaluate_expr(elem, env)?);
            }
            Ok(Value::Array(array))
        }
        IrExpr::RecordLiteral { fields, .. } => {
            let mut rec = HashMap::new();
            for (key, value) in fields {
                rec.insert(key.clone(), evaluate_expr(value, env)?);
            }
            Ok(Value::Record(rec))
        }
        IrExpr::StringConcat { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match (left_val, right_val) {
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                _ => Err(anyhow!("String concatenation requires two strings")),
            }
        }
        IrExpr::BooleanNegation { operand, .. } => {
            let val = evaluate_expr(operand, env)?;
            let bool_val = val.as_bool().unwrap_or(false);
            Ok(Value::Bool(!bool_val))
        }
        IrExpr::NumericNegation {
            operand,
            operand_type,
            ..
        } => {
            let val = evaluate_expr(operand, env)?;
            match operand_type {
                NumericType::Int => {
                    let int_val = val.as_i64().unwrap_or(0);
                    Ok(Value::Int(-int_val))
                }
                NumericType::Float => {
                    let float_val = val.as_f64().unwrap_or(0.0);
                    Ok(Value::Float(-float_val))
                }
            }
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Bool,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool == right_bool))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::String,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_str = left_val.as_str().unwrap_or("");
            let right_str = right_val.as_str().unwrap_or("");
            Ok(Value::Bool(left_str == right_str))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Int,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_int = left_val.as_i64().unwrap_or(0);
            let right_int = right_val.as_i64().unwrap_or(0);
            Ok(Value::Bool(left_int == right_int))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Float,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_float = left_val.as_f64().unwrap_or(0.0);
            let right_float = right_val.as_f64().unwrap_or(0.0);
            Ok(Value::Bool(left_float == right_float))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Option(_),
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            Ok(Value::Bool(left_val == right_val))
        }
        IrExpr::LessThan {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    left_int < right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    left_float < right_float
                }
            };
            Ok(Value::Bool(result))
        }

        IrExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    left_int <= right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    left_float <= right_float
                }
            };
            Ok(Value::Bool(result))
        }

        IrExpr::BooleanLogicalAnd { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool && right_bool))
        }

        IrExpr::BooleanLogicalOr { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool || right_bool))
        }

        IrExpr::NumericAdd {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    Ok(Value::Int(left_int + right_int))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    Ok(Value::Float(left_float + right_float))
                }
            }
        }

        IrExpr::NumericSubtract {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    Ok(Value::Int(left_int - right_int))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    Ok(Value::Float(left_float - right_float))
                }
            }
        }

        IrExpr::NumericMultiply {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    Ok(Value::Int(left_int * right_int))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    Ok(Value::Float(left_float * right_float))
                }
            }
        }
        IrExpr::EnumLiteral {
            variant_name,
            fields,
            ..
        } => {
            let mut field_values = HashMap::new();
            for (field_name, field_expr) in fields {
                let field_val = evaluate_expr(field_expr, env)?;
                field_values.insert(field_name.clone(), field_val);
            }
            Ok(Value::Enum {
                variant_name: variant_name.clone(),
                fields: field_values,
            })
        }
        IrExpr::OptionLiteral { value, .. } => match value {
            Some(inner) => Ok(Value::Some(Box::new(evaluate_expr(inner, env)?))),
            None => Ok(Value::None),
        },
        IrExpr::Match { match_, .. } => match match_ {
            Match::Enum { subject, arms } => {
                let subject_val = evaluate_expr(subject, env)?;

                let Value::Enum {
                    variant_name,
                    fields,
                } = &subject_val
                else {
                    return Err(anyhow!("Expected Enum value in match expression"));
                };

                for arm in arms {
                    let EnumPattern::Variant {
                        variant_name: pattern_variant,
                        ..
                    } = &arm.pattern;
                    if variant_name == pattern_variant {
                        // Bind fields to variables
                        let bindings_count = arm.bindings.len();
                        for (field_name, var_name) in &arm.bindings {
                            if let Some(field_val) = fields.get(field_name) {
                                env.push(var_name.clone(), field_val.clone());
                            }
                        }
                        let result = evaluate_expr(&arm.body, env);
                        for _ in 0..bindings_count {
                            env.pop();
                        }
                        return result;
                    }
                }

                Err(anyhow!(
                    "No matching arm found for variant '{}'",
                    variant_name
                ))
            }
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject_val = evaluate_expr(subject, env)?;
                let subject_bool = subject_val
                    .as_bool()
                    .ok_or_else(|| anyhow!("Match subject must evaluate to a boolean"))?;

                if subject_bool {
                    evaluate_expr(true_body, env)
                } else {
                    evaluate_expr(false_body, env)
                }
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let subject_val = evaluate_expr(subject, env)?;

                match subject_val {
                    Value::Some(inner) => {
                        if let Some(var_name) = some_arm_binding {
                            env.push(var_name.clone(), *inner);
                            let result = evaluate_expr(some_arm_body, env);
                            env.pop();
                            result
                        } else {
                            evaluate_expr(some_arm_body, env)
                        }
                    }
                    Value::None => evaluate_expr(none_arm_body, env),
                    _ => Err(anyhow!("Expected Option value in match expression")),
                }
            }
        },
        IrExpr::Let {
            var_name,
            value,
            body,
            ..
        } => {
            let val = evaluate_expr(value, env)?;
            env.push(var_name.clone(), val);
            let result = evaluate_expr(body, env)?;
            env.pop();
            Ok(result)
        }
        IrExpr::LetRecordDestructure {
            subject,
            bindings,
            body,
            ..
        } => {
            let subject_value = evaluate_expr(subject, env)?;
            let rec = subject_value
                .as_record()
                .ok_or_else(|| anyhow!("Expected record value in destructure"))?;
            let mut pushed = 0;
            for (field, var) in bindings {
                if let Some(field_val) = rec.get(field) {
                    env.push(var.clone(), field_val.clone());
                    pushed += 1;
                }
            }
            let result = evaluate_expr(body, env);
            for _ in 0..pushed {
                env.pop();
            }
            result
        }
        IrExpr::TwMerge { operand, .. } => {
            let val = evaluate_expr(operand, env)?;
            match val {
                Value::String(s) => Ok(Value::String(tw_merge(&s))),
                _ => Err(anyhow!("TwMerge requires a string argument")),
            }
        }
        IrExpr::ArrayLength { array, .. } => {
            let array_val = evaluate_expr(array, env)?;
            match array_val {
                Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
                _ => Err(anyhow!("ArrayLength requires an array argument")),
            }
        }
        IrExpr::ArrayIsEmpty { array, .. } => {
            let array_val = evaluate_expr(array, env)?;
            match array_val {
                Value::Array(arr) => Ok(Value::Bool(arr.is_empty())),
                _ => Err(anyhow!("ArrayIsEmpty requires an array argument")),
            }
        }
        IrExpr::StringIsEmpty { string, .. } => {
            let string_val = evaluate_expr(string, env)?;
            match string_val {
                Value::String(s) => Ok(Value::Bool(s.is_empty())),
                _ => Err(anyhow!("StringIsEmpty requires a string argument")),
            }
        }
        IrExpr::OptionIsSome { option, .. } => {
            let option_val = evaluate_expr(option, env)?;
            match option_val {
                Value::Some(_) => Ok(Value::Bool(true)),
                Value::None => Ok(Value::Bool(false)),
                _ => Err(anyhow!("OptionIsSome requires an Option argument")),
            }
        }
        IrExpr::OptionIsNone { option, .. } => {
            let option_val = evaluate_expr(option, env)?;
            match option_val {
                Value::Some(_) => Ok(Value::Bool(false)),
                Value::None => Ok(Value::Bool(true)),
                _ => Err(anyhow!("OptionIsNone requires an Option argument")),
            }
        }
        IrExpr::IntToString { value, .. } => {
            let int_val = evaluate_expr(value, env)?;
            match int_val {
                Value::Int(n) => Ok(Value::String(n.to_string())),
                _ => Err(anyhow!("IntToString requires an integer argument")),
            }
        }
        IrExpr::FloatToInt { value, .. } => {
            let float_val = evaluate_expr(value, env)?;
            match float_val {
                Value::Float(f) => Ok(Value::Int(f as i64)),
                Value::Int(i) => Ok(Value::Int(i)), // Already an int
                _ => Err(anyhow!("FloatToInt requires a float argument")),
            }
        }
        IrExpr::IntToFloat { value, .. } => {
            let int_val = evaluate_expr(value, env)?;
            match int_val {
                Value::Int(i) => Ok(Value::Float(i as f64)),
                _ => Err(anyhow!("IntToFloat requires an integer argument")),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Type;
    use crate::ir::syntax::builder::{build_ir, build_ir_no_params};
    use crate::symbols::type_name::TypeName;
    use crate::symbols::var_name::VarName;
    use expect_test::{Expect, expect};
    use std::sync::Arc;

    fn check(view: IrViewDeclaration, args: Vec<(&str, Value)>, expected: Expect) {
        let before = view.to_string();
        let args_map: HashMap<String, Value> =
            args.into_iter().map(|(k, v)| (k.to_string(), v)).collect();
        let after = evaluate_view(&view, args_map, &[]).expect("Evaluation should succeed");

        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_evaluate_simple_write() {
        check(
            build_ir_no_params("Test", |t| {
                t.write("<div>Hello World</div>");
            }),
            vec![],
            expect![[r#"
                -- before --
                view Test() {
                  write("<div>Hello World</div>")
                }

                -- after --
                <div>Hello World</div>
            "#]],
        );
    }

    #[test]
    fn should_escape_html_in_expressions() {
        check(
            build_ir("Test", [("content", "String")], |t| {
                t.write_expr_escaped(t.var("content"));
            }),
            vec![(
                "content",
                Value::String("<script>alert('xss')</script>".to_string()),
            )],
            expect![[r#"
                -- before --
                view Test(content: String) {
                  write_escaped(content)
                }

                -- after --
                &lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;
            "#]],
        );
    }

    #[test]
    fn should_render_if_body_when_condition_is_true() {
        check(
            build_ir("Test", [("show", "Bool")], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Visible</div>");
                });
            }),
            vec![("show", Value::Bool(true))],
            expect![[r#"
                -- before --
                view Test(show: Bool) {
                  if show {
                    write("<div>Visible</div>")
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
            build_ir("Test", [("show", "Bool")], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Hidden</div>");
                });
            }),
            vec![("show", Value::Bool(false))],
            expect![[r#"
                -- before --
                view Test(show: Bool) {
                  if show {
                    write("<div>Hidden</div>")
                  }
                }

                -- after --

            "#]],
        );
    }

    #[test]
    fn should_iterate_over_array_in_for_loop() {
        check(
            build_ir("Test", vec![("items", "Array[String]")], |t| {
                t.for_loop("item", t.var("items"), |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("item"));
                    t.write("</li>\n");
                });
            }),
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
                view Test(items: Array[String]) {
                  for item in items {
                    write("<li>")
                    write_escaped(item)
                    write("</li>\n")
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
    fn let_fragment_renders_into_a_value_then_writes_it() {
        check(
            build_ir_no_params("Test", |t| {
                t.let_fragment(
                    "v_0",
                    |t| {
                        t.write("<b>hi</b>");
                    },
                    |t| {
                        t.write_expr(t.var("v_0"), false);
                    },
                );
            }),
            vec![],
            expect![[r#"
                -- before --
                view Test() {
                  let v_0 = {
                    write("<b>hi</b>")
                  } in {
                    write_expr(v_0)
                  }
                }

                -- after --
                <b>hi</b>
            "#]],
        );
    }

    #[test]
    fn should_error_when_required_param_not_provided() {
        use crate::ir::ast::{IrExpr, IrParameter, IrStatement, IrViewDeclaration};

        // Create an view with a required parameter (no default)
        let view = IrViewDeclaration {
            name: TypeName::new("Test").unwrap(),
            parameters: vec![IrParameter {
                name: VarName::new("name").unwrap(),
                typ: Arc::new(Type::String),
            }],
            body: vec![IrStatement::WriteExpr {
                id: 0,
                expr: IrExpr::Var {
                    value: VarName::new("name").unwrap(),
                    kind: Arc::new(Type::String),
                    id: 1,
                },
                escape: false,
            }],
        };

        // Call without providing the required argument
        let result = evaluate_view(&view, HashMap::new(), &[]);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Missing required parameter"));
        assert!(err.to_string().contains("name"));
    }

    #[test]
    fn component_args_are_evaluated_in_caller_env_not_shadowed_by_earlier_params() {
        use crate::ir::syntax::builder::IrModuleBuilder;

        let module = IrModuleBuilder::new()
            .component("C", [("p0", "Int"), ("p1", "Int")], |t| {
                t.write_expr(t.int_to_string(t.var("p1")), false);
            })
            .view("Test", [("p0", "Int")], |t| {
                t.invoke_component("C", vec![("p0", t.int(999)), ("p1", t.var("p0"))]);
            })
            .build();

        let mut args = HashMap::new();
        args.insert("p0".to_string(), Value::Int(42));

        let output = evaluate_view(&module.views[0], args, &module.components)
            .expect("Evaluation should succeed");

        // p1 = p0 refers to the caller's p0 (42), not the component's own
        // p0 (999) that gets bound first.
        assert_eq!(output, "42");
    }
}
