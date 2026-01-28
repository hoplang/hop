use crate::document::CheapString;
use crate::ir::IrExpr;
use crate::{
    common::escape_html,
    dop::semantics::r#type::{ComparableType, EquatableType, NumericType},
};
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use tailwind_merge::tw_merge;

use crate::dop::patterns::{EnumPattern, Match};
use crate::ir::syntax::ast::{IrEntrypointDeclaration, IrForSource, IrStatement};

/// Fast stack-based environment for the evaluator.
/// Uses a Vec instead of HashMap for better performance with small scopes.
struct Env {
    stack: Vec<(CheapString, Value)>,
}

impl Env {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn push(&mut self, key: CheapString, value: Value) {
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

/// Runtime value for the Hop evaluator.
/// This enum properly represents all Hop types, unlike serde_json::Value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Bool(bool),
    Int(i64),
    Float(f64),
    Array(Vec<Value>),
    Record(HashMap<String, Value>),
    /// Option::Some with inner value
    Some(Box<Value>),
    /// Option::None
    None,
    /// Enum variant with name and optional fields
    Enum {
        variant_name: CheapString,
        fields: HashMap<String, Value>,
    },
}

impl Value {
    /// Convert from serde_json::Value to our Value type
    pub fn from_json(json: serde_json::Value) -> Self {
        match json {
            serde_json::Value::Null => Value::None,
            serde_json::Value::Bool(b) => Value::Bool(b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else if let Some(f) = n.as_f64() {
                    Value::Float(f)
                } else {
                    Value::Int(0)
                }
            }
            serde_json::Value::String(s) => Value::String(s),
            serde_json::Value::Array(arr) => {
                Value::Array(arr.into_iter().map(Value::from_json).collect())
            }
            serde_json::Value::Object(obj) => {
                let rec: HashMap<String, Value> = obj
                    .into_iter()
                    .map(|(k, v)| (k, Value::from_json(v)))
                    .collect();
                Value::Record(rec)
            }
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            // Allow Int to be read as f64 for convenience
            Value::Int(i) => Some(*i as f64),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&HashMap<String, Value>> {
        match self {
            Value::Record(rec) => Some(rec),
            _ => None,
        }
    }

    /// Convert Value to a display string for output
    pub fn to_output_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Array(_) => "[array]".to_string(),
            Value::Record(_) => "[record]".to_string(),
            Value::Some(inner) => inner.to_output_string(),
            Value::None => "".to_string(),
            Value::Enum { variant_name, .. } => variant_name.to_string(),
        }
    }

}

/// Evaluate an IR entrypoint with the given arguments
pub fn evaluate_entrypoint(
    entrypoint: &IrEntrypointDeclaration,
    args: HashMap<String, Value>,
) -> Result<String> {
    let mut env = Env::new();

    for (param_name, _param_type) in &entrypoint.parameters {
        if let Some(value) = args.get(param_name.as_str()) {
            env.push(param_name.as_cheap_string().clone(), value.clone());
        }
    }

    // Execute body
    let mut output = String::new();
    eval_statements(&entrypoint.body, &mut env, &mut output)?;

    Ok(output)
}

/// Evaluate a slice of IR statements
fn eval_statements(statements: &[IrStatement], env: &mut Env, output: &mut String) -> Result<()> {
    for statement in statements {
        eval_statement(statement, env, output)?;
    }
    Ok(())
}

/// Evaluate a single IR node
fn eval_statement(node: &IrStatement, env: &mut Env, output: &mut String) -> Result<()> {
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
                output.push_str(&escape_html(&s));
            } else {
                output.push_str(&s);
            }
            Ok(())
        }

        IrStatement::If {
            id: _,
            condition,
            body,
            else_body,
        } => {
            let cond_value = evaluate_expr(condition, env)?;
            if cond_value.as_bool().unwrap_or(false) {
                eval_statements(body, env, output)?;
            } else if let Some(else_stmts) = else_body {
                eval_statements(else_stmts, env, output)?;
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
                            env.push(var.as_cheap_string().clone(), item);
                        }
                        eval_statements(body, env, output)?;
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
                            env.push(var.as_cheap_string().clone(), Value::Int(i));
                        }
                        eval_statements(body, env, output)?;
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
            env.push(var.as_cheap_string().clone(), val);
            eval_statements(body, env, output)?;
            env.pop();
            Ok(())
        }

        IrStatement::Match { id: _, match_ } => match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject_value = env
                    .lookup(subject.0.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow!("Undefined variable: {}", subject.0))?;
                if subject_value.as_bool().unwrap_or(false) {
                    eval_statements(true_body, env, output)?;
                } else {
                    eval_statements(false_body, env, output)?;
                }
                Ok(())
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let subject_value = env
                    .lookup(subject.0.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow!("Undefined variable: {}", subject.0))?;

                match subject_value {
                    Value::Some(inner) => {
                        if let Some(var) = some_arm_binding {
                            env.push(var.as_cheap_string().clone(), *inner);
                            eval_statements(some_arm_body, env, output)?;
                            env.pop();
                        } else {
                            eval_statements(some_arm_body, env, output)?;
                        }
                    }
                    Value::None => {
                        eval_statements(none_arm_body, env, output)?;
                    }
                    _ => return Err(anyhow!("Expected Option value in match")),
                }
                Ok(())
            }
            Match::Enum { subject, arms } => {
                let subject_value = env
                    .lookup(subject.0.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow!("Undefined variable: {}", subject.0))?;

                let (variant_name, fields) = match &subject_value {
                    Value::Enum {
                        variant_name,
                        fields,
                    } => (variant_name.as_str(), fields),
                    _ => return Err(anyhow!("Expected Enum value in match")),
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
                            if let Some(field_val) = fields.get(field_name.as_str()) {
                                env.push(var_name.as_cheap_string().clone(), field_val.clone());
                            }
                        }
                        eval_statements(&arm.body, env, output)?;
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
                Ok(rec.get(field.as_str()).cloned().unwrap_or(Value::None))
            } else {
                Err(anyhow!("Expected record for field access"))
            }
        }
        IrExpr::StringLiteral { value: s, .. } => Ok(Value::String(s.to_string())),
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
                rec.insert(key.as_str().to_string(), evaluate_expr(value, env)?);
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
            operand_types: EquatableType::Enum { .. },
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            // Compare enum variants by their variant name (unit enums for now)
            Ok(Value::Bool(left_val == right_val))
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
                field_values.insert(field_name.as_str().to_string(), field_val);
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
                let subject_val = env
                    .lookup(subject.0.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow!("Undefined variable: {}", subject.0))?;

                let (variant_name, fields) = match &subject_val {
                    Value::Enum {
                        variant_name,
                        fields,
                    } => (variant_name.as_str(), fields),
                    _ => return Err(anyhow!("Expected Enum value in match expression")),
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
                            if let Some(field_val) = fields.get(field_name.as_str()) {
                                env.push(var_name.as_cheap_string().clone(), field_val.clone());
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
                let subject_val = env
                    .lookup(subject.0.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow!("Undefined variable: {}", subject.0))?;
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
                let subject_val = env
                    .lookup(subject.0.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow!("Undefined variable: {}", subject.0))?;

                match subject_val {
                    Value::Some(inner) => {
                        if let Some(var_name) = some_arm_binding {
                            env.push(var_name.as_cheap_string().clone(), *inner);
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
            var, value, body, ..
        } => {
            let val = evaluate_expr(value, env)?;
            env.push(var.as_cheap_string().clone(), val);
            let result = evaluate_expr(body, env)?;
            env.pop();
            Ok(result)
        }
        IrExpr::MergeClasses { args, .. } => {
            let mut strings = Vec::with_capacity(args.len());
            for arg in args {
                let val = evaluate_expr(arg, env)?;
                match val {
                    Value::String(s) => strings.push(s),
                    _ => return Err(anyhow!("MergeClasses requires string arguments")),
                }
            }
            let combined = strings.join(" ");
            Ok(Value::String(tw_merge(&combined)))
        }
        IrExpr::ArrayLength { array, .. } => {
            let array_val = evaluate_expr(array, env)?;
            match array_val {
                Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
                _ => Err(anyhow!("ArrayLength requires an array argument")),
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
        IrExpr::FloatToString { value, .. } => {
            let float_val = evaluate_expr(value, env)?;
            match float_val {
                Value::Float(f) => Ok(Value::String(f.to_string())),
                Value::Int(i) => Ok(Value::String((i as f64).to_string())),
                _ => Err(anyhow!("FloatToString requires a float argument")),
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
    use crate::dop::Type;
    use crate::ir::syntax::builder::{build_ir, build_ir_no_params, build_ir_with_enums_no_params};
    use expect_test::{Expect, expect};
    use std::sync::Arc;

    fn check(entrypoint: IrEntrypointDeclaration, args: Vec<(&str, Value)>, expected: Expect) {
        let before = entrypoint.to_string();
        let args_map: HashMap<String, Value> =
            args.into_iter().map(|(k, v)| (k.to_string(), v)).collect();
        let after = evaluate_entrypoint(&entrypoint, args_map).expect("Evaluation should succeed");

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
                Test() {
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
            build_ir("Test", [("content", Type::String)], |t| {
                t.write_expr_escaped(t.var("content"));
            }),
            vec![(
                "content",
                Value::String("<script>alert('xss')</script>".to_string()),
            )],
            expect![[r#"
                -- before --
                Test(content: String) {
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
            build_ir("Test", [("show", Type::Bool)], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Visible</div>");
                });
            }),
            vec![("show", Value::Bool(true))],
            expect![[r#"
                -- before --
                Test(show: Bool) {
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
            build_ir("Test", [("show", Type::Bool)], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Hidden</div>");
                });
            }),
            vec![("show", Value::Bool(false))],
            expect![[r#"
                -- before --
                Test(show: Bool) {
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
            build_ir(
                "Test",
                vec![("items", Type::Array(Arc::new(Type::String)))],
                |t| {
                    t.for_loop("item", t.var("items"), |t| {
                        t.write("<li>");
                        t.write_expr_escaped(t.var("item"));
                        t.write("</li>\n");
                    });
                },
            ),
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
                Test(items: Array[String]) {
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
    fn should_evaluate_enum_literal_in_condition() {
        check(
            build_ir_with_enums_no_params(
                "Test",
                vec![("Color", vec!["Red", "Green", "Blue"])],
                |t| {
                    let color = t.enum_variant("Color", "Red");
                    let red = t.enum_variant("Color", "Red");
                    t.if_stmt(t.eq(color, red), |t| {
                        t.write("is red");
                    });
                },
            ),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  if (Color::Red == Color::Red) {
                    write("is red")
                  }
                }

                -- after --
                is red
            "#]],
        );
    }

    #[test]
    fn should_evaluate_enum_equality_true() {
        check(
            build_ir_with_enums_no_params(
                "Test",
                vec![("Status", vec!["Active", "Inactive", "Pending"])],
                |t| {
                    let active1 = t.enum_variant("Status", "Active");
                    let active2 = t.enum_variant("Status", "Active");
                    t.if_stmt(t.eq(active1, active2), |t| {
                        t.write("equal");
                    });
                },
            ),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  if (Status::Active == Status::Active) {
                    write("equal")
                  }
                }

                -- after --
                equal
            "#]],
        );
    }

    #[test]
    fn should_evaluate_enum_equality_false() {
        check(
            build_ir_with_enums_no_params(
                "Test",
                vec![("Status", vec!["Active", "Inactive", "Pending"])],
                |t| {
                    let active = t.enum_variant("Status", "Active");
                    let inactive = t.enum_variant("Status", "Inactive");
                    t.if_else_stmt(
                        t.eq(active, inactive),
                        |t| {
                            t.write("equal");
                        },
                        |t| {
                            t.write("not equal");
                        },
                    );
                },
            ),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  if (Status::Active == Status::Inactive) {
                    write("equal")
                  } else {
                    write("not equal")
                  }
                }

                -- after --
                not equal
            "#]],
        );
    }
}
