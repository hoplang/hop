use std::collections::HashMap;

use crate::document::CheapString;
use crate::hop::patterns::{EnumPattern, Match};
use crate::ir::expr_id::ExprIdCounter;
use crate::ir::pure_module::PureExpr;
use crate::ir::var_id::VarId;

/// A pass that evaluates the constant parts of a Pure expression at compile
/// time.
///
/// - Operations whose operands are constant fold to their result, with the
///   backend semantics.
/// - Constants bound by a let propagate to the variable's use sites, and
///   the let itself is dropped. Dropping it right away matters for folding,
///   since a let left wrapping a literal would hide it from the rules below,
///   all of which match on a literal node directly.
/// - A match with a known subject is replaced by the selected arm, with the
///   arm's bindings turned into ordinary lets.
/// - A field access on a record literal projects the field.
pub fn perform_partial_evaluation(expr: PureExpr, expr_ids: &mut ExprIdCounter) -> PureExpr {
    let mut env = HashMap::new();
    eval(expr, &mut env, expr_ids)
}

/// Evaluate expr with an environment of known-constant bindings.
fn eval(
    expr: PureExpr,
    env: &mut HashMap<VarId, PureExpr>,
    expr_ids: &mut ExprIdCounter,
) -> PureExpr {
    match expr {
        PureExpr::Let {
            var,
            value,
            body,
            typ,
            id,
        } => {
            let value = eval(*value, env, expr_ids);
            if is_const(&value) {
                // Each copy taken out of the environment gets fresh ids, so
                // the use sites never share ids with each other.
                env.insert(var.id, instantiate(&value, expr_ids));
                // Every reference to `var` in the body is substituted from
                // the environment, so the binding is dead and the let goes
                // away with it. This keeps the constant directly visible to
                // the enclosing node, which is what lets a single pass
                // reach a fixpoint.
                return eval(*body, env, expr_ids);
            }
            let body = eval(*body, env, expr_ids);
            PureExpr::Let {
                var,
                value: Box::new(value),
                body: Box::new(body),
                typ,
                id,
            }
        }

        PureExpr::VariableReference { value, typ, id } => match env.get(&value.id) {
            Some(constant) => instantiate(constant, expr_ids),
            None => PureExpr::VariableReference { value, typ, id },
        },

        PureExpr::Match { match_, typ, id } => match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => {
                let subject = eval(*subject, env, expr_ids);
                match subject {
                    PureExpr::BooleanLiteral { value: true, .. } => eval(*true_body, env, expr_ids),
                    PureExpr::BooleanLiteral { value: false, .. } => {
                        eval(*false_body, env, expr_ids)
                    }
                    subject => PureExpr::Match {
                        match_: Match::Bool {
                            subject: Box::new(subject),
                            true_body: Box::new(eval(*true_body, env, expr_ids)),
                            false_body: Box::new(eval(*false_body, env, expr_ids)),
                        },
                        typ,
                        id,
                    },
                }
            }
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => {
                let subject = eval(*subject, env, expr_ids);
                match subject {
                    PureExpr::OptionLiteral {
                        value: Some(inner), ..
                    } => {
                        let selected = match some_arm_binding {
                            Some(binding) => PureExpr::Let {
                                var: binding,
                                value: inner,
                                body: some_arm_body,
                                typ,
                                id: expr_ids.next(),
                            },
                            None => *some_arm_body,
                        };
                        eval(selected, env, expr_ids)
                    }
                    PureExpr::OptionLiteral { value: None, .. } => {
                        eval(*none_arm_body, env, expr_ids)
                    }
                    subject => PureExpr::Match {
                        match_: Match::Option {
                            subject: Box::new(subject),
                            some_arm_binding,
                            some_arm_body: Box::new(eval(*some_arm_body, env, expr_ids)),
                            none_arm_body: Box::new(eval(*none_arm_body, env, expr_ids)),
                        },
                        typ,
                        id,
                    },
                }
            }
            Match::Enum { subject, arms } => {
                let subject = eval(*subject, env, expr_ids);
                match subject {
                    PureExpr::EnumLiteral {
                        enum_name,
                        variant_name,
                        fields,
                        ..
                    } => {
                        let arm = arms
                            .into_iter()
                            .find(|arm| {
                                let EnumPattern::Variant {
                                    variant_name: arm_variant,
                                    ..
                                } = &arm.pattern;
                                arm_variant.as_str() == variant_name.as_str()
                            })
                            .unwrap_or_else(|| {
                                panic!("no match arm for variant {enum_name}::{variant_name}")
                            });
                        // Turn the arm's field bindings into ordinary lets
                        // around the arm body.
                        let mut field_values: HashMap<_, _> = fields.into_iter().collect();
                        let mut selected = arm.body;
                        for (field_name, var) in arm.bindings.into_iter().rev() {
                            let value = field_values.remove(&field_name).unwrap_or_else(|| {
                                panic!(
                                    "variant {enum_name}::{variant_name} has no field {}",
                                    field_name.as_str()
                                )
                            });
                            selected = PureExpr::Let {
                                var,
                                value: Box::new(value),
                                body: Box::new(selected),
                                typ: typ.clone(),
                                id: expr_ids.next(),
                            };
                        }
                        eval(selected, env, expr_ids)
                    }
                    subject => PureExpr::Match {
                        match_: Match::Enum {
                            subject: Box::new(subject),
                            arms: arms
                                .into_iter()
                                .map(|mut arm| {
                                    arm.body = eval(arm.body, env, expr_ids);
                                    arm
                                })
                                .collect(),
                        },
                        typ,
                        id,
                    },
                }
            }
        },

        expr => try_fold(expr.map_children(&mut |child| eval(child, env, expr_ids))),
    }
}

/// Fold an operation whose children are already evaluated. Returns the
/// folded literal when the operand shapes allow it, and the expression
/// unchanged otherwise.
fn try_fold(expr: PureExpr) -> PureExpr {
    match expr {
        PureExpr::BooleanNegation { operand, id } => match *operand {
            PureExpr::BooleanLiteral { value, .. } => {
                PureExpr::BooleanLiteral { value: !value, id }
            }
            operand => PureExpr::BooleanNegation {
                operand: Box::new(operand),
                id,
            },
        },

        PureExpr::NumericNegation {
            operand,
            operand_type,
            id,
        } => match *operand {
            PureExpr::IntLiteral { value, .. } => PureExpr::IntLiteral {
                value: value.wrapping_neg(),
                id,
            },
            PureExpr::FloatLiteral { value, .. } => PureExpr::FloatLiteral { value: -value, id },
            operand => PureExpr::NumericNegation {
                operand: Box::new(operand),
                operand_type,
                id,
            },
        },

        PureExpr::IntToString { value, id } => match *value {
            PureExpr::IntLiteral { value, .. } => PureExpr::StringLiteral {
                value: CheapString::new(value.to_string()),
                id,
            },
            value => PureExpr::IntToString {
                value: Box::new(value),
                id,
            },
        },

        PureExpr::FloatToInt { value, id } => match *value {
            PureExpr::FloatLiteral { value, .. } => PureExpr::IntLiteral {
                value: value as i32,
                id,
            },
            value => PureExpr::FloatToInt {
                value: Box::new(value),
                id,
            },
        },

        PureExpr::IntToFloat { value, id } => match *value {
            PureExpr::IntLiteral { value, .. } => PureExpr::FloatLiteral {
                value: value as f64,
                id,
            },
            value => PureExpr::IntToFloat {
                value: Box::new(value),
                id,
            },
        },

        PureExpr::StringIsEmpty { string, id } => match *string {
            PureExpr::StringLiteral { value, .. } => PureExpr::BooleanLiteral {
                value: value.as_str().is_empty(),
                id,
            },
            string => PureExpr::StringIsEmpty {
                string: Box::new(string),
                id,
            },
        },

        PureExpr::OptionIsSome { option, id } => match *option {
            PureExpr::OptionLiteral { value, .. } => PureExpr::BooleanLiteral {
                value: value.is_some(),
                id,
            },
            option => PureExpr::OptionIsSome {
                option: Box::new(option),
                id,
            },
        },

        PureExpr::OptionIsNone { option, id } => match *option {
            PureExpr::OptionLiteral { value, .. } => PureExpr::BooleanLiteral {
                value: value.is_none(),
                id,
            },
            option => PureExpr::OptionIsNone {
                option: Box::new(option),
                id,
            },
        },

        PureExpr::ArrayIsEmpty { array, id } => match *array {
            PureExpr::ArrayLiteral { elements, .. } => PureExpr::BooleanLiteral {
                value: elements.is_empty(),
                id,
            },
            array => PureExpr::ArrayIsEmpty {
                array: Box::new(array),
                id,
            },
        },

        PureExpr::ArrayLength { array, id } => match *array {
            PureExpr::ArrayLiteral { elements, .. } => PureExpr::IntLiteral {
                value: elements.len() as i32,
                id,
            },
            array => PureExpr::ArrayLength {
                array: Box::new(array),
                id,
            },
        },

        PureExpr::FieldAccess {
            record,
            field,
            typ,
            id,
        } => match *record {
            PureExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => fields
                .into_iter()
                .find(|(name, _)| name.as_str() == field.as_str())
                .map(|(_, value)| value)
                .unwrap_or_else(|| panic!("record {record_name} has no field {}", field.as_str())),
            record => PureExpr::FieldAccess {
                record: Box::new(record),
                field,
                typ,
                id,
            },
        },

        PureExpr::Equals {
            left,
            right,
            operand_types,
            id,
        } => match (*left, *right) {
            (
                PureExpr::BooleanLiteral { value: l, .. },
                PureExpr::BooleanLiteral { value: r, .. },
            ) => PureExpr::BooleanLiteral { value: l == r, id },
            (
                PureExpr::StringLiteral { value: l, .. },
                PureExpr::StringLiteral { value: r, .. },
            ) => PureExpr::BooleanLiteral { value: l == r, id },
            (PureExpr::IntLiteral { value: l, .. }, PureExpr::IntLiteral { value: r, .. }) => {
                PureExpr::BooleanLiteral { value: l == r, id }
            }
            (PureExpr::FloatLiteral { value: l, .. }, PureExpr::FloatLiteral { value: r, .. }) => {
                PureExpr::BooleanLiteral { value: l == r, id }
            }
            (left, right) => PureExpr::Equals {
                left: Box::new(left),
                right: Box::new(right),
                operand_types,
                id,
            },
        },

        PureExpr::StringConcat { parts, id } => {
            let mut merged: Vec<PureExpr> = Vec::with_capacity(parts.len());
            for part in parts {
                let subparts = match part {
                    PureExpr::StringConcat { parts, .. } => parts,
                    part => vec![part],
                };
                for part in subparts {
                    match (merged.last_mut(), part) {
                        (_, PureExpr::StringLiteral { value, .. }) if value.as_str().is_empty() => {
                        }
                        (
                            Some(PureExpr::StringLiteral {
                                value: accumulated, ..
                            }),
                            PureExpr::StringLiteral { value, .. },
                        ) => {
                            let mut combined = String::with_capacity(
                                accumulated.as_str().len() + value.as_str().len(),
                            );
                            combined.push_str(accumulated.as_str());
                            combined.push_str(value.as_str());
                            *accumulated = CheapString::new(combined);
                        }
                        (_, part) => merged.push(part),
                    }
                }
            }
            match merged.len() {
                0 => PureExpr::StringLiteral {
                    value: CheapString::new(String::new()),
                    id,
                },
                1 => merged.pop().unwrap(),
                _ => PureExpr::StringConcat { parts: merged, id },
            }
        }

        PureExpr::BooleanLogicalAnd { left, right, id } => match (*left, *right) {
            (
                PureExpr::BooleanLiteral { value: l, .. },
                PureExpr::BooleanLiteral { value: r, .. },
            ) => PureExpr::BooleanLiteral { value: l && r, id },
            (left, right) => PureExpr::BooleanLogicalAnd {
                left: Box::new(left),
                right: Box::new(right),
                id,
            },
        },

        PureExpr::BooleanLogicalOr { left, right, id } => match (*left, *right) {
            (
                PureExpr::BooleanLiteral { value: l, .. },
                PureExpr::BooleanLiteral { value: r, .. },
            ) => PureExpr::BooleanLiteral { value: l || r, id },
            (left, right) => PureExpr::BooleanLogicalOr {
                left: Box::new(left),
                right: Box::new(right),
                id,
            },
        },

        PureExpr::NumericAdd {
            left,
            right,
            operand_types,
            id,
        } => match (*left, *right) {
            (PureExpr::IntLiteral { value: l, .. }, PureExpr::IntLiteral { value: r, .. }) => {
                PureExpr::IntLiteral {
                    value: l.wrapping_add(r),
                    id,
                }
            }
            (PureExpr::FloatLiteral { value: l, .. }, PureExpr::FloatLiteral { value: r, .. }) => {
                PureExpr::FloatLiteral { value: l + r, id }
            }
            (left, right) => PureExpr::NumericAdd {
                left: Box::new(left),
                right: Box::new(right),
                operand_types,
                id,
            },
        },

        PureExpr::NumericSubtract {
            left,
            right,
            operand_types,
            id,
        } => match (*left, *right) {
            (PureExpr::IntLiteral { value: l, .. }, PureExpr::IntLiteral { value: r, .. }) => {
                PureExpr::IntLiteral {
                    value: l.wrapping_sub(r),
                    id,
                }
            }
            (PureExpr::FloatLiteral { value: l, .. }, PureExpr::FloatLiteral { value: r, .. }) => {
                PureExpr::FloatLiteral { value: l - r, id }
            }
            (left, right) => PureExpr::NumericSubtract {
                left: Box::new(left),
                right: Box::new(right),
                operand_types,
                id,
            },
        },

        PureExpr::NumericMultiply {
            left,
            right,
            operand_types,
            id,
        } => match (*left, *right) {
            (PureExpr::IntLiteral { value: l, .. }, PureExpr::IntLiteral { value: r, .. }) => {
                PureExpr::IntLiteral {
                    value: l.wrapping_mul(r),
                    id,
                }
            }
            (PureExpr::FloatLiteral { value: l, .. }, PureExpr::FloatLiteral { value: r, .. }) => {
                PureExpr::FloatLiteral { value: l * r, id }
            }
            (left, right) => PureExpr::NumericMultiply {
                left: Box::new(left),
                right: Box::new(right),
                operand_types,
                id,
            },
        },

        PureExpr::LessThan {
            left,
            right,
            operand_types,
            id,
        } => match (*left, *right) {
            (PureExpr::IntLiteral { value: l, .. }, PureExpr::IntLiteral { value: r, .. }) => {
                PureExpr::BooleanLiteral { value: l < r, id }
            }
            (PureExpr::FloatLiteral { value: l, .. }, PureExpr::FloatLiteral { value: r, .. }) => {
                PureExpr::BooleanLiteral { value: l < r, id }
            }
            (left, right) => PureExpr::LessThan {
                left: Box::new(left),
                right: Box::new(right),
                operand_types,
                id,
            },
        },

        PureExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            id,
        } => match (*left, *right) {
            (PureExpr::IntLiteral { value: l, .. }, PureExpr::IntLiteral { value: r, .. }) => {
                PureExpr::BooleanLiteral { value: l <= r, id }
            }
            (PureExpr::FloatLiteral { value: l, .. }, PureExpr::FloatLiteral { value: r, .. }) => {
                PureExpr::BooleanLiteral { value: l <= r, id }
            }
            (left, right) => PureExpr::LessThanOrEqual {
                left: Box::new(left),
                right: Box::new(right),
                operand_types,
                id,
            },
        },

        expr => expr,
    }
}

/// Whether an expression is a fully-known constant, safe to bind into the
/// environment and copy to use sites.
fn is_const(expr: &PureExpr) -> bool {
    match expr {
        PureExpr::BooleanLiteral { .. }
        | PureExpr::StringLiteral { .. }
        | PureExpr::IntLiteral { .. }
        | PureExpr::FloatLiteral { .. } => true,

        PureExpr::EnumLiteral { fields, .. } => fields.iter().all(|(_, value)| is_const(value)),
        PureExpr::RecordLiteral { fields, .. } => fields.iter().all(|(_, value)| is_const(value)),
        PureExpr::ArrayLiteral { elements, .. } => elements.iter().all(is_const),
        PureExpr::OptionLiteral { value, .. } => value.as_ref().is_none_or(|inner| is_const(inner)),

        PureExpr::Let { .. }
        | PureExpr::Match { .. }
        | PureExpr::VariableReference { .. }
        | PureExpr::FieldAccess { .. }
        | PureExpr::FragmentRaw { .. }
        | PureExpr::FragmentEscape { .. }
        | PureExpr::FragmentConcat { .. }
        | PureExpr::FragmentFor { .. }
        | PureExpr::FunctionCall { .. }
        | PureExpr::StringConcat { .. }
        | PureExpr::NumericAdd { .. }
        | PureExpr::NumericSubtract { .. }
        | PureExpr::NumericMultiply { .. }
        | PureExpr::NumericNegation { .. }
        | PureExpr::BooleanNegation { .. }
        | PureExpr::BooleanLogicalAnd { .. }
        | PureExpr::BooleanLogicalOr { .. }
        | PureExpr::Equals { .. }
        | PureExpr::LessThan { .. }
        | PureExpr::LessThanOrEqual { .. }
        | PureExpr::ArrayLength { .. }
        | PureExpr::ArrayIsEmpty { .. }
        | PureExpr::StringIsEmpty { .. }
        | PureExpr::OptionIsSome { .. }
        | PureExpr::OptionIsNone { .. }
        | PureExpr::IntToString { .. }
        | PureExpr::FloatToInt { .. }
        | PureExpr::IntToFloat { .. } => false,
    }
}

/// Copy a constant with fresh ids for every node. Only called on is_const
/// expressions.
fn instantiate(expr: &PureExpr, expr_ids: &mut ExprIdCounter) -> PureExpr {
    match expr {
        PureExpr::BooleanLiteral { value, .. } => PureExpr::BooleanLiteral {
            value: *value,
            id: expr_ids.next(),
        },
        PureExpr::StringLiteral { value, .. } => PureExpr::StringLiteral {
            value: value.clone(),
            id: expr_ids.next(),
        },
        PureExpr::IntLiteral { value, .. } => PureExpr::IntLiteral {
            value: *value,
            id: expr_ids.next(),
        },
        PureExpr::FloatLiteral { value, .. } => PureExpr::FloatLiteral {
            value: *value,
            id: expr_ids.next(),
        },
        PureExpr::EnumLiteral {
            enum_name,
            variant_name,
            fields,
            typ,
            ..
        } => PureExpr::EnumLiteral {
            enum_name: enum_name.clone(),
            variant_name: variant_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), instantiate(value, expr_ids)))
                .collect(),
            typ: typ.clone(),
            id: expr_ids.next(),
        },
        PureExpr::RecordLiteral {
            record_name,
            fields,
            typ,
            ..
        } => PureExpr::RecordLiteral {
            record_name: record_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), instantiate(value, expr_ids)))
                .collect(),
            typ: typ.clone(),
            id: expr_ids.next(),
        },
        PureExpr::ArrayLiteral { elements, typ, .. } => PureExpr::ArrayLiteral {
            elements: elements
                .iter()
                .map(|element| instantiate(element, expr_ids))
                .collect(),
            typ: typ.clone(),
            id: expr_ids.next(),
        },
        PureExpr::OptionLiteral { value, typ, .. } => PureExpr::OptionLiteral {
            value: value
                .as_ref()
                .map(|inner| Box::new(instantiate(inner, expr_ids))),
            typ: typ.clone(),
            id: expr_ids.next(),
        },

        PureExpr::Let { .. }
        | PureExpr::Match { .. }
        | PureExpr::VariableReference { .. }
        | PureExpr::FieldAccess { .. }
        | PureExpr::FragmentRaw { .. }
        | PureExpr::FragmentEscape { .. }
        | PureExpr::FragmentConcat { .. }
        | PureExpr::FragmentFor { .. }
        | PureExpr::FunctionCall { .. }
        | PureExpr::StringConcat { .. }
        | PureExpr::NumericAdd { .. }
        | PureExpr::NumericSubtract { .. }
        | PureExpr::NumericMultiply { .. }
        | PureExpr::NumericNegation { .. }
        | PureExpr::BooleanNegation { .. }
        | PureExpr::BooleanLogicalAnd { .. }
        | PureExpr::BooleanLogicalOr { .. }
        | PureExpr::Equals { .. }
        | PureExpr::LessThan { .. }
        | PureExpr::LessThanOrEqual { .. }
        | PureExpr::ArrayLength { .. }
        | PureExpr::ArrayIsEmpty { .. }
        | PureExpr::StringIsEmpty { .. }
        | PureExpr::OptionIsSome { .. }
        | PureExpr::OptionIsNone { .. }
        | PureExpr::IntToString { .. }
        | PureExpr::FloatToInt { .. }
        | PureExpr::IntToFloat { .. } => {
            unreachable!("instantiate called on a non-constant expression")
        }
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
    fn fuzz_random_modules_evaluate_identically_after_partial_evaluation() {
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
        let mut expr_ids = module.expr_ids;
        let pages = module
            .pages
            .into_iter()
            .map(|page| PurePageDeclaration {
                name: page.name,
                parameters: page.parameters,
                body: perform_partial_evaluation(page.body, &mut expr_ids),
            })
            .collect();
        let functions = module
            .functions
            .into_iter()
            .map(|function| PureFunctionDeclaration {
                name: function.name,
                parameters: function.parameters,
                return_type: function.return_type,
                body: perform_partial_evaluation(function.body, &mut expr_ids),
            })
            .collect();
        PureModule {
            pages,
            functions,
            records: module.records,
            enums: module.enums,
            expr_ids,
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
    fn should_evaluate_bool_match_with_negated_subject() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.bool_match_expr(
                        t.not(t.not(t.bool(true))),
                        t.raw("yes"),
                        t.raw("no"),
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  concat(
                    match (!(!true)) {
                      true => { raw("yes") }
                      false => { raw("no") }
                    },
                  )
                }

                -- after --
                page Test() {
                  concat(raw("yes"))
                }
            "#]],
        );
    }

    #[test]
    fn should_preserve_match_with_dynamic_subject() {
        check(
            PureModuleBuilder::new()
                .view("Test", [("flag", "Bool")], |t| {
                    t.concat(vec![t.bool_match_expr(
                        t.var("flag"),
                        t.raw("yes"),
                        t.raw("no"),
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(flag@v0: Bool) {
                  concat(
                    match v0 {
                      true => { raw("yes") }
                      false => { raw("no") }
                    },
                  )
                }

                -- after --
                page Test(flag@v0: Bool) {
                  concat(
                    match v0 {
                      true => { raw("yes") }
                      false => { raw("no") }
                    },
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_constants_through_variables() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("greeting", t.str("Hello"), |t| {
                        t.concat(vec![
                            t.escape(t.var("greeting")),
                            t.escape(t.var("greeting")),
                        ])
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  let v0 = "Hello" in { concat(escape(v0), escape(v0)) }
                }

                -- after --
                page Test() {
                  concat(escape("Hello"), escape("Hello"))
                }
            "#]],
        );
    }

    #[test]
    fn should_merge_constants_adjacent_across_a_dynamic_part() {
        check(
            PureModuleBuilder::new()
                .view("Test", vec![("dyn", "String")], |t| {
                    t.escape(t.join(vec![t.var("dyn"), t.str("b"), t.str("c"), t.str("d")]))
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(dyn@v0: String) {
                  escape((v0 + " " + "b" + " " + "c" + " " + "d"))
                }

                -- after --
                page Test(dyn@v0: String) {
                  escape((v0 + " b c d"))
                }
            "#]],
        );
    }

    #[test]
    fn should_flatten_nested_string_concatenation_before_merging() {
        check(
            PureModuleBuilder::new()
                .view("Test", vec![("dyn", "String")], |t| {
                    t.escape(t.string_concat(vec![
                        t.string_concat(vec![t.var("dyn"), t.str("a")]),
                        t.string_concat(vec![t.str("b"), t.var("dyn")]),
                    ]))
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(dyn@v0: String) {
                  escape(((v0 + "a") + ("b" + v0)))
                }

                -- after --
                page Test(dyn@v0: String) {
                  escape((v0 + "ab" + v0))
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_empty_strings_from_concatenation() {
        check(
            PureModuleBuilder::new()
                .view("Test", vec![("dyn", "String")], |t| {
                    t.escape(t.string_concat(vec![t.str(""), t.var("dyn"), t.str("")]))
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(dyn@v0: String) {
                  escape(("" + v0 + ""))
                }

                -- after --
                page Test(dyn@v0: String) {
                  escape(v0)
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_string_concatenation_with_propagated_variables() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.let_expr("name", t.str("World"), |t| {
                        t.concat(vec![
                            t.escape(t.string_concat(vec![t.str("Hello, "), t.var("name")])),
                        ])
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  let v0 = "World" in { concat(escape(("Hello, " + v0))) }
                }

                -- after --
                page Test() {
                  concat(escape("Hello, World"))
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_equality_selecting_match_arm() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.bool_match_expr(
                        t.eq(t.str("a"), t.str("b")),
                        t.raw("equal"),
                        t.raw("different"),
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  concat(
                    match ("a" == "b") {
                      true => { raw("equal") }
                      false => { raw("different") }
                    },
                  )
                }

                -- after --
                page Test() {
                  concat(raw("different"))
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_arithmetic_with_wrapping() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![
                        t.escape(t.int_to_string(t.add(t.int(i32::MAX), t.int(1)))),
                    ])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  concat(escape((2147483647 + 1).to_string()))
                }

                -- after --
                page Test() {
                  concat(escape("-2147483648"))
                }
            "#]],
        );
    }

    #[test]
    fn should_saturate_float_to_int_conversion() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![
                        t.escape(t.int_to_string(t.float_to_int(t.float(1e300)))),
                    ])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  concat(
                    escape(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.to_int().to_string()),
                  )
                }

                -- after --
                page Test() {
                  concat(escape("2147483647"))
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_option_match_with_some_binding() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.option_match_expr_with_binding(
                        t.some(t.str("present")),
                        "v",
                        |t| t.escape(t.var("v")),
                        t.raw("none"),
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  concat(
                    match Option[String]::Some("present") {
                      Some(v0) => { escape(v0) }
                      None => { raw("none") }
                    },
                  )
                }

                -- after --
                page Test() {
                  concat(escape("present"))
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_option_match_with_none() {
        check(
            PureModuleBuilder::new()
                .view_no_params("Test", |t| {
                    t.concat(vec![t.option_match_expr_with_binding(
                        t.none("String"),
                        "v",
                        |t| t.escape(t.var("v")),
                        t.raw("none"),
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test() {
                  concat(
                    match Option[String]::None {
                      Some(v0) => { escape(v0) }
                      None => { raw("none") }
                    },
                  )
                }

                -- after --
                page Test() {
                  concat(raw("none"))
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_enum_match_selecting_correct_arm_with_bindings() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Status",
                    [
                        ("Active", vec![("since", "String"), ("by", "String")]),
                        ("Inactive", vec![]),
                    ],
                )
                .view_no_params("Test", |t| {
                    t.concat(vec![t.enum_match_expr(
                        t.enum_variant_with_fields(
                            "Status",
                            "Active",
                            vec![("since", t.str("today")), ("by", t.str("admin"))],
                        ),
                        |arms| {
                            arms.arm_bound("Active", [("since", "s"), ("by", "b")], |t| {
                                t.escape(t.string_concat(vec![
                                    t.var("s"),
                                    t.string_concat(vec![t.str(" / "), t.var("b")]),
                                ]))
                            });
                            arms.arm("Inactive", |t| t.raw("inactive"));
                        },
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                enum Status {
                  Active {since: String, by: String},
                  Inactive,
                }
                page Test() {
                  concat(
                    match Status::Active {since: "today", by: "admin"} {
                      Status::Active {since: v0, by: v1} => {
                        escape((v0 + (" / " + v1)))
                      }
                      Status::Inactive => { raw("inactive") }
                    },
                  )
                }

                -- after --
                enum Status {
                  Active {since: String, by: String},
                  Inactive,
                }
                page Test() {
                  concat(escape("today / admin"))
                }
            "#]],
        );
    }

    #[test]
    fn should_propagate_enum_constant_through_variables() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Status",
                    [("Active", vec![("since", "String")]), ("Inactive", vec![])],
                )
                .view_no_params("Test", |t| {
                    t.let_expr(
                        "status",
                        t.enum_variant_with_fields(
                            "Status",
                            "Active",
                            vec![("since", t.str("now"))],
                        ),
                        |t| {
                            t.concat(vec![t.enum_match_expr(t.var("status"), |arms| {
                                arms.arm_bound("Active", [("since", "s")], |t| {
                                    t.escape(t.var("s"))
                                });
                                arms.arm("Inactive", |t| t.raw("inactive"));
                            })])
                        },
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                enum Status {
                  Active {since: String},
                  Inactive,
                }
                page Test() {
                  let v0 = Status::Active {since: "now"} in {
                    concat(
                      match v0 {
                        Status::Active {since: v1} => { escape(v1) }
                        Status::Inactive => { raw("inactive") }
                      },
                    )
                  }
                }

                -- after --
                enum Status {
                  Active {since: String},
                  Inactive,
                }
                page Test() {
                  concat(escape("now"))
                }
            "#]],
        );
    }

    #[test]
    fn should_select_enum_arm_with_dynamic_field() {
        // Arm selection only needs the variant; the bound field stays
        // dynamic and becomes an ordinary let.
        check(
            PureModuleBuilder::new()
                .enum_("Wrap", [("Value", vec![("inner", "String")])])
                .view("Test", [("x", "String")], |t| {
                    t.concat(vec![t.enum_match_expr(
                        t.enum_variant_with_fields("Wrap", "Value", vec![("inner", t.var("x"))]),
                        |arms| {
                            arms.arm_bound("Value", [("inner", "v")], |t| t.escape(t.var("v")));
                        },
                    )])
                })
                .build(),
            expect![[r#"
                -- before --
                enum Wrap {
                  Value {inner: String},
                }
                page Test(x@v0: String) {
                  concat(
                    match Wrap::Value {inner: v0} {
                      Wrap::Value {inner: v1} => { escape(v1) }
                    },
                  )
                }

                -- after --
                enum Wrap {
                  Value {inner: String},
                }
                page Test(x@v0: String) {
                  concat(let v1 = v0 in { escape(v1) })
                }
            "#]],
        );
    }

    #[test]
    fn should_project_field_access_on_record_literal() {
        check(
            PureModuleBuilder::new()
                .record("User", [("name", "String"), ("title", "String")])
                .view("Test", [("dynamic", "String")], |t| {
                    t.concat(vec![t.escape(t.field_access(
                        t.record(
                            "User",
                            vec![("name", t.str("Ada")), ("title", t.var("dynamic"))],
                        ),
                        "name",
                    ))])
                })
                .build(),
            expect![[r#"
                -- before --
                record User {
                  name: String,
                  title: String,
                }
                page Test(dynamic@v0: String) {
                  concat(escape(User {name: "Ada", title: v0}.name))
                }

                -- after --
                record User {
                  name: String,
                  title: String,
                }
                page Test(dynamic@v0: String) {
                  concat(escape("Ada"))
                }
            "#]],
        );
    }

    #[test]
    fn should_evaluate_array_length_with_dynamic_elements() {
        check(
            PureModuleBuilder::new()
                .view("Test", [("x", "Int")], |t| {
                    t.concat(vec![t.escape(t.int_to_string(
                        t.array_length(t.array(vec![t.var("x"), t.int(2)])),
                    ))])
                })
                .build(),
            expect![[r#"
                -- before --
                page Test(x@v0: Int) {
                  concat(escape([v0, 2].len().to_string()))
                }

                -- after --
                page Test(x@v0: Int) {
                  concat(escape("2"))
                }
            "#]],
        );
    }
}
