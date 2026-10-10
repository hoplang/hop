use std::collections::HashMap;

use crate::document::CheapString;
use crate::hop::typing::Type;
use crate::ir::binder_id::BinderId;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatOp};
use crate::ir::ir_binary_op::IrBinaryOp;
use crate::ir::ir_match::{EnumMatchArm, EnumPattern, Match};
use crate::ir::ir_unary_op::IrUnaryOp;
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// A pass that evaluates the constant parts of a block at compile time.
///
/// - An op whose operands are constants becomes the constant it computes,
///   with the backend semantics.
/// - A field access on a record constructor reads the constructor's
///   operand, and a tuple index on a tuple constructor likewise.
/// - A match whose subject is a constructor runs the selected arm in place
///   of the match, and a Read of one of the arm's binders is the
///   constructor's operand it binds.
/// - A string concat takes the parts of a nested concat as its own, drops
///   empty constants and merges adjacent constants. An html concat takes
///   the parts of a nested concat as its own. A concat of one part is that
///   part.
///
/// A binding that turns out to be another name is dropped, and what read
/// it reads that name. The bindings nothing reads any more are left for
/// `eliminate_dead_bindings`.
///
/// One pass in binding order sees every operand's op before the op that
/// reads it, so nothing is left for a second pass.
pub fn perform_partial_evaluation(block: FlatBlock, value_ids: &mut ValueIdCounter) -> FlatBlock {
    let mut evaluator = Evaluator {
        value_ids,
        ops: HashMap::new(),
        renames: HashMap::new(),
        binders: HashMap::new(),
    };
    evaluator.evaluate_block(block)
}

/// What a binding folded to.
enum Folded {
    /// An op, bound to the name as before.
    Op(FlatOp),
    /// Another name, which the readers of the binding read instead.
    Name(ValueId),
}

struct Evaluator<'a> {
    value_ids: &'a mut ValueIdCounter,
    /// The ops of the bindings kept so far, except those holding blocks,
    /// for their readers to look at. Names are unique, so one map serves
    /// every nested block.
    ops: HashMap<ValueId, FlatOp>,
    /// The bindings that turned out to be another name.
    renames: HashMap<ValueId, ValueId>,
    /// The binders of selected arms, each with the constructor operand it
    /// stands for. A Read of one is that operand.
    binders: HashMap<BinderId, ValueId>,
}

impl Evaluator<'_> {
    fn resolve(&self, name: ValueId) -> ValueId {
        self.renames.get(&name).copied().unwrap_or(name)
    }

    fn evaluate_block(&mut self, block: FlatBlock) -> FlatBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.evaluate_binding(binding, &mut bindings);
        }
        FlatBlock {
            bindings,
            result: self.resolve(block.result),
        }
    }

    /// Run the arm selected for the match `name`, its bindings joining the
    /// block the match sits in, and let the readers of `name` read the
    /// arm's result.
    fn select_arm(&mut self, name: ValueId, arm: FlatBlock, out: &mut Vec<FlatBinding>) {
        for binding in arm.bindings {
            self.evaluate_binding(binding, out);
        }
        let result = self.resolve(arm.result);
        self.renames.insert(name, result);
    }

    fn evaluate_binding(&mut self, binding: FlatBinding, out: &mut Vec<FlatBinding>) {
        let FlatBinding { name, typ, mut op } = binding;
        op.for_each_operand_mut(&mut |operand| *operand = self.resolve(*operand));
        let op = match op {
            FlatOp::Match(match_) => {
                let match_ = match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => match self.ops.get(&subject) {
                        Some(FlatOp::BoolLiteral(true)) => {
                            self.select_arm(name, true_body, out);
                            return;
                        }
                        Some(FlatOp::BoolLiteral(false)) => {
                            self.select_arm(name, false_body, out);
                            return;
                        }
                        _ => Match::Bool {
                            subject,
                            true_body: self.evaluate_block(true_body),
                            false_body: self.evaluate_block(false_body),
                        },
                    },
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => match self.ops.get(&subject) {
                        Some(FlatOp::Option(Some(inner))) => {
                            let inner = *inner;
                            if let Some(binder) = some_arm_binding {
                                self.binders.insert(binder.var, inner);
                            }
                            self.select_arm(name, some_arm_body, out);
                            return;
                        }
                        Some(FlatOp::Option(None)) => {
                            self.select_arm(name, none_arm_body, out);
                            return;
                        }
                        _ => Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body: self.evaluate_block(some_arm_body),
                            none_arm_body: self.evaluate_block(none_arm_body),
                        },
                    },
                    Match::Enum { subject, arms } => match self.ops.get(&subject) {
                        Some(FlatOp::Enum {
                            variant_name,
                            fields,
                        }) => {
                            let variant_name = variant_name.clone();
                            let fields = fields.clone();
                            let arm = arms
                                .into_iter()
                                .find(|arm| {
                                    let EnumPattern::Variant {
                                        variant_name: pattern,
                                        ..
                                    } = &arm.pattern;
                                    *pattern == variant_name
                                })
                                .unwrap_or_else(|| {
                                    panic!("no match arm for variant {}", variant_name.as_str())
                                });
                            for (field, binder) in arm.bindings {
                                let value = fields
                                    .iter()
                                    .find(|(name, _)| *name == field)
                                    .map(|(_, value)| *value)
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "variant {} has no field {}",
                                            variant_name.as_str(),
                                            field.as_str()
                                        )
                                    });
                                self.binders.insert(binder.var, value);
                            }
                            self.select_arm(name, arm.body, out);
                            return;
                        }
                        _ => Match::Enum {
                            subject,
                            arms: arms
                                .into_iter()
                                .map(|arm| EnumMatchArm {
                                    pattern: arm.pattern,
                                    bindings: arm.bindings,
                                    body: self.evaluate_block(arm.body),
                                })
                                .collect(),
                        },
                    },
                };
                out.push(FlatBinding {
                    name,
                    typ,
                    op: FlatOp::Match(match_),
                });
                return;
            }

            FlatOp::HtmlFor { var, source, body } => {
                let body = self.evaluate_block(body);
                out.push(FlatBinding {
                    name,
                    typ,
                    op: FlatOp::HtmlFor { var, source, body },
                });
                return;
            }

            op => op,
        };
        match self.fold(op, out) {
            Folded::Name(target) => {
                self.renames.insert(name, target);
            }
            Folded::Op(op) => {
                self.ops.insert(name, op.clone());
                out.push(FlatBinding { name, typ, op });
            }
        }
    }

    /// Fold an op whose operands are resolved. Returns the constant or
    /// name it computes when the operands allow it, and the op unchanged
    /// otherwise.
    fn fold(&mut self, op: FlatOp, out: &mut Vec<FlatBinding>) -> Folded {
        match op {
            FlatOp::Read(binder) => match self.binders.get(&binder) {
                Some(value) => Folded::Name(*value),
                None => Folded::Op(FlatOp::Read(binder)),
            },

            FlatOp::FieldAccess { record, field } => match self.ops.get(&record) {
                Some(FlatOp::Record { fields }) => Folded::Name(
                    fields
                        .iter()
                        .find(|(name, _)| *name == field)
                        .map(|(_, value)| *value)
                        .unwrap_or_else(|| panic!("record has no field {}", field.as_str())),
                ),
                _ => Folded::Op(FlatOp::FieldAccess { record, field }),
            },

            FlatOp::TupleIndex { tuple, index } => match self.ops.get(&tuple) {
                Some(FlatOp::Tuple(elements)) => {
                    let len = elements.len();
                    Folded::Name(*elements.get(index).unwrap_or_else(|| {
                        panic!("index {index} is out of range for a tuple of {len}")
                    }))
                }
                _ => Folded::Op(FlatOp::TupleIndex { tuple, index }),
            },

            FlatOp::Binary { op, left, right } => {
                let folded = match (&op, self.ops.get(&left), self.ops.get(&right)) {
                    (
                        IrBinaryOp::NumericAdd(_),
                        Some(FlatOp::IntLiteral(l)),
                        Some(FlatOp::IntLiteral(r)),
                    ) => Some(FlatOp::IntLiteral(l.wrapping_add(*r))),
                    (
                        IrBinaryOp::NumericAdd(_),
                        Some(FlatOp::FloatLiteral(l)),
                        Some(FlatOp::FloatLiteral(r)),
                    ) => Some(FlatOp::FloatLiteral(l + r)),
                    (
                        IrBinaryOp::NumericSubtract(_),
                        Some(FlatOp::IntLiteral(l)),
                        Some(FlatOp::IntLiteral(r)),
                    ) => Some(FlatOp::IntLiteral(l.wrapping_sub(*r))),
                    (
                        IrBinaryOp::NumericSubtract(_),
                        Some(FlatOp::FloatLiteral(l)),
                        Some(FlatOp::FloatLiteral(r)),
                    ) => Some(FlatOp::FloatLiteral(l - r)),
                    (
                        IrBinaryOp::NumericMultiply(_),
                        Some(FlatOp::IntLiteral(l)),
                        Some(FlatOp::IntLiteral(r)),
                    ) => Some(FlatOp::IntLiteral(l.wrapping_mul(*r))),
                    (
                        IrBinaryOp::NumericMultiply(_),
                        Some(FlatOp::FloatLiteral(l)),
                        Some(FlatOp::FloatLiteral(r)),
                    ) => Some(FlatOp::FloatLiteral(l * r)),
                    (
                        IrBinaryOp::Equals(_),
                        Some(FlatOp::BoolLiteral(l)),
                        Some(FlatOp::BoolLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l == r)),
                    (
                        IrBinaryOp::Equals(_),
                        Some(FlatOp::StringLiteral(l)),
                        Some(FlatOp::StringLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l.as_str() == r.as_str())),
                    (
                        IrBinaryOp::Equals(_),
                        Some(FlatOp::IntLiteral(l)),
                        Some(FlatOp::IntLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l == r)),
                    (
                        IrBinaryOp::Equals(_),
                        Some(FlatOp::FloatLiteral(l)),
                        Some(FlatOp::FloatLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l == r)),
                    (
                        IrBinaryOp::LessThan(_),
                        Some(FlatOp::IntLiteral(l)),
                        Some(FlatOp::IntLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l < r)),
                    (
                        IrBinaryOp::LessThan(_),
                        Some(FlatOp::FloatLiteral(l)),
                        Some(FlatOp::FloatLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l < r)),
                    (
                        IrBinaryOp::LessThanOrEqual(_),
                        Some(FlatOp::IntLiteral(l)),
                        Some(FlatOp::IntLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l <= r)),
                    (
                        IrBinaryOp::LessThanOrEqual(_),
                        Some(FlatOp::FloatLiteral(l)),
                        Some(FlatOp::FloatLiteral(r)),
                    ) => Some(FlatOp::BoolLiteral(l <= r)),
                    _ => None,
                };
                Folded::Op(folded.unwrap_or(FlatOp::Binary { op, left, right }))
            }

            FlatOp::Unary { op, operand } => {
                let folded = match (&op, self.ops.get(&operand)) {
                    (IrUnaryOp::NumericNegation(_), Some(FlatOp::IntLiteral(value))) => {
                        Some(FlatOp::IntLiteral(value.wrapping_neg()))
                    }
                    (IrUnaryOp::NumericNegation(_), Some(FlatOp::FloatLiteral(value))) => {
                        Some(FlatOp::FloatLiteral(-value))
                    }
                    (IrUnaryOp::BoolNegation, Some(FlatOp::BoolLiteral(value))) => {
                        Some(FlatOp::BoolLiteral(!value))
                    }
                    (IrUnaryOp::IntToString, Some(FlatOp::IntLiteral(value))) => {
                        Some(FlatOp::StringLiteral(CheapString::new(value.to_string())))
                    }
                    (IrUnaryOp::FloatToInt, Some(FlatOp::FloatLiteral(value))) => {
                        Some(FlatOp::IntLiteral(*value as i32))
                    }
                    (IrUnaryOp::IntToFloat, Some(FlatOp::IntLiteral(value))) => {
                        Some(FlatOp::FloatLiteral(*value as f64))
                    }
                    (IrUnaryOp::StringIsEmpty, Some(FlatOp::StringLiteral(value))) => {
                        Some(FlatOp::BoolLiteral(value.as_str().is_empty()))
                    }
                    (IrUnaryOp::ArrayIsEmpty, Some(FlatOp::Array(elements))) => {
                        Some(FlatOp::BoolLiteral(elements.is_empty()))
                    }
                    (IrUnaryOp::ArrayLength, Some(FlatOp::Array(elements))) => {
                        Some(FlatOp::IntLiteral(elements.len() as i32))
                    }
                    (IrUnaryOp::OptionIsSome, Some(FlatOp::Option(value))) => {
                        Some(FlatOp::BoolLiteral(value.is_some()))
                    }
                    (IrUnaryOp::OptionIsNone, Some(FlatOp::Option(value))) => {
                        Some(FlatOp::BoolLiteral(value.is_none()))
                    }
                    _ => None,
                };
                Folded::Op(folded.unwrap_or(FlatOp::Unary { op, operand }))
            }

            FlatOp::StringConcat(parts) => {
                let mut flattened = Vec::with_capacity(parts.len());
                for part in parts {
                    match self.ops.get(&part) {
                        Some(FlatOp::StringConcat(subparts)) => {
                            flattened.extend(subparts.iter().copied());
                        }
                        _ => flattened.push(part),
                    }
                }
                let mut merged: Vec<ValueId> = Vec::with_capacity(flattened.len());
                for part in flattened {
                    let literal = match self.ops.get(&part) {
                        Some(FlatOp::StringLiteral(value)) => Some(value.clone()),
                        _ => None,
                    };
                    let Some(value) = literal else {
                        merged.push(part);
                        continue;
                    };
                    if value.as_str().is_empty() {
                        continue;
                    }
                    let previous = merged.last().and_then(|last| match self.ops.get(last) {
                        Some(FlatOp::StringLiteral(value)) => Some(value.clone()),
                        _ => None,
                    });
                    match previous {
                        // Two constants in a row become one new constant,
                        // bound just before the concat.
                        Some(previous) => {
                            let mut combined = String::with_capacity(
                                previous.as_str().len() + value.as_str().len(),
                            );
                            combined.push_str(previous.as_str());
                            combined.push_str(value.as_str());
                            let name = self.value_ids.next();
                            let op = FlatOp::StringLiteral(CheapString::new(combined));
                            self.ops.insert(name, op.clone());
                            out.push(FlatBinding {
                                name,
                                typ: Type::String,
                                op,
                            });
                            *merged.last_mut().expect("a previous part exists") = name;
                        }
                        None => merged.push(part),
                    }
                }
                match merged.len() {
                    0 => Folded::Op(FlatOp::StringLiteral(CheapString::new(String::new()))),
                    1 => Folded::Name(merged[0]),
                    _ => Folded::Op(FlatOp::StringConcat(merged)),
                }
            }

            FlatOp::HtmlConcat(parts) => {
                let mut flattened = Vec::with_capacity(parts.len());
                for part in parts {
                    match self.ops.get(&part) {
                        Some(FlatOp::HtmlConcat(subparts)) => {
                            flattened.extend(subparts.iter().copied());
                        }
                        _ => flattened.push(part),
                    }
                }
                if flattened.len() == 1 {
                    Folded::Name(flattened[0])
                } else {
                    Folded::Op(FlatOp::HtmlConcat(flattened))
                }
            }

            op => Folded::Op(op),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ir::flat_module::{FlatFunctionDeclaration, FlatModule};
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::{random_entry_args, random_module};
    use crate::ir::pure_to_flat::pure_to_flat;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entry;
    use crate::ir::runtime::value::Value;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    fn run(module: FlatModule) -> FlatModule {
        let mut value_ids = module.value_ids;
        let functions = module
            .functions
            .into_iter()
            .map(|function| FlatFunctionDeclaration {
                function: function.function,
                entry: function.entry,
                parameters: function.parameters,
                return_type: function.return_type,
                body: perform_partial_evaluation(function.body, &mut value_ids),
            })
            .collect();
        FlatModule {
            functions,
            value_ids,
            binder_ids: module.binder_ids,
        }
    }

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_partial_evaluation() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entry_args = random_entry_args(&module, &mut rng, &registry);
            let module = pure_to_flat(module);

            let before_module = module.to_string();
            let before: Vec<Result<String, EvalError>> = entry_args
                .iter()
                .map(|(function, args)| {
                    evaluate_entry(&module, function, args.clone()).map(Value::into_markup)
                })
                .collect();

            let module = run(module);

            // The pass drops no computation but a selected arm's siblings,
            // which never ran, so the output and whether the call depth
            // limit is hit both stay the same.
            for ((function, args), before) in entry_args.iter().zip(before) {
                let after = evaluate_entry(&module, function, args.clone()).map(Value::into_markup);
                match (before, after) {
                    (Ok(before), Ok(after)) => assert_eq!(
                        before, after,
                        "entry {function}\n-- before --\n{before_module}\n-- after --\n{module}"
                    ),
                    (
                        Err(EvalError::RecursionLimit { .. }),
                        Err(EvalError::RecursionLimit { .. }),
                    ) => {}
                    (before, after) => panic!(
                        "entry {function}: before {before:?}, after {after:?}\n-- before --\n{before_module}\n-- after --\n{module}"
                    ),
                }
            }
            Ok(())
        });
    }

    fn check(module: PureModule, expected: Expect) {
        let module = pure_to_flat(module);
        let before = module.to_string();
        let after = run(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn should_fold_arithmetic_with_wrapping() {
        check(
            PureModuleBuilder::new()
                .function("wrap", [], "Int", |t| t.add(t.int(i32::MAX), t.int(1)))
                .build(),
            expect![[r#"
                -- before --
                fn wrap@f0() -> Int {
                  let v0: Int = 2147483647
                  let v1: Int = 1
                  let v2: Int = v0 + v1
                  v2
                }

                -- after --
                fn wrap@f0() -> Int {
                  let v0: Int = 2147483647
                  let v1: Int = 1
                  let v2: Int = -2147483648
                  v2
                }
            "#]],
        );
    }

    #[test]
    fn should_select_the_arm_of_a_match_on_a_negated_constant() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [], "Html", |t| {
                    t.bool_match_expr(t.not(t.bool(true)), t.text("yes"), t.text("no"))
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
                  let v0: Bool = true
                  let v1: Bool = !v0
                  let v4: Html = match v1 {
                    true => {
                      let v2: Html = text("yes")
                      v2
                    }
                    false => {
                      let v3: Html = text("no")
                      v3
                    }
                  }
                  v4
                }

                -- after --
                entry fn Test@f0() -> Html {
                  let v0: Bool = true
                  let v1: Bool = false
                  let v3: Html = text("no")
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_a_match_on_a_dynamic_subject() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [("flag", "Bool")], "Html", |t| {
                    t.bool_match_expr(t.var("flag"), t.text("yes"), t.text("no"))
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0(flag@b0: Bool) -> Html {
                  let v0: Bool = b0
                  let v3: Html = match v0 {
                    true => {
                      let v1: Html = text("yes")
                      v1
                    }
                    false => {
                      let v2: Html = text("no")
                      v2
                    }
                  }
                  v3
                }

                -- after --
                entry fn Test@f0(flag@b0: Bool) -> Html {
                  let v0: Bool = b0
                  let v3: Html = match v0 {
                    true => {
                      let v1: Html = text("yes")
                      v1
                    }
                    false => {
                      let v2: Html = text("no")
                      v2
                    }
                  }
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_read_a_field_of_a_record_constructor() {
        check(
            PureModuleBuilder::new()
                .record("Point", [("x", "Int"), ("y", "Int")])
                .entry("Test", [("n", "Int")], "Html", |t| {
                    t.escape(t.int_to_string(t.field_access(
                        t.record("Point", vec![("x", t.var("n")), ("y", t.int(2))]),
                        "x",
                    )))
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0(n@b0: Int) -> Html {
                  let v0: Int = b0
                  let v1: Int = 2
                  let v2: Point = {x: v0, y: v1}
                  let v3: Int = v2.x
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entry fn Test@f0(n@b0: Int) -> Html {
                  let v0: Int = b0
                  let v1: Int = 2
                  let v2: Point = {x: v0, y: v1}
                  let v4: String = v0.to_string()
                  let v5: Html = escape(v4)
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_read_an_element_of_a_tuple_constructor() {
        check(
            PureModuleBuilder::new()
                .function("second", [], "String", |t| {
                    t.tuple_index(t.tuple(vec![t.int(1), t.str("two")]), 1)
                })
                .build(),
            expect![[r#"
                -- before --
                fn second@f0() -> String {
                  let v0: Int = 1
                  let v1: String = "two"
                  let v2: (Int, String) = (v0, v1)
                  let v3: String = v2.1
                  v3
                }

                -- after --
                fn second@f0() -> String {
                  let v0: Int = 1
                  let v1: String = "two"
                  let v2: (Int, String) = (v0, v1)
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_flatten_a_concat_and_merge_its_adjacent_constants() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [("dyn", "String")], "Html", |t| {
                    t.escape(t.string_concat(vec![
                        t.string_concat(vec![t.var("dyn"), t.str("a")]),
                        t.string_concat(vec![t.str("b"), t.var("dyn")]),
                    ]))
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0(dyn@b0: String) -> Html {
                  let v0: String = b0
                  let v1: String = "a"
                  let v2: String = concat(v0, v1)
                  let v3: String = "b"
                  let v4: String = b0
                  let v5: String = concat(v3, v4)
                  let v6: String = concat(v2, v5)
                  let v7: Html = escape(v6)
                  v7
                }

                -- after --
                entry fn Test@f0(dyn@b0: String) -> Html {
                  let v0: String = b0
                  let v1: String = "a"
                  let v2: String = concat(v0, v1)
                  let v3: String = "b"
                  let v4: String = b0
                  let v5: String = concat(v3, v4)
                  let v8: String = "ab"
                  let v6: String = concat(v0, v8, v4)
                  let v7: Html = escape(v6)
                  v7
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_empty_strings_and_read_the_one_part_left() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [("dyn", "String")], "Html", |t| {
                    t.escape(t.string_concat(vec![t.str(""), t.var("dyn"), t.str("")]))
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0(dyn@b0: String) -> Html {
                  let v0: String = ""
                  let v1: String = b0
                  let v2: String = ""
                  let v3: String = concat(v0, v1, v2)
                  let v4: Html = escape(v3)
                  v4
                }

                -- after --
                entry fn Test@f0(dyn@b0: String) -> Html {
                  let v0: String = ""
                  let v1: String = b0
                  let v2: String = ""
                  let v4: Html = escape(v1)
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_a_concat_of_constants_to_one_constant() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [], "Html", |t| {
                    t.escape(t.string_concat(vec![t.str("Hello, "), t.str("World")]))
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
                  let v0: String = "Hello, "
                  let v1: String = "World"
                  let v2: String = concat(v0, v1)
                  let v3: Html = escape(v2)
                  v3
                }

                -- after --
                entry fn Test@f0() -> Html {
                  let v0: String = "Hello, "
                  let v1: String = "World"
                  let v4: String = "Hello, World"
                  let v3: Html = escape(v4)
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_flatten_a_nested_html_concat() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [], "Html", |t| {
                    t.concat(vec![
                        t.text("a"),
                        t.concat(vec![t.text("b"), t.text("c")]),
                        t.concat(vec![t.text("d")]),
                    ])
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
                  let v0: Html = text("a")
                  let v1: Html = text("b")
                  let v2: Html = text("c")
                  let v3: Html = concat(v1, v2)
                  let v4: Html = text("d")
                  let v5: Html = concat(v4)
                  let v6: Html = concat(v0, v3, v5)
                  v6
                }

                -- after --
                entry fn Test@f0() -> Html {
                  let v0: Html = text("a")
                  let v1: Html = text("b")
                  let v2: Html = text("c")
                  let v3: Html = concat(v1, v2)
                  let v4: Html = text("d")
                  let v6: Html = concat(v0, v1, v2, v4)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_select_an_enum_arm_and_bind_its_fields_to_the_constructor_operands() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Shape",
                    [("Dot", vec![]), ("Circle", vec![("radius", "Int")])],
                )
                .function("area", [("n", "Int")], "Int", |t| {
                    t.enum_match_expr(
                        t.enum_variant_with_fields("Shape", "Circle", vec![("radius", t.var("n"))]),
                        |arms| {
                            arms.arm("Dot", |t| t.int(0));
                            arms.arm_bound("Circle", [("radius", "r")], |t| {
                                t.mul(t.var("r"), t.var("r"))
                            });
                        },
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                fn area@f0(n@b0: Int) -> Int {
                  let v0: Int = b0
                  let v1: Shape = Circle {radius: v0}
                  let v6: Int = match v1 {
                    Shape::Dot => {
                      let v2: Int = 0
                      v2
                    }
                    Shape::Circle {radius@b1: Int} => {
                      let v3: Int = b1
                      let v4: Int = b1
                      let v5: Int = v3 * v4
                      v5
                    }
                  }
                  v6
                }

                -- after --
                fn area@f0(n@b0: Int) -> Int {
                  let v0: Int = b0
                  let v1: Shape = Circle {radius: v0}
                  let v5: Int = v0 * v0
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_select_the_some_arm_and_bind_its_value() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [], "Html", |t| {
                    t.option_match_expr_with_binding(
                        t.some(t.str("x")),
                        "v",
                        |t| t.escape(t.var("v")),
                        t.text("none"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v5: Html = match v1 {
                    Some(b0: String) => {
                      let v2: String = b0
                      let v3: Html = escape(v2)
                      v3
                    }
                    None => {
                      let v4: Html = text("none")
                      v4
                    }
                  }
                  v5
                }

                -- after --
                entry fn Test@f0() -> Html {
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v3: Html = escape(v0)
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_saturate_a_float_to_int_conversion() {
        check(
            PureModuleBuilder::new()
                .function("big", [], "Int", |t| t.float_to_int(t.float(1e10)))
                .build(),
            expect![[r#"
                -- before --
                fn big@f0() -> Int {
                  let v0: Float = 10000000000
                  let v1: Int = v0.to_int()
                  v1
                }

                -- after --
                fn big@f0() -> Int {
                  let v0: Float = 10000000000
                  let v1: Int = 2147483647
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_select_an_arm_through_a_folded_equality() {
        check(
            PureModuleBuilder::new()
                .entry("Test", [], "Html", |t| {
                    t.bool_match_expr(
                        t.eq(t.str("a"), t.str("a")),
                        t.text("same"),
                        t.text("other"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                entry fn Test@f0() -> Html {
                  let v0: String = "a"
                  let v1: String = "a"
                  let v2: Bool = v0 == v1
                  let v5: Html = match v2 {
                    true => {
                      let v3: Html = text("same")
                      v3
                    }
                    false => {
                      let v4: Html = text("other")
                      v4
                    }
                  }
                  v5
                }

                -- after --
                entry fn Test@f0() -> Html {
                  let v0: String = "a"
                  let v1: String = "a"
                  let v2: Bool = true
                  let v3: Html = text("same")
                  v3
                }
            "#]],
        );
    }
}
