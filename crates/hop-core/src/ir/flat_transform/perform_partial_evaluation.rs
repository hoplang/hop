use std::collections::HashMap;

use crate::document::CheapString;
use crate::hop::typing::{Constructor, DataType, Pattern};
use crate::ir::flat_module::{
    FlatDataBinding, FlatDataBlock, FlatDataFunction, FlatDataOp, FlatFunctionDeclaration,
    FlatHtmlBinding, FlatHtmlBlock, FlatHtmlFunction, FlatHtmlOp, FlatModule,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::value_id::{ValueId, ValueIdCounter};
use crate::symbols::field_name::FieldName;

/// A pass that evaluates the constant parts of every body at compile time.
///
/// - An op whose operands are constants becomes the constant it computes,
///   with the backend semantics.
/// - A field access on a record constructor reads the constructor's
///   operand, and a tuple index on a tuple constructor likewise.
/// - A match runs an arm in place of the match when the constructors of
///   the subject show that the arm matches and no arm before it does, and
///   what reads a name its pattern binds reads the constructor operand the
///   name binds.
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
pub fn perform_partial_evaluation(module: FlatModule) -> FlatModule {
    let FlatModule {
        functions,
        mut value_ids,
    } = module;
    let functions = functions
        .into_iter()
        .map(|function| {
            let mut evaluator = Evaluator {
                value_ids: &mut value_ids,
                values: HashMap::new(),
                concats: HashMap::new(),
                renames: HashMap::new(),
            };
            match function {
                FlatFunctionDeclaration::Data(function) => {
                    FlatFunctionDeclaration::Data(FlatDataFunction {
                        body: evaluator.data_block(function.body),
                        ..function
                    })
                }
                FlatFunctionDeclaration::Html(function) => {
                    FlatFunctionDeclaration::Html(FlatHtmlFunction {
                        body: evaluator.html_block(function.body),
                        ..function
                    })
                }
            }
        })
        .collect();
    FlatModule {
        functions,
        value_ids,
    }
}

/// What a binding of a value folded to.
enum Folded {
    /// An op, bound to the name as before.
    Op(IrDataOp),
    /// Another name, which the readers of the binding read instead.
    Name(ValueId),
}

struct Evaluator<'a> {
    value_ids: &'a mut ValueIdCounter,
    /// The ops of data of the bindings kept so far, for their readers to
    /// look at. Names are unique, so one map serves every nested block.
    values: HashMap<ValueId, IrDataOp>,
    /// The parts of the Html concats kept so far, for the concats that
    /// read them to take as their own.
    concats: HashMap<ValueId, Vec<ValueId>>,
    /// The names that turned out to be another name: the bindings that
    /// folded to one, and the names the patterns of selected arms bind, each
    /// with the constructor operand it stands for.
    renames: HashMap<ValueId, ValueId>,
}

impl Evaluator<'_> {
    fn resolve(&self, name: ValueId) -> ValueId {
        self.renames.get(&name).copied().unwrap_or(name)
    }

    fn data_block(&mut self, block: FlatDataBlock) -> FlatDataBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.data_binding(binding, &mut bindings);
        }
        FlatDataBlock {
            bindings,
            result: self.resolve(block.result),
        }
    }

    fn html_block(&mut self, block: FlatHtmlBlock) -> FlatHtmlBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.data_binding(binding, &mut bindings);
        }
        let mut html = Vec::with_capacity(block.html.len());
        for binding in block.html {
            self.html_binding(binding, &mut bindings, &mut html);
        }
        FlatHtmlBlock {
            bindings,
            html,
            result: self.resolve(block.result),
        }
    }

    /// Whether the value of `name` surely matches the pattern, surely does
    /// not, or None when the ops of the bindings kept so far do not tell.
    /// On a sure match, each binding of the pattern is pushed to `bound`
    /// with the name of the part it binds.
    fn match_known(
        &self,
        pattern: &IrPattern,
        name: ValueId,
        bound: &mut Vec<(ValueId, ValueId)>,
    ) -> Option<bool> {
        let (constructor, args, fields) = match pattern {
            Pattern::Wildcard => return Some(true),
            Pattern::Binding(binder) => {
                bound.push((binder.var, name));
                return Some(true);
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => (constructor, args, fields),
        };
        let field_parts = |operands: &[(FieldName, ValueId)]| {
            fields
                .iter()
                .map(|field| {
                    let (_, operand) = operands
                        .iter()
                        .find(|(name, _)| *name == field.name)
                        .unwrap_or_else(|| panic!("no field {}", field.name.as_str()));
                    (&field.pattern, *operand)
                })
                .collect::<Vec<_>>()
        };
        let parts = match (constructor, self.values.get(&name)?) {
            (Constructor::BoolTrue, IrDataOp::BoolLiteral(value)) => return Some(*value),
            (Constructor::BoolFalse, IrDataOp::BoolLiteral(value)) => return Some(!*value),
            (Constructor::OptionSome, IrDataOp::OptionSome(inner)) => vec![(&args[0], *inner)],
            (Constructor::OptionSome, IrDataOp::OptionNone) => return Some(false),
            (Constructor::OptionNone, IrDataOp::OptionSome(_)) => return Some(false),
            (Constructor::OptionNone, IrDataOp::OptionNone) => return Some(true),
            (
                Constructor::EnumVariant { variant_name, .. },
                IrDataOp::Enum(op_variant, operands),
            ) => {
                if variant_name != op_variant {
                    return Some(false);
                }
                field_parts(operands)
            }
            (Constructor::Record { .. }, IrDataOp::Record(operands)) => field_parts(operands),
            (Constructor::Tuple, IrDataOp::Tuple(elements)) => {
                args.iter().zip(elements.iter().copied()).collect()
            }
            _ => return None,
        };
        // A part that surely does not match settles it, whatever the
        // other parts do.
        let mut sure = Some(true);
        for (pattern, part) in parts {
            match self.match_known(pattern, part, bound) {
                Some(false) => return Some(false),
                None => sure = None,
                Some(true) => {}
            }
        }
        sure
    }

    /// The first arm the subject surely matches, with each binding of its
    /// pattern and the part it binds, unless an arm before it may match as
    /// well.
    fn selected_arm<B>(&self, match_: &IrMatch<B>) -> Option<(usize, Vec<(ValueId, ValueId)>)> {
        for (index, arm) in match_.arms.iter().enumerate() {
            let mut bound = Vec::new();
            match self.match_known(&arm.pattern, match_.subject, &mut bound) {
                Some(true) => return Some((index, bound)),
                Some(false) => {}
                None => return None,
            }
        }
        None
    }

    /// Take the selected arm of a match, after letting the readers of the
    /// names its pattern binds read the parts they bind.
    fn take_arm<B>(
        &mut self,
        match_: IrMatch<B>,
        index: usize,
        bound: Vec<(ValueId, ValueId)>,
    ) -> B {
        self.renames.extend(bound);
        match_
            .arms
            .into_iter()
            .nth(index)
            .expect("the selected arm is one of the arms")
            .body
    }

    fn evaluate_arms<B>(
        &mut self,
        match_: IrMatch<B>,
        evaluate_block: impl Fn(&mut Self, B) -> B,
    ) -> IrMatch<B> {
        IrMatch {
            subject: match_.subject,
            arms: match_
                .arms
                .into_iter()
                .map(|arm| IrMatchArm {
                    pattern: arm.pattern,
                    body: evaluate_block(self, arm.body),
                })
                .collect(),
        }
    }

    fn data_binding(&mut self, binding: FlatDataBinding, out: &mut Vec<FlatDataBinding>) {
        let FlatDataBinding { binder, mut op } = binding;
        let name = binder.var;
        op.for_each_operand_mut(&mut |operand| *operand = self.resolve(*operand));
        match op {
            FlatDataOp::Match(match_) => {
                // The selected arm runs in place of the match, its bindings
                // joining the block the match sits in, and the readers of
                // the match read the arm's result.
                if let Some((index, bound)) = self.selected_arm(&match_) {
                    let arm = self.take_arm(match_, index, bound);
                    for binding in arm.bindings {
                        self.data_binding(binding, out);
                    }
                    let result = self.resolve(arm.result);
                    self.renames.insert(name, result);
                    return;
                }
                let match_ = self.evaluate_arms(match_, Self::data_block);
                out.push(FlatDataBinding {
                    binder,
                    op: FlatDataOp::Match(match_),
                });
            }
            FlatDataOp::Data(op) => match self.fold(op, out) {
                Folded::Name(target) => {
                    self.renames.insert(name, target);
                }
                Folded::Op(op) => {
                    self.values.insert(name, op.clone());
                    out.push(FlatDataBinding {
                        binder,
                        op: FlatDataOp::Data(op),
                    });
                }
            },
            op @ FlatDataOp::Call(..) => out.push(FlatDataBinding { binder, op }),
        }
    }

    /// The values of a selected arm join the values of the block, so they
    /// still come before its Html.
    fn html_binding(
        &mut self,
        binding: FlatHtmlBinding,
        bindings: &mut Vec<FlatDataBinding>,
        html: &mut Vec<FlatHtmlBinding>,
    ) {
        let FlatHtmlBinding { binder, mut op } = binding;
        let name = binder.var;
        op.for_each_operand_mut(&mut |operand| *operand = self.resolve(*operand));
        let op = match op {
            FlatHtmlOp::Match(match_) => {
                if let Some((index, bound)) = self.selected_arm(&match_) {
                    let arm = self.take_arm(match_, index, bound);
                    for binding in arm.bindings {
                        self.data_binding(binding, bindings);
                    }
                    for binding in arm.html {
                        self.html_binding(binding, bindings, html);
                    }
                    let result = self.resolve(arm.result);
                    self.renames.insert(name, result);
                    return;
                }
                FlatHtmlOp::Match(self.evaluate_arms(match_, Self::html_block))
            }
            FlatHtmlOp::For(IrFor { var, array, body }) => FlatHtmlOp::For(IrFor {
                var,
                array,
                body: self.html_block(body),
            }),
            FlatHtmlOp::Concat(parts) => {
                let mut flattened = Vec::with_capacity(parts.len());
                for part in parts {
                    match self.concats.get(&part) {
                        Some(subparts) => flattened.extend(subparts.iter().copied()),
                        None => flattened.push(part),
                    }
                }
                if flattened.len() == 1 {
                    self.renames.insert(name, flattened[0]);
                    return;
                }
                self.concats.insert(name, flattened.clone());
                FlatHtmlOp::Concat(flattened)
            }
            op => op,
        };
        html.push(FlatHtmlBinding { binder, op });
    }

    /// Fold an op whose operands are resolved. Returns the constant or
    /// name it computes when the operands allow it, and the op unchanged
    /// otherwise.
    fn fold(&mut self, op: IrDataOp, out: &mut Vec<FlatDataBinding>) -> Folded {
        match op {
            IrDataOp::FieldAccess(record, field) => match self.values.get(&record) {
                Some(IrDataOp::Record(fields)) => Folded::Name(
                    fields
                        .iter()
                        .find(|(name, _)| *name == field)
                        .map(|(_, value)| *value)
                        .unwrap_or_else(|| panic!("record has no field {}", field.as_str())),
                ),
                _ => Folded::Op(IrDataOp::FieldAccess(record, field)),
            },
            IrDataOp::TupleIndex(tuple, index) => match self.values.get(&tuple) {
                Some(IrDataOp::Tuple(elements)) => {
                    let len = elements.len();
                    Folded::Name(*elements.get(index).unwrap_or_else(|| {
                        panic!("index {index} is out of range for a tuple of {len}")
                    }))
                }
                _ => Folded::Op(IrDataOp::TupleIndex(tuple, index)),
            },
            IrDataOp::StringConcat(parts) => {
                let mut flattened = Vec::with_capacity(parts.len());
                for part in parts {
                    match self.values.get(&part) {
                        Some(IrDataOp::StringConcat(subparts)) => {
                            flattened.extend(subparts.iter().copied());
                        }
                        _ => flattened.push(part),
                    }
                }
                let mut merged: Vec<ValueId> = Vec::with_capacity(flattened.len());
                for part in flattened {
                    let literal = match self.values.get(&part) {
                        Some(IrDataOp::StringLiteral(value)) => Some(value.clone()),
                        _ => None,
                    };
                    let Some(value) = literal else {
                        merged.push(part);
                        continue;
                    };
                    if value.as_str().is_empty() {
                        continue;
                    }
                    let previous = merged.last().and_then(|last| match self.values.get(last) {
                        Some(IrDataOp::StringLiteral(value)) => Some(value.clone()),
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
                            let op = IrDataOp::StringLiteral(CheapString::new(combined));
                            self.values.insert(name, op.clone());
                            out.push(FlatDataBinding {
                                binder: IrBinder {
                                    var: name,
                                    typ: DataType::String,
                                },
                                op: FlatDataOp::Data(op),
                            });
                            *merged.last_mut().expect("a previous part exists") = name;
                        }
                        None => merged.push(part),
                    }
                }
                match merged.len() {
                    0 => Folded::Op(IrDataOp::StringLiteral(CheapString::new(String::new()))),
                    1 => Folded::Name(merged[0]),
                    _ => Folded::Op(IrDataOp::StringConcat(merged)),
                }
            }
            data => {
                let folded = match &data {
                    IrDataOp::NumericAdd(left, right) => {
                        match (self.values.get(left), self.values.get(right)) {
                            (Some(IrDataOp::IntLiteral(l)), Some(IrDataOp::IntLiteral(r))) => {
                                Some(IrDataOp::IntLiteral(l.wrapping_add(*r)))
                            }
                            (Some(IrDataOp::FloatLiteral(l)), Some(IrDataOp::FloatLiteral(r))) => {
                                Some(IrDataOp::FloatLiteral(l + r))
                            }
                            _ => None,
                        }
                    }
                    IrDataOp::NumericSubtract(left, right) => {
                        match (self.values.get(left), self.values.get(right)) {
                            (Some(IrDataOp::IntLiteral(l)), Some(IrDataOp::IntLiteral(r))) => {
                                Some(IrDataOp::IntLiteral(l.wrapping_sub(*r)))
                            }
                            (Some(IrDataOp::FloatLiteral(l)), Some(IrDataOp::FloatLiteral(r))) => {
                                Some(IrDataOp::FloatLiteral(l - r))
                            }
                            _ => None,
                        }
                    }
                    IrDataOp::NumericMultiply(left, right) => {
                        match (self.values.get(left), self.values.get(right)) {
                            (Some(IrDataOp::IntLiteral(l)), Some(IrDataOp::IntLiteral(r))) => {
                                Some(IrDataOp::IntLiteral(l.wrapping_mul(*r)))
                            }
                            (Some(IrDataOp::FloatLiteral(l)), Some(IrDataOp::FloatLiteral(r))) => {
                                Some(IrDataOp::FloatLiteral(l * r))
                            }
                            _ => None,
                        }
                    }
                    IrDataOp::Equals(left, right) => {
                        match (self.values.get(left), self.values.get(right)) {
                            (Some(IrDataOp::BoolLiteral(l)), Some(IrDataOp::BoolLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l == r))
                            }
                            (
                                Some(IrDataOp::StringLiteral(l)),
                                Some(IrDataOp::StringLiteral(r)),
                            ) => Some(IrDataOp::BoolLiteral(l.as_str() == r.as_str())),
                            (Some(IrDataOp::IntLiteral(l)), Some(IrDataOp::IntLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l == r))
                            }
                            (Some(IrDataOp::FloatLiteral(l)), Some(IrDataOp::FloatLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l == r))
                            }
                            _ => None,
                        }
                    }
                    IrDataOp::LessThan(left, right) => {
                        match (self.values.get(left), self.values.get(right)) {
                            (Some(IrDataOp::IntLiteral(l)), Some(IrDataOp::IntLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l < r))
                            }
                            (Some(IrDataOp::FloatLiteral(l)), Some(IrDataOp::FloatLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l < r))
                            }
                            _ => None,
                        }
                    }
                    IrDataOp::LessThanOrEqual(left, right) => {
                        match (self.values.get(left), self.values.get(right)) {
                            (Some(IrDataOp::IntLiteral(l)), Some(IrDataOp::IntLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l <= r))
                            }
                            (Some(IrDataOp::FloatLiteral(l)), Some(IrDataOp::FloatLiteral(r))) => {
                                Some(IrDataOp::BoolLiteral(l <= r))
                            }
                            _ => None,
                        }
                    }
                    IrDataOp::NumericNegation(operand) => match self.values.get(operand) {
                        Some(IrDataOp::IntLiteral(value)) => {
                            Some(IrDataOp::IntLiteral(value.wrapping_neg()))
                        }
                        Some(IrDataOp::FloatLiteral(value)) => Some(IrDataOp::FloatLiteral(-value)),
                        _ => None,
                    },
                    IrDataOp::BoolNegation(operand) => match self.values.get(operand) {
                        Some(IrDataOp::BoolLiteral(value)) => Some(IrDataOp::BoolLiteral(!value)),
                        _ => None,
                    },
                    IrDataOp::IntToString(operand) => match self.values.get(operand) {
                        Some(IrDataOp::IntLiteral(value)) => {
                            Some(IrDataOp::StringLiteral(CheapString::new(value.to_string())))
                        }
                        _ => None,
                    },
                    IrDataOp::FloatToInt(operand) => match self.values.get(operand) {
                        Some(IrDataOp::FloatLiteral(value)) => {
                            Some(IrDataOp::IntLiteral(*value as i32))
                        }
                        _ => None,
                    },
                    IrDataOp::IntToFloat(operand) => match self.values.get(operand) {
                        Some(IrDataOp::IntLiteral(value)) => {
                            Some(IrDataOp::FloatLiteral(*value as f64))
                        }
                        _ => None,
                    },
                    IrDataOp::StringIsEmpty(operand) => match self.values.get(operand) {
                        Some(IrDataOp::StringLiteral(value)) => {
                            Some(IrDataOp::BoolLiteral(value.as_str().is_empty()))
                        }
                        _ => None,
                    },
                    IrDataOp::ArrayIsEmpty(operand) => match self.values.get(operand) {
                        Some(IrDataOp::Array(elements)) => {
                            Some(IrDataOp::BoolLiteral(elements.is_empty()))
                        }
                        _ => None,
                    },
                    IrDataOp::ArrayLength(operand) => match self.values.get(operand) {
                        Some(IrDataOp::Array(elements)) => {
                            Some(IrDataOp::IntLiteral(elements.len() as i32))
                        }
                        _ => None,
                    },
                    IrDataOp::OptionIsSome(operand) => match self.values.get(operand) {
                        Some(IrDataOp::OptionSome(_)) => Some(IrDataOp::BoolLiteral(true)),
                        Some(IrDataOp::OptionNone) => Some(IrDataOp::BoolLiteral(false)),
                        _ => None,
                    },
                    IrDataOp::OptionIsNone(operand) => match self.values.get(operand) {
                        Some(IrDataOp::OptionSome(_)) => Some(IrDataOp::BoolLiteral(false)),
                        Some(IrDataOp::OptionNone) => Some(IrDataOp::BoolLiteral(true)),
                        _ => None,
                    },
                    _ => None,
                };
                Folded::Op(folded.unwrap_or(data))
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::document::Document;
    use crate::hop::typing::{Ty, TypeRegistryBuilder};
    use crate::ir::flat_module::FlatModule;
    use crate::ir::flat_module_builder::{FlatModuleBuilder, Pat};
    use crate::ir::flat_module_generator::{random_entrypoint_args, random_module};
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_flat};
    use crate::program::Program;
    use crate::root_contained_file_path::RootContainedFilePath;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_modules_evaluate_identically_after_partial_evaluation() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            let entrypoint_args = random_entrypoint_args(&module, &mut rng, &registry);

            let before_module = module.clone();
            let before: Vec<Result<String, EvalError>> = entrypoint_args
                .iter()
                .map(|(function, args)| {
                    evaluate_entrypoint(&module, function, args.clone()).map(Value::into_markup)
                })
                .collect();

            let module = perform_partial_evaluation(module);

            // The pass drops no computation but a selected arm's siblings,
            // which never ran, so the output and whether the call depth
            // limit is hit both stay the same.
            for ((function, args), before) in entrypoint_args.iter().zip(before) {
                let after =
                    evaluate_entrypoint(&module, function, args.clone()).map(Value::into_markup);
                match (before, after) {
                    (Ok(before), Ok(after)) => assert_eq!(
                        before, after,
                        "entrypoint {function}\n-- before --\n{before_module}\n-- after --\n{module}"
                    ),
                    (
                        Err(EvalError::CallDepthLimit { .. }),
                        Err(EvalError::CallDepthLimit { .. }),
                    )
                    | (
                        Err(EvalError::HtmlSizeLimit { .. }),
                        Err(EvalError::HtmlSizeLimit { .. }),
                    ) => {}
                    // The pass evaluates ops ahead of time, so the module may
                    // stay under the op count limit after it.
                    (Err(EvalError::OpCountLimit { .. }), _) => {}
                    (before, after) => panic!(
                        "entrypoint {function}: before {before:?}, after {after:?}\n-- before --\n{before_module}\n-- after --\n{module}"
                    ),
                }
            }
            Ok(())
        });
    }

    fn check(module: FlatModule, expected: Expect) {
        let before = module.to_string();
        let after = perform_partial_evaluation(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    /// Like `check`, on the Flat IR the source compiles to.
    fn check_source(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut program = Program::new();
        program.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = program.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (flat, _) = orchestrate_flat(program.typed_modules(), OrchestrateOptions::default());
        check(flat, expected);
    }

    #[test]
    fn should_select_an_arm_a_partly_known_subject_surely_matches() {
        check_source(
            indoc! {r#"
                page Test(nickname: Option[String]) {
                  fn body() -> Html {
                    match (true, nickname) {
                      (false, _) => <p>guest</p>,
                      (true, name) => <p>{name.unwrap_or("member")}</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entrypoint fn body@f0(nickname@v0: Option[String]) -> Html {
                  let v1: Bool = true
                  let v2: (Bool, Option[String]) = (v1, v0)
                  let v13: Html = match v2 {
                    (false, _) => {
                      let v3: Html = text("guest")
                      let v4: Html = concat(v3)
                      let v5: Html = element("p", {}, v4)
                      v5
                    }
                    (true, v6: Option[String]) => {
                      let v9: String = match v6 {
                        Some(v7: String) => {
                          v7
                        }
                        None => {
                          let v8: String = "member"
                          v8
                        }
                      }
                      let v10: Html = escape(v9)
                      let v11: Html = concat(v10)
                      let v12: Html = element("p", {}, v11)
                      v12
                    }
                  }
                  v13
                }

                -- after --
                entrypoint fn body@f0(nickname@v0: Option[String]) -> Html {
                  let v1: Bool = true
                  let v2: (Bool, Option[String]) = (v1, v0)
                  let v9: String = match v0 {
                    Some(v7: String) => {
                      v7
                    }
                    None => {
                      let v8: String = "member"
                      v8
                    }
                  }
                  let v10: Html = escape(v9)
                  let v12: Html = element("p", {}, v10)
                  v12
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_a_match_when_an_arm_before_the_one_that_matches_may_match() {
        check_source(
            indoc! {r#"
                page Test(nickname: Option[String]) {
                  fn body() -> Html {
                    match (true, nickname) {
                      (true, Some(name)) => <p>{name}</p>,
                      (_, _) => <p>member</p>,
                    }
                  }
                }
            "#},
            expect![[r#"
                -- before --
                entrypoint fn body@f0(nickname@v0: Option[String]) -> Html {
                  let v1: Bool = true
                  let v2: (Bool, Option[String]) = (v1, v0)
                  let v10: Html = match v2 {
                    (true, Some(v3: String)) => {
                      let v4: Html = escape(v3)
                      let v5: Html = concat(v4)
                      let v6: Html = element("p", {}, v5)
                      v6
                    }
                    (_, _) => {
                      let v7: Html = text("member")
                      let v8: Html = concat(v7)
                      let v9: Html = element("p", {}, v8)
                      v9
                    }
                  }
                  v10
                }

                -- after --
                entrypoint fn body@f0(nickname@v0: Option[String]) -> Html {
                  let v1: Bool = true
                  let v2: (Bool, Option[String]) = (v1, v0)
                  let v10: Html = match v2 {
                    (true, Some(v3: String)) => {
                      let v4: Html = escape(v3)
                      let v6: Html = element("p", {}, v4)
                      v6
                    }
                    (_, _) => {
                      let v7: Html = text("member")
                      let v9: Html = element("p", {}, v7)
                      v9
                    }
                  }
                  v10
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_arithmetic_with_wrapping() {
        check(
            FlatModuleBuilder::new()
                .function("wrap", [], Ty::Int, |t| t.add(t.int(i32::MAX), t.int(1)))
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
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.match_(t.not(t.bool(true)), |m| {
                        m.arm(Pat::True, |t, _| t.text("yes"));
                        m.arm(Pat::False, |t, _| t.text("no"));
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
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
                entrypoint fn Test@f0() -> Html {
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
            FlatModuleBuilder::new()
                .entrypoint("Test", [("flag", Ty::Bool)], Ty::Html, |t| {
                    t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.text("yes"));
                        m.arm(Pat::False, |t, _| t.text("no"));
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(flag@v0: Bool) -> Html {
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
                entrypoint fn Test@f0(flag@v0: Bool) -> Html {
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
        let types = TypeRegistryBuilder::new()
            .record("Point", [("x", Ty::Int), ("y", Ty::Int)])
            .build();
        check(
            FlatModuleBuilder::with_registry(types)
                .entrypoint("Test", [("n", Ty::Int)], Ty::Html, |t| {
                    t.escape(t.int_to_string(t.field_access(
                        t.record(Ty::named("Point"), [("x", t.param("n")), ("y", t.int(2))]),
                        "x",
                    )))
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(n@v0: Int) -> Html {
                  let v1: Int = 2
                  let v2: Point = {x: v0, y: v1}
                  let v3: Int = v2.x
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint fn Test@f0(n@v0: Int) -> Html {
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
            FlatModuleBuilder::new()
                .function("second", [], Ty::String, |t| {
                    t.tuple_index(t.tuple([t.int(1), t.str("two")]), 1)
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
            FlatModuleBuilder::new()
                .entrypoint("Test", [("dyn", Ty::String)], Ty::Html, |t| {
                    t.escape(t.string_concat([
                        t.string_concat(vec![t.param("dyn"), t.str("a")]),
                        t.string_concat(vec![t.str("b"), t.param("dyn")]),
                    ]))
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(dyn@v0: String) -> Html {
                  let v1: String = "a"
                  let v2: String = concat(v0, v1)
                  let v3: String = "b"
                  let v4: String = concat(v3, v0)
                  let v5: String = concat(v2, v4)
                  let v6: Html = escape(v5)
                  v6
                }

                -- after --
                entrypoint fn Test@f0(dyn@v0: String) -> Html {
                  let v1: String = "a"
                  let v2: String = concat(v0, v1)
                  let v3: String = "b"
                  let v4: String = concat(v3, v0)
                  let v7: String = "ab"
                  let v5: String = concat(v0, v7, v0)
                  let v6: Html = escape(v5)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_empty_strings_and_read_the_one_part_left() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Test", [("dyn", Ty::String)], Ty::Html, |t| {
                    t.escape(t.string_concat([t.str(""), t.param("dyn"), t.str("")]))
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(dyn@v0: String) -> Html {
                  let v1: String = ""
                  let v2: String = ""
                  let v3: String = concat(v1, v0, v2)
                  let v4: Html = escape(v3)
                  v4
                }

                -- after --
                entrypoint fn Test@f0(dyn@v0: String) -> Html {
                  let v1: String = ""
                  let v2: String = ""
                  let v4: Html = escape(v0)
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_a_concat_of_constants_to_one_constant() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.escape(t.string_concat([t.str("Hello, "), t.str("World")]))
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: String = "Hello, "
                  let v1: String = "World"
                  let v2: String = concat(v0, v1)
                  let v3: Html = escape(v2)
                  v3
                }

                -- after --
                entrypoint fn Test@f0() -> Html {
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
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.html_concat([
                        t.text("a"),
                        t.html_concat(vec![t.text("b"), t.text("c")]),
                        t.html_concat(vec![t.text("d")]),
                    ])
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
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
                entrypoint fn Test@f0() -> Html {
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
        let types = TypeRegistryBuilder::new()
            .enum_(
                "Shape",
                [("Dot", vec![]), ("Circle", vec![("radius", Ty::Int)])],
            )
            .build();
        check(
            FlatModuleBuilder::with_registry(types)
                .function("area", [("n", Ty::Int)], Ty::Int, |t| {
                    t.match_(
                        t.enum_variant_with_fields(
                            Ty::named("Shape"),
                            "Circle",
                            [("radius", t.param("n"))],
                        ),
                        |m| {
                            m.arm(Pat::variant("Dot", []), |t, _| t.int(0));
                            m.arm(
                                Pat::variant("Circle", [("radius", Pat::bind("r"))]),
                                |t, arm| t.mul(arm.binding("r"), arm.binding("r")),
                            );
                        },
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                fn area@f0(n@v0: Int) -> Int {
                  let v1: Shape = Circle {radius: v0}
                  let v5: Int = match v1 {
                    Shape::Dot => {
                      let v2: Int = 0
                      v2
                    }
                    Shape::Circle {radius@v3: Int} => {
                      let v4: Int = v3 * v3
                      v4
                    }
                  }
                  v5
                }

                -- after --
                fn area@f0(n@v0: Int) -> Int {
                  let v1: Shape = Circle {radius: v0}
                  let v4: Int = v0 * v0
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_select_the_some_arm_and_bind_its_value() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.match_(t.some(t.str("x")), |m| {
                        m.arm(Pat::some(Pat::bind("v")), |t, arm| {
                            t.escape(arm.binding("v"))
                        });
                        m.arm(Pat::None, |t, _| t.text("none"));
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v5: Html = match v1 {
                    Some(v2: String) => {
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
                entrypoint fn Test@f0() -> Html {
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
            FlatModuleBuilder::new()
                .function("big", [], Ty::Int, |t| t.float_to_int(t.float(1e10)))
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
            FlatModuleBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.match_(t.eq(t.str("a"), t.str("a")), |m| {
                        m.arm(Pat::True, |t, _| t.text("same"));
                        m.arm(Pat::False, |t, _| t.text("other"));
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
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
                entrypoint fn Test@f0() -> Html {
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
