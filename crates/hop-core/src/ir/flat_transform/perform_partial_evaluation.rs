use std::collections::HashMap;

use crate::document::CheapString;
use crate::hop::typing::{Constructor, Pattern};
use crate::html::write_escaped_html;
use crate::ir::flat_program::{
    FlatEntrypoint, FlatFor, FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlLambda,
    FlatHtmlOp, FlatProgram, FlatPureBlock, FlatPureLambda, FlatPureOp,
};
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function_declaration::IrFunctionDeclaration;
use crate::ir::ir_html_element::IrAttribute;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm, IrPattern};
use crate::ir::ir_type::{IrDataType, IrFunctionDataType, IrFunctionHtmlType, IrHtmlType};
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
///   empty constants and merges adjacent constants. An html concat or an
///   effect concat takes the parts of a nested concat as its own, so an
///   empty part goes. A concat of one part is that part.
/// - An html concat is bound just before each reader that is not a concat,
///   not where it stands. A path reads Html at most once, so a path that
///   read the concat reads each of its parts once.
/// - An escape of a constant String without a carriage return is text. An
///   attribute whose value is a constant String or whose presence is a
///   constant true is a constant attribute, and one whose presence is a
///   constant false goes.
/// - An application of a lambda bound earlier in the body runs a copy of
///   the lambda's body in place of the application, with fresh names, and
///   what reads a parameter reads the argument. Only a lambda whose
///   evaluated body fits `APPLY_BUDGET` is copied, and only while the body
///   being evaluated has `APPLY_FUEL` left, which bounds how much copying
///   can grow a body.
///
/// A cell is never constant, as a click may have set it. The bindings of
/// an arm or of a copy keep their names, which are unique, and the arm
/// runs in every invocation and the copy at every application, so a cell
/// or an element among them is identified from one invocation to the next
/// as before.
///
/// A binding that turns out to be another name is dropped, and what read
/// it reads that name. The bindings nothing reads any more are left for
/// `eliminate_dead_bindings`.
///
/// One pass in binding order sees every operand's op before the op that
/// reads it, so nothing is left for a second pass.
pub fn perform_partial_evaluation(program: FlatProgram) -> FlatProgram {
    let FlatProgram {
        types,
        functions,
        entrypoints,
        mut value_ids,
    } = program;
    let functions = functions
        .into_iter()
        .map(|function| {
            let mut evaluator = Evaluator::new(&mut value_ids);
            match function {
                FlatFunction::Pure(function) => FlatFunction::Pure(IrFunctionDeclaration {
                    body: evaluator.pure_block(function.body),
                    ..function
                }),
                FlatFunction::Html(function) => FlatFunction::Html(IrFunctionDeclaration {
                    body: evaluator.html_block(function.body),
                    ..function
                }),
            }
        })
        .collect();
    let entrypoints = entrypoints
        .into_iter()
        .map(|entrypoint| {
            let mut evaluator = Evaluator::new(&mut value_ids);
            FlatEntrypoint {
                body: evaluator.html_block(entrypoint.body),
                ..entrypoint
            }
        })
        .collect();
    FlatProgram {
        types,
        functions,
        entrypoints,
        value_ids,
    }
}

/// The most bindings, counting the blocks nested in them, that the evaluated
/// body of a lambda may have for an application to run a copy of it.
const APPLY_BUDGET: usize = 100;

/// The most bindings that the copies of lambda bodies may add to one body of a
/// function or entrypoint.
const APPLY_FUEL: usize = 10_000;

fn html_fits(block: &FlatHtmlBlock, budget: &mut usize) -> bool {
    block.bindings.iter().all(|binding| {
        let Some(rest) = budget.checked_sub(1) else {
            return false;
        };
        *budget = rest;
        match binding {
            FlatHtmlBinding::Pure(binding) => pure_op_fits(&binding.op, budget),
            FlatHtmlBinding::Cell(..) | FlatHtmlBinding::Track(..) => true,
            FlatHtmlBinding::Html(binding) => match &binding.op {
                FlatHtmlOp::Match(match_) => {
                    (match_.arms.iter()).all(|arm| html_fits(&arm.body, budget))
                }
                FlatHtmlOp::For(for_) => {
                    (for_.key.iter()).all(|key| pure_fits(key, budget))
                        && html_fits(&for_.body, budget)
                }
                FlatHtmlOp::Lambda(lambda) => html_fits(&lambda.body, budget),
                _ => true,
            },
        }
    })
}

fn pure_fits(block: &FlatPureBlock, budget: &mut usize) -> bool {
    block.bindings.iter().all(|binding| {
        let Some(rest) = budget.checked_sub(1) else {
            return false;
        };
        *budget = rest;
        pure_op_fits(&binding.op, budget)
    })
}

fn pure_op_fits(op: &FlatPureOp, budget: &mut usize) -> bool {
    match op {
        FlatPureOp::Match(match_) => (match_.arms.iter()).all(|arm| pure_fits(&arm.body, budget)),
        FlatPureOp::Lambda(lambda) => pure_fits(&lambda.body, budget),
        _ => true,
    }
}

fn arguments<P, B>(lambda: &IrLambda<P, B>, args: &[ValueId]) -> HashMap<ValueId, ValueId> {
    (lambda.parameters.iter())
        .map(|param| param.var)
        .zip(args.iter().copied())
        .collect()
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
    /// The Html concats seen so far, which are bound only where a reader
    /// other than a concat reads them.
    html_concats: HashMap<ValueId, HtmlConcat>,
    /// The names that turned out to be another name: the bindings that
    /// folded to one, and the names the patterns of selected arms bind, each
    /// with the constructor operand it stands for.
    renames: HashMap<ValueId, ValueId>,
    /// The lambdas kept so far, evaluated, whose body fits `APPLY_BUDGET`,
    /// with the bindings the body has, for the applications that read them
    /// to copy.
    pure_lambdas: HashMap<ValueId, (usize, FlatPureLambda)>,
    html_lambdas: HashMap<ValueId, (usize, FlatHtmlLambda)>,
    /// The bindings the copies of lambda bodies may still add to the body
    /// being evaluated. Once it runs out, applications are kept.
    fuel: usize,
}

struct HtmlConcat {
    parts: Vec<ValueId>,
    bound: bool,
}

impl<'a> Evaluator<'a> {
    fn new(value_ids: &'a mut ValueIdCounter) -> Self {
        Evaluator {
            value_ids,
            values: HashMap::new(),
            html_concats: HashMap::new(),
            renames: HashMap::new(),
            pure_lambdas: HashMap::new(),
            html_lambdas: HashMap::new(),
            fuel: APPLY_FUEL,
        }
    }

    fn resolve(&self, name: ValueId) -> ValueId {
        self.renames.get(&name).copied().unwrap_or(name)
    }

    fn bind_concat(&mut self, name: ValueId, out: &mut Vec<FlatHtmlBinding>) -> ValueId {
        let Some(concat) = self.html_concats.get_mut(&name) else {
            return name;
        };
        // Nothing binds the name of the concat where it stands, so the first
        // reader binds it, and each later reader, in another arm, binds a
        // fresh one.
        let var = if std::mem::replace(&mut concat.bound, true) {
            self.value_ids.next()
        } else {
            name
        };
        out.push(FlatHtmlBinding::Html(IrBinding {
            binder: IrBinder {
                var,
                typ: IrFunctionHtmlType::Html(IrHtmlType),
            },
            op: FlatHtmlOp::Concat(concat.parts.clone()),
        }));
        var
    }

    fn pure_block(&mut self, block: FlatPureBlock) -> FlatPureBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        for binding in block.bindings {
            self.pure_binding(binding, &mut bindings);
        }
        IrBlock {
            bindings,
            result: self.resolve(block.result),
        }
    }

    fn html_block(&mut self, block: FlatHtmlBlock) -> FlatHtmlBlock {
        let mut bindings = Vec::with_capacity(block.bindings.len());
        self.html_bindings(block.bindings, &mut bindings);
        let result = self.resolve(block.result);
        let result = self.bind_concat(result, &mut bindings);
        IrBlock { bindings, result }
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

    fn pure_binding(
        &mut self,
        binding: IrBinding<IrFunctionDataType, FlatPureOp>,
        out: &mut Vec<IrBinding<IrFunctionDataType, FlatPureOp>>,
    ) {
        let IrBinding { binder, mut op } = binding;
        let name = binder.var;
        op.for_each_operand_mut(&mut |operand| *operand = self.resolve(*operand));
        match op {
            FlatPureOp::Match(match_) => {
                // The selected arm runs in place of the match, its bindings
                // joining the block the match sits in, and the readers of
                // the match read the arm's result.
                if let Some((index, bound)) = self.selected_arm(&match_) {
                    let arm = self.take_arm(match_, index, bound);
                    for binding in arm.bindings {
                        self.pure_binding(binding, out);
                    }
                    let result = self.resolve(arm.result);
                    self.renames.insert(name, result);
                    return;
                }
                let match_ = self.evaluate_arms(match_, Self::pure_block);
                out.push(IrBinding {
                    binder,
                    op: FlatPureOp::Match(match_),
                });
            }
            FlatPureOp::Data(op) => match self.fold(op, out) {
                Folded::Name(target) => {
                    self.renames.insert(name, target);
                }
                Folded::Op(op) => {
                    self.values.insert(name, op.clone());
                    out.push(IrBinding {
                        binder,
                        op: FlatPureOp::Data(op),
                    });
                }
            },
            FlatPureOp::Lambda(IrLambda { parameters, body }) => {
                let lambda = IrLambda {
                    parameters,
                    body: self.pure_block(body),
                };
                let mut budget = APPLY_BUDGET;
                if pure_fits(&lambda.body, &mut budget) {
                    self.pure_lambdas
                        .insert(name, (APPLY_BUDGET - budget, lambda.clone()));
                }
                out.push(IrBinding {
                    binder,
                    op: FlatPureOp::Lambda(lambda),
                });
            }
            FlatPureOp::Apply(closure, args) => {
                // The copy runs in place of the application, its bindings
                // joining the block the application sits in, as the
                // selected arm of a match does.
                if let Some((size, lambda)) = self.pure_lambdas.get(&closure)
                    && let Some(fuel) = self.fuel.checked_sub(*size)
                {
                    self.fuel = fuel;
                    let body = Freshener::new(&mut *self.value_ids, arguments(lambda, &args))
                        .freshen_pure_block(&lambda.body);
                    for binding in body.bindings {
                        self.pure_binding(binding, out);
                    }
                    let result = self.resolve(body.result);
                    self.renames.insert(name, result);
                    return;
                }
                out.push(IrBinding {
                    binder,
                    op: FlatPureOp::Apply(closure, args),
                });
            }
            op @ (FlatPureOp::Call(..)
            | FlatPureOp::CellWrite(..)
            | FlatPureOp::CellRead(_)
            | FlatPureOp::EffectPure(_)
            | FlatPureOp::EffectBind(..)) => {
                out.push(IrBinding { binder, op });
            }
        }
    }

    fn html_bindings(&mut self, bindings: Vec<FlatHtmlBinding>, out: &mut Vec<FlatHtmlBinding>) {
        let mut pure = Vec::new();
        for binding in bindings {
            match binding {
                FlatHtmlBinding::Pure(binding) => {
                    self.pure_binding(binding, &mut pure);
                    out.extend(pure.drain(..).map(FlatHtmlBinding::Pure));
                }
                // A cell is never constant, as a click may have set it.
                FlatHtmlBinding::Cell(binder, initial) => {
                    out.push(FlatHtmlBinding::Cell(binder, self.resolve(initial)));
                }
                FlatHtmlBinding::Track(binder, cell) => {
                    out.push(FlatHtmlBinding::Track(binder, self.resolve(cell)));
                }
                FlatHtmlBinding::Html(binding) => self.html_binding(binding, out),
            }
        }
    }

    fn html_binding(
        &mut self,
        binding: IrBinding<IrFunctionHtmlType, FlatHtmlOp>,
        out: &mut Vec<FlatHtmlBinding>,
    ) {
        let IrBinding { binder, mut op } = binding;
        let name = binder.var;
        op.for_each_operand_mut(&mut |operand| *operand = self.resolve(*operand));
        let mut op = match op {
            FlatHtmlOp::Match(match_) => {
                if let Some((index, bound)) = self.selected_arm(&match_) {
                    let arm = self.take_arm(match_, index, bound);
                    self.html_bindings(arm.bindings, out);
                    let result = self.resolve(arm.result);
                    self.renames.insert(name, result);
                    return;
                }
                FlatHtmlOp::Match(self.evaluate_arms(match_, Self::html_block))
            }
            FlatHtmlOp::For(FlatFor {
                var,
                array,
                key,
                body,
            }) => FlatHtmlOp::For(FlatFor {
                var,
                array,
                key: key.map(|key| self.pure_block(key)),
                body: self.html_block(body),
            }),
            FlatHtmlOp::Lambda(IrLambda { parameters, body }) => {
                let lambda = IrLambda {
                    parameters,
                    body: self.html_block(body),
                };
                let mut budget = APPLY_BUDGET;
                if html_fits(&lambda.body, &mut budget) {
                    self.html_lambdas
                        .insert(name, (APPLY_BUDGET - budget, lambda.clone()));
                }
                FlatHtmlOp::Lambda(lambda)
            }
            FlatHtmlOp::Apply(closure, args) => {
                if let Some((size, lambda)) = self.html_lambdas.get(&closure)
                    && let Some(fuel) = self.fuel.checked_sub(*size)
                {
                    self.fuel = fuel;
                    let body = Freshener::new(&mut *self.value_ids, arguments(lambda, &args))
                        .freshen_html_block(&lambda.body);
                    self.html_bindings(body.bindings, out);
                    let result = self.resolve(body.result);
                    self.renames.insert(name, result);
                    return;
                }
                FlatHtmlOp::Apply(closure, args)
            }
            FlatHtmlOp::Concat(parts) => {
                let mut flattened = Vec::with_capacity(parts.len());
                for part in parts {
                    match self.html_concats.get(&part) {
                        Some(concat) => flattened.extend(concat.parts.iter().copied()),
                        None => flattened.push(part),
                    }
                }
                if flattened.len() == 1 {
                    self.renames.insert(name, flattened[0]);
                } else {
                    let concat = HtmlConcat {
                        parts: flattened,
                        bound: false,
                    };
                    self.html_concats.insert(name, concat);
                }
                return;
            }
            FlatHtmlOp::Escape(string) => match self.values.get(&string) {
                // TODO: Fold a literal with a carriage return too, once the
                // escapes of the evaluator and of the TypeScript and Rust
                // backends write it as &#13;.
                Some(IrDataOp::StringLiteral(value)) if !value.as_str().contains('\r') => {
                    let mut markup = String::new();
                    write_escaped_html(value.as_str(), &mut markup);
                    FlatHtmlOp::Text(CheapString::new(markup))
                }
                _ => FlatHtmlOp::Escape(string),
            },
            FlatHtmlOp::Element(mut element) => {
                let values = &self.values;
                element.attributes.retain(|_, attribute| {
                    !matches!(attribute, IrAttribute::Presence(present)
                        if matches!(values.get(present), Some(IrDataOp::BoolLiteral(false))))
                });
                for attribute in element.attributes.values_mut() {
                    let constant = match attribute {
                        IrAttribute::Value(value) => match values.get(value) {
                            Some(IrDataOp::StringLiteral(value)) => Some(Some(value.clone())),
                            _ => None,
                        },
                        IrAttribute::Presence(present) => match values.get(present) {
                            Some(IrDataOp::BoolLiteral(true)) => Some(None),
                            _ => None,
                        },
                        IrAttribute::Constant(_) => None,
                    };
                    if let Some(constant) = constant {
                        *attribute = IrAttribute::Constant(constant);
                    }
                }
                FlatHtmlOp::Element(element)
            }
            op => op,
        };
        op.for_each_operand_mut(&mut |operand| *operand = self.bind_concat(*operand, out));
        out.push(FlatHtmlBinding::Html(IrBinding { binder, op }));
    }

    /// Fold an op whose operands are resolved. Returns the constant or
    /// name it computes when the operands allow it, and the op unchanged
    /// otherwise.
    fn fold(
        &mut self,
        op: IrDataOp,
        out: &mut Vec<IrBinding<IrFunctionDataType, FlatPureOp>>,
    ) -> Folded {
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
                            out.push(IrBinding {
                                binder: IrBinder {
                                    var: name,
                                    typ: IrFunctionDataType::Data(IrDataType::String),
                                },
                                op: FlatPureOp::Data(op),
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
                    IrDataOp::NumericAdd(_, left, right) => {
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
                    IrDataOp::NumericSubtract(_, left, right) => {
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
                    IrDataOp::NumericMultiply(_, left, right) => {
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
                    IrDataOp::Equals(_, left, right) => {
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
                    IrDataOp::LessThan(_, left, right) => {
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
                    IrDataOp::LessThanOrEqual(_, left, right) => {
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
                    IrDataOp::NumericNegation(_, operand) => match self.values.get(operand) {
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
                    IrDataOp::ArrayGet(array, index) => {
                        match (self.values.get(array), self.values.get(index)) {
                            (
                                Some(IrDataOp::Array(elements)),
                                Some(IrDataOp::IntLiteral(index)),
                            ) => Some(
                                match usize::try_from(*index)
                                    .ok()
                                    .and_then(|index| elements.get(index))
                                {
                                    Some(element) => IrDataOp::OptionSome(*element),
                                    None => IrDataOp::OptionNone,
                                },
                            ),
                            _ => None,
                        }
                    }
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
    use crate::hop::typing::{EquatableType, NumericType, Ty};
    use crate::ir::flat_program::FlatProgram;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::flat_typecheck;
    use crate::ir::ir_type::IrType;
    use crate::ir::ir_type_declarations_builder::IrTypeDeclarationsBuilder;
    use crate::ir::runtime::EvalError;
    use crate::ir::runtime::flat_evaluator::evaluate_entrypoint;
    use crate::ir::runtime::value::Value;
    use crate::root_contained_file_path::RootContainedFilePath;
    use crate::session::Session;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    #[test]
    fn fuzz_random_programs_evaluate_identically_after_partial_evaluation() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            let entrypoint_args = random_entrypoint_args(&program, u)?;

            let before_program = program.clone();
            let before: Vec<Result<String, EvalError>> = entrypoint_args
                .iter()
                .map(|(entrypoint, args)| {
                    evaluate_entrypoint(&program, *entrypoint, args.clone()).map(Value::into_markup)
                })
                .collect();

            let program = perform_partial_evaluation(program);
            flat_typecheck::typecheck(&program).unwrap();

            // The pass drops no computation but a selected arm's siblings,
            // which never ran, so the output stays the same.
            for ((entrypoint, args), before) in entrypoint_args.iter().zip(before) {
                let after = evaluate_entrypoint(&program, *entrypoint, args.clone())
                    .map(Value::into_markup);
                match (before, after) {
                    (Ok(before), Ok(after)) => assert_eq!(
                        before, after,
                        "entrypoint {entrypoint}\n-- before --\n{before_program}\n-- after --\n{program}"
                    ),
                    (
                        Err(EvalError::HtmlSizeLimit { .. }),
                        Err(EvalError::HtmlSizeLimit { .. }),
                    )
                    | (
                        Err(EvalError::StringLengthLimit { .. }),
                        Err(EvalError::StringLengthLimit { .. }),
                    ) => {}
                    // The pass evaluates ops ahead of time, so the program may
                    // stay under the op count limit after it.
                    (Err(EvalError::OpCountLimit { .. }), _) => {}
                    // The pass runs the body of a lambda in place of its
                    // application, which opens no frame, so the program may
                    // stay under the call depth limit after it.
                    (Err(EvalError::CallDepthLimit { .. }), _) => {}
                    (before, after) => panic!(
                        "entrypoint {entrypoint}: before {before:?}, after {after:?}\n-- before --\n{before_program}\n-- after --\n{program}"
                    ),
                }
            }
            Ok(())
        });
    }

    fn check(program: FlatProgram, expected: Expect) {
        let before = program.to_string();
        let after = perform_partial_evaluation(program).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    /// Like `check`, on the Flat IR the source compiles to.
    fn check_source(source: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("main.hop").unwrap();
        let mut session = Session::new();
        session.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );
        let diagnostics = session.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let (flat, _) = session.compile_flat(None, None);
        check(flat, expected);
    }

    #[test]
    fn should_run_a_copy_of_a_lambda_in_place_of_each_application() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("n", Ty::Int)], |t| {
                    let ten = t.int(10);
                    let add = t.lambda([IrType::INT], |t, params| {
                        t.add(NumericType::Int, params[0], ten)
                    });
                    let known = t.apply(add, [t.int(1)]);
                    let unknown = t.apply(add, [t.param("n")]);
                    t.html_concat([
                        t.escape(t.int_to_string(known)),
                        t.escape(t.int_to_string(unknown)),
                    ])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(n@v0: Int) -> Html {
                  let v1: Int = 10
                  let v4: (Int) -> Int = fn(v2: Int) {
                    let v3: Int = add[Int](v2, v1)
                    v3
                  }
                  let v5: Int = 1
                  let v6: Int = v4(v5)
                  let v7: Int = v4(v0)
                  let v8: String = v6.to_string()
                  let v9: Html = escape(v8)
                  let v10: String = v7.to_string()
                  let v11: Html = escape(v10)
                  let v12: Html = concat(v9, v11)
                  v12
                }

                -- after --
                entrypoint Test@e0(n@v0: Int) -> Html {
                  let v1: Int = 10
                  let v4: (Int) -> Int = fn(v2: Int) {
                    let v3: Int = add[Int](v2, v1)
                    v3
                  }
                  let v5: Int = 1
                  let v13: Int = 11
                  let v14: Int = add[Int](v0, v1)
                  let v8: String = "11"
                  let v9: Html = text("11")
                  let v10: String = v14.to_string()
                  let v11: Html = escape(v10)
                  let v12: Html = concat(v9, v11)
                  v12
                }
            "#]],
        );
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
                entrypoint body@e0(nickname@v0: Option[String]) -> Html {
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
                entrypoint body@e0(nickname@v0: Option[String]) -> Html {
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
                entrypoint body@e0(nickname@v0: Option[String]) -> Html {
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
                entrypoint body@e0(nickname@v0: Option[String]) -> Html {
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
            FlatProgramBuilder::new()
                .function("wrap", [], Ty::Int, |t| {
                    t.add(NumericType::Int, t.int(i32::MAX), t.int(1))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn wrap@f0() -> Int {
                  let v0: Int = 2147483647
                  let v1: Int = 1
                  let v2: Int = add[Int](v0, v1)
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
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.match_(t.not(t.bool(true)), |m| {
                        m.arm(Pat::True, |t, _| t.text("yes"));
                        m.arm(Pat::False, |t, _| t.text("no"));
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Bool = true
                  let v1: Bool = not(v0)
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
                entrypoint Test@e0() -> Html {
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
            FlatProgramBuilder::new()
                .entrypoint("Test", [("flag", Ty::Bool)], |t| {
                    t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.text("yes"));
                        m.arm(Pat::False, |t, _| t.text("no"));
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(flag@v0: Bool) -> Html {
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
                entrypoint Test@e0(flag@v0: Bool) -> Html {
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
        let types = IrTypeDeclarationsBuilder::new()
            .record("Point", [("x", Ty::Int), ("y", Ty::Int)])
            .build();
        check(
            FlatProgramBuilder::with_types(types)
                .entrypoint("Test", [("n", Ty::Int)], |t| {
                    t.escape(t.int_to_string(t.field_access(
                        t.record(
                            t.ty(Ty::named("Point")),
                            [("x", t.param("n")), ("y", t.int(2))],
                        ),
                        "x",
                    )))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(n@v0: Int) -> Html {
                  let v1: Int = 2
                  let v2: Point = {x: v0, y: v1}
                  let v3: Int = v2.x
                  let v4: String = v3.to_string()
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                entrypoint Test@e0(n@v0: Int) -> Html {
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
    fn should_select_the_tuple_arm_and_bind_its_components() {
        check(
            FlatProgramBuilder::new()
                .function("second", [], Ty::String, |t| {
                    t.match_(t.tuple([t.int(1), t.str("two")]), |m| {
                        m.arm(
                            Pat::Tuple(vec![Pat::Wildcard, Pat::bind("second")]),
                            |_, arm| arm.binding("second"),
                        );
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                fn second@f0() -> String {
                  let v0: Int = 1
                  let v1: String = "two"
                  let v2: (Int, String) = (v0, v1)
                  let v4: String = match v2 {
                    (_, v3: String) => {
                      v3
                    }
                  }
                  v4
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
            FlatProgramBuilder::new()
                .entrypoint("Test", [("dyn", Ty::String)], |t| {
                    t.escape(t.string_concat([
                        t.string_concat(vec![t.param("dyn"), t.str("a")]),
                        t.string_concat(vec![t.str("b"), t.param("dyn")]),
                    ]))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(dyn@v0: String) -> Html {
                  let v1: String = "a"
                  let v2: String = string_concat(v0, v1)
                  let v3: String = "b"
                  let v4: String = string_concat(v3, v0)
                  let v5: String = string_concat(v2, v4)
                  let v6: Html = escape(v5)
                  v6
                }

                -- after --
                entrypoint Test@e0(dyn@v0: String) -> Html {
                  let v1: String = "a"
                  let v2: String = string_concat(v0, v1)
                  let v3: String = "b"
                  let v4: String = string_concat(v3, v0)
                  let v7: String = "ab"
                  let v5: String = string_concat(v0, v7, v0)
                  let v6: Html = escape(v5)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_drop_empty_strings_and_read_the_one_part_left() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("dyn", Ty::String)], |t| {
                    t.escape(t.string_concat([t.str(""), t.param("dyn"), t.str("")]))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(dyn@v0: String) -> Html {
                  let v1: String = ""
                  let v2: String = ""
                  let v3: String = string_concat(v1, v0, v2)
                  let v4: Html = escape(v3)
                  v4
                }

                -- after --
                entrypoint Test@e0(dyn@v0: String) -> Html {
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
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.escape(t.string_concat([t.str("Hello, "), t.str("World")]))
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "Hello, "
                  let v1: String = "World"
                  let v2: String = string_concat(v0, v1)
                  let v3: Html = escape(v2)
                  v3
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: String = "Hello, "
                  let v1: String = "World"
                  let v4: String = "Hello, World"
                  let v3: Html = text("Hello, World")
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_flatten_a_nested_html_concat() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.html_concat([
                        t.text("a"),
                        t.html_concat(vec![t.text("b"), t.text("c")]),
                        t.html_concat(vec![t.text("d")]),
                    ])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
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
                entrypoint Test@e0() -> Html {
                  let v0: Html = text("a")
                  let v1: Html = text("b")
                  let v2: Html = text("c")
                  let v4: Html = text("d")
                  let v6: Html = concat(v0, v1, v2, v4)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_bind_an_html_concat_in_the_arm_that_reads_it_whole() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("flag", Ty::Bool)], |t| {
                    let inner = t.html_concat([t.text("a"), t.text("b")]);
                    t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.html_concat([inner, t.text("c")]));
                        m.arm(Pat::False, |_, _| inner);
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(flag@v0: Bool) -> Html {
                  let v1: Html = text("a")
                  let v2: Html = text("b")
                  let v3: Html = concat(v1, v2)
                  let v6: Html = match v0 {
                    true => {
                      let v4: Html = text("c")
                      let v5: Html = concat(v3, v4)
                      v5
                    }
                    false => {
                      v3
                    }
                  }
                  v6
                }

                -- after --
                entrypoint Test@e0(flag@v0: Bool) -> Html {
                  let v1: Html = text("a")
                  let v2: Html = text("b")
                  let v6: Html = match v0 {
                    true => {
                      let v4: Html = text("c")
                      let v5: Html = concat(v1, v2, v4)
                      v5
                    }
                    false => {
                      let v3: Html = concat(v1, v2)
                      v3
                    }
                  }
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn should_select_an_enum_arm_and_bind_its_fields_to_the_constructor_operands() {
        let types = IrTypeDeclarationsBuilder::new()
            .enum_(
                "Shape",
                [("Dot", vec![]), ("Circle", vec![("radius", Ty::Int)])],
            )
            .build();
        check(
            FlatProgramBuilder::with_types(types)
                .function("area", [("n", Ty::Int)], Ty::Int, |t| {
                    t.match_(
                        t.enum_variant_with_fields(
                            t.ty(Ty::named("Shape")),
                            "Circle",
                            [("radius", t.param("n"))],
                        ),
                        |m| {
                            m.arm(Pat::variant("Dot", []), |t, _| t.int(0));
                            m.arm(
                                Pat::variant("Circle", [("radius", Pat::bind("r"))]),
                                |t, arm| {
                                    t.mul(NumericType::Int, arm.binding("r"), arm.binding("r"))
                                },
                            );
                        },
                    )
                })
                .build()
                .unwrap(),
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
                      let v4: Int = mul[Int](v3, v3)
                      v4
                    }
                  }
                  v5
                }

                -- after --
                fn area@f0(n@v0: Int) -> Int {
                  let v1: Shape = Circle {radius: v0}
                  let v4: Int = mul[Int](v0, v0)
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_select_the_some_arm_and_bind_its_value() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.match_(t.some(t.str("x")), |m| {
                        m.arm(Pat::some(Pat::bind("v")), |t, arm| {
                            t.escape(arm.binding("v"))
                        });
                        m.arm(Pat::None, |t, _| t.text("none"));
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
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
                entrypoint Test@e0() -> Html {
                  let v0: String = "x"
                  let v1: Option[String] = Some(v0)
                  let v3: Html = text("x")
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_saturate_a_float_to_int_conversion() {
        check(
            FlatProgramBuilder::new()
                .function("big", [], Ty::Int, |t| t.float_to_int(t.float(1e10)))
                .build()
                .unwrap(),
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
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.match_(t.eq(EquatableType::String, t.str("a"), t.str("a")), |m| {
                        m.arm(Pat::True, |t, _| t.text("same"));
                        m.arm(Pat::False, |t, _| t.text("other"));
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "a"
                  let v1: String = "a"
                  let v2: Bool = eq[String](v0, v1)
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
                entrypoint Test@e0() -> Html {
                  let v0: String = "a"
                  let v1: String = "a"
                  let v2: Bool = true
                  let v3: Html = text("same")
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_an_escaped_literal_into_text() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.html_concat([t.text("a "), t.escape(t.str("<b>")), t.text(" c")])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Html = text("a ")
                  let v1: String = "<b>"
                  let v2: Html = escape(v1)
                  let v3: Html = text(" c")
                  let v4: Html = concat(v0, v2, v3)
                  v4
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: Html = text("a ")
                  let v1: String = "<b>"
                  let v2: Html = text("&lt;b&gt;")
                  let v3: Html = text(" c")
                  let v4: Html = concat(v0, v2, v3)
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_an_escaped_literal_with_a_carriage_return() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| t.escape(t.str("a\rb")))
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "a\rb"
                  let v1: Html = escape(v0)
                  v1
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: String = "a\rb"
                  let v1: Html = escape(v0)
                  v1
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_literal_attributes_into_constants() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    t.element(
                        "p",
                        [
                            t.attr_value("class", t.str("note")),
                            t.attr_presence("hidden", t.bool(true)),
                            t.attr_presence("open", t.bool(false)),
                        ],
                        t.text("text"),
                    )
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "note"
                  let v1: Bool = true
                  let v2: Bool = false
                  let v3: Html = text("text")
                  let v4: Html = element("p", {class: v0, hidden: v1, open: v2}, v3)
                  v4
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: String = "note"
                  let v1: Bool = true
                  let v2: Bool = false
                  let v3: Html = text("text")
                  let v4: Html = element("p", {class: "note", hidden}, v3)
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_an_escape_of_a_string_the_pass_makes_constant() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let name = t.str("x");
                    let greeting = t.string_concat([t.str("hi "), name]);
                    t.html_concat([t.escape(name), t.escape(greeting)])
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "x"
                  let v1: String = "hi "
                  let v2: String = string_concat(v1, v0)
                  let v3: Html = escape(v0)
                  let v4: Html = escape(v2)
                  let v5: Html = concat(v3, v4)
                  v5
                }

                -- after --
                entrypoint Test@e0() -> Html {
                  let v0: String = "x"
                  let v1: String = "hi "
                  let v6: String = "hi x"
                  let v3: Html = text("x")
                  let v4: Html = text("hi x")
                  let v5: Html = concat(v3, v4)
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn should_fold_a_literal_bound_outside_a_loop_into_its_body() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("items", Ty::array(Ty::Int))], |t| {
                    let label = t.str("row");
                    t.html_for_unbound(t.param("items"), |t| {
                        t.element("li", [t.attr_value("title", label)], t.escape(label))
                    })
                })
                .build()
                .unwrap(),
            expect![[r#"
                -- before --
                entrypoint Test@e0(items@v0: Array[Int]) -> Html {
                  let v1: String = "row"
                  let v4: Html = for _ in v0 {
                    let v2: Html = escape(v1)
                    let v3: Html = element("li", {title: v1}, v2)
                    v3
                  }
                  v4
                }

                -- after --
                entrypoint Test@e0(items@v0: Array[Int]) -> Html {
                  let v1: String = "row"
                  let v4: Html = for _ in v0 {
                    let v2: Html = text("row")
                    let v3: Html = element("li", {title: "row"}, v2)
                    v3
                  }
                  v4
                }
            "#]],
        );
    }
}
