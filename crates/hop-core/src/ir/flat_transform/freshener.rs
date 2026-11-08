use std::collections::HashMap;

use crate::hop::typing::Pattern;
use crate::ir::flat_program::{
    FlatFor, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatPureBlock, FlatPureOp,
};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::ir_type::{IrFunctionDataType, IrFunctionHtmlType};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// Copies a block with fresh names, so the copy shares none of the names
/// it binds, cells among them, with the original or any other copy. A name
/// that has an argument is read as the argument in the copy. Any other name
/// the block reads or writes without binding it is read as it is.
pub struct Freshener<'a> {
    value_ids: &'a mut ValueIdCounter,
    /// The name in the copy for every name that has an argument, which is
    /// the argument, and for every name copied so far, which is fresh.
    names: HashMap<ValueId, ValueId>,
}

impl<'a> Freshener<'a> {
    /// A freshener that reads the argument for each name that has one.
    pub fn new(value_ids: &'a mut ValueIdCounter, arguments: HashMap<ValueId, ValueId>) -> Self {
        Freshener {
            value_ids,
            names: arguments,
        }
    }

    fn name(&self, name: ValueId) -> ValueId {
        self.names.get(&name).copied().unwrap_or(name)
    }

    fn fresh_binder<T: Clone>(&mut self, binder: &IrBinder<T>) -> IrBinder<T> {
        let var = self.value_ids.next();
        self.names.insert(binder.var, var);
        IrBinder {
            var,
            typ: binder.typ.clone(),
        }
    }

    /// Copy the arms of a match, each pattern binding fresh names, and its
    /// subject.
    fn freshen_match<B>(
        &mut self,
        match_: &IrMatch<B>,
        freshen_block: impl Fn(&mut Self, &B) -> B,
    ) -> IrMatch<B> {
        IrMatch {
            subject: self.name(match_.subject),
            arms: match_
                .arms
                .iter()
                .map(|arm| {
                    let mut pattern = arm.pattern.clone();
                    pattern.for_each_binding_mut(&mut |binding| {
                        if let Pattern::Binding(binder) = binding {
                            *binder = self.fresh_binder(binder);
                        }
                    });
                    IrMatchArm {
                        pattern,
                        body: freshen_block(self, &arm.body),
                    }
                })
                .collect(),
        }
    }

    fn freshen_lambda<P: Clone, B>(
        &mut self,
        lambda: &IrLambda<P, B>,
        freshen_block: impl Fn(&mut Self, &B) -> B,
    ) -> IrLambda<P, B> {
        IrLambda {
            parameters: (lambda.parameters.iter())
                .map(|binder| self.fresh_binder(binder))
                .collect(),
            body: freshen_block(self, &lambda.body),
        }
    }

    pub fn freshen_pure_block(&mut self, block: &FlatPureBlock) -> FlatPureBlock {
        let bindings = block
            .bindings
            .iter()
            .map(|binding| self.freshen_pure_binding(binding))
            .collect();
        IrBlock {
            bindings,
            result: self.name(block.result),
        }
    }

    pub fn freshen_html_block(&mut self, block: &FlatHtmlBlock) -> FlatHtmlBlock {
        let bindings = block
            .bindings
            .iter()
            .map(|binding| match binding {
                FlatHtmlBinding::Pure(binding) => {
                    FlatHtmlBinding::Pure(self.freshen_pure_binding(binding))
                }
                FlatHtmlBinding::Cell(binder, initial) => {
                    let initial = self.name(*initial);
                    FlatHtmlBinding::Cell(self.fresh_binder(binder), initial)
                }
                FlatHtmlBinding::Sample(binder, cell) => {
                    let cell = self.name(*cell);
                    FlatHtmlBinding::Sample(self.fresh_binder(binder), cell)
                }
                FlatHtmlBinding::Html(binding) => {
                    FlatHtmlBinding::Html(self.freshen_html_binding(binding))
                }
            })
            .collect();
        IrBlock {
            bindings,
            result: self.name(block.result),
        }
    }

    fn freshen_pure_binding(
        &mut self,
        binding: &IrBinding<IrFunctionDataType, FlatPureOp>,
    ) -> IrBinding<IrFunctionDataType, FlatPureOp> {
        let op = match &binding.op {
            FlatPureOp::Match(match_) => {
                FlatPureOp::Match(self.freshen_match(match_, Self::freshen_pure_block))
            }
            FlatPureOp::Lambda(lambda) => {
                FlatPureOp::Lambda(self.freshen_lambda(lambda, Self::freshen_pure_block))
            }
            op => {
                let mut op = op.clone();
                op.for_each_operand_mut(&mut |operand| *operand = self.name(*operand));
                op
            }
        };
        IrBinding {
            binder: self.fresh_binder(&binding.binder),
            op,
        }
    }

    fn freshen_html_binding(
        &mut self,
        binding: &IrBinding<IrFunctionHtmlType, FlatHtmlOp>,
    ) -> IrBinding<IrFunctionHtmlType, FlatHtmlOp> {
        let op = match &binding.op {
            FlatHtmlOp::Match(match_) => {
                FlatHtmlOp::Match(self.freshen_match(match_, Self::freshen_html_block))
            }
            FlatHtmlOp::For(FlatFor {
                var,
                array,
                key,
                body,
            }) => {
                let array = self.name(*array);
                let var = var.as_ref().map(|binder| self.fresh_binder(binder));
                FlatHtmlOp::For(FlatFor {
                    var,
                    array,
                    key: key.as_ref().map(|key| self.freshen_pure_block(key)),
                    body: self.freshen_html_block(body),
                })
            }
            FlatHtmlOp::Lambda(lambda) => {
                FlatHtmlOp::Lambda(self.freshen_lambda(lambda, Self::freshen_html_block))
            }
            op => {
                let mut op = op.clone();
                op.for_each_operand_mut(&mut |operand| *operand = self.name(*operand));
                op
            }
        };
        IrBinding {
            binder: self.fresh_binder(&binding.binder),
            op,
        }
    }
}
