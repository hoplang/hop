use std::collections::HashMap;

use crate::hop::typing::Pattern;
use crate::ir::flat_program::{FlatBlock, FlatFor, FlatOp};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_block::IrBlock;
use crate::ir::ir_lambda::IrLambda;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::ir_type::IrType;
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
    fn freshen_match(&mut self, match_: &IrMatch<FlatBlock>) -> IrMatch<FlatBlock> {
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
                        body: self.freshen_block(&arm.body),
                    }
                })
                .collect(),
        }
    }

    pub fn freshen_block(&mut self, block: &FlatBlock) -> FlatBlock {
        let bindings = block
            .bindings
            .iter()
            .map(|binding| self.freshen_binding(binding))
            .collect();
        IrBlock {
            bindings,
            result: self.name(block.result),
        }
    }

    fn freshen_binding(
        &mut self,
        binding: &IrBinding<IrType, FlatOp>,
    ) -> IrBinding<IrType, FlatOp> {
        let op = match &binding.op {
            FlatOp::Match(match_) => FlatOp::Match(self.freshen_match(match_)),
            FlatOp::Cell(initial) => FlatOp::Cell(self.freshen_block(initial)),
            FlatOp::For(FlatFor {
                var,
                array,
                key,
                body,
            }) => {
                let array = self.name(*array);
                let var = var.as_ref().map(|binder| self.fresh_binder(binder));
                FlatOp::For(FlatFor {
                    var,
                    array,
                    key: key.as_ref().map(|key| self.freshen_block(key)),
                    body: self.freshen_block(body),
                })
            }
            FlatOp::Lambda(lambda) => FlatOp::Lambda(IrLambda {
                parameters: (lambda.parameters.iter())
                    .map(|binder| self.fresh_binder(binder))
                    .collect(),
                body: self.freshen_block(&lambda.body),
            }),
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
