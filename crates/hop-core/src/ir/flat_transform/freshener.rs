use std::collections::HashMap;

use crate::hop::typing::Pattern;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatOp};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// Copies a block with fresh names, so the copy shares none of the names it
/// binds with the original or any other copy. A name that has an argument
/// is read as the argument in the copy. Any other name the block reads
/// without binding it is read as it is.
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

    fn fresh_binder(&mut self, binder: &IrBinder) -> IrBinder {
        let var = self.value_ids.next();
        self.names.insert(binder.var, var);
        IrBinder {
            var,
            typ: binder.typ.clone(),
        }
    }

    pub fn freshen_block(&mut self, block: &FlatBlock) -> FlatBlock {
        let bindings = block
            .bindings
            .iter()
            .map(|binding| self.freshen_binding(binding))
            .collect();
        FlatBlock {
            bindings,
            result: self.name(block.result),
        }
    }

    fn freshen_binding(&mut self, binding: &FlatBinding) -> FlatBinding {
        let op = match &binding.op {
            FlatOp::Match(match_) => FlatOp::Match(IrMatch {
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
            }),
            FlatOp::HtmlFor(IrFor { var, array, body }) => {
                let array = self.name(*array);
                let var = var.as_ref().map(|binder| self.fresh_binder(binder));
                FlatOp::HtmlFor(IrFor {
                    var,
                    array,
                    body: self.freshen_block(body),
                })
            }
            op => {
                let mut op = op.clone();
                op.for_each_operand_mut(&mut |operand| *operand = self.name(*operand));
                op
            }
        };
        FlatBinding {
            binder: self.fresh_binder(&binding.binder),
            op,
        }
    }
}
