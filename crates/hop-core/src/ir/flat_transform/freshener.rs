use std::collections::HashMap;

use crate::hop::typing::Pattern;
use crate::ir::binder_id::{BinderId, BinderIdCounter};
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatForSource, FlatOp};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// Copies a block with fresh names and binders, so the copy shares none of
/// the names and binders it binds with the original or any other copy. A
/// Read of a binder that has an argument is not copied, and what reads it
/// reads the argument instead. A name or binder the block reads without
/// binding it is read as it is.
pub struct Freshener<'a> {
    value_ids: &'a mut ValueIdCounter,
    binder_ids: &'a mut BinderIdCounter,
    /// The argument for each binder that has one.
    arguments: HashMap<BinderId, ValueId>,
    /// The name in the copy of every binding copied so far: the argument
    /// for a Read of a binder that has one, and a fresh name for any other
    /// binding.
    names: HashMap<ValueId, ValueId>,
    /// The fresh binder in the copy of every loop or pattern binder copied
    /// so far.
    binders: HashMap<BinderId, BinderId>,
}

impl<'a> Freshener<'a> {
    pub fn new(
        value_ids: &'a mut ValueIdCounter,
        binder_ids: &'a mut BinderIdCounter,
        arguments: HashMap<BinderId, ValueId>,
    ) -> Self {
        Freshener {
            value_ids,
            binder_ids,
            arguments,
            names: HashMap::new(),
            binders: HashMap::new(),
        }
    }

    fn name(&self, name: ValueId) -> ValueId {
        self.names.get(&name).copied().unwrap_or(name)
    }

    fn fresh_binder(&mut self, binder: &IrBinder) -> IrBinder {
        let var = self.binder_ids.next();
        self.binders.insert(binder.var, var);
        IrBinder {
            var,
            typ: binder.typ.clone(),
        }
    }

    pub fn freshen_block(&mut self, block: &FlatBlock) -> FlatBlock {
        let bindings = block
            .bindings
            .iter()
            .filter_map(|binding| self.freshen_binding(binding))
            .collect();
        FlatBlock {
            bindings,
            result: self.name(block.result),
        }
    }

    /// The copy of a binding, or nothing for a Read of a binder that has an
    /// argument, whose name stands for the argument in the copy.
    fn freshen_binding(&mut self, binding: &FlatBinding) -> Option<FlatBinding> {
        let op = match &binding.op {
            FlatOp::Read(binder) => {
                if let Some(argument) = self.arguments.get(binder) {
                    self.names.insert(binding.name, *argument);
                    return None;
                }
                FlatOp::Read(self.binders.get(binder).copied().unwrap_or(*binder))
            }
            FlatOp::Match(match_) => FlatOp::Match(IrMatch {
                subject: self.name(match_.subject),
                arms: match_
                    .arms
                    .iter()
                    .map(|arm| {
                        let mut pattern = arm.pattern.clone();
                        pattern.for_each_binding_mut(&mut |binding| {
                            if let Pattern::Binding { name, .. } = binding {
                                let fresh = self.binder_ids.next();
                                self.binders.insert(*name, fresh);
                                *name = fresh;
                            }
                        });
                        IrMatchArm {
                            pattern,
                            body: self.freshen_block(&arm.body),
                        }
                    })
                    .collect(),
            }),
            FlatOp::HtmlFor { var, source, body } => {
                let source = match source {
                    FlatForSource::Array(array) => FlatForSource::Array(self.name(*array)),
                    FlatForSource::RangeInclusive { start, end } => FlatForSource::RangeInclusive {
                        start: self.name(*start),
                        end: self.name(*end),
                    },
                };
                let var = var.as_ref().map(|binder| self.fresh_binder(binder));
                FlatOp::HtmlFor {
                    var,
                    source,
                    body: self.freshen_block(body),
                }
            }
            op => {
                let mut op = op.clone();
                op.for_each_operand_mut(&mut |operand| *operand = self.name(*operand));
                op
            }
        };
        let name = self.value_ids.next();
        self.names.insert(binding.name, name);
        Some(FlatBinding {
            name,
            typ: binding.typ.clone(),
            op,
        })
    }
}
