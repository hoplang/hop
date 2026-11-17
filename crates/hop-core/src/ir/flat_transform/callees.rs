use std::collections::BTreeSet;

use crate::ir::flat_program::{FlatBlock, FlatFor, FlatOp};
use crate::ir::function_id::FunctionId;

pub fn collect_callees(block: &FlatBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        match &binding.op {
            FlatOp::Call(function, ..) => {
                out.insert(function.id);
            }
            FlatOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_callees(&arm.body, out);
                }
            }
            FlatOp::For(FlatFor { key, body, .. }) => {
                if let Some(key) = key {
                    collect_callees(key, out);
                }
                collect_callees(body, out);
            }
            FlatOp::Lambda(lambda) => {
                collect_callees(&lambda.body, out);
            }
            FlatOp::Cell(initial) => collect_callees(initial, out),
            FlatOp::Data(_)
            | FlatOp::Mint
            | FlatOp::CellWrite(..)
            | FlatOp::CellRead(_)
            | FlatOp::Text(_)
            | FlatOp::Escape(_)
            | FlatOp::Concat(_)
            | FlatOp::Element(_)
            | FlatOp::Apply(..) => {}
        }
    }
}
