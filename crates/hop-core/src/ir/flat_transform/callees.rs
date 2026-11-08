use std::collections::BTreeSet;

use crate::ir::flat_program::{
    FlatFor, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatPureBlock, FlatPureOp,
};
use crate::ir::function_id::FunctionId;

pub fn collect_pure_callees(block: &FlatPureBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        collect_pure_op_callees(&binding.op, out);
    }
}

pub fn collect_html_callees(block: &FlatHtmlBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        let binding = match binding {
            FlatHtmlBinding::Pure(binding) => {
                collect_pure_op_callees(&binding.op, out);
                continue;
            }
            FlatHtmlBinding::Cell(..) | FlatHtmlBinding::Sample(..) => continue,
            FlatHtmlBinding::Html(binding) => binding,
        };
        match &binding.op {
            FlatHtmlOp::Call(function, ..) => {
                out.insert(function.id);
            }
            FlatHtmlOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_html_callees(&arm.body, out);
                }
            }
            FlatHtmlOp::For(FlatFor { key, body, .. }) => {
                if let Some(key) = key {
                    collect_pure_callees(key, out);
                }
                collect_html_callees(body, out);
            }
            FlatHtmlOp::Lambda(lambda) => {
                collect_html_callees(&lambda.body, out);
            }
            FlatHtmlOp::Text(_)
            | FlatHtmlOp::Escape(_)
            | FlatHtmlOp::Concat(_)
            | FlatHtmlOp::Element(_)
            | FlatHtmlOp::Apply(..) => {}
        }
    }
}

fn collect_pure_op_callees(op: &FlatPureOp, out: &mut BTreeSet<FunctionId>) {
    match op {
        FlatPureOp::Call(function, ..) => {
            out.insert(function.id);
        }
        FlatPureOp::Match(match_) => {
            for arm in &match_.arms {
                collect_pure_callees(&arm.body, out);
            }
        }
        FlatPureOp::Lambda(lambda) => {
            collect_pure_callees(&lambda.body, out);
        }
        FlatPureOp::Data(_)
        | FlatPureOp::CellWrite(..)
        | FlatPureOp::CellRead(_)
        | FlatPureOp::EffectPure(_)
        | FlatPureOp::EffectBind(..)
        | FlatPureOp::Apply(..) => {}
    }
}
