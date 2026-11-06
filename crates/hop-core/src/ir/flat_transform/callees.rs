use std::collections::BTreeSet;

use crate::ir::flat_program::{FlatBinding, FlatDataBlock, FlatDataOp, FlatHtmlBlock, FlatHtmlOp};
use crate::ir::function_id::FunctionId;
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_type::IrFunctionDataType;

pub fn collect_data_callees(block: &FlatDataBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        collect_data_binding_callees(binding, out);
    }
}

pub fn collect_html_callees(block: &FlatHtmlBlock, out: &mut BTreeSet<FunctionId>) {
    for binding in &block.bindings {
        let binding = match binding {
            FlatBinding::Data(binding) => {
                collect_data_binding_callees(binding, out);
                continue;
            }
            FlatBinding::Html(binding) => binding,
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
            FlatHtmlOp::For(IrFor { body, .. }) => {
                collect_html_callees(body, out);
            }
            FlatHtmlOp::Lambda(lambda) => {
                collect_html_callees(&lambda.body, out);
            }
            _ => {}
        }
    }
}

fn collect_data_binding_callees(
    binding: &IrBinding<IrFunctionDataType, FlatDataOp>,
    out: &mut BTreeSet<FunctionId>,
) {
    match &binding.op {
        FlatDataOp::Call(function, ..) => {
            out.insert(function.id);
        }
        FlatDataOp::Match(match_) => {
            for arm in &match_.arms {
                collect_data_callees(&arm.body, out);
            }
        }
        FlatDataOp::Lambda(lambda) => {
            collect_data_callees(&lambda.body, out);
        }
        FlatDataOp::Data(_) | FlatDataOp::Apply(..) => {}
    }
}
