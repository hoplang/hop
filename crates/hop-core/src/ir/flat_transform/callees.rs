use std::collections::BTreeSet;

use crate::ir::flat_program::{FlatBlock, FlatFor, FlatLambda, FlatOp};
use crate::ir::function_id::FunctionId;
use crate::ir::lambda_id::LambdaId;

pub fn collect_callees(
    block: &FlatBlock,
    lambdas: &[FlatLambda],
    created: &mut BTreeSet<LambdaId>,
    out: &mut BTreeSet<FunctionId>,
) {
    for binding in &block.bindings {
        match &binding.op {
            FlatOp::Call(function, ..) => {
                out.insert(*function);
            }
            FlatOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_callees(&arm.body, lambdas, created, out);
                }
            }
            FlatOp::HtmlFor(FlatFor { key, body, .. }) => {
                if let Some(key) = key {
                    collect_callees(key, lambdas, created, out);
                }
                collect_callees(body, lambdas, created, out);
            }
            FlatOp::Lambda(id, _) => {
                if created.insert(*id) {
                    let lambda = lambdas.iter().find(|lambda| lambda.id == *id).unwrap();
                    collect_callees(&lambda.body, lambdas, created, out);
                }
            }
            FlatOp::Cell(initial) => collect_callees(initial, lambdas, created, out),
            FlatOp::Data(_)
            | FlatOp::AssertEq(..)
            | FlatOp::HtmlText(_)
            | FlatOp::HtmlEscape(_)
            | FlatOp::HtmlConcat(_)
            | FlatOp::HtmlElement(_)
            | FlatOp::Apply(..) => {}
        }
    }
}
