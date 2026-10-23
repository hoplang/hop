use crate::hop::typing::Type;
use crate::ir::flat_module::{FlatBlock, FlatModule, FlatOp};
use crate::ir::ir_function::IrFunction;
use crate::ir::value_id::ValueId;
use std::collections::HashSet;

/// Panics unless every Html value is read at most once on each path and
/// never from a loop body it is bound outside of.
pub fn assert_html_affine(module: &FlatModule) {
    let mut html = HashSet::new();
    for function in &module.functions {
        html.extend(
            function
                .parameters
                .iter()
                .filter(|param| param.binder.typ == Type::Html)
                .map(|param| param.binder.var),
        );
        collect_html(&function.body, &mut html);
    }
    for function in &module.functions {
        let scope = function
            .parameters
            .iter()
            .map(|param| param.binder.var)
            .filter(|name| html.contains(name))
            .collect();
        check_block(&function.body, scope, &html, &function.function);
    }
}

fn collect_html(block: &FlatBlock, html: &mut HashSet<ValueId>) {
    for binding in &block.bindings {
        if binding.binder.typ == Type::Html {
            html.insert(binding.binder.var);
        }
        match &binding.op {
            FlatOp::Match(match_) => {
                for arm in &match_.arms {
                    collect_html(&arm.body, html);
                }
            }
            FlatOp::HtmlFor(for_) => collect_html(&for_.body, html),
            _ => {}
        }
    }
}

fn check_block(
    block: &FlatBlock,
    mut scope: HashSet<ValueId>,
    html: &HashSet<ValueId>,
    function: &IrFunction,
) -> HashSet<ValueId> {
    for binding in &block.bindings {
        binding
            .op
            .for_each_operand(&mut |operand| read(operand, &mut scope, html, function));
        match &binding.op {
            FlatOp::Match(match_) => {
                let mut after = scope.clone();
                for arm in &match_.arms {
                    let inner = check_block(&arm.body, scope.clone(), html, function);
                    after.retain(|name| inner.contains(name));
                }
                scope = after;
            }
            FlatOp::HtmlFor(for_) => {
                check_block(&for_.body, HashSet::new(), html, function);
            }
            _ => {}
        }
        if html.contains(&binding.binder.var) {
            scope.insert(binding.binder.var);
        }
    }
    read(block.result, &mut scope, html, function);
    scope
}

fn read(
    name: ValueId,
    scope: &mut HashSet<ValueId>,
    html: &HashSet<ValueId>,
    function: &IrFunction,
) {
    if html.contains(&name) {
        assert!(
            scope.remove(&name),
            "{function} reads the Html value {name} when the path has used it or in a loop body it is bound outside of"
        );
    }
}
