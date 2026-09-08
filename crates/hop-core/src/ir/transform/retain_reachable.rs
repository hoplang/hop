use std::collections::{HashMap, HashSet};

use crate::ir::function_id::FunctionId;
use crate::ir::pure_module::{PureExpr, PureModule};

/// A pass that drops the functions no page can reach.
pub fn retain_reachable(module: PureModule) -> PureModule {
    let PureModule {
        pages,
        functions,
        expr_ids,
        var_ids,
    } = module;

    let callees: HashMap<FunctionId, HashSet<FunctionId>> = functions
        .iter()
        .map(|function| {
            let mut out = HashSet::new();
            collect_callees(&function.body, &mut out);
            (function.function.id, out)
        })
        .collect();

    let mut reachable: HashSet<FunctionId> = HashSet::new();
    let mut frontier: Vec<FunctionId> = Vec::new();
    for page in &pages {
        let mut out = HashSet::new();
        collect_callees(&page.body, &mut out);
        frontier.extend(out);
    }
    while let Some(id) = frontier.pop() {
        if !reachable.insert(id) {
            continue;
        }
        if let Some(next) = callees.get(&id) {
            frontier.extend(next.iter().copied());
        }
    }

    let functions = functions
        .into_iter()
        .filter(|function| reachable.contains(&function.function.id))
        .collect();

    PureModule {
        pages,
        functions,
        expr_ids,
        var_ids,
    }
}

fn collect_callees(expr: &PureExpr, out: &mut HashSet<FunctionId>) {
    if let PureExpr::FunctionCall { function, .. } = expr {
        out.insert(function.id);
    }
    expr.for_each_child(&mut |child| collect_callees(child, out));
}
