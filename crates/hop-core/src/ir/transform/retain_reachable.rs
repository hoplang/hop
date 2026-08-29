use std::collections::{HashMap, HashSet};

use crate::ir::pure_module::{PureExpr, PureModule};
use crate::symbols::function_name::FunctionName;

/// A pass that drops the functions no page can reach.
pub fn retain_reachable(module: PureModule) -> PureModule {
    let PureModule {
        pages,
        functions,
        records,
        enums,
        expr_ids,
        var_ids,
    } = module;

    let callees: HashMap<FunctionName, HashSet<FunctionName>> = functions
        .iter()
        .map(|function| {
            let mut out = HashSet::new();
            collect_callees(&function.body, &mut out);
            (function.name.clone(), out)
        })
        .collect();

    let mut reachable: HashSet<FunctionName> = HashSet::new();
    let mut frontier: Vec<FunctionName> = Vec::new();
    for page in &pages {
        let mut out = HashSet::new();
        collect_callees(&page.body, &mut out);
        frontier.extend(out);
    }
    while let Some(name) = frontier.pop() {
        if !reachable.insert(name.clone()) {
            continue;
        }
        if let Some(next) = callees.get(&name) {
            frontier.extend(next.iter().cloned());
        }
    }

    let functions = functions
        .into_iter()
        .filter(|function| reachable.contains(&function.name))
        .collect();

    PureModule {
        pages,
        functions,
        records,
        enums,
        expr_ids,
        var_ids,
    }
}

fn collect_callees(expr: &PureExpr, out: &mut HashSet<FunctionName>) {
    if let PureExpr::FunctionCall { function_name, .. } = expr {
        out.insert(function_name.clone());
    }
    expr.for_each_child(&mut |child| collect_callees(child, out));
}
