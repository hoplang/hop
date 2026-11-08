use std::rc::Rc;

use crate::ir::flat_program::FlatFunction;
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::flat_evaluator::evaluate_pure_apply;
use crate::ir::runtime::html_node::{Html, HtmlNode};
use crate::ir::runtime::state::NodeId;
use crate::ir::runtime::value::Effect;

/// The effect of a click on each element of `html` that has a handler or is
/// inside an element that has one, by its node, in document order. The
/// handler of the element is applied, then those of the elements around it,
/// innermost first, and the effect runs the effects they give in that
/// order. A click whose handlers do not all give an effect fails with the
/// error of the first that does not.
pub fn clicks<'a>(
    functions: &'a [FlatFunction],
    html: &Html<'a>,
) -> Vec<(NodeId, Result<Rc<Effect<'a>>, EvalError>)> {
    fn walk<'a>(
        functions: &'a [FlatFunction],
        html: &Html<'a>,
        around: Option<&Result<Rc<Effect<'a>>, EvalError>>,
        out: &mut Vec<(NodeId, Result<Rc<Effect<'a>>, EvalError>)>,
    ) {
        match html.node() {
            HtmlNode::Element {
                children,
                on_click,
                node,
                ..
            } => {
                let effect = match on_click {
                    None => around.cloned(),
                    Some(handler) => {
                        // The handler is applied to nothing, on a frame and an
                        // op count of its own. It is Pure, so the application
                        // reads no state and adds none.
                        let own = evaluate_pure_apply(handler, Vec::new(), functions, 0, &mut 0);
                        Some(match (own, around) {
                            (Err(error), _) => Err(error),
                            (Ok(_), Some(Err(error))) => Err(error.clone()),
                            (Ok(own), None) => Ok(own.as_effect().clone()),
                            (Ok(own), Some(Ok(around))) => Ok(Rc::new(Effect::Then(
                                own.as_effect().clone(),
                                around.clone(),
                            ))),
                        })
                    }
                };
                if let Some(effect) = &effect {
                    out.push((*node, effect.clone()));
                }
                walk(functions, children, effect.as_ref(), out);
            }
            HtmlNode::Concat(parts) => {
                for part in parts {
                    walk(functions, part, around, out);
                }
            }
            HtmlNode::Text(_) | HtmlNode::Escape(_) => {}
        }
    }
    let mut out = Vec::new();
    walk(functions, html, None, &mut out);
    out
}
