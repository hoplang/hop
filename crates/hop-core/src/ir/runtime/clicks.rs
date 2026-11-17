use std::rc::Rc;

use crate::ir::runtime::state::ElementId;
use crate::ir::runtime::value::{Closure, Html, HtmlNode};

/// The handlers of a click on each element of `html` that has a handler or is
/// inside an element that has one, by its node, in document order: the
/// handler of the element, then those of the elements around it, innermost
/// first, in the order a click applies them.
pub fn clicks<'a>(html: &Html<'a>) -> Vec<(ElementId, Vec<Rc<Closure<'a>>>)> {
    fn walk<'a>(
        html: &Html<'a>,
        around: &[Rc<Closure<'a>>],
        out: &mut Vec<(ElementId, Vec<Rc<Closure<'a>>>)>,
    ) {
        match html.node() {
            HtmlNode::Element {
                children,
                on_click,
                identity,
                ..
            } => {
                let handlers: Vec<Rc<Closure<'a>>> = (on_click.iter().cloned())
                    .chain(around.iter().cloned())
                    .collect();
                if !handlers.is_empty() {
                    out.push((*identity, handlers.clone()));
                }
                walk(children, &handlers, out);
            }
            HtmlNode::Concat(parts) => {
                for part in parts {
                    walk(part, around, out);
                }
            }
            HtmlNode::Text(_) | HtmlNode::Escape(_) => {}
        }
    }
    let mut out = Vec::new();
    walk(html, &[], &mut out);
    out
}
