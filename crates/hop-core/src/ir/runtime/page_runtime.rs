use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::html::write_escaped_html;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_program::FlatProgram;
use crate::ir::runtime::clicks::clicks;
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::flat_evaluator::{invoke_entrypoint, run_effect};
use crate::ir::runtime::state::{ElementId, State};
use crate::ir::runtime::value::{Effect, Html, HtmlNode, Value};
use crate::symbols::attribute_name::AttributeName;

/// An entrypoint as a page runs it: the state of its cells and elements, the
/// Html its latest render gave, and the label of each element it has shown.
///
/// A click runs the effect of its handlers in an effect transaction on the
/// state, then renders the entrypoint in a render transaction on the state
/// the effects leave, and the runtime takes the state and the Html the
/// render gives. The runtime takes nothing until both have finished, so a
/// click that fails, whether its effect or the render after it, leaves the
/// runtime as it was.
///
/// An element is labelled by the order in which a render first shows it, in
/// document order, so that its label does not depend on the identity the
/// evaluator gives it. The runtime is displayed as the latest render, with
/// each element written with its label.
pub struct PageRuntime<'a> {
    program: &'a FlatProgram,
    entrypoint: EntrypointId,
    args: HashMap<AttributeName, Value<'a>>,
    state: State<'a>,
    html: Html<'a>,
    targets: Vec<(ElementId, Result<Rc<Effect<'a>>, EvalError>)>,
    shown: Vec<Shown>,
    labels: Labels,
}

/// An element the latest render shows.
pub struct Shown {
    pub label: usize,
    pub tag: String,
    /// Whether the element has a handler or is inside an element that has
    /// one.
    pub clickable: bool,
}

impl<'a> PageRuntime<'a> {
    /// Render the entrypoint on the arguments, from a state with no cells
    /// and no elements.
    pub fn open(
        program: &'a FlatProgram,
        entrypoint: EntrypointId,
        args: HashMap<AttributeName, Value<'a>>,
    ) -> Result<Self, EvalError> {
        let render = State::default().begin_render();
        let (value, state) = invoke_entrypoint(program, entrypoint, args.clone(), render)?;
        let mut runtime = PageRuntime {
            program,
            entrypoint,
            args,
            state,
            html: value.unwrap_html(),
            targets: Vec::new(),
            shown: Vec::new(),
            labels: Labels::default(),
        };
        runtime.show();
        Ok(runtime)
    }

    /// The elements the latest render shows, in document order.
    pub fn shown(&self) -> &[Shown] {
        &self.shown
    }

    /// The markup of the latest render, as written to a page.
    pub fn markup(&self) -> String {
        let mut markup = String::new();
        self.html.write(&mut markup);
        markup
    }

    /// Click the element with the label. A click on an element that is not
    /// clickable does nothing. Panics if the latest render does not show the
    /// element.
    pub fn click(&mut self, label: usize) -> Result<(), EvalError> {
        if !self.shown.iter().any(|shown| shown.label == label) {
            panic!("e{label} is not shown");
        }
        let identity = self.labels.identity(label);
        let Some((_, target)) = self.targets.iter().find(|(target, _)| *target == identity) else {
            return Ok(());
        };
        let effect = target.clone()?;
        let mut effects = self.state.begin_effects();
        run_effect(&effect, &mut effects, &self.program.functions)?;
        let render = effects.commit().begin_render();
        let (value, state) =
            invoke_entrypoint(self.program, self.entrypoint, self.args.clone(), render)?;
        self.state = state;
        self.html = value.unwrap_html();
        self.show();
        Ok(())
    }

    /// Take the clicks of the latest render, and the elements it shows,
    /// labelling those that no render has shown before in document order.
    fn show(&mut self) {
        self.targets = clicks(&self.program.functions, &self.html);
        self.shown.clear();
        let mut pending = vec![&self.html];
        while let Some(html) = pending.pop() {
            match html.node() {
                HtmlNode::Element {
                    element,
                    children,
                    identity,
                    ..
                } => {
                    self.shown.push(Shown {
                        label: self.labels.label(*identity),
                        tag: element.as_str().to_string(),
                        clickable: self.targets.iter().any(|(target, _)| target == identity),
                    });
                    pending.push(children);
                }
                HtmlNode::Concat(parts) => pending.extend(parts.iter().rev()),
                HtmlNode::Text(_) | HtmlNode::Escape(_) => {}
            }
        }
    }
}

/// The labels of the elements a run shows, by the order in which it first
/// shows them.
#[derive(Default)]
struct Labels {
    identities: Vec<ElementId>,
    labels: HashMap<ElementId, usize>,
}

impl Labels {
    /// The label of the identity, which takes the next label if the run has not
    /// shown it before. Should be called on each element a render shows, in
    /// document order.
    fn label(&mut self, node: ElementId) -> usize {
        *self.labels.entry(node).or_insert_with(|| {
            self.identities.push(node);
            self.identities.len()
        })
    }

    /// The label of the identity. Panics if the run has not shown it.
    fn get(&self, node: ElementId) -> usize {
        self.labels[&node]
    }

    /// The identity of the label. Panics if the run has not shown it.
    fn identity(&self, label: usize) -> ElementId {
        self.identities[label - 1]
    }
}

impl fmt::Display for PageRuntime<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        let mut items = Vec::new();
        collect_items(&self.html, &mut items);
        write_items(&items, 0, &self.labels, &mut out);
        f.write_str(&out)
    }
}

enum Item<'h, 'a> {
    Element(&'h Html<'a>),
    Text(String),
}

/// The elements and the text of the Html, with each run of text that is
/// not split by an element joined, and escaped as written to a page.
fn collect_items<'h, 'a>(html: &'h Html<'a>, out: &mut Vec<Item<'h, 'a>>) {
    let push_text = |out: &mut Vec<Item<'h, 'a>>, text: &str| match out.last_mut() {
        Some(Item::Text(last)) => last.push_str(text),
        _ => out.push(Item::Text(text.to_string())),
    };
    match html.node() {
        HtmlNode::Text(text) => push_text(out, text),
        HtmlNode::Escape(text) => {
            let mut escaped = String::new();
            write_escaped_html(text, &mut escaped);
            push_text(out, &escaped);
        }
        HtmlNode::Element { .. } => out.push(Item::Element(html)),
        HtmlNode::Concat(parts) => {
            for part in parts {
                collect_items(part, out);
            }
        }
    }
}

/// Write each item on lines of its own at the depth, with text trimmed and
/// text that is only whitespace left out.
fn write_items(items: &[Item<'_, '_>], depth: usize, labels: &Labels, out: &mut String) {
    for item in items {
        match item {
            Item::Text(text) => {
                let text = text.trim();
                if !text.is_empty() {
                    out.push_str(&"  ".repeat(depth));
                    out.push_str(text);
                    out.push('\n');
                }
            }
            Item::Element(html) => write_element(html, depth, labels, out),
        }
    }
}

/// Write the element with its label, on one line if its children are a
/// single text and indented below it otherwise.
fn write_element(html: &Html<'_>, depth: usize, labels: &Labels, out: &mut String) {
    let HtmlNode::Element {
        element,
        attributes,
        children,
        identity,
        ..
    } = html.node()
    else {
        unreachable!("an item is an element or text");
    };
    out.push_str(&"  ".repeat(depth));
    out.push_str(&format!("<{}@e{}", element.as_str(), labels.get(*identity)));
    for attribute in attributes {
        out.push(' ');
        out.push_str(attribute.name.as_str());
        if let Some(value) = &attribute.value {
            out.push_str("=\"");
            write_escaped_html(value, out);
            out.push('"');
        }
    }
    out.push('>');
    if element.is_void() {
        out.push('\n');
        return;
    }
    let mut items = Vec::new();
    collect_items(children, &mut items);
    if let [Item::Text(text)] = items.as_slice() {
        out.push_str(text);
    } else if !items.is_empty() {
        out.push('\n');
        write_items(&items, depth + 1, labels, out);
        out.push_str(&"  ".repeat(depth));
    }
    out.push_str(&format!("</{}>\n", element.as_str()));
}
