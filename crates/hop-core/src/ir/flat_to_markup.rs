use std::collections::{HashMap, HashSet};

use crate::hop::typing::Type;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatModule, FlatOp};
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_html_element::IrHtmlElement;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::markup_module::{
    MarkupBlock, MarkupBody, MarkupCallArg, MarkupFunctionDeclaration, MarkupModule, MarkupNode,
    MarkupValueBinding, MarkupValueBlock, MarkupValueOp,
};
use crate::ir::value_id::{ValueId, ValueIdCounter};

/// Convert a Flat module to Markup.
///
/// The Html a function returns is unfolded into markup from its result: an
/// Html binding becomes markup where it is read, as a part of a concat, the
/// children of an element, the result of a block of markup or the argument
/// of a call for an Html parameter. An Html parameter becomes its slot
/// where it is read. A binding unfolded twice gives two copies, and a match
/// or loop copied a second time binds fresh names. The names its blocks
/// read from outside stay shared by every copy. An Html binding that nothing
/// reads is dropped.
///
/// Every binding that is not Html stays a value binding in its block. Html
/// is never part of another type, so no value but a call reads Html. Panics
/// if one does all the same.
pub fn flat_to_markup(module: FlatModule) -> MarkupModule {
    let mut cx = Converter {
        value_ids: module.value_ids,
        html_parameters: module
            .functions
            .iter()
            .flat_map(|function| &function.parameters)
            .filter(|param| param.binder.typ == Type::Html)
            .map(|param| param.binder.var)
            .collect(),
        bindings: HashMap::new(),
        copies: HashMap::new(),
    };
    let functions = module
        .functions
        .into_iter()
        .map(|function| {
            let body = if function.return_type == Type::Html {
                MarkupBody::Markup(cx.markup_block(&function.body))
            } else {
                MarkupBody::Value(cx.value_block(&function.body))
            };
            MarkupFunctionDeclaration {
                function: function.function,
                entrypoint: function.entrypoint,
                parameters: function.parameters,
                return_type: function.return_type,
                body,
            }
        })
        .collect();
    MarkupModule {
        functions,
        value_ids: cx.value_ids,
    }
}

struct Converter {
    value_ids: ValueIdCounter,
    /// The Html parameters of every function. Html is never part of another
    /// type, so these are the only names of Html that no binding binds.
    html_parameters: HashSet<ValueId>,
    /// Every binding of the blocks converted so far, by name, to unfold an
    /// Html binding where it is read. Names are unique, so one map serves
    /// every block.
    bindings: HashMap<ValueId, FlatBinding>,
    /// How many times each match and loop of Html has been unfolded.
    copies: HashMap<ValueId, usize>,
}

impl Converter {
    /// The markup of a block whose result is Html, with the bindings of the
    /// block that are not Html.
    fn markup_block(&mut self, block: &FlatBlock) -> MarkupBlock {
        for binding in &block.bindings {
            self.bindings.insert(binding.binder.var, binding.clone());
        }
        let mut markup = Vec::new();
        self.unfold(block.result, &mut markup);
        let bindings = block
            .bindings
            .iter()
            .filter(|binding| binding.binder.typ != Type::Html)
            .map(|binding| self.value_binding(binding))
            .collect();
        MarkupBlock { bindings, markup }
    }

    /// The result of a block whose result is data, with the bindings of the
    /// block that are not Html. An Html binding of the block is unfolded
    /// where a call reads it, or dropped when nothing does.
    fn value_block(&mut self, block: &FlatBlock) -> MarkupValueBlock {
        for binding in &block.bindings {
            self.bindings.insert(binding.binder.var, binding.clone());
        }
        MarkupValueBlock {
            bindings: block
                .bindings
                .iter()
                .filter(|binding| binding.binder.typ != Type::Html)
                .map(|binding| self.value_binding(binding))
                .collect(),
            result: block.result,
        }
    }

    /// A binding of data. Panics if it has type Html, builds Html, or reads
    /// Html other than as the argument of a call.
    fn value_binding(&mut self, binding: &FlatBinding) -> MarkupValueBinding {
        let Type::Data(typ) = binding.binder.typ.clone() else {
            panic!("{} is a value of type Html", binding.binder.var);
        };
        if !matches!(binding.op, FlatOp::Call(..)) {
            binding.op.for_each_operand(&mut |operand| {
                assert!(
                    !self.html_parameters.contains(&operand)
                        && self
                            .bindings
                            .get(&operand)
                            .is_none_or(|b| b.binder.typ != Type::Html),
                    "{} reads the Html of {operand} as a value",
                    binding.binder.var
                );
            });
        }
        let op = match binding.op.clone() {
            FlatOp::Data(op) => MarkupValueOp::Data(op),
            FlatOp::Call(function, args) => MarkupValueOp::Call(function, self.call_args(args)),
            FlatOp::Match(IrMatch { subject, arms }) => MarkupValueOp::Match(IrMatch {
                subject,
                arms: arms
                    .into_iter()
                    .map(|arm| IrMatchArm {
                        pattern: arm.pattern,
                        body: self.value_block(&arm.body),
                    })
                    .collect(),
            }),
            op @ (FlatOp::HtmlText(_)
            | FlatOp::HtmlEscape(_)
            | FlatOp::HtmlConcat(_)
            | FlatOp::HtmlElement(_)
            | FlatOp::HtmlFor(_)) => {
                panic!("{} builds Html as a value: {op:?}", binding.binder.var)
            }
        };
        MarkupValueBinding {
            name: binding.binder.var,
            typ,
            op,
        }
    }

    /// Append the markup of the Html that `html` names.
    fn unfold(&mut self, html: ValueId, out: &mut Vec<MarkupNode>) {
        if self.html_parameters.contains(&html) {
            out.push(MarkupNode::Slot(html));
            return;
        }
        let binding = self.bindings[&html].clone();
        match binding.op {
            FlatOp::HtmlText(text) => out.push(MarkupNode::Text(text)),

            FlatOp::HtmlEscape(string) => out.push(MarkupNode::Escape(string)),

            FlatOp::HtmlConcat(parts) => {
                for part in parts {
                    self.unfold(part, out);
                }
            }

            FlatOp::HtmlElement(IrHtmlElement {
                element,
                attributes,
                children,
            }) => {
                let mut markup_children = Vec::new();
                if !element.is_void() {
                    self.unfold(children, &mut markup_children);
                }
                out.push(MarkupNode::Element(IrHtmlElement {
                    element,
                    attributes,
                    children: markup_children,
                }));
            }

            FlatOp::Match(_) | FlatOp::HtmlFor(_) => {
                let count = self.copies.entry(html).or_insert(0);
                *count += 1;
                let op = if *count == 1 {
                    binding.op
                } else {
                    self.freshen(&binding)
                };
                match op {
                    FlatOp::Match(IrMatch { subject, arms }) => {
                        let arms = arms
                            .into_iter()
                            .map(|arm| IrMatchArm {
                                pattern: arm.pattern,
                                body: self.markup_block(&arm.body),
                            })
                            .collect();
                        out.push(MarkupNode::Match(IrMatch { subject, arms }));
                    }
                    FlatOp::HtmlFor(IrFor { var, array, body }) => {
                        out.push(MarkupNode::For(IrFor {
                            var,
                            array,
                            body: self.markup_block(&body),
                        }));
                    }
                    _ => unreachable!("a copy of a match or loop is a match or loop"),
                }
            }

            FlatOp::Call(function, args) => {
                let args = self.call_args(args);
                out.push(MarkupNode::Call(function, args));
            }

            op => panic!("{html} holds Html as a value: {op:?}"),
        }
    }

    /// The arguments of a call: a value as it is, and Html as the markup it
    /// unfolds to.
    fn call_args(&mut self, args: Vec<ValueId>) -> Vec<MarkupCallArg> {
        args.into_iter()
            .map(|arg| {
                if self.html_parameters.contains(&arg)
                    || self
                        .bindings
                        .get(&arg)
                        .is_some_and(|b| b.binder.typ == Type::Html)
                {
                    let mut markup = Vec::new();
                    self.unfold(arg, &mut markup);
                    MarkupCallArg::Markup(markup)
                } else {
                    MarkupCallArg::Value(arg)
                }
            })
            .collect()
    }

    /// The op of a copy of a binding, with fresh names in its blocks.
    fn freshen(&mut self, binding: &FlatBinding) -> FlatOp {
        let mut freshener = Freshener::new(&mut self.value_ids, HashMap::new());
        let block = freshener.freshen_block(&FlatBlock {
            bindings: vec![binding.clone()],
            result: binding.binder.var,
        });
        block
            .bindings
            .into_iter()
            .next()
            .expect("a copy of a binding is a binding")
            .op
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::flat_module_builder::{FlatModuleBuilder, Pat};
    use crate::ir::flat_module_generator::random_module;
    use crate::ir::optimize_flat;
    use expect_test::{Expect, expect};

    fn check(module: FlatModule, expected: Expect) {
        let flat = optimize_flat(module);
        let before = flat.to_string();
        let markup = flat_to_markup(flat).to_string();
        expected.assert_eq(&format!("-- flat --\n{before}\n-- markup --\n{markup}"));
    }

    /// The conversion finds no Html held in a value, whether the module is
    /// optimized or not.
    #[test]
    fn fuzz_random_modules_convert_without_html_held_in_values() {
        arbtest::arbtest(|u| {
            let (module, _) = random_module(u);
            flat_to_markup(module.clone()).to_string();
            flat_to_markup(optimize_flat(module)).to_string();
            Ok(())
        });
    }

    #[test]
    fn should_convert_an_element_to_markup_and_keep_its_attributes_as_values() {
        check(
            FlatModuleBuilder::new()
                .entrypoint(
                    "Card",
                    [("title", "String"), ("hidden", "Bool")],
                    "Html",
                    |t| {
                        t.element(
                            "div",
                            [
                                t.attr("class", t.str("card")),
                                t.presence("hidden", t.var("hidden")),
                            ],
                            t.html_concat(vec![t.text("Title: "), t.escape(t.var("title"))]),
                        )
                    },
                )
                .build(),
            expect![[r#"
                -- flat --
                entrypoint fn Card@f0(title@v0: String, hidden@v1: Bool) -> Html {
                  let v2: String = "card"
                  let v3: Html = text("Title: ")
                  let v4: Html = escape(v0)
                  let v5: Html = concat(v3, v4)
                  let v6: Html = element("div", {class: v2, hidden: v1}, v5)
                  v6
                }

                -- markup --
                entrypoint fn Card@f0(title@v0: String, hidden@v1: Bool) -> Html {
                  let v2: String = "card"

                  element(
                    "div",
                    {class: v2, hidden: v1},
                    [text("Title: "), escape(v0)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_copy_html_used_twice_and_share_the_values_it_reads() {
        check(
            FlatModuleBuilder::new()
                .entrypoint("Greeting", [("name", "Option[String]")], "Html", |t| {
                    let greeting = t.match_expr(t.var("name"), |m| {
                        m.arm(Pat::some(Pat::bind("n")), |t| {
                            t.element("b", [], t.escape(t.var("n")))
                        });
                        m.arm(Pat::None, |t| t.text("anonymous"));
                    });
                    t.element("p", [], t.html_concat(vec![greeting, greeting]))
                })
                .build(),
            expect![[r#"
                -- flat --
                entrypoint fn Greeting@f0(name@v0: Option[String]) -> Html {
                  let v5: Html = match v0 {
                    Some(v1: String) => {
                      let v2: Html = escape(v1)
                      let v3: Html = element("b", {}, v2)
                      v3
                    }
                    None => {
                      let v4: Html = text("anonymous")
                      v4
                    }
                  }
                  let v6: Html = concat(v5, v5)
                  let v7: Html = element("p", {}, v6)
                  v7
                }

                -- markup --
                entrypoint fn Greeting@f0(name@v0: Option[String]) -> Html {
                  element(
                    "p",
                    {},
                    [
                      match v0 {
                        Some(v1: String) => {
                          element("b", {}, [escape(v1)])
                        }
                        None => {
                          text("anonymous")
                        }
                      },
                      match v0 {
                        Some(v8: String) => {
                          element("b", {}, [escape(v8)])
                        }
                        None => {
                          text("anonymous")
                        }
                      },
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_keep_a_value_outside_the_loop_that_renders_it() {
        check(
            FlatModuleBuilder::new()
                .entrypoint(
                    "Items",
                    [("items", "Array[String]"), ("n", "Int")],
                    "Html",
                    |t| {
                        let _unused = t.text("never");
                        let row = t.element(
                            "li",
                            [t.attr("data-x", t.int_to_string(t.add(t.var("n"), t.int(1))))],
                            t.text("x"),
                        );
                        t.element("ul", [], t.html_for(Some("item"), t.var("items"), |_| row))
                    },
                )
                .build(),
            expect![[r#"
                -- flat --
                entrypoint fn Items@f0(items@v0: Array[String], n@v1: Int) -> Html {
                  let v3: Int = 1
                  let v4: Int = v1 + v3
                  let v5: String = v4.to_string()
                  let v6: Html = text("x")
                  let v7: Html = element("li", {data-x: v5}, v6)
                  let v9: Html = for _ in v0 {
                    v7
                  }
                  let v10: Html = element("ul", {}, v9)
                  v10
                }

                -- markup --
                entrypoint fn Items@f0(items@v0: Array[String], n@v1: Int) -> Html {
                  let v3: Int = 1
                  let v4: Int = v1 + v3
                  let v5: String = v4.to_string()

                  element(
                    "ul",
                    {},
                    [
                      for _ in v0 {
                        element("li", {data-x: v5}, [text("x")])
                      },
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_unfold_the_children_of_inlined_calls() {
        check(
            FlatModuleBuilder::new()
                .function("Badge", [("label", "String")], "Html", |t| {
                    t.element("span", [], t.escape(t.var("label")))
                })
                .function("Wrap", [("content", "Html")], "Html", |t| {
                    t.element("div", [], t.var("content"))
                })
                .entrypoint("Test", [("label", "String")], "Html", |t| {
                    t.call(
                        "Wrap",
                        [t.html_concat(vec![
                            t.call("Badge", vec![t.var("label")]),
                            t.element("b", vec![], t.html_concat(vec![])),
                        ])],
                    )
                })
                .build(),
            expect![[r#"
                -- flat --
                entrypoint fn Test@f2(label@v5: String) -> Html {
                  let v11: Html = escape(v5)
                  let v12: Html = element("span", {}, v11)
                  let v7: Html = concat()
                  let v8: Html = element("b", {}, v7)
                  let v9: Html = concat(v12, v8)
                  let v13: Html = element("div", {}, v9)
                  v13
                }

                -- markup --
                entrypoint fn Test@f2(label@v5: String) -> Html {
                  element(
                    "div",
                    {},
                    [
                      element("span", {}, [escape(v5)]),
                      element("b", {}, []),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn should_pass_markup_to_a_recursive_function_that_renders_it_in_a_slot() {
        check(
            FlatModuleBuilder::new()
                .function(
                    "Nest",
                    [("depth", "Int"), ("children", "Html")],
                    "Html",
                    |t| {
                        t.match_expr(t.lt(t.int(0), t.var("depth")), |m| {
                            m.arm(Pat::True, |t| {
                                t.element(
                                    "div",
                                    [],
                                    t.call(
                                        "Nest",
                                        vec![t.sub(t.var("depth"), t.int(1)), t.var("children")],
                                    ),
                                )
                            });
                            m.arm(Pat::False, |t| t.var("children"));
                        })
                    },
                )
                .entrypoint("Test", [("name", "String")], "Html", |t| {
                    t.call(
                        "Nest",
                        [t.int(2), t.element("b", vec![], t.escape(t.var("name")))],
                    )
                })
                .build(),
            expect![[r#"
                -- flat --
                fn Nest@f0(depth@v0: Int, children@v1: Html) -> Html {
                  let v2: Int = 0
                  let v3: Bool = v2 < v0
                  let v8: Html = match v3 {
                    true => {
                      let v4: Int = 1
                      let v5: Int = v0 - v4
                      let v6: Html = Nest@f0(v5, v1)
                      let v7: Html = element("div", {}, v6)
                      v7
                    }
                    false => {
                      v1
                    }
                  }
                  v8
                }
                entrypoint fn Test@f1(name@v9: String) -> Html {
                  let v10: Int = 2
                  let v11: Html = escape(v9)
                  let v12: Html = element("b", {}, v11)
                  let v13: Html = Nest@f0(v10, v12)
                  v13
                }

                -- markup --
                fn Nest@f0(depth@v0: Int, children@v1: Html) -> Html {
                  let v2: Int = 0
                  let v3: Bool = v2 < v0

                  match v3 {
                    true => {
                      let v4: Int = 1
                      let v5: Int = v0 - v4

                      element("div", {}, [Nest@f0(v5, [slot(v1)])])
                    }
                    false => {
                      slot(v1)
                    }
                  }
                }
                entrypoint fn Test@f1(name@v9: String) -> Html {
                  let v10: Int = 2

                  Nest@f0(v10, [element("b", {}, [escape(v9)])])
                }
            "#]],
        );
    }
}
