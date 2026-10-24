use std::collections::{HashMap, HashSet};

use crate::hop::typing::Type;
use crate::ir::flat_module::{FlatBinding, FlatBlock, FlatModule, FlatOp};
use crate::ir::flat_transform::freshener::Freshener;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_html_element::IrHtmlElement;
use crate::ir::ir_match::{IrMatch, IrMatchArm};
use crate::ir::markup_module::{
    MarkupCallArg, MarkupDataBinding, MarkupDataBlock, MarkupDataFunction, MarkupDataOp,
    MarkupFunctionDeclaration, MarkupHtmlBlock, MarkupHtmlFunction, MarkupModule, MarkupNode,
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
        .map(|function| match function.return_type {
            Type::Html => MarkupFunctionDeclaration::Html(MarkupHtmlFunction {
                body: cx.html_block(&function.body),
                function: function.function,
                entrypoint: function.entrypoint,
                parameters: function.parameters,
            }),
            Type::Data(return_type) => MarkupFunctionDeclaration::Data(MarkupDataFunction {
                body: cx.data_block(&function.body),
                function: function.function,
                entrypoint: function.entrypoint,
                parameters: function.parameters,
                return_type,
            }),
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
    fn html_block(&mut self, block: &FlatBlock) -> MarkupHtmlBlock {
        for binding in &block.bindings {
            self.bindings.insert(binding.binder.var, binding.clone());
        }
        let mut markup = Vec::new();
        self.unfold(block.result, &mut markup);
        let bindings = block
            .bindings
            .iter()
            .filter(|binding| binding.binder.typ != Type::Html)
            .map(|binding| self.data_binding(binding))
            .collect();
        MarkupHtmlBlock { bindings, markup }
    }

    /// The result of a block whose result is data, with the bindings of the
    /// block that are not Html. An Html binding of the block is unfolded
    /// where a call reads it, or dropped when nothing does.
    fn data_block(&mut self, block: &FlatBlock) -> MarkupDataBlock {
        for binding in &block.bindings {
            self.bindings.insert(binding.binder.var, binding.clone());
        }
        MarkupDataBlock {
            bindings: block
                .bindings
                .iter()
                .filter(|binding| binding.binder.typ != Type::Html)
                .map(|binding| self.data_binding(binding))
                .collect(),
            result: block.result,
        }
    }

    /// A binding of data. Panics if it has type Html, builds Html, or reads
    /// Html other than as the argument of a call.
    fn data_binding(&mut self, binding: &FlatBinding) -> MarkupDataBinding {
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
            FlatOp::Data(op) => MarkupDataOp::Data(op),
            FlatOp::Call(function, args) => MarkupDataOp::Call(function, self.call_args(args)),
            FlatOp::Match(IrMatch { subject, arms }) => MarkupDataOp::Match(IrMatch {
                subject,
                arms: arms
                    .into_iter()
                    .map(|arm| IrMatchArm {
                        pattern: arm.pattern,
                        body: self.data_block(&arm.body),
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
        MarkupDataBinding {
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
                                body: self.html_block(&arm.body),
                            })
                            .collect();
                        out.push(MarkupNode::Match(IrMatch { subject, arms }));
                    }
                    FlatOp::HtmlFor(IrFor { var, array, body }) => {
                        out.push(MarkupNode::For(IrFor {
                            var,
                            array,
                            body: self.html_block(&body),
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
                    MarkupCallArg::Html(markup)
                } else {
                    MarkupCallArg::Data(arg)
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
    use crate::hop::typing::Ty;
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
                    [("title", Ty::String), ("hidden", Ty::Bool)],
                    Ty::Html,
                    |t| {
                        t.element(
                            "div",
                            [
                                t.attr_value("class", t.str("card")),
                                t.attr_presence("hidden", t.param("hidden")),
                            ],
                            t.html_concat(vec![t.text("Title: "), t.escape(t.param("title"))]),
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
    fn should_copy_html_used_in_two_arms_and_share_the_values_it_reads() {
        check(
            FlatModuleBuilder::new()
                .entrypoint(
                    "Greeting",
                    [("name", Ty::option(Ty::String)), ("formal", Ty::Bool)],
                    Ty::Html,
                    |t| {
                        let greeting = t.match_(t.param("name"), |m| {
                            m.arm(Pat::some(Pat::bind("n")), |t, arm| {
                                t.element("b", [], t.escape(arm.binding("n")))
                            });
                            m.arm(Pat::None, |t, _| t.text("anonymous"));
                        });
                        t.match_(t.param("formal"), |m| {
                            m.arm(Pat::True, |t, _| t.element("p", [], greeting));
                            m.arm(Pat::False, |t, _| t.element("i", [], greeting));
                        })
                    },
                )
                .build(),
            expect![[r#"
                -- flat --
                entrypoint fn Greeting@f0(name@v0: Option[String], formal@v1: Bool) -> Html {
                  let v6: Html = match v0 {
                    Some(v2: String) => {
                      let v3: Html = escape(v2)
                      let v4: Html = element("b", {}, v3)
                      v4
                    }
                    None => {
                      let v5: Html = text("anonymous")
                      v5
                    }
                  }
                  let v9: Html = match v1 {
                    true => {
                      let v7: Html = element("p", {}, v6)
                      v7
                    }
                    false => {
                      let v8: Html = element("i", {}, v6)
                      v8
                    }
                  }
                  v9
                }

                -- markup --
                entrypoint fn Greeting@f0(name@v0: Option[String], formal@v1: Bool) -> Html {
                  match v1 {
                    true => {
                      element(
                        "p",
                        {},
                        [
                          match v0 {
                            Some(v2: String) => {
                              element("b", {}, [escape(v2)])
                            }
                            None => {
                              text("anonymous")
                            }
                          },
                        ],
                      )
                    }
                    false => {
                      element(
                        "i",
                        {},
                        [
                          match v0 {
                            Some(v10: String) => {
                              element("b", {}, [escape(v10)])
                            }
                            None => {
                              text("anonymous")
                            }
                          },
                        ],
                      )
                    }
                  }
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
                    [("items", Ty::array(Ty::String)), ("n", Ty::Int)],
                    Ty::Html,
                    |t| {
                        let _unused = t.text("never");
                        let data_x = t.int_to_string(t.add(t.param("n"), t.int(1)));
                        t.element(
                            "ul",
                            [],
                            t.html_for(t.param("items"), |t, _| {
                                t.element("li", [t.attr_value("data-x", data_x)], t.text("x"))
                            }),
                        )
                    },
                )
                .build(),
            expect![[r#"
                -- flat --
                entrypoint fn Items@f0(items@v0: Array[String], n@v1: Int) -> Html {
                  let v3: Int = 1
                  let v4: Int = v1 + v3
                  let v5: String = v4.to_string()
                  let v9: Html = for _ in v0 {
                    let v7: Html = text("x")
                    let v8: Html = element("li", {data-x: v5}, v7)
                    v8
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
                .function("Badge", [("label", Ty::String)], Ty::Html, |t| {
                    t.element("span", [], t.escape(t.param("label")))
                })
                .function("Wrap", [("content", Ty::Html)], Ty::Html, |t| {
                    t.element("div", [], t.param("content"))
                })
                .entrypoint("Test", [("label", Ty::String)], Ty::Html, |t| {
                    t.call(
                        "Wrap",
                        [t.html_concat(vec![
                            t.call("Badge", vec![t.param("label")]),
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
                    [("depth", Ty::Int), ("children", Ty::Html)],
                    Ty::Html,
                    |t| {
                        t.match_(t.lt(t.int(0), t.param("depth")), |m| {
                            m.arm(Pat::True, |t, _| {
                                t.element(
                                    "div",
                                    [],
                                    t.call(
                                        "Nest",
                                        vec![
                                            t.sub(t.param("depth"), t.int(1)),
                                            t.param("children"),
                                        ],
                                    ),
                                )
                            });
                            m.arm(Pat::False, |t, _| t.param("children"));
                        })
                    },
                )
                .entrypoint("Test", [("name", Ty::String)], Ty::Html, |t| {
                    t.call(
                        "Nest",
                        [t.int(2), t.element("b", vec![], t.escape(t.param("name")))],
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
