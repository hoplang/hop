use std::collections::HashMap;

use crate::ir::binder_id::BinderId;
use crate::ir::flat_module::{
    FlatAttribute, FlatBinding, FlatBlock, FlatForSource, FlatFunctionDeclaration, FlatModule,
    FlatOp, FlatPageDeclaration,
};
use crate::ir::ir_match::{EnumMatchArm, Match};
use crate::ir::pure_module::{PureAttribute, PureExpr, PureForSource, PureModule};
use crate::ir::var_id::{VarId, VarIdCounter};

/// Lower a Pure module to the Flat IR.
///
/// Each Pure expression that computes a value becomes one binding with a
/// fresh name, in evaluation order. A let is the name of its body, and a
/// reference to the let's variable binds nothing, it is the name of the
/// let's value. A reference to any other binder, a parameter, a loop
/// variable or a match arm's variable, becomes a Read. The short-circuit
/// operators become bool matches, so the right operand is evaluated only
/// when the left one does not decide.
pub fn pure_to_flat(module: PureModule) -> FlatModule {
    let mut cx = Lowering {
        var_ids: VarIdCounter::new(),
        lets: HashMap::new(),
    };
    let pages = module
        .pages
        .into_iter()
        .map(|page| FlatPageDeclaration {
            name: page.name,
            parameters: page.parameters,
            head: lower_block(page.head, &mut cx),
            body: lower_block(page.body, &mut cx),
        })
        .collect();
    let functions = module
        .functions
        .into_iter()
        .map(|function| FlatFunctionDeclaration {
            function: function.function,
            parameters: function.parameters,
            return_type: function.return_type,
            body: lower_block(function.body, &mut cx),
        })
        .collect();
    FlatModule {
        pages,
        functions,
        var_ids: cx.var_ids,
        binder_ids: module.binder_ids,
    }
}

/// What lowering carries from one expression to the next.
struct Lowering {
    /// Names the bindings.
    var_ids: VarIdCounter,
    /// The binding each let variable stands for. Binders are unique across
    /// the module, so one map serves every body.
    lets: HashMap<BinderId, VarId>,
}

fn lower_block(expr: PureExpr, cx: &mut Lowering) -> FlatBlock {
    let mut bindings = Vec::new();
    let result = lower_expr(expr, &mut bindings, cx);
    FlatBlock { bindings, result }
}

/// Appends the bindings of `expr` to `out` and returns the name of its
/// value.
fn lower_expr(expr: PureExpr, out: &mut Vec<FlatBinding>, cx: &mut Lowering) -> VarId {
    let typ = expr.typ();
    let op = match expr {
        PureExpr::Let {
            var, value, body, ..
        } => {
            let value = lower_expr(*value, out, cx);
            cx.lets.insert(var.var, value);
            return lower_expr(*body, out, cx);
        }

        PureExpr::VariableReference { value, .. } => match cx.lets.get(&value) {
            Some(name) => return *name,
            None => FlatOp::Read(value),
        },

        PureExpr::Match { match_, .. } => FlatOp::Match(match match_ {
            Match::Bool {
                subject,
                true_body,
                false_body,
            } => Match::Bool {
                subject: Box::new(lower_expr(*subject, out, cx)),
                true_body: Box::new(lower_block(*true_body, cx)),
                false_body: Box::new(lower_block(*false_body, cx)),
            },
            Match::Option {
                subject,
                some_arm_binding,
                some_arm_body,
                none_arm_body,
            } => Match::Option {
                subject: Box::new(lower_expr(*subject, out, cx)),
                some_arm_binding,
                some_arm_body: Box::new(lower_block(*some_arm_body, cx)),
                none_arm_body: Box::new(lower_block(*none_arm_body, cx)),
            },
            Match::Enum { subject, arms } => {
                let subject = lower_expr(*subject, out, cx);
                let arms = arms
                    .into_iter()
                    .map(|arm| EnumMatchArm {
                        pattern: arm.pattern,
                        bindings: arm.bindings,
                        body: lower_block(arm.body, cx),
                    })
                    .collect();
                Match::Enum {
                    subject: Box::new(subject),
                    arms,
                }
            }
        }),

        PureExpr::FieldAccess { record, field, .. } => FlatOp::FieldAccess {
            record: lower_expr(*record, out, cx),
            field,
        },

        PureExpr::StringLiteral { value, .. } => FlatOp::StringLiteral(value),

        PureExpr::HtmlText { content, .. } => FlatOp::HtmlText(content),

        PureExpr::HtmlEscape { expr, .. } => FlatOp::HtmlEscape(lower_expr(*expr, out, cx)),

        PureExpr::HtmlElement {
            element,
            attributes,
            children,
            ..
        } => {
            let attributes = attributes
                .into_iter()
                .map(|attribute| match attribute {
                    PureAttribute::Value { name, value } => FlatAttribute::Value {
                        name,
                        value: lower_expr(value, out, cx),
                    },
                    PureAttribute::Presence { name, present } => FlatAttribute::Presence {
                        name,
                        present: lower_expr(present, out, cx),
                    },
                })
                .collect();
            FlatOp::HtmlElement {
                element,
                attributes,
                children: lower_expr(*children, out, cx),
            }
        }

        PureExpr::HtmlConcat { parts, .. } => FlatOp::HtmlConcat(
            parts
                .into_iter()
                .map(|part| lower_expr(part, out, cx))
                .collect(),
        ),

        PureExpr::HtmlFor {
            var, source, body, ..
        } => {
            let source = match *source {
                PureForSource::Array(array) => FlatForSource::Array(lower_expr(array, out, cx)),
                PureForSource::RangeInclusive { start, end } => {
                    let start = lower_expr(start, out, cx);
                    let end = lower_expr(end, out, cx);
                    FlatForSource::RangeInclusive { start, end }
                }
            };
            FlatOp::HtmlFor {
                var,
                source,
                body: lower_block(*body, cx),
            }
        }

        PureExpr::Call { function, args, .. } => FlatOp::Call {
            function,
            args: args
                .into_iter()
                .map(|arg| lower_expr(arg, out, cx))
                .collect(),
        },

        PureExpr::BoolLiteral { value, .. } => FlatOp::BoolLiteral(value),

        PureExpr::FloatLiteral { value, .. } => FlatOp::FloatLiteral(value),

        PureExpr::IntLiteral { value, .. } => FlatOp::IntLiteral(value),

        PureExpr::Array { elements, .. } => FlatOp::Array(
            elements
                .into_iter()
                .map(|element| lower_expr(element, out, cx))
                .collect(),
        ),

        PureExpr::Tuple { elements, .. } => FlatOp::Tuple(
            elements
                .into_iter()
                .map(|element| lower_expr(element, out, cx))
                .collect(),
        ),

        PureExpr::TupleIndex { tuple, index, .. } => FlatOp::TupleIndex {
            tuple: lower_expr(*tuple, out, cx),
            index,
        },

        PureExpr::Record { fields, .. } => FlatOp::Record {
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_expr(value, out, cx)))
                .collect(),
        },

        PureExpr::Enum {
            variant_name,
            fields,
            ..
        } => FlatOp::Enum {
            variant_name,
            fields: fields
                .into_iter()
                .map(|(name, value)| (name, lower_expr(value, out, cx)))
                .collect(),
        },

        PureExpr::Option { value, .. } => {
            FlatOp::Option(value.map(|value| lower_expr(*value, out, cx)))
        }

        PureExpr::StringConcat { parts, .. } => FlatOp::StringConcat(
            parts
                .into_iter()
                .map(|part| lower_expr(part, out, cx))
                .collect(),
        ),

        PureExpr::NumericAdd {
            left,
            right,
            operand_types,
            ..
        } => FlatOp::NumericAdd {
            left: lower_expr(*left, out, cx),
            right: lower_expr(*right, out, cx),
            operand_types,
        },

        PureExpr::NumericSubtract {
            left,
            right,
            operand_types,
            ..
        } => FlatOp::NumericSubtract {
            left: lower_expr(*left, out, cx),
            right: lower_expr(*right, out, cx),
            operand_types,
        },

        PureExpr::NumericMultiply {
            left,
            right,
            operand_types,
            ..
        } => FlatOp::NumericMultiply {
            left: lower_expr(*left, out, cx),
            right: lower_expr(*right, out, cx),
            operand_types,
        },

        PureExpr::NumericNegation {
            operand,
            operand_type,
            ..
        } => FlatOp::NumericNegation {
            operand: lower_expr(*operand, out, cx),
            operand_type,
        },

        PureExpr::BoolNegation { operand, .. } => {
            FlatOp::BoolNegation(lower_expr(*operand, out, cx))
        }

        // The right operand runs only when the left one is true, so it
        // becomes the true arm. The false arm returns the left operand,
        // which is false.
        PureExpr::BoolLogicalAnd { left, right, .. } => {
            let left = lower_expr(*left, out, cx);
            FlatOp::Match(Match::Bool {
                subject: Box::new(left),
                true_body: Box::new(lower_block(*right, cx)),
                false_body: Box::new(FlatBlock {
                    bindings: Vec::new(),
                    result: left,
                }),
            })
        }

        // The right operand runs only when the left one is false, so it
        // becomes the false arm. The true arm returns the left operand,
        // which is true.
        PureExpr::BoolLogicalOr { left, right, .. } => {
            let left = lower_expr(*left, out, cx);
            FlatOp::Match(Match::Bool {
                subject: Box::new(left),
                true_body: Box::new(FlatBlock {
                    bindings: Vec::new(),
                    result: left,
                }),
                false_body: Box::new(lower_block(*right, cx)),
            })
        }

        PureExpr::Equals {
            left,
            right,
            operand_types,
            ..
        } => FlatOp::Equals {
            left: lower_expr(*left, out, cx),
            right: lower_expr(*right, out, cx),
            operand_types,
        },

        PureExpr::LessThan {
            left,
            right,
            operand_types,
            ..
        } => FlatOp::LessThan {
            left: lower_expr(*left, out, cx),
            right: lower_expr(*right, out, cx),
            operand_types,
        },

        PureExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => FlatOp::LessThanOrEqual {
            left: lower_expr(*left, out, cx),
            right: lower_expr(*right, out, cx),
            operand_types,
        },

        PureExpr::ArrayLength { array, .. } => FlatOp::ArrayLength(lower_expr(*array, out, cx)),

        PureExpr::ArrayIsEmpty { array, .. } => FlatOp::ArrayIsEmpty(lower_expr(*array, out, cx)),

        PureExpr::StringIsEmpty { string, .. } => {
            FlatOp::StringIsEmpty(lower_expr(*string, out, cx))
        }

        PureExpr::OptionIsSome { option, .. } => FlatOp::OptionIsSome(lower_expr(*option, out, cx)),

        PureExpr::OptionIsNone { option, .. } => FlatOp::OptionIsNone(lower_expr(*option, out, cx)),

        PureExpr::IntToString { value, .. } => FlatOp::IntToString(lower_expr(*value, out, cx)),

        PureExpr::FloatToInt { value, .. } => FlatOp::FloatToInt(lower_expr(*value, out, cx)),

        PureExpr::IntToFloat { value, .. } => FlatOp::IntToFloat(lower_expr(*value, out, cx)),
    };
    let name = cx.var_ids.next();
    out.push(FlatBinding { name, typ, op });
    name
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::hop::typing::{ComparableType, EquatableType, NumericType, Type};
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use expect_test::{Expect, expect};

    fn check(module: PureModule, expected: Expect) {
        let before = module.to_string();
        let after = pure_to_flat(module).to_string();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}"));
    }

    #[test]
    fn lowers_a_let_to_the_name_of_its_value() {
        check(
            PureModuleBuilder::new()
                .function("square_next", [("x", "Int")], "Int", |t| {
                    t.let_expr("y", t.add(t.var("x"), t.int(1)), |t| {
                        t.mul(t.var("y"), t.var("y"))
                    })
                })
                .build(),
            expect![[r#"
                -- before --
                fn square_next@f0(x@b0: Int) -> Int {
                  let b1: Int = (b0 + 1) in { (b1 * b1) }
                }

                -- after --
                fn square_next@f0(x@b0: Int) -> Int {
                  let v0: Int = b0
                  let v1: Int = 1
                  let v2: Int = v0 + v1
                  let v3: Int = v2 * v2
                  v3
                }
            "#]],
        );
    }

    #[test]
    fn lowers_an_element_with_its_attributes_before_its_children() {
        check(
            PureModuleBuilder::new()
                .page("Card", [("title", "String"), ("hidden", "Bool")], |t| {
                    t.element(
                        "div",
                        vec![
                            t.attr("class", t.str("card")),
                            t.presence("hidden", t.var("hidden")),
                        ],
                        vec![t.escape(t.var("title"))],
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                page Card(title@b0: String, hidden@b1: Bool) {
                  html(
                    tag: "div",
                    attrs: [class: "card", hidden: b1],
                    children: concat(escape(b0)),
                  )
                }

                -- after --
                page Card(title@b0: String, hidden@b1: Bool) {
                  let v1: String = "card"
                  let v2: Bool = b1
                  let v3: String = b0
                  let v4: Html = escape(v3)
                  let v5: Html = concat(v4)
                  let v6: Html = html(tag: "div", attrs: [class: v1, hidden: v2], children: v5)
                  v6
                }
            "#]],
        );
    }

    #[test]
    fn lowers_a_loop_body_into_a_nested_block() {
        check(
            PureModuleBuilder::new()
                .page("Items", [("items", "Array[String]")], |t| {
                    t.element(
                        "ul",
                        vec![],
                        vec![t.html_for(Some("item"), t.var("items"), |t| {
                            t.element("li", vec![], vec![t.escape(t.var("item"))])
                        })],
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                page Items(items@b0: Array[String]) {
                  html(
                    tag: "ul",
                    attrs: [],
                    children: concat(
                      for b1: String in b0 {
                        html(
                          tag: "li",
                          attrs: [],
                          children: concat(escape(b1)),
                        )
                      },
                    ),
                  )
                }

                -- after --
                page Items(items@b0: Array[String]) {
                  let v1: Array[String] = b0
                  let v6: Html = for b1: String in v1 {
                    let v2: String = b1
                    let v3: Html = escape(v2)
                    let v4: Html = concat(v3)
                    let v5: Html = html(tag: "li", attrs: [], children: v4)
                    v5
                  }
                  let v7: Html = concat(v6)
                  let v8: Html = html(tag: "ul", attrs: [], children: v7)
                  v8
                }
            "#]],
        );
    }

    #[test]
    fn lowers_a_range_loop_without_a_variable() {
        check(
            PureModuleBuilder::new()
                .page_no_params("Dots", |t| {
                    t.html_for_range(None, t.int(1), t.int(3), |t| t.text("."))
                })
                .build(),
            expect![[r#"
                -- before --
                page Dots() {
                  for _ in 1..=3 { text(".") }
                }

                -- after --
                page Dots() {
                  let v1: Int = 1
                  let v2: Int = 3
                  let v4: Html = for _ in v1..=v2 {
                    let v3: Html = text(".")
                    v3
                  }
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn lowers_an_option_match_with_a_binding() {
        check(
            PureModuleBuilder::new()
                .page("Greeting", [("name", "Option[String]")], |t| {
                    t.option_match_expr_with_binding(
                        t.var("name"),
                        "n",
                        |t| t.escape(t.var("n")),
                        t.text("anonymous"),
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                page Greeting(name@b0: Option[String]) {
                  match b0 {
                    Some(b1: String) => {
                      escape(b1)
                    }
                    None => {
                      text("anonymous")
                    }
                  }
                }

                -- after --
                page Greeting(name@b0: Option[String]) {
                  let v1: Option[String] = b0
                  let v5: Html = match v1 {
                    Some(b1: String) => {
                      let v2: String = b1
                      let v3: Html = escape(v2)
                      v3
                    }
                    None => {
                      let v4: Html = text("anonymous")
                      v4
                    }
                  }
                  v5
                }
            "#]],
        );
    }

    #[test]
    fn lowers_an_enum_match_with_bindings_and_a_record() {
        check(
            PureModuleBuilder::new()
                .record("Point", [("x", "Int"), ("y", "Int")])
                .enum_(
                    "Shape",
                    [("Dot", vec![]), ("Circle", vec![("center", "Point")])],
                )
                .function("origin_x", [("shape", "Shape")], "Int", |t| {
                    t.enum_match_expr(t.var("shape"), |arms| {
                        arms.arm("Dot", |t| t.int(0));
                        arms.arm_bound("Circle", [("center", "c")], |t| {
                            t.field_access(t.var("c"), "x")
                        });
                    })
                })
                .function("unit", [], "Shape", |t| {
                    t.enum_variant_with_fields(
                        "Shape",
                        "Circle",
                        vec![(
                            "center",
                            t.record("Point", vec![("x", t.int(1)), ("y", t.int(2))]),
                        )],
                    )
                })
                .build(),
            expect![[r#"
                -- before --
                fn origin_x@f0(shape@b0: Shape) -> Int {
                  match b0 {
                    Shape::Dot => {
                      0
                    }
                    Shape::Circle {center@b1: Point} => {
                      b1.x
                    }
                  }
                }
                fn unit@f1() -> Shape {
                  Shape::Circle {center: Point {x: 1, y: 2}}
                }

                -- after --
                fn origin_x@f0(shape@b0: Shape) -> Int {
                  let v0: Shape = b0
                  let v4: Int = match v0 {
                    Shape::Dot => {
                      let v1: Int = 0
                      v1
                    }
                    Shape::Circle {center@b1: Point} => {
                      let v2: Point = b1
                      let v3: Int = v2.x
                      v3
                    }
                  }
                  v4
                }
                fn unit@f1() -> Shape {
                  let v5: Int = 1
                  let v6: Int = 2
                  let v7: Point = {x: v5, y: v6}
                  let v8: Shape = Circle {center: v7}
                  v8
                }
            "#]],
        );
    }

    #[test]
    fn lowers_the_short_circuit_operators_to_bool_matches() {
        check(
            PureModuleBuilder::new()
                .function("both", [("a", "Bool"), ("b", "Bool")], "Bool", |t| {
                    t.and(t.var("a"), t.not(t.var("b")))
                })
                .function("either", [("a", "Bool"), ("b", "Bool")], "Bool", |t| {
                    t.or(t.var("a"), t.not(t.var("b")))
                })
                .build(),
            expect![[r#"
                -- before --
                fn both@f0(a@b0: Bool, b@b1: Bool) -> Bool {
                  (b0 && (!b1))
                }
                fn either@f1(a@b2: Bool, b@b3: Bool) -> Bool {
                  (b2 || (!b3))
                }

                -- after --
                fn both@f0(a@b0: Bool, b@b1: Bool) -> Bool {
                  let v0: Bool = b0
                  let v3: Bool = match v0 {
                    true => {
                      let v1: Bool = b1
                      let v2: Bool = !v1
                      v2
                    }
                    false => {
                      v0
                    }
                  }
                  v3
                }
                fn either@f1(a@b2: Bool, b@b3: Bool) -> Bool {
                  let v4: Bool = b2
                  let v7: Bool = match v4 {
                    true => {
                      v4
                    }
                    false => {
                      let v5: Bool = b3
                      let v6: Bool = !v5
                      v6
                    }
                  }
                  v7
                }
            "#]],
        );
    }

    #[test]
    fn lowers_a_call() {
        check(
            PureModuleBuilder::new()
                .function("double", [("x", "Int")], "Int", |t| {
                    t.add(t.var("x"), t.var("x"))
                })
                .page_no_params("Answer", |t| {
                    t.escape(t.int_to_string(t.call("double", vec![("x", t.int(21))])))
                })
                .build(),
            expect![[r#"
                -- before --
                fn double@f0(x@b0: Int) -> Int {
                  (b0 + b0)
                }
                page Answer() {
                  escape(call double@f0(21).to_string())
                }

                -- after --
                fn double@f0(x@b0: Int) -> Int {
                  let v5: Int = b0
                  let v6: Int = b0
                  let v7: Int = v5 + v6
                  v7
                }
                page Answer() {
                  let v1: Int = 21
                  let v2: Int = call double@f0(v1)
                  let v3: String = v2.to_string()
                  let v4: Html = escape(v3)
                  v4
                }
            "#]],
        );
    }

    #[test]
    fn lowers_tuples_and_their_indexing() {
        check(
            PureModuleBuilder::new()
                .function("first", [], "Int", |t| {
                    t.tuple_index(t.tuple(vec![t.int(1), t.str("a")]), 0)
                })
                .build(),
            expect![[r#"
                -- before --
                fn first@f0() -> Int {
                  (1, "a").0
                }

                -- after --
                fn first@f0() -> Int {
                  let v0: Int = 1
                  let v1: String = "a"
                  let v2: (Int, String) = (v0, v1)
                  let v3: Int = v2.0
                  v3
                }
            "#]],
        );
    }

    /// Nodes of a Pure expression that compute a value, itself included. A
    /// let binds nothing, and nor does a reference to a let's variable,
    /// which is the name of the let's value. A reference to any other
    /// binder becomes a Read. `lets` gathers the let variables seen so far,
    /// which come before any reference to them.
    fn count_nodes(expr: &PureExpr, lets: &mut HashSet<BinderId>) -> usize {
        let mut count = match expr {
            PureExpr::Let { var, .. } => {
                lets.insert(var.var);
                0
            }
            PureExpr::VariableReference { value, .. } if lets.contains(value) => 0,
            _ => 1,
        };
        expr.for_each_child(&mut |child| count += count_nodes(child, lets));
        count
    }

    /// Bindings of a block, those of its nested blocks included.
    fn count_bindings(block: &FlatBlock) -> usize {
        let mut count = block.bindings.len();
        for binding in &block.bindings {
            match &binding.op {
                FlatOp::Match(Match::Bool {
                    true_body,
                    false_body,
                    ..
                }) => {
                    count += count_bindings(true_body) + count_bindings(false_body);
                }
                FlatOp::Match(Match::Option {
                    some_arm_body,
                    none_arm_body,
                    ..
                }) => {
                    count += count_bindings(some_arm_body) + count_bindings(none_arm_body);
                }
                FlatOp::Match(Match::Enum { arms, .. }) => {
                    for arm in arms {
                        count += count_bindings(&arm.body);
                    }
                }
                FlatOp::HtmlFor { body, .. } => count += count_bindings(body),
                _ => {}
            }
        }
        count
    }

    /// The type of a name in scope.
    fn type_of(names: &[(VarId, Type)], name: VarId) -> &Type {
        let Some((_, typ)) = names.iter().find(|(bound, _)| *bound == name) else {
            panic!("{name} is not in scope");
        };
        typ
    }

    /// Asserts that every binding and binder is bound once, that every
    /// operand and the result are bindings bound earlier in the block or in
    /// an enclosing one, that a Read reads a binder in scope, that the types
    /// an op writes agree with the names it reads and with its own binding,
    /// and that a loop variable or option binding has the type of the
    /// elements of its source or subject. Returns the type of the result.
    fn check_block(
        block: &FlatBlock,
        names: &mut Vec<(VarId, Type)>,
        binders: &mut Vec<(BinderId, Type)>,
        seen: &mut HashSet<VarId>,
        seen_binders: &mut HashSet<BinderId>,
    ) -> Type {
        let names_len = names.len();
        for binding in &block.bindings {
            assert!(seen.insert(binding.name), "{} is bound twice", binding.name);
            binding.op.for_each_operand(&mut |operand| {
                type_of(names, operand);
            });
            let binders_len = binders.len();
            let expected = match &binding.op {
                FlatOp::Read(binder) => {
                    let Some((_, typ)) = binders.iter().find(|(bound, _)| bound == binder) else {
                        panic!("{} reads {binder}, which is not in scope", binding.name);
                    };
                    Some(typ.clone())
                }
                FlatOp::NumericAdd {
                    left,
                    right,
                    operand_types,
                }
                | FlatOp::NumericSubtract {
                    left,
                    right,
                    operand_types,
                }
                | FlatOp::NumericMultiply {
                    left,
                    right,
                    operand_types,
                } => {
                    let typ = match operand_types {
                        NumericType::Int => Type::Int,
                        NumericType::Float => Type::Float,
                    };
                    assert_eq!(type_of(names, *left), &typ);
                    assert_eq!(type_of(names, *right), &typ);
                    Some(typ)
                }
                FlatOp::NumericNegation {
                    operand,
                    operand_type,
                } => {
                    let typ = match operand_type {
                        NumericType::Int => Type::Int,
                        NumericType::Float => Type::Float,
                    };
                    assert_eq!(type_of(names, *operand), &typ);
                    Some(typ)
                }
                FlatOp::Equals {
                    left,
                    right,
                    operand_types,
                } => {
                    let typ = match operand_types {
                        EquatableType::String => Type::String,
                        EquatableType::Bool => Type::Bool,
                        EquatableType::Int => Type::Int,
                        EquatableType::Float => Type::Float,
                    };
                    assert_eq!(type_of(names, *left), &typ);
                    assert_eq!(type_of(names, *right), &typ);
                    Some(Type::Bool)
                }
                FlatOp::LessThan {
                    left,
                    right,
                    operand_types,
                }
                | FlatOp::LessThanOrEqual {
                    left,
                    right,
                    operand_types,
                } => {
                    let typ = match operand_types {
                        ComparableType::Int => Type::Int,
                        ComparableType::Float => Type::Float,
                    };
                    assert_eq!(type_of(names, *left), &typ);
                    assert_eq!(type_of(names, *right), &typ);
                    Some(Type::Bool)
                }
                FlatOp::Match(Match::Bool {
                    subject,
                    true_body,
                    false_body,
                }) => {
                    assert_eq!(type_of(names, **subject), &Type::Bool);
                    let true_type = check_block(true_body, names, binders, seen, seen_binders);
                    let false_type = check_block(false_body, names, binders, seen, seen_binders);
                    assert_eq!(true_type, false_type);
                    Some(true_type)
                }
                FlatOp::Match(Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                }) => {
                    let Type::Option(inner) = type_of(names, **subject).clone() else {
                        panic!("{} matches {subject}, which is not an Option", binding.name);
                    };
                    if let Some(binder) = some_arm_binding {
                        assert_eq!(binder.typ, *inner, "{} has the wrong type", binder.var);
                        assert!(
                            seen_binders.insert(binder.var),
                            "{} is bound twice",
                            binder.var
                        );
                        binders.push((binder.var, binder.typ.clone()));
                    }
                    let some_type = check_block(some_arm_body, names, binders, seen, seen_binders);
                    binders.truncate(binders_len);
                    let none_type = check_block(none_arm_body, names, binders, seen, seen_binders);
                    assert_eq!(some_type, none_type);
                    Some(some_type)
                }
                FlatOp::Match(Match::Enum { arms, .. }) => {
                    let mut arm_type = None;
                    for arm in arms {
                        for (_, binder) in &arm.bindings {
                            assert!(
                                seen_binders.insert(binder.var),
                                "{} is bound twice",
                                binder.var
                            );
                            binders.push((binder.var, binder.typ.clone()));
                        }
                        let typ = check_block(&arm.body, names, binders, seen, seen_binders);
                        binders.truncate(binders_len);
                        if let Some(arm_type) = &arm_type {
                            assert_eq!(arm_type, &typ);
                        }
                        arm_type = Some(typ);
                    }
                    arm_type
                }
                FlatOp::HtmlFor { var, source, body } => {
                    if let Some(binder) = var {
                        let element_type = match source {
                            FlatForSource::Array(array) => match type_of(names, *array) {
                                Type::Array(element_type) => (**element_type).clone(),
                                typ => panic!("{} loops over {array} of type {typ}", binding.name),
                            },
                            FlatForSource::RangeInclusive { .. } => Type::Int,
                        };
                        assert_eq!(
                            binder.typ, element_type,
                            "{} has the wrong type",
                            binder.var
                        );
                        assert!(
                            seen_binders.insert(binder.var),
                            "{} is bound twice",
                            binder.var
                        );
                        binders.push((binder.var, binder.typ.clone()));
                    }
                    let body_type = check_block(body, names, binders, seen, seen_binders);
                    binders.truncate(binders_len);
                    assert_eq!(body_type, Type::Html);
                    Some(Type::Html)
                }
                _ => None,
            };
            if let Some(expected) = expected {
                assert_eq!(binding.typ, expected, "{} has the wrong type", binding.name);
            }
            names.push((binding.name, binding.typ.clone()));
        }
        let result = type_of(names, block.result).clone();
        names.truncate(names_len);
        result
    }

    #[test]
    fn fuzz_random_pure_modules_lower_to_well_formed_flat() {
        arbtest::arbtest(|u| {
            let (module, _) = random_module(u);
            let mut lets = HashSet::new();
            let nodes: usize = module
                .pages
                .iter()
                .map(|page| count_nodes(&page.head, &mut lets) + count_nodes(&page.body, &mut lets))
                .sum::<usize>()
                + module
                    .functions
                    .iter()
                    .map(|function| count_nodes(&function.body, &mut lets))
                    .sum::<usize>();

            let module = pure_to_flat(module);

            // Each Pure node that computes a value became one binding.
            let mut seen = HashSet::new();
            let mut seen_binders = HashSet::new();
            for page in &module.pages {
                let mut names: Vec<(VarId, Type)> = Vec::new();
                let mut binders: Vec<(BinderId, Type)> = Vec::new();
                for param in &page.parameters {
                    assert!(
                        seen_binders.insert(param.var),
                        "{} is bound twice",
                        param.var
                    );
                    binders.push((param.var, param.typ.clone()));
                }
                let head = check_block(
                    &page.head,
                    &mut names,
                    &mut binders,
                    &mut seen,
                    &mut seen_binders,
                );
                assert_eq!(head, Type::Html);
                let body = check_block(
                    &page.body,
                    &mut names,
                    &mut binders,
                    &mut seen,
                    &mut seen_binders,
                );
                assert_eq!(body, Type::Html);
            }
            for function in &module.functions {
                let mut names: Vec<(VarId, Type)> = Vec::new();
                let mut binders: Vec<(BinderId, Type)> = Vec::new();
                for param in &function.parameters {
                    assert!(
                        seen_binders.insert(param.var),
                        "{} is bound twice",
                        param.var
                    );
                    binders.push((param.var, param.typ.clone()));
                }
                let body = check_block(
                    &function.body,
                    &mut names,
                    &mut binders,
                    &mut seen,
                    &mut seen_binders,
                );
                assert_eq!(body, function.return_type);
            }
            let bindings: usize = module
                .pages
                .iter()
                .map(|page| count_bindings(&page.head) + count_bindings(&page.body))
                .sum::<usize>()
                + module
                    .functions
                    .iter()
                    .map(|function| count_bindings(&function.body))
                    .sum::<usize>();
            assert_eq!(nodes, bindings);
            Ok(())
        });
    }
}
