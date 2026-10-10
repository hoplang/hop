use std::collections::HashMap;

use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::ir::binder_id::BinderId;
use crate::ir::document_shell::DocumentShell;
use crate::ir::flat_module::{
    FlatAttribute, FlatBlock, FlatForSource, FlatFunctionDeclaration, FlatModule, FlatOp,
};
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_match::{EnumPattern, Match};
use crate::ir::runtime::eval_error::EvalError;
/// The most function frames that may be active at once. A call that would
/// open one more fails with a recursion limit error, so a function that
/// never stops calling itself reports an error instead of overflowing the
/// stack.
pub const MAX_CALL_DEPTH: usize = 12;
use crate::ir::runtime::html_node::{HtmlAttribute, HtmlNode, write_html};
use crate::ir::runtime::value::Value;
use crate::ir::var_id::VarId;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::type_name::TypeName;

/// The values of the bindings made so far in one function frame. A block
/// removes its bindings when it ends, so a loop body binds them afresh on
/// every iteration.
type Names = HashMap<VarId, Value>;

/// The values of the binders in scope in one function frame: its
/// parameters, and the binders of the loops and arms being evaluated. An
/// arm or a loop removes its binders when it ends.
type Binders = HashMap<BinderId, Value>;

pub fn evaluate_page(
    module: &FlatModule,
    page_name: &TypeName,
    mut args: HashMap<AttributeName, Value>,
    shell: Option<&DocumentShell>,
) -> Result<String, EvalError> {
    let page = module
        .pages
        .iter()
        .find(|page| &page.name == page_name)
        .ok_or_else(|| EvalError::PageNotFound {
            page: page_name.clone(),
        })?;

    let mut names = Names::new();
    let mut binders = Binders::new();
    for param in &page.parameters {
        if let Some(value) = args.remove(param.name()) {
            binders.insert(param.var, value);
        } else {
            return Err(EvalError::MissingParameter {
                page: page.name.clone(),
                param: param.name().clone(),
            });
        }
    }

    let head =
        evaluate_block(&page.head, &mut names, &mut binders, &module.functions, 0)?.unwrap_html();
    let body =
        evaluate_block(&page.body, &mut names, &mut binders, &module.functions, 0)?.unwrap_html();

    let mut html = String::new();
    match shell {
        Some(shell) => {
            html.push_str(shell.before_head);
            write_html(&head, &mut html);
            html.push_str(&shell.after_head);
            write_html(&body, &mut html);
            html.push_str(shell.after_body);
        }
        None => {
            write_html(&head, &mut html);
            write_html(&body, &mut html);
        }
    }
    Ok(html)
}

/// Evaluate a function on its arguments, one for each parameter in the
/// order they are declared, with `depth` function frames already active. A
/// call from outside any function has depth zero.
pub fn evaluate_function(
    functions: &[FlatFunctionDeclaration],
    function: &IrFunction,
    args: Vec<Value>,
    depth: usize,
) -> Result<Value, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::RecursionLimit {
            function: function.clone(),
            limit: MAX_CALL_DEPTH,
        });
    }
    let decl = functions
        .iter()
        .find(|decl| decl.function.id == function.id)
        .ok_or_else(|| EvalError::FunctionNotFound {
            function: function.clone(),
        })?;
    if args.len() != decl.parameters.len() {
        return Err(EvalError::ArgumentCount {
            function: function.clone(),
            expected: decl.parameters.len(),
            found: args.len(),
        });
    }
    // A recursive call binds the same names as the frame it was called
    // from, so each frame has names of its own.
    let mut names = Names::new();
    let mut binders = Binders::new();
    for (param, value) in decl.parameters.iter().zip(args) {
        binders.insert(param.var, value);
    }
    evaluate_block(&decl.body, &mut names, &mut binders, functions, depth + 1)
}

/// Evaluate the bindings in order and produce the result. The names bound
/// by the block are gone again when it returns, and so is anything bound
/// before an error.
fn evaluate_block(
    block: &FlatBlock,
    names: &mut Names,
    binders: &mut Binders,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
) -> Result<Value, EvalError> {
    for binding in &block.bindings {
        let value = evaluate_op(&binding.op, names, binders, functions, depth)?;
        names.insert(binding.name, value);
    }
    let result = names[&block.result].clone();
    for binding in &block.bindings {
        names.remove(&binding.name);
    }
    Ok(result)
}

fn evaluate_op(
    op: &FlatOp,
    names: &mut Names,
    binders: &mut Binders,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
) -> Result<Value, EvalError> {
    Ok(match op {
        FlatOp::Read(binder) => binders[binder].clone(),

        FlatOp::StringLiteral(value) => Value::String(value.to_string()),

        FlatOp::IntLiteral(value) => Value::Int(*value),

        FlatOp::FloatLiteral(value) => Value::Float(*value),

        FlatOp::BoolLiteral(value) => Value::Bool(*value),

        FlatOp::HtmlText(content) => Value::Html(vec![HtmlNode::Text(content.to_string())]),

        FlatOp::FieldAccess { record, field } => {
            let record = names[record].clone().unwrap_record();
            let (_, value) = record
                .into_iter()
                .find(|(name, _)| name == field)
                .unwrap_or_else(|| panic!("Field '{}' not found in record", field));
            value
        }

        FlatOp::TupleIndex { tuple, index } => names[tuple]
            .clone()
            .unwrap_tuple()
            .into_iter()
            .nth(*index)
            .unwrap_or_else(|| panic!("Index {} is out of range for the tuple", index)),

        FlatOp::Array(elements) => {
            Value::Array(elements.iter().map(|e| names[e].clone()).collect())
        }

        FlatOp::Tuple(elements) => {
            Value::Tuple(elements.iter().map(|e| names[e].clone()).collect())
        }

        FlatOp::Record { fields } => Value::Record(
            fields
                .iter()
                .map(|(name, value)| (name.clone(), names[value].clone()))
                .collect(),
        ),

        FlatOp::Enum {
            variant_name,
            fields,
        } => Value::Enum {
            variant_name: variant_name.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| (name.clone(), names[value].clone()))
                .collect(),
        },

        FlatOp::Option(value) => Value::Option(value.map(|inner| Box::new(names[&inner].clone()))),

        FlatOp::StringConcat(parts) => {
            let mut result = String::new();
            for part in parts {
                result.push_str(&names[part].clone().unwrap_string());
            }
            Value::String(result)
        }

        FlatOp::NumericAdd {
            left,
            right,
            operand_types,
        } => {
            let left = names[left].clone();
            let right = names[right].clone();
            match operand_types {
                NumericType::Int => Value::Int(left.unwrap_int().wrapping_add(right.unwrap_int())),
                NumericType::Float => Value::Float(left.unwrap_float() + right.unwrap_float()),
            }
        }

        FlatOp::NumericSubtract {
            left,
            right,
            operand_types,
        } => {
            let left = names[left].clone();
            let right = names[right].clone();
            match operand_types {
                NumericType::Int => Value::Int(left.unwrap_int().wrapping_sub(right.unwrap_int())),
                NumericType::Float => Value::Float(left.unwrap_float() - right.unwrap_float()),
            }
        }

        FlatOp::NumericMultiply {
            left,
            right,
            operand_types,
        } => {
            let left = names[left].clone();
            let right = names[right].clone();
            match operand_types {
                NumericType::Int => Value::Int(left.unwrap_int().wrapping_mul(right.unwrap_int())),
                NumericType::Float => Value::Float(left.unwrap_float() * right.unwrap_float()),
            }
        }

        FlatOp::NumericNegation {
            operand,
            operand_type,
        } => {
            let operand = names[operand].clone();
            match operand_type {
                NumericType::Int => Value::Int(operand.unwrap_int().wrapping_neg()),
                NumericType::Float => Value::Float(-operand.unwrap_float()),
            }
        }

        FlatOp::BoolNegation(operand) => Value::Bool(!names[operand].clone().unwrap_bool()),

        FlatOp::Equals {
            left,
            right,
            operand_types,
        } => {
            let left = names[left].clone();
            let right = names[right].clone();
            match operand_types {
                EquatableType::Bool => Value::Bool(left.unwrap_bool() == right.unwrap_bool()),
                EquatableType::String => Value::Bool(left.unwrap_string() == right.unwrap_string()),
                EquatableType::Int => Value::Bool(left.unwrap_int() == right.unwrap_int()),
                EquatableType::Float => Value::Bool(left.unwrap_float() == right.unwrap_float()),
            }
        }

        FlatOp::LessThan {
            left,
            right,
            operand_types,
        } => {
            let left = names[left].clone();
            let right = names[right].clone();
            match operand_types {
                ComparableType::Int => Value::Bool(left.unwrap_int() < right.unwrap_int()),
                ComparableType::Float => Value::Bool(left.unwrap_float() < right.unwrap_float()),
            }
        }

        FlatOp::LessThanOrEqual {
            left,
            right,
            operand_types,
        } => {
            let left = names[left].clone();
            let right = names[right].clone();
            match operand_types {
                ComparableType::Int => Value::Bool(left.unwrap_int() <= right.unwrap_int()),
                ComparableType::Float => Value::Bool(left.unwrap_float() <= right.unwrap_float()),
            }
        }

        FlatOp::ArrayLength(array) => Value::Int(names[array].clone().unwrap_array().len() as i32),

        FlatOp::ArrayIsEmpty(array) => Value::Bool(names[array].clone().unwrap_array().is_empty()),

        FlatOp::StringIsEmpty(string) => {
            Value::Bool(names[string].clone().unwrap_string().is_empty())
        }

        FlatOp::OptionIsSome(option) => {
            Value::Bool(names[option].clone().unwrap_option().is_some())
        }

        FlatOp::OptionIsNone(option) => {
            Value::Bool(names[option].clone().unwrap_option().is_none())
        }

        FlatOp::IntToString(value) => Value::String(names[value].clone().unwrap_int().to_string()),

        FlatOp::FloatToInt(value) => Value::Int(names[value].clone().unwrap_float() as i32),

        FlatOp::IntToFloat(value) => Value::Float(names[value].clone().unwrap_int() as f64),

        FlatOp::HtmlEscape(string) => Value::Html(vec![HtmlNode::Escape(
            names[string].clone().unwrap_string(),
        )]),

        FlatOp::HtmlConcat(parts) => {
            let mut nodes = Vec::new();
            for part in parts {
                nodes.extend(names[part].clone().unwrap_html());
            }
            Value::Html(nodes)
        }

        FlatOp::HtmlElement {
            element,
            attributes,
            children,
        } => {
            let mut rendered = Vec::new();
            for attribute in attributes {
                match attribute {
                    FlatAttribute::Value { name, value } => rendered.push(HtmlAttribute {
                        name: name.clone(),
                        value: Some(names[value].clone().unwrap_string()),
                    }),
                    FlatAttribute::Presence { name, present } => {
                        if names[present].clone().unwrap_bool() {
                            rendered.push(HtmlAttribute {
                                name: name.clone(),
                                value: None,
                            });
                        }
                    }
                }
            }
            let children = if element.is_void() {
                Vec::new()
            } else {
                names[children].clone().unwrap_html()
            };
            Value::Html(vec![HtmlNode::Element {
                element: element.clone(),
                attributes: rendered,
                children,
            }])
        }

        FlatOp::Call { function, args } => {
            let values = args.iter().map(|arg| names[arg].clone()).collect();
            evaluate_function(functions, function, values, depth)?
        }

        FlatOp::Match(Match::Enum { subject, arms }) => {
            let (variant_name, fields) = names[subject].clone().unwrap_enum();
            let mut matching_arms = arms.iter().filter(|arm| {
                let EnumPattern::Variant {
                    variant_name: pattern_variant,
                    ..
                } = &arm.pattern;
                variant_name == *pattern_variant
            });
            let arm = matching_arms
                .next()
                .unwrap_or_else(|| panic!("No matching arm found for variant '{}'", variant_name));
            assert!(
                matching_arms.next().is_none(),
                "Multiple matching arms found for variant '{}'",
                variant_name
            );
            for (field_name, binder) in &arm.bindings {
                let (_, field) = fields
                    .iter()
                    .find(|(name, _)| name == field_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Field '{}' not found in enum variant '{}'",
                            field_name, variant_name
                        )
                    });
                binders.insert(binder.var, field.clone());
            }
            let result = evaluate_block(&arm.body, names, binders, functions, depth);
            for (_, binder) in &arm.bindings {
                binders.remove(&binder.var);
            }
            result?
        }

        FlatOp::Match(Match::Bool {
            subject,
            true_body,
            false_body,
        }) => {
            if names[subject].clone().unwrap_bool() {
                evaluate_block(true_body, names, binders, functions, depth)?
            } else {
                evaluate_block(false_body, names, binders, functions, depth)?
            }
        }

        FlatOp::Match(Match::Option {
            subject,
            some_arm_binding,
            some_arm_body,
            none_arm_body,
        }) => match names[subject].clone().unwrap_option() {
            Some(inner) => {
                if let Some(binder) = some_arm_binding {
                    binders.insert(binder.var, *inner);
                }
                let result = evaluate_block(some_arm_body, names, binders, functions, depth);
                if let Some(binder) = some_arm_binding {
                    binders.remove(&binder.var);
                }
                result?
            }
            None => evaluate_block(none_arm_body, names, binders, functions, depth)?,
        },

        FlatOp::HtmlFor { var, source, body } => {
            let items = match source {
                FlatForSource::Array(array) => names[array].clone().unwrap_array(),
                FlatForSource::RangeInclusive { start, end } => {
                    let start = names[start].clone().unwrap_int();
                    let end = names[end].clone().unwrap_int();
                    (start..=end).map(Value::Int).collect()
                }
            };
            let mut nodes = Vec::new();
            for item in items {
                if let Some(binder) = var {
                    binders.insert(binder.var, item);
                }
                let result = evaluate_block(body, names, binders, functions, depth);
                if let Some(binder) = var {
                    binders.remove(&binder.var);
                }
                nodes.extend(result?.unwrap_html());
            }
            Value::Html(nodes)
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::pure_module::PureModule;
    use crate::ir::pure_module_builder::PureModuleBuilder;
    use crate::ir::pure_module_generator::random_module;
    use crate::ir::pure_to_flat::pure_to_flat;
    use crate::ir::runtime::pure_evaluator;
    use crate::ir::runtime::random::random_value;
    use crate::symbols::field_name::FieldName;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_pure_modules_evaluate_identically_after_lowering() {
        arbtest::arbtest(|u| {
            let (module, registry) = random_module(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);

            let page_args: Vec<(TypeName, HashMap<AttributeName, Value>)> = module
                .pages
                .iter()
                .map(|page| {
                    let args = page
                        .parameters
                        .iter()
                        .map(|p| {
                            (
                                p.name().clone(),
                                random_value(&mut rng, &p.typ, None, &registry),
                            )
                        })
                        .collect();
                    (page.name.clone(), args)
                })
                .collect();

            let before_module = module.to_string();
            let before: Vec<Result<String, EvalError>> = page_args
                .iter()
                .map(|(page_name, args)| {
                    pure_evaluator::evaluate_page(&module, page_name, args.clone(), None)
                })
                .collect();

            let module = pure_to_flat(module);

            // Lowering keeps the evaluation order and the call depth, so a
            // page that hits the depth limit in Pure hits it in the Flat IR too.
            for ((page_name, args), before) in page_args.iter().zip(before) {
                let after = evaluate_page(&module, page_name, args.clone(), None);
                match (before, after) {
                    (Ok(before), Ok(after)) => assert_eq!(
                        before, after,
                        "page {page_name}\n-- pure --\n{before_module}\n-- flat --\n{module}"
                    ),
                    (
                        Err(EvalError::RecursionLimit { .. }),
                        Err(EvalError::RecursionLimit { .. }),
                    ) => {}
                    (before, after) => panic!(
                        "page {page_name}: pure {before:?}, flat {after:?}\n-- pure --\n{before_module}\n-- flat --\n{module}"
                    ),
                }
            }
            Ok(())
        });
    }

    fn check(module: PureModule, args: Vec<(&str, Value)>, expected: Expect) {
        let module = pure_to_flat(module);
        let before = module.to_string();
        let args: HashMap<AttributeName, Value> = args
            .into_iter()
            .map(|(name, value)| (AttributeName::parse(name).unwrap(), value))
            .collect();
        let page_name = module.pages[0].name.clone();
        let after =
            evaluate_page(&module, &page_name, args, None).expect("Evaluation should succeed");
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}\n"));
    }

    #[test]
    fn should_bind_loop_names_afresh_on_each_iteration() {
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
            vec![(
                "items",
                Value::Array(vec![
                    Value::String("a<b".to_string()),
                    Value::String("c".to_string()),
                ]),
            )],
            expect![[r#"
                -- before --
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

                -- after --
                <ul><li>a&lt;b</li><li>c</li></ul>
            "#]],
        );
    }

    #[test]
    fn should_skip_the_right_operand_of_a_decided_and() {
        check(
            PureModuleBuilder::new()
                .function("diverge", [], "Bool", |t| t.call("diverge", vec![]))
                .page("Test", [("flag", "Bool")], |t| {
                    t.escape(t.bool_match_expr(
                        t.and(t.var("flag"), t.call("diverge", vec![])),
                        t.str("yes"),
                        t.str("no"),
                    ))
                })
                .build(),
            vec![("flag", Value::Bool(false))],
            expect![[r#"
                -- before --
                fn diverge@f0() -> Bool {
                  let v8: Bool = call diverge@f0()
                  v8
                }
                page Test(flag@b0: Bool) {
                  let v1: Bool = b0
                  let v3: Bool = match v1 {
                    true => {
                      let v2: Bool = call diverge@f0()
                      v2
                    }
                    false => {
                      v1
                    }
                  }
                  let v6: String = match v3 {
                    true => {
                      let v4: String = "yes"
                      v4
                    }
                    false => {
                      let v5: String = "no"
                      v5
                    }
                  }
                  let v7: Html = escape(v6)
                  v7
                }

                -- after --
                no
            "#]],
        );
    }

    #[test]
    fn should_select_an_enum_arm_and_bind_its_fields() {
        check(
            PureModuleBuilder::new()
                .enum_(
                    "Shape",
                    [("Dot", vec![]), ("Circle", vec![("radius", "Int")])],
                )
                .page("Test", [("shape", "Shape")], |t| {
                    t.escape(t.enum_match_expr(t.var("shape"), |arms| {
                        arms.arm("Dot", |t| t.str("dot"));
                        arms.arm_bound("Circle", [("radius", "r")], |t| {
                            t.int_to_string(t.var("r"))
                        });
                    }))
                })
                .build(),
            vec![(
                "shape",
                Value::Enum {
                    variant_name: TypeName::parse("Circle").unwrap(),
                    fields: vec![(FieldName::parse("radius").unwrap(), Value::Int(7))],
                },
            )],
            expect![[r#"
                -- before --
                page Test(shape@b0: Shape) {
                  let v1: Shape = b0
                  let v5: String = match v1 {
                    Shape::Dot => {
                      let v2: String = "dot"
                      v2
                    }
                    Shape::Circle {radius@b1: Int} => {
                      let v3: Int = b1
                      let v4: String = v3.to_string()
                      v4
                    }
                  }
                  let v6: Html = escape(v5)
                  v6
                }

                -- after --
                7
            "#]],
        );
    }
}
