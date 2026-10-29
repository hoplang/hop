use std::collections::HashMap;
use std::rc::Rc;

use rpds::HashTrieMap;

use crate::ir::document_shell::DocumentShell;
use crate::ir::flat_program::{
    FlatDataBinding, FlatDataBlock, FlatDataOp, FlatFunctionDeclaration, FlatHtmlBlock, FlatHtmlOp,
    FlatProgram,
};
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_for::IrFor;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_page::IrPage;
use crate::ir::runtime::bounded_vec::BoundedVec;
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::html_node::{Html, HtmlAttribute, HtmlNode};
use crate::ir::runtime::value::{MAX_ARRAY_LENGTH, MAX_STRING_LENGTH, Value};
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::type_name::TypeName;

/// The most function frames that may be active at once. A call that would
/// open one more fails with a call depth limit error.
pub const MAX_CALL_DEPTH: usize = 12;

/// The most ops one evaluation of a function from outside may evaluate,
/// data and Html alike.
pub const MAX_OP_COUNT: usize = 5_000;

const ARRAY_LENGTH_LIMIT_ERROR: EvalError = EvalError::ArrayLengthLimit {
    limit: MAX_ARRAY_LENGTH,
};

const STRING_LENGTH_LIMIT_ERROR: EvalError = EvalError::StringLengthLimit {
    limit: MAX_STRING_LENGTH,
};

/// Evaluate a page from the entrypoint functions it points at, each called with
/// the arguments named by the page's parameters. With a shell, the page
/// renders as a whole document, the shell around its head and its body.
pub fn evaluate_page(
    program: &FlatProgram,
    pages: &[IrPage],
    page_name: &TypeName,
    args: HashMap<AttributeName, Value>,
    shell: Option<&DocumentShell>,
) -> Result<String, EvalError> {
    let page = pages
        .iter()
        .find(|page| &page.name == page_name)
        .ok_or_else(|| EvalError::PageNotFound {
            page: page_name.clone(),
        })?;

    let head = match &page.head {
        Some(head) => evaluate_entrypoint(program, head, args.clone())?.unwrap_html(),
        None => Html::empty(),
    };
    let body = evaluate_entrypoint(program, &page.body, args)?.unwrap_html();

    let mut html = String::new();
    match shell {
        Some(shell) => {
            html.push_str(shell.before_head);
            head.write(&mut html);
            html.push_str(&shell.after_head);
            body.write(&mut html);
            html.push_str(shell.after_body);
        }
        None => {
            head.write(&mut html);
            body.write(&mut html);
        }
    }
    Ok(html)
}

/// Evaluate a function called from outside the program, on the arguments
/// named by its parameters.
pub fn evaluate_entrypoint(
    program: &FlatProgram,
    function: &IrFunction,
    mut args: HashMap<AttributeName, Value>,
) -> Result<Value, EvalError> {
    let decl = program
        .functions
        .iter()
        .find(|decl| decl.function().id == function.id)
        .unwrap();
    let mut values = Vec::with_capacity(decl.parameters().len());
    for param in decl.parameters() {
        let value = args
            .remove(param.name())
            .ok_or_else(|| EvalError::MissingParameter {
                function: function.clone(),
                param: param.name().clone(),
            })?;
        values.push(value);
    }
    evaluate_function(&program.functions, function, values, 0, &mut 0)
}

/// Evaluate a function on its arguments, one for each parameter in the
/// order they are declared, with `depth` function frames already active. A
/// call from outside any function has depth zero.
fn evaluate_function(
    functions: &[FlatFunctionDeclaration],
    function: &IrFunction,
    args: Vec<Value>,
    depth: usize,
    op_count: &mut usize,
) -> Result<Value, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::CallDepthLimit {
            function: function.clone(),
            limit: MAX_CALL_DEPTH,
        });
    }
    let decl = functions
        .iter()
        .find(|decl| decl.function().id == function.id)
        .unwrap();
    match decl {
        FlatFunctionDeclaration::Data(decl) => {
            let vars = decl.parameters.iter().map(|param| param.binder.var);
            evaluate_data_block(
                &decl.body,
                &vars.zip(args).collect(),
                functions,
                depth + 1,
                op_count,
            )
        }
        FlatFunctionDeclaration::Html(decl) => {
            let vars = decl.parameters.iter().map(|param| param.binder.var);
            evaluate_html_block(
                &decl.body,
                &vars.zip(args).collect(),
                functions,
                depth + 1,
                op_count,
            )
        }
    }
}

/// Evaluate the bindings in order and produce the result. The block extends
/// a copy of `env` and leaves `env` as it is, so its bindings are out of
/// scope again when it returns.
fn evaluate_data_block(
    block: &FlatDataBlock,
    env: &HashTrieMap<ValueId, Value>,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
    op_count: &mut usize,
) -> Result<Value, EvalError> {
    let mut env = env.clone();
    evaluate_data_bindings(&block.bindings, &mut env, functions, depth, op_count)?;
    Ok(env[&block.result].clone())
}

/// Evaluate the values and then the Html in order and produce the result,
/// leaving `env` as it is like `evaluate_data_block`.
fn evaluate_html_block(
    block: &FlatHtmlBlock,
    env: &HashTrieMap<ValueId, Value>,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
    op_count: &mut usize,
) -> Result<Value, EvalError> {
    let mut env = env.clone();
    evaluate_data_bindings(&block.bindings, &mut env, functions, depth, op_count)?;
    for binding in &block.html {
        let value = evaluate_html_op(&binding.op, &env, functions, depth, op_count)?;
        env.insert_mut(binding.binder.var, value);
    }
    Ok(env[&block.result].clone())
}

fn evaluate_data_bindings(
    bindings: &[FlatDataBinding],
    env: &mut HashTrieMap<ValueId, Value>,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
    op_count: &mut usize,
) -> Result<(), EvalError> {
    for binding in bindings {
        *op_count += 1;
        if *op_count > MAX_OP_COUNT {
            return Err(EvalError::OpCountLimit {
                limit: MAX_OP_COUNT,
            });
        }
        let value = match &binding.op {
            FlatDataOp::Data(op) => evaluate_data_op(op, env)?,
            FlatDataOp::Call(function, args) => {
                evaluate_call(function, args, env, functions, depth, op_count)?
            }
            FlatDataOp::Match(match_) => {
                let (body, env) = enter_arm(match_, env);
                evaluate_data_block(body, &env, functions, depth, op_count)?
            }
        };
        env.insert_mut(binding.binder.var, value);
    }
    Ok(())
}

fn evaluate_call(
    function: &IrFunction,
    args: &[ValueId],
    env: &HashTrieMap<ValueId, Value>,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
    op_count: &mut usize,
) -> Result<Value, EvalError> {
    let values = args.iter().map(|arg| env[arg].clone()).collect();
    evaluate_function(functions, function, values, depth, op_count)
}

/// The body of the first arm the subject matches, with `env` extended by
/// the names its pattern binds.
fn enter_arm<'a, B>(
    match_: &'a IrMatch<B>,
    env: &HashTrieMap<ValueId, Value>,
) -> (&'a B, HashTrieMap<ValueId, Value>) {
    let subject = &env[&match_.subject];
    let (arm, bound) = match_
        .arms
        .iter()
        .find_map(|arm| {
            let mut bound = Vec::new();
            subject
                .matches(&arm.pattern, &mut bound)
                .then_some((arm, bound))
        })
        .unwrap();
    let mut env = env.clone();
    for (name, value) in bound {
        env.insert_mut(name, value);
    }
    (&arm.body, env)
}

fn evaluate_html_op(
    op: &FlatHtmlOp,
    env: &HashTrieMap<ValueId, Value>,
    functions: &[FlatFunctionDeclaration],
    depth: usize,
    op_count: &mut usize,
) -> Result<Value, EvalError> {
    *op_count += 1;
    if *op_count > MAX_OP_COUNT {
        return Err(EvalError::OpCountLimit {
            limit: MAX_OP_COUNT,
        });
    }
    Ok(match op {
        FlatHtmlOp::Text(content) => Value::Html(Html::new(HtmlNode::Text(content.to_string()))?),

        FlatHtmlOp::Escape(string) => Value::Html(Html::new(HtmlNode::Escape(
            env[string].clone().unwrap_string(),
        ))?),

        FlatHtmlOp::Concat(parts) => Value::Html(Html::new(HtmlNode::Concat(
            parts
                .iter()
                .map(|part| env[part].clone().unwrap_html())
                .collect(),
        ))?),

        FlatHtmlOp::Element(IrHtmlElement {
            element,
            attributes,
            children,
        }) => {
            let mut rendered = Vec::new();
            for (name, attribute) in attributes {
                match attribute {
                    IrAttribute::Value(value) => rendered.push(HtmlAttribute {
                        name: name.clone(),
                        value: Some(env[value].clone().unwrap_string()),
                    }),
                    IrAttribute::Presence(present) => {
                        if env[present].as_bool() {
                            rendered.push(HtmlAttribute {
                                name: name.clone(),
                                value: None,
                            });
                        }
                    }
                }
            }
            let children = if element.is_void() {
                Html::empty()
            } else {
                env[children].clone().unwrap_html()
            };
            Value::Html(Html::new(HtmlNode::Element {
                element: element.clone(),
                attributes: rendered,
                children,
            })?)
        }

        FlatHtmlOp::Call(function, args) => {
            evaluate_call(function, args, env, functions, depth, op_count)?
        }

        FlatHtmlOp::Match(match_) => {
            let (body, env) = enter_arm(match_, env);
            evaluate_html_block(body, &env, functions, depth, op_count)?
        }

        FlatHtmlOp::For(IrFor { var, array, body }) => {
            let mut parts = Vec::new();
            for item in env[array].as_array() {
                let mut env = env.clone();
                if let Some(binder) = var {
                    env.insert_mut(binder.var, item.clone());
                }
                let part = evaluate_html_block(body, &env, functions, depth, op_count)?;
                parts.push(part.unwrap_html());
            }
            Value::Html(Html::new(HtmlNode::Concat(parts))?)
        }
    })
}

fn evaluate_data_op(op: &IrDataOp, env: &HashTrieMap<ValueId, Value>) -> Result<Value, EvalError> {
    Ok(match op {
        IrDataOp::StringLiteral(value) => Value::String(value.as_str().into()),
        IrDataOp::IntLiteral(value) => Value::Int(*value),
        IrDataOp::FloatLiteral(value) => Value::Float(*value),
        IrDataOp::BoolLiteral(value) => Value::Bool(*value),
        IrDataOp::FieldAccess(record, field) => {
            let record = env[record].as_record();
            let (_, value) = record.iter().find(|(name, _)| name == field).unwrap();
            value.clone()
        }
        IrDataOp::TupleIndex(tuple, index) => env[tuple].as_tuple()[*index].clone(),
        IrDataOp::Array(elements) => Value::Array(Rc::new(
            BoundedVec::new(elements.iter().map(|e| env[e].clone()))
                .ok_or(ARRAY_LENGTH_LIMIT_ERROR)?,
        )),
        IrDataOp::Tuple(elements) => {
            Value::Tuple(elements.iter().map(|e| env[e].clone()).collect())
        }
        IrDataOp::Record(fields) => Value::Record(
            fields
                .iter()
                .map(|(name, value)| (name.clone(), env[value].clone()))
                .collect(),
        ),
        IrDataOp::Enum(variant_name, fields) => Value::Enum(
            variant_name.clone(),
            fields
                .iter()
                .map(|(name, value)| (name.clone(), env[value].clone()))
                .collect(),
        ),
        IrDataOp::OptionSome(inner) => Value::Option(Some(Rc::new(env[inner].clone()))),
        IrDataOp::OptionNone => Value::Option(None),
        IrDataOp::StringConcat(parts) => {
            let length: usize = parts.iter().map(|part| env[part].as_str().len()).sum();
            if length > MAX_STRING_LENGTH {
                return Err(STRING_LENGTH_LIMIT_ERROR);
            }
            Value::String(
                parts
                    .iter()
                    .map(|part| env[part].as_str())
                    .collect::<String>()
                    .into(),
            )
        }
        IrDataOp::NumericAdd(left, right) => match &env[left] {
            Value::Int(left) => Value::Int(left.wrapping_add(env[right].as_int())),
            left => Value::Float(left.as_float() + env[right].as_float()),
        },
        IrDataOp::NumericSubtract(left, right) => match &env[left] {
            Value::Int(left) => Value::Int(left.wrapping_sub(env[right].as_int())),
            left => Value::Float(left.as_float() - env[right].as_float()),
        },
        IrDataOp::NumericMultiply(left, right) => match &env[left] {
            Value::Int(left) => Value::Int(left.wrapping_mul(env[right].as_int())),
            left => Value::Float(left.as_float() * env[right].as_float()),
        },
        IrDataOp::Equals(left, right) => {
            let right = &env[right];
            match &env[left] {
                Value::Bool(left) => Value::Bool(*left == right.as_bool()),
                Value::String(left) => Value::Bool(**left == *right.as_str()),
                Value::Int(left) => Value::Bool(*left == right.as_int()),
                left => Value::Bool(left.as_float() == right.as_float()),
            }
        }
        IrDataOp::LessThan(left, right) => match &env[left] {
            Value::Int(left) => Value::Bool(*left < env[right].as_int()),
            left => Value::Bool(left.as_float() < env[right].as_float()),
        },
        IrDataOp::LessThanOrEqual(left, right) => match &env[left] {
            Value::Int(left) => Value::Bool(*left <= env[right].as_int()),
            left => Value::Bool(left.as_float() <= env[right].as_float()),
        },
        IrDataOp::ArrayFromRange(left, right) => Value::Array(Rc::new(
            BoundedVec::new((env[left].as_int()..=env[right].as_int()).map(Value::Int))
                .ok_or(ARRAY_LENGTH_LIMIT_ERROR)?,
        )),
        IrDataOp::NumericNegation(operand) => match &env[operand] {
            Value::Int(value) => Value::Int(value.wrapping_neg()),
            operand => Value::Float(-operand.as_float()),
        },
        IrDataOp::BoolNegation(operand) => Value::Bool(!env[operand].as_bool()),
        IrDataOp::ArrayLength(operand) => Value::Int(env[operand].as_array().len() as i32),
        IrDataOp::ArrayIsEmpty(operand) => Value::Bool(env[operand].as_array().is_empty()),
        IrDataOp::StringIsEmpty(operand) => Value::Bool(env[operand].as_str().is_empty()),
        IrDataOp::OptionIsSome(operand) => Value::Bool(env[operand].as_option().is_some()),
        IrDataOp::OptionIsNone(operand) => Value::Bool(env[operand].as_option().is_none()),
        IrDataOp::IntToString(operand) => Value::String(env[operand].as_int().to_string().into()),
        IrDataOp::FloatToInt(operand) => Value::Int(env[operand].as_float() as i32),
        IrDataOp::IntToFloat(operand) => Value::Float(env[operand].as_int() as f64),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::typing::{Ty, TypeRegistry, TypeRegistryBuilder};
    use crate::ir::flat_program::FlatProgram;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::runtime::value_builder::ValueBuilder;
    use expect_test::{Expect, expect};
    use rand::{SeedableRng, rngs::SmallRng};

    #[test]
    fn fuzz_random_programs_evaluate_without_panicking() {
        arbtest::arbtest(|u| {
            let (program, registry) = random_program(u);
            let mut rng = SmallRng::seed_from_u64(u.arbitrary()?);
            for (function, args) in random_entrypoint_args(&program, &mut rng, &registry) {
                match evaluate_entrypoint(&program, &function, args) {
                    Ok(_)
                    | Err(
                        EvalError::CallDepthLimit { .. }
                        | EvalError::StringLengthLimit { .. }
                        | EvalError::HtmlSizeLimit { .. }
                        | EvalError::OpCountLimit { .. },
                    ) => {}
                    Err(error) => panic!("{error}"),
                }
            }
            Ok(())
        });
    }

    fn check(
        (program, types): (FlatProgram, TypeRegistry),
        args: impl FnOnce(&ValueBuilder<'_>) -> Vec<(&'static str, Value)>,
        expected: Expect,
    ) {
        let before = program.to_string();
        let args: HashMap<AttributeName, Value> = args(&ValueBuilder::new(&types))
            .into_iter()
            .map(|(name, value)| (AttributeName::parse(name).unwrap(), value))
            .collect();
        let entrypoint = program
            .functions
            .iter()
            .find(|decl| match decl {
                FlatFunctionDeclaration::Data(decl) => decl.entrypoint,
                FlatFunctionDeclaration::Html(decl) => decl.entrypoint,
            })
            .expect("the program has an entrypoint function")
            .function()
            .clone();
        let after = evaluate_entrypoint(&program, &entrypoint, args)
            .expect("Evaluation should succeed")
            .into_markup();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}\n"));
    }

    #[test]
    fn should_bind_loop_names_afresh_on_each_iteration() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Items", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    t.element(
                        "ul",
                        [],
                        t.html_for(t.param("items"), |t, item| {
                            t.element("li", vec![], t.escape(item))
                        }),
                    )
                })
                .build_with_registry(),
            |v| vec![("items", v.array(vec![v.str("a<b"), v.str("c")]))],
            expect![[r#"
                -- before --
                entrypoint fn Items@f0(items@v0: Array[String]) -> Html {
                  let v4: Html = for v1: String in v0 {
                    let v2: Html = escape(v1)
                    let v3: Html = element("li", {}, v2)
                    v3
                  }
                  let v5: Html = element("ul", {}, v4)
                  v5
                }

                -- after --
                <ul><li>a&lt;b</li><li>c</li></ul>
            "#]],
        );
    }

    #[test]
    fn should_skip_the_right_operand_of_a_decided_and() {
        check(
            FlatProgramBuilder::new()
                .function("diverge", [], Ty::Bool, |t| t.call("diverge", []))
                .entrypoint("Test", [("flag", Ty::Bool)], Ty::Html, |t| {
                    let and = t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.call("diverge", []));
                        m.arm(Pat::False, |t, _| t.bool(false));
                    });
                    t.escape(t.match_(and, |m| {
                        m.arm(Pat::True, |t, _| t.str("yes"));
                        m.arm(Pat::False, |t, _| t.str("no"));
                    }))
                })
                .build_with_registry(),
            |v| vec![("flag", v.bool(false))],
            expect![[r#"
                -- before --
                fn diverge@f0() -> Bool {
                  let v0: Bool = diverge@f0()
                  v0
                }
                entrypoint fn Test@f1(flag@v1: Bool) -> Html {
                  let v4: Bool = match v1 {
                    true => {
                      let v2: Bool = diverge@f0()
                      v2
                    }
                    false => {
                      let v3: Bool = false
                      v3
                    }
                  }
                  let v7: String = match v4 {
                    true => {
                      let v5: String = "yes"
                      v5
                    }
                    false => {
                      let v6: String = "no"
                      v6
                    }
                  }
                  let v8: Html = escape(v7)
                  v8
                }

                -- after --
                no
            "#]],
        );
    }

    #[test]
    fn should_select_an_enum_arm_and_bind_its_fields() {
        let types = TypeRegistryBuilder::new()
            .enum_(
                "Shape",
                [("Dot", vec![]), ("Circle", vec![("radius", Ty::Int)])],
            )
            .build();
        check(
            FlatProgramBuilder::with_registry(types)
                .entrypoint("Test", [("shape", Ty::named("Shape"))], Ty::Html, |t| {
                    t.escape(t.match_(t.param("shape"), |m| {
                        m.arm(Pat::variant("Dot", []), |t, _| t.str("dot"));
                        m.arm(
                            Pat::variant("Circle", [("radius", Pat::bind("r"))]),
                            |t, arm| t.int_to_string(arm.binding("r")),
                        );
                    }))
                })
                .build_with_registry(),
            |v| {
                vec![(
                    "shape",
                    v.enum_variant_with_fields("Shape", "Circle", vec![("radius", v.int(7))]),
                )]
            },
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(shape@v0: Shape) -> Html {
                  let v4: String = match v0 {
                    Shape::Dot => {
                      let v1: String = "dot"
                      v1
                    }
                    Shape::Circle {radius@v2: Int} => {
                      let v3: String = v2.to_string()
                      v3
                    }
                  }
                  let v5: Html = escape(v4)
                  v5
                }

                -- after --
                7
            "#]],
        );
    }

    #[test]
    fn should_hold_an_empty_tuple_in_a_record_field() {
        let types = TypeRegistryBuilder::new()
            .record("Holder", [("nothing", Ty::tuple([]))])
            .build();
        check(
            FlatProgramBuilder::with_registry(types)
                .entrypoint("Test", [], Ty::Html, |t| {
                    let held = t.record(Ty::named("Holder"), [("nothing", t.tuple(vec![]))]);
                    t.escape(
                        t.int_to_string(t.array_length(t.array([t.field_access(held, "nothing")]))),
                    )
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: () = ()
                  let v1: Holder = {nothing: v0}
                  let v2: () = v1.nothing
                  let v3: Array[()] = [v2]
                  let v4: Int = v3.len()
                  let v5: String = v4.to_string()
                  let v6: Html = escape(v5)
                  v6
                }

                -- after --
                1
            "#]],
        );
    }

    #[test]
    fn should_read_an_element_back_out_of_a_tuple() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    let pair = t.tuple([t.int(1), t.str("two")]);
                    t.escape(t.tuple_index(pair, 1))
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: Int = 1
                  let v1: String = "two"
                  let v2: (Int, String) = (v0, v1)
                  let v3: String = v2.1
                  let v4: Html = escape(v3)
                  v4
                }

                -- after --
                two
            "#]],
        );
    }

    #[test]
    fn should_read_an_element_back_out_of_a_one_tuple() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    let only = t.tuple([t.str("alone")]);
                    t.escape(t.tuple_index(only, 0))
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: String = "alone"
                  let v1: (String,) = (v0,)
                  let v2: String = v1.0
                  let v3: Html = escape(v2)
                  v3
                }

                -- after --
                alone
            "#]],
        );
    }

    #[test]
    fn should_index_a_nested_tuple() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    let inner = t.tuple([t.str("deep"), t.int(2)]);
                    let outer = t.tuple([t.int(1), inner]);
                    t.escape(t.tuple_index(t.tuple_index(outer, 1), 0))
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: String = "deep"
                  let v1: Int = 2
                  let v2: (String, Int) = (v0, v1)
                  let v3: Int = 1
                  let v4: (Int, (String, Int)) = (v3, v2)
                  let v5: (String, Int) = v4.1
                  let v6: String = v5.0
                  let v7: Html = escape(v6)
                  v7
                }

                -- after --
                deep
            "#]],
        );
    }

    #[test]
    fn should_wrap_int_addition_at_i32_boundary() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    let sum = t.add(t.int(2147483647), t.int(1));
                    t.escape(t.int_to_string(sum))
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: Int = 2147483647
                  let v1: Int = 1
                  let v2: Int = v0 + v1
                  let v3: String = v2.to_string()
                  let v4: Html = escape(v3)
                  v4
                }

                -- after --
                -2147483648
            "#]],
        );
    }

    #[test]
    fn should_evaluate_an_element() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.element("div", [], t.text("Hello World"))
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0() -> Html {
                  let v0: Html = text("Hello World")
                  let v1: Html = element("div", {}, v0)
                  v1
                }

                -- after --
                <div>Hello World</div>
            "#]],
        );
    }

    #[test]
    fn should_escape_attribute_values_and_settle_boolean_attributes() {
        check(
            FlatProgramBuilder::new()
                .entrypoint(
                    "Test",
                    [("cls", Ty::String), ("flag", Ty::Bool)],
                    Ty::Html,
                    |t| {
                        t.element(
                            "input",
                            [
                                t.attr_value("class", t.param("cls")),
                                t.attr_presence("disabled", t.param("flag")),
                                t.attr_presence("checked", t.bool(false)),
                            ],
                            t.html_concat(vec![]),
                        )
                    },
                )
                .build_with_registry(),
            |v| vec![("cls", v.str("a\"b")), ("flag", v.bool(true))],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(cls@v0: String, flag@v1: Bool) -> Html {
                  let v2: Bool = false
                  let v3: Html = concat()
                  let v4: Html = element("input", {class: v0, disabled: v1, checked: v2}, v3)
                  v4
                }

                -- after --
                <input class="a&quot;b" disabled>
            "#]],
        );
    }

    #[test]
    fn should_escape_html_in_expressions() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("content", Ty::String)], Ty::Html, |t| {
                    t.escape(t.param("content"))
                })
                .build_with_registry(),
            |v| vec![("content", v.str("<script>alert('xss')</script>"))],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(content@v0: String) -> Html {
                  let v1: Html = escape(v0)
                  v1
                }

                -- after --
                &lt;script&gt;alert('xss')&lt;/script&gt;
            "#]],
        );
    }

    #[test]
    fn should_render_if_body_when_condition_is_true() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("show", Ty::Bool)], Ty::Html, |t| {
                    t.match_(t.param("show"), |m| {
                        m.arm(Pat::True, |t, _| t.element("div", [], t.text("Visible")));
                        m.arm(Pat::False, |t, _| t.html_concat([]));
                    })
                })
                .build_with_registry(),
            |v| vec![("show", v.bool(true))],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(show@v0: Bool) -> Html {
                  let v4: Html = match v0 {
                    true => {
                      let v1: Html = text("Visible")
                      let v2: Html = element("div", {}, v1)
                      v2
                    }
                    false => {
                      let v3: Html = concat()
                      v3
                    }
                  }
                  v4
                }

                -- after --
                <div>Visible</div>
            "#]],
        );
    }

    #[test]
    fn should_skip_if_body_when_condition_is_false() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("show", Ty::Bool)], Ty::Html, |t| {
                    t.match_(t.param("show"), |m| {
                        m.arm(Pat::True, |t, _| t.element("div", [], t.text("Hidden")));
                        m.arm(Pat::False, |t, _| t.html_concat([]));
                    })
                })
                .build_with_registry(),
            |v| vec![("show", v.bool(false))],
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(show@v0: Bool) -> Html {
                  let v4: Html = match v0 {
                    true => {
                      let v1: Html = text("Hidden")
                      let v2: Html = element("div", {}, v1)
                      v2
                    }
                    false => {
                      let v3: Html = concat()
                      v3
                    }
                  }
                  v4
                }

                -- after --

            "#]],
        );
    }

    #[test]
    fn should_iterate_over_array_in_for_loop() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("items", Ty::array(Ty::String))], Ty::Html, |t| {
                    t.html_for(t.param("items"), |t, item| {
                        t.html_concat([t.element("li", vec![], t.escape(item)), t.text("\n")])
                    })
                })
                .build_with_registry(),
            |v| {
                vec![(
                    "items",
                    v.array(vec![v.str("Apple"), v.str("Banana"), v.str("Cherry")]),
                )]
            },
            expect![[r#"
                -- before --
                entrypoint fn Test@f0(items@v0: Array[String]) -> Html {
                  let v6: Html = for v1: String in v0 {
                    let v2: Html = escape(v1)
                    let v3: Html = element("li", {}, v2)
                    let v4: Html = text("\n")
                    let v5: Html = concat(v3, v4)
                    v5
                  }
                  v6
                }

                -- after --
                <li>Apple</li>
                <li>Banana</li>
                <li>Cherry</li>

            "#]],
        );
    }

    #[test]
    fn should_evaluate_a_recursive_function() {
        check(
            FlatProgramBuilder::new()
                .function("Sum", [("n", Ty::Int)], Ty::Int, |t| {
                    t.match_(t.lte(t.param("n"), t.int(0)), |m| {
                        m.arm(Pat::True, |t, _| t.int(0));
                        m.arm(Pat::False, |t, _| {
                            t.add(t.param("n"), t.call("Sum", [t.sub(t.param("n"), t.int(1))]))
                        });
                    })
                })
                .entrypoint("Test", [], Ty::Html, |t| {
                    t.escape(t.int_to_string(t.call("Sum", [t.int(10)])))
                })
                .build_with_registry(),
            |_| vec![],
            expect![[r#"
                -- before --
                fn Sum@f0(n@v0: Int) -> Int {
                  let v1: Int = 0
                  let v2: Bool = v0 <= v1
                  let v8: Int = match v2 {
                    true => {
                      let v3: Int = 0
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = v0 - v4
                      let v6: Int = Sum@f0(v5)
                      let v7: Int = v0 + v6
                      v7
                    }
                  }
                  v8
                }
                entrypoint fn Test@f1() -> Html {
                  let v9: Int = 10
                  let v10: Int = Sum@f0(v9)
                  let v11: String = v10.to_string()
                  let v12: Html = escape(v11)
                  v12
                }

                -- after --
                55
            "#]],
        );
    }

    #[test]
    fn should_error_when_a_function_never_stops_calling_itself() {
        let program = FlatProgramBuilder::new()
            .function("Loop", [("n", Ty::Int)], Ty::Int, |t| {
                t.call("Loop", [t.add(t.param("n"), t.int(1))])
            })
            .entrypoint("Test", [], Ty::Html, |t| {
                t.escape(t.int_to_string(t.call("Loop", [t.int(0)])))
            })
            .build();
        let result = evaluate_entrypoint(&program, program.functions[1].function(), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Function 'Loop@f0' exceeded the call depth limit of 12"
        );
    }

    #[test]
    fn should_error_when_a_function_calls_itself_on_copies_of_its_argument() {
        let types = TypeRegistryBuilder::new()
            .enum_(
                "Tree",
                [
                    ("Leaf", vec![]),
                    ("Node", vec![("children", Ty::array(Ty::named("Tree")))]),
                ],
            )
            .build();
        // Each call holds eight of the tree it was called with, so the tree at
        // the call depth limit has 8^11 leaves, too many to hold unless the
        // copies are shared.
        let program = FlatProgramBuilder::with_registry(types)
            .function("Grow", [("tree", Ty::named("Tree"))], Ty::Int, |t| {
                let tree = t.param("tree");
                let children = t.array([tree; 8]);
                let node =
                    t.enum_variant_with_fields(Ty::named("Tree"), "Node", [("children", children)]);
                t.call("Grow", [node])
            })
            .entrypoint("Test", [], Ty::Html, |t| {
                let leaf = t.enum_variant_with_fields(Ty::named("Tree"), "Leaf", []);
                t.escape(t.int_to_string(t.call("Grow", [leaf])))
            })
            .build();
        let result = evaluate_entrypoint(&program, program.functions[1].function(), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Function 'Grow@f0' exceeded the call depth limit of 12"
        );
    }

    #[test]
    fn should_error_when_a_range_is_too_long() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let all = t.array_from_range(t.int(i32::MIN), t.int(i32::MAX));
                t.escape(t.int_to_string(t.array_length(all)))
            })
            .build();
        let result = evaluate_entrypoint(&program, program.functions[0].function(), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "An array exceeded the array length limit of 10000000"
        );
    }

    #[test]
    fn should_error_when_a_string_is_too_long() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let mut string = t.str("x");
                for _ in 0..24 {
                    string = t.string_concat([string, string]);
                }
                t.escape(string)
            })
            .build();
        let result = evaluate_entrypoint(&program, program.functions[0].function(), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "A string exceeded the string length limit of 10000000 bytes"
        );
    }

    #[test]
    fn should_error_when_html_is_too_large() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], Ty::Html, |t| {
                let mut html = t.text("x");
                for _ in 0..24 {
                    html = t.html_concat(vec![html, html]);
                }
                html
            })
            .build();
        let result = evaluate_entrypoint(&program, program.functions[0].function(), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "An Html value exceeded the Html size limit of 10000000"
        );
    }

    #[test]
    fn should_error_when_a_function_calls_itself_in_each_element_of_a_loop() {
        let program = FlatProgramBuilder::new()
            .function("Tree", [("n", Ty::Int)], Ty::Html, |t| {
                let n = t.param("n");
                let deeper = t.lt(n, t.int(10));
                t.match_(deeper, |m| {
                    m.arm(Pat::True, |t, _| {
                        let three = t.array_from_range(t.int(1), t.int(3));
                        t.html_for_unbound(three, |t| t.call("Tree", [t.add(n, t.int(1))]))
                    });
                    m.arm(Pat::False, |t, _| t.text("leaf"));
                })
            })
            .entrypoint("Test", [], Ty::Html, |t| t.call("Tree", [t.int(0)]))
            .build();
        let result = evaluate_entrypoint(&program, program.functions[1].function(), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "An evaluation exceeded the op count limit of 5000"
        );
    }

    #[test]
    fn should_error_when_required_param_not_provided() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [("name", Ty::String)], Ty::Html, |t| {
                t.escape(t.param("name"))
            })
            .build();
        let result = evaluate_entrypoint(&program, program.functions[0].function(), HashMap::new());
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Missing required parameter"));
        assert!(err.to_string().contains("name"));
    }
}
