use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::ir::document_shell::DocumentShell;
use crate::ir::entrypoint_id::EntrypointId;
use crate::ir::flat_program::{
    FlatFor, FlatFunction, FlatHtmlBinding, FlatHtmlBlock, FlatHtmlOp, FlatProgram, FlatPureBlock,
    FlatPureOp,
};
use crate::ir::ir_binding::IrBinding;
use crate::ir::ir_data_op::IrDataOp;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_html_element::{IrAttribute, IrHtmlElement};
use crate::ir::ir_match::IrMatch;
use crate::ir::ir_page::IrPage;
use crate::ir::ir_type::IrFunctionDataType;
use crate::ir::runtime::bounded_vec::BoundedVec;
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::state::{IterationKey, Place, State, Step};
use crate::ir::runtime::value::{
    Closure, Effect, Env, Html, HtmlAttribute, HtmlNode, Lambda, MAX_ARRAY_LENGTH,
    MAX_STRING_LENGTH, Value,
};
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::type_name::TypeName;

/// The most frames that may be active at once, one for each call of a
/// function and each application of a closure. A call or an application
/// that would open one more fails with a call depth limit error.
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

pub fn evaluate_page<'a>(
    program: &'a FlatProgram,
    pages: &[IrPage],
    page_name: &TypeName,
    args: HashMap<AttributeName, Value<'a>>,
    shell: Option<&DocumentShell>,
) -> Result<String, EvalError> {
    let page = pages
        .iter()
        .find(|page| &page.name == page_name)
        .ok_or_else(|| EvalError::PageNotFound {
            page: page_name.clone(),
        })?;

    let head = match &page.head {
        Some(head) => evaluate_entrypoint(program, *head, args.clone())?.unwrap_html(),
        None => Html::new(HtmlNode::Concat(Vec::new()))?,
    };
    let body = evaluate_entrypoint(program, page.body, args)?.unwrap_html();

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

pub fn evaluate_entrypoint<'a>(
    program: &'a FlatProgram,
    entrypoint_id: EntrypointId,
    args: HashMap<AttributeName, Value<'a>>,
) -> Result<Value<'a>, EvalError> {
    let (value, _) = invoke_entrypoint(program, entrypoint_id, args, &State::default())?;
    Ok(value)
}

pub fn invoke_entrypoint<'a>(
    program: &'a FlatProgram,
    entrypoint_id: EntrypointId,
    mut args: HashMap<AttributeName, Value<'a>>,
    state: &State<'a>,
) -> Result<(Value<'a>, State<'a>), EvalError> {
    let entrypoint = &program.entrypoints[entrypoint_id];
    let mut env = Env::default();
    for param in &entrypoint.parameters {
        let value = args
            .remove(param.name())
            .ok_or_else(|| EvalError::MissingParameter {
                entrypoint: entrypoint.name.clone(),
                param: param.name().clone(),
            })?;
        env.bindings.push((param.binder.var, value));
    }
    let mut new_state = State {
        cells: BTreeMap::new(),
        elements: BTreeMap::new(),
        node_counter: state.node_counter,
    };
    let value = evaluate_html_block(
        &entrypoint.body,
        env,
        &program.functions,
        &Place::from([]),
        state,
        &mut new_state,
        1,
        &mut 0,
    )?;
    Ok((value, new_state))
}

fn evaluate_html_function<'a>(
    functions: &'a [FlatFunction],
    function: &IrFunction,
    args: Vec<Value<'a>>,
    place: &Place,
    old_state: &State<'a>,
    new_state: &mut State<'a>,
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::CallDepthLimit {
            limit: MAX_CALL_DEPTH,
        });
    }
    let decl = functions
        .iter()
        .find(|decl| decl.function().id == function.id)
        .unwrap();
    let FlatFunction::Html(decl) = decl else {
        panic!("an Html call calls {function}, which computes a value");
    };
    let vars = decl.parameters.iter().map(|param| param.binder.var);
    evaluate_html_block(
        &decl.body,
        Env {
            parent: None,
            bindings: vars.zip(args).collect(),
        },
        functions,
        place,
        old_state,
        new_state,
        depth + 1,
        op_count,
    )
}

fn evaluate_pure_function<'a>(
    functions: &'a [FlatFunction],
    function: &IrFunction,
    args: Vec<Value<'a>>,
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::CallDepthLimit {
            limit: MAX_CALL_DEPTH,
        });
    }
    let decl = functions
        .iter()
        .find(|decl| decl.function().id == function.id)
        .unwrap();
    let FlatFunction::Pure(decl) = decl else {
        panic!("a Pure call calls {function}, which is not Pure");
    };
    let vars = decl.parameters.iter().map(|param| param.binder.var);
    evaluate_pure_block(
        &decl.body,
        Env {
            parent: None,
            bindings: vars.zip(args).collect(),
        },
        functions,
        depth + 1,
        op_count,
    )
}

fn evaluate_pure_block<'a>(
    block: &'a FlatPureBlock,
    mut env: Env<'a>,
    functions: &'a [FlatFunction],
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    for binding in &block.bindings {
        *op_count += 1;
        if *op_count > MAX_OP_COUNT {
            return Err(EvalError::OpCountLimit {
                limit: MAX_OP_COUNT,
            });
        }
        evaluate_pure_binding(binding, &mut env, functions, depth, op_count)?;
    }
    Ok(env[&block.result].clone())
}

fn evaluate_html_block<'a>(
    block: &'a FlatHtmlBlock,
    mut env: Env<'a>,
    functions: &'a [FlatFunction],
    place: &Place,
    old_state: &State<'a>,
    new_state: &mut State<'a>,
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    for binding in &block.bindings {
        *op_count += 1;
        if *op_count > MAX_OP_COUNT {
            return Err(EvalError::OpCountLimit {
                limit: MAX_OP_COUNT,
            });
        }
        match binding {
            FlatHtmlBinding::Pure(binding) => {
                evaluate_pure_binding(binding, &mut env, functions, depth, op_count)?;
            }
            FlatHtmlBinding::Cell(binder, initial) => {
                let place: Place = place
                    .iter()
                    .cloned()
                    .chain([Step::Name(binder.var)])
                    .collect();
                new_state.bind_cell(old_state, &place, &env[initial]);
                env.bindings.push((binder.var, Value::Cell(place)));
            }
            FlatHtmlBinding::Track(binder, cell) => {
                let value = new_state.cells[env[cell].as_place()].clone();
                env.bindings.push((binder.var, value));
            }
            FlatHtmlBinding::Html(binding) => {
                let value = evaluate_html_op(
                    &binding.op,
                    &mut env,
                    functions,
                    &place
                        .iter()
                        .cloned()
                        .chain([Step::Name(binding.binder.var)])
                        .collect(),
                    old_state,
                    new_state,
                    depth,
                    op_count,
                )?;
                env.bindings.push((binding.binder.var, value));
            }
        }
    }
    Ok(env[&block.result].clone())
}

fn evaluate_pure_binding<'a>(
    binding: &'a IrBinding<IrFunctionDataType, FlatPureOp>,
    env: &mut Env<'a>,
    functions: &'a [FlatFunction],
    depth: usize,
    op_count: &mut usize,
) -> Result<(), EvalError> {
    let value = match &binding.op {
        FlatPureOp::Data(op) => evaluate_data_op(op, env)?,
        FlatPureOp::Call(function, args) => {
            let values = args.iter().map(|arg| env[arg].clone()).collect();
            evaluate_pure_function(functions, function, values, depth, op_count)?
        }
        FlatPureOp::Match(match_) => {
            let (body, env) = enter_arm(match_, env);
            evaluate_pure_block(body, env, functions, depth, op_count)?
        }
        FlatPureOp::CellWrite(cell, value) => Value::Effect(Rc::new(Effect::Write(
            env[cell].as_place().clone(),
            env[value].clone(),
        ))),
        FlatPureOp::CellRead(cell) => {
            Value::Effect(Rc::new(Effect::Read(env[cell].as_place().clone())))
        }
        FlatPureOp::EffectPure(value) => Value::Effect(Rc::new(Effect::Pure(env[value].clone()))),
        FlatPureOp::EffectBind(effect, continuation) => Value::Effect(Rc::new(Effect::Bind(
            env[effect].as_effect().clone(),
            env[continuation].as_closure().clone(),
        ))),
        FlatPureOp::Lambda(lambda) => Value::Closure(Rc::new(Closure {
            lambda: Lambda::Pure(lambda),
            env: env.freeze(),
        })),
        FlatPureOp::Apply(closure, args) => {
            let args = args.iter().map(|arg| env[arg].clone()).collect();
            evaluate_pure_apply(env[closure].as_closure(), args, functions, depth, op_count)?
        }
    };
    env.bindings.push((binding.binder.var, value));
    Ok(())
}

pub fn evaluate_pure_apply<'a>(
    closure: &Closure<'a>,
    args: Vec<Value<'a>>,
    functions: &'a [FlatFunction],
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::CallDepthLimit {
            limit: MAX_CALL_DEPTH,
        });
    }
    let Lambda::Pure(lambda) = closure.lambda else {
        panic!("a Pure application applies a closure that renders Html");
    };
    let body_env = Env {
        parent: closure.env.clone(),
        bindings: lambda
            .parameters
            .iter()
            .map(|param| param.var)
            .zip(args)
            .collect(),
    };
    evaluate_pure_block(&lambda.body, body_env, functions, depth + 1, op_count)
}

/// Performs the effect on the cells and gives its value and the cells as it
/// leaves them.
#[cfg_attr(not(test), allow(dead_code))]
pub fn run_effect<'a>(
    effect: &Effect<'a>,
    cells: &BTreeMap<Place, Value<'a>>,
    functions: &'a [FlatFunction],
    op_count: &mut usize,
) -> Result<(Value<'a>, BTreeMap<Place, Value<'a>>), EvalError> {
    let mut cells = cells.clone();
    let value = perform_effect(effect, &mut cells, functions, op_count)?;
    Ok((value, cells))
}

fn perform_effect<'a>(
    effect: &Effect<'a>,
    cells: &mut BTreeMap<Place, Value<'a>>,
    functions: &'a [FlatFunction],
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    match effect {
        Effect::Pure(value) => Ok(value.clone()),
        Effect::Write(place, value) => {
            *cells.get_mut(place).unwrap() = value.clone();
            Ok(Value::Tuple(Rc::new([])))
        }
        Effect::Read(place) => Ok(cells[place].clone()),
        Effect::Then(first, second) => {
            perform_effect(first, cells, functions, op_count)?;
            perform_effect(second, cells, functions, op_count)
        }
        Effect::Bind(effect, continuation) => {
            let value = perform_effect(effect, cells, functions, op_count)?;
            let next = evaluate_pure_apply(continuation, vec![value], functions, 0, op_count)?;
            perform_effect(next.as_effect(), cells, functions, op_count)
        }
    }
}

fn evaluate_html_apply<'a>(
    closure: &Closure<'a>,
    args: Vec<Value<'a>>,
    functions: &'a [FlatFunction],
    place: &Place,
    old_state: &State<'a>,
    new_state: &mut State<'a>,
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    if depth >= MAX_CALL_DEPTH {
        return Err(EvalError::CallDepthLimit {
            limit: MAX_CALL_DEPTH,
        });
    }
    let Lambda::Html(lambda) = closure.lambda else {
        panic!("an Html application applies a closure that computes a value");
    };
    let body_env = Env {
        parent: closure.env.clone(),
        bindings: lambda
            .parameters
            .iter()
            .map(|param| param.var)
            .zip(args)
            .collect(),
    };
    evaluate_html_block(
        &lambda.body,
        body_env,
        functions,
        place,
        old_state,
        new_state,
        depth + 1,
        op_count,
    )
}

fn enter_arm<'a, B>(match_: &'a IrMatch<B>, env: &mut Env<'a>) -> (&'a B, Env<'a>) {
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
    let env = Env {
        parent: env.freeze(),
        bindings: bound,
    };
    (&arm.body, env)
}

fn evaluate_html_op<'a>(
    op: &'a FlatHtmlOp,
    env: &mut Env<'a>,
    functions: &'a [FlatFunction],
    place: &Place,
    old_state: &State<'a>,
    new_state: &mut State<'a>,
    depth: usize,
    op_count: &mut usize,
) -> Result<Value<'a>, EvalError> {
    Ok(match op {
        FlatHtmlOp::Text(content) => Value::Html(Html::new(HtmlNode::Text(content.as_str()))?),

        FlatHtmlOp::Escape(string) => Value::Html(Html::new(HtmlNode::Escape(
            env[string].as_string().clone(),
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
            on_click,
        }) => {
            let mut rendered = Vec::new();
            for (name, attribute) in attributes {
                match attribute {
                    IrAttribute::Value(value) => rendered.push(HtmlAttribute {
                        name: name.clone(),
                        value: Some(env[value].as_string().clone()),
                    }),
                    IrAttribute::Presence(present) => {
                        if env[present].as_bool() {
                            rendered.push(HtmlAttribute {
                                name: name.clone(),
                                value: None,
                            });
                        }
                    }
                    IrAttribute::Constant(value) => rendered.push(HtmlAttribute {
                        name: name.clone(),
                        value: value.as_ref().map(|value| value.as_str().into()),
                    }),
                }
            }
            let children = match children {
                Some(children) => env[children].clone().unwrap_html(),
                None => Html::new(HtmlNode::Concat(Vec::new()))?,
            };
            let on_click = on_click.map(|handler| env[&handler].as_closure().clone());
            let identity = new_state.bind_element(old_state, place);
            Value::Html(Html::new(HtmlNode::Element {
                element: element.clone(),
                attributes: rendered,
                children,
                on_click,
                identity,
            })?)
        }

        FlatHtmlOp::Call(function, args) => {
            let values = args.iter().map(|arg| env[arg].clone()).collect();
            evaluate_html_function(
                functions, function, values, place, old_state, new_state, depth, op_count,
            )?
        }

        FlatHtmlOp::Match(match_) => {
            let (body, env) = enter_arm(match_, env);
            evaluate_html_block(
                body, env, functions, place, old_state, new_state, depth, op_count,
            )?
        }

        FlatHtmlOp::For(FlatFor {
            var,
            array,
            key,
            body,
        }) => {
            let mut parts = Vec::new();
            let mut occurrences: BTreeMap<IterationKey, usize> = BTreeMap::new();
            let items = env[array].clone();
            let parent = env.freeze();
            for (index, item) in items.as_array().iter().enumerate() {
                let mut env = Env {
                    parent: parent.clone(),
                    bindings: var
                        .iter()
                        .map(|binder| (binder.var, item.clone()))
                        .collect(),
                };
                let key = match key {
                    Some(key) => {
                        let env = Env {
                            parent: env.freeze(),
                            bindings: Vec::new(),
                        };
                        IterationKey::from_value(evaluate_pure_block(
                            key, env, functions, depth, op_count,
                        )?)
                    }
                    None => IterationKey::Int(index as i32),
                };
                let occurrence = occurrences.entry(key.clone()).or_default();
                let iteration: Place = place
                    .iter()
                    .cloned()
                    .chain([Step::Iteration(key, *occurrence)])
                    .collect();
                *occurrence += 1;
                let part = evaluate_html_block(
                    body, env, functions, &iteration, old_state, new_state, depth, op_count,
                )?;
                parts.push(part.unwrap_html());
            }
            Value::Html(Html::new(HtmlNode::Concat(parts))?)
        }

        FlatHtmlOp::Lambda(lambda) => Value::Closure(Rc::new(Closure {
            lambda: Lambda::Html(lambda),
            env: env.freeze(),
        })),

        FlatHtmlOp::Apply(closure, args) => {
            let args = args.iter().map(|arg| env[arg].clone()).collect();
            evaluate_html_apply(
                env[closure].as_closure(),
                args,
                functions,
                place,
                old_state,
                new_state,
                depth,
                op_count,
            )?
        }
    })
}

fn evaluate_data_op<'a>(op: &IrDataOp, env: &Env<'a>) -> Result<Value<'a>, EvalError> {
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
            let length: usize = parts.iter().map(|part| env[part].as_string().len()).sum();
            if length > MAX_STRING_LENGTH {
                return Err(STRING_LENGTH_LIMIT_ERROR);
            }
            Value::String(
                parts
                    .iter()
                    .map(|part| &**env[part].as_string())
                    .collect::<String>()
                    .into(),
            )
        }
        IrDataOp::NumericAdd(NumericType::Int, left, right) => {
            Value::Int(env[left].as_int().wrapping_add(env[right].as_int()))
        }
        IrDataOp::NumericAdd(NumericType::Float, left, right) => {
            Value::Float(env[left].as_float() + env[right].as_float())
        }
        IrDataOp::NumericSubtract(NumericType::Int, left, right) => {
            Value::Int(env[left].as_int().wrapping_sub(env[right].as_int()))
        }
        IrDataOp::NumericSubtract(NumericType::Float, left, right) => {
            Value::Float(env[left].as_float() - env[right].as_float())
        }
        IrDataOp::NumericMultiply(NumericType::Int, left, right) => {
            Value::Int(env[left].as_int().wrapping_mul(env[right].as_int()))
        }
        IrDataOp::NumericMultiply(NumericType::Float, left, right) => {
            Value::Float(env[left].as_float() * env[right].as_float())
        }
        IrDataOp::Equals(EquatableType::Bool, left, right) => {
            Value::Bool(env[left].as_bool() == env[right].as_bool())
        }
        IrDataOp::Equals(EquatableType::String, left, right) => {
            Value::Bool(env[left].as_string() == env[right].as_string())
        }
        IrDataOp::Equals(EquatableType::Int, left, right) => {
            Value::Bool(env[left].as_int() == env[right].as_int())
        }
        IrDataOp::Equals(EquatableType::Float, left, right) => {
            Value::Bool(env[left].as_float() == env[right].as_float())
        }
        IrDataOp::LessThan(ComparableType::Int, left, right) => {
            Value::Bool(env[left].as_int() < env[right].as_int())
        }
        IrDataOp::LessThan(ComparableType::Float, left, right) => {
            Value::Bool(env[left].as_float() < env[right].as_float())
        }
        IrDataOp::LessThanOrEqual(ComparableType::Int, left, right) => {
            Value::Bool(env[left].as_int() <= env[right].as_int())
        }
        IrDataOp::LessThanOrEqual(ComparableType::Float, left, right) => {
            Value::Bool(env[left].as_float() <= env[right].as_float())
        }
        IrDataOp::ArrayFromRange(left, right) => Value::Array(Rc::new(
            BoundedVec::new((env[left].as_int()..=env[right].as_int()).map(Value::Int))
                .ok_or(ARRAY_LENGTH_LIMIT_ERROR)?,
        )),
        IrDataOp::NumericNegation(NumericType::Int, operand) => {
            Value::Int(env[operand].as_int().wrapping_neg())
        }
        IrDataOp::NumericNegation(NumericType::Float, operand) => {
            Value::Float(-env[operand].as_float())
        }
        IrDataOp::BoolNegation(operand) => Value::Bool(!env[operand].as_bool()),
        IrDataOp::ArrayLength(operand) => Value::Int(env[operand].as_array().len() as i32),
        IrDataOp::ArrayIsEmpty(operand) => Value::Bool(env[operand].as_array().is_empty()),
        IrDataOp::ArrayGet(array, index) => Value::Option(
            usize::try_from(env[index].as_int())
                .ok()
                .and_then(|index| env[array].as_array().get(index))
                .map(|element| Rc::new(element.clone())),
        ),
        IrDataOp::ArrayEnumerate(operand) => {
            Value::Array(Rc::new(
                BoundedVec::new(env[operand].as_array().iter().enumerate().map(
                    |(index, element)| {
                        Value::Tuple(Rc::new([Value::Int(index as i32), element.clone()]))
                    },
                ))
                .ok_or(ARRAY_LENGTH_LIMIT_ERROR)?,
            ))
        }
        IrDataOp::StringIsEmpty(operand) => Value::Bool(env[operand].as_string().is_empty()),
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
    use crate::counting_allocator;
    use crate::hop::typing::{ComparableType, NumericType, Ty};
    use crate::ir::flat_program::FlatProgram;
    use crate::ir::flat_program_builder::{FlatProgramBuilder, Pat};
    use crate::ir::flat_program_generator::{random_entrypoint_args, random_program};
    use crate::ir::index_vec::Idx;
    use crate::ir::ir_type::IrType;
    use crate::ir::ir_type_declarations_builder::IrTypeDeclarationsBuilder;
    use crate::ir::runtime::clicks::clicks;
    use crate::ir::runtime::labels::Labels;
    use crate::ir::runtime::value_builder::ValueBuilder;
    use crate::symbols::function_name::FunctionName;
    use arbitrary::Unstructured;
    use expect_test::{Expect, expect};
    use rand::{RngExt, SeedableRng, rngs::SmallRng};
    use std::collections::BTreeSet;

    #[test]
    fn fuzz_random_programs_evaluate_without_panicking() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            for (entrypoint, args) in random_entrypoint_args(&program, u)? {
                match evaluate_entrypoint(&program, entrypoint, args) {
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

    #[test]
    fn count_allocations_in_random_programs() {
        const ARBTEST_SIZE_BYTES: u64 = 65_536;
        let mut generation_allocs = 0;
        let mut evaluation_allocs = 0;
        for seed in 0..32u64 {
            arbtest::arbtest(|u| {
                let allocations = counting_allocator::allocations();
                let program = random_program(u);
                generation_allocs += counting_allocator::allocations() - allocations;

                let allocations = counting_allocator::allocations();
                for (entrypoint, args) in random_entrypoint_args(&program, u)? {
                    let _ = evaluate_entrypoint(&program, entrypoint, args);
                }
                evaluation_allocs += counting_allocator::allocations() - allocations;
                Ok(())
            })
            .seed(seed << 32 | ARBTEST_SIZE_BYTES);
        }
        expect![[r#"
            program generation: 1390566 allocations
            program evaluation: 29479 allocations
        "#]]
        .assert_eq(&format!(
            "program generation: {generation_allocs} allocations\nprogram evaluation: {evaluation_allocs} allocations\n"
        ));
    }

    fn check(
        program: FlatProgram,
        args: impl FnOnce(&ValueBuilder<'_>) -> Vec<(&'static str, Value<'static>)>,
        expected: Expect,
    ) {
        let before = program.to_string();
        let args: HashMap<AttributeName, Value<'static>> = args(&ValueBuilder::new(&program.types))
            .into_iter()
            .map(|(name, value)| (AttributeName::parse(name).unwrap(), value))
            .collect();
        let after = evaluate_entrypoint(&program, EntrypointId::new(0), args)
            .expect("Evaluation should succeed")
            .into_markup();
        expected.assert_eq(&format!("-- before --\n{before}\n-- after --\n{after}\n"));
    }

    #[test]
    fn should_bind_loop_names_afresh_on_each_iteration() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Items", [("items", Ty::array(Ty::String))], |t| {
                    t.element(
                        "ul",
                        [],
                        Some(t.html_for(t.param("items"), |t, item| {
                            t.element("li", vec![], Some(t.escape(item)))
                        })),
                    )
                })
                .build()
                .unwrap(),
            |v| vec![("items", v.array(vec![v.str("a<b"), v.str("c")]))],
            expect![[r#"
                -- before --
                entrypoint Items@e0(items@v0: Array[String]) -> Html {
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
                .entrypoint("Test", [("flag", Ty::Bool)], |t| {
                    let and = t.match_(t.param("flag"), |m| {
                        m.arm(Pat::True, |t, _| t.call("diverge", []));
                        m.arm(Pat::False, |t, _| t.bool(false));
                    });
                    t.escape(t.match_(and, |m| {
                        m.arm(Pat::True, |t, _| t.str("yes"));
                        m.arm(Pat::False, |t, _| t.str("no"));
                    }))
                })
                .build()
                .unwrap(),
            |v| vec![("flag", v.bool(false))],
            expect![[r#"
                -- before --
                fn diverge@f0() -> Bool {
                  let v0: Bool = diverge@f0()
                  v0
                }
                entrypoint Test@e0(flag@v1: Bool) -> Html {
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
        let types = IrTypeDeclarationsBuilder::new()
            .enum_(
                "Shape",
                [("Dot", vec![]), ("Circle", vec![("radius", Ty::Int)])],
            )
            .build();
        check(
            FlatProgramBuilder::with_types(types)
                .entrypoint("Test", [("shape", Ty::named("Shape"))], |t| {
                    t.escape(t.match_(t.param("shape"), |m| {
                        m.arm(Pat::variant("Dot", []), |t, _| t.str("dot"));
                        m.arm(
                            Pat::variant("Circle", [("radius", Pat::bind("r"))]),
                            |t, arm| t.int_to_string(arm.binding("r")),
                        );
                    }))
                })
                .build()
                .unwrap(),
            |v| {
                vec![(
                    "shape",
                    v.enum_variant_with_fields("Shape", "Circle", vec![("radius", v.int(7))]),
                )]
            },
            expect![[r#"
                -- before --
                entrypoint Test@e0(shape@v0: Shape) -> Html {
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
        let types = IrTypeDeclarationsBuilder::new()
            .record("Holder", [("nothing", Ty::tuple([]))])
            .build();
        check(
            FlatProgramBuilder::with_types(types)
                .entrypoint("Test", [], |t| {
                    let held = t.record(t.ty(Ty::named("Holder")), [("nothing", t.tuple(vec![]))]);
                    t.escape(
                        t.int_to_string(t.array_length(t.array([t.field_access(held, "nothing")]))),
                    )
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Tuple[] = ()
                  let v1: Holder = {nothing: v0}
                  let v2: Tuple[] = v1.nothing
                  let v3: Array[Tuple[]] = [v2]
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
                .entrypoint("Test", [], |t| {
                    let pair = t.tuple([t.int(1), t.str("two")]);
                    t.match_(pair, |m| {
                        m.arm(
                            Pat::Tuple(vec![Pat::Wildcard, Pat::bind("second")]),
                            |t, arm| t.escape(arm.binding("second")),
                        );
                    })
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Int = 1
                  let v1: String = "two"
                  let v2: Tuple[Int, String] = (v0, v1)
                  let v5: Html = match v2 {
                    (_, v3: String) => {
                      let v4: Html = escape(v3)
                      v4
                    }
                  }
                  v5
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
                .entrypoint("Test", [], |t| {
                    let only = t.tuple([t.str("alone")]);
                    t.match_(only, |m| {
                        m.arm(Pat::Tuple(vec![Pat::bind("only")]), |t, arm| {
                            t.escape(arm.binding("only"))
                        });
                    })
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "alone"
                  let v1: Tuple[String] = (v0,)
                  let v4: Html = match v1 {
                    (v2: String,) => {
                      let v3: Html = escape(v2)
                      v3
                    }
                  }
                  v4
                }

                -- after --
                alone
            "#]],
        );
    }

    #[test]
    fn should_destructure_a_nested_tuple() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [], |t| {
                    let inner = t.tuple([t.str("deep"), t.int(2)]);
                    let outer = t.tuple([t.int(1), inner]);
                    t.match_(outer, |m| {
                        m.arm(
                            Pat::Tuple(vec![
                                Pat::Wildcard,
                                Pat::Tuple(vec![Pat::bind("deep"), Pat::Wildcard]),
                            ]),
                            |t, arm| t.escape(arm.binding("deep")),
                        );
                    })
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: String = "deep"
                  let v1: Int = 2
                  let v2: Tuple[String, Int] = (v0, v1)
                  let v3: Int = 1
                  let v4: Tuple[Int, Tuple[String, Int]] = (v3, v2)
                  let v7: Html = match v4 {
                    (_, (v5: String, _)) => {
                      let v6: Html = escape(v5)
                      v6
                    }
                  }
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
                .entrypoint("Test", [], |t| {
                    let sum = t.add(NumericType::Int, t.int(2147483647), t.int(1));
                    t.escape(t.int_to_string(sum))
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
                  let v0: Int = 2147483647
                  let v1: Int = 1
                  let v2: Int = add[Int](v0, v1)
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
                .entrypoint("Test", [], |t| {
                    t.element("div", [], Some(t.text("Hello World")))
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                entrypoint Test@e0() -> Html {
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
                .entrypoint("Test", [("cls", Ty::String), ("flag", Ty::Bool)], |t| {
                    t.element(
                        "input",
                        [
                            t.attr_value("class", t.param("cls")),
                            t.attr_presence("disabled", t.param("flag")),
                            t.attr_presence("checked", t.bool(false)),
                        ],
                        None,
                    )
                })
                .build()
                .unwrap(),
            |v| vec![("cls", v.str("a\"b")), ("flag", v.bool(true))],
            expect![[r#"
                -- before --
                entrypoint Test@e0(cls@v0: String, flag@v1: Bool) -> Html {
                  let v2: Bool = false
                  let v3: Html = element("input", {class: v0, disabled: v1, checked: v2})
                  v3
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
                .entrypoint("Test", [("content", Ty::String)], |t| {
                    t.escape(t.param("content"))
                })
                .build()
                .unwrap(),
            |v| vec![("content", v.str("<script>alert('xss')</script>"))],
            expect![[r#"
                -- before --
                entrypoint Test@e0(content@v0: String) -> Html {
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
                .entrypoint("Test", [("show", Ty::Bool)], |t| {
                    t.match_(t.param("show"), |m| {
                        m.arm(Pat::True, |t, _| {
                            t.element("div", [], Some(t.text("Visible")))
                        });
                        m.arm(Pat::False, |t, _| t.html_concat([]));
                    })
                })
                .build()
                .unwrap(),
            |v| vec![("show", v.bool(true))],
            expect![[r#"
                -- before --
                entrypoint Test@e0(show@v0: Bool) -> Html {
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
                .entrypoint("Test", [("show", Ty::Bool)], |t| {
                    t.match_(t.param("show"), |m| {
                        m.arm(Pat::True, |t, _| {
                            t.element("div", [], Some(t.text("Hidden")))
                        });
                        m.arm(Pat::False, |t, _| t.html_concat([]));
                    })
                })
                .build()
                .unwrap(),
            |v| vec![("show", v.bool(false))],
            expect![[r#"
                -- before --
                entrypoint Test@e0(show@v0: Bool) -> Html {
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
                .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                    t.html_for(t.param("items"), |t, item| {
                        t.html_concat([t.element("li", vec![], Some(t.escape(item))), t.text("\n")])
                    })
                })
                .build()
                .unwrap(),
            |v| {
                vec![(
                    "items",
                    v.array(vec![v.str("Apple"), v.str("Banana"), v.str("Cherry")]),
                )]
            },
            expect![[r#"
                -- before --
                entrypoint Test@e0(items@v0: Array[String]) -> Html {
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
                    t.match_(t.lte(ComparableType::Int, t.param("n"), t.int(0)), |m| {
                        m.arm(Pat::True, |t, _| t.int(0));
                        m.arm(Pat::False, |t, _| {
                            t.add(
                                NumericType::Int,
                                t.param("n"),
                                t.call("Sum", [t.sub(NumericType::Int, t.param("n"), t.int(1))]),
                            )
                        });
                    })
                })
                .entrypoint("Test", [], |t| {
                    t.escape(t.int_to_string(t.call("Sum", [t.int(10)])))
                })
                .build()
                .unwrap(),
            |_| vec![],
            expect![[r#"
                -- before --
                fn Sum@f0(n@v0: Int) -> Int {
                  let v1: Int = 0
                  let v2: Bool = lt_eq[Int](v0, v1)
                  let v8: Int = match v2 {
                    true => {
                      let v3: Int = 0
                      v3
                    }
                    false => {
                      let v4: Int = 1
                      let v5: Int = sub[Int](v0, v4)
                      let v6: Int = Sum@f0(v5)
                      let v7: Int = add[Int](v0, v6)
                      v7
                    }
                  }
                  v8
                }
                entrypoint Test@e0() -> Html {
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
    fn should_apply_a_closure_to_its_arguments_and_the_names_it_captures() {
        check(
            FlatProgramBuilder::new()
                .entrypoint("Test", [("base", Ty::Int)], |t| {
                    let base = t.param("base");
                    let add = t.lambda([IrType::INT], |t, params| {
                        t.add(NumericType::Int, params[0], base)
                    });
                    let one = t.apply(add, [t.int(1)]);
                    let two = t.apply(add, [t.int(2)]);
                    t.html_concat([
                        t.escape(t.int_to_string(one)),
                        t.escape(t.int_to_string(two)),
                    ])
                })
                .build()
                .unwrap(),
            |v| vec![("base", v.int(10))],
            expect![[r#"
                -- before --
                entrypoint Test@e0(base@v0: Int) -> Html {
                  let v3: (Int) -> Int = fn(v1: Int) {
                    let v2: Int = add[Int](v1, v0)
                    v2
                  }
                  let v4: Int = 1
                  let v5: Int = v3(v4)
                  let v6: Int = 2
                  let v7: Int = v3(v6)
                  let v8: String = v5.to_string()
                  let v9: Html = escape(v8)
                  let v10: String = v7.to_string()
                  let v11: Html = escape(v10)
                  let v12: Html = concat(v9, v11)
                  v12
                }

                -- after --
                1112
            "#]],
        );
    }

    #[test]
    fn should_render_a_closure_a_function_takes_once_per_application() {
        check(
            FlatProgramBuilder::new()
                .function(
                    "Rows",
                    [
                        ("items", Ty::array(Ty::String)),
                        ("row", Ty::function([Ty::String], Ty::Html)),
                    ],
                    Ty::Html,
                    |t| {
                        let rows =
                            t.html_for(t.param("items"), |t, item| t.apply(t.param("row"), [item]));
                        t.element("ul", [], Some(rows))
                    },
                )
                .entrypoint("Test", [("items", Ty::array(Ty::String))], |t| {
                    let prefix = t.str("- ");
                    let row = t.lambda([IrType::STRING], |t, params| {
                        let text = t.html_concat([t.escape(prefix), t.escape(params[0])]);
                        t.element("li", [], Some(text))
                    });
                    t.call("Rows", [t.param("items"), row])
                })
                .build()
                .unwrap(),
            |v| vec![("items", v.array(vec![v.str("a"), v.str("b")]))],
            expect![[r#"
                -- before --
                fn Rows@f0(items@v0: Array[String], row@v1: (String) -> Html) -> Html {
                  let v4: Html = for v2: String in v0 {
                    let v3: Html = v1(v2)
                    v3
                  }
                  let v5: Html = element("ul", {}, v4)
                  v5
                }
                entrypoint Test@e0(items@v6: Array[String]) -> Html {
                  let v7: String = "- "
                  let v13: (String) -> Html = fn(v8: String) {
                    let v9: Html = escape(v7)
                    let v10: Html = escape(v8)
                    let v11: Html = concat(v9, v10)
                    let v12: Html = element("li", {}, v11)
                    v12
                  }
                  let v14: Html = Rows@f0(v6, v13)
                  v14
                }

                -- after --
                <ul><li>- a</li><li>- b</li></ul>
            "#]],
        );
    }

    #[test]
    fn should_count_an_application_towards_the_call_depth() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                // Applying the last closure applies every closure before it,
                // twelve applications deep in the frame of Test.
                let mut closure = t.lambda([], |t, _| t.int(0));
                for _ in 1..12 {
                    let inner = closure;
                    closure = t.lambda([], |t, _| {
                        t.add(NumericType::Int, t.apply(inner, []), t.int(1))
                    });
                }
                t.escape(t.int_to_string(t.apply(closure, [])))
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Program exceeded the call depth limit of 12"
        );
    }

    #[test]
    fn should_error_when_a_function_never_stops_calling_itself() {
        let program = FlatProgramBuilder::new()
            .function("Loop", [("n", Ty::Int)], Ty::Int, |t| {
                t.call("Loop", [t.add(NumericType::Int, t.param("n"), t.int(1))])
            })
            .entrypoint("Test", [], |t| {
                t.escape(t.int_to_string(t.call("Loop", [t.int(0)])))
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Program exceeded the call depth limit of 12"
        );
    }

    #[test]
    fn should_error_when_a_function_calls_itself_on_copies_of_its_argument() {
        let types = IrTypeDeclarationsBuilder::new()
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
        let program = FlatProgramBuilder::with_types(types)
            .function("Grow", [("tree", Ty::named("Tree"))], Ty::Int, |t| {
                let tree = t.param("tree");
                let children = t.array([tree; 8]);
                let node = t.enum_variant_with_fields(
                    t.ty(Ty::named("Tree")),
                    "Node",
                    [("children", children)],
                );
                t.call("Grow", [node])
            })
            .entrypoint("Test", [], |t| {
                let leaf = t.enum_variant_with_fields(t.ty(Ty::named("Tree")), "Leaf", []);
                t.escape(t.int_to_string(t.call("Grow", [leaf])))
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Program exceeded the call depth limit of 12"
        );
    }

    #[test]
    fn should_error_when_a_range_is_too_long() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let all = t.array_from_range(t.int(i32::MIN), t.int(i32::MAX));
                t.escape(t.int_to_string(t.array_length(all)))
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "An array exceeded the array length limit of 10000000"
        );
    }

    #[test]
    fn should_error_when_a_string_is_too_long() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let mut string = t.str("x");
                for _ in 0..24 {
                    string = t.string_concat([string, string]);
                }
                t.escape(string)
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "A string exceeded the string length limit of 10000000 bytes"
        );
    }

    #[test]
    fn should_error_when_html_is_too_large() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [], |t| {
                let mut string = t.str("x");
                for _ in 0..23 {
                    string = t.string_concat([string, string]);
                }
                let first = t.escape(string);
                let second = t.escape(string);
                t.html_concat(vec![first, second])
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
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
                let deeper = t.lt(ComparableType::Int, n, t.int(10));
                t.match_(deeper, |m| {
                    m.arm(Pat::True, |t, _| {
                        let three = t.array_from_range(t.int(1), t.int(3));
                        t.html_for_unbound(three, |t| {
                            t.call("Tree", [t.add(NumericType::Int, n, t.int(1))])
                        })
                    });
                    m.arm(Pat::False, |t, _| t.text("leaf"));
                })
            })
            .entrypoint("Test", [], |t| t.call("Tree", [t.int(0)]))
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err().to_string(),
            "An evaluation exceeded the op count limit of 5000"
        );
    }

    #[test]
    fn should_error_when_required_param_not_provided() {
        let program = FlatProgramBuilder::new()
            .entrypoint("Test", [("name", Ty::String)], |t| {
                t.escape(t.param("name"))
            })
            .build()
            .unwrap();
        let result = evaluate_entrypoint(&program, EntrypointId::new(0), HashMap::new());
        assert_eq!(
            result.unwrap_err(),
            EvalError::MissingParameter {
                entrypoint: FunctionName::parse("Test").unwrap(),
                param: AttributeName::parse("name").unwrap(),
            }
        );
    }

    fn effects_are_equal<'a>(
        functions: &'a [FlatFunction],
        state: &State<'a>,
        a: &Effect<'a>,
        b: &Effect<'a>,
    ) -> bool {
        let (mut a_state, mut b_state) = (state.clone(), state.clone());
        match (a_state.run(a, functions), b_state.run(b, functions)) {
            (Ok(()), Ok(())) => a_state == b_state,
            (Err(a), Err(b)) => a == b,
            _ => false,
        }
    }

    fn observations_are_equal<'a>(
        functions: &'a [FlatFunction],
        state: &State<'a>,
        a: &Html<'a>,
        b: &Html<'a>,
    ) -> bool {
        let (mut a_markup, mut b_markup) = (String::new(), String::new());
        a.write(&mut a_markup);
        b.write(&mut b_markup);
        let (a_clicks, b_clicks) = (clicks(functions, a), clicks(functions, b));
        let (mut a_labels, mut b_labels) = (Labels::default(), Labels::default());
        a_markup == b_markup
            && a_clicks.len() == b_clicks.len()
            && (a_clicks.iter().zip(&b_clicks)).all(|((a_node, a), (b_node, b))| {
                a_labels.label(*a_node) == b_labels.label(*b_node)
                    && match (a, b) {
                        (Ok(a), Ok(b)) => effects_are_equal(functions, state, a, b),
                        (Err(a), Err(b)) => a == b,
                        _ => false,
                    }
            })
    }

    /// What following clicks did, over all programs: what each invocation
    /// gave, and what each click a render showed would do.
    #[derive(Default)]
    struct ClickStats {
        invocations: usize,
        call_depth_limit: usize,
        array_len_limit: usize,
        string_len_limit: usize,
        html_size_limit: usize,
        op_count_limit: usize,
        /// The cells of each render, summed.
        cells_bound: usize,
        /// The clicks rendered, over all programs.
        clicks: usize,
        /// Programs with a click.
        clicked: usize,
        /// Programs with a click that writes a cell a value other than the
        /// one it holds.
        changing: usize,
        /// Programs with a click after which the page renders differently.
        rerendering: usize,
        /// Programs with a click after which an element is evaluated or no
        /// longer evaluated although each iteration it is in is evaluated
        /// both before and after, as a match took another arm.
        switching_arms: usize,
        /// Programs with a click after which a cell is bound or no longer
        /// bound although each iteration it is in is evaluated both before
        /// and after, as a match took another arm. A cell that is no longer
        /// bound starts over when its arm is taken again.
        switching_cells: usize,
        /// Programs with a click after which a loop has other iterations:
        /// it gained, lost or re-keyed some. A match that takes another arm
        /// with a loop in it counts here as well.
        changing_loops: usize,
        /// Programs with a click that changes an array cell.
        changing_arrays: usize,
        /// What the clicks of the program being counted have done so far.
        program: Clicked,
    }

    #[derive(Default)]
    struct Clicked {
        any: bool,
        changing: bool,
        rerendering: bool,
        switching_arms: bool,
        switching_cells: bool,
        changing_loops: bool,
        changing_arrays: bool,
    }

    /// The iterations of the places, each as the steps up to and including
    /// it.
    fn iterations<'p>(places: impl Iterator<Item = &'p Place>) -> BTreeSet<Vec<Step>> {
        let mut iterations = BTreeSet::new();
        for place in places {
            let steps: Vec<Step> = place.iter().cloned().collect();
            for (index, step) in steps.iter().enumerate() {
                if matches!(step, Step::Iteration(..)) {
                    iterations.insert(steps[..=index].to_vec());
                }
            }
        }
        iterations
    }

    /// Whether each iteration of the place is in `both`, the iterations two
    /// invocations both evaluate, so that a place only one of them
    /// evaluates is not explained by a loop.
    fn in_both_iterations(place: &Place, both: &BTreeSet<Vec<Step>>) -> bool {
        let steps: Vec<Step> = place.iter().cloned().collect();
        (steps.iter().enumerate()).all(|(index, step)| {
            !matches!(step, Step::Iteration(..)) || both.contains(&steps[..=index])
        })
    }

    impl ClickStats {
        fn finish_program(&mut self) {
            let program = std::mem::take(&mut self.program);
            self.clicked += usize::from(program.any);
            self.changing += usize::from(program.changing);
            self.rerendering += usize::from(program.rerendering);
            self.switching_arms += usize::from(program.switching_arms);
            self.switching_cells += usize::from(program.switching_cells);
            self.changing_loops += usize::from(program.changing_loops);
            self.changing_arrays += usize::from(program.changing_arrays);
        }

        /// Count what an invocation gave.
        fn count_invocation<'a>(&mut self, result: &Result<(Value<'a>, State<'a>), EvalError>) {
            self.invocations += 1;
            self.call_depth_limit +=
                usize::from(matches!(result, Err(EvalError::CallDepthLimit { .. })));
            self.array_len_limit +=
                usize::from(matches!(result, Err(EvalError::ArrayLengthLimit { .. })));
            self.string_len_limit +=
                usize::from(matches!(result, Err(EvalError::StringLengthLimit { .. })));
            self.html_size_limit +=
                usize::from(matches!(result, Err(EvalError::HtmlSizeLimit { .. })));
            self.op_count_limit +=
                usize::from(matches!(result, Err(EvalError::OpCountLimit { .. })));
        }

        /// Count what each click of a render would do, each from the state
        /// of the render.
        fn count_clicks<'a>(
            &mut self,
            program: &'a FlatProgram,
            entrypoint: EntrypointId,
            args: &HashMap<AttributeName, Value<'a>>,
            html: &Html<'a>,
            state: &State<'a>,
        ) {
            self.cells_bound += state.cells.len();
            let mut markup = String::new();
            html.write(&mut markup);
            for (_, target) in clicks(&program.functions, html) {
                self.clicks += 1;
                self.program.any = true;
                let Ok(effect) = target else {
                    continue;
                };
                let mut next = state.clone();
                if next.run(&effect, &program.functions).is_err() {
                    continue;
                }
                let changed: Vec<&Value<'_>> = next
                    .cells
                    .iter()
                    .filter(|(place, value)| !state.cells[*place].total_eq(value))
                    .map(|(_, value)| value)
                    .collect();
                if changed.is_empty() {
                    continue;
                }
                self.program.changing = true;
                self.program.changing_arrays |=
                    changed.iter().any(|value| matches!(value, Value::Array(_)));
                if let Ok((value, next_state)) =
                    invoke_entrypoint(program, entrypoint, args.clone(), &next)
                {
                    let mut next_markup = String::new();
                    value.unwrap_html().write(&mut next_markup);
                    self.program.rerendering |= markup != next_markup;
                    // A loop iteration can hold cells and no elements, so
                    // the iterations come from both.
                    let before = iterations(state.elements.keys().chain(state.cells.keys()));
                    let after =
                        iterations(next_state.elements.keys().chain(next_state.cells.keys()));
                    let both = before.intersection(&after).cloned().collect();
                    self.program.changing_loops |= before != after;
                    let (elements, next_elements) = (&state.elements, &next_state.elements);
                    let dropped =
                        (elements.keys()).filter(|place| !next_elements.contains_key(*place));
                    let bound =
                        (next_elements.keys()).filter(|place| !elements.contains_key(*place));
                    self.program.switching_arms |= dropped
                        .chain(bound)
                        .any(|place| in_both_iterations(place, &both));
                    let (cells, next_cells) = (&state.cells, &next_state.cells);
                    let dropped = (cells.keys()).filter(|place| !next_cells.contains_key(*place));
                    let bound = (next_cells.keys()).filter(|place| !cells.contains_key(*place));
                    self.program.switching_cells |= dropped
                        .chain(bound)
                        .any(|place| in_both_iterations(place, &both));
                }
            }
        }
    }

    /// How many times `follow_clicks` draws arguments for the entrypoints of
    /// a program. Generating a program costs far more than evaluating it, so
    /// each program is evaluated on several.
    const ARGUMENT_SETS: usize = 4;

    /// How many clicks `follow_clicks` follows after the first render of an
    /// entrypoint, each followed by a render, unless a render has nothing to
    /// click or hits a limit of the evaluator.
    const CLICKS: usize = 8;

    /// Follow clicks on `program` as a page does, and check that its cells
    /// and elements keep their places and that each invocation is a fixed
    /// point. For each of `ARGUMENT_SETS` argument sets, each entrypoint is
    /// invoked on the state its invocation before left, then up to `CLICKS`
    /// clicks follow, each followed by an invocation on the same arguments.
    /// With `stats`, counts what each invocation gave and what each click it
    /// rendered would do.
    fn follow_clicks(
        program: &FlatProgram,
        u: &mut Unstructured<'_>,
        mut stats: Option<&mut ClickStats>,
    ) -> arbitrary::Result<()> {
        // The state of each entrypoint, in the order the program declares
        // them, which each invocation takes from the one before it.
        let mut states = Vec::new();
        for _ in 0..ARGUMENT_SETS {
            let invocations = random_entrypoint_args(program, u)?;
            states.resize(invocations.len(), State::default());
            for ((entrypoint, args), state) in invocations.into_iter().zip(&mut states) {
                for _ in 0..=CLICKS {
                    let result = invoke_entrypoint(program, entrypoint, args.clone(), state);
                    if let Some(stats) = stats.as_deref_mut() {
                        stats.count_invocation(&result);
                    }
                    let (value, new_state) = match result {
                        Ok(evaluated) => evaluated,
                        // A cell keeps a value from an earlier invocation or
                        // a click, so a range between two cells may exceed
                        // the array length limit where the Flat program does
                        // not.
                        Err(
                            EvalError::CallDepthLimit { .. }
                            | EvalError::ArrayLengthLimit { .. }
                            | EvalError::StringLengthLimit { .. }
                            | EvalError::HtmlSizeLimit { .. }
                            | EvalError::OpCountLimit { .. },
                        ) => break,
                        Err(error) => panic!("{error}"),
                    };
                    let html = value.unwrap_html();
                    if let Some(stats) = stats.as_deref_mut() {
                        stats.count_clicks(program, entrypoint, &args, &html, &new_state);
                    }
                    let targets = clicks(&program.functions, &html);
                    // A cell or an element bound at the same place as in the
                    // invocation before keeps its value or node, although the
                    // arguments differ, and a click wrote the cell. An
                    // element at a new place has a new node.
                    for (place, kept) in &new_state.cells {
                        if let Some(before) = state.cells.get(place) {
                            assert!(kept.total_eq(before), "{kept:?} is not {before:?}");
                        }
                    }
                    for (place, node) in &new_state.elements {
                        match state.elements.get(place) {
                            Some(before) => assert_eq!(node, before),
                            None => assert!(*node > state.node_counter),
                        }
                    }
                    // Invoking on the new state gives the same result and
                    // state.
                    let (again, again_state) =
                        invoke_entrypoint(program, entrypoint, args.clone(), &new_state).unwrap();
                    assert!(observations_are_equal(
                        &program.functions,
                        &new_state,
                        &again.unwrap_html(),
                        &html
                    ));
                    assert_eq!(again_state, new_state);
                    *state = new_state;
                    if targets.is_empty() {
                        break;
                    }
                    // A click whose handler reaches a limit of the evaluator
                    // performs nothing to compare.
                    let clicked = match &u.choose(&targets)?.1 {
                        Ok(effect) => state.run(effect, &program.functions),
                        Err(error) => Err(error.clone()),
                    };
                    match clicked {
                        Ok(()) => {}
                        Err(
                            EvalError::CallDepthLimit { .. }
                            | EvalError::ArrayLengthLimit { .. }
                            | EvalError::StringLengthLimit { .. }
                            | EvalError::HtmlSizeLimit { .. }
                            | EvalError::OpCountLimit { .. },
                        ) => break,
                        Err(error) => panic!("{error}"),
                    }
                }
            }
        }
        Ok(())
    }

    #[test]
    fn fuzz_random_programs_keep_cells_and_clicks_and_evaluate_to_a_fixed_point() {
        arbtest::arbtest(|u| {
            let program = random_program(u);
            follow_clicks(&program, u, None)
        });
    }

    /// What following clicks does on the programs `should_report_generator_stats`
    /// describes: the same seed and input give the same programs.
    #[test]
    fn should_report_click_stats() {
        const SAMPLES: usize = 100;
        const INPUT_LEN: usize = 65_536;
        let mut rng = SmallRng::seed_from_u64(0);
        let mut stats = ClickStats::default();
        for _ in 0..SAMPLES {
            let bytes: Vec<u8> = (0..INPUT_LEN).map(|_| rng.random()).collect();
            let mut u = Unstructured::new(&bytes);
            let program = random_program(&mut u);
            follow_clicks(&program, &mut u, Some(&mut stats)).unwrap();
            stats.finish_program();
        }
        let ClickStats {
            invocations,
            call_depth_limit,
            array_len_limit,
            string_len_limit,
            html_size_limit,
            op_count_limit,
            cells_bound,
            clicks,
            clicked,
            changing,
            rerendering,
            switching_arms,
            switching_cells,
            changing_loops,
            changing_arrays,
            program: _,
        } = stats;
        expect![[r#"
            3064 invocations
            25 invocations hit the call depth limit
            0 invocations hit the array length limit
            0 invocations hit the string length limit
            0 invocations hit the Html size limit
            19 invocations hit the op count limit
            37740 cells bound

            8321 clicks
            60 programs have a click
            27 programs have a click that changes a cell
            21 programs have a click that changes the page
            3 programs have a click that switches an arm of a match
            3 programs have a click that switches an arm of a match that binds cells
            5 programs have a click that changes the iterations of a loop
            5 programs have a click that changes an array cell
        "#]].assert_eq(&format!(
            "{invocations} invocations\n\
             {call_depth_limit} invocations hit the call depth limit\n\
             {array_len_limit} invocations hit the array length limit\n\
             {string_len_limit} invocations hit the string length limit\n\
             {html_size_limit} invocations hit the Html size limit\n\
             {op_count_limit} invocations hit the op count limit\n\
             {cells_bound} cells bound\n\n\
             {clicks} clicks\n\
             {clicked} programs have a click\n\
             {changing} programs have a click that changes a cell\n\
             {rerendering} programs have a click that changes the page\n\
             {switching_arms} programs have a click that switches an arm of a match\n\
             {switching_cells} programs have a click that switches an arm of a match that binds cells\n\
             {changing_loops} programs have a click that changes the iterations of a loop\n\
             {changing_arrays} programs have a click that changes an array cell\n"
        ));
    }
}
