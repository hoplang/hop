use crate::common::{Environment, Node, NodeType, Range, RangeError, Type};
use crate::unifier::Unifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub range: Range,
    pub type_: Type,
}

impl TypeAnnotation {
    pub fn new(range: Range, type_: Type) -> Self {
        Self { range, type_ }
    }
}

#[derive(Debug, Clone)]
pub struct TypeResult {
    pub parameter_types: HashMap<String, Type>,
    pub annotations: Vec<TypeAnnotation>,
    pub errors: Vec<RangeError>,
}

impl TypeResult {
    pub fn new(
        parameter_types: HashMap<String, Type>,
        annotations: Vec<TypeAnnotation>,
        errors: Vec<RangeError>,
    ) -> Self {
        Self {
            parameter_types,
            annotations,
            errors,
        }
    }
}

pub fn typecheck(root: &Node, import_types: &HashMap<String, Type>) -> TypeResult {
    let mut parameter_types = HashMap::new();
    let mut annotations = Vec::new();
    let mut errors = Vec::new();
    let mut unifier = Unifier::new();

    for node in root.get_children_of_type(NodeType::ComponentNode) {
        let component_name = node.get_attribute_value("name");

        if !node.has_attribute("params-as") {
            parameter_types.insert(component_name.to_string(), Type::Void);
            continue;
        }

        let mut component_annotations = Vec::new();
        let mut env = Environment::new();
        let t1 = unifier.new_type_var();
        let params_as_attr = node.get_attribute("params-as");

        env.push(params_as_attr.value.clone(), t1.clone());
        component_annotations.push(TypeAnnotation::new(
            params_as_attr.range.clone(),
            t1.clone(),
        ));

        typecheck_nodes(
            &mut component_annotations,
            &mut errors,
            import_types,
            &parameter_types,
            &node.children,
            &mut env,
            &mut unifier,
        );

        parameter_types.insert(component_name.to_string(), unifier.query(&t1));

        for annotation in component_annotations {
            annotations.push(TypeAnnotation::new(
                annotation.range,
                unifier.query(&annotation.type_),
            ));
        }
    }

    TypeResult::new(parameter_types, annotations, errors)
}

fn typecheck_nodes(
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
    import_types: &HashMap<String, Type>,
    parameter_types: &HashMap<String, Type>,
    nodes: &[Node],
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
) {
    for node in nodes {
        let mut did_push = false;

        match node.node_type {
            NodeType::ForNode => {
                let each_attr = node.get_attribute("each");

                match typecheck_expr(&each_attr.value, env, unifier) {
                    Ok(t1) => {
                        let t2 = unifier.new_type_var();

                        match unifier.unify(&t1, &Type::Array(Box::new(t2.clone()))) {
                            None => {
                                annotations.push(TypeAnnotation::new(each_attr.range.clone(), t1));

                                if node.has_attribute("as") {
                                    let as_attr = node.get_attribute("as");
                                    annotations.push(TypeAnnotation::new(
                                        as_attr.range.clone(),
                                        t2.clone(),
                                    ));
                                    env.push(as_attr.value.clone(), t2);
                                    did_push = true;
                                }
                            }
                            Some(err) => {
                                errors.push(RangeError::new(err.message, each_attr.range.clone()));
                            }
                        }
                    }
                    Err(err) => {
                        errors.push(RangeError::new(err, each_attr.range.clone()));
                    }
                }
            }

            NodeType::CondNode => {
                let if_attr = node.get_attribute("if");

                match typecheck_expr(&if_attr.value, env, unifier) {
                    Ok(t1) => match unifier.unify(&t1, &Type::Bool) {
                        None => {
                            annotations.push(TypeAnnotation::new(if_attr.range.clone(), t1));
                        }
                        Some(err) => {
                            errors.push(RangeError::new(err.message, if_attr.range.clone()));
                        }
                    },
                    Err(err) => {
                        errors.push(RangeError::new(err, if_attr.range.clone()));
                    }
                }
            }

            NodeType::RenderNode => {
                let component_attr = node.get_attribute("component");
                let params_attr = node.get_attribute("params");

                let t1 = if let Some(t) = import_types.get(&component_attr.value) {
                    t.clone()
                } else if let Some(t) = parameter_types.get(&component_attr.value) {
                    t.clone()
                } else {
                    errors.push(RangeError::new(
                        format!("Component {} not found", component_attr.value),
                        component_attr.range.clone(),
                    ));
                    continue;
                };

                match typecheck_expr(&params_attr.value, env, unifier) {
                    Ok(t2) => match unifier.unify(&t1, &t2) {
                        None => {
                            annotations.push(TypeAnnotation::new(params_attr.range.clone(), t1));
                        }
                        Some(err) => {
                            errors.push(RangeError::new(err.message, params_attr.range.clone()));
                        }
                    },
                    Err(err) => {
                        errors.push(RangeError::new(err, params_attr.range.clone()));
                    }
                }
            }

            NodeType::NativeHTMLNode => {
                if node.has_attribute("inner-text") {
                    let inner_text_attr = node.get_attribute("inner-text");

                    match typecheck_expr(&inner_text_attr.value, env, unifier) {
                        Ok(t1) => match unifier.unify(&t1, &Type::String) {
                            Some(err) => {
                                errors.push(RangeError::new(
                                    err.message,
                                    inner_text_attr.range.clone(),
                                ));
                            }
                            None => {
                                annotations
                                    .push(TypeAnnotation::new(inner_text_attr.range.clone(), t1));
                            }
                        },
                        Err(err) => {
                            errors.push(RangeError::new(err, inner_text_attr.range.clone()));
                        }
                    }
                }
            }

            _ => {} // Other node types don't need type checking
        }

        typecheck_nodes(
            annotations,
            errors,
            import_types,
            parameter_types,
            &node.children,
            env,
            unifier,
        );

        if did_push {
            env.pop();
        }
    }
}

fn typecheck_expr(
    expr: &str,
    env: &Environment<Type>,
    unifier: &mut Unifier,
) -> Result<Type, String> {
    let segments = parse_expr(expr);

    if segments.is_empty() {
        return Err("Empty expression".to_string());
    }

    if !env.has(&segments[0]) {
        return Err(format!("Undefined variable: {}", segments[0]));
    }

    let mut current_type = env.lookup(&segments[0]).unwrap().clone();

    for segment in &segments[1..] {
        let t2 = unifier.new_type_var();
        let mut props = HashMap::new();
        props.insert(segment.clone(), t2.clone());
        if let Type::TypeVar(id) = unifier.new_type_var() {
            let obj_type = Type::Object(props, id);
            match unifier.unify(&current_type, &obj_type) {
                None => {
                    current_type = t2;
                }
                Some(err) => {
                    return Err(err.message);
                }
            }
        }
    }

    Ok(current_type)
}

fn parse_expr(expr: &str) -> Vec<String> {
    expr.trim().split('.').map(|s| s.to_string()).collect()
}
