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
        let t1 = Type::TypeVar(unifier.next_type_var());
        let params_as_attr = node.get_attribute("params-as");

        env.push(params_as_attr.value.clone(), t1.clone());
        component_annotations.push(TypeAnnotation::new(params_as_attr.range, t1.clone()));

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

        for a in component_annotations {
            annotations.push(TypeAnnotation::new(a.range, unifier.query(&a.type_)));
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
                let t1 = Type::TypeVar(unifier.next_type_var());

                if let Some(err) = typecheck_expr(&t1, &each_attr.value, env, unifier) {
                    errors.push(RangeError::new(err, each_attr.range));
                    continue;
                }

                let t2 = Type::TypeVar(unifier.next_type_var());
                if let Some(err) = unifier.unify(&t1, &Type::Array(Box::new(t2.clone()))) {
                    errors.push(RangeError::new(err.message, each_attr.range));
                    continue;
                }

                annotations.push(TypeAnnotation::new(each_attr.range, t1));

                if !node.has_attribute("as") {
                    continue;
                }

                let as_attr = node.get_attribute("as");
                annotations.push(TypeAnnotation::new(as_attr.range, t2.clone()));
                env.push(as_attr.value.clone(), t2);
                did_push = true;
            }

            NodeType::CondNode => {
                let if_attr = node.get_attribute("if");
                let t1 = Type::TypeVar(unifier.next_type_var());

                if let Some(err) = typecheck_expr(&t1, &if_attr.value, env, unifier) {
                    errors.push(RangeError::new(err, if_attr.range));
                    continue;
                }

                if let Some(err) = unifier.unify(&t1, &Type::Bool) {
                    errors.push(RangeError::new(err.message, if_attr.range));
                    continue;
                }

                annotations.push(TypeAnnotation::new(if_attr.range, t1));
            }

            NodeType::RenderNode => {
                let component_attr = node.get_attribute("component");
                let params_attr = node.get_attribute("params");

                let t1 = if let Some(imported_type) = import_types.get(&component_attr.value) {
                    imported_type.clone()
                } else if let Some(param_type) = parameter_types.get(&component_attr.value) {
                    param_type.clone()
                } else {
                    errors.push(RangeError::new(
                        format!("Component {} not found", component_attr.value),
                        component_attr.range,
                    ));
                    continue;
                };

                let t2 = Type::TypeVar(unifier.next_type_var());
                if let Some(err) = typecheck_expr(&t2, &params_attr.value, env, unifier) {
                    errors.push(RangeError::new(err, params_attr.range));
                    continue;
                }

                if let Some(err) = unifier.unify(&t1, &t2) {
                    errors.push(RangeError::new(err.message, params_attr.range));
                    continue;
                }

                annotations.push(TypeAnnotation::new(params_attr.range, t1));
            }

            NodeType::NativeHTMLNode => {
                if !node.has_attribute("inner-text") {
                    continue;
                }

                let inner_text_attr = node.get_attribute("inner-text");
                let t1 = Type::TypeVar(unifier.next_type_var());

                if let Some(err) = typecheck_expr(&t1, &inner_text_attr.value, env, unifier) {
                    errors.push(RangeError::new(err, inner_text_attr.range));
                    continue;
                }

                if let Some(err) = unifier.unify(&t1, &Type::String) {
                    errors.push(RangeError::new(err.message, inner_text_attr.range));
                    continue;
                }

                annotations.push(TypeAnnotation::new(inner_text_attr.range, t1));
            }

            _ => {}
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
    t1: &Type,
    expr: &str,
    env: &Environment<Type>,
    unifier: &mut Unifier,
) -> Option<String> {
    let segments = parse_expr(expr);

    if segments.is_empty() {
        return Some("Empty expression".to_string());
    }

    if !env.has(&segments[0]) {
        return Some(format!("Undefined variable: {}", segments[0]));
    }

    let mut current_type = env.lookup(&segments[0]).unwrap().clone();

    for segment in &segments[1..] {
        let t2 = Type::TypeVar(unifier.next_type_var());
        let mut properties = HashMap::new();
        properties.insert(segment.clone(), t2.clone());
        let obj = Type::Object(properties, unifier.next_type_var());

        if let Some(err) = unifier.unify(&current_type, &obj) {
            return Some(err.message);
        }

        current_type = t2;
    }

    if let Some(err) = unifier.unify(t1, &current_type) {
        Some(err.message)
    } else {
        None
    }
}

fn parse_expr(expr: &str) -> Vec<String> {
    expr.trim().split('.').map(|s| s.to_string()).collect()
}
