use crate::common::{Attribute, Environment, Node, NodeType, Range, RangeError, Type};
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

        for child in &node.children {
            typecheck_node(
                &mut component_annotations,
                &mut errors,
                import_types,
                &parameter_types,
                child,
                &mut env,
                &mut unifier,
            );
        }

        parameter_types.insert(component_name.to_string(), unifier.query(&t1));

        for a in component_annotations {
            annotations.push(TypeAnnotation::new(a.range, unifier.query(&a.type_)));
        }
    }

    TypeResult::new(parameter_types, annotations, errors)
}

fn typecheck_node(
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
    import_types: &HashMap<String, Type>,
    parameter_types: &HashMap<String, Type>,
    node: &Node,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
) {
    let mut did_push = false;

    match node.node_type {
        NodeType::ForNode => {
            let each_attr = node.get_attribute("each");
            let t1 = Type::TypeVar(unifier.next_type_var());
            typecheck_expr(
                &Type::Array(Box::new(t1.clone())),
                &each_attr,
                env,
                unifier,
                errors,
                annotations,
            );

            if node.has_attribute("as") {
                let as_attr = node.get_attribute("as");
                annotations.push(TypeAnnotation::new(as_attr.range, t1.clone()));
                env.push(as_attr.value.clone(), t1);
                did_push = true;
            }
        }

        NodeType::CondNode => {
            let if_attr = node.get_attribute("if");
            typecheck_expr(&Type::Bool, &if_attr, env, unifier, errors, annotations);
        }

        NodeType::RenderNode => {
            let component_attr = node.get_attribute("component");
            let params_attr = node.get_attribute("params");

            let t1 = if let Some(type_) = import_types.get(&component_attr.value) {
                type_.clone()
            } else if let Some(type_) = parameter_types.get(&component_attr.value) {
                type_.clone()
            } else {
                errors.push(RangeError::new(
                    format!("Component {} not found", component_attr.value),
                    component_attr.range,
                ));
                return;
            };

            typecheck_expr(&t1, &params_attr, env, unifier, errors, annotations);
        }

        NodeType::NativeHTMLNode => {
            if node.has_attribute("inner-text") {
                let inner_text_attr = node.get_attribute("inner-text");
                typecheck_expr(
                    &Type::String,
                    &inner_text_attr,
                    env,
                    unifier,
                    errors,
                    annotations,
                );
            }
        }

        _ => {}
    }

    for child in &node.children {
        typecheck_node(
            annotations,
            errors,
            import_types,
            parameter_types,
            child,
            env,
            unifier,
        );
    }

    if did_push {
        env.pop();
    }
}

fn typecheck_expr(
    t1: &Type,
    attr: &Attribute,
    env: &Environment<Type>,
    unifier: &mut Unifier,
    errors: &mut Vec<RangeError>,
    annotations: &mut Vec<TypeAnnotation>,
) {
    let segments = parse_expr(&attr.value);

    if segments.is_empty() {
        errors.push(RangeError::new("Empty expression".to_string(), attr.range));
        return;
    }

    if !env.has(&segments[0]) {
        errors.push(RangeError::new(
            format!("Undefined variable: {}", segments[0]),
            attr.range,
        ));
        return;
    }

    let mut current_type = env.lookup(&segments[0]).unwrap().clone();

    for s in segments.iter().skip(1) {
        let t2 = Type::TypeVar(unifier.next_type_var());
        let mut properties = HashMap::new();
        properties.insert(s.clone(), t2.clone());
        let obj = Type::Object(properties, unifier.next_type_var());

        if let Some(err) = unifier.unify(&current_type, &obj) {
            errors.push(RangeError::new(err.message, attr.range));
            return;
        }
        current_type = t2;
    }

    if let Some(err) = unifier.unify(t1, &current_type) {
        errors.push(RangeError::new(err.message, attr.range));
        return;
    }

    annotations.push(TypeAnnotation::new(attr.range, t1.clone()));
}

fn parse_expr(expr: &str) -> Vec<String> {
    expr.trim().split('.').map(|s| s.to_string()).collect()
}
