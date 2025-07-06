use crate::common::{Attribute, ComponentNode, Environment, Node, Range, RangeError, Type};
use crate::unifier::Unifier;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub range: Range,
    pub type_: Type,
}

impl TypeAnnotation {
    pub fn new(range: Range, type_: Type) -> Self {
        TypeAnnotation { range, type_ }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
        TypeResult {
            parameter_types,
            annotations,
            errors,
        }
    }
}

struct TypecheckerState {
    annotations: Vec<TypeAnnotation>,
    import_types: HashMap<String, Type>,
    parameter_types: HashMap<String, Type>,
}

impl TypecheckerState {
    fn new(
        annotations: Vec<TypeAnnotation>,
        import_types: HashMap<String, Type>,
        parameter_types: HashMap<String, Type>,
    ) -> Self {
        TypecheckerState {
            annotations,
            import_types,
            parameter_types,
        }
    }

    fn push_annotation(&mut self, range: Range, type_: Type) {
        self.annotations.push(TypeAnnotation::new(range, type_));
    }

    fn get_type(&self, component_name: &str) -> Option<&Type> {
        if let Some(type_) = self.import_types.get(component_name) {
            return Some(type_);
        }
        if let Some(type_) = self.parameter_types.get(component_name) {
            return Some(type_);
        }
        None
    }
}

pub fn typecheck(components: &[ComponentNode], import_types: HashMap<String, Type>) -> TypeResult {
    let mut errors = Vec::new();
    let mut state = TypecheckerState::new(Vec::new(), import_types, HashMap::new());
    let mut unifier = Unifier::new();

    for component in components {
        let component_name = &component.name_attr.value;
        if component.params_as_attr.is_none() {
            state
                .parameter_types
                .insert(component_name.clone(), Type::Void);
            continue;
        }

        let mut env = Environment::new();
        let t1 = Type::TypeVar(unifier.next_type_var());
        let params_as_attr = component.params_as_attr.as_ref().unwrap();
        env.push(params_as_attr.value.clone(), t1.clone());
        state.push_annotation(params_as_attr.range, t1.clone());

        for child in &component.children {
            typecheck_node(&mut state, child, &mut env, &mut unifier, &mut errors);
        }

        state
            .parameter_types
            .insert(component_name.clone(), unifier.query(&t1));
    }

    // Apply unifier to all annotations
    let final_annotations = state
        .annotations
        .into_iter()
        .map(|a| TypeAnnotation::new(a.range, unifier.query(&a.type_)))
        .collect();

    TypeResult::new(state.parameter_types, final_annotations, errors)
}

fn typecheck_node(
    state: &mut TypecheckerState,
    node: &Node,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
    errors: &mut Vec<RangeError>,
) {
    match node {
        Node::For(for_node) => {
            let t1 = Type::TypeVar(unifier.next_type_var());
            typecheck_expr(
                state,
                &Type::Array(Box::new(t1.clone())),
                &for_node.each_attr,
                env,
                unifier,
                errors,
            );

            if let Some(as_attr) = &for_node.as_attr {
                state.push_annotation(as_attr.range, t1.clone());
                env.push(as_attr.value.clone(), t1);
            }

            for child in &for_node.children {
                typecheck_node(state, child, env, unifier, errors);
            }

            if for_node.as_attr.is_some() {
                env.pop();
            }
        }
        Node::Cond(cond_node) => {
            typecheck_expr(state, &Type::Bool, &cond_node.if_attr, env, unifier, errors);

            for child in &cond_node.children {
                typecheck_node(state, child, env, unifier, errors);
            }
        }
        Node::Render(render_node) => {
            if let Some(t1) = state.get_type(&render_node.component_attr.value) {
                if let Some(params_attr) = &render_node.params_attr {
                    typecheck_expr(state, &t1.clone(), params_attr, env, unifier, errors);
                }
            } else {
                errors.push(RangeError::new(
                    format!("Component {} not found", render_node.component_attr.value),
                    render_node.component_attr.range,
                ));
            }

            for child in &render_node.children {
                typecheck_node(state, child, env, unifier, errors);
            }
        }
        Node::NativeHTML(html_node) => {
            if let Some(inner_text_attr) = &html_node.inner_text_attr {
                typecheck_expr(state, &Type::String, inner_text_attr, env, unifier, errors);
            }

            for child in &html_node.children {
                typecheck_node(state, child, env, unifier, errors);
            }
        }
        Node::Error(err_node) => {
            for child in &err_node.children {
                typecheck_node(state, child, env, unifier, errors);
            }
        }
        Node::Text(_) | Node::Doctype(_) => {
            // No typechecking needed
        }
        Node::Import(_) | Node::Component(_) => {
            panic!("Unexpected node")
        }
    }
}

fn typecheck_expr(
    state: &mut TypecheckerState,
    t1: &Type,
    attr: &Attribute,
    env: &Environment<Type>,
    unifier: &mut Unifier,
    errors: &mut Vec<RangeError>,
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

    for s in &segments[1..] {
        let t2 = Type::TypeVar(unifier.next_type_var());
        let mut props = HashMap::new();
        props.insert(s.clone(), t2.clone());
        let obj = Type::Object(props, unifier.next_type_var());

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

    state.push_annotation(attr.range, t1.clone());
}

fn parse_expr(expr: &str) -> Vec<String> {
    expr.trim().split('.').map(|s| s.to_string()).collect()
}
