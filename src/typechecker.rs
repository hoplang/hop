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

struct TypecheckerState {
    annotations: Vec<TypeAnnotation>,
    errors: Vec<RangeError>,
    import_types: HashMap<String, Type>,
    parameter_types: HashMap<String, Type>,
}

impl TypecheckerState {
    fn new(import_types: HashMap<String, Type>) -> Self {
        Self {
            annotations: Vec::new(),
            errors: Vec::new(),
            import_types,
            parameter_types: HashMap::new(),
        }
    }

    fn push_annotation(&mut self, range: Range, type_: Type) {
        self.annotations.push(TypeAnnotation::new(range, type_));
    }

    fn push_error(&mut self, message: String, range: Range) {
        self.errors.push(RangeError::new(message, range));
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

pub fn typecheck(root: &Node, import_types: HashMap<String, Type>) -> TypeResult {
    let mut state = TypecheckerState::new(import_types);
    let mut unifier = Unifier::new();

    for node in root.get_children_of_type(NodeType::ComponentNode) {
        let component_name = node.get_attribute_value("name");

        if !node.has_attribute("params-as") {
            state
                .parameter_types
                .insert(component_name.to_string(), Type::Void);
            continue;
        }

        let mut env = Environment::new();
        let t1 = Type::TypeVar(unifier.next_type_var());
        let params_as_attr = node.get_attribute("params-as");
        env.push(params_as_attr.value.clone(), t1.clone());
        state.push_annotation(params_as_attr.range, t1.clone());

        for child in &node.children {
            typecheck_node(&mut state, child, &mut env, &mut unifier);
        }

        state
            .parameter_types
            .insert(component_name.to_string(), unifier.query(&t1));
    }

    let final_annotations = state
        .annotations
        .into_iter()
        .map(|a| TypeAnnotation::new(a.range, unifier.query(&a.type_)))
        .collect();

    TypeResult::new(state.parameter_types, final_annotations, state.errors)
}

fn typecheck_node(
    state: &mut TypecheckerState,
    node: &Node,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
) {
    match node.node_type {
        NodeType::ForNode => {
            let each_attr = node.get_attribute("each");
            let t1 = Type::TypeVar(unifier.next_type_var());
            typecheck_expr(
                state,
                Type::Array(Box::new(t1.clone())),
                &each_attr,
                env,
                unifier,
            );

            if node.has_attribute("as") {
                let as_attr = node.get_attribute("as");
                state.push_annotation(as_attr.range, t1.clone());
                env.push(as_attr.value.clone(), t1);
            }

            for child in &node.children {
                typecheck_node(state, child, env, unifier);
            }

            if node.has_attribute("as") {
                env.pop();
            }
        }

        NodeType::CondNode => {
            typecheck_expr(state, Type::Bool, &node.get_attribute("if"), env, unifier);

            for child in &node.children {
                typecheck_node(state, child, env, unifier);
            }
        }

        NodeType::RenderNode => {
            let component_attr = node.get_attribute("component");
            let params_attr = node.get_attribute("params");

            if let Some(t1) = state.get_type(&component_attr.value) {
                typecheck_expr(state, t1.clone(), &params_attr, env, unifier);
            } else {
                state.push_error(
                    format!("Component {} not found", component_attr.value),
                    component_attr.range,
                );
            }

            for child in &node.children {
                typecheck_node(state, child, env, unifier);
            }
        }

        NodeType::NativeHTMLNode => {
            if node.has_attribute("inner-text") {
                typecheck_expr(
                    state,
                    Type::String,
                    &node.get_attribute("inner-text"),
                    env,
                    unifier,
                );
            }

            for child in &node.children {
                typecheck_node(state, child, env, unifier);
            }
        }

        _ => {
            for child in &node.children {
                typecheck_node(state, child, env, unifier);
            }
        }
    }
}

fn typecheck_expr(
    state: &mut TypecheckerState,
    t1: Type,
    attr: &Attribute,
    env: &Environment<Type>,
    unifier: &mut Unifier,
) {
    let segments = parse_expr(&attr.value);

    if segments.is_empty() {
        state.push_error("Empty expression".to_string(), attr.range);
        return;
    }

    if !env.has(&segments[0]) {
        state.push_error(format!("Undefined variable: {}", segments[0]), attr.range);
        return;
    }

    let mut current_type = env.lookup(&segments[0]).unwrap().clone();

    for s in &segments[1..] {
        let t2 = Type::TypeVar(unifier.next_type_var());
        let mut properties = HashMap::new();
        properties.insert(s.clone(), t2.clone());
        let obj = Type::Object(properties, unifier.next_type_var());

        if let Some(err) = unifier.unify(&current_type, &obj) {
            state.push_error(err.message, attr.range);
            return;
        }

        current_type = t2;
    }

    if let Some(err) = unifier.unify(&t1, &current_type) {
        state.push_error(err.message, attr.range);
        return;
    }

    state.push_annotation(attr.range, t1);
}

fn parse_expr(expr: &str) -> Vec<String> {
    expr.trim().split('.').map(|s| s.to_string()).collect()
}
