use crate::common::{
    ComponentNode, CondNode, Environment, ErrorNode, ExprAttribute, ForNode, NativeHTMLNode, Node,
    Range, RangeError, RenderNode, Type,
};
use crate::unifier::Unifier;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub range: Range,
    pub value: Type,
}

impl TypeAnnotation {
    pub fn new(range: Range, value: Type) -> Self {
        TypeAnnotation { range, value }
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

pub fn typecheck(components: &[ComponentNode], import_types: &HashMap<String, Type>) -> TypeResult {
    let mut errors = Vec::new();
    let mut unifier = Unifier::new();
    let mut annotations: Vec<TypeAnnotation> = Vec::new();
    let mut parameter_types = import_types.clone();
    let mut env = Environment::new();

    for ComponentNode {
        name_attr,
        params_as_attr,
        children,
        ..
    } in components
    {
        if let Some(params_as_attr) = params_as_attr {
            let t1 = unifier.new_type_var();
            env.push(params_as_attr.value.clone(), t1.clone());
            annotations.push(TypeAnnotation::new(params_as_attr.range, t1.clone()));
            for child in children {
                typecheck_node(
                    child,
                    &parameter_types,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    &mut errors,
                );
            }
            env.pop();
            parameter_types.insert(name_attr.value.clone(), unifier.query(&t1));
        } else {
            parameter_types.insert(name_attr.value.clone(), Type::Void);
        }
    }

    let final_annotations = annotations
        .into_iter()
        .map(|a| TypeAnnotation::new(a.range, unifier.query(&a.value)))
        .collect();

    TypeResult::new(parameter_types, final_annotations, errors)
}

fn typecheck_node(
    node: &Node,
    parameter_types: &HashMap<String, Type>,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
) {
    match node {
        Node::For(ForNode {
            as_attr,
            each_attr,
            children,
            ..
        }) => {
            let t1 = unifier.new_type_var();
            typecheck_expr(
                &Type::Array(Box::new(t1.clone())),
                each_attr,
                env,
                unifier,
                annotations,
                errors,
            );

            if let Some(attr) = as_attr {
                annotations.push(TypeAnnotation::new(attr.range, t1.clone()));
                env.push(attr.value.clone(), t1);
            }

            for child in children {
                typecheck_node(child, parameter_types, env, unifier, annotations, errors);
            }

            if let Some(_) = as_attr {
                env.pop();
            }
        }
        Node::Cond(CondNode {
            if_attr, children, ..
        }) => {
            typecheck_expr(&Type::Bool, if_attr, env, unifier, annotations, errors);

            for child in children {
                typecheck_node(child, parameter_types, env, unifier, annotations, errors);
            }
        }
        Node::Render(RenderNode {
            component_attr,
            params_attr,
            children,
            ..
        }) => {
            if let Some(t1) = parameter_types.get(&component_attr.value) {
                if let Some(params_attr) = params_attr {
                    typecheck_expr(&t1.clone(), params_attr, env, unifier, annotations, errors);
                }
            } else {
                errors.push(RangeError::component_not_found(
                    &component_attr.value,
                    component_attr.range,
                ));
            }

            for child in children {
                typecheck_node(child, parameter_types, env, unifier, annotations, errors);
            }
        }
        Node::NativeHTML(NativeHTMLNode {
            inner_text_attr,
            children,
            ..
        }) => {
            if let Some(attr) = inner_text_attr {
                typecheck_expr(&Type::String, attr, env, unifier, annotations, errors);
            }

            for child in children {
                typecheck_node(child, parameter_types, env, unifier, annotations, errors);
            }
        }
        Node::Error(ErrorNode { children, .. }) => {
            for child in children {
                typecheck_node(child, parameter_types, env, unifier, annotations, errors);
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
    t1: &Type,
    attr: &ExprAttribute,
    env: &Environment<Type>,
    unifier: &mut Unifier,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
) {
    let segments = &attr.segments;
    if segments.is_empty() {
        errors.push(RangeError::empty_expression(attr.range));
        return;
    }

    if let Some(env_type) = env.lookup(&segments[0]) {
        let obj_type = segments.iter().skip(1).rev().fold(t1.clone(), |acc, s| {
            unifier.new_object(HashMap::from([(s.clone(), acc.clone())]))
        });

        if let Some(err) = unifier.unify(env_type, &obj_type) {
            errors.push(RangeError::unification_error(&err.message, attr.range));
            return;
        }

        annotations.push(TypeAnnotation::new(attr.range, t1.clone()));
    } else {
        errors.push(RangeError::undefined_variable(&segments[0], attr.range));
        return;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::tokenizer::tokenize;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::collections::HashMap;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_typechecker() {
        let entries = fs::read_dir(Path::new("test_data/typechecker")).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let input = archive.get("in").unwrap().content.trim();
            let expected = archive.get("out").unwrap().content.trim();

            let parse_result = parse(tokenize(input.to_string()));

            let type_result = typecheck(&parse_result.components, &HashMap::new());

            if !type_result.errors.is_empty() {
                let output = type_result
                    .errors
                    .iter()
                    .map(|e| e.message.clone())
                    .collect::<Vec<_>>()
                    .join("\n");
                assert_eq!(output, expected, "Mismatch in file: {}", file_name);
            } else {
                let output = type_result
                    .parameter_types
                    .get("main")
                    .expect("Type for main not found");

                assert_eq!(
                    format!("{}", output),
                    expected,
                    "Mismatch in file: {}",
                    file_name
                );
            }
        }
    }
}
