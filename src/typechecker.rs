use crate::common::{
    ComponentNode, CondNode, EntrypointNode, Environment, ErrorNode, ExprAttribute, ForNode,
    NativeHTMLNode, Node, Range, RangeError, RenderNode, Type,
};
use crate::parser::Module;
use crate::unifier::Unifier;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation(pub Range, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResult {
    pub parameter_types: HashMap<String, Type>,
    pub annotations: Vec<TypeAnnotation>,
}

impl TypeResult {
    pub fn new(parameter_types: HashMap<String, Type>, annotations: Vec<TypeAnnotation>) -> Self {
        TypeResult {
            parameter_types,
            annotations,
        }
    }
}

pub fn typecheck(
    module: &Module,
    import_types: &HashMap<String, Type>,
    errors: &mut Vec<RangeError>,
) -> TypeResult {
    let mut unifier = Unifier::new();
    let mut annotations: Vec<TypeAnnotation> = Vec::new();
    let mut parameter_types = import_types.clone();
    let mut env = Environment::new();

    for ComponentNode {
        name_attr,
        params_as_attr,
        children,
        ..
    } in &module.components
    {
        // Check for duplicate component names
        if parameter_types.contains_key(&name_attr.value) {
            errors.push(RangeError::component_already_defined(
                &name_attr.value,
                name_attr.range,
            ));
            continue;
        }

        if let Some(params_as_attr) = params_as_attr {
            let t1 = unifier.new_type_var();
            if !env.push(params_as_attr.value.clone(), t1.clone()) {
                panic!("Variable name for component parameter was unexpectedly in use")
            }
            annotations.push(TypeAnnotation(params_as_attr.range, t1.clone()));
            for child in children {
                typecheck_node(
                    child,
                    &parameter_types,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    errors,
                );
            }

            let final_type = unifier.query(&t1);
            if !env.pop() {
                errors.push(RangeError::unused_variable(
                    &params_as_attr.value,
                    params_as_attr.range,
                ));
            }
            parameter_types.insert(name_attr.value.clone(), final_type);
        } else {
            parameter_types.insert(name_attr.value.clone(), Type::Void);
            for child in children {
                typecheck_node(
                    child,
                    &parameter_types,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    errors,
                );
            }
        }
    }

    // Typecheck entrypoints similarly to components
    for EntrypointNode {
        name_attr,
        params_as_attr,
        children,
        ..
    } in &module.entrypoints
    {
        // Check for duplicate entrypoint names (they share the same namespace as components)
        if parameter_types.contains_key(&name_attr.value) {
            errors.push(RangeError::component_already_defined(
                &name_attr.value,
                name_attr.range,
            ));
            continue;
        }

        if let Some(params_as_attr) = params_as_attr {
            let t1 = unifier.new_type_var();
            if !env.push(params_as_attr.value.clone(), t1.clone()) {
                panic!("Variable name for entrypoint parameter was unexpectedly in use")
            }
            annotations.push(TypeAnnotation(params_as_attr.range, t1.clone()));
            for child in children {
                typecheck_node(
                    child,
                    &parameter_types,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    errors,
                );
            }

            let final_type = unifier.query(&t1);
            if !env.pop() {
                errors.push(RangeError::unused_variable(
                    &params_as_attr.value,
                    params_as_attr.range,
                ));
            }
            parameter_types.insert(name_attr.value.clone(), final_type);
        } else {
            parameter_types.insert(name_attr.value.clone(), Type::Void);
            for child in children {
                typecheck_node(
                    child,
                    &parameter_types,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    errors,
                );
            }
        }
    }

    let final_annotations = annotations
        .into_iter()
        .map(|TypeAnnotation(range, t)| TypeAnnotation(range, unifier.query(&t)))
        .collect();

    TypeResult::new(parameter_types, final_annotations)
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

            let mut pushed = false;

            if let Some(attr) = as_attr {
                annotations.push(TypeAnnotation(attr.range, t1.clone()));
                if env.push(attr.value.clone(), t1.clone()) {
                    pushed = true;
                } else {
                    errors.push(RangeError::variable_already_defined(
                        &attr.value,
                        attr.range,
                    ));
                }
            }

            for child in children {
                typecheck_node(child, parameter_types, env, unifier, annotations, errors);
            }

            if pushed {
                if let Some(attr) = as_attr {
                    if !env.pop() {
                        errors.push(RangeError::unused_variable(&attr.value, attr.range));
                    }
                }
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
                errors.push(RangeError::undefined_component(
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
        Node::Import(_) | Node::Component(_) | Node::Entrypoint(_) => {
            panic!("Unexpected node")
        }
    }
}

fn typecheck_expr(
    t1: &Type,
    attr: &ExprAttribute,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
) {
    let segments = &attr.segments;
    if segments.is_empty() {
        errors.push(RangeError::empty_expression(attr.range));
        return;
    }

    if let Some(val) = env.lookup(&segments[0]) {
        let obj_type = segments.iter().skip(1).rev().fold(t1.clone(), |acc, s| {
            unifier.new_object(HashMap::from([(s.clone(), acc.clone())]))
        });

        if let Some(err) = unifier.unify(val, &obj_type) {
            errors.push(RangeError::unification_error(&err.message, attr.range));
            return;
        }

        annotations.push(TypeAnnotation(attr.range, t1.clone()));
    } else {
        errors.push(RangeError::undefined_variable(&segments[0], attr.range));
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

            let expected = archive.get("out").unwrap().content.trim();
            let mut all_errors = Vec::new();
            let mut all_output_lines = Vec::new();
            let mut module_component_types: HashMap<String, HashMap<String, Type>> = HashMap::new();

            // Process all .hop files in the archive
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let mut errors = Vec::new();
                    let tokens = tokenize(&file.content.trim(), &mut errors);
                    let module = parse(tokens, &mut errors);

                    if !errors.is_empty() {
                        all_errors.extend(errors);
                        continue;
                    }

                    // Build import types from previously processed modules
                    let mut import_types = HashMap::new();
                    for import_node in &module.imports {
                        let from_module = &import_node.from_attr.value;
                        let component_name = &import_node.component_attr.value;

                        let module_types = module_component_types
                            .get(from_module)
                            .unwrap_or_else(|| panic!("Module '{}' not found", from_module));

                        let component_type =
                            module_types.get(component_name).unwrap_or_else(|| {
                                panic!(
                                    "Component '{}' not found in module '{}'",
                                    component_name, from_module
                                )
                            });

                        import_types.insert(component_name.clone(), component_type.clone());
                    }

                    let type_result = typecheck(&module, &import_types, &mut errors);

                    if !errors.is_empty() {
                        all_errors.extend(errors);
                    } else {
                        // Get module name from filename (without .hop extension)
                        let module_name = file.name.trim_end_matches(".hop");

                        // Store this module's component types
                        module_component_types
                            .insert(module_name.to_string(), type_result.parameter_types.clone());

                        for c in module.components {
                            all_output_lines.push(format!(
                                "{}::{} : {}",
                                module_name,
                                c.name_attr.value,
                                type_result
                                    .parameter_types
                                    .get(&c.name_attr.value)
                                    .expect("Type not found")
                            ));
                        }
                        for e in module.entrypoints {
                            all_output_lines.push(format!(
                                "{}::{} : {}",
                                module_name,
                                e.name_attr.value,
                                type_result
                                    .parameter_types
                                    .get(&e.name_attr.value)
                                    .expect("Type not found")
                            ));
                        }
                    }
                }
            }

            if !all_errors.is_empty() {
                let output = all_errors
                    .iter()
                    .map(|e| e.message.clone())
                    .collect::<Vec<_>>()
                    .join("\n");
                assert_eq!(output, expected, "Mismatch in file: {}", file_name);
            } else {
                assert_eq!(
                    all_output_lines.join("\n"),
                    expected,
                    "Mismatch in file: {}",
                    file_name
                );
            }
        }
    }
}
