use crate::common::{
    BinaryOp, RenderNode, ComponentDefinitionNode, CondNode, DefineSlotNode, Environment, ErrorNode,
    ExprAttribute, Expression, ForNode, ImportNode, NativeHTMLNode, Node, Range, RangeError,
    ComponentReferenceNode, SupplySlotNode, Type,
};
use crate::parser::Module;
use crate::unifier::Unifier;
use std::collections::{HashMap, HashSet};

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
    let mut component_slots: HashMap<String, Vec<String>> = HashMap::new();
    let mut env = Environment::new();

    let mut imported_names = HashSet::new();
    for ImportNode {
        component_attr,
        range,
        ..
    } in &module.imports
    {
        let name = component_attr.value.clone();
        if imported_names.contains(&name) {
            errors.push(RangeError::component_already_defined(&name, *range));
        } else {
            imported_names.insert(name);
        }
    }

    for RenderNode { children, .. } in &module.renders {
        for child in children {
            typecheck_node(
                child,
                &parameter_types,
                &component_slots,
                &mut env,
                &mut unifier,
                &mut annotations,
                errors,
            );
        }
    }

    for ComponentDefinitionNode {
        name,
        params_as_attr,
        children,
        slots,
        range,
        ..
    } in &module.components
    {
        // Check for duplicate component names
        if parameter_types.contains_key(name) {
            errors.push(RangeError::component_already_defined(name, *range));
            continue;
        }

        // Store component slots for validation
        component_slots.insert(name.clone(), slots.clone());

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
                    &component_slots,
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
            parameter_types.insert(name.clone(), final_type);
        } else {
            for child in children {
                typecheck_node(
                    child,
                    &parameter_types,
                    &component_slots,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    errors,
                );
            }
            parameter_types.insert(name.clone(), Type::Void);
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
    component_slots: &HashMap<String, Vec<String>>,
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
                typecheck_node(
                    child,
                    parameter_types,
                    component_slots,
                    env,
                    unifier,
                    annotations,
                    errors,
                );
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
                typecheck_node(
                    child,
                    parameter_types,
                    component_slots,
                    env,
                    unifier,
                    annotations,
                    errors,
                );
            }
        }
        Node::ComponentReference(ComponentReferenceNode {
            component,
            params_attr,
            children,
            range,
            ..
        }) => {
            if let Some(t1) = parameter_types.get(component) {
                if let Some(params_attr) = params_attr {
                    typecheck_expr(&t1.clone(), params_attr, env, unifier, annotations, errors);
                }
            } else {
                errors.push(RangeError::undefined_component(component, *range));
            }

            // Validate slots
            if let Some(defined_slots) = component_slots.get(component) {
                for child in children {
                    if let Node::SupplySlot(SupplySlotNode { name, range, .. }) = child {
                        if !defined_slots.contains(name) {
                            errors.push(RangeError::undefined_slot(name, component, *range));
                        }
                    }
                }
            }

            for child in children {
                typecheck_node(
                    child,
                    parameter_types,
                    component_slots,
                    env,
                    unifier,
                    annotations,
                    errors,
                );
            }
        }
        Node::NativeHTML(NativeHTMLNode {
            inner_text_attr,
            set_attributes,
            children,
            ..
        }) => {
            if let Some(attr) = inner_text_attr {
                typecheck_expr(&Type::String, attr, env, unifier, annotations, errors);
            }

            for set_attr in set_attributes {
                typecheck_expr(&Type::String, set_attr, env, unifier, annotations, errors);
            }

            for child in children {
                typecheck_node(
                    child,
                    parameter_types,
                    component_slots,
                    env,
                    unifier,
                    annotations,
                    errors,
                );
            }
        }
        Node::DefineSlot(DefineSlotNode { children, .. })
        | Node::SupplySlot(SupplySlotNode { children, .. })
        | Node::Render(RenderNode { children, .. })
        | Node::Error(ErrorNode { children, .. }) => {
            for child in children {
                typecheck_node(
                    child,
                    parameter_types,
                    component_slots,
                    env,
                    unifier,
                    annotations,
                    errors,
                );
            }
        }
        Node::Text(_) | Node::Doctype(_) => {
            // No typechecking needed
        }
        Node::Import(_) | Node::ComponentDefinition(_) => {
            panic!("Unexpected node")
        }
    }
}

fn typecheck_expr(
    expected_type: &Type,
    attr: &ExprAttribute,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
) {
    let expr_type = typecheck_expression(
        &attr.expression,
        env,
        unifier,
        annotations,
        errors,
        attr.range,
    );

    if let Some(err) = unifier.unify(&expr_type, expected_type) {
        errors.push(RangeError::unification_error(&err.message, attr.range));
        return;
    }

    annotations.push(TypeAnnotation(attr.range, expected_type.clone()));
}

fn typecheck_expression(
    expr: &Expression,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
    range: Range,
) -> Type {
    match expr {
        Expression::Variable(name) => {
            if let Some(var_type) = env.lookup(name) {
                var_type.clone()
            } else {
                errors.push(RangeError::undefined_variable(name, range));
                unifier.new_type_var()
            }
        }
        Expression::StringLiteral(_) => {
            // String literals always have type String
            Type::String
        }
        Expression::PropertyAccess(base_expr, property) => {
            let base_type =
                typecheck_expression(base_expr, env, unifier, annotations, errors, range);
            let property_type = unifier.new_type_var();
            let obj_type =
                unifier.new_object(HashMap::from([(property.clone(), property_type.clone())]));

            if let Some(err) = unifier.unify(&base_type, &obj_type) {
                errors.push(RangeError::unification_error(&err.message, range));
            }

            property_type
        }
        Expression::BinaryOp(left, BinaryOp::Equal, right) => {
            let left_type = typecheck_expression(left, env, unifier, annotations, errors, range);
            let right_type = typecheck_expression(right, env, unifier, annotations, errors, range);

            // Both operands should have the same type for equality comparison
            if let Some(err) = unifier.unify(&left_type, &right_type) {
                errors.push(RangeError::unification_error(
                    &format!("Type mismatch in equality comparison: {}", err.message),
                    range,
                ));
            }

            // The result of == is always boolean
            Type::Bool
        }
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
    use std::path::PathBuf;

    #[test]
    fn test_typechecker() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/typechecker.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();
            let mut all_errors = Vec::new();
            let mut all_output_lines = Vec::new();
            let mut module_component_types: HashMap<String, HashMap<String, Type>> = HashMap::new();

            println!("Test case {} (line {})", case_num + 1, line_number);

            // Process all .hop files in the archive
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let mut errors = Vec::new();
                    let tokens = tokenize(file.content.trim(), &mut errors);
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
                                c.name,
                                type_result
                                    .parameter_types
                                    .get(&c.name)
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
                assert_eq!(
                    output,
                    expected,
                    "Mismatch in test case {} (line {})",
                    case_num + 1,
                    line_number
                );
            } else {
                assert_eq!(
                    all_output_lines.join("\n"),
                    expected,
                    "Mismatch in test case {} (line {})",
                    case_num + 1,
                    line_number
                );
            }
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            if line == "## BEGIN" {
                assert!(
                    !in_case,
                    "Found '## BEGIN' at line {} while already inside a test case",
                    line_number
                );
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(
                    in_case,
                    "Found '## END' at line {} without matching '## BEGIN'",
                    line_number
                );
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(
            !in_case,
            "Reached end of file while inside a test case (missing '## END')"
        );

        test_cases
    }
}
