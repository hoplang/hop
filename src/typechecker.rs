use crate::common::{
    BinaryOp, ComponentDefinitionNode, ComponentReferenceNode, Environment, ErrorNode,
    ExprAttribute, Expression, ForNode, IfNode, ImportNode, NativeHTMLNode, Node, Range,
    RangeError, RenderNode, SlotDefinitionNode, SlotReferenceNode, Type, XExecNode, XRawNode,
};
use crate::parser::Module;
use crate::unifier::Unifier;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation(pub Range, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionLink {
    pub reference_range: Range,
    pub definition_module: String,
    pub definition_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentInfo {
    pub parameter_type: Type,
    pub slots: Vec<String>,
    pub definition_module: String,
    pub definition_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResult {
    pub component_info: HashMap<String, ComponentInfo>,
    pub annotations: Vec<TypeAnnotation>,
    pub definition_links: Vec<DefinitionLink>,
}

impl TypeResult {
    pub fn new(
        component_info: HashMap<String, ComponentInfo>,
        annotations: Vec<TypeAnnotation>,
        definition_links: Vec<DefinitionLink>,
    ) -> Self {
        TypeResult {
            component_info,
            annotations,
            definition_links,
        }
    }
}

pub fn typecheck(
    module: &Module,
    import_type_results: &HashMap<String, TypeResult>,
    errors: &mut Vec<RangeError>,
) -> TypeResult {
    let mut unifier = Unifier::new();
    let mut annotations: Vec<TypeAnnotation> = Vec::new();
    let mut definition_links: Vec<DefinitionLink> = Vec::new();
    let mut component_info = HashMap::new();
    let mut imported_components: HashMap<String, Range> = HashMap::new();
    let mut referenced_components: HashSet<String> = HashSet::new();

    // Build component_info from all imported components
    for ImportNode {
        from_attr,
        component_attr,
        range,
        ..
    } in &module.imports
    {
        let from_module = &from_attr.value;
        let component_name = &component_attr.value;

        if let Some(type_result) = import_type_results.get(from_module) {
            if let Some(comp_info) = type_result.component_info.get(component_name) {
                if component_info.contains_key(component_name) {
                    errors.push(RangeError::component_already_defined(
                        component_name,
                        *range,
                    ));
                } else {
                    component_info.insert(component_name.clone(), comp_info.clone());
                    imported_components.insert(component_name.clone(), *range);
                }
            } else {
                errors.push(RangeError::undeclared_component(
                    from_module,
                    component_name,
                    *range,
                ));
            }
        } else {
            errors.push(RangeError::undefined_module(from_module, *range));
        }
    }
    let mut env = Environment::new();

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
        if component_info.contains_key(name) {
            errors.push(RangeError::component_already_defined(name, *range));
            continue;
        }

        let parameter_type = if let Some(params_as_attr) = params_as_attr {
            let t1 = unifier.new_type_var();
            annotations.push(TypeAnnotation(params_as_attr.range, t1.clone()));
            env.push(params_as_attr.var_name.value.clone(), t1.clone());
            for child in children {
                typecheck_node(
                    child,
                    &component_info,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    &mut definition_links,
                    &mut referenced_components,
                    errors,
                );
            }
            if !env.pop() {
                errors.push(RangeError::unused_variable(
                    &params_as_attr.var_name.value,
                    params_as_attr.range,
                ));
            }
            unifier.query(&t1)
        } else {
            for child in children {
                typecheck_node(
                    child,
                    &component_info,
                    &mut env,
                    &mut unifier,
                    &mut annotations,
                    &mut definition_links,
                    &mut referenced_components,
                    errors,
                );
            }
            Type::Void
        };

        component_info.insert(
            name.clone(),
            ComponentInfo {
                parameter_type,
                slots: slots.clone(),
                definition_module: module.name.clone(),
                definition_range: *range,
            },
        );
    }

    for RenderNode { children, .. } in &module.renders {
        for child in children {
            typecheck_node(
                child,
                &component_info,
                &mut env,
                &mut unifier,
                &mut annotations,
                &mut definition_links,
                &mut referenced_components,
                errors,
            );
        }
    }

    // Check for unused imports
    for (component_name, import_range) in imported_components {
        if !referenced_components.contains(&component_name) {
            errors.push(RangeError::unused_import(&component_name, import_range));
        }
    }

    let final_annotations = annotations
        .into_iter()
        .map(|TypeAnnotation(range, t)| TypeAnnotation(range, unifier.query(&t)))
        .collect();

    TypeResult::new(component_info, final_annotations, definition_links)
}

fn typecheck_node(
    node: &Node,
    component_info: &HashMap<String, ComponentInfo>,
    env: &mut Environment<Type>,
    unifier: &mut Unifier,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    referenced_components: &mut HashSet<String>,
    errors: &mut Vec<RangeError>,
) {
    match node {
        Node::If(IfNode {
            condition,
            children,
            range,
            ..
        }) => {
            let condition_type =
                typecheck_expression(condition, env, unifier, annotations, errors, *range);
            if let Some(err) = unifier.unify(&Type::Bool, &condition_type) {
                errors.push(RangeError::unification_error(&err.message, *range));
            }

            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    unifier,
                    annotations,
                    definition_links,
                    referenced_components,
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
            // Track that this component is being referenced
            referenced_components.insert(component.clone());

            if let Some(comp_info) = component_info.get(component) {
                // Add definition link for go-to-definition
                definition_links.push(DefinitionLink {
                    reference_range: *range,
                    definition_module: comp_info.definition_module.clone(),
                    definition_range: comp_info.definition_range,
                });

                if let Some(params_attr) = params_attr {
                    typecheck_expr(
                        &comp_info.parameter_type,
                        params_attr,
                        env,
                        unifier,
                        annotations,
                        errors,
                    );
                }

                // Validate slots
                for child in children {
                    if let Node::SlotReference(SlotReferenceNode { name, range, .. }) = child {
                        if !comp_info.slots.contains(name) {
                            errors.push(RangeError::undefined_slot(name, component, *range));
                        }
                    }
                }
            } else {
                errors.push(RangeError::undefined_component(component, *range));
            }

            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    unifier,
                    annotations,
                    definition_links,
                    referenced_components,
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
                    component_info,
                    env,
                    unifier,
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }
        }
        Node::SlotDefinition(SlotDefinitionNode { children, .. })
        | Node::SlotReference(SlotReferenceNode { children, .. })
        | Node::XExec(XExecNode { children, .. })
        | Node::XRaw(XRawNode { children, .. })
        | Node::Error(ErrorNode { children, .. }) => {
            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    unifier,
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }
        }
        Node::For(ForNode {
            var_name,
            array_expr,
            children,
            range,
        }) => {
            // Typecheck the array expression
            let array_type =
                typecheck_expression(array_expr, env, unifier, annotations, errors, *range);
            let element_type = unifier.new_type_var();
            let expected_array_type = Type::Array(Box::new(element_type.clone()));

            if let Some(err) = unifier.unify(&array_type, &expected_array_type) {
                errors.push(RangeError::unification_error(&err.message, *range));
            }

            // Push the loop variable into scope for the children
            let mut pushed = false;
            if env.push(var_name.value.clone(), element_type) {
                pushed = true;
            } else {
                errors.push(RangeError::variable_already_defined(
                    &var_name.value,
                    *range,
                ));
            }

            // Typecheck children
            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    unifier,
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }

            // Pop the loop variable from scope
            if pushed {
                if !env.pop() {
                    errors.push(RangeError::unused_variable(&var_name.value, *range));
                }
            }
        }
        Node::Text(_) | Node::Doctype(_) => {
            // No typechecking needed
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
            let mut module_type_results: HashMap<String, TypeResult> = HashMap::new();

            println!("Test case {} (line {})", case_num + 1, line_number);

            // Process all .hop files in the archive
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let mut errors = Vec::new();
                    let tokens = tokenize(file.content.trim(), &mut errors);
                    let module_name = file.name.trim_end_matches(".hop");
                    let module = parse(module_name.to_string(), tokens, &mut errors);

                    if !errors.is_empty() {
                        all_errors.extend(errors);
                        continue;
                    }

                    let type_result = typecheck(&module, &module_type_results, &mut errors);

                    if !errors.is_empty() {
                        all_errors.extend(errors);
                    } else {
                        module_type_results.insert(module.name.clone(), type_result.clone());

                        for c in module.components {
                            all_output_lines.push(format!(
                                "{}::{}\n\t{}\n\t{:?}",
                                module.name,
                                c.name,
                                type_result
                                    .component_info
                                    .get(&c.name)
                                    .expect("Component info not found")
                                    .parameter_type,
                                type_result
                                    .component_info
                                    .get(&c.name)
                                    .expect("Component info not found")
                                    .slots
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
