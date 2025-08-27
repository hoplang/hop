use crate::common::{Position, Range, RangeError};
use crate::dop::{DopType, infer_type_from_json_file, is_subtype, typecheck_expr};
use crate::hop::ast::{
    ComponentDefinitionNode, ComponentReferenceNode, ErrorNode, ForNode, IfNode, ImportNode,
    NativeHTMLNode, Node, RenderNode, SlotDefinitionNode, SlotReferenceNode, XExecNode,
    XLoadJsonNode, XRawNode,
};
use crate::hop::environment::Environment;
use crate::hop::parser::Module;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub range: Range,
    pub typ: DopType,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentDefinitionLink {
    pub reference_opening_name_range: Range,
    pub reference_closing_name_range: Option<Range>,
    pub definition_module: String,
    pub definition_component_name: String,
    pub definition_opening_name_range: Range,
    pub definition_closing_name_range: Option<Range>,
}

impl ComponentDefinitionLink {
    pub fn reference_name_contains_position(&self, position: Position) -> bool {
        self.reference_opening_name_range
            .contains_position(position)
            || self
                .reference_closing_name_range
                .is_some_and(|range| range.contains_position(position))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentInfo {
    pub parameter_types: BTreeMap<String, DopType>,
    pub slots: Vec<String>,
    pub definition_module: String,
    pub definition_opening_name_range: Range,
    pub definition_closing_name_range: Option<Range>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResult {
    pub component_info: HashMap<String, ComponentInfo>,
    pub annotations: Vec<TypeAnnotation>,
    pub component_definition_links: Vec<ComponentDefinitionLink>,
}

impl TypeResult {
    pub fn new(
        component_info: HashMap<String, ComponentInfo>,
        annotations: Vec<TypeAnnotation>,
        definition_links: Vec<ComponentDefinitionLink>,
    ) -> Self {
        TypeResult {
            component_info,
            annotations,
            component_definition_links: definition_links,
        }
    }
}

pub fn typecheck(
    module: &Module,
    import_type_results: &HashMap<String, TypeResult>,
    errors: &mut Vec<RangeError>,
) -> TypeResult {
    let mut annotations: Vec<TypeAnnotation> = Vec::new();
    let mut definition_links: Vec<ComponentDefinitionLink> = Vec::new();
    let mut component_info = HashMap::new();
    let mut imported_components: HashMap<String, Range> = HashMap::new();
    let mut referenced_components: HashSet<String> = HashSet::new();

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
                    errors.push(RangeError::component_is_already_defined(
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

    env.push("HOP_MODE".to_string(), DopType::String);

    for ComponentDefinitionNode {
        name,
        params,
        children,
        preview,
        slots,
        range,
        opening_name_range,
        closing_name_range,
        ..
    } in &module.components
    {
        if component_info.contains_key(name) {
            errors.push(RangeError::component_is_already_defined(name, *range));
            continue;
        }

        let mut parameter_types = BTreeMap::new();

        for (param_name, param) in params {
            let param_type = param.type_annotation.clone();

            annotations.push(TypeAnnotation {
                range: param.var_name_range,
                typ: param_type.clone(),
                name: param.var_name.value.clone(),
            });
            env.push(param.var_name.value.clone(), param_type.clone());
            parameter_types.insert(param_name.clone(), param_type);
        }

        for child in children {
            typecheck_node(
                child,
                &component_info,
                &mut env,
                &mut annotations,
                &mut definition_links,
                &mut referenced_components,
                errors,
            );
        }

        // Check for unused variables (iterate in reverse to match push order)
        for (_, param) in params.iter().rev() {
            if !env.pop() {
                errors.push(RangeError::unused_variable(
                    &param.var_name.value,
                    param.var_name_range,
                ));
            }
        }

        component_info.insert(
            name.clone(),
            ComponentInfo {
                parameter_types,
                slots: slots.clone(),
                definition_module: module.name.clone(),
                definition_opening_name_range: *opening_name_range,
                definition_closing_name_range: *closing_name_range,
            },
        );

        if let Some(preview_nodes) = preview {
            for param in params.values() {
                env.push(param.var_name.value.clone(), param.type_annotation.clone());
            }
            for child in preview_nodes {
                typecheck_node(
                    child,
                    &component_info,
                    &mut env,
                    &mut annotations,
                    &mut definition_links,
                    &mut referenced_components,
                    errors,
                );
            }
            // Pop parameters
            for _ in params {
                env.pop();
            }
        }
    }

    for RenderNode { children, .. } in &module.renders {
        for child in children {
            typecheck_node(
                child,
                &component_info,
                &mut env,
                &mut annotations,
                &mut definition_links,
                &mut referenced_components,
                errors,
            );
        }
    }

    for (component_name, import_range) in imported_components {
        if !referenced_components.contains(&component_name) {
            errors.push(RangeError::unused_import(&component_name, import_range));
        }
    }

    let final_annotations = annotations;

    TypeResult::new(component_info, final_annotations, definition_links)
}

fn typecheck_node(
    node: &Node,
    component_info: &HashMap<String, ComponentInfo>,
    env: &mut Environment<DopType>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<ComponentDefinitionLink>,
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
            let condition_type = match typecheck_expr(condition, env, annotations, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    return; // Skip further processing of this branch
                }
            };
            if !is_subtype(&condition_type, &DopType::Bool) {
                errors.push(RangeError::new(
                    format!("Expected boolean condition, got {}", condition_type),
                    *range,
                ));
            }

            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }
        }
        Node::ComponentReference(ComponentReferenceNode {
            component,
            params,
            children,
            opening_name_range,
            closing_name_range,
            range,
            ..
        }) => {
            // Track that this component is being referenced
            referenced_components.insert(component.clone());

            if let Some(comp_info) = component_info.get(component) {
                // Add definition link for go-to-definition
                definition_links.push(ComponentDefinitionLink {
                    reference_opening_name_range: *opening_name_range,
                    reference_closing_name_range: *closing_name_range,
                    definition_module: comp_info.definition_module.clone(),
                    definition_component_name: component.clone(),
                    definition_opening_name_range: comp_info.definition_opening_name_range,
                    definition_closing_name_range: comp_info.definition_closing_name_range,
                });

                // Validate named arguments against parameter types
                let provided_args: std::collections::HashSet<_> = params.keys().collect();
                let expected_params: std::collections::HashSet<_> =
                    comp_info.parameter_types.keys().collect();

                // Check for missing required parameters
                for param_name in expected_params.difference(&provided_args) {
                    errors.push(RangeError::new(
                        format!("Missing required parameter '{}'", param_name),
                        *range,
                    ));
                }

                // Check for unexpected arguments
                for arg_name in provided_args.difference(&expected_params) {
                    errors.push(RangeError::new(
                        format!("Unexpected argument '{}'", arg_name),
                        *range,
                    ));
                }

                // Check each provided argument against its corresponding parameter type
                for (arg_name, (expression, expr_range)) in params {
                    if let Some(expected_type) = comp_info.parameter_types.get(arg_name) {
                        let expr_type = match typecheck_expr(expression, env, annotations, errors) {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(err);
                                continue; // Skip to next argument
                            }
                        };

                        if !is_subtype(&expr_type, expected_type) {
                            errors.push(RangeError::new(
                                format!(
                                    "Argument '{}' of type {} is incompatible with expected type {}",
                                    arg_name, expr_type, expected_type,
                                ),
                                *expr_range,
                            ));
                        } else {
                            annotations.push(TypeAnnotation {
                                range: *expr_range,
                                typ: expr_type,
                                name: format!("component parameter '{}'", arg_name),
                            });
                        }
                    }
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
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }
        }
        Node::NativeHTML(NativeHTMLNode {
            set_attributes,
            children,
            ..
        }) => {
            for set_attr in set_attributes {
                let expr_type = match typecheck_expr(&set_attr.expression, env, annotations, errors)
                {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err);
                        continue; // Skip this attribute
                    }
                };

                if !is_subtype(&expr_type, &DopType::String) {
                    errors.push(RangeError::new(
                        format!("Expected string attribute, got {}", expr_type),
                        set_attr.range,
                    ));
                    continue;
                }

                annotations.push(TypeAnnotation {
                    range: set_attr.range,
                    typ: DopType::String,
                    name: "attribute expression".to_string(),
                });
            }

            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
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
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }
        }
        Node::XLoadJson(XLoadJsonNode {
            file_attr,
            as_attr,
            children,
            range,
            ..
        }) => {
            // Infer the type from the JSON file
            let var_name = &as_attr.value;
            let file_path = &file_attr.value;

            let json_type = match infer_type_from_json_file(file_path) {
                Ok(typ) => typ,
                Err(err) => {
                    errors.push(RangeError::new(err, file_attr.range));
                    return; // Skip further processing
                }
            };

            // Push the JSON data variable into scope
            let mut pushed = false;
            if env.push(var_name.clone(), json_type.clone()) {
                pushed = true;
                // Add type annotation for the JSON variable
                annotations.push(TypeAnnotation {
                    range: as_attr.range,
                    typ: json_type,
                    name: var_name.clone(),
                });
            } else {
                errors.push(RangeError::variable_is_already_defined(
                    var_name,
                    as_attr.range,
                ));
            }

            // Typecheck children
            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }

            // Pop the JSON variable from scope
            if pushed && !env.pop() {
                errors.push(RangeError::unused_variable(var_name, *range));
            }
        }
        Node::For(ForNode {
            var_name: (var_name, var_name_range),
            array_expr,
            children,
            ..
        }) => {
            // Typecheck the array expression
            let array_type = match typecheck_expr(array_expr, env, annotations, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    return; // Skip further processing of this for loop
                }
            };
            let element_type = match &array_type {
                DopType::Array(Some(inner)) => *inner.clone(),
                DopType::Array(None) => {
                    errors.push(RangeError::new(
                        "Cannot iterate over an empty array with unknown element type".to_string(),
                        array_expr.range(),
                    ));
                    return;
                }
                _ => {
                    errors.push(RangeError::new(
                        format!("Can not iterate over {}", array_type),
                        array_expr.range(),
                    ));
                    return; // Skip further processing
                }
            };

            // Push the loop variable into scope for the children
            let mut pushed = false;
            if env.push(var_name.value.clone(), element_type.clone()) {
                pushed = true;
                // Add type annotation for the loop variable
                annotations.push(TypeAnnotation {
                    range: *var_name_range,
                    typ: element_type.clone(),
                    name: var_name.value.clone(),
                });
            } else {
                errors.push(RangeError::variable_is_already_defined(
                    &var_name.value,
                    *var_name_range,
                ));
            }

            // Typecheck children
            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    annotations,
                    definition_links,
                    referenced_components,
                    errors,
                );
            }

            // Pop the loop variable from scope
            if pushed && !env.pop() {
                errors.push(RangeError::unused_variable(
                    &var_name.value,
                    *var_name_range,
                ));
            }
        }
        Node::Text(_) | Node::Doctype(_) => {
            // No typechecking needed
        }
        Node::TextExpression(text_expr_node) => {
            // Typecheck the expression and ensure it's a string
            let expr_type =
                match typecheck_expr(&text_expr_node.expression, env, annotations, errors) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err);
                        return; // Skip further processing
                    }
                };
            if !is_subtype(&expr_type, &DopType::String) {
                errors.push(RangeError::new(
                    format!("Expected string for text expression, got {}", expr_type),
                    text_expr_node.range,
                ));
            }
            annotations.push(TypeAnnotation {
                range: text_expr_node.range,
                typ: DopType::String,
                name: "text expression".to_string(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::tui::error_formatter::ErrorFormatter;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    use std::collections::HashMap;

    fn check(archive_str: &str, expected: Expect) {
        let archive = Archive::from(archive_str);
        let mut error_formatter = ErrorFormatter::new();
        let mut all_output_lines = Vec::new();
        let mut module_type_results: HashMap<String, TypeResult> = HashMap::new();

        // Process all .hop files in the archive
        for file in archive.iter() {
            if file.name.ends_with(".hop") {
                let mut errors = Vec::new();
                let tokenizer = Tokenizer::new(file.content.trim());
                let module_name = file.name.trim_end_matches(".hop");
                let module = parse(module_name.to_string(), tokenizer, &mut errors);

                if !errors.is_empty() {
                    error_formatter.add_errors(
                        module_name.to_string(),
                        file.content.trim().to_string(),
                        errors,
                    );
                    continue;
                }

                let type_result = typecheck(&module, &module_type_results, &mut errors);

                if !errors.is_empty() {
                    error_formatter.add_errors(
                        module_name.to_string(),
                        file.content.trim().to_string(),
                        errors,
                    );
                } else {
                    module_type_results.insert(module.name.clone(), type_result.clone());

                    for c in module.components {
                        let component_info = type_result
                            .component_info
                            .get(&c.name)
                            .expect("Component info not found");
                        let param_types_str = component_info
                            .parameter_types
                            .iter()
                            .map(|(name, typ)| format!("{}: {}", name, typ))
                            .collect::<Vec<_>>()
                            .join(", ");
                        all_output_lines.push(format!(
                            "{}::{}\n\t{{{}}}\n\t{:?}\n",
                            module.name, c.name, param_types_str, component_info.slots
                        ));
                    }
                }
            }
        }

        let actual = if error_formatter.has_errors() {
            error_formatter.format_all_errors()
        } else {
            all_output_lines.join("\n")
        };

        expected.assert_eq(&actual);
    }

    // The typechecker allows empty files, and no types are emitted for them.
    #[test]
    fn test_empty_file() {
        check("-- main.hop --", expect![[r#""#]]);
    }

    // The parameter type of a single component without any content is `{}`.
    #[test]
    fn test_single_component_no_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	[]
            "#]],
        );
    }

    // When an undefined component is referenced, the typechecker outputs an error.
    #[test]
    fn test_undefined_component_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<h1>Hello, <render-name></render-name>!</h1>
                </main-comp>
            "#},
            expect![[r#"
                error: Component render-name is not defined
                  --> main.hop (line 2, col 13)
                1 | <main-comp>
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                  |                ^^^^^^^^^^^^^
            "#]],
        );
    }

    // When a component references itself, the typechecker outputs an error.
    #[test]
    fn test_component_references_itself() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<h1>Hello, <main-comp></main-comp>!</h1>
                </main-comp>
            "#},
            expect![[r#"
                error: Component main-comp is not defined
                  --> main.hop (line 2, col 13)
                1 | <main-comp>
                2 |     <h1>Hello, <main-comp></main-comp>!</h1>
                  |                ^^^^^^^^^^^
            "#]],
        );
    }

    // When a component is defined twice, the typechecker outputs an error.
    #[test]
    fn test_component_defined_twice() {
        check(
            indoc! {r#"
                -- main.hop --
                <foo-comp>
                </foo-comp>

                <foo-comp>
                </foo-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                  --> main.hop (line 4, col 1)
                3 | 
                4 | <foo-comp>
                  | ^^^^^^^^^^
            "#]],
        );
    }

    // When a component is defined with the same name as an imported component, the typechecker outputs an error.
    #[test]
    fn test_component_name_conflicts_with_import() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                </foo-comp>

                -- main.hop --
                <import component="foo-comp" from="other">

                <foo-comp>
                </foo-comp>

                <bar-comp>
                	<foo-comp/>
                </bar-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                  --> main.hop (line 3, col 1)
                2 | 
                3 | <foo-comp>
                  | ^^^^^^^^^^
            "#]],
        );
    }

    // When a component is imported twice, the typechecker outputs an error.
    #[test]
    fn test_component_imported_twice() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                </foo-comp>

                -- main.hop --
                <import component="foo-comp" from="other">
                <import component="foo-comp" from="other">

                <main-comp>
                	<foo-comp></foo-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                  --> main.hop (line 2, col 1)
                1 | <import component="foo-comp" from="other">
                2 | <import component="foo-comp" from="other">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // When a component is imported from a module that doesn't exist the typechecker outputs an error.
    // TODO: Improve error message
    #[test]
    fn test_import_from_nonexistent_module() {
        check(
            indoc! {r#"
                -- main.hop --
                <import component="foo-comp" from="other">

                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Module other is not defined
                  --> main.hop (line 1, col 1)
                1 | <import component="foo-comp" from="other">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // When a component that doesn't exist is imported from a module that does exist the typechecker outputs an error.
    #[test]
    fn test_import_nonexistent_component_from_existing_module() {
        check(
            indoc! {r#"
                -- other.hop --
                -- main.hop --
                <import component="foo-comp" from="other">

                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Module other does not declare a component named foo-comp
                  --> main.hop (line 1, col 1)
                1 | <import component="foo-comp" from="other">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // When a component is imported without being used the typechecker outputs an error.
    #[test]
    fn test_unused_import() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                </foo-comp>

                -- main.hop --
                <import component="foo-comp" from="other">

                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Unused import: foo-comp
                  --> main.hop (line 1, col 1)
                1 | <import component="foo-comp" from="other">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // The typechecker collects all errors rather than returning at the first error.
    #[test]
    fn test_multiple_errors_collected() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<h1>Hello, <render-name></render-name>!</h1>
                	<h1>Hello, <other-comp></other-comp>!</h1>
                </main-comp>
            "#},
            expect![[r#"
                error: Component render-name is not defined
                  --> main.hop (line 2, col 13)
                1 | <main-comp>
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                  |                ^^^^^^^^^^^^^

                error: Component other-comp is not defined
                  --> main.hop (line 3, col 13)
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                3 |     <h1>Hello, <other-comp></other-comp>!</h1>
                  |                ^^^^^^^^^^^^
            "#]],
        );
    }

    // Defining a component with the same name in two different modules is allowed.
    #[test]
    fn test_same_component_name_in_different_modules() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                </foo-comp>

                -- main.hop --
                <foo-comp>
                </foo-comp>
            "#},
            expect![[r#"
                other::foo-comp
                	{}
                	[]

                main::foo-comp
                	{}
                	[]
            "#]],
        );
    }

    // When a slot is defined twice in a component, the typechecker outputs an error.
    #[test]
    fn test_slot_defined_twice() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <slot-content>
                        First definition
                    </slot-content>
                    <slot-content>
                        Second definition
                    </slot-content>
                </main-comp>
            "#},
            expect![[r#"
                error: Slot 'content' is already defined
                  --> main.hop (line 5, col 5)
                4 |     </slot-content>
                5 |     <slot-content>
                  |     ^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Two components are allowed to have slots with the same name.
    #[test]
    fn test_same_slot_name_in_different_components() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <slot-content>
                        First definition
                    </slot-content>
                </main-comp>
                <foo-comp>
                    <slot-content>
                        First definition
                    </slot-content>
                </foo-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	["content"]

                main::foo-comp
                	{}
                	["content"]
            "#]],
        );
    }

    // When an undefined slot is referenced, the typechecker outputs an error.
    #[test]
    fn test_undefined_slot_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <strong>
                        <slot-data></slot-data>
                    </strong>
                </main-comp>

                <bar-comp>
                    <main-comp>
                        <with-invalid>
                            This slot doesn't exist
                        </with-invalid>
                    </main-comp>
                </bar-comp>
            "#},
            expect![[r#"
                error: Slot 'invalid' is not defined in component main-comp
                  --> main.hop (line 9, col 9)
                 8 |     <main-comp>
                 9 |         <with-invalid>
                   |         ^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Test undefined slot with imported component.
    #[test]
    fn test_undefined_slot_with_imported_component() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                    <strong>
                        <slot-data></slot-data>
                    </strong>
                </foo-comp>
                -- main.hop --
                <import component="foo-comp" from="other">

                <bar-comp>
                    <foo-comp>
                        <with-invalid>
                            This slot doesn't exist
                        </with-invalid>
                    </foo-comp>
                </bar-comp>
            "#},
            expect![[r#"
                error: Slot 'invalid' is not defined in component foo-comp
                  --> main.hop (line 5, col 9)
                4 |     <foo-comp>
                5 |         <with-invalid>
                  |         ^^^^^^^^^^^^^^
            "#]],
        );
    }

    // When slot-default is defined, it must be the only slot, otherwise the typechecker outputs an error.
    #[test]
    fn test_slot_default_must_be_only_slot() {
        check(
            indoc! {r#"
                -- main.hop --
                <mixed-comp>
                    <slot-default>Default slot</slot-default>
                    <slot-other>Other slot</slot-other>
                </mixed-comp>
            "#},
            expect![[r#"
                error: When using slot-default, it must be the only slot in the component
                  --> main.hop (line 3, col 5)
                2 |     <slot-default>Default slot</slot-default>
                3 |     <slot-other>Other slot</slot-other>
                  |     ^^^^^^^^^^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: object[foo: array[string]]}>
                  <for {items in items.foo}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: object[foo: array[string]]}>
                2 |   <for {items in items.foo}>
                  |         ^^^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_array_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: array[string]}>
                  <for {items in items}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: array[string]}>
                2 |   <for {items in items}>
                  |         ^^^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_nested_loops() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: object[a: array[string], b: array[string]]}>
                  <for {item in items.a}>
                    <for {item in items.b}>
                      <div>{item}</div>
                    </for>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Variable item is already defined
                  --> main.hop (line 3, col 11)
                2 |   <for {item in items.a}>
                3 |     <for {item in items.b}>
                  |           ^^^^
            "#]],
        );
    }

    // When an undefined variable is referenced, the typechecker outputs an error.
    #[test]
    fn test_undefined_variable_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: array[object[active: boolean]]}>
                	<for {item in params}>
                	  <if {item.active}>
                	  </if>
                	</for>
                	<if {item.active}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                error: Undefined variable: item
                  --> main.hop (line 6, col 7)
                5 |     </for>
                6 |     <if {item.active}>
                  |          ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_variable_second_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: array[string]}>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                  <for {item in items}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 5, col 9)
                4 |   </for>
                5 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_variable_first_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: array[string]}>
                  <for {item in items}>
                  </for>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: array[string]}>
                2 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_variable_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: array[string]}>
                  <for {item in items}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: array[string]}>
                2 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <bar-comp {p: string, s: string}>
                  <div>
                  	{s}
                  </div>
                </bar-comp>
            "#},
            expect![[r#"
                error: Unused variable p
                  --> main.hop (line 1, col 12)
                1 | <bar-comp {p: string, s: string}>
                  |            ^
            "#]],
        );
    }

    #[test]
    fn test_iterate_over_boolean_property() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: array[object[k: boolean]]}>
                	<for {item in params}>
                		<if {item.k}>
                		</if>
                	</for>
                	<for {item in params}>
                		<for {inner in item.k}>
                			<div>{inner}</div>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                error: Can not iterate over boolean
                  --> main.hop (line 7, col 18)
                 6 |     <for {item in params}>
                 7 |         <for {inner in item.k}>
                   |                        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_iterate_over_boolean_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: boolean}>
                	<if {params}>
                	</if>
                	<for {item in params}>
                		<div>{item}</div>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                error: Can not iterate over boolean
                  --> main.hop (line 4, col 16)
                3 |     </if>
                4 |     <for {item in params}>
                  |                   ^^^^^^
            "#]],
        );
    }

    // When successful, the typechecker identifies the parameter type of the component as well as the defined slots.
    #[test]
    fn test_successful_typechecking_with_complex_params() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[items: array[object[active: boolean, name: boolean]]}>
                	<for {item in params.items}>
                		<if {item.active}>
                		</if>
                		<if {item.name}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                error: Expected ',' or ']' after property type
                  --> main.hop (line 1, col 80)
                1 | <main-comp {params: object[items: array[object[active: boolean, name: boolean]]}>
                  |                                                                                ^
            "#]],
        );
    }

    // A component should be able to define the <head> tag and define a slot for the <title> tag.
    #[test]
    fn test_head_tag_with_title_slot() {
        check(
            indoc! {r#"
                -- main.hop --
                <custom-head>
                	<head>
                		<title><slot-title/></title>
                	</head>
                </custom-head>
            "#},
            expect![[r#"
                main::custom-head
                	{}
                	["title"]
            "#]],
        );
    }

    #[test]
    fn test_boolean_equality_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[a: string, b: boolean]}>
                  <if {(params.a == 'str') == params.b}>
                    <div>Match</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[a: string, b: boolean]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[enabled: boolean, users: array[object[profile: object[verified: boolean], posts: array[object[published: boolean]]]]]}>
                	<if {params.enabled}>
                		<for {user in params.users}>
                			<if {user.profile.verified}>
                				<for {post in user.posts}>
                					<if {post.published}>
                					  published
                					</if>
                				</for>
                			</if>
                		</for>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[enabled: boolean, users: array[object[posts: array[object[published: boolean]], profile: object[verified: boolean]]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_structure_2() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[sections: array[object[header: object[visible: boolean], items: array[object[data: object[valid: boolean]]]]]}>
                	<for {section in params.sections}>
                		<if {section.header.visible}>
                			<for {item in section.items}>
                				<if {item.data.valid}>
                				</if>
                			</for>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                error: Expected ',' or ']' after property type
                  --> main.hop (line 1, col 137)
                 1 | <main-comp {params: object[sections: array[object[header: object[visible: boolean], items: array[object[data: object[valid: boolean]]]]]}>
                   |                                                                                                                                         ^
            "#]],
        );
    }

    #[test]
    fn test_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[i: object[j: object[k: object[l: boolean]]]]}>
                	<if {params.i.j.k.l}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[i: object[j: object[k: object[l: boolean]]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_very_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[app: object[ui: object[theme: object[dark: boolean]], api: object[endpoints: object[users: object[enabled: boolean]]], database: object[connection: object[ssl: boolean]]]]}>
                	<if {params.app.ui.theme.dark}>
                	</if>
                	<if {params.app.api.endpoints.users.enabled}>
                	</if>
                	<if {params.app.database.connection.ssl}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[app: object[api: object[endpoints: object[users: object[enabled: boolean]]], database: object[connection: object[ssl: boolean]], ui: object[theme: object[dark: boolean]]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_with_entrypoint_script_and_style() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp entrypoint>
                    <script>
                        console.log("test");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_with_entrypoint_and_data() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp entrypoint {data: object[message: string]}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: object[message: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_string_comparisons() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[user: object[name: string], other_user: object[name: string], data: object[x: string, y: string]]}>
                  <if {params.user.name == params.other_user.name}>
                    <div>Same name</div>
                  </if>
                  <if {(params.data.x == params.data.y)}>
                    <div>Parentheses work</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[data: object[x: string, y: string], other_user: object[name: string], user: object[name: string]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_simple_string_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[x: string, y: string]}>
                  <if {params.x == params.y}>
                    <div>Values are equal</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[x: string, y: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_nested_array_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[foo: object[bar: array[boolean]]]}>
                	<for {j in params.foo.bar}>
                		<if {j}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[foo: object[bar: array[boolean]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_multiple_loops_same_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: array[object[a: boolean, b: boolean]]}>
                	<for {j in params}>
                		<if {j.a}>
                		</if>
                	</for>
                	<for {j in params}>
                		<if {j.b}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: array[object[a: boolean, b: boolean]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_nested_array_iteration() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {i: array[array[boolean]]}>
                	<for {j in i}>
                		<for {k in j}>
                			<if {k}>
                			</if>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{i: array[array[boolean]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_simple_array_iteration() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {i: array[boolean]}>
                	<for {j in i}>
                		<if {j}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{i: array[boolean]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_import_with_parameters() {
        check(
            indoc! {r#"
                -- utils.hop --
                <button-comp {text: string}>
                  <div>{text}</div>
                </button-comp>

                -- main.hop --
                <import component="button-comp" from="utils">

                <main-comp {label: string}>
                  <button-comp {text: label}/>
                </main-comp>
            "#},
            expect![[r#"
                utils::button-comp
                	{text: string}
                	[]

                main::main-comp
                	{label: string}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_multi_module_component_chain() {
        check(
            indoc! {r#"
                -- bar.hop --
                <widget-comp {config: object[enabled: boolean, title: string]}>
                  <if {config.enabled}>
                    <div>{config.title}</div>
                  </if>
                </widget-comp>

                -- foo.hop --
                <import component="widget-comp" from="bar">

                <panel-comp {data: object[items: array[object[enabled: boolean, title: string]]]}>
                  <for {item in data.items}>
                    <widget-comp {config: item}/>
                  </for>
                </panel-comp>

                -- main.hop --
                <import component="panel-comp" from="foo">

                <main-comp {settings: object[dashboard: object[items: array[object[enabled: boolean, title: string]]]]}>
                  <panel-comp {data: settings.dashboard}/>
                </main-comp>
            "#},
            expect![[r#"
                bar::widget-comp
                	{config: object[enabled: boolean, title: string]}
                	[]

                foo::panel-comp
                	{data: object[items: array[object[enabled: boolean, title: string]]]}
                	[]

                main::main-comp
                	{settings: object[dashboard: object[items: array[object[enabled: boolean, title: string]]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_simple_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: string}>
                	<div>{params}</div>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: string}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_data_structure() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[config: object[debug: boolean], data: array[object[id: boolean, attributes: array[boolean]]]]]}>
                	<if {params.config.debug}>
                	</if>
                	<for {item in params.data}>
                		<if {item.id}>
                		</if>
                		<for {attr in item.attributes}>
                			<if {attr}>
                			</if>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                error: Unexpected token after parameter
                  --> main.hop (line 1, col 121)
                 1 | <main-comp {params: object[config: object[debug: boolean], data: array[object[id: boolean, attributes: array[boolean]]]]]}>
                   |                                                                                                                         ^
            "#]],
        );
    }

    #[test]
    fn test_component_chain_with_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <step3-comp {settings: object[enabled: boolean]}>
                	<if {settings.enabled}>
                	</if>
                </step3-comp>

                <step2-comp {config: object[settings: object[enabled: boolean]]}>
                	<step3-comp {settings: config.settings}/>
                </step2-comp>

                <step1-comp {data: object[config: object[settings: object[enabled: boolean]]]}>
                	<step2-comp {config: data.config}/>
                </step1-comp>

                <main-comp {params: object[config: object[settings: object[enabled: boolean]]]}>
                	<step1-comp {data: params}/>
                </main-comp>
            "#},
            expect![[r#"
                main::step3-comp
                	{settings: object[enabled: boolean]}
                	[]

                main::step2-comp
                	{config: object[settings: object[enabled: boolean]]}
                	[]

                main::step1-comp
                	{data: object[config: object[settings: object[enabled: boolean]]]}
                	[]

                main::main-comp
                	{params: object[config: object[settings: object[enabled: boolean]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_three_level_component_hierarchy() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-card {item: object[title: string, active: boolean, status: string]}>
                  <div>{item.title}
                  </div>
                  <if {item.active}>
                    <span>{item.status}
                    </span>
                  </if>
                </main-card>

                <main-list {items: array[object[title: string, active: boolean, status: string]]}>
                  <for {item in items}>
                    <main-card {item: item}/>
                  </for>
                </main-list>

                <main-comp {data: object[items: array[object[title: string, active: boolean, status: string]]]}>
                  <main-list {items: data.items}/>
                </main-comp>
            "#},
            expect![[r#"
                main::main-card
                	{item: object[active: boolean, status: string, title: string]}
                	[]

                main::main-list
                	{items: array[object[active: boolean, status: string, title: string]]}
                	[]

                main::main-comp
                	{data: object[items: array[object[active: boolean, status: string, title: string]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_object_property_name_collision() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[i: object[j: object[k: object[l: boolean]], k: boolean]]}>
                	<if {params.i.j.k.l}>
                		<if {params.i.k}>
                		</if>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[i: object[j: object[k: object[l: boolean]], k: boolean]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_object_property_separate_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[i: object[j: object[k: object[l: boolean]], k: boolean]]}>
                	<if {params.i.j.k.l}>
                	</if>
                	<if {params.i.k}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[i: object[j: object[k: object[l: boolean]], k: boolean]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_multiple_array_properties() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[tags: array[boolean], categories: array[boolean], metadata: object[title: boolean]]}>
                	<for {tag in params.tags}>
                		<if {tag}>
                		</if>
                	</for>
                	<for {category in params.categories}>
                		<if {category}>
                		</if>
                	</for>
                	<if {params.metadata.title}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[categories: array[boolean], metadata: object[title: boolean], tags: array[boolean]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_simple_component_chain() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-bar {p: boolean}>
                  <if {p}>
                  </if>
                </main-bar>

                <main-foo {p: boolean}>
                  <main-bar {p: p}/>
                </main-foo>

                <main-comp {i: boolean}>
                  <main-foo {p: i}/>
                </main-comp>
            "#},
            expect![[r#"
                main::main-bar
                	{p: boolean}
                	[]

                main::main-foo
                	{p: boolean}
                	[]

                main::main-comp
                	{i: boolean}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_workflow_execution_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                <execute-step {step: object[condition: boolean]}>
                	<if {step.condition}>
                	</if>
                </execute-step>

                <execute-workflow {workflow: object[enabled: boolean, steps: array[object[condition: boolean]]]}>
                	<if {workflow.enabled}>
                		<for {step in workflow.steps}>
                			<execute-step {step: step}/>
                		</for>
                	</if>
                </execute-workflow>

                <main-comp {params: object[workflows: array[object[enabled: boolean, steps: array[object[condition: boolean]]]]]}>
                	<for {workflow in params.workflows}>
                		<execute-workflow {workflow: workflow}/>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::execute-step
                	{step: object[condition: boolean]}
                	[]

                main::execute-workflow
                	{workflow: object[enabled: boolean, steps: array[object[condition: boolean]]]}
                	[]

                main::main-comp
                	{params: object[workflows: array[object[enabled: boolean, steps: array[object[condition: boolean]]]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_two_component_chain() {
        check(
            indoc! {r#"
                -- main.hop --
                <foo-comp {p: boolean}>
                  <if {p}>
                  </if>
                </foo-comp>

                <main-comp {i: boolean}>
                  <foo-comp {p: i}/>
                </main-comp>
            "#},
            expect![[r#"
                main::foo-comp
                	{p: boolean}
                	[]

                main::main-comp
                	{i: boolean}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_process_item_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                <process-item {item: object[children: array[object[visible: boolean]], status: object[active: boolean]]}>
                	<if {item.status.active}>
                	</if>
                	<for {child in item.children}>
                		<if {child.visible}>
                		</if>
                	</for>
                </process-item>

                <main-comp {params: object[items: array[object[children: array[object[visible: boolean]], status: object[active: boolean]]]]}>
                	<for {item in params.items}>
                		<process-item {item: item}/>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::process-item
                	{item: object[children: array[object[visible: boolean]], status: object[active: boolean]]}
                	[]

                main::main-comp
                	{params: object[items: array[object[children: array[object[visible: boolean]], status: object[active: boolean]]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_set_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: object[url: string, theme: string]}>
                  <a set-href="user.url" set-class="user.theme">Link</a>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{user: object[theme: string, url: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_slot_usage() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <strong>
                        <slot-data></slot-data>
                    </strong>
                </main-comp>

                <bar-comp>
                    <main-comp>
                        <with-data>
                            Here's the content for the 'data' slot
                        </with-data>
                    </main-comp>
                </bar-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	["data"]

                main::bar-comp
                	{}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_simple_text_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {data: object[message: string]}>
                  <div>{data.message}
                  </div>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: object[message: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_string_comparison_conditional() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[role: string]}>
                  <if {params.role == 'admin'}>
                    <div>Admin</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[role: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_triple_nested_array() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: array[array[array[boolean]]]}>
                	<for {level1 in params}>
                		<for {level2 in level1}>
                			<for {level3 in level2}>
                				<if {level3}>
                				</if>
                			</for>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: array[array[array[boolean]]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_array_used_as_object_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: array[string]}>
                	<for {x in params}>
                		{x}
                	</for>
                	<for {y in params.foo}>
                		{y}
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                error: array[string] can not be used as an object
                  --> main.hop (line 5, col 13)
                4 |     </for>
                5 |     <for {y in params.foo}>
                  |                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_hop_x_raw_simple() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {data: object[message: string]}>
                	<hop-x-raw>foo bar</hop-x-raw>
                	<div>{data.message}</div>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: object[message: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_hop_x_raw_with_html() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<hop-x-raw>
                		<div>some html</div>
                		<p>more content</p>
                	</hop-x-raw>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	[]
            "#]],
        );
    }

    // Content inside <hop-x-raw> tags are not parsed nor typechecked.
    #[test]
    fn test_hop_x_raw_not_typechecked() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<hop-x-raw>
                		<undefined-component {nonexistent.field}>
                			<another-undefined {also.nonexistent} />
                		</undefined-component>
                	</hop-x-raw>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	[]
            "#]],
        );
    }

    // Test <if> tag with boolean expression.
    #[test]
    fn test_if_with_boolean_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: object[isActive: boolean]}>
                  <if {user.isActive}>
                    <div>User is active</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{user: object[isActive: boolean]}
                	[]
            "#]],
        );
    }

    // Test <if> tag with comparison expression.
    #[test]
    fn test_if_with_comparison_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {data: object[status: string]}>
                  <if {data.status == 'approved'}>
                    <div>Status is approved</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: object[status: string]}
                	[]
            "#]],
        );
    }

    // Comparison only works when the types of each operand are equal.
    #[test]
    fn test_comparison_type_mismatch_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <if {1 == 'approved'}>
                    <div>Status is approved</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                error: Can not compare number to string
                  --> main.hop (line 2, col 8)
                1 | <main-comp>
                2 |   <if {1 == 'approved'}>
                  |        ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Test <if> tag with nested conditionals.
    #[test]
    fn test_if_with_nested_conditionals() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {config: object[enabled: boolean, debug: boolean]}>
                  <if {config.enabled}>
                    <div>Feature enabled</div>
                    <if {config.debug}>
                      <div>Debug mode on</div>
                    </if>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{config: object[debug: boolean, enabled: boolean]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_argument_type_mismatch() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {config: object[debug: boolean]}>
                  <if {config.debug}>
                    <div>Debug mode on</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {config: 1}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Argument 'config' of type number is incompatible with expected type object[debug: boolean]
                  --> main.hop (line 7, col 15)
                6 | <foo-comp>
                7 |   <main-comp {config: 1}/>
                  |               ^^^^^^^^^
            "#]],
        );
    }

    // Test HOP_MODE global variable is available and has string type.
    #[test]
    fn test_hop_mode_global_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div>{HOP_MODE}</div>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	[]
            "#]],
        );
    }

    // Test HOP_MODE can be used in conditions.
    #[test]
    fn test_hop_mode_in_conditions() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <if {HOP_MODE == 'dev'}>
                    <div>Development mode</div>
                  </if>
                  <if {HOP_MODE == 'build'}>
                    <div>Build mode</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	[]
            "#]],
        );
    }

    // Test type inference works when comparing variables to HOP_MODE.
    #[test]
    fn test_hop_mode_type_inference() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[mode: string]}>
                  <if {params.mode == HOP_MODE}>
                    <div>Mode matches</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[mode: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_object_equality_and_string_equality() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: object[foo: string]}>
                  <if {params.foo == 'foo'}>
                    eq 1
                  </if>
                  <if {params == params}>
                    eq 2
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: object[foo: string]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_nested_data_structure_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {data: array[object[title: string, items: array[string]]]}>
                	<for {section in data}>
                		<h1>{section.title}</h1>
                		<for {item in section.items}>
                			<div>{item}</div>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: array[object[items: array[string], title: string]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_number() {
        check(
            indoc! {r#"
                -- main.hop --
                <string-comp {message: string}>
                	<div>{message}</div>
                </string-comp>
                <main-comp>
                	<string-comp {message: 42}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'message' of type number is incompatible with expected type string
                  --> main.hop (line 5, col 16)
                4 | <main-comp>
                5 |     <string-comp {message: 42}/>
                  |                   ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_object() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: object[name: string, age: string]}>
                	<div>{user.name} ({user.age})</div>
                </user-comp>
                <main-comp>
                	<user-comp {user: 'invalid'}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'user' of type string is incompatible with expected type object[age: string, name: string]
                  --> main.hop (line 5, col 14)
                4 | <main-comp>
                5 |     <user-comp {user: 'invalid'}/>
                  |                 ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_correct_object_passing() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: object[name: string, active: string]}>
                	<div>{user.name}: {user.active}</div>
                </user-comp>
                <main-comp {data: object[profile: object[name: string, active: string]]}>
                	<user-comp {user: data.profile}/>
                </main-comp>
            "#},
            expect![[r#"
                main::user-comp
                	{user: object[active: string, name: string]}
                	[]

                main::main-comp
                	{data: object[profile: object[active: string, name: string]]}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_string() {
        check(
            indoc! {r#"
                -- main.hop --
                <new-comp {user: object[name: string]}>
                	<div>{user.name}</div>
                </new-comp>
                <main-comp>
                	<new-comp {user: 'invalid'}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'user' of type string is incompatible with expected type object[name: string]
                  --> main.hop (line 5, col 13)
                4 | <main-comp>
                5 |     <new-comp {user: 'invalid'}/>
                  |                ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Component with explicit string parameter type.
    #[test]
    fn test_explicit_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <string-comp {message: string}>
                	<div>{message}</div>
                </string-comp>
            "#},
            expect![[r#"
                main::string-comp
                	{message: string}
                	[]
            "#]],
        );
    }

    // Component with explicit object parameter type.
    #[test]
    fn test_explicit_object_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: object[name: string]}>
                	<div>{user.name}</div>
                </user-comp>
            "#},
            expect![[r#"
                main::user-comp
                	{user: object[name: string]}
                	[]
            "#]],
        );
    }

    // Component with explicit array parameter type.
    #[test]
    fn test_explicit_array_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <list-comp {items: array[string]}>
                	<for {item in items}>
                		<div>{item}</div>
                	</for>
                </list-comp>
            "#},
            expect![[r#"
                main::list-comp
                	{items: array[string]}
                	[]
            "#]],
        );
    }

    // Component with explicit boolean parameter type.
    #[test]
    fn test_explicit_boolean_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <toggle-comp {enabled: boolean}>
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                </toggle-comp>
            "#},
            expect![[r#"
                main::toggle-comp
                	{enabled: boolean}
                	[]
            "#]],
        );
    }

    // Component with explicit number parameter type.
    #[test]
    fn test_explicit_number_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <counter-comp {count: number}>
                	<if {count == 0}>
                		<div>Zero</div>
                	</if>
                </counter-comp>
            "#},
            expect![[r#"
                main::counter-comp
                	{count: number}
                	[]
            "#]],
        );
    }

    // Component with nested object parameter type.
    #[test]
    fn test_nested_object_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <profile-comp {profile: object[user: object[name: string, age: number]]}>
                	<div>{profile.user.name}</div>
                	<if {profile.user.age == 25}>
                		<div>Quarter century</div>
                	</if>
                </profile-comp>
            "#},
            expect![[r#"
                main::profile-comp
                	{profile: object[user: object[age: number, name: string]]}
                	[]
            "#]],
        );
    }

    // Component with nested array parameter type.
    #[test]
    fn test_nested_array_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <matrix-comp {matrix: array[array[number]]}>
                	<for {row in matrix}>
                		<for {cell in row}>
                			<if {cell == 1}>
                				<span>One</span>
                			</if>
                		</for>
                	</for>
                </matrix-comp>
            "#},
            expect![[r#"
                main::matrix-comp
                	{matrix: array[array[number]]}
                	[]
            "#]],
        );
    }

    // Component with complex object with multiple properties.
    #[test]
    fn test_complex_object_multiple_properties() {
        check(
            indoc! {r#"
                -- main.hop --
                <card-comp {data: object[title: string, content: string, tags: array[string], metadata: object[author: string, published: boolean]]}>
                	<h1>{data.title}</h1>
                	<p>{data.content}</p>
                	<div>{data.metadata.author}</div>
                	<if {data.metadata.published}>
                		<span>Published</span>
                	</if>
                	<for {tag in data.tags}>
                		<span>{tag}</span>
                	</for>
                </card-comp>
            "#},
            expect![[r#"
                main::card-comp
                	{data: object[content: string, metadata: object[author: string, published: boolean], tags: array[string], title: string]}
                	[]
            "#]],
        );
    }

    // Error when passing wrong type to boolean parameter.
    #[test]
    fn test_wrong_type_to_boolean_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <toggle-comp {enabled: boolean}>
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                </toggle-comp>
                <main-comp>
                	<toggle-comp {enabled: 'not a boolean'}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'enabled' of type string is incompatible with expected type boolean
                  --> main.hop (line 7, col 16)
                6 | <main-comp>
                7 |     <toggle-comp {enabled: 'not a boolean'}/>
                  |                   ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Multi-file component with complex parameter types.
    #[test]
    fn test_multi_file_complex_parameters() {
        check(
            indoc! {r#"
                -- item-display.hop --
                <item-display {item: object[id: number, name: string, active: boolean]}>
                	<div>{item.name}</div>
                	<if {item.active}>
                		<span>Active item</span>
                	</if>
                	<if {item.id == 1}>
                		<span>First item</span>
                	</if>
                </item-display>
                -- data-list.hop --
                <import component="item-display" from="item-display">
                <data-list {items: array[object[id: number, name: string, active: boolean]]}>
                	<for {item in items}>
                		<item-display {item: item}/>
                	</for>
                </data-list>
                -- main.hop --
                <import component="data-list" from="data-list">
                <main-comp {items: array[object[id: number, name: string, active: boolean]]}>
                	<data-list {items: items}/>
                </main-comp>
            "#},
            expect![[r#"
                item-display::item-display
                	{item: object[active: boolean, id: number, name: string]}
                	[]

                data-list::data-list
                	{items: array[object[active: boolean, id: number, name: string]]}
                	[]

                main::main-comp
                	{items: array[object[active: boolean, id: number, name: string]]}
                	[]
            "#]],
        );
    }

    // Structural subtyping should work: an object with properties a and b should be compatible with a component that only needs property a.
    #[test]
    fn test_structural_subtyping_success() {
        check(
            indoc! {r#"
                -- main.hop --
                <needs-a {data: object[a: string]}>
                	<div>{data.a}</div>
                </needs-a>

                <main-comp {params: object[data: object[a: string, b: string]]}>
                	<needs-a {data: params.data}>
                	</needs-a>
                </main-comp>
            "#},
            expect![[r#"
                main::needs-a
                	{data: object[a: string]}
                	[]

                main::main-comp
                	{params: object[data: object[a: string, b: string]]}
                	[]
            "#]],
        );
    }

    // Typechecker should print an error if an object lacks required properties.
    #[test]
    fn test_structural_subtyping_missing_property() {
        check(
            indoc! {r#"
                -- main.hop --
                <needs-a {data: object[a: string]}>
                	<div>{data.a}</div>
                </needs-a>

                <main-comp {params: object[data: object[b: string]]}>
                	<needs-a {data: params.data}>
                	</needs-a>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'data' of type object[b: string] is incompatible with expected type object[a: string]
                  --> main.hop (line 6, col 12)
                5 | <main-comp {params: object[data: object[b: string]]}>
                6 |     <needs-a {data: params.data}>
                  |               ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
