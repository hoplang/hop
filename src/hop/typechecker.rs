use crate::common::{Range, RangeError, Ranged};
use crate::dop::{DopType, infer_type_from_json_file, is_subtype, typecheck_expr};
use crate::hop::ast::HopAST;
use crate::hop::ast::{ComponentDefinitionNode, HopNode, ImportNode, RenderNode};
use crate::hop::environment::Environment;
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub range: Range,
    pub typ: DopType,
    pub name: String,
}

impl Ranged for TypeAnnotation {
    fn range(&self) -> Range {
        self.range
    }
}

/// A definition link contains information of where a symbol is defined.
#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionLink {
    pub definition_module: String,
    pub definition_range: Range,
    pub reference_range: Range,
}

impl Ranged for DefinitionLink {
    fn range(&self) -> Range {
        self.reference_range
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentTypeInformation {
    // Track where the component is defined to be able to create DefinitionLinks
    // for ComponentReferences.
    definition_module: String,
    definition_range: Range,
    parameter_types: BTreeMap<String, DopType>,
    slots: Vec<String>,
}

pub fn typecheck(
    module: &HopAST,
    import_type_information: &HashMap<String, HashMap<String, ComponentTypeInformation>>,
    errors: &mut Vec<RangeError>,
    type_annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
) -> HashMap<String, ComponentTypeInformation> {
    let mut current_module_type_information = HashMap::new();

    for ImportNode {
        from_attr,
        component_attr,
        range,
        ..
    } in module.get_import_nodes()
    {
        let from_module = &from_attr.value;
        let component_name = &component_attr.value;

        if let Some(module_type_info) = import_type_information.get(from_module) {
            if let Some(comp_info) = module_type_info.get(component_name) {
                current_module_type_information.insert(component_name.clone(), comp_info.clone());
            } else {
                errors.push(RangeError::undeclared_component(
                    from_module,
                    component_name,
                    *range,
                ));
            }
        } else {
            errors.push(RangeError::import_from_undefined_module(
                from_module,
                *range,
            ));
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
        opening_name_range,
        ..
    } in module.get_component_definition_nodes()
    {
        let mut parameter_types = BTreeMap::new();

        for param in params {
            let param_name = &param.var_name.value;
            let param_type = param.type_annotation.clone();

            type_annotations.push(TypeAnnotation {
                range: param.var_name.range,
                typ: param_type.clone(),
                name: param.var_name.value.clone(),
            });
            env.push(param.var_name.value.clone(), param_type.clone());
            parameter_types.insert(param_name.clone(), param_type);
        }

        for child in children {
            typecheck_node(
                child,
                &current_module_type_information,
                &mut env,
                type_annotations,
                definition_links,
                errors,
            );
        }

        // Check for unused variables (iterate in reverse to match push order)
        for param in params.iter().rev() {
            if !env.pop() {
                errors.push(RangeError::unused_variable(
                    &param.var_name.value,
                    param.var_name.range,
                ));
            }
        }

        current_module_type_information.insert(
            name.clone(),
            ComponentTypeInformation {
                parameter_types,
                slots: slots.clone(),
                definition_module: module.name.clone(),
                definition_range: *opening_name_range,
            },
        );

        if let Some(preview_nodes) = preview {
            for param in params {
                env.push(param.var_name.value.clone(), param.type_annotation.clone());
            }
            for child in preview_nodes {
                typecheck_node(
                    child,
                    &current_module_type_information,
                    &mut env,
                    type_annotations,
                    definition_links,
                    errors,
                );
            }
            // Pop parameters
            for _ in params {
                env.pop();
            }
        }
    }

    for RenderNode { children, .. } in module.get_render_nodes() {
        for child in children {
            typecheck_node(
                child,
                &current_module_type_information,
                &mut env,
                type_annotations,
                definition_links,
                errors,
            );
        }
    }

    current_module_type_information
}

fn typecheck_node(
    node: &HopNode,
    component_info: &HashMap<String, ComponentTypeInformation>,
    env: &mut Environment<DopType>,
    annotations: &mut Vec<TypeAnnotation>,
    component_definition_links: &mut Vec<DefinitionLink>,
    errors: &mut Vec<RangeError>,
) {
    match node {
        HopNode::If {
            condition,
            children,
            range,
            ..
        } => {
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
                    component_definition_links,
                    errors,
                );
            }
        }
        HopNode::ComponentReference {
            component,
            args,
            children,
            opening_name_range,
            closing_name_range,
            range,
            ..
        } => {
            if let Some(comp_info) = component_info.get(component) {
                // Add definition link for go-to-definition
                component_definition_links.push(DefinitionLink {
                    reference_range: *opening_name_range,
                    definition_module: comp_info.definition_module.clone(),
                    definition_range: comp_info.definition_range,
                });
                if let Some(range) = closing_name_range {
                    component_definition_links.push(DefinitionLink {
                        reference_range: *range,
                        definition_module: comp_info.definition_module.clone(),
                        definition_range: comp_info.definition_range,
                    });
                }

                // Validate named arguments against parameter types
                let provided_args: std::collections::HashSet<_> =
                    args.iter().map(|arg| &arg.var_name.value).collect();
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
                for arg in args {
                    if let Some(expected_type) = comp_info.parameter_types.get(&arg.var_name.value)
                    {
                        let expr_type =
                            match typecheck_expr(&arg.expression, env, annotations, errors) {
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
                                    arg.var_name.value, expr_type, expected_type,
                                ),
                                arg.expression.range(),
                            ));
                        } else {
                            annotations.push(TypeAnnotation {
                                range: arg.expression.range(),
                                typ: expr_type,
                                name: format!("component parameter '{}'", arg.var_name.value),
                            });
                        }
                    }
                }

                // Validate slots
                for child in children {
                    if let HopNode::SlotReference { name, range, .. } = child {
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
                    component_definition_links,
                    errors,
                );
            }
        }
        HopNode::NativeHTML {
            set_attributes,
            children,
            ..
        } => {
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
                    component_definition_links,
                    errors,
                );
            }
        }
        HopNode::SlotDefinition { children, .. }
        | HopNode::SlotReference { children, .. }
        | HopNode::XExec { children, .. }
        | HopNode::XRaw { children, .. }
        | HopNode::Error { children, .. } => {
            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    annotations,
                    component_definition_links,
                    errors,
                );
            }
        }
        HopNode::XLoadJson {
            file_attr,
            as_attr,
            children,
            range,
            ..
        } => {
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
                    component_definition_links,
                    errors,
                );
            }

            // Pop the JSON variable from scope
            if pushed && !env.pop() {
                errors.push(RangeError::unused_variable(var_name, *range));
            }
        }
        HopNode::For {
            var_name,
            array_expr,
            children,
            ..
        } => {
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
                    range: var_name.range,
                    typ: element_type.clone(),
                    name: var_name.value.clone(),
                });
            } else {
                errors.push(RangeError::variable_is_already_defined(
                    &var_name.value,
                    var_name.range,
                ));
            }

            // Typecheck children
            for child in children {
                typecheck_node(
                    child,
                    component_info,
                    env,
                    annotations,
                    component_definition_links,
                    errors,
                );
            }

            // Pop the loop variable from scope
            if pushed && !env.pop() {
                errors.push(RangeError::unused_variable(&var_name.value, var_name.range));
            }
        }
        HopNode::Text { .. } | HopNode::Doctype { .. } => {
            // No typechecking needed
        }
        HopNode::TextExpression {
            expression, range, ..
        } => {
            // Typecheck the expression and ensure it's a string
            let expr_type = match typecheck_expr(expression, env, annotations, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    return; // Skip further processing
                }
            };
            if !is_subtype(&expr_type, &DopType::String) {
                errors.push(RangeError::new(
                    format!("Expected string for text expression, got {}", expr_type),
                    *range,
                ));
            }
            annotations.push(TypeAnnotation {
                range: *range,
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
        let mut module_type_results: HashMap<String, HashMap<String, ComponentTypeInformation>> =
            HashMap::new();

        // Process all .hop files in the archive
        for file in archive.iter() {
            if !file.name.ends_with(".hop") {
                panic!("Got invalid file name")
            }
            let mut errors = Vec::new();
            let tokenizer = Tokenizer::new(file.content.trim());
            let module_name = file.name.trim_end_matches(".hop");
            let module = parse(module_name.to_string(), tokenizer, &mut errors);

            if !errors.is_empty() {
                panic!("Got parse errors: {:#?}", errors);
            }

            let mut type_annotations = Vec::new();
            let mut component_definition_links = Vec::new();
            let type_result = typecheck(
                &module,
                &module_type_results,
                &mut errors,
                &mut type_annotations,
                &mut component_definition_links,
            );

            if !errors.is_empty() {
                error_formatter.add_errors(
                    module_name.to_string(),
                    file.content.trim().to_string(),
                    errors,
                );
            } else {
                module_type_results.insert(module.name.clone(), type_result.clone());

                for c in module.get_component_definition_nodes() {
                    let component_info =
                        type_result.get(&c.name).expect("Component info not found");
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
                  |                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                  |                ^^^^^^^^^^^^^^^^^^^^^^^
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

    // A component may be imported without being used.
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
                other::foo-comp
                	{}
                	[]

                main::main-comp
                	{}
                	[]
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
                  |                ^^^^^^^^^^^^^^^^^^^^^^^^^^^

                error: Component other-comp is not defined
                  --> main.hop (line 3, col 13)
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                3 |     <h1>Hello, <other-comp></other-comp>!</h1>
                  |                ^^^^^^^^^^^^^^^^^^^^^^^^^
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

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: {foo: array[string]}}>
                  <for {items in items.foo}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: {foo: array[string]}}>
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
                <main-comp {items: {a: array[string], b: array[string]}}>
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
                <main-comp {params: array[{active: boolean}]}>
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
                <main-comp {params: array[{k: boolean}]}>
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
                <main-comp {params: {items: array[{active: boolean, name: boolean}]}}>
                	<for {item in params.items}>
                		<if {item.active}>
                		</if>
                		<if {item.name}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {items: array[{active: boolean, name: boolean}]}}
                	[]
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
                <main-comp {params: {a: string, b: boolean}}>
                  <if {(params.a == 'str') == params.b}>
                    <div>Match</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {a: string, b: boolean}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {enabled: boolean, users: array[{profile: {verified: boolean}, posts: array[{published: boolean}]}]}}>
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
                	{params: {enabled: boolean, users: array[{posts: array[{published: boolean}], profile: {verified: boolean}}]}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_structure_2() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {sections: array[{header: {visible: boolean}, items: array[{data: {valid: boolean}}]}]}}>
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
                main::main-comp
                	{params: {sections: array[{header: {visible: boolean}, items: array[{data: {valid: boolean}}]}]}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {i: {j: {k: {l: boolean}}}}}>
                	<if {params.i.j.k.l}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {i: {j: {k: {l: boolean}}}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_very_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {app: {ui: {theme: {dark: boolean}}, api: {endpoints: {users: {enabled: boolean}}}, database: {connection: {ssl: boolean}}}}}>
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
                	{params: {app: {api: {endpoints: {users: {enabled: boolean}}}, database: {connection: {ssl: boolean}}, ui: {theme: {dark: boolean}}}}}
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
                <main-comp entrypoint {data: {message: string}}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: {message: string}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_complex_string_comparisons() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {user: {name: string}, other_user: {name: string}, data: {x: string, y: string}}}>
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
                	{params: {data: {x: string, y: string}, other_user: {name: string}, user: {name: string}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_simple_string_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {x: string, y: string}}>
                  <if {params.x == params.y}>
                    <div>Values are equal</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {x: string, y: string}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_nested_array_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {foo: {bar: array[boolean]}}}>
                	<for {j in params.foo.bar}>
                		<if {j}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {foo: {bar: array[boolean]}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_multiple_loops_same_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: array[{a: boolean, b: boolean}]}>
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
                	{params: array[{a: boolean, b: boolean}]}
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
                <widget-comp {config: {enabled: boolean, title: string}}>
                  <if {config.enabled}>
                    <div>{config.title}</div>
                  </if>
                </widget-comp>

                -- foo.hop --
                <import component="widget-comp" from="bar">

                <panel-comp {data: {items: array[{enabled: boolean, title: string}]}}>
                  <for {item in data.items}>
                    <widget-comp {config: item}/>
                  </for>
                </panel-comp>

                -- main.hop --
                <import component="panel-comp" from="foo">

                <main-comp {settings: {dashboard: {items: array[{enabled: boolean, title: string}]}}}>
                  <panel-comp {data: settings.dashboard}/>
                </main-comp>
            "#},
            expect![[r#"
                bar::widget-comp
                	{config: {enabled: boolean, title: string}}
                	[]

                foo::panel-comp
                	{data: {items: array[{enabled: boolean, title: string}]}}
                	[]

                main::main-comp
                	{settings: {dashboard: {items: array[{enabled: boolean, title: string}]}}}
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
                <main-comp {params: {config: {debug: boolean}, data: array[{id: boolean, attributes: array[boolean]}]}}>
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
                main::main-comp
                	{params: {config: {debug: boolean}, data: array[{attributes: array[boolean], id: boolean}]}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_chain_with_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <step3-comp {settings: {enabled: boolean}}>
                	<if {settings.enabled}>
                	</if>
                </step3-comp>

                <step2-comp {config: {settings: {enabled: boolean}}}>
                	<step3-comp {settings: config.settings}/>
                </step2-comp>

                <step1-comp {data: {config: {settings: {enabled: boolean}}}}>
                	<step2-comp {config: data.config}/>
                </step1-comp>

                <main-comp {params: {config: {settings: {enabled: boolean}}}}>
                	<step1-comp {data: params}/>
                </main-comp>
            "#},
            expect![[r#"
                main::step3-comp
                	{settings: {enabled: boolean}}
                	[]

                main::step2-comp
                	{config: {settings: {enabled: boolean}}}
                	[]

                main::step1-comp
                	{data: {config: {settings: {enabled: boolean}}}}
                	[]

                main::main-comp
                	{params: {config: {settings: {enabled: boolean}}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_three_level_component_hierarchy() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-card {item: {title: string, active: boolean, status: string}}>
                  <div>{item.title}
                  </div>
                  <if {item.active}>
                    <span>{item.status}
                    </span>
                  </if>
                </main-card>

                <main-list {items: array[{title: string, active: boolean, status: string}]}>
                  <for {item in items}>
                    <main-card {item: item}/>
                  </for>
                </main-list>

                <main-comp {data: {items: array[{title: string, active: boolean, status: string}]}}>
                  <main-list {items: data.items}/>
                </main-comp>
            "#},
            expect![[r#"
                main::main-card
                	{item: {active: boolean, status: string, title: string}}
                	[]

                main::main-list
                	{items: array[{active: boolean, status: string, title: string}]}
                	[]

                main::main-comp
                	{data: {items: array[{active: boolean, status: string, title: string}]}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_object_property_name_collision() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {i: {j: {k: {l: boolean}}, k: boolean}}}>
                	<if {params.i.j.k.l}>
                		<if {params.i.k}>
                		</if>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {i: {j: {k: {l: boolean}}, k: boolean}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_object_property_separate_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {i: {j: {k: {l: boolean}}, k: boolean}}}>
                	<if {params.i.j.k.l}>
                	</if>
                	<if {params.i.k}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {i: {j: {k: {l: boolean}}, k: boolean}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_multiple_array_properties() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {tags: array[boolean], categories: array[boolean], metadata: {title: boolean}}}>
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
                	{params: {categories: array[boolean], metadata: {title: boolean}, tags: array[boolean]}}
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
                <execute-step {step: {condition: boolean}}>
                	<if {step.condition}>
                	</if>
                </execute-step>

                <execute-workflow {workflow: {enabled: boolean, steps: array[{condition: boolean}]}}>
                	<if {workflow.enabled}>
                		<for {step in workflow.steps}>
                			<execute-step {step: step}/>
                		</for>
                	</if>
                </execute-workflow>

                <main-comp {params: {workflows: array[{enabled: boolean, steps: array[{condition: boolean}]}]}}>
                	<for {workflow in params.workflows}>
                		<execute-workflow {workflow: workflow}/>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::execute-step
                	{step: {condition: boolean}}
                	[]

                main::execute-workflow
                	{workflow: {enabled: boolean, steps: array[{condition: boolean}]}}
                	[]

                main::main-comp
                	{params: {workflows: array[{enabled: boolean, steps: array[{condition: boolean}]}]}}
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
                <process-item {item: {children: array[{visible: boolean}], status: {active: boolean}}}>
                	<if {item.status.active}>
                	</if>
                	<for {child in item.children}>
                		<if {child.visible}>
                		</if>
                	</for>
                </process-item>

                <main-comp {params: {items: array[{children: array[{visible: boolean}], status: {active: boolean}}]}}>
                	<for {item in params.items}>
                		<process-item {item: item}/>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                main::process-item
                	{item: {children: array[{visible: boolean}], status: {active: boolean}}}
                	[]

                main::main-comp
                	{params: {items: array[{children: array[{visible: boolean}], status: {active: boolean}}]}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_set_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: {url: string, theme: string}}>
                  <a set-href="user.url" set-class="user.theme">Link</a>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{user: {theme: string, url: string}}
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
                <main-comp {data: {message: string}}>
                  <div>{data.message}
                  </div>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: {message: string}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_string_comparison_conditional() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {role: string}}>
                  <if {params.role == 'admin'}>
                    <div>Admin</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {role: string}}
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
                <main-comp {data: {message: string}}>
                	<hop-x-raw>foo bar</hop-x-raw>
                	<div>{data.message}</div>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: {message: string}}
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
                <main-comp {user: {isActive: boolean}}>
                  <if {user.isActive}>
                    <div>User is active</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{user: {isActive: boolean}}
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
                <main-comp {data: {status: string}}>
                  <if {data.status == 'approved'}>
                    <div>Status is approved</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{data: {status: string}}
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
                <main-comp {config: {enabled: boolean, debug: boolean}}>
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
                	{config: {debug: boolean, enabled: boolean}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_argument_type_mismatch() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {config: {debug: boolean}}>
                  <if {config.debug}>
                    <div>Debug mode on</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {config: 1}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Argument 'config' of type number is incompatible with expected type {debug: boolean}
                  --> main.hop (line 7, col 23)
                6 | <foo-comp>
                7 |   <main-comp {config: 1}/>
                  |                       ^
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
                <main-comp {params: {mode: string}}>
                  <if {params.mode == HOP_MODE}>
                    <div>Mode matches</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{params: {mode: string}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_object_equality_and_string_equality() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {foo: string}}>
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
                	{params: {foo: string}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_nested_data_structure_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {data: array[{title: string, items: array[string]}]}>
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
                	{data: array[{items: array[string], title: string}]}
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
                  --> main.hop (line 5, col 25)
                4 | <main-comp>
                5 |     <string-comp {message: 42}/>
                  |                            ^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_object() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: {name: string, age: string}}>
                	<div>{user.name} ({user.age})</div>
                </user-comp>
                <main-comp>
                	<user-comp {user: 'invalid'}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'user' of type string is incompatible with expected type {age: string, name: string}
                  --> main.hop (line 5, col 20)
                4 | <main-comp>
                5 |     <user-comp {user: 'invalid'}/>
                  |                       ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_correct_object_passing() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: {name: string, active: string}}>
                	<div>{user.name}: {user.active}</div>
                </user-comp>
                <main-comp {data: {profile: {name: string, active: string}}}>
                	<user-comp {user: data.profile}/>
                </main-comp>
            "#},
            expect![[r#"
                main::user-comp
                	{user: {active: string, name: string}}
                	[]

                main::main-comp
                	{data: {profile: {active: string, name: string}}}
                	[]
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_string() {
        check(
            indoc! {r#"
                -- main.hop --
                <new-comp {user: {name: string}}>
                	<div>{user.name}</div>
                </new-comp>
                <main-comp>
                	<new-comp {user: 'invalid'}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'user' of type string is incompatible with expected type {name: string}
                  --> main.hop (line 5, col 19)
                4 | <main-comp>
                5 |     <new-comp {user: 'invalid'}/>
                  |                      ^^^^^^^^^
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
                <user-comp {user: {name: string}}>
                	<div>{user.name}</div>
                </user-comp>
            "#},
            expect![[r#"
                main::user-comp
                	{user: {name: string}}
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
                <profile-comp {profile: {user: {name: string, age: number}}}>
                	<div>{profile.user.name}</div>
                	<if {profile.user.age == 25}>
                		<div>Quarter century</div>
                	</if>
                </profile-comp>
            "#},
            expect![[r#"
                main::profile-comp
                	{profile: {user: {age: number, name: string}}}
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
                <card-comp {data: {title: string, content: string, tags: array[string], metadata: {author: string, published: boolean}}}>
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
                	{data: {content: string, metadata: {author: string, published: boolean}, tags: array[string], title: string}}
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
                  --> main.hop (line 7, col 25)
                6 | <main-comp>
                7 |     <toggle-comp {enabled: 'not a boolean'}/>
                  |                            ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Multi-file component with complex parameter types.
    #[test]
    fn test_multi_file_complex_parameters() {
        check(
            indoc! {r#"
                -- item-display.hop --
                <item-display {item: {id: number, name: string, active: boolean}}>
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
                <data-list {items: array[{id: number, name: string, active: boolean}]}>
                	<for {item in items}>
                		<item-display {item: item}/>
                	</for>
                </data-list>
                -- main.hop --
                <import component="data-list" from="data-list">
                <main-comp {items: array[{id: number, name: string, active: boolean}]}>
                	<data-list {items: items}/>
                </main-comp>
            "#},
            expect![[r#"
                item-display::item-display
                	{item: {active: boolean, id: number, name: string}}
                	[]

                data-list::data-list
                	{items: array[{active: boolean, id: number, name: string}]}
                	[]

                main::main-comp
                	{items: array[{active: boolean, id: number, name: string}]}
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
                <needs-a {data: {a: string}}>
                	<div>{data.a}</div>
                </needs-a>

                <main-comp {params: {data: {a: string, b: string}}}>
                	<needs-a {data: params.data}>
                	</needs-a>
                </main-comp>
            "#},
            expect![[r#"
                main::needs-a
                	{data: {a: string}}
                	[]

                main::main-comp
                	{params: {data: {a: string, b: string}}}
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
                <needs-a {data: {a: string}}>
                	<div>{data.a}</div>
                </needs-a>

                <main-comp {params: {data: {b: string}}}>
                	<needs-a {data: params.data}>
                	</needs-a>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'data' of type {b: string} is incompatible with expected type {a: string}
                  --> main.hop (line 6, col 18)
                5 | <main-comp {params: {data: {b: string}}}>
                6 |     <needs-a {data: params.data}>
                  |                     ^^^^^^^^^^^
            "#]],
        );
    }
}
