use crate::common::{Range, Ranged, TypeError};
use crate::dop::{DopParameter, DopType, is_subtype, typecheck_expr};
use crate::hop::ast::HopAst;
use crate::hop::ast::{ComponentDefinition, HopNode, Import, Render};
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
    // Track the parameter types for the component.
    parameter_types: Option<(BTreeMap<String, DopParameter>, Range)>,
    // Track whether the component has a slot-default.
    has_slot: bool,
}

pub type TypeCheckerState = HashMap<String, ComponentTypeInformation>;

pub fn typecheck(
    module: &HopAst,
    import_type_information: &HashMap<String, TypeCheckerState>,
    errors: &mut Vec<TypeError>,
    type_annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
) -> HashMap<String, ComponentTypeInformation> {
    let mut current_module_type_information = HashMap::new();

    for Import {
        from_attr,
        component_attr,
        ..
    } in module.get_imports()
    {
        let from_module = &from_attr.value;
        let component_name = &component_attr.value;

        if let Some(module_type_info) = import_type_information.get(from_module) {
            if let Some(comp_info) = module_type_info.get(component_name) {
                current_module_type_information.insert(component_name.clone(), comp_info.clone());
            } else {
                errors.push(TypeError::undeclared_component(
                    from_module,
                    component_name,
                    component_attr.range,
                ));
            }
        } else {
            errors.push(TypeError::import_from_undefined_module(
                from_module,
                from_attr.range,
            ));
        }
    }
    let mut env = Environment::new();

    let _ = env.push("HOP_MODE".to_string(), DopType::String);

    for ComponentDefinition {
        name,
        params,
        children,
        has_slot,
        opening_name_range,
        ..
    } in module.get_component_definitions()
    {
        if let Some((params, _)) = params {
            for param in params.values() {
                type_annotations.push(TypeAnnotation {
                    range: param.var_name.range,
                    typ: param.type_annotation.clone(),
                    name: param.var_name.value.clone(),
                });
                let _ = env.push(param.var_name.value.clone(), param.type_annotation.clone());
            }
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

        if let Some((params, _)) = params {
            for _ in 0..params.len() {
                let (key, _, accessed) = env.pop();
                if !accessed {
                    let param = params.get(&key).unwrap();
                    errors.push(TypeError::unused_variable(
                        &param.var_name.value,
                        param.var_name.range,
                    ))
                }
            }
        }

        current_module_type_information.insert(
            name.clone(),
            ComponentTypeInformation {
                parameter_types: params.clone(),
                has_slot: *has_slot,
                definition_module: module.name.clone(),
                definition_range: *opening_name_range,
            },
        );
    }

    for Render { children, .. } in module.get_renders() {
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
    errors: &mut Vec<TypeError>,
) {
    match node {
        HopNode::If {
            condition,
            children,
            ..
        } => {
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
            let condition_type = match typecheck_expr(condition, env, annotations, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    return; // Skip further processing of this branch
                }
            };

            if !is_subtype(&condition_type, &DopType::Bool) {
                errors.push(TypeError::expected_boolean_condition(
                    &condition_type.to_string(),
                    condition.range(),
                ));
            }
        }

        HopNode::For {
            var_name,
            array_expr,
            children,
            ..
        } => {
            let array_type = match typecheck_expr(array_expr, env, annotations, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    return;
                }
            };
            let element_type = match &array_type {
                DopType::Array(Some(inner)) => *inner.clone(),
                DopType::Array(None) => {
                    errors.push(TypeError::cannot_iterate_empty_array(array_expr.range()));
                    return;
                }
                _ => {
                    errors.push(TypeError::cannot_iterate_over(
                        &array_type.to_string(),
                        array_expr.range(),
                    ));
                    return;
                }
            };

            // Push the loop variable into scope
            let mut pushed = false;
            match env.push(var_name.value.clone(), element_type.clone()) {
                Ok(_) => {
                    pushed = true;
                    annotations.push(TypeAnnotation {
                        range: var_name.range,
                        typ: element_type.clone(),
                        name: var_name.value.clone(),
                    });
                }
                Err(_) => {
                    errors.push(TypeError::variable_is_already_defined(
                        &var_name.value,
                        var_name.range,
                    ));
                }
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

            if pushed {
                let (_, _, accessed) = env.pop();
                if !accessed {
                    errors.push(TypeError::unused_variable(&var_name.value, var_name.range))
                }
            }
        }

        HopNode::ComponentReference {
            component,
            args,
            children,
            opening_name_range,
            closing_name_range,
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

                match (&comp_info.parameter_types, args) {
                    (None, None) => {}
                    (None, Some((_, args_range))) => {
                        errors.push(TypeError::unexpected_arguments(*args_range));
                    }
                    (Some((params, _)), None) => {
                        errors.push(TypeError::missing_arguments(params, *opening_name_range));
                    }
                    (Some((params, _)), Some((args, args_range))) => {
                        for param in params.values() {
                            if !args.contains_key(&param.var_name.value) {
                                errors.push(TypeError::missing_required_parameter(
                                    &param.var_name.value,
                                    *args_range,
                                ));
                            }
                        }

                        for arg in args.values() {
                            let param = match params.get(&arg.var_name.value) {
                                None => {
                                    errors.push(TypeError::unexpected_argument(
                                        &arg.var_name.value,
                                        arg.var_name.range,
                                    ));
                                    continue;
                                }
                                Some(param) => param,
                            };

                            let evaluated_arg_type =
                                match typecheck_expr(&arg.expression, env, annotations, errors) {
                                    Ok(t) => t,
                                    Err(err) => {
                                        errors.push(err);
                                        continue;
                                    }
                                };

                            if !is_subtype(&evaluated_arg_type, &param.type_annotation) {
                                errors.push(TypeError::argument_is_incompatible(
                                    &param.type_annotation.to_string(),
                                    &evaluated_arg_type.to_string(),
                                    &arg.var_name.value,
                                    arg.expression.range(),
                                ));
                                continue;
                            }

                            annotations.push(TypeAnnotation {
                                range: arg.expression.range(),
                                typ: evaluated_arg_type,
                                name: format!("component parameter '{}'", arg.var_name.value),
                            });
                        }
                    }
                }

                // Validate that content is only passed to components with slot-default
                if !children.is_empty() && !comp_info.has_slot {
                    errors.push(TypeError::undefined_slot(component, *opening_name_range));
                }
            } else {
                errors.push(TypeError::undefined_component(
                    component,
                    *opening_name_range,
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

        HopNode::Html {
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
                    errors.push(TypeError::expected_string_attribute(
                        &expr_type.to_string(),
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

        HopNode::TextExpression { expression, range } => {
            let expr_type = match typecheck_expr(expression, env, annotations, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    return;
                }
            };
            if !is_subtype(&expr_type, &DopType::String) {
                errors.push(TypeError::expected_string_expression(
                    &expr_type.to_string(),
                    *range,
                ));
                return;
            }
            annotations.push(TypeAnnotation {
                range: *range,
                typ: DopType::String,
                name: "text expression".to_string(),
            });
        }

        HopNode::XExec { children, .. }
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

        HopNode::SlotDefinition { .. } | HopNode::Text { .. } | HopNode::Doctype { .. } => {
            // No typechecking needed
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::tui::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    use std::collections::HashMap;

    fn check(archive_str: &str, expected: Expect) {
        let archive = Archive::from(archive_str);
        let mut error_output_parts = Vec::new();
        let mut all_output_lines = Vec::new();
        let mut module_type_results: HashMap<String, HashMap<String, ComponentTypeInformation>> =
            HashMap::new();
        let annotator = SourceAnnotator::new()
            .with_label("error")
            .with_lines_before(1)
            .with_location();

        // Process all .hop files in the archive
        for file in archive.iter() {
            if !file.name.ends_with(".hop") {
                panic!("Got invalid file name")
            }
            let mut parse_errors = Vec::new();
            let tokenizer = Tokenizer::new(file.content.trim());
            let module_name = file.name.trim_end_matches(".hop");
            let module = parse(module_name.to_string(), tokenizer, &mut parse_errors);

            if !parse_errors.is_empty() {
                panic!("Got parse errors: {:#?}", parse_errors);
            }

            let mut type_annotations = Vec::new();
            let mut component_definition_links = Vec::new();
            let mut type_errors = Vec::new();
            let type_result = typecheck(
                &module,
                &module_type_results,
                &mut type_errors,
                &mut type_annotations,
                &mut component_definition_links,
            );

            if !type_errors.is_empty() {
                let formatted_errors =
                    annotator.annotate(Some(&file.name), file.content.trim(), &type_errors);
                error_output_parts.push(formatted_errors);
            } else {
                module_type_results.insert(module.name.clone(), type_result.clone());

                for c in module.get_component_definitions() {
                    let component_info =
                        type_result.get(&c.name).expect("Component info not found");
                    let param_types_str = component_info
                        .parameter_types
                        .as_ref()
                        .map(|(params, _)| {
                            params
                                .iter()
                                .map(|(_, p)| {
                                    format!("{}: {}", p.var_name.value.clone(), p.type_annotation)
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        })
                        .unwrap_or("".to_string());
                    all_output_lines.push(format!(
                        "{}::{}\n\t{{{}}}\n\t{}\n",
                        module.name, c.name, param_types_str, component_info.has_slot
                    ));
                }
            }
        }

        let actual = if !error_output_parts.is_empty() {
            error_output_parts.join("\n")
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
                	false
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
                	<h1>Hello,
                        <render-bar>
                            <div></div>
                        </render-bar>
                    </h1>
                    <render-foo></render-foo>
                </main-comp>
            "#},
            expect![[r#"
                error: Component render-bar is not defined
                  --> main.hop (line 3, col 10)
                2 |     <h1>Hello,
                3 |         <render-bar>
                  |          ^^^^^^^^^^

                error: Component render-foo is not defined
                  --> main.hop (line 7, col 6)
                6 |     </h1>
                7 |     <render-foo></render-foo>
                  |      ^^^^^^^^^^
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
                  --> main.hop (line 2, col 14)
                1 | <main-comp>
                2 |     <h1>Hello, <main-comp></main-comp>!</h1>
                  |                 ^^^^^^^^^
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
                  --> main.hop (line 1, col 36)
                1 | <import component="foo-comp" from="other">
                  |                                    ^^^^^
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
                  --> main.hop (line 1, col 20)
                1 | <import component="foo-comp" from="other">
                  |                    ^^^^^^^^
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
                	false

                main::main-comp
                	{}
                	false
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
                  --> main.hop (line 2, col 14)
                1 | <main-comp>
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                  |                 ^^^^^^^^^^^

                error: Component other-comp is not defined
                  --> main.hop (line 3, col 14)
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                3 |     <h1>Hello, <other-comp></other-comp>!</h1>
                  |                 ^^^^^^^^^^
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
                	false

                main::foo-comp
                	{}
                	false
            "#]],
        );
    }

    // When content is passed to a component without slot-default, the typechecker outputs an error.
    #[test]
    fn test_undefined_slot_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <strong>No slot here</strong>
                </main-comp>

                <bar-comp>
                    <main-comp>
                        This component has no slot
                    </main-comp>
                </bar-comp>
            "#},
            expect![[r#"
                error: Component main-comp does not have a slot-default
                  --> main.hop (line 6, col 6)
                5 | <bar-comp>
                6 |     <main-comp>
                  |      ^^^^^^^^^
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
                    <strong>No slot here</strong>
                </foo-comp>
                -- main.hop --
                <import component="foo-comp" from="other">

                <bar-comp>
                    <foo-comp>
                        This component has no slot
                    </foo-comp>
                </bar-comp>
            "#},
            expect![[r#"
                error: Component foo-comp does not have a slot-default
                  --> main.hop (line 4, col 6)
                3 | <bar-comp>
                4 |     <foo-comp>
                  |      ^^^^^^^^
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

    // Arguments may be passed in any order to a component.
    #[test]
    fn test_component_handles_arguments_in_any_order() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: boolean, b: string}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {b: 'foo', a: true}/>
                </foo-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{a: boolean, b: string}
                	false

                main::foo-comp
                	{}
                	false
            "#]],
        );
    }

    // When a parameter is missing the typechecker reports an error.
    #[test]
    fn test_component_missing_required_parameter_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: boolean, b: string}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {b: 'foo'}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Missing required parameter 'a'
                  --> main.hop (line 7, col 15)
                6 | <foo-comp>
                7 |   <main-comp {b: 'foo'}/>
                  |               ^^^^^^^^
            "#]],
        );
    }

    // When the whole parameter expression is missing the typechecker reports an error.
    #[test]
    fn test_component_does_not_specify_required_parameters_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: boolean, b: string}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp />
                </foo-comp>
            "#},
            expect![[r#"
                error: Component requires arguments: a, b
                  --> main.hop (line 7, col 4)
                6 | <foo-comp>
                7 |   <main-comp />
                  |    ^^^^^^^^^
            "#]],
        );
    }

    // When arguments are specified but the component does not expect any the typechecker reports
    // an error.
    #[test]
    fn test_component_has_no_parameters_but_arguments_were_supplied_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  hello world
                </main-comp>
                <foo-comp>
                  <main-comp {a: 'foo'} />
                </foo-comp>
            "#},
            expect![[r#"
                error: Component does not accept arguments
                  --> main.hop (line 5, col 15)
                4 | <foo-comp>
                5 |   <main-comp {a: 'foo'} />
                  |               ^^^^^^^^
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
    fn test_iterate_over_boolean_parameter() {
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
                	false
            "#]],
        );
    }

    // A component should be able to define the <head> tag and a default slot.
    #[test]
    fn test_head_tag_with_default_slot() {
        check(
            indoc! {r#"
                -- main.hop --
                <custom-head>
                	<head>
                		<title><slot-default/></title>
                	</head>
                </custom-head>
            "#},
            expect![[r#"
                main::custom-head
                	{}
                	true
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false

                main::main-comp
                	{label: string}
                	false
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
                	false

                foo::panel-comp
                	{data: {items: array[{enabled: boolean, title: string}]}}
                	false

                main::main-comp
                	{settings: {dashboard: {items: array[{enabled: boolean, title: string}]}}}
                	false
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
                	false
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
                	false
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
                	false

                main::step2-comp
                	{config: {settings: {enabled: boolean}}}
                	false

                main::step1-comp
                	{data: {config: {settings: {enabled: boolean}}}}
                	false

                main::main-comp
                	{params: {config: {settings: {enabled: boolean}}}}
                	false
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
                	false

                main::main-list
                	{items: array[{active: boolean, status: string, title: string}]}
                	false

                main::main-comp
                	{data: {items: array[{active: boolean, status: string, title: string}]}}
                	false
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
                	false
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
                	false
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
                	false
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
                	false

                main::main-foo
                	{p: boolean}
                	false

                main::main-comp
                	{i: boolean}
                	false
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
                	false

                main::execute-workflow
                	{workflow: {enabled: boolean, steps: array[{condition: boolean}]}}
                	false

                main::main-comp
                	{params: {workflows: array[{enabled: boolean, steps: array[{condition: boolean}]}]}}
                	false
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
                	false

                main::main-comp
                	{i: boolean}
                	false
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
                	false

                main::main-comp
                	{params: {items: array[{children: array[{visible: boolean}], status: {active: boolean}}]}}
                	false
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
                	false
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
                        <slot-default/>
                    </strong>
                </main-comp>

                <bar-comp>
                    <main-comp>
                        Here's the content for the default slot
                    </main-comp>
                </bar-comp>
            "#},
            expect![[r#"
                main::main-comp
                	{}
                	true

                main::bar-comp
                	{}
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
            "#]],
        );
    }

    // When an argument type does not match a parameter type the typechecker
    // reports an error.
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false

                main::main-comp
                	{data: {profile: {active: string, name: string}}}
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false
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
                	false

                data-list::data-list
                	{items: array[{active: boolean, id: number, name: string}]}
                	false

                main::main-comp
                	{items: array[{active: boolean, id: number, name: string}]}
                	false
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
                	<needs-a {data: params.data}/>
                </main-comp>
            "#},
            expect![[r#"
                main::needs-a
                	{data: {a: string}}
                	false

                main::main-comp
                	{params: {data: {a: string, b: string}}}
                	false
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
                	<needs-a {data: params.data}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'data' of type {b: string} is incompatible with expected type {a: string}
                  --> main.hop (line 6, col 18)
                5 | <main-comp {params: {data: {b: string}}}>
                6 |     <needs-a {data: params.data}/>
                  |                     ^^^^^^^^^^^
            "#]],
        );
    }

    // Using a condition that is not a boolean should produce an error
    #[test]
    fn test_if_condition_must_be_boolean() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-component>
                    <if {'str'}>
                      is str?
                    </if>
                </main-component>
            "#},
            expect![[r#"
                error: Expected boolean condition, got string
                  --> main.hop (line 2, col 10)
                1 | <main-component>
                2 |     <if {'str'}>
                  |          ^^^^^
            "#]],
        );
    }

    // Passing an argument that is not in the parameter list should produce an error
    #[test]
    fn test_unexpected_argument_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: string}>
                  {a}
                </main-comp>
                <foo-comp>
                    <main-comp {a: '', b: 1}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Unexpected argument 'b'
                  --> main.hop (line 5, col 24)
                4 | <foo-comp>
                5 |     <main-comp {a: '', b: 1}/>
                  |                        ^
            "#]],
        );
    }

    // Type errors in argument list should be reported
    #[test]
    fn test_type_errors_in_argument_list() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: string, b: string}>
                  {a} {b}
                </main-comp>
                <foo-comp>
                    <main-comp {a: 1 == '', b: 1 == ''}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Can not compare number to string
                  --> main.hop (line 5, col 20)
                4 | <foo-comp>
                5 |     <main-comp {a: 1 == '', b: 1 == ''}/>
                  |                    ^^^^^^^

                error: Can not compare number to string
                  --> main.hop (line 5, col 32)
                4 | <foo-comp>
                5 |     <main-comp {a: 1 == '', b: 1 == ''}/>
                  |                                ^^^^^^^
            "#]],
        );
    }

    // Trying to iterate over an empty array should produce an error
    #[test]
    fn test_iterate_over_empty_array_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-component>
                    <for {x in []}>
                      ?
                    </for>
                </main-component>
            "#},
            expect![[r#"
                error: Cannot iterate over an empty array with unknown element type
                  --> main.hop (line 2, col 16)
                1 | <main-component>
                2 |     <for {x in []}>
                  |                ^^
            "#]],
        );
    }

    // Trying to render a non-string should produce an error
    #[test]
    fn test_render_non_string_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-component>
                    {false}
                </main-component>
            "#},
            expect![[r#"
                error: Expected string for text expression, got boolean
                  --> main.hop (line 2, col 5)
                1 | <main-component>
                2 |     {false}
                  |     ^^^^^^^
            "#]],
        );
    }
}
