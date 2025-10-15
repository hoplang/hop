use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::cased_string::CasedString;
use crate::dop::property_name::PropertyName;
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::{BTreeMap, BTreeSet};

pub struct GoTranspiler {}

impl GoTranspiler {
    pub fn new() -> Self {
        Self {}
    }

    fn extract_and_generate_nested_type<'a>(
        &self,
        param_type: &'a Type,
        base_name: &str,
        field_name: &str,
        generated_types: &mut Vec<(String, BoxDoc<'a>)>,
    ) -> BoxDoc<'a> {
        match param_type {
            Type::Object(fields) => {
                // Generate the type name for this struct
                let type_name = format!(
                    "{}{}",
                    base_name,
                    CasedString::from_snake_case(field_name).to_pascal_case()
                );

                // Process fields depth-first, collecting nested types
                let mut field_docs = Vec::new();
                for (nested_field_name, nested_field_type) in fields {
                    let field_name_pascal = nested_field_name.to_pascal_case();

                    // Recursively process and get the type reference
                    let field_type_doc = self.extract_and_generate_nested_type(
                        nested_field_type,
                        &type_name, // New base name for nested types
                        nested_field_name.as_str(),
                        generated_types,
                    );

                    // Build field definition
                    let field_doc = BoxDoc::as_string(field_name_pascal)
                        .append(BoxDoc::text(" "))
                        .append(field_type_doc)
                        .append(BoxDoc::text(" `json:\""))
                        .append(BoxDoc::text(nested_field_name.as_str()))
                        .append(BoxDoc::text("\"`"));

                    field_docs.push(field_doc);
                }

                // Generate the struct definition
                let struct_def = BoxDoc::text("type ")
                    .append(BoxDoc::as_string(type_name.clone()))
                    .append(BoxDoc::text(" struct {"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(field_docs, BoxDoc::line()))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"));

                // Add to generated types
                generated_types.push((type_name.clone(), struct_def));

                // Return reference to this type
                BoxDoc::as_string(type_name)
            }
            Type::Array(Some(elem_type)) => {
                if matches!(elem_type.as_ref(), Type::Object(_)) {
                    // Generate type for array elements
                    let elem_type_doc = self.extract_and_generate_nested_type(
                        elem_type,
                        base_name,
                        &format!("{}_item", field_name),
                        generated_types,
                    );
                    BoxDoc::text("[]").append(elem_type_doc)
                } else {
                    // Regular array
                    BoxDoc::text("[]").append(self.transpile_type(elem_type))
                }
            }
            _ => {
                // Primitive types - just use regular transpilation
                self.transpile_type(param_type)
            }
        }
    }

    fn scan_for_imports(imports: &mut BTreeSet<String>, entrypoint: &IrEntrypoint) {
        // Use visitor pattern to scan statements for imports
        for stmt in &entrypoint.body {
            // Check for HTML escaping in WriteExpr statements
            stmt.traverse(&mut |s| {
                if let IrStatement::WriteExpr { escape: true, .. } = s {
                    imports.insert("html".to_string());
                }
            });

            // Check expressions for other imports (like json)
            stmt.traverse(&mut |s| {
                if let Some(primary_expr) = s.expr() {
                    primary_expr.traverse(&mut |expr| {
                        if let IrExpr::JsonEncode { .. } = expr {
                            imports.insert("encoding/json".to_string());
                        }
                    });
                }
            });
        }
    }

    // Helper method to escape strings for Go string literals
    fn escape_string(&self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }
}

impl Transpiler for GoTranspiler {
    fn transpile_module(&self, entrypoints: &[IrEntrypoint]) -> String {
        let mut imports = BTreeSet::new();

        // First pass: scan to determine what imports we need
        for entrypoint in entrypoints {
            Self::scan_for_imports(&mut imports, entrypoint);
        }

        // We always need strings.Builder for output
        imports.insert("strings".to_string());

        // Build the module content using BoxDoc
        let mut result = BoxDoc::nil();

        // Write package declaration
        result = result
            .append(BoxDoc::text("package components"))
            .append(BoxDoc::line())
            .append(BoxDoc::line());

        // Write imports if needed
        if !imports.is_empty() {
            result = result
                .append(BoxDoc::text("import ("))
                .append(
                    BoxDoc::nil()
                        .append(BoxDoc::line())
                        .append(BoxDoc::intersperse(
                            imports.iter().map(|import| {
                                BoxDoc::nil()
                                    .append(BoxDoc::text("\""))
                                    .append(BoxDoc::text(import.as_str()))
                                    .append(BoxDoc::text("\""))
                            }),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(1),
                )
                .append(BoxDoc::text(")"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Add JSON helper if needed
        if imports.contains("encoding/json") {
            result = result
                .append(BoxDoc::text("func mustJSONMarshal(v any) string {"))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::text("data, _ := json.Marshal(v)"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("return string(data)"))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        // Generate parameter structs for entrypoints that have parameters

        for entrypoint in entrypoints {
            if !entrypoint.parameters.is_empty() {
                let struct_name = format!(
                    "{}Params",
                    CasedString::from_kebab_case(&entrypoint.name).to_pascal_case()
                );

                let mut nested_types = Vec::new();

                // Process each parameter, extracting nested types
                let fields: Vec<_> = entrypoint
                    .parameters
                    .iter()
                    .map(|(param_name, param_type)| {
                        let field_name = param_name.to_pascal_case();

                        // Extract nested types and get the type reference
                        let field_type_doc = self.extract_and_generate_nested_type(
                            param_type,
                            &struct_name,
                            param_name.as_str(),
                            &mut nested_types,
                        );

                        BoxDoc::as_string(field_name)
                            .append(BoxDoc::text(" "))
                            .append(field_type_doc)
                            .append(BoxDoc::text(" `json:\""))
                            .append(BoxDoc::text(param_name.as_str()))
                            .append(BoxDoc::text("\"`"))
                    })
                    .collect();

                // First, add all nested types that were extracted
                for (_name, type_def) in &nested_types {
                    result = result
                        .append(type_def.clone())
                        .append(BoxDoc::line())
                        .append(BoxDoc::line());
                }

                // Then add the main parameter struct
                result = result
                    .append(BoxDoc::text("type "))
                    .append(BoxDoc::text(struct_name))
                    .append(BoxDoc::text(" struct {"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(fields, BoxDoc::line()))
                            .nest(1),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::line());
            }
        }

        // Transpile each entrypoint as a function
        for (i, entrypoint) in entrypoints.iter().enumerate() {
            result = result.append(self.transpile_entrypoint(&entrypoint.name, entrypoint));
            if i < entrypoints.len() - 1 {
                result = result.append(BoxDoc::line());
            }
        }

        // Render to string
        let mut buffer = Vec::new();
        result.render(80, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        // Post-process: convert spaces to tabs for Go convention
        let processed = output
            .lines()
            .map(|line| {
                // Count leading spaces
                let spaces = line.chars().take_while(|c| *c == ' ').count();
                let tabs = "\t".repeat(spaces);
                let rest = &line[spaces..];
                format!("{}{}", tabs, rest)
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Ensure file ends with a newline
        if !processed.ends_with('\n') {
            format!("{}\n", processed)
        } else {
            processed
        }
    }

    fn transpile_entrypoint<'a>(&self, name: &'a str, entrypoint: &'a IrEntrypoint) -> BoxDoc<'a> {
        // Convert kebab-case to PascalCase for Go function name
        let func_name = CasedString::from_kebab_case(name).to_pascal_case();

        let mut result = BoxDoc::text("func ").append(BoxDoc::as_string(func_name.clone()));

        if entrypoint.parameters.is_empty() {
            result = result.append(BoxDoc::text("() string {"));
        } else {
            let struct_name = format!("{}Params", func_name);
            result = result
                .append(BoxDoc::text("(params "))
                .append(BoxDoc::as_string(struct_name))
                .append(BoxDoc::text(") string {"));
        }

        // Function body
        let mut body = Vec::new();

        // Extract parameters into local variables
        for (param_name, _) in &entrypoint.parameters {
            let field_name = param_name.to_pascal_case();
            body.push(
                BoxDoc::nil()
                    .append(BoxDoc::text(param_name.as_str()))
                    .append(BoxDoc::text(" := params."))
                    .append(BoxDoc::as_string(field_name)),
            )
        }

        body.push(BoxDoc::text("var output strings.Builder"));
        body.push(self.transpile_statements(&entrypoint.body));
        body.push(BoxDoc::text("return output.String()"));

        result
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::intersperse(body, BoxDoc::line()))
                    .append(BoxDoc::line())
                    .nest(1),
            )
            .append(BoxDoc::text("}"))
            .append(BoxDoc::line())
    }
}

impl StatementTranspiler for GoTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::text("output.WriteString(\"")
            .append(BoxDoc::text(self.escape_string(content)))
            .append(BoxDoc::text("\")"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            BoxDoc::text("output.WriteString(html.EscapeString(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text("))"))
        } else {
            BoxDoc::text("output.WriteString(")
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(")"))
        }
    }

    fn transpile_if<'a>(&self, condition: &'a IrExpr, body: &'a [IrStatement], else_body: Option<&'a [IrStatement]>) -> BoxDoc<'a> {
        let mut doc = BoxDoc::nil()
            .append(BoxDoc::text("if "))
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(1),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"));

        if let Some(else_stmts) = else_body {
            doc = doc
                .append(BoxDoc::text(" else {"))
                .append(
                    BoxDoc::line()
                        .append(self.transpile_statements(else_stmts))
                        .nest(1),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"));
        }

        doc
    }

    fn transpile_for<'a>(
        &self,
        var: &'a str,
        array: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text("for _, ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" := range "))
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(" {"))
            .append(
                BoxDoc::line()
                    .append(self.transpile_statements(body))
                    .nest(1),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }

    fn transpile_let<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text(var)
            .append(BoxDoc::text(" := "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::line())
            .append(self.transpile_statements(body))
    }
}

impl ExpressionTranspiler for GoTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_property_access<'a>(&self, object: &'a IrExpr, property: &'a str) -> BoxDoc<'a> {
        let prop_name = CasedString::from_snake_case(property).to_pascal_case();
        self.transpile_expr(object)
            .append(BoxDoc::text("."))
            .append(BoxDoc::as_string(prop_name))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("\"{}\"", self.escape_string(value)))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        BoxDoc::text(if value { "true" } else { "false" })
    }

    fn transpile_float_literal<'a>(&self, value: f64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}", value))
    }

    fn transpile_int_literal<'a>(&self, value: i64) -> BoxDoc<'a> {
        BoxDoc::as_string(format!("{}", value))
    }

    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Option<Box<Type>>,
    ) -> BoxDoc<'a> {
        let type_part = match elem_type.as_ref() {
            Some(inner_type) => BoxDoc::text("[]").append(self.transpile_type(inner_type)),
            _ => BoxDoc::text("[]any"),
        };

        type_part
            .append(BoxDoc::text("{"))
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
    }

    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(PropertyName, IrExpr)],
        field_types: &'a BTreeMap<PropertyName, Type>,
    ) -> BoxDoc<'a> {
        let struct_type = self.transpile_object_type(field_types);
        struct_type
            .append(BoxDoc::text("{"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::intersperse(
                        properties.iter().map(|(key, value)| {
                            let field_name = key.to_pascal_case();
                            BoxDoc::as_string(field_name)
                                .append(BoxDoc::text(": "))
                                .append(self.transpile_expr(value))
                                .append(BoxDoc::text(","))
                        }),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(1),
            )
            .append(BoxDoc::text("}"))
    }

    fn transpile_string_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" == "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" != "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_less_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" < "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_greater_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" > "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_greater_than<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" > "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_less_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" <= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_less_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" <= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_greater_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" >= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_greater_than_or_equal<'a>(
        &self,
        left: &'a IrExpr,
        right: &'a IrExpr,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" >= "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("!(")
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("mustJSONMarshal(")
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_env_lookup<'a>(&self, key: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("os.Getenv(")
            .append(self.transpile_expr(key))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_and<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" && "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_logical_or<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" || "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_float_add<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::text("(")
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }
}

impl TypeTranspiler for GoTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("bool")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_trusted_html_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_float_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("float64")
    }

    fn transpile_int_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("int64")
    }

    fn transpile_array_type<'a>(&self, element_type: Option<&'a Type>) -> BoxDoc<'a> {
        match element_type {
            Some(elem) => BoxDoc::text("[]").append(self.transpile_type(elem)),
            None => BoxDoc::text("[]any"),
        }
    }

    fn transpile_object_type<'a>(&self, fields: &'a BTreeMap<PropertyName, Type>) -> BoxDoc<'a> {
        // Generate anonymous struct type
        BoxDoc::text("struct{")
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(BoxDoc::intersperse(
                        fields.iter().map(|(field_name, field_type)| {
                            let go_field = field_name.to_pascal_case();
                            BoxDoc::as_string(go_field)
                                .append(BoxDoc::text(" "))
                                .append(self.transpile_type(field_type))
                                .append(BoxDoc::text(" `json:\""))
                                .append(BoxDoc::text(field_name.as_str()))
                                .append(BoxDoc::text("\"`"))
                                .append(BoxDoc::nil().flat_alt(BoxDoc::text(";")))
                        }),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(1),
            )
            .append(BoxDoc::text("}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(entrypoints: &[IrEntrypoint]) -> String {
        let transpiler = GoTranspiler::new();
        transpiler.transpile_module(entrypoints)
    }

    fn check(entrypoints: &[IrEntrypoint], expected: Expect) {
        // Format before (IR)
        let before = entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        // Format after (Go code)
        let after = transpile_with_pretty(entrypoints);

        // Create output with before/after format
        let output = format!("-- before --\n{}\n-- after --\n{}", before, after);

        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_component() {
        let entrypoints = vec![build_ir_auto("test-main-comp", vec![], |t| {
            t.write("<div>Hello World</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp() {
                  write("<div>Hello World</div>\n")
                }

                -- after --
                package components

                import (
                	"strings"
                )

                func TestMainComp() string {
                	var output strings.Builder
                	output.WriteString("<div>Hello World</div>\n")
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_component_with_parameters() {
        let entrypoints = vec![build_ir_auto(
            "test-greeting-comp",
            vec![("name", Type::String), ("message", Type::String)],
            |t| {
                t.write("<h1>Hello ");
                t.write_expr_escaped(t.var("name"));
                t.write(", ");
                t.write_expr_escaped(t.var("message"));
                t.write("</h1>\n");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-greeting-comp(name: String, message: String) {
                  write("<h1>Hello ")
                  write_escaped(name)
                  write(", ")
                  write_escaped(message)
                  write("</h1>\n")
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                type TestGreetingCompParams struct {
                	Name string `json:"name"`
                	Message string `json:"message"`
                }

                func TestGreetingComp(params TestGreetingCompParams) string {
                	name := params.Name
                	message := params.Message
                	var output strings.Builder
                	output.WriteString("<h1>Hello ")
                	output.WriteString(html.EscapeString(name))
                	output.WriteString(", ")
                	output.WriteString(html.EscapeString(message))
                	output.WriteString("</h1>\n")
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_json_encode_empty_array_literal() {
        let entrypoints = vec![build_ir_auto("test-main-comp", vec![], |t| {
            t.write_expr(t.json_encode(t.array(vec![])), false);
        })];

        // In Go, []any{} is the correct way to declare an empty slice.
        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp() {
                  write_expr(JsonEncode([]))
                }

                -- after --
                package components

                import (
                	"encoding/json"
                	"strings"
                )

                func mustJSONMarshal(v any) string {
                	data, _ := json.Marshal(v)
                	return string(data)
                }

                func TestMainComp() string {
                	var output strings.Builder
                	output.WriteString(mustJSONMarshal([]any{}))
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_if_condition() {
        let entrypoints = vec![build_ir_auto(
            "test-main-comp",
            vec![("show", Type::Bool)],
            |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Visible</div>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp(show: Bool) {
                  if show {
                    write("<div>Visible</div>\n")
                  }
                }

                -- after --
                package components

                import (
                	"strings"
                )

                type TestMainCompParams struct {
                	Show bool `json:"show"`
                }

                func TestMainComp(params TestMainCompParams) string {
                	show := params.Show
                	var output strings.Builder
                	if show {
                		output.WriteString("<div>Visible</div>\n")
                	}
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_for_loop() {
        let entrypoints = vec![build_ir_auto(
            "test-main-comp",
            vec![("items", Type::Array(Some(Box::new(Type::String))))],
            |t| {
                t.for_loop("item", t.var("items"), |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("item"));
                    t.write("</li>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp(items: Array[String]) {
                  for item in items {
                    write("<li>")
                    write_escaped(item)
                    write("</li>\n")
                  }
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                type TestMainCompParams struct {
                	Items []string `json:"items"`
                }

                func TestMainComp(params TestMainCompParams) string {
                	items := params.Items
                	var output strings.Builder
                	for _, item := range items {
                		output.WriteString("<li>")
                		output.WriteString(html.EscapeString(item))
                		output.WriteString("</li>\n")
                	}
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_object_literals() {
        let entrypoints = vec![build_ir_auto("test-objects", vec![], |t| {
            t.let_stmt(
                "person",
                t.object(vec![
                    ("name", t.str("Alice")),
                    ("age", t.str("30.0")),
                    ("active", t.bool(true)),
                ]),
                |t| {
                    t.write_expr_escaped(t.prop_access(t.var("person"), "name"));
                    t.write(" is ");
                    t.write_expr(t.prop_access(t.var("person"), "age"), false);
                    t.write(" years old");
                },
            );
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-objects() {
                  let person = {
                    name: "Alice",
                    age: "30.0",
                    active: true,
                  } in {
                    write_escaped(person.name)
                    write(" is ")
                    write_expr(person.age)
                    write(" years old")
                  }
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                func TestObjects() string {
                	var output strings.Builder
                	person := struct{
                		Active bool `json:"active"`
                		Age string `json:"age"`
                		Name string `json:"name"`
                	}{
                		Name: "Alice",
                		Age: "30.0",
                		Active: true,
                	}
                	output.WriteString(html.EscapeString(person.Name))
                	output.WriteString(" is ")
                	output.WriteString(person.Age)
                	output.WriteString(" years old")
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_arrays_and_objects() {
        let parameters = vec![(
            "users",
            Type::Array(Some(Box::new(Type::Object({
                let mut map = BTreeMap::new();
                map.insert(PropertyName::new("name").unwrap(), Type::String);
                map.insert(PropertyName::new("id").unwrap(), Type::String);
                map
            })))),
        )];

        let entrypoints = vec![build_ir_auto("test-nested", parameters, |t| {
            t.for_loop("user", t.var("users"), |t| {
                t.write("<div>");
                t.write_expr_escaped(t.prop_access(t.var("user"), "name"));
                t.write(" (ID: ");
                t.write_expr(t.prop_access(t.var("user"), "id"), false);
                t.write(")</div>\n");
            });
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-nested(users: Array[{id: String, name: String}]) {
                  for user in users {
                    write("<div>")
                    write_escaped(user.name)
                    write(" (ID: ")
                    write_expr(user.id)
                    write(")</div>\n")
                  }
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                type TestNestedParamsUsersItem struct {
                	Id string `json:"id"`
                	Name string `json:"name"`
                }

                type TestNestedParams struct {
                	Users []TestNestedParamsUsersItem `json:"users"`
                }

                func TestNested(params TestNestedParams) string {
                	users := params.Users
                	var output strings.Builder
                	for _, user := range users {
                		output.WriteString("<div>")
                		output.WriteString(html.EscapeString(user.Name))
                		output.WriteString(" (ID: ")
                		output.WriteString(user.Id)
                		output.WriteString(")</div>\n")
                	}
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_loop_over_array_literal() {
        let entrypoints = vec![build_ir_auto("test-array-literal-loop", vec![], |t| {
            t.write("<ul>\n");
            t.for_loop(
                "color",
                t.array(vec![t.str("red"), t.str("green"), t.str("blue")]),
                |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("color"));
                    t.write("</li>\n");
                },
            );
            t.write("</ul>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-array-literal-loop() {
                  write("<ul>\n")
                  for color in ["red", "green", "blue"] {
                    write("<li>")
                    write_escaped(color)
                    write("</li>\n")
                  }
                  write("</ul>\n")
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                func TestArrayLiteralLoop() string {
                	var output strings.Builder
                	output.WriteString("<ul>\n")
                	for _, color := range []string{"red", "green", "blue"} {
                		output.WriteString("<li>")
                		output.WriteString(html.EscapeString(color))
                		output.WriteString("</li>\n")
                	}
                	output.WriteString("</ul>\n")
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_loop_over_object_array_literal() {
        let entrypoints = vec![build_ir_auto("test-products", vec![], |t| {
            t.for_loop(
                "product",
                t.array(vec![
                    t.object(vec![
                        ("name", t.str("Laptop")),
                        ("price", t.str("999.99")),
                        ("in_stock", t.bool(true)),
                    ]),
                    t.object(vec![
                        ("name", t.str("Mouse")),
                        ("price", t.str("29.99")),
                        ("in_stock", t.bool(false)),
                    ]),
                ]),
                |t| {
                    t.write("<div class=\"product\">\n");
                    t.write("<h3>");
                    t.write_expr_escaped(t.prop_access(t.var("product"), "name"));
                    t.write("</h3>\n");
                    t.write("<p>$");
                    t.write_expr(t.prop_access(t.var("product"), "price"), false);
                    t.write("</p>\n");
                    t.if_stmt(t.prop_access(t.var("product"), "in_stock"), |t| {
                        t.write("<span class=\"available\">In Stock</span>\n");
                    });
                    t.if_stmt(t.not(t.prop_access(t.var("product"), "in_stock")), |t| {
                        t.write("<span class=\"sold-out\">Sold Out</span>\n");
                    });
                    t.write("</div>\n");
                },
            );
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-products() {
                  for product in [
                    {name: "Laptop", price: "999.99", in_stock: true},
                    {name: "Mouse", price: "29.99", in_stock: false},
                  ] {
                    write("<div class=\"product\">\n")
                    write("<h3>")
                    write_escaped(product.name)
                    write("</h3>\n")
                    write("<p>$")
                    write_expr(product.price)
                    write("</p>\n")
                    if product.in_stock {
                      write("<span class=\"available\">In Stock</span>\n")
                    }
                    if (!product.in_stock) {
                      write("<span class=\"sold-out\">Sold Out</span>\n")
                    }
                    write("</div>\n")
                  }
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                func TestProducts() string {
                	var output strings.Builder
                	for _, product := range []struct{
                		InStock bool `json:"in_stock"`
                		Name string `json:"name"`
                		Price string `json:"price"`
                	}{struct{
                		InStock bool `json:"in_stock"`
                		Name string `json:"name"`
                		Price string `json:"price"`
                	}{
                		Name: "Laptop",
                		Price: "999.99",
                		InStock: true,
                	}, struct{
                		InStock bool `json:"in_stock"`
                		Name string `json:"name"`
                		Price string `json:"price"`
                	}{
                		Name: "Mouse",
                		Price: "29.99",
                		InStock: false,
                	}} {
                		output.WriteString("<div class=\"product\">\n")
                		output.WriteString("<h3>")
                		output.WriteString(html.EscapeString(product.Name))
                		output.WriteString("</h3>\n")
                		output.WriteString("<p>$")
                		output.WriteString(product.Price)
                		output.WriteString("</p>\n")
                		if product.InStock {
                			output.WriteString("<span class=\"available\">In Stock</span>\n")
                		}
                		if !(product.InStock) {
                			output.WriteString("<span class=\"sold-out\">Sold Out</span>\n")
                		}
                		output.WriteString("</div>\n")
                	}
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_deeply_nested_structs() {
        let parameters = vec![(
            "config",
            Type::Object({
                let mut config = BTreeMap::new();
                config.insert(PropertyName::new("api_key").unwrap(), Type::String);
                config.insert(
                    PropertyName::new("database").unwrap(),
                    Type::Object({
                        let mut db = BTreeMap::new();
                        db.insert(PropertyName::new("host").unwrap(), Type::String);
                        db.insert(PropertyName::new("port").unwrap(), Type::Float);
                        db.insert(
                            PropertyName::new("credentials").unwrap(),
                            Type::Object({
                                let mut creds = BTreeMap::new();
                                creds.insert(PropertyName::new("username").unwrap(), Type::String);
                                creds.insert(PropertyName::new("password").unwrap(), Type::String);
                                creds
                            }),
                        );
                        db
                    }),
                );
                config.insert(
                    PropertyName::new("features").unwrap(),
                    Type::Array(Some(Box::new(Type::Object({
                        let mut feature = BTreeMap::new();
                        feature.insert(PropertyName::new("name").unwrap(), Type::String);
                        feature.insert(PropertyName::new("enabled").unwrap(), Type::Bool);
                        feature.insert(
                            PropertyName::new("settings").unwrap(),
                            Type::Object({
                                let mut settings = BTreeMap::new();
                                settings.insert(PropertyName::new("level").unwrap(), Type::String);
                                settings.insert(PropertyName::new("timeout").unwrap(), Type::Float);
                                settings
                            }),
                        );
                        feature
                    })))),
                );
                config
            }),
        )];

        let entrypoints = vec![build_ir_auto("test-deep-config", parameters, |t| {
            t.write("<div>API Key: ");
            t.write_expr_escaped(t.prop_access(t.var("config"), "api_key"));
            t.write("</div>\n");
            t.write("<div>DB Host: ");
            t.write_expr_escaped(t.prop_access(t.prop_access(t.var("config"), "database"), "host"));
            t.write("</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-deep-config(
                  config: {
                    api_key: String,
                    database: {
                      credentials: {password: String, username: String},
                      host: String,
                      port: Float,
                    },
                    features: Array[{
                      enabled: Bool,
                      name: String,
                      settings: {level: String, timeout: Float},
                    }],
                  },
                ) {
                  write("<div>API Key: ")
                  write_escaped(config.api_key)
                  write("</div>\n")
                  write("<div>DB Host: ")
                  write_escaped(config.database.host)
                  write("</div>\n")
                }

                -- after --
                package components

                import (
                	"html"
                	"strings"
                )

                type TestDeepConfigParamsConfigDatabaseCredentials struct {
                	Password string `json:"password"`
                	Username string `json:"username"`
                }

                type TestDeepConfigParamsConfigDatabase struct {
                	Credentials TestDeepConfigParamsConfigDatabaseCredentials `json:"credentials"`
                	Host string `json:"host"`
                	Port float64 `json:"port"`
                }

                type TestDeepConfigParamsConfigFeaturesItemSettings struct {
                	Level string `json:"level"`
                	Timeout float64 `json:"timeout"`
                }

                type TestDeepConfigParamsConfigFeaturesItem struct {
                	Enabled bool `json:"enabled"`
                	Name string `json:"name"`
                	Settings TestDeepConfigParamsConfigFeaturesItemSettings `json:"settings"`
                }

                type TestDeepConfigParamsConfig struct {
                	ApiKey string `json:"api_key"`
                	Database TestDeepConfigParamsConfigDatabase `json:"database"`
                	Features []TestDeepConfigParamsConfigFeaturesItem `json:"features"`
                }

                type TestDeepConfigParams struct {
                	Config TestDeepConfigParamsConfig `json:"config"`
                }

                func TestDeepConfig(params TestDeepConfigParams) string {
                	config := params.Config
                	var output strings.Builder
                	output.WriteString("<div>API Key: ")
                	output.WriteString(html.EscapeString(config.ApiKey))
                	output.WriteString("</div>\n")
                	output.WriteString("<div>DB Host: ")
                	output.WriteString(html.EscapeString(config.Database.Host))
                	output.WriteString("</div>\n")
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_string_comparison() {
        let entrypoints = vec![build_ir_auto(
            "test-auth-check",
            vec![("user_role", Type::String), ("expected_role", Type::String)],
            |t| {
                t.if_stmt(t.eq(t.var("user_role"), t.var("expected_role")), |t| {
                    t.write("<div>Access granted</div>\n");
                });
                t.if_stmt(t.eq(t.var("user_role"), t.str("admin")), |t| {
                    t.write("<div>Admin panel available</div>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-auth-check(user_role: String, expected_role: String) {
                  if (user_role == expected_role) {
                    write("<div>Access granted</div>\n")
                  }
                  if (user_role == "admin") {
                    write("<div>Admin panel available</div>\n")
                  }
                }

                -- after --
                package components

                import (
                	"strings"
                )

                type TestAuthCheckParams struct {
                	UserRole string `json:"user_role"`
                	ExpectedRole string `json:"expected_role"`
                }

                func TestAuthCheck(params TestAuthCheckParams) string {
                	user_role := params.UserRole
                	expected_role := params.ExpectedRole
                	var output strings.Builder
                	if (user_role == expected_role) {
                		output.WriteString("<div>Access granted</div>\n")
                	}
                	if (user_role == "admin") {
                		output.WriteString("<div>Admin panel available</div>\n")
                	}
                	return output.String()
                }
            "#]],
        );
    }

    #[test]
    fn test_json_encode() {
        let parameters = vec![
            (
                "data",
                Type::Object({
                    let mut map = BTreeMap::new();
                    map.insert(PropertyName::new("title").unwrap(), Type::String);
                    map.insert(PropertyName::new("count").unwrap(), Type::Float);
                    map.insert(PropertyName::new("active").unwrap(), Type::Bool);
                    map
                }),
            ),
            ("items", Type::Array(Some(Box::new(Type::String)))),
        ];

        let entrypoints = vec![build_ir_auto("test-json", parameters, |t| {
            t.write("<script>\n");
            t.write("const data = ");
            t.write_expr(t.json_encode(t.var("data")), false);
            t.write(";\n");
            t.write("const items = ");
            t.write_expr(t.json_encode(t.var("items")), false);
            t.write(";\n");
            t.write("const config = ");
            t.write_expr(
                t.json_encode(t.object(vec![("debug", t.bool(true)), ("version", t.float(1.5))])),
                false,
            );
            t.write(";\n");
            t.write("</script>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-json(
                  data: {active: Bool, count: Float, title: String},
                  items: Array[String],
                ) {
                  write("<script>\n")
                  write("const data = ")
                  write_expr(JsonEncode(data))
                  write(";\n")
                  write("const items = ")
                  write_expr(JsonEncode(items))
                  write(";\n")
                  write("const config = ")
                  write_expr(JsonEncode({debug: true, version: 1.5}))
                  write(";\n")
                  write("</script>\n")
                }

                -- after --
                package components

                import (
                	"encoding/json"
                	"strings"
                )

                func mustJSONMarshal(v any) string {
                	data, _ := json.Marshal(v)
                	return string(data)
                }

                type TestJsonParamsData struct {
                	Active bool `json:"active"`
                	Count float64 `json:"count"`
                	Title string `json:"title"`
                }

                type TestJsonParams struct {
                	Data TestJsonParamsData `json:"data"`
                	Items []string `json:"items"`
                }

                func TestJson(params TestJsonParams) string {
                	data := params.Data
                	items := params.Items
                	var output strings.Builder
                	output.WriteString("<script>\n")
                	output.WriteString("const data = ")
                	output.WriteString(mustJSONMarshal(data))
                	output.WriteString(";\n")
                	output.WriteString("const items = ")
                	output.WriteString(mustJSONMarshal(items))
                	output.WriteString(";\n")
                	output.WriteString("const config = ")
                	output.WriteString(mustJSONMarshal(struct{
                		Debug bool `json:"debug"`
                		Version float64 `json:"version"`
                	}{
                		Debug: true,
                		Version: 1.5,
                	}))
                	output.WriteString(";\n")
                	output.WriteString("</script>\n")
                	return output.String()
                }
            "#]],
        );
    }
}
