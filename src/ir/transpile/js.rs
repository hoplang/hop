use pretty::BoxDoc;

use super::{ExpressionTranspiler, StatementTranspiler, Transpiler, TypeTranspiler};
use crate::cased_string::CasedString;
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum LanguageMode {
    JavaScript,
    TypeScript,
}

pub struct JsTranspiler {
    mode: LanguageMode,
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
}

impl JsTranspiler {
    pub fn new(mode: LanguageMode) -> Self {
        Self {
            mode,
            use_template_literals: false,
        }
    }

    fn scan_for_escape_html(&self, entrypoint: &IrEntrypoint) -> bool {
        let mut needs_escape = false;
        for stmt in &entrypoint.body {
            stmt.traverse(&mut |s| {
                if let IrStatement::WriteExpr { escape: true, .. } = s {
                    needs_escape = true;
                }
            });
            if needs_escape {
                break;
            }
        }
        needs_escape
    }

    // Helper method to escape strings for JavaScript literals
    fn escape_string(&self, s: &str) -> String {
        if self.use_template_literals {
            // For template literals, only escape backticks and ${
            s.replace('\\', "\\\\")
                .replace('`', "\\`")
                .replace("${", "\\${")
        } else {
            // For regular strings, escape double quotes and common escape sequences
            s.replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n")
                .replace('\r', "\\r")
                .replace('\t', "\\t")
        }
    }

    // Helper method to wrap a string in the appropriate quotes
    fn quote_string(&self, s: &str) -> String {
        if self.use_template_literals {
            format!("`{}`", self.escape_string(s))
        } else {
            format!("\"{}\"", self.escape_string(s))
        }
    }
}

impl Transpiler for JsTranspiler {
    fn transpile_module(&self, entrypoints: &[IrEntrypoint]) -> String {
        let mut needs_escape_html = false;
        for entrypoint in entrypoints {
            if self.scan_for_escape_html(entrypoint) {
                needs_escape_html = true;
                break;
            }
        }

        let mut result = BoxDoc::nil();

        if needs_escape_html {
            result = result
                .append(match self.mode {
                    LanguageMode::JavaScript => BoxDoc::text("function escapeHtml(str) {"),
                    LanguageMode::TypeScript => {
                        BoxDoc::text("function escapeHtml(str: string): string {")
                    }
                })
                .append(
                    BoxDoc::nil()
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("return str"))
                        .append(
                            BoxDoc::nil()
                                .append(BoxDoc::line())
                                .append(BoxDoc::intersperse(
                                    [
                                        BoxDoc::text(".replace(/&/g, '&amp;')"),
                                        BoxDoc::text(".replace(/</g, '&lt;')"),
                                        BoxDoc::text(".replace(/>/g, '&gt;')"),
                                        BoxDoc::text(".replace(/\"/g, '&quot;')"),
                                        BoxDoc::text(".replace(/'/g, '&#39;');"),
                                    ],
                                    BoxDoc::line(),
                                ))
                                .nest(4),
                        )
                        .append(BoxDoc::line())
                        .nest(4),
                )
                .append(BoxDoc::text("}"))
                .append(BoxDoc::line())
                .append(BoxDoc::line());
        }

        result = result
            .append(BoxDoc::text("export default {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::hardline())
                    .append(BoxDoc::intersperse(
                        entrypoints.iter().map(|entrypoint| {
                            self.transpile_entrypoint(&entrypoint.name, entrypoint)
                        }),
                        BoxDoc::text(",").append(BoxDoc::hardline()),
                    ))
                    .append(BoxDoc::hardline())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
            .append(BoxDoc::hardline());

        // Render to string
        let mut buffer = Vec::new();
        result.render(80, &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    fn transpile_entrypoint<'a>(&self, name: &'a str, entrypoint: &'a IrEntrypoint) -> BoxDoc<'a> {
        let camel_case_name = CasedString::from_kebab_case(name).to_camel_case();

        let mut result = BoxDoc::as_string(camel_case_name).append(BoxDoc::text(": ("));

        if !entrypoint.parameters.is_empty() {
            result = result
                .append(BoxDoc::text("{ "))
                .append(BoxDoc::intersperse(
                    entrypoint
                        .parameters
                        .iter()
                        .map(|(name, _)| BoxDoc::text(name.as_str())),
                    BoxDoc::text(", "),
                ))
                .append(BoxDoc::text(" }"));

            // Generate TypeScript interface for parameters
            if matches!(self.mode, LanguageMode::TypeScript) {
                result = result
                    .append(BoxDoc::text(": { "))
                    .append(BoxDoc::intersperse(
                        entrypoint.parameters.iter().map(|(name, ty)| {
                            BoxDoc::text(name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(self.transpile_type(ty))
                        }),
                        BoxDoc::text(", "),
                    ))
                    .append(BoxDoc::text(" }"));
            }
        }

        // Function body
        result
            .append(BoxDoc::text(")"))
            .append(if matches!(self.mode, LanguageMode::TypeScript) {
                BoxDoc::text(": string")
            } else {
                BoxDoc::nil()
            })
            .append(BoxDoc::text(" => {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::line())
                    .append(match self.mode {
                        LanguageMode::JavaScript => BoxDoc::text("let output = \"\";"),
                        LanguageMode::TypeScript => BoxDoc::text("let output: string = \"\";"),
                    })
                    .append(BoxDoc::line())
                    .append(self.transpile_statements(&entrypoint.body))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("return output;"))
                    .append(BoxDoc::line())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
    }
}

impl StatementTranspiler for JsTranspiler {
    fn transpile_write<'a>(&self, content: &'a str) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("output += "))
            .append(BoxDoc::as_string(self.quote_string(content)))
            .append(BoxDoc::text(";"))
    }

    fn transpile_write_expr<'a>(&self, expr: &'a IrExpr, escape: bool) -> BoxDoc<'a> {
        if escape {
            BoxDoc::nil()
                .append(BoxDoc::text("output += escapeHtml("))
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(");"))
        } else {
            BoxDoc::nil()
                .append(BoxDoc::text("output += "))
                .append(self.transpile_expr(expr))
                .append(BoxDoc::text(";"))
        }
    }

    fn transpile_if<'a>(&self, condition: &'a IrExpr, body: &'a [IrStatement]) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("if ("))
            .append(self.transpile_expr(condition))
            .append(BoxDoc::text(") {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::hardline())
                    .append(self.transpile_statements(body))
                    .append(BoxDoc::hardline())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
    }

    fn transpile_for<'a>(
        &self,
        var: &'a str,
        array: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("for (const "))
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" of "))
            .append(self.transpile_expr(array))
            .append(BoxDoc::text(") {"))
            .append(
                BoxDoc::nil()
                    .append(BoxDoc::hardline())
                    .append(self.transpile_statements(body))
                    .append(BoxDoc::hardline())
                    .nest(4),
            )
            .append(BoxDoc::text("}"))
    }

    fn transpile_let<'a>(
        &self,
        var: &'a str,
        value: &'a IrExpr,
        body: &'a [IrStatement],
    ) -> BoxDoc<'a> {
        BoxDoc::text("const ")
            .append(BoxDoc::text(var))
            .append(BoxDoc::text(" = "))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(";"))
            .append(BoxDoc::hardline())
            .append(self.transpile_statements(body))
    }
}

impl ExpressionTranspiler for JsTranspiler {
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(name)
    }

    fn transpile_property_access<'a>(&self, object: &'a IrExpr, property: &'a str) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(self.transpile_expr(object))
            .append(BoxDoc::text("."))
            .append(BoxDoc::text(property))
    }

    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a> {
        BoxDoc::text(self.quote_string(value))
    }

    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a> {
        match value {
            true => BoxDoc::text("true"),
            false => BoxDoc::text("false"),
        }
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
        _elem_type: &'a Option<Box<Type>>,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("["))
            .append(BoxDoc::intersperse(
                elements.iter().map(|e| self.transpile_expr(e)),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("]"))
    }

    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(String, IrExpr)],
        _field_types: &'a BTreeMap<String, Type>,
    ) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("{"))
            .append(BoxDoc::intersperse(
                properties.iter().map(|(key, value)| {
                    BoxDoc::text(key)
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_expr(value))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text("}"))
    }

    fn transpile_string_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_int_not_equals<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" !== "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("!("))
            .append(self.transpile_expr(operand))
            .append(BoxDoc::text(")"))
    }

    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("JSON.stringify("))
            .append(self.transpile_expr(value))
            .append(BoxDoc::text(")"))
    }

    fn transpile_string_concat<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" + "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }
}

impl TypeTranspiler for JsTranspiler {
    fn transpile_bool_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("boolean")
    }

    fn transpile_string_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("string")
    }

    fn transpile_number_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("number")
    }

    fn transpile_int_type<'a>(&self) -> BoxDoc<'a> {
        BoxDoc::text("number")
    }

    fn transpile_array_type<'a>(&self, element_type: Option<&'a Type>) -> BoxDoc<'a> {
        match element_type {
            Some(elem) => self.transpile_type(elem).append(BoxDoc::text("[]")),
            None => BoxDoc::text("unknown[]"),
        }
    }

    fn transpile_object_type<'a>(&self, fields: &'a BTreeMap<String, Type>) -> BoxDoc<'a> {
        if fields.is_empty() {
            return BoxDoc::text("{}");
        }

        BoxDoc::nil()
            .append(BoxDoc::text("{ "))
            .append(BoxDoc::intersperse(
                fields.iter().map(|(name, ty)| {
                    BoxDoc::text(name)
                        .append(BoxDoc::text(": "))
                        .append(self.transpile_type(ty))
                }),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(" }"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::build_ir_auto;
    use expect_test::{Expect, expect};

    fn transpile_with_pretty(entrypoints: &[IrEntrypoint], mode: LanguageMode) -> String {
        let transpiler = JsTranspiler::new(mode);
        transpiler.transpile_module(entrypoints)
    }

    fn check(entrypoints: &[IrEntrypoint], expected: Expect) {
        // Format before (IR)
        let before = entrypoints
            .iter()
            .map(|ep| ep.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        // Format TypeScript output
        let ts_output = transpile_with_pretty(entrypoints, LanguageMode::TypeScript);

        // Format JavaScript output
        let js_output = transpile_with_pretty(entrypoints, LanguageMode::JavaScript);

        // Create output with before/ts/js format
        let output = format!(
            "-- before --\n{}\n-- ts --\n{}\n-- js --\n{}",
            before, ts_output, js_output
        );

        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_component() {
        let entrypoints = vec![build_ir_auto("hello-world", vec![], |t| {
            t.write("<h1>Hello, World!</h1>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                hello-world() {
                  write("<h1>Hello, World!</h1>\n")
                }

                -- ts --
                export default {
                    helloWorld: (): string => {
                        let output: string = "";
                        output += "<h1>Hello, World!</h1>\n";
                        return output;
                    }
                }

                -- js --
                export default {
                    helloWorld: () => {
                        let output = "";
                        output += "<h1>Hello, World!</h1>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_component_with_params_and_escaping() {
        let entrypoints = vec![build_ir_auto(
            "user-info",
            vec![("name", Type::String), ("age", Type::String)],
            |t| {
                t.write("<div>\n");
                t.write("<h2>Name: ");
                t.write_expr_escaped(t.var("name"));
                t.write("</h2>\n");
                t.write("<p>Age: ");
                t.write_expr(t.var("age"), false);
                t.write("</p>\n");
                t.write("</div>\n");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                user-info(name: string, age: string) {
                  write("<div>\n")
                  write("<h2>Name: ")
                  write_escaped(name)
                  write("</h2>\n")
                  write("<p>Age: ")
                  write_expr(age)
                  write("</p>\n")
                  write("</div>\n")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    userInfo: ({ name, age }: { name: string, age: string }): string => {
                        let output: string = "";
                        output += "<div>\n";
                        output += "<h2>Name: ";
                        output += escapeHtml(name);
                        output += "</h2>\n";
                        output += "<p>Age: ";
                        output += age;
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    userInfo: ({ name, age }) => {
                        let output = "";
                        output += "<div>\n";
                        output += "<h2>Name: ";
                        output += escapeHtml(name);
                        output += "</h2>\n";
                        output += "<p>Age: ";
                        output += age;
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_typescript_with_types() {
        let entrypoints = vec![build_ir_auto(
            "conditional-display",
            vec![("title", Type::String), ("show", Type::Bool)],
            |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<h1>");
                    t.write_expr_escaped(t.var("title"));
                    t.write("</h1>\n");
                });
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                conditional-display(title: string, show: boolean) {
                  if show {
                    write("<h1>")
                    write_escaped(title)
                    write("</h1>\n")
                  }
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    conditionalDisplay: ({ title, show }: { title: string, show: boolean }): string => {
                        let output: string = "";
                        if (show) {
                            output += "<h1>";
                            output += escapeHtml(title);
                            output += "</h1>\n";
                        }
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    conditionalDisplay: ({ title, show }) => {
                        let output = "";
                        if (show) {
                            output += "<h1>";
                            output += escapeHtml(title);
                            output += "</h1>\n";
                        }
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_for_loop_with_array() {
        let entrypoints = vec![build_ir_auto(
            "list-items",
            vec![("items", Type::Array(Some(Box::new(Type::String))))],
            |t| {
                t.write("<ul>\n");
                t.for_loop("item", t.var("items"), |t| {
                    t.write("<li>");
                    t.write_expr_escaped(t.var("item"));
                    t.write("</li>\n");
                });
                t.write("</ul>\n");
            },
        )];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                list-items(items: array[string]) {
                  write("<ul>\n")
                  for item in items {
                    write("<li>")
                    write_escaped(item)
                    write("</li>\n")
                  }
                  write("</ul>\n")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    listItems: ({ items }: { items: string[] }): string => {
                        let output: string = "";
                        output += "<ul>\n";
                        for (const item of items) {
                            output += "<li>";
                            output += escapeHtml(item);
                            output += "</li>\n";
                        }
                        output += "</ul>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    listItems: ({ items }) => {
                        let output = "";
                        output += "<ul>\n";
                        for (const item of items) {
                            output += "<li>";
                            output += escapeHtml(item);
                            output += "</li>\n";
                        }
                        output += "</ul>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_let_binding() {
        let entrypoints = vec![build_ir_auto("greeting-card", vec![], |t| {
            t.let_stmt("greeting", t.str("Hello from hop!"), |t| {
                t.write("<div class=\"card\">\n");
                t.write("<p>");
                t.write_expr_escaped(t.var("greeting"));
                t.write("</p>\n");
                t.write("</div>\n");
            });
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                greeting-card() {
                  let greeting = "Hello from hop!" in {
                    write("<div class=\"card\">\n")
                    write("<p>")
                    write_escaped(greeting)
                    write("</p>\n")
                    write("</div>\n")
                  }
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    greetingCard: (): string => {
                        let output: string = "";
                        const greeting = "Hello from hop!";
                        output += "<div class=\"card\">\n";
                        output += "<p>";
                        output += escapeHtml(greeting);
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    greetingCard: () => {
                        let output = "";
                        const greeting = "Hello from hop!";
                        output += "<div class=\"card\">\n";
                        output += "<p>";
                        output += escapeHtml(greeting);
                        output += "</p>\n";
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_nested_components_with_let_bindings() {
        let entrypoints = vec![build_ir_auto("test-main-comp", vec![], |t| {
            t.write("<div data-hop-id=\"test/card-comp\">");
            t.let_stmt("title", t.str("Hello World"), |t| {
                t.write("<h2>");
                t.write_expr_escaped(t.var("title"));
                t.write("</h2>");
            });
            t.write("</div>");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-main-comp() {
                  write("<div data-hop-id=\"test/card-comp\">")
                  let title = "Hello World" in {
                    write("<h2>")
                    write_escaped(title)
                    write("</h2>")
                  }
                  write("</div>")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: (): string => {
                        let output: string = "";
                        output += "<div data-hop-id=\"test/card-comp\">";
                        const title = "Hello World";
                        output += "<h2>";
                        output += escapeHtml(title);
                        output += "</h2>";
                        output += "</div>";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testMainComp: () => {
                        let output = "";
                        output += "<div data-hop-id=\"test/card-comp\">";
                        const title = "Hello World";
                        output += "<h2>";
                        output += escapeHtml(title);
                        output += "</h2>";
                        output += "</div>";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_complex_literals_and_property_access() {
        let entrypoints = vec![build_ir_auto("test-product-list", vec![], |t| {
            t.write("<div class=\"products\">\n");
            t.for_loop(
                "product",
                t.array(vec![
                    t.object(vec![
                        ("name", t.str("Laptop")),
                        ("inStock", t.bool(true)),
                        ("category", t.str("electronics")),
                    ]),
                    t.object(vec![
                        ("name", t.str("Book")),
                        ("inStock", t.bool(false)),
                        ("category", t.str("books")),
                    ]),
                    t.object(vec![
                        ("name", t.str("T-Shirt")),
                        ("inStock", t.bool(true)),
                        ("category", t.str("clothing")),
                    ]),
                ]),
                |t| {
                    t.let_stmt(
                        "displayInfo",
                        t.object(vec![
                            ("currency", t.str("$")),
                            ("showStock", t.bool(true)),
                            ("prefix", t.str("PROD-")),
                        ]),
                        |t| {
                            t.write("<article class=\"product\">\n");
                            t.write("<h3>");
                            t.write_expr(t.prop_access(t.var("displayInfo"), "prefix"), false);
                            t.write_expr_escaped(t.prop_access(t.var("product"), "name"));
                            t.write("</h3>\n");
                            t.write("</p>\n");
                            t.write("<p>Category: ");
                            t.write_expr_escaped(t.prop_access(t.var("product"), "category"));
                            t.write("</p>\n");
                            t.if_stmt(t.prop_access(t.var("displayInfo"), "showStock"), |t| {
                                t.if_stmt(t.prop_access(t.var("product"), "inStock"), |t| {
                                    t.write("<span class=\"in-stock\">✓ In Stock</span>\n");
                                });
                                t.if_stmt(t.not(t.prop_access(t.var("product"), "inStock")), |t| {
                                    t.write("<span class=\"out-of-stock\">✗ Out of Stock</span>\n");
                                });
                            });
                            t.write("</article>\n");
                        },
                    );
                },
            );
            t.write("</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-product-list() {
                  write("<div class=\"products\">\n")
                  for product in [
                    {
                      name: "Laptop",
                      inStock: true,
                      category: "electronics",
                    },
                    {name: "Book", inStock: false, category: "books"},
                    {name: "T-Shirt", inStock: true, category: "clothing"},
                  ] {
                    let displayInfo = {
                      currency: "$",
                      showStock: true,
                      prefix: "PROD-",
                    } in {
                      write("<article class=\"product\">\n")
                      write("<h3>")
                      write_expr(displayInfo.prefix)
                      write_escaped(product.name)
                      write("</h3>\n")
                      write("</p>\n")
                      write("<p>Category: ")
                      write_escaped(product.category)
                      write("</p>\n")
                      if displayInfo.showStock {
                        if product.inStock {
                          write("<span class=\"in-stock\">✓ In Stock</span>\n")
                        }
                        if (!product.inStock) {
                          write("<span class=\"out-of-stock\">✗ Out of Stock</span>\n")
                        }
                      }
                      write("</article>\n")
                    }
                  }
                  write("</div>\n")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testProductList: (): string => {
                        let output: string = "";
                        output += "<div class=\"products\">\n";
                        for (const product of [{name: "Laptop", inStock: true, category: "electronics"}, {name: "Book", inStock: false, category: "books"}, {name: "T-Shirt", inStock: true, category: "clothing"}]) {
                            const displayInfo = {currency: "$", showStock: true, prefix: "PROD-"};
                            output += "<article class=\"product\">\n";
                            output += "<h3>";
                            output += displayInfo.prefix;
                            output += escapeHtml(product.name);
                            output += "</h3>\n";
                            output += "</p>\n";
                            output += "<p>Category: ";
                            output += escapeHtml(product.category);
                            output += "</p>\n";
                            if (displayInfo.showStock) {
                                if (product.inStock) {
                                    output += "<span class=\"in-stock\">✓ In Stock</span>\n";
                                }
                                if (!(product.inStock)) {
                                    output += "<span class=\"out-of-stock\">✗ Out of Stock</span>\n";
                                }
                            }
                            output += "</article>\n";
                        }
                        output += "</div>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testProductList: () => {
                        let output = "";
                        output += "<div class=\"products\">\n";
                        for (const product of [{name: "Laptop", inStock: true, category: "electronics"}, {name: "Book", inStock: false, category: "books"}, {name: "T-Shirt", inStock: true, category: "clothing"}]) {
                            const displayInfo = {currency: "$", showStock: true, prefix: "PROD-"};
                            output += "<article class=\"product\">\n";
                            output += "<h3>";
                            output += displayInfo.prefix;
                            output += escapeHtml(product.name);
                            output += "</h3>\n";
                            output += "</p>\n";
                            output += "<p>Category: ";
                            output += escapeHtml(product.category);
                            output += "</p>\n";
                            if (displayInfo.showStock) {
                                if (product.inStock) {
                                    output += "<span class=\"in-stock\">✓ In Stock</span>\n";
                                }
                                if (!(product.inStock)) {
                                    output += "<span class=\"out-of-stock\">✗ Out of Stock</span>\n";
                                }
                            }
                            output += "</article>\n";
                        }
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_typescript_with_complex_parameters() {
        let parameters = vec![
            (
                "users",
                Type::Array(Some(Box::new(Type::Object({
                    let mut map = BTreeMap::new();
                    map.insert("name".to_string(), Type::String);
                    map.insert("id".to_string(), Type::String);
                    map.insert("active".to_string(), Type::Bool);
                    map
                })))),
            ),
            ("title", Type::String),
        ];

        let entrypoints = vec![build_ir_auto("test-user-list", parameters, |t| {
            t.write("<div>\n");
            t.write("<h1>\n");
            t.write_expr_escaped(t.var("title"));
            t.write("</h1>\n");
            t.write("<ul>\n");
            t.for_loop("user", t.var("users"), |t| {
                t.write("\n");
                t.if_stmt(t.prop_access(t.var("user"), "active"), |t| {
                    t.write("\n<li>User ");
                    t.write_expr_escaped(t.prop_access(t.var("user"), "id"));
                    t.write(": ");
                    t.write_expr_escaped(t.prop_access(t.var("user"), "name"));
                    t.write("</li>\n");
                });
                t.write("\n");
            });
            t.write("</ul>\n");
            t.write("</div>\n");
        })];

        check(
            &entrypoints,
            expect![[r#"
                -- before --
                test-user-list(
                  users: array[{active: boolean, id: string, name: string}],
                  title: string,
                ) {
                  write("<div>\n")
                  write("<h1>\n")
                  write_escaped(title)
                  write("</h1>\n")
                  write("<ul>\n")
                  for user in users {
                    write("\n")
                    if user.active {
                      write("\n<li>User ")
                      write_escaped(user.id)
                      write(": ")
                      write_escaped(user.name)
                      write("</li>\n")
                    }
                    write("\n")
                  }
                  write("</ul>\n")
                  write("</div>\n")
                }

                -- ts --
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testUserList: ({ users, title }: { users: { active: boolean, id: string, name: string }[], title: string }): string => {
                        let output: string = "";
                        output += "<div>\n";
                        output += "<h1>\n";
                        output += escapeHtml(title);
                        output += "</h1>\n";
                        output += "<ul>\n";
                        for (const user of users) {
                            output += "\n";
                            if (user.active) {
                                output += "\n<li>User ";
                                output += escapeHtml(user.id);
                                output += ": ";
                                output += escapeHtml(user.name);
                                output += "</li>\n";
                            }
                            output += "\n";
                        }
                        output += "</ul>\n";
                        output += "</div>\n";
                        return output;
                    }
                }

                -- js --
                function escapeHtml(str) {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export default {
                    testUserList: ({ users, title }) => {
                        let output = "";
                        output += "<div>\n";
                        output += "<h1>\n";
                        output += escapeHtml(title);
                        output += "</h1>\n";
                        output += "<ul>\n";
                        for (const user of users) {
                            output += "\n";
                            if (user.active) {
                                output += "\n<li>User ";
                                output += escapeHtml(user.id);
                                output += ": ";
                                output += escapeHtml(user.name);
                                output += "</li>\n";
                            }
                            output += "\n";
                        }
                        output += "</ul>\n";
                        output += "</div>\n";
                        return output;
                    }
                }
            "#]],
        );
    }
}
