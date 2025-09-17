use pretty::BoxDoc;

use super::{Doc, PrettyExpressionTranspiler, TypeTranspiler};
use crate::dop::r#type::Type;
use crate::ir::ast::{IrEntrypoint, IrExpr, IrStatement};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum LanguageMode {
    JavaScript,
    TypeScript,
}

/// Transpiles an IR module to JavaScript or TypeScript code
pub struct PrettyJsTranspiler {
    mode: LanguageMode,
    /// Internal flag to use template literals instead of double quotes
    use_template_literals: bool,
}

impl PrettyJsTranspiler {
    pub fn new(mode: LanguageMode) -> Self {
        Self {
            mode,
            use_template_literals: false,
        }
    }

    fn scan_for_escape_html(&self, entrypoint: &IrEntrypoint) -> bool {
        // Use visitor pattern to scan for HTML escaping
        let mut needs_escape = false;
        for stmt in &entrypoint.body {
            stmt.visit(&mut |s| {
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

    fn emit_escape_html_helper(&mut self, doc: &mut Doc) {
        // Add the escape HTML helper function
        match self.mode {
            LanguageMode::JavaScript => {
                doc.write_line("function escapeHtml(str) {");
                doc.indent();
                doc.write_line("if (typeof str !== 'string') return str;");
            }
            LanguageMode::TypeScript => {
                doc.write_line("function escapeHtml(str: string): string {");
                doc.indent();
            }
        }
        doc.write_line("return str");
        doc.indent();
        doc.write_line(".replace(/&/g, '&amp;')");
        doc.write_line(".replace(/</g, '&lt;')");
        doc.write_line(".replace(/>/g, '&gt;')");
        doc.write_line(".replace(/\"/g, '&quot;')");
        doc.write_line(".replace(/'/g, '&#39;');");
        doc.dedent();
        doc.dedent();
        doc.write_line("}");
        doc.write_line("");
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

impl PrettyExpressionTranspiler for PrettyJsTranspiler {
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

    fn transpile_number_literal<'a>(&self, value: &'a serde_json::Number) -> BoxDoc<'a> {
        BoxDoc::as_string(value.to_string())
    }

    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        _elem_type: &'a Type,
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

    fn transpile_string_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
            .append(self.transpile_expr(right))
            .append(BoxDoc::text(")"))
    }

    fn transpile_bool_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a> {
        BoxDoc::nil()
            .append(BoxDoc::text("("))
            .append(self.transpile_expr(left))
            .append(BoxDoc::text(" === "))
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
}
