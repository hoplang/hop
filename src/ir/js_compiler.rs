use crate::ir::{BinaryOp, IrEntrypoint, IrExpr, IrModule, IrNode, UnaryOp};

/// Compiles an IR module to JavaScript code
pub struct JsCompiler {
    output: String,
    indent_level: usize,
}

impl JsCompiler {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
        }
    }

    /// Compile an entire IR module to JavaScript
    pub fn compile_module(ir_module: &IrModule) -> String {
        let mut compiler = Self::new();

        // Add the escape HTML helper function
        compiler.write_line("function escapeHtml(str) {");
        compiler.indent();
        compiler.write_line("if (typeof str !== 'string') return str;");
        compiler.write_line("return str");
        compiler.indent();
        compiler.write_line(".replace(/&/g, '&amp;')");
        compiler.write_line(".replace(/</g, '&lt;')");
        compiler.write_line(".replace(/>/g, '&gt;')");
        compiler.write_line(".replace(/\"/g, '&quot;')");
        compiler.write_line(".replace(/'/g, '&#39;');");
        compiler.dedent();
        compiler.dedent();
        compiler.write_line("}");
        compiler.write_line("");

        // Compile each entrypoint as an exported function
        for (name, entrypoint) in &ir_module.entry_points {
            compiler.compile_entrypoint(name, entrypoint);
            compiler.write_line("");
        }

        compiler.output
    }

    fn compile_entrypoint(&mut self, name: &str, entrypoint: &IrEntrypoint) {
        // Generate function signature
        let func_name = name.replace(['/', '-'], "_");

        if entrypoint.parameters.is_empty() {
            self.write_line(&format!("export function {}() {{", func_name));
        } else {
            // Destructure parameters from input object
            let params = entrypoint.parameters.join(", ");
            self.write_line(&format!(
                "export function {}({{ {} }}) {{",
                func_name, params
            ));
        }

        self.indent();
        self.write_line("let output = \"\";");

        // Compile the body
        self.compile_nodes(&entrypoint.body);

        self.write_line("return output;");
        self.dedent();
        self.write_line("}");
    }

    fn compile_nodes(&mut self, nodes: &[IrNode]) {
        for node in nodes {
            self.compile_node(node);
        }
    }

    fn compile_node(&mut self, node: &IrNode) {
        match node {
            IrNode::Write(s) => {
                // Escape the string for JavaScript string literal
                let escaped = s
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t");
                self.write_line(&format!("output += \"{}\";", escaped));
            }

            IrNode::WriteExpr { expr, escape } => {
                let js_expr = Self::compile_expr(expr);
                if *escape {
                    self.write_line(&format!("output += escapeHtml({});", js_expr));
                } else {
                    self.write_line(&format!("output += {};", js_expr));
                }
            }

            IrNode::If { condition, body } => {
                let js_cond = Self::compile_expr(condition);
                self.write_line(&format!("if ({}) {{", js_cond));
                self.indent();
                self.compile_nodes(body);
                self.dedent();
                self.write_line("}");
            }

            IrNode::For { var, array, body } => {
                let js_array = Self::compile_expr(array);
                self.write_line(&format!("for (const {} of {}) {{", var, js_array));
                self.indent();
                self.compile_nodes(body);
                self.dedent();
                self.write_line("}");
            }

            IrNode::Let { var, value, body } => {
                let js_value = Self::compile_expr(value);
                self.write_line(&format!("const {} = {};", var, js_value));
                self.compile_nodes(body);
            }
        }
    }

    fn compile_expr(expr: &IrExpr) -> String {
        match expr {
            IrExpr::Variable(name) => name.clone(),

            IrExpr::PropertyAccess { object, property } => {
                let obj = Self::compile_expr(object);
                format!("{}.{}", obj, property)
            }

            IrExpr::StringLiteral(value) => {
                // Escape for JavaScript string literal
                let escaped = value
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t");
                format!("\"{}\"", escaped)
            }

            IrExpr::BooleanLiteral(value) => value.to_string(),

            IrExpr::NumberLiteral(value) => value.to_string(),

            IrExpr::ArrayLiteral(elements) => {
                let items: Vec<String> = elements.iter().map(Self::compile_expr).collect();
                format!("[{}]", items.join(", "))
            }

            IrExpr::ObjectLiteral(properties) => {
                let props: Vec<String> = properties
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, Self::compile_expr(value)))
                    .collect();
                format!("{{{}}}", props.join(", "))
            }

            IrExpr::BinaryOp { left, op, right } => {
                let l = Self::compile_expr(left);
                let r = Self::compile_expr(right);
                match op {
                    BinaryOp::Equal => format!("({} === {})", l, r),
                }
            }

            IrExpr::UnaryOp { op, operand } => {
                let compiled_op = Self::compile_expr(operand);
                match op {
                    UnaryOp::Not => format!("!({})", compiled_op),
                }
            }
        }
    }

    // Helper methods for indentation
    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    fn write_line(&mut self, line: &str) {
        if !line.is_empty() {
            for _ in 0..self.indent_level {
                self.output.push_str("    ");
            }
        }
        self.output.push_str(line);
        self.output.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error_collector::ErrorCollector;
    use crate::hop::module_name::ModuleName;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::typechecker::TypeChecker;
    use crate::ir::Compiler;
    use expect_test::{Expect, expect};
    use std::collections::HashMap;

    fn compile_to_js(source: &str) -> String {
        let mut errors = ErrorCollector::new();
        let module_name = ModuleName::new("test".to_string()).unwrap();
        let tokenizer = Tokenizer::new(source.to_string());
        let ast = parse(module_name.clone(), tokenizer, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Type check
        let mut typechecker = TypeChecker::default();
        typechecker.typecheck(&[&ast]);
        assert!(
            typechecker
                .type_errors
                .get(&module_name)
                .unwrap()
                .is_empty(),
            "Type errors: {:?}",
            typechecker.type_errors
        );

        // Compile to IR
        let mut asts = HashMap::new();
        asts.insert(module_name, ast);
        let ir_module = Compiler::compile(&asts);

        // Compile to JavaScript
        JsCompiler::compile_module(&ir_module)
    }

    fn check_js_output(source: &str, expected: Expect) {
        let js = compile_to_js(source);
        expected.assert_eq(&js);
    }

    #[test]
    fn test_simple_component() {
        check_js_output(
            r#"
            <main-comp entrypoint>
                <div>Hello World</div>
            </main-comp>
            "#,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function test_main_comp() {
                    let output = "";
                    output += "\n                ";
                    output += "<div>";
                    output += "Hello World";
                    output += "</div>";
                    output += "\n            ";
                    return output;
                }

            "#]],
        );
    }

    #[test]
    fn test_component_with_parameters() {
        check_js_output(
            r#"
            <greeting-comp entrypoint {name: string, message: string}>
                <h1>Hello {name}, {message}</h1>
            </greeting-comp>
            "#,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function test_greeting_comp({ name, message }) {
                    let output = "";
                    const name_1 = name;
                    const message_2 = message;
                    output += "\n                ";
                    output += "<h1>";
                    output += "Hello ";
                    output += escapeHtml(name_1);
                    output += ", ";
                    output += escapeHtml(message_2);
                    output += "</h1>";
                    output += "\n            ";
                    return output;
                }

            "#]],
        );
    }

    #[test]
    fn test_if_condition() {
        check_js_output(
            r#"
            <main-comp entrypoint {show: boolean}>
                <if {show}>
                    <div>Visible</div>
                </if>
            </main-comp>
            "#,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function test_main_comp({ show }) {
                    let output = "";
                    const show_1 = show;
                    output += "\n                ";
                    if (show_1) {
                        output += "\n                    ";
                        output += "<div>";
                        output += "Visible";
                        output += "</div>";
                        output += "\n                ";
                    }
                    output += "\n            ";
                    return output;
                }

            "#]],
        );
    }

    #[test]
    fn test_for_loop() {
        check_js_output(
            r#"
            <main-comp entrypoint {items: array[string]}>
                <for {item in items}>
                    <li>{item}</li>
                </for>
            </main-comp>
            "#,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function test_main_comp({ items }) {
                    let output = "";
                    const items_1 = items;
                    output += "\n                ";
                    for (const item_2 of items_1) {
                        output += "\n                    ";
                        output += "<li>";
                        output += escapeHtml(item_2);
                        output += "</li>";
                        output += "\n                ";
                    }
                    output += "\n            ";
                    return output;
                }

            "#]],
        );
    }

    #[test]
    fn test_nested_components_with_let_bindings() {
        check_js_output(
            r#"
            <card-comp {title: string}>
                <h2>{title}</h2>
            </card-comp>
            
            <main-comp entrypoint>
                <card-comp {title: "Hello World"}/>
            </main-comp>
            "#,
            expect![[r#"
                function escapeHtml(str) {
                    if (typeof str !== 'string') return str;
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function test_main_comp() {
                    let output = "";
                    output += "\n                ";
                    output += "<div data-hop-id=\"test/card-comp\">";
                    const title_1 = "Hello World";
                    output += "\n                ";
                    output += "<h2>";
                    output += escapeHtml(title_1);
                    output += "</h2>";
                    output += "\n            ";
                    output += "</div>";
                    output += "\n            ";
                    return output;
                }

            "#]],
        );
    }
}
