use super::{
    ast::{IrEntrypoint, IrModule, IrNode},
    expr::{BinaryOp, IrExpr, IrExprValue, UnaryOp},
};
use crate::dop::r#type::Type;

#[derive(Debug, Clone, Copy)]
pub enum LanguageMode {
    JavaScript,
    TypeScript,
}

/// Compiles an IR module to JavaScript or TypeScript code
pub struct JsCompiler {
    output: String,
    indent_level: usize,
    mode: LanguageMode,
}

impl JsCompiler {
    pub fn new(mode: LanguageMode) -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            mode,
        }
    }

    pub fn compile_module(&mut self, ir_module: &IrModule) -> String {
        // Add the escape HTML helper function
        match self.mode {
            LanguageMode::JavaScript => {
                self.write_line("function escapeHtml(str) {");
                self.indent();
                self.write_line("if (typeof str !== 'string') return str;");
            }
            LanguageMode::TypeScript => {
                self.write_line("function escapeHtml(str: string): string {");
                self.indent();
            }
        }
        self.write_line("return str");
        self.indent();
        self.write_line(".replace(/&/g, '&amp;')");
        self.write_line(".replace(/</g, '&lt;')");
        self.write_line(".replace(/>/g, '&gt;')");
        self.write_line(".replace(/\"/g, '&quot;')");
        self.write_line(".replace(/'/g, '&#39;');");
        self.dedent();
        self.dedent();
        self.write_line("}");
        self.write_line("");

        // Compile each entrypoint as an exported function
        for (name, entrypoint) in &ir_module.entry_points {
            self.compile_entrypoint(name, entrypoint);
            self.write_line("");
        }

        self.output.clone()
    }

    fn compile_entrypoint(&mut self, name: &str, entrypoint: &IrEntrypoint) {
        // Generate function signature
        let func_name = name.replace(['/', '-'], "_");

        if entrypoint.parameters.is_empty() {
            match self.mode {
                LanguageMode::JavaScript => {
                    self.write_line(&format!("export function {}() {{", func_name));
                }
                LanguageMode::TypeScript => {
                    self.write_line(&format!("export function {}(): string {{", func_name));
                }
            }
        } else {
            // Build parameter list
            let params: Vec<String> = entrypoint
                .parameters
                .iter()
                .map(|(name, _)| name.clone())
                .collect();

            match self.mode {
                LanguageMode::JavaScript => {
                    // Destructure parameters from input object
                    let params_str = params.join(", ");
                    self.write_line(&format!(
                        "export function {}({{ {} }}) {{",
                        func_name, params_str
                    ));
                }
                LanguageMode::TypeScript => {
                    // Generate TypeScript interface for parameters
                    let type_params: Vec<String> = entrypoint
                        .parameters
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name, Self::type_to_typescript(ty)))
                        .collect();
                    let params_str = params.join(", ");
                    let type_params_str = type_params.join(", ");

                    self.write_line(&format!(
                        "export function {}({{ {} }}: {{ {} }}): string {{",
                        func_name, params_str, type_params_str
                    ));
                }
            }
        }

        self.indent();
        match self.mode {
            LanguageMode::JavaScript => self.write_line("let output = \"\";"),
            LanguageMode::TypeScript => self.write_line("let output: string = \"\";"),
        }

        // Compile the body
        self.compile_nodes(&entrypoint.body);

        self.write_line("return output;");
        self.dedent();
        self.write_line("}");
    }

    fn type_to_typescript(ty: &Type) -> String {
        match ty {
            Type::Bool => "boolean".to_string(),
            Type::String => "string".to_string(),
            Type::Number => "number".to_string(),
            Type::Array(elem) => match elem {
                Some(elem_type) => format!("{}[]", Self::type_to_typescript(elem_type)),
                None => "unknown[]".to_string(),
            },
            Type::Object(fields) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, Self::type_to_typescript(ty)))
                    .collect();
                format!("{{ {} }}", field_strs.join(", "))
            }
        }
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
        match &expr.value {
            IrExprValue::Var(name) => name.clone(),

            IrExprValue::PropertyAccess { object, property } => {
                let obj = Self::compile_expr(object);
                format!("{}.{}", obj, property)
            }

            IrExprValue::String(value) => {
                // Escape for JavaScript string literal
                let escaped = value
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t");
                format!("\"{}\"", escaped)
            }

            IrExprValue::Boolean(value) => value.to_string(),

            IrExprValue::Number(value) => value.to_string(),

            IrExprValue::Array(elements) => {
                let items: Vec<String> = elements.iter().map(Self::compile_expr).collect();
                format!("[{}]", items.join(", "))
            }

            IrExprValue::Object(properties) => {
                let props: Vec<String> = properties
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, Self::compile_expr(value)))
                    .collect();
                format!("{{{}}}", props.join(", "))
            }

            IrExprValue::BinaryOp { left, op, right } => {
                let l = Self::compile_expr(left);
                let r = Self::compile_expr(right);
                match op {
                    BinaryOp::Equal => format!("({} === {})", l, r),
                }
            }

            IrExprValue::UnaryOp { op, operand } => {
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

    fn compile_to_output(source: &str, mode: LanguageMode) -> String {
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

        // Compile to JavaScript or TypeScript
        let mut compiler = JsCompiler::new(mode);
        compiler.compile_module(&ir_module)
    }

    fn compile_to_js(source: &str) -> String {
        compile_to_output(source, LanguageMode::JavaScript)
    }

    fn compile_to_ts(source: &str) -> String {
        compile_to_output(source, LanguageMode::TypeScript)
    }

    fn check_js_output(source: &str, expected: Expect) {
        let js = compile_to_js(source);
        expected.assert_eq(&js);
    }

    fn check_ts_output(source: &str, expected: Expect) {
        let ts = compile_to_ts(source);
        expected.assert_eq(&ts);
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
                    output += "\n                <div>Hello World</div>\n            ";
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
                    output += "\n                <h1>Hello ";
                    output += escapeHtml(name);
                    output += ", ";
                    output += escapeHtml(message);
                    output += "</h1>\n            ";
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
                    output += "\n                ";
                    if (show) {
                        output += "\n                    <div>Visible</div>\n                ";
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
                    output += "\n                ";
                    for (const item of items) {
                        output += "\n                    <li>";
                        output += escapeHtml(item);
                        output += "</li>\n                ";
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
                    output += "\n                <div data-hop-id=\"test/card-comp\">";
                    const title = "Hello World";
                    output += "\n                <h2>";
                    output += escapeHtml(title);
                    output += "</h2>\n            ";
                    output += "</div>\n            ";
                    return output;
                }

            "#]],
        );
    }

    #[test]
    fn test_typescript_with_parameters() {
        check_ts_output(
            r#"
            <user-list entrypoint {users: array[{name: string, id: string, active: boolean}], title: string}>
                <div>
                    <h1>{title}</h1>
                    <ul>
                        <for {user in users}>
                            <if {user.active}>
                                <li>User {user.id}: {user.name}</li>
                            </if>
                        </for>
                    </ul>
                </div>
            </user-list>
            "#,
            expect![[r#"
                function escapeHtml(str: string): string {
                    return str
                        .replace(/&/g, '&amp;')
                        .replace(/</g, '&lt;')
                        .replace(/>/g, '&gt;')
                        .replace(/"/g, '&quot;')
                        .replace(/'/g, '&#39;');
                }

                export function test_user_list({ users, title }: { users: { active: boolean, id: string, name: string }[], title: string }): string {
                    let output: string = "";
                    output += "\n                <div>\n                    <h1>";
                    output += escapeHtml(title);
                    output += "</h1>\n                    <ul>\n                        ";
                    for (const user of users) {
                        output += "\n                            ";
                        if (user.active) {
                            output += "\n                                <li>User ";
                            output += escapeHtml(user.id);
                            output += ": ";
                            output += escapeHtml(user.name);
                            output += "</li>\n                            ";
                        }
                        output += "\n                        ";
                    }
                    output += "\n                    </ul>\n                </div>\n            ";
                    return output;
                }

            "#]],
        );
    }
}
