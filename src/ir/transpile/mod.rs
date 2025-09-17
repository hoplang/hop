pub mod go;
pub mod js;
pub mod pretty_js;

pub use go::GoTranspiler;
pub use js::{JsTranspiler, LanguageMode};
use pretty::BoxDoc;

use crate::dop::r#type::Type;
use crate::ir::ast::{BinaryOp, IrEntrypoint, IrExpr, IrModule, IrStatement, UnaryOp};
use std::collections::BTreeMap;

/// Document builder for generating indented code
pub struct Doc {
    content: String,
    indent_level: usize,
    indent_str: String,
}

impl Doc {
    /// Create a new Doc with the specified indentation string
    pub fn new(indent_str: &str) -> Self {
        Self {
            content: String::new(),
            indent_level: 0,
            indent_str: indent_str.to_string(),
        }
    }

    /// Create a new Doc with 4-space indentation
    pub fn new_with_spaces() -> Self {
        Self::new("    ")
    }

    /// Create a new Doc with tab indentation
    pub fn new_with_tabs() -> Self {
        Self::new("\t")
    }

    /// Write text without a newline
    pub fn write(&mut self, text: &str) {
        self.content.push_str(text);
    }

    /// Write a line with proper indentation
    pub fn write_line(&mut self, line: &str) {
        if !line.is_empty() {
            for _ in 0..self.indent_level {
                self.content.push_str(&self.indent_str);
            }
        }
        self.content.push_str(line);
        self.content.push('\n');
    }

    /// Write current indentation without content
    pub fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.content.push_str(&self.indent_str);
        }
    }

    /// Increase indentation level
    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    pub fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    /// Consume the Doc and return the built string
    pub fn into_string(self) -> String {
        self.content
    }

    /// Get a reference to the current content
    pub fn as_str(&self) -> &str {
        &self.content
    }
}

/// Trait for transpiling types to language-specific representations
pub trait TypeTranspiler {
    /// Transpile a primitive boolean type
    fn transpile_bool_type(&self, doc: &mut Doc);

    /// Transpile a primitive string type
    fn transpile_string_type(&self, doc: &mut Doc);

    /// Transpile a primitive number type
    fn transpile_number_type(&self, doc: &mut Doc);

    /// Transpile an array type
    fn transpile_array_type(&self, doc: &mut Doc, element_type: Option<&Type>);

    /// Transpile an object/struct type
    fn transpile_object_type(&self, doc: &mut Doc, fields: &BTreeMap<String, Type>);

    /// Main dispatcher for transpiling any type
    fn transpile_type(&self, doc: &mut Doc, ty: &Type) {
        match ty {
            Type::Bool => self.transpile_bool_type(doc),
            Type::String => self.transpile_string_type(doc),
            Type::Number => self.transpile_number_type(doc),
            Type::Array(elem) => self.transpile_array_type(doc, elem.as_deref()),
            Type::Object(fields) => self.transpile_object_type(doc, fields),
        }
    }
}

/// Fine-grained expression transpilation trait
pub trait PrettyExpressionTranspiler {
    // Variables and property access
    fn transpile_var<'a>(&self, name: &'a str) -> BoxDoc<'a>;
    fn transpile_property_access<'a>(&self, object: &'a IrExpr, property: &'a str) -> BoxDoc<'a>;

    // Literals
    fn transpile_string_literal<'a>(&self, value: &'a str) -> BoxDoc<'a>;
    fn transpile_boolean_literal<'a>(&self, value: bool) -> BoxDoc<'a>;
    fn transpile_number_literal<'a>(&self, value: &'a serde_json::Number) -> BoxDoc<'a>;

    // Complex literals
    fn transpile_array_literal<'a>(
        &self,
        elements: &'a [IrExpr],
        elem_type: &'a Type,
    ) -> BoxDoc<'a>;

    fn transpile_object_literal<'a>(
        &self,
        properties: &'a [(String, IrExpr)],
        field_types: &'a BTreeMap<String, Type>,
    ) -> BoxDoc<'a>;

    // Binary operations (type-specific)
    fn transpile_string_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;
    fn transpile_bool_equality<'a>(&self, left: &'a IrExpr, right: &'a IrExpr) -> BoxDoc<'a>;

    // Unary operations
    fn transpile_not<'a>(&self, operand: &'a IrExpr) -> BoxDoc<'a>;

    // Special operations
    fn transpile_json_encode<'a>(&self, value: &'a IrExpr) -> BoxDoc<'a>;

    // Main dispatcher (with default implementation)
    fn transpile_expr<'a>(&self, expr: &'a IrExpr) -> BoxDoc<'a> {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(value.as_str()),
            IrExpr::PropertyAccess {
                object, property, ..
            } => self.transpile_property_access(object, property),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(value),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(*value),
            IrExpr::NumberLiteral { value, .. } => self.transpile_number_literal(value),
            IrExpr::ArrayLiteral {
                elements,
                annotation: (_, typ),
                ..
            } => self.transpile_array_literal(elements, typ),
            IrExpr::ObjectLiteral {
                properties,
                annotation: (_, typ),
                ..
            } => match typ {
                Type::Object(fields) => self.transpile_object_literal(properties, fields),
                _ => panic!("Object literal must have object type"),
            },
            IrExpr::BinaryOp {
                left,
                operator: BinaryOp::Eq,
                right,
                ..
            } => {
                // Check the types of both operands for safety
                match (left.typ(), right.typ()) {
                    (Type::Bool, Type::Bool) => self.transpile_bool_equality(left, right),
                    (Type::String, Type::String) => self.transpile_string_equality(left, right),
                    _ => panic!(
                        "Equality comparison only supported for matching bool or string types, got {:?} and {:?}",
                        left.typ(),
                        right.typ()
                    ),
                }
            }
            IrExpr::UnaryOp {
                operator: UnaryOp::Not,
                operand,
                ..
            } => self.transpile_not(operand),
            IrExpr::JsonEncode { value, .. } => self.transpile_json_encode(value),
        }
    }
}

/// Fine-grained expression transpilation trait
pub trait ExpressionTranspiler {
    // Variables and property access
    fn transpile_var(&self, doc: &mut Doc, name: &str);
    fn transpile_property_access(&self, doc: &mut Doc, object: &IrExpr, property: &str);

    // Literals
    fn transpile_string_literal(&self, doc: &mut Doc, value: &str);
    fn transpile_boolean_literal(&self, doc: &mut Doc, value: bool);
    fn transpile_number_literal(&self, doc: &mut Doc, value: &serde_json::Number);

    // Complex literals
    fn transpile_array_literal(&self, doc: &mut Doc, elements: &[IrExpr], elem_type: &Type);
    fn transpile_object_literal(
        &self,
        doc: &mut Doc,
        properties: &[(String, IrExpr)],
        field_types: &BTreeMap<String, Type>,
    );

    // Binary operations (type-specific)
    fn transpile_string_equality(&self, doc: &mut Doc, left: &IrExpr, right: &IrExpr);
    fn transpile_bool_equality(&self, doc: &mut Doc, left: &IrExpr, right: &IrExpr);

    // Unary operations
    fn transpile_not(&self, doc: &mut Doc, operand: &IrExpr);

    // Special operations
    fn transpile_json_encode(&self, doc: &mut Doc, value: &IrExpr);

    // Main dispatcher (with default implementation)
    fn transpile_expr(&self, doc: &mut Doc, expr: &IrExpr) {
        match expr {
            IrExpr::Var { value, .. } => self.transpile_var(doc, value.as_str()),
            IrExpr::PropertyAccess {
                object, property, ..
            } => self.transpile_property_access(doc, object, property),
            IrExpr::StringLiteral { value, .. } => self.transpile_string_literal(doc, value),
            IrExpr::BooleanLiteral { value, .. } => self.transpile_boolean_literal(doc, *value),
            IrExpr::NumberLiteral { value, .. } => self.transpile_number_literal(doc, value),
            IrExpr::ArrayLiteral {
                elements,
                annotation: (_, typ),
                ..
            } => self.transpile_array_literal(doc, elements, typ),
            IrExpr::ObjectLiteral {
                properties,
                annotation: (_, typ),
                ..
            } => match typ {
                Type::Object(fields) => self.transpile_object_literal(doc, properties, fields),
                _ => panic!("Object literal must have object type"),
            },
            IrExpr::BinaryOp {
                left,
                operator: BinaryOp::Eq,
                right,
                ..
            } => {
                // Check the types of both operands for safety
                match (left.typ(), right.typ()) {
                    (Type::Bool, Type::Bool) => self.transpile_bool_equality(doc, left, right),
                    (Type::String, Type::String) => {
                        self.transpile_string_equality(doc, left, right)
                    }
                    _ => panic!(
                        "Equality comparison only supported for matching bool or string types, got {:?} and {:?}",
                        left.typ(),
                        right.typ()
                    ),
                }
            }
            IrExpr::UnaryOp {
                operator: UnaryOp::Not,
                operand,
                ..
            } => self.transpile_not(doc, operand),
            IrExpr::JsonEncode { value, .. } => self.transpile_json_encode(doc, value),
        }
    }
}

/// Trait for transpiling statements to language-specific representations
pub trait StatementTranspiler: ExpressionTranspiler {
    /// Transpile a Write statement (literal string output)
    fn transpile_write(&mut self, doc: &mut Doc, content: &str);

    /// Transpile a WriteExpr statement (expression output with optional escaping)
    fn transpile_write_expr(&mut self, doc: &mut Doc, expr: &IrExpr, escape: bool);

    /// Transpile an If statement (conditional execution)
    fn transpile_if(&mut self, doc: &mut Doc, condition: &IrExpr, body: &[IrStatement]);

    /// Transpile a For loop (iteration over array)
    fn transpile_for(&mut self, doc: &mut Doc, var: &str, array: &IrExpr, body: &[IrStatement]);

    /// Transpile a Let statement (variable binding with scope)
    fn transpile_let(&mut self, doc: &mut Doc, var: &str, value: &IrExpr, body: &[IrStatement]);

    /// Main dispatcher for transpiling statements
    fn transpile_statement(&mut self, doc: &mut Doc, statement: &IrStatement) {
        match statement {
            IrStatement::Write { content, .. } => {
                self.transpile_write(doc, content);
            }
            IrStatement::WriteExpr { expr, escape, .. } => {
                self.transpile_write_expr(doc, expr, *escape);
            }
            IrStatement::If {
                condition, body, ..
            } => {
                self.transpile_if(doc, condition, body);
            }
            IrStatement::For {
                var, array, body, ..
            } => {
                self.transpile_for(doc, var.as_str(), array, body);
            }
            IrStatement::Let {
                var, value, body, ..
            } => {
                self.transpile_let(doc, var.as_str(), value, body);
            }
        }
    }

    /// Transpile multiple statements
    fn transpile_statements(&mut self, doc: &mut Doc, statements: &[IrStatement]) {
        for statement in statements {
            self.transpile_statement(doc, statement);
        }
    }
}

/// Main transpiler trait for complete IR module transpilation
pub trait Transpiler: StatementTranspiler {
    /// Transpile an entrypoint to a function/method in the target language
    fn transpile_entrypoint(&mut self, doc: &mut Doc, name: &str, entrypoint: &IrEntrypoint);

    /// Transpile a complete IR module to the target language
    fn transpile_module(&mut self, ir_module: &IrModule) -> String;
}
