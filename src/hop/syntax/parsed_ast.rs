use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use pretty::BoxDoc;
use crate::dop::ParsedExpr;
use crate::dop::ParsedType;
use crate::dop::VarName;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::parsed_node::ParsedNode;

/// A ParsedParameter represents a parsed parameter with type annotation.
/// E.g. <my-comp {x: string, y: string}>
///                ^^^^^^^^^
#[derive(Debug, Clone)]
pub struct ParsedParameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: ParsedType,
}

impl Display for ParsedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl ParsedParameter {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.var_name.as_str())
            .append(BoxDoc::text(": "))
            .append(self.var_type.to_doc())
    }
}

#[derive(Debug, Clone)]
pub enum ParsedAttributeValue {
    Expressions(Vec<ParsedExpr>),
    String(DocumentRange),
}

impl ParsedAttributeValue {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedAttributeValue::Expressions(exprs) => {
                let mut doc = BoxDoc::text("{");
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        doc = doc.append(BoxDoc::text(", "));
                    }
                    doc = doc.append(expr.to_doc());
                }
                doc.append(BoxDoc::text("}"))
            }
            ParsedAttributeValue::String(range) => {
                BoxDoc::text(format!("\"{}\"", range.as_str()))
            }
        }
    }
}

/// A ParsedAttribute is an attribute on a node, it can either
/// be empty, an expression or a string value.
#[derive(Debug, Clone)]
pub struct ParsedAttribute {
    pub name: DocumentRange,
    pub value: Option<ParsedAttributeValue>,
}

impl ParsedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(self.name.as_str());
        match &self.value {
            Some(value) => name_doc.append(BoxDoc::text("=")).append(value.to_doc()),
            None => name_doc,
        }
    }
}

/// A declaration in a Hop module - can be an import, record, enum, or component.
#[derive(Debug, Clone)]
pub enum ParsedDeclaration {
    Import(ParsedImportDeclaration),
    Record(ParsedRecordDeclaration),
    Enum(ParsedEnumDeclaration),
    Component(ParsedComponentDeclaration),
}

impl ParsedDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedDeclaration::Import(import) => import.to_doc(),
            ParsedDeclaration::Record(record) => record.to_doc(),
            ParsedDeclaration::Enum(e) => e.to_doc(),
            ParsedDeclaration::Component(component) => component.to_doc(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedAst {
    pub name: ModuleName,
    declarations: Vec<ParsedDeclaration>,
}

impl ParsedAst {
    pub fn new(name: ModuleName, declarations: Vec<ParsedDeclaration>) -> Self {
        Self { name, declarations }
    }

    /// Returns a reference to all declarations in the AST, preserving their original order.
    pub fn get_declarations(&self) -> &[ParsedDeclaration] {
        &self.declarations
    }

    pub fn get_component_declaration(&self, name: &str) -> Option<&ParsedComponentDeclaration> {
        self.get_component_declarations()
            .find(|n| n.tag_name.as_str() == name)
    }

    /// Finds a record declaration by name.
    pub fn get_record_declaration(&self, name: &str) -> Option<&ParsedRecordDeclaration> {
        self.get_record_declarations().find(|r| r.name() == name)
    }

    /// Returns an iterator over all component declarations in the AST.
    pub fn get_component_declarations(&self) -> impl Iterator<Item = &ParsedComponentDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Component(c) => Some(c),
            _ => None,
        })
    }

    /// Returns an iterator over all import declarations in the AST.
    pub fn get_import_declarations(&self) -> impl Iterator<Item = &ParsedImportDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Import(i) => Some(i),
            _ => None,
        })
    }

    /// Returns an iterator over all record declarations in the AST.
    pub fn get_record_declarations(&self) -> impl Iterator<Item = &ParsedRecordDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Record(r) => Some(r),
            _ => None,
        })
    }

    /// Returns an iterator over all enum declarations in the AST.
    pub fn get_enum_declarations(&self) -> impl Iterator<Item = &ParsedEnumDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            ParsedDeclaration::Enum(e) => Some(e),
            _ => None,
        })
    }

    /// Returns an iterator over all nodes in the AST, iterating depth-first.
    pub fn iter_all_nodes(&self) -> impl Iterator<Item = &ParsedNode> {
        self.get_component_declarations()
            .flat_map(|n| &n.children)
            .flat_map(|n| n.iter_depth_first())
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        if self.declarations.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(
                self.declarations.iter().map(|d| d.to_doc()),
                BoxDoc::line().append(BoxDoc::line()),
            )
            .append(BoxDoc::line())
        }
    }
}

impl Display for ParsedAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedImportDeclaration {
    pub type_name: TypeName,
    /// The range of the type name in the source (for error reporting)
    pub type_name_range: DocumentRange,
    /// The full path range for error reporting (covers module::TypeName)
    pub path: DocumentRange,
    pub module_name: ModuleName,
}

impl ParsedImportDeclaration {
    pub fn imported_module(&self) -> &ModuleName {
        &self.module_name
    }
    pub fn imported_type_name(&self) -> &TypeName {
        &self.type_name
    }
    pub fn type_name_range(&self) -> &DocumentRange {
        &self.type_name_range
    }
    pub fn imports_type(&self, type_name: &str) -> bool {
        self.type_name.as_str() == type_name
    }
    pub fn imports_from(&self, module_name: &ModuleName) -> bool {
        &self.module_name == module_name
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("import")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.module_name.to_string()))
            .append(BoxDoc::text("::"))
            .append(BoxDoc::text(self.type_name.as_str()))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedRecordDeclarationField {
    pub name: FieldName,
    pub field_type: ParsedType,
}

impl ParsedRecordDeclarationField {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.name.as_str())
            .append(BoxDoc::text(": "))
            .append(self.field_type.to_doc())
    }
}

#[derive(Debug, Clone)]
pub struct ParsedRecordDeclaration {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub fields: Vec<ParsedRecordDeclarationField>,
}

impl ParsedRecordDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("record")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.fields.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.fields.iter().map(|f| f.to_doc()),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedEnumDeclarationVariant {
    pub name: TypeName,
}

impl ParsedEnumDeclarationVariant {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.name.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct ParsedEnumDeclaration {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub variants: Vec<ParsedEnumDeclarationVariant>,
}

impl ParsedEnumDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("enum")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.variants.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.variants.iter().map(|v| v.to_doc()),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedComponentDeclaration {
    pub component_name: ComponentName,
    pub tag_name: DocumentRange,
    pub closing_tag_name: Option<DocumentRange>,
    pub params: Option<(Vec<ParsedParameter>, DocumentRange)>,
    pub children: Vec<ParsedNode>,
    pub range: DocumentRange,
}

impl Ranged for ParsedComponentDeclaration {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl ParsedComponentDeclaration {
    pub fn tag_name_ranges(&self) -> impl Iterator<Item = &DocumentRange> {
        self.closing_tag_name.iter().chain(Some(&self.tag_name))
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("<")
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(match &self.params {
                Some((params, _)) if !params.is_empty() => BoxDoc::text(" {")
                    .append(BoxDoc::intersperse(
                        params.iter().map(|p| p.to_doc()),
                        BoxDoc::text(", "),
                    ))
                    .append(BoxDoc::text("}")),
                _ => BoxDoc::nil(),
            })
            .append(BoxDoc::text(">"))
            .append(if self.children.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.children.iter().map(|c| c.to_doc()),
                        BoxDoc::line(),
                    ))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("</"))
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(BoxDoc::text(">"))
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(ModuleName::new("test").unwrap(), source.to_string(), &mut errors);
        expected.assert_eq(&ast.to_doc().pretty(80).to_string());
    }

    #[test]
    fn import_declaration_to_doc() {
        check(
            "import foo::Bar",
            expect![[r#"
                import foo::Bar
            "#]],
        );
    }

    #[test]
    fn record_declaration_single_field_to_doc() {
        check(
            "record User { name: String }",
            expect![[r#"
                record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn enum_declaration_multiple_variants_to_doc() {
        check(
            "enum Color { Red, Green, Blue }",
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }
            "#]],
        );
    }

    #[test]
    fn two_record_declarations_to_doc() {
        check(
            "record User { name: String, age: Int } record Post { title: String, author: User }",
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                record Post {
                  title: String,
                  author: User,
                }
            "#]],
        );
    }

    #[test]
    fn component_declaration_to_doc() {
        check(
            "<Main {name: String, count: Int}><div>{name}</div></Main>",
            expect![[r#"
                <Main {name: String, count: Int}>
                  <div>
                    {name}
                  </div>
                </Main>
            "#]],
        );
    }
}
