use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::ParsedExpr;
use crate::dop::ParsedType;
use crate::dop::VarName;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use pretty::BoxDoc;

use super::parsed_node::ParsedNode;

/// A ParsedParameter represents a parsed parameter with type annotation
/// and optional default value.
/// E.g. <my-comp {x: String, y: String = "default"}>
///                ^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^
#[derive(Debug, Clone)]
pub struct ParsedParameter {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: ParsedType,
    pub default_value: Option<ParsedExpr>,
}

impl Display for ParsedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl ParsedParameter {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let base = BoxDoc::text(self.var_name.as_str())
            .append(BoxDoc::text(": "))
            .append(self.var_type.to_doc());
        match &self.default_value {
            Some(default) => base.append(BoxDoc::text(" = ")).append(default.to_doc()),
            None => base,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedAttributeValue {
    Expressions(Vec<ParsedExpr>),
    /// A quoted string value. None means empty string like `attr=""`
    String(Option<DocumentRange>),
}

impl ParsedAttributeValue {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedAttributeValue::Expressions(exprs) => BoxDoc::text("{")
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            exprs.iter().map(|e| e.to_doc()),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                        .nest(2),
                )
                .append(BoxDoc::line_())
                .append(BoxDoc::text("}"))
                .group(),
            ParsedAttributeValue::String(range) => {
                let content = range.as_ref().map(|r| r.as_str()).unwrap_or("");
                BoxDoc::text(format!("\"{}\"", content))
            }
        }
    }
}

/// A ParsedAttribute is an attribute on a node, it can either
/// be:
/// * empty - <foo a>
/// * an expression - <foo a={bar}>
/// * or a string value - <foo a="b">
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
            let mut doc = BoxDoc::nil();
            let mut prev_was_import = false;
            for (i, decl) in self.declarations.iter().enumerate() {
                if i > 0 {
                    doc = doc.append(BoxDoc::line());
                    let curr_is_import = matches!(decl, ParsedDeclaration::Import(_));
                    if !(prev_was_import && curr_is_import) {
                        doc = doc.append(BoxDoc::line());
                    }
                }
                doc = doc.append(decl.to_doc());
                prev_was_import = matches!(decl, ParsedDeclaration::Import(_));
            }
            doc.append(BoxDoc::line())
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
                    .append(
                        BoxDoc::line_()
                            .append(BoxDoc::intersperse(
                                params.iter().map(|p| p.to_doc()),
                                BoxDoc::text(",").append(BoxDoc::line()),
                            ))
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .nest(2),
                    )
                    .append(BoxDoc::line_())
                    .append(BoxDoc::text("}"))
                    .group(),
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
