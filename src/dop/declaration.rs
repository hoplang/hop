use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::hop::module_name::ModuleName;
use pretty::BoxDoc;

use super::field_name::FieldName;
use super::syntactic_type::SyntacticType;
use super::type_name::TypeName;

/// An EnumVariant represents a variant in an enum declaration.
/// E.g. enum Color {Red, Green, Blue}
///                  ^^^
#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: TypeName,
    pub name_range: DocumentRange,
}

/// An EnumDeclaration represents a full enum type declaration.
/// E.g. enum Color {Red, Green, Blue}
#[derive(Debug, Clone)]
pub struct EnumDeclaration {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub variants: Vec<EnumVariant>,
}

impl Display for EnumDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {} {{", self.name)?;
        for (i, variant) in self.variants.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", variant.name)?;
        }
        write!(f, "}}")
    }
}

/// A RecordDeclarationField represents a field in a record declaration.
/// E.g. record Foo {bar: String, baz: Int}
///                  ^^^^^^^^^^^
#[derive(Debug, Clone)]
pub struct RecordDeclarationField<T = SyntacticType> {
    pub name: FieldName,
    pub name_range: DocumentRange,
    pub field_type: T,
}

impl Display for RecordDeclarationField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.field_type)
    }
}

/// A RecordDeclaration represents a full record type declaration.
/// E.g. record User {name: String, age: Int}
#[derive(Debug, Clone)]
pub struct RecordDeclaration<A = SyntacticType> {
    pub name: TypeName,
    pub name_range: DocumentRange,
    pub fields: Vec<RecordDeclarationField<A>>,
}

impl Display for RecordDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "record {} {{", self.name)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, "}}")
    }
}

/// A declaration: either an import, a record, or an enum definition.
#[derive(Clone)]
pub enum Declaration {
    /// An import declaration: `import module::path::Name`
    Import {
        /// The name of the imported type.
        name: TypeName,
        /// The range of the type name in the source (for error reporting).
        name_range: DocumentRange,
        /// The full path range (module::path::Name) for error reporting.
        path: DocumentRange,
        /// The parsed module name.
        module_name: ModuleName,
        /// The full range of the declaration.
        range: DocumentRange,
    },
    /// A record declaration: `record Name {fields...}`
    Record {
        /// The fully parsed record declaration.
        declaration: RecordDeclaration,
        /// The full range of the declaration.
        range: DocumentRange,
    },
    /// An enum declaration: `enum Name {Variant1, Variant2, ...}`
    Enum {
        /// The fully parsed enum declaration.
        declaration: EnumDeclaration,
        /// The full range of the declaration.
        range: DocumentRange,
    },
}

impl Declaration {
    /// Convert this declaration to a pretty-printable document.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Declaration::Import {
                name, module_name, ..
            } => BoxDoc::text("Import")
                .append(BoxDoc::space())
                .append(BoxDoc::text("{"))
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::text("name: "))
                        .append(BoxDoc::text(name.to_string()))
                        .append(BoxDoc::text(","))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("module_name: "))
                        .append(BoxDoc::text(module_name.to_string()))
                        .append(BoxDoc::text(","))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}")),
            Declaration::Record { declaration, .. } => {
                let fields_doc = if declaration.fields.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            declaration.fields.iter().map(|f| {
                                BoxDoc::text(f.name.to_string())
                                    .append(BoxDoc::text(": "))
                                    .append(BoxDoc::text(f.field_type.to_string()))
                            }),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(","))
                        .nest(2)
                        .append(BoxDoc::line())
                };

                BoxDoc::text("Record")
                    .append(BoxDoc::space())
                    .append(BoxDoc::text("{"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("name: "))
                            .append(BoxDoc::text(declaration.name.as_str()))
                            .append(BoxDoc::text(","))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("fields: {"))
                            .append(fields_doc)
                            .append(BoxDoc::text("},"))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            Declaration::Enum { declaration, .. } => {
                let variants_doc = if declaration.variants.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            declaration
                                .variants
                                .iter()
                                .map(|v| BoxDoc::text(v.name.to_string())),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(","))
                        .nest(2)
                        .append(BoxDoc::line())
                };

                BoxDoc::text("Enum")
                    .append(BoxDoc::space())
                    .append(BoxDoc::text("{"))
                    .append(
                        BoxDoc::line()
                            .append(BoxDoc::text("name: "))
                            .append(BoxDoc::text(declaration.name.as_str()))
                            .append(BoxDoc::text(","))
                            .append(BoxDoc::line())
                            .append(BoxDoc::text("variants: {"))
                            .append(variants_doc)
                            .append(BoxDoc::text("},"))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

impl fmt::Debug for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Ranged for Declaration {
    fn range(&self) -> &DocumentRange {
        match self {
            Declaration::Import { range, .. }
            | Declaration::Record { range, .. }
            | Declaration::Enum { range, .. } => range,
        }
    }
}
