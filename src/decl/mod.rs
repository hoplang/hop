//! Declaration extraction module.
//!
//! This module provides functionality to extract import and record declarations
//! from raw source text before hop tokenization.
//!
//! Declarations must be extracted before hop tokenization because the hop
//! tokenizer breaks text at `{` and `<` boundaries, which would split record
//! declarations like `record Foo {bar: String}` into multiple tokens.

pub mod parser;
pub mod tokenizer;

use std::fmt;

#[cfg(test)]
use crate::document::document_cursor::DocumentCursor;
use crate::document::document_cursor::DocumentRange;
use crate::dop::RecordDeclaration;
use crate::error_collector::ErrorCollector;
use crate::dop::type_name::TypeName;
use crate::hop::module_name::ModuleName;
use crate::hop::parse_error::ParseError;
use pretty::BoxDoc;

/// A declaration: either an import or a record definition.
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
                let fields_doc = BoxDoc::intersperse(
                    declaration.fields.iter().map(|f| {
                        BoxDoc::text(f.name.to_string())
                            .append(BoxDoc::text(": "))
                            .append(BoxDoc::text(f.field_type.to_string()))
                    }),
                    BoxDoc::text(",").append(BoxDoc::line()),
                );

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
                            .append(
                                BoxDoc::line()
                                    .append(fields_doc)
                                    .append(BoxDoc::text(","))
                                    .nest(2),
                            )
                            .append(BoxDoc::line())
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

/// Parse all declarations from a source range.
///
/// This function scans source text for import and record declarations.
/// It must be called BEFORE hop tokenization because the tokenizer breaks
/// at `{` characters, which would split record declarations like
/// `record Foo {bar: String}` into multiple tokens.
///
/// Returns a vector of parsed declarations.
pub fn parse_declarations_from_source(
    text_range: DocumentRange,
    errors: &mut ErrorCollector<ParseError>,
) -> Vec<Declaration> {
    let source = text_range.as_str();
    let mut declarations = Vec::new();

    // Scan through the source looking for declarations using char indices
    let mut char_indices = source.char_indices().peekable();

    while let Some((byte_pos, ch)) = char_indices.next() {
        // Skip whitespace
        if ch.is_whitespace() {
            continue;
        }

        // Check if we're at an import or record declaration
        let remaining_text = &source[byte_pos..];

        if remaining_text.starts_with("import ") || remaining_text.starts_with("record ") {
            // Create a subrange starting at the current position
            let mut sub_cursor = text_range.clone().cursor();
            // Skip `byte_pos` characters (not bytes) to get to the current position
            let chars_to_skip = source[..byte_pos].chars().count();
            for _ in 0..chars_to_skip {
                if sub_cursor.next().is_none() {
                    break;
                }
            }
            let sub_range = sub_cursor.range();
            let mut decl_parser = parser::Parser::from(sub_range);

            match decl_parser.parse() {
                Ok(Some(decl)) => {
                    let decl_len = match &decl {
                        Declaration::Import { range, .. } => range.as_str().len(),
                        Declaration::Record { range, .. } => range.as_str().len(),
                    };
                    declarations.push(decl);
                    // Skip past the declaration by advancing char_indices
                    let target_byte_pos = byte_pos + decl_len;
                    while char_indices
                        .peek()
                        .is_some_and(|(pos, _)| *pos < target_byte_pos)
                    {
                        char_indices.next();
                    }
                    continue;
                }
                Ok(None) => {
                    // Not a declaration, skip this character
                }
                Err(err) => {
                    errors.push(err);
                }
            }
        }
    }

    declarations
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check_declarations(input: &str, expected: Expect) {
        let mut errors = ErrorCollector::new();
        let range = DocumentCursor::new(input.to_string()).range();
        let declarations = parse_declarations_from_source(range, &mut errors);

        let formatted: Vec<String> = declarations
            .iter()
            .map(|decl| match decl {
                Declaration::Import {
                    name, module_name, ..
                } => {
                    format!("Import {}::{}", module_name, name.as_str())
                }
                Declaration::Record { declaration, .. } => {
                    let fields: Vec<String> = declaration
                        .fields
                        .iter()
                        .map(|f| format!("{}: {}", f.name, f.field_type))
                        .collect();
                    format!(
                        "Record {} {{{}}}",
                        declaration.name.as_str(),
                        fields.join(", ")
                    )
                }
            })
            .collect();

        expected.assert_eq(&formatted.join("\n"));
    }

    #[test]
    fn should_extract_import_declaration() {
        check_declarations(
            r#"import user_list::UserList"#,
            expect!["Import user_list::UserList"],
        );
    }

    #[test]
    fn should_extract_record_declaration() {
        check_declarations(
            "record User {name: String}",
            expect!["Record User {name: String}"],
        );
    }

    #[test]
    fn should_extract_multiple_mixed_declarations() {
        check_declarations(
            r#"import header::Header
record User {name: String}
import footer::Footer
"#,
            expect![[r#"
                Import header::Header
                Record User {name: String}
                Import footer::Footer"#]],
        );
    }

    #[test]
    fn should_not_extract_text_starting_with_import_keyword() {
        // "important" starts with "import" but is not followed by a space,
        // so it should not be recognized as a declaration
        let mut errors = ErrorCollector::new();
        let range = DocumentCursor::new("important information".to_string()).range();
        let declarations = parse_declarations_from_source(range, &mut errors);
        assert!(declarations.is_empty());
    }

    #[test]
    fn should_not_extract_text_starting_with_record_keyword() {
        // "recording" starts with "record" but is not followed by a space,
        // so it should not be recognized as a declaration
        let mut errors = ErrorCollector::new();
        let range = DocumentCursor::new("recording started".to_string()).range();
        let declarations = parse_declarations_from_source(range, &mut errors);
        assert!(declarations.is_empty());
    }

    #[test]
    fn should_extract_declaration_after_multibyte_characters() {
        check_declarations(
            r#"• bullet point
record User {name: String}"#,
            expect!["Record User {name: String}"],
        );
    }
}
