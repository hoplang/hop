use std::iter::Peekable;

use crate::document::document_cursor::DocumentRange;
use crate::dop::Parser as DopParser;
use crate::dop::type_name::TypeName;
use crate::hop::module_name::ModuleName;
use crate::hop::parse_error::ParseError;

use super::Declaration;
use super::tokenizer::{Token, Tokenizer};

/// Parser for declaration syntax.
pub struct Parser {
    iter: Peekable<Tokenizer>,
    range: DocumentRange,
}

impl From<DocumentRange> for Parser {
    fn from(range: DocumentRange) -> Self {
        Self {
            iter: Tokenizer::from(range.clone()).peekable(),
            range,
        }
    }
}

impl Parser {
    /// Check if the next token matches (without consuming it).
    fn peek_token(&mut self) -> Option<Token> {
        self.iter
            .peek()
            .and_then(|res| res.as_ref().ok())
            .map(|(token, _)| token.clone())
    }

    /// Consume the next token.
    fn next_token(&mut self) -> Result<(Token, DocumentRange), ParseError> {
        match self.iter.next() {
            Some(Ok((token, range))) => Ok((token, range)),
            Some(Err(e)) => Err(e),
            None => Err(ParseError::GenericError {
                message: "Unexpected end of input".to_string(),
                range: self.range.clone(),
            }),
        }
    }

    /// Try to parse an import declaration.
    ///
    /// Syntax: `import module::path::ComponentName`
    ///
    /// Returns `Ok(Some(Declaration::Import))` if successful,
    /// `Ok(None)` if the input doesn't start with "import",
    /// or `Err` if parsing fails after recognizing an import.
    pub fn parse_import(&mut self) -> Result<Option<Declaration>, ParseError> {
        // Check if this looks like an import
        if self.peek_token() != Some(Token::Import) {
            return Ok(None);
        }

        // Consume "import"
        let (_, import_range) = self.next_token()?;

        // Parse the module path: identifier (:: identifier)*
        // The last identifier is the component name
        let mut path_segments: Vec<DocumentRange> = Vec::new();

        // Expect at least one identifier
        let (token, first_range) = self.next_token()?;
        let first_segment = match token {
            Token::Identifier(range) => range,
            _ => {
                return Err(ParseError::new(
                    format!("Expected module path after 'import', got {}", token),
                    first_range,
                ));
            }
        };
        path_segments.push(first_segment);
        let mut last_range = first_range;

        // Continue parsing :: identifier sequences
        while self.peek_token() == Some(Token::ColonColon) {
            // Consume ::
            self.next_token()?;

            // Expect identifier
            let (token, ident_range) = self.next_token()?;
            let segment = match token {
                Token::Identifier(range) => range,
                _ => {
                    return Err(ParseError::new(
                        format!("Expected identifier after '::', got {}", token),
                        ident_range,
                    ));
                }
            };
            last_range = ident_range;
            path_segments.push(segment);
        }

        // Need at least two segments: module_name::ComponentName
        if path_segments.len() < 2 {
            return Err(ParseError::new(
                "Import path must have at least two segments: module::Component".to_string(),
                last_range,
            ));
        }

        // Last segment is the type name, rest is the module path
        let name_range = path_segments.pop().unwrap();

        // Parse the type name
        let name = TypeName::new(name_range.as_str()).map_err(|e| ParseError::InvalidTypeName {
            error: e,
            range: name_range.clone(),
        })?;

        // Build the module name from the remaining segments
        let module_path_str = path_segments
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join("/");

        let module_name =
            ModuleName::new(&module_path_str).map_err(|e| ParseError::InvalidModuleName {
                error: e,
                range: path_segments
                    .first()
                    .unwrap()
                    .clone()
                    .to(path_segments.last().unwrap().clone()),
            })?;

        // Build the full path range for error reporting
        let path_range = path_segments
            .first()
            .unwrap()
            .clone()
            .to(name_range.clone());

        Ok(Some(Declaration::Import {
            name,
            name_range: name_range.clone(),
            path: path_range,
            module_name,
            range: import_range.to(last_range),
        }))
    }

    /// Try to parse a record declaration.
    ///
    /// Returns `Ok(Some(Declaration::Record))` if successful,
    /// `Ok(None)` if the input doesn't start with "record",
    /// or `Err` if parsing fails after recognizing a record.
    pub fn parse_record(&mut self) -> Result<Option<Declaration>, ParseError> {
        // Check if this looks like a record
        if self.peek_token() != Some(Token::Record) {
            return Ok(None);
        }

        // Consume "record"
        let (_, record_range) = self.next_token()?;

        // Expect identifier (record name)
        let (token, name_range) = self.next_token()?;
        let name = match token {
            Token::Identifier(range) => range,
            _ => {
                return Err(ParseError::new(
                    format!("Expected record name after 'record', got {}", token),
                    name_range,
                ));
            }
        };

        // Expect opening brace
        let (token, open_brace_range) = self.next_token()?;
        if token != Token::LeftBrace {
            return Err(ParseError::new(
                format!("Expected '{{' after record name, got {}", token),
                open_brace_range,
            ));
        }

        // Collect all tokens until matching closing brace to find the end of the record
        let fields_start = open_brace_range.clone();
        let mut brace_count = 1;
        let mut bracket_count = 0;
        let mut last_range = open_brace_range.clone();

        while brace_count > 0 {
            let Some(next) = self.iter.next() else {
                return Err(ParseError::UnmatchedCharacter {
                    ch: '{',
                    range: fields_start,
                });
            };

            let (token, range) = next?;
            last_range = range;

            match token {
                Token::LeftBrace => brace_count += 1,
                Token::RightBrace => brace_count -= 1,
                Token::LeftBracket => bracket_count += 1,
                Token::RightBracket => bracket_count -= 1,
                _ => {}
            }

            // Check for unbalanced brackets
            if bracket_count < 0 {
                return Err(ParseError::UnmatchedCharacter {
                    ch: ']',
                    range: last_range,
                });
            }
        }

        // Get the full range of the record declaration
        let full_range = record_range.to(last_range);

        // Use the dop parser to fully parse the record declaration
        let declaration = DopParser::from(full_range.clone())
            .parse_record()
            .map_err(|e| ParseError::new(e.to_string(), full_range.clone()))?;

        // Update the name to use the range from the decl tokenizer (to preserve source info)
        let declaration = crate::dop::RecordDeclaration {
            name,
            fields: declaration.fields,
        };

        Ok(Some(Declaration::Record {
            declaration,
            range: full_range,
        }))
    }

    /// Try to parse the next declaration, skipping any non-declaration content.
    ///
    /// Returns `Ok(Some(declaration))` if successful,
    /// `Ok(None)` if there are no more declarations,
    /// or `Err` if parsing fails.
    pub fn parse(&mut self) -> Result<Option<Declaration>, ParseError> {
        loop {
            match self.iter.peek() {
                Some(Ok((Token::Import, _))) => return self.parse_import(),
                Some(Ok((Token::Record, _))) => return self.parse_record(),
                Some(_) => {
                    // Skip unknown tokens and tokenizer errors
                    self.iter.next();
                }
                None => return Ok(None),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::document_cursor::DocumentCursor;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let range = DocumentCursor::new(input.to_string()).range();
        let mut parser = Parser::from(range);

        let result = match parser.parse() {
            Ok(Some(decl)) => format!("{:?}", decl),
            Ok(None) => "None".to_string(),
            Err(e) => format!("Error: {}", e),
        };

        expected.assert_eq(&result);
    }

    #[test]
    fn should_accept_import_with_simple_path() {
        check(
            indoc! {r#"
                import user_list::UserList
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  module_name: user_list,
                }"#]],
        );
    }

    #[test]
    fn should_accept_import_with_nested_path() {
        check(
            indoc! {r#"
                import components::header::Header
            "#},
            expect![[r#"
                Import {
                  name: Header,
                  module_name: components::header,
                }"#]],
        );
    }

    #[test]
    fn should_reject_import_when_path_has_only_one_segment() {
        check(
            indoc! {r#"
                import Header
            "#},
            expect!["Error: Import path must have at least two segments: module::Component"],
        );
    }

    #[test]
    fn should_reject_import_when_type_name_is_not_pascal_case() {
        check(
            indoc! {r#"
                import foo::bar
            "#},
            expect!["Error: Type name must start with an uppercase letter"],
        );
    }

    #[test]
    fn should_accept_record_with_multiple_fields() {
        check(
            indoc! {"
                record User {name: String, age: Int}
            "},
            expect![[r#"
                Record {
                  name: User,
                  fields: {
                    name: String,
                    age: Int,
                  },
                }"#]],
        );
    }

    #[test]
    fn should_accept_record_with_nested_array_field_type() {
        check(
            indoc! {"
                record UserList {users: Array[User]}
            "},
            expect![[r#"
                Record {
                  name: UserList,
                  fields: {
                    users: Array[User],
                  },
                }"#]],
        );
    }

    #[test]
    fn should_return_none_when_input_is_html() {
        check(
            indoc! {"
                <div>hello</div>
            "},
            expect!["None"],
        );
    }

    #[test]
    fn should_accept_import_with_leading_empty_lines() {
        check(
            indoc! {r#"


                import user_list::UserList
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  module_name: user_list,
                }"#]],
        );
    }
}
