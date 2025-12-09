//! Parser for declaration syntax (import and record declarations).

use std::iter::Peekable;

use crate::document::document_cursor::DocumentRange;
use crate::dop::Parser as DopParser;
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

        // Expect identifier (component name)
        let (token, name_range) = self.next_token()?;
        let name = match token {
            Token::Identifier(range) => range,
            _ => {
                return Err(ParseError::new(
                    format!("Expected component name after 'import', got {}", token),
                    name_range,
                ));
            }
        };

        // Expect "from"
        let (token, from_range) = self.next_token()?;
        if token != Token::From {
            return Err(ParseError::new(
                format!("Expected 'from' after component name, got {}", token),
                from_range,
            ));
        }

        // Expect string (path)
        let (token, path_token_range) = self.next_token()?;
        let path = match token {
            Token::String(range) => range,
            _ => {
                return Err(ParseError::new(
                    format!("Expected quoted path after 'from', got {}", token),
                    path_token_range,
                ));
            }
        };

        // Parse the module name from the path
        // Strip the @/ prefix for internal module resolution
        let module_name_input = match path.as_str().strip_prefix("@/") {
            Some(n) => n,
            None => {
                return Err(ParseError::MissingAtPrefixInImportPath { range: path });
            }
        };
        let module_name = ModuleName::new(module_name_input.to_string()).map_err(|e| {
            ParseError::InvalidModuleName {
                error: e,
                range: path.clone(),
            }
        })?;

        Ok(Some(Declaration::Import {
            name,
            path,
            module_name,
            range: import_range.to(path_token_range),
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
        let record_text = full_range.as_str();
        let declaration = DopParser::from(record_text)
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

    /// Try to parse any declaration.
    ///
    /// Returns `Ok(Some(declaration))` if successful,
    /// `Ok(None)` if the input doesn't look like a declaration,
    /// or `Err` if parsing fails.
    pub fn parse(&mut self) -> Result<Option<Declaration>, ParseError> {
        match self.peek_token() {
            Some(Token::Import) => self.parse_import(),
            Some(Token::Record) => self.parse_record(),
            _ => Ok(None),
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
    fn parse_import() {
        check(
            indoc! {r#"
                import UserList from "@/user_list"
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  path: "@/user_list",
                  module_name: user_list,
                }"#]],
        );
    }

    #[test]
    fn parse_import_with_path() {
        check(
            indoc! {r#"
                import Header from "@/components/header"
            "#},
            expect![[r#"
                Import {
                  name: Header,
                  path: "@/components/header",
                  module_name: components/header,
                }"#]],
        );
    }

    #[test]
    fn parse_import_missing_at_prefix() {
        check(
            indoc! {r#"
                import Header from "./components/header"
            "#},
            expect![
                "Error: Import paths must start with '@/' where '@' indicates the root directory"
            ],
        );
    }

    #[test]
    fn parse_record() {
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
    fn parse_record_with_nested_types() {
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
    fn not_a_declaration() {
        check(
            indoc! {"
                <div>hello</div>
            "},
            expect!["None"],
        );
    }

    #[test]
    fn text_starting_with_i() {
        check(
            indoc! {"
                important message
            "},
            expect!["None"],
        );
    }

    #[test]
    fn text_starting_with_r() {
        check(
            indoc! {"
                running fast
            "},
            expect!["None"],
        );
    }

    #[test]
    fn import_with_leading_empty_lines() {
        check(
            indoc! {r#"


                import UserList from "@/user_list"
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  path: "@/user_list",
                  module_name: user_list,
                }"#]],
        );
    }
}
