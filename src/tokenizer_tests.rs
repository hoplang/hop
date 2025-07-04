#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::Path;

    use crate::{
        common::{Attribute, Token, TokenType},
        tokenizer::tokenize,
    };

    fn format_attr(attr: &Attribute) -> String {
        format!(
            "{}=[{}] {}:{}-{}:{}",
            attr.name,
            attr.value,
            attr.range.start.line,
            attr.range.start.column,
            attr.range.end.line,
            attr.range.end.column
        )
    }

    fn format_token(token: &Token) -> String {
        let start = &token.range.start;
        let end = &token.range.end;

        match token.token_type {
            TokenType::Text | TokenType::Doctype | TokenType::Comment => {
                format!(
                    "{:?} {}:{}-{}:{}",
                    token.token_type, start.line, start.column, end.line, end.column
                )
            }
            TokenType::EndTag => {
                format!(
                    "{:?}({}) {}:{}-{}:{}",
                    token.token_type, token.value, start.line, start.column, end.line, end.column
                )
            }
            TokenType::StartTag | TokenType::SelfClosingTag => {
                let attrs = token
                    .attributes
                    .iter()
                    .map(format_attr)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(
                    "{:?}({}) [{}] {}:{}-{}:{}",
                    token.token_type,
                    token.value,
                    attrs,
                    start.line,
                    start.column,
                    end.line,
                    end.column
                )
            }
            TokenType::Error => {
                format!("{:?}", token.token_type)
            }
        }
    }

    #[test]
    fn test_tokenizer_with_txtar_files() {
        let test_data_dir = Path::new("test_data/tokenizer");

        let entries = fs::read_dir(test_data_dir).expect("Failed to read test_data directory");

        for entry in entries {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            println!("Running test for: {}", file_name);

            let content = fs::read_to_string(&path)
                .expect(&format!("Failed to read file: {}", path.display()));

            let archive = Archive::from(&content);

            let input_html = archive.get("input.html").unwrap().content.trim();
            let expected_tokens: Vec<&str> = archive
                .get("tokens.txt")
                .unwrap()
                .content
                .trim()
                .split('\n')
                .collect();

            let tokens = tokenize(input_html.to_string());
            let actual_tokens: Vec<String> = tokens.iter().map(format_token).collect();

            assert_eq!(
                actual_tokens, expected_tokens,
                "Mismatch in file: {}",
                file_name
            );
        }
    }
}
