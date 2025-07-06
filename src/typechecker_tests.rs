#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::tokenizer::tokenize;
    use crate::typechecker::typecheck;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::collections::HashMap;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_typechecker_positive() {
        let test_data_dir = Path::new("test_data/typechecker/positive");

        let entries = fs::read_dir(test_data_dir).expect("Failed to read test_data directory");

        for entry in entries {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            println!("Running test for: {}", file_name);

            let content = fs::read_to_string(&path)
                .expect(&format!("Failed to read file: {}", path.display()));

            let archive = Archive::from(&content);

            let input_html = archive.get("main.hop").unwrap().content.trim();
            let expected_output: &str = archive.get("type.json").unwrap().content.trim();

            let result = parse(tokenize(input_html.to_string()));

            let type_result = typecheck(&result.components, HashMap::new());

            let output = type_result
                .parameter_types
                .get("main")
                .expect("Type for main not found");

            assert_eq!(
                format!("{}", output),
                expected_output,
                "Mismatch in file: {}",
                file_name
            );
        }
    }
}
