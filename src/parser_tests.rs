#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::Path;

    use crate::common::{ComponentNode, CondNode, ForNode, NativeHTMLNode, Node, RenderNode};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;

    pub fn format_tree(root: &Node) -> String {
        let mut lines = Vec::new();

        fn format_node(node: &Node, depth: usize, lines: &mut Vec<String>) {
            let indent = "\t".repeat(depth);

            match node {
                Node::Import(_) => {
                    lines.push(format!("{}import", indent));
                }
                Node::Doctype(_) => {
                    lines.push(format!("{}doctype", indent));
                }
                Node::Render(RenderNode { children, .. }) => {
                    lines.push(format!("{}render", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::Cond(CondNode { children, .. }) => {
                    lines.push(format!("{}cond", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::For(ForNode { children, .. }) => {
                    lines.push(format!("{}for", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::Component(ComponentNode { children, .. }) => {
                    lines.push(format!("{}component", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::NativeHTML(NativeHTMLNode {
                    value, children, ..
                }) => {
                    lines.push(format!("{}{}", indent, value));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                _ => {}
            }
        }

        format_node(root, 0, &mut lines);
        lines.join("\n")
    }

    #[test]
    fn test_parser_negative() {
        let test_data_dir = Path::new("test_data/parser/negative");

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
            let expected_output: &str = archive.get("output.txt").unwrap().content.trim();

            let result = parse(tokenize(input_html.to_string()));

            assert_eq!(
                result.errors[0].message, expected_output,
                "Mismatch in file: {}",
                file_name
            );
        }
    }

    #[test]
    fn test_parser_positive() {
        let test_data_dir = Path::new("test_data/parser/positive");

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
            let expected_output: &str = archive.get("output.txt").unwrap().content.trim();

            let result = parse(tokenize(input_html.to_string()));

            for component in result.components {
                if component.name_attr.value == "main" {
                    let output = format_tree(&Node::Component(component));
                    assert_eq!(output, expected_output, "Mismatch in file: {}", file_name);
                }
            }
        }
    }
}
