mod common;
mod compiler;
mod formatter;
mod parser;
mod runtime;
mod scriptcollector;
mod tokenizer;
mod toposorter;
mod typechecker;
mod unifier;

use formatter::ErrorFormatter;
use parser::parse;
use tokenizer::tokenize;

// Example with multiple types of errors
const SOURCE_CODE: &str = r#"<component name="example">
    <div>Hello 😀</div> <render>
    <span>Some text</span>
    <br></br>
                                        <for each="items">
        <p>Item: unclosed paragraph
    </unknown-tag>
</component>"#;

fn main() {
    let tokens = tokenize(SOURCE_CODE);
    let parse_result = parse(tokens);

    if !parse_result.errors.is_empty() {
        println!("Found {} parse error(s):\n", parse_result.errors.len());

        let formatter = ErrorFormatter::new(SOURCE_CODE.to_string(), "example.hop".to_string());

        for (i, error) in parse_result.errors.iter().enumerate() {
            if i > 0 {
                println!();
            }
            print!("{}", formatter.format_error(error));
        }
    } else {
        println!("✅ No parse errors!");
    }
}
