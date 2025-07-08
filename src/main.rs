mod common;
mod compiler;
mod formatter;
mod parser;
mod runtime;
mod scriptcollector;
mod server;
mod tokenizer;
mod toposorter;
mod typechecker;
mod unifier;

use formatter::ErrorFormatter;
use parser::parse;
use tokenizer::tokenize;

// Example with multiple types of errors
const SOURCE_CODE: &str = r#"<component name="example">
    <div>Hello 😀</div> <render!>
    <span>Some text</span>
    <br></br>
                                        <for each="items">
        <p>Item: unclosed paragraph
    </unknown-tag>
</component>"#;

fn main() {
    let mut errors = Vec::new();
    let tokens = tokenize(SOURCE_CODE, &mut errors);
    let _ = parse(tokens, &mut errors);

    if !errors.is_empty() {
        println!("Found {} parse error(s):\n", errors.len());

        let formatter = ErrorFormatter::new(SOURCE_CODE.to_string(), "example.hop".to_string());

        for (i, error) in errors.iter().enumerate() {
            if i > 0 {
                println!();
            }
            print!("{}", formatter.format_error(error));
        }
    } else {
        println!("✅ No parse errors!");
    }
}
