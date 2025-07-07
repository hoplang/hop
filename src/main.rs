mod common;
mod compiler;
mod parser;
mod scriptbuilder;
mod tokenizer;
mod toposorter;
mod typechecker;
mod unifier;

use miette::{miette, Result};
use parser::parse;
use tokenizer::tokenize;

// Example with multiple types of errors
const SOURCE_CODE: &str = r#"<component name="example">
    <div>Hello 🌍</div>
    <render>
    <span>Some text</span>
    <br></br>
    <for each="items">
        <p>Item: unclosed paragraph
    </unknown-tag>
</component>"#;

fn main() -> Result<()> {
    let tokens = tokenize(SOURCE_CODE.to_string());
    let parse_result = parse(tokens);

    if !parse_result.errors.is_empty() {
        println!("Found {} parse error(s):\n", parse_result.errors.len());

        for (_, error) in parse_result.errors.iter().enumerate() {
            let report = miette!(labels = error.labels(), "{}", error)
                .with_source_code(SOURCE_CODE.to_string());
            println!("{:?}", report);
        }
    } else {
        println!("✅ No parse errors!");
    }

    Ok(())
}
