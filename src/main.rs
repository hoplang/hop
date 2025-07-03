mod common;
mod tokenizer;
mod parser;

use tokenizer::tokenize;

fn main() {
    println!("{:#?}", tokenize("<div>foo".to_string()));
}
