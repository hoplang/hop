mod common;
mod parser;
mod parser_tests;
mod tokenizer;
mod tokenizer_tests;

use parser::parse;
use tokenizer::tokenize;

fn main() {
    println!("{:#?}", parse(tokenize("<div>foo".to_string())));
}
