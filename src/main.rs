mod common;
mod parser;
mod tokenizer;

use parser::parse;
use tokenizer::tokenize;

fn main() {
    println!("{:#?}", parse(tokenize("<div>foo".to_string())));
}
