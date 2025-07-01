mod common;
mod tokenizer;

use tokenizer::tokenize;

fn main() {
    println!("{:#?}", tokenize("<div>foo".to_string()));
}
