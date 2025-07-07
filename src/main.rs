mod common;
mod parser;
mod sexpr;
mod tokenizer;
mod typechecker;
mod unifier;

use parser::parse;
use tokenizer::tokenize;

fn main() {
    println!("{:#?}", parse(tokenize("<div>foo".to_string())));
}
