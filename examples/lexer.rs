
extern crate parser;

use parser::lexer::*;

use std::fs::File;

fn main() {
    let f = File::open("test/test.c").unwrap();
    let mut lexer = Lexer::new(f);

    while let Some(tok) = lexer.next() {
        println!("{:?}", tok);
    }
}