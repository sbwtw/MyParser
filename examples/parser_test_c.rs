
extern crate parser;

use parser::lexer::*;
use parser::parser::Parser;
use parser::parser::recursive_descent::*;

use std::fs::File;

fn main() {
    let f = File::open("test/test.c").unwrap();
    let mut parser = RecursiveDescentParser::new(SimpleLexer::new(f));

    parser.run().ok();
    parser.dump();
}