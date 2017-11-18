
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;
use parser::token::*;
use parser::token::Token::*;

use std::fs::File;

fn main() {
    let src = "struct S { int a; double b; };";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    parser.run();
    parser.dump();
}