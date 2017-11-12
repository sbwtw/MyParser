
mod token;
mod lexer;
mod parser;

use lexer::*;
use parser::Parser;
use parser::recursive_descent::RecursiveDescentParser;

use std::fs::File;

fn main() {
    // let f = File::open("test/test.c").unwrap();
    let src = "int a ;";
    let lexer = Lexer::new(src.as_bytes());
    let mut parser = RecursiveDescentParser::new(lexer);

    println!("{}", parser.run());
}
