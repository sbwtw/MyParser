
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {
    let src = "int func() { a = 1; }";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {}\n", parser.run());
    parser.dump();
}