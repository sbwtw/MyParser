
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {
    // let src = "struct S { int a; double b; };";
    let src = "num1 + num2 * num3 + num4 / 23";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    parser.run();
    parser.dump();
}