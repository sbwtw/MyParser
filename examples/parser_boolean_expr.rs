
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {
    let src = "num1 == num3 + num4 || (num3 > num4) && !num2 == 1";
    // let src = "a == b || c + d == 1";
    // let src = "a+b!=c+d||(e+f>=0) && (d=1||f=2)";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    parser.run();
    parser.dump();
}