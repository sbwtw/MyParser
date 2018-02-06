
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {
    // let src = "struct S { int a; double b; };";
    let src = "

int f(int a, int b)
{
    if (a >= 0)
        return a;

    return 1 * 1 * 1 * 1;
}
    ";

    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("{:?}", parser.run());
    parser.dump();
}