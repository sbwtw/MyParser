
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {
    let src = "
    struct S { int a; };

    void func()
    {
        a = 1;

        if (a == 2 || a <= 1)
            a = 1;
    }
    ";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {}\n", parser.run());
    parser.dump();
}