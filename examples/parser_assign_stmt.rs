
extern crate parser;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {
    let src = "
int main()
{
    while (a > 5)
    {
        if (a == 0)
            break;
    }

    return 0;
}
    ";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {}\n", parser.run());
    parser.dump();
}