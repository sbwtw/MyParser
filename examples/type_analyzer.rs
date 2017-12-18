
extern crate parser;
extern crate env_logger;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;
use parser::parser::type_analyzer::*;

fn main() {

    env_logger::init().unwrap();

    let src = "

struct S { int a, b; };

int main()
{
    int a;

    return 0;
}
    ";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {:?}\n", parser.run());
    parser.dump();

    let mut type_analyzer = TypeAnalyzer::new(parser.syntax_tree());
    println!();
    println!();
}