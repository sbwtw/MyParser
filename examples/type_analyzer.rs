
extern crate parser;
extern crate env_logger;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;

fn main() {

    env_logger::init();

    let src = "
struct S { int a, b; char c; };
    ";
    let mut parser = RecursiveDescentParser::new(SimpleLexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {:?}\n", parser.run());
    parser.dump();

    // let mut type_analyzer = TypeAnalyzer::new(parser.syntax_tree());
    // println!();
    // println!();
}