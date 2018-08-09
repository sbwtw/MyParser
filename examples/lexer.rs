
extern crate parser;

use parser::lexer::*;

// use std::fs::File;

fn main() {
    // let f = File::open("test/test.c").unwrap();
    let s =
"int main() {
    a = \"\\s\";
}
";
    let mut lexer = SimpleLexer::new(s.as_bytes());

    println!("{}", s);
    while let Some(tok) = lexer.next() {
        println!("{:?}", tok);
    }
}