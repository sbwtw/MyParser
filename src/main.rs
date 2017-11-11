
mod token;
mod lexer;
mod lr_parser;

use lexer::*;
use lr_parser::*;

use std::fs::File;

fn main() {
    // let source = "if (11 + 22 == 33) then 2/1//aaa\n else b=1 (i + b++)".to_owned();
    // let source = "#include <iostream.h>".to_owned();
    // let source = "/*aaa*/".to_owned();
    // let source = r#""this\\ \n is a \"literal\" string""#;
    // let mut lexer = Lexer::new(source.as_bytes());

    let f = File::open("test/test.c").unwrap();
    let mut lexer = Lexer::new(f);

    // while let Some(tok) = lexer.next() {
    //     println!("{}", tok);
    // }

    let parser = LRParser::new(lexer);
}
