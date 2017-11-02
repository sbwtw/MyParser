
mod token;
mod lexer;

use token::*;
use lexer::*;

use std::fs::File;

fn main() {
    // let source = "if (11 + 22 == 33) then 2/1//aaa\n else b=1 (i + b++)".to_owned();
    // let source = "#include <iostream.h>".to_owned();
    // let source = "/*aaa*/".to_owned();

    let f = File::open("test/test.c").unwrap();

    // let mut lexer = Lexer::new(source.as_bytes());
    let mut lexer = Lexer::new(f);
    while let Some(tok) = lexer.next() {
        match tok {
            Token::Space => continue,
            _ => println!("{:?}", tok),
        };
    }
}
