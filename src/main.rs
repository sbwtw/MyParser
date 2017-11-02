
mod token;
mod lexer;

use token::*;
use lexer::*;

fn main() {
    let source = "if (11 + 22 == 33) then 2/1//aaa\n else bbb (i + b++)".to_owned();
    // let source = "#include <iostream.h>".to_owned();

    let mut lexer = Lexer::new(source.as_bytes());
    while let Some(tok) = lexer.next() {
        match tok {
            Token::Space => continue,
            _ => println!("{:?}", tok),
        };
    }
}
