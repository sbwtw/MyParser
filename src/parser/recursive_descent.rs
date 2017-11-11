
use token::*;
use lexer::Lexer;
use parser::Parser;

pub struct RecursiveDescentParser {
    lexer: Lexer,
    tokens: Vec<Token>,
}

impl RecursiveDescentParser {
    pub fn new(lexer: Lexer) -> RecursiveDescentParser {
        RecursiveDescentParser {
            lexer: lexer,
            tokens: vec![],
        }
    }
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        true
    }
}