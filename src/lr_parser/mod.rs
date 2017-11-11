
use lexer::*;
use token::*;

pub trait Parser {}

pub struct LRParser {
    lexer: Lexer,
    tokens: Vec<Token>,
}

impl LRParser {
    pub fn new(lexer: Lexer) -> LRParser {
        LRParser {
            lexer: lexer,
            tokens: vec![],
        }
    }
}

impl Parser for LRParser {}