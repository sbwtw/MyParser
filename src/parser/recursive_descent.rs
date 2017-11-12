
use token::*;
use token::Token::*;
use lexer::Lexer;
use parser::Parser;

///
/// variable = ...
/// type = char | short | ...
/// variable_define = type variable ;
///

pub struct RecursiveDescentParser {
    tokens: Vec<Token>,

    current: usize,
}

impl RecursiveDescentParser {
    pub fn new(lexer: Lexer) -> RecursiveDescentParser {
        RecursiveDescentParser {
            tokens: lexer.collect(),

            current: 0,
        }
    }

    fn match_type(&mut self) -> bool {

        if let KeyWord(ref k) = self.tokens[self.current] {
            if k.is_type() {
                self.current += 1;
                return true;
            }
        }

        return false;
    }

    fn match_variable_define(&mut self) -> bool {

        let cur = self.current;

        if self.match_type() && self.match_variable() && self.term(Token::Semicolon) {
            return true;
        }

        self.current = cur;
        return false;
    }

    fn match_variable(&mut self) -> bool {

        if let Variable(_) = self.tokens[self.current] {
            self.current += 1;
            return true;
        }

        return false;
    }

    fn term(&mut self, tok: Token) -> bool {
        if self.tokens[self.current] == tok {
            self.current += 1;
            return true;
        }

        return false;
    }
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        self.match_variable_define()
    }
}