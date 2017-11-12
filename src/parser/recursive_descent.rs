
use token::*;
use token::Token::*;
use lexer::Lexer;
use parser::Parser;

///
/// variable = ...
/// type = char | short | ...
/// variable_define = type variable ;
/// struct_define = struct variable { variable_define ... } ;
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

    fn match_struct_define(&mut self) -> bool {
        let cur = self.current;

        if !self.term(Token::KeyWord(KeyWords::Struct)) {
            self.current = cur;
            return false;
        }

        match self.tokens[self.current] {
            Token::Variable(_) => self.current += 1,
            _ => return false,
        }

        if !self.term(Token::Bracket(Brackets::LeftCurlyBracket)) {
            self.current = cur;
            return false;
        }

        while self.match_variable_define() {}

        if !self.term(Token::Bracket(Brackets::RightCurlyBracket)) ||
           !self.term(Token::Semicolon) {
            self.current = cur;
            return false;
        }

        return true;
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

#[cfg(test)]
mod test {

    use RecursiveDescentParser;
    use Lexer;

    #[test]
    fn test_variable_define() {
        let tests = vec!["int number;", "short num0 ; ", "double\nd;"];

        for test in tests {
            let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
            assert!(parser.match_variable_define());
        }
    }

    #[test]
    fn test_struct_define() {
        let test = "struct Str { int a; short b; };";

        let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
        assert!(parser.match_struct_define());
    }
}