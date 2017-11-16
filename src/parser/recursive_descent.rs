
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

type MatchResult = Option<Token>;

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

    fn match_type(&mut self) -> MatchResult {

        if let KeyWord(ref k) = self.tokens[self.current] {
            if k.is_type() {
                self.current += 1;
                return Some(Token::KeyWord(k.clone()));
            }
        }

        return None;
    }

    fn match_variable_define(&mut self) -> MatchResult {
        let cur = self.current;

        if let Some(Token::KeyWord(t)) = self.match_type() {
            println!("{:?}", t);

            if let Some(Token::Variable(v)) = self.match_variable() {
                println!("{:?}", v);

                if self.term(Token::Semicolon) {
                    // return define
                    // !!!!!!!!!!!!!!!   error   !!!!!!!!!!!!!!!!!
                    return Some(Token::Semicolon);
                }
            }
        }

        self.current = cur;
        return None;
    }

    fn match_struct_define(&mut self) -> MatchResult {
        let cur = self.current;

        if !self.term(Token::KeyWord(KeyWords::Struct)) {
            self.current = cur;
            return None;
        }

        match self.tokens[self.current] {
            Token::Variable(_) => self.current += 1,
            _ => return None,
        }

        if !self.term(Token::Bracket(Brackets::LeftCurlyBracket)) {
            self.current = cur;
            return None;
        }

        while self.match_variable_define().is_some() {}

        if !self.term(Token::Bracket(Brackets::RightCurlyBracket)) ||
           !self.term(Token::Semicolon) {
            self.current = cur;
            return None;
        }

        // !!!!!!!!!!! error !!!!!!!!!!!!!!!!
        return Some(Token::Semicolon);
    }

    fn match_variable(&mut self) -> MatchResult {

        if let Variable(ref v) = self.tokens[self.current] {
            self.current += 1;
            return Some(Token::Variable(v.clone()));
        }

        return None;
    }

    fn term(&mut self, tok: Token) -> bool {

        if self.current >= self.tokens.len() {
            return false;
        }

        if self.tokens[self.current] == tok {
            self.current += 1;
            return true;
        }

        return false;
    }
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        self.match_variable_define().is_some()
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
            assert!(parser.match_variable_define().is_some());
        }
    }

    #[ignore]
    #[test]
    fn test_variable_list() {
        let test = "int a, b_, c;";
        let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
        assert!(parser.match_variable_define().is_some());
    }

    #[test]
    fn test_struct_define() {
        let tests = vec!["struct Str { int a; short b; };",
                         "struct Str {};",
                         "\nstruct\nS\n{\nint\na\n;\n}\n;\n"];

        for test in tests {
            let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
            assert!(parser.match_struct_define().is_some());
        }

        let tests = vec!["struct for { int a; short b; };",
                         "struct S {}"];

        for test in tests {
            let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
            assert!(!parser.match_struct_define().is_some());
        }
    }
}