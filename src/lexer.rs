

use token::*;

use std::io::{Read, Bytes};
use std::iter::{Iterator, Peekable};

type LexerResult = Option<Token>;

pub struct Lexer<I: Read> {
    peeker: Peekable<Bytes<I>>,
}

impl<I: Read> Iterator for Lexer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

impl<I: Read> Lexer<I> {
    pub fn new(r: I) -> Lexer<I> {
        Lexer {
            peeker: r.bytes().peekable(),
        }
    }

    fn parse(&mut self) -> LexerResult {
        while let Some(c) = self.peek() {
            return match c {
                b'a'...b'z' | b'A'...b'Z' | b'_' => self.parse_string(),
                b'0'...b'9' => self.parse_number(),
                b'/' => self.parse_slash(),
                b'+' => self.parse_add(),
                b'-' => self.parse_minus(),
                b'#' => self.parse_preprocessor(),
                b'=' => self.parse_equal(),
                b'"' => self.parse_literal_str(),
                b'&' => self.parse_and(),
                b'|' => self.parse_or(),
                b'>' => self.parse_greater(),
                b'<' => self.parse_less(),
                b'!' => self.parse_not(),
                b';' => self.convert_char(Token::Semicolon),
                b'*' => self.convert_char(Token::Asterisk),
                b',' => self.convert_char(Token::Comma),
                b'.' => self.convert_char(Token::Dot),
                b'^' => self.convert_char(Token::Operator(Operators::Xor)),
                b'~' => self.convert_char(Token::Operator(Operators::Not)),
                b'(' => self.convert_char(Token::Bracket(Brackets::LeftParenthesis)),
                b')' => self.convert_char(Token::Bracket(Brackets::RightParenthesis)),
                b'[' => self.convert_char(Token::Bracket(Brackets::LeftSquareBracket)),
                b']' => self.convert_char(Token::Bracket(Brackets::RightSquareBracket)),
                b'{' => self.convert_char(Token::Bracket(Brackets::LeftCurlyBracket)),
                b'}' => self.convert_char(Token::Bracket(Brackets::RightCurlyBracket)),
                b' ' | b'\n' | b'\r' | b'\t' => { self.bump(); return self.parse(); },
                _ => self.parse_other(),
            };
        }

        None
    }

    fn convert_char(&mut self, r: Token) -> LexerResult {
        self.bump();

        Some(r)
    }

    fn parse_greater(&mut self) -> LexerResult {
        self.bump();

        return match self.peek() {
            Some(b'=') => self.convert_char(Token::Operator(Operators::GreaterEqual)),
            _ => Some(Token::Operator(Operators::Greater)),
        }
    }

    fn parse_less(&mut self) -> LexerResult {
        self.bump();

        return match self.peek() {
            Some(b'=') => self.convert_char(Token::Operator(Operators::LessEqual)),
            _ => Some(Token::Operator(Operators::Less)),
        }
    }

    fn parse_not(&mut self) -> LexerResult {
        self.bump();

        return match self.peek() {
            Some(b'=') => self.convert_char(Token::Operator(Operators::NotEqual)),
            _ => Some(Token::Operator(Operators::LogicNot)),
        }
    }

    fn parse_and(&mut self) -> LexerResult {
        self.bump();

        match self.peek() {
            Some(b'&') => return self.convert_char(Token::Operator(Operators::LogicAnd)),
            _ => {},
        }

        Some(Token::Operator(Operators::And))
    }

    fn parse_or(&mut self) -> LexerResult {
        self.bump();

        match self.peek() {
            Some(b'|') => return self.convert_char(Token::Operator(Operators::LogicOr)),
            _ => {},
        }

        Some(Token::Operator(Operators::Or))
    }

    fn parse_literal_str(&mut self) -> LexerResult {
        self.bump();
        let mut buf = "\"".to_owned();

        while let Some(c) = self.next() {
            match c {
                b'\\' => {
                    match self.next() {
                        Some(b'n') => buf.push('\n'),
                        Some(b'"') => buf.push('"'),
                        Some(b'\\') => buf.push('\\'),
                        _ => return None,
                    }
                },
                b'"' => {
                    buf.push('"');
                    return self.convert_char(Token::LiteralStr(buf));
                },
                _ => buf.push(c as char),
            }
        }

        None
    }

    fn parse_minus(&mut self) -> LexerResult {
        self.bump();

        match self.peek() {
            Some(b'>') => self.convert_char(Token::Arrow),
            Some(b'-') => self.convert_char(Token::Operator(Operators::DoubleMinus)),
            Some(b'=') => self.convert_char(Token::Operator(Operators::MinusEqual)),
            _ => Some(Token::Operator(Operators::Minus)),
        }
    }

    fn parse_equal(&mut self) -> LexerResult {
        self.bump();

        if let Some(b'=') = self.peek() {
            self.convert_char(Token::Operator(Operators::Equal))
        } else {
            Some(Token::Operator(Operators::Assign))
        }
    }

    fn parse_preprocessor(&mut self) -> LexerResult {
        let mut buf = String::new();

        while let Some(c) = self.next() {
            match c {
                b'\n' | b'\r' => break,
                _ => buf.push(c as char),
            }
        }

        Some(Token::Preprocessor(buf))
    }

    fn parse_string(&mut self) -> LexerResult {
        let mut buf = String::new();
        while let Some(ch) = self.peek() {
            if ch >= b'a' && ch <= b'z' || ch >= b'A' && ch <= b'Z' || ch >= b'0' && ch <= b'9' || ch == b'_' {
                buf.push(ch as char);
                self.bump();
            } else {
                break;
            }
        }

        let buf = &buf.as_str();

        if is_keywords(buf) {
            Some(Token::key_word(buf))
        } else {
            Some(Token::ident(buf))
        }
    }

    fn parse_number(&mut self) -> LexerResult {
        let mut buf = String::new();

        while let Some(ch) = self.peek() {
            if ch >= b'0' && ch <= b'9' {
                buf.push(ch as char);
                self.bump();
            } else {
                break;
            }
        }

        Some(Token::Number(Numbers::SignedInt(buf.parse::<isize>().unwrap())))
    }

    fn parse_add(&mut self) -> LexerResult {
        self.bump();

        match self.peek() {
            Some(c) => match c {
                b'+' => {
                    self.bump();
                    Some(Token::Operator(Operators::DoubleAdd))
                }
                _ => Some(Token::Operator(Operators::Add)),
            },
            None => Some(Token::Operator(Operators::Add)),
        }
    }

    fn parse_other(&mut self) -> LexerResult {
        let ch = self.next().unwrap() as char;

        println!("not handled character: {}", ch);

        Some(Token::comment(&ch.to_string()))
    }

    fn parse_slash(&mut self) -> LexerResult {
        self.bump();

        match self.peek() {
            Some(c) => match c {
                b'*' => self.parse_block_comment(),
                b'/' => self.parse_line_comment(),
                _ => Some(Token::Operator(Operators::Division)),
            },
            None => return None,
        }
    }

    fn parse_block_comment(&mut self) -> LexerResult {
        self.bump();
        let mut buf = "/*".to_owned();

        while let Some(c) = self.next() {
            match c {
                b'*' => {
                    buf.push('*');
                    match self.peek()? {
                        b'/' => {
                            buf.push('/');
                            return self.convert_char(Token::Comment(buf));
                        }
                        _ => continue,
                    }
                }
                c @ _ => buf.push(c as char),
            }
        }

        None
    }

    fn parse_line_comment(&mut self) -> LexerResult {
        let mut buf = "/".to_owned();
        while let Some(ch) = self.next() {
            if ch == b'\n' {
                break;
            }

            buf.push(ch as char);
        }

        return Some(Token::Comment(buf));
    }

    fn bump(&mut self) {
        let _ = self.next();
    }

    fn next(&mut self) -> Option<u8> {
        match self.peeker.next() {
            Some(Ok(ch)) => Some(ch),
            _ => None,
        }
    }

    fn peek(&mut self) -> Option<u8> {
        match self.peeker.peek() {
            Some(&Ok(ch)) => Some(ch),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {

    use lexer::*;

    #[test]
    fn test_key_words() {
        let source = "if else";

        let mut lexer = Lexer::new(source.as_bytes());
        assert_eq!(
            Iterator::next(&mut lexer).unwrap(),
            Token::KeyWord(KeyWords::If)
        );
        assert_eq!(
            Iterator::next(&mut lexer).unwrap(),
            Token::KeyWord(KeyWords::Else)
        );
        assert_eq!(Iterator::next(&mut lexer), None);
    }

    #[test]
    fn test_division() {
        let source = "2/3";

        let mut lexer = Lexer::new(source.as_bytes());
        assert_eq!(
            Iterator::next(&mut lexer).unwrap(),
            Token::Number(Numbers::from_str("2"))
        );
        assert_eq!(
            Iterator::next(&mut lexer).unwrap(),
            Token::Operator(Operators::Division)
        );
        assert_eq!(
            Iterator::next(&mut lexer).unwrap(),
            Token::Number(Numbers::from_str("3"))
        );
        assert_eq!(Iterator::next(&mut lexer), None);
    }

    #[test]
    fn test_cmp_op() {
        let source = "> >= < <= == !=";
        let s = source.clone();

        let mut lexer = Lexer::new(s.as_bytes());
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::Greater));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::GreaterEqual));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::Less));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::LessEqual));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::Equal));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::NotEqual));
        assert_eq!(Iterator::next(&mut lexer), None);
    }

    #[test]
    fn test_comment() {
        let source = "/**\naa\rbb\ta*/";
        let s = source.clone();

        let mut lexer = Lexer::new(s.as_bytes());
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Comment(source.to_owned()));
        assert_eq!(Iterator::next(&mut lexer), None);
    }

    #[test]
    fn test_literal_str() {
        let src = r#""this is literal \"String\".""#;
        let dst = "\"this is literal \"String\".\"".to_owned();

        let mut lexer = Lexer::new(src.as_bytes());
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::LiteralStr(dst));
        assert_eq!(Iterator::next(&mut lexer), None);

        let src = r#""with escape \n character \\""#;
        let dst = "\"with escape \n character \\\"".to_owned();

        let mut lexer = Lexer::new(src.as_bytes());
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::LiteralStr(dst));
        assert_eq!(Iterator::next(&mut lexer), None);
    }

    #[test]
    fn test_struct_define() {
        let src = "
    struct {
        /* field a */
        int a_;
        // field b
        unsigned int b0;
    };
    ";

        let mut lexer = Lexer::new(src.as_bytes());
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::KeyWord(KeyWords::Struct));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Bracket(Brackets::LeftCurlyBracket));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Comment("/* field a */".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::KeyWord(KeyWords::Int));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Identifier("a_".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Semicolon);
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Comment("// field b".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::KeyWord(KeyWords::Unsigned));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::KeyWord(KeyWords::Int));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Identifier("b0".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Semicolon);
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Bracket(Brackets::RightCurlyBracket));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Semicolon);
        assert_eq!(Iterator::next(&mut lexer), None);
    }

    #[test]
    fn test_double_minus() {
        let src = "
        point->x--;
        --i;
    ";

        let mut lexer = Lexer::new(src.as_bytes());
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Identifier("point".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Arrow);
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Identifier("x".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::DoubleMinus));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Semicolon);
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Operator(Operators::DoubleMinus));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Identifier("i".to_owned()));
        assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Semicolon);
        assert_eq!(Iterator::next(&mut lexer), None);
    }

}