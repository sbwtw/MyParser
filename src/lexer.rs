

use token::*;

use std::io;
use std::io::{Bytes, Read};
use std::iter::Iterator;

type LexerResult = io::Result<Option<Token>>;

pub struct Lexer<R> {
    ch: Option<u8>,
    iter: Bytes<R>,
}

impl<R: Read> Iterator for Lexer<R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parse() {
            Ok(it) => it,
            Err(e) => panic!("{:?}", e),
        }
    }
}

impl<R: Read> Lexer<R> {
    pub fn new(iter: R) -> Lexer<R> {
        Lexer {
            ch: None,
            iter: iter.bytes(),
        }
    }

    fn parse(&mut self) -> LexerResult {
        while let Some(c) = self.peek()? {
            return match c {
                b'a'...b'z' | b'A'...b'Z' | b'_' => self.parse_string(),
                b'0'...b'9' => self.parse_number(),
                b'/' => self.parse_slash(),
                b'+' => self.parse_add(),
                b'-' => self.parse_minus(),
                b'#' => self.parse_preprocessor(),
                b'=' => self.parse_equal(),
                b'"' => self.parse_literal_str(),
                b';' => self.convert_char(Token::Semicolon),
                b'*' => self.convert_char(Token::Asterisk),
                b',' => self.convert_char(Token::Comma),
                b'.' => self.convert_char(Token::Dot),
                b'(' => self.convert_char(Token::Bracket(Brackets::LeftParenthesis)),
                b')' => self.convert_char(Token::Bracket(Brackets::RightParenthesis)),
                b'[' => self.convert_char(Token::Bracket(Brackets::LeftSquareBracket)),
                b']' => self.convert_char(Token::Bracket(Brackets::RightSquareBracket)),
                b'{' => self.convert_char(Token::Bracket(Brackets::LeftCurlyBracket)),
                b'}' => self.convert_char(Token::Bracket(Brackets::RightCurlyBracket)),
                b' ' | b'\n' | b'\r' | b'\t' => self.convert_char(Token::Space),
                _ => self.parse_other(),
            };
        }

        Ok(None)
    }

    fn convert_char(&mut self, r: Token) -> LexerResult {
        self.bump();

        Ok(Some(r))
    }

    fn parse_literal_str(&mut self) -> LexerResult {
        self.bump();
        let mut buf = "\"".to_owned();

        while let Some(c) = self.next()? {
            match c {
                b'\\' => {
                    match self.next()? {
                        Some(b'n') => buf.push('\n'),
                        Some(b'"') => buf.push('"'),
                        Some(b'\\') => buf.push('\\'),
                        _ => return Ok(None),
                    }
                },
                b'"' => {
                    buf.push('"');
                    return self.convert_char(Token::LiteralStr(buf));
                },
                _ => buf.push(c as char),
            }
        }

        Ok(None)
    }

    fn parse_minus(&mut self) -> LexerResult {
        self.bump();

        match self.peek()? {
            Some(b'>') => self.convert_char(Token::Arrow),
            Some(b'-') => self.convert_char(Token::Operator(Operators::DoubleMinus)),
            Some(b'=') => self.convert_char(Token::Operator(Operators::MinusEqual)),
            _ => Ok(Some(Token::Operator(Operators::Minus))),
        }
    }

    fn parse_equal(&mut self) -> LexerResult {
        self.bump();

        if let Some(b'=') = self.peek()? {
            self.convert_char(Token::Operator(Operators::Equal))
        } else {
            Ok(Some(Token::Operator(Operators::Assign)))
        }
    }

    fn parse_preprocessor(&mut self) -> LexerResult {
        let mut buf = String::new();

        while let Some(c) = self.next()? {
            match c {
                b'\n' | b'\r' => break,
                _ => buf.push(c as char),
            }
        }

        Ok(Some(Token::Preprocessor(buf)))
    }

    fn parse_string(&mut self) -> LexerResult {
        let mut buf = String::new();
        while let Some(ch) = self.peek()? {
            if ch >= b'a' && ch <= b'z' || ch >= b'A' && ch <= b'Z' || ch >= b'0' && ch <= b'9' || ch == b'_' {
                buf.push(ch as char);
                self.bump();
            } else {
                break;
            }
        }

        let buf = &buf.as_str();

        if is_keywords(buf) {
            Ok(Some(Token::key_word(buf)))
        } else {
            Ok(Some(Token::variable(buf)))
        }
    }

    fn parse_number(&mut self) -> LexerResult {
        let mut buf = String::new();

        while let Some(ch) = self.peek()? {
            if ch >= b'0' && ch <= b'9' {
                buf.push(ch as char);
                self.bump();
            } else {
                break;
            }
        }

        Ok(Some(Token::Number(buf)))
    }

    fn parse_add(&mut self) -> LexerResult {
        self.bump();

        match self.peek()? {
            Some(c) => match c {
                b'+' => {
                    self.bump();
                    Ok(Some(Token::Operator(Operators::DoubleAdd)))
                }
                _ => Ok(Some(Token::Operator(Operators::Add))),
            },
            None => Ok(Some(Token::Operator(Operators::Add))),
        }
    }

    fn parse_other(&mut self) -> LexerResult {
        let ch = self.next().unwrap().unwrap() as char;

        println!("not handled character: {}", ch);

        Ok(Some(Token::comment(&ch.to_string())))
    }

    fn parse_slash(&mut self) -> LexerResult {
        self.bump();
        let ch = self.peek()?;
        match ch {
            Some(c) => match c {
                b'*' => self.parse_block_comment(),
                b'/' => self.parse_line_comment(),
                _ => Ok(Some(Token::Operator(Operators::Dvision))),
            },
            None => return Ok(None),
        }
    }

    fn parse_block_comment(&mut self) -> LexerResult {
        self.bump();
        let mut buf = "/*".to_owned();

        while let Some(c) = self.next()? {
            match c {
                b'*' => {
                    buf.push('*');
                    match self.peek()? {
                        Some(b'/') => {
                            buf.push('/');
                            return self.convert_char(Token::Comment(buf));
                        }
                        _ => continue,
                    }
                }
                c @ _ => buf.push(c as char),
            }
        }

        Ok(None)
    }

    fn parse_line_comment(&mut self) -> LexerResult {
        let mut buf = "/".to_owned();
        while let Some(ch) = self.next()? {
            if ch == b'\n' {
                break;
            }

            buf.push(ch as char);
        }

        return Ok(Some(Token::Comment(buf)));
    }

    fn bump(&mut self) {
        let _ = self.next();
    }

    fn next(&mut self) -> io::Result<Option<u8>> {
        match self.ch.take() {
            Some(ch) => Ok(Some(ch)),
            None => match self.iter.next() {
                Some(Err(e)) => Err(e),
                Some(Ok(ch)) => Ok(Some(ch)),
                None => Ok(None),
            },
        }
    }

    fn peek(&mut self) -> io::Result<Option<u8>> {
        match self.ch {
            Some(ch) => Ok(Some(ch)),
            None => match self.iter.next() {
                Some(Err(e)) => Err(e),
                Some(Ok(ch)) => {
                    self.ch = Some(ch);
                    Ok(self.ch)
                }
                None => Ok(None),
            },
        }
    }
}

#[test]
fn test_key_words() {
    let source = "if else".to_owned();

    let mut lexer = Lexer::new(source.as_bytes());
    assert_eq!(
        Iterator::next(&mut lexer).unwrap(),
        Token::KeyWord(KeyWords::If)
    );
    assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Space);
    assert_eq!(
        Iterator::next(&mut lexer).unwrap(),
        Token::KeyWord(KeyWords::Else)
    );
    assert_eq!(Iterator::next(&mut lexer), None);
}

#[test]
fn test_division() {
    let source = "2/3".to_owned();

    let mut lexer = Lexer::new(source.as_bytes());
    assert_eq!(
        Iterator::next(&mut lexer).unwrap(),
        Token::Number("2".to_owned())
    );
    assert_eq!(
        Iterator::next(&mut lexer).unwrap(),
        Token::Operator(Operators::Dvision)
    );
    assert_eq!(
        Iterator::next(&mut lexer).unwrap(),
        Token::Number("3".to_owned())
    );
    assert_eq!(Iterator::next(&mut lexer), None);
}

#[test]
fn test_comment() {
    let source = "/**\naa\rbb\ta*/".to_owned();
    let s = source.clone();

    let mut lexer = Lexer::new(s.as_bytes());
    assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::Comment(source));
    assert_eq!(Iterator::next(&mut lexer), None);
}

#[test]
fn test_literal_str() {
    let src = r#""this is literal \"String\".""#.to_owned();
    let dst = "\"this is literal \"String\".\"".to_owned();

    let mut lexer = Lexer::new(src.as_bytes());
    assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::LiteralStr(dst));
    assert_eq!(Iterator::next(&mut lexer), None);

    let src = r#""with escape \n character \\""#.to_owned();
    let dst = "\"with escape \n character \\\"".to_owned();

    let mut lexer = Lexer::new(src.as_bytes());
    assert_eq!(Iterator::next(&mut lexer).unwrap(), Token::LiteralStr(dst));
    assert_eq!(Iterator::next(&mut lexer), None);
}