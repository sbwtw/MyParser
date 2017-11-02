

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
        match self.peek()? {
            Some(ch) => match ch {
                b'a'...b'z' | b'A'...b'Z' => self.parse_string(),
                b'0'...b'9' => self.parse_number(),
                b'/' => self.parse_slash(),
                b'+' => self.parse_add(),
                b'#' => self.parse_preprocessor(),
                b' ' | b'\n' | b'\r' => {
                    self.bump();
                    Ok(Some(Token::Space))
                }
                _ => self.parse_other(),
            },
            None => Ok(None),
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
            if ch >= b'a' && ch <= b'z' || ch >= b'A' && ch <= b'Z' || ch >= b'0' && ch <= b'9' {
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
            Ok(Some(Token::comment(buf)))
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
