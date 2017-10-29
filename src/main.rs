
mod token;

use token::*;

use std::io;
use std::io::{Bytes, Read};

struct Lexer<R> {
    ch: Option<u8>,
    iter: Bytes<R>,
}

impl<R: Read> Lexer<R> {
    pub fn new(iter: R) -> Lexer<R> {
        Lexer {
            ch: None,
            iter: iter.bytes(),
        }
    }

    pub fn parse(&mut self) -> io::Result<Option<Token>> {
        match self.peek() {
            Ok(Some(ch)) => {
                let r = match ch {
                    b'a'...b'z' | b'A'...b'Z' => self.parse_string(),
                    b'0'...b'9' => self.parse_number(),
                    b'/' => self.parse_slash(),
                    b' ' | b'\n' | b'\r' => {
                        let _ = self.next();
                        Ok(None)
                    }
                    _ => self.parse_other(),
                };

                println!("{:?}", r);

                return r;
            }
            Ok(None) => return Ok(Some(Token::End)),
            Err(e) => panic!("{:?}", e),
        }
    }

    fn parse_string(&mut self) -> io::Result<Option<Token>> {
        let mut buf = String::new();
        while let Ok(Some(ch)) = self.peek() {
            if ch >= b'a' && ch <= b'z' || ch >= b'A' && ch <= b'Z' || ch >= b'0' && ch <= b'9' {
                buf.push(ch as char);
                let _ = self.next();
            } else {
                break;
            }
        }

        let buf = &buf.as_str();

        if is_keywords(buf) {
            println!("{}", Token::key_word(buf));
        } else {
            println!("string: {}", buf);
        }

        Ok(None)
    }

    fn parse_number(&mut self) -> io::Result<Option<Token>> {
        let mut buf = String::new();

        while let Ok(Some(ch)) = self.peek() {
            if ch >= b'0' && ch <= b'9' {
                buf.push(ch as char);
                let _ = self.next();
            } else {
                break;
            }
        }

        println!("number: {}", buf);

        Ok(None)
    }

    fn parse_other(&mut self) -> io::Result<Option<Token>> {
        println!(
            "got other character: `{}'",
            self.next().unwrap().unwrap() as char
        );

        Ok(None)
    }

    fn parse_slash(&mut self) -> io::Result<Option<Token>> {
        let _ = self.next(); // eat current ch
        let ch = self.peek();
        match ch {
            Ok(Some(c)) => match c {
                b'*' => self.parse_block_comment(),
                b'/' => self.parse_line_comment(),
                _ => {}
            },
            _ => {}
        }

        Ok(None)
    }

    fn parse_block_comment(&mut self) {}

    fn parse_line_comment(&mut self) {
        let mut buf = "/".to_owned();
        while let Ok(Some(ch)) = self.next() {
            if ch == b'\n' {
                let c = Token::Comment(buf);
                println!("{}", c);
                return;
            }

            buf.push(ch as char);
        }
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

fn main() {
    let source = "if (11 + 22 == 33) then 2/1//aaa\n else bbb".to_owned();

    let mut lexer = Lexer::new(source.as_bytes());
    while let Ok(r) = lexer.parse() {
        match r {
            Some(Token::End) => break,
            Some(token) => println!("{:?}", token),
            None => continue,
        }
    }
}
