
use std::io;
use std::io::{Bytes, Read};

struct Lexer<R>
{
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

    pub fn parse(&mut self) {
        loop {
            match self.peek() {
                Ok(Some(ch)) => {
                    match ch {
                        b'a'...b'z' | b'A'...b'Z' => self.parse_string(),
                        b'0'...b'9' => self.parse_number(),
                        b'/' => self.parse_slash(),
                        _ => self.parse_other(),
                    }
                },
                Err(e) => println!("{:?}", e),
                _ => break,
            }
        }
    }

    fn parse_string(&mut self) {
        let mut buf = String::new();
        while let Ok(Some(ch)) = self.peek() {
            if ch >= b'a' && ch <= b'z' || ch >= b'A' && ch <= b'Z' || ch >= b'0' && ch <= b'9' {
                buf.push(ch as char);
                let _ = self.next();
            } else {
                break;
            }
        }

        println!("string: {}", buf);
    }

    fn parse_number(&mut self) {
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
    }

    fn parse_other(&mut self) {
        println!("got other character: `{}'", self.next().unwrap().unwrap() as char);
    }

    fn parse_slash(&mut self) {
        let _ = self.next(); // eat current ch
        let ch = self.peek();
        match ch {
            Ok(Some(c)) => {
                match c {
                    b'*' => {},
                    b'/' => self.parse_line_comment(),
                    _ => {},
                }
            },
            _ => {},
        }
    }

    fn parse_line_comment(&mut self) {
        let mut buf = "/".to_owned();
        while let Ok(Some(ch)) = self.next() {
            if ch == b'\n' {
                println!("line comment: {}", buf);
                return;
            }

            buf.push(ch as char);
        }
    }

    fn next(&mut self) -> io::Result<Option<u8>> {
        match self.ch.take() {
            Some(ch) => Ok(Some(ch)),
            None => {
                match self.iter.next() {
                    Some(Err(e)) => Err(e),
                    Some(Ok(ch)) => Ok(Some(ch)),
                    None => Ok(None),
                }
            }
        }
    }

    fn peek(&mut self) -> io::Result<Option<u8>> {
        match self.ch {
            Some(ch) => Ok(Some(ch)),
            None => {
                match self.iter.next() {
                    Some(Err(e)) => Err(e),
                    Some(Ok(ch)) => {
                        self.ch = Some(ch);
                        Ok(self.ch)
                    }
                    None => Ok(None),
                }
            }
        }
    }
}

fn main() {
    let source = "if (11 + 22 == 33) then 2/1//aaa\n else bbb".to_owned();

    let mut lexer = Lexer::new(source.as_bytes());
    lexer.parse();
}
