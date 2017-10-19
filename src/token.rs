
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

pub enum Token
{
    Comment(String),
}

impl Token {
    pub fn comment(c: &str) -> Token {
        Token::Comment(c.to_owned())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Token::Comment(ref s) => write!(f, "comment: {}", s),
        }
    }
}