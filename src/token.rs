
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Debug)]
pub enum KeyWords
{
    IF,
    ELSE,
}

pub enum Token
{
    Comment(String),
    KeyWord(KeyWords),
}

impl Token {
    pub fn comment(c: &str) -> Token {
        Token::Comment(c.to_owned())
    }

    pub fn key_word(k: &str) -> Token {
        const KEY_TOKEN: &'static [KeyWords] = &[ KeyWords::IF, KeyWords::ELSE ];
        const KEY_WORDS: &'static [&'static str] = &[ "if", "else" ];
        let index = KEY_WORDS.iter().position(|&x| x == k).unwrap();

        Token::KeyWord(KEY_TOKEN[index].clone())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Token::Comment(ref s) => write!(f, "comment: {}", s),
            &Token::KeyWord(ref k) => write!(f, "keywords: {:?}", k),
        }
    }
}