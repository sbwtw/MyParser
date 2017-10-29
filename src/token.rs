
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Debug, PartialEq)]
pub enum KeyWords
{
    IF,
    ELSE,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operators
{
    ADD,
    DIVISION,
}

pub fn is_keywords(s: &str) -> bool {
    Token::key_word_index(s).is_some()
}

#[derive(Debug, PartialEq)]
pub enum Token
{
    Space,
    Comment(String),
    Number(String),
    KeyWord(KeyWords),
    Operator(Operators),
}

impl Token {
    pub fn comment(c: &str) -> Token {
        Token::Comment(c.to_owned())
    }

    pub fn key_word(k: &str) -> Token {
        const KEY_TOKEN: &'static [KeyWords] = &[ KeyWords::IF, KeyWords::ELSE ];
        let index = Token::key_word_index(k).unwrap();

        Token::KeyWord(KEY_TOKEN[index].clone())
    }

    fn key_word_index(s: &str) -> Option<usize> {
        const KEY_WORDS: &'static [&'static str] = &[ "if", "else" ];

        KEY_WORDS.iter().position(|&x| x == s)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Token::Space => write!(f, "space"),
            &Token::Number(ref n) => write!(f, "number: {}", n),
            &Token::Comment(ref s) => write!(f, "comment: {}", s),
            &Token::KeyWord(ref k) => write!(f, "keywords: {:?}", k),
            &Token::Operator(ref o) => write!(f, "operators: {:?}", o),
        }
    }
}