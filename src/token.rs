
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Debug)]
pub enum KeyWords
{
    IF,
    ELSE,
}

pub fn is_keywords(s: &str) -> bool {
    Token::key_word_index(s).is_some()
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
            &Token::Comment(ref s) => write!(f, "comment: {}", s),
            &Token::KeyWord(ref k) => write!(f, "keywords: {:?}", k),
        }
    }
}