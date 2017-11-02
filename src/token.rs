
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Debug, PartialEq)]
pub enum KeyWords
{
    If,
    Else,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operators
{
    Add,
    DoubleAdd,
    Dvision,
    Assign,
    Equal,
    AddEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Brackets
{
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
}

pub fn is_keywords(s: &str) -> bool {
    Token::key_word_index(s).is_some()
}

#[derive(Debug, PartialEq)]
pub enum Token
{
    Space,
    Bracket(Brackets),
    LiteralStr(String),
    Comment(String),
    Number(String),
    KeyWord(KeyWords),
    Operator(Operators),
    Preprocessor(String),
}

impl Token {
    pub fn comment(c: &str) -> Token {
        Token::Comment(c.to_owned())
    }

    pub fn key_word(k: &str) -> Token {
        const KEY_TOKEN: &'static [KeyWords] = &[ KeyWords::If, KeyWords::Else ];
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
            &Token::LiteralStr(ref s) => write!(f, "literal: {}", s),
            &Token::Bracket(ref b) => write!(f, "bracket: {:?}", b),
            &Token::Number(ref n) => write!(f, "number: {}", n),
            &Token::Comment(ref s) => write!(f, "comment: {}", s),
            &Token::KeyWord(ref k) => write!(f, "keywords: {:?}", k),
            &Token::Operator(ref o) => write!(f, "operators: {:?}", o),
            &Token::Preprocessor(ref p) => write!(f, "preprocessor: {}", p),
        }
    }
}