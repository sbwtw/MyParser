
mod simple_lexer;

pub use self::simple_lexer::SimpleLexer;

use token::Token;

use std::iter::Iterator;

type LexerResult = Result<Token, LexerError>;

#[derive(Debug)]
pub enum LexerError {
    Success,
    UnexpectEnd,
    UnexpectedChar(char, Vec<char>),
}

pub trait Lexer : Iterator<Item=Token> { }
