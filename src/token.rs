
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Debug, PartialEq)]
pub enum KeyWords {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
}

impl KeyWords {
    pub fn is_type(&self) -> bool {
        match self {
            // Char | Short | Int | Unsigned | Signed | Long | Double | Float => true,
            &KeyWords::Char => true,
            &KeyWords::Short => true,
            &KeyWords::Int => true,
            &KeyWords::Signed => true,
            &KeyWords::Unsigned => true,
            &KeyWords::Long => true,
            &KeyWords::Double => true,
            &KeyWords::Float => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operators {
    Add,
    Assign,
    AddEqual,
    And,
    Arrow,
    DoubleAdd,
    DoubleMinus,
    Division,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LogicAnd,
    LogicNot,
    LogicOr,
    Minus,
    MinusEqual,
    Mul,
    Not,
    NotEqual,
    Or,
    Xor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Brackets {
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Arrow,
    Asterisk,
    Bracket(Brackets),
    Comment(String),
    Comma,
    Dot,
    KeyWord(KeyWords),
    LiteralStr(String),
    Number(String),
    Operator(Operators),
    Preprocessor(String),
    Space,
    Semicolon,
    Variable(String),
}

pub fn is_keywords(s: &str) -> bool {
    Token::key_word_index(s).is_some()
}

impl Token {
    pub fn comment(c: &str) -> Token {
        Token::Comment(c.to_owned())
    }

    pub fn variable(v: &str) -> Token {
        Token::Variable(v.to_owned())
    }

    pub fn key_word(k: &str) -> Token {
        const KEY_TOKEN: &'static [KeyWords] = &[
            KeyWords::Auto,
            KeyWords::Break,
            KeyWords::Case,
            KeyWords::Char,
            KeyWords::Const,
            KeyWords::Continue,
            KeyWords::Default,
            KeyWords::Do,
            KeyWords::Double,
            KeyWords::Else,
            KeyWords::Enum,
            KeyWords::Extern,
            KeyWords::Float,
            KeyWords::For,
            KeyWords::Goto,
            KeyWords::If,
            KeyWords::Inline,
            KeyWords::Int,
            KeyWords::Long,
            KeyWords::Register,
            KeyWords::Restrict,
            KeyWords::Return,
            KeyWords::Short,
            KeyWords::Signed,
            KeyWords::Sizeof,
            KeyWords::Static,
            KeyWords::Struct,
            KeyWords::Switch,
            KeyWords::Typedef,
            KeyWords::Union,
            KeyWords::Unsigned,
            KeyWords::Void,
            KeyWords::Volatile,
            KeyWords::While,
        ];
        let index = Token::key_word_index(k).unwrap();

        Token::KeyWord(KEY_TOKEN[index].clone())
    }

    fn key_word_index(s: &str) -> Option<usize> {
        const KEY_WORDS: &'static [&'static str] = &[
            "auto",
            "break",
            "case",
            "char",
            "const",
            "continue",
            "default",
            "do",
            "double",
            "else",
            "enum",
            "extern",
            "float",
            "for",
            "goto",
            "if",
            "inline",
            "int",
            "long",
            "register",
            "restrict",
            "return",
            "short",
            "signed",
            "sizeof",
            "static",
            "struct",
            "switch",
            "typedef",
            "union",
            "unsigned",
            "void",
            "volatile",
            "while",
        ];

        KEY_WORDS.iter().position(|&x| x == s)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Token::Arrow => write!(f, "arrow:\t\t '->'"),
            &Token::Space => write!(f, "space:\t\t ' '"),
            &Token::Semicolon => write!(f, "semicolon:\t ';'"),
            &Token::Asterisk => write!(f, "asterisk:\t '*'"),
            &Token::Comma => write!(f, "comma:\t\t ','"),
            &Token::Dot => write!(f, "dot:\t\t '.'"),
            &Token::LiteralStr(ref s) => write!(f, "literal:\t {}", s),
            &Token::Bracket(ref b) => write!(f, "bracket:\t {:?}", b),
            &Token::Number(ref n) => write!(f, "number:\t\t {}", n),
            &Token::Comment(ref s) => write!(f, "comment:\t {}", s),
            &Token::KeyWord(ref k) => write!(f, "keywords:\t {:?}", k),
            &Token::Operator(ref o) => write!(f, "operators:\t {:?}", o),
            &Token::Preprocessor(ref p) => write!(f, "preprocessor:\t {}", p),
            &Token::Variable(ref v) => write!(f, "variable:\t {}", v),
        }
    }
}

#[test]
fn test_keywords() {
    assert!(is_keywords("struct"));
    assert!(is_keywords("unsigned"));
    assert!(!is_keywords("bool"));
}
