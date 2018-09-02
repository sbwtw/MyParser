
use std::io::{Read, Bytes};
use std::iter::{Iterator, Peekable};

enum RuleToken {
}

trait RuleLexer : Iterator<Item=RuleToken> { }

struct SimpleRuleLexer<I: Read> {
    stream: Peekable<Bytes<I>>,
}

impl<I: Read> Iterator for SimpleRuleLexer<I> {
    type Item=RuleToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

impl<I: Read> RuleLexer for SimpleRuleLexer<I> { }

impl<I: Read> SimpleRuleLexer<I> {
    pub fn new(r: I) -> SimpleRuleLexer<I> {
        SimpleRuleLexer {
            stream: r.bytes().peekable(),
        }
    }

    fn parse(&mut self) -> Option<RuleToken> {
        unimplemented!()
    }
}
