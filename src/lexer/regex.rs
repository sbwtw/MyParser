
use std::convert::From;
use std::str::Chars;
use std::iter::Peekable;
use std::string::ToString;

#[derive(Debug)]
enum RegexUnit {
    Character(char),
    CharacterRange(char, char),
    Items(Vec<RegexItem>),
}

#[derive(Debug, PartialEq)]
enum RegexAnnotation {
    StandAlone,
    OneOrZero,      // '?'
    GreaterZero,    // '+'
    AnyOccurs,      // '*'
}

#[derive(Debug)]
pub struct RegexItem {
    unit: RegexUnit,
    annotation: RegexAnnotation,
}

impl<'s> From<&'s str> for RegexItem {
    fn from(s: &'s str) -> RegexItem {
        RegexParser {
            input: s.chars().peekable(),
        }.parse().unwrap()
    }
}

impl ToString for RegexItem {
    fn to_string(&self) -> String {
        let mut r = String::new();


        assert_eq!(self.annotation, RegexAnnotation::StandAlone);

        r
    }
}

type RegexParserError = ();
type RegexParserResult = Result<RegexItem, RegexParserError>;

struct RegexParser<'s> {
    input: Peekable<Chars<'s>>,
}

impl<'s> RegexParser<'s> {
    fn parse(&mut self) -> RegexParserResult {
        let mut items = vec![];

        while let Ok(item) = self.dispatch() {
            items.push(item);
        }

        assert_eq!(self.parse_annotation(), RegexAnnotation::StandAlone);

        Ok(RegexItem {
            unit: RegexUnit::Items(items),
            annotation: RegexAnnotation::StandAlone,
        })
    }

    fn dispatch(&mut self) -> RegexParserResult {
        if let Some(c) = self.input.peek().map(|x| x.clone())
        {
            match c {
                '[' => self.parse_character_group(),
                '(' => self.parse_item_group(),
                _ => self.parse_character(),
            }
        } else {
            Err(())
        }
    }

    fn parse_character(&mut self) -> RegexParserResult {
        let c = self.input.next().unwrap();

        Ok(RegexItem {
            unit: RegexUnit::Character(c),
            annotation: self.parse_annotation(),
        })
    }

    fn parse_character_group(&mut self) -> RegexParserResult {
        self.input.next();
        let mut items = vec![];

        loop {
            match self.input.peek().map(|x| x.clone()) {
                Some(']') => {
                    self.input.next();

                    return Ok(RegexItem {
                        unit: RegexUnit::Items(items),
                        annotation: self.parse_annotation(),
                    })
                },
                Some(_) => items.push(self.dispatch()?),
                None => return Err(()),
            }
        }
    }

    fn parse_item_group(&mut self) -> RegexParserResult {
        self.input.next();
        let mut items = vec![];

        loop {
            match self.input.peek().map(|x| x.clone()) {
                Some(')') => {
                    self.input.next();

                    return Ok(RegexItem {
                        unit: RegexUnit::Items(items),
                        annotation: self.parse_annotation(),
                    })
                },
                Some(_) => items.push(self.dispatch()?),
                None => return Err(()),
            }
        }
    }

    fn parse_annotation(&mut self) -> RegexAnnotation {
        let r = match self.input.peek() {
            Some('?') => RegexAnnotation::OneOrZero,
            Some('+') => RegexAnnotation::GreaterZero,
            Some('*') => RegexAnnotation::AnyOccurs,
            _ => return RegexAnnotation::StandAlone,
        };

        self.input.next();
        r
    }

}

#[cfg(test)]
mod test {

    use lexer::regex::RegexItem;

    #[test]
    fn test() {
        let r: RegexItem = "a[abd]+".into();
        println!("{:#?}", r);
    }
}
