# The Lexer & Parser for C-Language written in Rust [![Build Status](https://travis-ci.org/sbwtw/MyParser.svg?branch=master)](https://travis-ci.org/sbwtw/MyParser)

## Examples
Print Token List:
```
    let source = "
#include <iostream.h>
int main()
{
    int num = 1;
    if (num == 0)
        return 0;
    else
        return 1;
}
".to_owned();

    let mut lexer = Lexer::new(source.as_bytes());
    while let Some(tok) = lexer.next() {
        println!("{:?}", tok),
    }
```
The output is:
```
Preprocessor("#include <iostream.h>")
KeyWord(Int)
Variable("main")
Bracket(LeftParenthesis)
Bracket(RightParenthesis)
Bracket(LeftCurlyBracket)
KeyWord(Int)
Variable("num")
Operator(Assign)
Number("1")
Semicolon
KeyWord(If)
Bracket(LeftParenthesis)
Variable("num")
Operator(Equal)
Number("0")
Bracket(RightParenthesis)
KeyWord(Return)
Number("0")
Semicolon
KeyWord(Else)
KeyWord(Return)
Number("1")
Semicolon
Bracket(RightCurlyBracket)
```

Print abstract syntax tree
```
    let src = "struct S {int a ; double b; };";
    let lexer = Lexer::new(src.as_bytes());
    let mut parser = RecursiveDescentParser::new(lexer);
    parser.run();
    parser.dump();
```
The output is:
```
SyntaxTree
  Struct
    Terminal(KeyWord(Struct))
    Terminal(Variable("S"))
    Terminal(Bracket(LeftCurlyBracket))
    Variable
      Terminal(KeyWord(Int))
      Terminal(Variable("a"))
      Terminal(Semicolon)
    Variable
      Terminal(KeyWord(Double))
      Terminal(Variable("b"))
      Terminal(Semicolon)
    Terminal(Bracket(RightCurlyBracket))
    Terminal(Semicolon)
```