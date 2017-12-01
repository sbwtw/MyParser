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

## C-language syntax defines
### 关键字
```
if, else, for, ...
short, int, long, unsigned, ...
```

### 标识符
- number = `\d+`
- identifier = `[a-z][a-z|0-9]*`
- ident = `number` | `identifier`

### 表达式
- expr:
    - `expr` `add_op` `expr_mul`
    - => `expr_mul` `expr_fix` （消除左递归后的产生式，下同）
- expr_fix:
    - `add_op` `expr_mul` `expr_fix` | `epsilon`

- expr_mul:
    - `expr_mul` `mul_op` `expr_factor`
    - => `expr_factor` `expr_mul_fix`
- expr_mul_fix = `mul_op` `expr_factor` `expr_mul_fix` | `epsilon`

- expr_factor = `(` `expr` `)` | `ident`

> 引入的 Tokens
- add_op = `+` | `-`
- mul_op = `*` | `/`
- single_op = `!` | `~`

#### 布尔表达式
> 原始定义
- bool_expr:
    - `bool_expr` `||` `bool_expr`
    - `bool_expr` `&&` `bool_expr`
    - `bool_expr` `equal_op` `bool_expr`
    - `bool_expr` `cmp_op` `bool_expr`
    - `!` `expr`
    - `expr`

> 消除左递归及添加优先级后的定义
- bool_expr:
    - `bool_expr` `||` `bool_expr_and`
    - => `bool_expr_and` `bool_expr_fix`
- bool_expr_fix:
    - `||` `bool_expr_and` `bool_expr_fix` | `epsilon`

- bool_expr_and:
    - `bool_expr_and` `&&` `bool_expr_equal`
    - => `bool_expr_equal` `bool_expr_and_fix`
- bool_expr_and_fix:
    - `&&` `bool_expr_equal` `bool_expr_and_fix` | `epsilon`

- bool_expr_equal:
    - `bool_expr_equal` `equal_op` `bool_expr_cmp`
    - => `bool_expr_cmp` `bool_expr_equal_fix`
- bool_expr_equal_fix:
    - `equal_op` `bool_expr_cmp` `bool_expr_equal_fix` | `epsilon`

- bool_expr_cmp:
    - `bool_expr_cmp` `cmp_op` `bool_expr_factor`
    - => `bool_expr_factor` `bool_expr_cmp_fix`
- bool_expr_cmp_fix:
    - `cmp_op` `bool_expr_factor` `bool_expr_cmp_fix` | `epsilon`

- bool_expr_factor:
    - `!` `bool_expr`
    - `(` `bool_expr` `)`
    - `expr`

> 引入的 Tokens
- cmp_op:
    - `>`
    - `>=`
    - `<`
    - `<=`
- equal_op:
    - `==`
    - `!=`

### 语句

- stmt:
    - `stmt_factor`

- stmt_factor:
    - `assign_stmt`
    - `if_stmt`
    - `stmt_block`

- stmt_list:
    - `stmt` `stmt_list` | `epsilon`

- stmt_block:
    - `{` `stmt_list` `}`

- assign_stmt:
    - `left_value` `=` `right_value` `;`

- if_stmt:
    - `if` `(` `bool_expr` `)` `stmt` `else` `stmt`

- left_value:
    - `identifier`
- right_value:
    - `bool_expr`

### 声明 & 定义
#### 变量定义
- variable_define:
    - `variable_type` `variable_def_list`

- variable_def_list:
    - `identifier` `;`
    - `identifier` `,` `variable_def_list`

- variable_type:
    - `variable_prefix` `variable_suffix`
    - `float`
    - `double`

- variable_prefix:
    - `unsigned`
    - `signed`
    - `long`
    - `long long`

- variable_suffix:
    - `int`

#### 函数声明
- func_declare:
    - `func_ret_type` `func_name` `(` `arg_list` `)` `;`

- func_name
    - `identifier`

- func_ret_type:
    - `type`

- arg_list:
    - `arg`
    - `arg` `,` `arg_list`
    - `epsilon`

#### 结构体定义
- struct_define:
    - `struct` `{` `struct_vars` `}` `;`
    - `struct` `identifier` `{` `struct_vars` `}` `;`

- struct_vars
    - `struct_var` `;`
    - `struct_var` `struct_vars`
    - `epsilon`

- struct_var
    - `variable_define`

#### 函数定义
- function:
    - `func_ret_type` `func_name` `(` `)` `{` `func_body` `}`

- func_body:
    - `stmt_list`

