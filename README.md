# The Lexer & Parser for C-Language written in Rust [![Build Status](https://travis-ci.org/sbwtw/MyParser.svg?branch=master)](https://travis-ci.org/sbwtw/MyParser)

## Goals
- [x] 词法分析
- [x] 基于递归下降的语法分析
- [ ] 基于`LL(1)`的语法分析
- [x] 抽象语法树`AST`的生成
- [ ] 基于`AST`的程序优化
- [x] 符号检查
- [ ] 类型检查与类型推导
- [ ] 基于常量传递的表达式优化
- [ ] 基于数学原理的表达式优化
- [ ] `LLVM IR`中间代码生成
- [ ] 利用`LLVM`进行`JIT`即时编译
- [ ] 三元式生成
- [ ] 目标代码生成

## Examples
使用递归下降分析器输出抽象语法树:
```
    let src = "
int func_add(int a, int b)
{
    return a + b;
}

int main()
{
    return 0;
}
    ";

    let lexer = Lexer::new(src.as_bytes());
    let mut parser = RecursiveDescentParser::new(lexer);
    parser.run();
    parser.dump();
```
> output AST:
```
SyntaxTree
  FuncDefine
    Terminal(KeyWord(Int))
    Terminal(Identifier("func_add"))
    FuncArg
      Terminal(KeyWord(Int))
      Terminal(Identifier("a"))
    FuncArg
      Terminal(KeyWord(Int))
      Terminal(Identifier("b"))
    ReturnStmt
      Expr
        Terminal(Identifier("a"))
        Terminal(Operator(Add))
        Terminal(Identifier("b"))
  FuncDefine
    Terminal(KeyWord(Int))
    Terminal(Identifier("main"))
    ReturnStmt
      Terminal(Number("0"))
```
符号检查
```
    let src = "
struct S { int a, b; };

struct S1 { double S1; int b; };

int a(int a, char b) {  }
    ";

    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));
    println!("result: {:?}\n", parser.run());
```
> output: result: Ok(())
```
    let src = "
// multi define of variable S::a
struct S { int a, b; char a };
    ";

    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));
    println!("result: {:?}\n", parser.run());
```
> output: result: Err(ParseErrInfo { err_type: MultiDefineError })

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
- expr_opt:
    - `bool_expr`
    - `epsilon`

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
    - `stmt_single` `;`
    - `stmt_block`
    - `stmt_control`
    - `;`

- stmt_single
    - `assign_stmt`
    - `break_stmt`
    - `return_stmt`

- stmt_control
    - `if_stmt`
    - `while_loop`
    - `for_loop`

- stmt_list:
    - `stmt` `stmt_list` | `epsilon`

- stmt_block:
    - `{` `stmt_list` `}`

- assign_stmt:
    - `left_value` `=` `right_value`

- if_stmt:
    - `if` `(` `bool_expr` `)` `stmt` `else` `stmt`

- while_loop:
    - `while` `(` `bool_expr` `)` `stmt`

- for_loop:
    - `for` `(` `expr_opt` `;` `expr_opt` `;` `expr_opt` `)` `stmt`

- break_stmt:
    - `break`

- return_stmt:
    - `return` `return_expr`

- return_expr:
    - `bool_expr`
    - `epsilon`

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
    - `func_ret_type` `func_name` `(` `func_arg_list` `)` `;`

- func_name
    - `identifier`

- func_ret_type:
    - `type`

- func_arg_list:
    - `func_arg` `func_arg_list_tail`
    - `epsilon`

- func_arg_list_tail:
    - `,` `func_arg` `func_arg_list_tail`
    - `epsilon`

- func_arg:
    - `func_arg_type` `func_arg_name`

- func_arg_type:
    - `type`

- func_arg_name:
    - `identifier`

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
    - `func_ret_type` `func_name` `(` `func_arg_list` `)` `{` `func_body` `}`

- func_body:
    - `stmt_list`

