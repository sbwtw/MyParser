
extern crate parser;
extern crate env_logger;
extern crate inkwell;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;
use parser::parser::llvm_ir_generater::*;

use inkwell::targets::{Target, InitializationConfig};
use inkwell::execution_engine::Symbol;

fn main() {

    env_logger::init();

    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let src = "
int f(int a, int b)
{
    if (a > b)
        return b;

    if (a <= 0)
        return 0;

    return a + b;
}

    ";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {:?}\n", parser.run());
    parser.dump();

    let mut generater = LLVMIRGenerater::new(parser.syntax_tree());
    generater.ir_gen().ok();

    let ee = generater.execution_engine().unwrap();

    let f: Symbol<unsafe extern "C" fn(u64, u64) -> u64> = unsafe { ee.get_function("f").unwrap() };
    assert_eq!(unsafe { f(2, 3) }, 5);
}