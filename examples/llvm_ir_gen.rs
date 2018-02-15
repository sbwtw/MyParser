
extern crate parser;
extern crate env_logger;
extern crate llvm;

use llvm::*;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;
use parser::parser::llvm_ir_generater::*;

use std::mem;

fn main() {

    env_logger::init();

    let src = "
int f(int a, int b)
{
    int c;
    c = a + b;

    return c;
}

int f1(int a)
{
    return f(a, 1);
}
    ";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {:?}\n", parser.run());
    parser.dump();

    let mut generater = LLVMIRGenerater::new(parser.syntax_tree());
    let module = generater.ir_gen();

    println!();
    module.dump();

    link_in_mcjit();
    initialize_native_target();
    initialize_native_asm_printer();

    let ee = ExecutionEngine::create_for_module(&module).unwrap();

    let f: extern "C" fn(i64, i64) -> i64 = unsafe { mem::transmute(ee.get_function_address("f").unwrap()) };
    assert_eq!(f(3, 2), 5);
}