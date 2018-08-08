
extern crate parser;
extern crate env_logger;
extern crate inkwell;

use parser::lexer::*;
use parser::parser::*;
use parser::parser::recursive_descent::*;
use parser::parser::llvm_ir_generater::*;

use inkwell::targets::{Target, InitializationConfig};

use std::mem;

fn main() {

    env_logger::init();

    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let src = "
int f(int a, int b)
{
    return a + b;
}

    ";
    let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {:?}\n", parser.run());
    parser.dump();

    let mut generater = LLVMIRGenerater::new(parser.syntax_tree());
    generater.ir_gen().ok();

    println!();

    // link_in_mcjit();
    // initialize_native_target();
    // initialize_native_asm_printer();

    // let ee = ExecutionEngine::create_for_module(&module).unwrap();

    // let f: extern "C" fn(i64, i64) -> i64 = unsafe { mem::transmute(ee.get_function_address("f").unwrap()) };
    // assert_eq!(f(3, 2), 5);
}