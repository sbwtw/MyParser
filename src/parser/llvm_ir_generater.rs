
use parser::syntax_node::SyntaxTree;
use parser::syntax_node::*;

use llvm::*;

pub trait LLVMIRGen {
    fn generate(&self);
}

impl LLVMIRGen for SyntaxType {
    fn generate(&self) {

    }
}

pub struct LLVMIRGenerater<'t> {
    ast: &'t SyntaxTree,
    context: Context,
    module: Module,
}

impl<'t> LLVMIRGenerater<'t> {
    pub fn new(ast: &'t SyntaxTree) -> LLVMIRGenerater<'t> {

        let context = Context::new();
        let module = context.module_create_with_name("module");

        LLVMIRGenerater {
            ast: ast,
            context: context,
            module: module,
        }
    }

    pub fn ir_gen(&mut self) {
        self.function_gen();
    }

    pub fn dump(&mut self) {
        self.module.dump();
    }

    fn function_gen(&mut self) {
        let mut builder = self.context.create_builder();

        let ret = self.context.void_type();
        let args = vec![];

        let func_type = types::Function::new(ret, &args, false);

        let mut func = self.module.add_function(func_type, "func_name");
        let bb = self.context.append_basic_block(&mut func, "func_name");
        builder.position_at_end(bb);

        /*
             let function_type = llvm::types::Function::new(
        i64::get_type_in_context(&context),
        &[
            i64::get_type_in_context(&context),
            i64::get_type_in_context(&context),
            i64::get_type_in_context(&context)
        ],
        false);
    let mut func = module.add_function(function_type, "fname");
    let bb = context.append_basic_block(&mut func, "fname");
    builder.position_at_end(bb);

    // get the function's arguments
    let x = func.get_param(0).unwrap();
    let y = func.get_param(1).unwrap();
    let z = func.get_param(2).unwrap();

    let b = context.cons(20i64);

    let s1 = builder.build_add(x, b, "s1");
    let s2 = builder.build_add(y, s1, "s2");
    let s3 = builder.build_add(z, s2, "s3");
    builder.build_ret(s3);

    module.dump();

    llvm::link_in_mcjit();
    llvm::initialize_native_target();
    llvm::initialize_native_asm_printer();

    let ee = llvm::ExecutionEngine::create_for_module(&module).unwrap();
    let addr = ee.get_function_address("fname").unwrap();

    unsafe {
        let f: extern "C" fn(u64, u64, u64) -> u64 = mem::transmute(addr);

        let x: u64 = 1;
        let y: u64 = 2;
        let z: u64 = 3;
        let res = f(x, y, z);

        println!("{} + {} + {} = {}", x, y, z, res);
}
         *
         *
         */
    }
}