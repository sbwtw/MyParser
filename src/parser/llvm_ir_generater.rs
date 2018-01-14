
use parser::syntax_node::SyntaxTree;
use parser::syntax_node::*;
use token::Token;
use token::KeyWords;
use token::Numbers;

use id_tree::*;
use llvm::*;

use std::rc::Rc;
use std::cell::RefCell;

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
    module: Rc<RefCell<Module>>,
}

impl<'t> LLVMIRGenerater<'t> {
    pub fn new(ast: &'t SyntaxTree) -> LLVMIRGenerater<'t> {

        let context = Context::new();
        let module = context.module_create_with_name("module");

        LLVMIRGenerater {
            ast: ast,
            context: context,
            module: Rc::new(RefCell::new(module)),
        }
    }

    pub fn ir_gen(&mut self) {
        // self.function_gen();
        let ids = self.children_ids(self.ast.root_node_id().unwrap());
        self.function_gen(&ids[0]);
    }

    pub fn function_addr(&mut self, func_name: &str) -> Option<extern "C" fn()> {
        let ee = ExecutionEngine::create_for_module(&self.module.borrow()).unwrap();

        ee.get_function_address(func_name)
    }

    #[inline]
    pub fn dump(&mut self) {
        self.module.borrow().dump();
    }

    fn function_gen(&mut self, node: &NodeId) {
        let ids = self.children_ids(node);
        let func_name = self.ident_name(&ids[1]);
        let module = self.module.clone();

        let ret_type = self.llvm_type(&ids[0]);

        // argument types
        let mut arg_types: Vec<&Type> = vec![];
        for id in ids.iter().skip(2) {
            match self.data(id) {
                &SyntaxType::FuncArg => {
                    arg_types.push(self.llvm_type(&self.children_ids(id)[0]));
                },
                _ => break,
            };
        }

        let func_type = types::Function::new(ret_type, &arg_types[..], false);
        let mut func = module.borrow_mut().add_function(func_type, &func_name);

        let bb = self.context.append_basic_block(&mut func, &func_name);
        let mut builder = self.context.create_builder();
        builder.position_at_end(bb);

        // start to build basic blocks
        for id in ids.iter().skip(arg_types.len() + 2) {
            match self.data(id) {
                &SyntaxType::ReturnStmt => self.return_stmt_gen(&mut builder, id),
                _ => break,
            }
        }

        // builder.build_ret_void();

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

    fn return_stmt_gen(&self, builder: &mut Builder, node_id: &NodeId) {
        let ids = self.children_ids(node_id);

        if ids.len() == 0 {
            builder.build_ret_void();
            return;
        }

        let token = self.token(&ids[0]);
        match *token.unwrap() {
            Token::Number(Numbers::SignedInt(v)) => {
                let ret_value = self.context.cons(v as i64);
                builder.build_ret(ret_value);
            },
            _ => {}
        }
    }

    fn llvm_type(&self, node_id: &NodeId) -> &Type {
        match *self.token(node_id).unwrap() {
            Token::KeyWord(KeyWords::Void) => &self.context.void_type(),
            Token::KeyWord(KeyWords::Int) => i64::get_type_in_context(&self.context),
            _ => panic!(),
        }
    }

    fn ident_name(&self, node_id: &NodeId) -> String {
        match self.token(node_id).unwrap().as_ref() {
            &Token::Identifier(ref name, _) => return name.clone(),
            _ => panic!(),
        }
    }

    #[inline]
    fn token(&self, node_id: &NodeId) -> Option<Rc<Token>> {
        self.data(node_id).token()
    }

    #[inline]
    fn data(&self, node_id: &NodeId) -> &SyntaxType {
        self.ast.get(node_id).unwrap().data()
    }

    #[inline]
    fn children_ids(&self, node_id: &NodeId) -> Vec<NodeId> {
        self.ast.children_ids(&node_id).unwrap().map(|x| x.clone()).collect()
    }
}