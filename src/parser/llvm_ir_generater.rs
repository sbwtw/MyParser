
use parser::syntax_node::SyntaxTree;
use parser::syntax_node::*;
use parser::symbol_manager::*;
use token::Token;
use token::KeyWords;
use token::Operators;
use token::Numbers;

use id_tree::*;
use llvm::*;
use llvm_sys::*;

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
    context: Rc<Context>,
    module: Rc<Module>,
    symbols: Rc<RefCell<SymbolManager<*mut LLVMValue, ()>>>,
}

impl<'t> LLVMIRGenerater<'t> {
    pub fn new(ast: &'t SyntaxTree) -> LLVMIRGenerater<'t> {

        let context = Context::new();
        let module = context.module_create_with_name("module");

        LLVMIRGenerater {
            ast: ast,
            context: Rc::new(context),
            module: Rc::new(module),
            symbols: Rc::new(RefCell::new(SymbolManager::new())),
        }
    }

    pub fn ir_gen(&mut self) {
        let ids = self.children_ids(self.ast.root_node_id().unwrap());
        self.function_gen(&ids[0]);
    }

    pub fn function_addr(&mut self, func_name: &str) -> Option<extern "C" fn()> {
        let ee = ExecutionEngine::create_for_module(&self.module).unwrap();

        ee.get_function_address(func_name)
    }

    #[inline]
    pub fn dump(&self) {
        self.module.dump();
    }

    fn function_gen(&mut self, node: &NodeId) {

        let context = self.context.clone();

        let ids = self.children_ids(node);
        let func_name = self.ident_name(&ids[1]).unwrap();
        let symbols = self.symbols.clone();

        // argument types
        let mut arg_types: Vec<&Type> = vec![];
        let mut arg_names = vec![];
        for id in ids.iter().skip(2) {
            match self.data(id) {
                &SyntaxType::FuncArg => {
                    let childs = self.children_ids(id);
                    arg_names.push(childs[1].clone());
                    arg_types.push(self.llvm_type(&context, &childs[0]));
                },
                _ => break,
            };
        }

        let func_type = types::Function::new(self.llvm_type(&context, &ids[0]), &arg_types[..], false);
        let mut func = Rc::get_mut(&mut self.module).unwrap().add_function(func_type, &func_name);

        let bb = context.append_basic_block(&mut func, "");
        let mut builder = context.create_builder();
        builder.position_at_end(bb);

        // add argument symbols
        for (index, arg) in arg_names.iter().enumerate() {
            symbols.borrow_mut().push_symbol(self.ident_name(&arg).unwrap(), func.get_param(index as u32).unwrap()).unwrap();
        }

        // start to build basic blocks
        for id in ids.iter().skip(arg_types.len() + 2) {
            match self.data(id) {
                &SyntaxType::ReturnStmt => self.return_stmt_gen(&mut builder, id),
                &SyntaxType::IfStmt => self.if_stmt_gen(&mut func, &mut builder, id),
                _ => {},
            }
        }

        // self.module = Some(module);
        // self.context = Some(context);

        // let x = func.get_param(0).unwrap();
        // let y = func.get_param(1).unwrap();
        // let () = x;
        // builder.build_ret(x);

        // builder.build_add(x, y, "tmpValue");

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

        assert_eq!(ids.len(), 1);

        match self.data(&ids[0]) {
            &SyntaxType::Terminal(ref token) => {
                match **token {
                    Token::Number(Numbers::SignedInt(v)) => {
                        let ret_value = self.context.cons(v as i64);
                        builder.build_ret(ret_value);
                    },
                    Token::Identifier(ref name, _) => {
                        builder.build_ret(*self.symbols.borrow().lookup(name).unwrap());
                    },
                    _ => {}
                }
            },
            &SyntaxType::Expr => {
                let r = self.expr_gen(builder, &ids[0]);
                builder.build_ret(r);
            }
            _ => {},
        }
    }

    fn if_stmt_gen(&self, func: &mut Function, builder: &mut Builder, node_id: &NodeId) {
        let childs = self.children_ids(node_id);
        println!("if stmt gen");
        let value = self.llvm_value(builder, &childs[0]);
        let v = builder.build_icmp(LLVMIntPredicate::LLVMIntSGE, value, self.context.cons(2), "cmp");

        let tb = self.context.append_basic_block(func, "tb");
        let fb = self.context.append_basic_block(func, "tb");
        builder.build_cond_br(v, tb, fb);

        builder.position_at_end(tb);
        builder.build_ret_void();
        builder.position_at_end(fb);
    }

    fn expr_gen(&self, builder: &mut Builder, node_id: &NodeId) -> *mut LLVMValue {
        let childs = self.children_ids(node_id);
        assert_eq!(childs.len(), 3);

        let value1 = self.llvm_value(builder, &childs[0]);
        let value2 = self.llvm_value(builder, &childs[2]);

        match *self.token(&childs[1]).unwrap() {
            Token::Operator(Operators::Add) => builder.build_add(value1, value2, "add"),
            Token::Operator(Operators::Mul) => builder.build_mul(value1, value2, "mul"),
            _ => unreachable!(),
        }
    }

    fn llvm_value(&self, builder: &mut Builder, node_id: &NodeId) -> *mut LLVMValue {
        match self.data(node_id) {
            &SyntaxType::Terminal(ref term) => {
                match term.as_ref() {
                    &Token::Identifier(ref name, _) => *self.symbols.borrow().lookup(name).unwrap(),
                    &Token::Number(Numbers::SignedInt(n)) => self.context.cons(n as i64),
                    _ => unreachable!(),
                }
            }
            &SyntaxType::Expr => self.expr_gen(builder, node_id),
            _ => unreachable!(),
        }
    }

    fn llvm_type<'a>(&self, context: &'a Context, node_id: &NodeId) -> &'a Type {
        match *self.token(node_id).unwrap() {
            Token::KeyWord(KeyWords::Void) => &context.void_type(),
            Token::KeyWord(KeyWords::Int) => i64::get_type_in_context(&context),
            _ => panic!(),
        }
    }

    #[inline]
    fn ident_name(&self, node_id: &NodeId) -> Option<String> {
        self.data(node_id).symbol().map(|x| x.to_owned())
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

    // #[inline]
    // fn scope_guard(&self, func: Function) -> ScopeGuard<*mut LLVMValue, Function> {
        // ScopeGuard::new(self.symbols.clone(), func)
    // }
}