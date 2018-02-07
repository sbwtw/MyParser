
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

///
/// # JIT Examples.
/// ```
/// extern crate llvm;
/// extern crate parser;
///
/// use self::llvm::*;
/// use parser::parser::*;
/// use parser::parser::recursive_descent::*;
/// use parser::parser::llvm_ir_generater::*;
/// use parser::lexer::*;
///
/// use std::mem;
///
/// # fn main () {
///
/// let src = "
///
/// int f(int a, int b)
/// {
///     if (a >= 5)
///         return a;
///
///     return a + b;
/// }
/// ";
///
/// let mut parser = RecursiveDescentParser::new(Lexer::new(src.as_bytes()));
/// parser.run().unwrap();
///
/// let mut generater = LLVMIRGenerater::new(parser.syntax_tree());
/// generater.ir_gen();
///
/// link_in_mcjit();
/// initialize_native_target();
/// initialize_native_asm_printer();
///
/// let module = generater.module();
/// let ee = ExecutionEngine::create_for_module(&module).unwrap();
///
/// let f: extern "C" fn(i64, i64) -> i64 = unsafe {
///     mem::transmute(ee.get_function_address("f").unwrap())
/// };
///
/// assert_eq!(5, f(2, 3));
/// assert_eq!(6, f(6, 5));
/// assert_eq!(7, f(3, 4));
/// assert_eq!(9, f(4, 5));
///
/// # }
/// ```
///

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
    symbols: Rc<SymbolManager<*mut LLVMValue, ()>>,
}

impl<'t> LLVMIRGenerater<'t> {
    pub fn new(ast: &'t SyntaxTree) -> LLVMIRGenerater<'t> {

        let context = Context::new();
        let module = context.module_create_with_name("module");

        LLVMIRGenerater {
            ast: ast,
            context: Rc::new(context),
            module: Rc::new(module),
            symbols: Rc::new(SymbolManager::new()),
        }
    }

    pub fn ir_gen(&mut self) {
        let ids = self.children_ids(self.ast.root_node_id().unwrap());
        self.function_gen(&ids[0]);
    }

    #[inline]
    pub fn module(&self) -> Rc<Module> {
        self.module.clone()
    }

    #[inline]
    pub fn dump(&self) {
        self.module.dump();
    }

    fn function_gen(&mut self, node: &NodeId) {

        let context = self.context.clone();
        let ids = self.children_ids(node);
        let func_name = self.ident_name(&ids[1]).unwrap();

        // argument types
        let mut arg_types: Vec<&Type> = vec![];
        let mut arg_names = vec![];
        for id in ids.iter().skip(2) {
            match self.data(id) {
                &SyntaxType::FuncArg => {
                    let childs = self.children_ids(id);
                    let llvm_type = self.llvm_type(&context, &childs[0]);

                    arg_names.push(childs[1].clone());
                    arg_types.push(llvm_type);
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
            let name = { self.ident_name(&arg).unwrap() };
            Rc::get_mut(&mut self.symbols).unwrap().push_symbol(name, func.get_param(index as u32).unwrap()).unwrap();
        }

        // start to build basic blocks
        for id in ids.iter().skip(arg_types.len() + 2) {
            match self.data(id) {
                &SyntaxType::ReturnStmt => self.return_stmt_gen(&mut builder, id),
                &SyntaxType::IfStmt => self.if_stmt_gen(&mut func, &mut builder, id),
                _ => {},
            }
        }
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
                        builder.build_ret(*self.symbols.lookup(name).unwrap());
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

        let lhs = self.llvm_value(builder, &childs[0]);
        let rhs = self.llvm_value(builder, &childs[2]);

        // binary op
        let if_result = match *self.token(&childs[1]).unwrap() {
            Token::Operator(Operators::Equal) =>
                builder.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, "icmp_eq"),
            Token::Operator(Operators::NotEqual) =>
                builder.build_icmp(LLVMIntPredicate::LLVMIntNE, lhs, rhs, "icmp_ne"),
            Token::Operator(Operators::Greater) =>
                builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, "icmp_sgt"),
            Token::Operator(Operators::GreaterEqual) =>
                builder.build_icmp(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, "icmp_sge"),
            _ => unreachable!(),
        };

        let tb = self.context.append_basic_block(func, "if");
        let fb = self.context.append_basic_block(func, "endif");
        builder.build_cond_br(if_result, tb, fb);

        // move to true branch
        builder.position_at_end(tb);
        if childs.len() > 3 {
            self.return_stmt_gen(builder, &childs[3]);
        }

        // move to end
        builder.position_at_end(fb);
    }

    fn expr_gen(&self, builder: &mut Builder, node_id: &NodeId) -> *mut LLVMValue {
        let childs = self.children_ids(node_id);
        assert!(childs.len() >= 3);

        let mut lhs = self.llvm_value(builder, &childs[0]);
        let mut current_op = 1;
        loop {
            let rhs = self.llvm_value(builder, &childs[current_op + 1]);

            lhs = match *self.token(&childs[current_op]).unwrap() {
                Token::Operator(Operators::Add) => builder.build_add(lhs, rhs, "add"),
                Token::Operator(Operators::Mul) => builder.build_mul(lhs, rhs, "mul"),
                Token::Operator(Operators::Minus) => builder.build_mul(lhs, rhs, "sub"),
                Token::Operator(Operators::Division) => builder.build_mul(lhs, rhs, "div"),
                _ => unreachable!(),
            };

            current_op += 2;
            if current_op >= childs.len() { break; }
        }

        lhs
    }

    fn llvm_value(&self, builder: &mut Builder, node_id: &NodeId) -> *mut LLVMValue {
        match self.data(node_id) {
            &SyntaxType::Terminal(ref term) => {
                match term.as_ref() {
                    &Token::Identifier(ref name, _) => *self.symbols.lookup(name).unwrap(),
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

#[cfg(test)]
mod test {

    use parser::*;
    use parser::recursive_descent::*;
    use lexer::*;
    use parser::llvm_ir_generater::*;

    use std::mem;

    macro_rules! create_llvm_execution_engine {
        ($src: ident, $ee: ident) => {
            let mut parser = RecursiveDescentParser::new(Lexer::new($src.as_bytes()));
            parser.run().unwrap();

            let mut generater = LLVMIRGenerater::new(parser.syntax_tree());
            generater.ir_gen();

            link_in_mcjit();
            initialize_native_target();
            initialize_native_asm_printer();

            let module = generater.module();
            let $ee = ExecutionEngine::create_for_module(&module).unwrap();
        };
    }

    macro_rules! func_addr_in_ee {
        ($ee: ident, $name: expr, $type: ty) => {
            unsafe {
                let f: $type = mem::transmute($ee.get_function_address($name).unwrap());
                f
            }
        }
    }

    #[test]
    fn test_jit_expr()
    {
        let src = "
int f(int a, int b)
{
    if (a >= 5)
        return a;

    return a + b;
}
        ";

        create_llvm_execution_engine!(src, ee);
        let f = func_addr_in_ee!(ee, "f", extern "C" fn(i64, i64) -> i64);

        assert_eq!(5, f(2, 3));
        assert_eq!(6, f(6, 5));
        assert_eq!(7, f(3, 4));
        assert_eq!(9, f(4, 5));
    }
}