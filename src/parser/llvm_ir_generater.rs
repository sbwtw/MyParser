
use parser::syntax_node::SyntaxTree;
use parser::syntax_node::*;
use parser::symbol_manager::*;
use token::Token;
use token::KeyWords;
use token::Operators;
use token::Numbers;
use token::Type as ValueType;

use id_tree::*;
use llvm::*;
use llvm_sys::*;

use std::rc::Rc;
use std::cell::RefCell;

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
/// let module = generater.ir_gen();
///
/// link_in_mcjit();
/// initialize_native_target();
/// initialize_native_asm_printer();
///
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

#[derive(Debug)]
enum SymbolType {
    LLVMValue(*mut LLVMValue),
    LLVMFunc(Rc<Function>),
}

#[derive(Debug)]
struct SymbolValue {
    symbol: SymbolType,
    value: ValueType,
}

pub struct LLVMIRGenerater<'t> {
    ast: &'t SyntaxTree,
    context: Rc<Context>,
    symbols: Rc<RefCell<SymbolManager<SymbolValue, Rc<Function>>>>,
}

struct GeneraterContext {
    module: Module,
    builder: Builder,
}

impl<'t> LLVMIRGenerater<'t> {
    pub fn new(ast: &'t SyntaxTree) -> LLVMIRGenerater<'t> {

        let context = Context::new();

        LLVMIRGenerater {
            ast: ast,
            context: Rc::new(context),
            symbols: Rc::new(RefCell::new(SymbolManager::new())),
        }
    }

    pub fn ir_gen(&mut self) -> Module {

        let module = self.context.module_create_with_name("module");
        let builder = self.context.create_builder();

        let mut context = GeneraterContext {
            module: module,
            builder: builder,
        };

        let ids = self.children_ids(self.ast.root_node_id().unwrap());
        for id in ids {
            self.dispatch_node(&mut context, &id);
        }

        context.module
    }

    fn dispatch_node(&mut self, context: &mut GeneraterContext, id: &NodeId) {
        match self.data(id) {
            &SyntaxType::FuncDefine => self.function_gen(context, id),
            &SyntaxType::ReturnStmt => self.return_stmt_gen(context, id),
            &SyntaxType::IfStmt => self.if_stmt_gen(context, id),
            &SyntaxType::VariableDefine => self.variable_define(context, id),
            &SyntaxType::AssignStmt => self.assign_stmt(context, id),
            _ => {},
        }
    }

    fn assign_stmt(&mut self, generater_context: &mut GeneraterContext, id: &NodeId) {
        let ids = self.children_ids(id);

        let lhs = self.llvm_value(generater_context, &ids[0]);
        let rhs = self.llvm_value(generater_context, &ids[1]);

        generater_context.builder.build_store(rhs, lhs);
    }

    fn variable_define(&mut self, generater_context: &mut GeneraterContext, id: &NodeId) {
        let ids = self.children_ids(id);

        let context = self.context.clone();
        let var_name = self.ident_name(&ids[1]).unwrap();
        let var_type = i64::get_type_in_context(&context);

        let value = generater_context.builder.build_alloca(var_type.into(), &var_name);

        let symbol_value = SymbolValue {
            symbol: SymbolType::LLVMValue(value),
            value: ValueType::Ptr(Box::new(ValueType::NoType)),
        };
        self.symbols.borrow_mut().push_symbol(var_name, symbol_value).ok();
    }

    fn function_gen(&mut self, generator_context: &mut GeneraterContext, node: &NodeId) {

        let context = self.context.clone();
        let ids = self.children_ids(node);
        let func_name = self.ident_name(&ids[1]).unwrap();

        // argument types
        let mut arg_types: Vec<&Type> = vec![];
        let mut arg_names = vec![];
        for id in ids.iter().skip(2) {
            match self.data(id) {
                &SyntaxType::FuncParam => {
                    let childs = self.children_ids(id);
                    let llvm_type = self.llvm_type(&context, &childs[0]);

                    arg_names.push(childs[1].clone());
                    arg_types.push(llvm_type);
                },
                _ => break,
            };
        }

        let func_type = types::Function::new(self.llvm_type(&context, &ids[0]), &arg_types[..], false);

        let func = {
            let mut func = generator_context.module.add_function(func_type, &func_name);

            let bb = context.append_basic_block(&mut func, "");
            generator_context.builder.position_at_end(bb);

            Rc::new(func)
        };

        let value = SymbolValue {
            symbol: SymbolType::LLVMFunc(func.clone()),
            value: ValueType::NoType,
        };

        self.symbols.borrow_mut().push_symbol(&func_name, value).ok();

        let __scope_guard = self.scope_guard(func);

        let func_params: Vec<*mut LLVMValue> = arg_names.iter().enumerate().map(|(index, _)| {
            self.symbols.borrow().current_scope().unwrap().get_param(index as u32).unwrap()
        }).collect();

        for (name, value) in arg_names.iter().zip(func_params.iter()) {
            let name = { self.ident_name(&name).unwrap() };
            let symbol_value = SymbolValue {
                symbol: SymbolType::LLVMValue(*value),
                value: ValueType::NoType,
            };
            self.symbols.borrow_mut().push_symbol(name, symbol_value).ok();
        }

        // start to build basic blocks
        for id in ids[arg_types.len() + 2..].iter() {
            self.dispatch_node(generator_context, id);
        }
    }

    fn return_stmt_gen(&mut self, context: &mut GeneraterContext, node_id: &NodeId) {
        let ids = self.children_ids(node_id);

        if ids.len() == 0 {
            context.builder.build_ret_void();
            return;
        }

        assert_eq!(ids.len(), 1);

        match self.data(&ids[0]) {
            &SyntaxType::Terminal(ref token) => {
                match **token {
                    Token::Number(Numbers::SignedInt(v)) => {
                        let ret_value = self.context.cons(v as i64);
                        context.builder.build_ret(ret_value);
                    },
                    Token::Identifier(ref name, _) => {
                        let value = self.ident_value(context, name);
                        context.builder.build_ret(value);
                    },
                    _ => {}
                }
            },
            &SyntaxType::Expr => {
                let r = self.expr_gen(context, &ids[0]);
                context.builder.build_ret(r);
            },
            &SyntaxType::FuncCall => {
                let r = self.func_call_gen(context, &ids[0]);
                context.builder.build_ret(r);
            },
            _ => {},
        }
    }

    fn func_call_gen(&self, context: &mut GeneraterContext, node_id: &NodeId) -> *mut LLVMValue {
        let ids = self.children_ids(node_id);
        let func_name = self.ident_name(&ids[0]).unwrap();

        self.symbols.borrow().lookup(func_name).map(move |symbol_value| {
            match symbol_value.symbol {
                SymbolType::LLVMFunc(ref func) => {
                    println!("cacacacac");
                    context.builder.build_call(&*func, vec![self.context.cons(1 as i64), self.context.cons(2 as i64)], "call")
                },
                _ => unreachable!(),
            }
        }).unwrap()
    }

    fn if_stmt_gen(&mut self, context: &mut GeneraterContext, node_id: &NodeId) {
        let childs = self.children_ids(node_id);

        let lhs = self.llvm_value(context, &childs[0]);
        let rhs = self.llvm_value(context, &childs[2]);

        // binary op
        let if_result = match *self.token(&childs[1]).unwrap() {
            Token::Operator(Operators::Equal) =>
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, "icmp_eq"),
            Token::Operator(Operators::NotEqual) =>
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, lhs, rhs, "icmp_ne"),
            Token::Operator(Operators::Greater) =>
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, "icmp_sgt"),
            Token::Operator(Operators::GreaterEqual) =>
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, "icmp_sge"),
            Token::Operator(Operators::Less) =>
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, "icmp_slt"),
            Token::Operator(Operators::LessEqual) =>
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLE, lhs, rhs, "icmp_sle"),
            _ => unreachable!(),
        };

        let (tb, fb) = {
            let symbols = self.symbols.clone();
            let mut symbols = symbols.borrow_mut();
            let func = symbols.current_scope_mut().expect("current scope error");

            let tb = self.context.append_basic_block(func, "if");
            let fb = self.context.append_basic_block(func, "endif");
            context.builder.build_cond_br(if_result, tb, fb);

            (tb, fb)
        };

        // move to true branch
        context.builder.position_at_end(tb);
        if childs.len() > 3 {
            self.return_stmt_gen(context, &childs[3]);
        }

        // move to end
        context.builder.position_at_end(fb);
    }

    fn expr_gen(&self, context: &mut GeneraterContext, node_id: &NodeId) -> *mut LLVMValue {
        let childs = self.children_ids(node_id);
        assert!(childs.len() >= 3);

        let mut lhs = self.llvm_value(context, &childs[0]);
        let mut current_op = 1;
        loop {
            let rhs = self.llvm_value(context, &childs[current_op + 1]);

            lhs = match *self.token(&childs[current_op]).unwrap() {
                Token::Operator(Operators::Add) => context.builder.build_add(lhs, rhs, "add"),
                Token::Operator(Operators::Mul) => context.builder.build_mul(lhs, rhs, "mul"),
                Token::Operator(Operators::Minus) => context.builder.build_mul(lhs, rhs, "sub"),
                Token::Operator(Operators::Division) => context.builder.build_mul(lhs, rhs, "div"),
                _ => unreachable!(),
            };

            current_op += 2;
            if current_op >= childs.len() { break; }
        }

        lhs
    }

    fn llvm_value(&self, context: &mut GeneraterContext, node_id: &NodeId) -> *mut LLVMValue {
        match self.data(node_id) {
            &SyntaxType::Terminal(ref term) => {
                match term.as_ref() {
                    &Token::Identifier(ref name, _) =>
                        match self.symbols.borrow().lookup(name).unwrap().symbol {
                            SymbolType::LLVMValue(v) => v,
                            _ => unreachable!(),
                        },
                    &Token::Number(Numbers::SignedInt(n)) => self.context.cons(n as i64),
                    _ => unreachable!(),
                }
            }
            &SyntaxType::Expr => self.expr_gen(context, node_id),
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

    fn ident_value(&self, context: &mut GeneraterContext, name: &str) -> *mut LLVMValue {

        self.symbols.borrow().lookup(name).map(|symbol_value| {
            match symbol_value.value {
                ValueType::Ptr(_) =>
                    match symbol_value.symbol {
                        SymbolType::LLVMValue(value) =>
                            self.dereference_ptr(context, value, &symbol_value.value),
                        _ => unreachable!(),
                    },
                _ =>
                    match symbol_value.symbol {
                        SymbolType::LLVMValue(value) => value,
                        _ => unreachable!(),
                    },
            }
        }).unwrap()
    }

    fn dereference_ptr(&self, context: &mut GeneraterContext, value: *mut LLVMValue, type_: &ValueType) -> *mut LLVMValue {
        match type_ {
            &ValueType::Ptr(ref ptr_type) => {
                let deref = context.builder.build_load(value, "deref");
                self.dereference_ptr(context, deref, ptr_type)
            },
            _ => value,
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

    #[inline]
    fn scope_guard(&self, func: Rc<Function>) -> ScopeGuard<SymbolValue, Rc<Function>> {
        ScopeGuard::new(self.symbols.clone(), func)
    }
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
            let module = generater.ir_gen();

            link_in_mcjit();
            initialize_native_target();
            initialize_native_asm_printer();

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

    #[test]
    fn test_local_variable()
    {
        let src = "
int f(int a, int b)
{
    int c;
    c = a + b;

    return c;
}";

        create_llvm_execution_engine!(src, ee);
        let f = func_addr_in_ee!(ee, "f", extern "C" fn(i64, i64) -> i64);

        assert_eq!(5, f(2, 3));
        assert_eq!(7, f(3, 4));
        assert_eq!(9, f(4, 5));
    }

    #[ignore]
    #[test]
    fn test_func_call()
    {
        let src = "
int f(int a, int b)
{
    int c;
    c = a + b;

    return c;
}

int f1(int a)
{
    return f(a, a + 1);
}
";

        create_llvm_execution_engine!(src, ee);
        let f = func_addr_in_ee!(ee, "f1", extern "C" fn(i64) -> i64);

        assert_eq!(5, f(2));
    }
}