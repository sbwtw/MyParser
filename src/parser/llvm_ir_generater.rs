
use parser::syntax_node::SyntaxTree;
use parser::syntax_node::*;
use parser::symbol_manager::*;
use token::Token;
use token::KeyWords;
use token::Operators;
use token::Numbers;

use id_tree::*;
use inkwell::IntPredicate;
use inkwell::support::LLVMString;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine};
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, BasicType};
use inkwell::values::{BasicValue, AnyValue, AnyValueEnum, FunctionValue};

use std::rc::Rc;
use std::cell::RefCell;

///
/// # JIT Examples.
/// ```
/// extern crate inkwell;
/// extern crate parser;
///
/// use self::inkwell::targets::{Target, InitializationConfig};
/// use self::inkwell::execution_engine::Symbol;
/// use parser::parser::*;
/// use parser::parser::recursive_descent::*;
/// use parser::parser::llvm_ir_generater::*;
/// use parser::lexer::*;
///
/// # fn main () {
///
/// Target::initialize_native(&InitializationConfig::default()).unwrap();
///
/// let src = "
///
/// int f(int a, int b)
/// {
///     if (a >= b)
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
/// let ee = generater.execution_engine().unwrap();
///
/// let f: Symbol<unsafe extern "C" fn(i64, i64) -> i64> = unsafe {
///     ee.get_function("f").unwrap()
/// };
///
/// assert_eq!(5, unsafe { f(2, 3) });
/// assert_eq!(6, unsafe { f(6, 5) });
/// assert_eq!(7, unsafe { f(3, 4) });
/// assert_eq!(5, unsafe { f(5, 5) });
///
/// # }
/// ```
///

impl SymbolManager<AnyValueEnum, String> {
    fn current_function(&self) -> FunctionValue {
        for table in self.symbols().iter().rev() {
            for (_, value) in table {
                if value.is_function_value() {
                    return value.into_function_value();
                }
            }
        }

        unimplemented!()
    }
}

pub struct LLVMIRGenerater<'t> {
    ast: &'t SyntaxTree,
    context: Context,
    module: Module,
    builder: Builder,
    symbols: Rc<RefCell<SymbolManager<AnyValueEnum, String>>>,
}

impl<'t> LLVMIRGenerater<'t> {
    pub fn new(ast: &'t SyntaxTree) -> LLVMIRGenerater<'t> {

        let context = Context::create();
        let module = context.create_module("main");
        let builder = context.create_builder();

        LLVMIRGenerater {
            ast,
            context,
            module,
            builder,
            symbols: Rc::new(RefCell::new(SymbolManager::new())),
        }
    }

    pub fn execution_engine(&self) -> Result<ExecutionEngine, LLVMString> {
        self.module.create_jit_execution_engine(OptimizationLevel::None)
    }

    pub fn ir_gen(&mut self) -> Result<(), ()> {

        let ids = self.children_ids(self.ast.root_node_id().unwrap());
        for id in ids {
            self.dispatch_node(&id);
        }

        self.module.verify().unwrap();

        Ok(())
    }

    fn dispatch_node(&mut self, id: &NodeId) {
        println!("DISPATCH {:?}", self.data(&id));

        match self.data(id) {
            &SyntaxType::FuncDefine => self.function_gen(id),
            &SyntaxType::ReturnStmt => self.return_stmt_gen(id),
            &SyntaxType::IfStmt => self.if_stmt_gen(id),
            // &SyntaxType::VariableDefine => self.variable_define(id),
            // &SyntaxType::AssignStmt => self.assign_stmt(id),
            _ => {},
        }
    }

    // fn assign_stmt(&mut self, id: &NodeId) {
    //     let ids = self.children_ids(id);
    //     let ptr = self.llvm_value(&ids[0]);
    //     let val = self.llvm_value(&ids[1]);
    //     let builder = Builder::new(self.context);

    //     builder.build_store(&val, &ptr);
    // }

    // fn variable_define(&mut self, id: &NodeId) {
    //     let ids = self.children_ids(id);
    //     let var_type = Type::get::<i64>(self.context);
    //     let builder = Builder::new(self.context);

    //     builder.build_alloca(var_type);
    // }

    fn function_gen(&mut self, node: &NodeId) {

        let ids = self.children_ids(node);
        let fn_name = self.ident_name(&ids[1]).unwrap();

        let mut args_type = vec![];
        let mut args_name = vec![];
        for id in ids.iter().skip(2) {
            match self.data(id) {
                &SyntaxType::FuncParam => {
                    let childs = self.children_ids(id);
                    let arg_type = self.llvm_basic_type(&childs[0]);
                    let arg_name = self.ident_name(&childs[1]).unwrap();

                    args_type.push(arg_type);
                    args_name.push(arg_name);
                },
                _ => break,
            };
        }

        // convert to trait objects.
        let arguments: Vec<&BasicType> = args_type.iter().map(|x| x as &BasicType).collect();
        let fn_type = self.context.i64_type().fn_type(&arguments[..], false);
        let function = self.module.add_function(&fn_name, &fn_type, None);

        self.push_identifier(&fn_name, function.into());

        let __scope_guard = self.scope_guard(&fn_name);
        let bb = self.context.append_basic_block(&function, &fn_name);
        self.builder.position_at_end(&bb);

        let param_count = function.count_params();
        assert_eq!(param_count, args_name.len() as u32);

        for (idx, param) in function.params().enumerate() {
            self.push_identifier(&args_name[idx], param.into());
        }

        // argument types
        // let mut arg_types: Vec<&Type> = vec![];
        // let mut arg_names = vec![];
        // for id in ids.iter().skip(2) {
        //     match self.data(id) {
        //         &SyntaxType::FuncParam => {
        //             let childs = self.children_ids(id);
        //             let llvm_type = self.llvm_type(&childs[0]);

        //             arg_names.push(childs[1].clone());
        //             arg_types.push(llvm_type);
        //         },
        //         _ => break,
        //     };
        // }

        // let func_type = types::Function::new(self.llvm_type(&context, &ids[0]), &arg_types[..], false);
        // let mut func = generator_context.module.add_function(func_type, &func_name);

        // let bb = context.append_basic_block(&mut func, "");
        // generator_context.builder.position_at_end(bb);

        // let func_params: Vec<*mut LLVMValue> = arg_names.iter().enumerate().map(|(index, _)| {
        //     self.symbols.borrow().current_scope().unwrap().get_param(index as u32).unwrap()
        // }).collect();

        // for (name, value) in arg_names.iter().zip(func_params.iter()) {
        //     let name = { self.ident_name(&name).unwrap() };
        //     let symbol_value = SymbolValue {
        //         symbol: SymbolType::LLVMValue(*value),
        //         value: ValueType::NoType,
        //     };
        //     self.symbols.borrow_mut().push_symbol(name, symbol_value).ok();
        // }

        // start to build basic blocks
        for id in ids[arguments.len() + 2..].iter() {
            self.dispatch_node(id);
        }

        self.module.print_to_stderr();
    }

    fn return_stmt_gen(&mut self, node_id: &NodeId) {
        println!("return stmt gen {:?}", self.data(&node_id));

        let ids = self.children_ids(node_id);

        if ids.len() == 0 {
            self.builder.build_return(None);
            return;
        }

        assert_eq!(ids.len(), 1);

        match self.data(&ids[0]) {
            &SyntaxType::Terminal(ref token) => {
                match **token {
                    Token::Number(Numbers::SignedInt(v)) => {
                        let r_type = self.context.i64_type();
                        let r_value = r_type.const_int(v as u64, false);
                        self.builder.build_return(Some(&r_value as &BasicValue));
                    },
                    Token::Identifier(ref name, _) => {
                        let value = self.ident_value(name);
                        self.builder.build_return(Some(value.as_int_value() as &BasicValue));
                    },
                    _ => unimplemented!()
                }
            },
            &SyntaxType::Expr => {
                let r = self.expr_gen(&ids[0]).into_int_value();
                self.builder.build_return(Some(&r as &BasicValue));
            }
            _ => unimplemented!()
        }
    }

    // fn func_call_gen(&mut self, context: &mut GeneraterContext, node_id: &NodeId) -> *mut LLVMValue {
    // }

    fn if_stmt_gen(&mut self, node_id: &NodeId) {
        println!("GEN {:?}", self.data(&node_id));

        let childs = self.children_ids(node_id);

        let lhs = self.llvm_value(&childs[0]).into_int_value().into();
        let rhs = self.llvm_value(&childs[2]).into_int_value().into();

        // binary op
        let if_result = match *self.token(&childs[1]).unwrap() {
            Token::Operator(Operators::Equal) =>
                self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "icmp_eq"),
            Token::Operator(Operators::NotEqual) =>
                self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "icmp_ne"),
            Token::Operator(Operators::Greater) =>
                self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "icmp_sgt"),
            Token::Operator(Operators::GreaterEqual) =>
                self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "icmp_sge"),
            // Token::Operator(Operators::Less) =>
                // context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, "icmp_slt"),
            // Token::Operator(Operators::LessEqual) =>
                // context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLE, lhs, rhs, "icmp_sle"),
            _ => unreachable!(),
        };

        let (tb, fb) = {
            let func = self.symbols.borrow().current_function();
            let tb = self.context.append_basic_block(&func, "if");
            let fb = self.context.append_basic_block(&func, "endif");

            self.builder.build_conditional_branch(&if_result, &tb, &fb);

            (tb, fb)
        };

        if childs.len() > 3 {
            self.builder.position_at_end(&tb);
            self.return_stmt_gen(&childs[3]);
        }

        // move to end
        self.builder.position_at_end(&fb);
    }

    fn expr_gen(&self, node_id: &NodeId) -> AnyValueEnum {
        println!("GEN {:?}", self.data(&node_id));

        let childs = self.children_ids(node_id);
        assert!(childs.len() >= 3);

        let mut lhs: AnyValueEnum = self.llvm_value(&childs[0]);

        let mut current_op = 1;
        loop {
            let rhs = self.llvm_value(&childs[current_op + 1]);

            lhs = match *self.token(&childs[current_op]).unwrap() {
                Token::Operator(Operators::Add) =>
                    self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add").as_any_value_enum(),
                // Token::Operator(Operators::Mul) =>
                    // self.builder.build_int_mul(lhs, rhs, "mul"),
                // Token::Operator(Operators::Minus) => self.builder.build_int_mul(lhs, rhs, "sub"),
                // Token::Operator(Operators::Division) => self.builder.build_mul(lhs, rhs, "div"),
                _ => unreachable!(),
            };

            current_op += 2;
            if current_op >= childs.len() { break; }
        }

        lhs

        // unimplemented!()
    }

    fn llvm_value(&self, node_id: &NodeId) -> AnyValueEnum {
        println!("GEN BasicValue {:?}", self.data(&node_id));

        let ident = self.ident_name(node_id).expect("Ident Node Error");

        self.symbols.borrow().lookup(ident).expect("Symbol Not Found").clone()
    }

    // fn llvm_value(&self, node_id: &NodeId) -> Value {
        // match self.data(node_id) {
        //     &SyntaxType::Terminal(ref term) => {
        //         match term.as_ref() {
        //             &Token::Identifier(ref name, _) =>
        //                 match self.symbols.borrow().lookup(name).unwrap().symbol {
        //                     SymbolType::LLVMValue(v) => v,
        //                     _ => unreachable!(),
        //                 },
        //             &Token::Number(Numbers::SignedInt(n)) => n.compile(self.module.get_context()),
        //             _ => unreachable!(),
        //         }
        //     }
        //     &SyntaxType::Expr => self.expr_gen(node_id),
        //     _ => unreachable!(),
        // }

        // unimplemented!()
    // }

    fn llvm_basic_type(&self, node_id: &NodeId) -> BasicTypeEnum {
        match *self.token(node_id).unwrap() {
            Token::KeyWord(KeyWords::Int) => self.context.i64_type().into(),
            _ => unimplemented!(),
        }
    }

    fn ident_value(&self, name: &str) -> AnyValueEnum {
        self.symbols.borrow().lookup(name).unwrap().clone()
    }

    // fn dereference_ptr(&self, value: *mut LLVMValue, type_: &ValueType) -> Value {
        // match type_ {
        //     &ValueType::Ptr(ref ptr_type) => {
        //         let deref = context.builder.build_load(value, "deref");
        //         self.dereference_ptr(context, deref, ptr_type)
        //     },
        //     _ => value,
        // }
        // unimplemented!()
    // }

    fn push_identifier(&self, ident: &str, value: AnyValueEnum) {
        self.symbols.borrow_mut().push_symbol(ident, value).unwrap();
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
    fn scope_guard<T: AsRef<str>>(&self, scope: T) -> ScopeGuard<AnyValueEnum, String> {
        ScopeGuard::new(self.symbols.clone(), scope.as_ref().to_owned())
    }
}

#[cfg(test)]
mod test {

    use parser::*;
    use parser::recursive_descent::*;
    use lexer::*;
    use parser::llvm_ir_generater::*;

    // macro_rules! create_llvm_execution_engine {
    //     ($src: ident, $ee: ident) => {
    //         let mut parser = RecursiveDescentParser::new(Lexer::new($src.as_bytes()));
    //         parser.run().unwrap();

    //         let mut generater = LLVMIRGenerater::new(parser.syntax_tree());
    //         let module = generater.ir_gen();

    //         link_in_mcjit();
    //         initialize_native_target();
    //         initialize_native_asm_printer();

    //         let $ee = ExecutionEngine::create_for_module(&module).unwrap();
    //     };
    // }

    macro_rules! func_addr_in_ee {
        ($ee: ident, $name: expr, $type: ty) => {
            unsafe {
                let f: $type = mem::transmute($ee.get_function_address($name).unwrap());
                f
            }
        }
    }

//     #[test]
//     fn test_jit_expr()
//     {
//         let src = "
// int f(int a, int b)
// {
//     if (a >= 5)
//         return a;

//     return a + b;
// }
//         ";

//         create_llvm_execution_engine!(src, ee);
//         let f = func_addr_in_ee!(ee, "f", extern "C" fn(i64, i64) -> i64);

//         assert_eq!(5, f(2, 3));
//         assert_eq!(6, f(6, 5));
//         assert_eq!(7, f(3, 4));
//         assert_eq!(9, f(4, 5));
//     }

//     #[test]
//     fn test_local_variable()
//     {
//         let src = "
// int f(int a, int b)
// {
//     int c;
//     c = a + b;

//     return c;
// }";

//         create_llvm_execution_engine!(src, ee);
//         let f = func_addr_in_ee!(ee, "f", extern "C" fn(i64, i64) -> i64);

//         assert_eq!(5, f(2, 3));
//         assert_eq!(7, f(3, 4));
//         assert_eq!(9, f(4, 5));
//     }

//     #[ignore]
//     #[test]
//     fn test_func_call()
//     {
//         let src = "
// int f(int a, int b)
// {
//     int c;
//     c = a + b;

//     return c;
// }

// int f1(int a)
// {
//     return f(a, a + 1);
// }
// ";

//         create_llvm_execution_engine!(src, ee);
//         let f = func_addr_in_ee!(ee, "f1", extern "C" fn(i64) -> i64);

//         assert_eq!(5, f(2));
//     }
}