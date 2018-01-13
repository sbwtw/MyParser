pub mod recursive_descent;
pub mod type_analyzer;
pub mod syntax_node;
pub mod llvm_ir_generater;
mod symbol_manager;
mod symbol_checker;

use id_tree::NodeId;
use self::syntax_node::SyntaxTree;

#[derive(Debug)]
pub enum ParseError {
    SyntaxError,
    SemanticError,
    MultiDefineError,
    UndefinedSymbol,
}

#[derive(Debug)]
pub struct ParseErrInfo {
    err_type: ParseError,
}

type ParserResult = Result<(), ParseErrInfo>;

pub trait Parser {
    fn run(&mut self) -> ParserResult;
    fn syntax_tree(&self) -> &SyntaxTree;
}

fn print_space(indentation: usize) {
    // for _ in 0..indentation { print!("  "); }
    for i in 0..indentation {
        match i % 4 {
            0 => print!("|  "),
            1 => print!(":  "),
            2 => print!("!  "),
            3 => print!(".  "),
            _ => {},
        }
    }
}

fn dump_tree(tree: &SyntaxTree, root: &NodeId, indentation: usize) {

    // print root
    print_space(indentation);
    println!("{:?}", tree.get(root).unwrap().data());

    for node in tree.children(root).unwrap() {
        print_space(indentation + 1);
        println!("{:?}", node.data());

        for child in node.children() {
            dump_tree(tree, child, indentation + 2);
        }
    }
}

#[cfg(test)]
mod test {

    use std::fs;
    use std::fs::File;
    use parser::*;
    use parser::recursive_descent::*;
    use lexer::Lexer;

    #[test]
    fn run_test_code_files() {
        for file in fs::read_dir("test").unwrap() {
            if let Ok(entry) = file {
                if entry.metadata().unwrap().is_file() {
                    let mut f = File::open(entry.path()).unwrap();
                    let mut parser = RecursiveDescentParser::new(Lexer::new(f));

                    assert!(parser.run().is_ok());

                    // TODO: compare with abstract syntax tree dump.
                }
            }
        }
    }
}