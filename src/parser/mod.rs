pub mod recursive_descent;
mod syntax_node;

use self::syntax_node::SyntaxTree;

pub trait Parser {
    fn run(&mut self) -> bool;
    fn syntax_tree(&self) -> &SyntaxTree;
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

                    assert!(parser.run());

                    // TODO: compare with abstract syntax tree dump.
                }
            }
        }
    }
}