
use token::*;
use token::Token::*;
use lexer::Lexer;
use parser::Parser;
use parser::syntax_node::*;

use id_tree::*;
use id_tree::InsertBehavior::*;
use id_tree::RemoveBehavior::*;

///
/// variable = ...
/// type = char | short | ...
/// variable_define = type variable ;
/// struct_define = struct variable { variable_define ... } ;
///
/// binary_op = && || ^ & |
/// expr = (expr) expr_fix |
///        !expr expr_fix |
///        ~expr expr_fix |
///        number expr_fix |
///        variable expr_fix
/// expr_fix = binary_op expr expr_fix | epsilon
///

type TokenResult = Option<Token>;

macro_rules! insert {
    ($tree: expr, $root: expr, $tok: expr) => {
        $tree.insert(Node::new(SyntaxType::Terminal($tok)), UnderNode(&$root)).unwrap();
    };
}

macro_rules! insert_type {
    ($tree: expr, $root: expr, $type: expr) => {
        $tree.insert(Node::new($type), UnderNode(&$root)).unwrap();
    };
}

fn print_space(indentation: usize) {
    for _ in 0..indentation { print!("  "); }
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

pub struct RecursiveDescentParser {
    tokens: Vec<Token>,
    current: usize,
    tree: SyntaxTree,
}

impl RecursiveDescentParser {
    pub fn new(lexer: Lexer) -> RecursiveDescentParser {

        let mut tree = SyntaxTree::new();
        let root_node = Node::new(SyntaxType::SyntaxTree);
        tree.insert(root_node, AsRoot).unwrap();

        RecursiveDescentParser {
            tokens: lexer.collect(),
            current: 0,
            tree: tree,
        }
    }

    pub fn dump(&self) {
        let id = self.root_id();
        dump_tree(&self.tree, &id, 0);
    }

    fn root_id(&self) -> NodeId {
        self.tree.root_node_id().unwrap().clone()
    }

    fn match_type(&mut self) -> TokenResult {

        if let KeyWord(ref k) = self.tokens[self.current] {
            if k.is_type() {
                self.current += 1;
                return Some(Token::KeyWord(k.clone()));
            }
        }

        return None;
    }

    fn match_variable_define(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::Variable);

        if let Some(t) = self.match_type() {
            insert!(self.tree, self_id, t);

            if let Some(v) = self.match_variable() {
                insert!(self.tree, self_id, v);

                if self.term(Token::Semicolon) {
                    insert!(self.tree, self_id, Token::Semicolon);
                    return true;
                }
            }
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        return false;
    }

    fn match_struct_define(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::Struct);

        loop {
            if !self.term(Token::KeyWord(KeyWords::Struct)) { break; }
            insert!(self.tree, self_id, Token::KeyWord(KeyWords::Struct));

            match &self.tokens[self.current] {
                &Token::Variable(ref v) => {
                    self.current += 1;
                    insert!(self.tree, self_id, Token::Variable(v.clone()));
                },
                _ => {},
            }

            if !self.term(Token::Bracket(Brackets::LeftCurlyBracket)) { break; }
            insert!(self.tree, self_id, Token::Bracket(Brackets::LeftCurlyBracket));

            while self.match_variable_define(&self_id) { }

            if !self.term(Token::Bracket(Brackets::RightCurlyBracket)) ||
               !self.term(Token::Semicolon) {
                break;
            }
            insert!(self.tree, self_id, Token::Bracket(Brackets::RightCurlyBracket));
            insert!(self.tree, self_id, Token::Semicolon);

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        return false;
    }

    fn match_expr(&mut self, root: &NodeId) -> Option<NodeId> {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::Expr);

        loop {
            // variable expr_fix
            if let Some(tok) = self.match_variable() {
                insert!(self.tree, self_id, tok);
                self.match_expr_fix(&self_id);
                return Some(self_id);
            }

            // number expr_fix
            if let Some(tok) = self.match_number() {
                insert!(self.tree, self_id, tok);
                self.match_expr_fix(&self_id);
                return Some(self_id);
            }

            // (expr) expr_fix
            if self.term(Token::Bracket(Brackets::LeftParenthesis)) {
                // insert!(self.tree, self_id, Token::Bracket(Brackets::LeftParenthesis));
                if self.match_expr(&self_id).is_some() &&
                   self.term(Token::Bracket(Brackets::RightParenthesis)) {
                    // insert!(self.tree, self_id, Token::Bracket(Brackets::RightParenthesis));
                    self.match_expr_fix(&self_id);
                    return Some(self_id);
                }
                break;
            }

            break;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        None
    }

    fn match_expr_without_subtree(&mut self, root: &NodeId) -> bool {
        if let Some(id) = self.match_expr(&root) {
            self.tree.remove_node(id, LiftChildren).unwrap();
        }

        false
    }

    /// expr_fix = binary_op expr expr_fix | epsilon
    fn match_expr_fix(&mut self, root: &NodeId) -> bool {
        if let Some(tok) = self.match_binary_op() {
            insert!(self.tree, root, tok);
            return self.match_expr_without_subtree(&root) && self.match_expr_fix(&root);
        }
        true
    }

    fn match_variable(&mut self) -> TokenResult {
        if let Variable(ref v) = self.tokens[self.current] {
            self.current += 1;
            return Some(Token::Variable(v.clone()));
        }

        return None;
    }

    fn match_number(&mut self) -> TokenResult {
        if let Number(ref n) = self.tokens[self.current] {
            self.current += 1;
            return Some(Token::Variable(n.clone()));
        }

        return None;
    }

    fn match_binary_op(&mut self) -> TokenResult {
        if self.term(Token::Operator(Operators::And)) {
            Some(Token::Operator(Operators::And))
        } else if self.term(Token::Operator(Operators::Or)) {
            Some(Token::Operator(Operators::Or))
        } else if self.term(Token::Operator(Operators::Xor)) {
            Some(Token::Operator(Operators::Xor))
        } else if self.term(Token::Operator(Operators::LogicAnd)) {
            Some(Token::Operator(Operators::LogicAnd))
        } else if self.term(Token::Operator(Operators::LogicOr)) {
            Some(Token::Operator(Operators::LogicOr))
        } else if self.term(Token::Operator(Operators::Equal)) {
            Some(Token::Operator(Operators::Equal))
        } else {
            None
        }
    }

    fn term(&mut self, tok: Token) -> bool {

        if self.current >= self.tokens.len() {
            return false;
        }

        if self.tokens[self.current] == tok {
            self.current += 1;
            return true;
        }

        return false;
    }
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        let id = self.root_id();
        self.match_expr(&id).is_some()
    }
}

#[cfg(test)]
mod test {

    use parser::recursive_descent::*;

    trait TestResult {
        fn ok(&self) -> bool;
    }

    impl TestResult for bool {
        fn ok(&self) -> bool { *self }
    }

    impl<T> TestResult for Option<T> {
        fn ok(&self) -> bool { self.is_some() }
    }

    impl<V, E> TestResult for Result<V, E> {
        fn ok(&self) -> bool { self.is_ok() }
    }

    macro_rules! run_func {
        ($tests: tt, $func: ident) => {
            for test in $tests {
                let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
                let id = parser.root_id();
                assert!(parser.$func(&id).ok());
            }
        };
        ($tests: tt, $func: ident, $r: expr) => {
            for test in $tests {
                let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
                let id = parser.root_id();
                assert_eq!(parser.$func(&id).ok(), $r);
            }
        };
    }

    #[test]
    fn test_variable_define() {
        let tests = vec!["int number;", "short num0 ; ", "double\nd;"];
        run_func!(tests, match_variable_define);
    }

    #[ignore]
    #[test]
    fn test_variable_list() {
        let tests = vec!["int a, b_, c;"];
        run_func!(tests, match_variable_define);
    }

    #[test]
    fn test_struct_define() {
        let tests = vec!["struct Str { int a; short b; };",
                         "struct Str {};",
                         "\nstruct\nS\n{\nint\na\n;\n}\n;\n"];
        run_func!(tests, match_struct_define);

        let tests = vec!["struct for { int a; short b; };",
                         "struct S {}"];
        run_func!(tests, match_struct_define, false);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec!["a == b",
                         "a == b || c ^ d == 1"];
        run_func!(tests, match_expr);
    }
}