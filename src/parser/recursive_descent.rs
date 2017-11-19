
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
/// expr = expr_mul expr_fix
/// expr_fix = add_op expr_mul | epsilon
///
/// expr_mul = expr_factor expr_mul_fix
/// expr_mul_fix = mul_op expr_factor | epsilon
///
/// expr -> expr_mul expr_fix
///      -> expr_factor expr_mul_fix expr_fix
///
/// expr_factor -> expr
///             -> expr_factor expr_mul_fix expr_fix
///             -> expr_factor mul_op expr_factor expr_fix |
///                expr_factor add_op expr_mul
///
/// expr_factor = (expr) expr_factor_fix |
///               variable expr_factor_fix |
///               number expr_factor_fix |
///               mul_op expr_factor expr_fix expr_factor_fix |
///               add_op expr_mul expr_factor_fix
/// expr_factor_fix = add_op expr_mul expr_factor_fix | epslion
///
/// add_op = + | -
/// mul_op = * | /
/// single_op = ! | ~
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

    // fn match_expr(&mut self, root: &NodeId) -> Option<NodeId> {
    //     let cur = self.current;
    //     let self_id = insert_type!(self.tree, root, SyntaxType::Expr);

    //     loop {
    //         // expr = number expr_fix | variable expr_fix
    //         if let Some(tok) = self.match_variable() {
    //             insert!(self.tree, self_id, tok);
    //             if self.match_expr_fix(&self_id) {
    //                 return Some(self_id);
    //             }
    //         }

    //         // number expr_fix
    //         if let Some(tok) = self.match_number() {
    //             insert!(self.tree, self_id, tok);
    //             if self.match_expr_fix(&self_id) {
    //                 return Some(self_id);
    //             }
    //         }

    //         break;
    //     }

    //     self.current = cur;
    //     self.tree.remove_node(self_id, DropChildren).unwrap();
    //     None
    // }

    // expr_mul expr_fix
    fn match_expr(&mut self, root: &NodeId) -> bool {
        println!("match expr -> {:?}", self.peek());
        if self.match_expr_mul(root) {
            return self.match_expr_fix(root);
        }

        false
    }

    /// expr_fix = add_op expr_mul | epsilon
    fn match_expr_fix(&mut self, root: &NodeId) -> bool {
        println!("match expr fix -> {:?}", self.peek());

        if let Some(tok) = self.match_add_op() {
            insert!(self.tree, root, tok);

            let self_id = insert_type!(self.tree, root, SyntaxType::Expr);
            if !self.match_expr_mul(&self_id) {
                self.tree.remove_node(self_id, DropChildren).unwrap();
            }
        }

        true
    }

    fn match_add_op(&mut self) -> Option<Token> {
        if self.term(Token::Operator(Operators::Add)) {
            return Some(Token::Operator(Operators::Add));
        }

        if self.term(Token::Operator(Operators::Minus)) {
            return Some(Token::Operator(Operators::Minus));
        }

        None
    }

    // expr_mul = expr_factor expr_mul_fix
    fn match_expr_mul(&mut self, root: &NodeId) -> bool {
        println!("match expr mul -> {:?}", self.peek());
        if self.match_expr_factor(root) {
            return self.match_expr_mul_fix(root);
        }

        false
    }

    // expr_mul_fix = mul_op expr_factor | epsilon
    fn match_expr_mul_fix(&mut self, root: &NodeId) -> bool {
        println!("match expr mul fix -> {:?}", self.peek());
        if let Some(tok) = self.match_mul_op() {
            insert!(self.tree, root, tok);

            let self_id = insert_type!(self.tree, root, SyntaxType::Expr);
            if !self.match_expr_factor(&self_id) {
                self.tree.remove_node(self_id, DropChildren).unwrap();
            }
        }

        true
    }

    fn match_mul_op(&mut self) -> Option<Token> {
        if self.term(Token::Operator(Operators::Division)) {
            return Some(Token::Operator(Operators::Division));
        }

        if self.term(Token::Asterisk) {
            return Some(Token::Operator(Operators::Mul));
        }

        None
    }

    /// expr_factor = (expr) expr_factor_fix |
    ///               variable expr_factor_fix |
    ///               number expr_factor_fix |
    ///               mul_op expr_factor expr_fix expr_factor_fix |
    ///               add_op expr_mul expr_factor_fix
    fn match_expr_factor(&mut self, root: &NodeId) -> bool {
        println!("match expr factor -> {:?}", self.peek());
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::Expr);

        loop {
            // (expr) expr_fix
            if self.term(Token::Bracket(Brackets::LeftParenthesis)) {
                if self.match_expr(&self_id) {
                    if self.term(Token::Bracket(Brackets::RightParenthesis)) {
                        return self.match_expr_factor_fix(&self_id);
                    }
                }
                break;
            }

            // mul_op expr_factor expr_fix expr_factor_fix
            if let Some(tok) = self.match_mul_op() {
                insert!(self.tree, self_id, tok);

                return self.match_expr_factor(&self_id) &&
                       self.match_expr_fix(&self_id) &&
                       self.match_expr_factor_fix(&self_id);
            }

            // add_op expr_mul expr_factor_fix
            if let Some(tok) = self.match_add_op() {
                insert!(self.tree, self_id, tok);

                return self.match_expr_mul(&self_id) &&
                       self.match_expr_factor_fix(&self_id);
            }

            // variable
            if let Some(tok) = self.match_variable() {
                insert!(self.tree, self_id, tok);
                return self.match_expr_factor_fix(&self_id);
            }

            // number expr_fix
            if let Some(tok) = self.match_number() {
                insert!(self.tree, self_id, tok);
                return self.match_expr_factor_fix(&self_id);
            }
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // expr_factor_fix = add_op expr_mul expr_factor_fix | epslion
    fn match_expr_factor_fix(&mut self, root: &NodeId) -> bool {
        println!("match expr factor fix -> {:?}", self.peek());

        if let Some(tok) = self.match_add_op() {
            insert!(self.tree, root, tok);

            let self_id = insert_type!(self.tree, root, SyntaxType::Expr);
            if !self.match_expr_mul(&self_id) {
                self.tree.remove_node(self_id, DropChildren).unwrap();
                return false;
            }

            return self.match_expr_factor_fix(root);
        }

        true
    }

    // fn match_expr_without_subtree(&mut self, root: &NodeId) -> Option<NodeId> {
    //     if let Some(id) = self.match_expr(&root) {
    //         self.tree.remove_node(id, LiftChildren).unwrap();
    //         return Some(root.clone());
    //     }

    //     None
    // }

    fn match_variable(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        if let Variable(ref v) = self.tokens[self.current] {
            self.current += 1;
            return Some(Token::Variable(v.clone()));
        }

        return None;
    }

    fn match_number(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        if let Number(ref n) = self.tokens[self.current] {
            self.current += 1;
            return Some(Token::Variable(n.clone()));
        }

        return None;
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

    fn peek<'a>(&'a self) -> Option<&'a Token> {
        if self.current >= self.tokens.len() {
            return None;
        }

        return Some(&self.tokens[self.current]);
    }
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        let id = self.root_id();
        self.match_expr(&id)
    }
}

#[cfg(test)]
mod test {

    use parser::recursive_descent::*;

    use id_tree::NodeId;

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
        ($tests: tt, $func: ident, $($r: tt)+) => {
            for test in $tests {
                let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
                let id = parser.root_id();
                assert!(matches!(parser.$func(&id), $($r)+));
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

    #[ignore]
    #[test]
    fn test_boolean_expression() {
        let tests = vec!["a == b",
                         "a == b || c ^ d == 1"];
        run_func!(tests, match_expr);
    }
}