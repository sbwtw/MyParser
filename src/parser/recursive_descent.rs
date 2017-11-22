
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
/// expr = expr add_op expr_mul
///     -> expr_mul expr_fix
/// expr_fix = add_op expt_mul expr_fix | epsilon
///
/// expr_mul = expr_mul mul_op expr_factor
///         -> expr_factor expr_mul_fix
/// expr_mul_fix = mul_op expr_factor expr_mul_fix | epsilon
///
/// expr_factor = (expr) | ident
///
/// ident = number | variable
/// add_op = + | -
/// mul_op = * | /
/// single_op = ! | ~
///
/// bool_expr = bool_expr || bool_expr |
///             bool_expr && bool_expr |
///             bool_expr equal_op bool_expr |
///             bool_expr cmp_op bool_expr |
///             !expr |
///             expr
///
/// bool_expr = bool_expr || bool_expr_and
///          -> bool_expr_and bool_expr_fix
/// bool_expr_fix = || bool_expr_and bool_expr_fix | epsilon
///
/// bool_expr_and = bool_expr_and && bool_expr_equal
///              -> bool_expr_equal bool_expr_and_fix
/// bool_expr_and_fix = && bool_expr_equal bool_expr_and_fix | epsilon
///
/// bool_expr_equal = bool_expr_equal equal_op bool_expr_cmp
///                -> bool_expr_cmp bool_expr_equal_fix
/// bool_expr_equal_fix = equal_op bool_expr_cmp bool_expr_equal_fix | epsilon
///
/// bool_expr_cmp = bool_expr_cmp cmp_op bool_expr_factor
///              -> bool_expr_factor bool_expr_cmp_fix
/// bool_expr_cmp_fix = cmp_op bool_expr_factor bool_expr_cmp_fix | epsilon
///
/// bool_expr_factor = !bool_expr | (bool_expr) | expr
///
/// cmp_op = > | >= | < | <=
/// equal_op = == | !=
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

macro_rules! replace {
    ($tree: expr, $root: expr, $type: expr) => {
        $tree.get_mut($root).unwrap().replace_data($type);
    }
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
        let ref id = self.root_id();
        dump_tree(&self.tree, id, 0);
    }

    pub fn traverse_pre_order(&self) -> PreOrderTraversal<SyntaxType> {
        let ref id = self.root_id();
        self.tree.traverse_pre_order(id).unwrap()
    }

    #[cfg(debug_assertions)]
    pub fn lexer_end(&self) -> bool {
        matches!(self.current == self.tokens.len(), true)
    }

    fn root_id(&self) -> NodeId {
        self.tree.root_node_id().unwrap().clone()
    }

    /// bool_expr = bool_expr || bool_expr_and
    ///          -> bool_expr_and bool_expr_fix
    fn match_bool_expr(&mut self, root: &NodeId) -> bool {
        self.match_bool_expr_and(root) &&
        self.match_bool_expr_fix(root)
    }

    /// bool_expr_fix = || bool_expr_and bool_expr_fix | epsilon
    fn match_bool_expr_fix(&mut self, root: &NodeId) -> bool {
        if self.term(Token::Operator(Operators::LogicOr)) {
            let id = insert!(self.tree, root, Token::Operator(Operators::LogicOr));
            let root_id = insert_type!(self.tree, root, SyntaxType::BooleanExpr);

            if self.match_bool_expr_and(&root_id) &&
               self.match_bool_expr_fix(&root_id) {
                self.adjust_single_child(root_id);
                return true;
            }

            self.tree.remove_node(id, DropChildren).unwrap();
            self.tree.remove_node(root_id, DropChildren).unwrap();
            return false;
        }

        true
    }

    /// bool_expr_and = bool_expr_and && bool_expr_equal
    ///              -> bool_expr_equal bool_expr_and_fix
    fn match_bool_expr_and(&mut self, root: &NodeId) -> bool {
        self.match_bool_expr_equal(root) &&
        self.match_bool_expr_and_fix(root)
    }

    /// bool_expr_and_fix = && bool_expr_equal bool_expr_and_fix | epsilon
    fn match_bool_expr_and_fix(&mut self, root: &NodeId) -> bool {
        if self.term(Token::Operator(Operators::LogicAnd)) {
            let id = insert!(self.tree, root, Token::Operator(Operators::LogicAnd));
            let root_id = insert_type!(self.tree, root, SyntaxType::BooleanExpr);

            if self.match_bool_expr_equal(&root_id) &&
               self.match_bool_expr_and_fix(&root_id) {
                self.adjust_single_child(root_id);
                return true;
            }

            self.tree.remove_node(id, DropChildren).unwrap();
            self.tree.remove_node(root_id, DropChildren).unwrap();
            return false;
        }

        true
    }

    /// bool_expr_equal = bool_expr_equal equal_op bool_expr_cmp
    ///                -> bool_expr_cmp bool_expr_equal_fix
    fn match_bool_expr_equal(&mut self, root: &NodeId) -> bool {
        self.match_bool_expr_cmp(root) &&
        self.match_bool_expr_equal_fix(root)
    }

    /// bool_expr_equal_fix = equal_op bool_expr_cmp bool_expr_equal_fix | epsilon
    fn match_bool_expr_equal_fix(&mut self, root: &NodeId) -> bool {
        if let Some(tok) = self.match_equal_op() {
            let id = insert!(self.tree, root, tok);

            if self.match_bool_expr_cmp(root) &&
               self.match_bool_expr_equal_fix(root) {
                return true;
            }

            self.tree.remove_node(id, DropChildren).unwrap();
            return false;
        }

        true
    }

    /// bool_expr_cmp = bool_expr_cmp cmp_op bool_expr_factor
    ///              -> bool_expr_factor bool_expr_cmp_fix
    fn match_bool_expr_cmp(&mut self, root: &NodeId) -> bool {
        self.match_bool_expr_factor(root) &&
        self.match_bool_expr_cmp_fix(root)
    }

    /// bool_expr_cmp_fix = cmp_op bool_expr_factor bool_expr_cmp_fix | epsilon
    fn match_bool_expr_cmp_fix(&mut self, root: &NodeId) -> bool {
        if let Some(tok) = self.match_cmp_op() {
            let id = insert!(self.tree, root, tok);

            if self.match_bool_expr_factor(root) &&
               self.match_bool_expr_cmp_fix(root) {
                return true;
            }

            self.tree.remove_node(id, DropChildren).unwrap();
            return false;
        }

        true
    }

    /// bool_expr_factor = !bool_expr | (bool_expr) | expr
    fn match_bool_expr_factor(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::BooleanExpr);

        loop {
            if self.term(Token::Operator(Operators::LogicNot)) {
                insert!(self.tree, &self_id, Token::Operator(Operators::LogicNot));
                if self.match_bool_expr(&self_id) {
                    return true;
                }
                break;
            }

            if self.term(Token::Bracket(Brackets::LeftParenthesis)) {
                if self.match_bool_expr(&self_id) &&
                   self.term(Token::Bracket(Brackets::RightParenthesis)) {
                    self.adjust_single_child(self_id);
                    return true;
                }
                break;
            }

            if self.match_expr(&self_id) {
                replace!(self.tree, &self_id, SyntaxType::Expr);
                self.adjust_single_child(self_id);
                return true;
            }

            break;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
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

    //// expr = expr add_op expr_mul
    ///      -> expr_mul expr_fix
    fn match_expr(&mut self, root: &NodeId) -> bool {
        if self.match_expr_mul(root) {
            return self.match_expr_fix(root);
        }

        false
    }

    /// expr_fix = add_op expt_mul expr_fix | epsilon
    fn match_expr_fix(&mut self, root: &NodeId) -> bool {
        let cur = self.current;

        loop {
            if let Some(tok) = self.match_add_op() {
                let id = insert!(self.tree, root, tok);

                let self_id = insert_type!(self.tree, root, SyntaxType::Expr);
                if self.match_expr_mul(&self_id) {
                    if self.match_expr_fix(&self_id) {
                        self.adjust_single_child(self_id);
                        return true;
                    }
                }

                self.tree.remove_node(self_id, DropChildren).unwrap();
                self.tree.remove_node(id, DropChildren).unwrap();
                break;
            }

            return true;
        }

        self.current = cur;
        false
    }

    /// expr_mul = expr_mul mul_op expr_factor
    ///         -> expr_factor expr_mul_fix
    fn match_expr_mul(&mut self, root: &NodeId) -> bool {
        if self.match_expr_factor(root) {
            return self.match_expr_mul_fix(root);
        }

        false
    }

    /// expr_mul_fix = mul_op expr_factor expr_mul_fix | epsilon
    fn match_expr_mul_fix(&mut self, root: &NodeId) -> bool {
        if let Some(tok) = self.match_mul_op() {
            insert!(self.tree, root, tok);

            let self_id = insert_type!(self.tree, root, SyntaxType::Expr);
            if !self.match_expr_factor(&self_id) {
                self.tree.remove_node(self_id, DropChildren).unwrap();
                return false;
            }
            if self.match_expr_mul_fix(root) {
                self.adjust_single_child(self_id);
                return true;
            } else {
                return false;
            }
        }

        true
    }


    /// expr_factor = (expr) | ident
    fn match_expr_factor(&mut self, root: &NodeId) -> bool {
        let cur = self.current;

        loop {
            // (expr)
            if self.term(Token::Bracket(Brackets::LeftParenthesis)) {
                if self.match_expr(root) {
                    if self.term(Token::Bracket(Brackets::RightParenthesis)) {
                        return true;
                    }
                }
                break;
            }

            // ident
            if let Some(tok) = self.match_ident() {
                insert!(self.tree, root, tok);
                return true;
            }

            break;
        }

        self.current = cur;
        false
    }

    // > | >= | < | <=
    fn match_cmp_op(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        return match self.tokens[self.current] {
            ref tok @ Token::Operator(Operators::Greater) |
            ref tok @ Token::Operator(Operators::GreaterEqual) |
            ref tok @ Token::Operator(Operators::Less) |
            ref tok @ Token::Operator(Operators::LessEqual) => {
                self.current += 1;
                Some(tok.clone())
            },
            _ => None,
        }
    }

    // == | !=
    fn match_equal_op(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        return match self.tokens[self.current] {
            ref tok @ Token::Operator(Operators::Equal) |
            ref tok @ Token::Operator(Operators::NotEqual) => {
                self.current += 1;
                Some(tok.clone())
            },
            _ => None,
        }
    }

    fn match_add_op(&mut self) -> TokenResult {
        if self.term(Token::Operator(Operators::Add)) {
            return Some(Token::Operator(Operators::Add));
        }

        if self.term(Token::Operator(Operators::Minus)) {
            return Some(Token::Operator(Operators::Minus));
        }

        None
    }

    fn match_mul_op(&mut self) -> TokenResult {
        if self.term(Token::Operator(Operators::Division)) {
            return Some(Token::Operator(Operators::Division));
        }

        if self.term(Token::Asterisk) {
            return Some(Token::Operator(Operators::Mul));
        }

        None
    }

    fn match_ident(&mut self) -> TokenResult {
        if let Some(t) = self.match_variable() { return Some(t); }
        if let Some(t) = self.match_number() { return Some(t); }

        None
    }

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
            return Some(Token::Number(n.clone()));
        }

        return None;
    }

    fn adjust_single_child(&mut self, node: NodeId) {
        let children_num = self.tree.children(&node).unwrap().count();
        assert!(children_num > 0);

        if children_num == 1 {
            self.tree.remove_node(node, LiftChildren).unwrap();
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

    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    fn peek<'a>(&'a self) -> Option<&'a Token> {
        if self.current >= self.tokens.len() {
            return None;
        }

        return Some(&self.tokens[self.current]);
    }
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        let ref id = self.root_id();

        self.match_bool_expr(id) ||
        self.match_struct_define(id)
    }

    fn syntax_tree(&self) -> &SyntaxTree {
        &self.tree
    }
}

#[cfg(test)]
mod test {

    use parser::recursive_descent::*;
    use parser::syntax_node::SyntaxType::*;

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

    macro_rules! test_func {
        ($tests: tt, $func: ident) => {
            for test in $tests {
                let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
                let id = parser.root_id();
                assert!(parser.$func(&id).ok() && parser.lexer_end());
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

    macro_rules! test_tree {
        ($test: tt, $func: ident, $tree: tt) => {
            let mut parser = RecursiveDescentParser::new(Lexer::new($test.as_bytes()));
            let id = parser.root_id();

            assert!(parser.$func(&id));
            assert!(parser.traverse_pre_order().map(|x| x.data()).eq($tree.iter()));
        }
    }

    #[test]
    fn test_variable_define() {
        let tests = vec!["int number;", "short num0 ; ", "double\nd;"];
        test_func!(tests, match_variable_define);
    }

    #[ignore]
    #[test]
    fn test_variable_list() {
        let tests = vec!["int a, b_, c;"];
        test_func!(tests, match_variable_define);
    }

    #[test]
    fn test_struct_define() {
        let tests = vec!["struct Str { int a; short b; };",
                         "struct Str {};",
                         "\nstruct\nS\n{\nint\na\n;\n}\n;\n"];
        test_func!(tests, match_struct_define);

        let tests = vec!["struct for { int a; short b; };",
                         "struct S {}"];
        test_func!(tests, match_struct_define, false);
    }

    #[test]
    fn test_experssion() {
        let tests = vec!["num1 + num2 * 1",
                         "(3)",
                         "3",
                         "num1 + num2",
                         "(e+f)",
                         "(3)+1",
                         "2 \n- \t4 +\n 3\n *\n2",
                        //  "3 ^ 2",
                        //  "3 % num",
                         "2-((4)*(2))"];
        test_func!(tests, match_expr);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec!["a == b",
                         "a == b || c + d == 1",
                         "a+b!=c+d||(e+f) && (d==1||f==2)",
                         "a&&b\n  ||c&&!d\t||\n   !\t\n!e||f\t+\n1==3",
                         "a == b || c + d == 1"];
        test_func!(tests, match_bool_expr);

        let test = "a + b != c + 1 || !e";
        let test1 = "(a+b)!=(c+1)||!(e)";
        let result = vec![
            SyntaxTree,
              Expr,
                Terminal(Token::Variable("a".to_owned())),
                Terminal(Token::Operator(Operators::Add)),
                Terminal(Token::Variable("b".to_owned())),
              Terminal(Token::Operator(Operators::NotEqual)),
              Expr,
                Terminal(Token::Variable("c".to_owned())),
                Terminal(Token::Operator(Operators::Add)),
                Terminal(Token::Number("1".to_owned())),
              Terminal(Token::Operator(Operators::LogicOr)),
              BooleanExpr,
                Terminal(Token::Operator(Operators::LogicNot)),
                Terminal(Token::Variable("e".to_owned()))];

        test_tree!(test, match_bool_expr, result);
        test_tree!(test1, match_bool_expr, result);
    }
}