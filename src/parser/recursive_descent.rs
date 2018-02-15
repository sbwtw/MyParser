
use token::*;
use token::Token::*;
use lexer::Lexer;
use parser::*;
use parser::symbol_checker::*;
use parser::syntax_node::*;

use id_tree::*;
use id_tree::InsertBehavior::*;
use id_tree::RemoveBehavior::*;

use std::io;
use std::rc::Rc;

type TokenResult = Option<Rc<Token>>;

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

pub struct RecursiveDescentParser {
    tokens: Vec<Rc<Token>>,
    current: usize,
    tree: SyntaxTree,
}

impl RecursiveDescentParser {
    pub fn new<I: io::Read>(lexer: Lexer<I>) -> RecursiveDescentParser {
        let mut tree = SyntaxTree::new();
        let root_node = Node::new(SyntaxType::SyntaxTree);
        tree.insert(root_node, AsRoot).unwrap();

        RecursiveDescentParser {
            tokens: lexer
                .filter(|x| !matches!(x, &Token::Comment(_)))
                .map(|x| Rc::new(x))
                .collect(),
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
        self.current == self.tokens.len()
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
            let id = insert!(self.tree, root, Rc::new(Token::Operator(Operators::LogicOr)));
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
            let id = insert!(self.tree, root, Rc::new(Token::Operator(Operators::LogicAnd)));
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
                insert!(self.tree, &self_id, Rc::new(Token::Operator(Operators::LogicNot)));
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

        if self.current >= self.tokens.len() { return None; }

        if let KeyWord(ref k) = *self.tokens[self.current] {
            if k.is_type() {
                let r = self.copy_current();
                self.current += 1;
                return r;
            }
        }

        return None;
    }

    fn match_variable_define_stmt(&mut self, root: &NodeId) -> bool {
        self.match_variable_define(root)
    }

    // variable_define = type variable_list
    fn match_variable_define(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::VariableDefine);

        if let Some(t) = self.match_type() {
            insert!(self.tree, self_id, t);

            if self.match_variable_list(&self_id) {
                return true;
            }
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        return false;
    }

    // variable_list = variable | variable , variable_list
    fn match_variable_list(&mut self, root: &NodeId) -> bool {
        if let Some(v) = self.match_identifier() {
            insert!(self.tree, root, v.clone());
        }

        if self.term(Token::Comma) {
            self.match_variable_list(root)
        } else {
            true
        }
    }

    fn match_struct_define(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::StructDefine);

        loop {
            if !self.term(Token::KeyWord(KeyWords::Struct)) { break; }

            if let Some(v) = self.match_identifier() {
                insert!(self.tree, self_id, v);
            }

            if !self.term(Token::Bracket(Brackets::LeftCurlyBracket)) { break; }

            while self.match_variable_define_stmt(&self_id) &&
                  self.term(Token::Semicolon) { }

            if !self.term(Token::Bracket(Brackets::RightCurlyBracket)) ||
               !self.term(Token::Semicolon) {
                break;
            }

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

            self.adjust_single_child(self_id);

            return self.match_expr_mul_fix(root);
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
            if let Some(tok) = self.match_expr_ident() {
                insert!(self.tree, root, tok);
                return true;
            }

            break;
        }

        self.current = cur;
        false
    }

    // - `stmt_factor`
    fn match_stmt(&mut self, root: &NodeId) -> bool {
        self.match_stmt_factor(root)
    }

    // - `stmt_single` `;`
    // - `stmt_block`
    // - `stmt_control`
    // - `;`
    fn match_stmt_factor(&mut self, root: &NodeId) -> bool {
        self.match_stmt_block(root) ||
        self.match_stmt_control(root) ||
        self.match_stmt_single(root) && self.term(Token::Semicolon) ||
        self.term(Token::Semicolon)
    }

    // - `assign_stmt`
    // - `break_stmt`
    // - `return_stmt`
    fn match_stmt_single(&mut self, root: &NodeId) -> bool {
        self.match_assign_stmt(root) ||
        self.match_break_stmt(root) ||
        self.match_return_stmt(root) ||
        self.match_variable_define_stmt(root)
    }

    // - `if_stmt`
    // - `while_loop`
    // - `for_loop`
    fn match_stmt_control(&mut self, root: &NodeId) -> bool {
        self.match_if_stmt(root) ||
        self.match_while_loop(root) ||
        self.match_for_loop(root)
    }

    fn match_stmt_list(&mut self, root: &NodeId) -> bool {
        while self.match_stmt(root) {}

        true
    }

    fn match_stmt_block(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::StmtBlock);

        loop {
            // `{`
            if !self.term(Token::Bracket(Brackets::LeftCurlyBracket)) { break; }

            // `stmt_list`
            if !self.match_stmt_list(&self_id) { break; }

            // '}'
            if !self.term(Token::Bracket(Brackets::RightCurlyBracket)) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // `for` `(` `expr_opt` `;` `expr_opt` `;` `expr_opt` `)` `stmt`
    fn match_for_loop(&mut self, root: &NodeId) -> bool {

        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::ForLoop);

        loop {
            // `for`
            if !self.term(Token::KeyWord(KeyWords::For)) { break; }

            // `(`
            if !self.term(Token::Bracket(Brackets::LeftParenthesis)) { break; }

            // expr_opt1 ;
            let expr_opt1 = insert_type!(self.tree, &self_id, SyntaxType::ExprOpt);
            self.match_assign_stmt(&expr_opt1);
            if !self.term(Token::Semicolon) { break; }

            // expr_opt2 ;
            let expr_opt2 = insert_type!(self.tree, &self_id, SyntaxType::ExprOpt);
            self.match_bool_expr(&expr_opt2);
            if !self.term(Token::Semicolon) { break; }

            // expr_opt3
            let expr_opt3 = insert_type!(self.tree, &self_id, SyntaxType::ExprOpt);
            if !self.match_assign_stmt(&expr_opt3) { break; }

            // ')'
            if !self.term(Token::Bracket(Brackets::RightParenthesis)) { break; }

            // `stmt`
            if !self.match_stmt(&self_id) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // `while` `(` `bool_expr` `)` `stmt`
    fn match_while_loop(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::WhileLoop);

        loop {
            // `while`
            if !self.term(Token::KeyWord(KeyWords::While)) { break; }

            // `(`
            if !self.term(Token::Bracket(Brackets::LeftParenthesis)) { break; }

            // `bool_expr`
            if !self.match_bool_expr(&self_id) { break; }

            // ')'
            if !self.term(Token::Bracket(Brackets::RightParenthesis)) { break; }

            // `stmt`
            if !self.match_stmt(&self_id) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // assign_stmt = left_value = right_value
    fn match_assign_stmt(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::AssignStmt);

        loop {
            // left_value
            if !self.match_left_value(&self_id) { break; }

            // `=`
            if !self.term(Token::Operator(Operators::Assign)) { break; }

            // right_value
            if !self.match_right_value(&self_id) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // if_stmt = if ( bool_expr ) stmt else stmt
    fn match_if_stmt(&mut self, root: &NodeId) -> bool {
        let cur = self.current;

        loop {
            // if
            if self.term(Token::KeyWord(KeyWords::If)) {
                let if_id = insert_type!(self.tree, root, SyntaxType::IfStmt);

                // ( bool_expr ) stmt
                if self.term(Token::Bracket(Brackets::LeftParenthesis)) &&
                   self.match_bool_expr(&if_id) &&
                   self.term(Token::Bracket(Brackets::RightParenthesis)) &&
                   self.match_stmt(&if_id) {
                    // else
                    if self.term(Token::KeyWord(KeyWords::Else)) {
                        let else_cur = self.current;
                        let else_id = insert_type!(self.tree, root, SyntaxType::ElseStmt);

                        // stmt
                        if self.match_stmt(&else_id) { return true; }

                        self.current = else_cur;
                        self.tree.remove_node(else_id, DropChildren).unwrap();
                    }

                    return true;
                }

                self.tree.remove_node(if_id, DropChildren).unwrap();
                break;
            }

            break;
        }

        self.current = cur;
        false
    }

    // `return` `return_expr`
    fn match_return_stmt(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::ReturnStmt);

        loop {
            if !self.term(Token::KeyWord(KeyWords::Return)) { break; }
            if !self.match_return_type(&self_id) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // `break`
    fn match_break_stmt(&mut self, root: &NodeId) -> bool {
        if self.term(Token::KeyWord(KeyWords::Break)) {
            insert_type!(self.tree, root, SyntaxType::BreakStmt);
            return true
        }

        false
    }

    // - `bool_expr`
    // - `epsilon`
    fn match_return_type(&mut self, root: &NodeId) -> bool {
        self.match_bool_expr(root);

        true
    }

    // left_value = ident
    fn match_left_value(&mut self, root: &NodeId) -> bool {
        if let Some(id) = self.match_identifier() {
            insert!(self.tree, root, id);

            return true;
        }
        return false;
    }

    // right_value = bool_expr
    fn match_right_value(&mut self, root: &NodeId) -> bool {
        self.match_bool_expr(root)
    }

    // `func_ret_type` `func_name` `(` `func_param_list` `)` `;`
    fn match_function_declare(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::FuncDeclare);

        loop {
            // type
            match self.match_type() {
                Some(t) => insert!(self.tree, self_id, t),
                _ => break,
            };

            // func_name
            match self.match_identifier() {
                Some(id) => insert!(self.tree, self_id, id),
                _ => break,
            };

            if !self.term(Token::Bracket(Brackets::LeftParenthesis)) { break; }
            if !self.match_func_param_list(&self_id) { break; }
            if !self.term(Token::Bracket(Brackets::RightParenthesis)) { break; }
            if !self.term(Token::Semicolon) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // - `func_ret_type` `func_name` `(` `func_param_list` `)` `{` `func_body` `}`
    fn match_function_define(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::FuncDefine);

        loop {
            // type
            match self.match_type() {
                Some(t) => insert!(self.tree, self_id, t),
                _ => break,
            };

            // func_name
            match self.match_identifier() {
                Some(id) => insert!(self.tree, self_id, id),
                _ => break,
            };

            if !self.term(Token::Bracket(Brackets::LeftParenthesis)) { break; }
            if !self.match_func_param_list(&self_id) { break; }
            if !self.term(Token::Bracket(Brackets::RightParenthesis)) { break; }
            if !self.term(Token::Bracket(Brackets::LeftCurlyBracket)) { break; }

            // func_body
            if !self.match_stmt_list(&self_id) { break; }

            if !self.term(Token::Bracket(Brackets::RightCurlyBracket)) { break; }

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // - `func_param` `func_param_list_tail`
    // - `epsilon`
    fn match_func_param_list(&mut self, root: &NodeId) -> bool {

        if self.match_func_param(root) {
            return self.match_func_param_list_tail(root);
        }

        true
    }

    // - `,` `func_param` `func_param_list_tail`
    // - `epsilon`
    fn match_func_param_list_tail(&mut self, root: &NodeId) -> bool {

        if self.term(Token::Comma) {
            return self.match_func_param(root) &&
                   self.match_func_param_list_tail(root);
        }

        true
    }

    // `func_param_type` `func_param_name`
    fn match_func_param(&mut self, root: &NodeId) -> bool {
        let cur = self.current;
        let self_id = insert_type!(self.tree, root, SyntaxType::FuncParam);

        loop {
            // func_param_type
            match self.match_type() {
                Some(t) => insert!(self.tree, self_id, t),
                _ => break,
            };

            match self.match_identifier() {
                Some(id) => insert!(self.tree, self_id, id),
                _ => break,
            };

            return true;
        }

        self.current = cur;
        self.tree.remove_node(self_id, DropChildren).unwrap();
        false
    }

    // > | >= | < | <=
    fn match_cmp_op(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        return match *self.tokens[self.current] {
            Token::Operator(Operators::Greater) |
            Token::Operator(Operators::GreaterEqual) |
            Token::Operator(Operators::Less) |
            Token::Operator(Operators::LessEqual) => {
                self.current += 1;
                self.copy_previous()
            },
            _ => None,
        }
    }

    // == | !=
    fn match_equal_op(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        return match *self.tokens[self.current] {
            Token::Operator(Operators::Equal) |
            Token::Operator(Operators::NotEqual) => {
                self.current += 1;
                self.copy_previous()
            },
            _ => None,
        }
    }

    fn match_add_op(&mut self) -> TokenResult {
        if self.term(Token::Operator(Operators::Add)) {
            return self.copy_previous();
        }

        if self.term(Token::Operator(Operators::Minus)) {
            return self.copy_previous();
        }

        None
    }

    fn match_mul_op(&mut self) -> TokenResult {
        if self.term(Token::Operator(Operators::Division)) {
            return self.copy_previous();
        }

        if self.term(Token::Asterisk) {
            return Some(Rc::new(Token::Operator(Operators::Mul)));
        }

        None
    }

    fn match_expr_ident(&mut self) -> TokenResult {
        if let Some(t) = self.match_identifier() { return Some(t); }
        if let Some(t) = self.match_number() { return Some(t); }

        None
    }

    fn match_identifier(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        if let Identifier(_, _) = *self.tokens[self.current] {
            self.current += 1;
            return self.copy_previous();
        }

        return None;
    }

    fn match_number(&mut self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }

        if let Number(_) = *self.tokens[self.current] {
            self.current += 1;
            return self.copy_previous();
        }

        return None;
    }

    fn adjust_single_child(&mut self, node: NodeId) {
        let children_num = self.tree.children(&node).unwrap().count();

        if children_num == 1 {
            self.tree.remove_node(node, LiftChildren).unwrap();
        }
    }

    fn copy_previous(&self) -> TokenResult {
        if self.current == 0 { return None; }
        return Some(self.tokens[self.current - 1].clone())
    }

    fn copy_current(&self) -> TokenResult {
        if self.current >= self.tokens.len() { return None; }
        return Some(self.tokens[self.current].clone())
    }

    fn term(&mut self, tok: Token) -> bool {

        if self.current >= self.tokens.len() {
            return false;
        }

        if *self.tokens[self.current] == tok {
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
    fn run(&mut self) -> ParserResult {
        let ref id = self.root_id();
        let mut last_pos = self.tokens.len();

        loop {
            if self.current == self.tokens.len() { break; }
            if self.current == last_pos {
                return Err(ParseErrInfo {
                        err_type: ParseError::SyntaxError
                       });
            }

            last_pos = self.current;

            self.match_struct_define(id);
            self.match_function_define(id);
            self.match_function_declare(id);
        }

        SymbolChecker::new(&mut self.tree).check()
    }

    fn syntax_tree(&self) -> &SyntaxTree {
        &self.tree
    }
}

#[cfg(test)]
mod test {

    use id_tree::Tree;
    use id_tree::InsertBehavior::*;

    use parser::recursive_descent::*;
    use parser::syntax_node::SyntaxType::*;

    macro_rules! test_func {
        ($tests: tt, $func: ident) => {
            for test in $tests {
                let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
                let id = parser.root_id();
                assert!(parser.$func(&id) && parser.lexer_end());
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
        ($test: expr, $func: ident, $tree: ident, $dump: expr) => {
            let mut parser = RecursiveDescentParser::new(Lexer::new($test.as_bytes()));
            let id = parser.root_id();

            let tree_root_id = $tree.root_node_id().unwrap();

            assert!(parser.$func(&id));

            if $dump { parser.dump(); }

            assert_eq!(parser.syntax_tree().height(), $tree.height());

            let tree_iter = $tree.traverse_pre_order(tree_root_id).unwrap();
            let parser_iter = parser.traverse_pre_order();
            assert_eq!(tree_iter.count(), parser_iter.count());

            let tree_iter = $tree.traverse_pre_order(tree_root_id).unwrap();
            let parser_iter = parser.traverse_pre_order();

            for (node1, node2) in tree_iter.zip(parser_iter) {
                assert_eq!(node1.data(), node2.data());
                assert_eq!(node1.children().len(), node2.children().len());
            }
        };

        ($test: expr, $func: ident, $tree: ident) => {
            test_tree!($test, $func, $tree, false);
        }
    }

    macro_rules! tree {
        () => {
            {
            let mut tree = Tree::new();
            let root_id = tree.insert(Node::new(SyntaxType::SyntaxTree), AsRoot).unwrap();
            (tree, root_id)
            }
        }
    }

    #[test]
    fn test_variable_define() {
        let tests = vec!["int number", "short num0 ", "double\nd"];
        test_func!(tests, match_variable_define);
    }

    #[test]
    fn test_variable_list() {
        let tests = vec!["int a, b_, c"];
        test_func!(tests, match_variable_define);
    }

    #[test]
    fn test_struct_define() {
        let tests = vec!["struct Str { int a; short b; };",
                         "\nstruct\n    { int a; }; \n",
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

        let test = "1 * 1 * 1 * 1";
        let (mut tree, root_id) = tree!();
        insert!(tree, root_id, Rc::new(Token::Number(Numbers::from_str("1"))));
        insert!(tree, root_id, Rc::new(Token::Operator(Operators::Mul)));
        insert!(tree, root_id, Rc::new(Token::Number(Numbers::from_str("1"))));
        insert!(tree, root_id, Rc::new(Token::Operator(Operators::Mul)));
        insert!(tree, root_id, Rc::new(Token::Number(Numbers::from_str("1"))));
        insert!(tree, root_id, Rc::new(Token::Operator(Operators::Mul)));
        insert!(tree, root_id, Rc::new(Token::Number(Numbers::from_str("1"))));

        test_tree!(test, match_expr, tree);
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
        let (mut tree, root_id) = tree!();
        let expr = insert_type!(tree, root_id, Expr);
            insert!(tree, expr, Rc::new(Token::Identifier("a".to_owned(), Type::NoType)));
            insert!(tree, expr, Rc::new(Token::Operator(Operators::Add)));
            insert!(tree, expr, Rc::new(Token::Identifier("b".to_owned(), Type::NoType)));
        insert!(tree, root_id, Rc::new(Token::Operator(Operators::NotEqual)));
        let expr = insert_type!(tree, root_id, Expr);
            insert!(tree, expr, Rc::new(Token::Identifier("c".to_owned(), Type::NoType)));
            insert!(tree, expr, Rc::new(Token::Operator(Operators::Add)));
            insert!(tree, expr, Rc::new(Token::Number(Numbers::from_str("1"))));
        insert!(tree, root_id, Rc::new(Token::Operator(Operators::LogicOr)));
        let bool_expr = insert_type!(tree, root_id, BooleanExpr);
            insert!(tree, bool_expr, Rc::new(Token::Operator(Operators::LogicNot)));
            insert!(tree, bool_expr, Rc::new(Token::Identifier("e".to_owned(), Type::NoType)));

        test_tree!(test, match_bool_expr, tree);
        test_tree!(test1, match_bool_expr, tree);
    }

    #[test]
    fn test_assign_stmt() {
        let tests = vec!["number = x + 1",
                         "num = x",
                         "num\n=\n1"];
        test_func!(tests, match_assign_stmt);

        let failure_tests = vec!["number = x"];
        test_func!(failure_tests, match_stmt, false);

        let (mut tree, root_id) = tree!();
        let assign = insert_type!(tree, root_id, AssignStmt);
            insert!(tree, assign, Rc::new(Token::Identifier("number".to_owned(), Type::NoType)));
            let expr = insert_type!(tree, assign, Expr);
            insert!(tree, expr, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
            insert!(tree, expr, Rc::new(Token::Operator(Operators::Add)));
            insert!(tree, expr, Rc::new(Token::Number(Numbers::from_str("1"))));

        test_tree!("number = x + 1;", match_assign_stmt, tree);
    }

    #[test]
    fn test_if_stmt() {
        let tests = vec!["if (x == 1) x = 1; else x = 2;"];
        test_func!(tests, match_if_stmt);

        // if-else
        let (mut tree, root_id) = tree!();
        let if_stmt = insert_type!(tree, root_id, IfStmt);
            insert!(tree, if_stmt, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
            insert!(tree, if_stmt, Rc::new(Token::Operator(Operators::Equal)));
            insert!(tree, if_stmt, Rc::new(Token::Number(Numbers::from_str("1"))));
            let assign = insert_type!(tree, if_stmt, AssignStmt);
                insert!(tree, assign, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
                insert!(tree, assign, Rc::new(Token::Number(Numbers::from_str("1"))));
        let else_stmt = insert_type!(tree, root_id, ElseStmt);
            let assign = insert_type!(tree, else_stmt, AssignStmt);
                insert!(tree, assign, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
                insert!(tree, assign, Rc::new(Token::Number(Numbers::from_str("2"))));

        let stmt = "if(x==1)x=1;else\nx=2;";
        test_tree!(stmt, match_if_stmt, tree);

        // if-if-else
        let (mut tree, root_id) = tree!();
        let if_stmt = insert_type!(tree, root_id, IfStmt);
            insert!(tree, if_stmt, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
            insert!(tree, if_stmt, Rc::new(Token::Operator(Operators::Equal)));
            insert!(tree, if_stmt, Rc::new(Token::Number(Numbers::from_str("1"))));
            let inner_if = insert_type!(tree, if_stmt, IfStmt);
                insert!(tree, inner_if, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
                insert!(tree, inner_if, Rc::new(Token::Operator(Operators::NotEqual)));
                insert!(tree, inner_if, Rc::new(Token::Number(Numbers::from_str("2"))));
                let assign = insert_type!(tree, inner_if, AssignStmt);
                    insert!(tree, assign, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
                    insert!(tree, assign, Rc::new(Token::Number(Numbers::from_str("3"))));
            let else_stmt = insert_type!(tree, if_stmt, ElseStmt);
                let assign = insert_type!(tree, else_stmt, AssignStmt);
                    insert!(tree, assign, Rc::new(Token::Identifier("x".to_owned(), Type::NoType)));
                    insert!(tree, assign, Rc::new(Token::Number(Numbers::from_str("2"))));

        let stmt = "if(x==1)if(x!=2)x=3;else\nx=2;";
        test_tree!(stmt, match_if_stmt, tree);
    }

    #[test]
    fn test_stmt_list() {
        let tests = vec!["a = 2; b = 3;",
                         "",
                        //  ";",
                         "a = 2;"];
        test_func!(tests, match_stmt_list);
    }
}