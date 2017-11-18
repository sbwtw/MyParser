
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

type TokenResult = Option<Token>;

macro_rules! insert {
    ($tree: expr, $root: expr, $tok: expr) => {
        $tree.insert(Node::new(SyntaxType::Terminal($tok)), UnderNode(&$root)).unwrap();
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

    pub fn root_id(&self) -> NodeId {
        self.tree.root_node_id().unwrap().clone()
    }

    pub fn dump(&self) {
        let id = self.root_id();
        dump_tree(&self.tree, &id, 0);
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
        let self_id = self.tree.insert(Node::new(SyntaxType::Variable), UnderNode(root)).unwrap();

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
        let self_id = self.tree.insert(Node::new(SyntaxType::Struct), UnderNode(root)).unwrap();

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

    fn match_variable(&mut self) -> TokenResult {

        if let Variable(ref v) = self.tokens[self.current] {
            self.current += 1;
            return Some(Token::Variable(v.clone()));
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
}

impl Parser for RecursiveDescentParser {
    fn run(&mut self) -> bool {
        let id = self.root_id();
        self.match_struct_define(&id)
    }
}

#[cfg(test)]
mod test {

    use lexer;
    use parser::recursive_descent::*;

    #[test]
    fn test_variable_define() {
        let tests = vec!["int number;", "short num0 ; ", "double\nd;"];

        for test in tests {
            let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
            let id = parser.root_id();
            assert!(parser.match_variable_define(&id));
        }
    }

    #[ignore]
    #[test]
    fn test_variable_list() {
        let test = "int a, b_, c;";
        let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
        let id = parser.root_id();
        assert!(parser.match_variable_define(&id));
    }

    #[test]
    fn test_struct_define() {
        let tests = vec!["struct Str { int a; short b; };",
                         "struct Str {};",
                         "\nstruct\nS\n{\nint\na\n;\n}\n;\n"];

        for test in tests {
            let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
            let id = parser.root_id();
            assert!(parser.match_struct_define(&id));
        }

        let tests = vec!["struct for { int a; short b; };",
                         "struct S {}"];

        for test in tests {
            let mut parser = RecursiveDescentParser::new(Lexer::new(test.as_bytes()));
            let id = parser.root_id();
            assert!(!parser.match_struct_define(&id));
        }
    }
}