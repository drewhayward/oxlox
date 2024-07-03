use crate::{scanner, scanner::{Token, TokenType}, vm::Chunk};


// TODO: Convert expectations in this module into properly handled errors.

#[derive(Debug)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

#[derive(Debug)]
pub struct Compiler {
    tokens: Vec<Token>,
    position: usize,
    had_error: bool,
}

impl Compiler {
    pub fn new(tokens: Vec<Token>) -> Compiler {
        if tokens.is_empty() {
            panic!("Attempting to parse an empty list of tokens")
        }

        Compiler {
            tokens,
            position: 0,
            had_error: false,
        }
    }

    pub fn compile(&mut self) -> Chunk {
        while self.has_tokens() {

        }

        Chunk::new()
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        eprint!("[line {}] Error", token.line);
        match token.ttype {
            TokenType::Eof => eprint!(" at end."),
            TokenType::Error => {}
            _ => eprint!(" at {:?} on line {}", token.ttype, token.line)
        }

        eprintln!("{}", message);
        self.had_error = true;
    }

    fn current_token(&mut self) -> Token {
        self.tokens
            .get(self.position)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
    }

    fn advance_token(&mut self) -> Token {
        let tok = self.current_token();
        self.position += 1;

        tok
    }

    fn consume_token(&mut self, ttype: TokenType) {
        // TODO: Handle error cases gracefully
        assert!(self.current_token().ttype == ttype, "Expected a token of {:?}", ttype);
        self.advance_token();
    }

    fn has_tokens(&mut self) -> bool {
        self.position < self.tokens.len() && self.current_token().ttype != TokenType::Eof
    }

    fn parse_stmt(&mut self) {

    }

    fn parse_expr(&mut self) {
        // Parse isolated expression
        
        // Parse lower-precedence
    }
}
