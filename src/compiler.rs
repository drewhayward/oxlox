use crate::{
    scanner::{Token, TokenType},
    vm::Chunk,
};

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
    current_chunk: Option<Chunk>,
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
            current_chunk: None,
        }
    }

    pub fn compile(&mut self) -> Chunk {
        self.current_chunk = Some(Chunk::new());

        self.consume_token(TokenType::Eof);

        // TODO: Return proper error here
        if self.has_tokens() {
            panic!("Tokens remaining after Eof");
        }

        self.current_chunk
            .take()
            .expect("Chunk should be defined after compilation")
    }

    fn current_chunk(&self) -> Option<&Chunk> {
        self.current_chunk.as_ref()
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        eprint!("[line {}] Error", token.line);
        match token.ttype {
            TokenType::Eof => eprint!(" at end."),
            TokenType::Error => {}
            _ => eprint!(" at {:?} on line {}", token.ttype, token.line),
        }

        eprintln!("{}", message);
        self.had_error = true;
    }

    /* Parsing Methods */

    fn previous_token(&mut self) -> Token {
        self.tokens
            .get(self.position - 1)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
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
        // TODO: Propagate errors up from the scanner

        tok
    }

    fn consume_token(&mut self, ttype: TokenType) {
        // TODO: Handle error cases gracefully
        assert!(
            self.current_token().ttype == ttype,
            "Expected a token of {:?}",
            ttype
        );
        self.advance_token();
    }

    fn has_tokens(&mut self) -> bool {
        self.position < self.tokens.len() && self.current_token().ttype != TokenType::Eof
    }

    fn parse_stmt(&mut self) {}

    fn parse_expr(&mut self) {
        // Parse isolated expression

        // Parse lower-precedence
    }

    /* Byte code emissions */

    fn emit_byte(&mut self) {}
}

#[cfg(test)]
mod test {
    use crate::vm::{self, OpCode};

    use super::*;

    fn ttypes_to_tokens(ttypes: Vec<TokenType>) -> Vec<Token> {
        ttypes
            .into_iter()
            .map(|ttype| Token { ttype, line: 1 })
            .collect()
    }

    #[test]
    fn it_emits_chunks() {
        let input_tokens: Vec<_> = ttypes_to_tokens(vec![TokenType::Eof]);
        let mut compiler = Compiler::new(input_tokens);
        let chunk = compiler.compile();

        assert!(chunk.code.len() == 0, "Chunk should be empty")
    }

    //#[test]
    //fn it_compiles_numbers() {
    //    let input_tokens: Vec<_> = ttypes_to_tokens(vec![TokenType::Number(1.0), TokenType::Eof]);
    //    let mut compiler = Compiler::new(input_tokens);
    //    let chunk = compiler.compile();
    //
    //    assert_eq!(vec![OpCode::Constant { index: 0 }], chunk.code)
    //}
}
