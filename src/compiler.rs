use crate::{
    scanner::{Token, TokenType},
    vm,
};

// TODO: Convert expectations in this module into properly handled errors.

#[derive(Debug)]
enum Precedence {
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

impl Precedence {
    fn from_prefix_token(ttype: TokenType) -> Precedence {
        match ttype {
            TokenType::Equal => Precedence::Assignment,
            TokenType::Or => Precedence::Or,
            TokenType::And => Precedence::And,
            TokenType::EqualEqual => Precedence::Equality,
            TokenType::BangEqual => Precedence::Equality,
            TokenType::Less => Precedence::Comparison,
            TokenType::LessEqual => Precedence::Comparison,
            TokenType::Greater => Precedence::Comparison,
            TokenType::GreaterEqual => Precedence::Comparison,
            TokenType::Plus => Precedence::Term,
            TokenType::Minus => Precedence::Term,
            TokenType::Star => Precedence::Factor,
            TokenType::Slash => Precedence::Factor,
            TokenType::Bang => Precedence::Unary,
            // TODO: Handle this case?
            TokenType::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

#[derive(Debug)]
pub struct Compiler {
    tokens: Vec<Token>,
    position: usize,
    had_error: bool,
    current_chunk: Option<vm::Chunk>,
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

    pub fn compile(&mut self) -> vm::Chunk {
        self.current_chunk = Some(vm::Chunk::new());

        self.consume_token(TokenType::Eof);

        // TODO: Return proper error here
        if self.has_tokens() {
            panic!("Tokens remaining after Eof");
        }

        self.current_chunk
            .take()
            .expect("Chunk should be defined after compilation")
    }

    fn current_chunk(&mut self) -> Option<&mut vm::Chunk> {
        self.current_chunk.as_mut()
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

    /* Parsing Methods
     * These methods parse the token stream and emit bytecode at the same time.
     */

    /// Return a copy of the previous token. This is the most-recently consumed
    /// token.
    fn previous_token(&mut self) -> Token {
        self.tokens
            .get(self.position - 1)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
    }

    /// Return a copy of the current token being examined. This token has NOT
    /// been consumed.
    fn current_token(&mut self) -> Token {
        self.tokens
            .get(self.position)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
    }

    /// Advance the compiler by one token and return the token.
    fn advance_token(&mut self) -> Token {
        let tok = self.current_token();
        self.position += 1;
        // TODO: Propagate errors up from the scanner

        tok
    }

    /// Advance the compiler assuming the current token matches the expected type.
    /// Panics if the token doesn't match the expected type.
    fn consume_token(&mut self, expected_ttype: TokenType) {
        // TODO: Handle error cases gracefully
        assert!(
            self.current_token().ttype == expected_ttype,
            "Expected a token of {:?}",
            expected_ttype
        );
        self.advance_token();
    }

    /// Returns true if there are more non-EOF tokens to consume.
    fn has_tokens(&mut self) -> bool {
        self.position < self.tokens.len() && self.current_token().ttype != TokenType::Eof
    }

    fn parse_stmt(&mut self) {}

    fn parse_expression(&mut self) {}

    /// Parse an expression of a specific precedence level or higher.
    fn parse_expr_w_precedence(&mut self, prec: Precedence) {
        // Check for prefix rule

        // Check for infix rule

        // Check for post-fix rule?
    }

    fn parse_number(&mut self) {
        //self.current_chunk()
        //self.emit_op()
    }

    /* Byte code emitters */

    fn emit_op(&mut self, op: vm::OpCode) {
        let line = self.previous_token().line;
        self.current_chunk()
            .expect("Chunk should be defined to write ops")
            .write_op(op, line);
    }

    fn emit_op_and_arg(&mut self, op: vm::OpCode, arg: u8) {
        let line = self.previous_token().line;
        let chunk = self.current_chunk().expect("Chunk should be defined.");
        chunk.write_op(op, line);
        chunk.write(arg, line);
        
    }

    fn emit_constant(&mut self, value: vm::Value) {
        let line = self.previous_token().line;
        let chunk = self.current_chunk().expect("Chunk should be defined.");

        let index = chunk.add_constant(value);
        self.emit_op_and_arg(vm::OpCode::Constant, index)
    }



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
