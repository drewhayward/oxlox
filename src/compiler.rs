use crate::{
    scanner::{Token, TokenType},
    vm,
};

// TODO: Convert expectations in this module into properly handled errors.

#[derive(Debug, PartialEq, PartialOrd)]
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
    /// Return the next highest precedence. Returns the same precedence if already at the highest
    fn successor(&self) -> Precedence {
        match *self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::Primary,
        }
    }

    fn from_token(ttype: &TokenType) -> Precedence {
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
            // TokenType::Minus => Precedence::Call,
            TokenType::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

#[derive(Debug)]
pub enum CompilationError {
    /// The compiler did not expect to see a token of the given type in its current state.
    UnexpectedToken(TokenType),
    /// Tokens remained after the completion of the complier.
    UnparsedTokens,
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

    pub fn compile(&mut self) -> Result<vm::Chunk, CompilationError> {
        // Create new compliation chunk
        self.current_chunk = Some(vm::Chunk::new());

        self.parse_expression();

        self.consume_token(TokenType::Eof);
        self.emit_op(vm::OpCode::Return);

        // TODO: Return proper error here
        if self.has_tokens() {
            return Err(CompilationError::UnparsedTokens);
        }

        match self.current_chunk.take() {
            Some(chunk) => Ok(chunk),
            None => panic!("Chunk should be defined."),
        }
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
    fn previous_token(&self) -> Token {
        self.tokens
            .get(self.position - 1)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
    }

    /// Return a copy of the current token being examined. This token has NOT
    /// been consumed.
    fn current_token(&self) -> Token {
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
        let current_ttype = self.current_token().ttype;
        assert!(
            current_ttype == expected_ttype,
            "Expected a token of {:?}",
            expected_ttype
        );

        self.error_at(
            &self.current_token(),
            format!("Unexpected token: {:?}", current_ttype).as_str(),
        );
        self.advance_token();
    }

    /// Returns true if there are more non-EOF tokens to consume.
    fn has_tokens(&self) -> bool {
        self.position < self.tokens.len() && self.current_token().ttype != TokenType::Eof
    }

    fn parse_stmt(&mut self) {}

    fn parse_expression(&mut self) {
        self.parse_expr_w_precedence(Precedence::Assignment);
    }

    /// Parse an expression of a specific precedence level or higher.
    fn parse_expr_w_precedence(&mut self, prec: Precedence) {
        self.advance_token();

        // Check for prefix rule
        let previous_ttype = self.previous_token().ttype;
        let prefix_handler = self
            .lookup_prefix_handler(previous_ttype)
            .expect("There should be a prefix rule");
        prefix_handler(self);

        // Check for infix rules while the observed prec is the same or higher
        while prec <= Precedence::from_token(&self.current_token().ttype) {
            self.advance_token();
            let previous_ttype = self.previous_token().ttype;
            let infix_handler = self
                .lookup_infix_handler(previous_ttype)
                .expect("Infix rule should exist if the precedence is higher for this token.");

            infix_handler(self);
        }
    }

    fn lookup_prefix_handler(&self, ttype: TokenType) -> Option<fn(&mut Compiler) -> ()> {
        match ttype {
            TokenType::Nil => Some(Compiler::handle_parse_literal),
            TokenType::True => Some(Compiler::handle_parse_literal),
            TokenType::False => Some(Compiler::handle_parse_literal),
            TokenType::Number(_) => Some(Compiler::handle_parse_number),
            TokenType::LeftParen => Some(Compiler::handle_parse_grouping),
            TokenType::Minus => Some(Compiler::handle_parse_unary),
            TokenType::Bang => Some(Compiler::handle_parse_unary),
            _ => None,
        }
    }

    fn lookup_infix_handler(&self, ttype: TokenType) -> Option<fn(&mut Compiler) -> ()> {
        match ttype {
            TokenType::Plus => Some(Compiler::handle_parse_binary),
            TokenType::Minus => Some(Compiler::handle_parse_binary),
            TokenType::Star => Some(Compiler::handle_parse_binary),
            TokenType::Slash => Some(Compiler::handle_parse_binary),
            // Logical comparison
            TokenType::EqualEqual => Some(Compiler::handle_parse_binary),
            TokenType::BangEqual => Some(Compiler::handle_parse_binary),
            TokenType::Greater => Some(Compiler::handle_parse_binary),
            TokenType::GreaterEqual => Some(Compiler::handle_parse_binary),
            TokenType::Less => Some(Compiler::handle_parse_binary),
            TokenType::LessEqual => Some(Compiler::handle_parse_binary),
            _ => None,
        }
    }

    fn handle_parse_number(&mut self) {
        if let Token {
            ttype: TokenType::Number(value),
            ..
        } = self.previous_token()
        {
            self.emit_constant(vm::Value::Number(value))
        }
        // TODO: handle token error
    }

    fn handle_parse_literal(&mut self) {
        match self.previous_token().ttype {
            TokenType::Nil => self.emit_op(vm::OpCode::Nil),
            TokenType::True => self.emit_op(vm::OpCode::True),
            TokenType::False => self.emit_op(vm::OpCode::False),
            _ => panic!("Unexpected token type {:?}", self.previous_token().ttype),
        }
    }

    fn handle_parse_grouping(&mut self) {
        self.parse_expression();
        self.consume_token(TokenType::RightParen);
    }

    fn handle_parse_unary(&mut self) {
        let operator_type = self.previous_token().ttype;

        // Parse the unary operand
        self.parse_expr_w_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit_op(vm::OpCode::Negate),
            TokenType::Bang => self.emit_op(vm::OpCode::Not),
            _ => unreachable!(),
        };
    }

    fn handle_parse_binary(&mut self) {
        let op_type = self.previous_token().ttype;
        let op_precedence = Precedence::from_token(&op_type);

        self.parse_expr_w_precedence(op_precedence.successor());

        match op_type {
            TokenType::BangEqual => {
                self.emit_op(vm::OpCode::Equal);
                self.emit_op(vm::OpCode::Not);
            }
            TokenType::EqualEqual => self.emit_op(vm::OpCode::Equal),
            TokenType::Greater => self.emit_op(vm::OpCode::Greater),
            TokenType::GreaterEqual => {
                self.emit_op(vm::OpCode::Less);
                self.emit_op(vm::OpCode::Not);
            },
            TokenType::Less => self.emit_op(vm::OpCode::Less),
            TokenType::LessEqual => {
                self.emit_op(vm::OpCode::Greater);
                self.emit_op(vm::OpCode::Not);
            },
            TokenType::Plus => self.emit_op(vm::OpCode::Add),
            TokenType::Minus => self.emit_op(vm::OpCode::Subtract),
            TokenType::Star => self.emit_op(vm::OpCode::Multiply),
            TokenType::Slash => self.emit_op(vm::OpCode::Divide),
            _ => unreachable!(),
        }
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
        let chunk = self.current_chunk().expect("Chunk should be defined.");
        let index = chunk.add_constant(value);
        self.emit_op_and_arg(vm::OpCode::Constant, index)
    }
}

#[cfg(test)]
mod test {
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
        let chunk = compiler.compile().unwrap();

        assert!(chunk.code.len() == 0, "Chunk should be empty")
    }
}
