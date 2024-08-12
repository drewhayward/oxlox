use core::panic;

use crate::object::ObjectType;
use crate::{
    heap::GcHeap,
    scanner::{Token, TokenType},
    vm,
};

// TODO: Convert expectations in this module into properly handled errors.
// Include error sycronization at statement-like points.

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
    UnexpectedToken { ttype: TokenType, msg: String },
    /// Expected the file to have more tokens but none were present.
    ExhaustedTokens,
}

#[derive(Debug)]
pub struct CompilationReport {
    pub errors: Vec<CompilationError>,
}

#[derive(Debug)]
pub struct Compiler<'vm> {
    heap: &'vm mut GcHeap,
    tokens: Vec<Token>,
    position: usize,
    _had_error: bool,
    current_chunk: Option<vm::Chunk>,
}

impl<'vm> Compiler<'vm> {
    pub fn new(tokens: Vec<Token>, heap: &'vm mut GcHeap) -> Compiler {
        if tokens.is_empty() {
            panic!("Attempting to parse an empty list of tokens")
        }

        Compiler {
            heap,
            tokens,
            position: 0,
            _had_error: false,
            current_chunk: None,
        }
    }

    pub fn compile(&mut self) -> Result<vm::Chunk, CompilationReport> {
        // Create new compliation chunk
        self.current_chunk = Some(vm::Chunk::new());

        let mut errors: Vec<CompilationError> = Vec::new();
        let mut last_pos = self.position;
        while !self.match_token(TokenType::Eof) {
            match self.parse_declaration() {
                Ok(_) => {}
                Err(error) => {
                    errors.push(error);
                    self.sychronize();
                }
            }

            // Ensure we're making progress
            if last_pos == self.position {
                panic!("We're stuck in a parsing loop");
            } else {
                last_pos = self.position;
            }
        }

        //self.emit_op(vm::OpCode::Return);

        // TODO: Return proper error here
        if self.has_tokens() {
            panic!("Tokens remained after the EOF token")
        }

        match self.current_chunk.take() {
            Some(chunk) => Ok(chunk),
            None => unreachable!("Chunk should be defined."),
        }
    }

    fn current_chunk(&mut self) -> Option<&mut vm::Chunk> {
        self.current_chunk.as_mut()
    }

    fn _error_at(&mut self, token: &Token, message: &str) {
        eprint!("[line {}] Error", token.line);
        match token.ttype {
            TokenType::Eof => eprint!(" at end."),
            TokenType::Error => {}
            _ => eprint!(" at {:?} on line {}", token.ttype, token.line),
        }

        eprintln!("{}", message);
        self._had_error = true;
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

        tok
    }

    fn match_token(&mut self, expected_ttype: TokenType) -> bool {
        if self.current_token().ttype.is_variant_eq(&expected_ttype) {
            self.advance_token();
            true
        } else {
            false
        }
    }

    /// Advance the compiler assuming the current token matches the expected type.
    /// Panics if the token doesn't match the expected type.
    fn consume_token(&mut self, expected_ttype: TokenType) -> Result<(), CompilationError> {
        let current_ttype = self.current_token().ttype;
        if !current_ttype.is_variant_eq(&expected_ttype) {
            return Err(CompilationError::UnexpectedToken {
                ttype: current_ttype.clone(),
                msg: format!(
                    "Expected a token of type {:?} but found {:?}",
                    expected_ttype, current_ttype
                ),
            });
        }

        self.advance_token();

        Ok(())
    }

    /// Returns true if there are more non-EOF tokens to consume.
    fn has_tokens(&self) -> bool {
        self.position < self.tokens.len() && self.current_token().ttype != TokenType::Eof
    }

    /// Reset the parser to a good state from an error state by advancing to the next statement.
    /// Semicolons and statement starting terms are used to determine when to stop advancing.
    fn sychronize(&mut self) {
        self._had_error = false;

        while self.current_token().ttype != TokenType::Eof {
            if self.previous_token().ttype == TokenType::Semicolon {
                return;
            }

            match self.current_token().ttype {
                TokenType::Class => return,
                TokenType::Fun => return,
                TokenType::Var => return,
                TokenType::For => return,
                TokenType::If => return,
                TokenType::While => return,
                TokenType::Print => return,
                TokenType::Return => return,
                _ => {
                    self.advance_token();
                }
            }
        }
    }

    fn parse_declaration(&mut self) -> Result<(), CompilationError> {
        match self.current_token().ttype {
            TokenType::Var => {
                self.advance_token();
                self.parse_variable_decl()
            }
            _ => self.parse_stmt(),
        }
    }

    fn parse_variable_decl(&mut self) -> Result<(), CompilationError> {
        let name_index = self.parse_variable()?;

        // Emit the variable declaration value
        if self.match_token(TokenType::Equal) {
            self.parse_expression();
        } else {
            self.emit_op(vm::OpCode::Nil);
        };

        self.emit_op_and_arg(vm::OpCode::DefineGlobal, name_index);
        self.consume_token(TokenType::Semicolon)?;

        Ok(())
    }

    /// Returns the index to the variable name constant
    fn parse_variable(&mut self) -> Result<u8, CompilationError> {
        self.consume_token(TokenType::Identifier {
            name: String::default(),
        })?;

        match self.previous_token().ttype {
            TokenType::Identifier { name } => Ok(self.make_identifier_constant(name)),
            _ => unreachable!("Previous must be an Identifier token"),
        }
    }

    fn parse_stmt(&mut self) -> Result<(), CompilationError> {
        match self.current_token().ttype {
            TokenType::Print => {
                self.advance_token();
                self.parse_expression()?;
                self.consume_token(TokenType::Semicolon)?;
                self.emit_op(vm::OpCode::Print);
            }
            // Expr statement
            _ => {
                self.parse_expression()?;
                self.consume_token(TokenType::Semicolon)?;
            }
        }

        Ok(())
    }

    fn parse_expression(&mut self) -> Result<(), CompilationError> {
        self.parse_expr_w_precedence(Precedence::Assignment)
    }

    /// Parse an expression of a specific precedence level or higher.
    fn parse_expr_w_precedence(&mut self, prec: Precedence) -> Result<(), CompilationError> {
        self.advance_token();

        // Check for prefix rule
        let previous_ttype = self.previous_token().ttype;
        let prefix_handler = self.lookup_prefix_handler(&previous_ttype).expect(
            format!(
                "There should be a prefix rule for token type {:?}",
                &previous_ttype
            )
            .as_ref(),
        );

        let can_assign = prec <= Precedence::Assignment;
        prefix_handler(self, can_assign);

        // Check for infix rules while the observed prec is the same or higher
        while prec <= Precedence::from_token(&self.current_token().ttype) {
            self.advance_token();
            let previous_ttype = self.previous_token().ttype;
            let infix_handler = self
                .lookup_infix_handler(previous_ttype)
                .expect("Infix rule should exist if the precedence is higher for this token.");

            infix_handler(self);
        }

        Ok(())
    }

    /* Pratt-Style Parsing table for Expressions */

    fn lookup_prefix_handler(
        &self,
        ttype: &TokenType,
    ) -> Option<fn(&mut Compiler<'vm>, bool) -> ()> {
        match ttype {
            TokenType::Nil => Some(Compiler::handle_parse_literal),
            TokenType::True => Some(Compiler::handle_parse_literal),
            TokenType::False => Some(Compiler::handle_parse_literal),
            TokenType::Number(_) => Some(Compiler::handle_parse_number),
            TokenType::LeftParen => Some(Compiler::handle_parse_grouping),
            TokenType::Minus => Some(Compiler::handle_parse_unary),
            TokenType::Bang => Some(Compiler::handle_parse_unary),
            TokenType::String { .. } => Some(Compiler::handle_parse_string),
            TokenType::Identifier { .. } => Some(Compiler::handle_parse_variable),
            _ => None,
        }
    }

    fn lookup_infix_handler(&self, ttype: TokenType) -> Option<fn(&mut Compiler<'vm>) -> ()> {
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

    fn handle_parse_number(&mut self, _can_assign: bool) {
        if let Token {
            ttype: TokenType::Number(value),
            ..
        } = self.previous_token()
        {
            self.emit_constant(vm::LoxValue::Number(value))
        }
        // TODO: handle token error
    }

    fn handle_parse_string(&mut self, _can_assign: bool) {
        if let Token {
            ttype: TokenType::String { literal },
            ..
        } = self.previous_token()
        {
            let heap_ref = self.heap.allocate(ObjectType::String(literal.clone()));
            self.emit_constant(vm::LoxValue::Object(heap_ref))
        }
    }

    fn handle_parse_variable(&mut self, can_assign: bool) {
        if let Token {
            ttype: TokenType::Identifier { name },
            ..
        } = self.previous_token()
        {
            let name_index = self.make_identifier_constant(name);

            // Depending on the next token we will change get vs set
            if can_assign && self.match_token(TokenType::Equal) {
                self.parse_expression();
                self.emit_op_and_arg(vm::OpCode::SetGlobal, name_index);
            } else {
                self.emit_op_and_arg(vm::OpCode::GetGlobal, name_index);
            }
        }
    }

    fn handle_parse_literal(&mut self, _can_assign: bool) {
        match self.previous_token().ttype {
            TokenType::Nil => self.emit_op(vm::OpCode::Nil),
            TokenType::True => self.emit_op(vm::OpCode::True),
            TokenType::False => self.emit_op(vm::OpCode::False),
            _ => panic!("Unexpected token type {:?}", self.previous_token().ttype),
        }
    }

    fn handle_parse_grouping(&mut self, _can_assign: bool) {
        // Reset the precedence since the parens ensure unambiguous parsing
        self.parse_expression();
        self.consume_token(TokenType::RightParen);
    }

    fn handle_parse_unary(&mut self, _can_assign: bool) {
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
            }
            TokenType::Less => self.emit_op(vm::OpCode::Less),
            TokenType::LessEqual => {
                self.emit_op(vm::OpCode::Greater);
                self.emit_op(vm::OpCode::Not);
            }
            TokenType::Plus => self.emit_op(vm::OpCode::Add),
            TokenType::Minus => self.emit_op(vm::OpCode::Subtract),
            TokenType::Star => self.emit_op(vm::OpCode::Multiply),
            TokenType::Slash => self.emit_op(vm::OpCode::Divide),
            _ => unreachable!(),
        }
    }

    fn make_identifier_constant(&mut self, name: String) -> u8 {
        let heap_ref = self.heap.allocate(ObjectType::String(name.clone()));
        let lox_value = vm::LoxValue::Object(heap_ref);

        let chunk = self.current_chunk().unwrap();
        let name_index = chunk.add_constant(lox_value);

        name_index
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

    fn emit_constant(&mut self, value: vm::LoxValue) {
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
        let mut heap = GcHeap::new();
        let input_tokens: Vec<_> = ttypes_to_tokens(vec![TokenType::Eof]);
        let mut compiler = Compiler::new(input_tokens, &mut heap);
        let chunk = compiler.compile().unwrap();

        assert!(chunk.code.len() == 0, "Chunk should be empty")
    }
}
