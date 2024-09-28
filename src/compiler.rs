use std::borrow::BorrowMut;
use std::{u16, u8};

use crate::object::ObjectType;
use crate::scanner::Scanner;
use crate::vm::{Chunk, OpCode};
use crate::{
    heap::GcHeap,
    scanner::{Token, TokenType},
    vm,
};

// WIP: Pull the parser out of the compiler struct so we can stack the compilers and they share the
// same parser/state

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
            // TokenType::Minus => Precedence::Call,
            TokenType::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    position: usize,
    total_consumed: u64,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        if tokens.is_empty() {
            panic!("Attempting to parse an empty list of tokens")
        }

        Parser {
            tokens,
            position: 0,
            total_consumed: 0,
        }
    }

    /// Return a copy of the previous token. This is the most-recently consumed token.
    fn previous_token(&self) -> Token {
        self.tokens
            .get(self.position - 1)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
    }

    /// Return a copy of the current token being examined. This token has NOT been consumed.
    fn current_token(&self) -> Token {
        self.tokens
            .get(self.position)
            .expect("Tried to get a position outside the token buffer.")
            .clone()
    }

    /// Return true if the current token of the provided type variant
    fn check_token(&self, ttype: &TokenType) -> bool {
        self.current_token().ttype.is_variant_eq(ttype)
    }

    /// Advance the compiler by one token and return the token.
    fn advance_token(&mut self) -> Token {
        let tok = self.current_token();
        self.position += 1;
        self.total_consumed += 1;

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
    /// Returns a compilation error if the token type doesn't match.
    fn consume_token(&mut self, expected_ttype: TokenType) -> Result<(), CompilationError> {
        let current_ttype = self.current_token().ttype;
        if !current_ttype.is_variant_eq(&expected_ttype) {
            return Err(CompilationError::UnexpectedToken {
                token: self.current_token(),
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
}

/// Used to track variables in the LexicalStack.
#[derive(Debug, Clone)]
struct Local {
    /// The name token of the local variable.
    name: String,
    /// The lexical scope depth of the variable, global=0 and increments from there.
    depth: u64,
    /// Flags if the variable has been initialized for use yet.
    initialized: bool,
}

impl Default for Local {
    fn default() -> Self {
        Local {
            name: String::default(),
            depth: 0,
            initialized: false,
        }
    }
}

pub const MAX_LOCAL_COUNT: usize = (u8::MAX as usize) + 1;

/// Struct used to track the lexical scope and list of defined variables
#[derive(Debug)]
pub struct LexicalStack {
    // We are only allowed 1 bytes worth of local variables since we just index into the stack
    locals: [Local; MAX_LOCAL_COUNT],
    local_count: usize,
    scope_depth: u64,
}

impl LexicalStack {
    pub fn new() -> LexicalStack {
        LexicalStack {
            locals: std::array::from_fn(|_| Local::default()),
            local_count: 0,
            scope_depth: 0,
        }
    }

    pub fn current_depth(&self) -> u64 {
        self.scope_depth
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1
    }

    /// Closes the currently tracked scope, removes local variables and returns the number removed
    pub fn end_scope(&mut self) -> u8 {
        self.scope_depth -= 1;

        let mut num_popped = 0;
        while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
            num_popped += 1;
            self.local_count -= 1;
        }

        num_popped
    }

    pub fn add_local(&mut self, name: String) -> Result<(), CompilationError> {
        if self.local_count >= MAX_LOCAL_COUNT {
            return Err(CompilationError::TooManyLocals);
        }

        // Check for double declarations
        for i in (0..self.local_count).rev() {
            let local = &self.locals[i];
            if local.depth < self.current_depth() {
                break;
            }

            if name == local.name {
                return Err(CompilationError::RedeclaredVariable);
            }
        }

        self.locals[self.local_count] = Local {
            name,
            depth: self.scope_depth,
            initialized: false,
        };
        self.local_count += 1;

        Ok(())
    }

    pub fn mark_initialized(&mut self) {
        if self.local_count != 0 {
            self.locals[self.local_count - 1].initialized = true;
        }
    }

    /// Attempts to find a local with the given name. Returns an a stack index as a Some(u8) if the
    /// variable exists.
    pub fn resolve_local(&self, name: &str) -> Result<Option<u8>, CompilationError> {
        for i in (0..self.local_count).rev() {
            let local = &self.locals[i];

            if local.name == name {
                if !local.initialized {
                    return Err(CompilationError::VarSelfInitUse);
                }
                assert!(i < MAX_LOCAL_COUNT);
                return Ok(Some(i as u8));
            }
        }

        Ok(None)
    }
}

#[derive(Debug)]
pub enum CompilationError {
    /// The compiler did not expect to see a token of the given type in its current state
    UnexpectedToken { token: Token, msg: String },
    /// Expected the file to have more tokens but none were present
    ExhaustedTokens,
    /// There are more than 256 local variables in scope
    TooManyLocals,
    /// A variable with this name has already been declared in this scope
    RedeclaredVariable,
    /// Use of variable in its own initializer
    VarSelfInitUse,
}

#[derive(Debug)]
pub struct CompilationReport {
    pub errors: Vec<CompilationError>,
}

/// Used by the Complier to store per-function compliation results.
#[derive(Debug)]
struct FuncComplier {
    chunk: Chunk,
    _enclosing: Option<Box<FuncComplier>>,
    lexical_stack: LexicalStack,
}

#[derive(Debug)]
struct Compiler<'vm> {
    heap: &'vm mut GcHeap,
    parser: Parser,
    current_function: Option<FuncComplier>,
}

impl<'vm> Compiler<'vm> {
    fn new(heap: &'vm mut GcHeap, parser: Parser) -> Compiler {
        Compiler {
            heap,
            parser,
            current_function: Some(FuncComplier {
                chunk: vm::Chunk::new(),
                lexical_stack: LexicalStack::new(),
                _enclosing: None,
            }),
        }
    }

    fn compile(&mut self) -> Result<vm::Chunk, CompilationReport> {
        let mut errors: Vec<CompilationError> = Vec::new();
        let mut total_consumed = self.parser.total_consumed;
        while !self.parser.match_token(TokenType::Eof) {
            match self.parse_declaration() {
                Ok(_) => {}
                Err(error) => {
                    eprintln!(
                        "Error: line={} {:?}",
                        self.parser.current_token().line,
                        error
                    );
                    errors.push(error);
                    //self.sychronize();
                }
            }

            // Ensure we're making progress
            let consumed = self.parser.total_consumed;
            if total_consumed == consumed {
                panic!("We're stuck in a parsing loop");
            } else {
                total_consumed = consumed;
            }
        }

        //self.emit_op(vm::OpCode::Return);
        if self.parser.has_tokens() {
            panic!("Tokens remained after the EOF token")
        }


        // Extract out the compiled script function
        Ok(self.current_function.take().unwrap().chunk)
    }

    fn current_chunk(&mut self) -> Option<&mut vm::Chunk> {
        self.current_function
            .as_mut()
            .map(|func_comp| func_comp.chunk.borrow_mut())
    }

    /// Returns a mutable reference to the lexical stack of the function currently being compiled
    fn current_lexical_stack(&mut self) -> &mut LexicalStack {
        self.current_function
            .as_mut()
            .unwrap()
            .lexical_stack
            .borrow_mut()
    }

    //fn _error_at(&mut self, token: &Token, message: &str) {
    //    eprint!("[line {}] Error", token.line);
    //    match token.ttype {
    //        TokenType::Eof => eprint!(" at end."),
    //        TokenType::Error => {}
    //        _ => eprint!(" at {:?} on line {}", token.ttype, token.line),
    //    }
    //
    //    eprintln!("{}", message);
    //    self._had_error = true;
    //}

    /* Parsing Methods
     * These methods parse the token stream and emit bytecode at the same time.
     */
    /// Returns true if the current token is equal to the provided type

    /// Reset the parser to a good state from an error state by advancing to the next statement. Semicolons and statement
    /// starting terms are used to determine when to stop advancing.
    //fn sychronize(&mut self) {
    //    self._had_error = false;
    //
    //    while self.current_token().ttype != TokenType::Eof {
    //        if self.previous_token().ttype == TokenType::Semicolon {
    //            return;
    //        }
    //
    //        match self.current_token().ttype {
    //            TokenType::Class => return,
    //            TokenType::Fun => return,
    //            TokenType::Var => return,
    //            TokenType::For => return,
    //            TokenType::If => return,
    //            TokenType::While => return,
    //            TokenType::Print => return,
    //            TokenType::Return => return,
    //            _ => {
    //                self.advance_token();
    //            }
    //        }
    //    }
    //}

    fn parse_declaration(&mut self) -> Result<(), CompilationError> {
        match self.parser.current_token().ttype {
            TokenType::Var => {
                self.parser.advance_token();
                self.parse_variable_decl()
            }
            TokenType::RightBrace => Ok(()),
            _ => self.parse_stmt(),
        }
    }

    fn parse_variable_decl(&mut self) -> Result<(), CompilationError> {
        let name_index = self.define_variable()?;

        // Emit the variable declaration value
        if self.parser.match_token(TokenType::Equal) {
            self.parse_expression()?;
        } else {
            self.emit_op(vm::OpCode::Nil);
        };

        if self.current_lexical_stack().current_depth() > 0 {
            self.parser.consume_token(TokenType::Semicolon)?;
            self.current_lexical_stack().mark_initialized();
            return Ok(());
        }

        self.emit_op_and_arg(
            vm::OpCode::DefineGlobal,
            name_index.expect("Name is global and is Some(u8)"),
        );
        self.parser.consume_token(TokenType::Semicolon)?;

        Ok(())
    }

    /// Defines a variable as either a global or a local in the scope tracker. If it's a global,
    /// return an index to the name constant in the code block, locals return Ok(None)
    fn define_variable(&mut self) -> Result<Option<u8>, CompilationError> {
        self.parser.consume_token(TokenType::Identifier {
            name: String::default(),
        })?;

        // In the case of a local variable, we don't need to do a runtime name-based lookup
        if self.current_lexical_stack().current_depth() > 0 {
            match self.parser.previous_token().ttype {
                TokenType::Identifier { name } => self.current_lexical_stack().add_local(name)?,
                _ => unreachable!("Previous token was just confirmed to be an Identifier"),
            }
            return Ok(None);
        }

        match self.parser.previous_token().ttype {
            TokenType::Identifier { name } => Ok(Some(self.make_identifier_constant(name))),
            _ => unreachable!("Previous must be an Identifier token"),
        }
    }

    fn parse_stmt(&mut self) -> Result<(), CompilationError> {
        match self.parser.current_token().ttype {
            TokenType::Print => {
                self.parser.advance_token();
                self.parse_expression()?;
                self.parser.consume_token(TokenType::Semicolon)?;
                self.emit_op(vm::OpCode::Print);
            }
            TokenType::LeftBrace => {
                self.parser.advance_token();
                self.parse_block()?;
            }
            TokenType::If => {
                self.parser.advance_token();
                self.parse_if()?;
            }
            TokenType::While => {
                self.parser.advance_token();
                self.parse_while()?;
            }
            TokenType::For => {
                self.parser.advance_token();
                self.parse_for()?;
            }
            // Expr statement
            _ => {
                self.parse_expression()?;
                self.parser.consume_token(TokenType::Semicolon)?;
            }
        }

        Ok(())
    }

    fn parse_block(&mut self) -> Result<(), CompilationError> {
        self.current_lexical_stack().begin_scope();

        while !self.parser.check_token(&TokenType::RightBrace)
            && !self.parser.check_token(&TokenType::Eof)
        {
            // TODO: How does error sync work with the lexical stack state?
            // Probably need to catch the error and unwind the scope before throwing it further up the
            // the chain
            self.parse_declaration()?;
        }

        self.parser.consume_token(TokenType::RightBrace)?;
        let num_popped = self.current_lexical_stack().end_scope();

        for _ in 0..num_popped {
            self.emit_op(vm::OpCode::Pop)
        }

        Ok(())
    }

    fn parse_if(&mut self) -> Result<(), CompilationError> {
        // Condition
        self.parser.consume_token(TokenType::LeftParen)?;
        self.parse_expression()?;
        self.parser.consume_token(TokenType::RightParen)?;

        // Emit condition jump op + pop the condition off the stack
        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_op(OpCode::Pop);
        self.parse_stmt()?; // Then Condition

        let else_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(then_jump);
        self.emit_op(OpCode::Pop);
        if self.parser.match_token(TokenType::Else) {
            self.parse_stmt()?; // Else Condition
        }

        self.patch_jump(else_jump);

        Ok(())
    }

    fn parse_while(&mut self) -> Result<(), CompilationError> {
        let loop_start = self.current_chunk().unwrap().code.len();

        // Condition
        self.parser.consume_token(TokenType::LeftParen)?;
        self.parse_expression()?;
        self.parser.consume_token(TokenType::RightParen)?;

        let break_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_op(OpCode::Pop);
        self.parse_stmt()?;
        self.emit_loop(loop_start);

        self.patch_jump(break_jump);
        self.emit_op(OpCode::Pop);

        Ok(())
    }

    fn parse_for(&mut self) -> Result<(), CompilationError> {
        self.current_lexical_stack().begin_scope();
        self.parser.consume_token(TokenType::LeftParen)?;

        // Initializer
        if self.parser.match_token(TokenType::Var) {
            self.parse_variable_decl()?;
        } else {
            self.parse_expression()?;
            self.parser.consume_token(TokenType::Semicolon)?;
        }

        // Condition
        let loop_start = self.current_chunk().unwrap().code.len();
        let mut exit_jump: Option<usize> = None;
        if !self.parser.match_token(TokenType::Semicolon) {
            self.parse_expression()?;
            self.parser.consume_token(TokenType::Semicolon)?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_op(OpCode::Pop);
        }
        let body_jump = self.emit_jump(OpCode::Jump);

        // increment step
        let mut increment_start: Option<usize> = None;
        if !(self.parser.match_token(TokenType::RightParen)) {
            increment_start = Some(self.current_chunk().unwrap().code.len());

            self.parse_expression()?;
            self.emit_op(OpCode::Pop);

            self.emit_loop(loop_start);
            self.parser.consume_token(TokenType::RightParen)?;
        }

        // Body
        self.patch_jump(body_jump);
        self.parse_stmt()?;

        // If there isn't an increment condition we can just skip up to the loop start
        if let Some(incr_start) = increment_start {
            self.emit_loop(incr_start)
        } else {
            self.emit_loop(loop_start)
        }

        // End of loop
        exit_jump.map(|exit_jump| self.patch_jump(exit_jump));
        self.emit_op(OpCode::Pop);
        self.current_lexical_stack().end_scope();

        Ok(())
    }

    fn parse_expression(&mut self) -> Result<(), CompilationError> {
        self.parse_expr_w_precedence(Precedence::Assignment)
    }

    /// Parse an expression of a specific precedence level or higher.
    fn parse_expr_w_precedence(&mut self, prec: Precedence) -> Result<(), CompilationError> {
        self.parser.advance_token();

        // Check for prefix rule
        let previous_ttype = self.parser.previous_token().ttype;
        let prefix_handler = self.lookup_prefix_handler(&previous_ttype).expect(
            format!(
                "There should be a prefix rule for token type {:?}",
                &previous_ttype
            )
            .as_ref(),
        );

        let can_assign = prec <= Precedence::Assignment;
        prefix_handler(self, can_assign)?;

        // Check for infix rules while the observed prec is the same or higher
        while prec <= Precedence::from_token(&self.parser.current_token().ttype) {
            self.parser.advance_token();
            let previous_ttype = self.parser.previous_token().ttype;
            let infix_handler = self
                .lookup_infix_handler(previous_ttype)
                .expect("Infix rule should exist if the precedence is higher for this token.");

            infix_handler(self)?;
        }

        Ok(())
    }

    /* Pratt-Style Parsing table for Expressions */

    fn lookup_prefix_handler(
        &self,
        ttype: &TokenType,
    ) -> Option<fn(&mut Compiler<'vm>, bool) -> Result<(), CompilationError>> {
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

    fn lookup_infix_handler(
        &self,
        ttype: TokenType,
    ) -> Option<fn(&mut Compiler<'vm>) -> Result<(), CompilationError>> {
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
            TokenType::And => Some(Compiler::handle_parse_and),
            TokenType::Or => Some(Compiler::handle_parse_or),
            _ => None,
        }
    }

    fn handle_parse_number(&mut self, _can_assign: bool) -> Result<(), CompilationError> {
        if let Token {
            ttype: TokenType::Number(value),
            ..
        } = self.parser.previous_token()
        {
            self.emit_constant(vm::LoxValue::Number(value));
            Ok(())
        } else {
            unreachable!("handler should be called when number literal token is previous.")
        }
    }

    fn handle_parse_string(&mut self, _can_assign: bool) -> Result<(), CompilationError> {
        if let Token {
            ttype: TokenType::String { literal },
            ..
        } = self.parser.previous_token()
        {
            let heap_ref = self.heap.allocate(ObjectType::String(literal.clone()));
            self.emit_constant(vm::LoxValue::Object(heap_ref));

            Ok(())
        } else {
            unreachable!("handler should be called when string token is previous.")
        }
    }

    fn handle_parse_variable(&mut self, can_assign: bool) -> Result<(), CompilationError> {
        if let Token {
            ttype: TokenType::Identifier { name },
            ..
        } = self.parser.previous_token()
        {
            let get_op: OpCode;
            let set_op: OpCode;
            let arg: u8;
            if let Some(local_var_slot) = self.current_lexical_stack().resolve_local(&name)? {
                get_op = OpCode::GetLocal;
                set_op = OpCode::SetLocal;
                arg = local_var_slot;
            } else {
                get_op = OpCode::GetGlobal;
                set_op = OpCode::SetGlobal;
                arg = self.make_identifier_constant(name);
            }

            // Depending on the next token we will change get vs set
            if can_assign && self.parser.match_token(TokenType::Equal) {
                self.parse_expression()?;
                self.emit_op_and_arg(set_op, arg);
            } else {
                self.emit_op_and_arg(get_op, arg);
            }

            return Ok(());
        }

        unreachable!("Function must always be called when an identifier has been consumed.")
    }

    fn handle_parse_literal(&mut self, _can_assign: bool) -> Result<(), CompilationError> {
        match self.parser.previous_token().ttype {
            TokenType::Nil => self.emit_op(vm::OpCode::Nil),
            TokenType::True => self.emit_op(vm::OpCode::True),
            TokenType::False => self.emit_op(vm::OpCode::False),
            _ => {
                return Err(CompilationError::UnexpectedToken {
                    token: self.parser.previous_token(),
                    msg: "Unexpected token for literal".to_string(),
                });
            }
        }

        Ok(())
    }

    fn handle_parse_grouping(&mut self, _can_assign: bool) -> Result<(), CompilationError> {
        // Reset the precedence since the parens ensure unambiguous parsing
        self.parse_expression()?;
        self.parser.consume_token(TokenType::RightParen)?;

        Ok(())
    }

    fn handle_parse_unary(&mut self, _can_assign: bool) -> Result<(), CompilationError> {
        let operator_type = self.parser.previous_token().ttype;

        // Parse the unary operand
        self.parse_expr_w_precedence(Precedence::Unary)?;

        match operator_type {
            TokenType::Minus => self.emit_op(vm::OpCode::Negate),
            TokenType::Bang => self.emit_op(vm::OpCode::Not),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn handle_parse_and(&mut self) -> Result<(), CompilationError> {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_op(OpCode::Pop);
        self.parse_expr_w_precedence(Precedence::And)?;

        self.patch_jump(end_jump);

        Ok(())
    }

    fn handle_parse_or(&mut self) -> Result<(), CompilationError> {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(else_jump);
        self.emit_op(OpCode::Pop);
        self.parse_expr_w_precedence(Precedence::Or)?;

        self.patch_jump(end_jump);

        Ok(())
    }

    fn handle_parse_binary(&mut self) -> Result<(), CompilationError> {
        let op_type = self.parser.previous_token().ttype;
        let op_precedence = Precedence::from_token(&op_type);

        self.parse_expr_w_precedence(op_precedence.successor())?;

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

        Ok(())
    }

    /* Helper functions */

    fn make_identifier_constant(&mut self, name: String) -> u8 {
        let heap_ref = self.heap.allocate(ObjectType::String(name.clone()));
        let lox_value = vm::LoxValue::Object(heap_ref);

        let chunk = self.current_chunk().unwrap();
        let name_index = chunk.add_constant(lox_value);

        name_index
    }

    /* Byte code emitters */

    fn emit_byte(&mut self, byte: u8) {
        let line = self.parser.previous_token().line;
        self.current_chunk()
            .expect("Chunk should be defined to write ops")
            .write(byte, line);
    }

    fn emit_op(&mut self, op: vm::OpCode) {
        let line = self.parser.previous_token().line;
        self.current_chunk()
            .expect("Chunk should be defined to write ops")
            .write_op(op, line);
    }

    fn emit_op_and_arg(&mut self, op: vm::OpCode, arg: u8) {
        let line = self.parser.previous_token().line;
        let chunk = self.current_chunk().expect("Chunk should be defined.");
        chunk.write_op(op, line);
        chunk.write(arg, line);
    }

    fn emit_constant(&mut self, value: vm::LoxValue) {
        let chunk = self.current_chunk().expect("Chunk should be defined.");
        let index = chunk.add_constant(value);
        self.emit_op_and_arg(vm::OpCode::Constant, index)
    }

    fn emit_jump(&mut self, jump_op: OpCode) -> usize {
        self.emit_op(jump_op);
        // Placeholder value since we don't know where to jump to yet
        self.emit_byte(0);
        self.emit_byte(0);

        // Return index to the start of the two blank bytes so we can write in the location later
        self.current_chunk().unwrap().code.len() - 2
    }

    /// Patches a jump with the current chunk location
    fn patch_jump(&mut self, patch_location: usize) {
        let chunk = self.current_chunk().unwrap();

        // ...
        // JumpIfFalse
        // Byte1 <--- patch_location
        // Byte2
        // ...                          --
        // ... other code ...            | desired offset
        // ...                           |
        // <--- chunk.code.len()        --
        let target_location = chunk.code.len() - patch_location - 2;
        if target_location > u16::MAX.into() {
            panic!("Too large a code block to jump, currently unsupported")
        }

        chunk.overwrite(patch_location, (target_location >> 8) as u8);
        chunk.overwrite(patch_location + 1, target_location as u8);
    }

    /// Emit a loop back to the given start location
    fn emit_loop(&mut self, jump_location: usize) {
        self.emit_op(OpCode::Loop);

        let current_location = self.current_chunk().unwrap().code.len() - jump_location + 2;

        self.emit_byte((current_location >> 8) as u8);
        self.emit_byte(current_location as u8);
    }
}

pub fn compile(source: String, heap: &mut GcHeap) -> Result<Chunk, CompilationReport> {
    // In the book, he uses global C pointers which are accessible by all the functions. Here we'll
    // just need to keep those structs in scope in the compile function
    let scanner = Scanner::new(&source);
    let parser = Parser::new(scanner.collect());

    let mut compiler = Compiler::new(heap, parser);

    return compiler.compile();
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
        let chunk = compile("".to_string(), &mut heap);

        assert!(chunk.unwrap().code.len() == 0, "Chunk should be empty")
    }
}
