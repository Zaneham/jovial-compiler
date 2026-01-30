//! JOVIAL J73 Parser
//!
//! Recursive descent parser for JOVIAL J73 (MIL-STD-1589C).

use crate::ast::*;
use crate::error::{CompileError, Result, Span};
use crate::lexer::{tokenize, Keyword, SpannedToken, Token};

/// Parser state
pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let tokens = tokenize(source);
        Self { tokens, pos: 0 }
    }

    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program> {
        self.skip_newlines();

        // Expect START
        self.expect_keyword(Keyword::Start)?;
        let start_span = self.prev_span();

        // Program name
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        let mut declarations = Vec::new();
        let mut statements = Vec::new();

        // Parse declarations and statements until TERM
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::Term) || self.at_end() {
                break;
            }

            if self.is_declaration_start() {
                declarations.push(self.parse_declaration()?);
            } else {
                statements.push(self.parse_statement()?);
            }
        }

        self.expect_keyword(Keyword::Term)?;
        let end_span = self.prev_span();

        Ok(Program {
            name,
            declarations,
            statements,
            span: start_span.merge(end_span),
        })
    }

    // =========================================================================
    // Token Management
    // =========================================================================

    fn current(&self) -> Option<&SpannedToken> {
        self.tokens.get(self.pos)
    }

    fn current_token(&self) -> Option<&Token> {
        self.current().map(|t| &t.token)
    }

    fn current_span(&self) -> Span {
        self.current().map(|t| t.span).unwrap_or(Span::new(0, 0))
    }

    fn prev_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::new(0, 0)
        }
    }

    fn advance(&mut self) -> Option<&SpannedToken> {
        if self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn skip_newlines(&mut self) {
        while matches!(self.current_token(), Some(Token::Newline | Token::Comment(_))) {
            self.advance();
        }
    }

    fn check(&self, expected: &Token) -> bool {
        self.current_token() == Some(expected)
    }

    fn check_keyword(&self, keyword: Keyword) -> bool {
        match self.current_token() {
            Some(Token::Identifier(name)) => Keyword::from_str(name) == Some(keyword),
            _ => false,
        }
    }

    fn expect(&mut self, expected: Token) -> Result<()> {
        self.skip_newlines();
        if self.check(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(CompileError::parse(
                self.current_span(),
                format!("Expected {:?}", expected),
            ))
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<()> {
        self.skip_newlines();
        if self.check_keyword(keyword) {
            self.advance();
            Ok(())
        } else {
            Err(CompileError::parse(
                self.current_span(),
                format!("Expected keyword {:?}", keyword),
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        self.skip_newlines();
        match self.current_token().cloned() {
            Some(Token::Identifier(name)) if !name.as_str().is_keyword() => {
                self.advance();
                Ok(name)
            }
            Some(Token::Identifier(name)) => {
                // Allow keywords as identifiers in some contexts
                self.advance();
                Ok(name)
            }
            _ => Err(CompileError::parse(
                self.current_span(),
                "Expected identifier",
            )),
        }
    }

    fn is_declaration_start(&self) -> bool {
        matches!(
            self.current_token().and_then(|t| t.as_keyword()),
            Some(
                Keyword::Define
                    | Keyword::Type
                    | Keyword::Item
                    | Keyword::Table
                    | Keyword::Proc
                    | Keyword::Block
                    | Keyword::Compool
            )
        )
    }

    // =========================================================================
    // Declaration Parsing
    // =========================================================================

    fn parse_declaration(&mut self) -> Result<Declaration> {
        self.skip_newlines();

        match self.current_token().and_then(|t| t.as_keyword()) {
            Some(Keyword::Define) => self.parse_define(),
            Some(Keyword::Type) => self.parse_type_decl(),
            Some(Keyword::Item) => self.parse_item_decl(),
            Some(Keyword::Table) => self.parse_table_decl(),
            Some(Keyword::Proc) => self.parse_proc_decl(),
            Some(Keyword::Block) => self.parse_block_decl(),
            Some(Keyword::Compool) => self.parse_compool_decl(),
            _ => Err(CompileError::parse(
                self.current_span(),
                "Expected declaration",
            )),
        }
    }

    fn parse_define(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Define)?;
        let name = self.expect_identifier()?;
        self.expect(Token::Eq)?;
        let value = self.parse_expression()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Define {
            name,
            value,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_type_decl(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Type)?;
        let name = self.expect_identifier()?;
        let type_spec = self.parse_type_spec()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Type {
            name,
            type_spec,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_item_decl(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Item)?;
        let name = self.expect_identifier()?;

        // Modifiers
        let mut is_static = false;
        let mut is_constant = false;
        let mut is_parallel = false;
        let mut like_source = None;
        let mut overlay_target = None;
        let mut pos_target = None;

        while self.check_keyword(Keyword::Static)
            || self.check_keyword(Keyword::Constant)
            || self.check_keyword(Keyword::Parallel)
        {
            if self.check_keyword(Keyword::Static) {
                self.advance();
                is_static = true;
            } else if self.check_keyword(Keyword::Constant) {
                self.advance();
                is_constant = true;
            } else if self.check_keyword(Keyword::Parallel) {
                self.advance();
                is_parallel = true;
            }
        }

        // Check for OVERLAY modifier - shares memory with target
        if self.check_keyword(Keyword::Overlay) {
            self.advance();
            let target_name = self.expect_identifier()?;
            overlay_target = Some(target_name);
        }

        // Check for POS modifier - positioned at bit offset into target
        if self.check_keyword(Keyword::Pos) {
            self.advance();
            self.expect(Token::LParen)?;
            let target_name = self.expect_identifier()?;
            self.expect(Token::Comma)?;
            let offset = if let Some(Token::Integer(n)) = self.current_token().cloned() {
                self.advance();
                n as u32
            } else {
                return Err(CompileError::parse(
                    self.current_span(),
                    "Expected bit offset integer",
                ));
            };
            self.expect(Token::RParen)?;
            pos_target = Some((target_name, offset));
        }

        // Check for LIKE modifier - copies type from another item
        let type_spec = if self.check_keyword(Keyword::Like) {
            self.advance();
            let source_name = self.expect_identifier()?;
            like_source = Some(source_name.clone());
            // Create a type reference that will be resolved later
            TypeSpec::type_ref(source_name, self.prev_span())
        } else {
            self.parse_type_spec()?
        };

        // Optional initial value
        let initial_value = if self.check(&Token::Eq) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        Ok(Declaration::Item {
            name,
            type_spec,
            is_static,
            is_constant,
            is_parallel,
            initial_value,
            like_source,
            overlay_target,
            pos_target,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_table_decl(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Table)?;
        let name = self.expect_identifier()?;

        // Dimensions
        let mut dimensions = Vec::new();
        if self.check(&Token::LParen) {
            self.advance();
            while !self.check(&Token::RParen) {
                let lower = self.parse_expression()?;
                self.expect(Token::Colon)?;
                let upper = self.parse_expression()?;
                dimensions.push((lower, upper));
                if self.check(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::RParen)?;
        }

        // Modifiers
        let mut is_parallel = false;
        let mut wordsize = None;

        if self.check_keyword(Keyword::Parallel) {
            self.advance();
            is_parallel = true;
        }
        if self.check_keyword(Keyword::WordSize) {
            self.advance();
            if let Some(Token::Integer(n)) = self.current_token().cloned() {
                self.advance();
                wordsize = Some(n as u32);
            }
        }

        self.expect(Token::Semicolon)?;

        // Table body
        self.expect_keyword(Keyword::Begin)?;
        let mut entries = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }
            if self.check_keyword(Keyword::Item) {
                entries.push(self.parse_item_decl()?);
            } else {
                break;
            }
        }
        self.expect_keyword(Keyword::End)?;

        Ok(Declaration::Table {
            name,
            dimensions,
            entries,
            is_parallel,
            wordsize,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_proc_decl(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Proc)?;
        let name = self.expect_identifier()?;

        // Parameters
        let mut params = Vec::new();
        let mut output_params = Vec::new();

        if self.check(&Token::LParen) {
            self.advance();
            let mut parsing_outputs = false;

            while !self.check(&Token::RParen) {
                self.skip_newlines();
                if self.check(&Token::Colon) {
                    self.advance();
                    parsing_outputs = true;
                    continue;
                }

                let param_name = self.expect_identifier()?;
                let param_span = self.prev_span();
                let param = Declaration::Item {
                    name: param_name,
                    type_spec: TypeSpec::void(param_span),
                    is_static: false,
                    is_constant: false,
                    is_parallel: false,
                    initial_value: None,
                    like_source: None,
                    overlay_target: None,
                    pos_target: None,
                    span: param_span,
                };

                if parsing_outputs {
                    output_params.push(param);
                } else {
                    params.push(param);
                }

                if self.check(&Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::RParen)?;
        }

        // Modifiers
        let is_recursive = false;
        let mut is_reentrant = false;
        let mut is_inline = false;

        loop {
            if self.check_keyword(Keyword::Rent) {
                self.advance();
                is_reentrant = true;
            } else if self.check_keyword(Keyword::Inline) {
                self.advance();
                is_inline = true;
            } else {
                break;
            }
        }

        // Return type
        let return_type = if !self.check(&Token::Semicolon) {
            Some(self.parse_type_spec()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;

        // Body
        self.expect_keyword(Keyword::Begin)?;

        let mut locals = Vec::new();
        let mut body = Vec::new();

        // Local declarations
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::Item)
                || self.check_keyword(Keyword::Table)
                || self.check_keyword(Keyword::Define)
                || self.check_keyword(Keyword::Type)
            {
                locals.push(self.parse_declaration()?);
            } else {
                break;
            }
        }

        // Statements
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) || self.at_end() {
                break;
            }
            body.push(self.parse_statement()?);
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Declaration::Proc {
            name,
            params,
            output_params,
            return_type,
            is_recursive,
            is_reentrant,
            is_inline,
            locals,
            body,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_block_decl(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Block)?;

        let name = if let Some(Token::Identifier(_)) = self.current_token() {
            Some(self.expect_identifier()?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;
        self.expect_keyword(Keyword::Begin)?;

        let mut declarations = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }
            declarations.push(self.parse_declaration()?);
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Declaration::Block {
            name,
            declarations,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_compool_decl(&mut self) -> Result<Declaration> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Compool)?;
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;
        self.expect_keyword(Keyword::Begin)?;

        let mut declarations = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }
            declarations.push(self.parse_declaration()?);
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Declaration::Compool {
            name,
            declarations,
            span: start_span.merge(self.prev_span()),
        })
    }

    // =========================================================================
    // Type Parsing
    // =========================================================================

    fn parse_type_spec(&mut self) -> Result<TypeSpec> {
        self.skip_newlines();
        let start_span = self.current_span();

        // STATUS type
        if self.check_keyword(Keyword::Status) {
            self.advance();
            let mut status_values = Vec::new();
            if self.check(&Token::LParen) {
                self.advance();
                loop {
                    self.skip_newlines();
                    if self.check(&Token::RParen) {
                        break;
                    }
                    if self.check_keyword(Keyword::V) {
                        self.advance();
                        self.expect(Token::LParen)?;
                        let name = self.expect_identifier()?;
                        status_values.push(name);
                        self.expect(Token::RParen)?;
                    }
                    if self.check(&Token::Comma) {
                        self.advance();
                    }
                }
                self.expect(Token::RParen)?;
            }
            return Ok(TypeSpec {
                base: BaseType::Status,
                size: None,
                scale: None,
                status_values,
                element_type: None,
                referenced_type: None,
                span: start_span.merge(self.prev_span()),
            });
        }

        // POINTER type
        if self.check_keyword(Keyword::Pointer) {
            self.advance();
            let element_type = if self.check(&Token::LParen) {
                self.advance();
                let t = self.parse_type_spec()?;
                self.expect(Token::RParen)?;
                Some(Box::new(t))
            } else {
                None
            };
            return Ok(TypeSpec {
                base: BaseType::Pointer,
                size: None,
                scale: None,
                status_values: Vec::new(),
                element_type,
                referenced_type: None,
                span: start_span.merge(self.prev_span()),
            });
        }

        // Simple types: S, U, F, B, C, H, A or full names
        let base = match self.current_token().and_then(|t| t.as_keyword()) {
            Some(Keyword::S | Keyword::Signed) => {
                self.advance();
                BaseType::Signed
            }
            Some(Keyword::U | Keyword::Unsigned) => {
                self.advance();
                BaseType::Unsigned
            }
            Some(Keyword::F | Keyword::Float) => {
                self.advance();
                BaseType::Float
            }
            Some(Keyword::B | Keyword::Bit) => {
                self.advance();
                BaseType::Bit
            }
            Some(Keyword::C) => {
                self.advance();
                BaseType::Character
            }
            Some(Keyword::H) => {
                self.advance();
                BaseType::Hollerith
            }
            Some(Keyword::A | Keyword::Fixed) => {
                self.advance();
                BaseType::Fixed
            }
            _ => {
                // Type reference
                let name = self.expect_identifier()?;
                return Ok(TypeSpec::type_ref(name, start_span.merge(self.prev_span())));
            }
        };

        // Size
        let size = if let Some(Token::Integer(n)) = self.current_token().cloned() {
            self.advance();
            Some(n as u32)
        } else {
            None
        };

        // Scale factor for fixed-point (A type with D n)
        let scale = if base == BaseType::Fixed && self.check_keyword(Keyword::D) {
            self.advance();
            if let Some(Token::Integer(n)) = self.current_token().cloned() {
                self.advance();
                Some(n as i32)
            } else {
                None
            }
        } else {
            None
        };

        Ok(TypeSpec {
            base,
            size,
            scale,
            status_values: Vec::new(),
            element_type: None,
            referenced_type: None,
            span: start_span.merge(self.prev_span()),
        })
    }

    // =========================================================================
    // Statement Parsing
    // =========================================================================

    fn parse_statement(&mut self) -> Result<Statement> {
        self.skip_newlines();
        let start_span = self.current_span();

        // Empty statement
        if self.check(&Token::Semicolon) {
            self.advance();
            return Ok(Statement::Null { span: start_span });
        }

        // BEGIN ... END block
        if self.check_keyword(Keyword::Begin) {
            return self.parse_block_stmt();
        }

        // IF statement
        if self.check_keyword(Keyword::If) {
            return self.parse_if_stmt();
        }

        // CASE statement
        if self.check_keyword(Keyword::Case) {
            return self.parse_case_stmt();
        }

        // WHILE statement
        if self.check_keyword(Keyword::While) {
            return self.parse_while_stmt();
        }

        // FOR statement
        if self.check_keyword(Keyword::For) {
            return self.parse_for_stmt();
        }

        // GOTO
        if self.check_keyword(Keyword::Goto) {
            self.advance();
            let label = self.expect_identifier()?;
            self.expect(Token::Semicolon)?;
            return Ok(Statement::Goto {
                label,
                span: start_span.merge(self.prev_span()),
            });
        }

        // RETURN
        if self.check_keyword(Keyword::Return) {
            self.advance();
            let value = if !self.check(&Token::Semicolon) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.expect(Token::Semicolon)?;
            return Ok(Statement::Return {
                value,
                span: start_span.merge(self.prev_span()),
            });
        }

        // EXIT
        if self.check_keyword(Keyword::Exit) {
            self.advance();
            self.expect(Token::Semicolon)?;
            return Ok(Statement::Exit {
                span: start_span.merge(self.prev_span()),
            });
        }

        // ABORT
        if self.check_keyword(Keyword::Abort) {
            self.advance();
            self.expect(Token::Semicolon)?;
            return Ok(Statement::Abort {
                span: start_span.merge(self.prev_span()),
            });
        }

        // STOP
        if self.check_keyword(Keyword::Stop) {
            self.advance();
            self.expect(Token::Semicolon)?;
            return Ok(Statement::Stop {
                span: start_span.merge(self.prev_span()),
            });
        }

        // Label or assignment/call
        let expr = self.parse_expression()?;

        // Check for label (identifier followed by colon, not :=)
        if self.check(&Token::Colon) && !self.check(&Token::Assign) {
            if let Expr::Ident { name, .. } = expr {
                self.advance(); // consume colon
                return Ok(Statement::Label {
                    name,
                    span: start_span.merge(self.prev_span()),
                });
            }
        }

        // Assignment
        if self.check(&Token::Assign) {
            self.advance();
            let value = self.parse_expression()?;
            self.expect(Token::Semicolon)?;
            return Ok(Statement::Assign {
                target: expr,
                value,
                span: start_span.merge(self.prev_span()),
            });
        }

        // Must be a procedure call
        self.expect(Token::Semicolon)?;
        Ok(Statement::Call {
            expr,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_block_stmt(&mut self) -> Result<Statement> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Begin)?;

        let mut statements = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }
            statements.push(self.parse_statement()?);
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::Block {
            statements,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Statement> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expression()?;
        self.expect(Token::Semicolon)?;

        let mut then_body = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) || self.check_keyword(Keyword::Else) {
                break;
            }
            then_body.push(self.parse_statement()?);
        }

        let mut else_body = Vec::new();
        if self.check_keyword(Keyword::Else) {
            self.advance();
            loop {
                self.skip_newlines();
                if self.check_keyword(Keyword::End) {
                    break;
                }
                else_body.push(self.parse_statement()?);
            }
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::If {
            condition,
            then_body,
            else_body,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_case_stmt(&mut self) -> Result<Statement> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::Case)?;
        let selector = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        self.expect_keyword(Keyword::Begin)?;

        let mut branches = Vec::new();
        let mut default = Vec::new();

        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }

            if self.check_keyword(Keyword::Default) {
                self.advance();
                self.expect(Token::Colon)?;
                loop {
                    self.skip_newlines();
                    if self.check_keyword(Keyword::End)
                        || self.check_keyword(Keyword::V)
                        || self.check_keyword(Keyword::Default)
                    {
                        break;
                    }
                    default.push(self.parse_statement()?);
                }
            } else if self.check_keyword(Keyword::V) {
                let branch_span = self.current_span();
                let mut values = Vec::new();

                while self.check_keyword(Keyword::V) {
                    self.advance();
                    self.expect(Token::LParen)?;
                    let name = self.expect_identifier()?;
                    let val_span = self.prev_span();
                    values.push(Expr::Status {
                        value: name,
                        span: val_span,
                    });
                    self.expect(Token::RParen)?;
                    if self.check(&Token::Comma) {
                        self.advance();
                    }
                }

                self.expect(Token::Colon)?;

                let mut body = Vec::new();
                let mut fallthru = false;

                loop {
                    self.skip_newlines();
                    if self.check_keyword(Keyword::End)
                        || self.check_keyword(Keyword::V)
                        || self.check_keyword(Keyword::Default)
                    {
                        break;
                    }
                    if self.check_keyword(Keyword::Fallthru) {
                        self.advance();
                        self.expect(Token::Semicolon)?;
                        fallthru = true;
                        break;
                    }
                    body.push(self.parse_statement()?);
                }

                branches.push(CaseBranch {
                    values,
                    body,
                    fallthru,
                    span: branch_span.merge(self.prev_span()),
                });
            } else {
                break;
            }
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::Case {
            selector,
            branches,
            default,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Statement> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expression()?;
        self.expect(Token::Semicolon)?;

        let mut body = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }
            body.push(self.parse_statement()?);
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::While {
            condition,
            body,
            span: start_span.merge(self.prev_span()),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Statement> {
        let start_span = self.current_span();
        self.expect_keyword(Keyword::For)?;
        let variable = self.expect_identifier()?;
        self.expect(Token::Colon)?;
        let start = self.parse_expression()?;
        self.expect_keyword(Keyword::By)?;
        let step = self.parse_expression()?;
        self.expect_keyword(Keyword::While)?;
        let end = self.parse_expression()?;
        self.expect(Token::Semicolon)?;

        let mut body = Vec::new();
        loop {
            self.skip_newlines();
            if self.check_keyword(Keyword::End) {
                break;
            }
            body.push(self.parse_statement()?);
        }

        self.expect_keyword(Keyword::End)?;

        Ok(Statement::For {
            variable,
            start,
            end,
            step: Some(step),
            body,
            span: start_span.merge(self.prev_span()),
        })
    }

    // =========================================================================
    // Expression Parsing (Pratt-style precedence climbing)
    // =========================================================================

    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_or_expr()
    }

    fn parse_or_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_xor_expr()?;
        while self.check_keyword(Keyword::Or) {
            self.advance();
            let right = self.parse_xor_expr()?;
            let span = left.span().merge(right.span());
            left = Expr::Binary {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn parse_xor_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_and_expr()?;
        while self.check_keyword(Keyword::Xor) {
            self.advance();
            let right = self.parse_and_expr()?;
            let span = left.span().merge(right.span());
            left = Expr::Binary {
                op: BinaryOp::Xor,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_not_expr()?;
        while self.check_keyword(Keyword::And) {
            self.advance();
            let right = self.parse_not_expr()?;
            let span = left.span().merge(right.span());
            left = Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }
        Ok(left)
    }

    fn parse_not_expr(&mut self) -> Result<Expr> {
        if self.check_keyword(Keyword::Not) {
            let start_span = self.current_span();
            self.advance();
            let operand = self.parse_not_expr()?;
            let span = start_span.merge(operand.span());
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                operand: Box::new(operand),
                span,
            });
        }
        self.parse_relational_expr()
    }

    fn parse_relational_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_additive_expr()?;

        let op = match self.current_token() {
            Some(Token::Eq) => Some(BinaryOp::Eq),
            Some(Token::Ne) => Some(BinaryOp::Ne),
            Some(Token::Lt) => Some(BinaryOp::Lt),
            Some(Token::Le) => Some(BinaryOp::Le),
            Some(Token::Gt) => Some(BinaryOp::Gt),
            Some(Token::Ge) => Some(BinaryOp::Ge),
            _ => None,
        };

        if let Some(op) = op {
            self.advance();
            let right = self.parse_additive_expr()?;
            let span = left.span().merge(right.span());
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_additive_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_multiplicative_expr()?;

        loop {
            let op = match self.current_token() {
                Some(Token::Plus) => Some(BinaryOp::Add),
                Some(Token::Minus) => Some(BinaryOp::Sub),
                _ => None,
            };

            if let Some(op) = op {
                self.advance();
                let right = self.parse_multiplicative_expr()?;
                let span = left.span().merge(right.span());
                left = Expr::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_power_expr()?;

        loop {
            let op = match self.current_token() {
                Some(Token::Star) => Some(BinaryOp::Mul),
                Some(Token::Slash) => Some(BinaryOp::Div),
                _ if self.check_keyword(Keyword::Mod) => Some(BinaryOp::Mod),
                _ => None,
            };

            if let Some(op) = op {
                self.advance();
                let right = self.parse_power_expr()?;
                let span = left.span().merge(right.span());
                left = Expr::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_power_expr(&mut self) -> Result<Expr> {
        let left = self.parse_unary_expr()?;

        if self.check(&Token::Power) {
            self.advance();
            let right = self.parse_power_expr()?; // Right-associative
            let span = left.span().merge(right.span());
            return Ok(Expr::Binary {
                op: BinaryOp::Power,
                left: Box::new(left),
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        let start_span = self.current_span();

        if self.check(&Token::Minus) {
            self.advance();
            let operand = self.parse_unary_expr()?;
            let span = start_span.merge(operand.span());
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
                span,
            });
        }

        if self.check(&Token::Plus) {
            self.advance();
            let operand = self.parse_unary_expr()?;
            let span = start_span.merge(operand.span());
            return Ok(Expr::Unary {
                op: UnaryOp::Pos,
                operand: Box::new(operand),
                span,
            });
        }

        if self.check(&Token::At) {
            self.advance();
            let operand = self.parse_unary_expr()?;
            let span = start_span.merge(operand.span());
            return Ok(Expr::Deref {
                operand: Box::new(operand),
                span,
            });
        }

        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr> {
        let mut left = self.parse_primary_expr()?;

        loop {
            // Call or index: name(args)
            if self.check(&Token::LParen) {
                self.advance();
                let mut args = Vec::new();
                let mut output_args = Vec::new();
                let mut parsing_outputs = false;

                loop {
                    self.skip_newlines();
                    if self.check(&Token::RParen) {
                        break;
                    }
                    if self.check(&Token::Colon) {
                        self.advance();
                        parsing_outputs = true;
                        continue;
                    }

                    let arg = self.parse_expression()?;
                    if parsing_outputs {
                        output_args.push(arg);
                    } else {
                        args.push(arg);
                    }

                    if self.check(&Token::Comma) {
                        self.advance();
                    }
                }

                self.expect(Token::RParen)?;
                let span = left.span().merge(self.prev_span());

                if let Expr::Ident { name, .. } = left {
                    left = Expr::Call {
                        name,
                        args,
                        output_args,
                        span,
                    };
                } else {
                    left = Expr::Index {
                        base: Box::new(left),
                        indices: args,
                        span,
                    };
                }
            }
            // Member access: .field
            else if self.check(&Token::Dot) {
                self.advance();
                let member = self.expect_identifier()?;
                let span = left.span().merge(self.prev_span());
                left = Expr::Member {
                    base: Box::new(left),
                    member,
                    span,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        self.skip_newlines();
        let start_span = self.current_span();

        // Integer literal
        if let Some(Token::Integer(n)) = self.current_token().cloned() {
            self.advance();
            return Ok(Expr::Integer {
                value: n,
                span: start_span,
            });
        }

        // Float literal
        if let Some(Token::Float(n)) = self.current_token().cloned() {
            self.advance();
            return Ok(Expr::Float {
                value: n,
                span: start_span,
            });
        }

        // String literal
        if let Some(Token::String(s)) = self.current_token().cloned() {
            self.advance();
            return Ok(Expr::String {
                value: s,
                span: start_span,
            });
        }

        // Status literal: V(NAME)
        if self.check_keyword(Keyword::V) {
            self.advance();
            self.expect(Token::LParen)?;
            let name = self.expect_identifier()?;
            self.expect(Token::RParen)?;
            return Ok(Expr::Status {
                value: name,
                span: start_span.merge(self.prev_span()),
            });
        }

        // Parenthesized expression
        if self.check(&Token::LParen) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect(Token::RParen)?;
            return Ok(expr);
        }

        // Identifier
        if let Some(Token::Identifier(name)) = self.current_token().cloned() {
            self.advance();
            return Ok(Expr::Ident {
                name,
                span: start_span,
            });
        }

        Err(CompileError::parse(start_span, "Expected expression"))
    }
}

// Helper trait
trait IsKeyword {
    fn is_keyword(&self) -> bool;
}

impl IsKeyword for str {
    fn is_keyword(&self) -> bool {
        Keyword::from_str(self).is_some()
    }
}

/// Parse source code into a program
pub fn parse(source: &str) -> Result<Program> {
    Parser::new(source).parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_program() {
        let source = "START TEST;\nTERM";
        let program = parse(source).unwrap();
        assert_eq!(program.name, "TEST");
    }

    #[test]
    fn test_proc() {
        let source = r#"
START TEST;
PROC ADD (A, B : RESULT);
BEGIN
    ITEM A S 16;
    ITEM B S 16;
    ITEM RESULT S 16;
    RESULT := A + B;
END
TERM
"#;
        let program = parse(source).unwrap();
        assert_eq!(program.declarations.len(), 1);
    }

    #[test]
    fn test_fixed_point_with_scale() {
        let source = r#"
START TEST;
ITEM RATE A 16 D 4;
TERM
"#;
        let program = parse(source).unwrap();
        assert_eq!(program.declarations.len(), 1);
        if let Declaration::Item { type_spec, .. } = &program.declarations[0] {
            assert_eq!(type_spec.base, BaseType::Fixed);
            assert_eq!(type_spec.size, Some(16));
            assert_eq!(type_spec.scale, Some(4));
        } else {
            panic!("Expected Item declaration");
        }
    }

    #[test]
    fn test_like_modifier() {
        let source = r#"
START TEST;
ITEM ORIGINAL S 32;
ITEM COPY LIKE ORIGINAL;
TERM
"#;
        let program = parse(source).unwrap();
        assert_eq!(program.declarations.len(), 2);
        if let Declaration::Item { like_source, .. } = &program.declarations[1] {
            assert_eq!(like_source.as_ref().unwrap(), "ORIGINAL");
        } else {
            panic!("Expected Item declaration with LIKE");
        }
    }

    #[test]
    fn test_overlay_modifier() {
        let source = r#"
START TEST;
ITEM BASE S 32;
ITEM ALIAS OVERLAY BASE S 32;
TERM
"#;
        let program = parse(source).unwrap();
        assert_eq!(program.declarations.len(), 2);
        if let Declaration::Item { overlay_target, .. } = &program.declarations[1] {
            assert_eq!(overlay_target.as_ref().unwrap(), "BASE");
        } else {
            panic!("Expected Item declaration with OVERLAY");
        }
    }

    #[test]
    fn test_pos_modifier() {
        let source = r#"
START TEST;
ITEM BASE S 32;
ITEM FIELD POS(BASE, 8) S 16;
TERM
"#;
        let program = parse(source).unwrap();
        assert_eq!(program.declarations.len(), 2);
        if let Declaration::Item { pos_target, .. } = &program.declarations[1] {
            let (target, offset) = pos_target.as_ref().unwrap();
            assert_eq!(target, "BASE");
            assert_eq!(*offset, 8);
        } else {
            panic!("Expected Item declaration with POS");
        }
    }
}
