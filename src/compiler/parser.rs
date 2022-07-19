#[allow(unused)]
use crate::debug;
use crate::compiler::lexer::{Lexer, LexerError, Token, TokenPos, TokenType};
use ExprPrecedence::{/* Assignment, Or, And, Equality, Comparison, */ Term, Factor, Unary, Call, Primary};
use crate::compiler::ast::{Expr, Stmt};

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
#[repr(u8)]
pub enum ExprPrecedence {
    None = 0,
    // Assignment,  // =
    // Or,          // ||
    // And,         // &&
    // Equality,    // == !=
    // Comparison,  // < > <= >=
    Term,        // + -
    Factor,      // * /
    Unary,       // ! -
    Call,        // . ()
    Primary,
}

impl ExprPrecedence {
    pub fn next_higher_precedence(&self) -> ExprPrecedence {
        match self {
            ExprPrecedence::None => Term, // Assignment,
            // Assignment => Or,
            // Or => And,
            // And => Equality,
            // Equality => Comparison,
            // Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => Primary,
        }
    }
}

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    previous: Token, current: Token,

    had_error: bool,
    panic_mode: bool,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'_>) -> Parser<'_> {
        Parser {
            lexer,
            previous: Token::empty(), current: Token::empty(),
            had_error: false, panic_mode: false,
        }
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    // Statement parsing

    pub fn parse(&mut self) -> Vec<Stmt> {
        let statements = Vec::new();

        while !self.is_eof() {
            // TODO
        }

        statements
    }

    // Expression parsing

    fn parse_expression(&mut self) -> Expr {
        self.consume();

        self.parse_term()
    }

    fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();

        while self.matches_any(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous.clone();
            let right = self.parse_factor();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_factor(&mut self) -> Expr {
        let mut expr = self.parse_unary();

        while self.matches(TokenType::Multiply) {
            let operator = self.previous.clone();
            let right = self.parse_unary();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_unary(&mut self) -> Expr {
        if self.matches(TokenType::Minus) {
            let operator = self.previous.clone();
            let right = self.parse_unary();

            return Expr::UnaryOperator { operator, expr: Box::new(right) };
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Expr {
        if self.matches(TokenType::Float) {
            let number = self.previous.clone();
            let parsed: Result<f32, _> = number.source().parse();

            return match parsed {
                Ok(number) => Expr::ConstantFloat(number),
                Err(err) => {
                    self.error(&format!("Failed to parse float literal: {}", err), true);
                    Expr::Error
                },
            }
        } else if self.matches(TokenType::Int) {
            let number = self.previous.clone();
            let parsed: Result<i32, _> = number.source().parse();

            return match parsed {
                Ok(number) => Expr::ConstantInt(number),
                Err(err) => {
                    self.error(&format!("Failed to parse int literal: {}", err), true);
                    Expr::Error
                },
            }
        } else if self.matches(TokenType::ParenthesisLeft) {
            let expr = self.parse_expression();
            self.expect(TokenType::ParenthesisRight, "Expected ')' after expression.");

            return Expr::Group(Box::new(expr));
        }

        self.consume();
        self.error("Expected expression.", true);

        Expr::Error
    }

    fn consume(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current); // self.previous = self.current; self.current gets replaced below

        loop {
            match self.lexer.scan_token() {
                Ok(token) => {
                    self.current = token;
                    break;
                },
                Err(LexerError::UnexpectedEof) => {
                    self.current = Token::new(TokenType::Eof, String::new(), TokenPos::begin(), TokenPos::begin());
                    break
                },

                Err(err) => self.error_from_parser(&err),
            }
        }
    }

    fn expect(&mut self, token_type: TokenType, message: &str) {
        if self.current.token_type() == token_type {
            self.consume();
            return;
        }

        self.error_at_current(message, true);
    }

    #[inline]
    fn expect_statement_end(&mut self) {
        self.expect(TokenType::Semicolon, "Expected ';' after statement.");
    }

    fn matches(&mut self, token_type: TokenType) -> bool { // Should be called "match", but that's a keyword
        if !self.check(token_type) {
            return false;
        }

        self.consume();
        true
    }

    fn matches_any(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.consume();
                return true;
            }
        }

        false
    }

    #[inline]
    fn check(&mut self, token_type: TokenType) -> bool {
        self.current.token_type() == token_type
    }

    fn is_eof(&self) -> bool {
        self.current.token_type() == TokenType::Eof
    }

    // Error handling

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.token_type() != TokenType::Eof {
            if self.previous.token_type() == TokenType::Semicolon {
                return;
            }

            match self.current.token_type() {
                TokenType::Fn => return,
                _ => {},
            };

            self.consume();
        }
    }

    // Error handling

    fn error_at_current(&mut self, message: &str, panic: bool) {
        Self::error_at(&mut self.had_error, &mut self.panic_mode, Some(&self.current), message, panic);
    }

    fn error(&mut self, message: &str, panic: bool) {
        Self::error_at(&mut self.had_error, &mut self.panic_mode, Some(&self.previous), message, panic);
    }

    fn error_from_parser(&mut self, error: &LexerError) {
        if let Some(pos) = error.get_pos() {
            eprint!("{} ", pos);
        }

        Self::error_at(&mut self.had_error, &mut self.panic_mode, None, &error.to_string(), true);
    }

    fn error_at(had_error: &mut bool, panic_mode: &mut bool, token: Option<&Token>, message: &str, panic: bool) {
        if *panic_mode {
            return;
        } else if panic {
            *panic_mode = true;
        }

        if let Some(token) = token {
            eprint!("{} Error", token.start());

            if token.token_type() == TokenType::Eof {
                eprint!(" at EOF: ");
            } else {
                eprint!(" at '{}': ", token.source());
            }
        } else {
            eprint!("Error: ");
        }

        eprintln!("{}", message);
        *had_error = true;
    }
}
