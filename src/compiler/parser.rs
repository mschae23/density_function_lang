use std::path::{Path, PathBuf};
use lazy_static::lazy_static;
#[allow(unused)]
use crate::debug;
use crate::compiler::lexer::{Lexer, LexerError, Token, TokenPos, TokenType};
use crate::compiler::ast::{Expr, JsonElementType, Decl, TemplateExpr};

lazy_static! {
    static ref ALLOWED_TEMPLATE_NAME_TYPES: [TokenType; 16] = [
        TokenType::Identifier,
        TokenType::Plus, TokenType::Minus, TokenType::Multiply, TokenType::Divide,
        TokenType::Equal, TokenType::NotEqual,
        TokenType::Greater, TokenType::GreaterEqual,
        TokenType::Less, TokenType::LessEqual,
        TokenType::And, TokenType::ShortcircuitAnd,
        TokenType::Or, TokenType::ShortcircuitOr,
        TokenType::SquareBracketLeft, // [] operator
    ];
}

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    previous: Token, current: Token,

    path: PathBuf,

    had_error: bool,
    panic_mode: bool,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'_>, path: PathBuf) -> Parser<'_> {
        Parser {
            lexer,
            path,
            previous: Token::empty(), current: Token::empty(),
            had_error: false, panic_mode: false,
        }
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    // Declaration parsing

    pub fn parse(&mut self) -> Vec<Decl> {
        self.consume();

        let mut statements = Vec::new();

        while !self.is_eof() {
            statements.push(self.parse_declaration());
        }

        statements
    }

    fn parse_declaration(&mut self) -> Decl {
        if self.matches(TokenType::Export) {
            return self.parse_export_declaration();
        } else if self.matches(TokenType::Template) {
            return self.parse_template_declaration();
        } else if self.matches(TokenType::Module) {
            return self.parse_module_declaration();
        } else if self.matches(TokenType::Include) {
            return self.parse_include_declaration();
        } else if self.matches(TokenType::Import) {
            return self.parse_import_declaration();
        }

        let stmt = self.parse_statement();

        if self.panic_mode {
            self.synchronize();
        }

        stmt
    }

    fn parse_export_declaration(&mut self) -> Decl {
        self.expect(TokenType::Identifier, "Expected name after 'export'");
        let name = self.previous.clone();

        self.expect(TokenType::Assign, "Expected '=' after export name");

        let expr = self.parse_expression();
        self.expect_statement_end();

        Decl::Export { name, expr }
    }

    fn parse_template_declaration(&mut self) -> Decl {
        self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, "Expected name after 'template'");
        let name = self.previous.clone();

        if name.token_type() == TokenType::SquareBracketLeft {
            self.expect(TokenType::SquareBracketRight, "Expected ']' after '[' in template name");
        }

        self.expect(TokenType::ParenthesisLeft, "Expected '(' after template name");
        let mut arguments = vec![];
        let mut this = None;

        if self.matches(TokenType::This) {
            let name = self.previous.clone();
            this = Some(name);

            if !self.check(TokenType::ParenthesisRight) {
                self.expect(TokenType::Comma, "Expected ',' or ')' after 'this'");
            }
        }

        if !self.check(TokenType::ParenthesisRight) {
            self.expect(TokenType::Identifier, "Expected template parameter name after '('");
            arguments.push(self.previous.clone());

            while self.matches(TokenType::Comma) {
                self.expect(TokenType::Identifier, "Expected template parameter name after ','");
                let name = self.previous.clone();

                if arguments.iter().any(|arg| *arg.source() == *name.source()) {
                    self.error("Duplicate parameter", false);
                }

                arguments.push(name);
            }
        }

        self.expect(TokenType::ParenthesisRight, "Expected ')' after template parameters");
        self.expect(TokenType::BracketLeft, "Expected '{' after ')'");

        let expr = self.parse_block_template_expression();
        Decl::Template { name, this, args: arguments, expr }
    }

    fn parse_module_declaration(&mut self) -> Decl {
        self.expect(TokenType::Identifier, "Expected name after 'module'");
        let name = self.previous.clone();

        self.expect(TokenType::BracketLeft, "Expected '{' after module name");

        let mut statements = Vec::new();

        while !self.check(TokenType::BracketRight) && !self.check(TokenType::Eof) {
            statements.push(self.parse_declaration());
        }

        self.expect(TokenType::BracketRight, "Expected '}' after statements in module");
        Decl::Module { name, statements }
    }

    fn parse_include_declaration(&mut self) -> Decl {
        self.expect(TokenType::String, "Expected string after 'include'");
        let name = self.previous.clone();
        self.expect_statement_end();

        Decl::Include { path: name }
    }

    fn parse_import_declaration(&mut self) -> Decl {
        let mut path = vec![];
        let mut selector = None;

        self.expect(TokenType::Identifier, "Expected name after 'import'");
        let name = self.previous.clone();
        path.push(name);

        if !self.check(TokenType::Dot) {
            self.error_at_current("Expected '.' after name", true);
        }

        while self.matches(TokenType::Dot) {
            if self.matches(TokenType::Multiply) {
                break;
            } else if self.matches(TokenType::BracketLeft) {
                let mut names = vec![];

                self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, "Expected name after '{'");
                let name = self.previous.clone();
                names.push(name);

                while self.matches(TokenType::Comma) {
                    if self.check(TokenType::BracketRight) {
                        break;
                    }

                    self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, "Expected name after ','");
                    let name = self.previous.clone();
                    names.push(name);
                }

                self.expect(TokenType::BracketRight, "Expected '}' after names");
                selector = Some(names);
                break;
            }

            self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, "Expected name after '.'");
            let name = self.previous.clone();

            if !self.check(TokenType::Semicolon) && name.token_type() != TokenType::Identifier {
                self.error("Expected name after '.'", true);
            } else if self.check(TokenType::Semicolon) {
                selector = Some(vec![name]);
                break;
            }

            path.push(name);
        }

        self.expect_statement_end();
        Decl::Import { path, selector }
    }

    fn parse_statement(&mut self) -> Decl {
        self.consume();
        self.error("Expected statement", true);
        Decl::Error
    }

    // Template expressions

    fn parse_template_expression(&mut self) -> TemplateExpr {
        if self.matches(TokenType::If) {
            return self.parse_if_template_expression();
        }

        TemplateExpr::Simple(self.parse_expression())
    }

    fn parse_block_template_expression(&mut self) -> TemplateExpr {
        let mut expressions = vec![];

        let mut expr = self.parse_template_expression();

        while self.matches(TokenType::Semicolon) {
            expressions.push(expr);
            expr = self.parse_template_expression();
        }

        self.expect(TokenType::BracketRight, "Expected '}' after last expression in block");
        TemplateExpr::Block { expressions, last: Box::new(expr) }
    }

    fn parse_if_template_expression(&mut self) -> TemplateExpr {
        self.expect(TokenType::ParenthesisLeft, "Expected '(' after 'if'");
        let token = self.previous.clone();

        let condition = self.parse_expression();
        self.expect(TokenType::ParenthesisRight, "Expected ')' after 'if' condition");

        let then = if self.matches(TokenType::BracketLeft) {
            self.parse_block_template_expression()
        } else {
            self.parse_template_expression()
        };

        self.expect(TokenType::Else, "Expected 'else' after then clause of 'if' expression");

        let otherwise = if self.matches(TokenType::BracketLeft) {
            self.parse_block_template_expression()
        } else {
            self.parse_template_expression()
        };

        TemplateExpr::If { token, condition, then: Box::new(then), otherwise: Box::new(otherwise) }
    }

    // Expression parsing

    fn parse_expression(&mut self) -> Expr {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr {
        let mut expr = self.parse_and();

        while self.matches(TokenType::ShortcircuitOr) {
            let operator = self.previous.clone();
            let right = self.parse_and();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_and(&mut self) -> Expr {
        let mut expr = self.parse_equality();

        while self.matches(TokenType::ShortcircuitAnd) {
            let operator = self.previous.clone();
            let right = self.parse_equality();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_equality(&mut self) -> Expr {
        let mut expr = self.parse_comparison();

        while self.matches_any(&[TokenType::Equal, TokenType::NotEqual]) {
            let operator = self.previous.clone();
            let right = self.parse_comparison();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut expr = self.parse_term();

        while self.matches_any(&[TokenType::Less, TokenType::LessEqual, TokenType::Greater, TokenType::GreaterEqual]) {
            let operator = self.previous.clone();
            let right = self.parse_term();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
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

        while self.matches_any(&[TokenType::Multiply, TokenType::Divide]) {
            let operator = self.previous.clone();
            let right = self.parse_unary();

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_unary(&mut self) -> Expr {
        if self.matches_any(&[TokenType::Minus, TokenType::Not]) {
            let operator = self.previous.clone();
            let right = self.parse_unary();

            return Expr::UnaryOperator { operator, expr: Box::new(right) };
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Expr {
        let mut expr = self.parse_primary();

        loop {
            if self.matches(TokenType::ParenthesisLeft) {
                expr = self.finish_call(expr);
            } else if self.matches(TokenType::Dot) {
                self.expect(TokenType::Identifier, "Expected member name after '.'");
                let name = self.previous.clone();

                expr = Expr::Member { receiver: Box::new(expr), name }
            } else if self.matches(TokenType::SquareBracketLeft) {
                let token = self.previous.clone();

                let index = self.parse_expression();
                self.expect(TokenType::SquareBracketRight, "Expected ']' after index expression");

                expr = Expr::Index { receiver: Box::new(expr), operator: token, index: Box::new(index) }
            } else {
                break;
            }
        }

        expr
    }

    fn finish_call(&mut self, callee: Expr) -> Expr {
        let paren_left = self.previous.clone();

        let mut arguments = vec![];

        if !self.check(TokenType::ParenthesisRight) {
            arguments.push(self.parse_expression());

            while self.matches(TokenType::Comma) {
                if arguments.len() >= 255 {
                    self.error_at_current("Can't have more than 255 function call arguments", false);
                }

                arguments.push(self.parse_expression());
            }
        }

        self.expect(TokenType::ParenthesisRight, "Expected ')' after function call arguments");
        Expr::FunctionCall { callee: Box::new(callee), token: paren_left, args: arguments }
    }

    fn parse_primary(&mut self) -> Expr {
        if self.matches(TokenType::Float) {
            let number = self.previous.clone();
            let parsed: Result<f64, _> = number.source().parse();

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
        } else if self.matches(TokenType::True) {
            return Expr::ConstantBoolean(true)
        } else if self.matches(TokenType::False) {
            return Expr::ConstantBoolean(false)
        } else if self.matches(TokenType::Identifier) || self.matches(TokenType::This) {
            return Expr::Identifier(self.previous.clone())
        } else if self.matches(TokenType::String) {
            return Expr::ConstantString(self.previous.source().to_owned())
        } else if self.matches(TokenType::ParenthesisLeft) {
            let expr = self.parse_expression();
            self.expect(TokenType::ParenthesisRight, "Expected ')' after expression");

            return expr;
        } else if self.matches(TokenType::BracketLeft) {
            return self.parse_object_expression();
        } else if self.matches(TokenType::SquareBracketLeft) {
            return self.parse_array_expression();
        } else if self.matches(TokenType::Builtin) {
            return self.parse_builtin_call();
        }

        self.consume();
        self.error("Expected expression", true);

        Expr::Error
    }

    fn parse_object_expression(&mut self) -> Expr {
        let mut fields = vec![];

        if !self.check(TokenType::BracketRight) {
            self.expect(TokenType::String, "Expected string after '{'");
            let name = self.previous.clone();
            self.expect(TokenType::Colon, "Expected ':' after object field key");
            let expr = self.parse_expression();
            fields.push((name, expr));

            while self.matches(TokenType::Comma) {
                if self.check(TokenType::BracketRight) {
                    break;
                }

                self.expect(TokenType::String, "Expected string after '{'");
                let name = self.previous.clone();
                self.expect(TokenType::Colon, "Expected ':' after object field key");
                let expr = self.parse_expression();
                fields.push((name, expr));
            }
        }

        self.expect(TokenType::BracketRight, "Expected '}' after object fields");
        Expr::Object(fields)
    }

    fn parse_array_expression(&mut self) -> Expr {
        let mut elements = vec![];

        if !self.check(TokenType::SquareBracketRight) {
            let expr = self.parse_expression();
            elements.push(expr);

            while self.matches(TokenType::Comma) {
                if self.check(TokenType::SquareBracketRight) {
                    break;
                }

                let expr = self.parse_expression();
                elements.push(expr);
            }
        }

        self.expect(TokenType::SquareBracketRight, "Expected ']' after array elements");
        Expr::Array(elements)
    }

    fn parse_builtin_call(&mut self) -> Expr {
        self.expect(TokenType::Dot, "Expected '.' after 'builtin'");
        self.expect(TokenType::Identifier, "Expected name after '.'");
        let name = self.previous.clone();

        if name.source() == "type" {
            return self.parse_builtin_type();
        }

        self.expect(TokenType::ParenthesisLeft, "Expected '(' after built-in function name");

        let mut arguments = vec![];

        if !self.check(TokenType::ParenthesisRight) {
            arguments.push(self.parse_expression());

            while self.matches(TokenType::Comma) {
                if arguments.len() >= 255 {
                    self.error_at_current("Can't have more than 255 function call arguments", false);
                }

                arguments.push(self.parse_expression());
            }
        }

        self.expect(TokenType::ParenthesisRight, "Expected ')' after built-in function call arguments");
        Expr::BuiltinFunctionCall { name, args: arguments }
    }

    fn parse_builtin_type(&mut self) -> Expr {
        self.expect(TokenType::Dot, "Expected '.' after 'type'");
        self.expect(TokenType::Identifier, "Expected name after '.'");
        let name = self.previous.clone();

        match name.source() {
            "float" => Expr::BuiltinType(JsonElementType::Float),
            "int" => Expr::BuiltinType(JsonElementType::Int),
            "boolean" => Expr::BuiltinType(JsonElementType::Boolean),
            "string" => Expr::BuiltinType(JsonElementType::String),
            "object" => Expr::BuiltinType(JsonElementType::Object),
            "array" => Expr::BuiltinType(JsonElementType::Array),
            "module" => Expr::BuiltinType(JsonElementType::Module),
            "template" => Expr::BuiltinType(JsonElementType::Template),
            "type" => Expr::BuiltinType(JsonElementType::Type),
            _ => {
                self.error("Unknown type", true);
                Expr::Error
            },
        }
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

    fn expect_any(&mut self, token_types: &[TokenType], message: &str) {
        for token_type in token_types {
            if self.current.token_type() == *token_type {
                self.consume();
                return;
            }
        }

        self.error_at_current(message, true);
    }

    #[inline]
    fn expect_statement_end(&mut self) {
        self.expect(TokenType::Semicolon, "Expected ';' after statement");
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
                TokenType::Export => return,
                TokenType::Template => return,
                TokenType::Module => return,
                _ => {},
            };

            self.consume();
        }
    }

    // Error handling

    fn error_at_current(&mut self, message: &str, panic: bool) {
        Self::error_at(&mut self.had_error, &mut self.panic_mode, &self.path, Some(&self.current), message, panic);
    }

    fn error(&mut self, message: &str, panic: bool) {
        Self::error_at(&mut self.had_error, &mut self.panic_mode, &self.path, Some(&self.previous), message, panic);
    }

    fn error_from_parser(&mut self, error: &LexerError) {
        if let Some(pos) = error.get_pos() {
            eprint!("[{}:{}:{}] ", self.path.to_string_lossy(), pos.line, pos.column);
        }

        Self::error_at(&mut self.had_error, &mut self.panic_mode, &self.path, None, &error.to_string(), true);
    }

    fn error_at(had_error: &mut bool, panic_mode: &mut bool, path: &Path, token: Option<&Token>, message: &str, panic: bool) {
        if *panic_mode {
            return;
        } else if panic {
            *panic_mode = true;
        }

        if let Some(token) = token {
            eprint!("[{}:{}:{}] Error", path.to_string_lossy(), token.start().line, token.start().column);

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
