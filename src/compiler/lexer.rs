use std::fmt::{Display, Formatter};
use std::str::Chars;
use crate::util;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenPos {
    pub line: i32,
    pub column: i32,
}

impl TokenPos {
    pub fn new(line: i32, column: i32) -> TokenPos {
        TokenPos { line, column }
    }

    pub fn begin() -> TokenPos {
        TokenPos::new(1, 1)
    }
}

impl Display for TokenPos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {} column {}]", self.line, self.column)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenType {
    None,

    ParenthesisLeft, ParenthesisRight,
    BracketLeft, BracketRight,
    SquareBracketLeft, SquareBracketRight,
    Dot, Comma, Semicolon, Colon,

    Assign, Equal,
    Not, NotEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Plus, PlusAssign,
    Minus, MinusAssign,
    Multiply, MultiplyAssign,
    Divide, DivideAssign,

    And, ShortcircuitAnd,
    Or, ShortcircuitOr,

    Identifier,
    Int, Float,
    String,

    // Keywords
    Builtin,
    Inline,
    Template,
    Function,
    Module, Include, Import,

    // EOF
    Eof,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    token_type: TokenType,
    source: String,
    start: TokenPos, end: TokenPos,
}

impl<'a> Token {
    pub fn new(token_type: TokenType, source: String, start: TokenPos, end: TokenPos) -> Token {
        Token {
            token_type, source,
            start, end
        }
    }

    pub fn empty() -> Token {
        Token {
            token_type: TokenType::None,
            source: String::from(""),
            start: TokenPos::begin(), end: TokenPos::begin(),
        }
    }

    pub fn token_type(&self) -> TokenType { self.token_type }
    pub fn source(&self) -> &str { &self.source }
    pub fn start(&self) -> &TokenPos { &self.start }
    pub fn end(&self) -> &TokenPos { &self.end }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.token_type {
            TokenType::None => f.write_str("None"),
            TokenType::Eof => f.write_str("Eof"),
            TokenType::String => write!(f, "`\"{}\"`", self.source),
            _ => write!(f, "`{}`", self.source),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexerError {
    UnexpectedEof,

    UnexpectedCharacter(TokenPos, char),
    ExpectedCharacter {
        pos: TokenPos,
        expected: char,
        got: char
    },
    ExpectedString {
        pos: TokenPos,
        expected: String,
    },
    UnterminatedString {
        pos: TokenPos,
    },

    OtherError(TokenPos, String)
}

impl LexerError {
    pub fn get_pos(&self) -> Option<TokenPos> {
        match self {
            LexerError::UnexpectedCharacter(pos, _) => Some(*pos),
            LexerError::ExpectedCharacter { pos, .. } => Some(*pos),
            LexerError::ExpectedString { pos, .. } => Some(*pos),
            LexerError::UnterminatedString { pos, .. } => Some(*pos),
            LexerError::OtherError(pos, _) => Some(*pos),
            _ => None,
        }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedEof => write!(f, "Unexpected EOF"),
            LexerError::UnexpectedCharacter(pos, c) => write!(f, "{} Unexpected character '{}'", pos, c),
            LexerError::ExpectedCharacter { pos, expected, got } => write!(f, "{} Unexpected character; expected '{}', got '{}'", pos, expected, got),
            LexerError::ExpectedString { pos, expected } => write!(f, "{} Unexpected character; expected \"{}\"", pos, expected),
            LexerError::UnterminatedString { pos } => write!(f, "{} Unterminated string", pos),
            LexerError::OtherError(pos, message) => write!(f, "{} {}", pos, message),
        }
    }
}

type LexerResult<T> = Result<T, LexerError>;

pub struct Lexer<'source> {
    input: &'source str,

    chars: Chars<'source>,
    peek_1: Option<char>,
    peek_2: Option<char>,

    start_index: usize,
    current_index: usize,

    start_pos: TokenPos,
    current_pos: TokenPos,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Lexer<'source> {
        Lexer {
            input: source,

            chars: source.chars(),
            peek_1: None,
            peek_2: None,

            start_index: 0,
            current_index: 0,

            start_pos: TokenPos::begin(),
            current_pos: TokenPos::begin(),
        }
    }

    pub fn scan_token(&mut self) -> LexerResult<Token> {
        loop {
            self.skip_whitespace();
            self.start_index = self.current_index;
            self.start_pos = self.current_pos;

            if self.is_eof() {
                return Ok(self.make_token(TokenType::Eof));
            }

            let c = self.consume()?;

            return match c {
                '(' => Ok(self.make_token(TokenType::ParenthesisLeft)),
                ')' => Ok(self.make_token(TokenType::ParenthesisRight)),
                '{' => Ok(self.make_token(TokenType::BracketLeft)),
                '}' => Ok(self.make_token(TokenType::BracketRight)),
                '[' => Ok(self.make_token(TokenType::SquareBracketLeft)),
                ']' => Ok(self.make_token(TokenType::SquareBracketRight)),
                '.' => Ok(self.make_token(TokenType::Dot)),
                ',' => Ok(self.make_token(TokenType::Comma)),
                ';' => Ok(self.make_token(TokenType::Semicolon)),
                ':' => Ok(self.make_token(TokenType::Colon)),

                // Comparison operators are unused by density functions
                // Only `=` is used
                '=' => Ok(if self.expect('=')? { self.make_token(TokenType::Equal) } else {
                    self.make_token(TokenType::Assign)
                }),
                '!' => Ok(if self.expect('=')? { self.make_token(TokenType::NotEqual) } else {
                    self.make_token(TokenType::Not)
                }),
                '>' => Ok(if self.expect('=')? { self.make_token(TokenType::GreaterEqual) } else {
                    self.make_token(TokenType::Greater)
                }),
                '<' => Ok(if self.expect('=')? { self.make_token(TokenType::LessEqual) } else {
                    self.make_token(TokenType::Less)
                }),
                '&' => Ok(if self.expect('&')? { self.make_token(TokenType::ShortcircuitAnd) } else {
                    self.make_token(TokenType::And)
                }),
                '|' => Ok(if self.expect('|')? { self.make_token(TokenType::ShortcircuitOr) } else {
                    self.make_token(TokenType::Or)
                }),

                // Only `+` and `*` are used, maybe `-`
                '+' => Ok(if self.expect('=')? { self.make_token(TokenType::PlusAssign) } else {
                    self.make_token(TokenType::Plus)
                }),
                '-' => {
                    let next = self.peek()?;

                    if next >= '0' && next <= '9' {
                        self.scan_number()
                    } else {
                        Ok(if self.expect('=')? { self.make_token(TokenType::MinusAssign) } else {
                            self.make_token(TokenType::Minus)
                        })
                    }
                },
                '*' => Ok(if self.expect('=')? { self.make_token(TokenType::MultiplyAssign) } else {
                    self.make_token(TokenType::Multiply)
                }),
                '/' => Ok(if self.expect('=')? { self.make_token(TokenType::DivideAssign) } else if self.expect('/')? {
                    self.skip_line();
                    continue;
                } // Skip line comments
                else if self.expect('*')? {
                    /* Skip block comments */
                    self.skip_block_comment();
                    continue;
                } else {
                    self.make_token(TokenType::Divide)
                }),

                '"' => self.scan_string(),
                '0'..='9' => self.scan_number(),
                c if util::is_alphabetic(c) => self.scan_identifier(),

                _ => Err(LexerError::UnexpectedCharacter(self.current_pos, c)),
            };
        }
    }

    fn scan_string(&mut self) -> LexerResult<Token> {
        while let Ok(c) = self.peek() {
            if c == '"' {
                break;
            }

            let _ = self.consume();
        }

        if self.is_eof() {
            Err(LexerError::UnterminatedString { pos: self.start_pos })
        } else {
            let _ = self.consume(); // the trailing '"'

            // Don't add leading and trailing '"' characters to token
            Ok(Token {
                token_type: TokenType::String,
                source: self.input[(self.start_index + 1)..(self.current_index - 1)].to_owned(),
                start: self.start_pos, end: self.current_pos,
            })
        }
    }

    fn scan_number(&mut self) -> LexerResult<Token> {
        while let Ok('0'..='9') = self.peek() {
            let _ = self.consume();
        }

        let mut floating_point = false;

        if let Ok('.') = self.peek() {
            if let Ok('0'..='9') = self.peek_next() {
                let _ = self.consume();
                floating_point = true;

                while let Ok('0'..='9') = self.peek() {
                    let _ = self.consume();
                }
            }
        }

        Ok(self.make_token(if floating_point { TokenType::Float } else { TokenType::Int }))
    }

    fn scan_identifier(&mut self) -> LexerResult<Token> {
        while let Ok(c) = self.peek() {
            if !util::is_alphanumeric(c) {
                break;
            }

            let _ = self.consume();
        }

        let name = &self.input[self.start_index..self.current_index];
        let mut chars = name.chars();

        let token_type = match chars.next().expect("Internal compiler error: Empty identifier") {
            'b' => Lexer::check_keyword(name, 1, "builtin", TokenType::Builtin),
            'f' => Lexer::check_keyword(name, 1, "function", TokenType::Function),
            'i' => {
                if let Some(c) = chars.next() {
                    match c {
                        'n' => {
                            if let Some(c) = chars.next() {
                                match c {
                                    'c' => Lexer::check_keyword(name, 3, "include", TokenType::Include),
                                    'l' => Lexer::check_keyword(name, 3, "inline", TokenType::Inline),
                                    _ => TokenType::Identifier,
                                }
                            } else { TokenType::Identifier }
                        },
                        'm' => Lexer::check_keyword(name, 2, "import", TokenType::Import),
                        _ => TokenType::Identifier,
                    }
                } else { TokenType::Identifier }
            },
            'm' => Lexer::check_keyword(name, 1, "module", TokenType::Module),
            't' => Lexer::check_keyword(name, 1, "template", TokenType::Template),
            _ => TokenType::Identifier,
        };

        Ok(Token { source: name.to_owned(), token_type, start: self.start_pos, end: self.current_pos })
    }

    fn check_keyword(name: &str, start: usize, keyword: &'static str, token_type: TokenType) -> TokenType {
        if name[start..] == keyword[start..] {
            token_type
        } else {
            TokenType::Identifier
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            source: self.input[self.start_index..self.current_index].to_owned(),

            start: self.start_pos, end: self.current_pos,
        }
    }

    fn consume(&mut self) -> LexerResult<char> {
        (if let Some(c) = self.peek_1.take() {
            self.peek_1 = self.peek_2.take();
            Ok(c)
        } else {
            self.chars.next().ok_or(LexerError::UnexpectedEof)
        }).map(|c| {
            self.current_index += 1;

            if c == '\n' {
                self.current_pos.line += 1;
                self.current_pos.column = 1;
            } else {
                self.current_pos.column += 1;
            }

            c
        })
    }

    fn peek(&mut self) -> LexerResult<char> {
        if let Some(c) = self.peek_1 {
            Ok(c)
        } else if let Some(c) = self.chars.next() {
            self.peek_1 = Some(c);
            Ok(c)
        } else {
            Err(LexerError::UnexpectedEof)
        }
    }

    fn peek_next(&mut self) -> LexerResult<char> {
        if let Some(c) = self.peek_2 {
            Ok(c)
        } else if self.peek_1.is_some() {
            if let Some(c) = self.chars.next() {
                self.peek_2 = Some(c);
                Ok(c)
            } else {
                Err(LexerError::UnexpectedEof)
            }
        } else if let Some(peek_1) = self.chars.next() {
            self.peek_1 = Some(peek_1);

            if let Some(c) = self.chars.next() {
                self.peek_2 = Some(c);
                Ok(c)
            } else {
                Err(LexerError::UnexpectedEof)
            }
        } else {
            Err(LexerError::UnexpectedEof)
        }
    }

    fn expect(&mut self, expected: char) -> LexerResult<bool> {
        let actual = self.peek()?;

        if expected == actual {
            self.consume().map(|_| true)
        } else {
            Ok(false)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Ok(c) = self.peek() {
            if !c.is_whitespace() {
                return;
            }

            let _ = self.consume();
        }
    }

    fn skip_line(&mut self) {
        while let Ok(c) = self.peek() {
            let _ = self.consume();

            if c == '\n' {
                return;
            }
        }
    }

    fn skip_block_comment(&mut self) {
        let mut comment_count = 1;

        while let Ok(c) = self.consume() {
            if c == '/' {
                if let Ok('*') = self.consume() {
                    comment_count += 1;
                }
            } else if c == '*' {
                if let Ok('/') = self.consume() {
                    comment_count -= 1;
                }
            }

            if comment_count <= 0 {
                return;
            }
        }
    }

    fn is_eof(&self) -> bool {
        self.current_index >= self.input.len()
    }
}
