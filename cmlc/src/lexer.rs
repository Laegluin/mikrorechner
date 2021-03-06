use crate::ast::{Ident, Lit};
use crate::span::{Index, Offset, Span, Spanned};
use std::fmt::{self, Display};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(Ident),
    Keyword(Keyword),
    Lit(Lit),
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Equal,
    BangEqual,
    DoubleEqual,
    Plus,
    Minus,
    Slash,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Pipe,
    DoublePipe,
    Comma,
    Semicolon,
    Bang,
    Dot,
    Colon,
    DoubleColon,
    Underscore,
    Amp,
    DoubleAmp,
    Arrow,
    Star,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Type,
    Ret,
    Let,
    Mut,
    If,
    While,
    Else,
    As,
}

pub struct StrStream<'s> {
    input: &'s str,
    next_char_idx: usize,
    next_byte_idx: Index,
    start_byte_idx: Index,
}

impl StrStream<'_> {
    pub fn new(input: &str) -> StrStream {
        StrStream {
            input,
            next_char_idx: 0,
            next_byte_idx: Index(1),
            start_byte_idx: Index(1),
        }
    }

    pub fn next_if_eq(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn next_if(&mut self, is_expected: impl FnOnce(char) -> bool) -> Option<char> {
        match self.peek() {
            Some(c) if is_expected(c) => {
                self.next();
                Some(c)
            }
            _ => None,
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let next = self.input[self.next_char_idx..].chars().next();

        if let Some(c) = next {
            self.next_char_idx += 1;
            self.next_byte_idx += Offset::from_char_utf8(c);
        }

        next
    }

    /// Sets the start of the span to the next char.
    pub fn set_span_start(&mut self) {
        self.start_byte_idx = self.next_byte_idx;
    }

    pub fn peek(&self) -> Option<char> {
        self.input[self.next_char_idx..].chars().next()
    }

    /// Returns the span starting at the current span start and ending at the
    /// current char.
    ///
    /// Use `advance_start` to set the current span start.
    pub fn span(&self) -> Span {
        Span::new(self.start_byte_idx, self.next_byte_idx)
    }

    /// Returns the span for the last char, or an empty span if there is no last char.
    pub fn last_char_span(&self) -> Span {
        Span::new(
            self.next_byte_idx - self.last_char_offset(),
            self.next_byte_idx,
        )
    }

    fn last_char_offset(&self) -> Offset {
        if self.next_char_idx == 0 {
            return Offset(0);
        }

        // the previous char must always exist since the index wouldn't have advanced otherwise
        let current_char = self.input[self.next_char_idx - 1..].chars().next().unwrap();
        Offset::from_char_utf8(current_char)
    }
}

impl<'s> From<&'s str> for StrStream<'s> {
    fn from(string: &str) -> StrStream {
        StrStream::new(string)
    }
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedToken,
    OverflowingIntLiteral,
    MissingStringEndDelimiter,
}

impl Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LexError::*;

        match *self {
            UnexpectedToken => write!(f, "unexpected token"),
            OverflowingIntLiteral => write!(f, "overflowing integer literal"),
            MissingStringEndDelimiter => write!(f, "missing terminating `\"` for string literal"),
        }
    }
}

pub fn lex<'a>(stream: impl Into<StrStream<'a>>) -> Result<Vec<Spanned<Token>>, Spanned<LexError>> {
    let mut stream = stream.into();
    let mut char_buf = String::new();
    let mut tokens = Vec::new();

    loop {
        stream.set_span_start();

        let c = match stream.next() {
            Some(c) => c,
            None => return Ok(tokens),
        };

        match c {
            c if c.is_whitespace() => (),
            '"' => {
                while let Some(c) = stream.next_if(|c| c != '"') {
                    if c == '\\' {
                        let maybe_char = stream.next();

                        char_buf.push(maybe_char.ok_or_else(|| {
                            Spanned::new(
                                LexError::MissingStringEndDelimiter,
                                stream.last_char_span(),
                            )
                        })?);
                    } else {
                        char_buf.push(c);
                    }
                }

                // consume the last `"`
                if stream.next().is_none() {
                    return Err(Spanned::new(
                        LexError::MissingStringEndDelimiter,
                        stream.last_char_span(),
                    ));
                }

                tokens.push(Spanned::new(
                    Token::Lit(Lit::Str(Rc::from(char_buf.as_str()))),
                    stream.span(),
                ));

                char_buf.clear();
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                char_buf.push(c);

                while let Some(c) = stream.next_if(is_ident_char) {
                    char_buf.push(c);
                }

                if let Some(token) = reserved_idents(&char_buf) {
                    tokens.push(Spanned::new(token, stream.span()));
                } else {
                    tokens.push(Spanned::new(
                        Token::Ident(Ident::new(&char_buf)),
                        stream.span(),
                    ));
                }

                char_buf.clear();
            }
            '0'..='9' => {
                char_buf.push(c);

                while let Some(c) = stream.next_if(|c| c.is_ascii_digit()) {
                    char_buf.push(c);
                }

                tokens.push(Spanned::new(
                    Token::Lit(Lit::Int(char_buf.parse().map_err(|_| {
                        Spanned::new(LexError::OverflowingIntLiteral, stream.span())
                    })?)),
                    stream.span(),
                ));

                char_buf.clear();
            }
            '+' => tokens.push(Spanned::new(Token::Plus, stream.span())),
            '-' => {
                if stream.next_if_eq('>') {
                    tokens.push(Spanned::new(Token::Arrow, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Minus, stream.span()))
                }
            }
            '*' => tokens.push(Spanned::new(Token::Star, stream.span())),
            '/' => {
                if stream.next_if_eq('/') {
                    // consume all chars until we find a `\n`, `\r` or `\r\n`
                    while let Some(c) = stream.next() {
                        match c {
                            '\n' => break,
                            '\r' => {
                                stream.next_if_eq('\n');
                                break;
                            }
                            _ => continue,
                        }
                    }
                } else {
                    tokens.push(Spanned::new(Token::Slash, stream.span()))
                }
            }
            '=' => {
                if stream.next_if_eq('=') {
                    tokens.push(Spanned::new(Token::DoubleEqual, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Equal, stream.span()))
                }
            }
            '<' => {
                if stream.next_if_eq('=') {
                    tokens.push(Spanned::new(Token::LessEqual, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Less, stream.span()))
                }
            }
            '>' => {
                if stream.next_if_eq('=') {
                    tokens.push(Spanned::new(Token::Greater, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::GreaterEqual, stream.span()))
                }
            }
            '{' => tokens.push(Spanned::new(Token::OpenBrace, stream.span())),
            '}' => tokens.push(Spanned::new(Token::CloseBrace, stream.span())),
            '(' => tokens.push(Spanned::new(Token::OpenParen, stream.span())),
            ')' => tokens.push(Spanned::new(Token::CloseParen, stream.span())),
            '[' => tokens.push(Spanned::new(Token::OpenBracket, stream.span())),
            ']' => tokens.push(Spanned::new(Token::CloseBracket, stream.span())),
            '|' => {
                if stream.next_if_eq('|') {
                    tokens.push(Spanned::new(Token::DoublePipe, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Pipe, stream.span()))
                }
            }
            ',' => tokens.push(Spanned::new(Token::Comma, stream.span())),
            ';' => tokens.push(Spanned::new(Token::Semicolon, stream.span())),
            '!' => {
                if stream.next_if_eq('=') {
                    tokens.push(Spanned::new(Token::BangEqual, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Bang, stream.span()))
                }
            }
            '.' => tokens.push(Spanned::new(Token::Dot, stream.span())),
            ':' => {
                if stream.next_if_eq(':') {
                    tokens.push(Spanned::new(Token::DoubleColon, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Colon, stream.span()))
                }
            }
            '&' => {
                if stream.next_if_eq('&') {
                    tokens.push(Spanned::new(Token::DoubleAmp, stream.span()))
                } else {
                    tokens.push(Spanned::new(Token::Amp, stream.span()))
                }
            }
            _ => return Err(Spanned::new(LexError::UnexpectedToken, stream.span())),
        }
    }
}

fn is_ident_char(c: char) -> bool {
    let a = true;
    if true && a {}
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
        _ => false,
    }
}

fn reserved_idents(string: &str) -> Option<Token> {
    match string {
        "_" => Some(Token::Underscore),
        "true" => Some(Token::Lit(Lit::Bool(true))),
        "false" => Some(Token::Lit(Lit::Bool(false))),
        "fn" => Some(Token::Keyword(Keyword::Fn)),
        "type" => Some(Token::Keyword(Keyword::Type)),
        "ret" => Some(Token::Keyword(Keyword::Ret)),
        "let" => Some(Token::Keyword(Keyword::Let)),
        "mut" => Some(Token::Keyword(Keyword::Mut)),
        "if" => Some(Token::Keyword(Keyword::If)),
        "while" => Some(Token::Keyword(Keyword::While)),
        "else" => Some(Token::Keyword(Keyword::Else)),
        "as" => Some(Token::Keyword(Keyword::As)),
        _ => None,
    }
}
