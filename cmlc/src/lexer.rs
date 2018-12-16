use crate::span::Spanned;
use codespan::{ByteIndex, ByteOffset, ByteSpan};
use std::rc::Rc;

#[derive(Debug)]
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
    Amp,
    DoubleAmp,
    Arrow,
    Star,
}

#[derive(Debug)]
pub enum Keyword {
    Fn,
    Type,
    Ret,
    Let,
    Mut,
    If,
    Else,
}

#[derive(Debug)]
pub struct Ident(Rc<str>);

#[derive(Debug)]
pub enum Lit {
    Str(Rc<str>),
    Int(u32),
    Bool(bool),
}

pub struct StrStream<'s> {
    input: &'s str,
    next_char_idx: usize,
    next_byte_idx: ByteIndex,
    start_byte_idx: ByteIndex,
}

impl StrStream<'_> {
    pub fn new(input: &str) -> StrStream {
        StrStream {
            input,
            next_char_idx: 0,
            next_byte_idx: ByteIndex(0),
            start_byte_idx: ByteIndex(0),
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
            self.next_byte_idx += ByteOffset::from_char_utf8(c);
        }

        next
    }

    /// Sets the start of the span to the current char's position.
    pub fn advance_start(&mut self) {
        self.start_byte_idx = self.next_byte_idx - self.current_char_offset();
    }

    pub fn peek(&self) -> Option<char> {
        self.input[self.next_char_idx..].chars().next()
    }

    /// Returns the span starting at the current span start and ending at the
    /// next char's position (exclusive).
    ///
    /// Use `advance_start` to set the current span start.
    pub fn span(&self) -> ByteSpan {
        ByteSpan::new(self.start_byte_idx, self.next_byte_idx)
    }

    /// Returns the span for the current char, or an empty span if there is not current char.
    pub fn span_at_current(&self) -> ByteSpan {
        ByteSpan::new(
            self.next_byte_idx - self.current_char_offset(),
            self.next_byte_idx,
        )
    }

    fn current_char_offset(&self) -> ByteOffset {
        if self.next_char_idx == 0 {
            return ByteOffset(0);
        }

        // the previous char must always exist since the index wouldn't have advanced otherwise
        let current_char = self.input[self.next_char_idx - 1..].chars().next().unwrap();
        ByteOffset::from_char_utf8(current_char)
    }
}

impl<'s> From<&'s str> for StrStream<'s> {
    fn from(string: &str) -> StrStream {
        StrStream::new(string)
    }
}

#[derive(Debug)]
pub enum LexError {
    UnknownToken,
    OverflowingIntLiteral,
    MissingStringEndDelimiter,
}

pub fn lex<'a>(stream: impl Into<StrStream<'a>>) -> Result<Vec<Spanned<Token>>, Spanned<LexError>> {
    let mut stream = stream.into();
    let mut char_buf = String::new();
    let mut tokens = Vec::new();

    while let Some(c) = stream.next() {
        stream.advance_start();

        match c {
            c if c.is_whitespace() => (),
            '"' => {
                while let Some(c) = stream.next_if(|c| c != '"') {
                    if c == '\\' {
                        let maybe_char = stream.next();

                        char_buf.push(maybe_char.ok_or_else(|| {
                            Spanned::new(
                                LexError::MissingStringEndDelimiter,
                                stream.span_at_current(),
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
                        stream.span(),
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

                if let Some(token) = to_keyword_like(&char_buf) {
                    tokens.push(Spanned::new(token, stream.span()));
                } else {
                    tokens.push(Spanned::new(
                        Token::Ident(Ident(Rc::from(char_buf.as_str()))),
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
            '!' => tokens.push(Spanned::new(Token::Bang, stream.span())),
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
            _ => return Err(Spanned::new(LexError::UnknownToken, stream.span())),
        }
    }

    Ok(tokens)
}

fn is_ident_char(c: char) -> bool {
    let a = true;
    if true && a {}
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
        _ => false,
    }
}

fn to_keyword_like(string: &str) -> Option<Token> {
    match string {
        "true" => Some(Token::Lit(Lit::Bool(true))),
        "false" => Some(Token::Lit(Lit::Bool(false))),
        "fn" => Some(Token::Keyword(Keyword::Fn)),
        "type" => Some(Token::Keyword(Keyword::Type)),
        "ret" => Some(Token::Keyword(Keyword::Ret)),
        "let" => Some(Token::Keyword(Keyword::Let)),
        "mut" => Some(Token::Keyword(Keyword::Mut)),
        "if" => Some(Token::Keyword(Keyword::If)),
        "else" => Some(Token::Keyword(Keyword::Else)),
        _ => None,
    }
}
