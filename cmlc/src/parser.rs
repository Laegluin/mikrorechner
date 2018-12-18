use crate::ast::*;
use crate::lexer::Token;
use crate::span::{Index, Span, Spanned};
use std::cell::Cell;

#[derive(Debug, Default)]
pub struct ParseError {
    msg: Option<String>,
    expected: Vec<String>,
    found: Option<String>,
}

impl ParseError {
    fn new() -> ParseError {
        ParseError::default()
    }

    fn msg(mut self, msg: impl Into<String>) -> ParseError {
        self.msg = Some(msg.into());
        self
    }

    fn expected(mut self, expected: impl Into<String>) -> ParseError {
        self.expected.push(expected.into());
        self
    }

    fn set_expected(mut self, expected: impl Into<String>) -> ParseError {
        self.expected.clear();
        self.expected.push(expected.into());
        self
    }

    fn found(mut self, found: impl Into<String>) -> ParseError {
        self.found = Some(found.into());
        self
    }
}

struct TokenStream<'t> {
    tokens: &'t [Spanned<Token>],
    consumed: Cell<usize>,
}

impl<'t> TokenStream<'t> {
    fn new(tokens: &[Spanned<Token>]) -> TokenStream<'_> {
        TokenStream {
            tokens,
            consumed: Cell::new(0),
        }
    }

    fn next(&self) -> Option<Token> {
        let consumed = self.consumed.get();

        if consumed < self.tokens.len() {
            self.consumed.set(consumed + 1);

            // tokens are cheap to clone
            Some(self.tokens[consumed].value.clone())
        } else {
            None
        }
    }

    fn peek(&self) -> Option<Token> {
        let consumed = self.consumed.get();

        if consumed < self.tokens.len() {
            Some(self.tokens[consumed].value.clone())
        } else {
            None
        }
    }

    /// Returns the span for the next token, or an empty span starting at the end of the
    /// current token if there is no next token.
    fn next_token_span(&self) -> Span {
        let consumed = self.consumed.get();

        if consumed < self.tokens.len() {
            self.tokens[consumed].span
        } else if consumed > 0 && (consumed - 1) < self.tokens.len() {
            let current_span = self.tokens[consumed - 1].span;
            Span::new(current_span.end(), current_span.end())
        } else {
            Span::new(Index(0), Index(0))
        }
    }

    fn start_span(&self) -> SpanStart<'_, 't> {
        SpanStart {
            start: self.consumed.get(),
            stream: &self,
        }
    }
}

struct SpanStart<'s, 't> {
    start: usize,
    stream: &'s TokenStream<'t>,
}

impl SpanStart<'_, '_> {
    fn end(self) -> Span {
        self.stream.tokens[self.start]
            .span
            .to(self.stream.tokens[self.stream.consumed.get()].span)
    }
}

pub fn parse(tokens: impl AsRef<[Spanned<Token>]>) -> Result<Ast, Spanned<ParseError>> {
    let tokens = TokenStream::new(tokens.as_ref());
    unimplemented!()
}

fn lit(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span = tokens.next_token_span();

    match tokens.peek() {
        Some(Token::Lit(lit)) => Ok(Spanned::new(Expr::Lit(lit), span)),
        Some(_) => Err(Spanned::new(
            ParseError::new()
                .msg("unexpected token")
                .expected("literal"),
            span,
        )),
        None => Err(Spanned::new(
            ParseError::new()
                .msg("unexpected end of file")
                .expected("literal"),
            span,
        )),
    }
}

fn var(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    ident(tokens)
        .map(|spanned| spanned.map(|ident| Expr::Var(ident)))
        .map_err(|spanned| spanned.map(|err| err.set_expected("variable")))
}

fn ident(tokens: &TokenStream<'_>) -> Result<Spanned<Ident>, Spanned<ParseError>> {
    let span = tokens.next_token_span();

    match tokens.peek() {
        Some(Token::Ident(ident)) => Ok(Spanned::new(ident, span)),
        Some(_) => Err(Spanned::new(
            ParseError::new()
                .msg("unexpected token")
                .expected("identifier"),
            span,
        )),
        None => Err(Spanned::new(
            ParseError::new()
                .msg("unexpected end of file")
                .expected("identifier"),
            span,
        )),
    }
}
