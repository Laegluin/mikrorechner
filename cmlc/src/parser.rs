use crate::ast::*;
use crate::lexer::{Keyword, Token};
use crate::span::{Index, Offset, Span, Spanned};
use std::cell::Cell;

#[derive(Debug, Default)]
pub struct ParseError {
    msg: Option<String>,
    expected: Vec<String>,
}

impl ParseError {
    fn new() -> ParseError {
        ParseError::default()
    }

    fn eof() -> ParseError {
        ParseError::new().msg("unexpected end of file")
    }

    fn unexpected_token() -> ParseError {
        ParseError::new().msg("unexpected token")
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

    /// Returns the span for the next token.
    ///
    /// ## Panics
    /// Panics if there is no next token in the stream.
    fn next_token_span(&self) -> Span {
        assert!(self.consumed.get() < self.tokens.len());
        self.tokens[self.consumed.get()].span
    }

    fn eof_span(&self) -> Span {
        let consumed = self.consumed.get();

        if consumed < self.tokens.len() {
            let this_is_the_end = self.tokens[consumed].span.end();
            Span::new(this_is_the_end, this_is_the_end)
        } else {
            Span::new(Index(0), Index(0))
        }
    }

    /// Returns the span for the last token.
    ///
    /// ## Panics
    /// Panics if there was no last token, meaning `next` was never called.
    fn last_token_span(&self) -> Span {
        assert!(self.consumed.get() > 0);
        self.tokens[self.consumed.get()].span
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

fn expr(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    unimplemented!()
}

fn lit(tokens: &TokenStream<'_>) -> Result<Expr, ParseError> {
    match tokens.next() {
        Some(Token::Lit(lit)) => Ok(Expr::Lit(lit)),
        Some(_) => Err(ParseError::unexpected_token().expected("literal")),
        None => Err(ParseError::eof().expected("literal")),
    }
}

fn var(tokens: &TokenStream<'_>) -> Result<Expr, ParseError> {
    ident(tokens)
        .map(Expr::Var)
        .map_err(|err| err.set_expected("variable"))
}

fn un_op(tokens: &TokenStream<'_>) -> Result<UnOp, Spanned<ParseError>> {
    let op = tokens.next().ok_or_else(|| {
        Spanned::new(
            ParseError::eof().expected("unary operator"),
            tokens.eof_span(),
        )
    })?;

    let mut is_double_ref = false;

    let op = match op {
        Token::Bang => UnOpKind::Not,
        Token::Star => UnOpKind::Deref,
        Token::Amp => {
            let next_token = tokens.peek().ok_or_else(|| {
                Spanned::new(
                    ParseError::eof().expected("mut").expected("expression"),
                    tokens.eof_span(),
                )
            })?;

            if next_token == Token::Keyword(Keyword::Mut) {
                tokens.next();
                UnOpKind::AddrOfMut
            } else {
                UnOpKind::AddrOf
            }
        }
        Token::DoubleAmp => {
            is_double_ref = true;
            UnOpKind::AddrOf
        }
        _ => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("unary operator"),
                tokens.last_token_span(),
            ))
        }
    };

    let op_span = tokens.last_token_span();
    let term = UnOp {
        op,
        operand: expr(tokens)
            .map(|spanned| spanned.map(Box::new))
            .map_err(|spanned| spanned.map(|err| err.expected("operand")))?,
    };

    if is_double_ref {
        // half the start of the span for op, discarding the first `&` of `&&`
        let half_offset = (op_span.end() - op_span.start()).to_usize() / 2;
        let span = Span::new(
            op_span.start() + Offset(half_offset as i64),
            term.operand.span.end(),
        );

        Ok(UnOp {
            op: UnOpKind::AddrOf,
            operand: Spanned::new(Box::new(Expr::UnOp(term)), span),
        })
    } else {
        Ok(term)
    }
}

fn ident(tokens: &TokenStream<'_>) -> Result<Ident, ParseError> {
    match tokens.next() {
        Some(Token::Ident(ident)) => Ok(ident),
        Some(_) => Err(ParseError::unexpected_token().expected("identifier")),
        None => Err(ParseError::eof().expected("identifier")),
    }
}
