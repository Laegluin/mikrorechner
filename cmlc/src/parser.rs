use crate::ast::*;
use crate::lexer::Token;
use crate::span::{Span, Spanned};
use std::cell::Cell;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParseError {}

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
