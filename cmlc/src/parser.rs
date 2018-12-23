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

    fn peek_n(&self, n: usize) -> Option<Vec<Token>> {
        let consumed = self.consumed.get();

        if consumed < self.tokens.len() && (consumed + n) < self.tokens.len() {
            let tokens = self.tokens[consumed..consumed + n]
                .iter()
                .map(|token| token.value.clone())
                .collect();

            Some(tokens)
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

    fn save_pos(&self) -> StreamPos {
        StreamPos(self.consumed.get())
    }

    fn restore_pos(&self, pos: StreamPos) {
        self.consumed.set(pos.0);
    }
}

#[derive(Clone, Copy)]
struct StreamPos(usize);

struct SpanStart<'s, 't> {
    start: usize,
    stream: &'s TokenStream<'t>,
}

impl SpanStart<'_, '_> {
    fn end(&self) -> Span {
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
    // don't forget normal ()
    // while loop for chained method calls
    unimplemented!()
}

fn lit(tokens: &TokenStream<'_>) -> Result<Expr, ParseError> {
    match tokens.next() {
        Some(Token::Lit(lit)) => Ok(Expr::Lit(lit)),
        Some(_) => Err(ParseError::unexpected_token().expected("literal")),
        None => Err(ParseError::eof().expected("literal")),
    }
}

fn var(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    match try_fn_call_start(tokens) {
        Some(span) => Err(Spanned::new(
            ParseError::new()
                .msg("unexpected function call")
                .expected("variable"),
            span,
        )),
        None => ident(tokens)
            .map(|ident| Expr::Var(ident.value))
            .map_err(|spanned| spanned.map(|err| err.set_expected("variable"))),
    }
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

fn bin_op(tokens: &TokenStream<'_>) -> Result<BinOp, Spanned<ParseError>> {
    let lhs = expr(tokens)?.map(Box::new);

    let op = tokens.next().ok_or_else(|| {
        Spanned::new(
            ParseError::eof().expected("binary operator"),
            tokens.eof_span(),
        )
    })?;

    let op = match op {
        Token::Plus => BinOpKind::Add,
        Token::Minus => BinOpKind::Sub,
        Token::Star => BinOpKind::Mul,
        Token::Slash => BinOpKind::Div,
        Token::DoubleAmp => BinOpKind::And,
        Token::DoublePipe => BinOpKind::Or,
        Token::Less => BinOpKind::Lt,
        Token::LessEqual => BinOpKind::Le,
        Token::Greater => BinOpKind::Gt,
        Token::GreaterEqual => BinOpKind::Ge,
        _ => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("binary operator"),
                tokens.last_token_span(),
            ))
        }
    };

    let rhs = expr(tokens)?.map(Box::new);

    Ok(BinOp { op, lhs, rhs })
}

fn fn_call(tokens: &TokenStream<'_>) -> Result<FnCall, Spanned<ParseError>> {
    let fn_name = item_path(tokens)?;

    match tokens.next() {
        Some(Token::Bang) => Ok(FnCall {
            name: fn_name,
            args: Vec::new(),
        }),
        Some(Token::Colon) => {
            let expected = || format!("argument for `{}`", fn_name.value);

            let first_arg = fn_arg(tokens)
                .map_err(|spanned| spanned.map(|err| err.set_expected(expected())))?;

            let mut args = vec![first_arg];

            while let Some(Token::Comma) = tokens.peek() {
                tokens.next();

                if try_fn_call_start(tokens).is_some() {
                    break;
                }

                let arg = fn_arg(tokens)
                    .map_err(|spanned| spanned.map(|err| err.set_expected(expected())))?;

                args.push(arg);
            }

            Ok(FnCall {
                name: fn_name,
                args,
            })
        }
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token()
                .expected("colon after function name")
                .expected("bang after function name"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof()
                .expected("colon after function name")
                .expected("bang after function name"),
            tokens.eof_span(),
        )),
    }
}

/// Returns Some if the next tokens would be the start of a function call, like
/// `path::function:`,`function:` or `function!`. The contained span is the span of
/// the function call start. Does not advance the stream.
fn try_fn_call_start(tokens: &TokenStream<'_>) -> Option<Span> {
    let span_start = tokens.start_span();
    let pos = tokens.save_pos();

    if item_path(tokens).is_err() {
        tokens.restore_pos(pos);
        return None;
    }

    let next = tokens.next();
    let span = span_start.end();
    tokens.restore_pos(pos);

    match next {
        Some(Token::Colon) | Some(Token::Bang) => Some(span),
        _ => None,
    }
}

fn item_path(tokens: &TokenStream<'_>) -> Result<Spanned<ItemPath>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut segments = vec![segment(tokens)?];

    while let Some(Token::DoubleColon) = tokens.peek() {
        tokens.next();
        segments.push(segment(tokens)?);
    }

    Ok(Spanned::new(ItemPath { segments }, span_start.end()))
}

fn fn_arg(tokens: &TokenStream<'_>) -> Result<Spanned<Arg>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    match tokens.peek_n(2).as_ref().map(|vec| vec.as_slice()) {
        Some(&[Token::Ident(ref ident), Token::Equal]) => {
            tokens.next();
            let ident_span = tokens.last_token_span();
            tokens.next();

            if let Some(span) = try_fn_call_start(tokens) {
                return Err(Spanned::new(
                    ParseError::new()
                        .msg("unexpected function call after named argument")
                        .expected("parenthesized function call")
                        .expected("expression"),
                    span,
                ));
            }

            expr(tokens).map(|expr| {
                Spanned::new(
                    Arg {
                        name: Some(Spanned::new(ident.clone(), ident_span)),
                        value: expr,
                    },
                    span_start.end(),
                )
            })
        }
        _ => {
            if let Some(span) = try_fn_call_start(tokens) {
                return Err(Spanned::new(
                    ParseError::new()
                        .msg("unexpected function call")
                        .expected("parenthesized function call")
                        .expected("expression"),
                    span,
                ));
            }

            expr(tokens).map(|expr| {
                Spanned::new(
                    Arg {
                        name: None,
                        value: expr,
                    },
                    span_start.end(),
                )
            })
        }
    }
}

fn segment(tokens: &TokenStream<'_>) -> Result<Spanned<Ident>, Spanned<ParseError>> {
    ident(tokens).map_err(|spanned| spanned.map(|err| err.set_expected("item identifier")))
}

fn field_access(tokens: &TokenStream<'_>) -> Result<FieldAccess, Spanned<ParseError>> {
    let value = expr(tokens)?.map(Box::new);

    match tokens.next() {
        Some(Token::Dot) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("dot"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("dot"),
                tokens.eof_span(),
            ))
        }
    }

    let mut num_derefs = 0;

    while let Some(Token::Star) = tokens.peek() {
        tokens.next();
        num_derefs += 1;
    }

    let field =
        ident(tokens).map_err(|spanned| spanned.map(|err| err.set_expected("field name")))?;

    Ok(FieldAccess {
        value,
        field,
        num_derefs,
    })
}

fn array_cons(tokens: &TokenStream<'_>) -> Result<ArrayCons, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::OpenBracket) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("opening bracket"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("opening bracket"),
                tokens.eof_span(),
            ))
        }
    }

    let mut elems = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseBracket) => {
                tokens.next();
                return Ok(ArrayCons { elems });
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("closing bracket"),
                    tokens.eof_span(),
                ))
            }
        }

        elems.push(expr(tokens)?);

        match tokens.next() {
            Some(Token::CloseBracket) => {
                return Ok(ArrayCons { elems });
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token()
                        .expected("closing bracket")
                        .expected("comma"),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof()
                        .expected("closing bracket")
                        .expected("comma"),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

fn parenthesized_or_tuple_cons(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::OpenParen) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("opening parenthesis"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("opening parenthesis"),
                tokens.eof_span(),
            ))
        }
    }

    let mut elems = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseParen) => {
                tokens.next();
                return Ok(Expr::TupleCons(TupleCons { elems }));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("closing parenthesis"),
                    tokens.eof_span(),
                ))
            }
        }

        elems.push(expr(tokens)?);

        match tokens.next() {
            Some(Token::CloseParen) => {
                // one expression in parenthesis without a comma is just for precedence
                if elems.len() == 1 {
                    return Ok(elems.pop().unwrap().value);
                } else {
                    return Ok(Expr::TupleCons(TupleCons { elems }));
                }
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token()
                        .expected("closing parenthesis")
                        .expected("comma"),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof()
                        .expected("closing parenthesis")
                        .expected("comma"),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

fn assignment(tokens: &TokenStream<'_>) -> Result<Assignment, Spanned<ParseError>> {
    let var_name = ident(tokens)?;

    match tokens.next() {
        Some(Token::Equal) => Ok(Assignment {
            var_name,
            value: expr(tokens)?.map(Box::new),
        }),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected("assigment operator"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected("assigment operator"),
            tokens.eof_span(),
        )),
    }
}

fn pattern(tokens: &TokenStream<'_>) -> Result<Spanned<Pattern>, Spanned<ParseError>> {
    one_of(tokens, &[discard_pattern, binding_pattern, tuple_pattern])
}

fn discard_pattern(tokens: &TokenStream<'_>) -> Result<Pattern, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::Underscore) => Ok(Pattern::Discard),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected("discard pattern"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected("discard pattern"),
            tokens.eof_span(),
        )),
    }
}

fn binding_pattern(tokens: &TokenStream<'_>) -> Result<Pattern, Spanned<ParseError>> {
    match tokens.peek() {
        Some(Token::Keyword(Keyword::Mut)) => {
            tokens.next();

            ident(tokens)
                .map(Pattern::MutBinding)
                .map_err(|spanned| spanned.map(|err| err.set_expected("binding pattern")))
        }
        Some(Token::Ident(_)) => ident(tokens)
            .map(Pattern::Binding)
            .map_err(|spanned| spanned.map(|err| err.set_expected("binding pattern"))),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token()
                .expected("binding pattern")
                .expected("mutable binding pattern"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof()
                .expected("binding pattern")
                .expected("mutable binding pattern"),
            tokens.eof_span(),
        )),
    }
}

fn tuple_pattern(tokens: &TokenStream<'_>) -> Result<Pattern, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    match tokens.next() {
        Some(Token::OpenParen) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("opening parenthesis"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("opening parenthesis"),
                tokens.eof_span(),
            ))
        }
    }

    let mut patterns = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseParen) => {
                tokens.next();
                return Ok(Pattern::Tuple(Spanned::new(patterns, span_start.end())));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("closing parenthesis"),
                    tokens.eof_span(),
                ))
            }
        }

        patterns.push(pattern(tokens)?.value);

        match tokens.next() {
            Some(Token::CloseParen) => {
                return Ok(Pattern::Tuple(Spanned::new(patterns, span_start.end())));
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token()
                        .expected("closing parenthesis")
                        .expected("comma"),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof()
                        .expected("closing parenthesis")
                        .expected("comma"),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

fn type_desc(tokens: &TokenStream<'_>) -> Result<Spanned<TypeDesc>, Spanned<ParseError>> {
    one_of(
        tokens,
        &[
            hole_type_desc,
            name_type_desc,
            ptr_type_desc,
            array_type_desc,
            function_type_desc,
            tuple_type_desc,
        ],
    )
}

fn hole_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::Underscore) => Ok(TypeDesc::Hole),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected("hole"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected("hole"),
            tokens.eof_span(),
        )),
    }
}

fn name_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    ident(tokens)
        .map(|ident| TypeDesc::Name(ident.value))
        .map_err(|spanned| spanned.map(|err| err.set_expected("type name")))
}

fn ptr_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::Star) => Ok(TypeDesc::Ptr(Box::new(
            type_desc(tokens).map(|spanned| spanned.value)?,
        ))),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected("pointer"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected("pointer"),
            tokens.eof_span(),
        )),
    }
}

fn array_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::OpenBracket) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("opening bracket"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("opening bracket"),
                tokens.eof_span(),
            ))
        }
    }

    let ty = type_desc(tokens)?.map(Box::new);

    match tokens.next() {
        Some(Token::Semicolon) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("semicolon"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("semicolon"),
                tokens.eof_span(),
            ))
        }
    }

    let len = match tokens.next() {
        Some(Token::Lit(Lit::Int(len))) => Spanned::new(len, tokens.last_token_span()),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("array length"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("semicolon"),
                tokens.eof_span(),
            ))
        }
    };

    match tokens.next() {
        Some(Token::CloseBracket) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("closing bracket"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("closing bracket"),
                tokens.eof_span(),
            ))
        }
    }

    Ok(TypeDesc::Array(ArrayDesc { ty, len }))
}

fn function_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::Keyword(Keyword::Fn)) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("fn"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("fn"),
                tokens.eof_span(),
            ))
        }
    }

    let mut params_ty = Vec::new();

    while tokens.peek() != Some(Token::Arrow) {
        params_ty.push(type_desc(tokens)?);

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            _ => break,
        }
    }

    match tokens.next() {
        Some(Token::Arrow) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("arrow"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("arrow"),
                tokens.eof_span(),
            ))
        }
    }

    let ret_ty = type_desc(tokens)?.map(Box::new);

    Ok(TypeDesc::Function(FunctionDesc { params_ty, ret_ty }))
}

fn tuple_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    match tokens.next() {
        Some(Token::OpenParen) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("opening parenthesis"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("opening parenthesis"),
                tokens.eof_span(),
            ))
        }
    }

    let mut tys = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseParen) => {
                tokens.next();
                return Ok(TypeDesc::Tuple(tys));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("closing parenthesis"),
                    tokens.eof_span(),
                ))
            }
        }

        tys.push(type_desc(tokens)?);

        match tokens.next() {
            Some(Token::CloseParen) => {
                return Ok(TypeDesc::Tuple(tys));
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token()
                        .expected("closing parenthesis")
                        .expected("comma"),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof()
                        .expected("closing parenthesis")
                        .expected("comma"),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

/// Parses `tokens` with one of the supplied parsers. The first match will be returned. If there is
/// no successful match, the error with the longest span will be returned.
///
/// The stream will only consume tokens on a successful parse.
fn one_of<T>(
    tokens: &TokenStream<'_>,
    parsers: &[fn(&TokenStream<'_>) -> Result<T, Spanned<ParseError>>],
) -> Result<Spanned<T>, Spanned<ParseError>>
where
{
    assert!(!parsers.is_empty());

    let start_pos = tokens.save_pos();
    let span_start = tokens.start_span();
    let mut best_err: Option<Spanned<ParseError>> = None;

    for parser in parsers {
        match parser(tokens) {
            Ok(value) => return Ok(Spanned::new(value, span_start.end())),
            Err(err) => match best_err {
                Some(ref best_err_val) if best_err_val.span < err.span => best_err = Some(err),
                None => best_err = Some(err),
                _ => (),
            },
        }

        // reset the stream after an error
        tokens.restore_pos(start_pos);
    }

    // must be Some because parsers is never empty
    Err(best_err.unwrap())
}

fn ident(tokens: &TokenStream<'_>) -> Result<Spanned<Ident>, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::Ident(ident)) => Ok(Spanned::new(ident, tokens.last_token_span())),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected("identifier"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected("identifier"),
            tokens.eof_span(),
        )),
    }
}
