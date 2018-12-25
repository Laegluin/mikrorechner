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
        self.tokens[self.consumed.get() - 1].span
    }

    fn start_span(&self) -> SpanStart<'_, 't> {
        SpanStart {
            start: self.consumed.get(),
            stream: &self,
        }
    }

    fn pos(&self) -> StreamPos {
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
    let mut items = Vec::new();

    while tokens.peek() != None {
        items.push(item(&tokens)?);
    }

    Ok(items)
}

fn item(tokens: &TokenStream<'_>) -> Result<Spanned<Item>, Spanned<ParseError>> {
    one_of(tokens, &[type_def, fn_def])
}

fn type_def(tokens: &TokenStream<'_>) -> Result<Item, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Type), "type")?;
    let name = ident(tokens)?;
    token(tokens, Token::Equal, "=")?;

    match tokens.next() {
        Some(Token::OpenBrace) => {
            let span_start = tokens.start_span();
            let record_def = record_def(name, tokens)?;
            let span = span_start.end();

            Ok(Item::TypeDef(TypeDef::RecordDef(Spanned::new(
                record_def, span,
            ))))
        }
        Some(Token::Pipe) => {
            let span_start = tokens.start_span();
            let variants_def = variants_def(name, tokens)?;
            let span = span_start.end();

            Ok(Item::TypeDef(TypeDef::VariantsDef(Spanned::new(
                variants_def,
                span,
            ))))
        }
        Some(_) => {
            let alias = type_desc(tokens)?;
            token(tokens, Token::Semicolon, ";")?;
            Ok(Item::TypeDef(TypeDef::Alias(alias)))
        }
        None => Err(Spanned::new(
            ParseError::eof()
                .expected("record definition")
                .expected("variants definition")
                .expected("type alias"),
            tokens.eof_span(),
        )),
    }
}

fn record_def(
    name: Spanned<Ident>,
    tokens: &TokenStream<'_>,
) -> Result<RecordDef, Spanned<ParseError>> {
    let mut fields = Vec::new();

    while tokens.peek() != Some(Token::CloseBrace) {
        let span_start = tokens.start_span();
        let field = field_def(tokens)?;
        let span = span_start.end();
        fields.push(Spanned::new(field, span));

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            _ => break,
        }
    }

    token(tokens, Token::CloseBrace, "}")?;
    Ok(RecordDef { name, fields })
}

fn field_def(tokens: &TokenStream<'_>) -> Result<FieldDef, Spanned<ParseError>> {
    let name = ident(tokens)?;
    token(tokens, Token::Colon, ":")?;
    let ty = type_desc(tokens)?;

    Ok(FieldDef { name, ty })
}

fn variants_def(
    name: Spanned<Ident>,
    tokens: &TokenStream<'_>,
) -> Result<VariantsDef, Spanned<ParseError>> {
    let mut variants = Vec::new();

    while tokens.peek() != Some(Token::Semicolon) {
        let span_start = tokens.start_span();
        let variant = variant_def(tokens)?;
        let span = span_start.end();
        variants.push(Spanned::new(variant, span));
    }

    token(tokens, Token::Semicolon, ";")?;
    Ok(VariantsDef { name, variants })
}

fn variant_def(tokens: &TokenStream<'_>) -> Result<VariantDef, Spanned<ParseError>> {
    let name = ident(tokens)?;
    let mut param_tys = Vec::new();

    while tokens.peek() != Some(Token::Pipe) && tokens.peek() != Some(Token::Semicolon) {
        param_tys.push(type_desc(tokens)?);

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            _ => break,
        }
    }

    match tokens.peek() {
        Some(Token::Pipe) => {
            tokens.next();
        }
        // don't consume the semicolon so `variants_def` the definition is done
        Some(Token::Semicolon) => (),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("|").expected(";"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("|").expected(";"),
                tokens.eof_span(),
            ))
        }
    }

    Ok(VariantDef { name, param_tys })
}

fn fn_def(tokens: &TokenStream<'_>) -> Result<Item, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Fn), "fn")?;
    let name = ident(tokens)?;
    token(tokens, Token::Equal, "=")?;

    let mut params = Vec::new();

    while tokens.peek() != Some(Token::Arrow) && tokens.peek() != Some(Token::OpenBrace) {
        let span_start = tokens.start_span();
        let param = param_def(tokens)?;
        let span = span_start.end();
        params.push(Spanned::new(param, span));

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            _ => break,
        }
    }

    let ret_ty = match tokens.peek() {
        Some(Token::Arrow) => {
            tokens.next();
            Some(type_desc(tokens)?)
        }
        Some(Token::OpenBrace) => None,
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("->"),
                tokens.last_token_span(),
            ))
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("->"),
                tokens.eof_span(),
            ))
        }
    };

    let body = block(tokens)?;

    Ok(Item::FnDef(FnDef {
        name,
        params,
        ret_ty,
        body,
    }))
}

fn param_def(tokens: &TokenStream<'_>) -> Result<ParamDef, Spanned<ParseError>> {
    let name = match tokens.peek() {
        Some(Token::Underscore) => {
            tokens.next();
            Spanned::new(None, tokens.last_token_span())
        }
        _ => ident(tokens)?.map(Some),
    };

    token(tokens, Token::Colon, ":")?;
    let ty = type_desc(tokens)?;

    Ok(ParamDef { name, ty })
}

fn expr(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    if tokens.peek() == Some(Token::Keyword(Keyword::Let)) {
        let_binding(tokens)
    } else {
        assignment(tokens)
    }
}

fn let_binding(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    token(tokens, Token::Keyword(Keyword::Let), "let")?;
    let pattern = pattern(tokens)?;

    let ty_hint = match tokens.peek() {
        Some(Token::Colon) => {
            tokens.next();
            Some(type_desc(tokens)?)
        }
        _ => None,
    };

    token(tokens, Token::Equal, "=")?;
    let expr = expr(tokens)?.map(Box::new);

    Ok(Spanned::new(
        Expr::LetBinding(LetBinding {
            pattern,
            ty_hint,
            expr,
        }),
        span_start.end(),
    ))
}

fn assignment(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    let mut target = logical_or(tokens)?;

    if tokens.peek() == Some(Token::Equal) {
        tokens.next();
        let value = expr(tokens)?.map(Box::new);

        target = Spanned::new(
            Expr::Assignment(Assignment {
                target: target.map(Box::new),
                value,
            }),
            span_start.end(),
        );
    }

    Ok(target)
}

fn logical_or(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = logical_and(tokens)?;

    while tokens.peek() == Some(Token::DoublePipe) {
        tokens.next();
        let rhs = logical_and(tokens)?.map(Box::new);

        lhs = Spanned::new(
            Expr::BinOp(BinOp {
                op: BinOpKind::Or,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn logical_and(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = equality(tokens)?;

    while tokens.peek() == Some(Token::DoubleAmp) {
        tokens.next();
        let rhs = equality(tokens)?.map(Box::new);

        lhs = Spanned::new(
            Expr::BinOp(BinOp {
                op: BinOpKind::And,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn equality(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = comparison(tokens)?;

    while tokens.peek() == Some(Token::DoubleEqual) || tokens.peek() == Some(Token::BangEqual) {
        let op = match tokens.next() {
            Some(Token::DoubleEqual) => BinOpKind::Eq,
            Some(Token::BangEqual) => BinOpKind::Ne,
            _ => unreachable!(),
        };

        let rhs = comparison(tokens)?.map(Box::new);

        lhs = Spanned::new(
            Expr::BinOp(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn comparison(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = additive(tokens)?;

    while tokens.peek() == Some(Token::Less)
        || tokens.peek() == Some(Token::LessEqual)
        || tokens.peek() == Some(Token::Greater)
        || tokens.peek() == Some(Token::GreaterEqual)
    {
        let op = match tokens.next() {
            Some(Token::Less) => BinOpKind::Lt,
            Some(Token::LessEqual) => BinOpKind::Le,
            Some(Token::Greater) => BinOpKind::Gt,
            Some(Token::GreaterEqual) => BinOpKind::Ge,
            _ => unreachable!(),
        };

        let rhs = additive(tokens)?.map(Box::new);

        lhs = Spanned::new(
            Expr::BinOp(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn additive(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = multiplicative(tokens)?;

    while tokens.peek() == Some(Token::Plus) || tokens.peek() == Some(Token::Minus) {
        let op = match tokens.next() {
            Some(Token::Plus) => BinOpKind::Add,
            Some(Token::Minus) => BinOpKind::Sub,
            _ => unreachable!(),
        };

        let rhs = multiplicative(tokens)?.map(Box::new);

        lhs = Spanned::new(
            Expr::BinOp(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn multiplicative(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = unary(tokens)?;

    while tokens.peek() == Some(Token::Star) || tokens.peek() == Some(Token::Slash) {
        let op = match tokens.next() {
            Some(Token::Star) => BinOpKind::Mul,
            Some(Token::Slash) => BinOpKind::Div,
            _ => unreachable!(),
        };

        let rhs = unary(tokens)?.map(Box::new);

        lhs = Spanned::new(
            Expr::BinOp(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn unary(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    if tokens.peek() == Some(Token::Bang)
        || tokens.peek() == Some(Token::Minus)
        || tokens.peek() == Some(Token::Star)
        || tokens.peek() == Some(Token::Amp)
        || tokens.peek() == Some(Token::DoubleAmp)
    {
        let mut is_double_ref = false;
        let mut inner_ref_start = Index(0);

        let op = match tokens.next() {
            Some(Token::Bang) => UnOpKind::Not,
            Some(Token::Minus) => UnOpKind::Negate,
            Some(Token::Star) => UnOpKind::Deref,
            Some(Token::Amp) => match tokens.peek() {
                Some(Token::Keyword(Keyword::Mut)) => {
                    tokens.next();
                    UnOpKind::AddrOfMut
                }
                _ => UnOpKind::AddrOf,
            },
            Some(Token::DoubleAmp) => {
                is_double_ref = true;

                // start at the half of the token, since both chars in `&&` have the same length
                let span = tokens.last_token_span();
                let inner_ref_offset = (span.end() - span.start()).to_usize() / 2;
                inner_ref_start = span.start() + Offset(inner_ref_offset as i64);

                match tokens.peek() {
                    Some(Token::Keyword(Keyword::Mut)) => {
                        tokens.next();
                        UnOpKind::AddrOfMut
                    }
                    _ => UnOpKind::AddrOf,
                }
            }
            _ => unreachable!(),
        };

        let operand = unary(tokens)?.map(Box::new);

        if is_double_ref {
            let inner_span = Span::new(inner_ref_start, operand.span.end());

            // wrap in the `&` that is part of `&&` or `&&mut` and was ignored above
            Ok(Spanned::new(
                Expr::UnOp(UnOp {
                    op: UnOpKind::AddrOf,
                    operand: Spanned::new(Box::new(Expr::UnOp(UnOp { op, operand })), inner_span),
                }),
                span_start.end(),
            ))
        } else {
            Ok(Spanned::new(
                Expr::UnOp(UnOp { op, operand }),
                span_start.end(),
            ))
        }
    } else {
        method_call(tokens)
    }
}

fn method_call(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut object = fn_call_expr(tokens)?;

    while let Some(&[Token::Ident(_), Token::Colon]) =
        tokens.peek_n(2).as_ref().map(|vec| vec.as_slice())
    {
        let call = fn_call(tokens)?;

        object = Spanned::new(
            Expr::MethodCall(MethodCall {
                object: object.map(Box::new),
                call,
            }),
            span_start.end(),
        )
    }

    Ok(object)
}

fn fn_call_expr(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    fn_call(tokens).map(|spanned| spanned.map(Expr::FnCall))
}

fn fn_call(tokens: &TokenStream<'_>) -> Result<Spanned<FnCall>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let fn_name = item_path(tokens)?;

    match tokens.next() {
        Some(Token::Bang) => Ok(Spanned::new(
            FnCall {
                name: fn_name,
                args: Vec::new(),
            },
            span_start.end(),
        )),
        Some(Token::Colon) => {
            let expected = || format!("argument for `{}`", fn_name.value);

            let first_arg = fn_arg(tokens)
                .map_err(|spanned| spanned.map(|err| err.set_expected(expected())))?;

            let mut args = vec![first_arg];

            while let Some(Token::Comma) = tokens.peek() {
                tokens.next();

                // if the following tokens cannot be parsed as expression of a higher
                // precedence, assume the comma is a trailing comma
                match fn_arg(tokens) {
                    Ok(arg) => args.push(arg),
                    Err(_) => break,
                }
            }

            Ok(Spanned::new(
                FnCall {
                    name: fn_name,
                    args,
                },
                span_start.end(),
            ))
        }
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected(":").expected("!"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected(":").expected("!"),
            tokens.eof_span(),
        )),
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

fn segment(tokens: &TokenStream<'_>) -> Result<Spanned<Ident>, Spanned<ParseError>> {
    ident(tokens).map_err(|spanned| spanned.map(|err| err.set_expected("item identifier")))
}

fn fn_arg(tokens: &TokenStream<'_>) -> Result<Spanned<Arg>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    match tokens.peek_n(2).as_ref().map(|vec| vec.as_slice()) {
        Some(&[Token::Ident(ref ident), Token::Equal]) => {
            tokens.next();
            let ident_span = tokens.last_token_span();
            tokens.next();

            atom_or_group(tokens).map(|expr| {
                Spanned::new(
                    Arg {
                        name: Some(Spanned::new(ident.clone(), ident_span)),
                        value: expr,
                    },
                    span_start.end(),
                )
            })
        }
        _ => atom_or_group(tokens).map(|expr| {
            Spanned::new(
                Arg {
                    name: None,
                    value: expr,
                },
                span_start.end(),
            )
        }),
    }
}

fn atom_or_group(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    one_of(
        tokens,
        &[
            lit,
            var,
            array_cons,
            parenthesized_or_tuple_cons,
            ret_expr,
            if_expr,
            block_expr,
        ],
    )
}

fn lit(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    match tokens.next() {
        Some(Token::Lit(lit)) => Ok(Expr::Lit(lit)),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected("literal"),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected("literal"),
            tokens.eof_span(),
        )),
    }
}

fn var(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    ident(tokens)
        .map(|ident| Expr::Var(ident.value))
        .map_err(|spanned| spanned.map(|err| err.set_expected("variable")))
}

fn array_cons(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::OpenBracket, "[")?;
    let mut elems = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseBracket) => {
                tokens.next();
                return Ok(Expr::ArrayCons(ArrayCons { elems }));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("]"),
                    tokens.eof_span(),
                ))
            }
        }

        elems.push(expr(tokens)?);

        match tokens.next() {
            Some(Token::CloseBracket) => {
                return Ok(Expr::ArrayCons(ArrayCons { elems }));
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token().expected("]").expected(","),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("]").expected(","),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

fn parenthesized_or_tuple_cons(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::OpenParen, "(")?;
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
                    ParseError::eof().expected(")"),
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
                    ParseError::unexpected_token().expected(")").expected(","),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")").expected(","),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

fn ret_expr(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Ret), "ret")?;
    Ok(Expr::Ret(expr(tokens)?.map(Box::new).value))
}

fn if_expr(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::If), "if")?;

    let cond = expr(tokens)?.map(Box::new);
    let then_block = block(tokens)?;

    let else_block = match tokens.peek() {
        Some(Token::Keyword(Keyword::Else)) => {
            tokens.next();

            match tokens.peek() {
                Some(Token::Keyword(Keyword::If)) => {
                    let span_start = tokens.start_span();
                    let if_expr = if_expr(tokens)?;
                    let span = span_start.end();

                    Some(Spanned::new(
                        Block {
                            exprs: vec![Spanned::new(if_expr, span)],
                            is_last_expr_stmt: false,
                        },
                        span,
                    ))
                }
                _ => Some(block(tokens)?),
            }
        }
        _ => None,
    };

    Ok(Expr::IfExpr(IfExpr {
        cond,
        then_block,
        else_block,
    }))
}

fn block_expr(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    block(tokens).map(|spanned| Expr::Block(spanned.value))
}

fn block(tokens: &TokenStream<'_>) -> Result<Spanned<Block>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    token(tokens, Token::OpenBrace, "{")?;
    let mut exprs = Vec::new();
    let mut is_last_expr_stmt = false;

    while tokens.peek() != Some(Token::CloseBrace) {
        exprs.push(expr(tokens)?);

        if tokens.peek() == Some(Token::Semicolon) {
            tokens.next();
            is_last_expr_stmt = true;
        } else {
            is_last_expr_stmt = false;
            break;
        }
    }

    token(tokens, Token::CloseBrace, "}")?;

    Ok(Spanned::new(
        Block {
            exprs,
            is_last_expr_stmt,
        },
        span_start.end(),
    ))
}

fn pattern(tokens: &TokenStream<'_>) -> Result<Spanned<Pattern>, Spanned<ParseError>> {
    one_of(tokens, &[discard_pattern, binding_pattern, tuple_pattern])
}

fn discard_pattern(tokens: &TokenStream<'_>) -> Result<Pattern, Spanned<ParseError>> {
    token(tokens, Token::Underscore, "_")?;
    Ok(Pattern::Discard)
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

    token(tokens, Token::OpenParen, "(")?;
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
                    ParseError::eof().expected(")"),
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
                    ParseError::unexpected_token().expected(")").expected(","),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")").expected(","),
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
    token(tokens, Token::Underscore, "_")?;
    Ok(TypeDesc::Hole)
}

fn name_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    ident(tokens)
        .map(|ident| TypeDesc::Name(ident.value))
        .map_err(|spanned| spanned.map(|err| err.set_expected("type name")))
}

fn ptr_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    token(tokens, Token::Star, "*")?;

    match tokens.peek() {
        Some(Token::Keyword(Keyword::Mut)) => {
            tokens.next();

            Ok(TypeDesc::MutPtr(Box::new(
                type_desc(tokens).map(|spanned| spanned.value)?,
            )))
        }
        _ => Ok(TypeDesc::Ptr(Box::new(
            type_desc(tokens).map(|spanned| spanned.value)?,
        ))),
    }
}

fn array_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    token(tokens, Token::OpenBracket, "[")?;
    let ty = type_desc(tokens)?.map(Box::new);
    token(tokens, Token::Semicolon, ";")?;

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
                ParseError::eof().expected("array length"),
                tokens.eof_span(),
            ))
        }
    };

    token(tokens, Token::CloseBracket, "]")?;
    Ok(TypeDesc::Array(ArrayDesc { ty, len }))
}

fn function_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Fn), "fn")?;
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

    token(tokens, Token::Arrow, "->")?;
    let ret_ty = type_desc(tokens)?.map(Box::new);

    Ok(TypeDesc::Function(FunctionDesc { params_ty, ret_ty }))
}

fn tuple_type_desc(tokens: &TokenStream<'_>) -> Result<TypeDesc, Spanned<ParseError>> {
    token(tokens, Token::OpenParen, "(")?;
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
                    ParseError::eof().expected(")"),
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
                    ParseError::unexpected_token().expected(")").expected(","),
                    tokens.last_token_span(),
                ))
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")").expected(","),
                    tokens.eof_span(),
                ))
            }
        }
    }
}

/// Parses `tokens` with one of the supplied parsers. The first match will be returned. If there is
/// no successful match, the error with the furthest end will be returned.
///
/// The stream will only consume tokens on a successful parse.
fn one_of<T>(
    tokens: &TokenStream<'_>,
    parsers: &[fn(&TokenStream<'_>) -> Result<T, Spanned<ParseError>>],
) -> Result<Spanned<T>, Spanned<ParseError>>
where
{
    assert!(!parsers.is_empty());

    let start_pos = tokens.pos();
    let span_start = tokens.start_span();
    let mut best_err: Option<Spanned<ParseError>> = None;

    for parser in parsers {
        match parser(tokens) {
            Ok(value) => return Ok(Spanned::new(value, span_start.end())),
            Err(err) => match best_err {
                Some(ref best_err_val) => {
                    let end = err.span.end();
                    let best_end = best_err_val.span.end();

                    if end > best_end {
                        best_err = Some(err)
                    }
                }
                None => best_err = Some(err),
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

fn token(
    tokens: &TokenStream<'_>,
    token: Token,
    expected: &'static str,
) -> Result<Token, Spanned<ParseError>> {
    match tokens.next() {
        Some(ref found_token) if *found_token == token => Ok(token),
        Some(_) => Err(Spanned::new(
            ParseError::unexpected_token().expected(expected),
            tokens.last_token_span(),
        )),
        None => Err(Spanned::new(
            ParseError::eof().expected(expected),
            tokens.eof_span(),
        )),
    }
}
