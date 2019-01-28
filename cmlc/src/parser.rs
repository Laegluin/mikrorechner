use crate::ast::*;
use crate::lexer::{Keyword, Token};
use crate::span::{Index, Offset, Span, Spanned};
use crate::typecheck::TypeRef;
use std::cell::Cell;
use std::fmt::{self, Display};

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

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        assert!(!self.expected.is_empty());

        if let Some(ref msg) = &self.msg {
            write!(f, "{}, ", msg)?;
        }

        if self.expected.len() == 1 {
            write!(f, "expected {}", self.expected[0])
        } else {
            let head = &self.expected[..self.expected.len() - 2];
            let head_str = head.join(", ");

            if !head.is_empty() {
                write!(f, "expected {}, ", head_str)?;
            }

            write!(
                f,
                "{} or {}",
                self.expected[self.expected.len() - 2],
                self.expected[self.expected.len() - 1]
            )
        }
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
        if self.tokens.is_empty() {
            Span::new(Index(1), Index(1))
        } else {
            let this_is_the_end = self.tokens[self.tokens.len() - 1].span.end();
            Span::new(this_is_the_end, this_is_the_end)
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
        let consumed = self.stream.consumed.get();
        assert!(consumed > 0);
        let last_token = consumed - 1;
        assert!(self.start <= last_token);

        self.stream.tokens[self.start]
            .span
            .to(self.stream.tokens[last_token].span)
    }
}

pub fn parse(tokens: impl AsRef<[Spanned<Token>]>) -> Result<Ast, Spanned<ParseError>> {
    let tokens = TokenStream::new(tokens.as_ref());
    let mut items = Vec::new();

    while tokens.peek() != None {
        items.push(item(&tokens)?);
    }

    Ok(Ast::new(items))
}

fn item(tokens: &TokenStream<'_>) -> Result<Spanned<Item>, Spanned<ParseError>> {
    one_of(tokens, &[type_def, fn_def])
}

fn type_def(tokens: &TokenStream<'_>) -> Result<Item, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Type), "type")?;
    let name = ident(tokens)?;
    token(tokens, Token::Equal, "=")?;

    match tokens.peek() {
        Some(Token::OpenBrace) => {
            let span_start = tokens.start_span();
            let record_def = record_def(name, tokens)?;
            let span = span_start.end();

            Ok(Item::TypeDef(TypeDef::RecordDef(Spanned::new(
                record_def, span,
            ))))
        }
        Some(_) => {
            let span_start = tokens.start_span();
            let alias = type_decl(tokens)?;
            let span = span_start.end();
            token(tokens, Token::Semicolon, ";")?;

            Ok(Item::TypeDef(TypeDef::Alias(Spanned::new(
                AliasDef { name, ty: alias },
                span,
            ))))
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
    token(tokens, Token::OpenBrace, "{")?;
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
    let ty = type_decl(tokens)?;

    Ok(FieldDef { name, ty })
}

fn fn_def(tokens: &TokenStream<'_>) -> Result<Item, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Fn), "fn")?;
    let name = ident(tokens)?;

    let mut params = Vec::new();

    while tokens.peek() != Some(Token::Arrow) && tokens.peek() != Some(Token::Equal) {
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

    let ret_ty_hint = match tokens.peek() {
        Some(Token::Arrow) => {
            tokens.next();
            Some(type_decl(tokens)?)
        }
        Some(Token::Equal) => None,
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("->").expected("="),
                tokens.last_token_span(),
            ));
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("->").expected("="),
                tokens.eof_span(),
            ));
        }
    };

    token(tokens, Token::Equal, "=")?;
    let body = block(tokens)?;

    Ok(Item::FnDef(FnDef {
        name: ItemPath::from(name),
        params,
        ret_ty_hint,
        ret_ty: TypeRef::invalid(),
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

    let ty_hint = match tokens.peek() {
        Some(Token::Colon) => {
            tokens.next();
            Some(type_decl(tokens)?)
        }
        _ => None,
    };

    Ok(ParamDef {
        name,
        ty_hint,
        ty: TypeRef::invalid(),
    })
}

fn expr(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    expr_inner(tokens, true)
}

fn expr_in_fn_arg(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    expr_inner(tokens, false)
}

fn expr_inner(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    if tokens.peek() == Some(Token::Keyword(Keyword::Let)) {
        let_binding(tokens, parse_method_calls)
    } else {
        assignment(tokens, parse_method_calls)
    }
}

fn let_binding(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    token(tokens, Token::Keyword(Keyword::Let), "let")?;
    let pattern = pattern(tokens)?;

    let ty_hint = match tokens.peek() {
        Some(Token::Colon) => {
            tokens.next();
            Some(type_decl(tokens)?)
        }
        _ => None,
    };

    token(tokens, Token::Equal, "=")?;
    let expr = expr_inner(tokens, parse_method_calls)?.map(Box::new);

    Ok(Spanned::new(
        Expr::let_binding(LetBinding {
            pattern,
            ty_hint,
            expr,
        }),
        span_start.end(),
    ))
}

fn assignment(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    let mut target = logical_or(tokens, parse_method_calls)?;

    if tokens.peek() == Some(Token::Equal) {
        tokens.next();
        let value = expr_inner(tokens, parse_method_calls)?.map(Box::new);

        target = Spanned::new(
            Expr::assignment(Assignment {
                target: target.map(Box::new),
                value,
            }),
            span_start.end(),
        );
    }

    Ok(target)
}

fn logical_or(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = logical_and(tokens, parse_method_calls)?;

    while tokens.peek() == Some(Token::DoublePipe) {
        tokens.next();
        let rhs = logical_and(tokens, parse_method_calls)?.map(Box::new);

        lhs = Spanned::new(
            Expr::bin_op(BinOp {
                op: BinOpKind::Or,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn logical_and(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = equality(tokens, parse_method_calls)?;

    while tokens.peek() == Some(Token::DoubleAmp) {
        tokens.next();
        let rhs = equality(tokens, parse_method_calls)?.map(Box::new);

        lhs = Spanned::new(
            Expr::bin_op(BinOp {
                op: BinOpKind::And,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn equality(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = comparison(tokens, parse_method_calls)?;

    while tokens.peek() == Some(Token::DoubleEqual) || tokens.peek() == Some(Token::BangEqual) {
        let op = match tokens.next() {
            Some(Token::DoubleEqual) => BinOpKind::Eq,
            Some(Token::BangEqual) => BinOpKind::Ne,
            _ => unreachable!(),
        };

        let rhs = comparison(tokens, parse_method_calls)?.map(Box::new);

        lhs = Spanned::new(
            Expr::bin_op(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn comparison(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = additive(tokens, parse_method_calls)?;

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

        let rhs = additive(tokens, parse_method_calls)?.map(Box::new);

        lhs = Spanned::new(
            Expr::bin_op(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn additive(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = multiplicative(tokens, parse_method_calls)?;

    while tokens.peek() == Some(Token::Plus) || tokens.peek() == Some(Token::Minus) {
        let op = match tokens.next() {
            Some(Token::Plus) => BinOpKind::Add,
            Some(Token::Minus) => BinOpKind::Sub,
            _ => unreachable!(),
        };

        let rhs = multiplicative(tokens, parse_method_calls)?.map(Box::new);

        lhs = Spanned::new(
            Expr::bin_op(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn multiplicative(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut lhs = cast(tokens, parse_method_calls)?;

    while tokens.peek() == Some(Token::Star) || tokens.peek() == Some(Token::Slash) {
        let op = match tokens.next() {
            Some(Token::Star) => BinOpKind::Mul,
            Some(Token::Slash) => BinOpKind::Div,
            _ => unreachable!(),
        };

        let rhs = cast(tokens, parse_method_calls)?.map(Box::new);

        lhs = Spanned::new(
            Expr::bin_op(BinOp {
                op,
                lhs: lhs.map(Box::new),
                rhs,
            }),
            span_start.end(),
        );
    }

    Ok(lhs)
}

fn cast(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut operand = unary(tokens, parse_method_calls)?;

    while tokens.peek() == Some(Token::Keyword(Keyword::As)) {
        tokens.next();
        let to_ty = type_decl(tokens)?;

        operand = Spanned::new(
            Expr::cast(Cast {
                expr: operand.map(Box::new),
                to_ty,
            }),
            span_start.end(),
        );
    }

    Ok(operand)
}

fn unary(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    if tokens.peek() == Some(Token::Bang)
        || tokens.peek() == Some(Token::Minus)
        || tokens.peek() == Some(Token::Star)
        || tokens.peek() == Some(Token::Amp)
        || tokens.peek() == Some(Token::DoubleAmp)
    {
        let mut is_double_ref = false;
        let mut inner_ref_start = Index(1);

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

        let operand = unary(tokens, parse_method_calls)?.map(Box::new);

        if is_double_ref {
            let inner_span = Span::new(inner_ref_start, operand.span.end());

            // wrap in the `&` that is part of `&&` or `&&mut` and was ignored above
            Ok(Spanned::new(
                Expr::un_op(UnOp {
                    op: UnOpKind::AddrOf,
                    operand: Spanned::new(Box::new(Expr::un_op(UnOp { op, operand })), inner_span),
                }),
                span_start.end(),
            ))
        } else {
            Ok(Spanned::new(
                Expr::un_op(UnOp { op, operand }),
                span_start.end(),
            ))
        }
    } else {
        method_call(tokens, parse_method_calls)
    }
}

fn method_call(
    tokens: &TokenStream<'_>,
    parse_method_calls: bool,
) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut object = fn_call_expr(tokens)?;

    while parse_method_calls && is_fn_call(tokens) {
        let mut call = fn_call(tokens)?;

        // fix the span and insert the object as auto-ref first arg
        call.span = span_start.end();

        let arg_span = object.span;
        let arg = Spanned::new(
            Arg {
                name: None,
                value: object.map(|obj| Expr::auto_ref(Box::new(obj))),
            },
            arg_span,
        );

        call.value.args.insert(0, arg);
        object = call.map(Expr::fn_call);
    }

    Ok(object)
}

fn fn_call_expr(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    if is_fn_call(tokens) {
        fn_call(tokens).map(|spanned| spanned.map(Expr::fn_call))
    } else {
        member_access(tokens)
    }
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
            // calls with a `:` must have at least one argument, which is not
            // possible if it immediately followed by another call
            // catching this here instead of when parsing the argument produces better
            // errors because the parser does not try to parse the function name as variable
            if let Some(span) = try_fn_call_head(tokens) {
                return Err(Spanned::new(
                    ParseError::new()
                        .msg("unexpected function call")
                        .expected("at least one argument"),
                    span,
                ));
            }

            let mut args = vec![fn_arg(tokens)?];

            while let Some(Token::Comma) = tokens.peek() {
                tokens.next();

                // explicitly skip fn calls since belong to a method call; this avoids
                // parsing the name of the function as variable when the call has a trailing
                // comma
                if is_fn_call(tokens) {
                    break;
                }

                let pos = tokens.pos();
                // if the following tokens cannot be parsed as expression, assume the comma is trailing
                match fn_arg(tokens) {
                    Ok(arg) => args.push(arg),
                    Err(_) => {
                        tokens.restore_pos(pos);
                        break;
                    }
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

    Ok(Spanned::new(ItemPath::from(segments), span_start.end()))
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

            expr_in_fn_arg(tokens).map(|expr| {
                Spanned::new(
                    Arg {
                        name: Some(Spanned::new(ident.clone(), ident_span)),
                        value: expr,
                    },
                    span_start.end(),
                )
            })
        }
        _ => expr_in_fn_arg(tokens).map(|expr| {
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

/// Returns true if the next tokens are a function call without consuming any tokens.
/// For example, possible calls would be: `function_name:`, `function_name!` or `long::path::with::function:`.
fn is_fn_call(tokens: &TokenStream<'_>) -> bool {
    try_fn_call_head(tokens).is_some()
}

fn try_fn_call_head(tokens: &TokenStream<'_>) -> Option<Span> {
    let span_start = tokens.start_span();
    let pos = tokens.pos();

    let maybe_span = if item_path(tokens).is_ok()
        && (tokens.peek() == Some(Token::Colon) || tokens.peek() == Some(Token::Bang))
    {
        tokens.next();
        Some(span_start.end())
    } else {
        None
    };

    tokens.restore_pos(pos);
    maybe_span
}

fn member_access(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();
    let mut value = atom_or_group(tokens)?;

    while tokens.peek() == Some(Token::Dot) || tokens.peek() == Some(Token::Arrow) {
        let is_deref = match tokens.next() {
            Some(Token::Dot) => false,
            Some(Token::Arrow) => true,
            _ => unreachable!(),
        };

        // desugar the `->` member access after deref
        if is_deref {
            value = Spanned::new(
                Expr::un_op(UnOp {
                    op: UnOpKind::Deref,
                    operand: value.map(Box::new),
                }),
                span_start.end(),
            )
        }

        let member = ident(tokens)?;

        value = Spanned::new(
            Expr::member_access(MemberAccess {
                value: value.map(Box::new),
                member,
            }),
            span_start.end(),
        );
    }

    Ok(value)
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
        Some(Token::Lit(lit)) => Ok(Expr::lit(lit)),
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
    item_path(tokens)
        .map(|ident| Expr::var(ident.value))
        .map_err(|spanned| spanned.map(|err| err.set_expected("variable")))
}

fn array_cons(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::OpenBracket, "[")?;
    let mut elems = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseBracket) => {
                tokens.next();
                return Ok(Expr::array_cons(ArrayCons { elems }));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("]"),
                    tokens.eof_span(),
                ));
            }
        }

        elems.push(expr(tokens)?);

        match tokens.next() {
            Some(Token::CloseBracket) => {
                return Ok(Expr::array_cons(ArrayCons { elems }));
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token().expected("]").expected(","),
                    tokens.last_token_span(),
                ));
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected("]").expected(","),
                    tokens.eof_span(),
                ));
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
                return Ok(Expr::tuple_cons(TupleCons { elems }));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")"),
                    tokens.eof_span(),
                ));
            }
        }

        elems.push(expr(tokens)?);

        match tokens.next() {
            Some(Token::CloseParen) => {
                // one expression in parenthesis without a comma is just for precedence
                if elems.len() == 1 {
                    return Ok(elems.pop().unwrap().value);
                } else {
                    return Ok(Expr::tuple_cons(TupleCons { elems }));
                }
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token().expected(")").expected(","),
                    tokens.last_token_span(),
                ));
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")").expected(","),
                    tokens.eof_span(),
                ));
            }
        }
    }
}

fn ret_expr(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Ret), "ret")?;

    match tokens.peek() {
        Some(Token::Semicolon) | Some(Token::CloseBrace) => Ok(Expr::ret(None)),
        _ => {
            let expr = expr(tokens)?.map(Box::new);
            Ok(Expr::ret(Some(expr)))
        }
    }
}

fn if_expr(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::If), "if")?;

    let cond = expr(tokens)?.map(Box::new);
    let then_block = block(tokens)?.map(Box::new);

    let else_block = match tokens.peek() {
        Some(Token::Keyword(Keyword::Else)) => {
            tokens.next();

            match tokens.peek() {
                Some(Token::Keyword(Keyword::If)) => {
                    let span_start = tokens.start_span();
                    let if_expr = if_expr(tokens)?;
                    let span = span_start.end();

                    Some(Spanned::new(
                        Box::new(Expr::block(Block {
                            exprs: vec![Spanned::new(if_expr, span)],
                            is_last_expr_stmt: false,
                        })),
                        span,
                    ))
                }
                _ => Some(block(tokens)?.map(Box::new)),
            }
        }
        _ => None,
    };

    Ok(Expr::if_expr(IfExpr {
        cond,
        then_block,
        else_block,
    }))
}

fn block_expr(tokens: &TokenStream<'_>) -> Result<Expr, Spanned<ParseError>> {
    block(tokens).map(|spanned| spanned.value)
}

fn block(tokens: &TokenStream<'_>) -> Result<Spanned<Expr>, Spanned<ParseError>> {
    let span_start = tokens.start_span();

    token(tokens, Token::OpenBrace, "{")?;
    let mut exprs = Vec::new();
    let mut is_last_expr_stmt = false;

    while tokens.peek() != Some(Token::CloseBrace) {
        let expr = expr(tokens)?;

        let is_block_delimited = match expr.value {
            Expr::Block(..) | Expr::IfExpr(..) => true,
            _ => false,
        };

        exprs.push(expr);

        if tokens.peek() == Some(Token::Semicolon) {
            tokens.next();
            is_last_expr_stmt = true;
        } else {
            is_last_expr_stmt = false;

            if !is_block_delimited {
                break;
            }
        }
    }

    token(tokens, Token::CloseBrace, "}")?;

    Ok(Spanned::new(
        Expr::block(Block {
            exprs,
            is_last_expr_stmt,
        }),
        span_start.end(),
    ))
}

fn pattern(tokens: &TokenStream<'_>) -> Result<Spanned<Pattern>, Spanned<ParseError>> {
    one_of(tokens, &[discard_pattern, binding_pattern, tuple_pattern])
}

fn discard_pattern(tokens: &TokenStream<'_>) -> Result<Pattern, Spanned<ParseError>> {
    token(tokens, Token::Underscore, "_")?;
    Ok(Pattern::discard())
}

fn binding_pattern(tokens: &TokenStream<'_>) -> Result<Pattern, Spanned<ParseError>> {
    match tokens.peek() {
        Some(Token::Keyword(Keyword::Mut)) => {
            tokens.next();

            ident(tokens)
                .map(Pattern::mut_binding)
                .map_err(|spanned| spanned.map(|err| err.set_expected("binding pattern")))
        }
        Some(Token::Ident(_)) => ident(tokens)
            .map(Pattern::binding)
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
    token(tokens, Token::OpenParen, "(")?;

    let mut patterns = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseParen) => {
                tokens.next();
                return Ok(Pattern::tuple(patterns));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")"),
                    tokens.eof_span(),
                ));
            }
        }

        patterns.push(pattern(tokens)?);

        match tokens.next() {
            Some(Token::CloseParen) => {
                return Ok(Pattern::tuple(patterns));
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token().expected(")").expected(","),
                    tokens.last_token_span(),
                ));
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")").expected(","),
                    tokens.eof_span(),
                ));
            }
        }
    }
}

fn type_decl(tokens: &TokenStream<'_>) -> Result<Spanned<TypeDecl>, Spanned<ParseError>> {
    one_of(
        tokens,
        &[
            hole_type_decl,
            name_type_decl,
            ptr_type_decl,
            array_type_decl,
            function_type_decl,
            tuple_type_decl,
        ],
    )
}

fn hole_type_decl(tokens: &TokenStream<'_>) -> Result<TypeDecl, Spanned<ParseError>> {
    token(tokens, Token::Underscore, "_")?;
    Ok(TypeDecl::Hole)
}

fn name_type_decl(tokens: &TokenStream<'_>) -> Result<TypeDecl, Spanned<ParseError>> {
    ident(tokens)
        .map(|ident| TypeDecl::Name(ident.value))
        .map_err(|spanned| spanned.map(|err| err.set_expected("type name")))
}

fn ptr_type_decl(tokens: &TokenStream<'_>) -> Result<TypeDecl, Spanned<ParseError>> {
    token(tokens, Token::Star, "*")?;

    match tokens.peek() {
        Some(Token::Keyword(Keyword::Mut)) => {
            tokens.next();
            Ok(TypeDecl::MutPtr(type_decl(tokens)?.map(Box::new)))
        }
        _ => Ok(TypeDecl::ConstPtr(type_decl(tokens)?.map(Box::new))),
    }
}

fn array_type_decl(tokens: &TokenStream<'_>) -> Result<TypeDecl, Spanned<ParseError>> {
    token(tokens, Token::OpenBracket, "[")?;
    let ty = type_decl(tokens)?.map(Box::new);
    token(tokens, Token::Semicolon, ";")?;

    let len = match tokens.next() {
        Some(Token::Lit(Lit::Int(len))) => Spanned::new(len, tokens.last_token_span()),
        Some(_) => {
            return Err(Spanned::new(
                ParseError::unexpected_token().expected("array length"),
                tokens.last_token_span(),
            ));
        }
        None => {
            return Err(Spanned::new(
                ParseError::eof().expected("array length"),
                tokens.eof_span(),
            ));
        }
    };

    token(tokens, Token::CloseBracket, "]")?;
    Ok(TypeDecl::Array(ArrayDecl { ty, len }))
}

fn function_type_decl(tokens: &TokenStream<'_>) -> Result<TypeDecl, Spanned<ParseError>> {
    token(tokens, Token::Keyword(Keyword::Fn), "fn")?;
    let mut param_tys = Vec::new();

    while tokens.peek() != Some(Token::Arrow) {
        param_tys.push(type_decl(tokens)?);

        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
            }
            _ => break,
        }
    }

    token(tokens, Token::Arrow, "->")?;
    let ret_ty = type_decl(tokens)?.map(Box::new);

    Ok(TypeDecl::Function(FunctionDecl { param_tys, ret_ty }))
}

fn tuple_type_decl(tokens: &TokenStream<'_>) -> Result<TypeDecl, Spanned<ParseError>> {
    token(tokens, Token::OpenParen, "(")?;
    let mut tys = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::CloseParen) => {
                tokens.next();
                return Ok(TypeDecl::Tuple(tys));
            }
            Some(_) => (),
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")"),
                    tokens.eof_span(),
                ));
            }
        }

        tys.push(type_decl(tokens)?);

        match tokens.next() {
            Some(Token::CloseParen) => {
                return Ok(TypeDecl::Tuple(tys));
            }
            Some(Token::Comma) => continue,
            Some(_) => {
                return Err(Spanned::new(
                    ParseError::unexpected_token().expected(")").expected(","),
                    tokens.last_token_span(),
                ));
            }
            None => {
                return Err(Spanned::new(
                    ParseError::eof().expected(")").expected(","),
                    tokens.eof_span(),
                ));
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

#[cfg(test)]
mod test {
    use crate::lexer;
    use crate::parser;

    #[test]
    fn parse_syntax_example() {
        let tokens = lexer::lex(include_str!("../tests/syntax.cml")).unwrap();
        parser::parse(&tokens).unwrap();
    }

    #[test]
    fn parse_pow_example() {
        let tokens = lexer::lex(include_str!("../tests/pow.cml")).unwrap();
        parser::parse(&tokens).unwrap();
    }

    #[test]
    fn parse_factorial_example() {
        let tokens = lexer::lex(include_str!("../tests/factorial.cml")).unwrap();
        parser::parse(&tokens).unwrap();
    }
}
