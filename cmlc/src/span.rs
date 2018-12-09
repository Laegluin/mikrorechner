use codespan::ByteSpan;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Spanned<T> {
    value: T,
    span: ByteSpan,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: ByteSpan) -> Spanned<T> {
        Spanned { value, span }
    }
}
