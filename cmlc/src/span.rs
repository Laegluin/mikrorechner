pub use codespan::ByteIndex as Index;
pub use codespan::ByteOffset as Offset;
pub use codespan::ByteSpan as Span;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Spanned<T> {
    value: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned { value, span }
    }
}
