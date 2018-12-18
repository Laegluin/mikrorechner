pub use codespan::ByteIndex as Index;
pub use codespan::ByteOffset as Offset;
pub use codespan::ByteSpan as Span;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned { value, span }
    }

    pub fn map<U>(self, mapper: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: mapper(self.value),
            span: self.span,
        }
    }
}
