use std::hash::{Hash, Hasher};

pub use codespan::ByteIndex as Index;
pub use codespan::ByteOffset as Offset;
pub use codespan::ByteSpan as Span;

#[derive(Debug, Clone, Copy)]
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

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned::new(&self.value, self.span)
    }

    pub fn as_mut(&mut self) -> Spanned<&mut T> {
        Spanned::new(&mut self.value, self.span)
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T> PartialEq for Spanned<T>
where
    T: PartialEq,
{
    fn eq(&self, rhs: &Spanned<T>) -> bool {
        self.value.eq(&rhs.value)
    }
}

impl<T> Eq for Spanned<T> where T: Eq {}

impl<T> Hash for Spanned<T>
where
    T: Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.value.hash(state);
    }
}
