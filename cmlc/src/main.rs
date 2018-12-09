mod lexer;
mod span;

use crate::lexer::StrStream;

fn main() {
    let tokens = lexer::lex(StrStream::new(include_str!("../tests/syntax.cml"))).unwrap();
    println!("{:#?}", tokens);
}
