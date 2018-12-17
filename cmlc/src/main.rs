mod lexer;
mod ast;
mod span;
mod typecheck;

fn main() {
    let tokens = lexer::lex(include_str!("../tests/syntax.cml")).unwrap();
    println!("{:#?}", tokens);
}
