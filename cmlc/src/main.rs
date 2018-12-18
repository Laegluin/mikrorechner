mod ast;
mod lexer;
mod parser;
mod span;
mod typecheck;

fn main() {
    let tokens = lexer::lex(include_str!("../tests/syntax.cml")).unwrap();
    let ast = parser::parse(&tokens).unwrap();
    println!("{:#?}", ast);
}
