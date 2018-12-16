mod lexer;
mod span;

fn main() {
    let tokens = lexer::lex(include_str!("../tests/syntax.cml")).unwrap();
    println!("{:#?}", tokens);
}
