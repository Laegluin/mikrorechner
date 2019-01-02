mod ast;
mod lexer;
mod parser;
mod span;
mod support;
mod typecheck;

fn main() {
    let tokens = lexer::lex(include_str!("../tests/syntax.cml")).unwrap();
    println!("=> tokens:");
    println!("{:#?}", tokens);
    let ast = parser::parse(&tokens).unwrap();
    let typed_ast = typecheck::typecheck(ast).unwrap();
    println!("=> ast:");
    println!("{:#?}", typed_ast);
}
