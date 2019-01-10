mod ast;
mod lexer;
mod parser;
mod span;
mod support;
mod typecheck;

use crate::span::Spanned;
use codespan::{CodeMap, FileMap, FileName};
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{self, Diagnostic, Label, LabelStyle, Severity};
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process;
use structopt::StructOpt;

#[derive(Debug)]
enum Error {
    Lex(lexer::LexError),
    Parse(parser::ParseError),
    Type(typecheck::TypeError),
}

#[derive(StructOpt)]
struct Args {
    /// The source file to compile
    #[structopt(parse(from_os_str))]
    src: PathBuf,
}

fn main() {
    match run() {
        Ok(true) => process::exit(0),
        Ok(false) => process::exit(1),
        Err(why) => {
            eprintln!("{}", why);
            process::exit(-1);
        }
    }
}

fn run() -> Result<bool, io::Error> {
    let args = Args::from_args();

    let mut code_map = CodeMap::new();
    let src = fs::read_to_string(&args.src)?;
    let file_map = code_map.add_filemap(FileName::Real(args.src), src);

    match compile(&file_map) {
        Ok(_) => Ok(true),
        Err(why) => {
            print_error(why, &code_map)?;
            Ok(false)
        }
    }
}

fn compile(file_map: &FileMap) -> Result<(), Spanned<Error>> {
    let tokens = lexer::lex(file_map.src()).map_err(|spanned| spanned.map(Error::Lex))?;
    let ast = parser::parse(&tokens).map_err(|spanned| spanned.map(Error::Parse))?;
    let typed_ast = typecheck::typecheck(ast).map_err(|spanned| spanned.map(Error::Type))?;
    println!("=> ast:");
    println!("{:#?}", typed_ast);

    Ok(())
}

// TODO: use Display
fn print_error<E>(error: Spanned<E>, code_map: &CodeMap) -> Result<(), io::Error>
where
    E: std::fmt::Debug,
{
    let diag = Diagnostic {
        severity: Severity::Error,
        code: None,
        message: format!("{:?}", error.value),
        labels: vec![Label {
            span: error.span,
            message: None,
            style: LabelStyle::Primary,
        }],
    };

    codespan_reporting::emit(StandardStream::stderr(ColorChoice::Auto), code_map, &diag)
}
