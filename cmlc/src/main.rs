mod ast;
mod codegen;
mod emit;
mod lexer;
mod parser;
mod scope_map;
mod span;
mod support;
mod typecheck;

use crate::span::Spanned;
use codespan::{CodeMap, FileMap, FileName};
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{self, Diagnostic, Label, LabelStyle, Severity};
use std::fmt::{self, Display};
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
    Codegen(codegen::CodegenError),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;

        match *self {
            Lex(ref why) => write!(f, "{}", why),
            Parse(ref why) => write!(f, "{}", why),
            Type(ref why) => write!(f, "{}", why),
            Codegen(ref why) => write!(f, "{}", why),
        }
    }
}

/// Compiles cml programs
#[derive(StructOpt)]
struct Args {
    /// The source file to compile
    #[structopt(parse(from_os_str))]
    src: PathBuf,

    /// A file the compiler will use for the output
    #[structopt(short = "o", long = "--output", parse(from_os_str))]
    output: PathBuf,

    /// Only emit assembler code, do not compile to an image
    #[structopt(long = "--emit-asm")]
    emit_asm: bool,
}

fn main() {
    match run() {
        Ok(code) => process::exit(code),
        Err(why) => {
            eprintln!("{}", why);
            process::exit(-1);
        }
    }
}

fn run() -> Result<i32, io::Error> {
    let args = Args::from_args();

    let mut code_map = CodeMap::new();
    let src = fs::read_to_string(&args.src)?;
    let file_map = code_map.add_filemap(FileName::Real(args.src), src);

    match compile(&file_map, args.emit_asm) {
        Ok(output) => {
            fs::write(args.output, output)?;
            Ok(0)
        }
        Err(why) => {
            print_error(why, &code_map)?;
            Ok(1)
        }
    }
}

fn compile(file_map: &FileMap, emit_asm: bool) -> Result<Vec<u8>, Spanned<Error>> {
    let tokens = lexer::lex(file_map.src()).map_err(|spanned| spanned.map(Error::Lex))?;
    let ast = parser::parse(&tokens).map_err(|spanned| spanned.map(Error::Parse))?;
    let typed_ast = typecheck::typecheck(ast).map_err(|spanned| spanned.map(Error::Type))?;
    let asm = codegen::gen_asm(typed_ast).map_err(|spanned| spanned.map(Error::Codegen))?;

    if emit_asm {
        Ok(emit::emit_asm(asm).into_bytes())
    } else {
        Ok(emit::emit_object(asm))
    }
}

fn print_error<E>(error: Spanned<E>, code_map: &CodeMap) -> Result<(), io::Error>
where
    E: std::fmt::Display,
{
    let diag = Diagnostic {
        severity: Severity::Error,
        code: None,
        message: format!("{}", error.value),
        labels: vec![Label {
            span: error.span,
            message: None,
            style: LabelStyle::Primary,
        }],
    };

    codespan_reporting::emit(StandardStream::stderr(ColorChoice::Auto), code_map, &diag)
}
