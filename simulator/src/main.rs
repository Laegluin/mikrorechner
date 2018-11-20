extern crate byteorder;
extern crate num_enum;
extern crate rand;
extern crate strum;
extern crate strum_macros;
extern crate structopt;

mod memory;
mod simulator;
mod simulation;

use memory::Word;
use structopt::StructOpt;
use std::process;
use std::path::PathBuf;

#[derive(Debug)]
pub struct Error {
    at: Word,
    kind: ErrorKind,
}

impl Error {
    fn new(at: Word, kind: ErrorKind) -> Error {
        Error { at, kind }
    }
}

struct ErrorContext {
    at: Word,
}

impl ErrorContext {
    fn at(at: Word) -> ErrorContext {
        ErrorContext { at }
    }

    fn map_err<T>(&self, res: Result<T, ErrorKind>) -> Result<T, Error> {
        res.map_err(|kind| Error::new(self.at, kind))
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    IllegalInstruction(Word),
    IllegalRegister(u8),
    UninitializedMemoryAccess(Word),
}

#[derive(StructOpt)]
struct Args {
    #[structopt(parse(from_os_str))]
    /// The image that is loaded into memory before startup. Addressing
    /// starts at 0x00000000.
    image: PathBuf,
}

fn main() {
    match run(Args::from_args()) {
        Ok(_) => process::exit(0),
        Err(why) => {
            // TODO: impl Display for Error
            eprintln!("error: {:?}", why);
            process::exit(1);
        }
    }
}

fn run(args: Args) -> Result<(), Error> {
    Ok(())
}
