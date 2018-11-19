extern crate byteorder;
extern crate num_enum;
extern crate rand;
extern crate structopt;

mod memory;
mod simulator;
mod simulation;

use memory::Word;

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

pub enum ErrorKind {
    IllegalInstruction(Word),
    IllegalRegister(u8),
    UninitializedMemoryAccess(Word),
}

fn main() {
    println!("Hello, world!");
}
