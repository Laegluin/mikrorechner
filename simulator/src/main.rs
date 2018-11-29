extern crate byteorder;
extern crate env_logger;
extern crate log;
extern crate num_enum;
extern crate rand;
extern crate structopt;
extern crate strum;
extern crate strum_macros;

mod asm;
mod memory;
mod simulation;
mod support;
mod vm;

use asm::AsmError;
use memory::{Memory, Word};
use simulation::Simulation;
use std::fmt::{self, Display};
use std::fs::{self, File};
use std::io::{self, BufReader};
use std::path::PathBuf;
use std::process;
use structopt::StructOpt;
use support::to_hex;
use vm::Breakpoints;

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

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "at: {}", to_hex(self.at))?;
        write!(f, "error: ")?;

        match self.kind {
            ErrorKind::IllegalInstruction(instr) => {
                write!(f, "illegal instruction: {}", to_hex(instr),)
            }
            ErrorKind::IllegalRegister(reg) => {
                write!(f, "illegal register: {:#0width$b}", reg, width = 8)
            }
            ErrorKind::UninitializedMemoryAccess(addr) => write!(
                f,
                "attempt to read from uninitialized memory at {}",
                to_hex(addr),
            ),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    IllegalInstruction(Word),
    IllegalRegister(u8),
    UninitializedMemoryAccess(Word),
}

#[derive(Debug)]
enum CliError {
    Io(io::Error),
    Asm(AsmError),
}

impl From<io::Error> for CliError {
    fn from(err: io::Error) -> CliError {
        CliError::Io(err)
    }
}

impl From<AsmError> for CliError {
    fn from(err: AsmError) -> CliError {
        CliError::Asm(err)
    }
}

#[derive(StructOpt)]
struct Args {
    #[structopt(parse(from_os_str))]
    /// The image that is loaded into memory before startup. Addressing
    /// starts at 0x00000000
    image: PathBuf,
    #[structopt(short = "c", long = "convert-from-text")]
    /// Load the image from a text instead of a binary file. The file
    /// must contain the binary representation of words, separated by newlines
    convert_from_text: bool,
}

fn main() {
    env_logger::init();

    match run(Args::from_args()) {
        Ok(_) => process::exit(0),
        Err(why) => {
            // TODO: add Display impl
            eprintln!("error: {:?}", why);
            process::exit(1);
        }
    }
}

fn run(args: Args) -> Result<(), CliError> {
    let img = if args.convert_from_text {
        let mut buf = Vec::new();
        asm::assemble(BufReader::new(File::open(args.image)?), &mut buf)?;
        buf
    } else {
        fs::read(args.image)?
    };

    let mut mem = Memory::new();
    mem.store(0, &img);

    let simulation = Simulation::new(
        mem,
        Breakpoints::new(),
        |state, err| {
            if let Some(err) = err {
                println!("{}", err);
                println!("");
            }

            println!("{}", state.lock().unwrap());
        },
        |state| println!("{}", state.lock().unwrap()),
    );

    let mut buf = String::new();
    while io::stdin().read_line(&mut buf)? > 0 {
        match buf.trim() {
            "s" => if let Err(_) = simulation.start() {
                eprintln!("error: simulation has been halted")
            },
            "p" => simulation.pause(),
            "q" => simulation.stop(),
            _ => eprintln!("error: unknown command"),
        }

        buf.clear();
    }

    Ok(())
}
