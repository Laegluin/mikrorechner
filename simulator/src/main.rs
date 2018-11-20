extern crate byteorder;
extern crate num_enum;
extern crate rand;
extern crate structopt;
extern crate strum;
extern crate strum_macros;

mod memory;
mod simulation;
mod vm;

use memory::{Memory, Word, WORD_HEX_FMT_WIDTH};
use simulation::Simulation;
use std::fmt::{self, Display};
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process;
use structopt::StructOpt;
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
        writeln!(f, "at: {:#0width$x}", self.at, width = WORD_HEX_FMT_WIDTH)?;
        write!(f, "error: ")?;

        match self.kind {
            ErrorKind::IllegalInstruction(instr) => write!(
                f,
                "illegal instruction: {:#0width$x}",
                instr,
                width = WORD_HEX_FMT_WIDTH
            ),
            ErrorKind::IllegalRegister(reg) => {
                write!(f, "illegal register: {:#0width$b}", reg, width = 8)
            }
            ErrorKind::UninitializedMemoryAccess(addr) => write!(
                f,
                "attempt to read from uninitialized memory at {:#0width$x}",
                addr,
                width = WORD_HEX_FMT_WIDTH
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

#[derive(StructOpt)]
struct Args {
    #[structopt(parse(from_os_str))]
    /// The image that is loaded into memory before startup. Addressing
    /// starts at 0x00000000
    image: PathBuf,
}

fn main() {
    match run(Args::from_args()) {
        Ok(_) => process::exit(0),
        Err(why) => {
            eprintln!("error: {}", why);
            process::exit(1);
        }
    }
}

fn run(args: Args) -> Result<(), io::Error> {
    let mut mem = Memory::new();
    let img = fs::read(args.image)?;
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
