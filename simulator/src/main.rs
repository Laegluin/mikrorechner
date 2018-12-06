extern crate byteorder;
extern crate crossbeam_channel;
extern crate env_logger;
extern crate log;
extern crate num_enum;
extern crate rand;
extern crate rustyline;
extern crate structopt;
extern crate strum;
extern crate strum_macros;

mod asm;
mod memory;
mod simulation;
mod support;
mod vm;

use asm::AsmError;
use memory::Memory;
use rustyline::Editor;
use simulation::{CtrlHandle, Request, Response, SimError};
use std::fs::{self, File};
use std::io::{self, BufReader};
use std::path::PathBuf;
use std::process;
use std::thread::{self, JoinHandle};
use structopt::StructOpt;
use vm::{Breakpoints, Status};

#[derive(Debug)]
pub enum CliError {
    Io(io::Error),
    Asm(AsmError),
    Sim(SimError),
    CannotJoinInputThread,
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

impl From<SimError> for CliError {
    fn from(err: SimError) -> CliError {
        CliError::Sim(err)
    }
}

#[derive(StructOpt)]
struct Args {
    /// The image that is loaded into memory before startup. Addressing
    /// starts at 0x00000000
    #[structopt(parse(from_os_str))]
    image: PathBuf,

    /// Load the image from a text instead of a binary file. The file
    /// must contain the binary representation of words, separated by newlines
    #[structopt(short = "c", long = "convert-from-text")]
    convert_from_text: bool,

    /// Start the simulation but immediately pause it before executing the first
    /// instruction.
    #[structopt(long = "start-paused")]
    start_paused: bool,
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

    let sim = simulation::start(mem, Breakpoints::new(), args.start_paused)?;
    let handle = listen_for_input(sim.ctrl_handle().clone())?;
    listen_for_events(sim.ctrl_handle());

    handle.join().map_err(|_| CliError::CannotJoinInputThread)?;
    sim.join()?;
    Ok(())
}

fn listen_for_events(sim: &CtrlHandle) {
    loop {
        match sim.recv() {
            Response::Pause(Status::Pause) => println!("▶ paused"),
            Response::Pause(Status::Break) => println!("▶ paused on breakpoint"),
            Response::Pause(Status::Halt) => println!("▶ halt"),
            Response::Exception(err) => println!("▶ error: {}", err),
            Response::Exit => return,
        }
    }
}

fn listen_for_input(sim: CtrlHandle) -> Result<JoinHandle<()>, CliError> {
    thread::Builder::new()
        .name("input".to_owned())
        .spawn(move || {
            let mut editor = Editor::<()>::new();
            for line in editor.iter(":: ") {
                let line = match line {
                    Ok(line) => line,
                    Err(why) => {
                        println!("error: {}", why);
                        sim.send(Request::Exit);
                        return;
                    }
                };

                if exec_command(line, &sim) {
                    return;
                }
            }
        }).map_err(CliError::Io)
}

fn exec_command(line: String, sim: &CtrlHandle) -> bool {
    let words: Vec<&str> = line
        .split(char::is_whitespace)
        .filter(|word| !word.is_empty())
        .collect();

    match &words[..] {
        &["continue"] | &["c"] => sim.send(Request::Continue),
        &["pause"] | &["p"] => sim.send(Request::Pause),
        &["exit"] => {
            sim.send(Request::Exit);
            return true;
        }
        _ => println!("error: unknown command"),
    }

    false
}
