mod asm;
mod memory;
mod simulation;
mod support;
mod vm;

use crate::asm::AsmError;
use crate::memory::{Memory, Word};
use crate::simulation::{CtrlHandle, Request, Response, SimError};
use crate::vm::{Breakpoints, Reg, Status, VmError};
use linefeed::ReadResult;
use std::fs::{self, File};
use std::io::{self, BufReader};
use std::path::PathBuf;
use std::process;
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use structopt::StructOpt;

#[derive(Debug)]
pub enum CliError {
    Io(io::Error),
    Asm(AsmError),
    Sim(SimError),
    Vm(VmError),
    CannotJoinInputThread,
    IllegalRegister,
    CannotParseWord(String),
    UnknownCommand,
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

impl From<VmError> for CliError {
    fn from(err: VmError) -> CliError {
        CliError::Vm(err)
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
    mem.store(0, &img).map_err(VmError::new)?;

    let sim = simulation::start(mem, Breakpoints::new(), args.start_paused)?;

    let printer = Arc::new(new_printer()?);
    let handle = listen_for_input(Arc::clone(&printer), sim.ctrl_handle().clone())?;
    listen_for_events(printer, sim.ctrl_handle());

    handle.join().map_err(|_| CliError::CannotJoinInputThread)?;
    sim.join()?;
    Ok(())
}

type Printer = linefeed::Interface<linefeed::DefaultTerminal>;

fn new_printer() -> Result<Printer, io::Error> {
    let printer = linefeed::Interface::new("simulator")?;

    printer.set_prompt(":: ")?;
    printer.set_report_signal(linefeed::Signal::Interrupt, true);
    printer.set_history_size(1000);

    Ok(printer)
}

macro_rules! displayln {
    ($printer: expr, $msg: expr) => {
        displayln!($printer, $msg,);
    };

    ($printer: expr, $msg: expr, $($fmt_args: tt)*) => {
        writeln!($printer.lock_writer_erase().unwrap(), $msg, $($fmt_args)*).unwrap();
    };
}

fn listen_for_events(printer: Arc<Printer>, sim: &CtrlHandle) {
    loop {
        match sim.recv() {
            Response::Pause(Status::Pause) => displayln!(printer, "▶ paused"),
            Response::Pause(Status::Break) => displayln!(printer, "▶ paused on breakpoint"),
            Response::Pause(Status::Halt) => displayln!(printer, "▶ halt"),
            Response::Exception(why) => displayln!(printer, "▶ error: {}", why),
            Response::Exit => return,
            Response::RegValue(val) | Response::WordValue(val) => {
                displayln!(printer, "{} ({})", support::to_hex(val), val)
            }
            Response::NextInstrAddrValue(addr) => displayln!(printer, "{}", support::to_hex(addr)),
            Response::CmpFlagValue(flag) => displayln!(printer, "{}", flag),
            Response::MemRange(bytes) => displayln!(printer, "{}", support::to_hex_octets(&bytes)),
            Response::InvalidRequest(why) => displayln!(printer, "error: {}", why),
        }
    }
}

fn listen_for_input(printer: Arc<Printer>, sim: CtrlHandle) -> Result<JoinHandle<()>, CliError> {
    thread::Builder::new()
        .name("input".to_owned())
        .spawn(move || {
            loop {
                let result = match printer.read_line() {
                    Ok(line) => line,
                    Err(why) => {
                        displayln!(printer, "error: {}", why);
                        sim.send(Request::Exit);
                        return;
                    }
                };

                match result {
                    ReadResult::Input(line) => {
                        match exec_command(&line, &sim) {
                            Ok(true) => return,
                            Ok(false) => printer.add_history(line),
                            // TOOD: impl Display
                            Err(why) => displayln!(printer, "error: {:?}", why),
                        }
                    }
                    ReadResult::Eof | ReadResult::Signal(_) => {
                        sim.send(Request::Exit);
                        return;
                    }
                }
            }
        })
        .map_err(CliError::Io)
}

fn exec_command(line: &str, sim: &CtrlHandle) -> Result<bool, CliError> {
    let words: Vec<&str> = line
        .split(char::is_whitespace)
        .filter(|word| !word.is_empty())
        .collect();

    match &words[..] {
        &["continue"] | &["c"] => sim.send(Request::Continue),
        &["pause"] | &["p"] => sim.send(Request::Pause),
        &["reg", reg] => sim.send(Request::GetReg(
            reg.parse::<Reg>().map_err(|_| CliError::IllegalRegister)?,
        )),
        &["pc"] => sim.send(Request::GetNextInstrAddr),
        &["cmp_flag"] => sim.send(Request::GetCmpFlag),
        &["word", addr] => sim.send(Request::GetWord(parse_word(addr)?)),
        &["mem", start, end] => {
            sim.send(Request::GetMemRange(parse_word(start)?, parse_word(end)?))
        }
        &["set_break", addr] | &["set_breakpoint", addr] => {
            sim.send(Request::SetBreakpoint(parse_word(addr)?))
        }
        &["remove_break", addr] | &["remove_breakpoint", addr] => {
            sim.send(Request::RemoveBreakpoint(parse_word(addr)?))
        }
        &["exit"] => {
            sim.send(Request::Exit);
            return Ok(true);
        }
        _ => return Err(CliError::UnknownCommand),
    }

    Ok(false)
}

fn parse_word(word: &str) -> Result<Word, CliError> {
    let result = if word.starts_with("0x") {
        Word::from_str_radix(&word[2..], 16)
    } else if word.starts_with("0b") {
        Word::from_str_radix(&word[2..], 2)
    } else {
        Word::from_str_radix(word, 10)
    };

    result.map_err(|_| CliError::CannotParseWord(word.to_owned()))
}
