mod asm;
mod memory;
mod simulation;
mod support;
mod vm;

use crate::asm::AsmError;
use crate::memory::{Access, Memory, Word};
use crate::simulation::{CtrlHandle, Request, Response, SimError};
use crate::vm::{Breakpoints, Reg, Status, VmError};
use linefeed::ReadResult;
use std::fmt::{self, Display};
use std::fs::{self, File};
use std::io::{self, BufReader};
use std::path::PathBuf;
use std::process;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use structopt::StructOpt;

static IS_TRACE_ENABLED: AtomicBool = AtomicBool::new(false);

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

impl Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::CliError::*;

        match *self {
            Io(ref why) => write!(f, "an IO error occurred: {}", why),
            Asm(ref why) => write!(f, "cannot convert text file to binary image: {}", why),
            Sim(ref why) => write!(f, "an error occurred in the simulator: {}", why),
            Vm(ref why) => write!(f, "{}", why),
            CannotJoinInputThread => write!(f, "cannot join on the input thread"),
            IllegalRegister => write!(f, "illegal register"),
            CannotParseWord(ref src) => write!(f, "cannot parse '{}' as word", src),
            UnknownCommand => write!(f, "unknown command"),
        }
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

    /// Dump the entire image to a file and exit
    #[structopt(short = "d", long = "dump-image", name = "path", parse(from_os_str))]
    dump_image: Option<PathBuf>,

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
            eprintln!("error: {}", why);
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

    if let Some(img_path) = args.dump_image {
        fs::write(img_path, &img)?;
        return Ok(());
    }

    let mut mem = Memory::new();
    mem.store(0, &img, Access::All).map_err(VmError::new)?;

    let printer = Arc::new(new_printer()?);
    let trace = tracer(Arc::clone(&printer));

    let sim = simulation::start(mem, Breakpoints::new(), args.start_paused, trace)?;
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
            Response::MemRange(bytes) => {
                displayln!(printer, "bytes: {}", support::to_hex_octets(&bytes));
                displayln!(printer, "utf-8: {}", String::from_utf8_lossy(&bytes));
            }
            Response::InvalidRequest(why) => displayln!(printer, "error: {}", why),
        }
    }
}

fn listen_for_input(printer: Arc<Printer>, sim: CtrlHandle) -> Result<JoinHandle<()>, CliError> {
    thread::Builder::new()
        .name("input".to_owned())
        .spawn(move || loop {
            let result = match printer.read_line() {
                Ok(line) => line,
                Err(why) => {
                    displayln!(printer, "error: {}", why);
                    sim.send(Request::Exit);
                    return;
                }
            };

            match result {
                ReadResult::Input(line) => match exec_command(&line, &printer, &sim) {
                    Ok(true) => return,
                    Ok(false) => printer.add_history(line),
                    Err(why) => displayln!(printer, "error: {}", why),
                },
                ReadResult::Eof | ReadResult::Signal(_) => {
                    sim.send(Request::Exit);
                    return;
                }
            }
        })
        .map_err(CliError::Io)
}

fn exec_command(line: &str, printer: &Printer, sim: &CtrlHandle) -> Result<bool, CliError> {
    let words: Vec<&str> = line
        .split(char::is_whitespace)
        .filter(|word| !word.is_empty())
        .collect();

    match &words[..] {
        &["continue"] | &["c"] => sim.send(Request::Continue),
        &["pause"] | &["p"] => sim.send(Request::Pause),
        &["enable_trace"] => IS_TRACE_ENABLED.store(true, Ordering::SeqCst),
        &["disable_trace"] => IS_TRACE_ENABLED.store(false, Ordering::SeqCst),
        &["reg", reg] => sim.send(Request::GetReg(
            reg.parse::<Reg>().map_err(|_| CliError::IllegalRegister)?,
        )),
        &["pc"] => sim.send(Request::GetNextInstrAddr),
        &["cmp_flag"] => sim.send(Request::GetCmpFlag),
        &["word", addr] => sim.send(Request::GetWord(parse_word(addr)?)),
        &["mem", start, end] => {
            sim.send(Request::GetMemRange(parse_word(start)?, parse_word(end)?))
        }
        &["set_break", addr] => sim.send(Request::SetBreakpoint(parse_word(addr)?)),
        &["remove_break", addr] => sim.send(Request::RemoveBreakpoint(parse_word(addr)?)),
        &["?"] | &["help"] => displayln!(printer, "{}", HELP),
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

fn tracer(printer: Arc<Printer>) -> impl 'static + Send + Sync + FnMut(Word) {
    move |instr| {
        if IS_TRACE_ENABLED.load(Ordering::SeqCst) {
            if let Some(instr) = vm::instr_to_string(instr) {
                displayln!(printer, "> {}", instr);
            }
        }
    }
}

const HELP: &str = r#"Inspect and interact with the simulator.

All commands interacting with the simulators state require the simulation to be paused.
Numeric literals are interpreted as decimal, but can be prefixed with `0x` for hexadecimal
or `0b` for binary.

c | continue            Continue execution if paused
p | pause               Pause execution if currently running
enable_trace            Log each executed instruction
disable_trace           Disable logging of instructions
reg <register>          Display the contents of `register`
pc                      Display the value of the program counter
cmp_flag                Display the value of the compare flag
word <addr>             Display the value of a word in memory starting at `addr`
mem <start> <end>       Display the contents of the memory starting at `start` (inclusive) and
                        ending at `end` (exclusive)
set_break <addr>        Set a breakpoint at `addr`
remove_break <addr>     Remove all breakpoints at `addr`
? | help                Display this help message
exit                    Exit the simulator
"#;
