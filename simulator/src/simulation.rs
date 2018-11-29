use memory::Memory;
use std::fmt::{self, Display};
use std::io;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Sender};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::{self, JoinHandle};
use vm::{self, Breakpoints, RegBank, Status};
use Error;

const SIM_THREAD_NAME: &str = "simulation";
const CONTROLLER_THREAD_NAME: &str = "controller";

pub enum SimError {
    Io(io::Error),
    ThreadPanicked(&'static str),
    Vm(Error),
}

impl From<io::Error> for SimError {
    fn from(err: io::Error) -> SimError {
        SimError::Io(err)
    }
}

impl From<Error> for SimError {
    fn from(err: Error) -> SimError {
        SimError::Vm(err)
    }
}

/// Immediately starts the simulation and blocks this thread until the VM halts.
pub fn run(mem: Memory, breakpoints: Breakpoints) -> Result<(RegBank, Memory), SimError> {
    let handle = start(mem, breakpoints, false)?;

    /// FIXME: exit on halt

    handle.join()
}

pub fn start(
    mem: Memory,
    breakpoints: Breakpoints,
    start_paused: bool,
) -> Result<SimHandle, SimError> {
    let signals = Arc::new(SimSignals {
        pause: AtomicBool::new(start_paused),
        stop: AtomicBool::new(false),
        cont: Condvar::new(),
    });

    let state = Arc::new(Mutex::new(State {
        regs: RegBank::new(),
        mem,
        breakpoints,
        status: Status::Ready,
    }));

    let simulation = Simulation::start(Arc::clone(&signals), Arc::clone(&state))?;
    let controller = Controller::start(signals, state)?;

    Ok(SimHandle {
        sim: simulation,
        controller,
    })
}

// TODO: add receiver for responses
pub struct SimHandle {
    sim: Simulation,
    controller: Controller,
}

impl SimHandle {
    pub fn join(self) -> Result<(RegBank, Memory), SimError> {
        self.controller
            .handle
            .join()
            .map_err(|_| SimError::ThreadPanicked(CONTROLLER_THREAD_NAME))??;

        self.sim
            .handle
            .join()
            .map_err(|_| SimError::ThreadPanicked(SIM_THREAD_NAME))
            .and_then(|state| {
                // other threads joined, so this is the last Arc
                let state = Arc::try_unwrap(state?).ok().unwrap();
                let state = Mutex::into_inner(state)
                    .map_err(|_| SimError::ThreadPanicked(SIM_THREAD_NAME))?;

                Ok((state.regs, state.mem))
            })
    }
}

struct Simulation {
    handle: JoinHandle<Result<Arc<Mutex<State>>, SimError>>,
    signals: Arc<SimSignals>,
}

struct SimSignals {
    pause: AtomicBool,
    stop: AtomicBool,
    cont: Condvar,
}

impl Simulation {
    fn start(signals: Arc<SimSignals>, state: Arc<Mutex<State>>) -> Result<Simulation, SimError> {
        let signals_clone = Arc::clone(&signals);

        let handle = thread::Builder::new()
            .name(SIM_THREAD_NAME.to_owned())
            .spawn(move || {
                let mut state_guard = state.lock().unwrap();

                loop {
                    {
                        let state = &mut *state_guard;

                        let status = vm::run(
                            &mut state.regs,
                            &mut state.mem,
                            &state.breakpoints,
                            &signals.pause,
                        )?;

                        state.status = status;
                    }

                    // yield lock to the controlling thread
                    state_guard = signals.cont.wait(state_guard).unwrap();

                    // abort if controlling thread set the stop flag
                    if signals.stop.load(Ordering::SeqCst) {
                        return Ok(Arc::clone(&state));
                    }
                }
            })?;

        Ok(Simulation {
            handle,
            signals: signals_clone,
        })
    }
}

struct Controller {
    handle: JoinHandle<Result<(), Error>>,
    sender: Sender<Message>,
}

impl Controller {
    fn start(signals: Arc<SimSignals>, state: Arc<Mutex<State>>) -> Result<Controller, SimError> {
        let (sender, receiver) = mpsc::channel();

        let handle = thread::Builder::new()
            .name(CONTROLLER_THREAD_NAME.to_owned())
            .spawn(move || {
                loop {
                    // should never be closed before Message::Exit
                    match receiver.recv().unwrap() {
                        Message::Pause => signals.pause.store(true, Ordering::Release),
                        Message::Exit => {
                            signals.pause.store(true, Ordering::Release);
                            signals.stop.store(true, Ordering::SeqCst);
                            signals.cont.notify_one();
                        }
                    }
                }
            })?;

        Ok(Controller { handle, sender })
    }
}

enum Message {
    Pause,
    Exit,
}

pub struct State {
    regs: RegBank,
    mem: Memory,
    breakpoints: Breakpoints,
    status: Status,
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Registers:")?;
        writeln!(f, "{}", self.regs)?;
        writeln!(f, "Breakpoints:")?;
        write!(f, "{}", self.breakpoints)
    }
}
