use crate::memory::{Memory, Word};
use crate::vm::{self, Breakpoints, Reg, RegBank, Status, VmError};
use crossbeam_channel::{self, Receiver, Sender};
use std::fmt::{self, Display};
use std::io;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, MutexGuard};
use std::thread::{self, JoinHandle};

const SIM_THREAD_NAME: &str = "simulation";
const CONTROLLER_THREAD_NAME: &str = "controller";

#[derive(Debug)]
pub enum SimError {
    Io(io::Error),
    ThreadPanicked(&'static str),
    Vm(VmError),
}

impl From<io::Error> for SimError {
    fn from(err: io::Error) -> SimError {
        SimError::Io(err)
    }
}

impl From<VmError> for SimError {
    fn from(err: VmError) -> SimError {
        SimError::Vm(err)
    }
}

/// Immediately starts the simulation and blocks this thread until the VM halts.
#[allow(unused)]
pub fn run(mem: Memory, breakpoints: Breakpoints) -> Result<(RegBank, Memory), SimError> {
    let handle = start(mem, breakpoints, false)?;

    match handle.recv() {
        Response::Exception(err) => return Err(SimError::Vm(err)),
        Response::Pause(Status::Halt) => handle.send(Request::Exit),
        Response::Exit => (),
        // we never generate any requests that could cause other responses
        _ => unreachable!(),
    }

    handle.join()
}

pub fn start(
    mem: Memory,
    breakpoints: Breakpoints,
    start_paused: bool,
) -> Result<SimHandle, SimError> {
    let signals = Arc::new(SimSignals {
        pause: AtomicBool::new(start_paused),
        is_running: AtomicBool::new(false),
        stop: AtomicBool::new(false),
        cont: Condvar::new(),
    });

    let state = Arc::new(Mutex::new(State {
        regs: RegBank::new(),
        mem,
        breakpoints,
        status: Status::Pause,
    }));

    let ctrl = CtrlHandle::new();
    let sim_thread = SimThread::start(&ctrl, Arc::clone(&signals), Arc::clone(&state))?;
    let ctrl_thread = CtrlThread::start(&ctrl, signals, state)?;

    Ok(SimHandle {
        sim_thread,
        ctrl_thread,
        ctrl,
    })
}

pub struct SimHandle {
    sim_thread: SimThread,
    ctrl_thread: CtrlThread,
    ctrl: CtrlHandle,
}

impl SimHandle {
    pub fn send(&self, req: Request) {
        self.ctrl.send(req)
    }

    pub fn recv(&self) -> Response {
        self.ctrl.recv()
    }
}

impl SimHandle {
    pub fn join(self) -> Result<(RegBank, Memory), SimError> {
        self.ctrl_thread
            .handle
            .join()
            .map_err(|_| SimError::ThreadPanicked(CONTROLLER_THREAD_NAME))??;

        self.sim_thread
            .handle
            .join()
            .map_err(|_| SimError::ThreadPanicked(SIM_THREAD_NAME))
            .and_then(|state| {
                // other threads joined, so this is the last Arc
                let state = Arc::try_unwrap(state).ok().unwrap();
                let state = Mutex::into_inner(state)
                    .map_err(|_| SimError::ThreadPanicked(SIM_THREAD_NAME))?;

                Ok((state.regs, state.mem))
            })
    }

    pub fn ctrl_handle(&self) -> &CtrlHandle {
        &self.ctrl
    }
}

struct SimThread {
    handle: JoinHandle<Arc<Mutex<State>>>,
}

struct SimSignals {
    pause: AtomicBool,
    is_running: AtomicBool,
    stop: AtomicBool,
    cont: Condvar,
}

impl SimSignals {
    /// If set to true, the simulation will pause on the next instruction. While paused, the lock
    /// to the state is yielded.
    fn set_pause(&self, pause: bool) {
        self.pause.store(pause, Ordering::Release);
    }

    fn set_is_running(&self, is_running: bool) {
        self.is_running.store(is_running, Ordering::SeqCst);
    }

    fn is_running(&self) -> bool {
        self.is_running.load(Ordering::SeqCst)
    }

    /// If set to true, the simulation thread will exit. Note that for this condition to be
    /// checked, the simulation must be paused and then continued again.
    fn set_stop(&self, stop: bool) {
        self.stop.store(stop, Ordering::SeqCst);
    }

    /// Continue the simulation. The simulation has to reacquire the lock to the state before
    /// continuing.
    ///
    /// Takes a `MutexGuard` to prevent deadlocks: the state must be locked before notifying the
    /// simulation thread, otherwise the continue notification could happen before it actually yields
    /// (meaning the thread will wait forever).
    fn cont(&self, _: MutexGuard<'_, State>) {
        self.cont.notify_one();
    }
}

impl SimThread {
    fn start(
        ctrl: &CtrlHandle,
        signals: Arc<SimSignals>,
        state: Arc<Mutex<State>>,
    ) -> Result<SimThread, SimError> {
        let resp_sender = ctrl.resp_sender.clone();

        let handle = thread::Builder::new()
            .name(SIM_THREAD_NAME.to_owned())
            .spawn(move || {
                let mut state_guard = state.lock().unwrap();

                loop {
                    {
                        let state = &mut *state_guard;
                        signals.set_is_running(true);

                        let result = vm::run(
                            &mut state.regs,
                            &mut state.mem,
                            &state.breakpoints,
                            &signals.pause,
                        );

                        signals.set_is_running(false);

                        match result {
                            Ok(status) => {
                                // update status and send notification about the pause
                                state.status = status;
                                let _ = resp_sender.send(Response::Pause(status));
                            }
                            Err(why) => {
                                let _ = resp_sender.send(Response::Exception(why));
                            }
                        }
                    }

                    // yield lock to the controlling thread
                    state_guard = signals.cont.wait(state_guard).unwrap();

                    // abort if controlling thread set the stop flag
                    if signals.stop.load(Ordering::SeqCst) {
                        let _ = resp_sender.send(Response::Exit);
                        return Arc::clone(&state);
                    }
                }
            })?;

        Ok(SimThread { handle })
    }
}

struct CtrlThread {
    handle: JoinHandle<Result<(), VmError>>,
}

#[derive(Clone)]
pub struct CtrlHandle {
    req_sender: Sender<Request>,
    resp_receiver: Receiver<Response>,
    resp_sender: Sender<Response>,
    req_receiver: Receiver<Request>,
}

impl CtrlHandle {
    fn new() -> CtrlHandle {
        let (req_sender, req_receiver) = crossbeam_channel::unbounded();
        let (resp_sender, resp_receiver) = crossbeam_channel::unbounded();

        CtrlHandle {
            req_sender,
            resp_receiver,
            resp_sender,
            req_receiver,
        }
    }

    pub fn send(&self, req: Request) {
        self.req_sender.send(req).unwrap()
    }

    pub fn recv(&self) -> Response {
        self.resp_receiver.recv().unwrap()
    }
}

impl CtrlThread {
    fn start(
        ctrl: &CtrlHandle,
        signals: Arc<SimSignals>,
        state: Arc<Mutex<State>>,
    ) -> Result<CtrlThread, SimError> {
        let req_receiver = ctrl.req_receiver.clone();
        let sender = ctrl.resp_sender.clone();
        let send = move |resp| sender.send(resp).unwrap();

        let handle = thread::Builder::new()
            .name(CONTROLLER_THREAD_NAME.to_owned())
            .spawn(move || {
                loop {
                    // sender should never be closed before Request::Exit
                    match req_receiver.recv().unwrap() {
                        Request::Continue => {
                            signals.set_pause(false);
                            signals.cont(state.lock().unwrap());
                        }
                        Request::Pause => {
                            signals.set_pause(true);
                        }
                        Request::Exit => {
                            signals.set_pause(true);
                            signals.set_stop(true);
                            signals.cont(state.lock().unwrap());
                            return Ok(());
                        }
                        Request::GetReg(reg) => {
                            if !signals.is_running() {
                                send(Response::RegValue(state.lock().unwrap().regs[reg]));
                            } else {
                                send(Response::InvalidRequest(RequestError::SimulationNotPaused));
                            }
                        }
                        Request::GetWord(addr) => {
                            let mut state = state.lock().unwrap();

                            match state.mem.load_word(addr) {
                                Ok(val) => send(Response::WordValue(val)),
                                Err(kind) => send(Response::InvalidRequest(RequestError::Vm(
                                    VmError::new(kind),
                                ))),
                            }
                        }
                        Request::GetMemRange(start, end) => {
                            if signals.is_running() {
                                send(Response::InvalidRequest(RequestError::SimulationNotPaused));
                                continue;
                            }

                            if start > end {
                                send(Response::InvalidRequest(
                                    RequestError::RangeStartOutOfBounds,
                                ));
                                continue;
                            }

                            let mut state = state.lock().unwrap();
                            let mut buf = vec![0; (end - start) as usize];

                            match state.mem.load(start, &mut buf) {
                                Ok(_) => send(Response::MemRange(Box::from(buf))),
                                Err(kind) => send(Response::InvalidRequest(RequestError::Vm(
                                    VmError::new(kind),
                                ))),
                            }
                        }
                    }
                }
            })?;

        Ok(CtrlThread { handle })
    }
}

pub enum Request {
    Continue,
    Pause,
    Exit,
    GetReg(Reg),
    GetWord(Word),
    GetMemRange(Word, Word),
}

pub enum Response {
    Exit,
    Pause(Status),
    Exception(VmError),
    RegValue(Word),
    WordValue(Word),
    MemRange(Box<[u8]>),
    InvalidRequest(RequestError),
}

pub enum RequestError {
    SimulationNotPaused,
    RangeStartOutOfBounds,
    Vm(VmError),
}

impl Display for RequestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::RequestError::*;

        match *self {
            SimulationNotPaused => write!(f, "simulation is not paused"),
            RangeStartOutOfBounds => write!(f, "start of memory range is greater than end"),
            Vm(ref why) => write!(f, "{}", why),
        }
    }
}

pub struct State {
    regs: RegBank,
    mem: Memory,
    breakpoints: Breakpoints,
    status: Status,
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Registers:")?;
        writeln!(f, "{}", self.regs)?;
        writeln!(f, "Breakpoints:")?;
        write!(f, "{}", self.breakpoints)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::asm;
    use crate::vm::Reg;
    use std::io::Cursor;

    #[test]
    fn sum() {
        let src = Cursor::new(&include_bytes!("../tests/sum.img.txt")[..]);
        let mut img = Vec::new();
        asm::assemble(src, &mut img).unwrap();

        let mut mem = Memory::new();
        mem.store(0, &img).unwrap();
        let (regs, _) = run(mem, Breakpoints::new()).unwrap();

        assert_eq!(regs[Reg::R0], 500);
        assert_eq!(regs[Reg::R1], 100);
    }
}
