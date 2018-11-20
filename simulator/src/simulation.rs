use memory::Memory;
use std::fmt::{self, Display};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use vm::{self, Breakpoints, RegBank, Status};
use Error;

pub struct Simulation {
    pause: Arc<AtomicBool>,
    sender: Sender<Message>,
}

impl Simulation {
    pub fn new<F, G>(
        mem: Memory,
        breakpoints: Breakpoints,
        on_halt: F,
        mut on_pause: G,
    ) -> Simulation
    where
        F: 'static + FnOnce(Arc<Mutex<State>>, Option<Error>) + Send,
        G: 'static + FnMut(Arc<Mutex<State>>) + Send,
    {
        let (sender, receiver) = mpsc::channel();

        let simulation = Simulation {
            pause: Arc::new(AtomicBool::new(false)),
            sender,
        };

        let state = Arc::new(Mutex::new(State {
            regs: RegBank::new(),
            mem,
            breakpoints,
        }));

        let pause = Arc::clone(&simulation.pause);

        thread::spawn(move || {
            while let Ok(msg) = receiver.recv() {
                if msg == Message::Stop {
                    on_halt(state, None);
                    break;
                }

                let result = {
                    let mut state = state.lock().unwrap();
                    let state = &mut *state;
                    vm::run(&mut state.regs, &mut state.mem, &state.breakpoints, &pause)
                };

                match result {
                    Ok(Status::Ready) => {
                        pause.store(false, Ordering::Release);
                        on_pause(Arc::clone(&state));
                        continue;
                    }
                    Ok(Status::Halt) => {
                        on_halt(state, None);
                        break;
                    }
                    Err(err) => {
                        on_halt(state, Some(err));
                        break;
                    }
                }
            }
        });

        simulation
    }

    pub fn start(&self) -> Result<(), SimulationHaltedError> {
        self.sender
            .send(Message::Start)
            .map_err(|_| SimulationHaltedError)
    }

    pub fn pause(&self) {
        self.pause.store(true, Ordering::Release);
    }

    pub fn stop(&self) {
        self.pause();

        // if there's no receiver, it's already stopped
        let _ = self.sender.send(Message::Stop);
    }
}

pub struct SimulationHaltedError;

pub struct State {
    regs: RegBank,
    mem: Memory,
    breakpoints: Breakpoints,
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Registers:")?;
        writeln!(f, "{}", self.regs)?;
        writeln!(f, "Breakpoints:")?;
        write!(f, "{}", self.breakpoints)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Message {
    Start,
    Stop,
}
