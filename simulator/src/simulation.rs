use memory::Memory;
use simulator::{self, Breakpoints, RegBank, Status};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use Error;

pub struct Simulation {
    state: Arc<Mutex<State>>,
    pause: Arc<AtomicBool>,
    start: Sender<Message>,
}

impl Simulation {
    pub fn new<F, G>(
        mem: Memory,
        breakpoints: Breakpoints,
        on_halt: F,
        mut on_pause: G,
    ) -> Simulation
    where
        F: 'static + FnOnce(Result<(), Error>) + Send,
        G: 'static + FnMut() + Send,
    {
        let (sender, receiver) = mpsc::channel();

        let simulation = Simulation {
            state: Arc::new(Mutex::new(State {
                regs: RegBank::new(),
                mem,
                breakpoints,
            })),
            pause: Arc::new(AtomicBool::new(false)),
            start: sender,
        };

        let pause = Arc::clone(&simulation.pause);
        let state = Arc::clone(&simulation.state);

        thread::spawn(move || {
            while let Ok(msg) = receiver.recv() {
                if msg == Message::Stop {
                    on_halt(Ok(()));
                    break;
                }

                let result = {
                    let mut state = state.lock().unwrap();
                    let state = &mut *state;
                    simulator::run(&mut state.regs, &mut state.mem, &state.breakpoints, &pause)
                };

                match result {
                    Ok(Status::Ready) => {
                        pause.store(false, Ordering::Release);
                        on_pause();
                        continue;
                    }
                    Ok(Status::Halt) => {
                        on_halt(Ok(()));
                        break;
                    }
                    Err(err) => {
                        on_halt(Err(err));
                        break;
                    }
                }
            }
        });

        simulation
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Message {
    Start,
    Stop,
}

struct State {
    regs: RegBank,
    mem: Memory,
    breakpoints: Breakpoints,
}
