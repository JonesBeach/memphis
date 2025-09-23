use std::{
    fmt::{Debug, Display, Error, Formatter},
    time::Instant,
};

use crate::{
    bytecode_vm::runtime::{Frame, Reference},
    core::Container,
};

#[derive(Clone, Debug)]
pub enum CoroutineState {
    Ready,
    WaitingOn(Container<Coroutine>),
    SleepingUntil(Instant),
    Finished(Reference),
}

#[derive(Clone)]
pub struct Coroutine {
    pub frame: Frame,
    pub state: CoroutineState,
    pub waiters: Vec<Container<Coroutine>>,
}

impl Display for Coroutine {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<coroutine '{}'>", self.frame.function.name())
    }
}

impl Debug for Coroutine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{} (state: {:?})", self.frame.name(), self.state)
    }
}

impl Coroutine {
    pub fn new(frame: Frame) -> Self {
        Self {
            frame,
            state: CoroutineState::Ready,
            waiters: vec![],
        }
    }
}
