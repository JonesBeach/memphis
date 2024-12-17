use std::collections::VecDeque;

use crate::{
    core::Container,
    parser::types::{Expr, LoopIndex, Statement},
    treewalk::types::ExprResult,
};

use super::Frame;
#[allow(unused_imports)]
use super::Pausable;

/// An enumeration of the possible states in which a [`Pausable`] can exist. This is key to
/// implementing stack-based control flow.
#[derive(PartialEq, Clone, Debug)]
pub enum PausableState {
    Created,
    Running,
    InWhileLoop(Expr),
    InForLoop {
        index: LoopIndex,
        queue: Container<VecDeque<ExprResult>>,
    },
    InBlock,
    Finished,
}

/// The context that allows a [`Pausable`] to be paused and resumed. This represents an individual
/// [`Frame`] and its current [`PausableState`].
#[derive(Clone)]
pub struct PausableToken {
    frame: Frame,
    state: PausableState,
}

impl PausableToken {
    pub fn new(frame: Frame, state: PausableState) -> Self {
        Self { frame, state }
    }
}

/// The context that allows a [`Pausable`] to be paused and resumed. This represents a stack of
/// [`PausableToken`] objects.
pub struct PausableContext(Vec<PausableToken>);

impl PausableContext {
    pub fn new(initial_frame: Frame) -> Container<Self> {
        Container::new(Self(vec![PausableToken::new(
            initial_frame,
            PausableState::Created,
        )]))
    }
}

impl Container<PausableContext> {
    pub fn push_context(&mut self, context: PausableToken) {
        self.borrow_mut().0.push(context);
    }

    pub fn pop_context(&self) -> Option<PausableToken> {
        self.borrow_mut().0.pop()
    }

    pub fn set_state(&self, state: PausableState) {
        if let Some(context) = self.borrow_mut().0.last_mut() {
            context.state = state;
        }
    }

    pub fn next_statement(&self) -> Statement {
        self.borrow_mut()
            .0
            .last_mut()
            .map(|context| context.frame.next_statement())
            .unwrap()
    }

    pub fn current_frame(&self) -> Frame {
        self.borrow().0.last().unwrap().frame.clone()
    }

    pub fn current_state(&self) -> PausableState {
        self.borrow().0.last().unwrap().state.clone()
    }

    pub fn restart_frame(&self) {
        if let Some(context) = self.borrow_mut().0.last_mut() {
            context.frame.restart();
        }
    }

    pub fn start(&self) {
        self.set_state(PausableState::Running);
    }

    pub fn step_back(&self) {
        if let Some(context) = self.borrow_mut().0.last_mut() {
            context.frame.step_back();
        }
    }
}
