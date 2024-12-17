use std::collections::VecDeque;

use crate::{
    core::{Container, RwStack},
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
    pub(crate) fn new(frame: Frame, state: PausableState) -> Self {
        Self { frame, state }
    }
}

/// The context that allows a [`Pausable`] to be paused and resumed. This represents a stack of
/// [`PausableToken`] objects.
pub struct PausableContext(RwStack<PausableToken>);

impl PausableContext {
    pub(crate) fn new(initial_frame: Frame) -> Container<Self> {
        Container::new(Self(RwStack::new(vec![PausableToken::new(
            initial_frame,
            PausableState::Created,
        )])))
    }
}

impl Container<PausableContext> {
    pub(crate) fn push_context(&mut self, context: PausableToken) {
        self.borrow_mut().0.push(context);
    }

    pub(crate) fn pop_context(&self) -> Option<PausableToken> {
        self.borrow_mut().0.pop()
    }

    pub(crate) fn set_state(&self, state: PausableState) {
        self.borrow_mut().0.with_top_mut(|context| {
            context.state = state;
        });
    }

    pub(crate) fn next_statement(&self) -> Statement {
        self.borrow_mut()
            .0
            .with_top_mut(|context| context.frame.next_statement())
            .unwrap()
    }

    pub(crate) fn current_frame(&self) -> Frame {
        self.borrow().0.top().unwrap().frame
    }

    pub(crate) fn current_state(&self) -> PausableState {
        self.borrow().0.top().unwrap().state
    }

    pub(crate) fn restart_frame(&self) {
        self.borrow_mut().0.with_top_mut(|context| {
            context.frame.restart();
        });
    }

    pub(crate) fn start(&self) {
        self.set_state(PausableState::Running);
    }

    pub(crate) fn step_back(&self) {
        self.borrow_mut().0.with_top_mut(|context| {
            context.frame.step_back();
        });
    }
}
