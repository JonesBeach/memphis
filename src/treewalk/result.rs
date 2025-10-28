use crate::{
    domain::{ExecutionError, ExecutionErrorKind},
    treewalk::{TreewalkInterpreter, TreewalkValue},
};

pub trait Raise<T> {
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T>;
}

impl<T> Raise<T> for Result<T, ExecutionErrorKind> {
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T> {
        self.map_err(|kind| interpreter.raise(kind))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkDisruption {
    Signal(TreewalkSignal), // Control flow (not errors)
    Error(ExecutionError),  // Actual Python runtime errors
}

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkSignal {
    Return(TreewalkValue),
    Yield(TreewalkValue),
    YieldFrom(TreewalkValue),
    Raise,
    Await,
    Sleep,
    Break,
    Continue,
}

pub type TreewalkResult<T> = Result<T, TreewalkDisruption>;
