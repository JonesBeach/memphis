use crate::{
    domain::{ExecutionError, ExecutionErrorKind},
    treewalk::{TreewalkInterpreter, TreewalkValue},
};

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkDisruption {
    Signal(TreewalkSignal), // Control flow (not errors)
    Error(ExecutionError),  // Actual Python runtime errors
}

#[cfg(test)]
impl TreewalkDisruption {
    pub fn as_err(&self) -> &ExecutionError {
        match self {
            TreewalkDisruption::Signal(_) => panic!("Expected error!"),
            TreewalkDisruption::Error(ref e) => e,
        }
    }
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
pub type ExecResult<T> = Result<T, ExecutionErrorKind>;

pub trait Raise<T> {
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T>;
}

impl<T> Raise<T> for ExecResult<T> {
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T> {
        self.map_err(|kind| interpreter.raise(kind))
    }
}
