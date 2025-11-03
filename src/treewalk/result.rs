use crate::{
    domain::{DomainResult, RuntimeError},
    treewalk::{TreewalkInterpreter, TreewalkValue},
};

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkDisruption {
    Signal(TreewalkSignal), // Control flow (not errors)
    Error(RuntimeError),    // Actual Python runtime errors
}

#[cfg(test)]
impl TreewalkDisruption {
    pub fn as_err(&self) -> &RuntimeError {
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

// control-flow & raised runtime errors, used in upper levels of the code
pub type TreewalkResult<T> = Result<T, TreewalkDisruption>;

pub trait Raise<T> {
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T>;
}

impl<T> Raise<T> for DomainResult<T> {
    /// Convert an `ExecutionError` into a raised runtime error
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T> {
        self.map_err(|kind| interpreter.raise(kind))
    }
}
