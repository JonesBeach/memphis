use crate::{domain::ExecutionError, treewalk::TreewalkValue};

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkDisruption {
    Signal(TreewalkSignal), // Control flow (not errors)
    Error(ExecutionError),  // Actual Python runtime errors
}

#[derive(Debug, PartialEq, Clone)]
pub enum TreewalkSignal {
    Return(TreewalkValue),
    Raise,
    Await,
    Sleep,
    Break,
    Continue,
}

pub type TreewalkResult<T> = Result<T, TreewalkDisruption>;
