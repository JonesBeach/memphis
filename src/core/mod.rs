use std::fmt::Display;

mod container;
mod log;
mod stack;

pub use container::{Container, Storable};
#[allow(unused_imports)]
pub use log::{log, log_impure, LogLevel};
pub use stack::Stack;

use crate::{parser::Parser, types::errors::MemphisError};

/// Return types which Void are used internally, but should never be displayed to the developer.
pub trait Voidable {
    fn is_void(&self) -> bool;
}

pub trait InterpreterEntrypoint {
    type Return: Display + Voidable;

    fn run(&mut self, parser: &mut Parser) -> Result<Self::Return, MemphisError>;
}
