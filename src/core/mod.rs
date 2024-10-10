use std::{fmt::Display, process};

mod container;
mod log;
mod stack;

pub use container::{Container, Storable};
pub use log::{log, log_impure, LogLevel};
pub use stack::Stack;

use crate::{parser::Parser, types::errors::MemphisError};

/// Return types which None are used internally, but should never be displayed to the developer.
pub trait Voidable {
    // This is only used in the REPL right now, but it is referenced other places.
    #[allow(dead_code)]
    fn is_none(&self) -> bool;
}

pub trait InterpreterEntrypoint {
    type Return: Display + Voidable;

    /// The primary interpreter entrypoint which is provided an AST in the form of the `Parser`.
    fn run(&mut self, parser: &mut Parser) -> Result<Self::Return, MemphisError>;

    /// The primary exit point from the interpreter. This should be used sparingly: either at the
    /// top level of the `Memphis` runtime or inline in select cases where a hard interface
    /// constraits the caller from propagating an error upwards.
    fn handle_runtime_error(&self, err: MemphisError) -> ! {
        eprintln!("{}", err);
        process::exit(1);
    }
}
