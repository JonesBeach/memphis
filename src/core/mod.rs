mod container;
mod log;

pub use container::Container;
pub use log::{log, log_impure, LogLevel};

use crate::{domain::MemphisValue, parser::Parser, types::errors::MemphisError};

/// Return types which None are used internally, but should never be displayed to the developer.
pub trait Voidable {
    // This is only used in the REPL right now, but it is referenced other places.
    #[allow(dead_code)]
    fn is_none(&self) -> bool;
}

pub trait Interpreter {
    /// The primary interpreter entrypoint which is provided an AST in the form of the `Parser`.
    fn run(&mut self, parser: &mut Parser) -> Result<MemphisValue, MemphisError>;
    fn read(&mut self, name: &str) -> Option<MemphisValue>;
}

pub mod memphis_utils {
    use std::process;

    use crate::MemphisError;

    /// The primary exit point from the interpreter. This should be used sparingly: either at the
    /// top level of the `Memphis` runtime or inline in select cases where a hard interface
    /// constraits the caller from propagating an error upwards.
    pub fn exit(err: MemphisError) -> ! {
        eprintln!("{}", err);
        process::exit(1);
    }
}
