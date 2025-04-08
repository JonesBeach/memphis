use crate::{domain::MemphisValue, types::errors::MemphisError};

/// The primary crosscheck interface, allowing us to run test Python code through multiple
/// interpreter engines and compare the output.
pub trait InterpreterTest {
    fn evaluate(&mut self, input: &str) -> Result<MemphisValue, MemphisError>;
    fn read(&mut self, var: &str) -> Option<MemphisValue>;
}
