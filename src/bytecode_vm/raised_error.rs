use std::fmt::{Debug, Display, Error, Formatter};

use crate::{bytecode_vm::runtime::types::Exception, domain::DebugCallStack, errors::MemphisError};

#[derive(PartialEq, Clone)]
pub struct RaisedException {
    pub debug_call_stack: DebugCallStack,
    pub exception: Exception,
}

impl RaisedException {
    pub fn new(debug_call_stack: DebugCallStack, execution_error: Exception) -> Self {
        Self {
            debug_call_stack,
            exception: execution_error,
        }
    }
}

impl Display for RaisedException {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.debug_call_stack)?;
        write!(f, "{}", self.exception)
    }
}

impl Debug for RaisedException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl From<RaisedException> for MemphisError {
    fn from(e: RaisedException) -> Self {
        MemphisError::new(e.to_string())
    }
}
