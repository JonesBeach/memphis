use std::fmt::{Debug, Display, Error, Formatter};

use crate::bytecode_vm::compiler::CodeObject;

/// The values which are passed to the VM are a subset of the types of [`Value`].
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    None,
    Boolean(bool),
    String(String),
    Code(CodeObject),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Constant::None => write!(f, "None"),
            Constant::Boolean(i) => write!(f, "{}", i),
            Constant::String(i) => write!(f, "{}", i),
            Constant::Code(i) => write!(f, "{}", i),
        }
    }
}
