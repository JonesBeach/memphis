use std::fmt::{Debug, Display, Error, Formatter};

use crate::bytecode_vm::compiler::CodeObject;

/// The values which are passed to the VM are a subset of the types of [`Value`].
#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    None,
    Boolean(bool),
    Int(i64),
    Float(f64),
    String(String),
    Code(CodeObject),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Constant::None => write!(f, "None"),
            Constant::Boolean(i) => write!(f, "{}", i),
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(i) => write!(f, "{}", i),
            Constant::String(i) => write!(f, "{}", i),
            Constant::Code(i) => write!(f, "{}", i),
        }
    }
}
