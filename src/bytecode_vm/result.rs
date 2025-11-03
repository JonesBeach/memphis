use crate::{bytecode_vm::CompilerError, domain::RuntimeError};

pub type CompilerResult<T> = Result<T, CompilerError>;

pub type VmResult<T> = Result<T, RuntimeError>;
