use crate::{bytecode_vm::CompilerError, domain::ExecutionError};

pub type CompilerResult<T> = Result<T, CompilerError>;

pub type VmResult<T> = Result<T, ExecutionError>;
