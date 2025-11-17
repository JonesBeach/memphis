use crate::{
    bytecode_vm::{CompilerError, VirtualMachine},
    domain::{DomainResult, ExecutionError, RuntimeError},
};

pub type CompilerResult<T> = Result<T, CompilerError>;

pub type VmResult<T> = Result<T, RuntimeError>;

pub trait Raise<T> {
    fn raise(self, vm: &VirtualMachine) -> VmResult<T>;
}

impl<T> Raise<T> for DomainResult<T> {
    /// Convert an `ExecutionError` into a raised runtime error
    fn raise(self, vm: &VirtualMachine) -> VmResult<T> {
        self.map_err(|kind| vm.raise(kind))
    }
}

impl<T> Raise<T> for ExecutionError {
    /// Raise this `ExecutionError` in the given interpreter, returning it as a
    /// `TreewalkResult<T>`.
    fn raise(self, vm: &VirtualMachine) -> VmResult<T> {
        Err(vm.raise(self))
    }
}
