use crate::bytecode_vm::{
    runtime::types::Exception, CompilerError, RaisedException, VirtualMachine,
};

pub type CompilerResult<T> = Result<T, CompilerError>;

pub type DomainResult<T> = Result<T, Exception>;
pub type VmResult<T> = Result<T, RaisedException>;

pub trait Raise<T> {
    fn raise(self, vm: &VirtualMachine) -> VmResult<T>;
}

impl<T> Raise<T> for DomainResult<T> {
    /// Convert an `Exception` into a raised runtime error
    fn raise(self, vm: &VirtualMachine) -> VmResult<T> {
        self.map_err(|kind| vm.raise(kind))
    }
}

impl<T> Raise<T> for Exception {
    /// Raise this `Exception` in the given interpreter, returning it as a
    /// `TreewalkResult<T>`.
    fn raise(self, vm: &VirtualMachine) -> VmResult<T> {
        Err(vm.raise(self))
    }
}
