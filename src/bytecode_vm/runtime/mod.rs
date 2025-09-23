mod builtin;
mod call_stack;
mod error_builder;
mod executor;
mod frame;
mod heap;
mod module_loader;
pub mod modules;
mod reference;
#[allow(clippy::module_inception)]
mod runtime;
pub mod types;
mod vm;

pub use builtin::{BuiltinFn, BuiltinFunction};
pub use call_stack::CallStack;
pub use executor::VmExecutor;
pub use frame::Frame;
pub use heap::Heap;
pub use reference::Reference;
pub use runtime::Runtime;
pub use vm::VirtualMachine;

pub mod components {
    use super::*;

    pub use error_builder::ErrorBuilder;
    pub use module_loader::ModuleLoader;
}
