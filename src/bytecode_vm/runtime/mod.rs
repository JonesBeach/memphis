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

pub use call_stack::CallStack;
pub use executor::VmExecutor;
pub use frame::Frame;
pub use heap::Heap;
pub use reference::{BuiltinFn, BuiltinFunction, Reference};
pub use runtime::Runtime;
pub use vm::VirtualMachine;
