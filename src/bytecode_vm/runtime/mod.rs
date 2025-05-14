mod call_stack;
mod error_builder;
mod frame;
mod heap;
mod module_loader;
#[allow(clippy::module_inception)]
mod runtime;
mod types;
mod vm;

pub use call_stack::CallStack;
pub use runtime::Runtime;
pub use types::{Class, FunctionObject, Method, Module, Object, Reference};
pub use vm::VirtualMachine;
