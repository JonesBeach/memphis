pub mod compiler;
pub mod indices;
mod interpreter;
mod result;
mod utils;
mod value;
#[allow(clippy::module_inception)]
pub mod vm;

pub use compiler::{Compiler, CompilerError};
pub use interpreter::VmInterpreter;
pub use result::{CompilerResult, VmResult};
pub use utils::find_index;
pub use value::VmValue;
pub use vm::VirtualMachine;
