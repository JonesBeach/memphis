pub mod compiler;
pub mod indices;
mod interpreter;
mod opcode;
pub mod types;
#[allow(clippy::module_inception)]
pub mod vm;

use compiler::Compiler;
pub use interpreter::VmInterpreter;
use opcode::Opcode;
use vm::VirtualMachine;
