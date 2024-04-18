mod bytecode_vm;
mod core;
pub mod crosscheck;
mod domain;
pub mod init;
mod lexer;
#[cfg(feature = "llvm_backend")]
mod llvm_backend;
mod parser;
mod treewalk;
mod types;

#[derive(PartialEq)]
pub enum Engine {
    TreeWalk,
    BytecodeVm,
    #[cfg(feature = "llvm_backend")]
    LlvmBackend,
}
