#[cfg(feature = "llvm_backend")]
use crate::llvm_backend::compile_ast_to_llvm;
use crate::{core::memphis_utils, Engine};

use super::MemphisContext;

pub struct Memphis;

impl Memphis {
    pub fn start(filepath: &str, engine: Engine) {
        match engine {
            Engine::TreeWalk => {
                let mut context = MemphisContext::from_path(filepath);

                match context.run() {
                    Ok(_) => {}
                    Err(err) => memphis_utils::exit(err),
                }
            }
            Engine::BytecodeVm => {
                let mut context = MemphisContext::from_path(filepath);

                match context.run_vm() {
                    Ok(_) => {}
                    Err(err) => memphis_utils::exit(err),
                }
            }
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => {
                compile_ast_to_llvm();
            }
        }
    }
}
