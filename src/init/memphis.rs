use std::process;

use super::Builder as MemphisBuilder;
#[cfg(feature = "llvm_backend")]
use crate::llvm_backend::compile_ast_to_llvm;
use crate::{core::InterpreterEntrypoint, Engine};

pub struct Memphis;

impl Memphis {
    pub fn start(filepath: &str, engine: Engine) {
        match engine {
            Engine::TreeWalk => {
                let (mut parser, mut interpreter) = MemphisBuilder::new().path(filepath).build();

                match interpreter.run(&mut parser) {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        process::exit(1);
                    }
                }
            }
            Engine::BytecodeVm => {
                let (mut parser, mut interpreter) = MemphisBuilder::new().path(filepath).build_vm();

                match interpreter.run(&mut parser) {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        process::exit(1);
                    }
                }
            }
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => {
                compile_ast_to_llvm();
            }
        }
    }
}
