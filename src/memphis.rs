#[cfg(feature = "repl")]
use crate::init::Repl;
#[cfg(feature = "llvm_backend")]
use crate::llvm_backend::compile_ast_to_llvm;
use crate::{core::memphis_utils, domain::Source, init::MemphisContext, Engine};

/// The entrypoint to the Memphis executable. Supports script mode or REPL mode.
pub struct Memphis;

impl Memphis {
    pub fn run_script(filepath: &str, engine: Engine) {
        match engine {
            Engine::Treewalk => {
                let mut context = MemphisContext::new(Source::from_path(filepath));

                match context.run_treewalk() {
                    Ok(_) => {}
                    Err(err) => memphis_utils::exit(err),
                }
            }
            Engine::BytecodeVm => {
                let mut context = MemphisContext::new(Source::from_path(filepath));

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

    #[cfg(feature = "repl")]
    pub fn run_repl(engine: Engine) {
        Repl::default().run(engine);
    }
}
