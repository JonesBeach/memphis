use std::process;

#[cfg(feature = "llvm_backend")]
use crate::llvm_backend::compile_ast_to_llvm;
#[cfg(feature = "repl")]
use crate::repl::Repl;
use crate::{core::memphis_utils, domain::Source, Engine, MemphisContext};

/// The entrypoint to the Memphis executable. Supports script mode or REPL mode.
pub struct Memphis;

impl Memphis {
    pub fn run_script(filepath: &str, engine: Engine) {
        match engine {
            Engine::Treewalk | Engine::BytecodeVm => {
                let source = Source::from_path(filepath)
                    .map_err(|err| {
                        eprintln!("{err}");
                        process::exit(1);
                    })
                    .unwrap();
                let _ = MemphisContext::new(engine, source)
                    .run()
                    .map_err(|err| memphis_utils::exit(err));
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
