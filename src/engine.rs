use std::env;

#[derive(PartialEq)]
pub enum Engine {
    TreeWalk,
    BytecodeVm,
    #[cfg(feature = "llvm_backend")]
    LlvmBackend,
}

impl Engine {
    /// I could see the default becoming [`Engine::BytecodeVm`] in the future once it supports more.
    const DEFAULT_ENGINE: Engine = Engine::TreeWalk;

    pub fn from_env() -> Self {
        if let Ok(mode) = env::var("MEMPHIS_ENGINE") {
            match mode.to_lowercase().as_str() {
                "vm" | "bytecode_vm" => Engine::BytecodeVm,
                #[cfg(feature = "llvm_backend")]
                "llvm" | "llvm_backend" | "native" => Engine::LlvmBackend,
                "tw" | "treewalk" => Engine::TreeWalk,
                _ => panic!("Unsupported engine: {}", mode),
            }
        } else {
            Self::DEFAULT_ENGINE
        }
    }
}
