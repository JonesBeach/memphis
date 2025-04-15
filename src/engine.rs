use std::{
    env,
    fmt::{Display, Formatter, Result},
};

#[derive(PartialEq, Clone, Copy)]
pub enum Engine {
    Treewalk,
    BytecodeVm,
    #[cfg(feature = "llvm_backend")]
    LlvmBackend,
}

impl Engine {
    /// I could see the default becoming [`Engine::BytecodeVm`] in the future once it supports more.
    pub const DEFAULT_ENGINE: Engine = Engine::Treewalk;

    pub fn from_env() -> Self {
        if let Ok(mode) = env::var("MEMPHIS_ENGINE") {
            match mode.to_lowercase().as_str() {
                "bytecode_vm" => Engine::BytecodeVm,
                #[cfg(feature = "llvm_backend")]
                "llvm_backend" => Engine::LlvmBackend,
                "treewalk" => Engine::Treewalk,
                _ => panic!("Unsupported engine: {}", mode),
            }
        } else {
            Self::DEFAULT_ENGINE
        }
    }
}

impl Display for Engine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Engine::Treewalk => write!(f, "treewalk"),
            Engine::BytecodeVm => write!(f, "bytecode VM"),
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => write!(f, "LLVM backend"),
        }
    }
}
