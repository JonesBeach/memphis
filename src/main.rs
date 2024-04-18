use std::{env, process};

use memphis::init::{Memphis, Repl};
use memphis::Engine;

/// I could see the default becoming [`Engine::BytecodeVm`] in the future once it supports more.
const DEFAULT_ENGINE: Engine = Engine::TreeWalk;

fn main() {
    let args: Vec<String> = env::args().collect();

    let engine = if let Some(mode) = args.get(2) {
        match mode.to_lowercase().as_str() {
            "vm" | "bytecode_vm" => Engine::BytecodeVm,
            #[cfg(feature = "llvm_backend")]
            "llvm" | "llvm_backend" | "native" => Engine::LlvmBackend,
            "tw" | "treewalk" => Engine::TreeWalk,
            _ => panic!("Unsupported engine: {}", mode),
        }
    } else {
        DEFAULT_ENGINE
    };

    match args.len() {
        1 => Repl::new().run(),
        2 | 3 => Memphis::start(&args[1], engine),
        _ => {
            eprintln!("Usage: memphis [<filename>]");
            process::exit(1);
        }
    }
}
