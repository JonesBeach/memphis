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

#[cfg(feature = "wasm")]
mod wasm {
    use console_error_panic_hook::set_once;
    use wasm_bindgen::prelude::wasm_bindgen;

    use super::init::MemphisContext;

    // Export a function to JavaScript
    #[wasm_bindgen]
    pub fn greet() -> String {
        "Hello from WebAssembly!".to_string()
    }

    #[wasm_bindgen]
    pub fn evaluate(code: String) -> String {
        // Set the panic hook for better error messages in the browser console
        set_once();

        let context = MemphisContext::from_text(&code);
        let result = context
            .evaluate_oneshot()
            .expect("Failed to evaluate expression.");
        format!("{}", result)
    }
}
