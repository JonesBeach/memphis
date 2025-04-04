mod bytecode_vm;
mod core;
pub mod crosscheck_utils;
pub mod domain;
mod engine;
pub mod init;
mod lexer;
#[cfg(feature = "llvm_backend")]
mod llvm_backend;
mod parser;
mod runtime;
mod treewalk;
mod types;

// We need these public for crosscheck
pub use engine::Engine;
pub use types::errors::MemphisError;

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

        let mut context = MemphisContext::from_text(&code);
        let result = context.evaluate().expect("Failed to evaluate.");
        format!("{}", result)
    }
}
