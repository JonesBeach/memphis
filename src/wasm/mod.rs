use console_error_panic_hook::set_once;
use wasm_bindgen::prelude::wasm_bindgen;

use crate::{domain::Source, Engine, MemphisContext};

// Export a function to JavaScript
#[wasm_bindgen]
pub fn greet() -> String {
    "Hello from WebAssembly!".to_string()
}

#[wasm_bindgen]
pub fn evaluate(code: String) -> String {
    // Set the panic hook for better error messages in the browser console
    set_once();

    let mut context = MemphisContext::new(Engine::Treewalk, Source::from_text(&code));
    let result = context.run().expect("Failed to evaluate.");
    format!("{}", result)
}
