#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    /// Callable functions
    Regular,
    /// Yielding, resumable functions
    Generator,
    /// Future-based coroutines
    Async,
}
