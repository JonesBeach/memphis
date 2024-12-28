/// Represents the scope in which a Python statement or expression is executed.
///
/// Python's scoping rules determine whether a variable is accessible in the global
/// (module-level) scope or the local (function-level) scope.
///
/// - `Global`: Refers to the module-level scope, where variables are accessible
///   throughout the entire module unless shadowed by a local definition.
/// - `Local`: Refers to the function-level scope, where variables are accessible
///   only within the specific function where they are defined.
#[derive(Clone, Debug)]
pub enum Context {
    Global,
    Local,
}
