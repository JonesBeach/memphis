/// Python's scoping rules rely on determining whether the current context is global (sometimes
/// known as module scope) or local (sometimes known as function scope).
#[derive(Clone, Debug)]
pub enum Context {
    Global,
    Local,
}
