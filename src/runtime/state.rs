use std::path::{Path, PathBuf};

use crate::{
    core::Container,
    domain::{
        resolve, DebugCallStack, DebugStackFrame, DomainResult, ExecutionError, ModuleName, Source,
        ToDebugStackFrame,
    },
};

use super::ImportResolver;

pub struct MemphisState {
    import_resolver: ImportResolver,
    debug_call_stack: DebugCallStack,
    line_number: usize,
}

impl Default for MemphisState {
    fn default() -> Self {
        Self::new()
    }
}

impl MemphisState {
    fn new() -> Self {
        MemphisState {
            import_resolver: ImportResolver::new(),
            debug_call_stack: DebugCallStack::new(),
            line_number: 1,
        }
    }

    pub fn from_source(source: &Source) -> Container<Self> {
        let state = Container::new(Self::new());
        state.register_root(source.path());
        state
    }
}

impl Container<MemphisState> {
    pub fn save_line_number(&self) {
        let line_number = self.borrow().line_number;
        self.borrow_mut()
            .debug_call_stack
            .update_line_number(line_number);
    }

    pub fn set_line_number(&self, line_number: usize) {
        self.borrow_mut().line_number = line_number;
    }

    /// Return the `CallStack` at the current moment in time. This should be used at the time of an
    /// exception or immediately before any other use as it is a snapshot and will not keep updating.
    pub fn debug_call_stack(&self) -> DebugCallStack {
        self.borrow().debug_call_stack.clone()
    }

    pub fn push_stack_frame<T: ToDebugStackFrame>(&self, context: &T) {
        self.borrow_mut()
            .debug_call_stack
            .push_stack_frame(context.to_stack_frame());
    }

    pub fn pop_stack_frame(&self) -> Option<DebugStackFrame> {
        self.borrow_mut().debug_call_stack.pop_stack_frame()
    }

    pub fn register_root(&self, path: &Path) {
        self.borrow_mut().import_resolver.register_root(path);
    }

    fn search_paths(&self) -> Vec<PathBuf> {
        self.borrow().import_resolver.search_paths().to_vec()
    }

    pub fn load_source(&self, module_name: &ModuleName) -> DomainResult<Source> {
        let path = self.resolve_module_path(module_name)?;
        Source::from_path(path)
            .map_err(|_| ExecutionError::import_error(format!("No module named {}", module_name)))
    }

    fn resolve_module_path(&self, module_name: &ModuleName) -> DomainResult<PathBuf> {
        let search_paths = self.search_paths();
        resolve(module_name, &search_paths)
            .ok_or_else(|| ExecutionError::import_error(format!("No module named {}", module_name)))
    }
}
