use std::path::{Path, PathBuf};

use crate::{
    core::Container,
    domain::{DebugCallStack, DebugStackFrame, Source, ToDebugStackFrame},
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
        if let Some(path) = source.path() {
            state.register_root(path);
        }
        state.push_stack_frame(source);
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

    pub fn search_paths(&self) -> Vec<PathBuf> {
        self.borrow().import_resolver.search_paths().to_vec()
    }
}
