use std::path::{Path, PathBuf};

use crate::{
    core::Container,
    domain::{DebugCallStack, DebugStackFrame, ToDebugStackFrame},
    parser::types::ImportPath,
    treewalk::{
        interpreter::TreewalkResult,
        module_loader,
        types::{domain::Type, utils::EnvironmentFrame, Class, Dict, ExprResult, Function, Module},
        EvaluatedModuleCache, ExecutionContextManager, Executor, Interpreter, ModuleContext,
        ModuleSource, Scope, ScopeManager, TypeRegistry,
    },
};

#[cfg(feature = "c_stdlib")]
use super::types::cpython::{BuiltinModuleCache, CPythonModule};

pub struct State {
    module_context: ModuleContext,
    debug_call_stack: DebugCallStack,
    module_source_stack: Vec<ModuleSource>,
    line_number: usize,

    module_cache: EvaluatedModuleCache,
    #[cfg(feature = "c_stdlib")]
    builtin_module_cache: BuiltinModuleCache,
    scope_manager: ScopeManager,
    executor: Container<Executor>,
    type_registry: TypeRegistry,
    execution_context: ExecutionContextManager,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    fn new() -> Self {
        let type_registry = TypeRegistry::new();
        let mut scope_manager = ScopeManager::new();

        // We still want the `TypeRegistry` to own the type classes, but we must make some of them
        // available in the builtin scope before execution begins.
        scope_manager.register_callable_builtin_types(&type_registry);

        State {
            module_context: ModuleContext::new(),
            debug_call_stack: DebugCallStack::new(),
            module_source_stack: Vec::new(),
            line_number: 1,

            module_cache: EvaluatedModuleCache::new(),
            type_registry,
            scope_manager,
            execution_context: ExecutionContextManager::new(),
            executor: Container::new(Executor::new()),
            #[cfg(feature = "c_stdlib")]
            builtin_module_cache: BuiltinModuleCache::new(),
        }
    }

    pub fn from_source(module_source: ModuleSource) -> Container<Self> {
        let state = Container::new(Self::new());
        if let Some(path) = module_source.path() {
            state.register_root(path);
        }
        state.push_module_source(module_source);
        state
    }
}

impl Container<State> {
    pub fn get_type(&self, result: &ExprResult) -> ExprResult {
        match result {
            #[cfg(feature = "c_stdlib")]
            ExprResult::CPythonObject(o) => o.get_type(),
            ExprResult::Object(o) => ExprResult::Class(o.borrow().class.clone()),
            _ => ExprResult::Class(self.get_type_class(result.get_type())),
        }
    }

    pub fn save_line_number(&self) {
        let line_number = self.borrow().line_number;
        self.borrow_mut()
            .debug_call_stack
            .update_line_number(line_number);
    }

    pub fn set_line_number(&self, line_number: usize) {
        self.borrow_mut().line_number = line_number;
    }

    /// Write an `ExprResult` to the symbol table.
    pub fn write(&self, name: &str, value: ExprResult) {
        self.borrow_mut().scope_manager.write(name, value);
    }

    /// Attempt to read an `ExprResult`, adhering to Python scoping rules.
    pub fn read(&self, name: &str) -> Option<ExprResult> {
        self.borrow().scope_manager.read(name)
    }

    pub fn read_or_disrupt(
        &self,
        name: &str,
        interpreter: &Interpreter,
    ) -> TreewalkResult<ExprResult> {
        self.read(name).ok_or_else(|| interpreter.name_error(name))
    }

    /// Attempt to delete an `ExprResult`, adhering to Python scoping rules.
    pub fn delete(&self, name: &str) -> Option<ExprResult> {
        self.borrow_mut().scope_manager.delete(name)
    }

    /// Return the `CallStack` at the current moment in time. This should be used at the time of an
    /// exception or immediately before any other use as it is a snapshot and will not keep updating.
    pub fn debug_call_stack(&self) -> DebugCallStack {
        self.borrow().debug_call_stack.clone()
    }

    pub fn get_executor(&self) -> Container<Executor> {
        self.borrow_mut().executor.clone()
    }

    pub fn push_captured_env(&self, frame: Container<EnvironmentFrame>) {
        self.borrow_mut().scope_manager.push_captured_env(frame);
    }

    pub fn pop_captured_env(&self) -> Option<Container<EnvironmentFrame>> {
        self.borrow_mut().scope_manager.pop_captured_env()
    }

    pub fn push_local(&self, scope: Container<Scope>) {
        self.borrow_mut().scope_manager.push_local(scope);
    }

    pub fn pop_local(&self) -> Option<Container<Scope>> {
        self.borrow_mut().scope_manager.pop_local()
    }

    pub fn push_stack_frame<T: ToDebugStackFrame>(&self, context: &T) {
        self.borrow_mut()
            .debug_call_stack
            .push_stack_frame(context.to_stack_frame());
    }

    pub fn pop_stack_frame(&self) -> Option<DebugStackFrame> {
        self.borrow_mut().debug_call_stack.pop_stack_frame()
    }

    pub fn push_module_source(&self, module_source: ModuleSource) {
        self.borrow_mut().module_source_stack.push(module_source);
    }

    pub fn pop_module_source(&self) -> Option<ModuleSource> {
        self.borrow_mut().module_source_stack.pop()
    }

    pub fn current_module_source(&self) -> Box<ModuleSource> {
        let binding = self.borrow();
        let current_module_source = binding
            .module_source_stack
            .last()
            .expect("No module source!");
        Box::new(current_module_source.to_owned())
    }

    pub fn push_module(&self, module: Container<Module>) {
        self.borrow_mut().scope_manager.push_module(module);
    }

    pub fn pop_module(&self) -> Option<Container<Module>> {
        self.borrow_mut().scope_manager.pop_module()
    }

    pub fn current_module(&self) -> Container<Module> {
        self.borrow().scope_manager.read_module()
    }

    pub fn current_context(&self) -> String {
        if let Some(function) = self.current_function() {
            function.borrow().name().to_string()
        } else {
            self.current_module().borrow().context().to_string()
        }
    }

    pub fn current_path(&self) -> Box<PathBuf> {
        let current_module = self.current_module();
        let binding = current_module.borrow();
        let path = binding.path();
        Box::new(path.to_owned())
    }

    pub fn push_class(&self, class: Container<Class>) {
        self.borrow_mut().execution_context.push_class(class);
    }

    pub fn pop_class(&self) -> Option<Container<Class>> {
        self.borrow_mut().execution_context.pop_class()
    }

    pub fn push_function(&self, function: Container<Function>) {
        self.borrow_mut().execution_context.push_function(function);
    }

    pub fn pop_function(&self) -> Option<Container<Function>> {
        self.borrow_mut().execution_context.pop_function()
    }

    pub fn push_receiver(&self, receiver: ExprResult) {
        self.borrow_mut().execution_context.push_receiver(receiver);
    }

    pub fn pop_receiver(&self) -> Option<ExprResult> {
        self.borrow_mut().execution_context.pop_receiver()
    }

    pub fn current_class(&self) -> Option<Container<Class>> {
        self.borrow().execution_context.read_class()
    }

    pub fn current_function(&self) -> Option<Container<Function>> {
        self.borrow().execution_context.read_current_function()
    }

    pub fn current_receiver(&self) -> Option<ExprResult> {
        self.borrow().execution_context.read_current_receiver()
    }

    pub fn read_captured_env(&self) -> Option<Box<Container<EnvironmentFrame>>> {
        self.borrow().scope_manager.read_captured_env()
    }

    pub fn read_globals(&self, interpreter: &Interpreter) -> Container<Dict> {
        let scope = self.borrow().scope_manager.read_module().borrow().clone();

        // This will make another function call to hash the keys so we do this in a separate
        // statement to avoid a mutable borrow error.
        scope.as_dict(interpreter)
    }

    pub fn mark_nonlocal(&self, name: &str) {
        self.borrow_mut().scope_manager.mark_nonlocal(name);
    }

    pub fn mark_global(&self, name: &str) {
        self.borrow_mut().scope_manager.mark_global(name);
    }

    /// Return a singleton `Class` for builtin types such as list, set, tuple, dict, etc.
    pub fn get_type_class(&self, type_: Type) -> Container<Class> {
        self.borrow().type_registry.get_type_class(type_)
    }

    pub fn get_environment_frame(&self) -> Container<EnvironmentFrame> {
        Container::new(EnvironmentFrame::new(
            self.borrow().scope_manager.read_local(),
            self.borrow().scope_manager.read_captured_env(),
        ))
    }

    pub fn is_class(&self, name: &str) -> bool {
        self.borrow().scope_manager.is_class(name)
    }

    pub fn register_root(&self, path: &Path) {
        self.borrow_mut().module_context.register_root(path);
    }

    #[cfg(feature = "c_stdlib")]
    pub fn import_builtin_module(&self, import_path: &ImportPath) -> Container<CPythonModule> {
        self.borrow_mut()
            .builtin_module_cache
            .import_builtin_module(import_path)
    }

    pub fn load_module_source(&self, import_path: &ImportPath) -> Option<ModuleSource> {
        let current_path = self.current_path();
        let binding = self.borrow();
        let search_paths = binding.module_context.search_paths();
        module_loader::load_module_source(import_path, &current_path, search_paths)
    }

    pub fn store_module(&self, import_path: &ImportPath, module: Container<Module>) {
        self.borrow_mut()
            .module_cache
            .store_module(import_path, module)
    }

    pub fn fetch_module(&self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.borrow_mut().module_cache.fetch_module(import_path)
    }
}
