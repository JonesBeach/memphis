use std::{cell::UnsafeCell, path::PathBuf};

use crate::{
    core::Container,
    domain::{DebugCallStack, DebugStackFrame, ImportPath, Source, ToDebugStackFrame, Type},
    runtime::MemphisState,
    treewalk::{
        types::{Class, Dict, Function, Module},
        utils::EnvironmentFrame,
        ExecutionContextManager, Executor, ModuleStore, Scope, ScopeManager, TreewalkInterpreter,
        TreewalkResult, TreewalkValue, TypeRegistry,
    },
};

#[cfg(feature = "c_stdlib")]
use super::types::cpython::{BuiltinModuleCache, CPythonModule};

pub struct TreewalkState {
    memphis_state: Container<MemphisState>,
    module_store: ModuleStore,
    type_registry: TypeRegistry,
    scope_manager: ScopeManager,
    execution_context: ExecutionContextManager,
    pub executor: UnsafeCell<Executor>,
    #[cfg(feature = "c_stdlib")]
    builtin_module_cache: BuiltinModuleCache,
}

impl Default for TreewalkState {
    fn default() -> Self {
        Self::new(Container::new(MemphisState::default()))
    }
}

impl TreewalkState {
    fn new(memphis_state: Container<MemphisState>) -> Self {
        let type_registry = TypeRegistry::new();
        let mut scope_manager = ScopeManager::new();

        // We still want the `TypeRegistry` to own the type classes, but we must make some of them
        // available in the builtin scope before execution begins.
        scope_manager.register_callable_builtin_types(&type_registry);

        let mut module_store = ModuleStore::new();
        module_store.load_builtins(&type_registry);

        TreewalkState {
            memphis_state,
            module_store,
            type_registry,
            scope_manager,
            execution_context: ExecutionContextManager::new(),
            executor: Executor::new().into(),
            #[cfg(feature = "c_stdlib")]
            builtin_module_cache: BuiltinModuleCache::new(),
        }
    }

    pub fn from_source_state(
        memphis_state: Container<MemphisState>,
        source: Source,
    ) -> Container<Self> {
        memphis_state.push_stack_frame(&source);

        let treewalk_state = Container::new(Self::new(memphis_state));
        treewalk_state.push_module(Container::new(Module::new(source)));
        treewalk_state
    }
}

impl Container<TreewalkState> {
    pub fn memphis_state(&self) -> Container<MemphisState> {
        self.borrow().memphis_state.clone()
    }

    pub fn save_line_number(&self) {
        self.borrow().memphis_state.save_line_number();
    }

    pub fn set_line_number(&self, line_number: usize) {
        self.borrow().memphis_state.set_line_number(line_number);
    }

    /// Return the `CallStack` at the current moment in time. This should be used at the time of an
    /// exception or immediately before any other use as it is a snapshot and will not keep updating.
    pub fn debug_call_stack(&self) -> DebugCallStack {
        self.borrow().memphis_state.debug_call_stack()
    }

    pub fn push_stack_frame<T: ToDebugStackFrame>(&self, context: &T) {
        self.borrow().memphis_state.push_stack_frame(context);
    }

    pub fn pop_stack_frame(&self) -> Option<DebugStackFrame> {
        self.borrow().memphis_state.pop_stack_frame()
    }

    /// Write an `TreewalkValue` to the symbol table.
    pub fn write(&self, name: &str, value: TreewalkValue) {
        self.borrow_mut().scope_manager.write(name, value);
    }

    /// Attempt to read an `TreewalkValue`, adhering to Python scoping rules.
    pub fn read(&self, name: &str) -> Option<TreewalkValue> {
        self.borrow().scope_manager.read(name)
    }

    pub fn read_or_disrupt(
        &self,
        name: &str,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<TreewalkValue> {
        self.read(name).ok_or_else(|| interpreter.name_error(name))
    }

    /// Attempt to delete an `TreewalkValue`, adhering to Python scoping rules.
    pub fn delete(&self, name: &str) -> Option<TreewalkValue> {
        self.borrow_mut().scope_manager.delete(name)
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

    pub fn push_module(&self, module: Container<Module>) {
        self.borrow_mut().scope_manager.push_module(module);
    }

    pub fn pop_module(&self) -> Option<Container<Module>> {
        self.borrow_mut().scope_manager.pop_module()
    }

    pub fn current_module(&self) -> Container<Module> {
        self.borrow().scope_manager.read_module()
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

    pub fn push_receiver(&self, receiver: TreewalkValue) {
        self.borrow_mut().execution_context.push_receiver(receiver);
    }

    pub fn pop_receiver(&self) -> Option<TreewalkValue> {
        self.borrow_mut().execution_context.pop_receiver()
    }

    pub fn current_class(&self) -> Option<Container<Class>> {
        self.borrow().execution_context.read_class()
    }

    pub fn current_function(&self) -> Option<Container<Function>> {
        self.borrow().execution_context.read_current_function()
    }

    pub fn current_receiver(&self) -> Option<TreewalkValue> {
        self.borrow().execution_context.read_current_receiver()
    }

    pub fn read_captured_env(&self) -> Option<Box<Container<EnvironmentFrame>>> {
        self.borrow().scope_manager.read_captured_env()
    }

    pub fn read_globals(&self, interpreter: &TreewalkInterpreter) -> Container<Dict> {
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

    pub fn class_of_value(&self, result: &TreewalkValue) -> TreewalkValue {
        match result {
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(o) => o.get_type(),
            TreewalkValue::Object(o) => TreewalkValue::Class(o.borrow().class()),
            _ => TreewalkValue::Class(self.class_of_type(&result.get_type())),
        }
    }

    /// Return a singleton `Class` for builtin types such as list, set, tuple, dict, etc.
    pub fn class_of_type(&self, type_: &Type) -> Container<Class> {
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

    pub fn load_source(&self, import_path: &ImportPath) -> Option<Source> {
        let current_path = self.current_path();
        let search_paths = self.borrow().memphis_state.search_paths();
        Source::from_import_path(import_path, &current_path, &search_paths)
    }

    #[cfg(feature = "c_stdlib")]
    pub fn import_builtin_module(&self, import_path: &ImportPath) -> Container<CPythonModule> {
        self.borrow_mut()
            .builtin_module_cache
            .import_builtin_module(import_path)
    }

    pub fn store_module(&self, import_path: &ImportPath, module: Container<Module>) {
        self.borrow_mut()
            .module_store
            .store_module(import_path, module)
    }

    pub fn fetch_module(&self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.borrow_mut().module_store.fetch_module(import_path)
    }

    pub fn read_class(&self, import_path: &ImportPath) -> Option<Container<Class>> {
        let mut segments = import_path.segments().to_vec();
        let class_name = segments.pop()?;
        let module_path = ImportPath::from_segments(&segments);
        let module = self.fetch_module(&module_path)?;
        let binding = module.borrow();
        binding.get(&class_name)?.as_class()
    }
}
