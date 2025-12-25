use std::cell::UnsafeCell;

use crate::{
    core::Container,
    domain::{
        resolve_import_path, DebugCallStack, DebugStackFrame, FromImportPath, ModuleName,
        ToDebugStackFrame, Type,
    },
    runtime::MemphisState,
    treewalk::{
        modules::builtins,
        types::{Class, Dict, Exception, Function, Module},
        utils::EnvironmentFrame,
        DomainResult, ExecutionContextManager, Executor, ModuleStore, RaisedException, Scope,
        ScopeManager, TreewalkInterpreter, TreewalkValue, TypeRegistry,
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
    pub fn new(memphis_state: Container<MemphisState>) -> Self {
        let type_registry = TypeRegistry::new();

        // We still want the `TypeRegistry` to own the type classes, but we must make some of them
        // available in the builtin module before execution begins.
        let builtins_mod = builtins::init(&type_registry);

        let mut module_store = ModuleStore::new();
        module_store.load_native_modules(&type_registry);

        TreewalkState {
            memphis_state,
            module_store,
            type_registry,
            scope_manager: ScopeManager::new(Container::new(builtins_mod)),
            execution_context: ExecutionContextManager::new(),
            executor: Executor::new().into(),
            #[cfg(feature = "c_stdlib")]
            builtin_module_cache: BuiltinModuleCache::new(),
        }
    }
}

impl Container<TreewalkState> {
    pub fn memphis_state(&self) -> Container<MemphisState> {
        self.borrow().memphis_state.clone()
    }

    pub fn push_module_context(&self, module: Container<Module>) {
        self.push_stack_frame(&*module.borrow());
        self.push_module(module);
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

    pub fn current_exception(&self) -> Option<RaisedException> {
        self.borrow().execution_context.current_exception()
    }

    pub fn set_current_exception(&self, exception: RaisedException) {
        self.borrow_mut()
            .execution_context
            .set_current_exception(exception);
    }

    pub fn clear_current_exception(&self) {
        self.borrow_mut()
            .execution_context
            .clear_current_exception();
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

    pub fn resolve_import_path(&self, import_path: &FromImportPath) -> DomainResult<ModuleName> {
        let current_module = self.current_module();
        let current_module = current_module.borrow();
        let current_module = current_module.name();
        resolve_import_path(import_path, current_module)
            .map_err(|e| Exception::import_error(e.message()))
    }

    #[cfg(feature = "c_stdlib")]
    pub fn import_builtin_module(&self, module_name: &ModuleName) -> Container<CPythonModule> {
        self.borrow_mut()
            .builtin_module_cache
            .import_builtin_module(module_name)
    }

    pub fn store_module(&self, module: Container<Module>) {
        self.borrow_mut().module_store.store_module(module)
    }

    pub fn fetch_module(&self, module_name: &ModuleName) -> Option<Container<Module>> {
        self.borrow_mut().module_store.fetch_module(module_name)
    }

    pub fn read_class(&self, module: &ModuleName, class: &str) -> Option<Container<Class>> {
        let module = self.fetch_module(module)?;
        let binding = module.borrow();
        binding.get(class)?.as_class().ok()
    }
}
