use std::path::PathBuf;

use crate::{
    core::Container,
    domain::{DebugCallStack, DebugStackFrame},
    parser::types::{ImportPath, LoopIndex},
    treewalk::{
        types::{domain::Type, utils::EnvironmentFrame, Class, Dict, ExprResult, Function, Module},
        ExecutionContextManager, Executor, ModuleLoader, ModuleSource, Scope, ScopeManager,
        TypeRegistry,
    },
};

#[cfg(feature = "c_stdlib")]
use super::types::cpython::CPythonModule;
use super::Interpreter;

pub struct State {
    module_loader: ModuleLoader,
    scope_manager: ScopeManager,
    call_stack: DebugCallStack,
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
    pub fn new() -> Self {
        let type_registry = TypeRegistry::new();
        let mut scope_manager = ScopeManager::new();

        // We still want the `TypeRegistry` to own the type classes, but we must make some of them
        // available in the builtin scope before execution begins.
        scope_manager.register_callable_builtin_types(&type_registry);

        State {
            scope_manager,
            module_loader: ModuleLoader::new(),
            call_stack: DebugCallStack::new(),
            executor: Container::new(Executor::new()),
            type_registry,
            execution_context: ExecutionContextManager::new(),
        }
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

    /// Write an `ExprResult` to the symbol table.
    pub fn write(&self, name: &str, value: ExprResult) {
        self.borrow_mut().scope_manager.write(name, value);
    }

    /// Attempt to read an `ExprResult`, adhering to Python scoping rules.
    pub fn read(&self, name: &str) -> Option<ExprResult> {
        self.borrow().scope_manager.read(name)
    }

    /// Attempt to delete an `ExprResult`, adhering to Python scoping rules.
    pub fn delete(&self, name: &str) -> Option<ExprResult> {
        self.borrow_mut().scope_manager.delete(name)
    }

    pub fn write_loop_index(&self, index: &LoopIndex, value: ExprResult) {
        match index {
            LoopIndex::Variable(var) => {
                self.write(var, value);
            }
            LoopIndex::Tuple(tuple_index) => {
                for (key, value) in tuple_index.iter().zip(value) {
                    self.write(key, value);
                }
            }
        };
    }

    /// Return the `CallStack` at the current moment in time. This should be used at the time of an
    /// exception or immediately before any other use as it is a snapshot and will not keep updating.
    pub fn call_stack(&self) -> DebugCallStack {
        self.borrow().call_stack.clone()
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

    pub fn push_context(&self, stack_frame: DebugStackFrame) {
        self.borrow_mut().call_stack.push_context(stack_frame);
    }

    pub fn set_line(&self, line: usize) {
        self.borrow_mut().call_stack.set_line(line);
    }

    pub fn pop_context(&self) -> Option<DebugStackFrame> {
        self.borrow_mut().call_stack.pop_context()
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

    pub fn register_root(&self, filepath: PathBuf) {
        self.borrow_mut().module_loader.register_root(filepath);
    }

    #[cfg(feature = "c_stdlib")]
    pub fn import_builtin_module(&self, import_path: &ImportPath) -> Container<CPythonModule> {
        self.borrow_mut()
            .module_loader
            .import_builtin_module(import_path)
    }

    pub fn load_module(
        &self,
        import_path: &ImportPath,
        current_path: &PathBuf,
    ) -> Option<ModuleSource> {
        self.borrow_mut()
            .module_loader
            .load_module(import_path, current_path)
    }

    pub fn store_module(&self, import_path: &ImportPath, module: Container<Module>) {
        self.borrow_mut()
            .module_loader
            .store_module(import_path, module)
    }

    pub fn fetch_module(&self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.borrow_mut().module_loader.fetch_module(import_path)
    }
}
