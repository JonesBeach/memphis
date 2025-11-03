use crate::{
    core::Container,
    domain::Context,
    treewalk::{types::Module, utils::EnvironmentFrame, Scope, TreewalkValue},
};

/// This struct implements Python's scoping rules by storing data to power the
/// `read`/`write`/`delete` interface available to the interpreter.
///
/// The rule of thumb for Python scoping is LEGB: local, enclosing, global (aka module), builtin.
pub struct ScopeManager {
    /// A stack of `Scope` objects for each local (think: function) scope.
    local_scope_stack: Vec<Container<Scope>>,

    /// A stack of captured environments to support closures.
    captured_env_stack: Vec<Container<EnvironmentFrame>>,

    /// A stack of modules to support symbol resolution local to specific modules.
    module_stack: Vec<Container<Module>>,

    /// The read-only module which contains builtin methods such as `print()`, `open()`, etc. There
    /// is only one of these so we do not need a stack.
    builtins: Container<Module>,

    /// This stack allows us to know whether to search on the `local_scope_stack` or the
    /// `module_stack` when resolving a symbol.
    context_stack: Vec<Context>,
}

impl ScopeManager {
    pub fn new(builtins: Container<Module>) -> Self {
        ScopeManager {
            local_scope_stack: vec![Container::new(Scope::default())],
            captured_env_stack: vec![],
            module_stack: vec![],
            builtins,
            context_stack: vec![],
        }
    }

    pub fn push_captured_env(&mut self, scope: Container<EnvironmentFrame>) {
        self.captured_env_stack.push(scope);
    }

    pub fn pop_captured_env(&mut self) -> Option<Container<EnvironmentFrame>> {
        self.captured_env_stack.pop()
    }

    pub fn push_local(&mut self, scope: Container<Scope>) {
        self.local_scope_stack.push(scope);
        self.context_stack.push(Context::Local);
    }

    pub fn pop_local(&mut self) -> Option<Container<Scope>> {
        self.context_stack.pop();
        self.local_scope_stack.pop()
    }

    pub fn push_module(&mut self, module: Container<Module>) {
        self.module_stack.push(module);
        self.context_stack.push(Context::Global);
    }

    pub fn pop_module(&mut self) -> Option<Container<Module>> {
        self.context_stack.pop();
        self.module_stack.pop()
    }

    /// Given a variable `var`, indicate that `var` should refer to the variable in the
    /// global/module scope for the duration of the current local scope.
    pub fn mark_global(&self, var: &str) {
        self.read_local().borrow_mut().mark_global(var);
    }

    /// Given a variable `var`, indicate that `var` should refer to the variable in the enclosing
    /// scope for the duration of the current local scope.
    pub fn mark_nonlocal(&self, var: &str) {
        self.read_local().borrow_mut().mark_nonlocal(var);
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        if self.read_local().borrow().get(name).is_some() {
            return self.read_local().borrow_mut().delete(name);
        }

        // TODO it sounds like there may be some nuances but ultimately we should be able to delete
        // from a captured env

        for module in self.module_stack.iter_mut().rev() {
            if module.borrow().get(name).is_some() {
                return module.borrow_mut().delete(name);
            }
        }

        None
    }

    pub fn read(&self, name: &str) -> Option<TreewalkValue> {
        if let Some(value) = self.read_local().borrow().get(name) {
            return Some(value);
        }

        // TODO I'm not sure we should be searching the entire captured environment here. I think
        // only the closure free vars should be available, but I don't yet know of a good way to
        // connect those here.
        if let Some(env) = self.read_captured_env() {
            if let Some(value) = env.borrow().read(name) {
                return Some(value);
            }
        }

        for module in self.module_stack.iter().rev() {
            if let Some(value) = module.borrow().get(name) {
                return Some(value);
            }
        }

        self.builtins.borrow().get(name)
    }

    pub fn write(&mut self, name: &str, value: TreewalkValue) {
        let local_scope = self.read_local().borrow().clone();

        if local_scope.has_global(name) {
            self.read_module().borrow_mut().insert(name, value);
        } else if local_scope.has_nonlocal(name) {
            if let Some(env) = self.read_captured_env() {
                env.borrow_mut().write(name, value);
            }
        } else {
            match self.read_context() {
                Context::Local => {
                    self.read_local().borrow_mut().insert(name, value);
                }
                Context::Global => {
                    self.read_module().borrow_mut().insert(name, value);
                }
            }
        }
    }

    /// This assumes we always have a local scope stack.
    pub fn read_local(&self) -> Container<Scope> {
        self.local_scope_stack
            .last()
            .expect("failed to find local scope")
            .clone()
    }

    pub fn read_captured_env(&self) -> Option<Box<Container<EnvironmentFrame>>> {
        self.captured_env_stack.last().cloned().map(Box::new)
    }

    /// This assumes we always have a module stack.
    pub fn read_module(&self) -> Container<Module> {
        self.module_stack
            .last()
            .expect("failed to find module scope")
            .clone()
    }

    /// This assumes we always have a context stack.
    fn read_context(&self) -> &Context {
        self.context_stack.last().expect("failed to find context")
    }
}
