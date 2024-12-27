use crate::{
    core::Container,
    domain::Context,
    treewalk::{
        executor::{AsyncioCreateTaskBuiltin, AsyncioRunBuiltin, AsyncioSleepBuiltin},
        types::{
            domain::{
                builtins::{
                    CallableBuiltin, DirBuiltin, GetattrBuiltin, GlobalsBuiltin, HashBuiltin,
                    IsinstanceBuiltin, IssubclassBuiltin, IterBuiltin, LenBuiltin, NextBuiltin,
                    PrintBuiltin,
                },
                traits::Callable,
            },
            utils::EnvironmentFrame,
            ExprResult, Module,
        },
    },
};

use super::{ModuleSource, Scope, TypeRegistry};

fn get_asyncio_builtins() -> Vec<Box<dyn Callable>> {
    vec![
        Box::new(AsyncioRunBuiltin),
        Box::new(AsyncioSleepBuiltin),
        Box::new(AsyncioCreateTaskBuiltin),
    ]
}

fn get_builtins() -> Vec<Box<dyn Callable>> {
    vec![
        Box::new(CallableBuiltin),
        Box::new(DirBuiltin),
        Box::new(GetattrBuiltin),
        Box::new(GlobalsBuiltin),
        Box::new(HashBuiltin),
        Box::new(IsinstanceBuiltin),
        Box::new(IssubclassBuiltin),
        Box::new(IterBuiltin),
        Box::new(LenBuiltin),
        Box::new(NextBuiltin),
        Box::new(PrintBuiltin),
    ]
}

fn init_builtin_scope() -> Scope {
    let mut scope = Scope::default();
    for builtin in get_builtins() {
        scope.insert(
            &builtin.name(),
            ExprResult::BuiltinFunction(Container::new(builtin)),
        );
    }

    let mut asyncio_scope = Scope::default();
    for builtin in get_asyncio_builtins() {
        asyncio_scope.insert(
            &builtin.name(),
            ExprResult::BuiltinFunction(Container::new(builtin)),
        );
    }

    scope.insert(
        "asyncio",
        ExprResult::Module(Container::new(Module::new(
            ModuleSource::default(),
            asyncio_scope,
        ))),
    );

    scope
}

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

    /// The read-only scope which contains builtin methods such as `print()`, `open()`, etc. There
    /// is only one of these so we do not need a stack.
    builtin_scope: Scope,

    /// This stack allows us to know whether to search on the `local_scope_stack` or the
    /// `module_stack` when resolving a symbol.
    context_stack: Vec<Context>,
}

impl ScopeManager {
    pub fn new() -> Self {
        ScopeManager {
            local_scope_stack: vec![Container::new(Scope::default())],
            captured_env_stack: vec![],
            module_stack: vec![Container::new(Module::default())],
            builtin_scope: init_builtin_scope(),
            context_stack: vec![Context::Global],
        }
    }

    /// This is to insert `list()`, `set()`, etc into the builtin scope. We must do it here instead
    /// of in `init_builtin_scope()` because we want to use the singleton instances owned by
    /// `TypeRegistry`.
    pub fn register_callable_builtin_types(&mut self, registry: &TypeRegistry) {
        for builtin_class in registry.get_callable_builtin_types() {
            self.builtin_scope.insert(
                builtin_class.borrow().builtin_type().into(),
                ExprResult::Class(builtin_class.clone()),
            );
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

    pub fn delete(&mut self, name: &str) -> Option<ExprResult> {
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

    pub fn read(&self, name: &str) -> Option<ExprResult> {
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
            // We really shouldn't be accessing the module scope directly here, but the `get`
            // method on either `MemberAccessor` or `ModuleInterface` requires a reference to the
            // Interpreter. We'll need to fix this at some point.
            if let Some(value) = module.borrow().get(name) {
                return Some(value);
            }
        }

        self.builtin_scope.get(name)
    }

    pub fn write(&mut self, name: &str, value: ExprResult) {
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

    /// Used during the parsing process to determine whether to insert a `Expr::FunctionCall` or
    /// `Expr::ClassInstantiation` into the AST.
    pub fn is_class(&self, name: &str) -> bool {
        matches!(self.read(name), Some(ExprResult::Class(_)))
    }
}
