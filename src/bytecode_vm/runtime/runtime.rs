use std::collections::HashMap;

use crate::{
    bytecode_vm::{
        runtime::{
            modules::{asyncio, builtins},
            types::Module,
            BuiltinFn, BuiltinFunction, Heap,
        },
        VmValue,
    },
    core::Container,
    domain::Dunder,
};

#[derive(Default)]
pub struct Runtime {
    /// This is kind of similar to the heap. When an object is created, it will live here and a
    /// reference to it will be placed on the stack. Objects here can be from any function context.
    /// This store retains ownership of the Rust objects throughout the runtime. When non-primitive
    /// objects are pushed onto the stack, a reference is used so as to not take ownership of the
    /// objects.
    pub heap: Heap,

    module_store: HashMap<String, Container<Module>>,
}

impl Runtime {
    pub fn new() -> Self {
        let mut runtime = Self::default();

        let _ = runtime.create_module(&Dunder::Main);

        builtins::init_module(&mut runtime);
        asyncio::init_module(&mut runtime);

        runtime
    }

    pub fn read_module(&self, name: &str) -> Option<Container<Module>> {
        self.module_store.get(name).cloned()
    }

    pub fn store_module(&mut self, module: Container<Module>) {
        let name = module.borrow().name.to_owned();
        self.module_store.insert(name, module);
    }

    /// Create a new empty `Module` of a given name and store it in the `Runtime`.
    pub fn create_module(&mut self, name: &str) -> Container<Module> {
        let module = Container::new(Module::new(name));
        self.store_module(module.clone());
        module
    }
}

pub fn register_builtin_funcs(
    runtime: &mut Runtime,
    module: &mut Module,
    builtins: &[(&str, BuiltinFn)],
) {
    for (name, func) in builtins {
        let func_ref = runtime
            .heap
            .allocate(VmValue::BuiltinFunction(BuiltinFunction::new(name, *func)));
        module.write(name, func_ref);
    }
}
