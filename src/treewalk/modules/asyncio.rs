use crate::{
    core::Container,
    domain::Source,
    parser::types::ImportPath,
    treewalk::{
        executor::{AsyncioCreateTaskBuiltin, AsyncioRunBuiltin, AsyncioSleepBuiltin},
        type_system::CloneableCallable,
        types::Module,
        ModuleStore, TreewalkValue,
    },
};

fn get_asyncio_builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![
        Box::new(AsyncioRunBuiltin),
        Box::new(AsyncioSleepBuiltin),
        Box::new(AsyncioCreateTaskBuiltin),
    ]
}

pub fn import(module_store: &mut ModuleStore) {
    let mut asyncio_mod = Module::new(Source::default());
    for builtin in get_asyncio_builtins() {
        asyncio_mod.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }

    module_store.store_module(
        &ImportPath::Absolute(vec!["asyncio".to_string()]),
        Container::new(asyncio_mod),
    );
}
