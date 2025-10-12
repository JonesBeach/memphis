use crate::{
    core::Container,
    domain::{ImportPath, Source},
    treewalk::{types::Module, ModuleStore},
};

pub fn import(module_store: &mut ModuleStore) {
    let net_mod = Module::new(Source::default());
    module_store.store_module(
        &ImportPath::Absolute(vec!["memphis".to_string(), "net".to_string()]),
        Container::new(net_mod),
    );
}
